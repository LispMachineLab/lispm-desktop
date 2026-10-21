// The pointer while LispM works: Mac OS 9's wristwatch, drawn by AppKit.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Emacs's Mac build knows a few fixed pointer shapes only, and shows no
// busy pointer at all: its hourglass is a stub there.  So the desktop's
// "loading" pointer is made here, from frames lispm-desktop-loading.el
// hands over (rows of "#" black, "." white, " " clear), and shown over
// Emacs's windows
//
//   - while Lisp says a loading is in front (`cursor-loading'), and
//   - while Emacs's thread has stayed away from its event loop longer than
//     a delay (`cursor-busy-delay'), as Emacs's own hourglass does
//     elsewhere.  A run-loop observer tells when it leaves and comes back.
//
// A thread of this module turns the watch's hands, and asks the window
// server where the pointer is: Emacs's thread never waits on either, but
// only flips a flag and notes its own windows.  Every -[NSCursor set] of
// the process goes through `set_cursor' below, which serialises them, and
// which shows the watch instead of whatever AppKit, Emacs or WebKit sets
// while the watch is up.

// Off the Mac, only the tests use what follows.
#![cfg_attr(not(target_os = "macos"), allow(dead_code))]

use std::time::Duration;

/// One frame of a pointer: its size in points and its pixels, RGBA.
#[derive(Debug, PartialEq)]
pub struct Pixels {
    pub width: usize,
    pub height: usize,
    pub rgba: Vec<u8>,
}

/// Read a frame drawn as ROWS, each a line of "#" (black), "." (white) and
/// " " (clear), into its pixels at SCALE pixels per point.  Short rows end
/// in clear pixels.
pub fn pixels(rows: &str, scale: usize) -> Result<Pixels, String> {
    let lines: Vec<&str> = rows.lines().collect();
    let width = lines
        .iter()
        .map(|line| line.chars().count())
        .max()
        .unwrap_or(0);
    let height = lines.len();
    if width == 0 || scale == 0 {
        return Err("a pointer frame needs at least one pixel".into());
    }
    let mut rgba = vec![0u8; width * scale * height * scale * 4];
    for (y, line) in lines.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            let color = match c {
                '#' => [0, 0, 0, 255],
                '.' => [255, 255, 255, 255],
                ' ' => continue,
                other => return Err(format!("a pointer frame has {other:?}, not #, . or space")),
            };
            for dy in 0..scale {
                for dx in 0..scale {
                    let i = (((y * scale + dy) * width * scale) + x * scale + dx) * 4;
                    rgba[i..i + 4].copy_from_slice(&color);
                }
            }
        }
    }
    Ok(Pixels {
        width,
        height,
        rgba,
    })
}

/// What decides whether the watch is up.
#[derive(Clone, Copy, Debug, Default)]
pub struct Situation {
    /// Frames of the watch there are.
    pub frames: usize,
    /// Lisp says a loading is in front.
    pub loading: bool,
    /// How long Emacs's thread has been away from its event loop, if it is.
    pub busy_for: Option<Duration>,
    /// How long it may be away before the watch shows, if it is watched.
    pub busy_delay: Option<Duration>,
    /// Emacs is the active application.
    pub active: bool,
    /// The pointer is over one of Emacs's windows.
    pub over_emacs: bool,
}

/// Whether the watch is to be up in SITUATION.
pub fn wanted(situation: &Situation) -> bool {
    let busy = matches!((situation.busy_for, situation.busy_delay),
                        (Some(busy_for), Some(delay)) if busy_for >= delay);
    situation.frames > 0 && situation.active && situation.over_emacs && (situation.loading || busy)
}

/// The frame to show next of FRAMES, FRAME being shown or, unless SHOWING,
/// the watch not being up: a watch comes up at its first frame.
pub fn next_frame(showing: bool, frame: usize, frames: usize) -> usize {
    if showing && frames > 0 {
        (frame + 1) % frames
    } else {
        0
    }
}

#[cfg(target_os = "macos")]
mod mac {
    use std::ffi::c_void;
    use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, AtomicUsize, Ordering};
    use std::sync::{Condvar, Mutex, MutexGuard, OnceLock, PoisonError};
    use std::time::{Duration, Instant};

    use dispatch2::DispatchQueue;
    use emacs::{defun, Env, Result, Value, Vector};
    use objc2::rc::Retained;
    use objc2::runtime::{AnyObject, Imp, Sel};
    use objc2::{msg_send, sel, AllocAnyThread, ClassType, MainThreadMarker};
    use objc2_app_kit::{
        NSApplication, NSBitmapImageRep, NSCursor, NSDeviceRGBColorSpace, NSEvent, NSImage,
        NSWindow,
    };
    use objc2_core_foundation::{
        kCFRunLoopCommonModes, CFRunLoop, CFRunLoopActivity, CFRunLoopObserver,
    };
    use objc2_foundation::{NSInteger, NSPoint, NSSize};

    use super::{next_frame, pixels, wanted, Situation};

    /// The watch's frames, which live as long as Emacs: `CURRENT' points
    /// into them from any thread.
    static FRAMES: Mutex<Vec<usize>> = Mutex::new(Vec::new());
    /// The frame up while `SHOWING'.
    static CURRENT: AtomicPtr<AnyObject> = AtomicPtr::new(std::ptr::null_mut());
    static FRAME: AtomicUsize = AtomicUsize::new(0);
    static SHOWING: AtomicBool = AtomicBool::new(false);
    static LOADING: AtomicBool = AtomicBool::new(false);
    /// Milliseconds between two frames; 0 until the frames are defined.
    static INTERVAL: AtomicU64 = AtomicU64::new(0);
    /// Milliseconds Emacs's thread may be away from its event loop before
    /// the watch shows, plus one; 0 when that is not watched.
    static BUSY_DELAY: AtomicU64 = AtomicU64::new(0);
    /// Nanoseconds after `epoch' at which Emacs's thread left its event
    /// loop; 0 while it waits there.
    static BUSY_SINCE: AtomicU64 = AtomicU64::new(0);
    /// The original -[NSCursor set].
    static ORIGINAL_SET: OnceLock<Imp> = OnceLock::new();
    /// Held around every -[NSCursor set], from whichever thread.
    static SET_LOCK: Mutex<()> = Mutex::new(());
    /// Set, with `WAKE' notified, to have the module's thread look at once.
    static WOKEN: Mutex<bool> = Mutex::new(false);
    static WAKE: Condvar = Condvar::new();
    /// Emacs's visible windows and whether it is active, as Emacs's thread
    /// last noted them, for the module's thread.
    static WINDOWS: Mutex<Vec<NSInteger>> = Mutex::new(Vec::new());
    static ACTIVE: AtomicBool = AtomicBool::new(false);
    /// Nanoseconds after `epoch' at which `WINDOWS' was last noted.
    static WINDOWS_NOTED: AtomicU64 = AtomicU64::new(0);
    /// Frames shown so far, for `cursor-state'.
    static TURNS: AtomicU64 = AtomicU64::new(0);

    fn epoch() -> Instant {
        static EPOCH: OnceLock<Instant> = OnceLock::new();
        *EPOCH.get_or_init(Instant::now)
    }

    fn now() -> u64 {
        // Never 0, which says "not busy".
        (epoch().elapsed().as_nanos() as u64).max(1)
    }

    fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
        mutex.lock().unwrap_or_else(PoisonError::into_inner)
    }

    /// Have the module's thread look at once.
    fn wake() {
        *lock(&WOKEN) = true;
        WAKE.notify_one();
    }

    /// The type of -[NSCursor set].
    type Set = unsafe extern "C-unwind" fn(*mut AnyObject, Sel);

    /// -[NSCursor set], for every cursor of the process: the watch's frame
    /// instead of any other while it is up, one set at a time.
    unsafe extern "C-unwind" fn set_cursor(this: *mut AnyObject, command: Sel) {
        let Some(original) = ORIGINAL_SET.get() else {
            return;
        };
        let original = std::mem::transmute::<Imp, Set>(*original);
        let current = CURRENT.load(Ordering::Acquire);
        let shown = if SHOWING.load(Ordering::Acquire) && !current.is_null() {
            current
        } else {
            this
        };
        let _set = lock(&SET_LOCK);
        original(shown, command);
    }

    fn install_set() {
        ORIGINAL_SET.get_or_init(|| {
            let method = NSCursor::class()
                .instance_method(sel!(set))
                .expect("NSCursor has -set");
            let ours: Set = set_cursor;
            // SAFETY: `set_cursor' has -set's signature.
            unsafe { method.set_implementation(std::mem::transmute::<Set, Imp>(ours)) }
        });
    }

    fn show(cursor: *mut AnyObject) {
        CURRENT.store(cursor, Ordering::Release);
        SHOWING.store(true, Ordering::Release);
        // SAFETY: CURSOR is one of `FRAMES', which are never released.
        unsafe {
            let _: () = msg_send![cursor, set];
        }
    }

    /// Show the next frame of the watch.
    fn turn() {
        let frames = lock(&FRAMES);
        if frames.is_empty() {
            return;
        }
        let frame = next_frame(
            SHOWING.load(Ordering::Acquire),
            FRAME.load(Ordering::Acquire),
            frames.len(),
        );
        FRAME.store(frame, Ordering::Release);
        TURNS.fetch_add(1, Ordering::Relaxed);
        show(frames[frame] as *mut AnyObject);
    }

    /// Take the watch down: the arrow, until AppKit sets the pointer of
    /// what is under it, Emacs's thread resetting its windows' cursor
    /// rectangles once it is free.
    fn take_down() {
        SHOWING.store(false, Ordering::Release);
        NSCursor::arrowCursor().set();
        DispatchQueue::main().exec_async(|| {
            // SAFETY: the main queue runs on Emacs's thread.
            let mtm = unsafe { MainThreadMarker::new_unchecked() };
            for window in application(mtm)
                .iter()
                .flat_map(|application| application.windows())
            {
                if let Some(view) = window.contentView() {
                    window.invalidateCursorRectsForView(&view);
                }
            }
        });
    }

    /// Emacs's application object, if there is one: a batch Emacs has none.
    fn application(_: MainThreadMarker) -> Option<Retained<NSApplication>> {
        extern "C" {
            static NSApp: *mut NSApplication;
        }
        // SAFETY: AppKit's own global, read on Emacs's thread.
        unsafe { Retained::retain(NSApp) }
    }

    /// For tests of a test Emacs in the background: the watch shows
    /// wherever the pointer is, whether Emacs is active or not.
    fn everywhere() -> bool {
        static EVERYWHERE: OnceLock<bool> = OnceLock::new();
        *EVERYWHERE.get_or_init(|| std::env::var_os("LISPM_DESKTOP_CURSOR_EVERYWHERE").is_some())
    }

    fn pointer_window() -> NSInteger {
        let point: NSPoint = NSEvent::mouseLocation();
        // SAFETY: a question to the window server, which any thread may ask.
        unsafe {
            msg_send![NSWindow::class(), windowNumberAtPoint: point, belowWindowWithWindowNumber: 0 as NSInteger]
        }
    }

    /// Note Emacs's visible windows and whether it is active, on Emacs's
    /// thread: AppKit's own lists, no question to the window server.
    fn note_windows(mtm: MainThreadMarker) {
        let Some(application) = application(mtm) else {
            return;
        };
        let numbers = application
            .windows()
            .iter()
            .filter(|w| w.isVisible())
            .map(|w| w.windowNumber())
            .collect();
        *lock(&WINDOWS) = numbers;
        ACTIVE.store(application.isActive(), Ordering::Release);
        WINDOWS_NOTED.store(now(), Ordering::Release);
    }

    fn busy_for() -> Option<Duration> {
        match BUSY_SINCE.load(Ordering::Acquire) {
            0 => None,
            since => Some(Duration::from_nanos(now().saturating_sub(since))),
        }
    }

    fn watched_delay() -> Option<Duration> {
        match BUSY_DELAY.load(Ordering::Acquire) {
            0 => None,
            delay => Some(Duration::from_millis(delay - 1)),
        }
    }

    fn situation(over_emacs: bool, active: bool) -> Situation {
        Situation {
            frames: lock(&FRAMES).len(),
            loading: LOADING.load(Ordering::Acquire),
            busy_for: busy_for(),
            busy_delay: watched_delay(),
            active,
            over_emacs,
        }
    }

    /// Put the watch up, turn it, or take it down, on the module's thread.
    /// The window server is asked where the pointer is only when the watch
    /// may be up.
    fn tick() {
        let busy = matches!((busy_for(), watched_delay()),
                            (Some(busy_for), Some(delay)) if busy_for >= delay);
        let showing = SHOWING.load(Ordering::Acquire);
        if !(LOADING.load(Ordering::Acquire) || busy || showing) {
            return;
        }
        let over_emacs = lock(&WINDOWS).contains(&pointer_window());
        if wanted(&situation(
            over_emacs || everywhere(),
            ACTIVE.load(Ordering::Acquire) || everywhere(),
        )) {
            turn();
        } else if showing {
            take_down();
        }
    }

    fn run() {
        loop {
            let interval = Duration::from_millis(INTERVAL.load(Ordering::Acquire).max(10));
            let (mut woken, _) = WAKE
                .wait_timeout_while(lock(&WOKEN), interval, |woken| !*woken)
                .unwrap_or_else(PoisonError::into_inner);
            *woken = false;
            drop(woken);
            tick();
        }
    }

    /// Note Emacs's thread leaving and coming back to its event loop.
    unsafe extern "C-unwind" fn observe(
        _: *mut CFRunLoopObserver,
        activity: CFRunLoopActivity,
        _: *mut c_void,
    ) {
        if activity == CFRunLoopActivity::AfterWaiting {
            BUSY_SINCE.store(now(), Ordering::Release);
            if now().saturating_sub(WINDOWS_NOTED.load(Ordering::Acquire)) > 100_000_000 {
                note_windows(MainThreadMarker::new_unchecked());
            }
        } else if activity == CFRunLoopActivity::BeforeWaiting {
            BUSY_SINCE.store(0, Ordering::Release);
            // Done: the watch it brought goes at once.
            if SHOWING.load(Ordering::Acquire) && !LOADING.load(Ordering::Acquire) {
                wake();
            }
        }
    }

    fn main_thread(env: &Env) -> Result<MainThreadMarker> {
        match MainThreadMarker::new() {
            Some(mtm) => Ok(mtm),
            None => env.signal(
                env.intern("error")?,
                ("The pointer is Emacs's thread's to change",),
            ),
        }
    }

    fn cursor(
        rows: &str,
        hot_x: f64,
        hot_y: f64,
    ) -> std::result::Result<Retained<NSCursor>, String> {
        let one = pixels(rows, 1)?;
        let size = NSSize::new(one.width as f64, one.height as f64);
        let image = NSImage::initWithSize(NSImage::alloc(), size);
        for scale in [1, 2] {
            let frame = pixels(rows, scale)?;
            let (wide, high) = (frame.width * scale, frame.height * scale);
            // SAFETY: null planes make the representation allocate its own.
            let representation = unsafe {
                NSBitmapImageRep::initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel(
                    NSBitmapImageRep::alloc(), std::ptr::null_mut(), wide as NSInteger, high as NSInteger,
                    8, 4, true, false, NSDeviceRGBColorSpace, (wide * 4) as NSInteger, 32)
            }
            .ok_or("AppKit made no bitmap for a pointer frame")?;
            // SAFETY: the representation holds WIDE * HIGH RGBA pixels;
            // clear ones are 0, so their premultiplied form is the same.
            unsafe {
                std::ptr::copy_nonoverlapping(
                    frame.rgba.as_ptr(),
                    representation.bitmapData(),
                    frame.rgba.len(),
                )
            };
            representation.setSize(size);
            image.addRepresentation(&representation);
        }
        Ok(NSCursor::initWithImage_hotSpot(
            NSCursor::alloc(),
            &image,
            NSPoint::new(hot_x, hot_y),
        ))
    }

    /// Make FRAMES, a vector of strings, the watch's frames, its hot spot
    /// HOT-X, HOT-Y points from its top left corner, and show each for
    /// INTERVAL milliseconds.  Each frame is rows of "#" (black), "."
    /// (white) and " " (clear).
    #[defun(mod_in_name = false, name = "cursor-define")]
    fn define(env: &Env, frames: Vector, hot_x: i64, hot_y: i64, interval: i64) -> Result<()> {
        main_thread(env)?;
        let mut cursors = Vec::with_capacity(frames.len());
        for i in 0..frames.len() {
            match cursor(&frames.get::<String>(i)?, hot_x as f64, hot_y as f64) {
                // Never released: `CURRENT' may point at it from any thread.
                Ok(cursor) => cursors.push(Retained::into_raw(cursor) as usize),
                Err(message) => return env.signal(env.intern("error")?, (message,)),
            }
        }
        install_set();
        *lock(&FRAMES) = cursors;
        INTERVAL.store(interval.max(10) as u64, Ordering::Release);
        static THREAD: OnceLock<()> = OnceLock::new();
        THREAD.get_or_init(|| {
            std::thread::Builder::new()
                .name("lispm-desktop-cursor".into())
                .spawn(run)
                .ok();
        });
        Ok(())
    }

    /// Show the watch over Emacs's windows while FLAG is non-nil, a loading
    /// being in front.
    #[defun(mod_in_name = false, name = "cursor-loading")]
    fn loading(env: &Env, flag: Value) -> Result<()> {
        let mtm = main_thread(env)?;
        LOADING.store(flag.is_not_nil(), Ordering::Release);
        note_windows(mtm);
        wake();
        Ok(())
    }

    /// Show the watch once Emacs's thread has stayed away from its event
    /// loop for SECONDS, a number; nil never does.
    #[defun(mod_in_name = false, name = "cursor-busy-delay")]
    fn busy_delay(env: &Env, seconds: Option<f64>) -> Result<()> {
        let mtm = main_thread(env)?;
        static OBSERVER: OnceLock<()> = OnceLock::new();
        OBSERVER.get_or_init(|| {
            let activities = CFRunLoopActivity::BeforeWaiting.0 | CFRunLoopActivity::AfterWaiting.0;
            // SAFETY: `observe' is a run-loop observer callback, which
            // needs no context.
            let observer = unsafe {
                CFRunLoopObserver::new(
                    None,
                    activities,
                    true,
                    0,
                    Some(observe),
                    std::ptr::null_mut(),
                )
            };
            if let (Some(observer), Some(run_loop)) = (observer, CFRunLoop::main()) {
                // SAFETY: a constant of CoreFoundation's.
                run_loop.add_observer(Some(&observer), unsafe { kCFRunLoopCommonModes });
                // The run loop holds it from now on.
                std::mem::forget(observer);
            }
        });
        let delay = seconds.map_or(0, |seconds| (seconds.max(0.0) * 1000.0) as u64 + 1);
        BUSY_DELAY.store(delay, Ordering::Release);
        note_windows(mtm);
        Ok(())
    }

    /// Return what the pointer shows, as (SHOWING FRAME LOADING TURNS
    /// IS-WATCH): whether the watch is up, the frame of it last shown,
    /// whether a loading is in front, how many frames were shown so far,
    /// and whether AppKit's current cursor is a watch frame.
    #[defun(mod_in_name = false, name = "cursor-state")]
    fn state(env: &Env) -> Result<Value<'_>> {
        main_thread(env)?;
        let current = Retained::into_raw(NSCursor::currentCursor());
        let is_watch = lock(&FRAMES).contains(&(current as usize));
        // SAFETY: balances `into_raw' above.
        unsafe { drop(Retained::from_raw(current)) };
        env.list((
            SHOWING.load(Ordering::Acquire),
            FRAME.load(Ordering::Acquire) as i64,
            LOADING.load(Ordering::Acquire),
            TURNS.load(Ordering::Relaxed) as i64,
            is_watch,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pixels_read_black_white_and_clear() {
        let frame = pixels("#.\n ", 1).unwrap();
        assert_eq!((frame.width, frame.height), (2, 2));
        assert_eq!(&frame.rgba[0..4], &[0, 0, 0, 255]);
        assert_eq!(&frame.rgba[4..8], &[255, 255, 255, 255]);
        // The short row ends clear, as does the space.
        assert_eq!(&frame.rgba[8..16], &[0; 8]);
    }

    #[test]
    fn pixels_scale_each_point_to_a_square() {
        let frame = pixels("#.", 2).unwrap();
        assert_eq!(frame.rgba.len(), 4 * 2 * 4);
        for row in 0..2 {
            let at = |x: usize| &frame.rgba[(row * 4 + x) * 4..(row * 4 + x) * 4 + 4];
            assert_eq!(at(0), &[0, 0, 0, 255]);
            assert_eq!(at(1), &[0, 0, 0, 255]);
            assert_eq!(at(2), &[255, 255, 255, 255]);
            assert_eq!(at(3), &[255, 255, 255, 255]);
        }
    }

    #[test]
    fn pixels_refuse_other_characters_and_nothing() {
        assert!(pixels("#x", 1).is_err());
        assert!(pixels("", 1).is_err());
    }

    #[test]
    fn the_watch_shows_for_a_loading_or_a_long_busy_spell() {
        let base = Situation {
            frames: 8,
            active: true,
            over_emacs: true,
            ..Default::default()
        };
        assert!(!wanted(&base));
        assert!(wanted(&Situation {
            loading: true,
            ..base
        }));
        let delay = Some(Duration::from_secs(1));
        assert!(!wanted(&Situation {
            busy_for: Some(Duration::from_millis(400)),
            busy_delay: delay,
            ..base
        }));
        assert!(wanted(&Situation {
            busy_for: Some(Duration::from_secs(2)),
            busy_delay: delay,
            ..base
        }));
        // Not watched: busy as long as it likes.
        assert!(!wanted(&Situation {
            busy_for: Some(Duration::from_secs(9)),
            ..base
        }));
    }

    #[test]
    fn the_watch_stays_off_other_applications() {
        let loading = Situation {
            frames: 8,
            loading: true,
            active: true,
            over_emacs: true,
            ..Default::default()
        };
        assert!(!wanted(&Situation {
            over_emacs: false,
            ..loading
        }));
        assert!(!wanted(&Situation {
            active: false,
            ..loading
        }));
        assert!(!wanted(&Situation {
            frames: 0,
            ..loading
        }));
    }

    #[test]
    fn the_hands_start_at_twelve_and_go_round() {
        assert_eq!(next_frame(false, 5, 8), 0);
        assert_eq!(next_frame(true, 0, 8), 1);
        assert_eq!(next_frame(true, 7, 8), 0);
        assert_eq!(next_frame(true, 3, 0), 0);
    }
}
