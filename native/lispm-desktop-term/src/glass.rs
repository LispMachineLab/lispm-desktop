// lispm-desktop-term: glass, the desktop's see-through windows on macOS.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Emacs paints every pixel of a frame opaque, and NS Emacs leaves the
// `alpha-background' frame parameter alone, so a frame cannot let the
// desktop show through where it is dark.  A look of glass (see
// lispm-desktop-glass.el) has Core Animation work each pixel's opacity out
// from its colour instead, as the window server composites the window:
//
//   alpha = GAIN * (red + green + blue)
//
// so pure black is clear, the darkest colours see-through and the rest
// solid, text on them too.  The frame's window is made non-opaque, its
// own background clear, and kept so: Emacs sets both again whenever the
// frame's background colour changes.
//
// The filter is Core Animation's own `colorMatrix', run where the window
// server composites.  A Core Image filter does the same sum, but costs a
// frame each time the window's contents change, halving the frames shown
// while a window scrolls.  `colorMatrix' is not public API: where the
// system lacks it, windows stay opaque.

#![cfg_attr(not(target_os = "macos"), allow(dead_code))]

/// Return Core Animation's colour matrix for glass of GAIN, row by row:
/// red, green and blue as they are, alpha GAIN times their sum.
pub fn color_matrix(gain: f32) -> [f32; 20] {
    [
        1.0, 0.0, 0.0, 0.0, 0.0, //
        0.0, 1.0, 0.0, 0.0, 0.0, //
        0.0, 0.0, 1.0, 0.0, 0.0, //
        gain, gain, gain, 0.0, 0.0,
    ]
}

#[cfg(target_os = "macos")]
mod mac {
    use std::ffi::c_void;
    use std::sync::OnceLock;

    use emacs::{defun, Env, Result};
    use objc2::ffi::{
        objc_getAssociatedObject, objc_setAssociatedObject, OBJC_ASSOCIATION_RETAIN_NONATOMIC,
    };
    use objc2::rc::Retained;
    use objc2::runtime::{AnyClass, AnyObject, Bool, Imp, Sel};
    use objc2::{class, msg_send, sel, ClassType, MainThreadMarker};
    use objc2_app_kit::{NSApplication, NSView, NSWindow};
    use objc2_foundation::NSString;

    use super::color_matrix;

    /// The type encoding of Core Animation's `CAColorMatrix', twenty floats.
    const MATRIX_TYPE: &std::ffi::CStr = c"{CAColorMatrix=ffffffffffffffffffff}";

    /// The key under which a window keeps its glass's gain, an NSNumber.
    static GAIN_KEY: u8 = 0;

    type SetOpaque = unsafe extern "C-unwind" fn(*mut AnyObject, Sel, Bool);
    type SetColor = unsafe extern "C-unwind" fn(*mut AnyObject, Sel, *mut AnyObject);

    /// AppKit's own -[NSWindow setOpaque:] and -setBackgroundColor:.
    static ORIGINAL_OPAQUE: OnceLock<Imp> = OnceLock::new();
    static ORIGINAL_BACKGROUND: OnceLock<Imp> = OnceLock::new();

    fn key() -> *const c_void {
        &GAIN_KEY as *const u8 as *const c_void
    }

    fn main_thread(env: &Env) -> Result<MainThreadMarker> {
        match MainThreadMarker::new() {
            Some(mtm) => Ok(mtm),
            None => env.signal(
                env.intern("error")?,
                ("Windows are Emacs's thread's to change",),
            ),
        }
    }

    fn application(_: MainThreadMarker) -> Option<Retained<NSApplication>> {
        extern "C" {
            static NSApp: *mut NSApplication;
        }
        // SAFETY: AppKit's own global, read on Emacs's thread.
        unsafe { Retained::retain(NSApp) }
    }

    /// Return the gain of WINDOW's glass, or None while it has none.
    fn gain_of(window: *const AnyObject) -> Option<f64> {
        // SAFETY: an object of AppKit's, asked on Emacs's thread.
        unsafe {
            let number = objc_getAssociatedObject(window, key());
            if number.is_null() {
                None
            } else {
                Some(msg_send![number, doubleValue])
            }
        }
    }

    fn clear_color() -> *mut AnyObject {
        // SAFETY: a class method of AppKit's, returning a shared colour.
        unsafe { msg_send![class!(NSColor), clearColor] }
    }

    unsafe extern "C-unwind" fn set_opaque(this: *mut AnyObject, cmd: Sel, flag: Bool) {
        let flag = if gain_of(this).is_some() {
            Bool::NO
        } else {
            flag
        };
        let original: SetOpaque =
            std::mem::transmute::<Imp, SetOpaque>(*ORIGINAL_OPAQUE.get().unwrap_unchecked());
        original(this, cmd, flag)
    }

    unsafe extern "C-unwind" fn set_background(
        this: *mut AnyObject,
        cmd: Sel,
        color: *mut AnyObject,
    ) {
        let color = if gain_of(this).is_some() {
            clear_color()
        } else {
            color
        };
        let original: SetColor =
            std::mem::transmute::<Imp, SetColor>(*ORIGINAL_BACKGROUND.get().unwrap_unchecked());
        original(this, cmd, color)
    }

    /// Keep windows of glass non-opaque and clear, whatever Emacs sets: a
    /// window without glass is passed through as it was.
    fn install() {
        let class = AnyClass::get(c"EmacsWindow").unwrap_or(NSWindow::class());
        ORIGINAL_OPAQUE.get_or_init(|| {
            let method = class
                .instance_method(sel!(setOpaque:))
                .expect("NSWindow has -setOpaque:");
            let ours: SetOpaque = set_opaque;
            // SAFETY: `set_opaque' has -setOpaque:'s signature.
            unsafe { method.set_implementation(std::mem::transmute::<SetOpaque, Imp>(ours)) }
        });
        ORIGINAL_BACKGROUND.get_or_init(|| {
            let method = class
                .instance_method(sel!(setBackgroundColor:))
                .expect("NSWindow has -setBackgroundColor:");
            let ours: SetColor = set_background;
            // SAFETY: `set_background' has -setBackgroundColor:'s signature.
            unsafe { method.set_implementation(std::mem::transmute::<SetColor, Imp>(ours)) }
        });
    }

    /// Return the window of the frame whose record is at ADDRESS: the one
    /// whose EmacsView points at it.
    fn window_of(mtm: MainThreadMarker, address: usize) -> Option<Retained<NSWindow>> {
        let view_class = AnyClass::get(c"EmacsView")?;
        let offset = view_class.instance_variable(c"emacsframe")?.offset();
        let application = application(mtm)?;
        let windows = application.windows();
        windows.iter().find(|window| {
            let Some(content) = window.contentView() else {
                return false;
            };
            let mut views: Vec<Retained<NSView>> = vec![content.clone()];
            views.extend(content.subviews().iter());
            views.iter().any(|view| {
                // SAFETY: asking a view its class, then reading the frame
                // an EmacsView keeps, a pointer at the offset its class has.
                unsafe {
                    let emacs: bool = msg_send![&**view, isKindOfClass: view_class];
                    emacs && {
                        let base = Retained::as_ptr(view) as *const u8;
                        *(base.offset(offset) as *const usize) == address
                    }
                }
            })
        })
    }

    fn layer_of(window: &NSWindow) -> Option<*mut AnyObject> {
        let content = window.contentView()?;
        content.setWantsLayer(true);
        // SAFETY: a layer-backed view's layer, on Emacs's thread.
        let layer: *mut AnyObject = unsafe { msg_send![&*content, layer] };
        (!layer.is_null()).then_some(layer)
    }

    /// Make WINDOW glass of GAIN; return whether the system could.
    fn apply(window: &NSWindow, gain: f64) -> bool {
        let Some(filter_class) = AnyClass::get(c"CAFilter") else {
            return false;
        };
        let Some(layer) = layer_of(window) else {
            return false;
        };
        install();
        let raw = window as *const NSWindow as *mut AnyObject;
        let matrix = color_matrix(gain as f32);
        // SAFETY: AppKit and Core Animation objects, made and set on
        // Emacs's thread; the matrix outlives the call copying it.
        unsafe {
            let number: *mut AnyObject = msg_send![class!(NSNumber), numberWithDouble: gain];
            objc_setAssociatedObject(raw, key(), number, OBJC_ASSOCIATION_RETAIN_NONATOMIC);
            let name = NSString::from_str("colorMatrix");
            let filter: *mut AnyObject = msg_send![filter_class, filterWithType: &*name];
            if filter.is_null() {
                objc_setAssociatedObject(
                    raw,
                    key(),
                    std::ptr::null_mut(),
                    OBJC_ASSOCIATION_RETAIN_NONATOMIC,
                );
                return false;
            }
            let value: *mut AnyObject = msg_send![class!(NSValue), valueWithBytes: matrix.as_ptr() as *const c_void,
                                                  objCType: MATRIX_TYPE.as_ptr()];
            let input = NSString::from_str("inputColorMatrix");
            let _: () = msg_send![filter, setValue: value, forKey: &*input];
            let filters: *mut AnyObject = msg_send![class!(NSArray), arrayWithObject: filter];
            let _: () = msg_send![layer, setFilters: filters];
            // Set through `install''s own methods, which make them so.
            let _: () = msg_send![raw, setOpaque: Bool::NO];
            let _: () = msg_send![raw, setBackgroundColor: clear_color()];
            let _: () = msg_send![raw, invalidateShadow];
        }
        true
    }

    /// Take WINDOW's glass away.  Emacs sets its background and opacity
    /// again with the frame's background colour.
    fn clear(window: &NSWindow) {
        let raw = window as *const NSWindow as *mut AnyObject;
        // SAFETY: as in `apply'.
        unsafe {
            objc_setAssociatedObject(
                raw,
                key(),
                std::ptr::null_mut(),
                OBJC_ASSOCIATION_RETAIN_NONATOMIC,
            );
            if let Some(layer) = layer_of(window) {
                let none: *mut AnyObject = std::ptr::null_mut();
                let _: () = msg_send![layer, setFilters: none];
            }
            let _: () = msg_send![raw, setOpaque: Bool::YES];
            let _: () = msg_send![raw, invalidateShadow];
        }
    }

    /// Make the window of the frame whose record is at ADDRESS glass of
    /// GAIN: each pixel as opaque as GAIN times the sum of its red, green
    /// and blue, between 0 and 1.  Return non-nil once it is; nil when no
    /// window is the frame's, or the system draws no glass.
    #[defun(mod_in_name = false, name = "glass-apply")]
    fn glass_apply(env: &Env, address: i64, gain: f64) -> Result<bool> {
        let mtm = main_thread(env)?;
        Ok(match window_of(mtm, address as usize) {
            Some(window) => {
                gain_of(&*window as *const NSWindow as *const AnyObject) == Some(gain)
                    || apply(&window, gain)
            }
            None => false,
        })
    }

    /// Make the window of the frame whose record is at ADDRESS opaque
    /// again.  Return non-nil when it had glass.
    #[defun(mod_in_name = false, name = "glass-clear")]
    fn glass_clear(env: &Env, address: i64) -> Result<bool> {
        let mtm = main_thread(env)?;
        Ok(match window_of(mtm, address as usize) {
            Some(window) => {
                let had = gain_of(&*window as *const NSWindow as *const AnyObject).is_some();
                clear(&window);
                had
            }
            None => false,
        })
    }

    /// Return the gain of the glass of the window of the frame whose record
    /// is at ADDRESS, or nil.
    #[defun(mod_in_name = false, name = "glass-gain")]
    fn glass_gain(env: &Env, address: i64) -> Result<Option<f64>> {
        let mtm = main_thread(env)?;
        Ok(window_of(mtm, address as usize)
            .and_then(|window| gain_of(&*window as *const NSWindow as *const AnyObject)))
    }
}

#[cfg(test)]
mod tests {
    use super::color_matrix;

    #[test]
    fn glass_keeps_colours_and_sums_alpha() {
        let m = color_matrix(4.0);
        // Colours pass as they are.
        assert_eq!(&m[0..5], &[1.0, 0.0, 0.0, 0.0, 0.0]);
        assert_eq!(&m[6..7], &[1.0]);
        assert_eq!(&m[12..13], &[1.0]);
        // Alpha is the gain times red, green and blue, nothing of the
        // pixel's own alpha and no bias: pure black is clear.
        assert_eq!(&m[15..20], &[4.0, 4.0, 4.0, 0.0, 0.0]);
    }
}
