/* app.m --- one of LispM's applications, a process of its own

   Copyright (C) 2026  Ryan.K
   SPDX-License-Identifier: AGPL-3.0-or-later

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.

   The executable of each application lispm-desktop-standalone.el makes
   for one of LispM's, in ~/Applications/LispM Apps: the application's
   own process, so its own tile in the Dock, its own menu bar and its own
   place in the application switcher.  LispM, an Emacs out of sight, draws
   its window, see display.m; this process shows that drawing in a window
   of its own and hands LispM what is typed and clicked there.

   Its Info.plist says which application it is (LispMApp), where LispM
   listens (LispMSocket), and how to start LispM out of sight when it does
   not (LispMApplication, LispMArguments).  They speak in lines of JSON,
   each an object whose "op" says what it is:

     to LispM     open, key, text, mouse, wheel, focus, menu, moved, quit
     from LispM   window, geometry, menus, cursor, activate, drag,
                  desktop, bye, stay

   Positions are in points from the top left of the main screen, as Emacs
   counts them, or of the window.  */

#import <AppKit/AppKit.h>
#import <QuartzCore/QuartzCore.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

static NSString *app_key;
static int lispm_socket = -1;

/* Send MESSAGE to LispM, a line of JSON.  */
static void
send_message (NSDictionary *message)
{
  if (lispm_socket < 0)
    return;
  NSData *data = [NSJSONSerialization dataWithJSONObject: message options: 0 error: nil];
  if (!data)
    return;
  NSMutableData *line = [data mutableCopy];
  [line appendBytes: "\n" length: 1];
  const char *bytes = [line bytes];
  size_t left = [line length];
  while (left > 0)
    {
      ssize_t sent = write (lispm_socket, bytes, left);
      if (sent <= 0)
        return;
      bytes += sent;
      left -= (size_t) sent;
    }
}

/* The modifiers of FLAGS, as Emacs names them: Option is Meta, Command is
   Super.  Shift only where SHIFT, as for keys that are not characters.  */
static NSArray *
modifiers (NSEventModifierFlags flags, BOOL shift)
{
  NSMutableArray *names = [NSMutableArray array];
  if (flags & NSEventModifierFlagControl)
    [names addObject: @"control"];
  if (flags & NSEventModifierFlagOption)
    [names addObject: @"meta"];
  if (flags & NSEventModifierFlagCommand)
    [names addObject: @"super"];
  if (shift && (flags & NSEventModifierFlagShift))
    [names addObject: @"shift"];
  return names;
}

/* The name Emacs gives the key of KEYCODE, when it is no character's.  */
static NSString *
key_name (unsigned short keycode)
{
  switch (keycode)
    {
    case 36: case 76: return @"return";
    case 48: return @"tab";
    case 51: return @"backspace";
    case 53: return @"escape";
    case 117: return @"delete";
    case 123: return @"left";
    case 124: return @"right";
    case 125: return @"down";
    case 126: return @"up";
    case 115: return @"home";
    case 119: return @"end";
    case 116: return @"prior";
    case 121: return @"next";
    case 114: return @"insert";
    case 122: return @"f1";
    case 120: return @"f2";
    case 99: return @"f3";
    case 118: return @"f4";
    case 96: return @"f5";
    case 97: return @"f6";
    case 98: return @"f7";
    case 100: return @"f8";
    case 101: return @"f9";
    case 109: return @"f10";
    case 103: return @"f11";
    case 111: return @"f12";
    default: return nil;
    }
}

/* Points of the main screen's top left, as Emacs counts, for RECT, as
   AppKit counts from its bottom left; and back.  */
static CGFloat
main_screen_height (void)
{
  return NSMaxY ([[[NSScreen screens] firstObject] frame]);
}

static NSRect
rect_from_lispm (NSDictionary *message)
{
  CGFloat x = [message[@"x"] doubleValue], y = [message[@"y"] doubleValue];
  CGFloat w = [message[@"w"] doubleValue], h = [message[@"h"] doubleValue];
  return NSMakeRect (x, main_screen_height () - y - h, w, h);
}

/* The window: no frame of its own, as LispM draws Mac OS 9's.  */
@interface LispMAppWindow : NSWindow
@end

@implementation LispMAppWindow
- (BOOL) canBecomeKeyWindow { return YES; }
- (BOOL) canBecomeMainWindow { return YES; }
@end

/* The view LispM's layers show in, which takes the keyboard and the mouse
   for it, and composes text with input methods.  */
@interface LispMAppView : NSView <NSTextInputClient>
@property (strong) NSAttributedString *marked;
@property (strong) CATextLayer *markedLayer;
@property NSRect cursor;
@property (strong) NSEvent *lastDown;
@property CGFloat wheel;
@property NSInteger buttons;
@end

@implementation LispMAppView

- (BOOL) isFlipped { return YES; }
- (BOOL) acceptsFirstResponder { return YES; }
- (BOOL) acceptsFirstMouse: (NSEvent *) event { return YES; }

- (NSDictionary *) position: (NSEvent *) event
{
  NSPoint point = [self convertPoint: [event locationInWindow] fromView: nil];
  return @{ @"x": @(point.x), @"y": @(point.y) };
}

- (void) mouse: (NSString *) kind button: (NSInteger) button event: (NSEvent *) event
{
  NSMutableDictionary *message = [[self position: event] mutableCopy];
  message[@"op"] = @"mouse";
  message[@"kind"] = kind;
  message[@"button"] = @(button);
  message[@"clicks"] = @([event clickCount]);
  message[@"mods"] = modifiers ([event modifierFlags], YES);
  send_message (message);
}

- (void) mouseDown: (NSEvent *) event
{
  self.lastDown = event;
  self.buttons |= 1;
  [self mouse: @"down" button: 1 event: event];
}
- (void) mouseUp: (NSEvent *) event { self.buttons &= ~1; [self mouse: @"up" button: 1 event: event]; }
- (void) mouseDragged: (NSEvent *) event { [self mouse: @"move" button: 1 event: event]; }
- (void) rightMouseDown: (NSEvent *) event { self.buttons |= 4; [self mouse: @"down" button: 3 event: event]; }
- (void) rightMouseUp: (NSEvent *) event { self.buttons &= ~4; [self mouse: @"up" button: 3 event: event]; }
- (void) rightMouseDragged: (NSEvent *) event { [self mouse: @"move" button: 3 event: event]; }
- (void) otherMouseDown: (NSEvent *) event { self.buttons |= 2; [self mouse: @"down" button: 2 event: event]; }
- (void) otherMouseUp: (NSEvent *) event { self.buttons &= ~2; [self mouse: @"up" button: 2 event: event]; }
- (void) otherMouseDragged: (NSEvent *) event { [self mouse: @"move" button: 2 event: event]; }

/* A wheel turns line by line, a trackpad by points: these are summed up
   into lines, one wheel event to LispM each.  */
- (void) scrollWheel: (NSEvent *) event
{
  CGFloat delta = [event scrollingDeltaY];
  if ([event hasPreciseScrollingDeltas])
    delta /= 16;
  self.wheel += delta;
  NSInteger lines = (NSInteger) self.wheel;
  if (lines == 0)
    return;
  self.wheel -= lines;
  NSMutableDictionary *message = [[self position: event] mutableCopy];
  message[@"op"] = @"wheel";
  message[@"lines"] = @(lines);
  message[@"mods"] = modifiers ([event modifierFlags], YES);
  send_message (message);
}

- (void) keyDown: (NSEvent *) event
{
  NSEventModifierFlags flags = [event modifierFlags];
  NSString *name = key_name ([event keyCode]);
  BOOL chord = (flags & (NSEventModifierFlagControl | NSEventModifierFlagOption
                         | NSEventModifierFlagCommand)) != 0;
  /* A key of its own, or a character with Control, Option or Command, goes
     to LispM as it is; text goes through the input method first.  */
  if (!self.marked && (name || chord))
    {
      NSString *key = name ? name : [event charactersIgnoringModifiers];
      if ([key length] == 0)
        return;
      send_message (@{ @"op": @"key", @"key": key, @"mods": modifiers (flags, name != nil) });
      return;
    }
  [self interpretKeyEvents: @[event]];
}

/* Command keys not in the menu bar's menus are LispM's: Super, in Emacs.  */
- (BOOL) performKeyEquivalent: (NSEvent *) event
{
  if ([[NSApp mainMenu] performKeyEquivalent: event])
    return YES;
  if ([event type] == NSEventTypeKeyDown && ([event modifierFlags] & NSEventModifierFlagCommand))
    {
      [self keyDown: event];
      return YES;
    }
  return NO;
}

/* NSTextInputClient.  */

- (void) insertText: (id) string replacementRange: (NSRange) range
{
  NSString *text = [string isKindOfClass: [NSAttributedString class]] ? [string string] : string;
  [self unmarkText];
  if ([text length] > 0)
    send_message (@{ @"op": @"text", @"text": text });
}

- (void) doCommandBySelector: (SEL) selector
{
  NSEvent *event = [NSApp currentEvent];
  if ([event type] == NSEventTypeKeyDown)
    {
      NSString *name = key_name ([event keyCode]);
      NSString *key = name ? name : [event charactersIgnoringModifiers];
      if ([key length] > 0)
        send_message (@{ @"op": @"key", @"key": key,
                         @"mods": modifiers ([event modifierFlags], name != nil) });
    }
}

- (void) setMarkedText: (id) string selectedRange: (NSRange) selected replacementRange: (NSRange) range
{
  NSAttributedString *text = [string isKindOfClass: [NSAttributedString class]]
    ? string : [[NSAttributedString alloc] initWithString: string];
  self.marked = [text length] > 0 ? text : nil;
  [self showMarked];
}

- (void) unmarkText
{
  self.marked = nil;
  [self showMarked];
}

/* The text being composed, over LispM's cursor, as LispM cannot show it.  */
- (void) showMarked
{
  if (!self.markedLayer)
    {
      self.markedLayer = [CATextLayer layer];
      self.markedLayer.contentsScale = [[self window] backingScaleFactor];
      self.markedLayer.fontSize = 14;
      self.markedLayer.foregroundColor = [[NSColor blackColor] CGColor];
      self.markedLayer.backgroundColor = [[NSColor colorWithCalibratedRed: 1 green: 1 blue: 0.85 alpha: 1] CGColor];
      self.markedLayer.borderColor = [[NSColor grayColor] CGColor];
      self.markedLayer.borderWidth = 1;
      [[self layer] addSublayer: self.markedLayer];
    }
  [CATransaction begin];
  [CATransaction setDisableActions: YES];
  if (self.marked)
    {
      NSFont *font = [NSFont systemFontOfSize: 14];
      NSDictionary *attributes = @{ NSFontAttributeName: font,
                                    NSUnderlineStyleAttributeName: @(NSUnderlineStyleSingle) };
      NSAttributedString *shown = [[NSAttributedString alloc] initWithString: [self.marked string]
                                                                  attributes: attributes];
      NSSize size = [shown size];
      self.markedLayer.string = shown;
      self.markedLayer.frame = CGRectMake (NSMinX (self.cursor), NSMinY (self.cursor),
                                           ceil (size.width) + 4, MAX (ceil (size.height), NSHeight (self.cursor)));
      self.markedLayer.hidden = NO;
    }
  else
    self.markedLayer.hidden = YES;
  [CATransaction commit];
}

- (BOOL) hasMarkedText { return self.marked != nil; }
- (NSRange) markedRange { return self.marked ? NSMakeRange (0, [self.marked length]) : NSMakeRange (NSNotFound, 0); }
- (NSRange) selectedRange { return NSMakeRange (self.marked ? [self.marked length] : 0, 0); }
- (NSArray *) validAttributesForMarkedText { return @[]; }
- (NSAttributedString *) attributedSubstringForProposedRange: (NSRange) range actualRange: (NSRangePointer) actual { return nil; }
- (NSUInteger) characterIndexForPoint: (NSPoint) point { return 0; }

/* Where the input method's candidates go: under LispM's cursor.  */
- (NSRect) firstRectForCharacterRange: (NSRange) range actualRange: (NSRangePointer) actual
{
  NSRect rect = [self convertRect: self.cursor toView: nil];
  return [[self window] convertRectToScreen: rect];
}

@end

/* The application.  */
@interface LispMAppDelegate : NSObject <NSApplicationDelegate, NSWindowDelegate>
@property (strong) LispMAppWindow *window;
@property (strong) LispMAppView *view;
@property (strong) CALayer *host;
@property (strong) NSMutableData *pending;
@property (strong) NSFileHandle *reader;
@property (strong) NSString *name;
@property (strong) NSDictionary *labels;
@property (strong) NSArray *lispmMenus;
@property (strong) NSURL *lispm;
@property BOOL leaving;
@property BOOL terminating;
@end

@implementation LispMAppDelegate

- (id) plist: (NSString *) key
{
  return [[NSBundle mainBundle] objectForInfoDictionaryKey: key];
}

/* The menu bar: the application's own menu, LispM's menus for it after.  */
- (NSString *) label: (NSString *) key or: (NSString *) english
{
  NSString *label = self.labels[key];
  return [label length] > 0 ? label : [NSString stringWithFormat: english, self.name];
}

- (void) setMenus: (NSArray *) menus
{
  self.lispmMenus = menus;
  NSMenu *bar = [[NSMenu alloc] init];
  NSMenuItem *own = [[NSMenuItem alloc] init];
  NSMenu *menu = [[NSMenu alloc] initWithTitle: self.name];
  /* In the language LispM speaks, once it said its words.  */
  [menu addItemWithTitle: [self label: @"about" or: @"About %@"]
                  action: @selector (orderFrontStandardAboutPanel:) keyEquivalent: @""];
  [menu addItem: [NSMenuItem separatorItem]];
  [menu addItemWithTitle: [self label: @"desktop" or: @"Show Desktop"]
                  action: @selector (showDesktop:) keyEquivalent: @""];
  [menu addItem: [NSMenuItem separatorItem]];
  [menu addItemWithTitle: [self label: @"hide" or: @"Hide %@"]
                  action: @selector (hide:) keyEquivalent: @"h"];
  [menu addItemWithTitle: [self label: @"quit" or: @"Quit %@"]
                  action: @selector (terminate:) keyEquivalent: @"q"];
  [own setSubmenu: menu];
  [bar addItem: own];
  for (NSDictionary *each in menus)
    {
      NSMenuItem *item = [[NSMenuItem alloc] initWithTitle: each[@"title"] action: nil keyEquivalent: @""];
      [item setSubmenu: [self menuOf: each[@"items"] titled: each[@"title"]]];
      [bar addItem: item];
    }
  [NSApp setMainMenu: bar];
}

- (NSMenu *) menuOf: (NSArray *) items titled: (NSString *) title
{
  NSMenu *menu = [[NSMenu alloc] initWithTitle: title];
  [menu setAutoenablesItems: NO];
  for (NSDictionary *each in items)
    {
      if ([each[@"separator"] boolValue])
        {
          [menu addItem: [NSMenuItem separatorItem]];
          continue;
        }
      NSString *label = each[@"label"] ?: @"";
      NSString *key = each[@"key"];
      NSString *equivalent = @"";
      /* Super and a character is a Command key; other keys show after the
         label, as Mac OS 9 showed them.  */
      if ([key length] == 3 && [key hasPrefix: @"s-"])
        equivalent = [key substringFromIndex: 2];
      else if ([key length] > 0)
        label = [NSString stringWithFormat: @"%@  (%@)", label, key];
      NSMenuItem *item = [[NSMenuItem alloc] initWithTitle: label
                                                    action: each[@"items"] ? nil : @selector (choose:)
                                             keyEquivalent: equivalent];
      [item setTarget: self];
      [item setTag: [each[@"id"] integerValue]];
      [item setEnabled: each[@"enabled"] ? [each[@"enabled"] boolValue] : YES];
      [item setState: [each[@"checked"] boolValue] ? NSControlStateValueOn : NSControlStateValueOff];
      if (each[@"items"])
        [item setSubmenu: [self menuOf: each[@"items"] titled: label]];
      [menu addItem: item];
    }
  return menu;
}

- (void) choose: (NSMenuItem *) item
{
  send_message (@{ @"op": @"menu", @"id": @([item tag]) });
}

- (void) showDesktop: (id) sender
{
  send_message (@{ @"op": @"desktop" });
}

/* LispM's messages.  */

- (void) handle: (NSDictionary *) message
{
  NSString *op = message[@"op"];
  if ([op isEqualToString: @"window"])
    [self openWindow: message];
  else if ([op isEqualToString: @"geometry"])
    {
      if (self.window)
        [self.window setFrame: rect_from_lispm (message) display: YES];
    }
  else if ([op isEqualToString: @"menus"])
    [self setMenus: message[@"menus"]];
  else if ([op isEqualToString: @"cursor"])
    {
      self.view.cursor = NSMakeRect ([message[@"x"] doubleValue], [message[@"y"] doubleValue],
                                     1, [message[@"h"] doubleValue]);
      [self.view showMarked];
    }
  else if ([op isEqualToString: @"activate"])
    {
      [NSApp activateIgnoringOtherApps: YES];
      [self.window makeKeyAndOrderFront: nil];
    }
  else if ([op isEqualToString: @"drag"])
    {
      /* LispM found the mouse pressed on the title bar: the window moves
         as the computer's do, LispM's windows following once it stops.  */
      if (self.view.lastDown && ([NSEvent pressedMouseButtons] & 1))
        [self.window performWindowDragWithEvent: self.view.lastDown];
    }
  else if ([op isEqualToString: @"desktop"])
    {
      /* LispM shows its desktop: the application is there.  */
      for (NSRunningApplication *running in [NSRunningApplication runningApplicationsWithBundleIdentifier:
                                                                    [[NSBundle bundleWithURL: self.lispm] bundleIdentifier]])
        [running activateWithOptions: NSApplicationActivateAllWindows];
      [self leave];
    }
  else if ([op isEqualToString: @"bye"])
    [self leave];
  else if ([op isEqualToString: @"stay"])
    {
      /* A window was kept open, as an alert asked.  */
      if (self.terminating)
        {
          self.terminating = NO;
          [NSApp replyToApplicationShouldTerminate: NO];
        }
    }
}

- (void) leave
{
  self.leaving = YES;
  if (self.terminating)
    [NSApp replyToApplicationShouldTerminate: YES];
  else
    [NSApp terminate: nil];
}

- (void) openWindow: (NSDictionary *) message
{
  NSRect rect = rect_from_lispm (message);
  if (!self.window)
    {
      self.window = [[LispMAppWindow alloc] initWithContentRect: rect
                                                      styleMask: NSWindowStyleMaskBorderless
                                                        backing: NSBackingStoreBuffered defer: NO];
      [self.window setOpaque: NO];
      [self.window setBackgroundColor: [NSColor clearColor]];
      [self.window setHasShadow: NO];
      [self.window setReleasedWhenClosed: NO];
      [self.window setDelegate: self];
      self.view = [[LispMAppView alloc] initWithFrame: NSMakeRect (0, 0, NSWidth (rect), NSHeight (rect))];
      [self.view setWantsLayer: YES];
      self.host = [NSClassFromString (@"CALayerHost") new];
      self.host.anchorPoint = CGPointZero;
      self.host.frame = CGRectMake (0, 0, NSWidth (rect), NSHeight (rect));
      [[self.view layer] addSublayer: self.host];
      [self.window setContentView: self.view];
      [self.window makeFirstResponder: self.view];
    }
  if ([message[@"labels"] isKindOfClass: [NSDictionary class]])
    {
      self.labels = message[@"labels"];
      [self setMenus: self.lispmMenus];
    }
  [self.host setValue: message[@"context"] forKey: @"contextId"];
  [self.window setTitle: message[@"title"] ?: self.name];
  [self.window setFrame: rect display: YES];
  [NSApp activateIgnoringOtherApps: YES];
  [self.window makeKeyAndOrderFront: nil];
}

- (void) windowDidResize: (NSNotification *) notification
{
  [CATransaction begin];
  [CATransaction setDisableActions: YES];
  self.host.frame = [self.view bounds];
  [CATransaction commit];
}

- (void) windowDidMove: (NSNotification *) notification
{
  NSRect frame = [self.window frame];
  send_message (@{ @"op": @"moved", @"x": @(NSMinX (frame)),
                   @"y": @(main_screen_height () - NSMaxY (frame)) });
}

- (void) windowDidBecomeKey: (NSNotification *) notification
{
  send_message (@{ @"op": @"focus", @"on": @YES });
}

- (void) windowDidResignKey: (NSNotification *) notification
{
  send_message (@{ @"op": @"focus", @"on": @NO });
}

/* Reading LispM.  */

- (void) readFrom: (int) fd
{
  self.pending = [NSMutableData data];
  self.reader = [[NSFileHandle alloc] initWithFileDescriptor: fd closeOnDealloc: NO];
  __weak LispMAppDelegate *weak = self;
  self.reader.readabilityHandler = ^(NSFileHandle *h) {
    NSData *data = [h availableData];
    dispatch_async (dispatch_get_main_queue (), ^{
      LispMAppDelegate *strong = weak;
      if ([data length] == 0)
        {
          h.readabilityHandler = nil;
          /* LispM is gone: so is its application.  */
          [strong leave];
          return;
        }
      [strong.pending appendData: data];
      const char *bytes = [strong.pending bytes];
      NSUInteger start = 0, length = [strong.pending length];
      for (NSUInteger i = 0; i < length; i++)
        if (bytes[i] == '\n')
          {
            NSData *line = [strong.pending subdataWithRange: NSMakeRange (start, i - start)];
            id message = [NSJSONSerialization JSONObjectWithData: line options: 0 error: nil];
            if ([message isKindOfClass: [NSDictionary class]])
              [strong handle: message];
            start = i + 1;
          }
      [strong.pending replaceBytesInRange: NSMakeRange (0, start) withBytes: NULL length: 0];
    });
  };
}

/* Connect to LispM, starting it out of sight first when it does not
   listen; give up after a minute and a half, as a first start installs.  */
static int
connect_to (NSString *path)
{
  struct sockaddr_un address = { .sun_family = AF_UNIX };
  if ([path length] >= sizeof address.sun_path)
    return -1;
  strlcpy (address.sun_path, [path fileSystemRepresentation], sizeof address.sun_path);
  int fd = socket (AF_UNIX, SOCK_STREAM, 0);
  if (fd < 0)
    return -1;
  if (connect (fd, (struct sockaddr *) &address, sizeof address) != 0)
    {
      close (fd);
      return -1;
    }
  return fd;
}

- (void) startLispM
{
  NSWorkspaceOpenConfiguration *configuration = [NSWorkspaceOpenConfiguration configuration];
  configuration.activates = NO;
  configuration.arguments = [self plist: @"LispMArguments"] ?: @[];
  [[NSWorkspace sharedWorkspace] openApplicationAtURL: self.lispm configuration: configuration
                                    completionHandler: nil];
}

- (void) connectAfter: (NSDate *) start started: (BOOL) started
{
  int fd = connect_to ([self plist: @"LispMSocket"]);
  if (fd >= 0)
    {
      lispm_socket = fd;
      [self readFrom: fd];
      send_message (@{ @"op": @"open", @"app": app_key, @"pid": @(getpid ()) });
      return;
    }
  if (!started)
    [self startLispM];
  if (-[start timeIntervalSinceNow] > 90)
    {
      NSAlert *alert = [[NSAlert alloc] init];
      alert.messageText = [NSString stringWithFormat: @"%@ could not open", self.name];
      alert.informativeText = @"LispM did not answer.";
      [alert runModal];
      [NSApp terminate: nil];
      return;
    }
  dispatch_after (dispatch_time (DISPATCH_TIME_NOW, (int64_t) (0.25 * NSEC_PER_SEC)),
                  dispatch_get_main_queue (), ^{ [self connectAfter: start started: YES]; });
}

- (void) applicationDidFinishLaunching: (NSNotification *) notification
{
  self.name = [self plist: @"CFBundleName"] ?: app_key;
  self.lispm = [NSURL fileURLWithPath: [self plist: @"LispMApplication"] ?: @"/Applications/LispM.app"];
  [self setMenus: @[]];
  [self connectAfter: [NSDate date] started: NO];
}

/* The tile in the Dock clicked, or the application chosen in the switcher.  */
- (BOOL) applicationShouldHandleReopen: (NSApplication *) sender hasVisibleWindows: (BOOL) visible
{
  [self.window makeKeyAndOrderFront: nil];
  return NO;
}

/* Quitting asks LispM to close the application's windows; it says bye.  */
- (NSApplicationTerminateReply) applicationShouldTerminate: (NSApplication *) sender
{
  if (self.leaving || lispm_socket < 0)
    return NSTerminateNow;
  self.terminating = YES;
  send_message (@{ @"op": @"quit" });
  return NSTerminateLater;
}

@end

int
main (int argc, const char *argv[])
{
  @autoreleasepool
    {
      app_key = [[NSBundle mainBundle] objectForInfoDictionaryKey: @"LispMApp"];
      if (!app_key)
        {
          fputs ("This is one of LispM's applications, made by LispM.\n", stderr);
          return 1;
        }
      NSApplication *application = [NSApplication sharedApplication];
      [application setActivationPolicy: NSApplicationActivationPolicyRegular];
      LispMAppDelegate *delegate = [LispMAppDelegate new];
      [application setDelegate: delegate];
      [application run];
    }
  return 0;
}
