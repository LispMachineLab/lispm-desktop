/* display.m --- LispM's windows drawn in the processes of applications of their own

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

   An Emacs module.  macOS gives each process one tile in the Dock, one
   menu bar and one place in the application switcher, and every window
   of LispM is its Emacs's.  So each of LispM's applications opened on
   its own has a process of its own, app.m beside this file, owning the
   window the computer shows; LispM draws it, as an X server draws for
   its clients, with the roles turned round.

   Here a window of LispM, a frame and the frames over it (the Mac OS 9
   frame drawn round an application's window is that window's parent,
   menus, alerts and completions are children of it), is exported: its
   layers are shown again in a Core Animation context of their own,
   whose number the application's process hosts with a CALayerHost, as
   WebKit and Chrome host the layers their other processes draw.  The
   windows themselves stay where they are, transparent and passed through
   by the mouse, so Emacs goes on drawing them; the layers show their
   last drawing, put over each other as the windows are.

   CAContext and CALayerHost are not public API.  Where the system lacks
   them, nothing is exported and the application opens as before.  */

#import <AppKit/AppKit.h>
#import <QuartzCore/QuartzCore.h>
#include <dlfcn.h>
#include <objc/runtime.h>
#include "emacs-module.h"

int plugin_is_GPL_compatible;

/* What this module needs of Core Animation's private CAContext.  */
@interface NSObject (LispMDisplayContext)
+ (id) contextWithCGSConnection: (uint32_t) connection options: (NSDictionary *) options;
+ (id) remoteContextWithOptions: (NSDictionary *) options;
- (uint32_t) contextId;
- (void) setLayer: (CALayer *) layer;
- (void) invalidate;
@end

/* The EmacsView in WINDOW, or nil.  */
static NSView *
emacs_view_of_window (NSWindow *window)
{
  Class emacs_view = NSClassFromString (@"EmacsView");
  NSView *content = [window contentView];
  if (!emacs_view || !content)
    return nil;
  if ([content isKindOfClass: emacs_view])
    return content;
  for (NSView *view in [content subviews])
    if ([view isKindOfClass: emacs_view])
      return view;
  return nil;
}

/* The window of the frame whose record is at ADDRESS, as Emacs prints a
   frame: the one whose EmacsView points at it; or nil.  */
static NSWindow *
window_of_frame (uintptr_t address)
{
  Class emacs_view = NSClassFromString (@"EmacsView");
  Ivar ivar = emacs_view ? class_getInstanceVariable (emacs_view, "emacsframe") : NULL;
  if (!ivar)
    return nil;
  ptrdiff_t offset = ivar_getOffset (ivar);
  for (NSWindow *window in [NSApp windows])
    {
      NSView *view = emacs_view_of_window (window);
      if (view && *(uintptr_t *) ((char *) (__bridge void *) view + offset) == address)
        return window;
    }
  return nil;
}

/* WINDOW and the windows over it, its children's children too, those shown,
   parents first: the order they are drawn in.  */
static void
collect_windows (NSWindow *window, NSMutableArray *into)
{
  [into addObject: window];
  for (NSWindow *child in [window childWindows])
    if ([child isVisible])
      collect_windows (child, into);
}

/* One window's layer shown again: the EmacsView's layer it shows the
   drawing of, and its own in the exported tree.  */
@interface LispMDisplayLayer : NSObject
@property (weak) NSWindow *window;
@property (strong) CALayer *source;
@property (strong) CALayer *shown;
@property NSWindowCollectionBehavior behavior;
@end

@implementation LispMDisplayLayer
@end

/* A window exported, its layers shown in a context of their own.  */
@interface LispMDisplayExport : NSObject
@property (weak) NSWindow *window;
@property (strong) id context;
@property (strong) CALayer *root;
@property (strong) NSMutableArray<LispMDisplayLayer *> *layers;
@end

@implementation LispMDisplayExport

/* Show the latest drawing of SOURCE again.  EmacsLayer reuses its
   surfaces, so a surface shown again may be the one shown before: as
   EmacsLayer itself does, the contents are cleared first, which tells
   the window server they changed.  */
- (void) observeValueForKeyPath: (NSString *) path ofObject: (id) source
                         change: (NSDictionary *) change context: (void *) context
{
  for (LispMDisplayLayer *layer in self.layers)
    if (layer.source == source)
      {
        id contents = [source contents];
        if (contents)
          {
            [CATransaction begin];
            [CATransaction setDisableActions: YES];
            layer.shown.contents = nil;
            layer.shown.contents = contents;
            [CATransaction commit];
          }
      }
}

- (void) forget: (LispMDisplayLayer *) layer
{
  @try
    {
      [layer.source removeObserver: self forKeyPath: @"contents"];
    }
  @catch (NSException *exception)
    {
    }
  [layer.shown removeFromSuperlayer];
  NSWindow *window = layer.window;
  if (window)
    {
      [window setAlphaValue: 1.0];
      [window setIgnoresMouseEvents: NO];
      [window setCollectionBehavior: layer.behavior];
    }
}

/* Put the layers shown as the windows are now, the window exported and
   those over it: a window come over it is shown too, and one gone, no
   longer.  The windows are kept transparent, out of the mouse's way.  */
- (void) layout
{
  NSWindow *top = self.window;
  if (!top)
    return;
  NSMutableArray *windows = [NSMutableArray array];
  collect_windows (top, windows);
  NSRect base = [top contentRectForFrameRect: [top frame]];

  [CATransaction begin];
  [CATransaction setDisableActions: YES];
  self.root.frame = CGRectMake (0, 0, NSWidth (base), NSHeight (base));
  for (LispMDisplayLayer *layer in [self.layers copy])
    if (![windows containsObject: layer.window]
        || [emacs_view_of_window (layer.window) layer] != layer.source)
      {
        [self forget: layer];
        [self.layers removeObject: layer];
      }
  NSMutableArray<CALayer *> *order = [NSMutableArray array];
  for (NSWindow *window in windows)
    {
      NSView *view = emacs_view_of_window (window);
      CALayer *source = [view layer];
      if (!source)
        continue;
      LispMDisplayLayer *layer = nil;
      for (LispMDisplayLayer *each in self.layers)
        if (each.window == window)
          layer = each;
      if (!layer)
        {
          layer = [LispMDisplayLayer new];
          layer.window = window;
          layer.source = source;
          layer.shown = [CALayer layer];
          layer.shown.anchorPoint = CGPointZero;
          layer.shown.contentsGravity = kCAGravityTopLeft;
          [self.layers addObject: layer];
          [source addObserver: self forKeyPath: @"contents" options: 0 context: NULL];
          layer.shown.contents = [source contents];
          /* Out of Mission Control and the window cycle, and on every
             Space, wherever the application's window is.  */
          layer.behavior = [window collectionBehavior];
          [window setCollectionBehavior: NSWindowCollectionBehaviorCanJoinAllSpaces
                                         | NSWindowCollectionBehaviorTransient
                                         | NSWindowCollectionBehaviorIgnoresCycle];
        }
      if ([window alphaValue] != 0.0)
        [window setAlphaValue: 0.0];
      if (![window ignoresMouseEvents])
        [window setIgnoresMouseEvents: YES];
      NSRect rect = [view convertRect: [view bounds] toView: nil];
      rect = [window convertRectToScreen: rect];
      layer.shown.contentsScale = source.contentsScale;
      layer.shown.frame = CGRectMake (NSMinX (rect) - NSMinX (base),
                                      NSMaxY (base) - NSMaxY (rect),
                                      NSWidth (rect), NSHeight (rect));
      [order addObject: layer.shown];
    }
  /* Each drawn over the one before, as the windows are ordered.  */
  if (![self.root.sublayers isEqualToArray: order])
    self.root.sublayers = order;
  [CATransaction commit];
}

- (void) end
{
  for (LispMDisplayLayer *layer in self.layers)
    [self forget: layer];
  [self.layers removeAllObjects];
  if ([self.context respondsToSelector: @selector (invalidate)])
    [self.context invalidate];
  self.context = nil;
}

@end

/* The windows exported, by their context's number.  */
static NSMutableDictionary<NSNumber *, LispMDisplayExport *> *exports;
static NSTimer *layout_timer;

/* Lay every window exported out again, many times a second: windows come
   and go over them and move with them, which no notification tells.  An
   export whose window is gone ends.  */
static void
layout_all (void)
{
  for (NSNumber *key in [exports allKeys])
    {
      LispMDisplayExport *export = exports[key];
      if (!export.window)
        {
          [export end];
          [exports removeObjectForKey: key];
        }
      else
        [export layout];
    }
  if ([exports count] == 0)
    {
      [layout_timer invalidate];
      layout_timer = nil;
    }
}

/* A context for layers another process hosts.  */
static id
make_context (void)
{
  Class context_class = NSClassFromString (@"CAContext");
  if (!context_class)
    return nil;
  uint32_t (*main_connection) (void) = dlsym (RTLD_DEFAULT, "CGSMainConnectionID");
  if (main_connection && [context_class respondsToSelector: @selector (contextWithCGSConnection:options:)])
    return [context_class contextWithCGSConnection: main_connection () options: @{}];
  if ([context_class respondsToSelector: @selector (remoteContextWithOptions:)])
    return [context_class remoteContextWithOptions: @{}];
  return nil;
}

/* LispM out of the Dock: Emacs makes itself a regular application again
   with every frame it makes, see Fx_create_frame; while LispM keeps out
   of the Dock, that is let pass without effect, which spares the Dock a
   tile shown and taken away at once.  */

static BOOL keep_accessory;
static IMP original_set_policy;

static BOOL
set_policy (id self, SEL selector, NSApplicationActivationPolicy policy)
{
  if (keep_accessory && policy == NSApplicationActivationPolicyRegular)
    return YES;
  return ((BOOL (*) (id, SEL, NSApplicationActivationPolicy)) original_set_policy) (self, selector, policy);
}

/* The Lisp side.  */

static emacs_value
lisp_nil (emacs_env *env)
{
  return env->intern (env, "nil");
}

static NSWindow *
window_arg (emacs_env *env, emacs_value address)
{
  intmax_t value = env->extract_integer (env, address);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return nil;
  return window_of_frame ((uintptr_t) value);
}

static emacs_value
Fexport (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  NSWindow *window = window_arg (env, args[0]);
  if (!window)
    return lisp_nil (env);
  for (LispMDisplayExport *export in [exports allValues])
    if (export.window == window)
      return env->make_integer (env, [export.context contextId]);
  id context = make_context ();
  if (!context)
    return lisp_nil (env);
  LispMDisplayExport *export = [LispMDisplayExport new];
  export.window = window;
  export.context = context;
  export.layers = [NSMutableArray array];
  export.root = [CALayer layer];
  export.root.geometryFlipped = YES;
  export.root.anchorPoint = CGPointZero;
  [context setLayer: export.root];
  if (!exports)
    exports = [NSMutableDictionary dictionary];
  exports[@([context contextId])] = export;
  [export layout];
  if (!layout_timer)
    {
      layout_timer = [NSTimer timerWithTimeInterval: 1.0 / 60 repeats: YES
                                              block: ^(NSTimer *timer) { layout_all (); }];
      [[NSRunLoop mainRunLoop] addTimer: layout_timer forMode: NSRunLoopCommonModes];
    }
  return env->make_integer (env, [context contextId]);
}

static emacs_value
Funexport (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  intmax_t number = env->extract_integer (env, args[0]);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return lisp_nil (env);
  LispMDisplayExport *export = exports[@(number)];
  if (!export)
    return lisp_nil (env);
  [export end];
  [exports removeObjectForKey: @(number)];
  return env->intern (env, "t");
}

static emacs_value
Ffocus (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  NSWindow *window = window_arg (env, args[0]);
  NSView *view = window ? emacs_view_of_window (window) : nil;
  if (!view)
    return lisp_nil (env);
  /* The window has no keyboard, the application's process has: Emacs is
     told as if the window had it, and draws its cursor so.  */
  if (env->is_not_nil (env, args[1]))
    {
      if ([view respondsToSelector: @selector (windowDidBecomeKey)])
        [view performSelector: @selector (windowDidBecomeKey)];
    }
  else if ([view respondsToSelector: @selector (windowDidResignKey:)])
    [view performSelector: @selector (windowDidResignKey:) withObject: nil];
  return env->intern (env, "t");
}

static emacs_value
Faccessory (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  if (!original_set_policy)
    {
      Method method = class_getInstanceMethod ([NSApplication class], @selector (setActivationPolicy:));
      if (!method)
        return lisp_nil (env);
      original_set_policy = method_setImplementation (method, (IMP) set_policy);
    }
  keep_accessory = env->is_not_nil (env, args[0]);
  ((BOOL (*) (id, SEL, NSApplicationActivationPolicy)) original_set_policy)
    (NSApp, @selector (setActivationPolicy:),
     keep_accessory ? NSApplicationActivationPolicyAccessory : NSApplicationActivationPolicyRegular);
  return env->intern (env, "t");
}

static emacs_value
Favailable (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  return (NSClassFromString (@"CAContext") && NSClassFromString (@"EmacsView"))
    ? env->intern (env, "t") : lisp_nil (env);
}

static void
defun (emacs_env *env, const char *name, ptrdiff_t min, ptrdiff_t max,
       emacs_value (*function) (emacs_env *, ptrdiff_t, emacs_value *, void *),
       const char *doc)
{
  emacs_value args[] = { env->intern (env, name),
                         env->make_function (env, min, max, function, doc, NULL) };
  env->funcall (env, env->intern (env, "defalias"), 2, args);
}

int
emacs_module_init (struct emacs_runtime *runtime)
{
  if (runtime->size < (ptrdiff_t) sizeof *runtime)
    return 1;
  emacs_env *env = runtime->get_environment (runtime);
  defun (env, "lispm-desktop-display-module-export", 1, 1, Fexport,
         "Export the window of the frame whose record is at ADDRESS.\n"
         "Return the number of the context its layers are shown in, for\n"
         "another process to host, or nil.\n\n(fn ADDRESS)");
  defun (env, "lispm-desktop-display-module-unexport", 1, 1, Funexport,
         "End the export whose context is NUMBER; its windows show again.\n\n(fn NUMBER)");
  defun (env, "lispm-desktop-display-module-focus", 2, 2, Ffocus,
         "Tell Emacs the frame whose record is at ADDRESS has the keyboard,\n"
         "when ON is non-nil, or no longer.\n\n(fn ADDRESS ON)");
  defun (env, "lispm-desktop-display-module-accessory", 1, 1, Faccessory,
         "Keep Emacs out of the Dock and the menu bar, when ON is non-nil,\n"
         "whatever Emacs sets meanwhile; or make it a regular application.\n\n(fn ON)");
  defun (env, "lispm-desktop-display-module-available-p", 0, 0, Favailable,
         "Return non-nil when windows can be exported here.\n\n(fn)");
  emacs_value feature[] = { env->intern (env, "lispm-desktop-display-module") };
  env->funcall (env, env->intern (env, "provide"), 1, feature);
  return 0;
}
