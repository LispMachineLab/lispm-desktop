;;; lispm-desktop-browser.el --- The browser, drawn as Netscape Communicator  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-desktop
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The browser is lispm-browser, LispM's WebKit browser, which keeps the
;; tabs, the history and bookmarks, the hints and the Chrome extensions.
;; This file makes it an application on the desktop (see
;; lispm-desktop-apps.el) with its own workspace and menus, and draws its
;; window as Mac OS 9's: title bar, toolbars, address field, tabs and
;; status bar.  Requires an Emacs built with xwidgets.

;;; Code:

(require 'cl-lib)
(require 'delsel)
(require 'format-spec)
(require 'url-parse)
(require 'lispm-browser)
(require 'lispm-browser-places)
(require 'lispm-browser-extensions)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-proxy)
(require 'lispm-desktop-i18n)

;;;; Browser
;;
;; The embedded WebKit browser is laid out as Netscape Communicator's window
;; was on Mac OS 9, its icons drawn in the theme's colours:
;;
;;   address window  title bar     the shown page's icon and title
;;                   header line   navigation toolbar: Back, Forward,
;;                                 Reload, Home, Search, History, Stop,
;;                                 and the Netscape N, its meteors falling
;;                                 while the page loads
;;                   text line     location toolbar: Bookmarks, the page's
;;                                 icon, the address in an edit field (RET),
;;                                 then a button per pinned extension and
;;                                 the Extensions button, its menu of them all
;;                   mode line     personal toolbar: a button per bookmark
;;   page window     tab line      a bevel button per tab, then "+"
;;                   below         the page
;;                   mode line     status bar: the padlock, the progress
;;                                 bar, what the page is doing
;;
;; The address window is a real buffer, so the address is edited in place.
;; A timer keeps it in step with the page while it is not being edited.
;; The History and Bookmarks pane is lispm-desktop-browser-places.el's.
;;
;; The window is drawn in one of two ways, as the editor's is, and its zoom
;; box moves it from one to the other:
;;
;;   full screen   in the main area of a workspace of its own, under the
;;                 menu bar
;;   floating      in a child frame over the desktop, moved by its title
;;                 bar and resized from its grow box or its edges
;;
;; Its pages are buffers, so moving the window only shows them elsewhere.

(declare-function lispm-desktop-show-desktop "lispm-desktop-coherence" (&optional screen))
(declare-function xwidget-at "xwidget" (pos))
(declare-function lispm-browser-set-proxy "lispm-browser-proxy" (url &optional exceptions))
(declare-function xwidget-webkit-uri "xwidget" (xwidget))
(declare-function xwidget-webkit-goto-uri "xwidget" (xwidget uri))
(declare-function xwidget-webkit-browse-url "xwidget" (url &optional new-session))
(declare-function xwidget-webkit-back-forward-list "xwidget.c" (xwidget &optional limit))
(declare-function xwidget-webkit-goto-history "xwidget.c" (xwidget rel-pos))
(declare-function xwidget-webkit-estimated-load-progress "xwidget.c" (xwidget))
(defvar xwidget-webkit-mode-map)
(defvar xwidget-webkit--loading-p)
(declare-function lispm-desktop-platinum-icon "lispm-desktop-platinum-icons" (name))
(defvar lispm-desktop-platinum-title)
(defvar lispm-desktop-platinum-icon)
(defvar lispm-desktop-platinum-box-functions)

(defconst lispm-desktop-browser-workspace-name "Browser"
  "Name of the workspace holding the embedded browser.")

(defconst lispm-desktop-browser-address-buffer-name " *Browser Address*"
  "Name of the address window's buffer; the leading space keeps it unlisted.")

(defun lispm-desktop-browser-workspace-p ()
  "Return non-nil when the browser workspace is selected."
  (and (bound-and-true-p tabspaces-mode)
       (equal (tabspaces--current-tab-name) lispm-desktop-browser-workspace-name)))

(defvar lispm-desktop-browser--frame nil
  "The floating browser's child frame, or nil while the browser is full screen.")

(defvar lispm-desktop-browser-floating nil
  "Non-nil when the browser opens floating over the desktop rather than full
screen.  Its zoom box, and the View menu, switch it.")


(defvar lispm-desktop-browser--navigation nil
  "What the shown tab can do, as the last sync found it:
\(:page P :back B :forward F :loading L).")

(defun lispm-desktop-browser-floating-p ()
  "Return non-nil while the browser floats in a window of its own."
  (frame-live-p lispm-desktop-browser--frame))

(defun lispm-desktop-browser--window ()
  "Return the window showing a browser tab, if any: in the floating browser,
or else in the main area."
  (seq-find (lambda (window)
              (eq (buffer-local-value 'major-mode (window-buffer window))
                  'xwidget-webkit-mode))
            (if (lispm-desktop-browser-floating-p)
                (window-list lispm-desktop-browser--frame 'no-mini)
              (lispm-desktop-main-area--windows))))

(defun lispm-desktop-browser--address-window ()
  "Return the address window: the floating browser's, or this frame's."
  (get-buffer-window lispm-desktop-browser-address-buffer-name
                     (and (lispm-desktop-browser-floating-p) lispm-desktop-browser--frame)))

(defun lispm-desktop-browser-search ()
  "Select the location field for a search: what is typed there that is not
an address is searched for, see `lispm-browser-input-url'."
  (interactive)
  (lispm-desktop-browser-focus-address))

;;;;; Menus

(declare-function lispm-desktop-browser-edit-bookmarks "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser-show-history "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser--bookmark-items "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser--places-shown "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser--personal-toolbar "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))

(defun lispm-desktop-browser--file-items ()
  "Return the items of the browser's File menu."
  (let ((page (and (lispm-desktop-browser--window) t)))
    `((:label ,(lispm-desktop-tr "New Tab") :action lispm-browser-new-tab)
      (:label ,(lispm-desktop-tr "Open Location…") :action lispm-desktop-browser-focus-address
              :key ,(lispm-desktop-browser--key 'lispm-desktop-browser-focus-address))
      (:label ,(lispm-desktop-tr "Close Tab") :action lispm-browser-close-shown-tab :enabled ,page)
      separator
      (:label ,(lispm-desktop-tr "Open in Default Browser") :action lispm-browser-open-external :enabled ,page)
      separator
      (:label ,(lispm-desktop-tr "Quit Browser") :action lispm-browser-quit))))

(defun lispm-desktop-browser--view-items ()
  "Return the items of the browser's View menu: full screen or floating,
then reloading and stopping, as Netscape's View menu had them."
  (let ((nav lispm-desktop-browser--navigation))
    `((:label ,(lispm-desktop-tr "Full Screen") :action lispm-desktop-browser-full-screen
              :checked ,(not (lispm-desktop-browser-floating-p)))
      (:label ,(lispm-desktop-tr "Floating Window") :action lispm-desktop-browser-float
              :checked ,(lispm-desktop-browser-floating-p))
      separator
      (:label ,(lispm-desktop-tr "Reload") :action lispm-browser-reload :enabled ,(plist-get nav :page)
              :key ,(lispm-desktop-browser--key 'lispm-browser-reload))
      (:label ,(lispm-desktop-tr "Stop Loading") :action lispm-browser-stop :enabled ,(plist-get nav :loading)
              :key ,(lispm-desktop-browser--key 'lispm-browser-stop)))))

(defun lispm-desktop-browser--session-items ()
  "Return the shown tab's pages back and forward as Go menu items, the one
shown checked, as Netscape's Go menu listed the window's pages.  Only
WebKitGTK tells them; on macOS there are none."
  (when-let* (((fboundp 'xwidget-webkit-back-forward-list))
              (buffer (lispm-browser-shown-buffer))
              (xwidget (with-current-buffer buffer (xwidget-at (point-min))))
              (pages (ignore-errors (xwidget-webkit-back-forward-list xwidget 8))))
    (pcase-let ((`(,back ,here ,forward) pages))
      (mapcar (pcase-lambda (`(,index ,title ,uri))
                (list :label (truncate-string-to-width
                              (if (string-empty-p (or title "")) (or uri "") title)
                              48 nil nil "…")
                      :action (lambda ()
                                (xwidget-webkit-goto-history xwidget index)
                                (lispm-desktop-browser--sync t))
                      :checked (= index 0)))
              (append (reverse forward) (list here) back)))))

(defun lispm-desktop-browser--go-items ()
  "Return the items of the browser's Go menu: Back, Forward and Home, then
the shown tab's pages."
  (let* ((nav lispm-desktop-browser--navigation)
         (pages (lispm-desktop-browser--session-items)))
    `((:label ,(lispm-desktop-tr "Back") :action lispm-browser-back :enabled ,(plist-get nav :back)
              :key ,(lispm-desktop-browser--key 'lispm-browser-back))
      (:label ,(lispm-desktop-tr "Forward") :action lispm-browser-forward :enabled ,(plist-get nav :forward)
              :key ,(lispm-desktop-browser--key 'lispm-browser-forward))
      (:label ,(lispm-desktop-tr-in "web page" "Home") :action lispm-browser-home :enabled ,(plist-get nav :page))
      ,@(when pages (cons 'separator pages)))))

(defun lispm-desktop-browser--window-items ()
  "Return the items of the browser's Window menu: History, then a tab each,
the shown one checked, as Mac OS 9's Window menus listed an application's
windows."
  (let ((shown (lispm-browser-shown-buffer)))
    `((:label ,(lispm-desktop-tr "History") :action lispm-desktop-browser-show-history
              :checked ,(eq (lispm-desktop-browser--places-shown) 'history)
              :key ,(lispm-desktop-browser--key 'lispm-desktop-browser-show-history))
      separator
      ,@(mapcar (lambda (buffer)
                  (list :label (truncate-string-to-width (lispm-browser-title buffer) 48 nil nil "…")
                        :action (lambda () (lispm-browser-select-tab buffer))
                        :checked (eq buffer shown)))
                (lispm-browser-tabs)))))

(defun lispm-desktop-browser-menus ()
  "Return the browser's menus, for the menu bar while it is in front.
Extensions is among them where pages can run Chrome extensions."
  `((lispm-desktop-browser-file ,(lispm-desktop-tr "File") lispm-desktop-browser--file-items)
    (lispm-desktop-browser-view ,(lispm-desktop-tr "View") lispm-desktop-browser--view-items)
    (lispm-desktop-browser-go ,(lispm-desktop-tr "Go") lispm-desktop-browser--go-items)
    (lispm-desktop-browser-bookmarks ,(lispm-desktop-tr "Bookmarks") lispm-desktop-browser--bookmark-items)
    ,@(when (and (fboundp 'lispm-browser-extensions-supported-p)
                 (lispm-browser-extensions-supported-p))
        `((lispm-desktop-browser-extensions-menu ,(lispm-desktop-tr "Extensions")
                                                 lispm-browser-extensions-menu-items)))
    (lispm-desktop-browser-window ,(lispm-desktop-tr "Window") lispm-desktop-browser--window-items)))

;;;;; Drawing Netscape's chrome
;;
;; Each part is an SVG drawn once per palette and state, then kept.

(defvar lispm-desktop-platinum-generation)

(defvar lispm-desktop-browser--images (make-hash-table :test #'equal)
  "Drawn chrome images, by palette generation and key.")

(defun lispm-desktop-browser--image (key draw)
  "Return the image of KEY, calling DRAW for its SVG text the first time."
  (let ((key (cons lispm-desktop-platinum-generation key)))
    (or (gethash key lispm-desktop-browser--images)
        (progn
          (when (> (hash-table-count lispm-desktop-browser--images) 256)
            (clrhash lispm-desktop-browser--images))
          (puthash key (create-image (funcall draw) 'svg t :ascent 'center :scale 1)
                   lispm-desktop-browser--images)))))

(defun lispm-desktop-browser--svg (width height &rest parts)
  "Return an SVG document WIDTH by HEIGHT pixels holding PARTS."
  (concat (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
                  width height width height)
          (apply #'concat parts)
          "</svg>"))

(defun lispm-desktop-browser--escape (text)
  "Return TEXT safe inside SVG."
  (replace-regexp-in-string
   "[&<>\"]" (lambda (c) (pcase c ("&" "&amp;") ("<" "&lt;") (">" "&gt;") (_ "&quot;")))
   text t t))

(defun lispm-desktop-browser--font ()
  "Return the toolbar labels' font as (FAMILY SIZE), the size in pixels:
a size under the list font's, as Netscape's labels were small."
  (let* ((font (ignore-errors (face-font 'lispm-desktop-platinum-list)))
         (info (and font (ignore-errors (font-info font))))
         (family (face-attribute 'lispm-desktop-platinum-list :family nil t)))
    (list (if (stringp family) family "Geneva")
          (max 10 (- (if info (aref info 2) 12) 2)))))

(defun lispm-desktop-browser--label-width (label)
  "Return the width in pixels of toolbar LABEL."
  (pcase-let ((`(,family ,size) (lispm-desktop-browser--font)))
    (string-pixel-width (propertize label 'face `(:family ,family :height ,(* 10 size))))))

(defun lispm-desktop-browser--ink (enabled)
  "Return the icons' colours as an alist for `format-spec': outline (i),
fill (f), face (c), the stop sign's red (r) and white (w); greyed unless
ENABLED."
  (let ((red (or (ignore-errors (face-foreground 'error nil t)) "#CC3333")))
    (if enabled
        `((?i . ,(lispm-desktop-platinum-color :text)) (?f . ,(lispm-desktop-platinum-color :accent))
          (?c . ,(lispm-desktop-platinum-color :content)) (?r . ,red) (?w . "#FFFFFF"))
      `((?i . ,(lispm-desktop-platinum-color :shade)) (?f . ,(lispm-desktop-platinum-color :muted))
        (?c . ,(lispm-desktop-platinum-color :chrome)) (?r . ,(lispm-desktop-platinum-color :muted))
        (?w . ,(lispm-desktop-platinum-color :chrome))))))

(defconst lispm-desktop-browser--icons
  '((back . "<path d='M2.5 12 L11.5 3.5 L11.5 8 L21.5 8 L21.5 16 L11.5 16 L11.5 20.5 Z' \
fill='%f' stroke='%i' stroke-linejoin='round'/>")
    (forward . "<path d='M21.5 12 L12.5 3.5 L12.5 8 L2.5 8 L2.5 16 L12.5 16 L12.5 20.5 Z' \
fill='%f' stroke='%i' stroke-linejoin='round'/>")
    (reload . "<path d='M17.6 6.6 A7.6 7.6 0 1 0 19.6 12.6' fill='none' stroke='%i' \
stroke-width='4.6'/><path d='M17.6 6.6 A7.6 7.6 0 1 0 19.6 12.6' fill='none' stroke='%f' \
stroke-width='2.6'/><path d='M13.2 6.8 L21 2.4 L21.2 11 Z' fill='%f' stroke='%i' \
stroke-linejoin='round'/>")
    (home . "<rect x='15.5' y='4.5' width='3' height='5' fill='%f' stroke='%i'/>\
<path d='M2 12.5 L12 3 L22 12.5 Z' fill='%f' stroke='%i' stroke-linejoin='round'/>\
<rect x='5.5' y='12.5' width='13' height='8' fill='%c' stroke='%i'/>\
<rect x='10.5' y='15' width='3' height='5.5' fill='%f' stroke='%i'/>")
    (search . "<path d='M14.6 14.6 L20.4 20.4' stroke='%i' stroke-width='4' \
stroke-linecap='round'/><circle cx='10' cy='10' r='6.2' fill='%c' stroke='%i' \
stroke-width='2.4'/><path d='M6.6 10 A3.4 3.4 0 0 1 10 6.6' fill='none' stroke='%f' \
stroke-width='1.6'/>")
    (history . "<circle cx='12' cy='12' r='9' fill='%c' stroke='%i' stroke-width='1.6'/>\
<path d='M12 3.8 L12 5.8 M20.2 12 L18.2 12 M12 20.2 L12 18.2 M3.8 12 L5.8 12' stroke='%i' \
stroke-width='1.4'/><path d='M12 12 L12 6.8 M12 12 L15.8 14.2' stroke='%f' stroke-width='2.2' \
stroke-linecap='round'/><circle cx='12' cy='12' r='1.3' fill='%i'/>")
    (stop . "<path d='M8 2.5 L16 2.5 L21.5 8 L21.5 16 L16 21.5 L8 21.5 L2.5 16 L2.5 8 Z' \
fill='%r' stroke='%i' stroke-linejoin='round'/><path d='M8.8 8.8 L15.2 15.2 M15.2 8.8 \
L8.8 15.2' stroke='%w' stroke-width='2.6' stroke-linecap='round'/>")
    (bookmarks . "<path d='M5.5 2.5 L18.5 2.5 L18.5 21.5 L12 16.5 L5.5 21.5 Z' fill='%f' \
stroke='%i' stroke-linejoin='round'/><path d='M8.5 6.5 L15.5 6.5' stroke='%c' \
stroke-width='1.5'/>")
    (extensions . "<path d='M4.5 8 H8.4 A2.6 2.6 0 1 1 13 8 H17 V11.9 A2.6 2.6 0 1 1 17 16.5 \
V20.5 H4.5 Z' fill='%f' stroke='%i' stroke-linejoin='round'/>"))
  "The toolbar's icons, each SVG drawn on a 24-pixel square; see
`lispm-desktop-browser--ink' for the colours named in them.")

(defconst lispm-desktop-browser-tool-height 46
  "Height in pixels of a navigation toolbar button.")

(defun lispm-desktop-browser--bevel (width height pressed)
  "Return SVG of a WIDTH by HEIGHT bevel, pushed in when PRESSED."
  (let ((light (lispm-desktop-platinum-color :light))
        (shade (lispm-desktop-platinum-color :shade))
        (chrome (lispm-desktop-platinum-color :chrome)))
    (concat
     (format "<rect width='%d' height='%d' fill='%s'/>" width height
             (if pressed (lispm-desktop-platinum-blend chrome shade 0.45) chrome))
     (format "<rect width='%d' height='1' fill='%s'/><rect width='1' height='%d' fill='%s'/>"
             width (if pressed shade light) height (if pressed shade light))
     (format "<rect y='%d' width='%d' height='1' fill='%s'/><rect x='%d' width='1' height='%d' fill='%s'/>"
             (1- height) width (if pressed light shade) (1- width) height (if pressed light shade)))))

(defun lispm-desktop-browser--draw-tool (icon label enabled pressed width)
  "Return SVG of the toolbar button ICON named LABEL, WIDTH pixels wide:
greyed unless ENABLED, pushed in when PRESSED."
  (pcase-let ((`(,family ,size) (lispm-desktop-browser--font))
              (ink (lispm-desktop-browser--ink enabled))
              (height lispm-desktop-browser-tool-height))
    (lispm-desktop-browser--svg
     width height
     (lispm-desktop-browser--bevel width height pressed)
     (format "<g transform='translate(%d %d)'>%s</g>" (/ (- width 24) 2) (if pressed 6 5)
             (format-spec (alist-get icon lispm-desktop-browser--icons) ink))
     (format "<text x='%d' y='%d' font-family='%s' font-size='%d' text-anchor='middle' fill='%s'>%s</text>"
             (/ width 2) (- height 6 (if pressed -1 0)) (lispm-desktop-browser--escape family) size
             (if enabled (lispm-desktop-platinum-color :text) (lispm-desktop-platinum-color :shade))
             (lispm-desktop-browser--escape label)))))

(defconst lispm-desktop-browser--logo
  "<defs><linearGradient id='ns-sky' x1='0' y1='0' x2='0' y2='1'>\
<stop offset='0' stop-color='#03032A'/><stop offset='1' stop-color='#22449C'/></linearGradient>\
<linearGradient id='ns-planet' x1='0' y1='0' x2='0' y2='1'><stop offset='0' \
stop-color='#46C2C2'/><stop offset='1' stop-color='#083650'/></linearGradient>\
<linearGradient id='ns-n' x1='0' y1='0' x2='1' y2='1'><stop offset='0' stop-color='#D8FFFF'/>\
<stop offset='0.55' stop-color='#5CC6D8'/><stop offset='1' stop-color='#1A6E96'/>\
</linearGradient></defs><rect width='40' height='40' fill='url(#ns-sky)'/><g fill='#FFFFFF'>\
<rect x='3' y='4' width='1' height='1'/><rect x='8' y='2' width='1' height='1'/>\
<rect x='19' y='3' width='1' height='1' opacity='0.7'/><rect x='36' y='9' width='1' height='1'/>\
<rect x='5' y='20' width='1' height='1' opacity='0.6'/><rect x='35' y='22' width='1' \
height='1' opacity='0.8'/><rect x='21' y='16' width='1' height='1' opacity='0.5'/>\
<rect x='2' y='12' width='1' height='1' opacity='0.8'/></g>%s<ellipse cx='20' cy='54' \
rx='36' ry='22' fill='url(#ns-planet)'/><path d='M6 6 L16 6 L26 21 L26 9 L23 9 L23 6 L34 6 \
L34 9 L31 9 L31 34 L26 34 L14 16 L14 31 L17 31 L17 34 L6 34 L6 31 L9 31 L9 9 L6 9 Z' \
fill='url(#ns-n)' stroke='#00142E' stroke-width='1.1'/>"
  "The Netscape N over its planet, on a 40-pixel square; %s takes the
meteors, drawn behind the planet.")

(defconst lispm-desktop-browser-throbber-frames 10
  "Frames of the throbber's meteor shower, one every tick while loading.")

(defun lispm-desktop-browser--meteor (x y length opacity)
  "Return SVG of a meteor at X, Y falling down to the left, its tail LENGTH
long, at OPACITY."
  (format "<g opacity='%.2f'><path d='M%.1f %.1f L%.1f %.1f L%.1f %.1f Z' fill='#FFFFFF' \
opacity='0.55'/><circle cx='%.1f' cy='%.1f' r='1.1' fill='#FFFFFF'/></g>"
          opacity x (- y 0.7) (+ x length) (- y (* 0.55 length)) x (+ y 0.7) x y))

(defun lispm-desktop-browser--meteors (frame)
  "Return SVG of the meteors in FRAME of the shower, or of the still sky
when FRAME is nil."
  (if (null frame)
      (lispm-desktop-browser--meteor 29.5 3.5 7 1)
    (mapconcat
     (lambda (start)
       (let ((step (mod (+ frame start) lispm-desktop-browser-throbber-frames)))
         (lispm-desktop-browser--meteor (- 40 (* 3.6 step)) (+ 1 (* 2.4 step)) 8
                                        (- 1 (/ (float step) lispm-desktop-browser-throbber-frames)))))
     '(0 4 7) "")))

(defun lispm-desktop-browser--draw-throbber (frame pressed)
  "Return SVG of the throbber: the Netscape N in a bevel, FRAME of its
meteor shower falling (nil while the page rests), pushed in when PRESSED."
  (let ((size lispm-desktop-browser-tool-height))
    (lispm-desktop-browser--svg
     size size
     (lispm-desktop-browser--bevel size size pressed)
     (format "<svg x='4' y='4' width='%d' height='%d' viewBox='0 0 40 40'>%s</svg>"
             (- size 8) (- size 8) (format lispm-desktop-browser--logo (lispm-desktop-browser--meteors frame))))))

(defun lispm-desktop-browser--draw-padlock (secure)
  "Return SVG of the status bar's padlock: shut and in the accent colour
when SECURE, open and grey when not."
  (let ((ink (lispm-desktop-platinum-color (if secure :text :shade)))
        (fill (lispm-desktop-platinum-color (if secure :accent :muted))))
    (lispm-desktop-browser--svg
     12 14
     (format "<path d='%s' fill='none' stroke='%s' stroke-width='1.6'/>"
             (if secure "M3.5 7 L3.5 4.5 A2.5 2.5 0 0 1 8.5 4.5 L8.5 7"
               "M3.5 7 L3.5 4.5 A2.5 2.5 0 0 1 8.5 4.5 L8.5 5")
             ink)
     (format "<rect x='1.5' y='6.5' width='9' height='7' fill='%s' stroke='%s'/>" fill ink))))

(defun lispm-desktop-browser--draw-progress (fraction)
  "Return SVG of the status bar's progress bar, FRACTION full."
  (let ((filled (round (* (min 1 (max 0 fraction)) 78))))
    (lispm-desktop-browser--svg
     80 10
     (format "<rect width='80' height='10' fill='%s'/>" (lispm-desktop-platinum-color :frame))
     (format "<rect x='1' y='1' width='78' height='8' fill='%s'/>" (lispm-desktop-platinum-color :content))
     (if (> filled 0)
         (format "<rect x='1' y='1' width='%d' height='8' fill='%s'/>" filled
                 (lispm-desktop-platinum-color :accent))
       ""))))

;;;;; Navigation toolbar

(defvar lispm-desktop-browser--throbber-frame nil
  "The throbber's frame while the shown page loads, or nil.")

(defvar lispm-desktop-browser--throbber-timer nil
  "Timer turning the throbber while the shown page loads.")

(defconst lispm-desktop-browser-tools
  `((back ,(lispm-desktop-tr-noop "Back") lispm-browser-back
          ,(lispm-desktop-tr-noop "Go back a page") :back)
    (forward ,(lispm-desktop-tr-noop "Forward") lispm-browser-forward
             ,(lispm-desktop-tr-noop "Go forward a page") :forward)
    (reload ,(lispm-desktop-tr-noop "Reload") lispm-browser-reload
            ,(lispm-desktop-tr-noop "Load the page again") :page)
    (home ,(lispm-desktop-tr-noop "Home" "web page") lispm-browser-home
          ,(lispm-desktop-tr-noop "Go to the home page") :page)
    (search ,(lispm-desktop-tr-noop "Search") lispm-desktop-browser-search
            ,(lispm-desktop-tr-noop "Type a search in the location field") t)
    (history ,(lispm-desktop-tr-noop "History") lispm-desktop-browser-show-history
             ,(lispm-desktop-tr-noop "List the pages visited") t)
    (stop ,(lispm-desktop-tr-noop "Stop") lispm-browser-stop
          ,(lispm-desktop-tr-noop "Stop loading the page") :loading))
  "The navigation toolbar's buttons: (ICON LABEL COMMAND HELP ENABLED),
LABEL and HELP in English, translated where shown.  ENABLED is t, or a
key of `lispm-desktop-browser--navigation' saying when.")

(defun lispm-desktop-browser--press-map (command row)
  "Return a keymap pressing a button on ROW that runs COMMAND, see
`lispm-desktop-platinum-press'."
  (let ((map (make-sparse-keymap)))
    (keymap-set map (format "<%s> <down-mouse-1>" row)
                (lambda (event)
                  (interactive "e")
                  (let ((start (event-start event)))
                    (lispm-desktop-platinum-press start (lispm-desktop-platinum-same-target-p start)
                                                  (lambda ()
                                                    (with-selected-window (posn-window start)
                                                      (call-interactively command)))))))
    (keymap-set map (format "<%s> <mouse-1>" row) #'ignore)
    map))

(defun lispm-desktop-browser--tool-button (tool)
  "Return navigation toolbar button TOOL, an entry of the toolbar's tools.
See `lispm-desktop-browser-tools'."
  (pcase-let* ((`(,icon ,english ,command ,help ,condition) tool)
               (label (lispm-desktop-tr english))
               (enabled (or (eq condition t) (plist-get lispm-desktop-browser--navigation condition)))
               (target (cons 'header-line icon))
               (pressed (and enabled (lispm-desktop-platinum-pressed-p target)))
               (width (max 46 (+ 14 (lispm-desktop-browser--label-width label)))))
    (propertize " " 'display (lispm-desktop-browser--image
                              (list 'tool icon label enabled pressed width)
                              (lambda () (lispm-desktop-browser--draw-tool icon label enabled pressed width)))
                'help-echo (lispm-desktop-tr help) 'pointer 'arrow 'lispm-desktop-platinum-target target
                'keymap (and enabled (lispm-desktop-browser--press-map command 'header-line)))))

(defun lispm-desktop-browser--throbber ()
  "Return the throbber, which goes home when clicked, as Netscape's N went
to Netscape's own page."
  (let* ((target '(header-line . throbber))
         (pressed (lispm-desktop-platinum-pressed-p target))
         (frame lispm-desktop-browser--throbber-frame))
    (propertize " " 'display (lispm-desktop-browser--image
                              (list 'throbber frame pressed)
                              (lambda () (lispm-desktop-browser--draw-throbber frame pressed)))
                'help-echo (lispm-desktop-tr "Go to the home page") 'pointer 'arrow
                'lispm-desktop-platinum-target target
                'keymap (lispm-desktop-browser--press-map #'lispm-browser-home 'header-line))))

(defun lispm-desktop-browser--toolbar ()
  "Return the navigation toolbar, the address window's header line: big
buttons, an icon over each name, and the throbber at the right end."
  (concat (lispm-desktop-platinum-space 6)
          (mapconcat #'lispm-desktop-browser--tool-button lispm-desktop-browser-tools (lispm-desktop-platinum-space 2))
          (propertize " " 'display `(space :align-to (- right (,(+ lispm-desktop-browser-tool-height 10)))))
          (lispm-desktop-browser--throbber)))

(defun lispm-desktop-browser--turn-throbber ()
  "Turn the throbber one frame while the shown page loads; stop it, still,
once the page rests."
  (let ((loading (when-let* ((buffer (lispm-browser-shown-buffer)))
                   (buffer-local-value 'xwidget-webkit--loading-p buffer))))
    (if loading
        (setq lispm-desktop-browser--throbber-frame
              (mod (1+ (or lispm-desktop-browser--throbber-frame -1)) lispm-desktop-browser-throbber-frames))
      (setq lispm-desktop-browser--throbber-frame nil)
      (when lispm-desktop-browser--throbber-timer
        (cancel-timer lispm-desktop-browser--throbber-timer)
        (setq lispm-desktop-browser--throbber-timer nil)))
    (when-let* ((buffer (get-buffer lispm-desktop-browser-address-buffer-name)))
      (with-current-buffer buffer
        (force-mode-line-update)))))

;;;;; Status bar

(defun lispm-desktop-browser--status ()
  "Return the status bar of the page being drawn: the padlock, then the
progress bar and what is loading, or Netscape's \"Document: Done\", or
the page's `lispm-browser-status-note'."
  (let* ((xwidget (xwidget-at (point-min)))
         (uri (and xwidget (xwidget-webkit-uri xwidget)))
         (secure (and uri (string-prefix-p "https:" uri)))
         (host (and uri (url-host (url-generic-parse-url uri)))))
    (concat (propertize " " 'display (lispm-desktop-browser--image
                                      (list 'padlock secure)
                                      (lambda () (lispm-desktop-browser--draw-padlock secure))))
            " "
            (if (and xwidget xwidget-webkit--loading-p)
                (let ((progress (or (ignore-errors (xwidget-webkit-estimated-load-progress xwidget))
                                    0)))
                  (concat (propertize " " 'display (lispm-desktop-browser--image
                                                    (list 'progress (round (* 20 progress)))
                                                    (lambda () (lispm-desktop-browser--draw-progress progress))))
                          " "
                          (if (and host (not (string-empty-p host)))
                              (lispm-desktop-tr "Loading %s…" host)
                            (lispm-desktop-tr "Loading…"))))
              (or lispm-browser-status-note (lispm-desktop-tr "Document: Done"))))))

;;;;; Extension buttons
;;
;; The location toolbar ends, as Chrome's toolbar does, with a button per
;; extension pinned to it, then the Extensions button, a puzzle piece,
;; whose menu holds them all; lispm-browser keeps them and runs them.  Each
;; is a small bevel button: the extension's own icon, greyed when it does
;; nothing on the page, its badge at the lower right, and pushed in while
;; its popup, hanging under it, is up.

(declare-function lispm-browser-extensions-buttons "lispm-browser-extensions" ())
(declare-function lispm-browser-extensions-button-menu-items "lispm-browser-extensions" (id))
(declare-function lispm-browser-extensions-button-anchor "lispm-browser-extensions" (window top height id))
(declare-function lispm-browser-extensions-event-anchor "lispm-browser-extensions" (event))
(defvar lispm-browser-extensions-anchor-function)
(defvar lispm-desktop-popup-parent)

(defconst lispm-desktop-browser-extension-size 24
  "Size in pixels of an extension's button on the location toolbar.")

(defconst lispm-desktop-browser-extension-gap 2
  "Pixels between the location toolbar's extension buttons.")

(defun lispm-desktop-browser--draw-badge (text color text-color)
  "Return SVG of an extension button's badge TEXT, in COLOR with its text in
TEXT-COLOR, at the button's lower right, as Chrome draws badges: four
characters at most."
  (let* ((text (truncate-string-to-width text 4))
         (size lispm-desktop-browser-extension-size)
         (width (max 10 (+ 4 (* 5 (string-width text)))))
         (family (car (lispm-desktop-browser--font))))
    (format "<rect x='%d' y='%d' width='%d' height='10' rx='2' fill='%s'/>\
<text x='%d' y='%d' font-family='%s' font-size='8' font-weight='bold' text-anchor='middle' fill='%s'>%s</text>"
            (- size width 1) (- size 11) width (lispm-desktop-browser--escape color)
            (- size 1 (/ width 2)) (- size 3) (lispm-desktop-browser--escape family)
            (lispm-desktop-browser--escape text-color) (lispm-desktop-browser--escape text))))

(defun lispm-desktop-browser--draw-extension (button)
  "Return SVG of extension BUTTON's toolbar button, see
`lispm-browser-extensions-buttons'."
  (let* ((size lispm-desktop-browser-extension-size)
         (icon (plist-get button :icon))
         (data (and icon (image-property icon :data)))
         (enabled (plist-get button :enabled))
         (pressed (plist-get button :open))
         (inset (+ (/ (- size 16) 2) (if pressed 1 0)))
         (badge (plist-get button :badge)))
    (concat
     (format "<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' \
width='%d' height='%d' viewBox='0 0 %d %d'>" size size size size)
     (lispm-desktop-browser--bevel size size pressed)
     (if data
         (format "<image x='%d' y='%d' width='16' height='16' opacity='%s' xlink:href='data:image/png;base64,%s'/>"
                 inset inset (if enabled "1" "0.45") (base64-encode-string data t))
       ;; No icon: the initial of its name, as Chrome shows.
       (format "<text x='%d' y='%d' font-family='%s' font-size='13' font-weight='bold' text-anchor='middle' fill='%s'>%s</text>"
               (/ size 2) (+ inset 13) (lispm-desktop-browser--escape (car (lispm-desktop-browser--font)))
               (lispm-desktop-platinum-color (if enabled :text :shade))
               (lispm-desktop-browser--escape (upcase (substring (or (plist-get button :name) "?") 0 1)))))
     (if badge
         (lispm-desktop-browser--draw-badge badge (plist-get button :badge-color) (plist-get button :badge-text-color))
       "")
     "</svg>")))

(defun lispm-desktop-browser--draw-puzzle (pressed)
  "Return SVG of the Extensions button, the puzzle piece in a bevel button,
pushed in when PRESSED."
  (let ((size lispm-desktop-browser-extension-size))
    (lispm-desktop-browser--svg
     size size
     (lispm-desktop-browser--bevel size size pressed)
     (format "<g transform='translate(%d %d) scale(0.75)'>%s</g>" (+ 3 (if pressed 1 0)) (+ 3 (if pressed 1 0))
             (format-spec (alist-get 'extensions lispm-desktop-browser--icons) (lispm-desktop-browser--ink t))))))

(defun lispm-desktop-browser--popup-at (items anchor)
  "Pop up ITEMS as a Mac OS 9 menu under ANCHOR, a button's rectangle in
its frame, see `lispm-browser-extensions-anchor-function': flush with its
right edge, as the buttons end the toolbar."
  (pcase-let ((`(,frame ,x ,y ,width ,height) anchor))
    (let ((lispm-desktop-popup-parent frame))
      (lispm-desktop-popup-menu items (+ x width) (+ y height) 'right))))

(defun lispm-desktop-browser-extensions-menu (&optional event)
  "Pop up the Extensions menu under the location toolbar's Extensions
button, or at mouse EVENT."
  (interactive (list (and (mouse-event-p last-nonmenu-event) last-nonmenu-event)))
  (let ((anchor (or (and event (lispm-browser-extensions-event-anchor event))
                    (lispm-desktop-browser--extension-anchor 'menu))))
    (if anchor
        (lispm-desktop-browser--popup-at (lispm-browser-extensions-menu-items) anchor)
      (lispm-desktop-popup-menu (lispm-browser-extensions-menu-items)))))

(defun lispm-desktop-browser--extension-menu (id event)
  "Pop up the menu of extension ID's button, clicked with EVENT."
  (when-let* ((anchor (lispm-browser-extensions-event-anchor event)))
    (lispm-desktop-browser--popup-at (lispm-browser-extensions-button-menu-items id) anchor)))

(defun lispm-desktop-browser--extension-map (id)
  "Return the keymap of extension ID's toolbar button: a click presses it,
its popup hanging under it; a right or Control click shows its menu."
  (let ((map (make-sparse-keymap))
        (menu (lambda (event) (interactive "e") (lispm-desktop-browser--extension-menu id event))))
    (keymap-set map "<down-mouse-1>" #'ignore)
    (keymap-set map "<mouse-1>" (lambda (event)
                                  (interactive "e")
                                  (lispm-browser-extensions-press id (lispm-browser-extensions-event-anchor event))))
    (keymap-set map "<down-mouse-3>" menu)
    (keymap-set map "<mouse-3>" #'ignore)
    (keymap-set map "C-<down-mouse-1>" menu)
    (keymap-set map "C-<mouse-1>" #'ignore)
    map))

(defvar lispm-desktop-browser-extensions-button-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<down-mouse-1>" #'lispm-desktop-browser-extensions-menu)
    (keymap-set map "<mouse-1>" #'ignore)
    map)
  "Keymap of the location toolbar's Extensions button.")

(defun lispm-desktop-browser--extension-button (button)
  "Return extension BUTTON for the location toolbar."
  (let ((id (plist-get button :id)))
    (propertize " " 'display (lispm-desktop-browser--image
                              (list 'extension id (plist-get button :name)
                                    (let ((icon (plist-get button :icon))) (and icon (image-property icon :data)))
                                    (plist-get button :badge) (plist-get button :badge-color)
                                    (plist-get button :badge-text-color) (plist-get button :enabled)
                                    (plist-get button :open))
                              (lambda () (lispm-desktop-browser--draw-extension button)))
                'lispm-browser-extensions-button id 'pointer 'arrow
                'help-echo (plist-get button :name)
                'keymap (lispm-desktop-browser--extension-map id))))

(defun lispm-desktop-browser--extension-buttons ()
  "Return the location toolbar's extension buttons and its Extensions
button, as (STRING . WIDTH), WIDTH in pixels; nil where pages run no
extensions, or nothing is drawn."
  (when (and (display-graphic-p)
             (fboundp 'lispm-browser-extensions-supported-p)
             (lispm-browser-extensions-supported-p))
    (let* ((buttons (lispm-browser-extensions-buttons))
           (gap (lispm-desktop-platinum-space lispm-desktop-browser-extension-gap))
           (size lispm-desktop-browser-extension-size))
      (cons (concat (mapconcat (lambda (button) (concat (lispm-desktop-browser--extension-button button) gap))
                               buttons "")
                    (propertize " " 'display (lispm-desktop-browser--image
                                              '(puzzle) (lambda () (lispm-desktop-browser--draw-puzzle nil)))
                                'lispm-browser-extensions-button 'menu 'pointer 'arrow
                                'help-echo (lispm-desktop-tr "Extensions: press one, pin them, manage them")
                                'keymap lispm-desktop-browser-extensions-button-map))
            (+ (* (length buttons) (+ size lispm-desktop-browser-extension-gap)) size)))))

(defun lispm-desktop-browser--extension-anchor (id)
  "Return where the location toolbar's button of extension ID is, or the
Extensions button's for `menu', for its popup to hang from.  This is
`lispm-browser-extensions-anchor-function' on the desktop."
  (when-let* ((window (lispm-desktop-browser--address-window)))
    (pcase-let ((`(,_ ,top . ,_) (window-pixel-edges window))
                (`(,_ ,text-top . ,_) (window-inside-pixel-edges window)))
      (lispm-browser-extensions-button-anchor
       window (+ (- text-top top) (/ (- lispm-desktop-browser-address-height lispm-desktop-browser-extension-size) 2))
       lispm-desktop-browser-extension-size id))))

(defun lispm-desktop-browser--redraw-extensions ()
  "Redraw the location toolbar's extension buttons, leaving the address
alone.  This is on `lispm-browser-extensions-changed-hook'."
  (when-let* ((buffer (get-buffer lispm-desktop-browser-address-buffer-name)))
    (with-current-buffer buffer
      (lispm-desktop-browser--draw-field)
      (force-mode-line-update))))

;;;;; Address window

(defvar-local lispm-desktop-browser--address-start nil
  "Marker at the start of the editable address in the address buffer.")

(defvar-local lispm-desktop-browser--address-edited nil
  "Non-nil once the address was edited since the address buffer was filled.
An edited address is kept while the page stays put; see
`lispm-desktop-browser--sync'.")

(defvar-local lispm-desktop-browser--field nil
  "Overlay drawing the address as an edit field.")

(defun lispm-desktop-browser--note-address-edit (&rest _)
  "Record that the address was edited, from `after-change-functions'."
  (setq lispm-desktop-browser--address-edited t))

(defconst lispm-desktop-browser-address-height 30
  "Pixel height of the address line, the edit field and the toolbar around it.")

(defvar lispm-desktop-browser-bookmarks-button-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<down-mouse-1>" #'lispm-desktop-browser-bookmarks-menu)
    (keymap-set map "<mouse-1>" #'ignore)
    map)
  "Keymap of the location toolbar's Bookmarks button.")

(defun lispm-desktop-browser-bookmarks-menu (&optional _event)
  "Pop up the Bookmarks menu, as the location toolbar's Bookmarks button did."
  (interactive (list last-nonmenu-event))
  (lispm-desktop-popup-menu (lispm-desktop-browser--bookmark-items)))

(defun lispm-desktop-browser--bookmarks-button ()
  "Return the location toolbar's Bookmarks button: a bevel button holding
the bookmark ribbon, its name and a menu arrow."
  (let ((label (concat " "
                       (propertize " " 'display (lispm-desktop-browser--image
                                                 '(ribbon)
                                                 (lambda ()
                                                   (lispm-desktop-browser--svg
                                                    16 16
                                                    (format "<g transform='scale(0.66)'>%s</g>"
                                                            (format-spec (alist-get 'bookmarks lispm-desktop-browser--icons)
                                                                         (lispm-desktop-browser--ink t)))))))
                       " " (lispm-desktop-tr "Bookmarks") " ▾ ")))
    (add-face-text-property 0 (length label) 'lispm-desktop-platinum-button t label)
    (propertize label 'keymap lispm-desktop-browser-bookmarks-button-map 'pointer 'arrow
                'mouse-face 'lispm-desktop-platinum-button-pressed
                'help-echo (lispm-desktop-tr "Bookmarks: add one, edit them, or go to one"))))

(defun lispm-desktop-browser--render-address (uri)
  "Fill the address buffer with the location toolbar and the editable URI:
the Bookmarks button, the page's icon and the Location label, as
Netscape's location toolbar had them.  The URI sits in an edit field, see
`lispm-desktop-browser--draw-field'.  Filling is not an edit: change hooks are
inhibited and the edit mark cleared."
  (with-current-buffer (lispm-desktop-browser--address-buffer)
    (setq lispm-desktop-browser--address-edited nil)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (page (lispm-browser-shown-buffer)))
      (erase-buffer)
      (insert (propertize " " 'display `(space :width (8) :height (,lispm-desktop-browser-address-height)
                                               :ascent 65))
              (lispm-desktop-browser--bookmarks-button)
              (lispm-desktop-platinum-space 10)
              (or (and page (lispm-browser-favicon page)) (lispm-desktop-platinum-icon "globe"))
              " "
              (propertize (lispm-desktop-tr "Location:") 'face 'lispm-desktop-platinum-toolbar-label)
              " ")
      (add-text-properties (point-min) (point)
                           '(read-only t rear-nonsticky (read-only)))
      (setq lispm-desktop-browser--address-start (copy-marker (point)))
      (insert (or uri ""))
      (lispm-desktop-browser--draw-field)
      (when-let* ((window (get-buffer-window (current-buffer))))
        (set-window-point window (point-max))))))

(defun lispm-desktop-browser--draw-field ()
  "Draw the address as Mac OS 9's edit text field: framed in black, in the
list colour, reaching across the toolbar to a few pixels short of its
right edge, or of the extension buttons that end it; ringed in the focus
colour while it has the keyboard.  The field grows as the address is
typed at its end."
  (let ((face (if (eq (selected-window) (get-buffer-window (current-buffer)))
                  'lispm-desktop-platinum-field-focused
                'lispm-desktop-platinum-field))
        (extensions (lispm-desktop-browser--extension-buttons)))
    (unless lispm-desktop-browser--field
      (setq lispm-desktop-browser--field (make-overlay 1 1 nil nil t)))
    (overlay-put lispm-desktop-browser--field 'face face)
    (overlay-put lispm-desktop-browser--field 'before-string
                 (propertize " " 'face face 'display '(space :width (4))))
    ;; The cursor at the address's end stands on the field's blank rest,
    ;; then come the extension buttons.
    (overlay-put lispm-desktop-browser--field 'after-string
                 (concat (propertize " " 'face face 'cursor t
                                     'display `(space :align-to (- right (,(+ 12 (if extensions (+ (cdr extensions) 8) 0))))))
                         (if extensions
                             (concat (lispm-desktop-platinum-space 8) (car extensions))
                           "")))
    (when lispm-desktop-browser--address-start
      (move-overlay lispm-desktop-browser--field lispm-desktop-browser--address-start (point-max)))))

(defun lispm-desktop-browser--field-focus (_window)
  "Ring the address field while its window has the keyboard, and not after.
This is a buffer-local `window-selection-change-functions' function."
  (with-current-buffer (lispm-desktop-browser--address-buffer)
    (lispm-desktop-browser--draw-field)))

(defun lispm-desktop-browser--edited-address ()
  "Return the address typed in the address buffer, or nil before its first render."
  (with-current-buffer (lispm-desktop-browser--address-buffer)
    (when lispm-desktop-browser--address-start
      (string-trim (buffer-substring-no-properties lispm-desktop-browser--address-start (point-max))))))

(defun lispm-desktop-browser-address-submit ()
  "Visit the address or search typed in the address window."
  (interactive)
  (when-let* ((input (lispm-desktop-browser--edited-address))
              ((not (string-empty-p input)))
              (window (lispm-desktop-browser--window))
              (target (lispm-browser-input-url input)))
    (with-current-buffer (window-buffer window)
      (xwidget-webkit-goto-uri (xwidget-at (point-min)) target))
    (lispm-desktop-browser--render-address target)
    (select-window window)))

(defun lispm-desktop-browser-focus-address ()
  "Select the address window with the whole address marked for replacement:
typing or pasting replaces it, see
`lispm-desktop-browser--replace-marked-address'."
  (interactive)
  (when-let* ((window (lispm-desktop-browser--address-window)))
    (unless (eq (window-frame window) (selected-frame))
      (select-frame-set-input-focus (window-frame window)))
    (select-window window)
    (goto-char (point-max))
    (push-mark lispm-desktop-browser--address-start t t)))

(defun lispm-desktop-browser--replace-marked-address ()
  "Delete the marked address before a command that types or pastes, as
`delete-selection-mode' does, from the address buffer's `pre-command-hook':
the desktop leaves that mode off, and the field behaves so all the same.
Only commands with a `delete-selection' property act on the mark; the
others, moving, selecting, killing, do as they always do."
  (unless delete-selection-mode
    (let ((delete-selection-mode t))
      (delete-selection-pre-hook))))

(defun lispm-desktop-browser--keep-in-field ()
  "Keep point and the mark in the address field, off the toolbar before it,
from the address buffer's `post-command-hook'.  The Bookmarks button, the
page's icon and the Location label are text of the buffer, which a triple
click, selecting the whole line, a drag or \\`C-a' would otherwise select
or reach; a Mac OS 9 edit field selects only its own text, and a triple
click all of it."
  (when-let* ((start lispm-desktop-browser--address-start))
    (when (< (point) start)
      (goto-char start))
    (when (and (mark t) (< (mark t) start))
      (set-marker (mark-marker) start))))

(defconst lispm-desktop-browser-keys
  '(("C-l" . lispm-desktop-browser-focus-address)
    ("M-l" . lispm-desktop-browser-focus-address)
    ("M-[" . lispm-browser-back)
    ("M-]" . lispm-browser-forward)
    ("M-r" . lispm-browser-reload)
    ("M-." . lispm-browser-stop)
    ("M-d" . lispm-browser-add-bookmark)
    ("M-b" . lispm-desktop-browser-edit-bookmarks)
    ("M-y" . lispm-desktop-browser-show-history)
    ("M-t" . lispm-browser-new-tab)
    ("M-}" . lispm-browser-next-tab)
    ("M-{" . lispm-browser-previous-tab)
    ("M-e" . lispm-browser-extensions-press))
  "The browser's keys, Command keys as Netscape's were where Command is
Meta: Command-[ and ] go back and forward, Command-D bookmarks the page,
Command-B edits the bookmarks.  History is Command-Y, as macOS keeps
Command-H for hiding.  Command-T opens a tab and Command-} and Command-{
go to the next and the previous, as browsers with tabs do; on the page,
Command-W closes the tab, see `lispm-desktop-setup-interface'.  Command-E
presses an extension's button, see `lispm-browser-extensions-press'.")

(defun lispm-desktop-browser-bind-keys (map)
  "Bind `lispm-desktop-browser-keys' in keymap MAP."
  (pcase-dolist (`(,key . ,command) lispm-desktop-browser-keys)
    (keymap-set map key command)))

(defun lispm-desktop-browser--key (command)
  "Return the key of COMMAND among `lispm-desktop-browser-keys', for a menu."
  (car (rassq command lispm-desktop-browser-keys)))

(defvar lispm-desktop-browser-address-mode-map
  (let ((map (make-sparse-keymap)))
    (lispm-desktop-browser-bind-keys map)
    (keymap-set map "RET" #'lispm-desktop-browser-address-submit)
    map)
  "Keymap of the browser address window.")

(defun lispm-desktop-browser--close-window (_window)
  "Close the browser window's shown tab, from its title bar's close box."
  (lispm-browser-close-shown-tab))

(defun lispm-desktop-browser--zoom-box (_window)
  "Move the browser between full screen and floating, from its zoom box."
  (lispm-desktop-browser-toggle-floating))

(defun lispm-desktop-browser--collapse-box (_window)
  "Roll the floating browser up to its toolbar, or down again."
  (lispm-desktop-browser-shade))

(defun lispm-desktop-browser--boxes ()
  "Return the browser window's title bar boxes, see
`lispm-desktop-platinum-box-functions': only a floating window rolls up."
  `(:close lispm-desktop-browser--close-window :zoom lispm-desktop-browser--zoom-box
    ,@(when (lispm-desktop-browser-floating-p) '(:collapse lispm-desktop-browser--collapse-box))))

(define-derived-mode lispm-desktop-browser-address-mode fundamental-mode "Address"
  "Major mode of the browser's address window.
Its title bar names the shown page, its header line is the navigation
toolbar, its one line the location toolbar and its mode line the personal
toolbar.
\\{lispm-desktop-browser-address-mode-map}"
  (setq-local mode-line-format '(:eval (lispm-desktop-browser--personal-toolbar))
              header-line-format '(:eval (lispm-desktop-browser--toolbar))
              truncate-lines t
              cursor-type 'bar
              lispm-desktop-platinum-title #'lispm-desktop-browser--no-page-title
              lispm-desktop-platinum-icon (lispm-desktop-platinum-icon "netscape")
              lispm-desktop-platinum-box-functions (lispm-desktop-browser--boxes))
  ;; Its height is the toolbars': a frame growing, as on a tall screen,
  ;; gives its height to the page, the toolbars fitted again at once.
  (add-hook 'window-size-change-functions #'lispm-desktop-browser--refit-address nil t)
  (face-remap-add-relative 'default 'lispm-desktop-platinum-toolbar)
  (face-remap-add-relative 'header-line 'lispm-desktop-platinum-header)
  ;; The personal toolbar is a toolbar, lit or not.
  (face-remap-add-relative 'mode-line 'lispm-desktop-platinum-toolbar)
  (face-remap-add-relative 'mode-line-inactive 'lispm-desktop-platinum-toolbar)
  (display-line-numbers-mode -1)
  (visual-line-mode -1)
  (add-hook 'pre-command-hook #'lispm-desktop-browser--replace-marked-address nil t)
  (add-hook 'post-command-hook #'lispm-desktop-browser--keep-in-field nil t)
  (add-hook 'after-change-functions #'lispm-desktop-browser--note-address-edit nil t)
  (add-hook 'window-selection-change-functions #'lispm-desktop-browser--field-focus nil t))

(defun lispm-desktop-browser--address-buffer ()
  "Return the address buffer, creating it in `lispm-desktop-browser-address-mode'."
  (or (get-buffer lispm-desktop-browser-address-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-browser-address-buffer-name)
        (lispm-desktop-browser-address-mode)
        (current-buffer))))

;;;;; Titles
;;
;; A tab shows its page's icon, which lispm-browser fetches, and its title.
;; Until the icon is known, or when the page has none, a globe stands in.

(defun lispm-desktop-browser--no-page-title ()
  "Return the browser window's title while it shows no page: Browser, in
the language LispM speaks.  This is the address buffer's
`lispm-desktop-platinum-title' then."
  (lispm-desktop-tr "Browser"))

(defun lispm-desktop-browser--retitle ()
  "Name the browser window after the shown page, with the page's icon.
Each tab's window bottom is named after its page too, not after the
buffer, whose name WebKit prefixes."
  (dolist (buffer (lispm-browser-tabs))
    (with-current-buffer buffer
      (setq lispm-desktop-platinum-title (lispm-browser-title buffer))))
  (when-let* ((buffer (get-buffer lispm-desktop-browser-address-buffer-name)))
    (let ((shown (and (lispm-desktop-browser--window) (window-buffer (lispm-desktop-browser--window)))))
      (with-current-buffer buffer
        (setq lispm-desktop-platinum-title (if shown (lispm-browser-title shown)
                                             #'lispm-desktop-browser--no-page-title)
              lispm-desktop-platinum-icon (or (and shown (lispm-browser-favicon shown))
                                              (lispm-desktop-platinum-icon "globe")))))))

(defun lispm-desktop-browser--redraw-tabs ()
  "Redraw the browser window's title bar and tabs, leaving the address alone."
  (lispm-desktop-browser--retitle)
  (when-let* ((window (or (lispm-desktop-browser--address-window) (lispm-desktop-browser--window))))
    (lispm-desktop-platinum-redraw-title-bars (window-frame window))))

;;;;; Tabs

(defun lispm-desktop-browser--tab-line ()
  "Return the row of tabs atop the page: a tab per page, then + for a new one.
The tabs are Mac OS 9's, the shown page's in front.  Each shows a cross
closing it, then its page's icon before its title, or the globe until the
icon is known and when the page has none.  A middle click closes a tab
too."
  (let ((shown (and (lispm-desktop-browser--window) (window-buffer (lispm-desktop-browser--window)))))
    (lispm-desktop-platinum-tabs
     (append
      (mapcar (lambda (buffer)
                (let ((close (lispm-browser-tab-command #'lispm-browser-close-tab buffer))
                      (map (lispm-browser-tab-keymap
                            (lispm-browser-tab-command #'lispm-browser-select-tab buffer))))
                  (keymap-set map "<tab-line> <down-mouse-2>" #'ignore)
                  (keymap-set map "<tab-line> <mouse-2>" close)
                  (list (concat (or (lispm-browser-favicon buffer) (lispm-desktop-platinum-icon "globe"))
                                " "
                                (truncate-string-to-width (lispm-browser-title buffer)
                                                          lispm-browser-tab-width nil nil "…"))
                        (eq buffer shown)
                        :close close
                        'keymap map
                        'help-echo (or (lispm-browser-uri buffer) (lispm-desktop-tr "Show this tab"))
                        'pointer 'arrow)))
              (lispm-browser-tabs))
      (list (list "+" nil 'keymap (lispm-browser-tab-keymap #'lispm-browser-new-tab)
                  'help-echo (lispm-desktop-tr "New tab") 'pointer 'arrow)))
     (- (window-pixel-width) (window-right-divider-width) (window-scroll-bar-width)))))

(declare-function lispm-desktop-show "lispm-desktop-surface" (&optional window))
(declare-function lispm-desktop-empty-buffer "lispm-desktop-surface" ())
(declare-function lispm-desktop--new-tab-empty "lispm-desktop-surface" ())
(declare-function lispm-desktop--select-tab "lispm-desktop-surface" (&optional frame))
(declare-function lispm-desktop-editor-hide "lispm-desktop-editor" ())
(declare-function lispm-desktop-editor-floating-p "lispm-desktop-editor" ())
(defvar lispm-desktop-activate-hook)

(declare-function lispm-desktop-activate "lispm-desktop-surface" ())

(defun lispm-desktop-browser--before-quit ()
  "Close the browser's window as it quits, remembering where it floated.
This is on `lispm-browser-before-quit-hook'."
  (when-let* ((address (lispm-desktop-browser--address-window)))
    (unless (lispm-desktop-browser-floating-p)
      (delete-window address)))
  (lispm-desktop-browser--delete-frame))

(defun lispm-desktop-browser--quit ()
  "Bring the desktop in front once the browser quit, and close its
workspace; it opens again full screen or floating, as it was.  The
desktop stays behind when another application came in front: the window
behind the floating browser took the keyboard, see
`lispm-desktop-apps-focus-after'.  This is on `lispm-browser-quit-hook'."
  (when-let* ((address (get-buffer lispm-desktop-browser-address-buffer-name)))
    (kill-buffer address))
  ;; With the desktop hidden, the windows left out keep the screen.
  (when (and (eq (lispm-desktop-apps-frontmost) 'browser) (not (lispm-desktop-apps-hidden-p)))
    (lispm-desktop-activate))
  (lispm-desktop-browser--close-workspace))

;;;;; Layout and synchronisation

(defvar lispm-desktop-browser--sync-timer nil
  "Repeating timer keeping the address window in step with the page.")

(defvar lispm-desktop-browser--synced-state nil
  "Address, tab titles, shown tab and what it can do at the last sync.")


(defun lispm-desktop-browser--state ()
  "Return what the address window shows: the address, tab titles, shown tab.
What the shown tab can do is included, see
`lispm-desktop-browser--navigation': a finished load redraws the tab line,
whose page icons are looked up once a page has loaded, and the toolbar, whose
buttons it enables."
  (let ((shown (lispm-browser-shown-buffer)))
    (list (lispm-browser-uri)
          (mapcar #'buffer-name (lispm-browser-tabs))
          shown
          (lispm-browser-navigation-state))))

(defun lispm-desktop-browser--sync (&optional force)
  "Show the current page's address and tabs in the address window.
Nothing is redrawn unless the state of `lispm-desktop-browser--state' changed,
or FORCE: the page is never redisplayed by an idle tick, which could drop a
click in progress.  The address follows the page unless it was edited: an
edited address is kept while the page stays put, and while it is being typed
even when the page moves; FORCE, as navigation and switching tabs pass, always
shows the page's address.  The timer stops once no address window is left."
  (if-let* ((address (lispm-desktop-browser--address-window)))
      (let* ((state (lispm-desktop-browser--state))
             (moved (not (equal (car state) (car lispm-desktop-browser--synced-state))))
             (edited (buffer-local-value 'lispm-desktop-browser--address-edited
                                         (window-buffer address))))
        (when (or force (not (equal state lispm-desktop-browser--synced-state)))
          (setq lispm-desktop-browser--synced-state state
                lispm-desktop-browser--navigation (nth 3 state))
          (when (and (plist-get lispm-desktop-browser--navigation :loading)
                     (not lispm-desktop-browser--throbber-timer))
            (setq lispm-desktop-browser--throbber-timer
                  (run-with-timer 0 0.12 #'lispm-desktop-browser--turn-throbber)))
          (lispm-desktop-browser--redraw-tabs)
          (when (or force (not edited)
                    (and moved (not (eq address (selected-window)))))
            (lispm-desktop-browser--render-address (car state)))
          (with-current-buffer (window-buffer address)
            (force-mode-line-update))))
    (when lispm-desktop-browser--sync-timer
      (cancel-timer lispm-desktop-browser--sync-timer)
      (setq lispm-desktop-browser--sync-timer nil))))

(defun lispm-desktop-browser--fit-address ()
  "Fit the address window to its title bar, toolbar and address line.
They are taller than text lines, so the window is fitted in pixels, then
kept so: a frame growing, as on a tall screen, gives its height to the
page, never to the toolbars."
  (when-let* ((address (lispm-desktop-browser--address-window)))
    (let ((window-resize-pixelwise t))
      (fit-window-to-buffer address nil nil nil nil t))))

(defun lispm-desktop-browser--refit-address (window)
  "Fit the address WINDOW to its toolbars again once its size changed, as
a frame growing or shrinking changes it, unless it is alone in its frame,
as in a rolled-up browser.  This is a buffer-local
`window-size-change-functions' function."
  (unless (frame-root-window-p window)
    (let ((window-resize-pixelwise t))
      (fit-window-to-buffer window nil nil nil nil t))))

(defun lispm-desktop-browser--ensure-layout ()
  "Put the address window above the browser window, once, and keep it synced.
The address window is fitted to its lines, see
`lispm-desktop-browser--fit-address'."
  (when-let* ((window (lispm-desktop-browser--window)))
    (with-current-buffer (lispm-desktop-browser--address-buffer)
      (setq lispm-desktop-platinum-box-functions (lispm-desktop-browser--boxes)))
    (unless (lispm-desktop-browser--address-window)
      (let* ((window-min-height 1)
             (address (split-window window -2 'above)))
        (set-window-buffer address (lispm-desktop-browser--address-buffer))
        (set-window-parameter address 'lispm-desktop-chrome t)
        (set-window-dedicated-p address t)))
    ;; The address window wears the browser window's title bar, active
    ;; while the page is.
    (set-window-parameter (lispm-desktop-browser--address-window) 'lispm-desktop-platinum-title-for window)
    (lispm-desktop-browser--sync t)
    ;; A new window's lines are measured right only once it was drawn, so
    ;; it is fitted again after the next redisplay.
    (lispm-desktop-browser--fit-address)
    (run-at-time 0 nil #'lispm-desktop-browser--fit-address)
    (unless lispm-desktop-browser--sync-timer
      (setq lispm-desktop-browser--sync-timer (run-with-timer 0.7 0.7 #'lispm-desktop-browser--sync-tick)))))

(defun lispm-desktop-browser--sync-tick ()
  "Sync the address window, see `lispm-desktop-browser--sync', unless its
frame is out of sight: the page is synced again when it shows, and the
tick stops once no address window is left."
  (let ((address (lispm-desktop-browser--address-window)))
    (unless (and address (not (eq (frame-visible-p (window-frame address)) t)))
      (lispm-desktop-browser--sync))))

(defun lispm-desktop-browser-show ()
  "Show the browser, full screen or floating as it was last, with the most
recent tab, opening `lispm-browser-home-url' when there is no tab.  The
desktop's Browser icon runs this, and the browser comes in front."
  (interactive)
  ;; With the desktop hidden, the browser floats out on the computer's screen.
  (if (or lispm-desktop-browser-floating (lispm-desktop-apps-hidden-p))
      (lispm-desktop-browser--show-floating)
    (lispm-desktop-browser--show-full-screen))
  (force-mode-line-update t))

(defun lispm-desktop-browser--show-full-screen (&optional shown)
  "Show the browser in its workspace, SHOWN or its first tab in the main area
under the address window.  The floating editor, whose window would cover
the page, is hidden.  The workspace starts on the desktop's bare colour,
not the editor's start page, which a document there would take for the
editor's own; it is shown in place, see `lispm-desktop-show-in-new-tab', so
no session of the workspace left comes along."
  (when (fboundp 'lispm-desktop-editor-hide)
    (lispm-desktop-editor-hide))
  (lispm-desktop-apps-set-frontmost 'browser)
  (let* ((fresh (let ((tab-bar-new-tab-choice #'lispm-desktop--new-tab-empty))
                  (lispm-desktop-workspace-enter lispm-desktop-browser-workspace-name)))
         (browser (or shown (car (lispm-browser-tabs)))))
    (cond ((and browser fresh) (lispm-desktop-workspace-show-alone browser))
          (browser (unless (get-buffer-window browser)
                     (select-window (lispm-desktop-platinum-main-window))
                     (pop-to-buffer-same-window browser)))
          (t (select-window (lispm-desktop-platinum-main-window))
             (xwidget-webkit-browse-url lispm-browser-home-url t)
             (when fresh (tabspaces-reset-buffer-list))))
    (lispm-desktop-browser--ensure-layout)))

(defun lispm-desktop-browser--default-geometry (screen)
  "Return where the browser floats first: most of SCREEN, under its menu bar."
  (let* ((width (frame-inner-width screen))
         (height (frame-inner-height screen))
         (top (+ (lispm-desktop-app-window-menu-bar-height screen) 30))
         (left (round (* width 0.1))))
    (list left top (- width (* 2 left)) (- height top 50))))

(defun lispm-desktop-browser--roll (_frame)
  "Leave the floating browser, selected, its address window alone, and
return that window's height, its title bar and toolbars: the browser
rolls up to it.  Without the address window, return nil."
  (when-let* ((address (lispm-desktop-browser--address-window)))
    (let ((height (window-pixel-height address))
          (ignore-window-parameters t))
      (delete-other-windows address)
      height)))

(defun lispm-desktop-browser--hidden-by (app)
  "Return non-nil when the floating browser hides from APP in front: the
full-screen editor, whose workspace it would cover."
  (and (eq app 'editor) (lispm-desktop-apps-full-screen-p app)))

(lispm-desktop-app-window-define 'browser
  :name (lispm-desktop-tr-noop "Browser") :geometry #'lispm-desktop-browser--default-geometry
  :roll #'lispm-desktop-browser--roll :hidden-by #'lispm-desktop-browser--hidden-by)

(defun lispm-desktop-browser--make-frame (screen)
  "Return the floating browser's child frame of SCREEN, not yet shown, on the
empty buffer until a page shows in it."
  (with-current-buffer (lispm-desktop-empty-buffer)
    (lispm-desktop-app-window-make-frame 'browser screen)))

(defun lispm-desktop-browser--show-floating (&optional shown)
  "Show the browser floating over the desktop, SHOWN or its first tab in its
window, the window made when missing.  The floating windows left over the
desktop show with it."
  (let ((screen (lispm-desktop-apps-screen))
        (browser (or shown (car (lispm-browser-tabs)))))
    (unless (lispm-desktop-apps-hidden-p screen)
      (with-selected-frame screen
        (lispm-desktop--select-tab))
      (run-hooks 'lispm-desktop-activate-hook))
    (unless (lispm-desktop-browser-floating-p)
      (setq lispm-desktop-browser--frame (lispm-desktop-browser--make-frame screen)))
    (with-selected-frame lispm-desktop-browser--frame
      (cond ((lispm-desktop-browser--window)
             (when shown (set-window-buffer (lispm-desktop-browser--window) shown)))
            (browser (set-window-buffer (frame-first-window) browser))
            (t (select-window (frame-first-window))
               (xwidget-webkit-browse-url lispm-browser-home-url t)))
      (lispm-desktop-browser--ensure-layout))
    (lispm-desktop-platinum-show-window lispm-desktop-browser--frame)
    (lispm-desktop-apps-keep-on-desktop lispm-desktop-browser--frame)
    (select-frame-set-input-focus lispm-desktop-browser--frame)
    (select-window (or (lispm-desktop-browser--window) (frame-first-window lispm-desktop-browser--frame)))
    (lispm-desktop-apps-set-frontmost 'browser)))

(defun lispm-desktop-browser--delete-frame (&optional full-screen)
  "Delete the floating browser's window, remembering where it was.  The
keyboard it had goes to the window behind it, see
`lispm-desktop-apps-focus-after', or with FULL-SCREEN, the browser going
full screen, to the screen."
  (when (lispm-desktop-browser-floating-p)
    (let ((frame lispm-desktop-browser--frame))
      (lispm-desktop-browser--unshade)
      (lispm-desktop-app-window-release frame)
      (setq lispm-desktop-browser--frame nil)
      (when (lispm-desktop-app-window-keyboard-p frame)
        (if full-screen
            (select-frame-set-input-focus (lispm-desktop-apps-window-screen frame))
          (lispm-desktop-apps-focus-after frame)))
      (delete-frame frame))))

(defun lispm-desktop-browser--close-workspace ()
  "Close the full-screen browser's workspace, the desktop's in front instead."
  (when-let* ((screen (lispm-desktop-apps-screen))
              ((with-selected-frame screen
                 (tab-bar--tab-index-by-name lispm-desktop-browser-workspace-name))))
    (with-selected-frame screen
      (when (lispm-desktop-browser-workspace-p)
        (lispm-desktop--select-tab))
      (tab-bar-close-tab (1+ (tab-bar--tab-index-by-name lispm-desktop-browser-workspace-name))))))

(defun lispm-desktop-browser-float ()
  "Float the browser in a window of its own over the desktop."
  (interactive)
  (setq lispm-desktop-browser-floating t)
  (unless (lispm-desktop-browser-floating-p)
    (let ((shown (and (lispm-desktop-browser--window) (window-buffer (lispm-desktop-browser--window)))))
      (lispm-desktop-browser--close-workspace)
      (lispm-desktop-browser--show-floating shown))))

(defun lispm-desktop-browser-full-screen ()
  "Bring the floating browser back to full screen, under the menu bar."
  (interactive)
  (setq lispm-desktop-browser-floating nil)
  ;; Full screen is the desktop's: it shows first.
  (when (and (lispm-desktop-apps-hidden-p) (fboundp 'lispm-desktop-show-desktop))
    (lispm-desktop-show-desktop))
  (when (lispm-desktop-browser-floating-p)
    (let ((shown (and (lispm-desktop-browser--window) (window-buffer (lispm-desktop-browser--window)))))
      (lispm-desktop-browser--delete-frame t)
      (lispm-desktop-browser--show-full-screen shown))))

(defun lispm-desktop-browser-toggle-floating ()
  "Move the browser between full screen and a floating window, as its zoom
box does."
  (interactive)
  (if (lispm-desktop-browser-floating-p) (lispm-desktop-browser-full-screen) (lispm-desktop-browser-float)))

(defun lispm-desktop-browser--unshade ()
  "Roll the floating browser down again, when it is rolled up."
  (when (lispm-desktop-browser-floating-p)
    (lispm-desktop-app-window-roll-down lispm-desktop-browser--frame)))

(defun lispm-desktop-browser-shade ()
  "Roll the floating browser up to its title bar and toolbar, or down
again, as Mac OS 9 rolled a window up into its title bar."
  (interactive)
  (when (lispm-desktop-browser-floating-p)
    (lispm-desktop-app-window-toggle-roll lispm-desktop-browser--frame)))

(defun lispm-desktop-browser ()
  "Switch to the browser, or back to the previous workspace when its
full-screen workspace is in front; see `lispm-desktop-browser-show'."
  (interactive)
  (if (lispm-desktop-browser-workspace-p)
      (progn (tab-bar-switch-to-recent-tab)
             (force-mode-line-update t))
    (lispm-desktop-browser-show)))

(defun lispm-desktop-browser--setup ()
  "Draw a new tab's page in the browser window: its tabs on Mac OS 9's,
its status bar Netscape's.  Its title bar is the address window's, which
stands at the top of the browser, so the page shows neither a title bar of
its own nor WebKit's title line.  This is on `lispm-browser-tab-hook'."
  ;; The browser window's title bar is the address window's, above the
  ;; toolbar; the page's own top line holds the tabs, and WebKit's title
  ;; line would only repeat the title.
  (setq-local tab-line-format '(:eval (lispm-desktop-browser--tab-line))
              header-line-format nil
              ;; The bottom edge is Netscape's status bar.
              lispm-desktop-platinum-info #'lispm-desktop-browser--status)
  ;; The title bar above lights and dims with the page.
  (add-hook 'window-selection-change-functions
            (lambda (window)
              (when (window-live-p window)
                (lispm-desktop-platinum-redraw-title-bars (window-frame window))))
            nil t)
  ;; The unified copy and paste keys act on the page's selection and its
  ;; focused field, not on the buffer, which holds only the view.
  (setq-local lispm-desktop-copy-function #'xwidget-webkit-copy-selection-as-kill
              lispm-desktop-paste-function #'lispm-browser-paste))

(defun lispm-desktop-browser--on-workspace-select (&rest _)
  "Resume address synchronisation when the browser workspace is selected."
  (when (lispm-desktop-browser-workspace-p)
    (lispm-desktop-browser--ensure-layout)))

;;;;; Setup

;; Names the desktop's browser had before it became lispm-browser.
(define-obsolete-variable-alias 'lispm-desktop-browser-home-url 'lispm-browser-home-url "0.2")
(define-obsolete-variable-alias 'lispm-desktop-xwidget-search-url 'lispm-browser-search-url "0.2")
(define-obsolete-variable-alias 'lispm-desktop-browser-history-file 'lispm-browser-history-file "0.2")
(define-obsolete-variable-alias 'lispm-desktop-browser-history-size 'lispm-browser-history-size "0.2")
(define-obsolete-variable-alias 'lispm-desktop-browser-hint-characters 'lispm-browser-hint-characters "0.2")
(define-obsolete-variable-alias 'lispm-desktop-browser-extensions-enabled
  'lispm-browser-extensions-enabled "0.2")
(define-obsolete-variable-alias 'lispm-desktop-browser-extensions-directory
  'lispm-browser-extensions-directory "0.2")

(defun lispm-desktop-browser--adopt-old-files ()
  "Give lispm-browser the history and the extensions the desktop's browser
kept under their old names, once: each is moved to its new name unless
something is there already."
  (pcase-dolist (`(,old . ,new)
                 `((,(locate-user-emacs-file "lispm-desktop-browser-history.eld")
                    . ,lispm-browser-history-file)
                   (,(locate-user-emacs-file "lispm-desktop-browser-extensions/")
                    . ,lispm-browser-extensions-directory)))
    (when (and (file-exists-p old) (not (file-exists-p new)))
      (ignore-errors
        (make-directory (file-name-directory (directory-file-name new)) t)
        (rename-file (directory-file-name old) (directory-file-name new))))))

(defun lispm-desktop-browser--raise ()
  "Bring the browser in front, unless it is.
This is `lispm-browser-raise-function' on the desktop."
  (unless (eq (lispm-desktop-apps-frontmost) 'browser)
    (lispm-desktop-browser-show)))

(defun lispm-desktop-browser--sync-now ()
  "Show the page's address and tabs at once, see `lispm-desktop-browser--sync'."
  (lispm-desktop-browser--sync t))

(defvar lispm-browser-clipboard-function)

(defun lispm-desktop-browser--scroll-bar (buffer)
  "Say no scroll bar for BUFFER, a page, which scrolls itself.
This is one of `lispm-desktop-platinum-scroll-bar-functions'."
  (and (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'xwidget-webkit-mode) 'no))

(defun lispm-desktop-browser--follow-proxy (url hosts)
  "Send the browser's pages through the proxy at URL, HOSTS directly, as
LispM's other connections go; directly when URL is nil.
This is a `lispm-desktop-proxy-change-functions' function.  A browser
older than its proxy is left as it is."
  (when (require 'lispm-browser-proxy nil t)
    (lispm-browser-set-proxy url hosts)))

(defun lispm-desktop-setup-browser ()
  "Set lispm-browser up as the desktop's browser, drawn as Netscape's.
Its pages take the desktop's keys, and \\`C-c x' prefixes its commands."
  (lispm-desktop-browser--adopt-old-files)
  (setq lispm-browser-window-function #'lispm-desktop-browser--window
        lispm-browser-tab-window-function #'lispm-desktop-platinum-main-window
        lispm-browser-show-function #'lispm-desktop-browser-show
        lispm-browser-raise-function #'lispm-desktop-browser--raise
        lispm-browser-clipboard-function #'lispm-desktop-clipboard-text)
  (add-hook 'lispm-browser-tab-hook #'lispm-desktop-browser--setup)
  (add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-desktop-browser--scroll-bar)
  (add-hook 'lispm-browser-change-hook #'lispm-desktop-browser--sync-now)
  ;; The address line's words, in the language now spoken.
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-browser--sync-now)
  (add-hook 'lispm-browser-redraw-hook #'lispm-desktop-browser--retitle)
  (add-hook 'lispm-browser-before-quit-hook #'lispm-desktop-browser--before-quit)
  ;; The extension buttons on the location toolbar.
  (setq lispm-browser-extensions-anchor-function #'lispm-desktop-browser--extension-anchor)
  (add-hook 'lispm-browser-extensions-changed-hook #'lispm-desktop-browser--redraw-extensions)
  (add-hook 'lispm-browser-quit-hook #'lispm-desktop-browser--quit)
  ;; Its pages go through LispM's proxy, as its other connections do.
  (add-hook 'lispm-desktop-proxy-change-functions #'lispm-desktop-browser--follow-proxy)
  (lispm-desktop-browser--follow-proxy (lispm-desktop-proxy-url) (lispm-desktop-proxy-direct-hosts))
  ;; After the workspace's own functions, which run first.
  (add-hook 'tab-bar-tab-post-select-functions #'lispm-desktop-browser--on-workspace-select 90)
  (with-eval-after-load 'xwidget
    (lispm-desktop-browser-bind-keys xwidget-webkit-mode-map)
    ;; C-y pastes into the page, which the buffer's text is not, and
    ;; Command-W closes the tab, as in the Finder's and Control Panels'
    ;; windows; in the location field M-w copies, as in Emacs.
    (keymap-set xwidget-webkit-mode-map "C-y" #'lispm-desktop-paste)
    (keymap-set xwidget-webkit-mode-map "M-w" #'lispm-browser-close-shown-tab))
  (keymap-global-set "C-c x b" #'lispm-browser-browse-url)
  (keymap-global-set "C-c x s" #'lispm-browser-web-search)
  (keymap-global-set "C-c x u" #'lispm-browser-open-url-at-point)
  (lispm-browser-setup 'none))

(provide 'lispm-desktop-browser)
;;; lispm-desktop-browser.el ends here
