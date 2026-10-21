;;; lispm-desktop-term-sums.eld --- The terminal modules LispM downloads  -*- lisp-data -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; The SHA-256 of each library of the release term-module-VERSION, the
;; module lispm-desktop-term.el downloads for the sources in
;; native/lispm-desktop-term: a library whose SHA-256 is not the one here
;; is refused.  VERSION must be the version of those sources.
;; scripts/pin-term-module.py writes it (make pin) once the terminal
;; module workflow has published that release.

(:version "bc91eaad319656b5"
 :modules (("aarch64-apple-darwin" . "0e41751a9e1df18295356b10ae51b0f307e36df1b61ca837d1fec7906859c754")
           ("x86_64-apple-darwin" . "735db087584a52195e59b4ae8605157fc89e43fa69c752acf31f35d33b0e2c81")
           ("x86_64-unknown-linux-gnu" . "c689b49d7e64c6b7f579d44fea45f1f188021d3c3aa49cb44de36eca46d71e51")))
