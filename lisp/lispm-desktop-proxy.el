;;; lispm-desktop-proxy.el --- One proxy for every connection LispM makes  -*- lexical-binding: t; -*-

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

;; LispM can send every request it makes through one proxy, an HTTP proxy
;; or a SOCKS5 one.  By default it sets none: Emacs connects as its
;; environment says, through the proxy http_proxy names if any.  The
;; Network control panel chooses one, see lispm-desktop-settings.el, and
;; personal.el can set it too:
;;
;;   (setq lispm-desktop-proxy-type 'socks5
;;         lispm-desktop-proxy-server "127.0.0.1:7890")
;;
;; A change takes effect at once, on the three ways out of Emacs:
;;
;;   Emacs's own requests   url.el's, which package.el, EWW and the agents'
;;                          tools fetch with.  url.el asks
;;                          `lispm-desktop-proxy-find-for-url' for each one:
;;                          an HTTP proxy is url.el's own, HTTPS tunnelled
;;                          through it with CONNECT.  A SOCKS5 proxy is
;;                          socks.el's, TLS negotiated over it as ERC does.
;;   programs Emacs starts  git, curl, the agents, the terminals' shells:
;;                          http_proxy, https_proxy and all_proxy in
;;                          `process-environment', and no_proxy.
;;   the browser's pages    through `lispm-desktop-proxy-change-functions',
;;                          which the browser's setup adds to.
;;
;; Loopback addresses never go through it: LispM's own servers and the
;; models served on this machine are reached directly, as are the hosts of
;; `lispm-desktop-proxy-exceptions'.  Turned off, the environment gets back
;; what it held before.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar url-proxy-locator)
(defvar url-gateway-method)
(defvar url-gateway-unplugged)
(defvar socks-server)
(defvar socks-noproxy)
(declare-function url-default-find-proxy-for-url "url-proxy" (urlobj host))
(declare-function socks-open-network-stream "socks" (name buffer host service))
(declare-function gnutls-negotiate "gnutls" (&rest spec))
(declare-function nsm-verify-connection "nsm"
                  (process host port &optional save-fingerprint warn-unencrypted))
(declare-function puny-encode-domain "puny" (domain))

(defgroup lispm-desktop-proxy nil
  "The proxy every connection LispM makes goes through."
  :group 'lispm-desktop)

(defcustom lispm-desktop-proxy-type nil
  "The kind of proxy LispM's connections go through: `http', `socks5', or
nil for none of LispM's own, the environment's proxy, if any, still
used.  The proxy listens at `lispm-desktop-proxy-server'."
  :type '(choice (const :tag "None" nil)
                 (const :tag "HTTP" http)
                 (const :tag "SOCKS5" socks5))
  :group 'lispm-desktop-proxy)

(defcustom lispm-desktop-proxy-server nil
  "The proxy's address, HOST:PORT, as \"127.0.0.1:7890\".
It is kept while `lispm-desktop-proxy-type' is nil, for the next time a
proxy is used."
  :type '(choice (const :tag "None" nil) (string :tag "Host:port"))
  :group 'lispm-desktop-proxy)

(defcustom lispm-desktop-proxy-exceptions nil
  "Hosts reached directly, not through the proxy: names, as \"example.com\",
each with its subdomains, and addresses.  Loopback addresses always are,
see `lispm-desktop-proxy-loopback'."
  :type '(repeat string)
  :group 'lispm-desktop-proxy)

(defconst lispm-desktop-proxy-loopback '("localhost" "127.0.0.1" "::1")
  "Hosts always reached directly: this machine, where LispM's own servers
and the models served locally listen.")

(defconst lispm-desktop-proxy-variables
  '("http_proxy" "https_proxy" "all_proxy" "HTTP_PROXY" "HTTPS_PROXY" "ALL_PROXY")
  "The environment variables naming the proxy for programs.
curl reads the lower-case http_proxy only; other programs read either.")

(defconst lispm-desktop-proxy-exception-variables '("no_proxy" "NO_PROXY")
  "The environment variables naming the hosts programs reach directly.")

(defvar lispm-desktop-proxy-change-functions nil
  "Functions called once LispM's proxy changed, with its URL, as
`lispm-desktop-proxy-url' returns it, and the hosts reached directly.
The URL is nil when there is no proxy.  The browser's setup adds one, so
that its pages follow.")

;;;; The proxy

(defun lispm-desktop-proxy-parse-server (text)
  "Return the proxy's address TEXT as (HOST . PORT), or nil when it names none.
TEXT is HOST:PORT, an IPv6 HOST in brackets; a scheme before it, as
http://, is passed over."
  (let ((text (string-trim (or text ""))))
    (when (string-match (rx bos (? (+ (any alnum "+.-")) "://")
                            (or (seq "[" (group (+ (not (any "]")))) "]")
                                (group (+ (not (any ":/[] \t")))))
                            ":" (group (+ digit)) (? "/") eos)
                        text)
      (let ((host (or (match-string 1 text) (match-string 2 text)))
            (port (string-to-number (match-string 3 text))))
        (when (<= 1 port 65535)
          (cons host port))))))

(defun lispm-desktop-proxy--url (type server)
  "Return the URL of the proxy of TYPE at SERVER, or nil when there is none:
http://HOST:PORT, or socks5h://HOST:PORT, the proxy looking names up."
  (when-let* ((type)
              (address (lispm-desktop-proxy-parse-server server)))
    (format "%s://%s:%d" (if (eq type 'socks5) "socks5h" "http")
            (if (string-search ":" (car address)) (format "[%s]" (car address)) (car address))
            (cdr address))))

(defun lispm-desktop-proxy-url ()
  "Return the URL of the proxy LispM's connections go through, or nil."
  (lispm-desktop-proxy--url lispm-desktop-proxy-type lispm-desktop-proxy-server))

(defun lispm-desktop-proxy--direct-hosts (exceptions)
  "Return the hosts reached directly: the loopback ones and EXCEPTIONS."
  (delete-dups (append lispm-desktop-proxy-loopback exceptions)))

(defun lispm-desktop-proxy-direct-hosts ()
  "Return the hosts LispM's connections reach directly, not through the
proxy: the loopback ones and `lispm-desktop-proxy-exceptions'."
  (lispm-desktop-proxy--direct-hosts lispm-desktop-proxy-exceptions))

(defun lispm-desktop-proxy-direct-p (host)
  "Return non-nil when HOST is reached directly, not through the proxy: a
loopback address, or one of `lispm-desktop-proxy-exceptions' or below
one.  An exception \"*\" is every host."
  (let ((host (downcase (string-remove-suffix "]" (string-remove-prefix "[" host)))))
    (or (string-prefix-p "127." host)
        (seq-some (lambda (exception)
                    (let ((domain (downcase (string-trim-left exception "[*.]+"))))
                      (if (string-empty-p domain)
                          (string-search "*" exception)
                        (or (equal host domain)
                            (string-suffix-p (concat "." domain) host)))))
                  (lispm-desktop-proxy-direct-hosts)))))

;;;; Programs Emacs starts

(defvar lispm-desktop-proxy--original-environment nil
  "The proxy variables of the environment before LispM set them, as
\(VARIABLE . VALUE), VALUE nil for one unset; nil while LispM sets none.")

(defun lispm-desktop-proxy--environment (url exceptions original)
  "Return the environment for programs to go through the proxy at URL, the
hosts EXCEPTIONS directly, as (VARIABLE . VALUE).
ORIGINAL is what the variables held before: the hosts its no_proxy named
are still reached directly."
  (append (mapcar (lambda (variable) (cons variable url)) lispm-desktop-proxy-variables)
          (mapcar (lambda (variable)
                    (cons variable
                          (string-join (delete-dups
                                        (append (lispm-desktop-proxy--direct-hosts exceptions)
                                                (split-string (or (cdr (assoc variable original)) "")
                                                              "[, ]+" t)))
                                       ",")))
                  lispm-desktop-proxy-exception-variables)))

(defun lispm-desktop-proxy--set-environment (url exceptions)
  "Make the programs Emacs starts go through the proxy at URL, the hosts
EXCEPTIONS directly; when URL is nil, give the environment back what it
held before LispM set a proxy."
  (if url
      (progn
        (unless lispm-desktop-proxy--original-environment
          (setq lispm-desktop-proxy--original-environment
                (mapcar (lambda (variable) (cons variable (getenv variable)))
                        (append lispm-desktop-proxy-variables lispm-desktop-proxy-exception-variables))))
        (pcase-dolist (`(,variable . ,value)
                       (lispm-desktop-proxy--environment url exceptions
                                                         lispm-desktop-proxy--original-environment))
          (setenv variable value)))
    (pcase-dolist (`(,variable . ,value) lispm-desktop-proxy--original-environment)
      (setenv variable value))
    (setq lispm-desktop-proxy--original-environment nil)))

;;;; Emacs's own requests

(defvar lispm-desktop-proxy--url-locator nil
  "The function url.el found proxies with before LispM's.
It still does while LispM sets no proxy.")

(defun lispm-desktop-proxy-find-for-url (urlobj host)
  "Return how url.el reaches HOST to retrieve URLOBJ, as `url-proxy-locator'
answers: \"PROXY HOST:PORT\" through LispM's HTTP proxy; \"DIRECT\" for a
host reached directly, and under a SOCKS5 proxy, which
`lispm-desktop-proxy--url-open-stream' goes through.  While LispM sets no
proxy, the locator url.el had before answers."
  (cond ((not (lispm-desktop-proxy-url))
         (funcall (or lispm-desktop-proxy--url-locator #'url-default-find-proxy-for-url)
                  urlobj host))
        ((or (not (eq lispm-desktop-proxy-type 'http))
             (lispm-desktop-proxy-direct-p host))
         "DIRECT")
        (t (format "PROXY %s" (string-remove-prefix "http://" (lispm-desktop-proxy-url))))))

(defun lispm-desktop-proxy--url-open-stream (open name buffer host service &optional gateway-method)
  "Open url.el's stream NAME to HOST and SERVICE, through the SOCKS5 proxy
when one is set and HOST is not reached directly; else as OPEN does.
BUFFER and GATEWAY-METHOD are as `url-open-stream' takes them: an HTTPS
stream, method `tls', is negotiated over the proxy's, as ERC's
`erc-open-socks-tls-stream' does.  This is an `:around' advice of
`url-open-stream'."
  (if-let* (((eq lispm-desktop-proxy-type 'socks5))
            ((not url-gateway-unplugged))
            (address (lispm-desktop-proxy-parse-server lispm-desktop-proxy-server))
            ((not (lispm-desktop-proxy-direct-p host))))
      (progn
        (require 'socks)
        (let* ((socks-server (list "LispM" (car address) (cdr address) 5))
               (socks-noproxy nil)
               ;; Bytes, as `url-open-stream' opens its streams: url.el
               ;; counts a body's length in them.
               (coding-system-for-read 'binary)
               (coding-system-for-write 'binary)
               (stream (socks-open-network-stream name buffer host service)))
          (if (memq (or gateway-method url-gateway-method) '(tls ssl))
              (progn
                (require 'gnutls)
                (require 'nsm)
                (require 'puny)
                (nsm-verify-connection (gnutls-negotiate :process stream
                                                         :hostname (puny-encode-domain host)
                                                         :verify-error nil)
                                       host service))
            stream)))
    (funcall open name buffer host service gateway-method)))

;;;; Changes

(defun lispm-desktop-proxy--apply (type server exceptions)
  "Send LispM's connections through the proxy of TYPE at SERVER, the hosts
EXCEPTIONS directly, or through none when TYPE is nil.
url.el asks for the proxy at each request; the environment of the
programs Emacs starts and `lispm-desktop-proxy-change-functions' follow
now."
  (let ((url (lispm-desktop-proxy--url type server)))
    (lispm-desktop-proxy--set-environment url exceptions)
    (run-hook-with-args 'lispm-desktop-proxy-change-functions
                        url (lispm-desktop-proxy--direct-hosts exceptions))))

(defun lispm-desktop-proxy-apply ()
  "Send LispM's connections through the proxy the settings name."
  (lispm-desktop-proxy--apply lispm-desktop-proxy-type lispm-desktop-proxy-server
                              lispm-desktop-proxy-exceptions))

(defun lispm-desktop-proxy--changed (symbol value operation where)
  "Follow the proxy as the setting SYMBOL becomes VALUE.
OPERATION and WHERE are as a variable watcher gets them: a binding with
`let' or a buffer's own value changes nothing."
  (when (and (eq operation 'set) (null where))
    (let ((settings `((lispm-desktop-proxy-type . ,lispm-desktop-proxy-type)
                      (lispm-desktop-proxy-server . ,lispm-desktop-proxy-server)
                      (lispm-desktop-proxy-exceptions . ,lispm-desktop-proxy-exceptions))))
      (setf (alist-get symbol settings) value)
      (lispm-desktop-proxy--apply (alist-get 'lispm-desktop-proxy-type settings)
                                  (alist-get 'lispm-desktop-proxy-server settings)
                                  (alist-get 'lispm-desktop-proxy-exceptions settings)))))

(defun lispm-desktop-setup-proxy ()
  "Send LispM's connections through the proxy the settings name, and
again whenever they change: from the control panel, Customize, or a
`setq' in personal.el."
  (with-eval-after-load 'url-proxy
    (unless (eq url-proxy-locator #'lispm-desktop-proxy-find-for-url)
      (setq lispm-desktop-proxy--url-locator url-proxy-locator
            url-proxy-locator #'lispm-desktop-proxy-find-for-url)))
  (advice-add 'url-open-stream :around #'lispm-desktop-proxy--url-open-stream)
  (dolist (variable '(lispm-desktop-proxy-type lispm-desktop-proxy-server lispm-desktop-proxy-exceptions))
    (add-variable-watcher variable #'lispm-desktop-proxy--changed))
  (lispm-desktop-proxy-apply))

(provide 'lispm-desktop-proxy)
;;; lispm-desktop-proxy.el ends here
