;;; frp-csp-benchmark.el --- Benchmark Frame-Synced FRP and Generational CSP -*- lexical-binding: t; -*-

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

;; Synthetic benchmark measuring the architectural impact of the
;; Frame-Synced FRP DAG and Generational CSP Kernel refactoring (#223):
;;
;; 1. Frame-Synced FRP Batching vs. Unbatched Diamond Propagation
;;    (`frame-unbatched' vs. `frame-batched'):
;;    Simulates N frame ticks where 4 frame-clock clients update 4 source
;;    signals feeding 3 intermediate derived signals and a downstream
;;    render effect that allocates a synthetic chrome layout plist.
;;    Measures wall elapsed time, total effect evaluations, and Emacs GCs.
;;
;; 2. Relational `lispm-ui-list-derive' Cutoff vs. Imperative Recompute
;;    (`list-imperative' vs. `list-reactive'):
;;    Simulates M UI poll/read cycles over a 1,000-entry Console event log
;;    where 90% of poll ticks observe unchanged filter/event state and 10%
;;    mutate the filter or append an event.  Measures wall elapsed time,
;;    filter predicate evaluations, and Emacs GCs.
;;
;; 3. Generational CSP Preemption (`:supersede t') vs. Unsuperseded Burst
;;    (`csp-unsuperseded' vs. `csp-superseded'):
;;    Simulates B rapid refresh bursts (e.g. Process Manager / Git status
;;    refreshes) where each response callback parses 2,000 lines of tabular
;;    output on Emacs's thread while a 2 ms responsiveness probe timer
;;    runs.  Measures total elapsed time, heavy callback parses executed,
;;    and 2 ms probe timer lateness quantiles (p50, p95, p99, max).
;;
;; The JSON report is printed after the line `BENCH-JSON'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'lispm-ui-frp)
(require 'lispm-ui-list)
(require 'lispm-desktop-frame)
(require 'lispm-desktop-kernel)

(defvar lispm-desktop-kernel-use-exec)
(defvar lispm-exec-function)

(defvar lispm-desktop-bench--interval 0.002
  "Seconds between the responsiveness probe timer's runs.")

(defvar lispm-desktop-bench--samples nil
  "How late each probe timer run was, in milliseconds.")

(defvar lispm-desktop-bench--timer nil
  "The active probe timer while a benchmark case runs.")

;;;; Probe Timer & Quantiles

(defun lispm-desktop-bench--percentile (values fraction)
  "Return the FRACTION quantile of VALUES, a list of numbers, or 0."
  (if (null values)
      0.0
    (let* ((sorted (sort (copy-sequence values) #'<))
           (index (min (1- (length sorted))
                       (floor (* fraction (length sorted))))))
      (float (nth index sorted)))))

(defun lispm-desktop-bench--start-probe ()
  "Start timing how late a timer of `lispm-desktop-bench--interval' runs."
  (setq lispm-desktop-bench--samples nil)
  (let ((expected (+ (float-time) lispm-desktop-bench--interval)))
    (setq lispm-desktop-bench--timer
          (run-at-time lispm-desktop-bench--interval
                       lispm-desktop-bench--interval
                       (lambda ()
                         (let ((now (float-time)))
                           (push (max 0.0 (* 1000.0 (- now expected)))
                                 lispm-desktop-bench--samples)
                           (setq expected (+ expected
                                             lispm-desktop-bench--interval))))))))

(defun lispm-desktop-bench--stop-probe ()
  "Stop the probe timer and return its sample count and lateness quantiles."
  (when lispm-desktop-bench--timer
    (cancel-timer lispm-desktop-bench--timer)
    (setq lispm-desktop-bench--timer nil))
  (let ((values (nreverse lispm-desktop-bench--samples)))
    (setq lispm-desktop-bench--samples nil)
    (list :samples (length values)
          :p50 (lispm-desktop-bench--percentile values 0.50)
          :p95 (lispm-desktop-bench--percentile values 0.95)
          :p99 (lispm-desktop-bench--percentile values 0.99)
          :max (if values (float (apply #'max values)) 0.0))))

(defun lispm-desktop-bench--measure (mode fn)
  "Run FN under GC and wall-clock measurement and return a report plist for MODE."
  (garbage-collect)
  (let ((start (float-time))
        (gcs gcs-done)
        (gc-time gc-elapsed))
    (lispm-desktop-bench--start-probe)
    (let ((extra (unwind-protect
                     (funcall fn)
                   nil))
          (probe (lispm-desktop-bench--stop-probe)))
      (append (list :mode mode
                    :elapsed_ms (* 1000.0 (- (float-time) start))
                    :gcs (- gcs-done gcs)
                    :gc_ms (* 1000.0 (- gc-elapsed gc-time))
                    :probe probe)
              extra))))

;;;; Suite 1: Frame-Synced FRP Batching vs. Unbatched Diamond Propagation

(defun lispm-desktop-bench--synthetic-render (theme glass load battery)
  "Perform synthetic layout/chrome allocation for THEME, GLASS, LOAD, BATTERY."
  (let ((acc 0))
    (dotimes (i 200)
      (setq acc (+ acc (sxhash-equal (list i theme glass load battery)))))
    acc))

(defun lispm-desktop-bench--run-frame-case (ticks batched-p)
  "Run TICKS frame slices with 4 diamond signal clients; batch when BATCHED-P."
  (let* ((s-theme (lispm-ui-signal 'platinum #'eq))
         (s-glass (lispm-ui-signal t #'eq))
         (s-load (lispm-ui-signal "0.10"))
         (s-batt (lispm-ui-signal '(:percent 100 :charging t)))
         (d-appearance (lispm-ui-signal-combine
                        (lambda (th gl) (cons th (and gl 0.72)))
                        (list s-theme s-glass)))
         (d-telemetry (lispm-ui-signal-combine
                       (lambda (ld bt) (list ld (plist-get bt :percent)))
                       (list s-load s-batt)))
         (d-chrome (lispm-ui-signal-combine
                    (lambda (app tel) (list :app app :tel tel))
                    (list d-appearance d-telemetry)))
         (effect-runs 0)
         (checksum 0)
         (eff (lispm-ui-effect
               d-chrome
               (lambda (chrome)
                 (setq effect-runs (1+ effect-runs))
                 (let ((app (plist-get chrome :app))
                       (tel (plist-get chrome :tel)))
                   (setq checksum
                         (logxor checksum
                                 (lispm-desktop-bench--synthetic-render
                                  (car app) (cdr app) (car tel) (cadr tel)))))))))
    (unwind-protect
        (progn
          (dotimes (tick ticks)
            (let ((next-theme (if (cl-evenp tick) 'phosphor 'neon))
                  (next-glass (cl-evenp tick))
                  (next-load (format "%.2f" (* 0.05 (mod tick 40))))
                  (next-batt (list :percent (- 100 (mod tick 90))
                                   :charging (cl-oddp tick))))
              (if batched-p
                  (lispm-ui-frp-batch
                    (lispm-ui-signal-set s-theme next-theme)
                    (lispm-ui-signal-set s-glass next-glass)
                    (lispm-ui-signal-set s-load next-load)
                    (lispm-ui-signal-set s-batt next-batt))
                (lispm-ui-signal-set s-theme next-theme)
                (lispm-ui-signal-set s-glass next-glass)
                (lispm-ui-signal-set s-load next-load)
                (lispm-ui-signal-set s-batt next-batt))))
          (list :ticks ticks
                :work_units effect-runs
                :checksum checksum))
      (lispm-ui-effect-dispose eff))))

;;;; Suite 2: Relational `lispm-ui-list-derive' vs. Imperative Recompute

(defun lispm-desktop-bench--make-events (count)
  "Create COUNT synthetic console events."
  (let ((events nil)
        (severities '(error warning note))
        (sources '("Kernel" "Store" "Updater" "Agent")))
    (dotimes (i count)
      (push (list :id i
                  :severity (nth (mod i 3) severities)
                  :source (nth (mod i 4) sources)
                  :score (mod (* i 17) 100))
            events))
    (nreverse events)))

(defun lispm-desktop-bench--run-list-case (cycles reactive-p)
  "Run CYCLES console table reads over 1,000 events; use FRP when REACTIVE-P."
  (let* ((initial-events (lispm-desktop-bench--make-events 1000))
         (events-sig (lispm-ui-signal initial-events #'equal))
         (filter-sig (lispm-ui-signal '(error . "Kernel") #'equal))
         (sort-sig (lispm-ui-signal 'desc #'eq))
         (evals 0)
         (pred (lambda (ev flt)
                 (setq evals (1+ evals))
                 (and (or (null (car flt))
                          (eq (plist-get ev :severity) (car flt)))
                      (or (null (cdr flt))
                          (equal (plist-get ev :source) (cdr flt))))))
         (comp (lambda (a b dir)
                 (if (eq dir 'desc)
                     (> (plist-get a :score) (plist-get b :score))
                   (< (plist-get a :score) (plist-get b :score)))))
         (derived-sig (and reactive-p
                           (lispm-ui-list-derive
                            events-sig
                            :filter-signal filter-sig
                            :sort-signal sort-sig
                            :predicate pred
                            :comparator comp)))
         (total-rows 0))
    (dotimes (cycle cycles)
      ;; Every 10th cycle changes the filter criteria; the other 9 cycles
      ;; are steady-state redraw/poll checks with unchanged inputs.
      (when (zerop (mod cycle 10))
        (let ((next-flt (if (zerop (mod cycle 20))
                            '(warning . "Store")
                          '(error . "Kernel"))))
          (lispm-ui-signal-set filter-sig next-flt)))
      (let ((rows (if reactive-p
                      (lispm-ui-signal-get derived-sig)
                    (let* ((flt (lispm-ui-signal-get filter-sig))
                           (dir (lispm-ui-signal-get sort-sig))
                           (filtered (seq-filter (lambda (item)
                                                   (funcall pred item flt))
                                                 (lispm-ui-signal-get events-sig))))
                      (sort (copy-sequence filtered)
                            (lambda (x y) (funcall comp x y dir)))))))
        (setq total-rows (+ total-rows (length rows)))))
    (list :ticks cycles
          :work_units evals
          :rows total-rows)))

;;;; Suite 3: Generational CSP Preemption vs. Unsuperseded Burst

(defun lispm-desktop-bench--heavy-parse (payload)
  "Simulate parsing a 2,000-line process/git table PAYLOAD on Emacs's thread."
  (let ((lines (split-string payload "\n" t))
        (count 0))
    (dolist (line lines)
      (let ((fields (split-string line " " t)))
        (setq count (+ count (length fields)))))
    count))

(defun lispm-desktop-bench--make-payload (lines-count)
  "Generate a synthetic table payload with LINES-COUNT lines."
  (mapconcat (lambda (i)
               (format "%droot 12.4 0.8 /opt/lispm/bin/worker-%d --port %d"
                       (+ 1000 i) i (+ 8000 (mod i 100))))
             (number-sequence 1 lines-count)
             "\n"))

(defun lispm-desktop-bench--run-csp-case (burst-size supersede-p)
  "Simulate BURST-SIZE rapid async requests; preempt stale ones when SUPERSEDE-P."
  (let* ((payload (lispm-desktop-bench--make-payload 2000))
         (pending nil)
         (parsed-callbacks 0)
         (total-fields 0)
         (done nil)
         (lispm-desktop-kernel-use-exec nil)
         (lispm-exec-function
          (lambda (_program _args callback &rest _)
            (push callback pending)
            'mock-handle)))
    ;; Issue BURST-SIZE rapid requests separated by brief event-loop turns:
    (dotimes (i burst-size)
      (let ((is-last (= i (1- burst-size))))
        (lispm-desktop-kernel-exec
         "ps" '("-axo" "pid=,user=,pcpu=,pmem=,args=")
         (lambda (exit output _)
           (when (eql exit 0)
             (setq parsed-callbacks (1+ parsed-callbacks))
             (setq total-fields (+ total-fields
                                   (lispm-desktop-bench--heavy-parse output)))
             (when is-last
               (setq done t))))
         :owner 'buffer-manager
         :supersede supersede-p
         :priority 10)))
  ;; Deliver the queued completions across timers so the 2 ms probe timer
    ;; measures main-thread responsiveness during callback bursts:
    (let ((queue (nreverse pending))
          (timers nil)
          (idx 0))
      (unwind-protect
          (progn
            (dolist (cb queue)
              (setq idx (1+ idx))
              (push (run-at-time (* idx 0.004) nil
                                 (lambda (fn)
                                   (funcall fn 0 payload ""))
                                 cb)
                    timers))
            (let ((deadline (+ (float-time) (max 5.0 (+ 2.0 (* burst-size 0.05))))))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.001)))
            (unless (or done (<= burst-size 0))
              (error "CSP benchmark timed out before completing %d burst requests"
                     burst-size)))
        (dolist (tm timers)
          (when (timerp tm)
            (cancel-timer tm)))))
    (list :ticks burst-size
          :work_units parsed-callbacks
          :rows total-fields)))

;;;; Main Entry Point

(defun lispm-desktop-bench-run ()
  "Run all three benchmark suites and print the JSON report after `BENCH-JSON'."
  (let* ((ticks (string-to-number (or (getenv "LISPM_BENCH_TICKS") "500")))
         (cycles (string-to-number (or (getenv "LISPM_BENCH_CYCLES") "1000")))
         (burst (string-to-number (or (getenv "LISPM_BENCH_BURST") "20")))
         (order (or (getenv "LISPM_BENCH_ORDER") "baseline-first"))
         (baseline-first (not (equal order "optimized-first")))
         (results nil))
    (cl-flet ((run-pair (base-mode base-fn opt-mode opt-fn)
                (if baseline-first
                    (progn
                      (push (lispm-desktop-bench--measure base-mode base-fn) results)
                      (push (lispm-desktop-bench--measure opt-mode opt-fn) results))
                  (push (lispm-desktop-bench--measure opt-mode opt-fn) results)
                  (push (lispm-desktop-bench--measure base-mode base-fn) results))))
      (run-pair "frame-unbatched"
                (lambda () (lispm-desktop-bench--run-frame-case ticks nil))
                "frame-batched"
                (lambda () (lispm-desktop-bench--run-frame-case ticks t)))
      (run-pair "list-imperative"
                (lambda () (lispm-desktop-bench--run-list-case cycles nil))
                "list-reactive"
                (lambda () (lispm-desktop-bench--run-list-case cycles t)))
      (run-pair "csp-unsuperseded"
                (lambda () (lispm-desktop-bench--run-csp-case burst nil))
                "csp-superseded"
                (lambda () (lispm-desktop-bench--run-csp-case burst t))))
    (let ((report (list :environment (list :emacs_version emacs-version
                                           :system_type (symbol-name system-type)
                                           :order order
                                           :ticks ticks
                                           :cycles cycles
                                           :burst burst)
                        :results (vconcat (nreverse results)))))
      (princ "BENCH-JSON\n")
      (princ (json-encode report))
      (princ "\n"))))

(defun lispm-desktop-bench-main ()
  "Run the benchmark automatically when loaded as the top-level batch script."
  (when (and noninteractive
             (not (bound-and-true-p lispm-desktop-bench-inhibit-autorun)))
    (lispm-desktop-bench-run)))

(lispm-desktop-bench-main)

(provide 'frp-csp-benchmark)
;;; frp-csp-benchmark.el ends here
