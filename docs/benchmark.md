# Frame-Synced FRP & Generational CSP Kernel Benchmark

The v0.25.0 refactoring unified `lispm-desktop`'s 60 Hz frame clock with
`lispm-ui`'s glitch-free push-pull FRP runtime (`1 Frame Tick ≡ 1 Logical
Instant`), moved relational list derivation (`lispm-ui-list-derive`) into
`lispm-ui`, and introduced subsystem-isolated generational CSP kernel channels
(`lispm-desktop-kernel-exec` with `:owner` and `:supersede`).

This benchmark measures all three architectural dimensions in paired A/B batch
Emacs runs (`make bench`), reporting wall-clock elapsed time, work units
executed (downstream effect renders, list predicate evaluations, or callback
parses), Emacs GC counts/time, and 2 ms responsiveness probe timer lateness
quantiles.

## What is measured

1. **Suite 1 — Frame-Synced FRP Batching vs. Unbatched Diamond Propagation
   (`frame-unbatched` vs. `frame-batched`)**:
   - **Workload.** 500 frame ticks. In each tick, four frame-clock clients
     update four source signals (`theme`, `glass`, `load`, `battery`) that feed
     two intermediate derived signals (`appearance`, `telemetry`) and a
     combined `chrome` signal observed by a downstream layout/render effect.
   - **Invariant tested.** Without frame-tick batching, updating 4 source
     signals across a single frame slice fires the downstream render effect 4
     times per frame (with transient intermediate states). Wrapping the frame
     slice in `lispm-ui-frp-batch` (`lispm-desktop-frame--tick`) coalesces the
     diamond graph so the downstream effect runs exactly once per frame slice.

2. **Suite 2 — Relational `lispm-ui-list-derive` Cutoff vs. Imperative Recompute
   (`list-imperative` vs. `list-reactive`)**:
   - **Workload.** 1,000 UI poll/redraw cycles over a 1,000-entry Console event
     log with severity/source filtering and score sorting. Every 10th cycle
     mutates the filter criteria (`(error . "Kernel")` ↔ `(warning . "Store")`);
     the remaining 90% of cycles represent steady-state UI queries where inputs
     have not changed.
   - **Invariant tested.** Imperative table rendering re-filters and re-sorts
     all 1,000 items on every cycle (1,000,000 predicate evaluations).
     `lispm-ui-list-derive` memoizes the derived signal in the push-pull DAG
     and recomputes only when `items-signal`, `filter-signal`, or `sort-signal`
     actually changes.

3. **Suite 3 — Generational CSP Preemption (`:supersede t`) vs. Unsuperseded
   Burst (`csp-unsuperseded` vs. `csp-superseded`)**:
   - **Workload.** A burst of 20 rapid async refresh requests for the same
     subsystem owner (`:owner 'buffer-manager`), each returning a 2,000-line
     process table parsed on Emacs's thread while a 2 ms responsiveness probe
     timer runs.
   - **Invariant tested.** Without generational superseding, all 20 completions
     parse their 2,000-line payload on Emacs's main thread, stalling the 2 ms
     probe timer and triggering multiple GCs. With `:supersede t`,
     `lispm-desktop-kernel-exec` advances the owner's generation on each new
     request and drops stale in-flight completions in $O(1)$ before parsing.

4. **Suite 4 — Floating Window Frame Pooling & Finder Render Deduplication
   (`window-recreate` vs. `window-pooled`)**:
   - **Workload.** 20 window close/reopen cycles over an 80-item Finder folder
     where each window open invokes `lispm-finder--render` 4 times (initial
     `lispm-finder--go`, `lispm-desktop-finder-open-directory`, and 2
     `window-size-change-functions` callbacks as child and outer frames settle).
   - **Invariant tested.** Without frame pooling and render deduplication,
     closing and reopening a window destroys and recreates the frame 20 times
     and rebuilds the Finder buffer 80 times (100 total rebuilds). With
     `lispm-desktop-app-window-pool`, `lispm-finder--rendered-key`,
     `lispm-finder--icon-names`, and `lispm-finder--fit-cache`, the pooled frame
     is reused without `make-frame` (`0` frame allocations) and the Finder
     buffer renders once (`1` work unit vs. `100`).

- **Repetition & Order Alternation.** Four batch Emacs runs (`BENCH_REPEATS=4`),
  alternating `baseline-first` and `optimized-first` so neither variant benefits
  from warm allocator state more often. All figures below are the medians across
  the 4 runs.
- **Host.** Apple M1 Max (10 cores), macOS 27.0.1, GNU Emacs 31.1.

## Results

Medians of 4 runs, milliseconds. Raw data is recorded in
[frp-csp-benchmark.json](frp-csp-benchmark.json).

| mode | elapsed (ms) | work units | p50 (ms) | p95 (ms) | p99 (ms) | max (ms) | Emacs GCs | GC time (ms) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `frame-unbatched` | 202.87 | 1,998 renders | 0.00 | 0.00 | 0.00 | 0.00 | 4.0 | 53.30 |
| `frame-batched` | **57.59** | **500 renders** | 0.00 | 0.00 | 0.00 | 0.00 | **1.0** | **13.56** |
| `list-imperative` | 679.65 | 1,000,000 predicates | 0.00 | 0.00 | 0.00 | 0.00 | 13.0 | 175.60 |
| `list-reactive` | **66.31** | **101,000 predicates** | 0.00 | 0.00 | 0.00 | 0.00 | **1.0** | **13.21** |
| `csp-unsuperseded` | 93.20 | 20 callback parses | 3.21 | 13.83 | 17.55 | 17.55 | 1.0 | 13.38 |
| `csp-superseded` | **85.76** | **1 callback parse** | **0.11** | **0.30** | **3.12** | **3.12** | **0.0** | **0.00** |
| `window-recreate` | 676.61 | 100 frame/buffer rebuilds | 0.00 | 0.00 | 0.00 | 0.00 | 7.0 | 102.77 |
| `window-pooled` | **90.26** | **1 buffer build (0 frame allocs)** | 0.00 | 0.00 | 0.00 | 0.00 | **0.0** | **0.00** |

### Summary of Speedups

| Architectural Mechanism | Wall-Clock Speedup | Work Reduction | GC Reduction | Responsiveness (p95 Lateness) |
|---|---:|---:|---:|---:|
| **Frame-Synced FRP Batching** (`lispm-desktop-frame--tick`) | **3.52x faster** (202.9 ms → 57.6 ms) | **4.0x fewer** renders (1,998 → 500) | **4.0x fewer GCs** (53.3 ms → 13.6 ms) | Glitch-free single commit per frame |
| **Relational List Derivation** (`lispm-ui-list-derive`) | **10.25x faster** (679.7 ms → 66.3 ms) | **9.9x fewer** predicate evaluations | **13.0x fewer GCs** (175.6 ms → 13.2 ms) | $O(1)$ signal read when unchanged |
| **Generational CSP Preemption** (`lispm-desktop-kernel-exec :supersede t`) | **1.09x faster** (93.2 ms → 85.8 ms) | **20.0x fewer** callback parses (20 → 1) | **Eliminated GCs** (1.0 → 0.0) | **46.56x lower p95 wait** (13.83 ms → 0.30 ms) |
| **Window Frame Pool & Finder Render Dedup** (`lispm-desktop-app-window-pool`) | **7.50x faster** in batch (**13x–46x faster** in live AppKit GUI: 220–430 ms → 7–16 ms) | **100.0x fewer** frame/buffer rebuilds (100 → 1) | **Eliminated GCs** (7.0 → 0.0) | Zero `NSWindow` teardown/recreate on reopen |

## Reproducing

Run the full benchmark suite from the repository root:

```sh
make bench
```

Or customize the repetition count and workload sizes:

```sh
python3 scripts/bench-frp-csp.py --repeats 4 --ticks 500 --cycles 1000 --burst 20 \
  --output docs/frp-csp-benchmark.json
```
