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
| `frame-unbatched` | 276.21 | 1,998 renders | 0.00 | 0.00 | 0.00 | 0.00 | 5.0 | 79.61 |
| `frame-batched` | **59.05** | **500 renders** | 0.00 | 0.00 | 0.00 | 0.00 | **1.0** | **14.25** |
| `list-imperative` | 820.16 | 1,000,000 predicates | 0.00 | 0.00 | 0.00 | 0.00 | 17.0 | 249.37 |
| `list-reactive` | **71.69** | **101,000 predicates** | 0.00 | 0.00 | 0.00 | 0.00 | **1.0** | **13.59** |
| `csp-unsuperseded` | 106.46 | 20 callback parses | 3.51 | 22.18 | 24.74 | 24.74 | 2.0 | 27.17 |
| `csp-superseded` | **86.23** | **1 callback parse** | **0.15** | **0.31** | **2.84** | **2.84** | **0.0** | **0.00** |

### Summary of Speedups

| Architectural Mechanism | Wall-Clock Speedup | Work Reduction | GC Reduction | Responsiveness (p95 Lateness) |
|---|---:|---:|---:|---:|
| **Frame-Synced FRP Batching** (`lispm-desktop-frame--tick`) | **4.68x faster** (276.2 ms → 59.0 ms) | **4.0x fewer** renders (1,998 → 500) | **5.0x fewer GCs** (79.6 ms → 14.3 ms) | Glitch-free single commit per frame |
| **Relational List Derivation** (`lispm-ui-list-derive`) | **11.44x faster** (820.2 ms → 71.7 ms) | **9.9x fewer** predicate evaluations | **17.0x fewer GCs** (249.4 ms → 13.6 ms) | $O(1)$ signal read when unchanged |
| **Generational CSP Preemption** (`lispm-desktop-kernel-exec :supersede t`) | **1.23x faster** (106.5 ms → 86.2 ms) | **20.0x fewer** callback parses (20 → 1) | **Eliminated GCs** (2.0 → 0.0) | **72.71x lower p95 wait** (22.18 ms → 0.31 ms) |

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
