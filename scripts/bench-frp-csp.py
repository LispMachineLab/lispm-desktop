#!/usr/bin/env python3
# Copyright (C) 2026  Ryan.K
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Run the synthetic Frame-Synced FRP & Generational CSP benchmark, repeat and aggregate it.

Each repetition runs one batch Emacs process executing three paired A/B suites:
1. Frame-Synced FRP Batching vs. Unbatched Diamond Propagation
   (`frame-unbatched` vs. `frame-batched`)
2. Relational `lispm-ui-list-derive` Cutoff vs. Imperative Recompute
   (`list-imperative` vs. `list-reactive`)
3. Generational CSP Preemption (`:supersede t`) vs. Unsuperseded Burst
   (`csp-unsuperseded` vs. `csp-superseded`)

Repetitions alternate `baseline-first` and `optimized-first` in pairs so
neither variant benefits from warm allocator/JIT caches more often.
"""

import argparse
import json
import os
from pathlib import Path
import platform
import resource
import statistics
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parent.parent
MODES = (
    "frame-unbatched",
    "frame-batched",
    "list-imperative",
    "list-reactive",
    "csp-unsuperseded",
    "csp-superseded",
    "window-recreate",
    "window-pooled",
)


def existing_directory(value, what):
    path = Path(value).expanduser().resolve()
    if not path.is_dir():
        sys.exit(f"No {what} at {path}")
    return path


def emacs_paths(args):
    ui = existing_directory(
        args.ui or os.environ.get("LISPM_UI") or ROOT.parent / "lispm-ui",
        "LispM UI checkout",
    )
    agent_default = ROOT.parent / "lispm-agent" / "lisp"
    agent = Path(args.agent or os.environ.get("LISPM_AGENT") or agent_default).expanduser().resolve()
    load_path = [ROOT / "lisp", ui]
    if agent.is_dir():
        load_path.append(agent)
    kernel = (ROOT.parent / "lispm-kernel" / "lisp").resolve()
    if kernel.is_dir():
        load_path.append(kernel)
    return [str(p) for p in load_path]


def run_once(args, load_path, order):
    env = dict(
        os.environ,
        LISPM_BENCH_TICKS=str(args.ticks),
        LISPM_BENCH_CYCLES=str(args.cycles),
        LISPM_BENCH_BURST=str(args.burst),
        LISPM_BENCH_ORDER=order,
    )
    command = [args.emacs, "-Q", "--batch"]
    for path in load_path:
        command += ["-L", path]
    command += ["-l", str(ROOT / "bench" / "frp-csp-benchmark.el")]
    before = resource.getrusage(resource.RUSAGE_CHILDREN)
    started = time.time()
    completed = subprocess.run(
        command,
        env=env,
        cwd=ROOT,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        timeout=args.timeout,
    )
    elapsed = time.time() - started
    after = resource.getrusage(resource.RUSAGE_CHILDREN)
    if completed.returncode != 0:
        sys.stderr.write(completed.stderr[-4000:])
        sys.exit(f"Emacs exited {completed.returncode} for order {order}")
    lines = completed.stdout.splitlines()
    if "BENCH-JSON" not in lines:
        sys.stderr.write(completed.stdout[-2000:])
        sys.stderr.write(completed.stderr[-4000:])
        sys.exit(f"No BENCH-JSON in Emacs output for order {order}")
    report = json.loads(lines[lines.index("BENCH-JSON") + 1])
    report["process_resources"] = {
        "wall_seconds": round(elapsed, 3),
        "user_cpu_seconds": round(after.ru_utime - before.ru_utime, 3),
        "system_cpu_seconds": round(after.ru_stime - before.ru_stime, 3),
    }
    return report


def sysctl(key):
    if platform.system() != "Darwin":
        return None
    try:
        completed = subprocess.run(
            ["sysctl", "-n", key],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        )
    except OSError:
        return None
    value = completed.stdout.strip()
    return value or None


def host_info():
    arm64 = sysctl("hw.optional.arm64") == "1"
    return {
        "system": platform.platform(),
        "machine": "arm64" if arm64 else platform.machine(),
        "processor": sysctl("machdep.cpu.brand_string"),
        "cpus": os.cpu_count(),
        "python": platform.python_version(),
    }


def median(values):
    return statistics.median(values) if values else None


def summarize(runs):
    summary = {}
    for mode in MODES:
        cases = [next(r for r in run["results"] if r["mode"] == mode) for run in runs]
        summary[mode] = {
            "elapsed_ms": round(median([c["elapsed_ms"] for c in cases]), 3),
            "work_units": int(median([c["work_units"] for c in cases])),
            "p50_ms": round(median([c["probe"]["p50"] for c in cases]), 3),
            "p95_ms": round(median([c["probe"]["p95"] for c in cases]), 3),
            "p99_ms": round(median([c["probe"]["p99"] for c in cases]), 3),
            "max_ms": round(median([c["probe"]["max"] for c in cases]), 3),
            "gcs": round(median([c["gcs"] for c in cases]), 3),
            "gc_ms": round(median([c["gc_ms"] for c in cases]), 3),
        }
    return summary


def comparison(summary):
    def ratio(base, opt, key="elapsed_ms"):
        if not summary[opt][key] or not summary[base][key]:
            return None
        return round(summary[base][key] / summary[opt][key], 2)

    return {
        "frame_batch_speedup": ratio("frame-unbatched", "frame-batched"),
        "frame_effect_reduction": ratio("frame-unbatched", "frame-batched", "work_units"),
        "list_derive_speedup": ratio("list-imperative", "list-reactive"),
        "list_predicate_reduction": ratio("list-imperative", "list-reactive", "work_units"),
        "csp_supersede_speedup": ratio("csp-unsuperseded", "csp-superseded"),
        "csp_parse_reduction": ratio("csp-unsuperseded", "csp-superseded", "work_units"),
        "csp_p95_lateness_reduction": ratio("csp-unsuperseded", "csp-superseded", "p95_ms"),
        "window_pool_speedup": ratio("window-recreate", "window-pooled"),
        "window_work_reduction": ratio("window-recreate", "window-pooled", "work_units"),
    }


def print_table(summary, comp):
    print(
        f"{'mode':<18} {'elapsed(ms)':>12} {'work_units':>12} "
        f"{'p50':>8} {'p95':>8} {'p99':>8} {'max':>8} {'GCs':>6}"
    )
    for mode in MODES:
        row = summary[mode]
        print(
            f"{mode:<18} {row['elapsed_ms']:>12.2f} {row['work_units']:>12d} "
            f"{row['p50_ms']:>8.2f} {row['p95_ms']:>8.2f} "
            f"{row['p99_ms']:>8.2f} {row['max_ms']:>8.2f} {row['gcs']:>6.1f}"
        )
    print(
        f"\nSpeedups (baseline / refactored):\n"
        f"  1. Frame-Synced FRP Batching : {comp['frame_batch_speedup']}x faster "
        f"({comp['frame_effect_reduction']}x fewer effect renders)\n"
        f"  2. Relational list-derive    : {comp['list_derive_speedup']}x faster "
        f"({comp['list_predicate_reduction']}x fewer predicate evaluations)\n"
        f"  3. Generational CSP Supersede: {comp['csp_supersede_speedup']}x faster "
        f"({comp['csp_parse_reduction']}x fewer callback parses, "
        f"{comp['csp_p95_lateness_reduction']}x lower p95 timer lateness)\n"
        f"  4. Window Pool & Finder Dedup: {comp['window_pool_speedup']}x faster "
        f"({comp['window_work_reduction']}x fewer frame/buffer rebuilds)"
    )


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repeats", type=int, default=4)
    parser.add_argument("--ticks", type=int, default=500)
    parser.add_argument("--cycles", type=int, default=1000)
    parser.add_argument("--burst", type=int, default=20)
    parser.add_argument(
        "--output",
        type=Path,
        default=ROOT / "docs" / "frp-csp-benchmark.json",
    )
    parser.add_argument("--emacs", default=os.environ.get("EMACS", "emacs"))
    parser.add_argument("--ui", help="LispM UI checkout")
    parser.add_argument("--agent", help="lispm-agent lisp directory")
    parser.add_argument("--timeout", type=int, default=180)
    return parser.parse_args()


def main():
    args = parse_args()
    if args.repeats < 2 or args.repeats % 2:
        sys.exit("--repeats must be an even number of at least 2")
    load_path = emacs_paths(args)
    runs = []
    for index in range(args.repeats):
        order = "baseline-first" if index % 2 == 0 else "optimized-first"
        print(f"Run {index + 1}/{args.repeats} ({order}) ...", file=sys.stderr)
        runs.append(run_once(args, load_path, order))
    summary = summarize(runs)
    comp = comparison(summary)
    report = {
        "generated_at": time.strftime("%Y-%m-%dT%H:%M:%S%z"),
        "repeats": args.repeats,
        "host": host_info(),
        "orders": [
            "baseline-first" if i % 2 == 0 else "optimized-first"
            for i in range(args.repeats)
        ],
        "environment": runs[0]["environment"],
        "summary": summary,
        "comparison": comp,
        "runs": runs,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print_table(summary, comp)
    print(f"wrote {args.output}")


if __name__ == "__main__":
    main()
