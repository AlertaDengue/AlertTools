"""Bounded real-INLA benchmark. Run from the repository root, on host or in Docker."""
import argparse
import csv
import hashlib
import itertools
import json
import os
import pathlib
import signal
import subprocess
import time


def rss_tree(roots):
    listing = subprocess.check_output(["ps", "-axo", "pid=,ppid=,rss="], text=True)
    processes = [tuple(map(int, line.split())) for line in listing.splitlines()]
    descendants = set(roots)
    while True:
        expanded = descendants | {p for p, parent, _ in processes if parent in descendants}
        if expanded == descendants:
            break
        descendants = expanded
    return sum(rss * 1024 for p, _, rss in processes if p in descendants)


def terminate(process):
    # Also clean up forked municipal workers and the external INLA engine.
    try:
        os.killpg(process.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    try:
        process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.wait()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=pathlib.Path)
    parser.add_argument("--environment", choices=["host", "docker"], required=True)
    parser.add_argument("--repetitions", type=int, default=3)
    parser.add_argument("--timeout", type=float, default=300)
    parser.add_argument("--max-rss-mib", type=float, default=2048)
    parser.add_argument("--quick", action="store_true", help="one repetition; still all eight configurations")
    args = parser.parse_args()
    if args.repetitions < 1 or args.timeout <= 0 or args.max_rss_mib <= 0:
        parser.error("repetitions, timeout and memory budget must be positive")
    args.output.mkdir(parents=True, exist_ok=True)
    if (args.output / "runs.csv").exists():
        parser.error("output already contains a run; use a new directory to preserve evidence")
    source = hashlib.sha256()
    files = sorted(pathlib.Path("R").glob("*.R")) + [
        pathlib.Path("tests/reference/inla-validation.R"), pathlib.Path(__file__),
        pathlib.Path("tools/inla-benchmark-worker.R")]
    for path in files:
        source.update(str(path).encode())
        source.update(path.read_bytes())
    manifest = dict(environment=args.environment, source_sha256=source.hexdigest(),
                    sampling_seconds=0.1, memory_limit_mib=args.max_rss_mib,
                    timeout_seconds=args.timeout, comparison_absolute_tolerance=1e-7,
                    comparison_scope="repetition and municipal/state concurrency at fixed fitting threads",
                    rss_limitation="aggregate sampled RSS can double-count shared pages; not a private-memory peak")
    (args.output / "protocol.json").write_text(json.dumps(manifest, indent=2) + "\n")
    environment = dict(os.environ, OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1", OMP_NUM_THREADS="1")
    rows = []
    repetitions = 1 if args.quick else args.repetitions
    for states, workers, threads, repetition in itertools.product([1, 2], [1, 2], [1, 2], range(1, repetitions + 1)):
        name = f"s{states}-w{workers}-t{threads}-r{repetition}"
        directory = args.output / name
        directory.mkdir()
        processes, logs = [], []
        started, peak = time.monotonic(), 0
        try:
            for state in range(1, states + 1):
                log = (directory / f"state-{state}.log").open("w")
                logs.append(log)
                processes.append(subprocess.Popen([
                    "Rscript", "tools/inla-benchmark-worker.R", str(workers), str(threads),
                    str(directory / f"state-{state}.rds"), str(directory / f"state-{state}.csv")],
                    stdout=log, stderr=log, env=environment, start_new_session=True))
            while any(p.poll() is None for p in processes):
                peak = max(peak, rss_tree([p.pid for p in processes]))
                if time.monotonic() - started > args.timeout:
                    raise RuntimeError(f"{name}: time budget exceeded")
                if peak > args.max_rss_mib * 1024 * 1024:
                    raise RuntimeError(f"{name}: aggregate RSS budget exceeded")
                if any(p.poll() not in (None, 0) for p in processes):
                    raise RuntimeError(f"{name}: worker failed; inspect its log")
                time.sleep(0.1)
            if any(p.returncode for p in processes):
                raise RuntimeError(f"{name}: worker failed; inspect its log")
        except BaseException as exc:
            (directory / "failure.txt").write_text(str(exc) + "\n")
            raise
        finally:
            for process in processes:
                # Clean descendants even when the leader has already exited.
                terminate(process)
            for log in logs:
                log.close()
        row = dict(directory=name, states=states, workers=workers, threads=threads,
                   repetition=repetition, environment=args.environment, exit_code=0,
                   wall_seconds=time.monotonic() - started, aggregate_peak_rss_bytes=peak)
        rows.append(row)
        with (args.output / "runs.csv").open("w", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=row.keys())
            writer.writeheader()
            writer.writerows(rows)
        print(json.dumps(row), flush=True)
    subprocess.run(["Rscript", "tools/inla-benchmark-worker.R", "--compare", str(args.output)],
                   env=environment, check=True)
    for name in ("memory.peak", "memory.events", "cpu.max", "memory.max"):
        file = pathlib.Path("/sys/fs/cgroup") / name
        if file.exists():
            # Scope is the whole benchmark container, not a single configuration.
            (args.output / ("container-lifetime-" + name + ".txt")).write_text(file.read_text())


if __name__ == "__main__":
    main()
