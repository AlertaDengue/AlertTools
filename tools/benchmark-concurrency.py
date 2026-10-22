"""Measure isolated R runs with Python's standard library and Unix ps."""
import csv
import hashlib
import itertools
import pathlib
import subprocess
import sys
import tempfile
import time


def rss_tree(pid):
    listing = subprocess.check_output(
        ["ps", "-axo", "pid=,ppid=,rss="], text=True
    )
    processes = [tuple(map(int, line.split())) for line in listing.splitlines()]
    descendants = {pid}
    while True:
        expanded = descendants | {p for p, parent, _ in processes if parent in descendants}
        if expanded == descendants:
            break
        descendants = expanded
    return sum(rss * 1024 for p, _, rss in processes if p in descendants)


results = []
source = hashlib.sha256()
for path in sorted(pathlib.Path("R").glob("*.R")):
    source.update(str(path).encode())
    source.update(path.read_bytes())
for workers, threads, repetition in itertools.product([1, 2], [1, 2], range(1, 4)):
    with tempfile.TemporaryDirectory(prefix="alerttools-benchmark-") as directory:
        directory = pathlib.Path(directory)
        artifact = directory / "result.csv"
        with (directory / "log").open("w+") as log:
            process = subprocess.Popen(
                ["Rscript", "tools/benchmark-concurrency.R", "--worker",
                 str(workers), str(threads), str(artifact)], stdout=log, stderr=log
            )
            peak = 0
            try:
                while process.poll() is None:
                    peak = max(peak, rss_tree(process.pid))
                    time.sleep(0.05)
            except BaseException:
                if process.poll() is None:
                    process.terminate()
                process.wait()
                raise
            if process.returncode:
                log.seek(0)
                raise RuntimeError(log.read())
        with artifact.open() as stream:
            measured = next(csv.DictReader(stream))
        results.append(dict(municipality_workers=workers, inla_threads=threads,
                            repetition=repetition, mode="none", seed=42,
                            source_sha256=source.hexdigest(),
                            aggregate_peak_rss_bytes=peak, **measured))

with open(sys.argv[1], "w", newline="") as stream:
    writer = csv.DictWriter(stream, fieldnames=results[0].keys())
    writer.writeheader()
    writer.writerows(results)
