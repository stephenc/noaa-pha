#!/usr/bin/env python3
"""Falsify a parameter value against the sentinel stations.

WHY THIS TEST EXISTS

The composite mean is a weak instrument. It averages 27,793 stations, and most
of them carry noise. A wrong parameter thus shows only as a small change in a
soft number. You can always argue about a small change.

This test gives a result that is almost binary. It does not ask which value
scores best. It asks which values are impossible.

WHAT A SENTINEL IS

A sentinel is a station with two properties:

  1. It reproduces the published QCF exactly. Its composite score is 1.0.
  2. No station within N hops of it fails to do the same.

Property 2 does the work. PHA adjusts each station against its 40 neighbours.
The result for a station is therefore a function of its own data and of the
data of its neighbours. Everything that reaches a sentinel already reproduces
NOAA exactly. Under the correct parameters the sentinel must stay exact. If a
parameter change moves a sentinel, that parameter is not the one NOAA used.

You do not need to explain how the change happened. You need only the fact that
it happened.

DIRECTION OF THE GRAPH

Influence moves from a neighbour INTO the station that lists it. If station N
appears in the neighbour list of station S, then N informs S. This tool
therefore measures hop distance on the REVERSED neighbour graph. It is easy to
get this backwards, and the result then looks plausible but means nothing.

TWO CONDITIONS THAT MUST HOLD

The test is void if either condition fails:

  * Every arm uses the same TOB data and the same history data. Only the
    parameter differs.
  * Every arm uses the same neighbour file. The sentinel set is then identical
    under each probe, instead of being derived again from moved correlations.

If a probe must rebuild the network, add a control arm at the shipped values.
The control must break no sentinel. If it does, the rebuild itself perturbs the
result, and no other arm can be read.

WHAT TO COUNT

Count a change to the CHANGEPOINT SET. Do not count a change to the composite
score alone.

A perturbation of about 0.01 degC travels roughly 3 hops through the network. It
pushes a composite below 1.0 while the changepoint set stays identical. That is
numerical cascade, not a different decision by PHA. A perturbation of that size
cannot move a changepoint into another month, and it cannot change how many
changepoints there are. Half of the apparent breaks for one probe were of this
kind. Counting them overstates the evidence.

WHAT A NULL RESULT MEANS

A probe that breaks no sentinel is not thereby correct. The test falsifies; it
does not confirm. The logic runs in one direction only.

HOW THE SENTINEL SET WAS OBTAINED

The set is not free. It exists only because the reconstruction is already good.
You must first recover the TOB adjustments and the station histories, because
without them almost nothing reproduces QCF exactly and there is no sentinel to
test with. You must then improve the fit over several rounds. Each round adds
exactly-reproduced stations, and the clean region around them grows.

The count is a direct measure of that progress:

    composite 0.8281 ->   377 sentinels (at 3 hops)
    composite 0.8421 -> 2,325 sentinels (at 5 hops)

The better reconstruction gave six times as many sentinels at a stricter depth.
Below about 0.8 there are too few sentinels to attempt this test at all. Do not
expect to run this tool early in a reconstruction.

Usage:
    core_test.py --baseline perstation_best.tsv \
        --neighbors shared/neighbor-correlation.txt \
        --probe alt=perstation_alt.tsv [--probe ...] [--hops 5]
"""

from __future__ import annotations

import argparse
from collections import deque
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

FIELD = 12  # fixed-width column stride in the neighbour files


def read_neighbors(path: str) -> Dict[str, List[str]]:
    """station id -> its neighbour ids, self excluded.

    The file holds three lines for each station: ids, indices and correlations.
    Each line holds fixed 12-character fields. Field 0 is the station itself.
    """
    out: Dict[str, List[str]] = {}
    with open(path) as fh:
        while True:
            ids = fh.readline()
            if not ids:
                break
            fh.readline()  # indices
            fh.readline()  # correlations
            row = [ids[i:i + FIELD].strip()
                   for i in range(0, len(ids.rstrip("\n")), FIELD)]
            row = [r for r in row if r]
            if row:
                out[row[0]] = [r for r in row[1:] if r != row[0]]
    return out


def read_scores(path: str) -> Dict[str, float]:
    out: Dict[str, float] = {}
    with open(path) as fh:
        header = fh.readline().rstrip("\n").split("\t")
        ci = header.index("composite")
        for line in fh:
            f = line.rstrip("\n").split("\t")
            out[f[0]] = float(f[ci])
    return out


def read_transitions(path: str) -> Dict[str, Tuple[int, ...]]:
    """station id -> (n_target, n_matched, n_missing, n_extra).

    This is the falsification-grade signal. See the module docstring: a
    numerical cascade cannot alter these four counts, but a different decision
    by PHA does alter them.
    """
    out: Dict[str, Tuple[int, ...]] = {}
    with open(path) as fh:
        header = fh.readline().rstrip("\n").split("\t")
        idx = [header.index(c)
               for c in ("n_target", "n_matched", "n_missing", "n_extra")]
        for line in fh:
            f = line.rstrip("\n").split("\t")
            out[f[0]] = tuple(int(f[i]) for i in idx)
    return out


def sentinel_set(base: Dict[str, float], nbr: Dict[str, List[str]],
                 hops: int) -> Set[str]:
    """Stations that are exact, and that no inexact station reaches in `hops`."""
    ids = set(base)
    perfect = {s for s in ids if base[s] >= 1.0}
    # Reverse the edges: N -> S for every N in the neighbour list of S.
    infl: Dict[str, List[str]] = {s: [] for s in ids}
    for s in ids:
        for n in nbr.get(s, ()):
            if n in infl:
                infl[n].append(s)
    dist: Dict[str, int] = {s: 0 for s in ids if s not in perfect}
    q = deque(dist)
    while q:
        v = q.popleft()
        for w in infl[v]:
            if w not in dist:
                dist[w] = dist[v] + 1
                q.append(w)
    return {s for s in perfect if dist.get(s, 10 ** 9) >= hops}


def main(argv: Optional[List[str]] = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--baseline", required=True,
                    help="per-station scores for ALL stations at the best "
                         "configuration; hop distance needs the full set")
    ap.add_argument("--neighbors", required=True)
    ap.add_argument("--probe", action="append", default=[], metavar="NAME=TSV")
    ap.add_argument("--probe-dir", default=None,
                    help="directory of perstation_<name>.tsv files to test")
    ap.add_argument("--hops", type=int, default=5,
                    help="insulation depth (default 5; a perturbation travels "
                         "about 3 hops, so 5 leaves a margin of 2)")
    ap.add_argument("--strict-hops", type=int, default=6,
                    help="deeper stratum, reported beside --hops")
    args = ap.parse_args(argv)

    base = read_scores(args.baseline)
    base_tr = read_transitions(args.baseline)
    nbr = read_neighbors(args.neighbors)
    core = sentinel_set(base, nbr, args.hops)
    strict = sentinel_set(base, nbr, args.strict_hops)

    perfect = sum(1 for v in base.values() if v >= 1.0)
    print("# baseline: %d stations, %d exact" % (len(base), perfect))
    print("# sentinels >=%d hops: %d   |   >=%d hops: %d"
          % (args.hops, len(core), args.strict_hops, len(strict)))
    if not core:
        print("# no sentinels -- the reconstruction is not yet good enough")
        return 1

    specs = [(n, p) for n, _, p in (s.partition("=") for s in args.probe)]
    if args.probe_dir:
        for p in sorted(Path(args.probe_dir).glob("perstation_*.tsv")):
            specs.append((p.stem[len("perstation_"):], str(p)))

    rows = []
    for name, path in specs:
        try:
            sc = read_scores(path)
            tr = read_transitions(path)
        except OSError:
            continue
        broke = [s for s in core if s in sc and sc[s] < 1.0]
        moved = [s for s in broke if tr.get(s) != base_tr.get(s)]
        drops = [base[s] - sc[s] for s in broke]
        rows.append((len(moved), name, len(broke),
                     sum(1 for s in moved if s in strict),
                     sum(drops) / len(drops) if drops else 0.0,
                     len([s for s in core if s in sc])))

    # A probe with no output scores zero stations. Without this split it looks
    # the same as a probe that broke nothing. That is the difference between no
    # evidence and evidence of no effect, so the two never merge.
    nodata = [r for r in rows if r[5] == 0]
    rows = [r for r in rows if r[5] > 0]
    rows.sort()

    print("\nprobe\tchangepoint_set_differs_of_%d\tany_break\tat_>=%dhop\tmean_drop"
          % (len(core), args.strict_hops))
    for n, name, nb, ns, mean, _scored in rows:
        print("%s\t%d\t%d\t%d\t%s"
              % (name, n, nb, ns, "%.4f" % mean if nb else "-"))

    if nodata:
        print("\n# NO DATA (absent or incomplete output): %s"
              % ", ".join(sorted(r[1] for r in nodata)))
    clean = [r[1] for r in rows if r[0] == 0]
    print("# probes that move no sentinel: %d%s"
          % (len(clean), (" -- " + ", ".join(clean)) if clean else ""))
    print("# probes that move at least one: %d of %d"
          % (sum(1 for r in rows if r[0] > 0), len(rows)))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
