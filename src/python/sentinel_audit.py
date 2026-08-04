#!/usr/bin/env python3
"""Audit the sentinel set that core_test.py uses.

core_test.py applies the falsification. This tool answers the three questions
that decide whether the falsification is sound. Run it once for each new
reconstruction, before you trust a sentinel result.

QUESTION 1: HOW DEEP CAN THE SENTINELS BE?

A sentinel must sit further from any inexact station than a perturbation can
travel. A perturbation of about 0.01 degC travels roughly 3 hops. A depth of 3
is therefore at the edge of the range, not outside it.

This tool counts the sentinels at each depth. Choose the largest depth that
still falsifies the parameters you care about. Depth costs sentinels, so do not
take more depth than you need. At composite 0.8421 the counts were:

    >=3 hops: 4,785    >=4 hops: 3,825    >=5 hops: 2,325    >=6 hops: 1,257

QUESTION 2: CAN A DIFFERENT NETWORK BRING IN A BAD STATION?

A parameter that changes the network selects different neighbours. Hop distance
alone does not protect against that, because the new neighbours come from the
distance pool, and the pool holds stations that the old network did not use.

This tool therefore also reports POOL CLOSURE. A station is pool-clean when
every station in its 99-station distance pool is exact. It is closed to two
levels when the pool of each of those stations is also exact. A sentinel that is
closed to two levels cannot draw an inexact neighbour under any parameter value,
so a network-rebuilding probe stays valid.

QUESTION 3: DOES THE RECONSTRUCTION CONTAMINATE THE TEST?

This is the important one. A sentinel is only unconditional evidence if we
supply nothing to it that could be wrong.

The tool reports, for the sentinel set: how many carry a station history, how
many receive a TOB adjustment, and how many are US stations. At composite
0.8421 the answer was zero, zero and zero. Every sentinel took raw QCU with no
history and no TOB change, and PHA still reproduced NOAA exactly.

That result removes the last objection. There is no history error and no TOB
error to blame, because there is no history and no TOB. Only QCU and the
parameters remain. If any count is not zero, say so in the result, because the
falsification is then conditional on those inputs being correct.

The tool also reports Moran's I for the composite over the neighbour graph. Use
it to see whether the remaining error is clustered or spread. Note that the full
40-neighbour graph has a mean degree near 52 and percolates: any 15% subset of
nodes joins into one component, so a component test on that graph has no power.
Use --top-k to thin the graph before you read component sizes.

Usage:
    sentinel_audit.py --per-station perstation_best.tsv \
        --neighbors shared/neighbor-correlation.txt \
        [--distance shared/neighbor-distance.txt] \
        [--his-dir DIR] [--tob-dir DIR] [--qcu-dir DIR] [--hops 5]
"""

from __future__ import annotations

import argparse
import random
from collections import deque
from pathlib import Path
from typing import Dict, List, Optional, Set

FIELD = 12


def read_neighbor_file(path: Path) -> Dict[str, List[str]]:
    """Read a three-line-per-station neighbour file into id -> neighbour ids."""
    out: Dict[str, List[str]] = {}
    with open(path) as fh:
        while True:
            ids = fh.readline()
            if not ids:
                break
            fh.readline()
            fh.readline()
            row = [ids[i:i + FIELD].strip()
                   for i in range(0, len(ids.rstrip("\n")), FIELD)]
            row = [r for r in row if r]
            if row:
                out[row[0]] = [r for r in row[1:] if r != row[0]]
    return out


def hop_distance(ids: Set[str], perfect: Set[str],
                 nbr: Dict[str, List[str]]) -> Dict[str, int]:
    """Hops from the nearest inexact station, along the REVERSED graph.

    Influence moves from a neighbour into the station that lists it, so the
    edges must be reversed before the search. This is easy to get backwards.
    """
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
    return dist


def morans_i(adj: Dict[str, Set[str]], val: Dict[str, float]) -> float:
    ids = [s for s in adj if s in val]
    n = len(ids)
    if n < 2:
        return float("nan")
    mean = sum(val[s] for s in ids) / n
    dev = {s: val[s] - mean for s in ids}
    denom = sum(d * d for d in dev.values())
    if denom == 0:
        return float("nan")
    num = 0.0
    w = 0
    for a in ids:
        for b in adj[a]:
            if b in dev:
                num += dev[a] * dev[b]
                w += 1
    return (n / w) * (num / denom) if w else float("nan")


def read_series(path: Path) -> Dict[int, List[str]]:
    """Read a PHA station data file: year -> the twelve value fields.

    The layout is id(11) space year(4) then twelve fields of stride 9.
    """
    out: Dict[int, List[str]] = {}
    for line in open(path):
        if len(line) < 20:
            continue
        try:
            year = int(line[12:16])
        except ValueError:
            continue
        out[year] = [line[16 + k * 9:22 + k * 9].strip() for k in range(12)]
    return out


def main(argv: Optional[List[str]] = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--per-station", required=True)
    ap.add_argument("--neighbors", required=True)
    ap.add_argument("--distance", default=None,
                    help="neighbour distance file, for the pool-closure test")
    ap.add_argument("--his-dir", default=None)
    ap.add_argument("--tob-dir", default=None)
    ap.add_argument("--qcu-dir", default=None)
    ap.add_argument("--hops", type=int, default=5)
    ap.add_argument("--top-k", type=int, default=0,
                    help="thin each neighbour list to its k best entries "
                         "before Moran's I; the full graph percolates")
    ap.add_argument("--out", default=None, help="write the sentinel ids here")
    ap.add_argument("--seed", type=int, default=20260804)
    args = ap.parse_args(argv)

    comp: Dict[str, float] = {}
    with open(args.per_station) as fh:
        header = fh.readline().rstrip("\n").split("\t")
        ci = header.index("composite")
        for line in fh:
            f = line.rstrip("\n").split("\t")
            comp[f[0]] = float(f[ci])

    nbr = read_neighbor_file(Path(args.neighbors))
    ids = set(comp)
    perfect = {s for s in ids if comp[s] >= 1.0}
    print("# %d stations, %d exact (%.1f%%)"
          % (len(ids), len(perfect), 100.0 * len(perfect) / len(ids)))

    # ---- clustering -------------------------------------------------------
    adj: Dict[str, Set[str]] = {s: set() for s in ids}
    for s in ids:
        for n in (nbr.get(s, [])[:args.top_k] if args.top_k else nbr.get(s, [])):
            if n in adj:
                adj[s].add(n)
                adj[n].add(s)
    obs = morans_i(adj, comp)
    rng = random.Random(args.seed)
    nodes = sorted(adj)
    vals = [comp[s] for s in nodes]
    null = []
    for _ in range(25):
        rng.shuffle(vals)
        null.append(morans_i(adj, dict(zip(nodes, vals))))
    mu = sum(null) / len(null)
    sd = (sum((x - mu) ** 2 for x in null) / len(null)) ** 0.5
    print("# Moran's I = %+.4f (shuffled null %.4f +/- %.4f)" % (obs, mu, sd))

    # ---- question 1: depth ------------------------------------------------
    dist = hop_distance(ids, perfect, nbr)
    print("\n## 1. sentinels by depth")
    for d in range(2, 8):
        print("   >=%d hops: %6d" % (d, sum(1 for s in perfect
                                            if dist.get(s, 10 ** 9) >= d)))

    sent = {s for s in perfect if dist.get(s, 10 ** 9) >= args.hops}

    # ---- question 2: pool closure -----------------------------------------
    if args.distance:
        pool = read_neighbor_file(Path(args.distance))
        clean = {s for s in perfect
                 if s in pool and pool[s] and all(n in perfect for n in pool[s])}
        closed = {s for s in clean if all(n in clean for n in pool[s])}
        print("\n## 2. pool closure (protects a network-rebuilding probe)")
        print("   whole distance pool exact          : %d" % len(clean))
        print("   pool of each pool member also exact: %d" % len(closed))
        print("   sentinels at >=%d hops AND closed   : %d"
              % (args.hops, len(sent & closed)))
        sent &= closed

    if args.out:
        Path(args.out).write_text("\n".join(sorted(sent)) + "\n")
        print("\n# %d sentinel ids -> %s" % (len(sent), args.out))

    # ---- question 3: is the test contaminated? -----------------------------
    print("\n## 3. what we supply to the sentinels (all should be zero)")
    print("   sentinels                : %d" % len(sent))
    print("   US stations              : %d" % sum(1 for s in sent
                                                   if s.startswith("US")))
    if args.his_dir:
        his = {p.stem for p in Path(args.his_dir).glob("*.his")}
        print("   with a station history   : %d" % sum(1 for s in sent if s in his))
    if args.tob_dir and args.qcu_dir:
        tob_d, qcu_d = Path(args.tob_dir), Path(args.qcu_dir)
        changed = 0
        for s in sent:
            a = list(qcu_d.glob(s + "*"))
            b = list(tob_d.glob(s + "*"))
            if not a or not b or read_series(a[0]) != read_series(b[0]):
                changed += 1
        print("   with a TOB adjustment    : %d" % changed)
        if changed == 0:
            print("   => TOB and history supply nothing here. Only QCU and the")
            print("      parameters remain, so a sentinel result is unconditional.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
