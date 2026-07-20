#!/usr/bin/env python3
"""PHA-robust TOB-exactness acceptance metric.

The residual R(t) = QCF(t) - TOB(t) equals the PHA adjustment when our TOB
matches NOAA's.  PHA adjustments are piecewise-constant *step functions*
(month-independent levels; steps at documented/undocumented changepoints,
typically >=12-18 months apart).  A TOB error, by contrast, is *seasonal*
(a 12-month-periodic ringing).  So the honest test of TOB-exactness is:

    after removing the piecewise-constant PHA structure, is there any
    residual seasonal (per-calendar-month) bias?

We remove the step structure with a centered rolling MEDIAN (window +-12
months): the median tracks the dominant PHA level and is insensitive to a
single step inside the window.  We then take, per calendar month, the MEDIAN
anomaly over all years.  For TOB-exact stations this is ~0 for every month
(the few months near a step are scattered across calendar months/years and
wash out in the median).  A genuine seasonal TOB error yields a persistent
nonzero anomaly in the affected months.

WHY THE NAIVE METRIC IS WRONG.  An earlier acceptance metric grouped residuals
by (obs-time .his regime, calendar month) and compared each month's median to
the regime's modal median.  But obs-time regimes are often *longer* than PHA
segments: a single regime can straddle several PHA changepoints, so a calendar
month's median blends multiple flat PHA levels and looks "off" even though the
residual is a perfect step function with zero seasonality.  That metric
under-counted TOB-exact stations (flagged 23 "failures" that are all artifacts).

Positive control (self-test): injecting a synthetic +3c into every July of a
known-clean station makes this metric flag exactly month 7 (returns {7: 3.0}),
confirming it is not blind.

Usage:
  python3 src/python/tob_seasonal_acceptance.py \
      --tob-dir data/input/tob-pattern3/tavg --qcf-dir data/output/qcf/tavg \
      --qcu-dir data/input/raw/tavg [--thresh 0.5] [--report R.txt]
"""
import argparse
from collections import defaultdict
from pathlib import Path
from statistics import median


def parse_m(path):
    d = {}
    for line in open(path):
        if len(line) < 16:
            continue
        y = int(line[12:16])
        for m in range(12):
            b = 16 + m * 9
            try:
                v = int(line[b:b + 6])
            except ValueError:
                continue
            if v != -9999:
                d[(y, m + 1)] = v
    return d


def seasonal_anomaly(residual, half=12):
    """Return {calendar_month: median rolling-detrended anomaly}."""
    order = sorted(residual)
    vals = [residual[t] for t in order]
    anom = defaultdict(list)
    for i, t in enumerate(order):
        win = vals[max(0, i - half):min(len(vals), i + half + 1)]
        anom[t[1]].append(residual[t] - median(win))
    return {m: median(a) for m, a in anom.items() if a}


def flagged_months(residual, thresh=0.5, half=12):
    med = seasonal_anomaly(residual, half)
    return {m: round(v, 1) for m, v in med.items() if abs(v) > thresh}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tob-dir", default="data/input/tob-pattern3/tavg")
    ap.add_argument("--qcf-dir", default="data/output/qcf/tavg")
    ap.add_argument("--qcu-dir", default="data/input/raw/tavg")
    ap.add_argument("--thresh", type=float, default=0.5)
    ap.add_argument("--report", default=None)
    ap.add_argument("--self-test", action="store_true",
                    help="inject a synthetic July error and confirm detection")
    args = ap.parse_args()

    tob_dir, qcf_dir, qcu_dir = map(Path, (args.tob_dir, args.qcf_dir, args.qcu_dir))

    if args.self_test:
        for tp in tob_dir.glob("*.tob.tavg"):
            sid = tp.name.split(".")[0]
            qp = qcf_dir / f"{sid}.qcf.tavg"
            if not qp.exists():
                continue
            tob, qcf = parse_m(tp), parse_m(qp)
            ser = {t: qcf[t] - tob[t] for t in tob if t in qcf}
            if len(ser) < 60 or flagged_months(ser, args.thresh):
                continue
            inj = {t: (v + 3 if t[1] == 7 else v) for t, v in ser.items()}
            print(f"self-test on {sid}: clean={flagged_months(ser, args.thresh)} "
                  f"injected={flagged_months(inj, args.thresh)}")
            return
        print("self-test: no clean station found")
        return

    n = exact = 0
    flagged = []
    for tp in sorted(tob_dir.glob("*.tob.tavg")):
        sid = tp.name.split(".")[0]
        qp = qcf_dir / f"{sid}.qcf.tavg"
        if not qp.exists():
            continue
        tob, qcf = parse_m(tp), parse_m(qp)
        ser = {t: qcf[t] - tob[t] for t in tob if t in qcf}
        if len(ser) < 24:
            continue
        qcu_f = qcu_dir / f"{sid}.raw.tavg"
        qcu = parse_m(qcu_f) if qcu_f.exists() else {}
        if not any(tob[t] != qcu.get(t) for t in tob if t in qcu):
            continue  # TOB applied no adjustment -> nothing to verify
        n += 1
        fm = flagged_months(ser, args.thresh)
        if fm:
            flagged.append((sid, fm))
        else:
            exact += 1
    print(f"US TOB-adjusted stations: {n}")
    print(f"  TOB-exact (no residual seasonality): {exact} "
          f"({100 * exact / max(1, n):.2f}%)")
    print(f"  flagged: {len(flagged)}")
    for sid, fm in sorted(flagged, key=lambda x: -len(x[1])):
        print(f"    {sid}: {fm}")
    if args.report:
        with open(args.report, "w") as f:
            for sid, fm in flagged:
                f.write(f"{sid} {fm}\n")


if __name__ == "__main__":
    main()
