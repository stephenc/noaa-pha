#!/usr/bin/env python3
"""Plot per-station PHA fit scores on a Mercator projection.

A table of component sizes says whether the residual is clustered; a map says
*where*, which is what actually points at a cause.  Mercator is the right
projection here despite its area distortion: the station network is what it is,
and preserving the familiar shape of coastlines makes a cluster recognisable as
"the US Midwest" or "Scandinavia" at a glance.

Mercator diverges at the poles, so latitude is clipped to +/-85.  A station
beyond that is drawn ON the clip line rather than dropped, because the polar
stations are among the worst-fitting in the corpus and a map that hides them
answers the wrong question.  Their plotted latitude is therefore approximate:
the Antarctic cluster along the bottom edge sits at -90, not -85.

Draw order matters: well-fitting stations are plotted first and badly-fitting
ones last, so that in dense regions the problem stations are visible rather
than buried under thousands of green dots.  With ~28k points in a
continent-sized scatter the overplotting would otherwise decide the story.

Usage:
    pha_fit_map.py --per-station perstation.tsv --inventory station.inv \
        --out fit_map.png [--region conus] [--title ...]
"""

from __future__ import annotations

import argparse
import math
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import matplotlib

matplotlib.use("Agg")  # headless: no display on this box
import matplotlib.pyplot as plt  # noqa: E402
from matplotlib.colors import LinearSegmentedColormap  # noqa: E402

LAT_CLIP = 85.0

# Dark ground: ~70% of stations score near 1.0, so the plot is mostly green.
# Against white that green washes out and the sparse dark-red failures -- the
# only thing the map exists to show -- lose contrast against the gridlines.
BG = "#0e1117"
FG = "#e6e6e6"
LABEL = "#c8ccd4"
MUTED = "#9aa0aa"
SPINE = "#3a3f4b"
GRID = "#262b36"

REGIONS = {
    "world": (-180.0, 180.0, -90.0, 90.0),
    "conus": (-126.0, -66.0, 23.0, 51.0),
    "europe": (-12.0, 42.0, 34.0, 72.0),
}


def merc_y(lat: float) -> float:
    lat = max(-LAT_CLIP, min(LAT_CLIP, lat))
    return math.degrees(math.log(math.tan(math.pi / 4 + math.radians(lat) / 2)))


def read_inventory(path: Path) -> Dict[str, Tuple[float, float]]:
    out = {}
    with open(path) as fh:
        for line in fh:
            if len(line) < 31:
                continue
            try:
                out[line[0:11]] = (float(line[12:20]), float(line[21:30]))
            except ValueError:
                continue
    return out


def main(argv: Optional[List[str]] = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    ap.add_argument("--per-station", required=True)
    ap.add_argument("--inventory", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--column", default="composite")
    ap.add_argument("--region", default="world", choices=sorted(REGIONS))
    ap.add_argument("--title", default=None)
    ap.add_argument("--size", type=float, default=None, help="marker size override")
    ap.add_argument("--dpi", type=int, default=160)
    args = ap.parse_args(argv)

    val: Dict[str, float] = {}
    with open(args.per_station) as fh:
        header = fh.readline().rstrip("\n").split("\t")
        ci = header.index(args.column)
        for line in fh:
            f = line.rstrip("\n").split("\t")
            val[f[0]] = float(f[ci])

    inv = read_inventory(Path(args.inventory))
    lon0, lon1, lat0, lat1 = REGIONS[args.region]

    pts = []
    for sid, v in val.items():
        if sid not in inv:
            continue
        lat, lon = inv[sid]
        if not (lon0 <= lon <= lon1 and lat0 <= lat <= lat1):
            continue
        pts.append((v, lon, merc_y(lat)))
    # Worst last, so the problem stations are not hidden under the good ones.
    pts.sort(key=lambda p: -p[0])

    y0, y1 = merc_y(lat0), merc_y(lat1)
    aspect = (y1 - y0) / (lon1 - lon0)
    width = 16.0
    fig, ax = plt.subplots(figsize=(width, max(4.0, width * aspect + 1.2)))

    # Red = bad, yellow = middling, green = perfect.  A diverging map would
    # imply a meaningful midpoint; this scale is a one-way "how wrong".  Six
    # stops rather than five, with a near-black red at the bottom: on a dark
    # ground the worst stations have to darken *away* from the mid-tones to
    # stay legible, and the extra stop keeps the 0.5-0.8 band from collapsing
    # into a single orange where most of the interesting variation sits.
    cmap = LinearSegmentedColormap.from_list(
        "fit",
        ["#67000d", "#d7301f", "#fdae61", "#fee08b", "#a6d96a", "#1a9850"],
    )
    size = (
        args.size
        if args.size is not None
        else (7.0 if args.region == "world" else 16.0)
    )
    fig.patch.set_facecolor(BG)
    ax.set_facecolor(BG)
    sc = ax.scatter(
        [p[1] for p in pts],
        [p[2] for p in pts],
        c=[p[0] for p in pts],
        cmap=cmap,
        vmin=0.0,
        vmax=1.0,
        s=size,
        linewidths=0,
        alpha=0.92,
    )

    ax.set_xlim(lon0, lon1)
    ax.set_ylim(y0, y1)
    step = 30 if args.region == "world" else 10
    ax.set_xticks([x for x in range(int(lon0), int(lon1) + 1) if x % step == 0])
    ax.set_xticklabels(["%d°" % x for x in ax.get_xticks()])
    # Ticks stop at the clip: a "-90" label on the clipped edge would
    # misstate where the bottom row of points actually is.
    lat_ticks = [
        t for t in range(-90, 91, step) if lat0 <= t <= lat1 and abs(t) <= LAT_CLIP
    ]
    ax.set_yticks([merc_y(t) for t in lat_ticks])
    ax.set_yticklabels(["%d°" % t for t in lat_ticks])
    ax.grid(True, color=GRID, linewidth=0.6)
    ax.tick_params(colors=MUTED)
    for s in ax.spines.values():
        s.set_color(SPINE)

    n_bad = sum(1 for p in pts if p[0] < 0.5)
    mean = sum(p[0] for p in pts) / len(pts) if pts else float("nan")
    ax.set_title(
        args.title
        or "PHA fit %s -- %d stations, mean %.4f, %d below 0.5 (%.1f%%)"
        % (args.column, len(pts), mean, n_bad, 100.0 * n_bad / max(1, len(pts))),
        color=FG,
        fontsize=15,
        pad=14,
    )
    cb = fig.colorbar(sc, ax=ax, fraction=0.025, pad=0.01)
    cb.set_label("%s (1 = exact match to published QCF)" % args.column, color=LABEL)
    cb.ax.yaxis.set_tick_params(color=MUTED)
    cb.outline.set_edgecolor(SPINE)
    plt.setp(plt.getp(cb.ax.axes, "yticklabels"), color=MUTED)
    fig.tight_layout()
    fig.savefig(args.out, dpi=args.dpi, facecolor=fig.get_facecolor())
    print("# %d stations plotted -> %s" % (len(pts), args.out))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
