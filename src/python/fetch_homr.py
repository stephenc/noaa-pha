#!/usr/bin/env python3
"""Fetch the HOMR station-metadata inputs, recording provenance.

History recovery consumes two NOAA metadata products alongside QCU/QCF:

  * **MSHR enhanced** -- coordinates, ground elevation and documented
    relocations (``mshr_enhanced.txt.zip``)
  * **PHR** -- publication history: observation time and equipment
    (``phr.txt.zip``)

Both are undated "latest" files that NOAA refreshes, so a reconstruction is only
reproducible if you record *which* copy you used.  This tool downloads each file
plus its official fixed-width layout spec, and writes ``homr_provenance.json``
with the URL, fetch timestamp, byte size and SHA-256 of every artifact.

Layout specs are fetched too, not just referenced: every column offset in
``ghcn_io.read_mshr`` / ``read_phr`` cites them, and NOAA has changed layouts
before (the Enhanced Lite report gained RELOCATION in June 2014).  Keeping the
spec beside the data makes a later mismatch diagnosable instead of mysterious.

Usage:
    uv run python src/python/fetch_homr.py --base data
    uv run python src/python/fetch_homr.py --base data --check   # verify only
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Optional

HOMR = "https://www.ncei.noaa.gov/access/homr/file"

ARTIFACTS = [
    ("mshr_enhanced.txt.zip", f"{HOMR}/mshr_enhanced.txt.zip", "data"),
    ("MSHR_Enhanced_Table.txt", f"{HOMR}/MSHR_Enhanced_Table.txt", "spec"),
    ("phr.txt.zip", f"{HOMR}/phr.txt.zip", "data"),
    ("PHR_Table.txt", f"{HOMR}/PHR_Table.txt", "spec"),
]

PROVENANCE = "homr_provenance.json"


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def fetch(url: str, dest: Path, timeout: int) -> None:
    tmp = dest.with_suffix(dest.suffix + ".part")
    req = urllib.request.Request(url, headers={"User-Agent": "noaa-pha-recover/1"})
    with urllib.request.urlopen(req, timeout=timeout) as resp, open(tmp, "wb") as out:
        while True:
            chunk = resp.read(1 << 20)
            if not chunk:
                break
            out.write(chunk)
    tmp.replace(dest)


def main(argv: Optional[List[str]] = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--base", required=True, help="base directory to populate")
    ap.add_argument("--timeout", type=int, default=180)
    ap.add_argument(
        "--check",
        action="store_true",
        help="do not download; verify present files against homr_provenance.json",
    )
    ap.add_argument(
        "--force", action="store_true", help="re-download even if the file exists"
    )
    args = ap.parse_args(argv)

    base = Path(args.base)
    base.mkdir(parents=True, exist_ok=True)
    prov_path = base / PROVENANCE

    if args.check:
        if not prov_path.exists():
            sys.exit("no %s in %s -- nothing to check" % (PROVENANCE, base))
        prov = json.loads(prov_path.read_text())
        bad = 0
        for name, rec in prov.get("artifacts", {}).items():
            p = base / name
            if not p.exists():
                print("MISSING  %s" % name)
                bad += 1
                continue
            got = sha256(p)
            ok = got == rec.get("sha256")
            print("%-28s %s" % (name, "ok" if ok else "SHA MISMATCH"))
            bad += 0 if ok else 1
        print("# fetched_utc: %s" % prov.get("fetched_utc"))
        return 1 if bad else 0

    prov: Dict[str, dict] = {
        "fetched_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "source": HOMR,
        "artifacts": {},
    }
    for name, url, kind in ARTIFACTS:
        dest = base / name
        if dest.exists() and not args.force:
            print("have    %s (use --force to refresh)" % name)
        else:
            print("fetch   %s" % url, flush=True)
            try:
                fetch(url, dest, args.timeout)
            except Exception as exc:
                sys.exit("failed to fetch %s: %s" % (url, exc))
        prov["artifacts"][name] = {
            "url": url,
            "kind": kind,
            "bytes": dest.stat().st_size,
            "sha256": sha256(dest),
        }
        print(
            "        %s bytes=%d sha256=%s"
            % (
                name,
                prov["artifacts"][name]["bytes"],
                prov["artifacts"][name]["sha256"][:16],
            )
        )

    prov_path.write_text(json.dumps(prov, indent=1) + "\n")
    print("# provenance -> %s" % prov_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
