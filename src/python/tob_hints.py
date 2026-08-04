#!/usr/bin/env python3
"""Cross-vintage TOB evidence: hint capture, I/O, and consolidation.

Residual reconstruction can only recover TOB codes where QCU∩QCF months
exist.  Where QCF is absent (PHA auto-exclusion, vintage trimming), an
emitted ``.his`` carries default/pad codes.  Another vintage may have QCF
coverage for exactly those exterior years.  This module records, at solve
time, *what the residual solve actually established* (constraint runs,
per-regime coverage, offset-ambiguity sets, boundary evidence) into a
per-station **hints** file, and consumes donor hints at emission time to
extend a station's timeline outside its own QCF hull -- never inside it.

It subsumes ``merge_history.py``: the same hull / adoptability / effective-
month / boundary-crossing / refusal semantics, moved from ``.his``-row space
to regime+evidence space.  Hard invariants:

  * No tolerances anywhere -- integer cents / interval logic only.
  * Consolidation acts strictly outside the current QCF hull, so an adopted
    regime can never displace a month this vintage's residuals constrain.
    (Hints additionally steer the solve inside the hull -- enumeration
    preference and hinted-boundary retry -- which changes which exact
    decomposition is found, never whether it reproduces QCF.  ``reconstruct_his
    --no-vintage-hints`` turns that off, leaving consolidation alone.)
  * With no ``--hints`` supplied, emitted ``.his`` bytes are unchanged.

Schema: ``tob-hints/1`` (see ``StationHints`` / the proposal §3.2).  Files
live at ``<base>/intermediate/hints/<sid>.hints.json``.
"""

from __future__ import annotations

import json
import os
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

import ghcn_io

Ym = Tuple[int, int]
Ymd = Tuple[int, int, int]

# --------------------------------------------------------------------------
# Schema constants
# --------------------------------------------------------------------------

HINT_FORMAT = "tob-hints/1"

# Evidence classes (schema enum).  ``metadata-only`` is reserved (non-CONUS
# metadata rows): nothing produces or consumes it, but the loader accepts it so
# a future producer does not trip validation.
EVIDENCE_CLASSES = frozenset(
    {
        "residual-proven",
        "residual-ambiguous",
        "residual-partial",
        # Phase 2 (§9.1): a proven/ambiguous reading that a VINTAGE hint tipped
        # at equal rank (policy adoption).  Valid, but NEVER adoptable
        # downstream -- it would launder the donor's tie-break into this
        # vintage's "independent proof".
        "residual-proven-hinted",
        "unconstrained",
        "metadata-only",
    }
)

# Classes whose regimes may be adopted into an exterior window (§6.1).
ADOPTABLE_CLASSES = frozenset({"residual-proven", "residual-ambiguous"})

BOUNDARY_KINDS = frozenset({"constrained", "gap", "record-start"})

# Day-resolution sources (§3.2).  ``search-blend`` is the only one that
# carries a ``feasible_days`` set; the others record only the chosen day.
DAY_SOURCES = frozenset({"search-blend", "two-blend-repair", "magnitude-refined"})

# Valid TOB labels for a history row (BASIS_CODES plus 00HR alias of 24HR).
# Port of ``merge_history._VALID_TOB_LABELS``.
_VALID_TOB_LABELS = set(ghcn_io.BASIS_CODES) | {"00HR"}

# Zero-adjustment codes (00HR / 24HR both decode to obs_code 24).
_PAD_CODES = {24}


class HintFormatError(ValueError):
    """Raised when a hints file cannot be trusted (parse / schema failure).

    A ``HintFormatError`` is always a *refusal* -- the caller must never fall
    back to silent adoption of a malformed or unknown-format hints file.
    """


# --------------------------------------------------------------------------
# TOB code predicates (ported from merge_history; single source of truth)
# --------------------------------------------------------------------------


def is_no_adjustment(code: int, label: str = "") -> bool:
    """True for the no-adjustment convention (00HR / 24HR -> code 24)."""
    if code in _PAD_CODES:
        return True
    lab = (label or "").strip().upper()
    return lab in ("00HR", "24HR")


def is_valid_tob(code: int, label: str = "") -> bool:
    """True for a known TOB basis code (hourly 01-24 / 00HR, or 00RS/SR/SS).

    Aligns with ``ghcn_io.BASIS_CODES`` and ``his_emit`` (which rejects a
    nonblank ``obs_code == 99``).  Sentinels (9999, 8888, ...) and unknowns
    (DE, UN, ...) are not valid.
    """
    lab = (label or "").strip().upper()
    if lab in ghcn_io.PHR_OBS_SENTINELS:
        return False
    if code == 99:
        return False
    if lab in _VALID_TOB_LABELS:
        return True
    if 1 <= code <= 24:
        return True
    if code in (26, 27, 28):  # 00RS / 00SR / 00SS
        return True
    return False


def is_valid_code_label(label: str) -> bool:
    """True if ``label`` is a code a hints file may legitimately carry."""
    lab = (label or "").strip().upper()
    if not lab:
        return False
    return is_valid_tob(ghcn_io.decode_obtime(lab), lab)


def is_adoptable_code(label: str) -> bool:
    """Adoptable donor TOB: a valid basis code (pads included per §6.1).

    Unlike ``merge_history.is_adoptable_donor_tob``, pad codes (00HR/24HR)
    **are** adoptable here -- they participate in precedence and, on winning,
    emit an explicit 24HR row (§6.5).  Sentinels / unknowns remain excluded.
    """
    return is_valid_code_label(label)


# --------------------------------------------------------------------------
# Tuple validation helpers
# --------------------------------------------------------------------------


def _as_month(v: object, where: str) -> Ym:
    if (
        not isinstance(v, (list, tuple))
        or len(v) != 2
        or not all(isinstance(x, int) and not isinstance(x, bool) for x in v)
    ):
        raise HintFormatError(f"{where}: malformed month tuple {v!r}")
    y, m = int(v[0]), int(v[1])
    if not (1 <= m <= 12):
        raise HintFormatError(f"{where}: month out of range in {v!r}")
    return (y, m)


def _as_date(v: object, where: str) -> Ymd:
    if (
        not isinstance(v, (list, tuple))
        or len(v) != 3
        or not all(isinstance(x, int) and not isinstance(x, bool) for x in v)
    ):
        raise HintFormatError(f"{where}: malformed date tuple {v!r}")
    y, m, d = int(v[0]), int(v[1]), int(v[2])
    if not (1 <= m <= 12):
        raise HintFormatError(f"{where}: month out of range in {v!r}")
    if not (1 <= d <= 31):
        raise HintFormatError(f"{where}: day out of range in {v!r}")
    return (y, m, d)


def _as_month_or_none(v: object, where: str) -> Optional[Ym]:
    return None if v is None else _as_month(v, where)


def _as_run(v: object, where: str) -> Tuple[Ym, Ym]:
    if not isinstance(v, (list, tuple)) or len(v) != 2:
        raise HintFormatError(f"{where}: malformed run {v!r}")
    return (_as_month(v[0], where), _as_month(v[1], where))


def _ml(ym: Optional[Ym]) -> Optional[List[int]]:
    return None if ym is None else [ym[0], ym[1]]


def _dl(ymd: Optional[Ymd]) -> Optional[List[int]]:
    return None if ymd is None else [ymd[0], ymd[1], ymd[2]]


# --------------------------------------------------------------------------
# Schema dataclasses
# --------------------------------------------------------------------------


@dataclass
class Boundary:
    """Begin-date evidence for a regime (§3.2)."""

    kind: str  # "constrained" | "gap" | "record-start"
    gap_months_before: Optional[int] = None
    day_resolved: bool = False
    day_source: Optional[str] = (
        None  # search-blend | two-blend-repair | magnitude-refined
    )
    feasible_days: Optional[List[int]] = None

    def to_dict(self) -> dict:
        return {
            "kind": self.kind,
            "gap_months_before": self.gap_months_before,
            "day_resolved": self.day_resolved,
            "day_source": self.day_source,
            "feasible_days": self.feasible_days,
        }

    @classmethod
    def from_dict(cls, d: object, where: str) -> "Boundary":
        if not isinstance(d, dict):
            raise HintFormatError(f"{where}: boundary not an object")
        kind = d.get("kind")
        if kind not in BOUNDARY_KINDS:
            raise HintFormatError(f"{where}: bad boundary kind {kind!r}")
        src = d.get("day_source")
        if src is not None and src not in DAY_SOURCES:
            raise HintFormatError(f"{where}: bad day_source {src!r}")
        fd = d.get("feasible_days")
        if fd is not None:
            if not isinstance(fd, list) or not all(
                isinstance(x, int) and not isinstance(x, bool) for x in fd
            ):
                raise HintFormatError(f"{where}: malformed feasible_days {fd!r}")
        gmb = d.get("gap_months_before")
        if gmb is not None and (not isinstance(gmb, int) or isinstance(gmb, bool)):
            raise HintFormatError(f"{where}: malformed gap_months_before {gmb!r}")
        return cls(
            kind=kind,
            gap_months_before=gmb,
            day_resolved=bool(d.get("day_resolved", False)),
            day_source=src,
            feasible_days=list(fd) if fd is not None else None,
        )


@dataclass
class RegimeEvidence:
    """Per-regime evidence block (§3.2)."""

    cls: str  # the schema "class" field
    n_constrained: int
    constrained_runs: List[Tuple[Ym, Ym]]
    first_constrained: Optional[Ym]
    last_constrained: Optional[Ym]
    ambiguous_codes: List[str]
    flutter_months: List[Ym]
    deviant_months: List[Ym]
    boundary: Boundary

    def to_dict(self) -> dict:
        return {
            "class": self.cls,
            "n_constrained": self.n_constrained,
            "constrained_runs": [[_ml(a), _ml(b)] for a, b in self.constrained_runs],
            "first_constrained": _ml(self.first_constrained),
            "last_constrained": _ml(self.last_constrained),
            "ambiguous_codes": list(self.ambiguous_codes),
            "flutter_months": [_ml(m) for m in self.flutter_months],
            "deviant_months": [_ml(m) for m in self.deviant_months],
            "boundary": self.boundary.to_dict(),
        }

    @classmethod
    def from_dict(cls, d: object, where: str) -> "RegimeEvidence":
        if not isinstance(d, dict):
            raise HintFormatError(f"{where}: evidence not an object")
        cl = d.get("class")
        if cl not in EVIDENCE_CLASSES:
            raise HintFormatError(f"{where}: bad evidence class {cl!r}")
        codes = d.get("ambiguous_codes", [])
        if not isinstance(codes, list):
            raise HintFormatError(f"{where}: ambiguous_codes not a list")
        for c in codes:
            if not isinstance(c, str) or not is_valid_code_label(c):
                raise HintFormatError(f"{where}: bad code label {c!r}")
        nconst = d.get("n_constrained", 0)
        if not isinstance(nconst, int) or isinstance(nconst, bool):
            raise HintFormatError(f"{where}: malformed n_constrained")
        return cls(
            cls=cl,
            n_constrained=nconst,
            constrained_runs=[
                _as_run(r, f"{where}.constrained_runs")
                for r in d.get("constrained_runs", [])
            ],
            first_constrained=_as_month_or_none(
                d.get("first_constrained"), f"{where}.first_constrained"
            ),
            last_constrained=_as_month_or_none(
                d.get("last_constrained"), f"{where}.last_constrained"
            ),
            ambiguous_codes=[c.strip().upper() for c in codes],
            flutter_months=[
                _as_month(m, f"{where}.flutter_months")
                for m in d.get("flutter_months", [])
            ],
            deviant_months=[
                _as_month(m, f"{where}.deviant_months")
                for m in d.get("deviant_months", [])
            ],
            boundary=Boundary.from_dict(d.get("boundary", {}), f"{where}.boundary"),
        )


@dataclass
class HintRegime:
    """One residual regime plus its evidence (§3.2)."""

    begin: Ymd
    end: Optional[Ymd]
    code: str
    blend_day: Optional[int]
    evidence: RegimeEvidence

    def to_dict(self) -> dict:
        return {
            "begin": _dl(self.begin),
            "end": _dl(self.end),
            "code": self.code,
            "blend_day": self.blend_day,
            "evidence": self.evidence.to_dict(),
        }

    @classmethod
    def from_dict(cls, d: object, where: str) -> "HintRegime":
        if not isinstance(d, dict):
            raise HintFormatError(f"{where}: regime not an object")
        code = d.get("code")
        if not isinstance(code, str) or not is_valid_code_label(code):
            raise HintFormatError(f"{where}: bad regime code {code!r}")
        bd = d.get("blend_day")
        if bd is not None and (not isinstance(bd, int) or isinstance(bd, bool)):
            raise HintFormatError(f"{where}: malformed blend_day {bd!r}")
        return cls(
            begin=_as_date(d.get("begin"), f"{where}.begin"),
            end=(
                None if d.get("end") is None else _as_date(d.get("end"), f"{where}.end")
            ),
            code=code.strip().upper(),
            blend_day=bd,
            evidence=RegimeEvidence.from_dict(
                d.get("evidence", {}), f"{where}.evidence"
            ),
        )


@dataclass
class Provenance:
    base: str
    generated: str = ""
    kind: str = "tob"
    exact: bool = False
    cost: Optional[List[int]] = None

    def to_dict(self) -> dict:
        return {
            "base": self.base,
            "generated": self.generated,
            "kind": self.kind,
            "exact": self.exact,
            "cost": list(self.cost) if self.cost is not None else None,
        }

    @classmethod
    def from_dict(cls, d: object, where: str) -> "Provenance":
        if not isinstance(d, dict):
            raise HintFormatError(f"{where}: provenance not an object")
        base = d.get("base")
        if not isinstance(base, str) or not base:
            raise HintFormatError(f"{where}: provenance.base missing")
        cost = d.get("cost")
        if cost is not None and not isinstance(cost, list):
            raise HintFormatError(f"{where}: provenance.cost malformed")
        return cls(
            base=base,
            generated=str(d.get("generated", "")),
            kind=str(d.get("kind", "tob")),
            exact=bool(d.get("exact", False)),
            cost=list(cost) if cost is not None else None,
        )


@dataclass
class StationHints:
    """Full ``tob-hints/1`` document for one station."""

    station_id: str
    solver_version: int
    provenance: Provenance
    qcu_hull: Optional[Tuple[Ym, Ym]]
    qcf_hull: Optional[Tuple[Ym, Ym]]
    evidence_runs: List[Tuple[Ym, Ym]]
    regimes: List[HintRegime]
    deviants: List[Ym] = field(default_factory=list)
    audits: List[str] = field(default_factory=list)

    def to_dict(self) -> dict:
        return {
            "format": HINT_FORMAT,
            "solver_version": self.solver_version,
            "station_id": self.station_id,
            "provenance": self.provenance.to_dict(),
            "qcu_hull": (
                None
                if self.qcu_hull is None
                else [_ml(self.qcu_hull[0]), _ml(self.qcu_hull[1])]
            ),
            "qcf_hull": (
                None
                if self.qcf_hull is None
                else [_ml(self.qcf_hull[0]), _ml(self.qcf_hull[1])]
            ),
            "evidence_runs": [[_ml(a), _ml(b)] for a, b in self.evidence_runs],
            "regimes": [r.to_dict() for r in self.regimes],
            "deviants": [_ml(m) for m in self.deviants],
            "audits": list(self.audits),
        }

    @classmethod
    def from_dict(cls, d: object) -> "StationHints":
        if not isinstance(d, dict):
            raise HintFormatError("top-level document is not an object")
        fmt = d.get("format")
        if fmt != HINT_FORMAT:
            raise HintFormatError(f"unknown format {fmt!r} (want {HINT_FORMAT!r})")
        sid = d.get("station_id")
        if not isinstance(sid, str) or not sid:
            raise HintFormatError("station_id missing")
        sv = d.get("solver_version")
        if not isinstance(sv, int) or isinstance(sv, bool):
            raise HintFormatError(f"malformed solver_version {sv!r}")
        hull_u = d.get("qcu_hull")
        hull_f = d.get("qcf_hull")
        return cls(
            station_id=sid,
            solver_version=sv,
            provenance=Provenance.from_dict(d.get("provenance", {}), "provenance"),
            qcu_hull=None if hull_u is None else _as_run(hull_u, "qcu_hull"),
            qcf_hull=None if hull_f is None else _as_run(hull_f, "qcf_hull"),
            evidence_runs=[
                _as_run(r, "evidence_runs") for r in d.get("evidence_runs", [])
            ],
            regimes=[
                HintRegime.from_dict(r, f"regimes[{i}]")
                for i, r in enumerate(d.get("regimes", []))
            ],
            deviants=[_as_month(m, "deviants") for m in d.get("deviants", [])],
            audits=[str(a) for a in d.get("audits", [])],
        )


# --------------------------------------------------------------------------
# I/O
# --------------------------------------------------------------------------


def write_station_hints(hints_dir: Path, hints: StationHints) -> Path:
    """Atomically write ``<sid>.hints.json`` and return its path.

    Writes ``<name>.tmp`` then ``os.replace`` -- a killed run never leaves a
    truncated file that a later ``--hints`` load would have to reject.
    """
    hints_dir = Path(hints_dir)
    hints_dir.mkdir(parents=True, exist_ok=True)
    path = hints_dir / f"{hints.station_id}.hints.json"
    tmp = hints_dir / f"{hints.station_id}.hints.json.tmp"
    with open(tmp, "w", encoding="ascii") as fh:
        json.dump(hints.to_dict(), fh, indent=2, sort_keys=False)
        fh.write("\n")
    os.replace(tmp, path)
    return path


def read_station_hints(path: Path) -> StationHints:
    """Parse and validate a hints file; raise ``HintFormatError`` on any
    parse failure, unknown format, invalid class/code, or malformed tuple."""
    path = Path(path)
    try:
        with open(path, "r", encoding="ascii") as fh:
            raw = json.load(fh)
    except (OSError, json.JSONDecodeError) as exc:
        raise HintFormatError(f"{path}: cannot read/parse ({exc})") from exc
    hints = StationHints.from_dict(raw)
    return hints


def load_hint_sets(
    dirs: Sequence[Path],
    sid: str,
    solver_version: Optional[int] = None,
    log=None,
) -> List[Tuple[str, StationHints]]:
    """Return ``(tag, hints)`` for each dir that has a valid hints file.

    Order follows *dirs* (``--hints`` order); ``tag`` is the donor's
    ``provenance.base`` (fallback: the dir's parent-of-``hints`` name, else the
    dir name).  Unreadable / invalid files produce a **logged refusal** entry
    via *log* (a callable taking one string) -- never a silent skip.  When
    *solver_version* is given, a mismatch is warned (staleness) but the hints
    are still returned.
    """
    out: List[Tuple[str, StationHints]] = []

    def _emit(msg: str) -> None:
        if log is not None:
            log(msg)

    for d in dirs:
        d = Path(d)
        p = d / f"{sid}.hints.json"
        if not p.exists():
            continue
        try:
            hints = read_station_hints(p)
        except HintFormatError as exc:
            _emit(f"refuse-hints {p}: {exc}")
            continue
        tag = hints.provenance.base or _fallback_tag(d)
        if solver_version is not None and hints.solver_version != solver_version:
            _emit(
                f"warn-hints {p}: solver_version {hints.solver_version} "
                f"!= {solver_version} (possible staleness)"
            )
        out.append((tag, hints))
    return out


def _fallback_tag(hints_dir: Path) -> str:
    """A tag when provenance.base is empty: the base dir name if the layout is
    ``<base>/intermediate/hints`` else the dir's own name."""
    p = Path(hints_dir)
    if p.name == "hints" and p.parent.name == "intermediate":
        return p.parent.parent.name or p.name
    return p.name


# --------------------------------------------------------------------------
# Derivation from a solution (§4.4 / §4.6): class computed post-relabel
# --------------------------------------------------------------------------

# Deviant ``why`` labels that DEMOTE a regime to residual-partial.  Everything
# else (flutter, qcf-only-month, partial-month-evidence:*) is exempt.
_DEMOTING_WHY = frozenset(
    {"unexplained", "blend-unresolved", "no-single-S", "repair-merged"}
)


def _present_months(values: Dict[Ym, int]) -> List[Ym]:
    """Sorted months with a present value (present-value rule of
    ``merge_history._present_months`` / ``_present_months``)."""
    return sorted(
        ym
        for ym, v in values.items()
        if v is not None and v != ghcn_io.MISSING and v > -9990
    )


def _present_hull(values: Dict[Ym, int]) -> Optional[Tuple[Ym, Ym]]:
    months = _present_months(values)
    if not months:
        return None
    return (months[0], months[-1])


def _demotes(why: str) -> bool:
    return why in _DEMOTING_WHY


def _mi(ym: Ym) -> int:
    return ym[0] * 12 + (ym[1] - 1)


def hints_from_solution(
    sid: str,
    sol_dict: dict,
    qcu_values: Dict[Ym, int],
    qcf_values: Dict[Ym, int],
    base_name: str,
    stamp: str = "",
) -> StationHints:
    """Build a ``StationHints`` from a *residual* solution dict.

    ``sol_dict`` must be the pure residual solve (this runs BEFORE consolidation
    -- §6.7 invariant 1).  Class and the final ``deviant_months`` are computed
    here from the **post-relabel** deviants (``classify_partial_months`` has
    already run in the driver), so a long regime with one exempted partial
    month is not demoted (§4.4).
    """
    regimes_in = sol_dict.get("regimes", [])
    # Load-bearing no-laundering guard (§6.7 invariant 1): a consolidated
    # (donor-adopted) regime reaching this path would be re-exported as
    # independent evidence.  Raise (not assert -- must survive `python -O`).
    laundered = [
        r.get("source") for r in regimes_in if r.get("source", "residual") != "residual"
    ]
    if laundered:
        raise ValueError(
            "hints_from_solution must only ever see residual regimes "
            f"(§6.7 inv. 1); got non-residual source(s): {sorted(set(laundered))}"
        )

    ev = sol_dict.get("evidence") or {}
    ev_regimes = ev.get("regimes", [])
    solver_version = int(ev.get("solver_version", 0))

    # Post-relabel deviants: month -> demotes?
    demoting_mis = set()
    for entry in sol_dict.get("deviants", []):
        ym, why = tuple(entry[0]), entry[1]
        if _demotes(why):
            demoting_mis.add(_mi((ym[0], ym[1])))

    # Regime span boundaries (by begin month index), for span-scoped deviants.
    begins_mi = [_mi((r["begin"][0], r["begin"][1])) for r in ev_regimes]

    hint_regimes: List[HintRegime] = []
    for k, er in enumerate(ev_regimes):
        begin_mi = begins_mi[k]
        next_mi = begins_mi[k + 1] if k + 1 < len(ev_regimes) else None
        span_dev = sorted(
            mi
            for mi in demoting_mis
            if mi >= begin_mi and (next_mi is None or mi < next_mi)
        )
        n_constrained = int(er.get("n_constrained", 0))
        amb = [c for c in er.get("ambiguous_codes", [])]
        # Class, computed post-relabel (§4.4).
        if span_dev:
            cls = "residual-partial"
        elif n_constrained == 0:
            cls = "unconstrained"
        elif len(amb) >= 2:
            cls = "residual-ambiguous"
        else:
            cls = "residual-proven"
        # Phase 2 (§9.1): a vintage-hint policy adoption demotes a proven/
        # ambiguous reading so it can never re-launder downstream.
        if er.get("hint_influenced") and cls in (
            "residual-proven",
            "residual-ambiguous",
        ):
            cls = "residual-proven-hinted"

        def _m(v):
            return None if v is None else (v[0], v[1])

        evid = RegimeEvidence(
            cls=cls,
            n_constrained=n_constrained,
            constrained_runs=[
                (tuple(a), tuple(b)) for a, b in er.get("constrained_runs", [])
            ],
            first_constrained=_m(er.get("first_constrained")),
            last_constrained=_m(er.get("last_constrained")),
            ambiguous_codes=amb,
            flutter_months=[tuple(m) for m in er.get("flutter_months", [])],
            deviant_months=[(mi // 12, mi % 12 + 1) for mi in span_dev],
            boundary=Boundary.from_dict(
                er.get("boundary", {}), f"evidence.regimes[{k}]"
            ),
        )
        begin = tuple(er["begin"])
        hint_regimes.append(
            HintRegime(
                begin=begin,
                end=None,
                code=er["code"].strip().upper(),
                blend_day=er.get("blend_day"),
                evidence=evid,
            )
        )

    prov = Provenance(
        base=base_name,
        generated=stamp,
        kind=sol_dict.get("kind", "tob"),
        exact=bool(sol_dict.get("exact", False)),
        cost=list(sol_dict.get("cost")) if sol_dict.get("cost") is not None else None,
    )
    return StationHints(
        station_id=sid,
        solver_version=solver_version,
        provenance=prov,
        qcu_hull=_present_hull(qcu_values),
        qcf_hull=_present_hull(qcf_values),
        evidence_runs=[(tuple(a), tuple(b)) for a, b in ev.get("evidence_runs", [])],
        regimes=hint_regimes,
        deviants=sorted({(mi // 12, mi % 12 + 1) for mi in demoting_mis}),
        audits=[],
    )


# --------------------------------------------------------------------------
# Consolidation (§6): extend a station's timeline OUTSIDE its QCF hull using
# donor hints.  Primary-only per exterior window.  Subsumes merge_history.
# --------------------------------------------------------------------------

# Max code changes his_emit / TOBMain accept.
MAX_CODE_CHANGES = 200

# Class rank for precedence (§6.3 step 5).
_CLASS_RANK = {"residual-proven": 2, "residual-ambiguous": 1}


def vintage_hints_from_sets(
    hint_sets: Sequence[Tuple[str, StationHints]],
) -> List[Tuple[Ymd, str]]:
    """Phase 2 (§9.2): (date, code) events from donors' ADOPTABLE hint regimes
    for the solver's tie-break machinery.  Proven -> one event; ambiguous ->
    one event per ambiguity member (like PHR ``codes_after``).  Non-adoptable
    classes (incl. ``residual-proven-hinted``) contribute nothing, so vintage
    influence never chains through a prior policy adoption."""
    out: List[Tuple[Ymd, str]] = []
    for _tag, hints in hint_sets:
        for r in hints.regimes:
            if r.evidence.cls not in ADOPTABLE_CLASSES or not is_valid_code_label(
                r.code
            ):
                continue
            if r.evidence.cls == "residual-ambiguous":
                for c in r.evidence.ambiguous_codes:
                    if is_valid_code_label(c):
                        out.append((r.begin, c.strip().upper()))
            else:
                out.append((r.begin, r.code))
    return out


def _ym_add(ym: Ym, n: int = 1) -> Ym:
    y, m = ym
    m += n
    while m > 12:
        m -= 12
        y += 1
    while m < 1:
        m += 12
        y -= 1
    return (y, m)


def _effective_begin_month(begin: Ymd) -> Ym:
    """First month ``code_at`` (day-15 probe) can see a regime starting *begin*."""
    y, m, d = begin
    if d <= 15:
        return (y, m)
    return _ym_add((y, m), 1)


def _is_pad_code(code: str) -> bool:
    return is_no_adjustment(ghcn_io.decode_obtime(code), code)


@dataclass
class ConsolidationResult:
    regimes: List[dict]  # {begin,end,code,blend_day,source[,ambiguous_codes]}
    adopted_pre: int = 0
    adopted_post: int = 0
    promoted: bool = False
    notes: List[str] = field(default_factory=list)
    refusals: List[str] = field(default_factory=list)


class _HintView:
    """A donor hint set's monthly code / evidence function (effective-month
    rule)."""

    def __init__(self, tag: str, hints: StationHints, j: int):
        self.tag = tag
        self.hints = hints
        self.j = j
        self.timeline: List[Tuple[Ym, HintRegime]] = sorted(
            ((_effective_begin_month(r.begin), r) for r in hints.regimes),
            key=lambda x: x[0],
        )

    def active(self, ym: Ym) -> Optional[HintRegime]:
        cur = None
        for eff, r in self.timeline:
            if eff <= ym:
                cur = r
            else:
                break
        return cur

    def e(self, ym: Ym) -> bool:
        r = self.active(ym)
        if r is None:
            return False
        return any(a <= ym <= b for a, b in r.evidence.constrained_runs)


def _codes_compatible(a: HintRegime, b: HintRegime) -> bool:
    """§6.3 step 4: compatible iff ambiguity sets intersect, or both pad."""
    sa = set(a.evidence.ambiguous_codes)
    sb = set(b.evidence.ambiguous_codes)
    if sa & sb:
        return True
    return _is_pad_code(a.code) and _is_pad_code(b.code)


def _regime_current_timeline(regimes: List[dict]) -> List[Tuple[Ym, dict]]:
    return sorted(
        ((_effective_begin_month(tuple(r["begin"])), r) for r in regimes),
        key=lambda x: x[0],
    )


def _current_code_at(timeline: List[Tuple[Ym, dict]], ym: Ym) -> Optional[dict]:
    cur = None
    for eff, r in timeline:
        if eff <= ym:
            cur = r
        else:
            break
    return cur


def consolidate(
    residual_regimes: List[dict],
    current_kind: str,
    hint_sets: List[Tuple[str, StationHints]],
    qcu_values: Dict[Ym, int],
    qcf_values: Dict[Ym, int],
    current_offsets: Optional[Dict[str, Dict[Ym, int]]] = None,
    promote_pha_only: bool = False,
) -> ConsolidationResult:
    """Extend ``residual_regimes`` with donor hints outside the QCF hull.

    Returns a ``ConsolidationResult`` whose ``regimes`` is the consolidated
    plain-dict timeline (each carries ``source``; adopted ambiguous regimes
    also carry ``ambiguous_codes``).  The current in-hull timeline always wins;
    hints only fill the exterior windows PRE = ``[first_qcu, qcf_first)`` and
    POST = ``(qcf_last, last_qcu]``.
    """
    notes: List[str] = []
    refusals: List[str] = []

    def _unchanged(promoted: bool = False) -> ConsolidationResult:
        regs = [dict(r, source=r.get("source", "residual")) for r in residual_regimes]
        return ConsolidationResult(
            regimes=regs, notes=notes, refusals=refusals, promoted=promoted
        )

    qcf_months = _present_months(qcf_values)
    qcu_months = _present_months(qcu_values)
    if not qcf_months:
        refusals.append("no QCF months")
        return _unchanged()
    if not qcu_months:
        return _unchanged()

    first_qcu, last_qcu = qcu_months[0], qcu_months[-1]
    qcf_first, qcf_last = qcf_months[0], qcf_months[-1]
    qcu_set = set(qcu_months)
    both = [ym for ym in qcf_months if ym in qcu_set]
    first_known_tob = both[0] if both else qcf_first

    notes.append(
        f"qcu_hull=[{first_qcu[0]}-{first_qcu[1]:02d}.."
        f"{last_qcu[0]}-{last_qcu[1]:02d}]"
    )
    notes.append(
        f"qcf_hull=[{qcf_first[0]}-{qcf_first[1]:02d}.."
        f"{qcf_last[0]}-{qcf_last[1]:02d}] (current definitive)"
    )

    # Current in-hull timeline.  For a promoted pha-only station synthesize a
    # 24HR hull regime (zero adjustment) from first_known_tob.
    promoted = False
    if current_kind == "pha-only":
        if not promote_pha_only:
            return _unchanged()
        current_regimes = [
            {
                "begin": [first_known_tob[0], first_known_tob[1], 1],
                "end": None,
                "code": "24HR",
                "blend_day": None,
            }
        ]
        promoted = True
    else:
        current_regimes = [dict(r) for r in residual_regimes]

    cur_timeline = _regime_current_timeline(current_regimes)
    hull_end_reg = _current_code_at(cur_timeline, qcf_last)
    last_hull_code = hull_end_reg["code"] if hull_end_reg else "24HR"

    def _in_pre(ym: Ym) -> bool:
        return first_qcu <= ym < qcf_first

    def _in_post(ym: Ym) -> bool:
        return qcf_last < ym <= last_qcu

    views = [_HintView(tag, h, j) for j, (tag, h) in enumerate(hint_sets)]

    # In-hull contradiction telemetry (§6.9): current always wins, but a donor
    # regime evidence-backed inside the hull with an incompatible code is a
    # solver-error tell.
    for v in views:
        ym = qcf_first
        while ym <= qcf_last:
            if v.e(ym):
                hr = v.active(ym)
                cur = _current_code_at(cur_timeline, ym)
                if hr is not None and cur is not None:
                    sa = set(hr.evidence.ambiguous_codes)
                    cur_code = cur["code"]
                    compat = (cur_code in sa) or (
                        _is_pad_code(cur_code) and _is_pad_code(hr.code)
                    )
                    if not compat:
                        notes.append(
                            f"in-hull-contradiction@{ym[0]}-{ym[1]:02d} "
                            f"{v.tag}:{hr.code} vs current:{cur_code}"
                        )
            ym = _ym_add(ym, 1)

    adopted_pre_regs, n_pre = _consolidate_window(
        views,
        _in_pre,
        "pre",
        first_qcu,
        _ym_add(qcf_first, -1),
        current_offsets,
        qcu_set,
        refusals,
        notes,
    )
    adopted_post_regs, n_post = _consolidate_window(
        views,
        _in_post,
        "post",
        _ym_add(qcf_last, 1),
        last_qcu,
        current_offsets,
        qcu_set,
        refusals,
        notes,
    )

    # Assemble: PRE adopted + current in-hull + POST adopted, in time order.
    assembled: List[dict] = []
    for beg, code, blend, tag, amb in adopted_pre_regs:
        r = {
            "begin": list(beg),
            "end": None,
            "code": code,
            "blend_day": blend,
            "source": f"hint:{tag}",
        }
        if amb is not None:
            r["ambiguous_codes"] = list(amb)
        assembled.append(r)
    for r in current_regimes:
        assembled.append(dict(r, source=r.get("source", "residual")))
    for beg, code, blend, tag, amb in adopted_post_regs:
        r = {
            "begin": list(beg),
            "end": None,
            "code": code,
            "blend_day": blend,
            "source": f"hint:{tag}",
        }
        if amb is not None:
            r["ambiguous_codes"] = list(amb)
        assembled.append(r)

    assembled.sort(key=lambda r: tuple(r["begin"]))
    final = _dedupe_consecutive(assembled)

    # Guard: total code changes (§6.8).
    n_changes = _count_code_changes(final)
    if n_changes > MAX_CODE_CHANGES:
        refusals.append(f"too many code changes ({n_changes} > {MAX_CODE_CHANGES})")
        return _unchanged(promoted=False)

    return ConsolidationResult(
        regimes=final,
        adopted_pre=n_pre,
        adopted_post=n_post,
        promoted=promoted and (n_pre + n_post > 0),
        notes=notes,
        refusals=refusals,
    )


def _consolidate_window(
    views: List["_HintView"],
    in_window,
    label: str,
    win_lo: Ym,
    win_hi: Ym,
    current_offsets: Optional[Dict[str, Dict[Ym, int]]],
    qcu_set: set,
    refusals: List[str],
    notes: List[str],
) -> Tuple[List[Tuple[Ymd, str, Optional[int], str, Optional[List[str]]]], int]:
    """Pick the primary donor for one exterior window and return its adopted
    regimes as ``(begin, emit_code, blend_day, tag, ambiguous_codes|None)``
    plus the count of window months covered by an adopted hint code."""
    if win_lo > win_hi:
        return [], 0
    win_months = _months_inclusive(win_lo, win_hi)

    # Eligible sets: those with >=1 adoptable begin in-window (class + code +
    # boundary-crossing).  A boundary-crossing regime disqualifies the SET for
    # this window.
    eligible: Dict[int, List[HintRegime]] = {}
    for v in views:
        disq = False
        adoptable: List[HintRegime] = []
        for r in v.hints.regimes:
            if r.evidence.cls not in ADOPTABLE_CLASSES or not is_adoptable_code(r.code):
                continue
            eff = _effective_begin_month(r.begin)
            raw = (r.begin[0], r.begin[1])
            eff_in = in_window(eff)
            raw_in = in_window(raw)
            if eff_in != raw_in:
                refusals.append(
                    f"boundary-crossing {label} {v.tag}:{r.code} "
                    f"begin {r.begin[0]}-{r.begin[1]:02d}-{r.begin[2]:02d} "
                    f"(effective {eff[0]}-{eff[1]:02d} in-window={eff_in}, "
                    f"raw in-window={raw_in}); set disqualified for window"
                )
                disq = True
                break
            if eff_in and raw_in:
                adoptable.append(r)
        if not disq and adoptable:
            eligible[v.j] = adoptable

    if not eligible:
        return [], 0

    # Conflict scan (§6.3 step 4): de-adopt regimes proven incompatible on a
    # shared evidence-backed month.
    de_adopted: set = set()  # (j, begin-tuple)
    for ym in win_months:
        parts = []
        for v in views:
            if v.j not in eligible:
                continue
            if not v.e(ym):
                continue
            r = v.active(ym)
            if r is None or r not in eligible[v.j]:
                continue
            parts.append((v, r))
        for a in range(len(parts)):
            for b in range(a + 1, len(parts)):
                (va, ra), (vb, rb) = parts[a], parts[b]
                if not _codes_compatible(ra, rb):
                    de_adopted.add((va.j, tuple(ra.begin)))
                    de_adopted.add((vb.j, tuple(rb.begin)))
                    refusals.append(
                        f"hint-conflict@{ym[0]}-{ym[1]:02d} "
                        f"{va.tag}:{ra.code} vs {vb.tag}:{rb.code}"
                    )

    # Recompute eligibility after de-adoption.
    for j in list(eligible):
        eligible[j] = [r for r in eligible[j] if (j, tuple(r.begin)) not in de_adopted]
        if not eligible[j]:
            del eligible[j]
    if not eligible:
        return [], 0

    # Precedence (§6.3 step 5): (best class rank, n window months e-backed, j).
    def _score(v: "_HintView") -> Tuple[int, int, int]:
        best_rank = max(_CLASS_RANK.get(r.evidence.cls, 0) for r in eligible[v.j])
        n_e = sum(1 for ym in win_months if v.e(ym))
        return (best_rank, n_e, v.j)

    cand_views = [v for v in views if v.j in eligible]
    primary = max(cand_views, key=_score)
    notes.append(
        f"{label}-primary={primary.tag} "
        f"(score={_score(primary)}; {len(cand_views)} candidate set(s))"
    )

    # Adopt the primary's non-de-adopted regimes with effective begin in-window,
    # in time order.  Following h_primary between begins is exactly holding the
    # previous adopted code (emission truncates), so we emit the regimes only.
    adopt_regs = sorted(
        eligible[primary.j], key=lambda r: _effective_begin_month(r.begin)
    )
    out: List[Tuple[Ymd, str, Optional[int], str, Optional[List[str]]]] = []
    covered = 0
    for idx, r in enumerate(adopt_regs):
        eff = _effective_begin_month(r.begin)
        # Coverage of this adopted regime within the window.
        nxt_eff = (
            _effective_begin_month(adopt_regs[idx + 1].begin)
            if idx + 1 < len(adopt_regs)
            else _ym_add(win_hi, 1)
        )
        cover_hi = _ym_add(nxt_eff, -1)
        if cover_hi > win_hi:
            cover_hi = win_hi

        amb = None
        if r.evidence.cls == "residual-ambiguous":
            ok = _ambiguous_identity_holds(
                r, eff, cover_hi, current_offsets, qcu_set, refusals
            )
            if not ok:
                continue
            amb = list(r.evidence.ambiguous_codes)

        emit_code = "24HR" if _is_pad_code(r.code) else r.code
        out.append((tuple(r.begin), emit_code, r.blend_day, primary.tag, amb))
        covered += sum(1 for ym in _months_inclusive(eff, cover_hi) if ym in qcu_set)

    return out, covered


def _ambiguous_identity_holds(
    regime: HintRegime,
    lo: Ym,
    hi: Ym,
    current_offsets: Optional[Dict[str, Dict[Ym, int]]],
    qcu_set: set,
    refusals: List[str],
) -> bool:
    """§6.4: a residual-ambiguous regime is adoptable over [lo, hi] only if all
    members of its ambiguity set share the same CURRENT-vintage offset on every
    covered QCU month."""
    if current_offsets is None:
        refusals.append(
            f"ambiguous-no-basis {regime.code} over "
            f"{lo[0]}-{lo[1]:02d}..{hi[0]}-{hi[1]:02d} (current_offsets None)"
        )
        return False
    codes = regime.evidence.ambiguous_codes
    for ym in _months_inclusive(lo, hi):
        if ym not in qcu_set:
            continue
        vals = {current_offsets.get(c, {}).get(ym) for c in codes}
        if len(vals) != 1 or None in vals:
            refusals.append(
                f"ambiguous-identity-diverges@{ym[0]}-{ym[1]:02d} "
                f"{{{','.join(codes)}}}"
            )
            return False
    return True


def _months_inclusive(a: Ym, b: Ym) -> List[Ym]:
    if a > b:
        return []
    out: List[Ym] = []
    cur = a
    while cur <= b:
        out.append(cur)
        cur = _ym_add(cur, 1)
    return out


def _dedupe_consecutive(regimes: List[dict]) -> List[dict]:
    out: List[dict] = []
    for r in regimes:
        if (
            out
            and out[-1]["code"] == r["code"]
            and out[-1].get("blend_day") == r.get("blend_day")
        ):
            continue
        out.append(r)
    return out


def _count_code_changes(regimes: List[dict]) -> int:
    changes = 0
    prev = None
    for r in regimes:
        if prev is None or r["code"] != prev:
            changes += 1
        prev = r["code"]
    return changes


# --------------------------------------------------------------------------
# Consolidate-only CLI (§6.8): iterate on consolidation without re-solving.
# --------------------------------------------------------------------------

_REPO = Path(__file__).resolve().parents[2]


def _is_conus(sid: str, lat: float, lon: float) -> bool:
    return sid.startswith("US") and 23.0 <= lat <= 50.0 and -126.0 <= lon <= -65.0


def _cli_paths(base: Path) -> dict:
    """Mirror reconstruct_his path mapping for a --base workspace.

    All derived state lives under the base dir (nothing under work/)."""
    base = Path(base)
    tag = base.name
    return {
        "base": base,
        "tag": tag,
        "solutions": base / "intermediate" / "solutions",
        "cache": base / "intermediate" / "tob_basis",
        "raw": base / "input" / "raw" / "tavg",
        "qcf": base / "output" / "qcf" / "tavg",
        "inv": base / "input" / "station.inv",
        "history": base / "intermediate" / "history",
        "hints": base / "intermediate" / "hints",
        "tob_bin": _REPO / "bin" / "TOBMain",
        "scratch": base / "scratch",
    }


def _station_list(paths: dict, stations_arg: Optional[str]) -> List[str]:
    if stations_arg:
        return [s.strip() for s in stations_arg.split(",") if s.strip()]
    return sorted(p.name[: -len(".json")] for p in paths["solutions"].glob("*.json"))


def _cached_offsets(paths, sid, coord):
    """Chosen-coord basis offsets from cache only (never runs TOBMain)."""
    try:
        from tob_basis import BasisRunner

        runner = BasisRunner(paths["tob_bin"], paths["scratch"], paths["cache"])
        bases = runner.get_bases_cached(
            sid, paths["raw"] / f"{sid}.raw.tavg", [tuple(coord)]
        )
        if bases:
            return bases[0].code_offsets_by_ym
    except Exception:
        pass
    return None


def cmd_consolidate(paths: dict, args) -> int:
    import his_emit

    inv = ghcn_io.read_inventory(paths["inv"]) if paths["inv"].exists() else {}
    sids = _station_list(paths, args.stations)
    n_written = 0
    n_adopted = 0
    for sid in sids:
        sol_path = paths["solutions"] / f"{sid}.json"
        if not sol_path.exists():
            continue
        with open(sol_path) as fh:
            sol = json.load(fh)
        # Always start from the pure residual regimes (idempotency).
        residual_regimes = sol.get("residual_regimes")
        if residual_regimes is None:
            residual_regimes = sol.get("regimes", [])
        hc = sol.get("hint_consolidation", {})
        source_kind = hc.get("source_kind")
        if source_kind is None:
            source_kind = (
                "pha-only"
                if "hint-promoted-pha-only" in sol.get("audits", [])
                else sol.get("kind", "tob")
            )

        raw_path = paths["raw"] / f"{sid}.raw.tavg"
        qcf_path = paths["qcf"] / f"{sid}.qcf.tavg"
        if not raw_path.exists() or not qcf_path.exists():
            continue
        raw = ghcn_io.read_station_data(raw_path)
        qcf = ghcn_io.read_station_data(qcf_path)
        coord = sol.get("coord") or [
            inv[sid].lat if sid in inv else 0.0,
            inv[sid].lon if sid in inv else 0.0,
        ]
        rec = inv.get(sid)
        conus = _is_conus(sid, rec.lat, rec.lon) if rec else sid.startswith("US")
        if not conus:
            continue

        hint_sets = load_hint_sets(args.hint_dirs, sid, solver_version=None, log=print)
        if not hint_sets:
            continue

        cur_offsets = _cached_offsets(paths, sid, coord)
        cons = consolidate(
            residual_regimes,
            source_kind,
            hint_sets,
            raw.values,
            qcf.values,
            current_offsets=cur_offsets,
            promote_pha_only=args.promote_pha_only,
        )

        # Rebuild the solution dict deterministically from the residual solve.
        new_sol = dict(sol)
        new_sol["kind"] = source_kind
        new_sol["regimes"] = residual_regimes
        new_sol.pop("residual_regimes", None)
        new_sol.pop("hint_consolidation", None)
        new_sol["audits"] = [
            a
            for a in sol.get("audits", [])
            if not a.startswith("hints-consolidated") and a != "hint-promoted-pha-only"
        ]
        emit_kind = source_kind
        if cons.adopted_pre or cons.adopted_post:
            new_sol["residual_regimes"] = residual_regimes
            new_sol["regimes"] = cons.regimes
            new_sol["audits"].append(
                f"hints-consolidated:pre={cons.adopted_pre},post={cons.adopted_post}"
            )
            new_sol["hint_consolidation"] = {
                "source_kind": source_kind,
                "notes": cons.notes,
                "refusals": cons.refusals,
            }
            if cons.promoted:
                new_sol["kind"] = "tob"
                emit_kind = "tob"
            n_adopted += 1

        if args.dry_run:
            print(
                f"{sid}\tpre={cons.adopted_pre}\tpost={cons.adopted_post}"
                f"\tpromoted={cons.promoted}\t(dry-run)"
            )
            continue

        with open(sol_path, "w") as fh:
            json.dump(new_sol, fh, indent=1)
        n_written += 1

        # Re-emit .his for tob (incl. promoted) stations; pha-only get none.
        if emit_kind == "tob" and new_sol["regimes"] and rec is not None:
            first_year = raw.years[0] if raw.years else 1895
            regimes = [
                his_emit.Regime(begin=tuple(r["begin"]), obs_time=r["code"])
                for r in new_sol["regimes"]
            ]
            dms = ghcn_io.dms_quantize(coord[0], coord[1])
            his_emit.emit_station_his(
                sid, regimes, dms, rec, paths["history"], first_year
            )
        print(
            f"{sid}\tpre={cons.adopted_pre}\tpost={cons.adopted_post}"
            f"\tpromoted={cons.promoted}"
        )
    print(
        f"# consolidate: {n_written} solutions rewritten, {n_adopted} adopted "
        f"(dry_run={args.dry_run})",
        flush=True,
    )
    return 0


def cmd_derive(paths: dict, args) -> int:
    """(Re)write hint files from stored solutions carrying evidence (§6.8)."""
    sids = _station_list(paths, args.stations)
    n_written = 0
    n_skipped = 0
    for sid in sids:
        sol_path = paths["solutions"] / f"{sid}.json"
        if not sol_path.exists():
            continue
        with open(sol_path) as fh:
            sol = json.load(fh)
        if "evidence" not in sol:
            print(f"# warn {sid}: no evidence in solution (older solve); skipping")
            n_skipped += 1
            continue
        raw_path = paths["raw"] / f"{sid}.raw.tavg"
        qcf_path = paths["qcf"] / f"{sid}.qcf.tavg"
        if not raw_path.exists() or not qcf_path.exists():
            continue
        raw = ghcn_io.read_station_data(raw_path)
        qcf = ghcn_io.read_station_data(qcf_path)
        # Derive from the PURE residual regimes (never the consolidated ones).
        src = dict(sol)
        if "residual_regimes" in sol:
            src["regimes"] = sol["residual_regimes"]
            src["kind"] = sol.get("hint_consolidation", {}).get(
                "source_kind", sol.get("kind", "tob")
            )
        hints_obj = hints_from_solution(
            sid, src, raw.values, qcf.values, base_name=paths["tag"]
        )
        if not args.dry_run:
            write_station_hints(paths["hints"], hints_obj)
            n_written += 1
    print(f"# derive: {n_written} hint files written, {n_skipped} skipped", flush=True)
    return 0


def main(argv: Optional[Sequence[str]] = None) -> int:
    import argparse

    ap = argparse.ArgumentParser(description=__doc__)
    sub = ap.add_subparsers(dest="cmd", required=True)

    pc = sub.add_parser("consolidate", help="re-consolidate stored solutions")
    pc.add_argument("--base", default="data")
    pc.add_argument("--hints", action="append", default=None, metavar="DIR")
    pc.add_argument("--stations", default=None)
    pc.add_argument("--dry-run", action="store_true")
    pc.add_argument("--promote-pha-only", action="store_true")

    pd = sub.add_parser("derive", help="(re)write hints from stored solutions")
    pd.add_argument("--base", default="data")
    pd.add_argument("--stations", default=None)
    pd.add_argument("--dry-run", action="store_true")

    args = ap.parse_args(argv)
    paths = _cli_paths(Path(args.base))

    if args.cmd == "consolidate":
        if not args.hints:
            ap.error("consolidate requires at least one --hints DIR")
        active = paths["hints"].resolve()
        hint_dirs = []
        for h in args.hints:
            hp = Path(h).resolve()
            if hp == active:
                ap.error(f"--hints {h} is this base's own hints dir (self-hints)")
            if not hp.is_dir():
                ap.error(f"--hints {h}: not a directory")
            hint_dirs.append(hp)
        args.hint_dirs = hint_dirs
        return cmd_consolidate(paths, args)
    if args.cmd == "derive":
        return cmd_derive(paths, args)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
