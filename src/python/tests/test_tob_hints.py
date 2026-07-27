"""Tests for tob_hints: schema/I-O/validation (§10.1), derivation (§10.2),
and consolidation (§10.3-10.4).

All fixtures are synthetic; nothing under the workspace ``data/`` trees is
read or required.
"""

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import tob_hints as th  # noqa: E402


def _boundary(kind="record-start", **kw):
    return th.Boundary(kind=kind, **kw)


def _evid(
    cls="residual-proven",
    codes=("17HR",),
    runs=(((1951, 11), (2011, 2)),),
    n=423,
    first=(1951, 11),
    last=(2011, 2),
    boundary=None,
):
    return th.RegimeEvidence(
        cls=cls,
        n_constrained=n,
        constrained_runs=[tuple(r) for r in runs],
        first_constrained=first,
        last_constrained=last,
        ambiguous_codes=list(codes),
        flutter_months=[],
        deviant_months=[],
        boundary=boundary or _boundary(),
    )


def _regime(begin=(1951, 11, 1), code="17HR", blend_day=None, evidence=None):
    return th.HintRegime(
        begin=begin,
        end=None,
        code=code,
        blend_day=blend_day,
        evidence=evidence or _evid(),
    )


def _hints(sid="USC00299085", base="data-oldest", regimes=None, sv=1):
    return th.StationHints(
        station_id=sid,
        solver_version=sv,
        provenance=th.Provenance(
            base=base,
            generated="2026-07-27T18:04:11Z",
            kind="tob",
            exact=True,
            cost=[0, 0, 0, 4, 0, 0],
        ),
        qcu_hull=((1905, 5), (2011, 2)),
        qcf_hull=((1951, 11), (2011, 2)),
        evidence_runs=[((1951, 11), (1956, 12)), ((1958, 12), (2011, 2))],
        regimes=regimes if regimes is not None else [_regime()],
        deviants=[],
        audits=[],
    )


class TestSchemaIO(unittest.TestCase):
    def test_round_trip(self):
        h = _hints()
        d = h.to_dict()
        # JSON round-trip (strings, not tuples, on the wire).
        d2 = json.loads(json.dumps(d))
        h2 = th.StationHints.from_dict(d2)
        self.assertEqual(h2.to_dict(), d)

    def test_write_read_round_trip(self):
        with tempfile.TemporaryDirectory() as td:
            hd = Path(td) / "hints"
            p = th.write_station_hints(hd, _hints())
            self.assertTrue(p.exists())
            h2 = th.read_station_hints(p)
            self.assertEqual(h2.to_dict(), _hints().to_dict())

    def test_atomic_no_tmp_left_behind(self):
        with tempfile.TemporaryDirectory() as td:
            hd = Path(td) / "hints"
            th.write_station_hints(hd, _hints())
            leftovers = list(hd.glob("*.tmp"))
            self.assertEqual(leftovers, [])

    def test_partial_write_refused_on_load(self):
        with tempfile.TemporaryDirectory() as td:
            p = Path(td) / "USC00299085.hints.json"
            p.write_text('{"format": "tob-hints/1", "solver_ver')  # truncated
            with self.assertRaises(th.HintFormatError):
                th.read_station_hints(p)

    def test_unknown_format_refused(self):
        d = _hints().to_dict()
        d["format"] = "tob-hints/2"
        with self.assertRaises(th.HintFormatError):
            th.StationHints.from_dict(d)

    def test_bad_class_enum_refused(self):
        d = _hints().to_dict()
        d["regimes"][0]["evidence"]["class"] = "totally-made-up"
        with self.assertRaises(th.HintFormatError):
            th.StationHints.from_dict(d)

    def test_bad_code_label_refused(self):
        d = _hints().to_dict()
        d["regimes"][0]["code"] = "9999"
        with self.assertRaises(th.HintFormatError):
            th.StationHints.from_dict(d)

    def test_bad_ambiguous_code_refused(self):
        d = _hints().to_dict()
        d["regimes"][0]["evidence"]["ambiguous_codes"] = ["17HR", "DE"]
        with self.assertRaises(th.HintFormatError):
            th.StationHints.from_dict(d)

    def test_malformed_month_tuple_refused(self):
        d = _hints().to_dict()
        d["evidence_runs"][0][0] = [1951, 13]  # month 13
        with self.assertRaises(th.HintFormatError):
            th.StationHints.from_dict(d)

    def test_malformed_date_tuple_refused(self):
        d = _hints().to_dict()
        d["regimes"][0]["begin"] = [1951, 11]  # missing day
        with self.assertRaises(th.HintFormatError):
            th.StationHints.from_dict(d)

    def test_bad_boundary_kind_refused(self):
        d = _hints().to_dict()
        d["regimes"][0]["evidence"]["boundary"]["kind"] = "nonsense"
        with self.assertRaises(th.HintFormatError):
            th.StationHints.from_dict(d)

    def test_bad_day_source_refused(self):
        d = _hints().to_dict()
        d["regimes"][0]["evidence"]["boundary"]["day_source"] = "made-up-source"
        with self.assertRaises(th.HintFormatError):
            th.StationHints.from_dict(d)


class TestLoadHintSets(unittest.TestCase):
    def test_order_and_tags_preserved(self):
        with tempfile.TemporaryDirectory() as td:
            d1 = Path(td) / "a" / "intermediate" / "hints"
            d2 = Path(td) / "b" / "intermediate" / "hints"
            th.write_station_hints(d1, _hints(base="data"))
            th.write_station_hints(d2, _hints(base="data-oldest"))
            sets = th.load_hint_sets([d1, d2], "USC00299085")
            self.assertEqual([t for t, _ in sets], ["data", "data-oldest"])

    def test_missing_file_silently_skipped(self):
        with tempfile.TemporaryDirectory() as td:
            d1 = Path(td) / "hints"
            d1.mkdir(parents=True)
            logs = []
            sets = th.load_hint_sets([d1], "NOPE0000001", log=logs.append)
            self.assertEqual(sets, [])
            self.assertEqual(logs, [])  # absent != refused

    def test_invalid_file_logged_refusal(self):
        with tempfile.TemporaryDirectory() as td:
            d1 = Path(td) / "hints"
            d1.mkdir(parents=True)
            (d1 / "USC00299085.hints.json").write_text("{ not json")
            logs = []
            sets = th.load_hint_sets([d1], "USC00299085", log=logs.append)
            self.assertEqual(sets, [])
            self.assertTrue(any("refuse-hints" in m for m in logs), logs)

    def test_solver_version_mismatch_warns(self):
        with tempfile.TemporaryDirectory() as td:
            d1 = Path(td) / "hints"
            th.write_station_hints(d1, _hints(sv=1))
            logs = []
            sets = th.load_hint_sets(
                [d1], "USC00299085", solver_version=2, log=logs.append
            )
            self.assertEqual(len(sets), 1)  # returned despite mismatch
            self.assertTrue(any("staleness" in m for m in logs), logs)


def _sol_dict(regimes, deviants, ev_regimes, evidence_runs=None, kind="tob"):
    return {
        "station_id": "USTEST00001",
        "kind": kind,
        "exact": not deviants,
        "cost": [0, 0, 0, len(regimes), 0, 0],
        "regimes": regimes,
        "deviants": deviants,
        "evidence": {
            "solver_version": 1,
            "kind": kind,
            "evidence_runs": evidence_runs or [[[1950, 1], [1979, 12]]],
            "regimes": ev_regimes,
        },
    }


def _ev_regime(
    begin=(1950, 1, 1),
    code="17HR",
    n=359,
    codes=("17HR",),
    runs=(((1950, 1), (1979, 12)),),
    first=(1950, 1),
    last=(1979, 12),
    blend_day=None,
    boundary_kind="record-start",
    gap=None,
    day_resolved=False,
    day_source=None,
    feasible=None,
    hint_influenced=False,
):
    return {
        "begin": list(begin),
        "code": code,
        "blend_day": blend_day,
        "n_constrained": n,
        "constrained_runs": [[list(a), list(b)] for a, b in runs],
        "first_constrained": list(first) if first else None,
        "last_constrained": list(last) if last else None,
        "ambiguous_codes": list(codes),
        "flutter_months": [],
        "deviant_months": [],
        "hint_influenced": hint_influenced,
        "boundary": {
            "kind": boundary_kind,
            "gap_months_before": gap,
            "day_resolved": day_resolved,
            "day_source": day_source,
            "feasible_days": feasible,
        },
    }


class TestHintsFromSolution(unittest.TestCase):
    def _derive(self, sd, qcu=None, qcf=None):
        qcu = qcu or {(1950, 1): 100, (1979, 12): 100}
        qcf = qcf or {(1950, 1): 100, (1979, 12): 100}
        return th.hints_from_solution(
            sd["station_id"], sd, qcu, qcf, base_name="data", stamp="STAMP"
        )

    def test_partial_month_deviant_does_not_demote(self):
        sd = _sol_dict(
            regimes=[
                {"begin": [1950, 1, 1], "end": None, "code": "17HR", "blend_day": None}
            ],
            deviants=[[[1950, 1], "partial-month-evidence:hiatus-20mo"]],
            ev_regimes=[_ev_regime()],
        )
        h = self._derive(sd)
        self.assertEqual(h.regimes[0].evidence.cls, "residual-proven")
        self.assertEqual(h.regimes[0].evidence.deviant_months, [])

    def test_unexplained_deviant_demotes(self):
        sd = _sol_dict(
            regimes=[
                {"begin": [1950, 1, 1], "end": None, "code": "17HR", "blend_day": None}
            ],
            deviants=[[[1955, 6], "unexplained"]],
            ev_regimes=[_ev_regime()],
        )
        h = self._derive(sd)
        self.assertEqual(h.regimes[0].evidence.cls, "residual-partial")
        self.assertIn((1955, 6), h.regimes[0].evidence.deviant_months)

    def test_ambiguous_class_from_offset_identity(self):
        sd = _sol_dict(
            regimes=[
                {"begin": [1950, 1, 1], "end": None, "code": "24HR", "blend_day": None}
            ],
            deviants=[],
            ev_regimes=[_ev_regime(code="24HR", codes=("24HR", "00HR"))],
        )
        h = self._derive(sd)
        self.assertEqual(h.regimes[0].evidence.cls, "residual-ambiguous")

    def test_unconstrained_class(self):
        sd = _sol_dict(
            regimes=[
                {"begin": [1950, 1, 1], "end": None, "code": "17HR", "blend_day": None}
            ],
            deviants=[],
            ev_regimes=[_ev_regime(n=0, runs=(), first=None, last=None)],
        )
        h = self._derive(sd)
        self.assertEqual(h.regimes[0].evidence.cls, "unconstrained")

    def test_hull_present_value_rule(self):
        sd = _sol_dict(
            regimes=[
                {"begin": [1950, 1, 1], "end": None, "code": "17HR", "blend_day": None}
            ],
            deviants=[],
            ev_regimes=[_ev_regime()],
        )
        qcu = {(1940, 1): 100, (1979, 12): 100, (1980, 1): -9999}  # missing excluded
        qcf = {(1950, 1): 100, (1979, 12): 100}
        h = self._derive(sd, qcu=qcu, qcf=qcf)
        self.assertEqual(h.qcu_hull, ((1940, 1), (1979, 12)))
        self.assertEqual(h.qcf_hull, ((1950, 1), (1979, 12)))

    def test_boundary_kinds_gap_and_constrained(self):
        sd = _sol_dict(
            regimes=[
                {"begin": [1950, 1, 1], "end": None, "code": "17HR", "blend_day": None},
                {"begin": [1970, 1, 1], "end": None, "code": "07HR", "blend_day": None},
            ],
            deviants=[],
            ev_regimes=[
                _ev_regime(
                    code="17HR",
                    runs=(((1950, 1), (1965, 12)),),
                    first=(1950, 1),
                    last=(1965, 12),
                ),
                _ev_regime(
                    begin=(1970, 1, 1),
                    code="07HR",
                    runs=(((1970, 1), (1979, 12)),),
                    first=(1970, 1),
                    last=(1979, 12),
                    boundary_kind="gap",
                    gap=48,
                ),
            ],
        )
        h = self._derive(sd)
        self.assertEqual(h.regimes[0].evidence.boundary.kind, "record-start")
        self.assertEqual(h.regimes[1].evidence.boundary.kind, "gap")
        self.assertEqual(h.regimes[1].evidence.boundary.gap_months_before, 48)

    def test_pha_only_pseudo_regime(self):
        sd = _sol_dict(
            regimes=[],
            deviants=[],
            ev_regimes=[_ev_regime(code="24HR", codes=("24HR", "00HR"))],
            kind="pha-only",
        )
        h = self._derive(sd)
        self.assertEqual(len(h.regimes), 1)
        self.assertEqual(h.regimes[0].code, "24HR")
        self.assertEqual(h.regimes[0].evidence.ambiguous_codes, ["24HR", "00HR"])

    def test_laundering_assertion_fires_on_hint_source(self):
        sd = _sol_dict(
            regimes=[
                {
                    "begin": [1950, 1, 1],
                    "end": None,
                    "code": "17HR",
                    "blend_day": None,
                    "source": "hint:oldest",
                }
            ],
            deviants=[],
            ev_regimes=[_ev_regime()],
        )
        # Now an explicit ValueError (was a bare assert) so it survives -O.
        with self.assertRaises(ValueError):
            self._derive(sd)

    def test_round_trips_through_schema(self):
        sd = _sol_dict(
            regimes=[
                {"begin": [1950, 1, 1], "end": None, "code": "17HR", "blend_day": None}
            ],
            deviants=[],
            ev_regimes=[_ev_regime()],
        )
        h = self._derive(sd)
        h2 = th.StationHints.from_dict(json.loads(json.dumps(h.to_dict())))
        self.assertEqual(h2.to_dict(), h.to_dict())


class TestPhase2Laundering(unittest.TestCase):
    """§9.1 -- policy adoptions export as residual-proven-hinted (never
    re-adoptable); §9.2 -- vintage_hints_from_sets filters correctly."""

    def _derive(self, sd):
        qcu = {(1950, 1): 100, (1979, 12): 100}
        return th.hints_from_solution(
            sd["station_id"], sd, qcu, qcu, base_name="data", stamp=""
        )

    def test_hint_influenced_demotes_proven(self):
        sd = _sol_dict(
            regimes=[
                {"begin": [1950, 1, 1], "end": None, "code": "17HR", "blend_day": None}
            ],
            deviants=[],
            ev_regimes=[_ev_regime(hint_influenced=True)],
        )
        h = self._derive(sd)
        self.assertEqual(h.regimes[0].evidence.cls, "residual-proven-hinted")

    def test_hint_influenced_demotes_ambiguous(self):
        sd = _sol_dict(
            regimes=[
                {"begin": [1950, 1, 1], "end": None, "code": "24HR", "blend_day": None}
            ],
            deviants=[],
            ev_regimes=[
                _ev_regime(code="24HR", codes=("24HR", "00HR"), hint_influenced=True)
            ],
        )
        h = self._derive(sd)
        self.assertEqual(h.regimes[0].evidence.cls, "residual-proven-hinted")

    def test_residual_proven_hinted_not_adoptable(self):
        self.assertNotIn("residual-proven-hinted", th.ADOPTABLE_CLASSES)
        self.assertIn("residual-proven-hinted", th.EVIDENCE_CLASSES)

    def test_residual_proven_hinted_not_consolidated(self):
        # A donor regime demoted to residual-proven-hinted is never adopted.
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        r = _hr(
            (1951, 11, 1),
            "17HR",
            cls="residual-proven-hinted",
            runs=[((1951, 11), (1960, 12))],
        )
        donor = _sh([r])
        res = th.consolidate(
            residual,
            "tob",
            [("d", donor)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
        )
        self.assertEqual(res.adopted_post, 0)

    def test_vintage_hints_from_sets_proven_and_ambiguous(self):
        proven = _hr((1951, 11, 1), "17HR")
        amb = _hr(
            (1960, 1, 1), "00SS", cls="residual-ambiguous", codes=["00SS", "18HR"]
        )
        hinted = _hr((1970, 1, 1), "07HR", cls="residual-proven-hinted")
        partial = _hr((1980, 1, 1), "08HR", cls="residual-partial")
        sh = _sh([proven, amb, hinted, partial])
        vh = th.vintage_hints_from_sets([("d", sh)])
        # proven -> one; ambiguous -> two (per member); hinted/partial -> none.
        self.assertIn(((1951, 11, 1), "17HR"), vh)
        self.assertIn(((1960, 1, 1), "00SS"), vh)
        self.assertIn(((1960, 1, 1), "18HR"), vh)
        self.assertNotIn(((1970, 1, 1), "07HR"), vh)
        self.assertEqual(len(vh), 3)


class TestDeriveRealSolve(unittest.TestCase):
    """Solve a synthetic station through residual_solver, then derive hints."""

    def _basis(self, yms, offs, sid="USQSYNTH000"):
        from tob_basis import Basis

        b = Basis(station_id=sid, coord=(40.0, -90.0))
        for code, per_month in offs.items():
            b.code_offsets[code] = dict(per_month)
            b.code_offsets_by_ym[code] = {
                (y, m): per_month[m] for (y, m) in yms if m in per_month
            }
        b.station_adjusted = True
        return b

    def test_holes_and_runs(self):
        import fp32
        import residual_solver as rs

        OFF17 = {
            1: -6,
            2: -6,
            3: -7,
            4: -6,
            5: -5,
            6: -4,
            7: -3,
            8: -3,
            9: -4,
            10: -5,
            11: -5,
            12: -6,
        }
        OFF24 = {m: 0 for m in range(1, 13)}
        yms = [
            (y, m)
            for y in range(1950, 1980)
            for m in range(1, 13)
            if not (1960 <= y <= 1961)  # 2-year hole
        ]
        s_era = fp32.f32(-0.3)
        qcu, qcf = {}, {}
        import random

        rng = random.Random(5)
        for y, m in yms:
            v = rng.randint(-300, 3200)
            qcu[(y, m)] = v
            qcf[(y, m)] = fp32.pha_qcf(v + OFF17[m], s_era)
        basis = self._basis(yms, {"17HR": OFF17, "24HR": OFF24})
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="USQHOLE0001")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost}")
        d = sol.to_dict()
        h = th.hints_from_solution("USQHOLE0001", d, qcu, qcf, base_name="data")
        self.assertEqual(h.regimes[0].code, "17HR")
        self.assertEqual(h.regimes[0].evidence.cls, "residual-proven")
        # Hole splits the single regime's constrained runs into two.
        self.assertEqual(len(h.regimes[0].evidence.constrained_runs), 2)
        # Station-level evidence_runs also show the hole.
        self.assertEqual(len(h.evidence_runs), 2)


def _vrange(y0, m0, y1, m1):
    out = {}
    y, m = y0, m0
    while (y, m) <= (y1, m1):
        out[(y, m)] = 100
        m += 1
        if m == 13:
            y, m = y + 1, 1
    return out


def _hr(begin, code, cls="residual-proven", runs=None, codes=None, blend=None):
    runs = runs or [((begin[0], begin[1]), (begin[0] + 40, begin[1]))]
    codes = codes or [code]
    ev = th.RegimeEvidence(
        cls=cls,
        n_constrained=400,
        constrained_runs=[tuple(r) for r in runs],
        first_constrained=runs[0][0],
        last_constrained=runs[-1][1],
        ambiguous_codes=codes,
        flutter_months=[],
        deviant_months=[],
        boundary=th.Boundary("record-start"),
    )
    return th.HintRegime(begin, None, code, blend, ev)


def _sh(regimes, base="donor", qcu_hull=None, qcf_hull=None):
    return th.StationHints(
        station_id="USTEST00001",
        solver_version=1,
        provenance=th.Provenance(base=base),
        qcu_hull=qcu_hull,
        qcf_hull=qcf_hull,
        evidence_runs=[],
        regimes=regimes,
    )


def _codes(regimes):
    return [(r["code"], tuple(r["begin"]), r.get("source")) for r in regimes]


class TestConsolidation(unittest.TestCase):
    def test_post_hull_adoption_flagship(self):
        residual = [
            {"begin": [1905, 5, 1], "end": None, "code": "00SS", "blend_day": None}
        ]
        donor = _sh([_hr((1951, 11, 1), "17HR", runs=[((1951, 11), (2011, 2))])])
        res = th.consolidate(
            residual,
            "tob",
            [("data-oldest", donor)],
            _vrange(1905, 5, 2011, 12),
            _vrange(1905, 5, 1944, 7),
        )
        self.assertTrue(res.adopted_post > 0)
        self.assertEqual(
            _codes(res.regimes),
            [
                ("00SS", (1905, 5, 1), "residual"),
                ("17HR", (1951, 11, 1), "hint:data-oldest"),
            ],
        )

    def test_pre_hull_adoption(self):
        residual = [
            {"begin": [1950, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        donor = _sh([_hr((1940, 1, 1), "18HR", runs=[((1940, 1), (1949, 12))])])
        res = th.consolidate(
            residual,
            "tob",
            [("d", donor)],
            _vrange(1940, 1, 1959, 12),
            _vrange(1950, 1, 1959, 12),
        )
        self.assertTrue(res.adopted_pre > 0)
        self.assertEqual(
            _codes(res.regimes),
            [("18HR", (1940, 1, 1), "hint:d"), ("07HR", (1950, 1, 1), "residual")],
        )

    def test_midmonth_begin_day_preserved(self):
        residual = [
            {"begin": [1905, 5, 1], "end": None, "code": "00SS", "blend_day": None}
        ]
        # begin day 18 -> effective December; both raw and effective in POST.
        donor = _sh([_hr((1951, 11, 18), "17HR", runs=[((1951, 12), (2011, 2))])])
        res = th.consolidate(
            residual,
            "tob",
            [("d", donor)],
            _vrange(1905, 5, 2011, 12),
            _vrange(1905, 5, 1944, 7),
        )
        adopted = [r for r in res.regimes if r["source"] == "hint:d"]
        self.assertEqual(tuple(adopted[0]["begin"]), (1951, 11, 18))

    def test_boundary_crossing_disqualifies_window(self):
        # begin day 18 in qcf_last month: effective POST, raw in hull -> refuse.
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        donor = _sh([_hr((1951, 11, 18), "17HR", runs=[((1951, 12), (1960, 1))])])
        res = th.consolidate(
            residual,
            "tob",
            [("d", donor)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1951, 11),
        )
        self.assertEqual(res.adopted_post, 0)
        self.assertTrue(
            any("boundary-crossing" in r for r in res.refusals), res.refusals
        )

    def test_qcu_clamp_excludes_outside(self):
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        # donor begins 1911 but QCU ends 1910 -> outside span, not adopted.
        donor = _sh([_hr((1911, 1, 1), "18HR", runs=[((1911, 1), (1915, 12))])])
        res = th.consolidate(
            residual,
            "tob",
            [("d", donor)],
            _vrange(1900, 1, 1910, 12),
            _vrange(1900, 1, 1909, 12),
        )
        self.assertEqual(res.adopted_post, 0)

    def test_missing_qcu_skips(self):
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        donor = _sh([_hr((1911, 1, 1), "18HR")])
        res = th.consolidate(
            residual, "tob", [("d", donor)], {}, _vrange(1900, 1, 1909, 12)
        )
        self.assertEqual(res.adopted_pre + res.adopted_post, 0)

    def test_no_qcf_refused(self):
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        donor = _sh([_hr((1911, 1, 1), "18HR")])
        res = th.consolidate(
            residual, "tob", [("d", donor)], _vrange(1900, 1, 1920, 12), {}
        )
        self.assertTrue(any("no QCF months" in r for r in res.refusals))

    def test_conflict_deadopts_both_leaves_rest(self):
        # Two donors, incompatible proven codes over the same POST months.
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        a = _sh([_hr((1951, 1, 1), "17HR", runs=[((1951, 1), (1960, 12))])], base="A")
        b = _sh([_hr((1951, 1, 1), "18HR", runs=[((1951, 1), (1960, 12))])], base="B")
        res = th.consolidate(
            residual,
            "tob",
            [("A", a), ("B", b)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
        )
        self.assertEqual(res.adopted_post, 0)
        self.assertTrue(any("hint-conflict" in r for r in res.refusals), res.refusals)

    def test_precedence_proven_beats_ambiguous(self):
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        # Compatible codes (share 17HR) so precedence, not conflict, decides.
        # Ambiguous donor listed LATER (would win ties) but proven must win.
        proven = _sh(
            [_hr((1951, 1, 1), "17HR", runs=[((1951, 1), (1960, 12))])], base="P"
        )
        amb = _sh(
            [
                _hr(
                    (1951, 1, 1),
                    "17HR",
                    cls="residual-ambiguous",
                    codes=["17HR", "07HR"],
                    runs=[((1951, 1), (1960, 12))],
                )
            ],
            base="Q",
        )
        res = th.consolidate(
            residual,
            "tob",
            [("P", proven), ("Q", amb)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
        )
        adopted = [r for r in res.regimes if r["source"].startswith("hint")]
        self.assertEqual(adopted[0]["code"], "17HR")
        self.assertEqual(adopted[0]["source"], "hint:P")

    def test_later_listed_wins_full_tie(self):
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        # Identical class + identical coverage -> later-listed (j larger) wins.
        run = [((1951, 1), (1960, 12))]
        a = _sh([_hr((1951, 1, 1), "17HR", runs=run)], base="A")
        b = _sh([_hr((1951, 1, 1), "17HR", runs=run)], base="B")
        res = th.consolidate(
            residual,
            "tob",
            [("A", a), ("B", b)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
        )
        adopted = [r for r in res.regimes if r["source"].startswith("hint")]
        self.assertEqual(adopted[0]["source"], "hint:B")

    def test_proven_pad_emits_24hr_row(self):
        # POST proven pad claim wins -> explicit 24HR row over held non-pad code.
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "17HR", "blend_day": None}
        ]
        pad = _sh([_hr((1951, 1, 1), "00HR", runs=[((1951, 1), (1960, 12))])], base="Z")
        res = th.consolidate(
            residual,
            "tob",
            [("Z", pad)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
        )
        adopted = [r for r in res.regimes if r["source"].startswith("hint")]
        self.assertEqual(adopted[0]["code"], "24HR")

    def test_ambiguous_refused_when_offsets_diverge(self):
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        amb = _sh(
            [
                _hr(
                    (1951, 1, 1),
                    "00SS",
                    cls="residual-ambiguous",
                    codes=["00SS", "18HR"],
                    runs=[((1951, 1), (1960, 12))],
                )
            ]
        )
        # current offsets diverge for 00SS vs 18HR in summer -> refuse.
        offs = {
            "00SS": {ym: 5 for ym in _vrange(1951, 1, 1960, 12)},
            "18HR": {
                ym: (5 if ym[1] in (12, 1, 2) else 9)
                for ym in _vrange(1951, 1, 1960, 12)
            },
        }
        res = th.consolidate(
            residual,
            "tob",
            [("d", amb)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
            current_offsets=offs,
        )
        self.assertEqual(res.adopted_post, 0)
        self.assertTrue(any("ambiguous-identity-diverges" in r for r in res.refusals))

    def test_ambiguous_adopted_when_identity_holds(self):
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        amb = _sh(
            [
                _hr(
                    (1951, 1, 1),
                    "00SS",
                    cls="residual-ambiguous",
                    codes=["00SS", "18HR"],
                    runs=[((1951, 1), (1960, 12))],
                )
            ]
        )
        offs = {
            "00SS": {ym: 5 for ym in _vrange(1951, 1, 1960, 12)},
            "18HR": {ym: 5 for ym in _vrange(1951, 1, 1960, 12)},
        }
        res = th.consolidate(
            residual,
            "tob",
            [("d", amb)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
            current_offsets=offs,
        )
        self.assertTrue(res.adopted_post > 0)
        adopted = [r for r in res.regimes if r["source"].startswith("hint")][0]
        self.assertEqual(adopted["ambiguous_codes"], ["00SS", "18HR"])

    def test_ambiguous_refused_when_offsets_none(self):
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        amb = _sh(
            [
                _hr(
                    (1951, 1, 1),
                    "00SS",
                    cls="residual-ambiguous",
                    codes=["00SS", "18HR"],
                    runs=[((1951, 1), (1960, 12))],
                )
            ]
        )
        res = th.consolidate(
            residual,
            "tob",
            [("d", amb)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
            current_offsets=None,
        )
        self.assertEqual(res.adopted_post, 0)

    def test_in_hull_contradiction_logged_current_wins(self):
        residual = [
            {"begin": [1900, 1, 1], "end": None, "code": "07HR", "blend_day": None}
        ]
        # Donor proven 18HR evidence-backed INSIDE the current hull.
        donor = _sh([_hr((1905, 1, 1), "18HR", runs=[((1905, 1), (1940, 12))])])
        res = th.consolidate(
            residual,
            "tob",
            [("d", donor)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
        )
        self.assertTrue(any("in-hull-contradiction" in n for n in res.notes), res.notes)
        # Current still wins in-hull.
        self.assertEqual(
            [r["code"] for r in res.regimes if r["source"] == "residual"], ["07HR"]
        )

    def test_pha_only_promotion(self):
        # pha-only current, exact; donor exterior 17HR -> promote to 24HR hull.
        donor = _sh([_hr((1951, 1, 1), "17HR", runs=[((1951, 1), (1960, 12))])])
        res = th.consolidate(
            [],
            "pha-only",
            [("d", donor)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
            promote_pha_only=True,
        )
        self.assertTrue(res.promoted)
        self.assertTrue(res.adopted_post > 0)
        codes = [r["code"] for r in res.regimes]
        self.assertIn("24HR", codes)  # the synthesized hull
        self.assertIn("17HR", codes)  # adopted exterior

    def test_pha_only_inert_without_promote(self):
        donor = _sh([_hr((1951, 1, 1), "17HR", runs=[((1951, 1), (1960, 12))])])
        res = th.consolidate(
            [],
            "pha-only",
            [("d", donor)],
            _vrange(1900, 1, 1960, 12),
            _vrange(1900, 1, 1950, 12),
            promote_pha_only=False,
        )
        self.assertFalse(res.promoted)
        self.assertEqual(res.regimes, [])

    def test_no_adoption_is_residual_only(self):
        residual = [
            {"begin": [1905, 5, 1], "end": None, "code": "00SS", "blend_day": None}
        ]
        res = th.consolidate(
            residual, "tob", [], _vrange(1905, 5, 2011, 12), _vrange(1905, 5, 1944, 7)
        )
        self.assertEqual(res.adopted_pre + res.adopted_post, 0)
        self.assertEqual([r["code"] for r in res.regimes], ["00SS"])


import types  # noqa: E402

import ghcn_io  # noqa: E402


def _write_station_data(path, sid, months):
    path.parent.mkdir(parents=True, exist_ok=True)
    by_year = {}
    for y, m in months:
        by_year.setdefault(y, {})[m] = 100
    lines = []
    for y in sorted(by_year):
        parts = [f"{sid:<11s}", " ", f"{y:4d}"]
        for m in range(1, 13):
            v = by_year[y].get(m, -9999)
            parts.append(f"{v:6d}   ")
        lines.append("".join(parts))
    path.write_text("\n".join(lines) + "\n", encoding="latin-1")


def _mrange(y0, m0, y1, m1):
    out = []
    y, m = y0, m0
    while (y, m) <= (y1, m1):
        out.append((y, m))
        m += 1
        if m == 13:
            y, m = y + 1, 1
    return out


class TestConsolidateCLI(unittest.TestCase):
    def _workspace(self, td):
        td = Path(td)
        paths = {
            "base": td / "data",
            "tag": "data",
            "solutions": td / "work" / "solutions",
            "cache": td / "work" / "tob_basis",
            "raw": td / "data" / "input" / "raw" / "tavg",
            "qcf": td / "data" / "output" / "qcf" / "tavg",
            "inv": td / "data" / "input" / "station.inv",
            "history": td / "data" / "intermediate" / "history",
            "hints": td / "data" / "intermediate" / "hints",
            "tob_bin": td / "bin" / "TOBMain",
            "scratch": td / "work" / "scratch",
        }
        for k in ("solutions", "raw", "qcf", "history", "hints"):
            paths[k].mkdir(parents=True, exist_ok=True)
        paths["inv"].parent.mkdir(parents=True, exist_ok=True)
        sid = "USC00299085"
        paths["inv"].write_text(
            f"{sid:<11s} {40.0:8.4f} {-100.0:9.4f} {300.0:6.1f} TEST\n"
        )
        _write_station_data(
            paths["raw"] / f"{sid}.raw.tavg", sid, _mrange(1905, 5, 2011, 12)
        )
        _write_station_data(
            paths["qcf"] / f"{sid}.qcf.tavg", sid, _mrange(1905, 5, 1944, 7)
        )
        # Residual solution: single 00SS regime, with evidence.
        sol = {
            "station_id": sid,
            "kind": "tob",
            "coord_index": 0,
            "regimes": [
                {"begin": [1905, 5, 1], "end": None, "code": "00SS", "blend_day": None}
            ],
            "segments": [],
            "deviants": [],
            "knife_edges": [],
            "audits": [],
            "cost": [0, 0, 0, 1, 0, 0],
            "exact": True,
            "stats": {},
            "coord": [40.0, -100.0],
            "coord_provenance": "inventory",
            "evidence": {
                "solver_version": 1,
                "kind": "tob",
                "evidence_runs": [[[1905, 5], [1944, 7]]],
                "regimes": [
                    _ev_regime(
                        begin=(1905, 5, 1),
                        code="00SS",
                        runs=(((1905, 5), (1944, 7)),),
                        first=(1905, 5),
                        last=(1944, 7),
                    )
                ],
            },
        }
        (paths["solutions"] / f"{sid}.json").write_text(json.dumps(sol, indent=1))
        # Donor hints: proven 17HR POST (keyed to the real station id).
        donor_dir = td / "donor" / "intermediate" / "hints"
        donor_hints = _sh(
            [_hr((1951, 11, 1), "17HR", runs=[((1951, 11), (2011, 2))])], base="oldest"
        )
        donor_hints.station_id = sid
        th.write_station_hints(donor_dir, donor_hints)
        return paths, sid, donor_dir

    def test_consolidate_emits_and_is_idempotent(self):
        with tempfile.TemporaryDirectory() as td:
            paths, sid, donor = self._workspace(td)
            args = types.SimpleNamespace(
                hint_dirs=[donor], stations=sid, dry_run=False, promote_pha_only=False
            )
            th.cmd_consolidate(paths, args)
            his_path = paths["history"] / f"{sid}.his"
            self.assertTrue(his_path.exists())
            rows = ghcn_io.read_station_data  # noqa (silence lints)
            his1 = his_path.read_bytes()
            sol1 = (paths["solutions"] / f"{sid}.json").read_text()
            self.assertIn("residual_regimes", sol1)
            self.assertIn("hints-consolidated", sol1)
            # Idempotent: re-run yields identical files.
            th.cmd_consolidate(paths, args)
            self.assertEqual(his_path.read_bytes(), his1)
            self.assertEqual((paths["solutions"] / f"{sid}.json").read_text(), sol1)
            # The consolidated .his has 00HR pad, 00SS, then 17HR.
            from his_emit import validate_his_file

            parsed = validate_his_file(his_path)
            labels = [ghcn_io.decode_obtime(r.obs_time_raw) for r in parsed]
            codes = [r.obs_time_raw.strip() for r in parsed]
            self.assertIn("17HR", codes)
            self.assertIn("00SS", codes)

    def test_dry_run_writes_nothing(self):
        with tempfile.TemporaryDirectory() as td:
            paths, sid, donor = self._workspace(td)
            before = (paths["solutions"] / f"{sid}.json").read_text()
            args = types.SimpleNamespace(
                hint_dirs=[donor], stations=sid, dry_run=True, promote_pha_only=False
            )
            th.cmd_consolidate(paths, args)
            self.assertFalse((paths["history"] / f"{sid}.his").exists())
            self.assertEqual((paths["solutions"] / f"{sid}.json").read_text(), before)

    def test_derive_backfills_hints(self):
        with tempfile.TemporaryDirectory() as td:
            paths, sid, donor = self._workspace(td)
            args = types.SimpleNamespace(stations=sid, dry_run=False)
            th.cmd_derive(paths, args)
            hp = paths["hints"] / f"{sid}.hints.json"
            self.assertTrue(hp.exists())
            h = th.read_station_hints(hp)
            self.assertEqual(h.regimes[0].code, "00SS")

    def test_derive_skips_solution_without_evidence(self):
        with tempfile.TemporaryDirectory() as td:
            paths, sid, donor = self._workspace(td)
            sp = paths["solutions"] / f"{sid}.json"
            sol = json.loads(sp.read_text())
            del sol["evidence"]
            sp.write_text(json.dumps(sol))
            args = types.SimpleNamespace(stations=sid, dry_run=False)
            th.cmd_derive(paths, args)
            self.assertFalse((paths["hints"] / f"{sid}.hints.json").exists())


class TestCodePredicates(unittest.TestCase):
    def test_valid_labels(self):
        self.assertTrue(th.is_valid_code_label("17HR"))
        self.assertTrue(th.is_valid_code_label("00SS"))
        self.assertTrue(th.is_valid_code_label("24HR"))
        self.assertTrue(th.is_valid_code_label("00HR"))

    def test_pads_are_adoptable_here(self):
        self.assertTrue(th.is_adoptable_code("24HR"))
        self.assertTrue(th.is_adoptable_code("00HR"))

    def test_sentinels_invalid(self):
        for lab in ("9999", "8888", "UN", "DE", "MI"):
            self.assertFalse(th.is_valid_code_label(lab), lab)
            self.assertFalse(th.is_adoptable_code(lab), lab)


if __name__ == "__main__":
    unittest.main()
