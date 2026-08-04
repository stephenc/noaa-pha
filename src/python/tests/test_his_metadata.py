#!/usr/bin/env python3
"""Tests for the metadata-complete .his emission and the PHR fill helpers."""

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import ghcn_io  # noqa: E402
import his_emit  # noqa: E402
import his_metadata as hm  # noqa: E402
from phr_fill import runs_of  # noqa: E402


def mshr(begin, end=None, lat=None, lon=None, elev=None, reloc=""):
    return ghcn_io.MshrRec(
        station_id="USC00000001",
        begin=begin,
        end=end,
        lat=lat,
        lon=lon,
        elev_ft=elev,
        relocation=reloc,
    )


def phr(begin, end=None, obs=None, equip=""):
    return ghcn_io.PhrRec(
        station_id="USC00000001",
        begin=begin,
        end=end,
        obs_time=obs,
        equipment=equip,
    )


INV = ghcn_io.Inv(
    station_id="USC00000001", lat=40.0, lon=-100.0, elev_m=300.0, name="TEST"
)


class TestEquipmentMapping(unittest.TestCase):
    def test_known_equipment_maps_to_pha_token(self):
        self.assertEqual(hm.map_equipment("MMTS"), "MMTS")
        self.assertEqual(hm.map_equipment("mxmn"), "MXMN")

    def test_nimbus_uses_phas_spelling(self):
        # PHA's instr_strings says NIMBS, the PHR value says NIMBUS; emitting
        # the PHR spelling would silently never match.
        self.assertEqual(hm.map_equipment("NIMBUS"), "NIMBS")

    def test_unknown_equipment_maps_to_none(self):
        for v in ("UNKNOWN", "ATEMP", "", "  ", "NOT-A-REAL-CODE"):
            self.assertIsNone(hm.map_equipment(v), v)


class TestBuildRows(unittest.TestCase):
    def setUp(self):
        self.regimes = [
            his_emit.Regime(begin=(1950, 1, 1), obs_time="07HR"),
            his_emit.Regime(begin=(1980, 6, 1), obs_time="17HR"),
        ]

    def test_obs_timeline_is_preserved(self):
        rows = hm.build_rows(self.regimes, [], [], INV)
        self.assertEqual([r[1].obs_time for r in rows], ["07HR", "17HR"])
        self.assertEqual([r[0] for r in rows], [(1950, 1, 1), (1980, 6, 1)])

    def test_metadata_before_history_start_does_not_move_first_row(self):
        # TOBMain's tob_apply_year is the first row's begin year; an MSHR record
        # predating the history must not drag it earlier.
        recs = [mshr((1900, 1, 1), lat=41.0, lon=-101.0, elev=1000.0)]
        rows = hm.build_rows(self.regimes, recs, [], INV)
        self.assertEqual(rows[0][0], (1950, 1, 1))

    def test_elevation_change_creates_a_row(self):
        recs = [
            mshr((1950, 1, 1), end=(1959, 12, 31), elev=1000.0),
            mshr((1960, 1, 1), elev=1200.0),
        ]
        rows = hm.build_rows(self.regimes, recs, [], INV)
        self.assertIn((1960, 1, 1), [r[0] for r in rows])

    def test_relocation_marker_lands_only_on_its_own_row(self):
        recs = [
            mshr((1950, 1, 1), end=(1969, 12, 31), elev=1000.0),
            mshr((1970, 3, 1), elev=1000.0, reloc="1.5 MI NW"),
        ]
        rows = hm.build_rows(self.regimes, recs, [], INV)
        marked = [(d, dd) for d, _s, dd in rows if dd]
        self.assertEqual(len(marked), 1)
        self.assertEqual(marked[0][0], (1970, 3, 1))
        self.assertTrue(marked[0][1].startswith("1.5 MI NW"))

    def test_instrument_change_creates_a_row(self):
        recs = [
            phr((1950, 1, 1), end=(1984, 12, 31), obs="0700", equip="MXMN"),
            phr((1985, 1, 1), obs="0700", equip="MMTS"),
        ]
        rows = hm.build_rows(self.regimes, [], recs, INV)
        self.assertIn((1985, 1, 1), [r[0] for r in rows])
        by_date = {r[0]: r[1] for r in rows}
        self.assertEqual(by_date[(1985, 1, 1)].instruments, ("MMTS",))

    def test_unmapped_equipment_emits_no_instrument(self):
        recs = [phr((1950, 1, 1), obs="0700", equip="ATEMP")]
        rows = hm.build_rows(self.regimes, [], recs, INV)
        self.assertEqual(rows[0][1].instruments, ())

    def test_rows_with_no_visible_change_are_dropped(self):
        # Two MSHR periods with identical location/elevation and no relocation
        # must not produce two rows.
        recs = [
            mshr((1950, 1, 1), end=(1959, 12, 31), lat=40.0, lon=-100.0, elev=984.0),
            mshr((1960, 1, 1), lat=40.0, lon=-100.0, elev=984.0),
        ]
        rows = hm.build_rows(self.regimes, recs, [], INV)
        self.assertNotIn((1960, 1, 1), [r[0] for r in rows])


class TestRoundTrip(unittest.TestCase):
    def test_emitted_rows_parse_and_validate(self):
        import tempfile

        regimes = [his_emit.Regime(begin=(1950, 1, 1), obs_time="07HR")]
        recs = [
            mshr((1950, 1, 1), end=(1969, 12, 31), elev=1000.0),
            mshr((1970, 3, 1), elev=1200.0, reloc="2 MI S"),
        ]
        rows = hm.build_rows(regimes, recs, [], INV)
        with tempfile.TemporaryDirectory() as td:
            path = hm.write_rows("USC00000001", rows, Path(td))
            parsed = his_emit.validate_his_file(path, warnings_out=[])
            self.assertEqual(len(parsed), len(rows))
            # every row is exactly FORMAT-90 width and re-reads its obs code
            for row in parsed:
                self.assertNotEqual(row.obs_code, 99)


class TestRunsOf(unittest.TestCase):
    def test_contiguous_runs(self):
        self.assertEqual(runs_of([1, 2, 3, 7, 8, 20]), [(1, 3), (7, 8), (20, 20)])

    def test_empty(self):
        self.assertEqual(runs_of([]), [])


class TestObsVocabularyGuard(unittest.TestCase):
    """A raw HOMR clock time must never reach the .his obs-time field."""

    def test_raw_phr_clock_time_is_refused(self):
        import tempfile

        # 0730 decodes to 30 (TRID/sunset) and 0700 to 24 (midnight): both are
        # "valid" to a code check yet completely wrong.
        for raw in ("0730", "0700", "1830"):
            with tempfile.TemporaryDirectory() as td:
                p = Path(td) / "USC00000001.his"
                p.write_text(
                    ghcn_io.build_his_row(
                        source=0,
                        station_id="USC00000001",
                        beg=(1950, 1, 1),
                        end=(9999, 12, 31),
                        dms=ghcn_io.dms_quantize(40.0, -100.0),
                        elev_ft=984,
                        obs_time=raw,
                    )
                    + "\n"
                )
                with self.assertRaises(ValueError, msg=raw):
                    his_emit.validate_his_file(p, warnings_out=[])

    def test_mapped_labels_are_accepted(self):
        import tempfile

        for label in ("07HR", "24HR", "00SR", "00SS", "00RS", "00HR"):
            with tempfile.TemporaryDirectory() as td:
                p = Path(td) / "USC00000001.his"
                p.write_text(
                    ghcn_io.build_his_row(
                        source=0,
                        station_id="USC00000001",
                        beg=(1950, 1, 1),
                        end=(9999, 12, 31),
                        dms=ghcn_io.dms_quantize(40.0, -100.0),
                        elev_ft=984,
                        obs_time=label,
                    )
                    + "\n"
                )
                his_emit.validate_his_file(p, warnings_out=[])


class TestResolveObs(unittest.TestCase):
    def test_midnight_is_not_derived_by_sorting(self):
        from metadata_accuracy import resolve_obs

        # 00:43 brackets midnight (24HR) and 01HR.  Sorting the set numerically
        # would pick 01HR as the earlier hour, which is backwards.
        self.assertEqual(resolve_obs("0043"), "24HR")

    def test_truncates_to_the_hour_at_or_before_the_reading(self):
        from metadata_accuracy import resolve_obs

        self.assertEqual(resolve_obs("2330"), "23HR")
        self.assertEqual(resolve_obs("0730"), "07HR")

    def test_on_the_hour_is_unambiguous(self):
        from metadata_accuracy import resolve_obs

        self.assertEqual(resolve_obs("0700"), "07HR")

    def test_sentinels_resolve_to_specials_not_clock_times(self):
        from metadata_accuracy import resolve_obs

        self.assertEqual(resolve_obs("0630"), "00SR")
        self.assertEqual(resolve_obs("1830"), "00SS")

    def test_unusable_returns_none(self):
        from metadata_accuracy import resolve_obs

        for v in ("9999", "UNKN", "", None):
            self.assertIsNone(resolve_obs(v), v)


if __name__ == "__main__":
    unittest.main()
