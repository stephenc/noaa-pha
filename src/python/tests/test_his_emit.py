import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import ghcn_io  # noqa: E402
import his_emit  # noqa: E402
from his_emit import Regime  # noqa: E402


def _inv():
    return ghcn_io.Inv("USC00010063", 34.2553, -87.1814, 249.3, "ADDISON")


def _dms():
    return ghcn_io.dms_quantize(34.2553, -87.1814)


class TestEmit(unittest.TestCase):
    def test_three_regimes(self):
        regimes = [
            Regime(begin=(1902, 1, 1), obs_time="18HR"),
            Regime(begin=(1950, 3, 1), obs_time="07HR"),
            Regime(begin=(1971, 6, 15), obs_time="00SS"),
        ]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_station_his(
                "USC00010063", regimes, _dms(), _inv(), Path(td), 1902
            )
            warnings_out = []
            rows = his_emit.validate_his_file(path, 1902, warnings_out)
            self.assertEqual(warnings_out, [])
            self.assertEqual(len(rows), 3)
            self.assertEqual(
                [r.obs_time_raw.strip() for r in rows], ["18HR", "07HR", "00SS"]
            )
            # explicit obs time on every row, chronological
            self.assertEqual(rows[0].beg, (1902, 1, 1))
            # end = day before next begin (month rollover)
            self.assertEqual(rows[0].end, (1950, 2, 28))
            self.assertEqual(rows[1].end, (1971, 6, 14))
            # final row ends 9999/12/31
            self.assertEqual(rows[2].end, (9999, 12, 31))
            # same coordinate on every row
            for r in rows:
                self.assertEqual(
                    (r.lat_d, r.lat_m, r.lat_s),
                    (_dms().lat_d, _dms().lat_m, _dms().lat_s),
                )
                self.assertEqual(r.distdir, "")
            # elevation ft = round(249.3 * 3.28084)
            self.assertEqual(rows[0].elev, 818)
            # source 0 everywhere
            self.assertTrue(all(r.source == 0 for r in rows))

    def test_prepends_00hr_before_first_regime(self):
        # First regime begins mid-1948 but data starts 1902: an explicit
        # zero-adjustment 00HR row must cover 1902-01-01 .. 1948-06-30 so the
        # pre-history sunset default can never leak into adjusted months.
        regimes = [
            Regime(begin=(1948, 7, 1), obs_time="18HR"),
            Regime(begin=(1960, 1, 1), obs_time="07HR"),
        ]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_station_his(
                "USC00010063", regimes, _dms(), _inv(), Path(td), 1902
            )
            warnings_out = []
            rows = his_emit.validate_his_file(path, 1902, warnings_out)
            self.assertEqual(warnings_out, [])
            self.assertEqual(len(rows), 3)
            self.assertEqual(rows[0].beg, (1902, 1, 1))
            self.assertEqual(rows[0].end, (1948, 6, 30))
            self.assertEqual(rows[0].obs_time_raw.strip(), "00HR")
            self.assertEqual(rows[0].obs_code, 24)  # zero adjustment
            self.assertEqual(rows[1].beg, (1948, 7, 1))
            self.assertEqual(rows[1].obs_time_raw.strip(), "18HR")
            # prepended row repeats all verbatim fields
            self.assertEqual(rows[0].elev, rows[1].elev)
            self.assertEqual(rows[0].instr_height, rows[1].instr_height)
            self.assertEqual(rows[0].instruments, rows[1].instruments)

    def test_no_prepend_when_first_regime_covers_start(self):
        regimes = [Regime(begin=(1902, 1, 1), obs_time="18HR")]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_station_his(
                "USC00010063", regimes, _dms(), _inv(), Path(td), 1902
            )
            rows = his_emit.validate_his_file(path, 1902)
            self.assertEqual(len(rows), 1)
            self.assertEqual(rows[0].obs_time_raw.strip(), "18HR")

    def test_leap_day_before(self):
        regimes = [
            Regime(begin=(1999, 1, 1), obs_time="17HR"),
            Regime(begin=(2000, 3, 1), obs_time="07HR"),
        ]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_station_his(
                "USC00010063", regimes, _dms(), _inv(), Path(td), 1999
            )
            rows = his_emit.validate_his_file(path)
            self.assertEqual(rows[0].end, (2000, 2, 29))

    def test_mshr_split(self):
        regimes = [
            Regime(begin=(1902, 1, 1), obs_time="18HR"),
            Regime(begin=(1950, 3, 1), obs_time="07HR"),
        ]
        mshr = [
            ghcn_io.MshrRec(
                "USC00010063",
                (1931, 5, 10),
                (1950, 2, 28),
                34.25,
                -87.18,
                820.0,
                "1.2 MI NNE OF PO",
            ),
            ghcn_io.MshrRec(
                "USC00010063", (1902, 1, 1), (1931, 5, 9), 34.20, -87.10, 800.0, ""
            ),
        ]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_station_his(
                "USC00010063", regimes, _dms(), _inv(), Path(td), 1902, mshr=mshr
            )
            warnings_out = []
            rows = his_emit.validate_his_file(path, 1902, warnings_out)
            self.assertEqual(len(rows), 3)
            self.assertEqual(rows[1].beg, (1931, 5, 10))
            self.assertEqual(rows[1].obs_time_raw.strip(), "18HR")
            self.assertEqual(rows[1].distdir, "1.2 MI NNE")
            self.assertEqual(rows[0].end, (1931, 5, 9))
            self.assertEqual(rows[1].end, (1950, 2, 28))
            # move rows keep the single chosen coordinate
            self.assertEqual(rows[1].lat_d, rows[0].lat_d)
            # the intentional move surfaces as a dist/dir warning (and only
            # that row)
            self.assertEqual(len(warnings_out), 1)
            self.assertIn("dist/dir", warnings_out[0])

    def test_validator_warns_late_first_row(self):
        with tempfile.TemporaryDirectory() as td:
            dms = _dms()
            p = Path(td) / "USC00010063.his"
            r1 = ghcn_io.build_his_row(
                0, "USC00010063", (1950, 6, 1), (9999, 12, 31), dms, 818, "07HR"
            )
            p.write_text(r1 + "\n")
            warnings_out = []
            his_emit.validate_his_file(p, 1902, warnings_out)
            self.assertEqual(len(warnings_out), 1)
            self.assertIn("sunset", warnings_out[0])

    def test_validator_warns_verbatim_field_change(self):
        with tempfile.TemporaryDirectory() as td:
            dms = _dms()
            p = Path(td) / "USC00010063.his"
            r1 = ghcn_io.build_his_row(
                0, "USC00010063", (1940, 1, 1), (1949, 12, 31), dms, 818, "18HR"
            )
            r2 = ghcn_io.build_his_row(
                0, "USC00010063", (1950, 1, 1), (9999, 12, 31), dms, 820, "18HR"
            )
            p.write_text(r1 + "\n" + r2 + "\n")
            warnings_out = []
            his_emit.validate_his_file(p, warnings_out=warnings_out)
            self.assertEqual(len(warnings_out), 1)
            self.assertIn("elevation/instrument", warnings_out[0])

    def test_validator_rejects_disorder(self):
        with tempfile.TemporaryDirectory() as td:
            dms = _dms()
            p = Path(td) / "USC00010063.his"
            r1 = ghcn_io.build_his_row(
                0, "USC00010063", (1950, 1, 1), (1960, 12, 31), dms, 818, "07HR"
            )
            r2 = ghcn_io.build_his_row(
                0, "USC00010063", (1940, 1, 1), (1949, 12, 31), dms, 818, "18HR"
            )
            p.write_text(r1 + "\n" + r2 + "\n")
            with self.assertRaises(ValueError):
                his_emit.validate_his_file(p)

    def test_validator_rejects_unknown_code(self):
        with tempfile.TemporaryDirectory() as td:
            dms = _dms()
            p = Path(td) / "USC00010063.his"
            r1 = ghcn_io.build_his_row(
                0, "USC00010063", (1950, 1, 1), (9999, 12, 31), dms, 818, "XXXX"
            )
            p.write_text(r1 + "\n")
            with self.assertRaises(ValueError):
                his_emit.validate_his_file(p)

    def test_validator_rejects_bad_width(self):
        with tempfile.TemporaryDirectory() as td:
            p = Path(td) / "USC00010063.his"
            p.write_text("0 USC00010063 1950\n")
            with self.assertRaises(ValueError):
                his_emit.validate_his_file(p)

    def test_regimes_must_be_chronological(self):
        regimes = [
            Regime(begin=(1950, 3, 1), obs_time="07HR"),
            Regime(begin=(1902, 1, 1), obs_time="18HR"),
        ]
        with tempfile.TemporaryDirectory() as td:
            with self.assertRaises(ValueError):
                his_emit.emit_station_his(
                    "USC00010063", regimes, _dms(), _inv(), Path(td), 1902
                )

    def test_golden_first_line(self):
        # first_data_year 1949: first regime already begins before Jan 1
        # 1949, so no 00HR row is prepended and the file is a single row.
        regimes = [Regime(begin=(1948, 7, 1), obs_time="18HR")]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_station_his(
                "USC00010063", regimes, _dms(), _inv(), Path(td), 1949
            )
            lines = path.read_text().splitlines()
            self.assertEqual(len(lines), 1)
            expected = ghcn_io.build_his_row(
                source=0,
                station_id="USC00010063",
                beg=(1948, 7, 1),
                end=(9999, 12, 31),
                dms=_dms(),
                elev_ft=818,
                obs_time="18HR",
            )
            self.assertEqual(lines[0], expected)


class TestMetadataHis(unittest.TestCase):
    """Metadata-derived histories for stations outside the TOB gate."""

    def _inv(self):
        return ghcn_io.Inv("USR0000TEST", 61.2, -149.9, 40.0, "TEST AK")

    def test_merged_periods_and_relocation(self):
        phr = [
            ghcn_io.PhrRec("USR0000TEST", (1950, 1, 1), (1960, 1, 1), "0700"),
            ghcn_io.PhrRec("USR0000TEST", (1960, 1, 1), None, "1830"),
        ]
        mshr = [
            ghcn_io.MshrRec(
                "USR0000TEST", (1948, 3, 15), None, 61.2, -149.9, 130.0, ""
            ),
            ghcn_io.MshrRec(
                "USR0000TEST",
                (1972, 6, 2),
                None,
                61.25,
                -149.95,
                145.0,
                "MOVED 1.2MI NE",
            ),
        ]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_metadata_his(
                "USR0000TEST", phr, mshr, self._inv(), Path(td)
            )
            rows = his_emit.validate_his_file(path, warnings_out=[])
            # PHR boundaries carry no .his-visible change (obs is always
            # the no-TOB code), so rows are the MSHR-driven changes only.
            self.assertEqual(len(rows), 2)
            for r in rows:
                self.assertEqual(r.obs_time_raw, "24HR")
            # relocation row: dist/dir populated, elevation + coords change
            self.assertEqual(rows[1].distdir.strip(), "MOVED 1.2MI")
            self.assertEqual(rows[1].elev, 145)
            self.assertEqual(rows[1].beg, (1972, 6, 2))
            # contiguity and final sentinel end
            self.assertEqual(rows[1].end, (9999, 12, 31))

    def test_clamped_to_first_data_month(self):
        # Metadata periods that begin before the raw record (some MSHR rows
        # carry year-1 begin dates) collapse into a first row starting on
        # the first day of the earliest data month, carrying the metadata
        # state active at that date; a relocation marker on a clamped-away
        # boundary is dropped (the move predates the data).
        mshr = [
            ghcn_io.MshrRec("USR0000TEST", (1, 1, 1), None, 61.0, -149.0, 100.0, ""),
            ghcn_io.MshrRec(
                "USR0000TEST", (1930, 5, 20), None, 61.2, -149.9, 130.0, "MOVED 2MI N"
            ),
            ghcn_io.MshrRec("USR0000TEST", (1980, 7, 1), None, 61.3, -149.8, 140.0, ""),
        ]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_metadata_his(
                "USR0000TEST",
                None,
                mshr,
                self._inv(),
                Path(td),
                first_data=(1948, 6),
            )
            rows = his_emit.validate_his_file(path, warnings_out=[])
            self.assertEqual(rows[0].beg, (1948, 6, 1))
            # carries the 1930-move period's state, but not its dist/dir
            self.assertEqual(rows[0].elev, 130)
            self.assertEqual(rows[0].distdir.strip(), "")
            self.assertEqual(rows[1].beg, (1980, 7, 1))

    def test_international_station_gets_file(self):
        # MSHR carries international GHCN-D ids; south/west encode as
        # negative degrees with positive minutes/seconds
        inv = ghcn_io.Inv("ASN00086017", -38.4931, 145.0, 10.0, "MELBOURNE")
        phr = [ghcn_io.PhrRec("ASN00086017", (1950, 1, 1), None, "0900")]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_metadata_his("ASN00086017", phr, None, inv, Path(td))
            rows = his_emit.validate_his_file(path, warnings_out=[])
            self.assertEqual(len(rows), 1)
            self.assertEqual(rows[0].obs_time_raw, "24HR")
            self.assertEqual(rows[0].lat_d, -38)
            self.assertGreaterEqual(rows[0].lat_m, 0)

    def test_no_records_no_file(self):
        with tempfile.TemporaryDirectory() as td:
            self.assertIsNone(
                his_emit.emit_metadata_his("USR0000TEST", [], [], self._inv(), Path(td))
            )

    def test_obs_field_is_no_tob_code(self):
        phr = [ghcn_io.PhrRec("USR0000TEST", (1950, 1, 1), None, "0730")]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_metadata_his(
                "USR0000TEST", phr, None, self._inv(), Path(td)
            )
            rows = his_emit.validate_his_file(path, warnings_out=[])
            self.assertEqual(len(rows), 1)
            self.assertEqual(rows[0].obs_time_raw, "24HR")
            self.assertEqual(rows[0].obs_code, 24)

    def test_boundary_without_visible_change_merged(self):
        phr = [
            ghcn_io.PhrRec("USR0000TEST", (1950, 1, 1), None, "0700"),
            ghcn_io.PhrRec("USR0000TEST", (1955, 1, 1), None, "0700"),
        ]
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_metadata_his(
                "USR0000TEST", phr, None, self._inv(), Path(td)
            )
            rows = his_emit.validate_his_file(path, warnings_out=[])
            self.assertEqual(len(rows), 1)


class TestMetadataHisRealData(unittest.TestCase):
    """Guarded on the real workspace + HOMR zips being present."""

    def test_alaska_station_and_tobmain_verbatim(self):
        import shutil
        import subprocess

        repo = Path(__file__).resolve().parents[3]
        inv_path = repo / "data" / "input" / "station.inv"
        mshr_zip = repo / "data" / "mshr_enhanced.txt.zip"
        phr_zip = repo / "data" / "phr.txt.zip"
        tob_bin = repo / "bin" / "TOBMain"
        raw_dir = repo / "data" / "input" / "raw" / "tavg"
        if not (inv_path.exists() and mshr_zip.exists() and phr_zip.exists()):
            self.skipTest("real workspace not present")
        inv_all = ghcn_io.read_inventory(inv_path)
        # first Alaska-ish station (outside the CONUS gate) with data + HOMR
        sid = None
        for cand, rec in sorted(inv_all.items()):
            if not cand.startswith("US"):
                continue
            if 23.0 <= rec.lat <= 50.0 and -126.0 <= rec.lon <= -65.0:
                continue
            if not (raw_dir / f"{cand}.raw.tavg").exists():
                continue
            mshr = ghcn_io.read_mshr(mshr_zip, {cand}).get(cand)
            phr = ghcn_io.read_phr(phr_zip, {cand}).get(cand)
            if mshr or phr:
                sid = cand
                break
        if sid is None:
            self.skipTest("no non-CONUS US station with HOMR records")
        with tempfile.TemporaryDirectory() as td:
            td = Path(td)
            path = his_emit.emit_metadata_his(
                sid, phr, mshr, inv_all[sid], td / "history"
            )
            self.assertIsNotNone(path)
            rows = his_emit.validate_his_file(path, warnings_out=[])
            self.assertGreater(len(rows), 0)
            if not tob_bin.exists():
                self.skipTest("bin/TOBMain not built")
            # TOBMain must verbatim-copy this non-CONUS station even with a
            # .his file present
            for d in ("raw/tavg", "tob/tavg"):
                (td / "input" / d).mkdir(parents=True)
            shutil.copy(raw_dir / f"{sid}.raw.tavg", td / "input" / "raw" / "tavg")
            grep = [l for l in open(inv_path) if l.startswith(sid)]
            (td / "station.inv").write_text("".join(grep))
            # clone the workspace properties, re-rooting the TOB paths
            overrides = {
                "tob.path.station-element-data-in": f"{td}/input/raw/tavg/",
                "tob.path.station-element-data-out": f"{td}/input/tob/tavg/",
                "pha.path.station-history": f"{td}/history/",
                "pha.path.station-metadata": f"{td}/station.inv",
                "tob.logger.filename": f"{td}/tob.log",
            }
            out_lines = []
            for line in open(repo / "data" / "tob.properties"):
                key = line.split("=")[0].strip()
                if key in overrides:
                    out_lines.append(f"{key} = {overrides[key]}\n")
                else:
                    out_lines.append(line)
            ppath = td / "tob.properties"
            ppath.write_text("".join(out_lines))
            out = td / "input" / "tob" / "tavg" / f"{sid}.tob.tavg"
            # run WITH the .his present
            subprocess.run(
                [str(tob_bin), "-p", str(ppath)],
                check=True,
                capture_output=True,
                cwd=repo,
            )
            with_his = out.read_bytes()
            # run again WITHOUT any history
            for f in (td / "history").glob("*.his"):
                f.unlink()
            subprocess.run(
                [str(tob_bin), "-p", str(ppath)],
                check=True,
                capture_output=True,
                cwd=repo,
            )
            without_his = out.read_bytes()
            # the .his file must have no effect on a non-CONUS station
            self.assertEqual(with_his, without_his)
            # and the values must be numerically untouched vs raw
            raw_vals = ghcn_io.read_station_data(raw_dir / f"{sid}.raw.tavg").values
            out.write_bytes(with_his)
            tob_vals = ghcn_io.read_station_data(out).values
            self.assertEqual(raw_vals, tob_vals)


if __name__ == "__main__":
    unittest.main()
