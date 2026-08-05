import os
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import ghcn_io  # noqa: E402

REPO = Path(__file__).resolve().parents[3]
REAL_RAW = REPO / "data" / "input" / "raw" / "tavg" / "USC00010063.raw.tavg"
REAL_INV = REPO / "data" / "input" / "station.inv"
REAL_MSHR = REPO / "data" / "mshr_enhanced.txt.zip"
REAL_PHR = REPO / "data" / "phr.txt.zip"


class TestDecodeObtime(unittest.TestCase):
    def test_cases(self):
        cases = {
            "TRID": 30,
            "07HR": 7,
            "24HR": 24,
            "00HR": 24,  # hour 00 == midnight
            "  HR": 24,  # blank hour reads 0 -> 24
            "18HR": 18,
            "xxHR": 99,
            "0000": 24,  # sub-code chars 3:4 == '00'
            "9SS9": 28,  # 9x*9 pattern -> sub = chars 2:3
            "99SS": 28,  # char4 != '9' -> sub = chars 3:4
            "9909": 9,  # literal exception -> sub = chars 3:4
            "9919": 19,
            "9179": 17,  # 9x*9 -> chars 2:3 = '17'
            "00RS": 26,
            "00VR": 26,
            "00VA": 26,
            "00SR": 27,
            "00SS": 28,
            "00PM": 28,
            "0099": 99,
            "    ": 99,
            "00DE": 99,
            "00UN": 99,
            "00XX": 99,  # numeric parse fails
            "0007": 7,
            "0031": 99,  # explicit sentinel
        }
        for raw, expected in cases.items():
            self.assertEqual(
                ghcn_io.decode_obtime(raw), expected, f"decode_obtime({raw!r})"
            )


class TestDms(unittest.TestCase):
    def test_west_longitude_fields(self):
        dms = ghcn_io.dms_quantize(34.2553, -87.1814)
        self.assertGreaterEqual(dms.lat_m, 0)
        self.assertGreaterEqual(dms.lat_s, 0)
        self.assertLess(dms.lon_d, 0)
        self.assertGreaterEqual(dms.lon_m, 0)
        self.assertGreaterEqual(dms.lon_s, 0)
        # round-trip error <= 0.5 arc-sec
        self.assertLessEqual(abs(dms.qlat - 34.2553), 0.5 / 3600 + 1e-9)
        self.assertLessEqual(abs(dms.qlon - (-87.1814)), 0.5 / 3600 + 1e-9)
        # qlon formula: deg - min/60 - sec/3600
        self.assertAlmostEqual(
            dms.qlon, dms.lon_d - dms.lon_m / 60.0 - dms.lon_s / 3600.0
        )

    def test_exact_arcsecond(self):
        dms = ghcn_io.dms_quantize(35.0 + 5.0 / 60.0, -(89.0 + 58.0 / 60.0))
        self.assertEqual((dms.lat_d, dms.lat_m, dms.lat_s), (35, 5, 0))
        self.assertEqual((dms.lon_d, dms.lon_m, dms.lon_s), (-89, 58, 0))

    def test_west_longitude_below_one_degree_round_trips(self):
        # TOBMain subtracts the longitude minutes, so a zero degree field
        # still carries west.  63 v4 stations sit in (-1, 0).
        dms = ghcn_io.dms_quantize(35.0, -0.5)
        self.assertEqual((dms.lon_d, dms.lon_m, dms.lon_s), (0, 30, 0))
        self.assertAlmostEqual(dms.qlon, -0.5)
        self.assertAlmostEqual(
            dms.lon_d - dms.lon_m / 60.0 - dms.lon_s / 3600.0, -0.5
        )

    def test_east_longitude_borrows(self):
        # TOBMain SUBTRACTS the longitude minutes, so an east longitude is the
        # degree ABOVE it minus a positive remainder.
        for lon, want in ((15.58, (16, 25, 12)), (145.0, (145, 0, 0)),
                          (0.5, (1, 30, 0))):
            dms = ghcn_io.dms_quantize(35.0, lon)
            self.assertEqual((dms.lon_d, dms.lon_m, dms.lon_s), want, lon)
            self.assertAlmostEqual(
                dms.lon_d - dms.lon_m / 60.0 - dms.lon_s / 3600.0, lon, places=4
            )
            self.assertAlmostEqual(dms.qlon, lon, places=4)

    def test_south_latitude_borrows(self):
        # Both readers ADD the latitude minutes, so a south latitude is the
        # degree BELOW it plus a positive remainder.
        for lat, want in ((-0.5, (-1, 30, 0)), (-33.5, (-34, 30, 0)),
                          (-33.0, (-33, 0, 0)), (-0.21667, (-1, 47, 0))):
            dms = ghcn_io.dms_quantize(lat, 35.0)
            self.assertEqual((dms.lat_d, dms.lat_m, dms.lat_s), want, lat)
            # the reader's own formula, in both Fortran readers
            self.assertAlmostEqual(
                dms.lat_d + dms.lat_m / 60.0 + dms.lat_s / 3600.0, lat, places=4
            )
            self.assertAlmostEqual(dms.qlat, lat, places=4)


class TestHisRow(unittest.TestCase):
    def golden_row(self):
        # Hand-assembled per the FORMAT 90 column map (independent of
        # build_his_row's f-string construction).
        cols = [" "] * 152

        def put(start_1based, text):
            for i, ch in enumerate(text):
                cols[start_1based - 1 + i] = ch

        put(1, "0")
        put(2, "USC00010063")
        put(15, "19480701")
        put(24, "19500630")
        put(33, " 35  5  0")
        put(43, " -89 58  0")
        put(66, "  165")
        put(78, "18HR")
        put(87, "CRS  ")
        put(93, "MXMN ")
        return "".join(cols)

    def test_golden(self):
        dms = ghcn_io.dms_quantize(35.0 + 5.0 / 60.0, -(89.0 + 58.0 / 60.0))
        row = ghcn_io.build_his_row(
            source=0,
            station_id="USC00010063",
            beg=(1948, 7, 1),
            end=(1950, 6, 30),
            dms=dms,
            elev_ft=165,
            obs_time="18HR",
            instruments=["CRS", "MXMN"],
        )
        self.assertEqual(len(row), 152)
        self.assertEqual(row, self.golden_row())

    def test_field_positions(self):
        dms = ghcn_io.dms_quantize(34.2553, -87.1814)
        row = ghcn_io.build_his_row(
            source=2,
            station_id="USC00010063",
            beg=(1902, 3, 15),
            end=(9999, 12, 31),
            dms=dms,
            elev_ft=818,
            obs_time="07HR",
        )
        # 1-based cols 78-81 == 0-based [77:81]
        self.assertEqual(row[77:81], "07HR")
        self.assertEqual(row[0], "2")
        self.assertEqual(row[1:12], "USC00010063")
        self.assertEqual(row[14:22], "19020315")
        self.assertEqual(row[23:31], "99991231")

    def test_round_trip(self):
        dms = ghcn_io.dms_quantize(40.5, -105.25)
        for code in ["07HR", "18HR", "00RS", "00SR", "00SS", "TRID", "24HR"]:
            row = ghcn_io.build_his_row(
                source=0,
                station_id="USW00094074",
                beg=(1931, 1, 1),
                end=(1940, 12, 31),
                dms=dms,
                elev_ft=5280,
                obs_time=code,
            )
            parsed = ghcn_io.parse_his_row(row)
            self.assertEqual(parsed.source, 0)
            self.assertEqual(parsed.station_id, "USW00094074")
            self.assertEqual(parsed.beg, (1931, 1, 1))
            self.assertEqual(parsed.end, (1940, 12, 31))
            self.assertEqual(parsed.obs_time_raw.strip(), code)
            self.assertEqual(parsed.obs_code, ghcn_io.decode_obtime(code))
            self.assertEqual(parsed.lat_d, dms.lat_d)
            self.assertEqual(parsed.lon_d, dms.lon_d)
            self.assertAlmostEqual(parsed.lon_tob, dms.qlon)

    def test_blank_obs_and_dms(self):
        row = ghcn_io.build_his_row(
            source=0,
            station_id="USC00010063",
            beg=(1948, 7, 1),
            end=(1950, 6, 30),
            dms=None,
            elev_ft=0,
            obs_time="",
        )
        parsed = ghcn_io.parse_his_row(row)
        self.assertEqual(parsed.obs_code, 99)
        self.assertEqual(parsed.lat_d, 0.0)

    def test_missing_date_fills(self):
        row = ghcn_io.build_his_row(
            source=0,
            station_id="USC00010063",
            beg=(1948, 99, 99),
            end=(9999, 99, 99),
            dms=None,
            elev_ft=0,
            obs_time="18HR",
        )
        parsed = ghcn_io.parse_his_row(row, fill_end_year=2025)
        self.assertEqual(parsed.beg, (1948, 6, 15))
        self.assertEqual(parsed.end, (2025, 12, 31))
        parsed2 = ghcn_io.parse_his_row(row)
        self.assertEqual(parsed2.end, (9999, 12, 31))


class TestStationData(unittest.TestCase):
    def synthetic_lines(self):
        line1 = "USX00000001" + " " + "2001" + " -9999   " + "  1677  0" * 11
        return [line1]

    def test_synthetic_round_trip(self):
        with tempfile.TemporaryDirectory() as td:
            p = Path(td) / "x.raw.tavg"
            with open(p, "w") as fh:
                for ln in self.synthetic_lines():
                    fh.write(ln + "\n")
            sd = ghcn_io.read_station_data(p)
            self.assertEqual(sd.station_id, "USX00000001")
            self.assertNotIn((2001, 1), sd.values)
            self.assertEqual(sd.values[(2001, 2)], 1677)
            self.assertEqual(sd.flags[(2001, 2)], "  0")
            q = Path(td) / "y.raw.tavg"
            ghcn_io.write_station_data(q, sd, sd.data_type)
            self.assertEqual(p.read_text(), q.read_text())

    @unittest.skipUnless(REAL_RAW.exists(), "real data not present")
    def test_real_round_trip(self):
        sd = ghcn_io.read_station_data(REAL_RAW)
        self.assertEqual(sd.station_id, "USC00010063")
        with tempfile.TemporaryDirectory() as td:
            q = Path(td) / "out.tavg"
            ghcn_io.write_station_data(q, sd, sd.data_type)
            self.assertEqual(REAL_RAW.read_bytes(), q.read_bytes())


class TestInventory(unittest.TestCase):
    def test_synthetic(self):
        with tempfile.TemporaryDirectory() as td:
            p = Path(td) / "station.inv"
            with open(p, "w") as fh:
                fh.write("ACW00011604  57.7667   11.8667   18.0 SAVE" + " " * 26 + "\n")
                fh.write("short line\n")
            inv = ghcn_io.read_inventory(p)
            self.assertEqual(len(inv), 1)
            rec = inv["ACW00011604"]
            self.assertAlmostEqual(rec.lat, 57.7667)
            self.assertAlmostEqual(rec.lon, 11.8667)
            self.assertAlmostEqual(rec.elev_m, 18.0)
            self.assertEqual(rec.name, "SAVE")

    @unittest.skipUnless(REAL_INV.exists(), "real data not present")
    def test_real(self):
        inv = ghcn_io.read_inventory(REAL_INV)
        self.assertGreater(len(inv), 27000)
        rec = inv["USC00010063"]
        self.assertAlmostEqual(rec.lat, 34.2553)
        self.assertAlmostEqual(rec.lon, -87.1814)


class TestMshrPhr(unittest.TestCase):
    def _mshr_line(self, sid, begin, end, lat, lon, elev, reloc):
        line = [" "] * 1500

        def put(start, text):
            for i, ch in enumerate(text):
                line[start + i] = ch

        put(32, begin)
        put(41, end)
        put(239, sid)
        put(989, elev)
        put(1299, lat)
        put(1320, lon)
        put(1352, reloc)
        return "".join(line)

    def _phr_line(self, sid, begin, end, element, program, obstime):
        line = [" "] * 220

        def put(start, text):
            for i, ch in enumerate(text):
                line[start + i] = ch

        put(85, sid)
        put(106, begin)
        put(115, end)
        put(124, element)
        put(135, program)
        put(197, obstime)
        return "".join(line)

    def _zip_with(self, td, name, lines):
        import zipfile

        zp = Path(td) / "t.zip"
        with zipfile.ZipFile(zp, "w") as zf:
            zf.writestr(name, "\n".join(lines) + "\n")
        return zp

    def test_mshr_synthetic(self):
        with tempfile.TemporaryDirectory() as td:
            zp = self._zip_with(
                td,
                "mshr.txt",
                [
                    self._mshr_line(
                        "USC00010063",
                        "19480701",
                        "19500630",
                        "34.2553",
                        "-87.1814",
                        "820",
                        "MOVED 1 MI N",
                    ),
                    self._mshr_line(
                        "USC00099999", "19600101", "19701231", "30.0", "-90.0", "10", ""
                    ),
                    "too short",
                ],
            )
            recs = ghcn_io.read_mshr(zp, {"USC00010063"})
            self.assertEqual(list(recs), ["USC00010063"])
            r = recs["USC00010063"][0]
            self.assertEqual(r.begin, (1948, 7, 1))
            self.assertEqual(r.end, (1950, 6, 30))
            self.assertAlmostEqual(r.lat, 34.2553)
            self.assertAlmostEqual(r.lon, -87.1814)
            self.assertAlmostEqual(r.elev_ft, 820.0)
            self.assertEqual(r.relocation, "MOVED 1 MI N")

    def test_phr_synthetic(self):
        with tempfile.TemporaryDirectory() as td:
            zp = self._zip_with(
                td,
                "phr.txt",
                [
                    self._phr_line(
                        "USC00010063",
                        "19480701",
                        "19500630",
                        "TEMP",
                        "COOP SOD",
                        "1800",
                    ),
                    self._phr_line(
                        "USC00010063",
                        "19500701",
                        "19601231",
                        "TEMP",
                        "COOP SOD",
                        "9999",
                    ),
                    self._phr_line(
                        "USC00010063",
                        "19480701",
                        "19500630",
                        "PRCP",
                        "COOP SOD",
                        "0700",
                    ),
                ],
            )
            recs = ghcn_io.read_phr(zp, {"USC00010063"})
            self.assertEqual(len(recs["USC00010063"]), 2)
            self.assertEqual(recs["USC00010063"][0].obs_time, "1800")
            self.assertIsNone(recs["USC00010063"][1].obs_time)

    @unittest.skipUnless(
        REAL_MSHR.exists() and REAL_INV.exists(), "real data not present"
    )
    def test_mshr_real(self):
        recs = ghcn_io.read_mshr(REAL_MSHR, {"USC00010063"})
        if "USC00010063" in recs:
            for r in recs["USC00010063"]:
                if r.lat is not None:
                    self.assertTrue(20.0 < r.lat < 55.0, r)
                if r.lon is not None:
                    self.assertTrue(-130.0 < r.lon < -60.0, r)
                if r.begin is not None:
                    self.assertTrue(1800 <= r.begin[0] <= 2100, r)

    @unittest.skipUnless(REAL_PHR.exists(), "real data not present")
    def test_phr_real(self):
        recs = ghcn_io.read_phr(REAL_PHR, {"USC00010063"})
        for r in recs.get("USC00010063", []):
            if r.begin is not None:
                self.assertTrue(1800 <= r.begin[0] <= 2100, r)


class TestProperties(unittest.TestCase):
    def test_write(self):
        with tempfile.TemporaryDirectory() as td:
            p = Path(td) / "x.properties"
            ghcn_io.write_properties(p, {"a.b": "1", "c.d": "e"})
            self.assertEqual(p.read_text(), "a.b = 1\nc.d = e\n")


if __name__ == "__main__":
    unittest.main()
