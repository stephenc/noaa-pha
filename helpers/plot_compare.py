#!/usr/bin/env python3
"""
Plot and compare two monthly station data files (raw or adjusted).

This tool visualizes temperature data from two files, showing both the
original series and their difference.

Supported input formats (auto-detected per line):
  A) "PW100010148 1900  12 monthly values..."
     - Station ID is the first whitespace token
     - Year is the second whitespace token
  B) "PW10001014831900  12 monthly values..."
     - Station ID is fixed-width 11 chars
     - Then a single extra code char (e.g. "3")
     - Then 4-digit year
     - Monthly values follow (whitespace-separated)

Monthly values are integers in hundredths of °C; missing = -9999.
"""

import argparse
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Optional

import matplotlib.pyplot as plt
import numpy as np


class GHCNMDataReader:
    """Read monthly station data files in the formats described in the module docstring."""

    def __init__(self, file_path: Path, verbose: bool = False):
        self.file_path = file_path
        self.verbose = verbose

        self.station_id: Optional[str] = None
        self.data: Dict[Tuple[int, int], float] = {}  # (year, month) -> value (°C)

        # If we see the "extra code char" format, track it (informational).
        self.extra_code: Optional[str] = None

        # Internal: warn only once if we encounter weird parse situations.
        self._warned_station_mismatch = False
        self._warned_short_months = False
        self._warned_mixed_formats = False
        self._seen_format_a = False
        self._seen_format_b = False

    def log(self, message: str) -> None:
        if self.verbose:
            print(message, file=sys.stderr)

    def warn(self, message: str) -> None:
        print(f"Warning: {message}", file=sys.stderr)

    @staticmethod
    def _is_plausible_year(y: int) -> bool:
        # Wide enough to not reject historical data, but still sanity-check.
        return 1600 <= y <= 2300

    def _parse_line(self, line: str, line_num: int) -> Optional[Tuple[str, int, List[int], Optional[str]]]:
        """
        Returns (station_id, year, month_values_ints, extra_code) or None if unparseable.

        month_values_ints is a list of up to 12 ints (we'll use whatever is available).
        """
        s = line.rstrip("\n")
        if not s or s.startswith("#"):
            return None

        # Try format B first: fixed 11-char station id + 1 char code + 4-digit year
        # Example: "PW10001014831900 -9999 ..."
        if len(s) >= 16:
            st_fixed = s[0:11]
            maybe_code = s[11]
            maybe_year = s[12:16]
            if maybe_code.isdigit() and maybe_year.isdigit():
                year_i = int(maybe_year)
                if self._is_plausible_year(year_i):
                    rest = s[16:].strip()
                    if rest:
                        try:
                            vals = [int(tok) for tok in rest.split()]
                        except ValueError:
                            vals = []
                        if vals:
                            self._seen_format_b = True
                            return st_fixed.strip(), year_i, vals[:12], maybe_code

        # Fallback to format A: whitespace tokens: station year v1..v12
        parts = s.split()
        if len(parts) >= 3:
            st = parts[0]
            try:
                year_i = int(parts[1])
            except ValueError:
                return None
            if not self._is_plausible_year(year_i):
                return None
            try:
                vals = [int(tok) for tok in parts[2:]]
            except ValueError:
                return None
            if vals:
                self._seen_format_a = True
                return st, year_i, vals[:12], None

        # Nothing matched
        self.log(f"Warning: Could not parse line {line_num}: {s!r}")
        return None

    def read(self) -> None:
        self.log(f"Reading {self.file_path}")

        with open(self.file_path, "r") as f:
            for line_num, line in enumerate(f, 1):
                parsed = self._parse_line(line, line_num)
                if parsed is None:
                    continue

                station_id, year, month_vals, extra_code = parsed

                if self.station_id is None:
                    self.station_id = station_id
                elif self.station_id != station_id and not self._warned_station_mismatch:
                    self.warn(
                        f"{self.file_path}: multiple station IDs found "
                        f"({self.station_id!r} and {station_id!r}); proceeding."
                    )
                    self._warned_station_mismatch = True

                if extra_code is not None:
                    if self.extra_code is None:
                        self.extra_code = extra_code
                    elif self.extra_code != extra_code:
                        # Not fatal; just informational.
                        self.warn(
                            f"{self.file_path}: multiple extra code characters seen "
                            f"({self.extra_code!r} and {extra_code!r})."
                        )

                # Warn once if file appears to mix both formats
                if self._seen_format_a and self._seen_format_b and not self._warned_mixed_formats:
                    self.warn(f"{self.file_path}: mixed line formats detected; parser will handle both.")
                    self._warned_mixed_formats = True

                if len(month_vals) < 12 and not self._warned_short_months:
                    self.warn(
                        f"{self.file_path}: line {line_num} has only {len(month_vals)} monthly values; "
                        f"expected 12. Missing months will be skipped."
                    )
                    self._warned_short_months = True

                # Store values
                for month in range(1, min(12, len(month_vals)) + 1):
                    v = month_vals[month - 1]
                    if v == -9999:
                        continue
                    self.data[(year, month)] = v / 100.0

        extra = f", extra_code={self.extra_code!r}" if self.extra_code is not None else ""
        self.log(f"Read {len(self.data)} data points from {self.station_id or 'unknown'}{extra}")

    def get_series(self) -> Tuple[List[Tuple[int, int]], List[float]]:
        """Get sorted time series data."""
        sorted_data = sorted(self.data.items())
        dates = [date for date, _ in sorted_data]
        values = [value for _, value in sorted_data]
        return dates, values

    def get_annual_series(self, min_months: int = 6) -> Tuple[List[int], List[float]]:
        """
        Get annual average time series data.

        Args:
            min_months: Minimum number of months required to calculate annual average

        Returns:
            Tuple of (years, annual_values)
        """
        from collections import defaultdict

        # Group data by year
        yearly_data: Dict[int, List[float]] = defaultdict(list)
        for (year, month), value in self.data.items():
            yearly_data[year].append(value)

        # Calculate annual averages (only if sufficient months)
        years = []
        annual_values = []
        for year in sorted(yearly_data.keys()):
            month_values = yearly_data[year]
            if len(month_values) >= min_months:
                years.append(year)
                annual_values.append(float(np.mean(month_values)))

        return years, annual_values


def plot_comparison(
    file1: Path,
    file2: Path,
    output: Path = None,
    title1: str = "Dataset 1",
    title2: str = "Dataset 2",
    verbose: bool = False,
    by_year: bool = False,
) -> None:
    """Plot comparison of two data files."""
    reader1 = GHCNMDataReader(file1, verbose)
    reader1.read()

    reader2 = GHCNMDataReader(file2, verbose)
    reader2.read()

    if not reader1.data:
        print(f"Error: No data found in {file1}", file=sys.stderr)
        return

    if not reader2.data:
        print(f"Error: No data found in {file2}", file=sys.stderr)
        return

    if by_year:
        # Get annual average time series
        years1, values1 = reader1.get_annual_series()
        years2, values2 = reader2.get_annual_series()

        if not years1:
            print(f"Error: No annual data in {file1}", file=sys.stderr)
            return
        if not years2:
            print(f"Error: No annual data in {file2}", file=sys.stderr)
            return

        # Find common years for difference calculation
        common_years = sorted(set(years1) & set(years2))
        if not common_years:
            print("Error: No overlapping years between files", file=sys.stderr)
            return

        # Create dictionaries for lookup
        data1_dict = dict(zip(years1, values1))
        data2_dict = dict(zip(years2, values2))
        diff_values = [data1_dict[y] - data2_dict[y] for y in common_years]

        x1 = [float(y) for y in years1]
        x2 = [float(y) for y in years2]
        x_common = [float(y) for y in common_years]
        time_label = "Year"

    else:
        # Get monthly time series
        dates1, values1 = reader1.get_series()
        dates2, values2 = reader2.get_series()

        # Find common dates for difference calculation
        common_dates = sorted(set(dates1) & set(dates2))
        if not common_dates:
            print("Error: No overlapping dates between files", file=sys.stderr)
            return

        diff_values = [reader1.data[d] - reader2.data[d] for d in common_dates]

        def date_to_decimal(date_tuple: Tuple[int, int]) -> float:
            year, month = date_tuple
            return year + (month - 0.5) / 12.0

        x1 = [date_to_decimal(d) for d in dates1]
        x2 = [date_to_decimal(d) for d in dates2]
        x_common = [date_to_decimal(d) for d in common_dates]
        time_label = "Year"

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 8), sharex=True)

    ax1.plot(x1, values1, "b-", label=title1, alpha=0.7, linewidth=0.8)
    ax1.plot(x2, values2, "r-", label=title2, alpha=0.7, linewidth=0.8)
    ax1.set_ylabel("Temperature (°C)")
    time_type = "Annual Average" if by_year else "Monthly"
    ax1.set_title(f"{time_type} Comparison: {reader1.station_id or 'Unknown Station'}")
    ax1.legend(loc="best")
    ax1.grid(True, alpha=0.3)

    ax2.plot(x_common, diff_values, "g-", alpha=0.7, linewidth=0.8)
    ax2.axhline(y=0, color="k", linestyle="--", alpha=0.5, linewidth=0.8)
    ax2.set_xlabel(time_label)
    ax2.set_ylabel(f"Difference (°C)\n({title1} - {title2})")
    ax2.set_title("Difference Between Datasets")
    ax2.grid(True, alpha=0.3)

    mean_diff = float(np.mean(diff_values))
    std_diff = float(np.std(diff_values))
    max_diff = float(np.max(np.abs(diff_values)))

    stats_text = (
        f"Mean diff: {mean_diff:.3f}°C\n"
        f"Std: {std_diff:.3f}°C\n"
        f"Max |diff|: {max_diff:.3f}°C"
    )
    ax2.text(
        0.02,
        0.98,
        stats_text,
        transform=ax2.transAxes,
        verticalalignment="top",
        bbox=dict(boxstyle="round", facecolor="wheat", alpha=0.5),
        fontsize=9,
    )

    plt.tight_layout()

    if output:
        output.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output, dpi=150, bbox_inches="tight")
        print(f"Saved plot to {output}")
        plt.close()
    else:
        def on_key(event):
            if event.key == "escape":
                plt.close()

        fig.canvas.mpl_connect("key_press_event", on_key)
        print("Press ESC to close the plot window")
        plt.show()


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Plot and compare two monthly station data files",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument("file1", type=Path, help="First data file")
    parser.add_argument("file2", type=Path, help="Second data file")
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="Output image file (PNG, PDF, etc.). If not specified, display interactively.",
    )
    parser.add_argument("--label1", default="Dataset 1", help="Label for first dataset")
    parser.add_argument("--label2", default="Dataset 2", help="Label for second dataset")
    parser.add_argument(
        "--by-year",
        action="store_true",
        help="Display annual averages instead of monthly data (requires at least 6 months per year)",
    )
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose output")

    args = parser.parse_args()

    if not args.file1.exists():
        print(f"Error: File not found: {args.file1}", file=sys.stderr)
        return 1
    if not args.file2.exists():
        print(f"Error: File not found: {args.file2}", file=sys.stderr)
        return 1

    try:
        plot_comparison(
            args.file1,
            args.file2,
            output=args.output,
            title1=args.label1,
            title2=args.label2,
            verbose=args.verbose,
            by_year=args.by_year,
        )
        return 0
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        if args.verbose:
            import traceback

            traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
