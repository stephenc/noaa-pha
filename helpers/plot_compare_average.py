#!/usr/bin/env python3
"""
Plot and compare network averages from two directories of station data files.

Computes the average temperature anomaly across all stations in each directory,
then visualizes the difference between the two network averages.

Averaging strategies:
  - equal-weight: Simple mean across all stations (current implementation)
  - gridded: Area-weighted gridding (future implementation)

Station filtering:
  - Optional glob pattern to filter station IDs
  - Filter applies to first dot-separated segment of filename
"""

import argparse
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from collections import defaultdict
from fnmatch import fnmatch

import matplotlib.pyplot as plt
import numpy as np


class GHCNMDataReader:
    """Read monthly station data files in GHCNM format."""

    def __init__(self, file_path: Path, verbose: bool = False):
        self.file_path = file_path
        self.verbose = verbose
        self.station_id: Optional[str] = None
        self.data: Dict[Tuple[int, int], float] = {}  # (year, month) -> value (°C)
        self._warned_station_mismatch = False

    def log(self, message: str) -> None:
        if self.verbose:
            print(message, file=sys.stderr)

    def warn(self, message: str) -> None:
        print(f"Warning: {message}", file=sys.stderr)

    @staticmethod
    def _is_plausible_year(y: int) -> bool:
        return 1600 <= y <= 2300

    def _parse_line(self, line: str, line_num: int) -> Optional[Tuple[str, int, List[int]]]:
        """Parse line and return (station_id, year, month_values_ints) or None."""
        s = line.rstrip("\n")
        if not s or s.startswith("#"):
            return None

        # Try fixed-width format first: 11-char station id + 1 char code + 4-digit year
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
                            return st_fixed.strip(), year_i, vals[:12]

        # Fallback to whitespace-separated format
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
                return st, year_i, vals[:12]

        return None

    def read(self) -> bool:
        """Read data file. Returns True if successful, False otherwise."""
        try:
            with open(self.file_path, "r") as f:
                for line_num, line in enumerate(f, 1):
                    parsed = self._parse_line(line, line_num)
                    if parsed is None:
                        continue

                    station_id, year, month_vals = parsed

                    if self.station_id is None:
                        self.station_id = station_id
                    elif self.station_id != station_id and not self._warned_station_mismatch:
                        self.warn(
                            f"{self.file_path}: multiple station IDs found "
                            f"({self.station_id!r} and {station_id!r})"
                        )
                        self._warned_station_mismatch = True

                    # Store values
                    for month in range(1, min(12, len(month_vals)) + 1):
                        v = month_vals[month - 1]
                        if v == -9999:
                            continue
                        self.data[(year, month)] = v / 100.0

            self.log(f"Read {len(self.data)} data points from {self.station_id or 'unknown'}")
            return len(self.data) > 0

        except Exception as e:
            self.warn(f"Error reading {self.file_path}: {e}")
            return False


class NetworkAverager:
    """Compute network average from multiple station data files."""

    def __init__(self, directory: Path, glob_filter: Optional[str] = None, verbose: bool = False):
        self.directory = directory
        self.glob_filter = glob_filter
        self.verbose = verbose
        self.station_data: Dict[str, Dict[Tuple[int, int], float]] = {}  # station_id -> {(year, month) -> value}

    def log(self, message: str) -> None:
        if self.verbose:
            print(message, file=sys.stderr)

    def _matches_filter(self, filename: str) -> bool:
        """Check if filename matches glob filter (applies to first dot-separated segment)."""
        if self.glob_filter is None:
            return True

        # Extract station ID (first dot-separated segment)
        station_id = filename.split('.')[0]
        return fnmatch(station_id, self.glob_filter)

    def load_stations(self) -> int:
        """Load all station files from directory. Returns number of stations loaded."""
        if not self.directory.exists():
            print(f"Error: Directory not found: {self.directory}", file=sys.stderr)
            return 0

        files = sorted(self.directory.iterdir())
        loaded_count = 0
        skipped_count = 0

        for file_path in files:
            if not file_path.is_file():
                continue

            # Apply glob filter to first dot-separated segment
            if not self._matches_filter(file_path.name):
                skipped_count += 1
                continue

            reader = GHCNMDataReader(file_path, verbose=self.verbose)
            if reader.read():
                if reader.station_id:
                    self.station_data[reader.station_id] = reader.data
                    loaded_count += 1
                else:
                    self.log(f"Skipping {file_path.name}: no station ID found")

        self.log(f"Loaded {loaded_count} stations from {self.directory}")
        if skipped_count > 0 and self.verbose:
            self.log(f"Skipped {skipped_count} files not matching filter '{self.glob_filter}'")

        return loaded_count

    def compute_equal_weight_average(self) -> Dict[Tuple[int, int], float]:
        """
        Compute simple equal-weight average across all stations.

        Returns dictionary mapping (year, month) -> average temperature (°C)
        """
        # Collect all values for each (year, month)
        monthly_values: Dict[Tuple[int, int], List[float]] = defaultdict(list)

        for station_id, data in self.station_data.items():
            for (year, month), value in data.items():
                monthly_values[(year, month)].append(value)

        # Compute mean for each (year, month)
        averages = {}
        for (year, month), values in monthly_values.items():
            if values:
                averages[(year, month)] = float(np.mean(values))

        self.log(f"Computed average for {len(averages)} time points using {len(self.station_data)} stations")
        return averages

    def compute_annual_equal_weight_average(self, min_months: int = 6) -> Dict[int, float]:
        """
        Compute annual equal-weight average across all stations.

        Args:
            min_months: Minimum number of months required to calculate annual average

        Returns:
            Dictionary mapping year -> average temperature (°C)
        """
        # First compute monthly averages
        monthly_averages = self.compute_equal_weight_average()

        # Group by year and calculate annual averages
        yearly_data: Dict[int, List[float]] = defaultdict(list)
        for (year, month), value in monthly_averages.items():
            yearly_data[year].append(value)

        # Calculate annual averages (only if sufficient months)
        annual_averages = {}
        for year, month_values in yearly_data.items():
            if len(month_values) >= min_months:
                annual_averages[year] = float(np.mean(month_values))

        self.log(f"Computed {len(annual_averages)} annual averages using {len(self.station_data)} stations")
        return annual_averages


def plot_network_comparison(
    dir1: Path,
    dir2: Path,
    output: Optional[Path] = None,
    label1: str = "Directory 1",
    label2: str = "Directory 2",
    glob_filter: Optional[str] = None,
    strategy: str = "equal-weight",
    verbose: bool = False,
    by_year: bool = False,
) -> None:
    """
    Plot comparison of network averages from two directories.

    Args:
        dir1: First directory containing station data files
        dir2: Second directory containing station data files
        output: Output image file (if None, display interactively)
        label1: Label for first network
        label2: Label for second network
        glob_filter: Optional glob pattern to filter station IDs
        strategy: Averaging strategy ('equal-weight' or future: 'gridded')
        verbose: Verbose output
        by_year: Display annual averages instead of monthly data
    """
    # Load and compute averages for both directories
    print(f"Loading stations from {dir1}...")
    avg1 = NetworkAverager(dir1, glob_filter, verbose)
    count1 = avg1.load_stations()
    if count1 == 0:
        print(f"Error: No data loaded from {dir1}", file=sys.stderr)
        return

    print(f"Loading stations from {dir2}...")
    avg2 = NetworkAverager(dir2, glob_filter, verbose)
    count2 = avg2.load_stations()
    if count2 == 0:
        print(f"Error: No data loaded from {dir2}", file=sys.stderr)
        return

    # Compute averages based on strategy and time resolution
    if strategy == "equal-weight":
        if by_year:
            print(f"Computing annual equal-weight averages...")
            series1 = avg1.compute_annual_equal_weight_average()
            series2 = avg2.compute_annual_equal_weight_average()
        else:
            print(f"Computing monthly equal-weight averages...")
            series1 = avg1.compute_equal_weight_average()
            series2 = avg2.compute_equal_weight_average()
    else:
        print(f"Error: Unknown averaging strategy '{strategy}'", file=sys.stderr)
        print("Available strategies: equal-weight", file=sys.stderr)
        return

    if not series1 or not series2:
        print("Error: No data in one or both networks", file=sys.stderr)
        return

    if by_year:
        # Annual data: keys are years (int)
        common_years = sorted(set(series1.keys()) & set(series2.keys()))
        if not common_years:
            print("Error: No overlapping years between networks", file=sys.stderr)
            return

        years1 = sorted(series1.keys())
        years2 = sorted(series2.keys())
        values1 = [series1[y] for y in years1]
        values2 = [series2[y] for y in years2]
        diff_values = [series1[y] - series2[y] for y in common_years]

        x1 = [float(y) for y in years1]
        x2 = [float(y) for y in years2]
        x_common = [float(y) for y in common_years]
        time_type = "Annual Average"
    else:
        # Monthly data: keys are (year, month) tuples
        common_dates = sorted(set(series1.keys()) & set(series2.keys()))
        if not common_dates:
            print("Error: No overlapping dates between networks", file=sys.stderr)
            return

        dates1 = sorted(series1.keys())
        dates2 = sorted(series2.keys())
        values1 = [series1[d] for d in dates1]
        values2 = [series2[d] for d in dates2]
        diff_values = [series1[d] - series2[d] for d in common_dates]

        def date_to_decimal(date_tuple: Tuple[int, int]) -> float:
            year, month = date_tuple
            return year + (month - 0.5) / 12.0

        x1 = [date_to_decimal(d) for d in dates1]
        x2 = [date_to_decimal(d) for d in dates2]
        x_common = [date_to_decimal(d) for d in common_dates]
        time_type = "Monthly"

    # Create figure with two subplots
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 8), sharex=True)

    # Top plot: Both network averages
    ax1.plot(x1, values1, "b-", label=label1, alpha=0.7, linewidth=0.8)
    ax1.plot(x2, values2, "r-", label=label2, alpha=0.7, linewidth=0.8)
    ax1.set_ylabel("Temperature (°C)")
    filter_str = f" (filter: {glob_filter})" if glob_filter else ""
    ax1.set_title(f"{time_type} Network Average Comparison{filter_str}\n{count1} stations vs {count2} stations")
    ax1.legend(loc="best")
    ax1.grid(True, alpha=0.3)

    # Bottom plot: Difference
    ax2.plot(x_common, diff_values, "g-", alpha=0.7, linewidth=0.8)
    ax2.axhline(y=0, color="k", linestyle="--", alpha=0.5, linewidth=0.8)
    ax2.set_xlabel("Year")
    ax2.set_ylabel(f"Difference (°C)\n({label1} - {label2})")
    ax2.set_title("Difference Between Network Averages")
    ax2.grid(True, alpha=0.3)

    # Calculate and display statistics
    mean_diff = float(np.mean(diff_values))
    std_diff = float(np.std(diff_values))
    max_diff = float(np.max(np.abs(diff_values)))

    stats_text = (
        f"Mean diff: {mean_diff:.3f}°C\n"
        f"Std: {std_diff:.3f}°C\n"
        f"Max |diff|: {max_diff:.3f}°C\n"
        f"Strategy: {strategy}"
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
        description="Plot and compare network averages from two directories",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Compare raw vs adjusted network averages
  %(prog)s data/raw/ output/adjusted/tavg/ \\
    --label1 "Raw Network" --label2 "Adjusted Network"

  # Compare with station filter (only PW stations)
  %(prog)s pw-data-v3/monthly/raw/ pw-data-v4/output/adjusted/tavg/ \\
    --filter "PW*" --label1 "v52i" --label2 "v4"

  # Compare two different PHA runs
  %(prog)s output-run1/adjusted/tmax/ output-run2/adjusted/tmax/ \\
    --label1 "confirm=2" --label2 "confirm=3" -o comparison.png

  # Filter by station ID pattern
  %(prog)s dir1/ dir2/ --filter "US*" --label1 "US Stations Only"
        """,
    )

    parser.add_argument("dir1", type=Path, help="First directory containing station data files")
    parser.add_argument("dir2", type=Path, help="Second directory containing station data files")
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="Output image file (PNG, PDF, etc.). If not specified, display interactively.",
    )
    parser.add_argument(
        "--label1",
        default="Directory 1",
        help="Label for first network (default: Directory 1)"
    )
    parser.add_argument(
        "--label2",
        default="Directory 2",
        help="Label for second network (default: Directory 2)"
    )
    parser.add_argument(
        "--filter",
        dest="glob_filter",
        help="Glob pattern to filter station IDs (e.g., 'PW*', 'US*')",
    )
    parser.add_argument(
        "--strategy",
        choices=["equal-weight"],
        default="equal-weight",
        help="Averaging strategy (default: equal-weight)",
    )
    parser.add_argument(
        "--by-year",
        action="store_true",
        help="Display annual averages instead of monthly data (requires at least 6 months per year)",
    )
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose output")

    args = parser.parse_args()

    if not args.dir1.exists():
        print(f"Error: Directory not found: {args.dir1}", file=sys.stderr)
        return 1
    if not args.dir2.exists():
        print(f"Error: Directory not found: {args.dir2}", file=sys.stderr)
        return 1

    try:
        plot_network_comparison(
            args.dir1,
            args.dir2,
            output=args.output,
            label1=args.label1,
            label2=args.label2,
            glob_filter=args.glob_filter,
            strategy=args.strategy,
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
