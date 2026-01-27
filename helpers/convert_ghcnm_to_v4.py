#!/usr/bin/env python3
"""
Convert NOAA GHCN-Monthly v4 distribution format to PHAMain input format.

NOAA distributes GHCNM v4 data as:
  - .inv file: Station inventory (already in correct format)
  - .dat file: Single monolithic file with all stations

This script:
  - Splits the .dat file into individual per-station files
  - Converts from NOAA format to PHAMain input format
  - Creates required directory structure for PHAMain

NOAA .dat format (columns):
  1-11:    Station ID
  12-15:   Year
  16-19:   Element (TAVG, TMAX, TMIN, TDTR)
  20-24:   VALUE1 (5-char integer)
  25-27:   FLAGS1 (DMFLAG, QCFLAG, DSFLAG)
  ...
  108-112: VALUE12 (5-char integer)
  113-115: FLAGS12 (DMFLAG, QCFLAG, DSFLAG)

PHAMain input format:
  Columns 1-11:  Station ID
  Column 12:     Space
  Columns 13-16: Year
  Then 12 groups of: VALUE(6-char) FLAGS(3-char)

Example:
  NOAA:     ACW000112101931TAVG -9999     -9999     -9999     -9999    ...
  PHAMain:  ACW00011210 1931 -9999    -9999    -9999    -9999    ...
"""

import argparse
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Optional, TextIO
from collections import defaultdict


class GHCNMConverter:
    """Convert NOAA GHCNM v4 distribution format to PHAMain input format."""

    def __init__(self, input_dir: Path, output_dir: Path, verbose: bool = False):
        self.input_dir = input_dir
        self.output_dir = output_dir
        self.verbose = verbose

        # Statistics
        self.stations_by_element: Dict[str, set] = defaultdict(set)
        self.records_by_element: Dict[str, int] = defaultdict(int)
        self.errors: List[str] = []

        # Track which files we've created (so we can use append mode for subsequent writes)
        self.files_created: set = set()  # Set of (station_id, element) tuples

    def log(self, message: str) -> None:
        if self.verbose:
            print(message, file=sys.stderr)

    def warn(self, message: str) -> None:
        print(f"Warning: {message}", file=sys.stderr)

    def error(self, message: str) -> None:
        print(f"Error: {message}", file=sys.stderr)
        self.errors.append(message)

    def find_input_files(self) -> Tuple[Optional[Path], Optional[Path]]:
        """Find .inv and .dat files in input directory."""
        inv_file = None
        dat_file = None

        for file_path in self.input_dir.iterdir():
            if not file_path.is_file():
                continue

            if file_path.suffix == ".inv":
                if inv_file is None:
                    inv_file = file_path
                else:
                    self.warn(f"Multiple .inv files found, using {inv_file.name}")

            elif file_path.suffix == ".dat":
                if dat_file is None:
                    dat_file = file_path
                else:
                    self.warn(f"Multiple .dat files found, using {dat_file.name}")

        return inv_file, dat_file

    def create_directory_structure(self, elements: List[str]) -> None:
        """Create required directory structure for PHAMain."""
        directories = [
            self.output_dir / "data" / "raw",
            self.output_dir / "history",
            self.output_dir / "output" / "neighbors",
        ]

        # Create output directories for each element
        for element in elements:
            directories.append(
                self.output_dir / "output" / "adjusted" / element.lower()
            )

        for directory in directories:
            directory.mkdir(parents=True, exist_ok=True)
            self.log(f"Created directory: {directory}")

    def parse_dat_line(self, line: str) -> Optional[Tuple[str, int, str, List[Tuple[int, str]]]]:
        """
        Parse a line from NOAA .dat file.

        Returns: (station_id, year, element, [(value1, flags1), ..., (value12, flags12)])
        """
        if len(line) < 115:
            return None

        try:
            station_id = line[0:11].strip()
            year = int(line[11:15])
            element = line[15:19].strip()

            # Parse 12 monthly values and flags
            monthly_data = []
            for month in range(12):
                # Calculate column positions for this month
                value_start = 19 + month * 8
                value_end = value_start + 5
                flags_start = value_end
                flags_end = flags_start + 3

                value_str = line[value_start:value_end].strip()
                flags = line[flags_start:flags_end]

                # Parse value (may be missing)
                if value_str:
                    value = int(value_str)
                else:
                    value = -9999

                monthly_data.append((value, flags))

            return station_id, year, element, monthly_data

        except (ValueError, IndexError) as e:
            return None

    def write_to_station_file(
        self, station_id: str, element: str, station_id_fmt: str, year: int, monthly_data: List[Tuple[int, str]]
    ) -> None:
        """Write a line to the station file (opens, writes, closes)."""
        key = (station_id, element)
        element_lower = element.lower()
        output_path = (
            self.output_dir / "data" / "raw" / f"{station_id}.raw.{element_lower}"
        )

        # Use "w" for first write, "a" for subsequent writes
        is_new_file = key not in self.files_created
        mode = "w" if is_new_file else "a"

        if is_new_file:
            self.files_created.add(key)

        # Open, write, close immediately
        with open(output_path, mode) as f:
            # Format: "STATION_ID YEAR VALUE1   FFF VALUE2   FFF ... VALUE12  FFF"
            # Station ID is 11 chars, space, year is 4 digits
            parts = [f"{station_id_fmt:11s}", f" {year:4d}"]

            for value, flags in monthly_data:
                # Value is 6 characters (right-justified), flags are 3 characters
                parts.append(f"{value:6d}{flags}")

            line = "".join(parts) + "\n"
            f.write(line)

    def convert(self) -> bool:
        """Run the conversion process."""
        # Find input files
        self.log(f"Searching for input files in {self.input_dir}")
        inv_file, dat_file = self.find_input_files()

        if inv_file is None:
            self.error(f"No .inv file found in {self.input_dir}")
            return False

        if dat_file is None:
            self.error(f"No .dat file found in {self.input_dir}")
            return False

        self.log(f"Found inventory: {inv_file.name}")
        self.log(f"Found data file: {dat_file.name}")

        # Copy inventory file to output
        output_inv = self.output_dir / "ghcnm.inv"
        self.log(f"Copying {inv_file} to {output_inv}")
        output_inv.parent.mkdir(parents=True, exist_ok=True)

        with open(inv_file, "r") as src, open(output_inv, "w") as dst:
            dst.write(src.read())

        # Create initial directory structure (we'll add element-specific dirs as we find them)
        self.create_directory_structure([])

        # Process .dat file line by line
        self.log(f"Processing {dat_file.name}...")
        line_count = 0
        error_count = 0

        try:
            with open(dat_file, "r") as f:
                for line_num, line in enumerate(f, 1):
                    line_count += 1

                    if line_count % 100000 == 0:
                        print(
                            f"Processed {line_count:,} lines, "
                            f"{sum(len(s) for s in self.stations_by_element.values())} stations, "
                            f"{sum(self.records_by_element.values()):,} records",
                            file=sys.stderr,
                        )

                    parsed = self.parse_dat_line(line)
                    if parsed is None:
                        error_count += 1
                        if error_count <= 10:  # Only show first 10 errors
                            self.warn(f"Could not parse line {line_num}: {line[:50]}...")
                        continue

                    station_id, year, element, monthly_data = parsed

                    # Track statistics
                    self.stations_by_element[element].add(station_id)
                    self.records_by_element[element] += 1

                    # Create element-specific output directory if this is the first time we see this element
                    element_lower = element.lower()
                    element_dir = self.output_dir / "output" / "adjusted" / element_lower
                    if not element_dir.exists():
                        element_dir.mkdir(parents=True, exist_ok=True)
                        self.log(f"Created output directory for element: {element}")

                    # Write line to station file (opens, writes, closes)
                    self.write_to_station_file(station_id, element, station_id, year, monthly_data)

        except Exception as e:
            self.error(f"Error processing {dat_file}: {e}")
            return False

        # Print summary
        print("\nConversion complete!")
        print(f"Processed {line_count:,} lines from {dat_file.name}")

        if error_count > 0:
            print(f"Warning: {error_count} lines could not be parsed")

        print("\nStatistics by element:")
        for element in sorted(self.stations_by_element.keys()):
            station_count = len(self.stations_by_element[element])
            record_count = self.records_by_element[element]
            print(
                f"  {element}: {station_count:,} stations, {record_count:,} records"
            )

        print(f"\nOutput directory: {self.output_dir}")
        print(f"  Inventory: {output_inv}")
        print(f"  Data files: {self.output_dir / 'data' / 'raw'}/")
        print(f"  Output directories: {self.output_dir / 'output' / 'adjusted'}/{{element}}/")

        return True


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Convert NOAA GHCNM v4 distribution format to PHAMain input format",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Convert GHCNM v4 data to PHAMain format
  %(prog)s ./ghcnm.v4.0.1.20260126 ./ghcnm-data -v

  # This will create:
  #   ./ghcnm-data/ghcnm.inv              (station inventory)
  #   ./ghcnm-data/data/raw/*.raw.{element}  (individual station files)
  #   ./ghcnm-data/output/adjusted/{element}/ (for PHAMain output)
  #   ./ghcnm-data/history/               (for station history files)

After conversion, configure a properties file:
  pha.path.station-metadata = ghcnm-data/ghcnm.inv
  pha.path.station-element-data-in = ghcnm-data/data/raw/
  pha.path.station-element-data-out = ghcnm-data/output/adjusted/{element}/
  pha.path.station-history = ghcnm-data/history/
        """,
    )

    parser.add_argument(
        "input_dir",
        type=Path,
        help="Input directory containing .inv and .dat files from NOAA",
    )
    parser.add_argument(
        "output_dir",
        type=Path,
        help="Output directory for PHAMain input format",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Verbose output"
    )

    args = parser.parse_args()

    # Validate input directory
    if not args.input_dir.exists():
        print(f"Error: Input directory not found: {args.input_dir}", file=sys.stderr)
        return 1

    if not args.input_dir.is_dir():
        print(f"Error: Not a directory: {args.input_dir}", file=sys.stderr)
        return 1

    # Create output directory if needed
    args.output_dir.mkdir(parents=True, exist_ok=True)

    # Run conversion
    converter = GHCNMConverter(args.input_dir, args.output_dir, verbose=args.verbose)

    try:
        if converter.convert():
            return 0
        else:
            return 1
    except KeyboardInterrupt:
        print("\nInterrupted by user", file=sys.stderr)
        return 130
    except Exception as e:
        print(f"Fatal error: {e}", file=sys.stderr)
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
