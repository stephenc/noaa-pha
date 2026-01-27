#!/usr/bin/env python3
"""
Convert GHCNM v3/v52i format data to GHCNM v4 format.

This tool converts Peter's World and other v52i test data to the format
expected by the reconstructed GHCNM v4 codebase.

Format differences:
- v3 station list: ID, Lat, Lon, TZ, Elev, Name
- v4 station list (inv): ID, Lat, Lon, Elev, Name (no TZ)
- v3 data: ID Year 12×Value (no flags)
- v4 data: ID Year 12×(Value + 3 flags)
"""

import argparse
import sys
from pathlib import Path
from typing import TextIO


class V3ToV4Converter:
    """Convert v3/v52i format to v4 format."""

    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.stations_converted = 0
        self.records_converted = 0

    def log(self, message: str) -> None:
        """Print message if verbose mode enabled."""
        if self.verbose:
            print(message, file=sys.stderr)

    def convert_station_list(
        self, input_file: Path, output_file: Path
    ) -> None:
        """
        Convert v3 station list to v4 inventory format.

        v3 format (space-separated):
        PW100010148  34.23    -86.17   +6 01  348    ALBERTVILLE 2 SE

        v4 format (fixed-width):
        Cols 1-11:   Station ID (A11)
        Cols 13-20:  Latitude (F8.4)
        Cols 22-30:  Longitude (F9.4)
        Cols 32-37:  Elevation (F6.1)
        Cols 39-68:  Station Name (A30)
        """
        self.log(f"Converting station list: {input_file} -> {output_file}")

        with open(input_file, "r") as infile, open(
            output_file, "w"
        ) as outfile:
            for line_num, line in enumerate(infile, 1):
                line = line.rstrip("\n")
                if not line or line.startswith("#"):
                    continue

                try:
                    # Parse v3 format (space-separated)
                    # Format: ID Lat Lon TZ1 TZ2 Elev Name
                    parts = line.split(None, 6)  # Split on whitespace, max 7 parts
                    if len(parts) < 6:
                        self.log(
                            f"Warning: Skipping malformed line {line_num}: {line}"
                        )
                        continue

                    station_id = parts[0]
                    lat = float(parts[1])
                    lon = float(parts[2])
                    # parts[3] is timezone part 1 (+6) - skip for v4
                    # parts[4] is timezone part 2 (01) - skip for v4
                    elev = float(parts[5])
                    name = parts[6].strip() if len(parts) > 6 else ""

                    # Format as v4 fixed-width
                    # Station ID: 11 chars, left-justified
                    # Latitude: 8.4f starting at col 13
                    # Longitude: 9.4f starting at col 22
                    # Elevation: 6.1f starting at col 32
                    # Name: variable length (max 30 chars) starting at col 39
                    v4_line = (
                        f"{station_id:<11s} "
                        f"{lat:8.4f} "
                        f"{lon:9.4f} "
                        f"{elev:6.1f} "
                        f"{name[:30]}"
                    )

                    outfile.write(v4_line + "\n")
                    self.stations_converted += 1

                except (ValueError, IndexError) as e:
                    self.log(
                        f"Warning: Error parsing line {line_num}: {e}\n  Line: {line}"
                    )
                    continue

        self.log(f"Converted {self.stations_converted} stations")

    def convert_data_file(
        self, input_file: Path, output_file: Path, add_flags: str = "   "
    ) -> None:
        """
        Convert v3 data file to v4 format.

        v3 format (space-separated):
        PW100027370 1900 -9999 -9999 -9999 1703 1971 2351 2848 2738 2365 1747 1164 747

        v4 format (space-separated with flags):
        PW100027370 1900 -9999    -9999    -9999     1703     1971     2351     2848     2738     2365     1747     1164      747

        Each value becomes: value (6 chars) + 3 flag chars
        Default flags: '   ' (3 spaces = no flags set)
        """
        self.log(f"Converting data file: {input_file} -> {output_file}")

        records_in_file = 0
        with open(input_file, "r") as infile, open(
            output_file, "w"
        ) as outfile:
            for line_num, line in enumerate(infile, 1):
                line = line.rstrip("\n")
                if not line or line.startswith("#"):
                    continue

                try:
                    parts = line.split()
                    if len(parts) < 14:  # ID + Year + 12 months
                        self.log(
                            f"Warning: Incomplete record at line {line_num}: {line}"
                        )
                        continue

                    station_id = parts[0]
                    year = parts[1]
                    values = parts[2:14]  # 12 monthly values

                    # Build v4 format line
                    v4_line = f"{station_id} {year}"
                    for value in values:
                        # Format: 6-character value right-justified + 3 flags
                        v4_line += f"{int(value):6d}{add_flags}"

                    outfile.write(v4_line + "\n")
                    records_in_file += 1

                except (ValueError, IndexError) as e:
                    self.log(
                        f"Warning: Error parsing line {line_num}: {e}\n  Line: {line}"
                    )
                    continue

        self.records_converted += records_in_file
        self.log(f"Converted {records_in_file} records from {input_file.name}")

    def convert_directory(
        self,
        input_dir: Path,
        output_dir: Path,
        pattern: str = "*.raw.*",
        add_flags: str = "   ",
    ) -> None:
        """
        Convert all data files in a directory.

        Args:
            input_dir: Source directory with v3 format files
            output_dir: Destination directory for v4 format files
            pattern: Glob pattern for data files
            add_flags: Flag characters to add (default: 3 spaces)
        """
        self.log(f"Converting directory: {input_dir} -> {output_dir}")

        output_dir.mkdir(parents=True, exist_ok=True)

        files = sorted(input_dir.glob(pattern))
        if not files:
            self.log(f"Warning: No files matching {pattern} in {input_dir}")
            return

        for input_file in files:
            output_file = output_dir / input_file.name
            self.convert_data_file(input_file, output_file, add_flags)

        self.log(
            f"Converted {len(files)} files with {self.records_converted} total records"
        )

    def convert_all(
        self,
        input_base: Path,
        output_base: Path,
        element: str = "tavg",
        station_list: str = "world1_stnlist.tavg",
        add_flags: str = "   ",
    ) -> None:
        """
        Convert entire v3 dataset to v4 format.

        Args:
            input_base: Base directory with v3 structure (contains meta/ and monthly/)
            output_base: Output base directory for v4 structure
            element: Element type (tavg, tmax, tmin)
            station_list: Station list filename in meta/ directory
            add_flags: Flag characters to add (default: 3 spaces)
        """
        self.log(f"Converting full dataset: {input_base} -> {output_base}")

        # Create complete v4 directory structure
        self.log(f"Creating v4 directory structure...")
        output_base.mkdir(parents=True, exist_ok=True)
        (output_base / "data" / "raw").mkdir(parents=True, exist_ok=True)
        (output_base / "output").mkdir(parents=True, exist_ok=True)
        (output_base / "output" / "adjusted" / element).mkdir(parents=True, exist_ok=True)
        (output_base / "history").mkdir(parents=True, exist_ok=True)

        # Convert station list
        station_list_path = input_base / "meta" / station_list
        output_inv = output_base / "ghcnm.inv"

        if not station_list_path.exists():
            self.log(f"Warning: Station list not found: {station_list_path}")
            self.log(f"Skipping station list conversion")
        else:
            self.log(f"Converting station list...")
            self.convert_station_list(station_list_path, output_inv)

        # Convert data files
        data_dir = input_base / "monthly" / "raw"
        output_data_dir = output_base / "data" / "raw"
        pattern = f"*.raw.{element}"

        if not data_dir.exists():
            self.log(f"Warning: Data directory not found: {data_dir}")
            self.log(f"Skipping data conversion")
        else:
            self.log(f"Converting data files...")
            self.convert_directory(data_dir, output_data_dir, pattern, add_flags)

        self.log(
            f"Full conversion complete: {self.stations_converted} stations, "
            f"{self.records_converted} records"
        )
        self.log(f"Created directory structure:")
        self.log(f"  {output_base}/ghcnm.inv")
        self.log(f"  {output_base}/data/raw/")
        self.log(f"  {output_base}/output/")
        self.log(f"  {output_base}/output/adjusted/{element}/")
        self.log(f"  {output_base}/history/")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Convert GHCNM v3/v52i format to v4 format",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Convert entire dataset (default)
  %(prog)s pw-data-v3 pw-data-v4
  %(prog)s pw-data-v3/benchmark/world1 pw-data-v4 --element tavg -v
        """,
    )

    # Positional arguments for full conversion (default mode)
    parser.add_argument(
        "input_base", type=Path,
        help="Input base directory (contains meta/ and monthly/ subdirs)"
    )
    parser.add_argument(
        "output_base", type=Path,
        help="Output base directory for v4 structure"
    )

    # Optional arguments
    parser.add_argument(
        "--element",
        default="tavg",
        help="Element type (tavg, tmax, tmin) (default: tavg)",
    )
    parser.add_argument(
        "--station-list",
        default="world1_stnlist.tavg",
        help="Station list filename in meta/ directory (default: world1_stnlist.tavg)",
    )
    parser.add_argument(
        "--flags",
        default="   ",
        help="Flag characters to add to each value (default: 3 spaces)",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Verbose output"
    )

    args = parser.parse_args()

    converter = V3ToV4Converter(verbose=args.verbose)

    try:
        if not args.input_base.is_dir():
            print(
                f"Error: Input directory not found: {args.input_base}",
                file=sys.stderr,
            )
            return 1

        converter.convert_all(
            args.input_base,
            args.output_base,
            args.element,
            args.station_list,
            args.flags,
        )

        print(
            f"Conversion complete: {converter.stations_converted} stations, "
            f"{converter.records_converted} records"
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
