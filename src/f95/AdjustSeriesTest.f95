!> @brief The module for running unit tests against the AdjustSeries
!! module subroutines.
!!
!! @copyright COPYRIGHT
!! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC
!! DOMAIN AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE. THEY ARE
!! FURNISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
!! INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
!! EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE AND
!! DOCUMENTATION FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR
!! THE USE OF THE SOFTWARE AND DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL
!! SUPPORT TO USERS.
!!
module AdjustSeriesTest

  use AdjustSeries
  use PropertyReader
  use UnitTest
  use FileUtils
  use DatetimeUtils
  use PropertyParameters

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine adjust_series_tests()
    use CommonVariables, only: end_year
    end_year = get_current_year()

    call test_get_output_filepath()
    call test_write_station_output()
    call test_get_element_id()

  end subroutine adjust_series_tests

  subroutine test_write_station_output()
    use CommonVariables, only: begin_year, end_year
    character(len=256), dimension(:) :: expected_output(6)
    character(len=256), dimension(:) :: actual_output(6)
    character(len=256) :: file_path
    character(len=11) :: station_id
    integer, dimension(:,:) :: output_values(begin_year:end_year,12)
    character(3), dimension(:,:) :: output_flags(begin_year:end_year,12)

    ! Initialize entire array to print nothing unless specified below.
    output_values(:,:) = 0
    output_flags(:,:) = "---"

    file_path = "./final_output.temp"
    station_id = "USH00021614"
    ! Output values and flags for first line of file
    output_values(1917,1) = 1368
    output_values(1917,2) = 1754
    output_values(1917,3) = 2125
    output_values(1917,4) = 2512
    output_values(1917,5) = 2762
    output_values(1917,6) = 3818
    output_values(1917,7) = 3844
    output_values(1917,8) = 3757
    output_values(1917,9) = 3411
    output_values(1917,10) = -9999
    output_values(1917,11) = 2532
    output_values(1917,12) = 2283
    output_flags(1917,1) = 'a  '
    output_flags(1917,2:12) = '   '

    ! Output values and flags that should be ignored due to --- flag indicators
    output_values(1918,1:12) = 1000
    output_flags(1918,1:12) = '---'

    ! Output values and flags for 3rd line of file
    output_values(1919,1:8) = -9999
    output_values(1919,9) = 3528
    output_values(1919,10) = 2523
    output_values(1919,11) = -9999
    output_values(1919,12) = 1710
    output_flags(1919,1:4) = ' X '
    output_flags(1919,5) = '   '
    output_flags(1919,6) = ' X '
    output_flags(1919,7) = ' Q '
    output_flags(1919,8) = ' X '
    output_flags(1919,9:10) = 'f  '
    output_flags(1919,11) = '   '
    output_flags(1919,12) = '   '

    call write_station_output(file_path, station_id, output_values, output_flags)

    expected_output(1) = "USH0002161411917  1368a    1754     2125     2512     2762     3818     3844     "  &
                       //"3757     3411    -9999     2532     2283"
    expected_output(2) = "USH0002161411919 -9999 X  -9999 X  -9999 X  -9999 X  -9999    -9999 X  -9999 Q  "  &
                       //"-9999 X   3528f    2523f   -9999     1710"

    actual_output = get_file_lines(file_path)

    ! Compare output lines to expected, and ensure that only 2 lines were written.
    call assert_equals(expected_output(1), actual_output(1), "valid output file line 1")
    call assert_equals(expected_output(2), actual_output(2), "valid output file line 1")
    call assert_equals(2, count_file_lines(file_path), "Only 2 of the 3 lines should have been printed to the file.")

    ! Clean up
    call delete_file(file_path)

  end subroutine test_write_station_output

  !> Tests the get_output_filepath function, which constructs the output filepath
  !! for a given station_id.
  subroutine test_get_output_filepath()
    character(len=256) :: filepath_actual
    character(len=256) :: filepath_expected
    character(len=256) :: data_dir
    character(len=11) :: station_id
    character(len=3) :: version

    data_dir = get_property_chars(PROP_PATH_ELEMENT_DATA_OUT)
    version = get_property_chars(PROP_VERSION)

    station_id = "USH00018323"

    filepath_expected = trim(data_dir)//"USH00018323."//"WMs."//version//".tmax"
    filepath_actual = get_output_filepath(station_id)
    call assert_equals(filepath_expected, filepath_actual, "USH station output path")

    station_id = "10160475000"

    filepath_expected = trim(data_dir)//"10160475000."//"WMs."//version//".tmax"
    filepath_actual = get_output_filepath(station_id)
    call assert_equals(filepath_expected, filepath_actual, "non-USH station output path")

  end subroutine test_get_output_filepath

  subroutine test_get_element_id()

    call assert_equals(get_element_id(), 1, "Element id for tmax")

  end subroutine test_get_element_id

end module AdjustSeriesTest
