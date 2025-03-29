!> @brief Program to run all unit tests for this project.
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
program PHATestUnits

  use Logger
  use PropertyReader
  use UnitTest
  use DatetimeUtilsTest
  use FileUtilsTest
  use MathUtilsTest
  use ModelFitUtilsTest
  use SkylineUtilsTest
  use ReadInputFilesTest
  use CalculateDiffsTest
  use FindChangepointsTest
  use ChooseNeighborsTest
  use ChangepointSizeTest
  use AdjustSeriesTest
  use ChangepointSumsTypeTest
  use ChangepointSpanTypeTest
  use ChangepointHitTypeTest
  use ChangepointTypeTest
  use StationTypeTest
  use StationNeighborPairTypeTest
  use ElementValueTypeTest
  use ConfigurationUtilsTest

  implicit none

  real :: timer
  integer :: total
  integer :: successes
  integer :: failures

  ! Properties file set up for unit testing
  call properties_init("./build/ghcnm-pha.unit-test.properties")
  call detokenize_all_properties()
  call log_init(get_property_chars(PROP_LOG_FILENAME),     &
                get_property_chars(PROP_LOG_LEVEL),        &
                get_property_logical(PROP_LOG_STDOUT),     &
                get_property_logical(PROP_LOG_DATESTAMP),  &
                get_property_logical(PROP_LOG_ROLLOVER))
  call set_common_variables()

  ! Call each test module's top-level test subroutine, each
  ! of which calls all the test subroutines within that module.
  call file_utils_tests()
  call datetime_utils_tests()
  call math_utils_tests()
  call model_fit_utils_tests()
  call skyline_utils_tests()
  call changepoint_tests()
  call changepoint_sums_tests()
  call changepoint_span_tests()
  call changepoint_hit_tests()
  call station_tests()
  call station_neighbor_pair_tests()
  call element_value_tests()
  call choose_neighbors_tests()
  call calculate_diffs_tests()
  call find_changepoints_tests()
  call changepoint_size_tests()
  call adjust_series_tests()
  call read_input_files_tests()
  call configuration_utils_tests()

  ! Print out results of all tests.
  call log_info("It is expected to see ERROR and WARN messages when running unit tests, due to the testing of "  &
                   // "edge cases and error handling. As long as there are ZERO 'Failed' unit tests, all is OK.")
  call report_test_results(total, successes, failures)

  ! Measure runtime of unit tests
  call cpu_time(timer)
  write(*,1) timer
  1 format ('Unit tests completed in',f6.2,' seconds.')

  ! If any tests failed, exit with error.
  if(failures>0) then
    call exit(9)
  endif

contains

  !> Sets the global variables in CommonVariables that are used across
  !! multiple modules throughout the program.
  !! @todo: Auto-detect begin_year when reading in files?
  subroutine set_common_variables()
    use CommonVariables, only: begin_year, end_year, por_months

    integer :: por_years

    begin_year = get_property_int(PROP_BEGIN_YEAR)
    end_year = get_current_year()
    por_years = end_year - begin_year + 1
    por_months = por_years * 12

    call log_info('Total POR years: '//trim(log_string(por_years)))

  end subroutine set_common_variables

end program PHATestUnits
