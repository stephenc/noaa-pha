!> @brief Program to run TOB unit tests only.
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
program TOBTestUnits

  use Logger
  use PropertyParameters
  use PropertyReader
  use ConfigurationUtils
  use UnitTest
  use TOBUtilsTest

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

  call tob_utils_tests()

  call log_info("It is expected to see ERROR and WARN messages when running unit tests, due to the testing of "  &
                   // "edge cases and error handling. As long as there are ZERO 'Failed' unit tests, all is OK.")
  call report_test_results(total, successes, failures)

  call cpu_time(timer)
  write(*,1) timer
  1 format ('Unit tests completed in',f6.2,' seconds.')

  if(failures>0) then
    call exit(9)
  endif

end program TOBTestUnits
