!> @brief Unit tests for FileUtils module.
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
module DatetimeUtilsTest

  use UnitTest
  use DatetimeUtils
  use FileUtils

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine datetime_utils_tests()
 
    call test_seconds_to_hours_minutes()
    call test_get_current_year()

  end subroutine datetime_utils_tests

  !> Tests the conversion of seconds to hours, minutes, and seconds.
  subroutine test_seconds_to_hours_minutes()
    real :: seconds
    integer :: minutes
    integer :: hours

    ! Testing an even 60 seconds.
    call seconds_to_hours_minutes(60.0, hours, minutes, seconds)
    call assert_equals(0, hours, "60.0 seconds to hours")
    call assert_equals(1, minutes, "60.0 seconds to minutes")
    call assert_equals_within_tolerance(0.0, seconds, .001, "60.0 seconds to seconds")

    ! Testing 3603 seconds.
    call seconds_to_hours_minutes(3603.0, hours, minutes, seconds)
    call assert_equals(1, hours, "3603.0 seconds to hours")
    call assert_equals(0, minutes, "3603.0 seconds to minutes")
    call assert_equals_within_tolerance(3.0, seconds, .001, "3603.0 seconds to seconds")

    ! Testing zero seconds.
    call seconds_to_hours_minutes(0.0, hours, minutes, seconds)
    call assert_equals(0, hours, "0.0 seconds to hours")
    call assert_equals(0, minutes, "0.0 seconds to minutes")
    call assert_equals_within_tolerance(0.0, seconds, .001, "0.0 seconds to seconds")

  end subroutine test_seconds_to_hours_minutes

  !> Tests the get_current_year() function.
  subroutine test_get_current_year()
    integer, parameter :: hardcoded_year = 2015
    integer :: dynamic_year
    character(len=10) :: temp_file = "datetest.temp"
    integer :: temp_unit

    ! As of the date this test was created/updated, it is 2015
    ! so we'll make sure the year is 2015 or later.
    call assert_true(get_current_year() .ge. hardcoded_year, "Year should be at least 2015")
 
    ! Get the current year using a different method than
    ! the function being tested to decrease the chance that
    ! both years are being incorrectly generated for a false pass.
    temp_unit = get_available_file_unit()
    call system('date +%Y >'//temp_file)
    open(unit=temp_unit,file=temp_file, status='unknown')
    read(temp_unit,'(i4)') dynamic_year
    close(temp_unit)
    call delete_file(temp_file)

    call assert_equals(dynamic_year, get_current_year(), "Year should be equal to dynamic_year")

  end subroutine test_get_current_year

end module DatetimeUtilsTest
