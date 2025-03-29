!> @brief The module for running unit tests against the CalculateDiffs
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
module CalculateDiffsTest

  use FindChangepoints ! CalculateDiffs no longer a separate module
  use PropertyReader
  use UnitTest
  use AlgorithmParameters

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine calculate_diffs_tests()

    call test_calculate_difference_value()

  end subroutine calculate_diffs_tests

  !> Tests the calculate_difference function, which calculates the
  !! difference between two real numbers.
  subroutine test_calculate_difference_value()
    real :: difference

    difference = calculate_difference_value(44.020000, -7.97000)
    call assert_equals_within_tolerance(51.99, difference, .0001, 'diff 1 neg 1 pos')

    difference = calculate_difference_value(40.799999, 359.17000)
    call assert_equals_within_tolerance(-318.370001, difference, .0001, 'diff 2 positives')

    difference = calculate_difference_value(-40.799999, -78.17000)
    call assert_equals_within_tolerance(37.370001, difference, .0001, 'diff 2 negatives')

    difference = calculate_difference_value(-40.799999, MISSING_REAL)
    call assert_true(is_missing_real(difference), 'diff value2 MISSING')

    difference = calculate_difference_value(MISSING_REAL, -178.17000)
    call assert_true(is_missing_real(difference), 'diff value1 MISSING')

    difference = calculate_difference_value(MISSING_REAL, MISSING_REAL)
    call assert_true(is_missing_real(difference), 'diff of two MISSING')

  end subroutine test_calculate_difference_value

end module CalculateDiffsTest
