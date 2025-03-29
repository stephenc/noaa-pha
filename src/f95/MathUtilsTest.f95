!> @brief Unit tests for MathUtils module.
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
module MathUtilsTest

  use UnitTest
  use MathUtils
  use AlgorithmParameters

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine math_utils_tests()
 
    call test_calculate_average()
    call test_is_missing_real()
    call test_is_missing_int()
    call test_calculate_correlation

  end subroutine math_utils_tests

  !> Tests the calculate_average functions.
  subroutine test_calculate_average()
    real, dimension(:) :: values(5)

    values = (/ 2.0, 1.5, 3.0, 2.3, 0.0 /)
    call assert_equals_within_tolerance(calculate_average(values), 1.76, .0001, "Average of full array of values")
    call assert_equals_within_tolerance(calculate_average(values, 4), 2.2, .0001, "Average of 4 values")
    call assert_equals_within_tolerance(calculate_average(values, 1), 2.0, .0001, "Average of 1 value")

  end subroutine test_calculate_average

  !> Tests the is_missing_real function.
  subroutine test_is_missing_real()

    call assert_true(is_missing_real(MISSING_REAL), "Test MISSING_REAL for is_missing_real")
    call assert_true(is_missing_real(-99.99), "Test -99.99 for is_missing_real")
    call assert_true(is_missing_real(-99.90), "Test -99.90 for is_missing_real")
    call assert_true(is_missing_real(-120.0), "Test -120.0 for is_missing_real")
    call assert_false(is_missing_real(99.99), "Test positive 99.99 for is_missing_real")

  end subroutine test_is_missing_real

  !> Tests the is_missing_int function.
  subroutine test_is_missing_int()

    ! Only an exact match to MISSING_INT (-9999) will evaluate to true, to match original code
    call assert_true(is_missing_int(MISSING_INT), "Test MISSING_INT for is_missing_int")
    call assert_true(is_missing_int(-9999), "Test -9999 for is_missing_int")
    call assert_false(is_missing_int(-9990), "Test -9990 for is_missing_int")
    call assert_false(is_missing_int(9999), "Test positive 9999 for is_missing_int")
    call assert_false(is_missing_int(-9989), "Test -9989 for is_missing_int")
    call assert_false(is_missing_int(-18980), "Test -18980 for is_missing_int")

  end subroutine test_is_missing_int

  subroutine test_calculate_correlation()

    integer :: good_values_count
    real, dimension(:) :: target_values(10)
    real, dimension(:) :: neighbor_values(10)
    real :: coefficient

    target_values = (/ 6.0, 4.2, 6.0, 1.0, 0.2, 0.1, 8.2, 8.3, 8.4, 5.7 /)
    neighbor_values = (/ 6.0, 4.2, 6.0, 1.0, 0.2, 0.1, 8.2, 8.3, 8.4, 5.7 /)

    coefficient = calculate_correlation(4,target_values,neighbor_values)
    call assert_equals_within_tolerance(coefficient, -99.99, .0001, "correlation is missing due to lack of data")

  end subroutine test_calculate_correlation

end module MathUtilsTest
