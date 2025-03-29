!> @brief Unit tests for ChangepointSums type.
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
module ChangepointSumsTypeTest

  use ChangepointSumsType
  use UnitTest
  use Logger

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine changepoint_sums_tests

    call test_changpoint_sums_string()
    call test_changpoint_sums_eq()

  end subroutine changepoint_sums_tests

  !> Tests the changepoint_sums_string function.
  subroutine test_changpoint_sums_string()
  
    type(ChangepointSums) :: this_sums
    character(len=145) :: expected_string

    ! Null changepoint_sums record
    this_sums = null_ChangepointSums()
    expected_string = "[ChangepointSums] offset_sum:      0.00 zscore_sum:    0.00 sum_count:     0"
    call assert_equals(expected_string, changepoint_sums_string(this_sums), "NULL ChangepointSums")

    ! Typical test case
    this_sums = new_ChangepointSums(23.81, 58.72, 80)
    expected_string = "[ChangepointSums] offset_sum:     23.81 zscore_sum:   58.72 sum_count:    80"
    call assert_equals(expected_string, changepoint_sums_string(this_sums), "Standard ChangepointSums string")

  end subroutine test_changpoint_sums_string

  !> Tests the eq function for single items and for arrays
  subroutine test_changpoint_sums_eq()
    type(ChangepointSums) :: null_sums1
    type(ChangepointSums) :: null_sums2
    type(ChangepointSums) :: sums1a
    type(ChangepointSums) :: sums1b
    type(ChangepointSums) :: sums2a
    type(ChangepointSums) :: sums2b
    type(ChangepointSums), dimension(:) :: sums_array1a(3)
    type(ChangepointSums), dimension(:) :: sums_array1b(3)
    type(ChangepointSums), dimension(:) :: sums_array2(3)
    type(ChangepointSums), dimension(:) :: sums_array3(4)

    ! Define ChangepointSums to test against
    null_sums1 = null_ChangepointSums()
    null_sums2 = null_ChangepointSums()
    sums1a = new_ChangepointSums(20.8, 62.0, 23)
    sums1b = new_ChangepointSums(20.8, 62.0, 23)
    sums2a = new_ChangepointSums(11.1, 51.5, 2)
    sums2b = new_ChangepointSums(11.1, 51.5, 2)

    ! Run tests on individual items
    call assert_true(null_sums1 .eq. null_sums2, "eq for null ChangepointSums")
    call assert_true(sums1a .eq. sums1b, "eq for identical ChangepointSums")
    call assert_false(sums1a .eq. sums2a, "eq for differing ChangepointSums")
    call assert_false(sums1a .eq. null_sums1, "eq for differing ChangepointSums (one null)")

    ! Set up test arrays
    sums_array1a = (/ null_sums1, sums1a, sums2a /)
    sums_array1b = (/ null_sums2, sums1b, sums2b /)
    sums_array2 = (/ sums1a, sums2a, null_sums1 /)
    sums_array3 = (/ sums1a, sums2a, null_sums1, null_sums2 /)

    ! Run tests on arrays
    call assert_true(sums_array1a .eq. sums_array1b, "eq for arrays with identical items")
    call assert_true(sums_array1b .eq. sums_array1a, "eq for arrays with identical items (swapped)")
    call assert_false(sums_array1a .eq. sums_array2, "eq for arrays with same length but diff items")
    call assert_false(sums_array2 .eq. sums_array1a, "eq for arrays with same length but diff items (swapped)")
    call assert_false(sums_array2 .eq. sums_array3, "eq for arrays with different lengths")
    call assert_false(sums_array3 .eq. sums_array2, "eq for arrays with different lengths (swapped)")
  end subroutine test_changpoint_sums_eq

end module ChangepointSumsTypeTest

