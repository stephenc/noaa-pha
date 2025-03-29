!> @brief Unit tests for Changepoint type.
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
module ChangepointTypeTest

  use ChangepointType
  use UnitTest
  use Logger

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine changepoint_tests

    call test_changepoint_string()
    call test_changepoint_eq()

  end subroutine changepoint_tests

  !> Tests the changepoint_string function.
  subroutine test_changepoint_string()
  
    type(Changepoint) :: this_changepoint
    character(len=145) :: expected_string

    ! Null changepoint record
    this_changepoint = null_Changepoint()
    expected_string = "[Changepoint] NULL"
    call assert_equals(expected_string, changepoint_string(this_changepoint), "NULL Changepoint")

    ! Typical test case
    this_changepoint = new_Changepoint(23, 58, 340.23)
    expected_string = "[Changepoint] sky month:      23 day of month:    58 amplitude:  340.23"
    call assert_equals(expected_string, changepoint_string(this_changepoint), "Standard Changepoint string")

  end subroutine test_changepoint_string

  !> Tests the eq function for single items and for arrays
  subroutine test_changepoint_eq()
    type(Changepoint) :: null_changepoint1
    type(Changepoint) :: null_changepoint2
    type(Changepoint) :: changepoint1a
    type(Changepoint) :: changepoint1b
    type(Changepoint) :: changepoint2a
    type(Changepoint) :: changepoint2b
    type(Changepoint), dimension(:) :: changepoint_array1a(3)
    type(Changepoint), dimension(:) :: changepoint_array1b(3)
    type(Changepoint), dimension(:) :: changepoint_array2(3)
    type(Changepoint), dimension(:) :: changepoint_array3(4)

    ! Define changepoints to test against
    null_changepoint1 = null_Changepoint()
    null_changepoint2 = null_Changepoint()
    changepoint1a = new_Changepoint(20, 62, 23.23)
    changepoint1b = new_Changepoint(20, 62, 23.23)
    changepoint2a = new_Changepoint(11, 51, 0.0)
    changepoint2b = new_Changepoint(11, 51, 0.0)

    ! Run tests on individual items
    call assert_true(null_changepoint1 .eq. null_changepoint2, "eq for null Changepoints")
    call assert_true(changepoint1a .eq. changepoint1b, "eq for identical Changepoints")
    call assert_false(changepoint1a .eq. changepoint2a, "eq for differing Changepoints")
    call assert_false(changepoint1a .eq. null_changepoint1, "eq for differing Changepoints (one null)")

    ! Set up test arrays
    changepoint_array1a = (/ null_changepoint1, changepoint1a, changepoint2a /)
    changepoint_array1b = (/ null_changepoint2, changepoint1b, changepoint2b /)
    changepoint_array2 = (/ changepoint1a, changepoint2a, null_changepoint1 /)
    changepoint_array3 = (/ changepoint1a, changepoint2a, null_changepoint1, null_changepoint2 /)

    ! Run tests on arrays
    call assert_true(changepoint_array1a .eq. changepoint_array1b, "eq for arrays with identical items")
    call assert_true(changepoint_array1b .eq. changepoint_array1a, "eq for arrays with identical items (swapped)")
    call assert_false(changepoint_array1a .eq. changepoint_array2, "eq for arrays with same length but diff items")
    call assert_false(changepoint_array2 .eq. changepoint_array1a, "eq for arrays with same length but diff items (swapped)")
    call assert_false(changepoint_array2 .eq. changepoint_array3, "eq for arrays with different lengths")
    call assert_false(changepoint_array3 .eq. changepoint_array2, "eq for arrays with different lengths (swapped)")
  end subroutine test_changepoint_eq

end module ChangepointTypeTest

