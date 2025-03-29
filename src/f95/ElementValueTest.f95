!> @brief Unit tests for ElementValue type.
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
module ElementValueTypeTest

  use ElementValueType
  use UnitTest
  use Logger

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine element_value_tests

    call test_element_value_string()
    call test_element_value_eq()

  end subroutine element_value_tests

  !> Tests the element_value_string function.
  subroutine test_element_value_string()
  
    type(ElementValue) :: this_val
    character(len=145) :: expected_string

    ! Null element_value record
    this_val = missing_ElementValue()
    expected_string = "[ElementValue] value:  -99.99 flag:"
    call assert_equals(expected_string, element_value_string(this_val), "MISSING ElementValue")

    ! Typical test case
    this_val = new_ElementValue(23.00, "xyz")
    expected_string = "[ElementValue] value:   23.00 flag: xyz"
    call assert_equals(expected_string, element_value_string(this_val), "Standard ElementValue string")

  end subroutine test_element_value_string

  !> Tests the eq function for single items and for arrays
  subroutine test_element_value_eq()
    type(ElementValue) :: null_val1
    type(ElementValue) :: null_val2
    type(ElementValue) :: val1a
    type(ElementValue) :: val1b
    type(ElementValue) :: val2a
    type(ElementValue) :: val2b
    type(ElementValue), dimension(:) :: val_array1a(3)
    type(ElementValue), dimension(:) :: val_array1b(3)
    type(ElementValue), dimension(:) :: val_array2(3)
    type(ElementValue), dimension(:) :: val_array3(4)

    ! Define ElementValue to test against
    null_val1 = missing_ElementValue()
    null_val2 = missing_ElementValue()
    val1a = new_ElementValue(20.13, "111")
    val1b = new_ElementValue(20.13, "111")
    val2a = new_ElementValue(11.11, "112")
    val2b = new_ElementValue(11.11, "112")

    ! Run tests on individual items
    call assert_true(null_val1 .eq. null_val2, "eq for null ElementValues")
    call assert_true(val1a .eq. val1b, "eq for identical ElementValues")
    call assert_false(val1a .eq. val2a, "eq for differing ElementValues")
    call assert_false(val1a .eq. null_val1, "eq for differing ElementValues (one null)")

    ! Set up test arrays
    val_array1a = (/ null_val1, val1a, val2a /)
    val_array1b = (/ null_val2, val1b, val2b /)
    val_array2 = (/ val1a, val2a, null_val1 /)
    val_array3 = (/ val1a, val2a, null_val1, null_val2 /)

    ! Run tests on arrays
    call assert_true(val_array1a .eq. val_array1b, "eq for arrays with identical items")
    call assert_true(val_array1b .eq. val_array1a, "eq for arrays with identical items (swapped)")
    call assert_false(val_array1a .eq. val_array2, "eq for arrays with same length but diff items")
    call assert_false(val_array2 .eq. val_array1a, "eq for arrays with same length but diff items (swapped)")
    call assert_false(val_array2 .eq. val_array3, "eq for arrays with different lengths")
    call assert_false(val_array3 .eq. val_array2, "eq for arrays with different lengths (swapped)")
  end subroutine test_element_value_eq

end module ElementValueTypeTest

