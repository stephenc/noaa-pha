!> @brief Unit tests for ChangepointSpan type.
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
module ChangepointSpanTypeTest

  use ChangepointSpanType
  use UnitTest
  use Logger
  use AlgorithmParameters

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine changepoint_span_tests

    call test_span_number_convert()
    call test_changepoint_span_string()
    call test_changepoint_span_eq()

  end subroutine changepoint_span_tests

  subroutine test_span_number_convert()
    integer :: sn
    type(ChangepointSpan) :: this

    this = new_ChangepointSpan(1, 99, FALSE)
    sn = this%span_number
    call assert_equals(99, sn, "Convert span_number at the if statement upper bound")

    this = new_ChangepointSpan(1, 100, FALSE)
    sn = this%span_number
    call assert_equals(1, sn, "Convert span_number 1 above the if statement upper bound")

    this = new_ChangepointSpan(1, 199, FALSE)
    sn = this%span_number
    call assert_equals(100, sn, "Convert span_number just below even hundred")

    this = new_ChangepointSpan(1, 200, FALSE)
    sn = this%span_number
    call assert_equals(1, sn, "Convert span_number at an even hundred")

    this = new_ChangepointSpan(1, 201, FALSE)
    sn = this%span_number
    call assert_equals(2, sn, "Convert span_number just above an even hundred")

    this = new_ChangepointSpan(1, 137, FALSE)
    sn = this%span_number
    call assert_equals(38, sn, "Convert arbitrary span_number needing to be converted")

    this = new_ChangepointSpan(1, -4, FALSE)
    sn = this%span_number
    call assert_equals(-4, sn, "Convert negative span_number")

    this = new_ChangepointSpan(1, 50, FALSE)
    sn = this%span_number
    call assert_equals(50, sn, "Convert span_number in middle of small range")

    this = new_ChangepointSpan(1, 137, FALSE)
    sn = this%span_number
    call assert_equals(38, sn, "Create ChangepointSpan with too large span_number")

    this = new_ChangepointSpan(1, 55, FALSE)
    sn = this%span_number
    call assert_equals(55, sn, "Create ChangepointSpan with small span_number")

  end subroutine test_span_number_convert

  !> Tests the changepoint_span_string function.
  subroutine test_changepoint_span_string()
  
    type(ChangepointSpan) :: this_span
    character(len=145) :: expected_string

    ! Null changepoint_span record
    this_span = null_ChangepointSpan()
    expected_string = "[ChangepointSpan] model_type:  0 span_number:  0 is_deleted: F"
    call assert_equals(expected_string, changepoint_span_string(this_span), "NULL ChangepointSpan")

    ! Typical test cases
    this_span = new_ChangepointSpan(2, 3, TRUE)
    expected_string = "[ChangepointSpan] model_type:  2 span_number:  3 is_deleted: T"
    call assert_equals(expected_string, changepoint_span_string(this_span), "Standard ChangepointSpan string (1)")

    this_span = new_ChangepointSpan(3, 2, FALSE)
    expected_string = "[ChangepointSpan] model_type:  3 span_number:  2 is_deleted: F"
    call assert_equals(expected_string, changepoint_span_string(this_span), "Standard ChangepointSpan string (2)")

  end subroutine test_changepoint_span_string

  !> Tests the eq function for single items and for arrays
  subroutine test_changepoint_span_eq()
    type(ChangepointSpan) :: null_span1
    type(ChangepointSpan) :: null_span2
    type(ChangepointSpan) :: span1a
    type(ChangepointSpan) :: span1b
    type(ChangepointSpan) :: span2a
    type(ChangepointSpan) :: span2b
    type(ChangepointSpan), dimension(:) :: span_array1a(3)
    type(ChangepointSpan), dimension(:) :: span_array1b(3)
    type(ChangepointSpan), dimension(:) :: span_array2(3)
    type(ChangepointSpan), dimension(:) :: span_array3(4)

    ! Define ChangepointSpan to test against
    null_span1 = null_ChangepointSpan()
    null_span2 = null_ChangepointSpan()
    span1a = new_ChangepointSpan(2, 3, TRUE)
    span1b = new_ChangepointSpan(2, 3, TRUE)
    span2a = new_ChangepointSpan(3, 1, FALSE)
    span2b = new_ChangepointSpan(3, 1, FALSE)

    ! Run tests on individual items
    call assert_true(null_span1 .eq. null_span2, "eq for null ChangepointSpans")
    call assert_true(span1a .eq. span1b, "eq for identical ChangepointSpans")
    call assert_false(span1a .eq. span2a, "eq for differing ChangepointSpans")
    call assert_false(span1a .eq. null_span1, "eq for differing ChangepointSpans (one null)")

    ! Set up test arrays
    span_array1a = (/ null_span1, span1a, span2a /)
    span_array1b = (/ null_span2, span1b, span2b /)
    span_array2 = (/ span1a, span2a, null_span1 /)
    span_array3 = (/ span1a, span2a, null_span1, null_span2 /)

    ! Run tests on arrays
    call assert_true(span_array1a .eq. span_array1b, "eq for arrays with identical items")
    call assert_true(span_array1b .eq. span_array1a, "eq for arrays with identical items (swapped)")
    call assert_false(span_array1a .eq. span_array2, "eq for arrays with same length but diff items")
    call assert_false(span_array2 .eq. span_array1a, "eq for arrays with same length but diff items (swapped)")
    call assert_false(span_array2 .eq. span_array3, "eq for arrays with different lengths")
    call assert_false(span_array3 .eq. span_array2, "eq for arrays with different lengths (swapped)")
  end subroutine test_changepoint_span_eq

end module ChangepointSpanTypeTest
