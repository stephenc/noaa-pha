!> @brief Unit tests for ChangepointHit type.
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
module ChangepointHitTypeTest

  use ChangepointHitType
  use UnitTest
  use Logger

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine changepoint_hit_tests

    call test_changpoint_hit_string()
    call test_changpoint_hit_eq()

  end subroutine changepoint_hit_tests

  !> Tests the changepoint_hit_string function.
  subroutine test_changpoint_hit_string()
  
    type(ChangepointHit) :: this_hit
    character(len=145) :: expected_string

    ! Null changepoint_hit record
    this_hit = null_ChangepointHit()
    expected_string = "[ChangepointHit] hit_count:   0 amplitude:    0.00"
    call assert_equals(expected_string, changepoint_hit_string(this_hit), "NULL ChangepointHit")

    ! Typical test case
    this_hit = new_ChangepointHit(23, 58.72)
    expected_string = "[ChangepointHit] hit_count:  23 amplitude:   58.72"
    call assert_equals(expected_string, changepoint_hit_string(this_hit), "Standard ChangepointHit string")

  end subroutine test_changpoint_hit_string

  !> Tests the eq function for single items and for arrays
  subroutine test_changpoint_hit_eq()
    type(ChangepointHit) :: null_hit1
    type(ChangepointHit) :: null_hit2
    type(ChangepointHit) :: hit1a
    type(ChangepointHit) :: hit1b
    type(ChangepointHit) :: hit2a
    type(ChangepointHit) :: hit2b
    type(ChangepointHit), dimension(:) :: hit_array1a(3)
    type(ChangepointHit), dimension(:) :: hit_array1b(3)
    type(ChangepointHit), dimension(:) :: hit_array2(3)
    type(ChangepointHit), dimension(:) :: hit_array3(4)

    ! Define ChangepointHit to test against
    null_hit1 = null_ChangepointHit()
    null_hit2 = null_ChangepointHit()
    hit1a = new_ChangepointHit(20, 62.0)
    hit1b = new_ChangepointHit(20, 62.0)
    hit2a = new_ChangepointHit(11, 51.5)
    hit2b = new_ChangepointHit(11, 51.5)

    ! Run tests on individual items
    call assert_true(null_hit1 .eq. null_hit2, "eq for null ChangepointHits")
    call assert_true(hit1a .eq. hit1b, "eq for identical ChangepointHits")
    call assert_false(hit1a .eq. hit2a, "eq for differing ChangepointHits")
    call assert_false(hit1a .eq. null_hit1, "eq for differing ChangepointHits (one null)")

    ! Set up test arrays
    hit_array1a = (/ null_hit1, hit1a, hit2a /)
    hit_array1b = (/ null_hit2, hit1b, hit2b /)
    hit_array2 = (/ hit1a, hit2a, null_hit1 /)
    hit_array3 = (/ hit1a, hit2a, null_hit1, null_hit2 /)

    ! Run tests on arrays
    call assert_true(hit_array1a .eq. hit_array1b, "eq for arrays with identical items")
    call assert_true(hit_array1b .eq. hit_array1a, "eq for arrays with identical items (swapped)")
    call assert_false(hit_array1a .eq. hit_array2, "eq for arrays with same length but diff items")
    call assert_false(hit_array2 .eq. hit_array1a, "eq for arrays with same length but diff items (swapped)")
    call assert_false(hit_array2 .eq. hit_array3, "eq for arrays with different lengths")
    call assert_false(hit_array3 .eq. hit_array2, "eq for arrays with different lengths (swapped)")
  end subroutine test_changpoint_hit_eq

end module ChangepointHitTypeTest

