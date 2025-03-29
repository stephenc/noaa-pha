!> @brief The module for running unit tests against the ChangepointSize
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
module ChangepointSizeTest

  use ChangepointSize
  use UnitTest

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine changepoint_size_tests()

    call test_array_sort_two()
    call test_array_sort_three()

  end subroutine changepoint_size_tests

  !> Tests the arrays_sort subroutine with only 2 arrays.
  subroutine test_array_sort_two()
    !Arrays to be sorted
    real, dimension(:) :: sort_array(6)
    integer, dimension(:) :: assoc_array(6)
    ! Expected results
    real, dimension(:) :: exp_sort_array(6)
    integer, dimension(:) :: exp_assoc_array(6)

    sort_array = (/ 2.0, 5.2, 0.1, -6.4, 10.3, 5.3 /)
    assoc_array = (/ 1, 2, 3, 4, 5, 6 /)
    call arrays_sort(6, sort_array, assoc_array)

    exp_sort_array = (/ -6.4, 0.1, 2.0, 5.2, 5.3, 10.3 /)
    exp_assoc_array = (/ 4, 3, 1, 2, 6, 5 /)

    call assert_equals_within_tolerance(sort_array, exp_sort_array, .0001, 'sorted array (2) vs. expected')
    call assert_equals(assoc_array, exp_assoc_array, 'associated array (2) vs. exp_assoc_array')

  end subroutine test_array_sort_two

  !> Tests the arrays_sort subroutine with three arrays.
  subroutine test_array_sort_three()
    ! Arrays to be sorted
    real, dimension(:) :: sort_array(6)
    integer, dimension(:) :: assoc_array1(6)
    real, dimension(:) :: assoc_array2(6)
    ! Expected results
    real, dimension(:) :: exp_sort_array(6)
    integer, dimension(:) :: exp_assoc_array1(6)
    real, dimension(:) :: exp_assoc_array2(6)

    sort_array = (/ 2.0, 5.2, 0.1, -6.4, 10.3, 5.3 /)
    assoc_array1 = (/ 1, 2, 3, 4, 5, 6 /)
    assoc_array2 = (/ 1.9, 2.9, 3.8, 4.7, 5.8, 6.9 /)

    ! Test sort_array with three arrays, ascending
    call arrays_sort(6, sort_array, assoc_array1, assoc_array2, 1)

    exp_sort_array = (/ -6.4, 0.1, 2.0, 5.2, 5.3, 10.3 /)
    exp_assoc_array1 = (/ 4, 3, 1, 2, 6, 5 /)
    exp_assoc_array2 = (/ 4.7, 3.8, 1.9, 2.9, 6.9, 5.8 /)

    call assert_equals_within_tolerance(sort_array, exp_sort_array, .0001, 'sorted array (3, desc) vs. expected')
    call assert_equals(assoc_array1, exp_assoc_array1, 'associated array1 (3, desc) vs. exp_assoc_array1')
    call assert_equals_within_tolerance(assoc_array2, exp_assoc_array2, .0001, 'associated array2 (3, desc) vs. exp_assoc_array2')

    ! Test sort_array with three arrays, ascending, usign default scend rather than specifying ascending
    call arrays_sort(6, sort_array, assoc_array1, assoc_array2)

    call assert_equals_within_tolerance(sort_array, exp_sort_array, .0001, 'sorted array (3, desc) vs. expected')
    call assert_equals(assoc_array1, exp_assoc_array1, 'associated array1 (3, desc) vs. exp_assoc_array1')
    call assert_equals_within_tolerance(assoc_array2, exp_assoc_array2, .0001, 'associated array2 (3, desc) vs. exp_assoc_array2')

    ! Test sort_array with three arrays, descending
    call arrays_sort(6, sort_array, assoc_array1, assoc_array2, 0)

    exp_sort_array = (/ 10.3, 5.3, 5.2, 2.0, 0.1, -6.4 /)
    exp_assoc_array1 = (/ 5, 6, 2, 1, 3, 4 /)
    exp_assoc_array2 = (/ 5.8, 6.9, 2.9, 1.9, 3.8, 4.7 /)

    call assert_equals_within_tolerance(sort_array, exp_sort_array, .0001, 'sorted array (3, asc) vs. expected')
    call assert_equals(assoc_array1, exp_assoc_array1, 'associated array1 (3, asc) vs. exp_assoc_array1')
    call assert_equals_within_tolerance(assoc_array2, exp_assoc_array2, .0001, 'associated array2 (3, asc) vs. exp_assoc_array2')

  end subroutine test_array_sort_three

end module ChangepointSizeTest
