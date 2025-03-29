!> @brief Provides assert statements for unit tests and tracks the passage 
!! and failure of all asserts.  
!!
!! @copyright
!! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC
!! DOMAIN AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE. THEY ARE
!! FURNISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
!! INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
!! EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE AND
!! DOCUMENTATION FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR
!! THE USE OF THE SOFTWARE AND DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL
!! SUPPORT TO USERS.
!! 
module UnitTest

implicit none

! Declare which subroutines are public
public :: assert_equals, assert_equals_within_tolerance
public :: assert_true, assert_false
public :: fail
public :: report_test_results

private :: to_string
private :: total_asserts, failed_asserts

!> Assert two values are equal.
!! 
!! @param[in] val1 The first value to compare.
!! @param[in] val2 The second value to compare.
!! @param[in] fail_message The message to be displayed upon failure.
interface assert_equals

  ! Non-array assert_equals
  !> Assert two integer values are equal.
  module procedure assert_equals_int
  !> Assert two complex number values are equal.
  module procedure assert_equals_complex
  !> Assert two character array values are equal.
  module procedure assert_equals_chars
  !> Assert two logical values are equal.
  module procedure assert_equals_logical

  !> Assert two 1-D integer arrays are equal.
  module procedure assert_equals_int_1d
  !> Assert two 2-D integer arrays are equal.
  module procedure assert_equals_int_2d
  !> Assert two 3-D integer arrays are equal.
  module procedure assert_equals_int_3d
  !> Assert two 4-D integer arrays are equal.
  module procedure assert_equals_int_4d
  !> Assert two 5-D integer arrays are equal.
  module procedure assert_equals_int_5d

  !> Assert two 1-D complex arrays are equal.
  module procedure assert_equals_complex_1d
  !> Assert two 2-D complex arrays are equal.
  module procedure assert_equals_complex_2d
  !> Assert two 3-D complex arrays are equal.
  module procedure assert_equals_complex_3d
  !> Assert two 4-D complex arrays are equal.
  module procedure assert_equals_complex_4d
  !> Assert two 5-D complex arrays are equal.
  module procedure assert_equals_complex_5d

  !> Assert two 1-D character arrays are equal.
  module procedure assert_equals_chars_1d
  !> Assert two 2-D character arrays are equal.
  module procedure assert_equals_chars_2d

  !> Assert two 1-D character arrays are equal.
  module procedure assert_equals_logical_1d
  !> Assert two 2-D character arrays are equal.
  module procedure assert_equals_logical_2d

end interface assert_equals

!> @brief Assert two values are equal within a specified tolerance.
!> @details A tolerance is required for comparing reals because they are
!! not represented exactly, for instance, 28.24 might actually
!! be represented as 28.23339 and so would fail an exact 
!! comparison against 28.24.
!!
!! @param[in] val1 The first value to compare.
!! @param[in] val2 The second value to compare.
!! @param[in] tolerance The maximum allowed absolute difference between the
!!                      two values in order to consider them equivalent.
!! @param[in] fail_message The message to be displayed upon failure.
interface assert_equals_within_tolerance
  !> Assert two real number values are equal within a specified tolerance.
  module procedure assert_equals_real
  !> Assert two 1-D real number arrays are equal within a specified tolerance.
  module procedure assert_equals_real_1d
  !> Assert two 2-D real number arrays are equal within a specified tolerance.
  module procedure assert_equals_real_2d
  !> Assert two 3-D real number arrays are equal within a specified tolerance.
  module procedure assert_equals_real_3d
  !> Assert two 4-D real number arrays are equal within a specified tolerance.
  module procedure assert_equals_real_4d
  !> Assert two 5-D real number arrays are equal within a specified tolerance.
  module procedure assert_equals_real_5d
end interface assert_equals_within_tolerance

private 

!! Helper functions (private), included in this module only 
!! to allow this module to remain self-contained?
interface to_string
  module procedure to_string_real
  module procedure to_string_int
  module procedure to_string_complex
  module procedure to_string_logical
  module procedure to_string_int_1d
  module procedure to_string_int_2d
  module procedure to_string_int_3d
  module procedure to_string_int_4d
  module procedure to_string_int_5d
  module procedure to_string_real_1d
  module procedure to_string_real_2d
  module procedure to_string_real_3d
  module procedure to_string_real_4d
  module procedure to_string_real_5d
  module procedure to_string_complex_1d
  module procedure to_string_complex_2d
  module procedure to_string_complex_3d
  module procedure to_string_complex_4d
  module procedure to_string_complex_5d
  module procedure to_string_logical_1d
  module procedure to_string_logical_2d
end interface to_string

integer :: total_asserts = 0
integer :: failed_asserts = 0

contains

!> Assert two real values are equal.
!! 
!! @param[in] val1 The first value to compare.
!! @param[in] val2 The second value to compare.
!! @param[in] tolerance The maximum allowed absolute difference between the
!!                      two values in order to consider them equivalent.
!! @param[in]  fail_message The message to be displayed upon failure.
subroutine assert_equals_real(val1, val2, tolerance, fail_message) 
  real, intent(in) :: val1
  real, intent(in) :: val2
  real, intent(in) :: tolerance

  character(len=*), intent(in) :: fail_message

  total_asserts = total_asserts + 1
  if(abs(val1-val2) > tolerance) then
    call equals_failure(to_string(val1), to_string(val2), "tolerance: "//to_string(tolerance)//" "//trim(adjustl(fail_message)))
  endif

  return

end subroutine assert_equals_real


!> Assert two integer values are equal.
!! 
!! @param[in] val1 The first value to compare.
!! @param[in] val2 The second value to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_int(val1, val2, fail_message)
  integer :: val1
  integer :: val2
  character(len=*) :: fail_message

  total_asserts = total_asserts + 1
  if(val1 /= val2) then
    call equals_failure(to_string(val1), to_string(val2), fail_message)
  endif

  return

end subroutine assert_equals_int


!> Assert two complex values are equal.
!! 
!! @param[in] val1 The first value to compare.
!! @param[in] val2 The second value to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_complex(val1, val2, fail_message)
  complex :: val1
  complex :: val2
  character(len=*) :: fail_message

  total_asserts = total_asserts + 1
  if(val1 /= val2) then
    call equals_failure(to_string(val1), to_string(val2), fail_message)
  endif

  return

end subroutine assert_equals_complex


!> Assert two logical values are equal.
!!
!! @param[in] val1 The first value to compare.
!! @param[in] val2 The second value to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_logical(val1, val2, fail_message)
  logical :: val1
  logical :: val2
  character(len=*) :: fail_message

  total_asserts = total_asserts + 1
  if(val1.NEQV.val2) then
    call equals_failure(to_string(val1), to_string(val2), fail_message)
  endif

  return

end subroutine assert_equals_logical


!> Assert two character array values are equal.
!! 
!! @param[in] val1 The first value to compare.
!! @param[in] val2 The second value to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_chars(val1, val2, fail_message)
  character(len=*) :: val1
  character(len=*) :: val2
  character(len=*) :: fail_message

  total_asserts = total_asserts + 1
  if(val1 /= val2) then
    call equals_failure(val1, val2, fail_message)
  endif

  return

end subroutine assert_equals_chars


!> Assert two 1-dimensional integer arrays are equal.
!! 
!! @param[in] array1 The first integer array to compare.
!! @param[in] array2 The second integer array to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_int_1d(array1, array2, fail_message)
  integer, dimension(:) :: array1
  integer, dimension(:) :: array2
  character(len=*)      :: fail_message

  total_asserts = total_asserts + 1
  if(any(array1 /= array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif
end subroutine assert_equals_int_1d

!> Assert two 2-dimensional integer arrays are equal.
!! 
!! @param[in] array1 The first integer array to compare.
!! @param[in] array2 The second integer array to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_int_2d(array1, array2, fail_message)
  integer, dimension(:,:) :: array1
  integer, dimension(:,:) :: array2
  character(len=*)        :: fail_message

  total_asserts = total_asserts + 1
  if(any(array1 /= array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif
end subroutine assert_equals_int_2d

!> Assert two 3-dimensional integer arrays are equal.
!! 
!! @param[in] array1 The first integer array to compare.
!! @param[in] array2 The second integer array to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_int_3d(array1, array2, fail_message)
  integer, dimension(:,:,:) :: array1
  integer, dimension(:,:,:) :: array2
  character(len=*)        :: fail_message

  total_asserts = total_asserts + 1
  if(any(array1 /= array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif
end subroutine assert_equals_int_3d

!> Assert two 4-dimensional integer arrays are equal.
!! 
!! @param[in] array1 The first integer array to compare.
!! @param[in] array2 The second integer array to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_int_4d(array1, array2, fail_message)
  integer, dimension(:,:,:,:) :: array1
  integer, dimension(:,:,:,:) :: array2
  character(len=*)        :: fail_message

  total_asserts = total_asserts + 1
  if(any(array1 /= array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif
end subroutine assert_equals_int_4d

!> Assert two 5-dimensional integer arrays are equal.
!! 
!! @param[in] array1 The first integer array to compare.
!! @param[in] array2 The second integer array to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_int_5d(array1, array2, fail_message)
  integer, dimension(:,:,:,:,:) :: array1
  integer, dimension(:,:,:,:,:) :: array2
  character(len=*)        :: fail_message

  total_asserts = total_asserts + 1
  if(any(array1 /= array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif
end subroutine assert_equals_int_5d

!> Assert two 1-D real arrays are equal to within tolerance.
!! 
!! @param[in] val1 The first array to compare.
!! @param[in] val2 The second array to compare.
!! @param[in] tolerance The maximum allowed absolute difference between the
!!                      two values in order to consider them equivalent.
!! @param[in]  fail_message The message to be displayed upon failure.
subroutine assert_equals_real_1d(val1, val2, tolerance, fail_message) 
  real, dimension(:), intent(in) :: val1
  real, dimension(:), intent(in) :: val2
  real, intent(in) :: tolerance

  character(len=*), intent(in) :: fail_message

  total_asserts = total_asserts + 1
  if(any(abs(val1-val2) > tolerance)) then
    call equals_failure(to_string(val1), to_string(val2), "tolerance: "//to_string(tolerance)//" "//trim(adjustl(fail_message)))
  endif

  return

end subroutine assert_equals_real_1d

!> Assert two 2-D real arrays are equal to within tolerance.
!! 
!! @param[in] val1 The first array to compare.
!! @param[in] val2 The second array to compare.
!! @param[in] tolerance The maximum allowed absolute difference between the
!!                      two values in order to consider them equivalent.
!! @param[in]  fail_message The message to be displayed upon failure.
subroutine assert_equals_real_2d(val1, val2, tolerance, fail_message) 
  real, dimension(:,:), intent(in) :: val1
  real, dimension(:,:), intent(in) :: val2
  real, intent(in) :: tolerance

  character(len=*), intent(in) :: fail_message

  total_asserts = total_asserts + 1
  if(any(abs(val1-val2) > tolerance)) then
    call equals_failure(to_string(val1), to_string(val2), "tolerance: "//to_string(tolerance)//" "//trim(adjustl(fail_message)))
  endif

  return

end subroutine assert_equals_real_2d

!> Assert two 3-D real arrays are equal to within tolerance.
!! 
!! @param[in] val1 The first array to compare.
!! @param[in] val2 The second array to compare.
!! @param[in] tolerance The maximum allowed absolute difference between the
!!                      two values in order to consider them equivalent.
!! @param[in]  fail_message The message to be displayed upon failure.
subroutine assert_equals_real_3d(val1, val2, tolerance, fail_message) 
  real, dimension(:,:,:), intent(in) :: val1
  real, dimension(:,:,:), intent(in) :: val2
  real, intent(in) :: tolerance

  character(len=*), intent(in) :: fail_message

  total_asserts = total_asserts + 1
  if(any(abs(val1-val2) > tolerance)) then
    call equals_failure(to_string(val1), to_string(val2), "tolerance: "//to_string(tolerance)//" "//trim(adjustl(fail_message)))
  endif

  return

end subroutine assert_equals_real_3d

!> Assert two 4-D real arrays are equal to within tolerance.
!! 
!! @param[in] val1 The first array to compare.
!! @param[in] val2 The second array to compare.
!! @param[in] tolerance The maximum allowed absolute difference between the
!!                      two values in order to consider them equivalent.
!! @param[in]  fail_message The message to be displayed upon failure.
subroutine assert_equals_real_4d(val1, val2, tolerance, fail_message) 
  real, dimension(:,:,:,:), intent(in) :: val1
  real, dimension(:,:,:,:), intent(in) :: val2
  real, intent(in) :: tolerance

  character(len=*), intent(in) :: fail_message

  total_asserts = total_asserts + 1
  if(any(abs(val1-val2) > tolerance)) then
    call equals_failure(to_string(val1), to_string(val2), "tolerance: "//to_string(tolerance)//" "//trim(adjustl(fail_message)))
  endif

  return

end subroutine assert_equals_real_4d

!> Assert two 5-D real arrays are equal to within tolerance.
!! 
!! @param[in] val1 The first array to compare.
!! @param[in] val2 The second array to compare.
!! @param[in] tolerance The maximum allowed absolute difference between the
!!                      two values in order to consider them equivalent.
!! @param[in]  fail_message The message to be displayed upon failure.
subroutine assert_equals_real_5d(val1, val2, tolerance, fail_message) 
  real, dimension(:,:,:,:,:), intent(in) :: val1
  real, dimension(:,:,:,:,:), intent(in) :: val2
  real, intent(in) :: tolerance

  character(len=*), intent(in) :: fail_message

  total_asserts = total_asserts + 1
  if(any(abs(val1-val2) > tolerance)) then
    call equals_failure(to_string(val1), to_string(val2), "tolerance: "//to_string(tolerance)//" "//trim(adjustl(fail_message)))
  endif

  return

end subroutine assert_equals_real_5d

!> Assert two 1-dimensional complex arrays are equal.
!! 
!! @param[in] array1 The first complex array to compare.
!! @param[in] array2 The second complex array to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_complex_1d(array1, array2, fail_message)
  complex, dimension(:) :: array1
  complex, dimension(:) :: array2
  character(len=*)      :: fail_message

  total_asserts = total_asserts + 1
  if(any(array1 /= array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif
end subroutine assert_equals_complex_1d

!> Assert two 2-dimensional complex arrays are equal.
!!
!! @param[in] array1 The first complex array to compare.
!! @param[in] array2 The second complex array to compare
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_complex_2d(array1, array2, fail_message)
  complex, dimension(:,:) :: array1
  complex, dimension(:,:) :: array2
  character(len=*)      :: fail_message

  total_asserts = total_asserts + 1

  if(any(array1 /= array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif

end subroutine assert_equals_complex_2d

!> Assert two 3-dimensional complex arrays are equal.
!!
!! @param[in] array1 The first complex array to compare.
!! @param[in] array2 The second complex array to compare
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_complex_3d(array1, array2, fail_message)
  complex, dimension(:,:,:) :: array1
  complex, dimension(:,:,:) :: array2
  character(len=*)      :: fail_message

  total_asserts = total_asserts + 1

  if(any(array1 /= array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif

end subroutine assert_equals_complex_3d

!> Assert two 4-dimensional complex arrays are equal.
!!
!! @param[in] array1 The first complex array to compare.
!! @param[in] array2 The second complex array to compare
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_complex_4d(array1, array2, fail_message)
  complex, dimension(:,:,:,:) :: array1
  complex, dimension(:,:,:,:) :: array2
  character(len=*)      :: fail_message

  total_asserts = total_asserts + 1

  if(any(array1 /= array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif

end subroutine assert_equals_complex_4d

!> Assert two 5-dimensional complex arrays are equal.
!!
!! @param[in] array1 The first complex array to compare.
!! @param[in] array2 The second complex array to compare
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_complex_5d(array1, array2, fail_message)
  complex, dimension(:,:,:,:,:) :: array1
  complex, dimension(:,:,:,:,:) :: array2
  character(len=*)      :: fail_message

  total_asserts = total_asserts + 1

  if(any(array1 /= array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif

end subroutine assert_equals_complex_5d

!> Assert two 1-dimensional character arrays are equal.
!! 
!! @param[in] array1 The first character array to compare.
!! @param[in] array2 The second character array to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_chars_1d(array1, array2, fail_message)
  character(len=*), dimension(:) :: array1
  character(len=*), dimension(:) :: array2
  character(len=*)      :: fail_message
  integer :: index

  total_asserts = total_asserts + 1

  index = 1
  do while (index <= size(array1))
    if(array1(index) /= array2(index)) then
      call equals_failure(array1(index), array2(index), fail_message)
      return
    endif
    index = index + 1
  enddo
end subroutine assert_equals_chars_1d

!> Assert two 2-dimensional charaacter arrays are equal.
!!
!> @todo gfortran doesn't like arrays with variable length strings
!!  so have to implement a method that pads strings to match largest length
!! @param[in] array1 The first character array to compare.
!! @param[in] array2 The second character array to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_chars_2d(array1, array2, fail_message)
  character(len=*), dimension(:,:) :: array1
  character(len=*), dimension(:,:) :: array2
  character(len=*)      :: fail_message
  integer :: index1
  integer :: index2

  total_asserts = total_asserts + 1

  index2 = 1
  do while (index2 <= size(array1,2))
    index1 = 1
    do while (index1 <= size(array1,1))
      if(array1(index1,index2) /= array2(index1,index2)) then
        call equals_failure(array1(index1,index2), array2(index1,index2), fail_message)
        return
      endif
      index1 = index1 + 1
    enddo
    index2 = index2 + 1
  enddo
end subroutine assert_equals_chars_2d

!> Assert two 1-dimensional logical arrays are equal.
!! 
!! @param[in] array1 The first logical array to compare.
!! @param[in] array2 The second logical array to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_logical_1d(array1, array2, fail_message)
  logical, dimension(:) :: array1
  logical, dimension(:) :: array2
  character(len=*)      :: fail_message

  total_asserts = total_asserts + 1
  if(any(array1 .neqv. array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif
end subroutine assert_equals_logical_1d

!> Assert two 2-dimensional logical arrays are equal.
!! 
!! @param[in] array1 The first logical array to compare.
!! @param[in] array2 The second logical array to compare.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_equals_logical_2d(array1, array2, fail_message)
  logical, dimension(:,:) :: array1
  logical, dimension(:,:) :: array2
  character(len=*)      :: fail_message

  total_asserts = total_asserts + 1
  if(any(array1 .neqv. array2)) then
    call equals_failure(to_string(array1), to_string(array2), fail_message)
  endif
end subroutine assert_equals_logical_2d

!> Assert a logical value is true.
!! 
!! @param[in] val The logical value to be tested for true.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_true(val, fail_message) 
  logical :: val
  character(len=*) :: fail_message

  total_asserts = total_asserts + 1
  if(.not. val) then
    write(*,*) 'Value expected to be true, but was false: ', val, fail_message
    failed_asserts = failed_asserts + 1
  endif

  return

end subroutine assert_true

!> Assert a logical value is false.
!!
!! @param[in] val The logical value to be tested for false.
!! @param[in] fail_message The message to be displayed upon failure.
subroutine assert_false(val, fail_message)
  logical :: val
  character(len=*) :: fail_message

  total_asserts = total_asserts + 1
  if(val) then
    write(*,*) 'Value expected to be false, but was true:', val, fail_message
    failed_asserts = failed_asserts + 1
  endif

  return

end subroutine assert_false

!> @brief Will always fail with the given failure message. 
!> @details This can be useful as a temporary reminder, 
!! ensuring an unimplemented unit test fails until it is 
!! properly implemented.
!!
!! @param[in] fail_message The message to be displayed upon failure.
subroutine fail(fail_message)
  character(len=*) :: fail_message

  write(*,*) 'This test is set to always fail. ', fail_message
  total_asserts = total_asserts + 1
  failed_asserts = failed_asserts + 1

  return

end subroutine fail


!> Writes to stdout the results of all tests run, including
!! number of total tests, passed tests, and failed tests.
!!
!! @param[out] total Total number of asserts that ran
!! @param[out] passed Total number of successful asserts that ran
!! @param[out] failed Total number of failed asserts that ran
subroutine report_test_results(total, passed, failed)
  integer, intent(out) :: total
  integer, intent(out) :: passed
  integer, intent(out) :: failed
  
  failed = failed_asserts
  total = total_asserts
  passed = total - failed

  write(*,*) 'Tests complete.'
  write(*,*) 'Total:  ', total
  write(*,*) 'Passed: ', passed
  write(*,*) 'Failed: ', failed
    
  return
end subroutine report_test_results

!> A convenience method for handling a failure in any
!! assert_equals method.
!!
!! @param[in] val1 A string representation of the first value
!! @param[in] val2 A string representation of the second value
!! @param[in] fail_message The fail_message passed in from the caller of the assert_equals
subroutine equals_failure(val1, val2, fail_message)
  character(len=*), intent(in) :: val1
  character(len=*), intent(in) :: val2
  character(len=*), intent(in) :: fail_message

  write(*,*) 'Values not equal: ', trim(adjustl(val1)), ' /= ', trim(adjustl(val2)), ' ', fail_message
  failed_asserts = failed_asserts + 1

  return

end subroutine equals_failure

! to_string_xxxx functions: For converting non-strings to strings

!> Converts a real to a string
function to_string_real(val) result (str)
  real :: val
  character(len=300) :: str

  write(str,*) val
  str = trim(adjustl(str))
  return
end function to_string_real

!> Converts an integer to a string
function to_string_int(val) result (str)
  integer :: val
  character(len=200) :: str

  write(str,*) val
  str = trim(adjustl(str))
  return
end function to_string_int

!> Converts a complex number to a string
function to_string_complex(val) result (str)
  complex :: val
  character(len=500) :: str

  write(str,*) val
  str = trim(adjustl(str))
  return
end function to_string_complex


!> Converts a logical to a string
function to_string_logical(val) result (str)
  logical :: val
  character(len=5) :: str

  if(val) then
    str = 'True'
  else
    str = 'False'
  endif
  return
end function to_string_logical


!> Converts a 1-D integer array to a string
function to_string_int_1d(val) result (str)
  integer, dimension(:) :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(I5))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_int_1d


!> Converts a 2-D integer array to a string
function to_string_int_2d(val) result (str)
  integer, dimension(:,:)  :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(I5))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_int_2d

!> Converts a 3-D integer array to a string
function to_string_int_3d(val) result (str)
  integer, dimension(:,:,:)  :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(I5))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_int_3d

!> Converts a 4-D integer array to a string
function to_string_int_4d(val) result (str)
  integer, dimension(:,:,:,:)  :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(I5))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_int_4d

!> Converts a 5-D integer array to a string
function to_string_int_5d(val) result (str)
  integer, dimension(:,:,:,:,:)  :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(I5))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_int_5d

!> Converts a 1-D real array to a string
function to_string_real_1d(val) result (str)
  real, dimension(:) :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(f5.2))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_real_1d

!> Converts a 2-D real array to a string
function to_string_real_2d(val) result (str)
  real, dimension(:,:) :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(f5.2))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_real_2d

!> Converts a 3-D real array to a string
function to_string_real_3d(val) result (str)
  real, dimension(:,:,:) :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(f5.2))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_real_3d

!> Converts a 4-D real array to a string
function to_string_real_4d(val) result (str)
  real, dimension(:,:,:,:) :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(f5.2))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_real_4d

!> Converts a 5-D real array to a string
function to_string_real_5d(val) result (str)
  real, dimension(:,:,:,:,:) :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(f5.2))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_real_5d

!> Converts a 1-D complex array to a string
function to_string_complex_1d(val) result (str)
  complex, dimension(:) :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(F5.2,1x,F5.2))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_complex_1d


!> Converts a 2-D complex array to a string
function to_string_complex_2d(val) result (str)
  complex, dimension(:,:)  :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(2F8.4))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_complex_2d

!> Converts a 3-D complex array to a string
function to_string_complex_3d(val) result (str)
  complex, dimension(:,:,:)  :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(F8.4,F8.4))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_complex_3d

!> Converts a 4-D complex array to a string
function to_string_complex_4d(val) result (str)
  complex, dimension(:,:,:,:)  :: val
  character(len=1000) :: str
  character(len=1000) :: str_fmt

  write(str_fmt,'( "(", I3, "(F8.4,F8.4))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_complex_4d

!> Converts a 5-D complex array to a string
function to_string_complex_5d(val) result (str)
  complex, dimension(:,:,:,:,:)  :: val
  character(len=2000) :: str
  character(len=2000) :: str_fmt

  write(str_fmt,'( "(", I3, "(F8.4,F8.4))" )' )  size(val)
  write(str,str_fmt) val
  str = trim(adjustl(str))
  return
end function to_string_complex_5d

!> Converts a 1-D logical array to a string
function to_string_logical_1d(val) result (str)
  logical, dimension(:) :: val
  character(len=200) :: str

  write(str,*) val
  str = trim(adjustl(str))
  return
end function to_string_logical_1d

!> Converts a 2-D logical array to a string
function to_string_logical_2d(val) result (str)
  logical, dimension(:,:)  :: val
  character(len=200) :: str

  write(str,*) val
  str = trim(adjustl(str))
  return
end function to_string_logical_2d

end module UnitTest

!> @file 
!! Contains the UnitTest module while provides assert statements 
!! and other helpful functionality for creating unit tests.

