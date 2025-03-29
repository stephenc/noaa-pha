!> @brief
!! A type which represents information about a single skyline date of
!! a potential changepoint between a target and all its neighbors.
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
module ChangepointHitType

  use Logger
  use AlgorithmParameters

  implicit none

  private :: new_ChangepointHit_int_convert, changepoint_hit_equals, changepoint_hit_array_equals

  !> A type containing information
  !! about a single skyline date of a potential changepoint between
  !! a target and all it's neighbors.
  type ChangepointHit
    integer(1) :: hit_count
    real :: amplitude
  end type ChangepointHit

  !> Creates a new ChangepointHit instance.
  interface new_ChangepointHit
    module procedure new_ChangepointHit
    module procedure new_ChangepointHit_int_convert
  end interface new_ChangepointHit

  !> Test equality of ChangepointHit instances.
  interface operator (.eq.)
    module procedure changepoint_hit_equals
    module procedure changepoint_hit_array_equals
  end interface

contains

  !> Creates a new ChangepointHit.
  !!
  !! @param hit_count The number of hits found for all neighbors for the month
  !! @param amplitude The intial estimated amplitude for this hit
  !! @return The ChangepointHit instance being created.
  function new_ChangepointHit(hit_count, amplitude) result(this)
    type (ChangepointHit) :: this
    integer(1) :: hit_count
    real :: amplitude

    ! Set values for ChangepointHit
    this%hit_count = hit_count
    this%amplitude = amplitude

  end function new_ChangepointHit

  !> Creates a new ChangepointHit.
  !!
  !! @param hit_count The number of hits found for all neighbors for the month
  !! @param amplitude The intial estimated amplitude for this hit
  !! @return The ChangepointHit instance being created.
  function new_ChangepointHit_int_convert(hit_count, amplitude) result(this)
    type (ChangepointHit) :: this
    integer :: hit_count
    real :: amplitude

    integer(1) :: hc
    hc = hit_count

    ! Set values for ChangepointHit
    this%hit_count = hc
    this%amplitude = amplitude

  end function new_ChangepointHit_int_convert

  !> Returns a "null" ChangepointHit. This is handy for
  !! setting array values to "null" so that junk isn't
  !! accidentally stored in the array.
  !!
  !! @return A ChangepointHit with empty/zero values and
  !!         not_null set to false.
  function null_ChangepointHit() result(this)
    type (ChangepointHit) :: this

    this = new_ChangepointHit(ZERO_SIZE1, ZERO_REAL)

  end function null_ChangepointHit

  !> Outputs a string representation of a ChangepointHit instance.
  !!
  !! @param this The ChangepointHit instance.
  !! @return The string representation of this ChangepointHit.
  function changepoint_hit_string(this) result(changepoint_hit_str)
    type(ChangepointHit) :: this
    character(len=145) :: changepoint_hit_str

    ! Create station string
    write(changepoint_hit_str, '(a,a,i3,a,f7.2)') '[ChangepointHit]', &
          ' hit_count: ',this%hit_count, ' amplitude: ',this%amplitude

  end function changepoint_hit_string

  !> Tests the equality of two ChangepointHit instances.
  !!
  !! @param changepoint_hit1 The first ChangepointHit instance to compare.
  !! @param changepoint_hit2 The second ChangepointHit instance to compare.
  !! @return True if the Changepoint instances have equal values
  !!         for their attributes. Otherwise false.
  function changepoint_hit_equals(changepoint_hit1, changepoint_hit2) result(is_equal)
    type(ChangepointHit), intent(in) :: changepoint_hit1
    type(ChangepointHit), intent(in) :: changepoint_hit2
    logical :: is_equal

    if((changepoint_hit1%hit_count .eq. changepoint_hit2%hit_count) .and. &
       (changepoint_hit1%amplitude .eq. changepoint_hit2%amplitude)) then
      is_equal = .true.
    else
      is_equal = .false.
    endif  

  end function changepoint_hit_equals

  !> Tests the equality of two arrays of Changepoint instances.
  !! 
  !! @param changepoint_hit_array1 The first array of ChangepointHit instances
  !!                       to compare.
  !! @param changepoint_hit_array2 The second array of ChangepointHit instances
  !!                       to compare.
  !! @return True if the arrays are the same length and contain equal
  !!         ChangepointHit instances in the same order. Otherwise false.
  function changepoint_hit_array_equals(changepoint_hit_array1, changepoint_hit_array2) &
      result(is_equal)
    type(ChangepointHit), dimension(:), intent(in) :: changepoint_hit_array1
    type(ChangepointHit), dimension(:), intent(in) :: changepoint_hit_array2
    logical :: is_equal
    integer :: i

    if(size(changepoint_hit_array1) /= size(changepoint_hit_array2)) then
      is_equal = .false.
      return
    endif

    do i=1, size(changepoint_hit_array1)
      if(.not. changepoint_hit_array1(i) .eq. changepoint_hit_array2(i)) then
        is_equal = .false.
        return
      endif
    end do

    is_equal = .true.

  end function changepoint_hit_array_equals

end module ChangepointHitType

!> @file
!! The file containing the ChangepointHit type and any related
!! subroutines.
