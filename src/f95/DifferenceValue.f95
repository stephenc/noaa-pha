!> @brief
!! A type which represents a difference value and related information.
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
module DifferenceValueType

  use Logger
  use AlgorithmParameters

  implicit none

  private :: difference_equals, difference_array_equals

  !> A type containing information about a difference value.
  !! at a station.
  type DifferenceValue
    real :: value1
    real :: value2
    real :: difference
  end type DifferenceValue

  !> Creates a new DifferenceValue instance.
  interface new_DifferenceValue
    module procedure new_DifferenceValue
  end interface new_DifferenceValue

  !> Test equality of DifferenceValue instances.
  interface operator (.eq.)
    module procedure difference_equals
    module procedure difference_array_equals
  end interface

contains

  !> Creates a new difference value. The calculation is not performed here, only stored.
  !! Rules for when a difference should be set to MISSING might make the difference
  !! value different than expected, based on value1 and value2.
  !!
  !! @param value1 The first value from which the difference was calculated.
  !! @param value2 The second value from which the difference was calculated.
  !! @param difference The difference between value1 and value2.
  !! @return The DifferenceValue instance being created.
  function new_DifferenceValue(value1, value2, difference) &
               result(this)
    type(DifferenceValue) :: this
    real :: value1
    real :: value2
    real :: difference

    ! Set values for Station
    this%value1 = value1
    this%value2 = value2
    this%difference = difference

  end function new_DifferenceValue

  !> Returns a "null" DifferenceValue. This is handy for
  !! setting array values to "null" so that junk isn't
  !! accidentally stored in the array.
  !!
  !! @return A DifferenceValue with empty/zero values and
  !!         not_null set to false.
  function null_DifferenceValue() result(this)
    type(DifferenceValue) :: this

    this = new_DifferenceValue(ZERO_REAL, ZERO_REAL, ZERO_REAL)

  end function null_DifferenceValue

  !> Outputs a string representation of a DifferenceValue instance.
  !!
  !! @param this The DifferenceValue instance.
  !! @return The string representation of this DifferenceValue.
  function difference_string(this) result(difference_str)
    type(DifferenceValue) :: this
    character(len=145) :: difference_str

    ! If the station object is null, skip the rest
    if(difference_is_null(this)) then
      difference_str = "[DifferenceValue] NULL"
      return
    endif

    ! Create difference string
    write(difference_str, '(a,f9.4,a,f9.4,a,f9.4)') '[DifferenceValue] value1: ', &
          this%value1, ' value2: ',this%value2, ' difference: ',this%difference

  end function difference_string

  !> Tests the equality of two DifferenceValue instances.
  !!
  !! @param difference1 The first DifferenceValue instance to compare.
  !! @param difference2 The second DifferenceValue instance to compare.
  !! @return True if the DifferenceValue instances have equal values
  !!         for their attributes. Otherwise false.
  function difference_equals(difference1, difference2) result(is_equal)
    type(DifferenceValue), intent(in) :: difference1
    type(DifferenceValue), intent(in) :: difference2
    logical :: is_equal

    if((difference1%value1 .eq. difference2%value1) .and. &
       (difference1%value2 .eq. difference2%value2).and. &
       (difference1%difference .eq. difference2%difference)) then
      is_equal = .true.
    else
      is_equal = .false.
    endif  

  end function difference_equals

  !> Tests the equality of two arrays of DifferenceValue instances.
  !! 
  !! @param difference_array1 The first array of DifferenceValue instances
  !!                       to compare.
  !! @param difference_array2 The second array of DifferenceValue instances
  !!                       to compare.
  !! @return True if the arrays are the same length and contain equal
  !!         Station instances in the same order. Otherwise false.
  function difference_array_equals(difference_array1, difference_array2) &
      result(is_equal)
    type(DifferenceValue), dimension(:), intent(in) :: difference_array1
    type(DifferenceValue), dimension(:), intent(in) :: difference_array2
    logical :: is_equal
    integer :: i

    if(size(difference_array1) /= size(difference_array2)) then
      is_equal = .false.
      return
    endif

    do i=1, size(difference_array1)
      if(.not. difference_array1(i) .eq. difference_array2(i)) then
        is_equal = .false.
        return
      endif
    end do

    is_equal = .true.

  end function difference_array_equals

  !> Determine whether this DifferenceValue instance only contains
  !! the default initialization values, indicating it is "null".
  !!
  !! @param this The DifferenceValue to analyze.
  !! @result Returns true if this difference is "null". Otherwise, false.
  function difference_is_null(this) result(is_null)
    type(DifferenceValue) :: this
    logical :: is_null

    is_null = (this .eq. null_DifferenceValue())

  end function difference_is_null

end module DifferenceValueType

!> @file
!! The file containing the DifferenceValue type and any related
!! subroutines.
