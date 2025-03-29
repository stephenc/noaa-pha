!> @brief
!! A type which represents an element value and flag data for a
!! particular station and date.
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
module ElementValueType

  use Logger
  use AlgorithmParameters

  implicit none

  private :: element_value_equals, element_value_array_equals

  !> A type containing information
  !! about a single skyline date of a potential changepoint between
  !! a target and all it's neighbors.
  type ElementValue
    real :: value
    character(len=3) :: flag
  end type ElementValue

  !> Creates a new ElementValue instance.
  interface new_ElementValue
    module procedure new_ElementValue
  end interface new_ElementValue

  !> Test equality of ElementValue instances.
  interface operator (.eq.)
    module procedure element_value_equals
    module procedure element_value_array_equals
  end interface

contains

  !> Creates a new ElementValue.
  !!
  !! @param value
  !! @param flag
  !! @return The ElementValue instance being created.
  function new_ElementValue(value, flag) result(this)
    type (ElementValue) :: this
    real :: value
    character(len=3) :: flag

    ! Set values for ElementValue
    this%value = value
    this%flag = flag

  end function new_ElementValue

  !> Returns a "null" ElementValue. This is handy for
  !! setting array values to "null" so that junk isn't
  !! accidentally stored in the array.
  !!
  !! @return A ElementValue with empty/zero values and
  !!         not_null set to false.
  function missing_ElementValue() result(this)
    type (ElementValue) :: this

    this = new_ElementValue(MISSING_REAL, "   ")

  end function missing_ElementValue

  !> Outputs a string representation of a ElementValue instance.
  !!
  !! @param this The ElementValue instance.
  !! @return The string representation of this ElementValue.
  function element_value_string(this) result(element_value_str)
    type(ElementValue) :: this
    character(len=145) :: element_value_str

    ! Create station string
    write(element_value_str, '(a,a,f7.2,a,a)') '[ElementValue]', &
          ' value: ',this%value, ' flag: ',this%flag

  end function element_value_string

  !> Tests the equality of two ElementValue instances.
  !!
  !! @param element_value1 The first ElementValue instance to compare.
  !! @param element_value2 The second ElementValue instance to compare.
  !! @return True if the Changepoint instances have equal values
  !!         for their attributes. Otherwise false.
  function element_value_equals(element_value1, element_value2) result(is_equal)
    type(ElementValue), intent(in) :: element_value1
    type(ElementValue), intent(in) :: element_value2
    logical :: is_equal

    if((element_value1%value .eq. element_value2%value) .and. &
       (element_value1%flag .eq. element_value2%flag)) then
      is_equal = .true.
    else
      is_equal = .false.
    endif  

  end function element_value_equals

  !> Tests the equality of two arrays of Changepoint instances.
  !! 
  !! @param element_value_array1 The first array of ElementValue instances
  !!                       to compare.
  !! @param element_value_array2 The second array of ElementValue instances
  !!                       to compare.
  !! @return True if the arrays are the same length and contain equal
  !!         ElementValue instances in the same order. Otherwise false.
  function element_value_array_equals(element_value_array1, element_value_array2) &
      result(is_equal)
    type(ElementValue), dimension(:), intent(in) :: element_value_array1
    type(ElementValue), dimension(:), intent(in) :: element_value_array2
    logical :: is_equal
    integer :: i

    if(size(element_value_array1) /= size(element_value_array2)) then
      is_equal = .false.
      return
    endif

    do i=1, size(element_value_array1)
      if(.not. element_value_array1(i) .eq. element_value_array2(i)) then
        is_equal = .false.
        return
      endif
    end do

    is_equal = .true.

  end function element_value_array_equals

end module ElementValueType

!> @file
!! The file containing the ElementValue type and any related
!! subroutines.
