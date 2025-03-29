!> @brief
!! A type which represents a documented changepoint
!! (such as a record from a history file) or undocumented changepoint
!! (found by the algorithm).
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
module ChangepointType

  use Logger
  use AlgorithmParameters

  implicit none

  private :: new_Changepoint_convert_int, changepoint_equals, changepoint_array_equals

  !> A type containing information about a documented changepoint
  !! at a station.
  type Changepoint
    integer :: sky_month
    integer(1) :: day_of_month
    real :: amplitude
  end type Changepoint

  !> Creates a new Changepoint instance.
  interface new_Changepoint
    module procedure new_Changepoint
    module procedure new_Changepoint_convert_int
  end interface new_Changepoint

  !> Test equality of Changepoint instances.
  interface operator (.eq.)
    module procedure changepoint_equals
    module procedure changepoint_array_equals
  end interface

contains

  !> Creates a new changepoint.
  !!
  !! @param sky_month The skyline month on which this changepoint occurred.
  !! @param day_of_month The day of the month on which this changepoint occurred (1-31).
  !! @param amplitude The amplitude of this documented changepoint.
  !! @return The Changepoint instance being created.
  function new_Changepoint(sky_month, day_of_month, amplitude) &
               result(this)
    type(Changepoint) :: this
    integer :: sky_month
    integer(1) :: day_of_month
    real :: amplitude

    ! Set values for Station
    this%sky_month = sky_month
    this%day_of_month = day_of_month
    this%amplitude = amplitude

  end function new_Changepoint

  !> Creates a new changepoint.
  !!
  !! @param sky_month The skyline month on which this changepoint occurred.
  !! @param day_of_month The day of the month on which this changepoint occurred (1-31).
  !! @param amplitude The amplitude of this documented changepoint.
  !! @return The Changepoint instance being created.
  function new_Changepoint_convert_int(sky_month, day_of_month, amplitude) &
               result(this)
    type(Changepoint) :: this
    integer :: sky_month
    integer :: day_of_month
    real :: amplitude

    integer(1) :: dom
    dom = day_of_month

    this = new_Changepoint(sky_month, dom, amplitude)

  end function new_Changepoint_convert_int

  !> Returns a "null" Changepoint. This is handy for
  !! setting array values to "null" so that junk isn't
  !! accidentally stored in the array.
  !!
  !! @return A Changepoint with empty/zero values and
  !!         not_null set to false.
  function null_Changepoint() result(this)
    type(Changepoint) :: this

    this = new_Changepoint(ZERO, ZERO_SIZE1, ZERO_REAL)

  end function null_Changepoint

  !> Outputs a string representation of a Changepoint instance.
  !!
  !! @param this The Changepoint instance.
  !! @return The string representation of this Changepoint.
  function changepoint_string(this) result(changepoint_str)
    type(Changepoint) :: this
    character(len=145) :: changepoint_str

    ! If the station object is null, skip the rest
    if(changepoint_is_null(this)) then
      changepoint_str = "[Changepoint] NULL"
      return
    endif

    ! Create changepoint string
    write(changepoint_str, '(a,i7,a,i5,a,f7.2)') '[Changepoint] sky month: ', &
          this%sky_month, ' day of month: ',this%day_of_month, ' amplitude: ',this%amplitude

  end function changepoint_string

  !> Tests the equality of two Changepoint instances.
  !!
  !! @param changepoint1 The first Changepoint instance to compare.
  !! @param changepoint2 The second Changepoint instance to compare.
  !! @return True if the Changepoint instances have equal values
  !!         for their attributes. Otherwise false.
  function changepoint_equals(changepoint1, changepoint2) result(is_equal)
    type(Changepoint), intent(in) :: changepoint1
    type(Changepoint), intent(in) :: changepoint2
    logical :: is_equal

    if((changepoint1%sky_month .eq. changepoint2%sky_month) .and. &
       (changepoint1%day_of_month .eq. changepoint2%day_of_month).and. &
       (changepoint1%amplitude .eq. changepoint2%amplitude)) then
      is_equal = .true.
    else
      is_equal = .false.
    endif  

  end function changepoint_equals

  !> Tests the equality of two arrays of Changepoint instances.
  !! 
  !! @param changepoint_array1 The first array of Changepoint instances
  !!                       to compare.
  !! @param changepoint_array2 The second array of Changepoint instances
  !!                       to compare.
  !! @return True if the arrays are the same length and contain equal
  !!         Station instances in the same order. Otherwise false.
  function changepoint_array_equals(changepoint_array1, changepoint_array2) &
      result(is_equal)
    type(Changepoint), dimension(:), intent(in) :: changepoint_array1
    type(Changepoint), dimension(:), intent(in) :: changepoint_array2
    logical :: is_equal
    integer :: i

    if(size(changepoint_array1) /= size(changepoint_array2)) then
      is_equal = .false.
      return
    endif

    do i=1, size(changepoint_array1)
      if(.not. changepoint_array1(i) .eq. changepoint_array2(i)) then
        is_equal = .false.
        return
      endif
    end do

    is_equal = .true.

  end function changepoint_array_equals

  !> Determine whether this Changepoint instance only contains
  !! the default initialization values, indicating it is "null".
  !!
  !! @param this The Changepoint to analyze.
  !! @result Returns true if this changepoint is "null". Otherwise, false.
  function changepoint_is_null(this) result(is_null)
    type(Changepoint) :: this
    logical :: is_null

    is_null = (this .eq. null_Changepoint())

  end function changepoint_is_null

end module ChangepointType

!> @file
!! The file containing the Changepoint type and any related
!! subroutines.
