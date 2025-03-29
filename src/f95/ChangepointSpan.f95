!> @brief
!! A type which represents information about an individual skyline date
!! between two changepoints.
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
module ChangepointSpanType

  use Logger
  use AlgorithmParameters

  implicit none

  private :: new_ChangepointSpan_int_convert, changepoint_span_equals, changepoint_span_array_equals,  &
             convert_span_number

  !> A type containing information
  !! about a single skyline date of a potential changepoint between
  !! a target and all it's neighbors.
  type ChangepointSpan
    integer(1) :: model_type
    integer(1) :: span_number
    logical(1) :: is_deleted
  end type ChangepointSpan

  !> Creates a new ChangepointSpan instance.
  interface new_ChangepointSpan
    module procedure new_ChangepointSpan
    module procedure new_ChangepointSpan_int_convert
  end interface new_ChangepointSpan

  !> Test equality of ChangepointSpan instances.
  interface operator (.eq.)
    module procedure changepoint_span_equals
    module procedure changepoint_span_array_equals
  end interface

contains

  !> Creates a new ChangepointSpan.
  !!
  !! @param model_type A model number indicating the shape of the changepoint
  !! @param span_number A number, like an ID, for this span
  !! @param is_deleted True if the values in this span are to be deleted
  !! @return The ChangepointSpan instance being created.
  function new_ChangepointSpan(model_type, span_number, is_deleted) result(this)
    type (ChangepointSpan) :: this
    integer(1) :: model_type
    integer(1) :: span_number
    logical(1) :: is_deleted

    ! Set values for ChangepointSpan
    this%model_type = model_type
    this%span_number = span_number
    this%is_deleted = is_deleted

  end function new_ChangepointSpan

  !> Creates a new ChangepointSpan. This constructor takes in values of the default
  !! integer type and converts them to integer(1).
  !!
  !! @param model_type A model number indicating the shape of the changepoint
  !! @param span_number A number, like an ID, for this span
  !! @param is_deleted True if the values in this span are to be deleted
  !! @return The ChangepointSpan instance being created.
  function new_ChangepointSpan_int_convert(model_type, span_number, is_deleted) result(this)
    type (ChangepointSpan) :: this
    integer :: model_type
    integer :: span_number
    logical(1) :: is_deleted

    ! This fixes a compiler issue with passing integers with unspecified size
    ! Fortran treats logicals as integers so that's an issue too
    integer(1) :: mt
    integer(1) :: sn
    mt = model_type
    sn = convert_span_number(span_number)

    ! Set values for ChangepointSpan
    this = new_ChangepointSpan(mt, sn, is_deleted)

  end function new_ChangepointSpan_int_convert

  !> Returns a "null" ChangepointSpan. This is handy for
  !! setting array values to "null" so that junk isn't
  !! accidentally stored in the array.
  !!
  !! @return A ChangepointSpan with empty/zero values and
  !!         not_null set to false.
  function null_ChangepointSpan() result(this)
    type (ChangepointSpan) :: this

    this = new_ChangepointSpan(ZERO_SIZE1, ZERO_SIZE1, FALSE)

  end function null_ChangepointSpan

  !> Converts the actual span_number value into an integer that can fit
  !! into type integer(1) which allows a range of -128 to +127.
  !! This confusing implementation has enormous memory savings because
  !! the array of ChangepointSpans is gigantic. Using integer(1)
  !! instead of integer(2) for span_number field cuts our program's memory
  !! usage almost in half. Because a span_number can, in rare cases, be
  !! greater than 127 (which is the max size for integer(1)), we have to
  !! accommodate for these slightly larger values. The actual span_number
  !! used is arbitrary and only has to be non-zero and different than the
  !! adjacent span_number.
  !!
  !! @param span_number The actual span_number value
  !! @return The stored value between -128 and 127 representing the actual span_number.
  function convert_span_number(span_number) result(span_number_stored)
    integer :: span_number
    integer(1) :: span_number_stored

    if(span_number > 99) then
      span_number_stored = mod(span_number, 100) + 1
    else
      span_number_stored = span_number
    end if

  end function convert_span_number

  !> Outputs a string representation of a ChangepointSpan instance.
  !!
  !! @param this The ChangepointSpan instance.
  !! @return The string representation of this ChangepointSpan.
  function changepoint_span_string(this) result(changepoint_span_str)
    type(ChangepointSpan) :: this
    character(len=145) :: changepoint_span_str

    ! Create station string
    write(changepoint_span_str, '(a,a,i2,a,i2,a,l1)') '[ChangepointSpan]', &
          ' model_type: ',this%model_type, ' span_number: ',this%span_number, ' is_deleted: ',this%is_deleted

  end function changepoint_span_string

  !> Tests the equality of two ChangepointSpan instances.
  !!
  !! @param changepoint_span1 The first ChangepointSpan instance to compare.
  !! @param changepoint_span2 The second ChangepointSpan instance to compare.
  !! @return True if the Changepoint instances have equal values
  !!         for their attributes. Otherwise false.
  function changepoint_span_equals(changepoint_span1, changepoint_span2) result(is_equal)
    type(ChangepointSpan), intent(in) :: changepoint_span1
    type(ChangepointSpan), intent(in) :: changepoint_span2
    logical :: is_equal

    if((changepoint_span1%model_type .eq. changepoint_span2%model_type) .and. &
       (changepoint_span1%span_number .eq. changepoint_span2%span_number) .and. &
       (changepoint_span1%is_deleted .eqv. changepoint_span2%is_deleted)) then
      is_equal = .true.
    else
      is_equal = .false.
    endif  

  end function changepoint_span_equals

  !> Tests the equality of two arrays of Changepoint instances.
  !! 
  !! @param changepoint_span_array1 The first array of ChangepointSpan instances
  !!                       to compare.
  !! @param changepoint_span_array2 The second array of ChangepointSpan instances
  !!                       to compare.
  !! @return True if the arrays are the same length and contain equal
  !!         ChangepointSpan instances in the same order. Otherwise false.
  function changepoint_span_array_equals(changepoint_span_array1, changepoint_span_array2) &
      result(is_equal)
    type(ChangepointSpan), dimension(:), intent(in) :: changepoint_span_array1
    type(ChangepointSpan), dimension(:), intent(in) :: changepoint_span_array2
    logical :: is_equal
    integer :: i

    if(size(changepoint_span_array1) /= size(changepoint_span_array2)) then
      is_equal = .false.
      return
    endif

    do i=1, size(changepoint_span_array1)
      if(.not. changepoint_span_array1(i) .eq. changepoint_span_array2(i)) then
        is_equal = .false.
        return
      endif
    end do

    is_equal = .true.

  end function changepoint_span_array_equals

end module ChangepointSpanType

!> @file
!! The file containing the ChangepointSpan type and any related
!! subroutines.
