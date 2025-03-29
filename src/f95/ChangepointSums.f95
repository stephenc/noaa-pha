!> @brief
!! A type which represents a single skyline date of a potential changepoint
!! between a target and all it's neighbors.
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
module ChangepointSumsType

  use Logger
  use AlgorithmParameters

  implicit none

  private :: new_ChangepointSums_int_convert, changepoint_sums_equals, changepoint_sums_array_equals

  !> A type containing information
  !! about a single skyline date of a potential changepoint between
  !! a target and all it's neighbors.
  type ChangepointSums
    real :: offset_sum
    real :: zscore_sum
    integer(1) :: sum_count
  end type ChangepointSums

  !> Creates a new ChangepointSums instance.
  interface new_ChangepointSums
    module procedure new_ChangepointSums
    module procedure new_ChangepointSums_int_convert
  end interface new_ChangepointSums

  !> Test equality of ChangepointSums instances.
  interface operator (.eq.)
    module procedure changepoint_sums_equals
    module procedure changepoint_sums_array_equals
  end interface

contains

  !> Creates a new ChangepointSums.
  !!
  !! @param offset_sum The sum of changepoint offsets for all neighbors for the month
  !! @param zscore_sum The sum of changepoint zscores for all neighbors for the month
  !! @param sum_count The number of changepoints for all neighbors for the month
  !! @return The ChangepointSums instance being created.
  function new_ChangepointSums(offset_sum, zscore_sum, sum_count) result(this)
    type (ChangepointSums) :: this
    real :: offset_sum
    real :: zscore_sum
    integer(1) :: sum_count

    ! Set values for ChangepointSums
    this%offset_sum = offset_sum
    this%zscore_sum = zscore_sum
    this%sum_count = sum_count

  end function new_ChangepointSums

  !> Creates a new ChangepointSums.
  !!
  !! @param offset_sum The sum of changepoint offsets for all neighbors for the month
  !! @param zscore_sum The sum of changepoint zscores for all neighbors for the month
  !! @param sum_count The number of changepoints for all neighbors for the month
  !! @return The ChangepointSums instance being created.
  function new_ChangepointSums_int_convert(offset_sum, zscore_sum, sum_count) result(this)
    type (ChangepointSums) :: this
    real :: offset_sum
    real :: zscore_sum
    integer :: sum_count

    integer(1) :: sc
    sc = sum_count

    ! Set values for ChangepointSums
    this%offset_sum = offset_sum
    this%zscore_sum = zscore_sum
    this%sum_count = sc

  end function new_ChangepointSums_int_convert

  !> Returns a "null" ChangepointSums. This is handy for
  !! setting array values to "null" so that junk isn't
  !! accidentally stored in the array.
  !!
  !! @return A ChangepointSums with empty/zero values and
  !!         not_null set to false.
  function null_ChangepointSums() result(this)
    type (ChangepointSums) :: this

    this = new_ChangepointSums(ZERO_REAL, ZERO_REAL, ZERO_SIZE1)

  end function null_ChangepointSums

  !> Outputs a string representation of a ChangepointSums instance.
  !!
  !! @param this The ChangepointSums instance.
  !! @return The string representation of this ChangepointSums.
  function changepoint_sums_string(this) result(changepoint_sums_str)
    type(ChangepointSums) :: this
    character(len=145) :: changepoint_sums_str

    ! Create station string
    write(changepoint_sums_str, '(a,a,f9.2,a,f7.2,a,i5)') '[ChangepointSums]', &
          ' offset_sum: ',this%offset_sum, ' zscore_sum: ',this%zscore_sum, ' sum_count: ',this%sum_count

  end function changepoint_sums_string

  !> Tests the equality of two ChangepointSums instances.
  !!
  !! @param changepoint_sums1 The first ChangepointSums instance to compare.
  !! @param changepoint_sums2 The second ChangepointSums instance to compare.
  !! @return True if the Changepoint instances have equal values
  !!         for their attributes. Otherwise false.
  function changepoint_sums_equals(changepoint_sums1, changepoint_sums2) result(is_equal)
    type(ChangepointSums), intent(in) :: changepoint_sums1
    type(ChangepointSums), intent(in) :: changepoint_sums2
    logical :: is_equal

    if((changepoint_sums1%offset_sum .eq. changepoint_sums2%offset_sum) .and. &
       (changepoint_sums1%zscore_sum .eq. changepoint_sums2%zscore_sum) .and. &
       (changepoint_sums1%sum_count .eq. changepoint_sums2%sum_count)) then
      is_equal = .true.
    else
      is_equal = .false.
    endif  

  end function changepoint_sums_equals

  !> Tests the equality of two arrays of Changepoint instances.
  !! 
  !! @param changepoint_sums_array1 The first array of ChangepointSums instances
  !!                       to compare.
  !! @param changepoint_sums_array2 The second array of ChangepointSums instances
  !!                       to compare.
  !! @return True if the arrays are the same length and contain equal
  !!         ChangepointSums instances in the same order. Otherwise false.
  function changepoint_sums_array_equals(changepoint_sums_array1, changepoint_sums_array2) &
      result(is_equal)
    type(ChangepointSums), dimension(:), intent(in) :: changepoint_sums_array1
    type(ChangepointSums), dimension(:), intent(in) :: changepoint_sums_array2
    logical :: is_equal
    integer :: i

    if(size(changepoint_sums_array1) /= size(changepoint_sums_array2)) then
      is_equal = .false.
      return
    endif

    do i=1, size(changepoint_sums_array1)
      if(.not. changepoint_sums_array1(i) .eq. changepoint_sums_array2(i)) then
        is_equal = .false.
        return
      endif
    end do

    is_equal = .true.

  end function changepoint_sums_array_equals

end module ChangepointSumsType

!> @file
!! The file containing the ChangepointSums type and any related
!! subroutines.
