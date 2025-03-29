!> @brief
!! A type which represents a target station and one of its neighbor stations.
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
module StationNeighborPairType

  use AlgorithmParameters

  implicit none

  private :: new_StationNeighborPair_Corr, station_neighbor_equals, station_neighbor_array_equals

  !> A type which holds information about a target station and
  !! a neighbor station.
  type StationNeighborPair
    character(len=11) :: target_id
    character(len=11) :: neighbor_id
    integer :: target_index
    integer :: neighbor_index
    real :: distance
    real :: correlation
    integer :: begin_month_index
    integer :: end_month_index
    integer :: neigh2targ_index
  end type StationNeighborPair

  !> Creates a new StationNeighborPair instance.
  interface new_StationNeighborPair
    module procedure new_StationNeighborPair
    module procedure new_StationNeighborPair_Corr
  end interface new_StationNeighborPair

  interface operator (.eq.)
    module procedure station_neighbor_equals
    module procedure station_neighbor_array_equals
  end interface

  !> A constant representing a null station ID value.
  character(len=11), parameter :: EMPTY_STATION_ID = "           "
 
contains

  !> Default constructor for a StationNeighborPair.
  !!
  !! @param target_id The station ID for the target station.
  !! @param neighbor_id The station ID for the neighbor station.
  !! @param target_index The index for the target station.
  !! @param neighbor_index The index for the neighbor station.
  !! @param distance The distance between the target and neighbor.
  !! @return The StationNeighborPair being created.
  function new_StationNeighborPair(target_id, neighbor_id, &
               target_index, neighbor_index, distance) result(this)
    type (StationNeighborPair) :: this
    character(len=11) :: target_id
    character(len=11) :: neighbor_id
    integer :: target_index
    integer :: neighbor_index
    real :: distance
    real :: correlation

    correlation = -1.0
    this = new_StationNeighborPair_Corr(target_id, neighbor_id,  &
          target_index, neighbor_index, distance, correlation)
  end function new_StationNeighborPair

  !> Constructor for a StationNeighborPair that also takes a
  !! correlation value.
  !!
  !! @param target_id The station ID for the target station.
  !! @param neighbor_id The station ID for the neighbor station.
  !! @param target_index The index for the target station.
  !! @param neighbor_index The index for the neighbor station.
  !! @param distance The distance between the target and neighbor.
  !! @param correlation The correlation between the target and
  !!                    neighbor data (most recent year).
  !! @return The StationNeighborPair being created.
  function new_StationNeighborPair_Corr(target_id, neighbor_id, &
               target_index, neighbor_index, distance, correlation) result(this)
    type (StationNeighborPair) :: this
    character(len=11) :: target_id
    character(len=11) :: neighbor_id
    integer :: target_index
    integer :: neighbor_index
    real :: distance
    real :: correlation

    this%target_id = target_id
    this%neighbor_id = neighbor_id
    this%target_index = target_index
    this%neighbor_index = neighbor_index
    this%distance = distance
    this%correlation = correlation
    this%begin_month_index = 0 ! Gets set after initialization
    this%end_month_index = 0 ! Gets set after initialization
    this%neigh2targ_index = -1

  end function new_StationNeighborPair_Corr

  !> Returns a "null" StationNeighborPair. This is handy for 
  !! setting array values to "null" so that junk isn't 
  !! accidentally stored in the array.
  !!
  !! @return A StationNeighborPair with empty/zero values and 
  !!         not_null set to false.
  function null_StationNeighborPair() result(this)
    type (StationNeighborPair) :: this

    this = new_StationNeighborPair_Corr(EMPTY_STATION_ID, EMPTY_STATION_ID, ZERO, ZERO, ZERO_REAL, ZERO_REAL)

  end function null_StationNeighborPair

  !> Outputs a string representation of a StationNeighborPair
  !!  instance.
  !!
  !! @param this The StationNeighborPair instance.
  !! @return The string representation of this StationNeighborPair.
  function station_neighbor_string(this) result(pair_str)
    type(StationNeighborPair) :: this
    character(len=150) :: pair_str

    ! If the station object is null, skip the rest
    if(this%target_index == 0) then
      pair_str = "[StationNeighborPair] NULL"
      return
    endif

    ! Create station string
    write(pair_str, '(a,a,a,a,a,i5,a,i5,a,f7.2,a,f5.2)') '[StationNeighborPair] target id: ', &
          this%target_id, ' neighbor id: ', this%neighbor_id, ' target index: ',    & 
          this%target_index, ' neighbor_index: ', this%neighbor_index,  &
          ' distance: ', this%distance, ' correlation: ', this%correlation

  end function station_neighbor_string

  !> Tests two StationNeighborPair instances for equality.
  !!
  !! @param[in] pair1 The first StationNeighborPair to test.
  !! @param[in] pair2 The second StationNeighborPair to test.
  !! @return True if the attributes of the instances are the same.
  !!         Otherwise false.
  function station_neighbor_equals(pair1, pair2) result(is_equal)
    type(StationNeighborPair), intent(in) :: pair1
    type(StationNeighborPair), intent(in) :: pair2
    logical :: is_equal

    if ((pair1%target_id .eq. pair2%target_id) .and. &
        (pair1%neighbor_id .eq. pair2%neighbor_id) .and. &
        (pair1%target_index .eq. pair2%target_index) .and. &
        (pair1%neighbor_index .eq. pair2%neighbor_index) .and. &
        (pair1%distance .eq. pair2%distance) .and. &
        (pair1%correlation .eq. pair2%correlation)) then
      is_equal = .true.
    else
      is_equal = .false.
    endif

  end function station_neighbor_equals

  !> Tests two arrays of StationNeighborPair instances for equality.
  !!
  !! @param[in] pair_array1 The first array of StationNeighborPair
  !!                        instances to test. 
  !! @param[in] pair_array2 The second array of StationNeighborPair
  !!                        instances to test.
  !! @return True if the arrays are the same size and contain items
  !!         that are equal in the same order. Otherwise false.
  function station_neighbor_array_equals(pair_array1, pair_array2) &
      result(is_equal)
    type(StationNeighborPair), dimension(:), intent(in) :: pair_array1
    type(StationNeighborPair), dimension(:), intent(in) :: pair_array2
    logical :: is_equal
    integer :: i

    if(size(pair_array1) /= size(pair_array2)) then
      is_equal = .false.
      return
    endif

    do i=1, size(pair_array1)
      if(.not. pair_array1(i) .eq. pair_array2(i)) then
        is_equal = .false.
        return
      endif
    end do

    is_equal = .true.
    
  end function station_neighbor_array_equals

end module StationNeighborPairType

!> @file
!! The file containing the StationNeighborPair type and any related
!! subroutines.
