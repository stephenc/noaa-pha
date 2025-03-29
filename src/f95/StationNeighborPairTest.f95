!> @brief Unit tests for StationNeighborPair type.
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
module StationNeighborPairTypeTest

  use StationNeighborPairType
  use UnitTest
  use Logger

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine station_neighbor_pair_tests

    call test_null_StationNeighborPair()
    call test_station_neighbor_string()
    call test_station_neighbor_eq()

  end subroutine station_neighbor_pair_tests

  subroutine test_null_StationNeighborPair()
    type(StationNeighborPair) :: this
    
    this = null_StationNeighborPair()
    call assert_equals(EMPTY_STATION_ID, this%target_id, "Null target_id value")
    call assert_equals(EMPTY_STATION_ID, this%neighbor_id, "Null neighbor_id value")
    call assert_equals(0, this%target_index, "Null target_index value")
    call assert_equals(0, this%neighbor_index, "Null neighbor_index value")
    call assert_equals_within_tolerance(0.0, this%distance, .01, "Null distance value")
    call assert_equals_within_tolerance(0.0, this%correlation, .01, "Null correlation value")

  end subroutine test_null_StationNeighborPair

  !> Tests the station_string function.
  subroutine test_station_neighbor_string()
    type(StationNeighborPair) :: this_pair
    character(len=150) :: expected_string
    character(len=6), dimension(:) :: composite_ids(4)

    this_pair = null_StationNeighborPair()
    expected_string = "[StationNeighborPair] NULL"
    call assert_equals(expected_string, station_neighbor_string(this_pair), "NULL StationNeighborPair")

    ! Test case with correlation
    expected_string = "[StationNeighborPair] target id: MyIdentity1 neighbor id: MyIdentity2 " &
           //"target index:     1 neighbor_index:     2 distance:   23.23 correlation: -1.00"
    this_pair = new_StationNeighborPair("MyIdentity1", "MyIdentity2", 1, 2, 23.23)
    call assert_equals(expected_string, station_neighbor_string(this_pair), "Pair with distance only.")

    ! Test case with correlation
    expected_string = "[StationNeighborPair] target id: MyIdentity1 neighbor id: MyIdentity2 " &
           //"target index:     1 neighbor_index:     2 distance:   23.23 correlation:  0.23"
    this_pair = new_StationNeighborPair("MyIdentity1", "MyIdentity2", 1, 2, 23.23, 0.23)
    call assert_equals(expected_string, station_neighbor_string(this_pair), "Pair with correlation.")

  end subroutine test_station_neighbor_string

  !> Tests the overloaded .eq. operator
  subroutine test_station_neighbor_eq()
    type(StationNeighborPair) :: null_pair1
    type(StationNeighborPair) :: null_pair2
    type(StationNeighborPair) :: pair1a
    type(StationNeighborPair) :: pair1b
    type(StationNeighborPair) :: pair2a
    type(StationNeighborPair) :: pair2b
    type(StationNeighborPair), dimension(:) :: pair_array1a(3)
    type(StationNeighborPair), dimension(:) :: pair_array1b(3)  
    type(StationNeighborPair), dimension(:) :: pair_array2(3) 
    type(StationNeighborPair), dimension(:) :: pair_array3(4)

    ! Define StationNeighborPairs to test against 
    null_pair1 = null_StationNeighborPair()
    null_pair2 = null_StationNeighborPair()
    pair1a = new_StationNeighborPair("MyIdentity1", "MyIdentity2", 1, 2, 23.23)
    pair1b = new_StationNeighborPair("MyIdentity1", "MyIdentity2", 1, 2, 23.23)
    pair2a = new_StationNeighborPair("MyIdentity2", "MyIdentity1", 2, 1, 0.0)
    pair2b = new_StationNeighborPair("MyIdentity2", "MyIdentity1", 2, 1, 0.0)

    ! Run tests on individual items
    call assert_true(null_pair1 .eq. null_pair2, "eq for null StationNeighborPair")
    call assert_true(pair1a .eq. pair1b, "eq for identical StationNeighbor Pairs")
    call assert_false(pair1a .eq. pair2a, "eq for differing StationNeighbor Pairs")

    ! Set up test arrays
    pair_array1a = (/ null_pair1, pair1a, pair2a /)
    pair_array1b = (/ null_pair2, pair1b, pair2b /)
    pair_array2 = (/ pair1a, pair2a, null_pair1 /)
    pair_array3 = (/ pair1a, pair2a, null_pair1, null_pair2 /)

    ! Run tests on arrays
    call assert_true(pair_array1a .eq. pair_array1b, "eq for arrays with identical items")
    call assert_true(pair_array1b .eq. pair_array1a, "eq for arrays with identical items (swapped)")
    call assert_false(pair_array1a .eq. pair_array2, "eq for arrays with same length but diff items")
    call assert_false(pair_array2 .eq. pair_array1a, "eq for arrays with same length but diff items (swapped)")
    call assert_false(pair_array2 .eq. pair_array3, "eq for arrays with different lengths")
    call assert_false(pair_array3 .eq. pair_array2, "eq for arrays with different lengths (swapped)")

  end subroutine test_station_neighbor_eq

end module StationNeighborPairTypeTest

