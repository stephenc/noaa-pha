!> @brief Unit tests for Station type.
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
module StationTypeTest

  use StationType
  use UnitTest
  use Logger

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine station_tests

    call test_station_valid_latitude()
    call test_station_valid_longitude()
    call test_is_hcn_record()
    call test_station_string()
    call test_station_eq()

  end subroutine station_tests

  !> Tests the station_valid_latitude function.
  subroutine test_station_valid_latitude()
 
    ! Valid latitudes
    call assert_true(station_valid_latitude(89.9), "Valid latitude is just below upper limit")
    call assert_true(station_valid_latitude(0.0), "Valid latitude of 0")
    call assert_true(station_valid_latitude(90.0), "Valid latitude is exactly at upper limit")
    call assert_true(station_valid_latitude(-0.00), "Valid latitude of negative 0")
    call assert_true(station_valid_latitude(-89.99), "Valid latitude is just above lower limit")
    call assert_true(station_valid_latitude(-90.0), "Valid latitude is exactly at lower limit")

    ! Invalid latitudes
    call assert_false(station_valid_latitude(90.01), "Invalid latitude is just above upper limit") 
    call assert_false(station_valid_latitude(-90.01), "Invalid latitude is just below lower limit")
    call assert_false(station_valid_latitude(178.0), "Invalid latitude is within longitude upper limit")
    call assert_false(station_valid_latitude(-178.0), "Invalid latitude is within longitude lower limit") 

  end subroutine test_station_valid_latitude

  !> Tests the station_valid_longitude function.
  subroutine test_station_valid_longitude()

    ! Valid longitudes
    call assert_true(station_valid_longitude(90.5), "Valid longitude is just above latitude upper limit")
    call assert_true(station_valid_longitude(-90.5), "Valid longitude is just below latitude lower limit")
    call assert_true(station_valid_longitude(0.0), "Valid longitude is 0")
    call assert_true(station_valid_longitude(-0.0), "Valid longitude is negative 0")
    call assert_true(station_valid_longitude(360.0), "Valid longitude is exactly at upper limit")
    call assert_true(station_valid_longitude(-180.0), "Valid longitude is exactly at lower limit")

    ! Invalid longitudes
    call assert_false(station_valid_longitude(360.01), "Invalid longitude is just above upper limit")
    call assert_false(station_valid_longitude(-180.01), "Invalid longitude is just below lower limit")
  
  end subroutine test_station_valid_longitude

  !> Tests the is_hcn_record function, which determines whether a record
  !! that begins with a station ID is associated with a USHCN station.
  subroutine test_is_hcn_record()
    character(len=110) :: record

    record = "USH00018323  31.8075  -85.9722  165.2 AL TROY  USC00018323 ----------- ----------- ----------- +6"
    call assert_true(is_hcn_record(record), "Expected to be USHCN")

    record = "USC00018380  33.2119  -87.6161   51.5 AL TUSCALOOSA ACFD USC00018380 USC00018385 USC00018454 ----------- +6"
    call assert_false(is_hcn_record(record), "Expected to be coop - not USHCN")

    record = "10160475000  35.4800    8.1300  813.0 TEBESSA                         818U   67MVxxno-9A 2WARM FOR./FIELD B"
    call assert_false(is_hcn_record(record), "Expected to be global - not USHCN")

  end subroutine test_is_hcn_record

  !> Tests the station_string function.
  subroutine test_station_string()
  
    type(Station) :: this_station
    character(len=145) :: expected_string
    character(len=11), dimension(:) :: composite_ids(4)

    this_station = null_Station()
    expected_string = "[Station] NULL"
    call assert_equals(expected_string, station_string(this_station), "NULL Station")
    ! Test case without composite ids
    this_station = new_Station("MyIdentity1", 1, 23.23, 340.23)
    expected_string = "[Station] id: MyIdentity1 index:     1 lat:  23.23 lon:  340.23"  
    call assert_equals(expected_string, station_string(this_station), "Station without composite ids.")

    ! Test case with composite ids
    expected_string = "[Station] id: MyIdentity2 index:     2 lat:  23.23 lon:  340.23"&
                    //" composite ids: USC00123456 USH00543210 USW00333444 VQS00999000"
    composite_ids(1) = "USC00123456"
    composite_ids(2) = "USH00543210"
    composite_ids(3) = "USW00333444"
    composite_ids(4) = "VQS00999000"
    this_station = new_Station("MyIdentity2", 2, 23.23, 340.23, composite_ids)
    call assert_equals(expected_string, station_string(this_station), "Station with composite ids.")

  end subroutine test_station_string

    !> Tests the overloaded .eq. operator
  subroutine test_station_eq()
    type(Station) :: null_station1
    type(Station) :: null_station2
    type(Station) :: station1a
    type(Station) :: station1b
    type(Station) :: station2a
    type(Station) :: station2b
    type(Station), dimension(:) :: station_array1a(3)
    type(Station), dimension(:) :: station_array1b(3)
    type(Station), dimension(:) :: station_array2(3)
    type(Station), dimension(:) :: station_array3(4)

    ! Define Stations to test against
    null_station1 = null_Station()
    null_station2 = null_Station()
    station1a = new_Station("MyIdentity1", 1, 23.23, 340.23)
    station1b = new_Station("MyIdentity1", 1, 23.23, 340.23)
    station2a = new_Station("MyIdentity2", 2, 90.0, 0.0)
    station2b = new_Station("MyIdentity2", 2, 90.0, 0.0)

    ! Run tests on individual items
    call assert_true(null_station1 .eq. null_station2, "eq for null Station")
    call assert_true(station1a .eq. station1b, "eq for identical Stations")
    call assert_false(station1a .eq. station2a, "eq for differing Stations")

    ! Set up test arrays
    station_array1a = (/ null_station1, station1a, station2a /)
    station_array1b = (/ null_station2, station1b, station2b /)
    station_array2 = (/ station1a, station2a, null_station1 /)
    station_array3 = (/ station1a, station2a, null_station1, null_station2 /)

    ! Run tests on arrays
    call assert_true(station_array1a .eq. station_array1b, "eq for arrays with identical items")
    call assert_true(station_array1b .eq. station_array1a, "eq for arrays with identical items (swapped)")
    call assert_false(station_array1a .eq. station_array2, "eq for arrays with same length but diff items")
    call assert_false(station_array2 .eq. station_array1a, "eq for arrays with same length but diff items (swapped)")
    call assert_false(station_array2 .eq. station_array3, "eq for arrays with different lengths")
    call assert_false(station_array3 .eq. station_array2, "eq for arrays with different lengths (swapped)")

  end subroutine test_station_eq

end module StationTypeTest

