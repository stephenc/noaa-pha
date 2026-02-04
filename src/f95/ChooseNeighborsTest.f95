!> @brief The module for running unit tests against the ChooseNeighbors
!! module subroutines.
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
module ChooseNeighborsTest

  use ChooseNeighbors
  use StationType
  use PropertyReader
  use UnitTest
  use AlgorithmParameters

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine choose_neighbors_tests

    call test_read_station_metadata()
    call test_is_hcn_record()
    call test_calculate_distance()
    call test_is_same_station()
    call test_sort_station_distances()
    call test_write_distance_output()
    call test_calculate_correlation()
    call test_calculate_first_diffs()
    call test_sort_station_correlations()
    call test_get_input_data_filepath()
    call test_is_missing_real()
    call test_write_correlation_output()

  end subroutine choose_neighbors_tests

  !> Tests the read_station_metadata subroutine to ensure the
  !! expected stations are retrieved from the input file.
  subroutine test_read_station_metadata()
    character(len=11), dimension(4) :: composite_ids1
    character(len=11), dimension(4) :: composite_ids2
    type(Station) :: station1
    type(Station) :: station2
    type(Station) :: station3
    type(Station) :: station4
    type(Station) :: station5
    type(Station) :: station6
    type(Station) :: station7
    type(Station), dimension(:), allocatable :: read_stations
    type(Station), dimension(7) :: expected_stations

    composite_ids1 = (/ "USC00018323", EMPTY_COMPOSITE_ID, EMPTY_COMPOSITE_ID, EMPTY_COMPOSITE_ID /)
    composite_ids2 = (/ "USC00018380", "USC00018385", "USC00018454", EMPTY_COMPOSITE_ID /)
    station1 = new_Station("USH00018323", 1, 31.8075, -85.9722, composite_ids1)
    station2 = new_Station("USH00018380", 2, 33.2119, -87.6161, composite_ids2)
    station3 = new_Station("10160475000", 3, 35.4800, 8.1300)
    station4 = new_Station("10160490000", 4, 35.6000, -0.6000)
    station5 = new_Station("10160518000", 5, 35.3000, -1.3500)
    station6 = new_Station("10160522000", 6, 34.8200, -1.7800)
    station7 = new_Station("10160525000", 7, 34.8000, 5.7300)
    expected_stations = (/station1, station2, station3, station4, station5, &
                          station6, station7 /)

    call read_station_metadata("./build/test-station-meta-v3.txt", read_stations)
    call assert_true(read_stations .eq. expected_stations, "Station metadata doesn't match expected.")

  end subroutine test_read_station_metadata

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


  !> Tests the get_neighbors_by_distance function against a very small set
  !! of test stations.
  subroutine test_get_neighbors_by_distance()
    integer :: neighbor_limit
    type(StationNeighborPair), dimension(:,:), allocatable :: expected_neighbors
    type(StationNeighborPair), dimension(:,:), allocatable :: station_neighbors

    character(len=11), dimension(4) :: composite_ids1
    character(len=11), dimension(4) :: composite_ids2
    type(Station) :: station1
    type(Station) :: station2
    type(Station) :: station3
    type(Station) :: station4
    type(Station) :: station5
    type(Station) :: station6
    type(Station) :: station7
    type(Station), dimension(:), allocatable :: test_stations
    allocate(test_stations(7))

    composite_ids1 = (/ "USC00018323", EMPTY_COMPOSITE_ID, EMPTY_COMPOSITE_ID, EMPTY_COMPOSITE_ID /)
    composite_ids2 = (/ "USC00018380", "USC00018385", "USC00018454", EMPTY_COMPOSITE_ID /)
    station1 = new_Station("USH00018323", 1, 31.8075, -85.9722, composite_ids1)
    station2 = new_Station("USH00018380", 2, 33.2119, -87.6161, composite_ids2)
    station3 = new_Station("10160475000", 3, 35.4800, 8.1300)
    station4 = new_Station("10160490000", 4, 35.6000, -0.6000)
    station5 = new_Station("10160518000", 5, 35.3000, -1.3500)
    station6 = new_Station("10160522000", 6, 34.8200, -1.7800)
    station7 = new_Station("10160525000", 7, 34.8000, 5.7300)
    test_stations = (/station1, station2, station3, station4, station5, &
                          station6, station7 /)

    neighbor_limit = 5
    allocate(expected_neighbors(size(test_stations), neighbor_limit))
    allocate(station_neighbors(size(test_stations), neighbor_limit))
!    station_neighbors = get_neighbors_by_distance(test_stations, neighbor_limit)
!    call assert_true((expected_neighbors .eq. station_neighbors), &
!            "Odd number of distance neighbors")
    deallocate(expected_neighbors)
    deallocate(station_neighbors)

    neighbor_limit = 6
    allocate(expected_neighbors(size(test_stations), neighbor_limit))

  end subroutine test_get_neighbors_by_distance

  !> Tests the get_neighbors_for_station function against a very small set
  !! of test stations.
  subroutine test_get_neighbors_for_station()
    integer :: distance_neighbor_limit
    integer :: neighbor_count

    character(len=11), dimension(4) :: composite_ids1
    character(len=11), dimension(4) :: composite_ids2
    type(Station) :: station1
    type(Station) :: station2
    type(Station) :: station3
    type(Station) :: station4
    type(Station) :: station5
    type(Station) :: station6
    type(Station) :: station7
    type(Station), dimension(7) :: test_stations
    type(StationNeighborPair), dimension(:) :: actual_neighbors(size(test_stations))
    type(StationNeighborPair), dimension(:) :: expected_neighbors(size(test_stations))

    composite_ids1 = (/ "USC00018323", EMPTY_COMPOSITE_ID, EMPTY_COMPOSITE_ID, EMPTY_COMPOSITE_ID /)
    composite_ids2 = (/ "USC00018380", "USC00018385", "USC00018454", EMPTY_COMPOSITE_ID /)
    station1 = new_Station("USH00018323", 1, 31.8075, -85.9722, composite_ids1)
    station2 = new_Station("USH00018380", 2, 33.2119, -87.6161, composite_ids2)
    station3 = new_Station("10160475000", 3, 35.4800, 8.1300)
    station4 = new_Station("10160490000", 4, 35.6000, -0.6000)
    station5 = new_Station("10160518000", 5, 35.3000, -1.3500)
    station6 = new_Station("10160522000", 6, 34.8200, -1.7800)
    station7 = new_Station("10160525000", 7, 34.8000, 5.7300)
    test_stations = (/station1, station2, station3, station4, station5, &
                          station6, station7 /)

    neighbor_count = 7
    distance_neighbor_limit = 5

    actual_neighbors = &
      get_neighbors_for_station(station3, test_stations, neighbor_count, distance_neighbor_limit)

    ! Set up expected StationNeighborPairs
    expected_neighbors(1) = &
        new_StationNeighborPair(station3%station_id, station4%station_id,  &
                                station3%station_index, station4%station_index, &
                                790.01, -1.00)
    expected_neighbors(2) = &
        new_StationNeighborPair(station3%station_id, station5%station_id,  &
                                station3%station_index, station5%station_index, &
                                859.59, -1.00)
    expected_neighbors(3) = &
        new_StationNeighborPair(station3%station_id, station6%station_id,  &
                                station3%station_index, station6%station_index, &
                                903.96, -1.00)
    expected_neighbors(4) = &
        new_StationNeighborPair(station3%station_id, station7%station_id,  &
                                station3%station_index, station7%station_index, &
                                230.96, -1.00)
    expected_neighbors(5) = null_StationNeighborPair()
    expected_neighbors(6) = null_StationNeighborPair()
    expected_neighbors(7) = null_StationNeighborPair()

    call assert_true(expected_neighbors .eq. actual_neighbors, "Getting neighbors for global station")

  end subroutine test_get_neighbors_for_station

  !> Tests the calculate_distance subroutine, which calculates the
  !! distance between two stations based on their lats and lons.
  subroutine test_calculate_distance()
    real :: distance

    distance = calculate_distance(44.020000, -107.97000, 44.970001, -110.70000)
    call assert_equals_within_tolerance(240.9187, distance, .001, 'Typical case')

    distance = calculate_distance(40.799999, 359.17000, 42.080002, -104.22000)
    call assert_equals_within_tolerance(8618.6533, distance, .001, 'lon_diff > abs(180)')

    distance = calculate_distance(40.799999, -178.17000, 42.080002, 20.22000)
    call assert_equals_within_tolerance(13470.3930, distance, .005, 'lon_diff < abs(180)')

  end subroutine test_calculate_distance

  !> Tests the is_same_station function.
  subroutine test_is_same_station()
    type(Station) :: primary
    type(Station) :: neighbor
    character(len=11), dimension(:) :: composite_ids(4)

    ! Testing identical stations
    primary = new_Station("USW00094958", 1, 41.6236, -98.9517)
    neighbor = new_Station("USW00094958", 1, 41.6236, -98.9517)
    call assert_true(is_same_station(primary, primary), "Testing station against itself")
    call assert_true(is_same_station(primary, neighbor), "Testing station against identical")

    ! Testing a station that doesn't match
    neighbor = new_Station("USC00011085", 2, 1.0, 1.0)
    call assert_false(is_same_station(primary, neighbor), "USH and USC that don't match")

    ! Testing an HCN against equivalent COOP stations
    primary = new_Station("USH00011084", 1, 1.0, 1.0)
    primary%is_hcn = .true.
    neighbor = new_Station("USC00011084", 2, 1.0, 1.0)
    call assert_true(is_same_station(primary, neighbor), "USH and USC equivalents")

    ! Testing a station with composites
    composite_ids(1) = "USC00011085"
    composite_ids(2) = "USW00094958"
    composite_ids(3) = "VQW00011640"
    composite_ids(4) = "VQC00671740"
    primary = new_Station("USH00011084", 1, 1.0, 1.0, composite_ids)

    neighbor = new_Station("USC00011085", 2, 1.0, 1.0)
    call assert_true(is_same_station(primary, neighbor), "Neighbor is composite 1")

    neighbor = new_Station("USW00094958", 1, 41.6236, -98.9517)
    call assert_true(is_same_station(primary, neighbor), "Neighbor is composite 2")

    neighbor = new_Station("VQW00011640", 1, 18.3331, -64.9667)
    call assert_true(is_same_station(primary, neighbor), "Neighbor is composite 3")

    neighbor = new_Station("VQC00671740", 1, 17.7469, -64.7014)
    call assert_true(is_same_station(primary, neighbor), "Neighbor is composite 4")

  end subroutine test_is_same_station

  !> Tests the sort_station_distances subroutine, which sorts
  !! StationNeighborPair instances based on their distance attributes.
  subroutine test_sort_station_distances()
    type(StationNeighborPair) :: pair1
    type(StationNeighborPair) :: pair2
    type(StationNeighborPair) :: pair3
    type(StationNeighborPair) :: pair4
    type(StationNeighborPair) :: pair5
    type(StationNeighborPair) :: pair6
    type(StationNeighborPair) :: pair7
    type(StationNeighborPair), dimension(:) :: pair_array_a(1)
    type(StationNeighborPair), dimension(:) :: pair_array_b(7)
    type(StationNeighborPair), dimension(:) :: pair_array_c(7)
    type(StationNeighborPair), dimension(:) :: pair_array_d(6)
    type(StationNeighborPair), dimension(:) :: pair_array_e(6)

    ! Set up StationNeighborPairs
    pair1 = new_StationNeighborPair("Station1   ", "Station2   ", 1, 2, 25.00)
    pair2 = new_StationNeighborPair("Station3   ", "Station4   ", 3, 4, 2.00)
    pair3 = new_StationNeighborPair("Station5   ", "Station6   ", 5, 6, 52.06)
    pair4 = new_StationNeighborPair("Station7   ", "Station8   ", 7, 8, 52.00)
    pair5 = new_StationNeighborPair("Station9   ", "Station10   ", 9, 10, 8000.00)
    pair6 = new_StationNeighborPair("Station11   ", "Station12   ", 11, 12, 52.06)
    pair7 = new_StationNeighborPair("Station13   ", "Station14   ", 13, 14, 0.00)

    ! Set up arrays
    ! TODO Why doesn't sort work with 1 item? Probably need a better sort algorithm.
    pair_array_a = (/ pair1 /)
    !call sort_station_distances(pair_array_a, 1)
    !call assert_equals(1, size(pair_array_a), "Array with 1 distance pair to sort")

    pair_array_b = (/ pair1, pair2, pair3, pair4, pair5, pair6, pair7 /)
    call sort_station_distances(pair_array_b, 7)
    call assert_equals(7, size(pair_array_b), "Sorting odd-count station pair array - testing size")
    call assert_true(pair_array_b .eq. (/ pair7, pair2, pair1, pair4, pair6, pair3, pair5 /), &
             "Sorting odd-count station pair array in random order - testing equality")

    pair_array_c = (/ pair5, pair3, pair6, pair4, pair1, pair2, pair7 /)
    call sort_station_distances(pair_array_c, 7)
    call assert_equals(7, size(pair_array_c), "Sorting odd-count station pair array - testing size")
    call assert_true(pair_array_c .eq. (/ pair7, pair2, pair1, pair4, pair6, pair3, pair5 /), &
             "Sorting odd-count station pair array in reverse distance order - testing equality")

    pair_array_d = (/ pair1, pair2, pair3, pair4, pair5, pair6 /)
    call sort_station_distances(pair_array_d, 6)
    call assert_equals(6, size(pair_array_d), "Sorting even-count station pair array - testing size")
    call assert_true(pair_array_d .eq. (/ pair2, pair1, pair4, pair3, pair6, pair5 /), &
             "Sorting even-count station pair array in random order - testing equality")

    pair_array_e = (/ pair5, pair3, pair6, pair4, pair1, pair2 /)
    call sort_station_distances(pair_array_e, 6)
    call assert_equals(6, size(pair_array_e), "Sorting even-count station pair array - testing size")
    call assert_true(pair_array_e .eq. (/ pair2, pair1, pair4, pair6, pair3, pair5 /), &
             "Sorting even-count station pair array in reverse distance order - testing equality")

  end subroutine test_sort_station_distances

  !> Tests the write_distance_output subroutine.
  subroutine test_write_distance_output()
    type(StationNeighborPair) :: pair1
    type(StationNeighborPair) :: pair2
    type(StationNeighborPair) :: pair3
    type(StationNeighborPair) :: pair4
    type(StationNeighborPair) :: pair5
    type(StationNeighborPair) :: pair6
    type(StationNeighborPair) :: pair7
    type(StationNeighborPair) :: null_pair
    type(StationNeighborPair), dimension(:) :: pair_array_a(7)
    type(StationNeighborPair), dimension(:) :: pair_array_b(7)
    type(StationNeighborPair), dimension(:) :: pair_array_c(7)
    type(StationNeighborPair), dimension(:) :: pair_array_d(7)
    type(StationNeighborPair), dimension(:) :: pair_array_e(7)
    type(StationNeighborPair), dimension(5,7) :: all_pair_arrays
    character(len=60) :: outfile
    character(len=200), dimension(15) :: expected_output
    character(len=2000), dimension(15) :: actual_output
    integer :: i

    outfile = "./src/test/resources/neighbors-distance.test-output.temp"

    ! Delete existing output file, if it exists
    call delete_file(outfile)

    ! Set up StationNeighborPairs
    pair1 = new_StationNeighborPair("Station1xxx", "Station2xxx", 1, 2, 25.00)
    pair2 = new_StationNeighborPair("Station3xxx", "Station4xxx", 3, 4, 2.00)
    pair3 = new_StationNeighborPair("Station5xxx", "Station6xxx", 5, 6, 52.06)
    pair4 = new_StationNeighborPair("Station7xxx", "Station8xxx", 7, 8, 52.00)
    pair5 = new_StationNeighborPair("Station9xxx", "Station10xxx", 9, 10, 5000.00)
    pair6 = new_StationNeighborPair("Station11xxx", "Station12xxx", 11, 12, 52.06)
    pair7 = new_StationNeighborPair("Station13xxx", "Station14xxx", 13, 14, 0.00)
    null_pair = null_StationNeighborPair()

    ! Arrays of StationNeighborPairs
    pair_array_a = (/ pair1, null_pair, null_pair, null_pair, null_pair, &
                      null_pair, null_pair /)
    pair_array_b = (/ pair2, pair1, pair3, pair4, pair5, pair6, pair7 /)
    pair_array_c = (/ pair5, pair3, pair6, pair4, pair1, pair2, pair7 /)
    pair_array_d = (/ pair3, pair2, pair1, pair4, pair5, pair6, null_pair /)
    pair_array_e = (/ pair4, pair3, pair6, pair5, pair1, pair2, null_pair /)

    ! Array of arrays of StationNeighborPairs
    all_pair_arrays(1,:) = pair_array_a
    all_pair_arrays(2,:) = pair_array_b
    all_pair_arrays(3,:) = pair_array_c
    all_pair_arrays(4,:) = pair_array_d
    all_pair_arrays(5,:) = pair_array_e

    ! Write output with a max number of 5 neighbors for a target.
    call write_distance_output(outfile, all_pair_arrays, 5)

    ! The task of write_distance_output is to write out the StationNeighborPairs
    ! that it is given. It does not matter that the example here does not follow
    ! the rules of ordered closest neighbors, only that they are printed
    ! as expected based on what was given.
    expected_output(1)  = "Station1xxx Station2xxx"
    expected_output(2)  = "          1           2           0           0           0           0"
    expected_output(3)  = "        0.0        25.0         0.0         0.0         0.0         0.0"
    expected_output(4)  = "Station3xxx Station4xxx Station2xxx Station6xxx Station8xxx Station10xx"
    expected_output(5)  = "          3           4           2           6           8          10"
    expected_output(6)  = "        0.0         2.0        25.0        52.1        52.0      5000.0"
    expected_output(7)  = "Station9xxx Station10xx Station6xxx Station12xx Station8xxx Station2xxx"
    expected_output(8)  = "          9          10           6          12           8           2"
    expected_output(9)  = "        0.0      5000.0        52.1        52.1        52.0        25.0"
    expected_output(10) = "Station5xxx Station6xxx Station4xxx Station2xxx Station8xxx Station10xx"
    expected_output(11) = "          5           6           4           2           8          10"
    expected_output(12) = "        0.0        52.1         2.0        25.0        52.0      5000.0"
    expected_output(13) = "Station7xxx Station8xxx Station6xxx Station12xx Station10xx Station2xxx"
    expected_output(14) = "          7           8           6          12          10           2"
    expected_output(15) = "        0.0        52.0        52.1        52.1      5000.0        25.0"

    call assert_equals(15, count_file_lines(outfile), "Checking number of file lines in neighbor file")

    ! Read the actual output into an array
    actual_output = get_file_lines(outfile)

    ! For each line, compare expected to actual
    do i=1, size(expected_output)
      call assert_equals(trim(expected_output(i)), trim(actual_output(i)), &
               "line by line comparison, line: "//log_string(i))
    enddo

    ! Cleaning up - delete existing output file if it exists
    call delete_file(outfile)

  end subroutine test_write_distance_output

  !> Tests the calculate_correlation subroutine, which calculates correlation
  !! between two stations.
  subroutine test_calculate_correlation()
    integer :: months_count
    real, dimension(:), allocatable :: station1_vals
    real, dimension(:), allocatable :: station2_vals
    real :: correlation

    ! First test, 47 months of data
    months_count = 47
    allocate(station1_vals(months_count))
    allocate(station2_vals(months_count))
    station1_vals = (/ -1.1984845, -0.3438392, -1.9600573, 2.3922076, 1.8798704, 0.0117383, &
                       -0.7329121, 0.7860937, -1.0607443, -0.7872915, -0.8175340, 1.8809527, &
                       -4.2484846, 4.6061606, -1.2100568, 1.4922073, -1.7201293, -0.5382624, &
                       0.5670881, 0.7860937, 0.0892563, -0.1372919, -0.5675340, -2.0690472, &
                       -0.1484845, 3.3061604, -0.2600570, -3.0077925, 0.7798705, 1.2117381, &
                       -0.5329123, 0.0360937, -0.0607433, -0.1872926, -0.3675337, -2.1190472, &
                       1.1015158, -0.1438394, 1.7399430, -0.5077927, 0.6298707, -0.2882619, &
                       1.7670879, -1.9139071, 0.8892565, 0.6127081, -0.4175339 /)

    station2_vals = (/ 0.0468754, -0.1062503, -1.7859375, 0.8921871, 1.3140638, -0.8953137, &
                       -0.3921890, 0.4828148, -1.0031242, 0.7781248, -0.2593760, 0.8281251, &
                       -2.6531248, 1.8937497, 0.4140624, 0.1421872, -0.3859365, -0.7453141, &
                       0.3578110, 0.1828146, 0.4468765, -0.2218752, 0.3906236, -0.3718746, &
                       -1.5531249, 1.6937499, 0.8640624, -1.7078127, 0.5640631, 0.6046863, &
                       -0.9421892, 0.2328148, -0.3531227, 0.5781240, 0.1406236, -1.1718745, &
                       -0.0031247, -0.2562501, 0.0140622, 1.2921875, -0.1859367, 0.0546865, &
                       0.3578110, -0.3671856, 0.2468767, 0.5281248, -0.0093770 /)

    correlation = calculate_correlation(months_count, station1_vals, station2_vals)
    call assert_equals_within_tolerance(0.75665992, correlation, .0000001, "correlation with 47 months")

    deallocate(station1_vals)
    deallocate(station2_vals)

    ! Second test, 43 months of data
    months_count = 43
    allocate(station1_vals(months_count))
    allocate(station2_vals(months_count))
    station1_vals = (/ 0.0637484, 0.1662502, 1.6899996, -0.6225007, -2.5937500, -0.3625005, &
                       1.7012839, 1.4269218, -3.5256407, 1.2653840, -0.2966995, 0.7875032, &
                       0.1137486, 0.9162502, -0.4600000, -0.5225012,  -0.2437495, 0.4374992, &
                       -0.6487160, 1.8769221, 0.2743591, 0.5653840, -1.2966995, -0.5124969, &
                       0.1137486, -0.5837498, 0.8400002, -1.7225015, 2.2062507, -0.4125010, &
                       -0.4987154, 0.5269217, 2.1743591, -2.6346161, 1.6033010, -0.3987494, &
                       -0.1837492, -0.2600002, -0.1725008, -0.2437497, 0.8374990, 0.8512845, &
                       1.1269217 /)

    station2_vals = (/ 0.9198093, -0.3640604, -0.2812519, 1.1140633, -2.6734369, 1.4268148, &
                       0.7193551, 1.8564508, -1.5790318, -1.4467739, 0.1870966, 0.4209642, &
                       0.3198099, 0.1859398, -0.1812515, -0.6859369, -0.0234370, -0.9731851, &
                       1.1193547, 0.9564509, 0.0209682, 0.4032261, -1.1129036, 0.6709652, &
                       -0.2301903, -0.5640602, 0.0687475, -0.4859366, 1.2265632, -0.4231856, &
                       -0.6306448, 1.2564509, -0.7790319, -1.7467736, 1.2870958, 0.2407751, &
                       1.1359396, -0.5812521, -0.5359368, -0.1734366, 1.4268146, -1.0306449, &
                       0.5564508 /)

    correlation = calculate_correlation(months_count, station1_vals, station2_vals)
    call assert_equals_within_tolerance(0.51990634, correlation, .0000002, "correlation with 43 months")

    deallocate(station1_vals)
    deallocate(station2_vals)

    ! Third test, 47 months of data, with low correlation value
    months_count = 47
    allocate(station1_vals(months_count))
    allocate(station2_vals(months_count))
    station1_vals = (/ 0.0000868, 0.7210903, -0.6672497, -1.2277369, 1.0635047, 0.5872970, &
                       1.1645436, -1.4471860, -0.5774322, 0.7123041, -0.4942417, 0.1150208, &
                       0.1500864, -0.2789097, 0.6327500, -1.0777369, 1.3635049, -0.1627035, &
                       -0.3354564, -0.3471861, -0.2274318, -0.1376953, -1.0442429, 2.2150211, &
                       -1.2999144, 0.6710911, -0.0672503, 0.3222632, -0.2364950, -0.7127037, &
                       -0.2354560, 1.4028139, -1.0274315, 0.7123041, 0.2057571, -0.2349787, &
                       1.4000864, -1.2789097, 0.0050130, -0.4364953, 0.4872966, -0.6854558, &
                       -0.1471863, 0.3725681, 0.1623039, 0.9057579, -0.4349794 /)

    station2_vals = (/ -0.2204666, 1.0730734, 0.4846172, -1.7653837, 0.8127499, 0.6218643, &
                       0.4288483, -1.8384628, 0.5012274, -0.1412573, 0.7592602, -0.3660698, &
                       0.1295319, -0.0769253, -0.2653828, -0.6653843, 0.6127501, 0.7718649, &
                       -1.3711519, 0.4615364, 0.0012283, 0.9087439, -0.8907413, 0.1839314, &
                       0.0295315, 0.0230742, -0.7653828, 0.2846155, -0.0372500, -0.3781352, &
                       0.7788486, 0.4615364, -0.5487719, 0.3087425, -0.3907413, -0.1160679, &
                       -0.0704689, 0.2230749, 1.9692326, -2.4372501, 0.2218657, -0.4711523, &
                       0.3615370, 0.4012280, 0.3587437, -0.3907413, -0.4160690 /)

    correlation = calculate_correlation(months_count, station1_vals, station2_vals)
    call assert_equals_within_tolerance(0.39725798, correlation, .0000001, "correlation with 43 months")

  end subroutine test_calculate_correlation

  !> Tests the calculate_first_diffs function.
  subroutine test_calculate_first_diffs()

    real, dimension(:) :: values(10)
    real, dimension(:) :: expected_first_diffs(10)
    integer :: values_count

    values = (/ 26.4, 23.2, 19.0, 21.1, MISSING_REAL, 20.2, 19.7, 18.4, MISSING_REAL, 45.0 /)
    values_count = 10
    expected_first_diffs = (/ -1.6, -2.1, 1.05, MISSING_REAL,MISSING_REAL, -0.25, -0.65, MISSING_REAL,MISSING_REAL,MISSING_REAL /)
    call assert_equals_within_tolerance(expected_first_diffs,calculate_first_diffs(values_count,values),FP_EPSILON, "First diffs")

    ! Values where first value is MISSING
    values = (/ MISSING_REAL, 26.4, 23.2, 19.0, 21.1, MISSING_REAL, 20.2, 19.7, 18.4, 45.0 /)
    expected_first_diffs = (/ MISSING_REAL, -1.6, -2.1, 1.05, MISSING_REAL, MISSING_REAL, -0.25, -0.65, 13.3, MISSING_REAL /)
    call assert_equals_within_tolerance(expected_first_diffs,calculate_first_diffs(values_count,values),FP_EPSILON,"First diffs")

  end subroutine test_calculate_first_diffs

  !> Tests the sort_station_correlations subroutiine.
  subroutine test_sort_station_correlations()
    type(StationNeighborPair) :: pair1
    type(StationNeighborPair) :: pair2
    type(StationNeighborPair) :: pair3
    type(StationNeighborPair) :: pair4
    type(StationNeighborPair) :: pair5
    type(StationNeighborPair) :: pair6
    type(StationNeighborPair) :: pair7
    type(StationNeighborPair), dimension(:) :: pair_array_a(1)
    type(StationNeighborPair), dimension(:) :: pair_array_b(7)
    type(StationNeighborPair), dimension(:) :: pair_array_c(7)
    type(StationNeighborPair), dimension(:) :: pair_array_d(6)
    type(StationNeighborPair), dimension(:) :: pair_array_e(6)

    ! Set up StationNeighborPairs
    pair1 = new_StationNeighborPair("Station1   ", "Station2   ", 1, 2, 25.00, .16)
    pair2 = new_StationNeighborPair("Station3   ", "Station4   ", 3, 4, 2.00, .58)
    pair3 = new_StationNeighborPair("Station5   ", "Station6   ", 5, 6, 52.06, 1.0)
    pair4 = new_StationNeighborPair("Station7   ", "Station8   ", 7, 8, 52.00, .17)
    pair5 = new_StationNeighborPair("Station9   ", "Station10   ", 9, 10, 8000.00, .581)
    pair6 = new_StationNeighborPair("Station11   ", "Station12   ", 11, 12, 52.06, .32)
    pair7 = new_StationNeighborPair("Station13   ", "Station14   ", 13, 14, 0.00, .32)

    ! Set up arrays
    pair_array_a = (/ pair1 /)
    call sort_station_correlations(pair_array_a, 1)
    call assert_equals(1, size(pair_array_a), "Array with 1 correlation pair to sort")

    pair_array_b = (/ pair1, pair2, pair3, pair4, pair5, pair6, pair7 /)
    call sort_station_correlations(pair_array_b, 7)
    call assert_equals(7, size(pair_array_b), "Sorting odd-count station pair array - testing size")
    call assert_true(pair_array_b .eq. (/ pair3, pair5, pair2, pair6, pair7, pair4, pair1 /), &
             "Sorting odd-count station pair array in random order - testing equality")

    pair_array_c = (/ pair1, pair4, pair7, pair6, pair2, pair5, pair3 /)
    call sort_station_correlations(pair_array_c, 7)
    call assert_equals(7, size(pair_array_c), "Sorting odd-count station pair array - testing size")
    call assert_true(pair_array_c .eq. (/ pair3, pair5, pair2, pair6, pair7, pair4, pair1 /), &
             "Sorting odd-count station pair array in reverse correlation order - testing equality")

    pair_array_d = (/ pair1, pair2, pair3, pair4, pair5, pair6 /)
    call sort_station_correlations(pair_array_d, 6)
    call assert_equals(6, size(pair_array_d), "Sorting even-count station pair array - testing size")
    call assert_true(pair_array_d .eq. (/ pair3, pair5, pair2, pair6, pair4, pair1 /), &
             "Sorting even-count station pair array in random order - testing equality")

    pair_array_e = (/ pair1, pair4, pair6, pair2, pair5, pair3 /)
    call sort_station_correlations(pair_array_e, 6)
    call assert_equals(6, size(pair_array_e), "Sorting even-count station pair array - testing size")
    call assert_true(pair_array_e .eq. (/ pair3, pair5, pair2, pair6, pair4, pair1 /), &
             "Sorting even-count station pair array in reverse correlation order - testing equality")

  end subroutine test_sort_station_correlations

  !> Tests all 4 logical paths of the get_input_data_filepath function.
  subroutine test_get_input_data_filepath()
    character(len=256) :: filepath_actual
    character(len=256) :: filepath_expected
    character(len=256) :: data_dir
    character(len=11), dimension(4) :: composite_ids
    type(Station) :: station_ushcn
    type(Station) :: station_global

    data_dir = get_property_chars(PROP_PATH_ELEMENT_DATA_IN)

    composite_ids = (/ "USC00018380", "USC00018385", "USC00018454", EMPTY_COMPOSITE_ID /)
    station_ushcn = new_Station("USH00018323", 1, 31.8075, -85.9722, composite_ids)
    station_global = new_Station("10160475000", 3, 35.4800, 8.1300)

    filepath_expected = trim(data_dir)//"USH00018323.raw.tmax"
    filepath_actual = get_input_data_filepath(station_ushcn, "tmax")
    call assert_equals(filepath_expected, filepath_actual, "input data file path USHCN station")

    ! Only tob because we're using the USHCN properties file. Not a real-world file path.
    filepath_expected = trim(data_dir)//"10160475000.raw.tmax"
    filepath_actual = get_input_data_filepath(station_global, "tmax")
    call assert_equals(filepath_expected, filepath_actual, "input data file path global station")

  end subroutine test_get_input_data_filepath

  !> Tests whether a real value is equal to the expected MISSING value.
  subroutine test_is_missing_real()

    call assert_true(is_missing_real(-99.99), "Missing real value expected TRUE")
    call assert_true(is_missing_real(-99.989999999), "Missing real value close enough expected TRUE")
    call assert_false(is_missing_real(99.99), "Valid real value not missing")
    call assert_false(is_missing_real(0.0), "Valid real value of 0 not missing")

  end subroutine test_is_missing_real

  !> Tests the write_correlation_output subroutine.
  subroutine test_write_correlation_output()
    type(StationNeighborPair) :: pair1
    type(StationNeighborPair) :: pair2
    type(StationNeighborPair) :: pair3
    type(StationNeighborPair) :: pair4
    type(StationNeighborPair) :: pair5
    type(StationNeighborPair) :: pair6
    type(StationNeighborPair) :: pair7
    type(StationNeighborPair) :: null_pair
    type(StationNeighborPair), dimension(:) :: pair_array_a(7)
    type(StationNeighborPair), dimension(:) :: pair_array_b(7)
    type(StationNeighborPair), dimension(:) :: pair_array_c(7)
    type(StationNeighborPair), dimension(:) :: pair_array_d(7)
    type(StationNeighborPair), dimension(:) :: pair_array_e(7)
    type(StationNeighborPair), dimension(5,7) :: all_pair_arrays
    character(len=60) :: outfile
    character(len=200), dimension(15) :: expected_output
    character(len=2000), dimension(15) :: actual_output
    integer :: i

    outfile = "./src/test/resources/neighbors-correlation.test-output.temp"

    ! Delete existing output file, if it exists
    call delete_file(outfile)

    ! Set up StationNeighborPairs
    pair1 = new_StationNeighborPair("Station1xxx", "Station2xxx", 1, 2, 25.00, 0.16)
    pair2 = new_StationNeighborPair("Station3xxx", "Station4xxx", 3, 4, 2.00, .58)
    pair3 = new_StationNeighborPair("Station5xxx", "Station6xxx", 5, 6, 52.06, 1.0)
    pair4 = new_StationNeighborPair("Station7xxx", "Station8xxx", 7, 8, 52.00, .17)
    pair5 = new_StationNeighborPair("Station9xxx", "Station10xxx", 9, 10, 5000.00, .581)
    pair6 = new_StationNeighborPair("Station11xxx", "Station12xxx", 11, 12, 52.06, 0.32)
    pair7 = new_StationNeighborPair("Station13xxx", "Station14xxx", 13, 14, 0.00, .32)
    null_pair = null_StationNeighborPair()

    ! Arrays of StationNeighborPairs
    pair_array_a = (/ pair1, null_pair, null_pair, null_pair, null_pair, &
                      null_pair, null_pair /)
    pair_array_b = (/ pair2, pair1, pair3, pair4, pair5, pair6, pair7 /)
    pair_array_c = (/ pair5, pair3, pair6, pair4, pair1, pair2, pair7 /)
    pair_array_d = (/ pair3, pair2, pair1, pair4, pair5, pair6, null_pair /)
    pair_array_e = (/ pair4, pair3, pair6, pair5, pair1, pair2, null_pair /)

    ! Array of arrays of StationNeighborPairs
    all_pair_arrays(1,:) = pair_array_a
    all_pair_arrays(2,:) = pair_array_b
    all_pair_arrays(3,:) = pair_array_c
    all_pair_arrays(4,:) = pair_array_d
    all_pair_arrays(5,:) = pair_array_e

    ! Write output with a max number of 5 neighbors for a target.
    call write_correlation_output(outfile, all_pair_arrays, 5)

    ! The task of write_correlation_output is to write out the StationNeighborPairs
    ! that it is given. It does not matter that the example here does not follow
    ! the rules of ordered closest neighbors, only that they are printed
    ! as expected based on what was given.
    expected_output(1)  = "Station1xxx Station2xxx"
    expected_output(2)  = "          1           2           0           0           0           0"
    expected_output(3)  = "       1.00        0.16        0.00        0.00        0.00        0.00"
    expected_output(4)  = "Station3xxx Station4xxx Station2xxx Station6xxx Station8xxx Station10xx"
    expected_output(5)  = "          3           4           2           6           8          10"
    expected_output(6)  = "       1.00        0.58        0.16        1.00        0.17        0.58"
    expected_output(7)  = "Station9xxx Station10xx Station6xxx Station12xx Station8xxx Station2xxx"
    expected_output(8)  = "          9          10           6          12           8           2"
    expected_output(9)  = "       1.00        0.58        1.00        0.32        0.17        0.16"
    expected_output(10) = "Station5xxx Station6xxx Station4xxx Station2xxx Station8xxx Station10xx"
    expected_output(11) = "          5           6           4           2           8          10"
    expected_output(12) = "       1.00        1.00        0.58        0.16        0.17        0.58"
    expected_output(13) = "Station7xxx Station8xxx Station6xxx Station12xx Station10xx Station2xxx"
    expected_output(14) = "          7           8           6          12          10           2"
    expected_output(15) = "       1.00        0.17        1.00        0.32        0.58        0.16"

    call assert_equals(15, count_file_lines(outfile), "Checking number of file lines in neighbor file")

    ! Read the actual output into an array
    actual_output = get_file_lines(outfile)

    ! For each line, compare expected to actual
    do i=1, size(expected_output)
      call assert_equals(trim(expected_output(i)), trim(actual_output(i)), &
               "line by line comparison, line: "//log_string(i))
    enddo

    ! Cleaning up - delete existing output file if it exists
    call delete_file(outfile)

  end subroutine test_write_correlation_output

end module ChooseNeighborsTest
