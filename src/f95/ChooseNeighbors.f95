!> @brief
!! For each target station, determines the set of "best" neighbor stations, both
!! according to distance only, and according to closest correlation. Outputs both
!! distance and correlation neighbors to files. See Req. 1.1 for more information.
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
module ChooseNeighbors

  use FileUtils
  use DatetimeUtils
  use MathUtils
  use Logger
  use PropertyReader
  use StationType
  use StationNeighborPairType
  use PropertyParameters
  use AlgorithmParameters

  implicit none

contains

  !> Choose the "best" neighbors for each target station, based on
  !!   configured options. Writes to configured output files, a file
  !!   for closest neighbors and a file for most correlated neighbors.
  !!   See Req. 1.1 for more information.
  !!
  !! @param[out] correlation_neighbors A collection of target-neighbor pairs for each station
  !!   with their ids, indexes, and correlations.
  subroutine choose_neighbors(correlation_neighbors)

    type(StationNeighborPair), dimension(:,:), allocatable, intent(out) :: correlation_neighbors

    type(Station), dimension(:), allocatable :: all_stations
    type(StationNeighborPair), dimension(:,:), allocatable :: neighbors
    character(len=256) :: metafile
    character(len=256) :: distance_outfile
    character(len=256) :: corr_outfile
    integer :: distance_neighbor_limit
    integer :: final_neighbor_limit
    character(len=len(OPT_NEIGH_METHOD_ANOMALY)) :: method

    ! Read in the station metadata
    metafile = get_property_chars(PROP_PATH_STATION_META)
    call log_info("Station metadata file: "//metafile)
    call read_station_metadata(metafile, all_stations)

    ! Find the [X] closest (non-USHCN) neighbors for each target
    ! Subtracting 1 because the first "neighbor" is the target station and we treat it differently.
    distance_neighbor_limit = get_property_int(PROP_NEIGH_BUFFERED_LIMIT)-1
    allocate(neighbors(size(all_stations), distance_neighbor_limit))
    neighbors = get_neighbors_by_distance(all_stations, distance_neighbor_limit)
    call log_info("Number of closest neighbors output: "//log_string(distance_neighbor_limit))

    ! Find the [Y] most correlated neighbors of the [X] closest neighbors.
    method = get_property_chars(PROP_NEIGH_METHOD)
    call log_info("Get neighbors by method: "//method)

    ! Read in properties for writing neighbors files
    distance_outfile = get_property_chars(PROP_PATH_NEIGH_DISTANCE)
    call log_info("Neighbors-by-distance output file: "//distance_outfile)

    ! Write neighbors-distance file
    call write_distance_output(distance_outfile, neighbors, distance_neighbor_limit)
    call log_info("ChooseNeighbors successfully created a neighbors-distance file")

    ! Subtracting 1 because the first "neighbor" is the target station and we treat it differently.
    final_neighbor_limit = get_property_int(PROP_NEIGH_FINAL_LIMIT)-1

    allocate(correlation_neighbors(size(all_stations), distance_neighbor_limit))
    call get_neighbors_by_correlation(all_stations, neighbors, &
             final_neighbor_limit, correlation_neighbors)

    corr_outfile = get_property_chars(PROP_PATH_NEIGH_CORRELATION)
    call log_info("Neighbors-by-correlation output file: "//corr_outfile)

    call write_correlation_output(corr_outfile, correlation_neighbors, final_neighbor_limit)
    call log_info("ChooseNeighbors successfully created a neighbors-correlation file")

  end subroutine choose_neighbors

  !> Reads the station metadata file (.inv file) and stores
  !!   data in arrays of Station types.
  !!
  !! @param[in] metafile The location of the file containing station metadata.
  !! @param[out] all_stations An array containing all stations in the file.
  subroutine read_station_metadata(metafile, all_stations)

    character(len=*), intent(in) :: metafile
    type(Station), dimension(:), allocatable, intent(out) :: all_stations

    ! Station data
    real :: latitude, longitude
    character(len=11) :: station_id
    character(len=11), dimension(:) :: composite_ids(4)
    type(Station) :: this_station
    ! File reading variables
    integer :: hcn_count
    integer :: station_index
    integer :: line_count
    character(len=130), dimension(:), allocatable :: file_lines
    character(len=130) :: line
    100 format(a11, f9.4, f10.4, 43x, 4(a11, 1x))

    ! Number of lines in file indicate number of stations.
    line_count = count_file_lines(metafile)
    allocate(file_lines(line_count))
    allocate(all_stations(line_count))

    ! Tracking this number for logging purposes only.
    hcn_count = 0

    ! Loop through all station records in metafile
    file_lines = get_file_lines(metafile)
    do station_index = 1, line_count
      line = file_lines(station_index)
      if(is_hcn_record(line)) then
        ! reading the candidates and composites in USHCN format
        read(line, 100) station_id, latitude, longitude, composite_ids(1:4)
        this_station = new_Station(station_id, station_index, latitude, longitude,&
                             composite_ids)
        hcn_count = hcn_count+1
      else
        ! read station list in GHCN-D format
        read(line, 100) station_id, latitude, longitude
        this_station = new_Station(station_id, station_index, latitude, longitude)
      endif
      all_stations(station_index) = this_station
    end do

    call log_info('Number of USHCN stations read: '//log_string(hcn_count))
    call log_info('Number of stations read: '//log_string(size(all_stations)))

  end subroutine read_station_metadata

  !> Chooses closest neighbors for each target station, based on
  !!   distance between stations only, and returns them in order
  !!   by distance.
  !!
  !! @param all_stations The array of all stations read from the
  !!   input file.
  !! @param distance_neighbor_limit The number of closest neighbors to
  !!   return for each station.
  !! @return A collection of pairs for each station with their ids and distances
  !!   with a limit to the number of station pairs with the shortest distances.
  function get_neighbors_by_distance(all_stations, distance_neighbor_limit)  &
    result(all_neighbors)

    type(Station), dimension(:), allocatable :: all_stations
    integer :: distance_neighbor_limit
    type(StationNeighborPair), dimension(:,:) ::  &
           all_neighbors(size(all_stations), distance_neighbor_limit)

    integer :: all_count
    integer :: curr_neighbor_count
    type(Station) :: curr_station
    type(StationNeighborPair), dimension(:) :: neighbors(distance_neighbor_limit)
    type(StationNeighborPair), dimension(:) :: neighbors_prelim(size(all_stations))

    neighbors(:) = null_StationNeighborPair()
    neighbors_prelim(:) = null_StationNeighborPair()

    ! For all stations, get the list of closest neighbors.
    do all_count=1, size(all_stations)
      curr_station = all_stations(all_count)

      ! Get all reasonably close neighbors for the current station
      neighbors_prelim = get_neighbors_for_station(curr_station, all_stations, &
               curr_neighbor_count, distance_neighbor_limit)

      ! Sort neighbors by distance
      call sort_station_distances(neighbors_prelim, curr_neighbor_count)

      ! Reduce array of neighbors to distance_neighbor_limit
      neighbors(1:distance_neighbor_limit) = neighbors_prelim(1:distance_neighbor_limit)

      ! Add the current station neighbors to the 2D array of all neighbors
      ! for all stations
      all_neighbors(all_count,:) = neighbors

    end do

  end function get_neighbors_by_distance

  !> Gets the neighbors for each station that are within a
  !!   specified maximum distance.
  !!
  !! @param curr_station The current station being processed.
  !! @param all_stations All stations available.
  !! @param neighbor_count The number of neighbors found for the current
  !!   station.
  !! @param distance_neighbor_limit The maximum number of stations that
  !!   that will be in the final neighbors list.
  !! @return A collection of pairs for each station with their ids and distances
  !!   less than an allowed distance length.
  function get_neighbors_for_station(curr_station, all_stations, neighbor_count, distance_neighbor_limit)  &
                result(neighbors)

    type(Station) :: curr_station
    type(Station), dimension(:) :: all_stations
    integer :: neighbor_count
    integer :: distance_neighbor_limit
    type(StationNeighborPair), dimension(:) :: neighbors(size(all_stations))

    type(StationNeighborPair) :: this_pair
    type(Station) :: curr_neighbor
    integer :: station_index
    real :: distance
    integer :: allowed_distance

    integer, parameter :: dist_init = 1000
    integer, parameter :: dist_incr = 500
    integer, parameter :: dist_max = 5000

    allowed_distance = dist_init

    ! Keep looping through all the potential neighbor stations until
    ! enough neighbors are found.
    ! @todo Recalculates the distance of all neighbors each time allowed_distance
    ! is increased. Consider a more efficient strategy.
    do allowed_distance=dist_init, dist_max, dist_incr

      ! Null out unused array
      neighbors(:) = null_StationNeighborPair()

      neighbor_count = 0
      do station_index=1, size(all_stations)
        curr_neighbor = all_stations(station_index)

        ! Req. 1.1.2 HCN stations are not used as neighbors
        if(curr_neighbor%is_hcn) cycle
        ! Includes checking for composites of HCN stations
        if(is_same_station(curr_station, curr_neighbor)) cycle

        ! Calculate the distance between target station and current neighbor
        distance = calculate_distance(curr_station%latitude, curr_station%longitude, &
                     curr_neighbor%latitude, curr_neighbor%longitude)
        if(distance < allowed_distance) then
          neighbor_count = neighbor_count + 1
          ! Populate the neighbors return value
          this_pair = new_StationNeighborPair(curr_station%station_id, &
                                       curr_neighbor%station_id, curr_station%station_index, &
                                       curr_neighbor%station_index, distance)
          neighbors(neighbor_count) = this_pair
        endif
      end do

      ! See if there were enough stations. If not, increase maximum distance.
      if(neighbor_count < distance_neighbor_limit .and. allowed_distance < dist_max) then
        cycle
      else
        exit ! exit from main do loop when enough stations are found
      endif
    end do

  end function get_neighbors_for_station

  !> Compares two stations to determine if they are the same
  !!   or if one is a composite station for the other.
  !!
  !! @param primary The target station against which we compare
  !!   a potential neighbor station.
  !! @param neighbor The potential neighbor station being
  !!   considered.
  !! @return True if the neighbor is the same station or a
  !!   component of the composite station of the primary.
  function is_same_station(primary, neighbor) result(is_same)

    type(Station) :: primary
    type(Station) :: neighbor
    logical :: is_same

    integer :: composite_index

    is_same = .false.

    ! skip the candidate station in the reference network
    if(primary%station_id .eq. neighbor%station_id) then
      is_same = .true.
      return
    endif

    ! Special considerations for HCN stations
    if(primary%is_hcn) then
      ! The case where an HCN station and a COOP station are the same.
      ! Example: USH00011084 USC00011084
      if(primary%station_id(6:11) .eq. neighbor%station_id(6:11)) then
        is_same = .true.
        return
      endif

      ! skip the composite station in the reference network
      do composite_index = 1,4
        if(primary%composite_ids(composite_index) .ne. EMPTY_COMPOSITE_ID .and. &
           neighbor%station_id .eq. primary%composite_ids(composite_index)) then
          call log_debug('Skip composite:'//station_string(primary)// &
                        ' and neighbor '//neighbor%station_id)
          is_same = .true.
          return
        endif
      end do
    endif  ! End special handling of HCN stations

  end function is_same_station

  !> Calculates the distance between two neighbor stations
  !!   given the latitude and longitude of each.
  !!   Code imported from legacy PHA. Needs refactoring.
  !!
  !! @param lat_target Latitude of target station with double precision.
  !! @param lon_target Longitude of target station with double precision.
  !! @param lat_neighbor Latitude of neighbor station with double precision.
  !! @param lon_neighbor Longitude of neighbor station with double precision.
  !! @return Distance in kilometers.
  function calculate_distance(lat_target, lon_target, lat_neighbor, lon_neighbor) result(distance)
    ! Params and return value
    real :: lat_target
    real :: lon_target
    real :: lat_neighbor
    real :: lon_neighbor
    real :: distance
    ! Intermediate calculation values
    real :: lat_avg
    real :: lat_diff_radians
    real :: lon_diff_radians
    real :: lon_diff
    ! Constants
    real :: degrees_to_radians = 3.14159265 / 180.0
    real :: radius_earth = 6371.0

    ! Calculate longitude difference, handling special case of different hemispheres
    lon_diff = lon_target - lon_neighbor
    if(lon_diff .lt. -180.0) lon_diff = lon_diff + 360.0
    if(lon_diff .gt. 180.0) lon_diff = lon_diff - 360.0
    lon_diff_radians = degrees_to_radians * lon_diff

    ! Calculate latitude difference
    lat_avg = degrees_to_radians * (lat_target + lat_neighbor) / 2.0
    lat_diff_radians = degrees_to_radians * (lat_target - lat_neighbor)

    ! Final calculation of distance
    ! @todo get reference for great circle distance
    distance = abs(acos(cos(lat_diff_radians) * cos(lon_diff_radians * cos(lat_avg))))   &
               * radius_earth

  end function calculate_distance

  !> Sorts the nearest neighbors for each target station based on distance.
  !!
  !! @param[inout] neighbors A collection of pairs for each station with their ids and distances
  !!   that is sorted by distance.
  !! @param[in] station_count The number of neighbors in the neighbors array.
  !! @todo Consider other sort functions, like merge sort.
  subroutine sort_station_distances(neighbors, station_count)

    type(StationNeighborPair), dimension(:), intent(inout) ::  neighbors
    integer, intent(in) :: station_count

    type(StationNeighborPair) ::  curr_neighbor
    real :: curr_distance ! Distance currently being handled
    integer :: i, j, l, ir ! Indexes

    l = station_count/2 + 1
    ir = station_count

    do while(.true.)
      if(l .gt. 1) then
        l = l - 1
        curr_neighbor = neighbors(l)
        curr_distance = neighbors(l)%distance
      else
        curr_neighbor = neighbors(ir)
        curr_distance = neighbors(ir)%distance
        neighbors(ir) = neighbors(1)
        ir = ir - 1
        if(ir .eq. 1) then
          neighbors(1) = curr_neighbor
          return
        endif
      endif

      i = l
      j = l*2
      do while(j .le. ir)
        if(j .lt. ir) then
          if(neighbors(j)%distance .lt. neighbors(j+1)%distance) then
            j = j + 1
          endif
        endif
        if(curr_distance .lt. neighbors(j)%distance) then
          neighbors(i) = neighbors(j)
          i = j
          j = j + j
        else
          j = ir + 1
        endif
      end do

      neighbors(i) = curr_neighbor
    end do

  end subroutine sort_station_distances

  !> Writes the neighbors-distance output for all stations.
  !!   Output per station includes a line of station identifiers, a line of
  !!   station indexes and a line of distances from the target station.
  !!
  !! @param[in] outfile The path of the write file.
  !! @param[in] neighbors The station-neighbors for each target station
  !!   to write.
  !! @param[in] distance_neighbor_limit The maximum number of neighbors to
  !!   include for each target station.
  subroutine write_distance_output(outfile, neighbors, distance_neighbor_limit)

    character(len=*), intent(in) :: outfile
    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    integer, intent(in) :: distance_neighbor_limit

    type(StationNeighborPair) :: target
    integer :: file_status
    integer :: dist_file_unit
    integer :: i, station_index ! loop iterators

    real, parameter :: self_distance = 0.0

    dist_file_unit = get_available_file_unit()

    ! open output file
    open(dist_file_unit, file=outfile, iostat=file_status, status='replace')
    if(file_status /= 0) then
      call log_fatal('ChooseNeighbors::write_distance_output: Cannot open output file '  &
                      //trim(outfile)//'. Program is aborting.')
      stop 1
    endif

    do station_index = 1, size(neighbors, 1)
      ! the first pair is the target
      target = neighbors(station_index, 1)

      write(dist_file_unit, '(500(a11,1x))') target%target_id,  &
             (neighbors(station_index,i)%neighbor_id,i=1,distance_neighbor_limit)

      write(dist_file_unit, '(500(i11,1x))') target%target_index, &
             (neighbors(station_index,i)%neighbor_index, i=1,distance_neighbor_limit)

      write(dist_file_unit, '(500(f11.1,1x))') self_distance,  &
             (neighbors(station_index,i)%distance,i=1,distance_neighbor_limit)

    end do

    close(dist_file_unit)

  end subroutine write_distance_output

  !> Chooses the [Y] most correlated neighbors for each target station,
  !!   from the [X] closest neighbor stations.
  !!
  !! param[in] all_stations The array of all Stations being processed.
  !! param[in] distance_neighbors A collection of pairs for each station with their ids and distances.
  !! param[in] corr_neigh_limit The maximum number of best correlated neighbors to use.
  !! param[out] correlation_neighbors A collection of pairs for each station with their ids and correlations.
  subroutine get_neighbors_by_correlation(all_stations, distance_neighbors, corr_neigh_limit, correlation_neighbors)
    use CommonVariables, Only: begin_year

    type(Station), dimension(:), allocatable, intent(in) :: all_stations
    type(StationNeighborPair), dimension(:,:), allocatable, intent(in) :: distance_neighbors
    integer, intent(in) :: corr_neigh_limit
    type(StationNeighborPair), dimension(:,:), allocatable, intent(out) :: correlation_neighbors

    type(Station) :: current_station
    type(StationNeighborPair) :: this_corr_pair
    type(StationNeighborPair), dimension(:) :: station_corr_neighbors(size(all_stations))

    real, dimension(:,:,:), allocatable :: station_data

    character(len=4) :: element
    integer :: station_count
    integer :: month
    integer :: year
    integer :: prev_neighbor_count
    integer :: neighbor_count
    integer :: target_index
    integer :: neighbor_index

    integer :: dist_neigh_limit
    integer :: min_neighbors
    integer :: end_year
    integer :: num_years
    real :: min_corr

    character(len=11) :: target_id
    character(len=11), parameter :: empty_id = ''
    integer, parameter :: min_months = 4
    integer :: j, k ! loop iterators

    ! Variables for minimizing # neighbors
    logical :: can_remove
    integer, dimension(:,:), allocatable :: obs_count ! Number of non-missing observations for the station

    ! After getting the current year, we can allocate arrays
    end_year = get_current_year()
    allocate(obs_count(begin_year:end_year, 12))

    if(end_year .lt. begin_year) then
      call log_fatal('begin_year: '//trim(log_string(begin_year))  &
        //' too high, must be less than end_year: '//trim(log_string(end_year)))
      stop 1
    endif
    call log_info("Highest year to process: "//log_string(end_year))

    element = trim(get_property_chars(PROP_ELEMENT))
    call log_info('Processing element: '//trim(element))

    ! Subtracting 1 because the first "neighbor" is the target station and we treat it differently.
    dist_neigh_limit = get_property_int(PROP_NEIGH_BUFFERED_LIMIT)-1
    min_corr = get_property_real(PROP_NEIGH_MIN_COEFF)
    min_neighbors = get_property_int(PROP_NEIGH_MIN_COVERAGE)

    station_count = size(all_stations)
    allocate(correlation_neighbors(station_count, corr_neigh_limit))

    ! Read station data for all stations
    call get_element_data(all_stations, element, station_data)

    ! TODO What is this for?
    open(40,file='kstn.out')

    ! get correlations for nearest neighbors, sort them, and then
    ! write along with station ids
    do j = 1, station_count
      ! Get the target station_id and station_index
      target_id = distance_neighbors(j, 1)%target_id
      target_index = distance_neighbors(j, 1)%target_index

      current_station = all_stations(target_index)
      if(current_station%station_id .ne. target_id) then
        call log_info(log_string(target_index)//' '//current_station%station_id//' '//  &
          target_id//' lost synch between dist and meta indices')
        return
      endif

      ! For this target station, get all the neighbor station correlations.
      call get_station_correlations(target_index, all_stations, distance_neighbors, station_data, corr_neigh_limit,  &
                                    station_corr_neighbors)

      ! sort 10/20 best correlated stations
      call sort_station_correlations(station_corr_neighbors, dist_neigh_limit)

      ! try to minimize the number of neighbors in the network
      ! sum all of the neighbor year/months
      do year = begin_year, end_year
        do month = 1, 12
          obs_count(year,month) = 0
          if (.not. is_missing_real(station_data(target_index, year, month))) then
            ! go thru the entire distance array
            do k = 1, dist_neigh_limit
              ! obs_count[y,m] = nobs in all dist-neigh stations > min_corr
              if(station_corr_neighbors(k)%correlation .gt. min_corr) then
                if(.not. is_missing_real(station_data(station_corr_neighbors(k)%neighbor_index, year, month))) &
                  obs_count(year, month) = obs_count(year, month) + 1
              else
                exit
              endif
            end do
            neighbor_count = k - 1
            write(40,*) j,month,year,neighbor_count
          endif
        end do
      end do

      ! go back thru the stations, from least to highest corr
      ! or nearest to farthest when corr_type == near
      ! see if we can reduce the number of neighbors
      prev_neighbor_count = neighbor_count
      ! if there are only corr_neigh_limit or less neighbors, skip trying to fill
      ! neighbor sparse periods
      if(neighbor_count .gt. corr_neigh_limit) then
        ! find the stations we can potentially remove and
        ! NOT remove (any obs_count[y,m] < min_neighbors for cand-neigh POR)
        do k = prev_neighbor_count, 1, -1
          neighbor_index = station_corr_neighbors(k)%neighbor_index
          can_remove = .true.
          potential_stn: do year = begin_year, end_year
            do month = 1, 12
              if (.not. is_missing_real(station_data(target_index,year,month)) .and. &
                  .not. is_missing_real(station_data(neighbor_index,year,month))) then
                if(obs_count(year,month) .le. min_neighbors) then
                  can_remove = .false.
                  exit potential_stn
                endif
              endif
            end do
          end do potential_stn

          ! remove the station if possible
          if(can_remove) then
            if(neighbor_count-1 .lt. corr_neigh_limit) exit
            call log_debug("Remove: "//current_station%station_id//'-'//all_stations(target_index)%station_id// &
                log_string(station_corr_neighbors(k)%correlation))
            station_corr_neighbors(k)%correlation = 0.0
            neighbor_count = neighbor_count - 1
            do year = begin_year, end_year
              do month = 1, 12
                if (.not. is_missing_real(station_data(target_index,year,month)) .and. &
                    .not. is_missing_real(station_data(neighbor_index,year,month))) then
                  obs_count(year,month) = obs_count(year,month) - 1
                endif
              end do
            end do
          endif
        end do
      else
        call log_debug(current_station%station_id//" too few neighbors: "//log_string(neighbor_count) &
        //" for sparse re-distrib min: "//log_string(corr_neigh_limit))
      endif

      ! sort 10/20 best correlated stations
      ! must go through the entire dist_neigh_limit elements to get the corr_neigh_limit network!
      call sort_station_correlations(station_corr_neighbors, dist_neigh_limit)

      ! Create the final correlation pairs
      do k = 1, corr_neigh_limit
        if(station_corr_neighbors(k)%correlation .gt. min_corr) then
          this_corr_pair = station_corr_neighbors(k)
        else
          this_corr_pair = new_StationNeighborPair(  &
            target_id, empty_id, &
            target_index, 0, 0.0, 0.0)
        endif
        correlation_neighbors(j,k) = this_corr_pair
      end do

    end do

    close(40)

  end subroutine get_neighbors_by_correlation

  !> Reads data for the given element from relevant data input files for
  !!   each station.
  !!
  !! @param[in] all_stations The array of all stations being processed.
  !! @param[in] element The type of data (tmax, tmin, tavg, tdtr) being read.
  !! @param[out] station_data The data read from the files.
  subroutine get_element_data(all_stations, element, station_data)

    use CommonVariables, Only: begin_year

    type(Station), dimension(:), allocatable, intent(in) :: all_stations
    character(len=*), intent(in) :: element
    real, dimension(:,:,:), allocatable, intent(out) :: station_data

    character(len=256) :: filename
    character(len=2000), dimension(:), allocatable :: data_file_lines
    integer :: file_line_count
    integer :: month, year
    integer :: end_year
    integer :: station_idx
    integer, dimension(:) :: vals_for_year(12)
    integer :: record_year
    integer, dimension(:) :: good_month_count(12)
    real, dimension(:) :: good_month_sum(12)
    real :: month_avg
    character(len=3), dimension(:) :: flags(12)
    real, dimension(:,:), allocatable :: data
    character(len=50) :: data_format
    character(len=11) station_id
    ! current GHCNMv3 and USHCNv2 scale are 0.01C & 0.01cm, (same for pw?)
    real, parameter :: elem_scale = 100.0
    integer :: i, j

    end_year = get_current_year()
    allocate(station_data(size(all_stations),begin_year:end_year,12))
    allocate(data(begin_year:end_year,12))

    do station_idx = 1, size(all_stations)
      ! Complex set of rules to determine filename and file location
      filename = get_input_data_filepath(all_stations(station_idx), element)

      ! initialize data array for current station
      data(:,:) = MISSING_REAL
      station_data(station_idx,:,:) = MISSING_REAL

      ! open station data file
      file_line_count = count_file_lines(filename)
      if(allocated(data_file_lines)) deallocate(data_file_lines)
      allocate(data_file_lines(file_line_count))
      data_file_lines = get_file_lines(filename)

      good_month_sum(1:12) = 0.0
      good_month_count(1:12) = 0

      ! Read data from file
      data_format = '(a11,1x,i4,12(i6,a3))'
      do j = 1, file_line_count
        ! get data values and day flags
        read(data_file_lines(j), data_format) station_id, record_year, (vals_for_year(i),flags(i),i=1,12)
        ! Now that we have the data, regardless of which input format
        if(record_year .lt. begin_year .or. record_year .gt. end_year) cycle
        do month = 1, 12
          if(vals_for_year(month) .eq. MISSING_INT .or. flags(month)(2:2) .ne. ' ') then
            data(record_year,month) = MISSING_REAL
          else
            data(record_year,month) = vals_for_year(month)/elem_scale
            good_month_sum(month) = good_month_sum(month) + data(record_year,month)
            good_month_count(month) = good_month_count(month) + 1
          endif
        end do
      end do

      ! save station data
      do month = 1,12
        month_avg = good_month_sum(month) / good_month_count(month)
        do year = begin_year, end_year
          if (.not. is_missing_real(data(year,month))) then
            station_data(station_idx, year, month) = data(year,month) - month_avg
          end if
        end do
      end do
    end do
  end subroutine get_element_data

  !> For a given target station, gets the correlations between that target station and
  !!   each of its neighbors.
  !!
  !! param[in] target_index The target index for a particular station.
  !! param[in] all_stations The array of all Stations being processed.
  !! param[in] distance_neighbors A collection of pairs for each station with their ids and distances.
  !! param[in] station_data The element data for all the stations.
  !! param[in] corr_neigh_limit The maximum number of best correlated neighbors to use.
  !! param[out] station_corr_neighbors A collection of pairs for each station
  !!   with their ids, distances and correlations.
  subroutine get_station_correlations(target_index, all_stations, distance_neighbors, station_data, corr_neigh_limit,  &
                                      station_corr_neighbors)
    use CommonVariables, Only: begin_year

    integer, intent(in) :: target_index
    type(Station), dimension(:), allocatable, intent(in) :: all_stations
    type(StationNeighborPair), dimension(:,:), allocatable, intent(in) :: distance_neighbors
    integer, intent(in) :: corr_neigh_limit
    type(StationNeighborPair), dimension(:), intent(out) :: station_corr_neighbors(size(all_stations))
    real, dimension(:,:,:), allocatable, intent(in) :: station_data

    type(Station) :: current_station
    integer :: month
    integer :: year
    integer :: neighbor_index
    integer :: good_values_count
    integer :: dist_neigh_limit

    integer :: end_year
    integer :: num_years
    real, dimension(:), allocatable :: target_values
    real, dimension(:), allocatable :: neigh_values
    real, dimension(:), allocatable :: target_first_diffs
    real, dimension(:), allocatable :: neigh_first_diffs

    real :: correlation
    character(len=20) :: corr_type
    character(len=11) :: target_id
    integer good_month_count(12)

    integer, parameter :: min_months = 4
    integer :: i, k ! loop iterators

    ! After getting the current year, we can allocate arrays
    end_year = get_current_year()
    num_years = end_year - begin_year+1
    allocate(target_values(num_years*12))
    allocate(neigh_values(num_years*12))

    ! Subtracting 1 because the first "neighbor" is the target station and we treat it differently.
    dist_neigh_limit = get_property_int(PROP_NEIGH_BUFFERED_LIMIT)-1
    corr_type = get_property_chars(PROP_NEIGH_METHOD)

    ! Initialize correlation neighbors for this station
    station_corr_neighbors(:) = null_StationNeighborPair()

    target_id = all_stations(target_index)%station_id

    ! We have all the necessary data, compute correlations with network
    correlation_loop: do k = 1, dist_neigh_limit
      neighbor_index = distance_neighbors(target_index, k)%neighbor_index

      ! If the neighbor is null, don't try to process.
      if(neighbor_index == 0) cycle

      station_corr_neighbors(k) = new_StationNeighborPair(  &
        target_id, all_stations(neighbor_index)%station_id, &
        target_index, neighbor_index, distance_neighbors(target_index, k)%distance, 0.0)

      ! Initialize these handy variables
      good_month_count(1:12) = 0
      good_values_count = 0

      ! For each year, count the number of good data for each month.
      ! So how many good values for each January, each February, etc.
      do year = begin_year, end_year
        do month = 1, 12
          if (.not. is_missing_real(station_data(target_index,year,month)) .and.  &
              .not. is_missing_real(station_data(neighbor_index,year,month))) then
            good_month_count(month) = good_month_count(month) + 1
            good_values_count = good_values_count + 1
            target_values(good_values_count) = station_data(target_index, year, month)
            neigh_values(good_values_count) = station_data(neighbor_index, year, month)
          endif
        end do
      end do

      do month = 1, 12
        if(good_month_count(month) .lt. min_months) then
          station_corr_neighbors(k)%correlation = 0.0
          station_corr_neighbors(k)%neighbor_index = 0
          call log_debug(log_string(target_index)//" Not enough common: "//target_id//" - "  &
              //all_stations(neighbor_index)%station_id//" month= "//log_string(month)//" good_month_count= "  &
              //log_string(good_month_count(month)))
          cycle correlation_loop
        endif
      end do

      if(allocated(target_first_diffs)) deallocate(target_first_diffs)
      allocate(target_first_diffs(good_values_count))
      if(allocated(neigh_first_diffs)) deallocate(neigh_first_diffs)
      allocate(neigh_first_diffs(good_values_count))

      ! fool algorithm that every neighbor has the same correlation
      ! therefore distance is pre-eminent
      ! of producing bogus correlation file when set to distance-only.
      if(corr_type .eq. OPT_NEIGH_METHOD_DISTANCE) then
        station_corr_neighbors(k)%correlation = 1.0
      else if(corr_type .eq. OPT_NEIGH_METHOD_1ST_DIFFS) then
        ! make the first difference of the candidate station
        target_first_diffs = calculate_first_diffs(good_values_count, target_values)
        ! make the first difference of each network station series
        neigh_first_diffs = calculate_first_diffs(good_values_count, neigh_values)
        ! calculate correlation w/1st diff or full series
        correlation = calculate_correlation(good_values_count, target_first_diffs, neigh_first_diffs)
        station_corr_neighbors(k)%correlation = correlation
      else
        ! skip first difference calculation, use full series
        do i = 1, good_values_count
          target_first_diffs(i) = target_values(i)
          neigh_first_diffs(i) = neigh_values(i)
        end do
      endif
    end do correlation_loop

  end subroutine get_station_correlations

  !> Computes a first difference time series of values
  !!   taking into account missing monthly values.
  !!
  !! @param values_count The number of monthly mean temperatures.
  !! @param values The monthly mean temperatures.
  !! @return first_diffs The first differences calculated.
  function calculate_first_diffs (values_count, values) result(first_diffs)

    real, dimension(:) :: values(values_count)
    real, dimension(:) :: first_diffs(values_count)
    integer :: values_count

    integer :: i

    ! Initialize
    first_diffs(:) = MISSING_REAL

    ! Find first good value
    do i = 1,values_count-1
      if (.not. is_missing_real(values(i)) .and.   &
          .not. is_missing_real(values(i+1))) then ! if not missing
        ! Get first difference accounting for missing data
        first_diffs(i) = (values(i+1) - values(i))/2
      endif
    end do

  end function calculate_first_diffs

  !> Given an array of StationNeighborPairs, sorts them in order of their
  !!   correlation values, from highest to lowest.
  !!
  !! @param[inout] correlation_neighbors An array of StationNeighborPairs with their
  !!   correlation values calculated.
  !! @param[in] neighbors_size The number of actual neighbor-pairs in the
  !!   correlation_neighbors array (which may be different than the array size).
  subroutine sort_station_correlations(correlation_neighbors, neighbors_size)

    type(StationNeighborPair), dimension(:), intent(inout) :: correlation_neighbors
    integer :: neighbors_size

    type(StationNeighborPair) :: curr_pair
    integer :: max
    integer :: i, j

    ! sort in descending order
    do i = 1, neighbors_size
      max = i
      do j = i, neighbors_size
        if(correlation_neighbors(j)%correlation .gt. correlation_neighbors(max)%correlation) then
          max = j
        end if
      end do

      ! Move the current StationNeighborPair to it's new position
      if(max .ne. i) then
        curr_pair = correlation_neighbors(i)
        correlation_neighbors(i) = correlation_neighbors(max)
        correlation_neighbors(max) = curr_pair
      end if
    end do

  end subroutine sort_station_correlations

  !> Gets the filename and path of the input data file, given the relevant
  !!   information about the data being requested.
  !!
  !! @param this_station The station object for which data is needed.
  !! @param element The element for which data is needed (tmax, tmin, tavg, tdtr).
  !! @return The full path and filename of the needed data file.
  function get_input_data_filepath(this_station, element) result(data_filename)

    type(Station) :: this_station
    character(len=*) :: element
    character(len=256) :: data_filename

    character(len=8) :: data_type
    character(len=256) :: data_dir

    data_type = get_property_chars(PROP_NEIGH_INPUT_DATA_TYPE)
    data_dir = get_property_chars(PROP_PATH_NEIGH_ELEMENT_DATA_IN)

    data_filename = trim(data_dir) // this_station%station_id // '.' //   &
                      trim(data_type) // '.' // trim(element)

  end function get_input_data_filepath

  !> Writes the neighbors-correlation output for all stations.
  !!   Output per station includes a line of station identifiers, a line of
  !!   station indexes and a line of correlation values.
  !!
  !! @param[in] outfile The path of the write file.
  !! @param[in] correlation_neighbors The station-neighbors for each target station
  !!   to write.
  !! @param[in] correlation_neighbor_limit The maximum number of neighbors to
  !!   include for each target station.
  subroutine write_correlation_output(outfile, correlation_neighbors, correlation_neighbor_limit)

    character(len=*), intent(in) :: outfile
    type(StationNeighborPair), dimension(:,:), intent(in) :: correlation_neighbors
    integer, intent(in) :: correlation_neighbor_limit

    type(StationNeighborPair) :: target
    real, parameter :: self_correlation = 1.00

    integer :: file_status
    integer :: corr_file_unit
    integer :: i, counter

    corr_file_unit = get_available_file_unit()

    ! open output file
    open(corr_file_unit, file=outfile, iostat=file_status, status='replace')
    if(file_status /= 0) then
      call log_fatal('ChooseNeighbors::write_correlation_output: Cannot open output file '  &
                      //trim(outfile)//'. Program is aborting.')
      stop 1
    endif

    do counter = 1, size(correlation_neighbors, 1)
      ! the first pair is the target
      target = correlation_neighbors(counter, 1)

      write(corr_file_unit, '(500(a11,1x))') target%target_id,  &
             (correlation_neighbors(counter,i)%neighbor_id,i=1,correlation_neighbor_limit)

      write(corr_file_unit, '(500(i11,1x))') target%target_index, &
             (correlation_neighbors(counter,i)%neighbor_index, i=1,correlation_neighbor_limit)

      write(corr_file_unit, '(500(f11.2,1x))') self_correlation, &
             (correlation_neighbors(counter,i)%correlation,i=1,correlation_neighbor_limit)

    end do

    close(corr_file_unit)

  end subroutine write_correlation_output

end module ChooseNeighbors

!> @file
!! Contains functionality for choosing the "best" neighbors for each target
!! station.
