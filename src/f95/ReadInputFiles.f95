!> @brief
!! Reads the specific input files needed to create input data for the rest of the
!! algorithm modules.
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
module ReadInputFiles

  use Logger
  use PropertyReader
  use SkylineUtils
  use MathUtils
  use FileUtils
  use StationType
  use StationNeighborPairType
  use ElementValueType
  use ChangepointType
  use AlgorithmParameters
  use PropertyParameters

  implicit none

  private :: read_standard_history, read_mmenne_history, read_pthorne_history

contains

  !> Validates directories that are configured in the properties file.
  !!   If any paths do not exist, the program will print all bad paths and exit without
  !!   running PHA. Blank path values are considered acceptable; it is assumed that if
  !!   a property is left blank, it is because it isn't needed.
  subroutine validate_configured_paths()

    character(len=256) :: directory
    logical :: errors_found
    integer :: i

    errors_found = .false.

    do i=1, size(PROPS_DIRECTORIES)
      directory = get_property_chars(PROPS_DIRECTORIES(i))

      ! Blank is considered OK
      if(trim(directory) .eq. '') cycle

      ! Check for existance and log if bad path
      if(.not. does_directory_exist(directory)) then
        call log_fatal("Configured directory does not exist: "//trim(directory))
        errors_found = .true.
      end if
    end do

    ! Kill program if any errors found
    if(errors_found) call exit(1)

  end subroutine validate_configured_paths

  !> Sets the skyline_months common variable, allocates skyline arrays,
  !!   and populates skyline work arrays with data for each station.
  !!
  !! @param[in] neighbors A collection of target-neighbor pairs for each station
  !!   with their ids, indexes.
  subroutine set_skyline_variables(neighbors)
    use CommonVariables, only: skyline_months, end_year, begin_year

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors

    character(1024), dimension(:), allocatable :: data_file_lines
    integer :: station_index
    integer :: current_sky_index
    integer :: station_sky_first
    integer :: first_year
    integer :: last_year
    integer :: station_years
    integer :: summed_station_years
    integer :: year
    integer :: line_count
    integer :: i
    character(11) :: station_id
    character(256) :: file_name
    character(len=1) :: blank

    call allocate_skyline(size(neighbors, 1))

    current_sky_index = 1
    summed_station_years = 0

    ! for candidate & each reference station read in its data
    ! changed for the GHCNMv2 station and GHCN-daily filename formats
    do station_index = 1, size(neighbors,1)
      file_name = get_data_file_name(neighbors(station_index,1)%target_id)

      line_count = count_file_lines(file_name)
      allocate(data_file_lines(line_count))
      data_file_lines = get_file_lines(file_name)

      ! First and last year (for sky_year)
      first_year = MISSING_INT
      last_year = MISSING_INT
      ! set the sky_first of the first station datum (monthly)
      station_sky_first = current_sky_index
      call set_station_sky_first(station_index, station_sky_first)

      do i=1, line_count
        read(data_file_lines(i), '(a11,a1,i4,a)') station_id, blank, year, blank

        if(year < begin_year .OR. year > end_year) then
          call log_warn("YEAR " // trim(log_string(year)) // " outside of expected range")
        end if
        ! Keep track of begin/end years
        if(i == 1) first_year = year
        ! last successful line read will have it's year set as the last_year
        last_year = year
        current_sky_index = station_sky_first + (last_year - first_year) * 12

      end do

      deallocate(data_file_lines)

      ! Since we have this information now, set the skyline years for station
      call set_station_sky_years(station_index, first_year, last_year)
      station_years = last_year - first_year + 1
      summed_station_years = summed_station_years + station_years

      if(first_year /= MISSING_INT) then
        ! adjust working sky index for next station
        current_sky_index = station_sky_first + (last_year - first_year + 1) * 12
      else
        ! if no data AT ALL - leave a year worth of space
        current_sky_index = current_sky_index + 12
      endif
    end do

    skyline_months = summed_station_years * 12
    call log_info("skyline_months: "//trim(log_string(skyline_months)))

  end subroutine set_skyline_variables

  !> Gets each target station index and its corresponding ordered list of
  !!   neighbor station indexes.
  !!
  !! @param[out] neighbors A collection of target-neighbor pairs for each station
  !!   with their ids, indexes.
  subroutine read_neighbors(neighbors)

    type(StationNeighborPair), dimension(:,:), allocatable, intent(out) :: neighbors

    character(len=11), dimension(:), allocatable :: neighbor_ids
    integer, dimension(:), allocatable :: neighbor_indexes
    integer :: target_index
    integer :: neighbor_index
    integer :: neighbor_count
    integer :: buffered_neighbor_count
    character(len=11) :: target_id
    character(len=11) :: neighbor_id
    integer :: station_loop
    integer :: neighbor_loop
    integer :: station_count
    integer :: line_count
    character(len=256) :: neighbor_file
    character(len=1000), dimension(:), allocatable :: file_lines
    integer :: i

    neighbor_count = get_property_int(PROP_NEIGH_FINAL_LIMIT)
    allocate(neighbor_indexes(neighbor_count))
    allocate(neighbor_ids(neighbor_count))

    neighbor_file = get_property_chars(PROP_PATH_NEIGH_CORRELATION_IN)
    line_count = count_file_lines(neighbor_file)
    station_count = line_count/3
    allocate(file_lines(line_count))
    buffered_neighbor_count = get_property_int(PROP_NEIGH_BUFFERED_LIMIT)
    if(allocated(neighbors)) deallocate(neighbors)
    allocate(neighbors(station_count, buffered_neighbor_count))

    neighbors(:,:) = null_StationNeighborPair()
    file_lines = get_file_lines(neighbor_file)

    ! read in all subnetworks from the correlation output
    do station_loop=1, line_count, 3
      read(file_lines(station_loop), '(500(a11,1x))') (neighbor_ids(i),i=1,neighbor_count)
      read(file_lines(station_loop+1), *) (neighbor_indexes(i),i=1,neighbor_count)

      target_id = neighbor_ids(1)
      target_index = neighbor_indexes(1)
      do neighbor_loop = 1, neighbor_count
        neighbor_id = neighbor_ids(neighbor_loop)
        neighbor_index = neighbor_indexes(neighbor_loop)

        if(neighbor_index==0) cycle

        neighbors(target_index,neighbor_loop) = new_StationNeighborPair( &
            target_id, neighbor_id, target_index, neighbor_index, 0.0) ! distance and correlation don't matter
      enddo
    enddo

  end subroutine read_neighbors

  !> Reads the element data from the file for each station.
  !!
  !! @param[in] neighbors A collection of target-neighbor pairs for each station
  !!   with their ids, indexes.
  !! @param[out] element_data_process The element data read from the files for each station.
  subroutine read_element_data(neighbors, element_data_process)
    use CommonVariables, only: skyline_months

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ElementValue), dimension(:), allocatable :: element_data_process

    logical :: mmenne_random_test

    allocate(element_data_process(skyline_months))

    mmenne_random_test = .false. ! TODO Parameterize this or detect presence of file

    if(mmenne_random_test) then

      call read_mmenne_data(element_data_process)

    else

      call read_standard_data(neighbors, element_data_process)

    endif

  end subroutine read_element_data

  !> Reads the element data from the file for each station (production/operations).
  !!
  !! @param[in] neighbors A collection of target-neighbor pairs for each station
  !!   with their ids, indexes.
  !! @param[out] element_data_process The element data read from the files for each station.
  subroutine read_standard_data(neighbors, element_data_process)
    use CommonVariables, only: skyline_months, end_year, begin_year

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ElementValue), dimension(:), intent(out) :: element_data_process

    character(11) :: station_id
    integer :: station_index
    integer :: current_sky_index
    integer :: station_sky_first
    integer :: first_year
    integer :: last_year
    integer :: sky_index

    character(256) :: file_name
    integer :: line_count
    character(1024), dimension(:), allocatable :: data_file_lines
    integer, dimension(:):: in_values(12)
    character(3), dimension(:) :: in_flags(12)
    character(len=1) :: blank
    integer :: month
    integer :: year
    character(len=3) :: flag
    real :: value
    integer :: i

    ! initialize the element data array
    element_data_process(:) = missing_ElementValue()
    current_sky_index = 1
    in_values(:) = 0
    in_flags(:) = "   "

    ! for candidate & each reference station read in its data
    ! changed for the GHCNMv2 station and GHCN-daily filename formats
    do station_index = 1, size(neighbors,1)
      file_name = get_data_file_name(neighbors(station_index,1)%target_id)

      line_count = count_file_lines(file_name)
      allocate(data_file_lines(line_count))
      data_file_lines = get_file_lines(file_name)

      ! First and last year (for sky_year)
      first_year = get_first_year(station_index)
      last_year = get_last_year(station_index)
      ! set the sky_first of the first station datum (monthly)
      station_sky_first = get_first_sky(station_index)

      do i=1, line_count
        read(data_file_lines(i), '(a11,a1,i4,12(i6,a3))') station_id, blank, year,(in_values(month),in_flags(month),month=1,12)

        if(year < begin_year .OR. year > end_year) then
          call log_warn("YEAR " // trim(log_string(year)) // " outside of expected range")
        end if

        current_sky_index = station_sky_first + (year - first_year) * 12

        ! check the sky_first
        if(current_sky_index+11 > skyline_months) then
          call log_fatal("ReadInputFiles::read_standard_data: Incoming data exceeds Skyline Maximum: station_index: "  &
                         //trim(log_string(station_index))//" current: "//trim(log_string(station_sky_first))//" max: "  &
                         //trim(log_string(skyline_months)))
          stop 1
        endif

        do month = 1, 12
          sky_index = current_sky_index + month - 1

          ! convert external format and missing to internal values
          ! filter out the NEW Integrated Monthly Dataset (after QC)
          if(is_missing_int(in_values(month))) then
            value = MISSING_REAL
            in_flags(month)(1:2) = ' M'
          else if (in_flags(month)(2:2) .ne. ' ') then
            ! GHCN Input & flag decoding
            value = MISSING_REAL
            in_flags(month)(1:2) = ' Q'
          else
            value = in_values(month)/VALUE_SCALE
          endif
          flag = in_flags(month)
          element_data_process(sky_index) = new_ElementValue(value, flag)
        end do
      end do

      deallocate(data_file_lines)

    end do

  end subroutine read_standard_data

  !> Gets the name of the data file for the given station.
  !!
  !! @param station_id The station_id of the desired data file.
  !! @return The name of the data file for the given station.
  function get_data_file_name(station_id) result(file_name)

    character(len=11) :: station_id
    character(len=256) :: file_name

    character(len=4) :: element
    character(len=4) :: data_type
    character(len=256) :: data_dir

    element = get_property_chars(PROP_ELEMENT)
    data_type = get_property_chars(PROP_INPUT_DATA_TYPE)
    data_dir = get_property_chars(PROP_PATH_ELEMENT_DATA_IN)

    if(data_dir == '') then
      call log_fatal("ReadInputFiles::get_data_file_name: Incoming data directory is not defined in properties file.")
      stop 1
    end if

    file_name = trim(data_dir) // station_id // '.' // trim(data_type) // '.' // trim(element)

  end function get_data_file_name

  !> Reads the history data for the current network from either the standard (formerly USHCN) format
  !!   or meta.txt file format (if mmenne_random_test is .true.). Stores these data
  !!   into the UCP's Station History "Hit" array target_ids - Candidate stations sub-network.
  !!
  !! @param[in] neighbors Array of StationNeighborPair items which contain the list
  !!   of target stations.
  !! @param[out] history_changepoints The array of history records (documented changepoints)
  !!   for each target station.
  subroutine read_history(neighbors, history_changepoints)

    type(StationNeighborPair), dimension(:,:) :: neighbors
    type(Changepoint), dimension(:,:), allocatable :: history_changepoints

    character(len=256) :: mmenne_meta
    logical :: mmenne_random_test
    integer :: use_shf_meta

    allocate(history_changepoints(size(neighbors,1), MAX_CHANGEPOINTS))
    history_changepoints(:,:) = null_Changepoint()

    mmenne_random_test = .false. ! TODO parameterize or detect presence of file

    ! If properties indicate no history to be used,
    ! then don't bother running this subroutine
    use_shf_meta = get_property_int(PROP_USE_HISTORY_FILES)
    if(use_shf_meta == OPT_HISTORY_IGNORE) return

    ! initialize mmts dates array
    mmenne_meta = '' ! TODO read file path from properties file

    if(mmenne_random_test) then

      call read_mmenne_history(neighbors, history_changepoints)

    ! Peter Thorne random series station history files
    else if(use_shf_meta /= OPT_HISTORY_IGNORE .and. mmenne_meta /= '') then

      call read_pthorne_history(neighbors, history_changepoints)

    ! Typical/Standard case, used for production
    else if(use_shf_meta /= OPT_HISTORY_IGNORE) then

      call read_standard_history(neighbors, history_changepoints)

    endif

  end subroutine read_history

  !> Given an array of stations on which to get data, read in the history information
  !! for those stations and determine documented station changepoints and changepoint
  !! dates (day of month).
  !!
  !! @param[in] neighbors Array of StationNeighborPair items which contain the list
  !!   of target stations.
  !! @param[out] history_changepoints The array of history records (documented changepoints)
  !!   for each target station.
  subroutine read_standard_history(neighbors, history_changepoints)
    use CommonVariables, only: end_year, begin_year

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(Changepoint), dimension(:,:), intent(out) :: history_changepoints

    character(len=11) :: station_id
    integer :: station_index
    integer :: target_index
    integer :: first_sky
    integer :: last_sky
    integer :: distance
    real    :: distance_real ! Because sometimes distance in SHF is integer, sometimes it's real
    integer :: elevation
    integer :: beg_month
    integer :: beg_year
    integer :: beg_day
    integer :: end_y
    integer :: end_m
    integer :: end_d
    integer :: byear
    integer :: last_elev
    integer :: last_latsec
    integer :: last_lonsec
    integer :: last_beg_day
    integer :: last_beg_month
    integer :: last_beg_year
    integer :: num_moves
    integer :: move_sky_index
    integer :: no_history
    integer :: pos_change
    integer :: instr_type_index
    integer :: instr_index
    integer :: source
    integer :: history_code
    integer :: line_count
    integer :: curr_line
    real :: lat_degrees
    real :: lat_minutes
    real :: lat_seconds
    real :: lon_degrees
    real :: lon_minutes
    real :: lon_seconds
    real :: lat_dec_degrees
    real :: lat_last
    real :: lon_dec_degrees
    real :: lon_last
    real :: lat_diff
    real :: lon_diff
    logical :: check_for_move
    logical :: is_coop
    character(len=30) :: move_string

    ! Array of MMTS dates: first is when going to MMTS, second is leaving MMTS,
    ! no MMTS is 9999. Contains the dates of going to/from MMTS.
    integer, dimension(:,:) :: mmts_dates(size(neighbors,1),2)
    character(len=11) :: distance_and_direction
    character(len=3) :: direction
    character(len=2) :: instr_height
    character(len=4) :: instr_height_full
    character(len=2) :: last_instr_height = '  '
    character(len=4) :: obs_time_full
    character(len=4) :: obs_time
    character(len=4) :: last_obs_time
    character(len=4) :: not_move
    character(len=256) :: history_file
    character(len=1000), dimension(:), allocatable :: file_lines

    ! instrument string parameters
    integer, parameter :: MAX_INSTR_COUNT = 11
    character(len=5) :: station_instr(MAX_INSTR_COUNT)

    integer, parameter :: INSTR_TYPES_NUM = 38
    integer, dimension(:) :: instr(INSTR_TYPES_NUM)
    integer, dimension(:) :: last_instr(INSTR_TYPES_NUM)
    character(len=5) :: instr_strings(INSTR_TYPES_NUM) = (/ 'AI   ','CRS  ','DT   ','EVA  ', 'FP   ','HYTHG', &
                    'MXMN ','NRIG ','NSRG ','NSS  ','RRIG ', 'RRNG ','SDE  ','SG   ','SRG  ', &
                    'SS   ','TG   ','DGT  ', 'TB   ','EVO  ','MMTS ','TELSY','HYGRO','HY6  ', &
                    'HY8  ', 'SFP  ','SRRNG','SSG  ','SSRG ','STB  ','AMOS ','AUTOB', 'PSY  ', &
                    'ASOS ','PLAST','STO  ','HYGR ','NIMBS' /)

    ! because of the measurement changes in Lat/Lon there is a dramatic change in lat/lon
    ! resolution as of 1998. In the new code, this will have to be dealt with better,
    ! probably with testing the measurement type, then the amount of change (=45 arcseconds)
    real, parameter :: latlon_epsilon = 0.0125

    ! NH         Net History
    ! 0 => No Move
    ! 1 => Real Move and Begin Day = 01
    ! 2 => Real Move and Begin Day > 01
    ! 9 => Missing
    !
    ! not_move     Determines if Station Moved:
    ! (1:1) = 0 => Change in Observation Time (Temperature)
    ! (1:1) = 1 => No Change in Observation Time or Current/
    !              Previous Observation Time Unknown
    !              (Temperature)
    ! (1:1) = 1 => (Precipitation)
    ! (2:2) = 0 => Change in Instrument Height
    ! (2:2) = 1 => No Change in Instrument Height, or
    !              Current/Previous Instrument Height Unknown
    ! (3:3) = 0 => Change in Instrument "CRS" or "NSS"
    !              (Temperature)
    ! (3:3) = 1 => No Change in Instrument "CRS" and "NSS"
    !              (Temperature)
    ! (3:3) = 0 => Change in Instrument "FP", "NSRG", "RRNG",
    !              or "TB" (Precipitation)
    ! (3:3) = 1 => No Change in Instrument "FP" and "NSRG"
    !              and "RRNG" and "TB" (Precipitation)
    ! (4:4) = 0 => Change in Distance and/or Direction from
    !              Previous Location
    ! (4:4) = 1 => No Change in Distance and Direction from
    !              Previous Location, or Unknown Distance
    !              from Previous Location

    ! Lahey compiler with checks turned on throws an error unless we re-initialize
    ! this array before use within this subroutine.
    history_changepoints(:,:) = null_Changepoint()

    mmts_dates(:,:) = 9999

    no_history = 0
    do station_index = 1,size(neighbors,1)
      station_id = neighbors(station_index, 1)%target_id

      ! initialize instrument string
      last_instr(:) = 0
      instr(:) = 0

      ! define paths to USHCN & CDMP-3220 datasets (and current type - is_coop)
      if(is_hcn_record(station_id)) then
        is_coop = .false.
      else
        is_coop = .true.
      endif

      history_file = get_history_file_name(station_id)

      if(.not. does_file_exist(history_file)) then
        no_history = no_history + 1
        cycle ! Expected that some stations don't have history.
      end if
      line_count = count_file_lines(history_file)
      allocate(file_lines(line_count))
      file_lines = get_file_lines(history_file)

      ! Initialize variables
      num_moves = 0
      check_for_move = .false.
      last_beg_year = 0
      last_beg_month = 0
      last_beg_day = 0

      ! Read station history records from history files
      do curr_line=1,line_count

        read(file_lines(curr_line), 90) source, beg_year, beg_month, beg_day, end_y, end_m, end_d,  &
                lat_degrees, lat_minutes, lat_seconds,lon_degrees, lon_minutes, lon_seconds,   &
                distance_and_direction, elevation, instr_height_full, obs_time_full, station_instr

90      format(i1,12x,2(1x,i4,2i2),1x,f3.0,2f3.0,1x,f4.0,2f3.0,1x,a11,1x,i5,2x,a4,1x,a4,5x,11(a5,1x))

        history_code = 0

        ! The daily time of observation history records are not used here...
        if(source == 1) cycle
        ! CDMP data should not be used with HCN stations!
        if(source == 3 .and. .not. is_coop) cycle

        ! MSHR history records are used after the last begin date of the
        ! USHCN & CDMP history records
        if(source == 2 .and. (beg_year < last_beg_year .or. (beg_year == last_beg_year .and. beg_month < last_beg_month) .or. &
            (beg_year == last_beg_year .and. beg_month == last_beg_month .and. beg_day <= last_beg_day))) then
          cycle
        endif

        ! shift the instrument strings for the CDMP_3220 history
        if(is_coop) then
          do instr_index = 1, MAX_INSTR_COUNT
            if(len_trim(station_instr(instr_index)) == 0) exit
            do while (station_instr(instr_index)(1:1)/=' ')
              station_instr(instr_index)(1:4) = station_instr(instr_index)(2:5)
              station_instr(instr_index)(5:5) = ' '
            enddo
          enddo
        endif

        ! Get lat/lon ready
        lat_dec_degrees = lat_degrees + ((lat_minutes + lat_seconds/60.0)/60.0)
        lon_dec_degrees = lon_degrees + ((lon_minutes + lon_seconds/60.0)/60.0)

        ! decode last dir/dir from the USHCN
        if(source == 0 .and. .not. is_coop) then
          ! see if there is a decimal in distance
          if(distance_and_direction(1:1) == '.' .or. distance_and_direction(2:2) == '.'   &
               .or. distance_and_direction(3:3) == '.') then
            read(distance_and_direction(1:3), '(f3.1)') distance_real
            distance = int(distance_real * 10.)
          else
            read(distance_and_direction(1:3),*) distance
          endif
          direction = distance_and_direction(5:7)
        endif

        ! find instruments
        do instr_index= 1, MAX_INSTR_COUNT
          do instr_type_index = 1, INSTR_TYPES_NUM
            if(station_instr(instr_index) == instr_strings(instr_type_index)) then
              instr(instr_type_index) = 1
              exit
            endif
          end do
        end do

        ! Find instrument height
        ! Positions 3 and 4: Temperature height
        ! Positions 1 and 2: Precip height
        instr_height = instr_height_full(3:4)

        ! Get history for correct time period
        if(end_y >= begin_year .and. beg_year <= end_year) then

          ! Decode temperature observation time:  Observation time = "xxHR" or
          ! "TRID" => Copy to temperature time; Observation time = "9xx9" =>
          ! Copy "xx" to temperature time; Otherwise, copy to temperature time.
          if(obs_time_full(3:4) == 'HR' .or. obs_time_full(3:4) == 'ID') then
            obs_time = obs_time_full
          else if(obs_time_full(1:1) == '9' .and. obs_time_full(2:2) /= '9') then
            obs_time = obs_time_full(2:3)
          else
            obs_time = obs_time_full(3:4)
          end if

          ! Fill in missing dates
          if(beg_month == 99) beg_month = 6
          if(beg_day == 99) beg_day = 15
          if(end_m == 99 .and. end_d == 99 .and. end_y == 9999) then
            end_m = 12
            end_d = 31
            end_y = end_year
          else
            if(end_m == 99) end_m = 6
            if(end_d == 99) end_d = 15
          end if

          ! Merge Instruments
          !   Modified the instrument types "EQUATES" for "HYGROS" and "SHIELDS"
          !   Set all "EQUATES" to instr(2) ("CRS") - "REFRESHES" saved list
          !   In the event an instrument change in "EQUATES" occurs.
          !   "MMTS" have been adjusted a priori
          !   "HYGROS" will be evaluated "SHAP"
          !   Changes from "DT" TO "CRS", etc. are evaluated
          !   Changes from "CRS", ETC. TO "HYGRO", etc. are evaluated
          !   Treat "DT"s with/without shelters as same
          !   Am changing back some of the instrumentation before to 90's.....
          !   1) Setting the MMTS apart again (#21)
          !   Changes for the Millenneum are:
          !   1) Added ASOS as a instrument (#34)
          !   2) For Precip also added 4"Plastic (#35) and STO in Hiwaii (#36)
          !   3) Source 2 (the MSHR) generic Hygrothemometer (assumed HY8) (#37)

          !   Master Station History Records (source=2) have two new fields:
          !   1) the primary precip instrument and
          !   2) the primary temperature instrument
          !   USHCN SHF (source=1) primary instruments are determined the from the mix
          !     of instruments at any given time.

          ! MXMN == HYTHG == SS == TG == DGT
          ! E2 - modification remove MMTS from "equate"
          !        i.e. add back MMTS only record as changepoint
          if(instr(7) == 1 .or. instr(6) == 1 .or. instr(16) == 1 .or. instr(17) == 1 .or. &
             instr(18) == 1) then
            instr(2) = 1
          else
            instr(2) = 0
          end if

          ! HYGR == HY8
          if(instr(37) == 1) instr(25) = 1

          ! Added "EQUATES" for the shielded precip instruments
          ! SFP == FP : SRRNG == RRNG : SSRG == SRG
          if(instr(26) == 1) instr(5) = 1
          if(instr(27) == 1) instr(12) = 1
          if(instr(29) == 1) instr(15) = 1

          ! Determin begin and end years for station moves loop
          if(beg_year < begin_year) then
            beg_month = 1
            beg_day = 1
            byear = begin_year
          else
            byear = beg_year
          end if

          if(end_y > end_year) then
            end_m = 12
            end_d = 31
          end if

          move_string = ''

          ! Test for real move
          if(check_for_move) then

            not_move = '0000'

            ! Check observation time
            ! No observation time change if:  Precipitation; Unknow
            ! observation time; Unknown previous observation time; OR
            ! observation time is same as previous observation time.
            if(obs_time == '99' .or. obs_time == '  ' .or. last_obs_time == '99' .or. &
               last_obs_time == '  ' .or. obs_time == last_obs_time) then
              not_move(1:1) = '1'
            else
              move_string = trim(move_string) // ' OBT'
            endif

            ! Check instrument height: No instrument height change if unknown instrument height;
            ! Unknown previous instrument height; Or instrument height is same as previous instrument height.
            if(instr_height == '99' .or. instr_height == '  ' .or. last_instr_height == '99' .or. last_instr_height == '  '  &
                .or. instr_height == last_instr_height) then
              not_move(2:2) = '1'
            else
              history_code = 1
              move_string = trim(move_string)  // ' IHT'
            end if

            ! Test instruments
            ! No instrument change if:  Temperature, and no change in "CRS" and no change in "NSS";
            ! or precip, and no change in "SRG" or no change in "FP" and no change in "NSRG"
            ! and no change in "RRNG" and no change in "TB".
            ! If current && last ASOS inst then no move
            if(instr(34) == 1 .and. last_instr(34) == 1) then
              not_move(3:3) = '1'
            ! then CRS
            else if(instr(34) == last_instr(34) .and. instr(2) == 1 .and. last_instr(2) == 1) then
              ! if ASOS not changed and current && last CRS inst then no move
              not_move(3:3) = '1'
              ! last - everybody else
            else if(instr(34) == last_instr(34) .and. instr(2) == last_instr(2) .and. instr(21) == last_instr(21) .and. &
                      instr(22) == last_instr(22) .and. instr(10) == last_instr(10)) then
              not_move(3:3) = '1'
            else
              history_code = 1
              if(instr(35) == 1 .and. last_instr(35) == 0) then
                move_string = trim(move_string) // ' ASOS'
              else
                move_string = trim(move_string) // ' INST'
              endif
            end if

           ! Test last distance and direction temperature moved);
           ! with the newer keyed sources (MSHR & CDMP) - nonblank distance_and_direction == move
           if(is_coop .or. (.not. is_coop .and. source .ne. 0)) then
              if(distance_and_direction /= "           ") then
                move_string = trim(move_string) // ' LDIS'
                history_code = 1
              endif
            else if(source == 0 .and. .not. is_coop) then
              if(distance == 999) then
                move_string = trim(move_string) // ' LDIS'
                history_code = 1
              ! With the USHCN if distance != 999 then No dis/dir
              ! Chage if: No change in distance and no change in direction;
              ! Temperature inst and distance only if "8XX" (I.E., precip move);
              ! Precip instr and distance only if "9XX"
              else if((distance == 0 .and. direction == '000') .or. (distance / 100) == 8) then
                not_move(4:4) = '1'
              else
                move_string = trim(move_string) // ' LDIS'
                history_code = 1
              end if
            end if

            ! Initialize station move variable
            pos_change = 0

            ! Test elevation, latitude, and longitude
            ! MOVE if:  Change in elevation, latitude, or longitude
            if(elevation /= last_elev) then
              move_string = trim(move_string) // ' ELEV'
              pos_change = pos_change+1
            endif

            lat_diff = abs(lat_dec_degrees - lat_last)
            lon_diff = abs(lon_dec_degrees - lon_last)
            if(lat_diff > latlon_epsilon .or. lon_diff > latlon_epsilon) then
              move_string = trim(move_string) // ' LALO'
              pos_change = pos_change+1
            endif

            if(pos_change == 0) then
              not_move(4:4) = '1'
            ! Real move occurred
            else
              move_string = trim(move_string) // ' MOVE'
              history_code = 1
            end if
          end if

          ! last shot to affect history record - is this a GPS only entry
          if(byear > 1995 .and. move_string == ' LALO MOVE' .and. last_latsec == 0 .and. last_lonsec == 0 .and. &
                 lat_diff <= latlon_epsilon .and. lon_diff <= latlon_epsilon) then
            history_code = 0
            move_string = ' GPS'
          endif

          ! get first and last skyline indices for current station
          target_index = neighbors(station_index,1)%target_index
          first_sky = get_first_sky(target_index)
          last_sky = get_last_sky(target_index)

          ! need to save off the beginning of the history records
          if(num_moves == 0) then
            num_moves = num_moves + 1
            move_sky_index = get_sky_from_year_month(target_index, byear, beg_month)
            ! if the stnhist is before the beginning, set to the beginning
            if(move_sky_index <= first_sky) then
              history_changepoints(station_index, num_moves) = new_Changepoint(first_sky, 1, 0.0)
            else
              history_changepoints(station_index, num_moves) = new_Changepoint(move_sky_index, beg_day, 0.0)
            endif
          end if

          move_sky_index = get_sky_from_year_month(target_index, byear, beg_month)
          if(history_code /= 0) then
            ! accumulate move dates from the shf records, use in next step with data
            ! if there are more than one before the beginning, skip
            if(move_sky_index > first_sky .and. move_sky_index <= last_sky  &
                  .and. move_sky_index /= history_changepoints(station_index, num_moves)%sky_month) then
              num_moves = num_moves + 1
              history_changepoints(station_index, num_moves) = new_Changepoint(move_sky_index, beg_day, 0.0)
            endif
          end if
          ! test for MMTS instr and whether to update mmts_dates
          if(instr(21) == 1) then
            ! MMTS in use - set begin date if not already set
            if(mmts_dates(station_index,1) == 9999) then
              ! MM dates must be a monthly index
              move_sky_index = get_sky_from_year_month(target_index, byear, beg_month)
              mmts_dates(station_index,1) = move_sky_index
            endif
          endif

          ! Past initial station history record: Initialize pass variable so test for real moves
          check_for_move = .true.

          ! Save history data
          last_elev = elevation
          last_obs_time = obs_time
          last_instr_height = instr_height
          lat_last = lat_dec_degrees
          lon_last = lon_dec_degrees
          last_latsec = lat_seconds
          last_lonsec = lon_seconds
          ! this is needed for both USHCN & CDMP
          if(source == 0) then
            last_beg_year = beg_year
            last_beg_month = beg_month
            last_beg_day = beg_day
          endif

        end if ! end of process record within begyr-endyr

        ! Prepare last_instr for next loop pass
        do instr_type_index = 1, INSTR_TYPES_NUM
          last_instr(instr_type_index) = instr(instr_type_index)
          instr(instr_type_index) = 0
        end do
      end do ! end of read station history data loop

      ! align SHF moves with missing and min-length data segments
      if(num_moves == 0) then
        call log_debug('SHF: '//trim(log_string(station_index))//' Station has no moves: '//station_id)
        no_history = no_history + 1
      endif

      deallocate(file_lines)
    end do
    call log_info(trim(log_string(no_history))//' stations have no history')

  end subroutine read_standard_history

  !!   Refactor later. Lower priority for refactoring. Used for research/testing.
  !!   Doesn't run in production and hasn't been used in recent years.
  subroutine read_mmenne_data(element_data_process)

    type(ElementValue), dimension(:), intent(out) :: element_data_process

    ! mattdata = data file with generated series
    character(len=256) mattdata
    integer :: imoff
    integer :: sky_index
    integer :: iskyoff
    integer :: station_index
    integer :: total_months
    integer :: station_count
    integer :: read_status
    integer :: line_count
    character(1024), dimension(:), allocatable :: data_file_lines
    real, dimension(:) :: element_values(size(element_data_process))

    ! MM's missing value -999.999
    real, parameter :: MM_MISSING = -999.

    line_count = count_file_lines(mattdata)
    allocate(data_file_lines(line_count))
    station_count = line_count

    ! initialize station ID array & original data array
    element_values(:) = MISSING_REAL
    element_data_process(:) = missing_ElementValue()

    ! read Mmenne random series data
    if(mattdata /= '') then
      data_file_lines = get_file_lines(mattdata)
      ! monthly series assumed. annual no longer supported.
      ! warning: assume 1200 values per candidate-network
      total_months = line_count * 12
      do iskyoff = 1, total_months ! This seems like it should be line_count(# of years), not total_months
        read(data_file_lines(iskyoff), *, iostat=read_status) &
               imoff,(element_values((station_index-1)*total_months+iskyoff),station_index=1,station_count)
        if(read_status < 0) then
          call log_info('End of candidate network records')
          return
        endif

        if(iskyoff /= imoff) then
          call log_fatal('ReadInputFiles::read_mmenne_data: Monthly input and YYYY/MM out of sync')
          stop 1
        endif
        ! convert MM's missing to internal missing
        do sky_index = 1, station_count*total_months
          if(element_values(sky_index) <= MM_MISSING)then
            element_values(sky_index)=MISSING_REAL
          endif
          element_data_process(sky_index) = new_ElementValue(element_values(sky_index), "   ")
        enddo
      enddo
    endif

  end subroutine read_mmenne_data

  !> Refactor later. Lower priority for refactoring. Used for research/testing.
  !!   Doesn't run in production and hasn't been used in recent years.
  subroutine read_mmenne_history(neighbors, history_changepoints)

    ! network station list (base station first)
    type(StationNeighborPair), dimension(:,:) :: neighbors
    type(Changepoint), dimension(:,:) :: history_changepoints

    integer :: station_index
    integer :: target_index
    integer :: change_index
    integer :: first_sky
    integer :: last_sky
    integer :: file_status
    integer :: num_changes
    integer :: mmunit
    integer :: isdum
    character(len=11) :: station_id
    character(len=256) :: mattmeta
    integer, dimension(:) :: inmove(size(history_changepoints,2))
    real, dimension(:) :: amounts(size(history_changepoints,2))

    call log_info('--------  Mmenne random series history --------')
    ! since, in the realworld, there are no amounts - they are ignored...
    ! note: readnet brings in data and generates Sky indices

    ! metafile for generated series
    mattmeta = '' ! TODO read this in from properties file
    if(mattmeta /= '') mmunit = 15

    open (unit=mmunit, file=trim(mattmeta), iostat=file_status)
    if(file_status > 0) then
      call log_fatal('ReadInputFiles::read_mmenne_history: Cannot open mattmeta file '//trim(mattmeta))
      stop 1
    end if
    call log_info('Opened file: '//mattmeta)

    ! go thru each station
    do station_index = 1,size(neighbors,1)
      target_index = neighbors(station_index,1)%target_index
      first_sky = get_first_sky(target_index)
      last_sky = get_last_sky(target_index)
      history_changepoints(:,:) = new_Changepoint(0, 31, 0.0)

      ! setup the first move to begin at the beginning
      history_changepoints(station_index, 1)%sky_month = first_sky
      history_changepoints(station_index, 1)%day_of_month = 1

      ! read the station history
      read(mmunit,*,iostat=file_status) station_id, isdum, num_changes,  &
                 (inmove(change_index),amounts(change_index), change_index=2,num_changes+1)
      if(file_status /=0) then
        call log_fatal('ReadInputFiles::read_mmenne_history: Unable to read mattmeta file ' // trim(mattmeta))
        stop 1
      end if
      do change_index = 2, num_changes+1
        history_changepoints(station_index, change_index)%sky_month = inmove(change_index) + first_sky - 1
      enddo

      history_changepoints(station_index, num_changes+2)%sky_month = last_sky

      ! align SHF moves with missing and min-length data segments
      ! and sets SAHIST for main processing
    enddo

    ! if mmunit == 0, defer to realworld histories if use_shf_meta != OPT_HISTORY_IGNORE
  end subroutine read_mmenne_history

  !> Refactor later. Lower priority for refactoring. Used for research/testing.
  !!   Doesn't run in production and hasn't been used in recent years.
  subroutine read_pthorne_history(neighbors, history_changepoints)

    ! network station list (base station first)
    type(StationNeighborPair), dimension(:,:) :: neighbors
    integer :: station_index
    integer :: target_index
    integer :: first_sky
    integer :: last_sky
    integer :: file_status
    integer :: num_changes
    integer :: mmunit
    integer :: year
    integer :: month
    integer :: year_month
    integer :: itemp
    integer :: total_shf_changes
    integer :: sky_index
    integer :: total_shf_stations
    logical :: stations_found
    character(len=11) :: station_id
    ! input PTHORNE station
    character(11) :: pthorne_station
    character(11) :: last_station
    character(len=256) :: mattmeta
    type(Changepoint), dimension(:,:) :: history_changepoints

    call log_info('---------- Pthorne random series history ---------')

    mattmeta = '' ! TODO read this in from properties file
    if(mattmeta /= '') mmunit = 15

    open (unit=mmunit, file=trim(mattmeta), iostat=file_status)
    if(file_status > 0) then
      call log_fatal('ReadInputFiles::read_pthorne_history: Cannot open mattmeta file: '//trim(mattmeta))
      stop 1
    end if
    call log_info('Opened file: '//mattmeta)

    ! init last station
    last_station = ''
    num_changes = 0
    station_index = 0
    total_shf_stations = 0
    total_shf_changes = 0

    do while (1 == 1)

      read(mmunit,'(2x,a11,i7,i2)', IOSTAT=file_status) pthorne_station, year_month, itemp
      if (file_status > 0 ) then
        call perror('ReadInputFiles::read_pthorne_history: Cannot read mattmeta file ' // trim(mattmeta))
        stop 1
      endif
      if (file_status < 0 ) exit

      ! see if station has changed
      if(pthorne_station /= last_station) then
        if(num_changes >= 1) then
          ! finish out the last station move with the end-of-period
          num_changes = num_changes + 1
          history_changepoints(station_index, num_changes)%sky_month = last_sky
          total_shf_changes = total_shf_changes + num_changes
        endif

        ! initialize metadata arrays for current station
        num_changes = 0
        history_changepoints(:,:) = new_Changepoint(0, 31, MISSING_REAL)

        ! find the current station
        stations_found = .false.
        station_id = pthorne_station
        do station_index = 1, size(neighbors,1)
          if(neighbors(station_index,1)%target_id == station_id) then
            stations_found = .true.
            total_shf_stations = total_shf_stations + 1
            target_index = neighbors(station_index,1)%target_index
            first_sky = get_first_sky(target_index)
            last_sky = get_last_sky(target_index)

            ! setup the first move to begin at the beginning
            num_changes = 1
            history_changepoints(station_index, num_changes)%sky_month = first_sky
            history_changepoints(station_index, num_changes)%day_of_month = 1

            exit
          endif
        enddo

        if(stations_found) then
          last_station = pthorne_station
        else
          call log_info('No station found for '//station_id)
        endif
      endif

      if(stations_found) then
        year = year_month/100
        month = mod(year_month,100)
        sky_index = get_sky_from_year_month(target_index, year, month)
        if(sky_index > 0) then
          num_changes = num_changes + 1
          history_changepoints(station_index, num_changes)%sky_month = sky_index
        endif
      endif
    enddo

    if(num_changes >= 1) then
      total_shf_stations = total_shf_stations + 1
      num_changes = num_changes + 1
      history_changepoints(station_index, num_changes)%sky_month = last_sky
      ! process accumulated metadata for last station
      ! align SHF moves with missing and min-length data segments

      total_shf_changes = total_shf_changes + num_changes
    endif
    call log_info("Number of PW-SHF stations, changes: "//trim(log_string(total_shf_stations)) &
              // " " //trim(log_string(total_shf_changes)))

  end subroutine read_pthorne_history

  !> Gets a station's history file name based on its station id
  !!
  !! @param station_id The station id.
  !! @result history_file The resulting history file.
  function get_history_file_name(station_id) result(history_file)
    character(len=*) :: station_id
    character(len=256) :: history_file
    character(len=256) :: history_dir

    history_dir = get_property_chars(PROP_PATH_HISTORY)
    history_file = trim(history_dir) // station_id // '.his'

  end function get_history_file_name

end module ReadInputFiles

!> @file
!! Contains functionality for reading specific input files.
