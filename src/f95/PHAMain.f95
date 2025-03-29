!> @brief
!! Main controller program for GHCN-Monthly Pairwise Homogeneity
!! Algorithm (PHA). PHA is an automated homogenization algorithm based
!! on the pairwise comparison of monthly temperature series is described.
!! The algorithm works by forming pairwise difference series between
!! serial monthly temperature values from a network of observing stations.
!! Each difference series is then evaluated for undocumented shifts, and
!! the station series responsible for such breaks is identified
!! automatically. The algorithm also makes use of station history
!! information, when available, to improve the identification of artificial
!! shifts in temperature data. In addition, an evaluation is carried out to
!! distinguish trend inhomogeneities from abrupt shifts. When the magnitude
!! of an apparent shift attributed to a particular station can be reliably
!! estimated, an adjustment is made for the target series.
!!
!! Menne, Matthew J., Claude N. Williams (2009), Homogenization of Temperature Series
!! via Pairwise Comparisons. J. Climate, 22, 1700-1717. doi: 10.1175/2008JCLI2263.1.
!!
!! USAGE: PHAMain -d yyyymmdd -p properties-filename(s)
!!    ex: PHAMain -d 20160504 -p ghcnm-pha.properties
!!        PHAMain -d 20160504 -p ghcnm-pha.properties,ghcnm-pha.second.properties
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
program PHAMain

  use Logger
  use PropertyReader
  use DatetimeUtils
  use ConfigurationUtils
  use PropertyParameters
  use StationNeighborPairType
  use ElementValueType
  use DifferenceValueType
  use ChangepointType
  use ChangepointSumsType
  use ChangepointSpanType
  use ChangepointHitType
  use ReadInputFiles
  use ChooseNeighbors
  use FindChangepoints
  use AttributeChangepoints
  use MergeChangepoints
  use ChangepointSize
  use AdjustSeries

  implicit none

  type(StationNeighborPair), dimension(:,:), allocatable :: neighbors
  type(ElementValue), dimension(:), allocatable :: element_data_process
  type(ElementValue), dimension(:), allocatable :: element_data_orig
  type(ElementValue), dimension(:), allocatable :: element_data_interim
  type(Changepoint), dimension(:,:), allocatable :: history_changepoints
  type(ChangepointSums), dimension(:), allocatable :: changepoint_sums
  type(ChangepointSpan), dimension(:,:), allocatable :: changepoint_spans
  type(ChangepointHit), dimension(:), allocatable :: changepoint_hits
  type(Changepoint), dimension(:,:), allocatable :: changepoints
  logical(1), dimension(:,:), allocatable :: delete_flags
  integer, dimension(:), allocatable :: merged_hits
  integer, dimension(:,:), allocatable :: unstable_dates
  integer, dimension(:), allocatable :: num_changes

  real :: previous_duration
  character(len=1024), dimension(:), allocatable :: properties_files
  character(len=8) :: process_date

  logical :: do_run_neighbors
  logical :: do_run_main

  previous_duration = 0.000000

  ! Get command line args, initialize properties files reader and logger
  call get_properties_files(properties_files)

  call properties_init(properties_files)
  call detokenize_all_properties()
  call log_init(get_property_chars(PROP_LOG_FILENAME),     &
                get_property_chars(PROP_LOG_LEVEL),        &
                get_property_logical(PROP_LOG_STDOUT),     &
                get_property_logical(PROP_LOG_DATESTAMP),  &
                get_property_logical(PROP_LOG_ROLLOVER))
  call log_info("BEGIN run of GHCN-Monthly PHA")
  call log_info("Using properties file: "//trim(log_string(properties_files)))
  call log_info("Writing to log file: "//trim(get_log_file()))
  call log_properties()

  ! Properties validation and array set-up work
  call validate_configured_paths()
  call set_common_variables()

  ! Call ChooseNeighbors to determine best neighbors for each target station
  do_run_neighbors = get_property_logical("pha.do-run-neighbors")
  if(do_run_neighbors) then
    call choose_neighbors(neighbors)
    call log_duration("ChooseNeighbors: Choose best neighbors")
  endif

  ! Call PHA main program - to be refactored into multiple modules.
  do_run_main = get_property_logical("pha.do-run-main")
  if(do_run_main) then
    ! Read in all candidates and their neighboring stations.
    call read_neighbors(neighbors)
    call log_duration("ReadInputFiles: read neighbors")

    ! Now that we know the number of stations, allocate the SkylineUtils arrays.
    call set_skyline_variables(neighbors)

    ! Read the element values and flags for each year in each station's input file.
    call read_element_data(neighbors, element_data_process)
    call log_duration("ReadInputFiles: read element data for stations")

    ! Get the history data from the station history files
    call read_history(neighbors, history_changepoints)
    call log_duration("ReadInputFiles: read history records for stations")

    ! Copy the original data before changes are made to an array for use later by AdjustSeries
    allocate(element_data_orig(size(element_data_process)))
    element_data_orig(:) = element_data_process(:)

    ! Initial call to align_changepoints based on history file documented changepoints
    call align_changepoints(history_changepoints, element_data_process)

    ! Find all initial changepoints between paired stations
    call find_paired_changepoints(neighbors, element_data_process, changepoint_spans, changepoint_sums)
    call log_duration("FindChangepoints: Find paired change point hits")

    ! Determine which station is responsible for each found changepoint
    call attribute_changepoints(neighbors, changepoint_spans, changepoint_sums, changepoint_hits)
    call log_duration("AttributeChangepoints: Attribute each changepoint to a station")
    deallocate(changepoint_sums)

    ! Merge undocumented changepoints with nearby documented (history) changepoints and
    ! merge closely clustered changepoints. Second call to align_changepoints within merged changepoints.
    call merge_changepoints(neighbors, element_data_process, history_changepoints, changepoint_spans, changepoint_hits,  &
                            merged_hits, delete_flags)
    call log_duration("MergeChangepoints: Merge found changepoints with history changepoints and with clustered changepoints")
    deallocate(history_changepoints)
    deallocate(changepoint_hits)
    deallocate(changepoint_spans)

    ! This copy needed for AdjustSeries, but needs to be created before ChangepointSize changes element_data_process.
    allocate(element_data_interim(size(element_data_process)))
    element_data_interim(:) = element_data_process(:)

    ! Estimate the size of the final changepoints
    call estimate_changepoint_size(neighbors, element_data_process, changepoints, merged_hits, delete_flags,  &
                                   num_changes,  unstable_dates)
    call log_duration("ChangepointSize: Calculate size of final changepoints")
    deallocate(merged_hits)
    deallocate(delete_flags)

    ! Adjust the series data for each station and write final output to files.
    call adjust_series(neighbors, element_data_process, element_data_interim, element_data_orig, changepoints,  &
                       num_changes, unstable_dates)
    call log_duration("AdjustSeries: Adjust final values and print to output files")

  endif

  ! Success message if PHA completes normally with total duration.
  call log_total_duration()

contains

  !> Sets the global variables in CommonVariables that are used across
  !! multiple modules throughout the program.
  subroutine set_common_variables()
    use CommonVariables, only: begin_year, end_year, por_months

    integer :: por_years

    begin_year = get_property_int(PROP_BEGIN_YEAR)
    end_year = get_current_year()
    por_years = end_year - begin_year + 1
    por_months = por_years * 12

    call log_info('Total POR years: '//trim(log_string(por_years)))

  end subroutine set_common_variables

  !> Log all configured properties at startup time. 
  !! See Req. 8.1
  subroutine log_properties()
    integer :: i
    
    call log_info("PROPERTIES:") 
    do i=1, size(keys_values)
      if(len_trim(key_value_string(keys_values(i))) > 0) then
        call log_info(adjustl(key_value_string(keys_values(i))))
      end if
    end do
    call log_info("END PROPERTIES")

  end subroutine log_properties

  !> Measure runtime at the time this subroutine is called
  !! since the last time this subroutine was called, and 
  !! prints a message to the log. See Req. 8.3.
  !!
  !! @param component_description Text describing the component being timed.
  subroutine log_duration(component_description)
    character(len=*) :: component_description
    real :: full_duration
    real :: curr_duration
    integer :: hours, minutes
    real :: seconds
    character(len=60+len(component_description)) :: log_message

    ! Get the duration and convert to current duration in 
    ! hours, minutes, seconds
    call cpu_time(full_duration)
    curr_duration = full_duration - previous_duration
    call seconds_to_hours_minutes(curr_duration, hours, minutes, seconds)

    ! Write the log message
    log_message = "DURATION of "//component_description//" completed in "
    if(hours .gt. 0)   &
      log_message = trim(log_message)//" "//trim(log_string(hours))//" hours"
    if(minutes .gt. 0) &
      log_message = trim(log_message)//" "//trim(log_string(minutes))//" minutes"
    log_message = trim(log_message)//" "//trim(log_string(seconds))//" seconds."

    ! Write the log message
    call log_info(trim(log_message))

    ! Set the previous duration to this current duration
    previous_duration = previous_duration + curr_duration

  end subroutine log_duration

  !> Measure total runtime of the program. Log Notification System (see Mike Urzen)
  !! expects the word "SUCCESS" in the last line of the log file.
  subroutine log_total_duration()
    real :: full_duration
    integer :: hours, minutes
    real :: seconds
    character(len=100) :: log_message

    ! Get the duration and convert to current duration in
    ! hours, minutes, seconds
    call cpu_time(full_duration)
    call seconds_to_hours_minutes(full_duration, hours, minutes, seconds)

    ! Write the log message
    log_message = "SUCCESS PHA completed normally in "
    if(hours .gt. 0)   &
      log_message = trim(log_message)//" "//trim(log_string(hours))//" hours"
    if(minutes .gt. 0) &
      log_message = trim(log_message)//" "//trim(log_string(minutes))//" minutes"
    log_message = trim(log_message)//" "//trim(log_string(seconds))//" seconds."

    ! Write the log message
    call log_info(trim(log_message))

  end subroutine log_total_duration

  !> Determines the filenames of the properties files based
  !! on command line arguments. -p filename
  !!
  !! @param[out] props_files The filename of the properties file.
  subroutine get_properties_files(props_files)
    character(len=1024), dimension(:), allocatable, intent(out) :: props_files

    character(len=1024) :: props_arg_value
    integer :: i

    props_arg_value = trim(read_command_arg('-p'))

    ! Check to make sure arg value supplied for -p
    if(trim(props_arg_value) .eq. "") then
      ! Can't use Logger since properties with logging info not yet initialized.
      print *, "No properties file name supplied. Will attempt to use default: ghcnm-pha.properties"
      print *, "To specify a different properties file, use command-line argument: `-p filename`"
      props_arg_value = "ghcnm-pha.properties"
    end if

    ! Split input string into multiple properties files if , appears in string
    call split_string(props_arg_value, props_files)

    ! Loop through all properties file names and check if they exist
    do i=1, size(props_files)
      if(.not. does_file_exist(props_files(i))) then
        print *, "PHAMain::get_properties_filename: Specified properties file '",  &
                      trim(props_files(i)),"' does not exist. Program is aborting."
        stop 1
      end if
    end do

  end subroutine get_properties_files

end program PHAMain

!> @file
!! Main controller program for GHCN-M PHA.
