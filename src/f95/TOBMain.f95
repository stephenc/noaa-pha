!> @brief
!! TOB structure and plumbing pass.  Boots via the same properties
!! mechanism as PHAMain, iterates every station in the metadata file,
!! copies each raw data file verbatim into a tob/ output directory,
!! and parses .his files to log the observation-time history.
!! Stations without a .his file are noted as defaulting to TOB=2400.
!! No data values are changed in this pass.
!!
!! USAGE: TOBMain -p properties-filename(s)
!!    ex: TOBMain -p ghcnm-pha.properties
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
program TOBMain

  use Logger
  use PropertyReader
  use ConfigurationUtils
  use PropertyParameters
  use FileUtils

  implicit none

  ! ---------------------------------------------------------------------------
  ! Program-level variables
  ! ---------------------------------------------------------------------------
  character(len=1024), dimension(:), allocatable :: properties_files

  character(len=256) :: meta_file
  character(len=256) :: raw_dir
  character(len=256) :: history_dir
  character(len=256) :: tob_dir
  character(len=4)   :: element
  character(len=4)   :: data_type

  character(len=256) :: input_file
  character(len=256) :: output_file
  character(len=256) :: his_file

  character(len=11)  :: station_id
  character(len=130) :: meta_line

  integer :: meta_unit, meta_lines
  integer :: in_unit, out_unit
  integer :: read_status
  integer :: n_stations, n_with_history, n_without_history
  integer :: idx

  ! ---------------------------------------------------------------------------
  ! 1.  Bootstrap: properties, logger
  ! ---------------------------------------------------------------------------
  call get_properties_files(properties_files)

  call properties_init(properties_files)
  call detokenize_all_properties()
  call log_init(get_property_chars(PROP_LOG_FILENAME),     &
                get_property_chars(PROP_LOG_LEVEL),        &
                get_property_logical(PROP_LOG_STDOUT),     &
                get_property_logical(PROP_LOG_DATESTAMP),  &
                get_property_logical(PROP_LOG_ROLLOVER))

  call log_info("BEGIN TOBMain run")

  ! ---------------------------------------------------------------------------
  ! 2.  Read configuration from properties
  ! ---------------------------------------------------------------------------
  element   = get_property_chars(PROP_ELEMENT)
  data_type = get_property_chars(PROP_INPUT_DATA_TYPE)
  raw_dir   = get_property_chars(PROP_PATH_ELEMENT_DATA_IN)
  history_dir = get_property_chars(PROP_PATH_HISTORY)
  meta_file = get_property_chars(PROP_PATH_STATION_META)

  call log_info("Element:      " // trim(element))
  call log_info("Data type:    " // trim(data_type))
  call log_info("Raw dir:      " // trim(raw_dir))
  call log_info("History dir:  " // trim(history_dir))
  call log_info("Metadata:     " // trim(meta_file))

  ! ---------------------------------------------------------------------------
  ! 3.  Derive tob output directory by replacing data_type with "tob"
  ! ---------------------------------------------------------------------------
  tob_dir = replace_first(trim(raw_dir), trim(data_type), "tob")
  call log_info("TOB dir:      " // trim(tob_dir))

  ! Create tob_dir if it does not yet exist
  if(.not. does_directory_exist(tob_dir)) then
    call execute_command_line("mkdir -p " // trim(tob_dir))
    call log_info("Created output directory: " // trim(tob_dir))
  end if

  ! ---------------------------------------------------------------------------
  ! 4.  Verify metadata file exists
  ! ---------------------------------------------------------------------------
  if(.not. does_file_exist(meta_file)) then
    call log_warn("TOBMain: metadata file does not exist: " // trim(meta_file))
    call log_info("SUCCESS TOBMain completed (no stations).")
    stop
  end if

  ! ---------------------------------------------------------------------------
  ! 5.  Iterate every station in the metadata file
  ! ---------------------------------------------------------------------------
  n_stations         = 0
  n_with_history     = 0
  n_without_history  = 0

  meta_unit  = get_available_file_unit()
  open(unit=meta_unit, file=trim(meta_file), status='old', iostat=read_status)
  if(read_status /= 0) then
    call log_warn("TOBMain: cannot open metadata file: " // trim(meta_file))
    call log_info("SUCCESS TOBMain completed (no stations).")
    stop
  end if

  do
    ! Read next line from metadata file (station_id is columns 1:11)
    read(meta_unit, '(a11)', iostat=read_status) station_id
    if(read_status /= 0) exit   ! EOF or error

    ! Skip blank station IDs
    if(len_trim(station_id) == 0) cycle

    n_stations = n_stations + 1

    ! -----------------------------------------------------------------------
    ! 5a. Build file paths
    ! -----------------------------------------------------------------------
    input_file  = trim(raw_dir) // trim(station_id) // '.' // trim(data_type) // '.' // trim(element)
    output_file = trim(tob_dir) // trim(station_id) // '.tob.' // trim(element)
    his_file    = trim(history_dir) // trim(station_id) // '.his'

    ! -----------------------------------------------------------------------
    ! 5b. Skip station if raw input file is missing
    ! -----------------------------------------------------------------------
    if(.not. does_file_exist(input_file)) then
      call log_warn("TOBMain: input file missing, skipping station " // trim(station_id) // &
                    " (" // trim(input_file) // ")")
      cycle
    end if

    ! -----------------------------------------------------------------------
    ! 5c. Parse .his file and log obs-time history  OR  note default TOB=2400
    ! -----------------------------------------------------------------------
    if(does_file_exist(his_file)) then
      call read_and_log_history(his_file, station_id)
      n_with_history = n_with_history + 1
    else
      call log_info("TOBMain: " // trim(station_id) // " — no .his file; defaults to TOB=2400")
      n_without_history = n_without_history + 1
    end if

    ! -----------------------------------------------------------------------
    ! 5d. Copy raw data file verbatim to tob output
    ! -----------------------------------------------------------------------
    call copy_file(input_file, output_file)
    call log_info("TOBMain: copied " // trim(station_id))

  end do   ! station loop

  close(meta_unit)

  ! ---------------------------------------------------------------------------
  ! 6.  Summary
  ! ---------------------------------------------------------------------------
  call log_info("TOBMain summary:")
  call log_info("  Stations processed:   " // trim(int_to_str(n_stations)))
  call log_info("  With .his history:    " // trim(int_to_str(n_with_history)))
  call log_info("  Without .his (2400):  " // trim(int_to_str(n_without_history)))
  call log_info("SUCCESS TOBMain completed normally.")

contains

  ! ===========================================================================
  !  LOCAL SUBROUTINES / FUNCTIONS
  ! ===========================================================================

  !> Determines the filenames of the properties files from the -p command-line
  !! argument.  Identical logic to PHAMain::get_properties_files.
  subroutine get_properties_files(props_files)
    character(len=1024), dimension(:), allocatable, intent(out) :: props_files
    character(len=1024) :: props_arg_value
    integer :: i

    props_arg_value = trim(read_command_arg('-p'))

    if(trim(props_arg_value) .eq. "") then
      print *, "No properties file name supplied. Will attempt to use default: ghcnm-pha.properties"
      print *, "To specify a different properties file, use command-line argument: `-p filename`"
      props_arg_value = "ghcnm-pha.properties"
    end if

    call split_string(props_arg_value, props_files)

    do i=1, size(props_files)
      if(.not. does_file_exist(props_files(i))) then
        print *, "TOBMain: Specified properties file '", trim(props_files(i)), &
                 "' does not exist. Program is aborting."
        stop 1
      end if
    end do

  end subroutine get_properties_files

  ! ---------------------------------------------------------------------------
  !  replace_first – return result of replacing the first occurrence of
  !  'old' with 'new' inside 'src'.  Pure string operation.
  ! ---------------------------------------------------------------------------
  function replace_first(src, old, new) result(res)
    character(len=*), intent(in) :: src, old, new
    character(len=256) :: res
    integer :: pos

    pos = index(src, old)
    if(pos == 0) then
      ! old not found – return src unchanged
      res = src
    else
      res = src(1:pos-1) // new // src(pos+len(old):)
    end if
  end function replace_first

  ! ---------------------------------------------------------------------------
  !  int_to_str – convert integer to trimmed string (no external dependency)
  ! ---------------------------------------------------------------------------
  function int_to_str(val) result(res)
    integer, intent(in) :: val
    character(len=20) :: res
    write(res, '(i0)') val
  end function int_to_str

  ! ---------------------------------------------------------------------------
  !  copy_file – copy a file line-by-line (verbatim, no data changes)
  ! ---------------------------------------------------------------------------
  subroutine copy_file(src_path, dst_path)
    character(len=*), intent(in) :: src_path, dst_path
    character(len=1024) :: line
    integer :: u_in, u_out, ios

    u_in  = get_available_file_unit()
    open(unit=u_in, file=trim(src_path), status='old', action='read', iostat=ios)
    if(ios /= 0) then
      call log_warn("TOBMain: cannot open input file for copy: " // trim(src_path))
      return
    end if

    u_out = get_available_file_unit()
    open(unit=u_out, file=trim(dst_path), status='replace', action='write', iostat=ios)
    if(ios /= 0) then
      close(u_in)
      call log_warn("TOBMain: cannot open output file for copy: " // trim(dst_path))
      return
    end if

    do
      read(u_in, '(a)', iostat=ios) line
      if(ios /= 0) exit
      write(u_out, '(a)') trim(line)
    end do

    close(u_in)
    close(u_out)
  end subroutine copy_file

  ! ---------------------------------------------------------------------------
  !  read_and_log_history – open a .his file, iterate its records, and log the
  !  observation-time field from each record.
  !
  !  Record format matches ReadInputFiles.f95:572 —
  !    format(i1,12x,2(1x,i4,2i2),1x,f3.0,2f3.0,1x,f4.0,2f3.0,1x,a11,1x,i5,2x,a4,1x,a4,5x,11(a5,1x))
  !  We only extract the fields needed for TOB logging:
  !    source       – i1          (record source: 0=SHF, 1=daily, 2=MSHR, 3=CDMP)
  !    beg_year     – i4          (begin year)
  !    beg_month    – i2          (begin month)
  !    beg_day      – i2          (begin day)
  !    end_year     – i4          (end year)
  !    end_month    – i2          (end month)
  !    end_day      – i2          (end day)
  !    obs_time     – a4          (the second a4 field: observation time code)
  ! ---------------------------------------------------------------------------
  subroutine read_and_log_history(his_path, station_id)
    character(len=*), intent(in) :: his_path
    character(len=11), intent(in) :: station_id

    integer  :: u_his, ios, rec_count
    integer  :: source
    integer  :: beg_year, beg_month, beg_day
    integer  :: end_year, end_month, end_day
    ! Placeholders for fields we must consume but do not log
    real     :: lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec
    character(len=11) :: dist_dir
    integer  :: elev
    character(len=4)  :: instr_height_full
    character(len=4)  :: obs_time
    character(len=5)  :: instr(11)
    character(len=80) :: msg

    ! The same FORMAT used by ReadInputFiles::read_standard_history (line 572)
90  format(i1,12x,2(1x,i4,2i2),1x,f3.0,2f3.0,1x,f4.0,2f3.0, &
           1x,a11,1x,i5,2x,a4,1x,a4,5x,11(a5,1x))

    u_his = get_available_file_unit()
    open(unit=u_his, file=trim(his_path), status='old', action='read', iostat=ios)
    if(ios /= 0) then
      call log_warn("TOBMain: cannot open history file: " // trim(his_path))
      return
    end if

    call log_info("TOBMain: " // trim(station_id) // " — reading .his")
    rec_count = 0

    do
      read(u_his, 90, iostat=ios) source, beg_year, beg_month, beg_day,  &
                                  end_year, end_month, end_day,           &
                                  lat_deg, lat_min, lat_sec,             &
                                  lon_deg, lon_min, lon_sec,             &
                                  dist_dir, elev, instr_height_full,     &
                                  obs_time, instr
      if(ios /= 0) exit

      ! Skip source==1 (daily obs-time records) to match PHA convention
      if(source == 1) cycle

      rec_count = rec_count + 1
      write(msg, '(a,i1,a,i4.4,a,i2.2,a,i2.2,a,i4.4,a,i2.2,a,i2.2,a,a4)') &
            "  src=", source,                   &
            " beg=", beg_year, "-", beg_month, "-", beg_day, &
            " end=", end_year, "-", end_month, "-", end_day, &
            " obs_time=", obs_time
      call log_info("TOBMain: " // trim(station_id) // trim(msg))
    end do

    close(u_his)

    if(rec_count == 0) then
      call log_info("TOBMain: " // trim(station_id) // " — .his file empty or unparseable; defaults to TOB=2400")
    end if

  end subroutine read_and_log_history

end program TOBMain

!> @file
!! TOB structure and plumbing pass for GHCN-Monthly.
