!> @brief
!! TOB structure, plumbing, and adjustment pass.  Boots via the same properties
!! mechanism as PHAMain, iterates every station in the metadata file, reads raw
!! data and .his observation-time history, computes Karl-et-al bias estimates
!! via TOBUtils, applies day-weighted monthly adjustments, and writes the
!! corrected series into a tob/ output directory.
!! Stations without a .his file default to TOB=2400 (no adjustment; verbatim
!! copy).  Stations outside the contiguous US (ITZ not in {1-4}) or with fewer
!! than 5 years of data are also copied verbatim.
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
  use TOBPropertyParameters
  use FileUtils
  use AlgorithmParameters
  use TOBUtils

  implicit none

  ! ---------------------------------------------------------------------------
  ! Constants
  ! ---------------------------------------------------------------------------
  integer, parameter :: MAX_CHANGES = 200

  ! ---------------------------------------------------------------------------
  ! Program-level variables (accessed by contained procedures via host
  ! association — do NOT duplicate these names as dummy arguments in any
  ! contained subroutine/function).
  ! ---------------------------------------------------------------------------
  character(len=1024), dimension(:), allocatable :: properties_files

  character(len=256) :: meta_file
  character(len=256) :: raw_dir
  character(len=256) :: history_dir
  character(len=256) :: tob_dir
  character(len=4)   :: element
  character(len=4)   :: data_type
  integer            :: tob_start_year
  logical            :: tob_start_from_history
  integer            :: tob_apply_year

  character(len=256) :: input_file
  character(len=256) :: output_file
  character(len=256) :: his_file

  character(len=11)  :: station_id
  real               :: lat, lon

  integer :: meta_unit
  integer :: read_status
  integer :: n_stations, n_with_history, n_without_history, n_adjusted

  ! Per-station history timeline
  integer :: nchgs
  integer :: byrh(MAX_CHANGES+1), bmoh(MAX_CHANGES+1), bdayh(MAX_CHANGES+1)
  integer :: tobtmp(0:MAX_CHANGES)

  ! Per-station raw data (allocated each station)
  integer, allocatable :: values(:, :)
  character(len=3), allocatable :: flags(:, :)
  integer :: first_year, last_year
  character(len=1) :: elem_char

  ! Computation temporaries
  real :: ltix(0:13)
  real :: a_table(12, 24)
  integer :: itz, mict

  ! ---------------------------------------------------------------------------
  ! 1.  Bootstrap: properties, logger
  ! ---------------------------------------------------------------------------
  call get_properties_files(properties_files)

  call properties_init(properties_files)
  call detokenize_all_properties()
  call log_init(get_tob_or_pha_chars(PROP_TOB_LOG_FILENAME, PROP_LOG_FILENAME),     &
                get_tob_or_pha_chars(PROP_TOB_LOG_LEVEL,    PROP_LOG_LEVEL),        &
                get_tob_or_pha_logical(PROP_TOB_LOG_STDOUT, PROP_LOG_STDOUT),       &
                get_tob_or_pha_logical(PROP_TOB_LOG_DATESTAMP, PROP_LOG_DATESTAMP), &
                get_tob_or_pha_logical(PROP_TOB_LOG_ROLLOVER, PROP_LOG_ROLLOVER))

  call log_info("BEGIN TOBMain run")

  ! ---------------------------------------------------------------------------
  ! 2.  Read configuration from properties
  ! ---------------------------------------------------------------------------
  element     = get_property_chars(PROP_ELEMENT)
  data_type   = get_property_chars(PROP_INPUT_DATA_TYPE)
  raw_dir     = get_property_chars(PROP_TOB_PATH_ELEMENT_DATA_IN)
  history_dir = get_property_chars(PROP_PATH_HISTORY)
  meta_file   = get_property_chars(PROP_PATH_STATION_META)
  tob_start_year = get_property_int(PROP_TOB_START_YEAR)
  tob_start_from_history = get_property_logical(PROP_TOB_START_FROM_HISTORY)

  if(len_trim(data_type) == 0) then
    if(index(raw_dir, "/raw/") > 0) then
      data_type = "raw"
    else if(index(raw_dir, "/tob/") > 0) then
      data_type = "tob"
    end if
  end if

  call log_info("Element:      " // trim(element))
  call log_info("Data type:    " // trim(data_type))
  call log_info("Raw dir:      " // trim(raw_dir))
  call log_info("History dir:  " // trim(history_dir))
  call log_info("Metadata:     " // trim(meta_file))
  call log_info("TOB start:    " // trim(int_to_str(tob_start_year)))
  call log_info("TOB start from history: " // trim(log_string(tob_start_from_history)))

  ! ---------------------------------------------------------------------------
  ! 3.  Derive tob output directory
  ! ---------------------------------------------------------------------------
  tob_dir = get_property_chars(PROP_TOB_PATH_ELEMENT_DATA_OUT)
  call log_info("TOB dir:      " // trim(tob_dir))

  if(.not. does_directory_exist(tob_dir)) then
    call log_fatal("TOBMain: output directory missing: " // trim(tob_dir))
    call log_info("TOBMain: create it before running.")
    stop
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
  n_adjusted         = 0

  meta_unit  = get_available_file_unit()
  open(unit=meta_unit, file=trim(meta_file), status='old', iostat=read_status)
  if(read_status /= 0) then
    call log_warn("TOBMain: cannot open metadata file: " // trim(meta_file))
    call log_info("SUCCESS TOBMain completed (no stations).")
    stop
  end if

  do
    ! Read station_id, lat, lon from .inv  (a11,f9.4,f10.4)
    read(meta_unit, '(a11,f9.4,f10.4)', iostat=read_status) station_id, lat, lon
    if(read_status /= 0) exit
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
    ! 5c. Read observation-time history
    ! -----------------------------------------------------------------------
    call read_history()

    if (nchgs == 0) then
      ! No usable history -> copy verbatim (default TOB=2400)
      call copy_file(input_file, output_file)
      call log_info("TOBMain: " // trim(station_id) // " — no history; copied verbatim (TOB=2400)")
      n_without_history = n_without_history + 1
      cycle
    end if

    n_with_history = n_with_history + 1

    ! -----------------------------------------------------------------------
    ! 5d. Read raw data into memory
    ! -----------------------------------------------------------------------
    call read_raw_data()

    if (first_year > last_year) then
      call log_warn("TOBMain: " // trim(station_id) // " — empty data file; skipped")
      cycle
    end if

    ! -----------------------------------------------------------------------
    ! 5e. Compute long-term monthly means and check minimum-years threshold
    ! -----------------------------------------------------------------------
    call compute_monthly_means()

    if (mict < 5) then
      call log_warn("TOBMain: " // trim(station_id) // " — only " // &
                    trim(int_to_str(mict)) // " years; copied verbatim")
      call copy_file(input_file, output_file)
      if (allocated(values)) deallocate(values)
      if (allocated(flags))  deallocate(flags)
      cycle
    end if

    ! -----------------------------------------------------------------------
    ! 5f. Time-zone check (contiguous US only)
    ! -----------------------------------------------------------------------
    itz = get_timezone(lon)
    if (itz < 1 .or. itz > 4) then
      call log_warn("TOBMain: " // trim(station_id) // " — outside contiguous US; copied verbatim")
      call copy_file(input_file, output_file)
      if (allocated(values)) deallocate(values)
      if (allocated(flags))  deallocate(flags)
      cycle
    end if

    ! -----------------------------------------------------------------------
    ! 5g. Finalise history: resolve special codes, set sentinel
    ! -----------------------------------------------------------------------
    call resolve_special_codes()
    tob_apply_year = get_tob_start_year()

    byrh(nchgs+1)  = last_year
    bmoh(nchgs+1)  = 12
    bdayh(nchgs+1) = 32   ! one past end – matches v3 convention

    ! -----------------------------------------------------------------------
    ! 5h. Compute 12x24 bias table via Karl et al. algorithm
    ! -----------------------------------------------------------------------
    call compute_bias_tables(lat, lon, itz, ltix, a_table, element)

    ! -----------------------------------------------------------------------
    ! 5i. Apply day-weighted adjustments to each year-month
    ! -----------------------------------------------------------------------
    call apply_adjustments()

    ! -----------------------------------------------------------------------
    ! 5j. Write adjusted data
    ! -----------------------------------------------------------------------
    call write_data()
    n_adjusted = n_adjusted + 1
    call log_info("TOBMain: adjusted " // trim(station_id))

    if (allocated(values)) deallocate(values)
    if (allocated(flags))  deallocate(flags)

  end do   ! station loop

  close(meta_unit)

  ! ---------------------------------------------------------------------------
  ! 6.  Summary
  ! ---------------------------------------------------------------------------
  call log_info("TOBMain summary:")
  call log_info("  Stations processed:   " // trim(int_to_str(n_stations)))
  call log_info("  With .his history:    " // trim(int_to_str(n_with_history)))
  call log_info("  Without .his (2400):  " // trim(int_to_str(n_without_history)))
  call log_info("  Adjusted:             " // trim(int_to_str(n_adjusted)))
  call log_info("SUCCESS TOBMain completed normally.")

contains

  function get_tob_or_pha_chars(tob_key, pha_key) result(value)
    character(len=*), intent(in) :: tob_key
    character(len=*), intent(in) :: pha_key
    character(len=256) :: value
    value = get_property_chars(tob_key)
    if (len_trim(value) == 0) then
      value = get_property_chars(pha_key)
    end if
  end function get_tob_or_pha_chars

  function get_tob_or_pha_logical(tob_key, pha_key) result(value)
    character(len=*), intent(in) :: tob_key
    character(len=*), intent(in) :: pha_key
    logical :: value
    character(len=256) :: raw
    raw = get_property_chars(tob_key)
    if (len_trim(raw) == 0) then
      value = get_property_logical(pha_key)
    else
      value = get_property_logical(tob_key)
    end if
  end function get_tob_or_pha_logical

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
  !  'old' with 'new' inside 'src'.
  ! ---------------------------------------------------------------------------
  function replace_first(src, old, new) result(res)
    character(len=*), intent(in) :: src, old, new
    character(len=256) :: res
    integer :: pos

    pos = index(src, old)
    if(pos == 0) then
      res = src
    else
      res = src(1:pos-1) // new // src(pos+len(old):)
    end if
  end function replace_first

  ! ---------------------------------------------------------------------------
  !  int_to_str – convert integer to trimmed string
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

  ! ===========================================================================
  !  HISTORY I/O
  ! ===========================================================================

  !> Read the .his file named by host variable his_file and build the
  !! observation-time change timeline into host arrays nchgs/byrh/bmoh/bdayh/tobtmp.
  !! Uses FORMAT 90 (ReadInputFiles.f95:572).  Source==1 (TD3200 daily) records
  !! are skipped.  Consecutive records with the same decoded obs-time code are
  !! collapsed.
  subroutine read_history()

    integer  :: u_his, ios, source
    integer  :: beg_year, beg_month, beg_day
    integer  :: end_year, end_month, end_day
    real     :: lat_deg, lat_min, lat_sec
    real     :: lon_deg, lon_min, lon_sec
    character(len=11) :: dist_dir
    integer  :: elev
    character(len=4)  :: instr_height_full, obs_time
    character(len=5)  :: instr(11)
    integer  :: obcode, prev_code

    ! FORMAT 90 – matches ReadInputFiles.f95:572
  90 format(i1,12x,2(1x,i4,2i2),1x,f3.0,2f3.0,1x,f4.0,2f3.0, &
            1x,a11,1x,i5,2x,a4,1x,a4,5x,11(a5,1x))

    nchgs      = 0
    prev_code  = -1
    tobtmp(0)  = 28        ! pre-history default = sunset (matches v3 NRMTOB)

    if (.not. does_file_exist(his_file)) return

    u_his = get_available_file_unit()
    open(unit=u_his, file=trim(his_file), status='old', action='read', iostat=ios)
    if (ios /= 0) return

    call log_info("TOBMain: " // trim(station_id) // " — reading .his")

    do
      read(u_his, 90, iostat=ios) source,                          &
                                  beg_year, beg_month, beg_day,    &
                                  end_year, end_month, end_day,    &
                                  lat_deg,  lat_min,   lat_sec,    &
                                  lon_deg,  lon_min,   lon_sec,    &
                                  dist_dir, elev,                  &
                                  instr_height_full, obs_time,     &
                                  instr
      if (ios /= 0) exit

      ! Skip daily (TD3200) records
      if (source == 1) cycle

      ! Decode obs_time A4 -> integer code
      obcode = decode_obtime(obs_time)

      ! Only record if different from previous
      if (obcode /= prev_code) then
        if (nchgs >= MAX_CHANGES) then
          call log_warn("TOBMain: " // trim(station_id) // &
                        " — exceeded MAX_CHANGES; remaining history ignored")
          exit
        end if
        nchgs = nchgs + 1
        byrh(nchgs)   = beg_year
        bmoh(nchgs)   = beg_month
        bdayh(nchgs)  = beg_day
        tobtmp(nchgs) = obcode
        prev_code     = obcode
      end if
    end do

    close(u_his)

    if (nchgs > 0) then
      call log_info("TOBMain: " // trim(station_id) // " — " // &
                    trim(int_to_str(nchgs)) // " history change(s)")
    end if
  end subroutine read_history

  ! ===========================================================================
  !  RAW DATA I/O
  ! ===========================================================================

  !> Read the raw data file named by host variable input_file into host
  !! allocatable arrays values/flags.  Two-pass: first to find the year range,
  !! second to fill arrays.  Sets host variables first_year, last_year, elem_char.
  subroutine read_raw_data()

    character(len=11) :: sid
    character(len=1)  :: ec
    integer :: year, m, u_in, ios
    integer :: tv(12)
    character(len=3) :: tf(12)

    first_year = 99999
    last_year  = -99999
    elem_char  = ' '

    ! --- pass 1: year range ---
    u_in = get_available_file_unit()
    open(unit=u_in, file=trim(input_file), status='old', action='read', iostat=ios)
    if (ios /= 0) then
      call log_warn("TOBMain: cannot open data file: " // trim(input_file))
      first_year = 0;  last_year = -1
      return
    end if

    do
      read(u_in, '(a11,a1,i4,12(i6,a3))', iostat=ios) sid, ec, year, &
           (tv(m), tf(m), m=1,12)
      if (ios /= 0) exit
      if (first_year == 99999) elem_char = ec
      if (year < first_year) first_year = year
      if (year > last_year)  last_year  = year
    end do
    close(u_in)

    if (first_year > last_year) return   ! no data

    ! --- allocate ---
    allocate(values(first_year:last_year, 12))
    allocate(flags(first_year:last_year, 12))
    values = MISSING_INT
    flags  = '   '

    ! --- pass 2: fill ---
    u_in = get_available_file_unit()
    open(unit=u_in, file=trim(input_file), status='old', action='read', iostat=ios)
    do
      read(u_in, '(a11,a1,i4,12(i6,a3))', iostat=ios) sid, ec, year, &
           (tv(m), tf(m), m=1,12)
      if (ios /= 0) exit
      values(year, :) = tv
      flags(year, :)  = tf
    end do
    close(u_in)
  end subroutine read_raw_data

  ! ---------------------------------------------------------------------------
  !  write_data – write adjusted data in GHCNMv4 format
  ! ---------------------------------------------------------------------------
  subroutine write_data()

    integer :: u_out, ios, y, m
    character(len=124) :: line

    u_out = get_available_file_unit()
    open(unit=u_out, file=trim(output_file), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      call log_warn("TOBMain: cannot write output: " // trim(output_file))
      return
    end if

    do y = first_year, last_year
      ! Build the 124-character line into a buffer first, then write it.
      ! Avoids a mixed integer/character implied-do in a single write statement
      ! which gfortran does not handle correctly for assumed-shape arrays.
      write(line, '(a11,a1,i4)') station_id, elem_char, y
      do m = 1, 12
        write(line(17+(m-1)*9:25+(m-1)*9), '(i6,a3)') values(y, m), flags(y, m)
      end do
      write(u_out, '(a)') line
    end do

    close(u_out)
  end subroutine write_data

  ! ===========================================================================
  !  MONTHLY-MEAN & CODE-RESOLUTION HELPERS
  ! ===========================================================================

  !> Compute long-term monthly means from host arrays values/first_year/last_year.
  !! Sets host ltix(0:13) where indices 1-12 are monthly means in degrees C,
  !! 0 = December (for drift wrap) and 13 = January.
  !! Also sets host mict = max count of non-missing years across all months.
  subroutine compute_monthly_means()

    integer :: y, m, ict(12)
    real    :: sum_val(12)

    ict = 0
    sum_val = 0.
    mict = 0

    do m = 1, 12
      do y = first_year, last_year
        if (values(y, m) /= MISSING_INT) then
          ict(m) = ict(m) + 1
          sum_val(m) = sum_val(m) + real(values(y, m)) / VALUE_SCALE
        end if
      end do
      if (ict(m) > mict) mict = ict(m)
    end do

    do m = 1, 12
      if (ict(m) > 0) then
        ltix(m) = sum_val(m) / real(ict(m))
      else
        ltix(m) = 0.0
      end if
    end do

    ! Wrap for drift calculation
    ltix(0)  = ltix(12)
    ltix(13) = ltix(1)
  end subroutine compute_monthly_means

  !> Resolve special observation-time codes in place in host array tobtmp.
  !! 25 (HR) -> 24 (midnight).
  !! 29, 99 (unknown) -> previous code.
  !! 30 (TRID / traditional) -> previous code.
  !! Codes 26-28 (RS/SR/SS) are left intact; they are resolved per-month
  !! by tobchg at adjustment time.
  subroutine resolve_special_codes()
    integer :: n

    ! tobtmp(0) is already set to 28 (sunset) in read_history

    do n = 1, nchgs
      select case (tobtmp(n))
        case (30)   ! TRID – traditional, same as previous
          tobtmp(n) = tobtmp(n-1)
        case (25)   ! HR – treat as midnight
          tobtmp(n) = 24
        case (99, 29)   ! unknown / other
          tobtmp(n) = tobtmp(n-1)
      end select
    end do
  end subroutine resolve_special_codes

  ! ===========================================================================
  !  ADJUSTMENT APPLICATION
  ! ===========================================================================

  !> Walk every year-month in the data, compute the day-weighted TOB
  !! adjustment, and subtract it from non-missing values.
  !! Operates on host arrays values, first_year, last_year, nchgs, byrh,
  !! bmoh, bdayh, tobtmp, a_table.
  subroutine apply_adjustments()
    integer :: y, m
    real    :: adj

    do y = first_year, last_year
      do m = 1, 12
        if (values(y, m) == MISSING_INT) cycle
        if (y < tob_apply_year) cycle
        adj = get_monthly_adj(y, m)
        values(y, m) = values(y, m) - nint(adj * VALUE_SCALE)
      end do
    end do
  end subroutine apply_adjustments

  !> Determine the first year to apply TOB adjustments for this station.
  !! If tob.start-from-history is true, use the first history record that
  !! has a resolvable observation time; otherwise use tob.start-year.
  function get_tob_start_year() result(start_year)
    integer :: start_year
    integer :: n

    start_year = tob_start_year
    if(.not. tob_start_from_history) return

    do n = 1, nchgs
      if (tobtmp(n) >= 1 .and. tobtmp(n) <= 24) then
        start_year = byrh(n)
        return
      end if
      if (tobtmp(n) >= 26 .and. tobtmp(n) <= 28) then
        start_year = byrh(n)
        return
      end if
    end do
  end function get_tob_start_year

  !> Compute the day-weighted monthly adjustment for year y, month m.
  !! Scans the history timeline for any obs-time change that falls within
  !! the month and produces a weighted average of the per-hour biases.
  function get_monthly_adj(y, m) result(adj)
    integer, intent(in) :: y, m
    real :: adj

    integer :: active_idx, n, tob
    integer :: seg_start, seg_end, days_tot
    real    :: weighted_sum

    days_tot = days_in_month(y, m)

    ! Find the change active at the start of this month
    active_idx = find_active_change(y, m, 1)

    weighted_sum = 0.
    seg_start    = 1

    ! Scan forward for changes that fall within this month
    do n = active_idx + 1, nchgs
      ! Past this month?
      if (byrh(n) > y .or. (byrh(n) == y .and. bmoh(n) > m)) exit
      ! Change is inside this month – close the preceding segment
      seg_end = bdayh(n) - 1
      if (seg_end >= seg_start) then
        tob = tobtmp(active_idx)
        if (tob >= 26 .and. tob <= 28) call tobchg(m, tob)
        if (tob >= 1 .and. tob <= 24) then
          weighted_sum = weighted_sum + real(seg_end - seg_start + 1) * a_table(m, tob)
        end if
      end if
      seg_start  = bdayh(n)
      active_idx = n
    end do

    ! Final (or only) segment: seg_start .. end of month
    seg_end = days_tot
    if (seg_end >= seg_start) then
      tob = tobtmp(active_idx)
      if (tob >= 26 .and. tob <= 28) call tobchg(m, tob)
      if (tob >= 1 .and. tob <= 24) then
        weighted_sum = weighted_sum + real(seg_end - seg_start + 1) * a_table(m, tob)
      end if
    end if

    adj = weighted_sum / real(days_tot)
  end function get_monthly_adj

  !> Find the index of the latest history change whose begin-date is
  !! at or before (y, m, d).  Returns 0 if the date precedes all changes
  !! (pre-history; tobtmp(0) = sunset is used).
  function find_active_change(y, m, d) result(idx)
    integer, intent(in) :: y, m, d
    integer :: idx, n

    idx = 0
    do n = 1, nchgs
      if (date_le(byrh(n), bmoh(n), bdayh(n), y, m, d)) then
        idx = n
      else
        exit   ! changes are chronological
      end if
    end do
  end function find_active_change

  ! ---------------------------------------------------------------------------
  !  date_le – is date (y1/m1/d1) <= date (y2/m2/d2)?
  ! ---------------------------------------------------------------------------
  function date_le(y1, m1, d1, y2, m2, d2) result(le)
    integer, intent(in) :: y1, m1, d1, y2, m2, d2
    logical :: le
    le = (y1 < y2) .or. &
         (y1 == y2 .and. m1 < m2) .or. &
         (y1 == y2 .and. m1 == m2 .and. d1 <= d2)
  end function date_le

  ! ---------------------------------------------------------------------------
  !  days_in_month – with leap-year February
  ! ---------------------------------------------------------------------------
  function days_in_month(y, m) result(d)
    integer, intent(in) :: y, m
    integer :: d
    integer, parameter :: NDAYS(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    d = NDAYS(m)
    if (m == 2 .and. is_leap_year(y)) d = 29
  end function days_in_month

  ! ---------------------------------------------------------------------------
  !  is_leap_year
  ! ---------------------------------------------------------------------------
  function is_leap_year(y) result(leap)
    integer, intent(in) :: y
    logical :: leap
    leap = (mod(y,4) == 0 .and. mod(y,100) /= 0) .or. mod(y,400) == 0
  end function is_leap_year

end program TOBMain

!> @file
!! TOB structure, plumbing and adjustment pass for GHCN-Monthly.
