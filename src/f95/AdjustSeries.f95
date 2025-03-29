!> @brief
!! Starting from the most recent change point and then going backwards in time,
!! adjusts each temperature value by the accumulated adjustment size of all change
!! points encountered in the series. Finalizes the adjusted data values and flags
!! and then writes the data to files. See req. 1.9 for more information.
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
module AdjustSeries

  use PropertyReader
  use FileUtils
  use SkylineUtils
  use MathUtils
  use PropertyParameters
  use StationNeighborPairType
  use ElementValueType
  use ChangepointType
  use AlgorithmParameters

  implicit none

contains

  !> Starting from the most recent change point and then going backwards in time,
  !! adjusts each temperature value by the accumulated adjustment size of all change points
  !! encountered in the series. Finalizes the adjusted data values and flags and then
  !! writes the data to files.
  !!
  !! @param[in] neighbors The array of station neighbors and their IDs.
  !! @param[inout] element_data_process An array for all skyline for the processed data values.
  !! @param[in] element_data_interim The holding array for element_data_process before it was changed by ChangepointSize.
  !! @param[in] element_data_orig The holding array for element_data_process when it was first read in.
  !! @param[in] changepoints The final changepoints, sky_month and amplitude, for the entire network.
  !! @param[in] num_changes The number of changepoints in each target station.
  !! @param[in] unstable_dates A list of unstable dates per station.
  subroutine adjust_series(neighbors, element_data_process, element_data_interim, element_data_orig, changepoints,  &
                           num_changes, unstable_dates)
    use CommonVariables, only: begin_year, end_year

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ElementValue), dimension(:) :: element_data_process
    type(ElementValue), dimension(:), intent(in) :: element_data_interim
    type(ElementValue), dimension(:), intent(in) :: element_data_orig
    type(Changepoint), dimension(:,:), intent(in) :: changepoints
    integer, dimension(:), intent(in) :: num_changes
    integer, dimension(:,:), intent(in) :: unstable_dates

    character(len=11) :: target_id
    integer :: change_index
    integer :: sky_index
    integer :: end_sky
    integer :: start_sky
    integer :: first_sky
    integer :: last_sky
    integer :: target_index
    integer :: first_skyline
    integer :: last_skyline
    integer :: station_index
    integer :: year
    integer :: month
    character(len=256) :: file_path

    ! adjustment arrays for output
    real, dimension(:,:) :: out_values(begin_year:end_year, 12)
    integer, dimension(:,:) :: final_values(begin_year:end_year, 12)
    character(len=3), dimension(:,:) :: out_flags(begin_year:end_year, 12)

    call log_info(' ------------ Output Adjustments ------------')

    ! finished with entire network adj passes - write out results
    do station_index = 1, size(neighbors,1)
      ! target_index is the index of the sub-network (candidate)
      target_index = neighbors(station_index,1)%target_index

      call remove_unstable_dates(target_index, element_data_process, element_data_interim, unstable_dates)

      ! see if the end of the incoming POR index is different than
      ! the highest adjusted index

      first_skyline = get_first_sky(target_index)
      last_skyline = get_last_sky(target_index)
      out_flags(:,:) = '   '
      out_values(:,:) = MISSING_REAL

      ! readjust back to last month with data/flag of last year
      do sky_index = last_skyline, first_skyline, -1
        if (.not. is_missing_real(element_data_process(sky_index)%value)   &
               .or. element_data_process(sky_index)%flag(2:2) .ne. ' ') then
          last_sky = sky_index
          exit
        endif
      enddo

      ! set end of first segment at end POR (or last valid chgpt)
      change_index = num_changes(target_index)
      end_sky = changepoints(target_index, change_index)%sky_month
      ! if the end indices are different - print out
      if(end_sky .ne. last_sky .and. end_sky .gt. 0) then
        call log_info('Mis-matched end_sky: '//trim(log_string(end_sky))//'  last_sky: '//trim(log_string(last_sky)))
        do sky_index = last_sky, end_sky, -1
          call get_year_month_from_sky(year,month,sky_index,target_index)
          out_values(year,month) = MISSING_REAL
          out_flags(year,month) = element_data_process(sky_index)%flag
          if(element_data_process(sky_index)%flag(2:2) .eq. 'X') then
            out_values(year,month) = element_data_orig(sky_index)%value
            out_flags(year,month)(2:2) = ' '
          endif
        enddo
      endif

      ! see if the beginning of the incoming POR index is different than
      ! the lowest adjusted index

      ! readjust back to last month with data/flag of last year
      do sky_index = first_skyline, last_skyline
        if (element_data_process(sky_index)%value .gt. MISSING_REAL + 1  &
               .or. element_data_process(sky_index)%flag(2:2) .ne. ' ') then
          first_sky = sky_index
          exit
        endif
      enddo
      ! set the beginning of the first segment at the begin POR
      start_sky = changepoints(target_index, 1)%sky_month
      ! if the begin indices are different - print out
      if(start_sky .ne. first_sky .and. start_sky .gt. 0) then
        call log_info('Mis-matched start_sky: '//trim(log_string(start_sky))//'  first_sky: '//trim(log_string(first_sky)))
        do sky_index = first_sky, start_sky-1
          call get_year_month_from_sky(year,month,sky_index,target_index)
          out_values(year,month) = MISSING_REAL
          out_flags(year,month) = element_data_process(sky_index)%flag
        enddo
      endif

      ! Adjust the candidate series output use element_data_interim, it has all of incoming data
      call adjust_series_values(target_index, element_data_process, element_data_interim, changepoints,  &
                                change_index, out_values, out_flags)

      ! Finalize values and flags based on PHA policy for missing values and flagging.
      call finalize_station_data(out_values, final_values, out_flags)

      ! Get the file path for this station's output file.
      target_id = neighbors(target_index,1)%target_id
      file_path = get_output_filepath(target_id)

      ! Write output to file.
      call write_station_output(file_path, target_id, final_values, out_flags)

    enddo ! end of station write output loop

  end subroutine adjust_series

  !> Loops through any unstable changepoint dates and flags the shortest segments for later
  !!    conversion to missing.
  !!
  !! @param[in] target_index The index of the target station.
  !! @param[inout] element_data_process An array for all skyline for the processed data values.
  !! @param[in] element_data_interim The holding array for element_data_process before it was changed earlier in PHA.
  !! @param[in] unstable_dates A list of unstable dates per station.
  subroutine remove_unstable_dates(target_index, element_data_process, element_data_interim, unstable_dates)

    integer, intent(in) :: target_index
    type(ElementValue), dimension(:) :: element_data_process
    type(ElementValue), dimension(:), intent(in) :: element_data_interim
    integer, dimension(:,:), intent(in) :: unstable_dates

    integer, dimension(:) :: target_unstable_dates(size(unstable_dates,2))
    integer, dimension(:) :: segment_data_count(size(unstable_dates,2))
    integer :: last_unstable
    integer :: least_count
    integer :: least_count_index
    integer :: first_skyline
    integer :: last_skyline
    integer :: sky_index
    integer :: unstable_index
    character(len=256) :: log_message

    ! for unstable segments -
    !   1) Count the number of observations in each set of data before
    !                 and after unstable point for each year with observations.
    !   2) Calculate the percent with flags & with observations.
    !   3) Remove the one with more flags and/or less observations
    ! If there is more than one unstable point, then repeat until only one segment is left.

    ! If there is at least 1 unstable date for this target station
    if(unstable_dates(target_index, 1) > 0) then

      first_skyline = get_first_sky(target_index)
      last_skyline = get_last_sky(target_index)

      call log_info('UNSTABLE: target_index: '//trim(log_string(target_index)))
      call log_info('1: first_skyline: '//trim(log_string(first_skyline)))
      call log_info('2: last_skyline: '//trim(log_string(last_skyline)))

      ! initialize stable segments amount of data and sky indices
      segment_data_count(:) = 0
      target_unstable_dates(:) = 0

      ! Array inverted - End of station is first
      target_unstable_dates(1) = last_skyline
      ! Fill the middle with the unstable breaks
      unstable_index = 1
      do while(unstable_dates(target_index,unstable_index) > 0)
        target_unstable_dates(unstable_index + 1) = unstable_dates(target_index,unstable_index)
        unstable_index = unstable_index + 1
      enddo

      ! Last index is 2 more than unstable changepoints
      last_unstable = unstable_index + 1
      ! Begin of station record is last
      target_unstable_dates(last_unstable) = first_skyline

      ! Count valid obs in each stable segment
      do unstable_index = 2, last_unstable
        do sky_index = target_unstable_dates(unstable_index-1), target_unstable_dates(unstable_index), -1
          if(.not. is_missing_real(element_data_interim(sky_index)%value)  &
                .and. element_data_process(sky_index)%flag(2:2) .eq. " ") then
            segment_data_count(unstable_index) = segment_data_count(unstable_index) + 1
          endif
        enddo
      enddo

      ! Figure out which segment to remove (the one with less data)
      ! add 'g' (gap) to the sement that will be removed in Fillin
      do while (last_unstable .gt. 2)
        call log_debug(trim(log_string(target_index))//' UNSTABLE Compress loop')
        least_count = 9999
        do  unstable_index = 2, last_unstable
          write(log_message,'(3i8)') unstable_index,target_unstable_dates(unstable_index),segment_data_count(unstable_index)
          call log_info(log_message)
          if(segment_data_count(unstable_index) .lt. least_count) then
            least_count_index = unstable_index
            least_count = segment_data_count(unstable_index)
          endif
        enddo
        call log_debug("Remove segment: "//log_string(least_count_index)//&
          log_string(target_unstable_dates(least_count_index-1))//log_string(target_unstable_dates(least_count_index)))
        do sky_index = target_unstable_dates(least_count_index-1), target_unstable_dates(least_count_index), -1
          if(element_data_interim(sky_index)%value .ne. MISSING_REAL)  &
            element_data_process(sky_index)%flag(1:2)=' g'
        enddo

        ! Compress out the one that is gone
        if(least_count_index .lt. last_unstable) then
          do unstable_index = least_count_index, last_unstable-1
            target_unstable_dates(unstable_index) = target_unstable_dates(unstable_index+1)
            segment_data_count(unstable_index) = segment_data_count(unstable_index+1)
          enddo
        else
          target_unstable_dates(last_unstable-1) = target_unstable_dates(last_unstable)
        endif
        target_unstable_dates(last_unstable) = 0
        segment_data_count(last_unstable) = 0
        last_unstable = last_unstable - 1
      enddo
    endif

  end subroutine remove_unstable_dates

  !> Uses the now confirmed changepoints to go through input data and adjusts each
  !!   value for the station series based on the cumulative changepoint size.
  !!
  !! @param[in] target_index The index of the target station.
  !! @param[inout] element_data_process An array for all skyline for the processed data values.
  !! @param[in] element_data_interim The holding array for element_data_process before it was changed earlier in PHA.
  !! @param[in] changepoints The final changepoints, sky_month and amplitude, for the entire network.
  !! @param[in] change_index The index of the current changepoint used for adjusting values.
  !! @param[inout] out_values The adjusted element values as an array with dimensions (year, month).
  !! @param[inout] out_flags The data flags as an array with dimensions (year, month).
  subroutine adjust_series_values(target_index, element_data_process, element_data_interim, changepoints, change_index,  &
                                  out_values, out_flags)
    use CommonVariables, only: begin_year, end_year

    integer, intent(in) :: target_index
    type(ElementValue), dimension(:) :: element_data_process
    type(ElementValue), dimension(:), intent(in) :: element_data_interim
    type(Changepoint), dimension(:,:), intent(in) :: changepoints
    integer, intent(in) :: change_index
    real, dimension(:,:) :: out_values(begin_year:end_year, 12)
    character(len=3), dimension(:,:) :: out_flags(begin_year:end_year, 12)

    integer :: adjustment_count
    integer :: active_change
    integer :: begin_sky
    integer :: end_sky
    integer :: sky_index
    integer :: year
    integer :: month
    real :: sum_change
    character(len=256) :: log_message

    sum_change = 0.0
    ! count the adjusted chgpt - going backwards
    adjustment_count = 0

    end_sky = changepoints(target_index, change_index)%sky_month

    active_change = change_index-1
    do while(active_change .ge. 1)
      adjustment_count = adjustment_count + 1

      if(active_change .eq. 1) then
        begin_sky = changepoints(target_index, active_change)%sky_month
      else
        begin_sky = changepoints(target_index, active_change)%sky_month + 1
      endif

      do sky_index = end_sky, begin_sky, -1
        call get_year_month_from_sky(year,month,sky_index,target_index)
        if(.not. is_missing_real(element_data_interim(sky_index)%value)) then
          out_values(year,month) = element_data_interim(sky_index)%value - sum_change
        endif
        out_flags(year,month) = element_data_process(sky_index)%flag
      enddo

      write(log_message,'("Adj write:",i5,i3,2i5,f7.2)') target_index, change_index, active_change, adjustment_count, sum_change
      call log_info(log_message)

      sum_change = sum_change + changepoints(target_index, active_change)%amplitude
      end_sky = changepoints(target_index, active_change)%sky_month
      active_change = active_change - 1
    enddo

  end subroutine adjust_series_values

  !> Prepares data for output by making final changes to data values and flags.
  !!
  !! @param[in] data_values The previously adjusted temperatures.
  !! @param[out] final_values The final values to be written, converted to integers.
  !! @param[inout] data_flags The data flags used to make final changes to data values and flags.
  subroutine finalize_station_data(data_values, final_values, data_flags)
    use CommonVariables, only: begin_year, end_year

    real, dimension(:,:), intent(in) :: data_values(begin_year:end_year,12)
    integer, dimension(:,:), intent(out) :: final_values(begin_year:end_year,12)
    character(len=3), dimension(:,:) :: data_flags(begin_year:end_year,12)

    integer :: month
    integer :: year
    integer :: good_data_count
    character(len=1) :: data_flag

    do year = begin_year, end_year
      good_data_count = 0
      ! Rearrange to permit flags for missing data to pass
      ! Q - quality control from previous GHCND & GHCNM apps
      ! leave flag as is: value is missing
      do month = 1, 12
        data_flag = data_flags(year,month)(2:2)

        ! Allow station-year records that have only FLAGS to be added to
        ! print output. That is, there may not be observed data in a stn-yr record.
        if(data_flag == 'Q') then
          final_values(year,month) = MISSING_INT
          data_flags(year,month)(2:2) = 'Q'
          good_data_count = good_data_count + 1
        ! M - incoming value is missing ensure
        ! flags are blank : value is missing
        else if(data_flag == 'M') then
          final_values(year,month) = MISSING_INT
          data_flags(year,month) = '   '
        !  All other non-blank flags := PHA has deleted the data value
        !  standardize data_flag to X : value is missing
        else if(data_flag /= ' ') then
          final_values(year,month) = MISSING_INT
          data_flags(year,month)(1:1) = ' '
          data_flags(year,month)(2:2) = 'X'
          good_data_count = good_data_count + 1
        else if(is_missing_real(data_values(year,month))) then
          final_values(year,month) = MISSING_INT
          data_flags(year,month) = '   '
        else
          final_values(year,month) = nint(data_values(year,month)*VALUE_SCALE)
          good_data_count = good_data_count + 1
        endif
      enddo

      ! Provides an internal indicator to write_station_output for years that should not
      ! be written to the output file. This string '---' is not used for writing output.
      if(good_data_count == 0) then
        data_flags(year,:) = '---'
      end if
    end do

  end subroutine finalize_station_data

  !> Writes the final adjusted output to the appropriate file for the given station.
  !!
  !! @param[in] file_path The file path of the output file.
  !! @param[in] station_id The station_id of the output file.
  !! @param[in] output_values The values to be written to the file for the given station.
  !! @param[in] output_flags The flags to be written to the file for the given station.
  subroutine write_station_output(file_path, station_id, output_values, output_flags)
    use CommonVariables, only: begin_year, end_year

    character(len=256), intent(in) :: file_path
    character(len=11), intent(in) :: station_id
    integer, dimension(:,:), intent(in) :: output_values(begin_year:end_year,12)
    character(len=3), dimension(:,:), intent(in) :: output_flags(begin_year:end_year,12)

    integer :: month
    integer :: output_file_unit
    integer :: year
    integer :: file_status
    integer :: element_id

    element_id = get_element_id()

    ! Open this station's output file
    output_file_unit = get_available_file_unit()
    open(output_file_unit, file=file_path, iostat=file_status)
    if (file_status > 0) then
      call log_fatal('AdjustSeries::write_station_output: Cannot open output file ' // trim(file_path))
      stop 1
    endif

    ! Write each line (1 line per year) of this station's output file.
    do year = begin_year, end_year

      ! TODO This is a temporary solution for years that don't contain any good data
      ! Perhaps implement StationType to include individual begin_year and end_year for each station.
      if(output_flags(year,1) == '---') cycle

      write(output_file_unit,"(a11,i1,I4,12(i6,a3))", iostat=file_status) &
        station_id, element_id, year, (output_values(year,month),output_flags(year,month), month=1,12)
      if (file_status > 0) then
        call log_fatal('AdjustSeries::write_station_output: Cannot write to output directory ' // trim(file_path))
        stop 1
      endif

    enddo

    close(output_file_unit)

  end subroutine write_station_output

  !> Gets the name of the output file for the given station.
  !!
  !! @param station_id The station_id of the output file.
  !! @return file_path The name and full path of the output file for the given station.
  function get_output_filepath(station_id) result(file_path)

    character(len=11) :: station_id
    character(len=256) :: file_path

    character(len=4) :: version
    character(len=4) :: element
    character(len=256) :: file_dir
    character(len=8) :: output_indicator ! Uses method and version
    character(len=2), parameter :: method = 'WM' ! Williams-Menne

    element = trim(get_property_chars(PROP_ELEMENT))
    version = get_property_chars(PROP_VERSION)

    ! Get the base path for the output file.
    file_dir = trim(get_property_chars(PROP_PATH_ELEMENT_DATA_OUT))

    ! Construct the full path and file name.
    output_indicator = method // 's.' // trim(version)
    file_path = trim(file_dir)//station_id // '.' // trim(output_indicator) // '.' // element

    ! Logging
    if(file_path == '') then
      call log_error("AdjustSeries::get_output_filepath: Output directory missing")
    else
      call log_info("Output file is "//trim(file_path))
    end if

  end function get_output_filepath

  !> Returns the element ID corresponding to the type of data input.
  !!
  !! @return element_id An integer representing the element type.
  function get_element_id() result(element_id)
    character(len=4) :: element
    integer :: element_id

    element = trim(get_property_chars(PROP_ELEMENT))
    if(element .eq. OPT_ELEMENT_TMAX) then
      element_id = 1
    else if(element .eq. OPT_ELEMENT_TMIN) then
      element_id = 2
    else if(element .eq. OPT_ELEMENT_TAVG) then
      element_id = 3
    ! element_id = 4 is used in production for prcp which is not processed in PHA
    else if(element .eq. OPT_ELEMENT_TDTR) then
      element_id = 5
    else
      call log_error('AdjustSeries::get_element_id: Element parameter '//element//  &
                     ' is unknown. Options are tmax, tmin, tavg, tdtr.')
      stop 1
    endif

  end function get_element_id

end module AdjustSeries
!> @file
!! Contains functionality for writing the output files using the change points to
!! adjust the element data values.
