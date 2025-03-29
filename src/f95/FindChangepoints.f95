!> @brief
!! Locates all potential change points in each target-neighbor difference series.
!! A change point is an artificial shift in the temperature data record for a
!! station, resulting from a change to station location, station configuration,
!! or surrounding topography. See Req. 1.3 for more information.
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
module FindChangepoints

  use Logger
  use PropertyReader
  use SkylineUtils
  use ModelFitUtils
  use MathUtils
  use AlgorithmParameters
  use StationType
  use StationNeighborPairType
  use ElementValueType
  use DifferenceValueType
  use ChangepointSumsType
  use ChangepointSpanType

  implicit none

contains

  !> Finds the potential changepoints in each target-neighbor difference series.
  !!
  !! @param[inout] neighbors The entire network of target-neighbor pairs.
  !! @param[in] element_data_process All of the raw element values in one skyline array.
  !! @param[out] changepoint_spans For all skyline segments and for all neighbors of each target shows the
  !!               influence of neighbors on their targets (this is a large array in terms of memory).
  !! @param[out] changepoint_sums For all skyline the cumulative adjustments due to changepoints from
  !!               station pairs.
  subroutine find_paired_changepoints(neighbors, element_data_process, changepoint_spans, changepoint_sums)
    use CommonVariables, only: por_months, skyline_months

    type(StationNeighborPair), dimension(:,:) :: neighbors
    type(ElementValue), dimension(:), intent(in) :: element_data_process
    type(ChangepointSpan), dimension(:,:), allocatable, intent(out) :: changepoint_spans
    type(ChangepointSums), dimension(:), allocatable, intent(out) :: changepoint_sums

    type(StationNeighborPair) :: station_pair
    type(DifferenceValue), dimension(:), allocatable :: diff_values
    integer, dimension(:) :: change_month(MAX_CHANGEPOINTS)
    character(len=11) :: target_id
    integer :: station_index
    integer :: target_index
    integer :: neighbor_index
    integer :: neigh2targ_index
    integer :: targ2neigh_index
    integer :: station_count
    integer :: neighbor_count
    integer :: change_count
    integer :: buffered_neighbor_count
    integer :: max_backfilled_neighbors
    integer :: use_shf_meta
    logical :: do_process

    ! since the network is NOT circular - but to keep down the number
    ! of paired iterations - keep track of pairs done.
    logical(1), dimension(:,:), allocatable :: pairs_done

    station_count = size(neighbors,1)

    neighbor_count = get_property_int(PROP_NEIGH_FINAL_LIMIT)
    buffered_neighbor_count = get_property_int(PROP_NEIGH_BUFFERED_LIMIT)
    ! Change from original code: used to be hard-coded to 100 instead of buffered_neighbor_count
    if(neighbor_count > buffered_neighbor_count) then
      call log_info('Reset PW neighbors to Incoming Network '//trim(log_string(neighbor_count))// &
             ' to '//trim(log_string(buffered_neighbor_count)))
      neighbor_count = buffered_neighbor_count
    endif

    ! increase maximum number of paired stations from the PW combo input
    ! add a buffer ~ 1/2 size of neighbor_count
    max_backfilled_neighbors = neighbor_count + neighbor_count/2
    ! Change from original code: used to be hard-coded to 100 instead of buffered_neighbor_count
    if(max_backfilled_neighbors > buffered_neighbor_count)  &
        max_backfilled_neighbors = buffered_neighbor_count
    call log_info('max_backfilled_neighbors buffer limit set to: '//trim(log_string(max_backfilled_neighbors)))

    allocate(pairs_done(station_count, buffered_neighbor_count))
    allocate(changepoint_spans(buffered_neighbor_count, skyline_months))
    allocate(changepoint_sums(skyline_months))
    allocate(diff_values(por_months))

    ! initialize round robin array
    changepoint_spans(:,:) = null_ChangepointSpan()
    changepoint_sums(:) = null_ChangepointSums()
    pairs_done(:,:) = FALSE

    ! If use_shf_meta is OPT_HISTORY_ONLY, then skip all this because we're only using SHF.
    use_shf_meta = get_property_int(PROP_USE_HISTORY_FILES)
    if(use_shf_meta == OPT_HISTORY_ONLY) return

    do station_index = 1, size(neighbors,1)
      target_index = neighbors(station_index,1)%target_index
      target_id = neighbors(target_index,1)%target_id

      if(target_index == 0) exit

      do targ2neigh_index = 2, size(neighbors,2)
        station_pair = neighbors(target_index, targ2neigh_index)
        neighbor_index = station_pair%neighbor_index

        ! finished with neighborhood if neighbor index == 0 or
        ! there is no data for the "pair" station
        if(neighbor_index == 0) exit

        ! if the pair has already been processed - skip make-work
        if(pairs_done(target_index, targ2neigh_index)) cycle

        ! calculating diffs has the side effect of setting begin_month_index in the station_pair
        call calculate_diffs_for_pair(station_pair, element_data_process, diff_values, do_process)

        ! Skip if there is insufficient overlapping or monthly average data for this pair
        if(.not. do_process) then
          call log_info(target_id//"-"//station_pair%neighbor_id//  &
                        " Station pair has insufficient overlapping or monthly average data - skipping")
          cycle
        end if

        call backfill_neighbor(station_pair, neighbors, max_backfilled_neighbors)
        neigh2targ_index = station_pair%neigh2targ_index

        ! Skip if there is insufficient difference data for this pair
        if(station_pair%begin_month_index == 0) then
          call log_info(target_id//"-"//station_pair%neighbor_id//" Station pair differences have insufficient data - skipping")
          cycle
        end if

        call identify_changepoints(station_pair, changepoint_spans(targ2neigh_index,:), change_month,  &
               change_count, diff_values)

        ! loop back through the combined change points -
        !   use the same assumptions that the single test routines use
        !   1) the chgpt is still valid (not straight line)
        !   2) same type as originally detected
        ! also
        !   3) if 2 or more of the chgpts are within MINLEN
        !     a) if the chgpt est are of the same sign then
        !        test each singly with same endpoints & keep lowest BIC Q
        !     b) if not same sign then
        ! opt1:  remove all data (mark as is_deleted) over interim chgpts
        ! opt2:  remove all data (mark as is_deleted) over both segments
        !        in either case: retain earliest chgpt

        call prune_and_store_changepoints(station_pair, diff_values(:),  &
                 changepoint_spans(targ2neigh_index,:), &
                 changepoint_spans(neigh2targ_index,:), changepoint_sums, change_month, change_count)

        ! set targ-neigh pair index to DONE
        pairs_done(target_index, targ2neigh_index) = .true.
        ! for accumulation phase - set neigh-targ pair index to DONE, if applicable
        if(neigh2targ_index > 0) pairs_done(neighbor_index, neigh2targ_index) = .true.

      enddo ! end do all neighbors
    enddo ! end do all candidates
  end subroutine find_paired_changepoints

  !> Calculates a difference series for a target-neighbor pair.
  !!   See Req. 1.2 for more information.
  !!
  !! @param[inout] target_neighbor_pair The target-neighbor pair of stations being processed. A side-
  !!   effect of this subroutine is that the begin/end_month_index for the pair is set.
  !! @param[in] element_data_process All of the raw element values in one skyline array.
  !! @param[out] diff_values The array of DifferenceValues for this pair, containing the
  !!   deseasonalized values and the difference of them.
  !! @param[out] do_process True if further processing is needed for this station pair. Otherwise, false.
  subroutine calculate_diffs_for_pair(target_neighbor_pair, element_data_process, diff_values, do_process)
    use CommonVariables, only: por_months

    type(StationNeighborPair) :: target_neighbor_pair
    type(ElementValue), dimension(:), intent(in) :: element_data_process
    type(DifferenceValue), dimension(:), intent(out) :: diff_values

    real, dimension(:,:) :: pair_values(2, por_months)
    real :: diff_value
    integer :: target_index
    integer :: neigh_index
    integer :: begin_month_index
    integer :: end_month_index
    integer :: month_index
    logical, intent(out) :: do_process

    diff_values = null_DifferenceValue()

    ! Prepare the raw values array for the pair. Won't bother processing if
    ! no common years or if either station has too much missing data.
    target_index = target_neighbor_pair%target_index
    neigh_index = target_neighbor_pair%neighbor_index
    call prepare_paired_data(target_index, neigh_index, element_data_process, pair_values, do_process)

    ! Not enough data to bother processing. Note that begin_month_index and end_month_index won't
    ! be set in this case, making that a good indicator for pairs that don't need to be processed.
    if(.not. do_process) return

    ! For each year-month starting at the earliest common to both, calcuate the
    ! difference and store a MISSING value if either element value is MISSING
    begin_month_index = 0
    do month_index = 1, size(diff_values)
      diff_value = calculate_difference_value(pair_values(1, month_index), pair_values(2, month_index))

      diff_values(month_index) = new_DifferenceValue(pair_values(1, month_index),  &
                                   pair_values(2, month_index), diff_value)

      if(.not. is_missing_real(diff_values(month_index)%difference)) then
        if(begin_month_index == 0) begin_month_index = month_index
        end_month_index = month_index
      endif
    enddo

    ! For the case where there is overlapping data, but all diffs are MISSING
    if(begin_month_index == 0) return

    ! Set the month index values for this pair, only if there is sufficient non-MISSING data
    target_neighbor_pair%begin_month_index = begin_month_index
    target_neighbor_pair%end_month_index = end_month_index

  end subroutine calculate_diffs_for_pair

  !> Prepares the array of raw values for the target and neighbor stations needed
  !!   to calculate the difference series.
  !!
  !! @param[in] target_index The station index of the target.
  !! @param[in] neigh_index The station index of the neighbor.
  !! @param[in] element_data_process An array for all skyline for the processed data values.
  !! @param[out] pair_values An array of target and neighbor temperatures.
  !! @param[out] do_process True if further processing is needed for this station pair. Otherwise, false.
  subroutine prepare_paired_data(target_index, neigh_index, element_data_process, pair_values, do_process)
    use CommonVariables, only: por_months

    integer, intent(in) :: target_index
    integer, intent(in) :: neigh_index
    type(ElementValue), dimension(:), intent(in) :: element_data_process
    real, dimension(:,:), intent(out) :: pair_values
    logical, intent(out) :: do_process

    integer :: common_first_year
    integer :: common_last_year
    integer :: neigh_last_year
    integer :: neigh_first_year
    integer :: target_first_year
    integer :: target_last_year
    integer :: values_count
    real, dimension(:) :: deseasoned_values(por_months)

    do_process = .true.

    ! Get begin/end years for both stations to determine common years
    target_first_year = get_first_year(target_index)
    target_last_year = get_last_year(target_index)
    neigh_first_year = get_first_year(neigh_index)
    neigh_last_year = get_last_year(neigh_index)
    common_first_year = max(target_first_year, neigh_first_year)
    common_last_year = min(target_last_year, neigh_last_year)

    ! If no common years, don't process
    if(common_last_year <= common_first_year) then
      do_process = .false.
      return
    endif

    ! Move common POR data for target station from incoming
    ! array to round-robin for pairwise processing
    call deseasonalize_values(target_index, element_data_process, deseasoned_values, values_count)
    if(values_count < MIN_MONTHS_RAW) then
      do_process = .false.
      return
    endif
    pair_values(1,:) = deseasoned_values

    ! Do the same for the neighbor stations
    call deseasonalize_values(neigh_index, element_data_process, deseasoned_values, values_count)
    if(values_count < MIN_MONTHS_RAW) then
      do_process = .false.
      return
    endif
    pair_values(2,:) = deseasoned_values

  end subroutine prepare_paired_data

  !> Deseasonalizes raw data by computing monthly anomalies using the mean monthly values.
  !!   Target and pair series "are first deseasonalized" (Menne and Williams, 2009, Section 3-a).
  !!
  !! @param[in] station_index The index for the station being processed.
  !! @param[in] element_data_process An array for all skyline for the processed data values.
  !! @param[out] deseasoned_values A array of deseasonalized element values.
  !! @param[out] values_count The number of non-missing values in all months.
  subroutine deseasonalize_values(station_index, element_data_process, deseasoned_values, values_count)

    integer, intent(in) :: station_index
    type(ElementValue), dimension(:), intent(in) :: element_data_process
    real, dimension(:), intent(out) :: deseasoned_values
    integer, intent(out) :: values_count

    real :: month_avg(13)
    integer :: first_year
    integer :: first_sky
    integer :: last_sky
    real, dimension(:) :: month_sums(12)
    integer, dimension(:) :: month_counts(12)
    integer :: sky_index
    integer :: month_index_start
    integer :: month_index
    integer :: month

    month_sums(:) = 0.0
    month_counts(:) = 0
    values_count = 0
    deseasoned_values(:) = MISSING_REAL

    ! Get the skyline info for both stations for deseasonlizing values
    first_year = get_first_year(station_index)
    first_sky = get_first_sky(station_index)
    last_sky = get_last_sky(station_index)

    ! month_index_start is the year-month offset of the start of the
    ! common POR
    month_index_start = get_month_index(first_year, 1)

    do sky_index = first_sky, last_sky
      month = mod(sky_index - first_sky, 12) + 1
      if(.not. is_missing_real(element_data_process(sky_index)%value)) then
        month_sums(month) = month_sums(month) + element_data_process(sky_index)%value
        month_counts(month) = month_counts(month) + 1
      endif
    enddo

    ! If enough months, compute monthly average
    do month = 1, 12
      if(month_counts(month) >= MIN_YEARS_FOR_MONTHLY_AVE) then
        month_avg(month) = month_sums(month) / month_counts(month)
      else
        month_avg(month) = MISSING_REAL
      endif
    enddo

    ! For all sky time, populate round robin array which is detrended by monthly averages
    do sky_index = first_sky, last_sky
      month_index = month_index_start + sky_index - first_sky
      month = mod(sky_index - first_sky, 12) + 1
      if(.not. is_missing_real(element_data_process(sky_index)%value) .AND. .not. is_missing_real(month_avg(month))) then
        deseasoned_values(month_index) = element_data_process(sky_index)%value - month_avg(month)
        values_count = values_count + 1
      endif
    enddo

  end subroutine deseasonalize_values

  !> Calculate the difference between two real values.
  !!
  !! @param value1 The number being subtracted from (minuend)
  !! @param value2 The number being subtracted (subtrahend)
  !! @return The difference of the two values.
  function calculate_difference_value(value1, value2) result(diff)

    real :: value1
    real :: value2
    real :: diff

    if(is_missing_real(value1) .OR. is_missing_real(value2)) then
      diff = MISSING_REAL
    else
      diff = value1 - value2
    endif

  end function calculate_difference_value

  !> If a target has a neighbor, but that neighbor does not have the target
  !!   as one of its neighbors, set the neigh2targ_index field for that
  !!   target-neighbor pair and also add the neighbor-target pair to the
  !!   neighbors array.
  !!
  !! @param[inout] station_pair The target-neighbor pair being backfilled.
  !! @param[inout] neighbors The entire network of target-neighbor pairs.
  !! @param[in] max_backfilled_neighbors The maximum total number of neighbors to allow for a target station.
  subroutine backfill_neighbor(station_pair, neighbors, max_backfilled_neighbors)

    type(StationNeighborPair) :: station_pair
    type(StationNeighborPair), dimension(:,:) :: neighbors
    integer, intent(in) :: max_backfilled_neighbors

    integer :: target_index
    integer :: neighbor_index
    character(len=11) :: target_id
    character(len=11) :: neighbor_id
    integer :: neigh2targ_index
    integer :: find_targ_index
    integer :: find_n2t_index

    target_id = station_pair%target_id
    target_index = station_pair%target_index
    neighbor_index = station_pair%neighbor_index

    ! check to see if stn1 is a neigh of stn2 (neigh2targ_index=-1 if not)
    neigh2targ_index = -1
    do find_n2t_index = 2, size(neighbors,2)
      find_targ_index = neighbors(neighbor_index, find_n2t_index)%neighbor_index
      ! if there is no value in the pair array, loop out
      if(find_targ_index == 0) exit
      if(find_targ_index == target_index) then
        neigh2targ_index = find_n2t_index
      endif
    enddo

    ! If not back linked, add target to pair neighbor list in order to make the
    ! network to be closed/circular, otherwise the code won't work properly.
    if(.not.is_hcn_record(target_id) .and. (neigh2targ_index == -1)) then
      ! Changing nstns to size(neighbors,2) changes the number of neighbors that
      !   can be added here and therefore changes the output.
      if(find_n2t_index <= max_backfilled_neighbors) then
        neighbor_id = neighbors(neighbor_index,1)%neighbor_id
        neighbors(neighbor_index, find_n2t_index) =  &
            new_StationNeighborPair(neighbor_id, target_id, neighbor_index, target_index, 0.0)
        neigh2targ_index = find_n2t_index
      endif
    endif

    station_pair%neigh2targ_index = neigh2targ_index

  end subroutine backfill_neighbor

  !> Determines the timing of changepoints in each target-neighbor difference series
  !!   (Menne and Williams, 2009, Sections 3-b and 3-c).
  !!   See Req. 1.3 and 1.4 for more information.
  !!
  !! @param[in] station_pair The target-neighbor pair being analyzed.
  !! @param[inout] targ2neigh_spans A slice of the changepoint_spans array for one target index.
  !! @param[out] change_month The array of the month indexes of the changepoints.
  !! @param[out] change_count The number of changepoints in change_month.
  !! @param[in] diff_values The array of differences of a target-neighbor pair.
  subroutine identify_changepoints(station_pair, targ2neigh_spans, change_month, change_count, diff_values)

    type(StationNeighborPair), intent(in) :: station_pair
    type(ChangepointSpan), dimension(:) :: targ2neigh_spans
    integer, dimension(:), intent(out) :: change_month
    integer, intent(out) :: change_count
    type(DifferenceValue), dimension(:), intent(in) :: diff_values

    type(DifferenceValue), dimension(:) :: diff_values_copy(size(diff_values))
    integer :: target_index
    integer :: total_changes
    integer :: merge_changes
    integer :: split_changes
    integer :: change_index
    integer :: month_index
    integer :: begin_month_index
    integer :: end_month_index
    integer :: loop_pass
    integer :: change_month_count
    integer :: num_merge_in2
    integer :: num_merge_out
    integer :: num_values
    integer :: num_split_in2
    integer :: num_split_out
    integer :: year
    integer :: month
    logical :: first_split
    logical :: do_split
    logical :: do_snht
    character(len=1024) :: log_message

    ! inhomogeneous breakpoint array (including begin/end of series)
    integer, dimension(:) :: change_month_indexes(MAX_CHANGEPOINTS)
    integer, dimension(:) :: change_models(MAX_CHANGEPOINTS)
    integer, dimension(:) :: last_split_in2(MAX_CHANGEPOINTS)
    integer, dimension(:) :: last_split_out(MAX_CHANGEPOINTS)
    integer, dimension(:) :: last_merge_in2(MAX_CHANGEPOINTS)
    integer, dimension(:) :: last_merge_out(MAX_CHANGEPOINTS)

    ! General note: During the splitting calls to split_and_merge_segments there is a set of work arrays
    ! used (with "ex" in their names) to accumulate the changes to the changepoint arrays.
    ! All internal results in split_and_merge_segments must be accounted for in these arrays. During the
    ! merging calls to split_and_merge_segments, changes are made only to the incoming arrays.

    diff_values_copy(:) = diff_values(:)

    target_index = station_pair%target_index
    begin_month_index = station_pair%begin_month_index
    end_month_index = station_pair%end_month_index

    ! ------  Initialize data and reference series  ------
    ! set up the temperature arrays and the yr position array
    ! begin_month_index & end_month_index are the begin and end indices of paired station's data

    ! Initialize arguments to test segments:
    ! first_split requires the series to be split the first pass
    first_split = .false.
    total_changes = 1
    loop_pass = 1

    ! change_month_indexes is the month index of the chgpts
    ! change_month_indexes(1) is the begin of series, change_month_indexes(change_month_count) is the end of series
    ! change_models is the model
    change_month_indexes(:) = 0

    num_values = 0
    do month_index = begin_month_index, end_month_index
      if(diff_values_copy(month_index)%difference > MISSING_REAL+1.) then
        num_values = num_values + 1
      endif
    change_month_indexes(1) = begin_month_index
    end do ! end candidate/neighbor loop
    if(num_values < MIN_MONTHS_RAW) then
      call log_info("difference values to missing: "//trim(log_string(begin_month_index))//" "//trim(log_string(end_month_index)))
      do month_index = begin_month_index, end_month_index
        diff_values_copy(month_index)%difference = MISSING_REAL
      enddo
      change_month_count = 1
    else
      change_month_count = 2
      change_month_indexes(change_month_count) = end_month_index
    endif

    ! if there was no changepoints - break first series anyway
    if(change_month_count == 2) first_split = .true.

    ! initialize number of chgpts in last split/merge testseg calls
    num_split_in2 = 0
    num_split_out = 0
    num_merge_in2 = 0
    num_merge_out = 0

    last_split_in2(:) = 0
    last_merge_in2(:) = 0
    last_split_out(:) = 0
    last_merge_out(:) = 0
    change_models(:) = 0

    do_split = .true.
    do_snht = .true.
    ! do_snht = .true. to run the standard normal (in)homogeneity test (snht) that finds
    !   changepoints - Menne and Duchon (2001). do_snht is set to .false. at the end of the
    !   process for the micbic run that finds the best fit models at those changepoints.

    do while (total_changes .ne. 0 .or. do_snht)
      total_changes = 0
      merge_changes = 0
      split_changes = 0

      if(do_split) then
        call split_and_merge_segments(diff_values_copy, do_snht, change_models, first_split, change_month_count,  &
                                      change_month_indexes, num_split_in2, last_split_in2, .true., split_changes)
        do change_index = 1, size(change_month_indexes)
          last_split_in2(change_index) = last_split_out(change_index)
          last_split_out(change_index) = change_month_indexes(change_index)
        enddo
        num_split_in2 = num_split_out
        num_split_out = change_month_count
      endif

      ! If first_split pass complete, loop back and test shorter segments
      if(first_split) then
        first_split = .false.
        cycle
      endif

      call split_and_merge_segments(diff_values_copy, do_snht, change_models, first_split, change_month_count,  &
                                    change_month_indexes, num_merge_in2, last_merge_in2, .false., merge_changes)
      do change_index = 1, size(change_month_indexes)
        last_merge_in2(change_index) = last_merge_out(change_index)
        last_merge_out(change_index) = change_month_indexes(change_index)
      enddo

      num_merge_in2 = num_merge_out
      num_merge_out = change_month_count
      total_changes = split_changes + merge_changes
      loop_pass = loop_pass + 1

      ! test to see if the undoc stats are finished with the split/merge
      if(do_snht .AND. (total_changes == 0 .OR. loop_pass > 10)) then
        ! setup minbic run for the end of the process
        total_changes = 1
        do_snht = .false.
        loop_pass = 1
        do_split = .false.

        call test_segment_min_length(diff_values_copy, targ2neigh_spans, change_month_indexes, target_index, change_month_count)

      endif
      ! if do_snht has been turned OFF and either no changes have occured or there have
      ! been 10 passes then leave split_and_merge_segments loop
      if(.not. do_snht .AND. (total_changes == 0 .OR. loop_pass > 10)) then
        exit
      endif

    enddo

    ! initialize output arrays
    change_month(:) = 0
    change_count = 0
    do change_index = 2, change_month_count-1

      if(change_models(change_index) >= 3) then
        change_count = change_count + 1
        ! year/month of sig changepoint (beg-end of span)
        change_month(change_count) = change_month_indexes(change_index)

        ! Logging
        call get_year_month_from_month_index(year, month, change_month(change_count))
        write(log_message,'(a,2i5,1x," TESTSEG ADJ: ",i5,i5,i3," model:",i2)')  &
               station_pair%target_id//"-"//station_pair%neighbor_id, target_index, station_pair%neighbor_index,  &
               change_month(change_count), year, month, change_models(change_index)
        call log_info(log_message)

      endif
    enddo

  end subroutine identify_changepoints

  !> Either splits (creates a new changepoint between two existing changepoints)
  !!   or merges (removes a changepoint that is not significant). For the final pass
  !!   this routine also classifies the changepoints into model types that are used to
  !!   remove insignificant changepoints. (Menne and Williams, 2009, Sections 3-b and 3-c)
  !!   See Req. 1.3 and 1.4 for more information.
  !!
  !! @param[in] diff_values The array of value differences of a target-neighbor pair.
  !! @param[in] do_snht The logical flag when true find and elliminates chagepoints
  !!   and when false changepoint models are identified.
  !! @param[inout] change_models The array of the model types of the changepoints.
  !! @param[in] first_split The logical flag when true forces splits of segments
  !!   on the first pass by adding changepoints even when the test statistic
  !!   is not greater than the critical value. Currently this is used to force a
  !!   changepoint at the peak, whether significant or not, to test the shorter series.
  !! @param[inout] change_month_count The number of changepoints in change_month_indexes.
  !! @param[inout] change_month_indexes The array of the month indexes of the changepoints.
  !! @param[in] last_month_count The number of changepoints in last_month_index.
  !! @param[in] last_month_index The array of the month indexes of the changepoints
  !!   from two cycles ago.
  !! @param[in] do_split The logical flag when true splits segments by adding changepoints,
  !!   when false merges segments by deleting changepoints.
  !! @param[out] change_count The number of changes to the changepoints for either a
  !!   split round or merge round.
  subroutine split_and_merge_segments(diff_values, do_snht, change_models, first_split, change_month_count,   &
                     change_month_indexes, last_month_count, last_month_index, do_split, change_count)
    use CommonVariables, only: por_months

    type(DifferenceValue), dimension(:), intent(in) :: diff_values
    logical, intent(in) :: do_snht
    integer, dimension(:) :: change_models
    logical, intent(in) :: first_split
    integer :: change_month_count
    integer, dimension(:) :: change_month_indexes
    integer, intent(in) :: last_month_count
    integer, dimension(:), intent(in) :: last_month_index
    logical, intent(in) :: do_split
    integer, intent(out) :: change_count

    integer :: change_index
    integer :: stable_index
    integer :: data_index
    integer :: tstat_index
    integer :: month_index
    integer :: start_month_index
    integer :: stop_month_index
    integer :: expans_pointer
    integer :: tstat_max_index
    integer :: end1_month_index
    integer :: end1_data_index
    integer :: first_change
    integer :: last_change
    integer :: model_type
    integer :: expans_limit
    integer :: valid_count
    integer :: month_count
    integer :: snht_threshold
    real :: critical_value
    real :: test_stat
    real :: tstat_max
    real :: schwarz_min
    real :: sse_best
    real :: offset_best
    logical :: is_homogeneous

    integer, dimension(:) :: expans_month_indexes(size(change_month_indexes))
    integer, dimension(:) :: expans_models(size(change_models))
    ! difference between cand & composite neighbors (q) and standardized (standard) series
    real, dimension(:) :: x_data(por_months)
    real, dimension(:) :: y_data(por_months)
    real, dimension(:) :: standard_values(por_months)
    ! T-statistic and 9-point Binomial Average series derived from standard_values
    real, dimension(:) :: t_stats(por_months)
    integer, dimension(:) :: seg_value_counts(2)
    real, dimension(:) :: best_y_intercepts(2)
    real, dimension(:) :: best_slopes(2)

    ! SNHT significance level (SNHT_THRES)
    ! 1 = 97.5%; 5 = 95%; 10=90%
    snht_threshold = get_property_int(PROP_SNHT_THRESHOLD)

    change_count = 0

    ! initialize inhomog expansion series array and indices these are working arrays
    ! which are copied back to the incoming arrays at the end of the split process
    if(do_split) then
      expans_limit = change_month_count
      expans_month_indexes(:) = change_month_indexes(:)
      expans_models(:) = change_models(:)
    endif

    ! Initialize array pointers. Always start out with the first segment.
    first_change = 0
    if(do_split) then
      last_change = 1
    else
      last_change = 2
    endif
    expans_pointer = last_change

    ! Loop through all of the incoming segments (parse) or adjacent incoming segments (merge)
    ! splitting when definitely inhomogeneous, keeping when definitely homogeneous, and restraining
    ! when questionable
    segment_loop: do while (last_change < change_month_count)

      ! Update chgpt date pointers for next segment at beginning of loop
      ! since some if/else branches cycle before getting to the very end.
      last_change = last_change + 1
      first_change = first_change + 1
      expans_pointer = expans_pointer + 1

      is_homogeneous = .false.
      if(first_change == 1) then
        start_month_index = change_month_indexes(first_change)
      else
        start_month_index = change_month_indexes(first_change)+1
      endif
      stop_month_index = change_month_indexes(last_change)

      if(do_snht) then

        ! is this segment stable (that is, is the segment the same as the
        ! last time split_and_merge_segments was called)
        stable_loop: do stable_index = 1, last_month_count-1
          if(change_month_indexes(first_change) == last_month_index(stable_index)) then
            do change_index = 1, last_change-first_change
              if(change_month_indexes(first_change+change_index) /= last_month_index(stable_index+change_index)) &
                exit stable_loop
            enddo
            cycle segment_loop
          endif
        enddo stable_loop
      end if

      ! initialize final stat series
      x_data(:) = MISSING_REAL
      y_data(:) = MISSING_REAL
      standard_values(:) = MISSING_REAL
      t_stats(:) = 0.0

      ! ------ Calculate/Recalculate ref series for current segment --------
      ! estimate variance of each monthly time series
      ! valid_count = number of non-missing months between start_month_index & stop_month_index
      ! month_count = number of serial months between start_month_index & stop_month_index
      valid_count = 0
      data_index = 0
      do month_index = start_month_index,stop_month_index
        data_index = data_index + 1
        x_data(data_index) = month_index
        y_data(data_index) = diff_values(month_index)%difference
        if(diff_values(month_index)%difference > MISSING_REAL+1.) valid_count = valid_count + 1
      enddo
      month_count = stop_month_index - start_month_index + 1

      if(valid_count < MIN_MONTHS_RAW) then
        cycle segment_loop
      endif

      ! standardize series for split/merge stats
      if(do_snht) then
        call standardize_values(y_data, month_count, standard_values)
      else
        ! for estamt use full temperature series
        do data_index = 1, month_count
          standard_values(data_index) = y_data(data_index)
        enddo
      endif

      ! ------ Generate test statistic for shift or step-change ----------
      if(do_snht) then

        call calculate_snht_statistics(standard_values, month_count, t_stats)
        model_type = 3

        ! -------- Evaluate test statistic wrt threshold ---------------------
        tstat_max_index = 0
        tstat_max = 0.0
        do tstat_index = 2, month_count - 1
          if(t_stats(tstat_index) > tstat_max) then
            tstat_max_index = tstat_index
            tstat_max = t_stats(tstat_index)
          endif
        enddo

        critical_value = get_critical_value(valid_count, snht_threshold)

        if(tstat_max_index <= 0) then
          is_homogeneous = .true.
          if(.not. do_split) then
            ! for merging, this is a change (collapse). for parsing, this is NOT a change
            call collapse_breakpoints(first_change, last_change, change_month_count, change_month_indexes,  &
                     change_models, change_count)
          endif
          cycle segment_loop
        endif

        ! test the peak stat against the critical value
        if(tstat_max < critical_value) is_homogeneous = .true.
        ! set the month index of the changepoint
        end1_month_index = x_data(tstat_max_index)

        ! Fragment First if either homog or inhomog
        if(first_split) then
          ! force first to split if homog, to test shorter segments
          if(is_homogeneous) is_homogeneous = .false.
        else if(.not. do_split) then
          if(is_homogeneous)then
            ! in a merge pass, collapse a homog chgpt
            ! in a merge pass, leave an inhomog segment alone
            call collapse_breakpoints(first_change, last_change, change_month_count, change_month_indexes,  &
                    change_models, change_count)
          end if
          cycle segment_loop
        else
          if(is_homogeneous) then
            ! in a split pass, leave a homog segment alone
            cycle segment_loop
          endif
        endif

        ! in a split pass, fragment an inhomog segment
        ! This is the ONLY parsing change point! These must NOT be
        ! added back into the orginal chgpt array, but kept in an array of their own
        change_count = change_count + 1

        ! expand expansion year series arrays to make room for the new changepoint.
        expans_limit = expans_limit + 1
        do change_index = expans_limit, expans_pointer+1, -1
          expans_month_indexes(change_index) = expans_month_indexes(change_index-1)
          expans_models(change_index) = expans_models(change_index-1)
        enddo

        ! add the new changepoint
        expans_month_indexes(expans_pointer) = end1_month_index
        expans_models(expans_pointer) = model_type
        expans_pointer = expans_pointer + 1

      else if(.not. do_snht) then

        ! Run the BI
        end1_month_index = change_month_indexes(first_change+1)
        do data_index = 1, month_count
          if(end1_month_index < x_data(data_index)) then
            end1_data_index = data_index - 1
            exit
          endif
        enddo

        ! determine the optimum Bayesian Info Criteria for found station/chgpt
        call minbic(1, x_data, y_data, end1_data_index, month_count, critical_value, test_stat, schwarz_min,  &
                      offset_best, best_y_intercepts, best_slopes, sse_best, model_type, seg_value_counts)

        ! tests for "break slope" model inconclusive, see versions before saving BIC data
        change_models(first_change+1) = model_type
      endif
    enddo segment_loop ! end of series segmenting loop

    if(do_split .AND. change_count > 0) then
      ! repopulate the incoming array if this is a split pass with changes
      change_month_count = expans_limit
      do change_index = 1, expans_limit
        change_month_indexes(change_index) = expans_month_indexes(change_index)
        change_models(change_index) = expans_models(change_index)
      enddo
    endif

  end subroutine split_and_merge_segments

  !> During a merge round, removes changepoints that are found not to be significant.
  !!
  !! @param[inout] first_change The first changepoint index for segment analysis.
  !! @param[inout] last_change The last changepoint index for segment analysis.
  !! @param[inout] change_month_count The number of changepoints in change_month_indexes.
  !! @param[inout] change_month_indexes The array of the month indexes of the changepoints.
  !! @param[inout] change_models The array of the model types of the changepoints.
  !! @param[inout] change_count The number of changes to the changepoints for either a
  !!   split round or merge round.
  subroutine collapse_breakpoints(first_change, last_change, change_month_count, change_month_indexes,  &
                    change_models, change_count)

    integer :: first_change
    integer :: last_change
    integer :: change_month_count
    integer, dimension(:) :: change_month_indexes
    integer, dimension(:) :: change_models
    integer :: change_count

    integer :: i

    ! collapse the breakpoint array
    do i = first_change+1, change_month_count-1
      change_month_indexes(i) = change_month_indexes(i+1)
      change_models(i) = change_models(i+1)
    enddo
    change_month_count = change_month_count - 1
    last_change = last_change - 1
    first_change = first_change - 1
    change_count = change_count + 1

  end subroutine collapse_breakpoints

  !> At the end of the split/merge/classify process if changepoints are too close
  !!   together, then removes the changepoints, deletes the data and marks for deletion
  !!   in changepoint_spans.
  !! @param[inout] diff_values An array of differences of the paired values.
  !! @param[inout] targ2neigh_spans A slice of the changepoint_spans array for one neighbor
  !!   index.
  !! @param[inout] change_month_indexes The array of the month indexes of the changepoints.
  !! @param[in] target_index The station index of the target.
  !! @param[inout] change_month_count The number of changepoints in change_month_indexes.
  subroutine test_segment_min_length(diff_values, targ2neigh_spans, change_month_indexes, target_index, change_month_count)

    type(DifferenceValue), dimension(:) :: diff_values
    type(ChangepointSpan), dimension(:) :: targ2neigh_spans
    integer, dimension(:) :: change_month_indexes
    integer, intent(in) :: target_index
    integer :: change_month_count

    integer :: sky_index
    integer :: change_index
    integer :: change1_month
    integer :: change2_month
    integer :: month_index
    integer :: month_begin
    integer :: month_end
    integer :: non_missing

    ! At this point, Merge sees all of the changepoints as valid.
    ! For all of the segments less than min_test(=5)
    !   1) delete data in pair_diffs
    !   2) increment nDelete
    !   3) remove changepoint (at upper end of segment)
    ! The number of chgpts may change, use variables for loop

    change1_month = 1
    change2_month = 2
    do while (change2_month .le. change_month_count)
      if(change1_month == 1) then
        month_begin = change_month_indexes(change1_month)
      else
        month_begin = change_month_indexes(change1_month)+1
      endif
      month_end = change_month_indexes(change2_month)
      non_missing = 0
      do month_index = month_begin, month_end
        if(diff_values(month_index)%difference > MISSING_REAL+1.) non_missing = non_missing + 1
      enddo

      if(non_missing < MIN_MONTHS_RAW) then
        do month_index = month_begin, month_end
          if(diff_values(month_index)%difference > MISSING_REAL+1.) then
            sky_index = get_sky_from_month_index(target_index,month_index)
            targ2neigh_spans(sky_index)%is_deleted = .true.
            diff_values(month_index)%difference = MISSING_REAL
          endif
        enddo
        do change_index = change2_month, change_month_count
          change_month_indexes(change_index - 1) = change_month_indexes(change_index)
        enddo
        change_month_indexes(change_month_count) = 0
        change_month_count = change_month_count - 1
      else ! Don't skip sequential short segments
        change2_month = change2_month + 1
        change1_month = change1_month + 1
      endif
    enddo

  end subroutine test_segment_min_length

  !> Prunes short changepoint spans and stores the changepoint data for later use
  !!   (Menne and Williams (2009), beginning of section 3-d).
  !!
  !! @param[in] station_pair The target-neighbor pair being analyzed.
  !! @param[inout] diff_values An array of differences of the paired values.
  !! @param[inout] targ2neigh_spans A slice of the changepoint_spans array for the target station.
  !! @param[inout] neigh2targ_spans A slice of the changepoint_spans array .
  !! @param[inout] changepoint_sums For all skyline the cumulative adjustments due to changepoints
  !!   from station pairs.
  !! @param[in] change_month An array of monthly indexes of significant changepoints
  !!   for a target/neighbor pair.
  !! @param[in] change_count The number of the changepoints for a target/neighbor pair.
  subroutine prune_and_store_changepoints(station_pair, diff_values, targ2neigh_spans, neigh2targ_spans,  &
                 changepoint_sums, change_month, change_count)
    use CommonVariables, only: por_months

    type(StationNeighborPair), intent(in) :: station_pair
    type(DifferenceValue), dimension(:) :: diff_values
    type(ChangepointSpan), dimension(:) :: targ2neigh_spans
    type(ChangepointSpan), dimension(:) :: neigh2targ_spans
    type(ChangepointSums), dimension(:) :: changepoint_sums
    integer, dimension(:), intent(in) :: change_month
    integer, intent(in) :: change_count

    real, dimension(:) :: x_data(size(diff_values))
    real, dimension(:) :: y_data(size(diff_values))
    integer, dimension(:) :: interim_change_month(size(change_month))
    integer, dimension(:) :: model_types(por_months)
    integer, dimension(:) :: span_nums(por_months)
    logical(1), dimension(:) :: is_deleted(por_months)
    real, dimension(:) :: adjustments(MAX_CHANGEPOINTS)
    integer, dimension(:) :: saved_end1s(MAX_CHANGEPOINTS)
    integer, dimension(:) :: interim_month_indexes(MAX_CHANGEPOINTS)
    integer, dimension(:,:) :: change_counts(por_months, 2)
    real, dimension(:,:) :: change_sums(por_months, 2)
    real, dimension(:,:) :: change_zscores(por_months, 2)
    real, dimension(:) :: best_y_intercepts(2)
    real, dimension(:) :: best_slopes(2)
    integer, dimension(:) :: seg_value_counts(2)
    integer :: target_index
    integer :: neighbor_index
    integer :: neigh2targ_index
    integer :: sky_index
    integer :: change_index
    integer :: data_index
    integer :: begin_data_index
    integer :: end1_data_index
    integer :: end2_data_index
    integer :: month_index
    integer :: end_month_index
    integer :: begin_month_index
    integer :: min_month_index
    integer :: interim_index
    integer :: missing_index
    integer :: target_first_year
    integer :: target_last_year
    integer :: neigh_first_year
    integer :: neigh_last_year
    integer :: common_first_year
    integer :: common_last_year
    integer :: target_first_sky
    integer :: target_last_sky
    integer :: neigh_first_sky
    integer :: neigh_last_sky
    integer :: negative_count
    integer :: positive_count
    integer :: zero_count
    integer :: diffs_count
    integer :: diff_zero_count
    integer :: min_model_type
    integer :: values_count
    integer :: end_change_month
    integer :: model_type
    integer :: num_interim
    integer :: min_seg_length
    integer :: sum_count
    real :: zscore_sum
    real :: val1_sqr
    real :: val2_sqr
    real :: std_val1
    real :: std_val2
    real :: val1_sum
    real :: val2_sum
    real :: vals_count
    real :: low_std
    real :: change_adj
    real :: change_zscore
    real :: critical_thresh_best
    real :: test_stat
    real :: schwarz_min
    real :: schwarz_lowest
    real :: sse_best
    real :: offset_sum
    real :: offset_best
    logical :: is_same_sign

    min_seg_length = get_property_int(PROP_ADJUST_MIN_LENGTH)

    target_index = station_pair%target_index
    neighbor_index = station_pair%neighbor_index
    begin_month_index = station_pair%begin_month_index
    end_month_index = station_pair%end_month_index
    neigh2targ_index = station_pair%neigh2targ_index

    ! find the begin and end years of the common POR
    ! between the Target and current Paired neighbor
    target_first_year = get_first_year(target_index)
    target_last_year = get_last_year(target_index)
    neigh_first_year = get_first_year(neighbor_index)
    neigh_last_year = get_last_year(neighbor_index)
    common_first_year = max(target_first_year, neigh_first_year)
    common_last_year = min(target_last_year, neigh_last_year)

    ! then find the first and last indices of the Targ, Pair
    ! Skyline indices of the common POR
    target_first_sky = get_sky_from_year_month(target_index, common_first_year, 1)
    neigh_first_sky = get_sky_from_year_month(neighbor_index, common_first_year, 1)
    target_last_sky = get_sky_from_year_month(target_index, common_last_year, 12)
    neigh_last_sky = get_sky_from_year_month(neighbor_index, common_last_year, 12)

    ! initialize statsubs temporary arrays
    change_sums(:,:) = 0
    change_zscores(:,:) = 0
    change_counts(:,:) = 0

    change_index = 1
    model_types(:) = 0
    span_nums(:) = 0
    is_deleted(:) = FALSE

    do while(change_index .le. change_count)
      ! initialize segment series and raw diff series
      x_data(:) = MISSING_REAL
      y_data(:) = MISSING_REAL

      ! initialize interim changepoints
      interim_index = 1
      interim_change_month(interim_index) = change_month(change_index)
      values_count = 0

      ! if any are within min_seg_length, put into interim chgpt array
      do while(values_count .le. min_seg_length .and. change_index .lt. change_count)
        values_count = 0
        do month_index = change_month(change_index)+1, change_month(change_index+1)
          if(diff_values(month_index)%difference > MISSING_REAL + 1.0) values_count = values_count + 1
        enddo
        if(values_count <= min_seg_length) then
          change_index = change_index + 1
          interim_index = interim_index + 1
          interim_change_month(interim_index) = change_month(change_index)
        endif
      enddo
      if(change_index < change_count) then
        end_change_month = change_month(change_index+1)
      else
        end_change_month = end_month_index
      endif

      num_interim = interim_index
      val1_sum = 0.0
      val2_sum = 0.0
      val1_sqr = 0.0
      val2_sqr = 0.0
      vals_count = 0.0

      ! set up the x/y segments for testing
      data_index = 0
      diffs_count = 0
      diff_zero_count = 0
      do month_index = begin_month_index, end_change_month
        if(diff_values(month_index)%difference > MISSING_REAL + 1.0) then
          if(data_index == 0) begin_data_index = month_index
          data_index = month_index - begin_data_index + 1
          ! data series for minbic
          y_data(data_index) = diff_values(month_index)%difference
          x_data(data_index) = month_index

          ! count exactly equal data
          if(diff_values(month_index)%difference == 0.0)  &
              diff_zero_count = diff_zero_count+1
          diffs_count = diffs_count + 1


          ! for the standardized offset
          val1_sum = val1_sum + diff_values(month_index)%value1
          val2_sum = val2_sum + diff_values(month_index)%value2
          val1_sqr = val1_sqr + diff_values(month_index)%value1 * diff_values(month_index)%value1
          val2_sqr = val2_sqr + diff_values(month_index)%value2 * diff_values(month_index)%value2
          vals_count = vals_count + 1
        endif
      enddo
      end2_data_index = data_index

      ! skip if paired series are duplicate
      if(diff_zero_count /= diffs_count) then

        ! calculate std for each station in window.... assume the
        ! lower std as the one without a chgpt and use for chgpt zscore
        std_val1 = sqrt((val1_sqr - (val1_sum * val1_sum / vals_count)) / (vals_count - 1))
        std_val2 = sqrt((val2_sqr - (val2_sum * val2_sum / vals_count)) / (vals_count - 1))
        low_std = std_val1
        if(std_val2 < low_std .AND. abs(std_val2 - 0.0) >= FP_EPSILON) low_std = std_val2

        ! Code modifications from here to "if(min_model_type .ge. 3)" are due
        ! to an inaccurate "corner case" found by D. Rothenberger Aug.2011.
        ! The case involved an "interim chgpt" condition with 3 or more
        ! changepoints with the first having an adjustment of 0.0.....
        ! run BIC on all of the interior points, keeping the best BIC Q chgpt
        schwarz_lowest = 9999.
        do interim_index = 1, num_interim
          do data_index = 1, end2_data_index
            if(x_data(data_index) .ne. MISSING_REAL .AND. x_data(data_index) <= interim_change_month(interim_index))  &
                end1_data_index = data_index
          enddo
          call minbic(2, x_data, y_data, end1_data_index, end2_data_index, critical_thresh_best, test_stat, schwarz_min,  &
                        offset_best, best_y_intercepts, best_slopes, sse_best, model_type, seg_value_counts)
          adjustments(interim_index) = offset_best
          saved_end1s(interim_index) = end1_data_index
          interim_month_indexes(interim_index) = int(x_data(end1_data_index))
          if(schwarz_min < schwarz_lowest) then
            min_month_index = int(x_data(end1_data_index))
            min_model_type = model_type
            schwarz_lowest = schwarz_min
            change_adj = offset_best
          endif
        enddo

        ! see if all of the adjustments have the same sign (is_same_sign = .true.)
        ! or not (is_same_sign = .false.)
        ! With only 2 "interim" changepoints -
        !   Zero is considered "neutral" - neither positive nor negative and data is not removed
        ! With 3 or more -
        !   Zero is considered "confused" - and is considered part of the problem
        !   1) if all other chgpts same sign, treat as "Same domain"
        !   2) if opposite sign, treat as "Diff domain"

        ! Init as Same Domain
        is_same_sign = .true.
        positive_count = 0
        negative_count = 0
        zero_count = 0
        if(num_interim > 1) then
          do interim_index = 1, num_interim
            if(adjustments(interim_index) > 0.0) then
              positive_count = positive_count + 1
            else if(adjustments(interim_index) < 0.0) then
              negative_count = negative_count + 1
            else
              zero_count = zero_count + 1
            endif
          enddo
        endif

        !  Diff domain if +/- chgpts occur
        if(positive_count > 0 .AND. negative_count > 0) is_same_sign = .false.

        !  if the interim chgpts are all the same sign, pick best
        !  min_model_type will be >= 3 and best (schwarz_lowest) will be kept
        !  if all of the interim chgpts came back as straight lines
        !  min_model_type will be <=2 and all chgpts will be skipped
        if (.NOT.(is_same_sign .OR. zero_count == num_interim)) then
          ! if chgpts are not same sign, assume unusable data - remove
          ! OPTION 1: data between interim chgpts.
          end1_data_index = saved_end1s(1)
          missing_index = end1_data_index

          do month_index = interim_month_indexes(1)+1, interim_month_indexes(num_interim)
            is_deleted(month_index) = TRUE
            diff_values(month_index)%difference = MISSING_REAL
            missing_index = missing_index + 1
            x_data(missing_index) = MISSING_REAL
            y_data(missing_index) = MISSING_REAL
          enddo

          ! segment evaluation has changed, go back through MINBIC for finals
          call minbic(2, x_data, y_data, end1_data_index, end2_data_index, critical_thresh_best, test_stat,   &
                        schwarz_min, offset_best, best_y_intercepts, best_slopes, sse_best, model_type, seg_value_counts)
          change_adj = offset_best
          min_month_index = interim_month_indexes(1)
          min_model_type = model_type
        endif

        if(min_model_type >= 3) then
          do month_index = min_month_index, por_months

            ! Save the changepoint for the confirm filters
            model_types(month_index) = min_model_type
            span_nums(month_index) = change_index

            ! the changpoint is POSITIVE for the first station of pair
            change_sums(month_index,1) = change_sums(month_index,1) + change_adj
            change_zscore = abs(change_adj / low_std)
            change_zscores(month_index,1) = change_zscores(month_index,1) + change_zscore
            change_counts(month_index,1) = change_counts(month_index,1) + 1

            ! the changpoint is NEGATIVE for the second station of pair
            change_sums(month_index,2) = change_sums(month_index,2) - change_adj
            change_zscores(month_index,2) = change_zscores(month_index,2) + change_zscore
            change_counts(month_index,2) = change_counts(month_index,2) + 1

            ! This condition sets only one time increment at changepoint if there is
            !   no missing data found in the next time slot.
            ! If there is missing data all the subsequent missing slots are spanned.
            if(diff_values(month_index+1)%difference > MISSING_REAL + 1.0) exit
          enddo
        endif

      else
        call log_info("Segments are equal: "//station_pair%target_id//"-"//station_pair%neighbor_id)
      end if

      ! set beginning of next test segment to best interim chgpt
      begin_month_index = min_month_index + 1
      change_index = change_index + 1
    enddo

    ! Accumulate 1st set of chgpt results into the work arrays, Candidate first
    month_index = get_month_index(common_first_year, 1)
    do sky_index = target_first_sky, target_last_sky
      targ2neigh_spans(sky_index) = &
          new_ChangepointSpan(model_types(month_index), span_nums(month_index), is_deleted(month_index))
      month_index = month_index + 1
    enddo

    ! Neighbor (paired) second - iff candidate is a neighbor of the neighbor
    if(neigh2targ_index > 0) then
      neigh2targ_spans(neigh_first_sky:neigh_last_sky) = targ2neigh_spans(target_first_sky:target_last_sky)
    endif

    ! Accumulate 2nd set of chgpt results, Candidate first
    month_index = get_month_index(common_first_year, 1)
    do sky_index = target_first_sky, target_last_sky
      offset_sum = changepoint_sums(sky_index)%offset_sum + change_sums(month_index, 1)
      zscore_sum = changepoint_sums(sky_index)%zscore_sum + change_zscores(month_index, 1)
      sum_count = changepoint_sums(sky_index)%sum_count + change_counts(month_index, 1)
      changepoint_sums(sky_index) = new_ChangepointSums(offset_sum, zscore_sum, sum_count)
      month_index = month_index + 1
    enddo

    ! Neighbor (paired) second
    if(neigh2targ_index > 0) then
      month_index = get_month_index(common_first_year, 1)
      do sky_index = neigh_first_sky, neigh_last_sky
        offset_sum = changepoint_sums(sky_index)%offset_sum + change_sums(month_index, 2)
        zscore_sum = changepoint_sums(sky_index)%zscore_sum + change_zscores(month_index, 2)
        sum_count = changepoint_sums(sky_index)%sum_count + change_counts(month_index, 2)
        changepoint_sums(sky_index) = new_ChangepointSums(offset_sum, zscore_sum, sum_count)
        month_index = month_index + 1
      enddo
    endif

  end subroutine prune_and_store_changepoints

  !> Standardizes input data values. See req. 1.3.1
  !!
  !! @param[in] data_values The input array of data values.
  !! @param[in] data_count The number of data values.
  !! @param[out] standard_values The output array of standardized data values.
  subroutine standardize_values(data_values, data_count, standard_values)

    real, dimension(:), intent(in) :: data_values
    integer, intent(in) :: data_count
    real, dimension(:), intent(out) :: standard_values

    integer :: data_index
    real :: values_mean
    real :: valid_count
    real :: values_sum
    real :: stand_dev
    real :: variance_sum

    ! Compute mean
    values_sum = 0.0
    valid_count = 0.0
    do data_index=1, data_count
      if(.not. is_missing_real(data_values(data_index))) then
        values_sum = data_values(data_index) + values_sum
        valid_count = valid_count + 1
      endif
    enddo
    values_mean = values_sum / valid_count

    ! Compute standard deviation
    variance_sum = 0.0
    do data_index = 1, data_count
      if(.not. is_missing_real(data_values(data_index))) variance_sum = variance_sum + (data_values(data_index)-values_mean)**2
    enddo
    stand_dev = sqrt(variance_sum / (valid_count-2))

    ! Normalize data with mean and standard deviation
    do data_index = 1, data_count
      if(.not. is_missing_real(data_values(data_index)) .and. stand_dev /= 0) then
        standard_values(data_index) = (data_values(data_index) - values_mean) / stand_dev
      else
        standard_values(data_index) = MISSING_REAL
      endif
    enddo

  end subroutine standardize_values

  !> Gets the critical value of the standard normal (in)homogeneity test statistic
  !!   based on the significance level and the number of observations.
  !!
  !! @param values_count The number of values.
  !! @param snht_threshold The configured SNHT significance level
  !! @return critical_value The critical value of the snht statistic.
  function get_critical_value(values_count, snht_threshold) result(critical_value)

    integer :: values_count
    integer :: snht_threshold
    real :: critical_value

    integer :: signif_index
    integer :: diff_2vals
    integer :: diff_obs_val
    integer, parameter :: sig_count = 14
    integer, dimension(:) :: num_sig(sig_count)
    real, dimension(:) :: sig_90(sig_count)
    real, dimension(:) :: sig_95(sig_count)
    real, dimension(:) :: sig_975(sig_count)
    real, dimension(:) :: sig_vals(sig_count)

    ! Set look-up tables of critical values of ts for one-sided
    ! 90%, 95%, and 97.5% significance levels.
    ! The values are taken from the 1986 and 1997 papers by Alexandersson,
    ! and Alexandersson and Moberg, respectively.
    if(snht_threshold == OPT_SNHT_95) then ! This is default so put first
      data sig_95/4.54, 5.70,6.95,7.65,8.10,8.45,8.65,8.80,8.95,9.05,9.15,9.35,9.55,9.70/
      sig_vals = sig_95
    else if(snht_threshold == OPT_SNHT_90) then
      data sig_90/4.27, 5.05,6.10,6.65,7.00,7.25,7.40,7.55,7.70,7.80, 7.85,8.05,8.20,8.35/
      sig_vals = sig_90
    else if(snht_threshold == OPT_SNHT_97_5) then
      data sig_975/4.71, 6.25,7.80,8.65,9.25,9.65,9.85,10.1,10.2,10.3,10.4,10.8,11.0,11.2/
      sig_vals = sig_975
    end if

    data num_sig/5,  10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250/

    ! SNHT one-sided critical value estimations. No good if less than 10....
    if (values_count < num_sig(1)) then
      critical_value = 99999.
    ! if values_count is greater than 250 set sig-lev = sig at 250
    else if(values_count >= num_sig(sig_count)) then
      critical_value = sig_vals(sig_count)
    else
      ! Select the critical value of t for the specified significance
      ! level for the input number of values
      do signif_index = 1, sig_count
        if(values_count < num_sig(signif_index)) exit
      enddo
      ! Estimate critical values of ts using linear interpolation between table values
      diff_2vals = num_sig(signif_index) - num_sig(signif_index-1)
      diff_obs_val = values_count - num_sig(signif_index-1)
        critical_value = ((sig_vals(signif_index) - sig_vals(signif_index-1)) / diff_2vals) *   &
                           diff_obs_val +  sig_vals(signif_index-1)
    endif

  end function get_critical_value

  !> Calculates the standard normal (in)homogeneity test
  !!   statistic used to find a step change or shift in the standardized
  !!   difference data between a candidate and its neighbor. The algorithm
  !!   to calculate test_stat is Eq.(4) in Alexandersson and Moberg,
  !!   Int'l Jour. Climat., 17, 25-34 (1997).
  !!
  !! @param[in] standard_values The input array of standardized data values.
  !! @param[in] data_count The number of data values.
  !! @param[out] test_stats The output array of statistic values.
  subroutine calculate_snht_statistics(standard_values, data_count, test_stats)

    real, dimension(:), intent(in) :: standard_values
    integer, intent(in) :: data_count
    real, dimension(:), intent(out) :: test_stats

    integer :: data_index
    integer :: seg_index
    integer :: seg1_count
    integer :: seg2_count
    real :: seg1_mean
    real :: seg2_mean

    test_stats(:) = MISSING_REAL

    ! Loop through all possible "change" points
    do data_index = 1, data_count - 1    !use this if window is used
      if(.not. is_missing_real(standard_values(data_index))) then
        seg1_mean = 0.0
        seg2_mean = 0.0
        seg1_count = 0
        seg2_count = 0

        ! Compute mean of first segment
        do seg_index = 1, data_index
          if(standard_values(seg_index) > MISSING_REAL + 1.0) then
            seg1_mean = seg1_mean + standard_values(seg_index)
            seg1_count = seg1_count + 1
          endif
        enddo

        if(seg1_count /= 0) then
          seg1_mean = seg1_mean / seg1_count
        else
          cycle
        endif

        ! Compute mean of second segment
        do seg_index = data_index+1, data_count
          if(standard_values(seg_index) > MISSING_REAL + 1.0) then
            seg2_mean = seg2_mean + standard_values(seg_index)
            seg2_count = seg2_count + 1
          endif
        enddo

        if(seg2_count /= 0) then
          seg2_mean = seg2_mean / seg2_count
        else
          cycle
        endif

        ! Compute Standard Normal Homogeneity Test statistic
        test_stats(data_index) = seg1_count * seg1_mean * seg1_mean +   &
                                 seg2_count * seg2_mean * seg2_mean
      endif
    enddo

  end subroutine calculate_snht_statistics

end module FindChangepoints

!> @file
!! Contains functionality for locating all potential change points in each
!! difference series.
