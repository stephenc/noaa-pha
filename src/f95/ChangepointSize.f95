!> @brief
!! Calculates the size of the adjustment for each change point.
!! See Req. 1.8 for more information.
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
!!
module ChangepointSize

  use Logger
  use PropertyReader
  use MathUtils
  use SkylineUtils
  use ModelFitUtils
  use StationNeighborPairType
  use ElementValueType
  use ChangepointType
  use CommonVariables
  use PropertyParameters
  use AlgorithmParameters

  implicit none 

contains

  !> Estimates the amplitude of the changepoints.
  !!
  !! @param[in] neighbors The array of station neighbors and their IDs.
  !! @param[inout] element_data_process An array for all skyline for the processed data values.
  !! @param[inout] merged_hits Merged documented and undocumented changepoints.
  !! @param[out] changepoints The final changepoints, sky_month and amplitude, for the entire network.
  !! @param[in] delete_flags Flag for data deletion.
  !! @param[out] num_changes The number of changepoints in each target station.
  !! @param[out] unstable_dates A list of unstable dates per station.
  subroutine estimate_changepoint_size(neighbors, element_data_process, changepoints, merged_hits, delete_flags,  &
                                       num_changes, unstable_dates)
    use CommonVariables, only: skyline_months

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ElementValue), dimension(:) :: element_data_process
    integer, dimension(:) :: merged_hits
    type(Changepoint), dimension(:,:), allocatable, intent(out) :: changepoints
    logical(1), dimension(:,:), intent(in) :: delete_flags
    integer, dimension(:), allocatable, intent(out) :: num_changes
    integer, dimension(:,:), allocatable, intent(out) :: unstable_dates

    character(len=11) :: target_id
    integer :: step_index
    integer :: station_index
    integer :: target_index
    integer :: neighbor_index
    integer :: targ2neigh_index
    integer :: sky_index
    integer :: change_index
    integer :: data_index
    integer :: month_index
    integer :: begin_month_index
    integer :: end1_month_index
    integer :: end2_month_index
    integer :: end1_data_index
    integer :: end2_data_index
    integer :: change_num
    integer :: change1
    integer :: change2
    integer :: targets_size
    integer :: neighbors_size
    integer :: stations_passed
    integer :: unstable_count
    ! begin & end month indices for target segments 1 & 2 (sky indices)
    integer :: begin1_sky
    integer :: end1_sky
    integer :: begin2_sky
    integer :: end2_sky
    integer :: num_compare
    integer :: num_adj_filter
    integer :: seg1_months
    integer :: seg2_months
    integer :: num_neighs
    integer :: target_sky
    integer :: pass_num
    integer :: target_begin2_sky
    integer :: target_begin1_sky
    integer :: target_end1_sky
    integer :: target_end2_sky
    integer :: neigh_sky
    integer :: neigh_end1_sky
    integer :: neigh_first_sky
    integer :: neigh_last_sky
    integer :: first_step
    integer :: seg1_count
    integer :: seg2_count
    integer :: min_seg_length
    integer :: year
    integer :: month
    integer :: begin1_month
    integer :: begin2_month
    integer :: end1_month
    integer :: end2_month
    integer :: begin1_year
    integer :: begin2_year
    integer :: end1_year
    integer :: end2_year
    integer :: last_changepoint
    integer :: valid_pair_count
    integer :: change_count
    integer :: num_pairs_tested
    integer :: min_compare
    integer :: pair_begin2_sky
    integer adj_win_months
    integer :: diffs_count
    integer :: diff_zero_count
    integer :: i ! reusable loop index
    real :: outlier_qscale
    real :: quartile_25
    real :: quartile_50
    real :: quartile_75
    real :: middle_50
    real :: schwarz_min
    real :: range_low
    real :: range_high
    real :: signif_range_low
    real :: signif_range_high
    real :: signif_quartile_25
    real :: signif_quartile_50
    real :: signif_quartile_75
    real :: adj_signif
    real :: adj_estimate
    real :: sse_tested
    real :: signif_qscale
    real :: corr_threshold
    real :: crit_thresh
    real :: test_stat
    logical :: adj_remove_outliers
    logical :: remove_it
    logical :: is_removed
    logical :: insig_remove
    character(len=1) :: use_code(size(neighbors,2))
    ! Adjustment estimate method, read in from properties
    character(len=3) :: adj_est_method
    ! Adjustment estimate filter method, read in from properties
    character(len=4) :: adj_filt_method
    ! Log message variables
    character(len=6) :: process_str
    character(len=79) :: changepoint_str
    character(len=13) :: step_str
    character(len=256) :: log_message

    ! for monthly anomaly calc
    real, dimension(:,:) :: monthly_average(13, size(neighbors,1))
    ! for the final stat test (including BIC) routine
    real, dimension(:) :: diffs(por_months)
    real, dimension(:) :: x_data(por_months)
    real, dimension(:) :: y_data(por_months)
    real, dimension(:) :: targ_first_diffs(por_months)
    real, dimension(:) :: neigh_first_diffs(por_months)
    integer, dimension(:) :: models_tested(size(neighbors,2))
    integer, dimension(:) :: values_tested(2)
    real, dimension(:) :: y_intercepts_tested(2)
    real, dimension(:) :: slopes_tested(2)
    real, dimension(:,:) :: adjustments(size(neighbors,2), MAX_CHANGEPOINTS)
    real, dimension(:,:) :: correlations(size(neighbors,2), MAX_CHANGEPOINTS)
    integer, dimension(:,:) :: meta_removal(size(neighbors,1),MAX_CHANGEPOINTS)
    ! trimmed adj arrays for model >= 3
    real, dimension(:) :: adj_used(size(neighbors,2))
    real, dimension(:) :: corr_used(size(neighbors,2))
    integer, dimension(:) :: neigh_index_used(size(neighbors,2))
    integer, dimension(:) :: num_neighs_used(size(neighbors,2))
    ! for inner-quartile range
    real, dimension(:) :: adj_tested(size(neighbors,2))
    real, dimension(:) :: corr_tested(size(neighbors,2))
    real, dimension(:) :: target_residuals(por_months)
    real, dimension(:) :: neigh_residuals(por_months)
    ! beg/end period of record
    integer, dimension(:) :: first_data_sky(size(neighbors,1))
    integer, dimension(:) :: last_data_sky(size(neighbors,1))
    ! changepoints - skyline month index and amplitude of the changepoints for the entire network
    ! num_changes - number of changepoints in each target station
    ! PRECEEDING ARRAYS DO NOT CHANGE IN ADJUSTMENT LOOPS
    ! change_status - status of each changepoint per pass/step
    !   0 - to be evaluated
    !   1 or 2 - pass_num number
    ! num - number of targ-neigh pairs used to form adjustment estimate
    integer, dimension(:,:) :: change_status(size(neighbors,1), MAX_CHANGEPOINTS)

    ! Minimum number of station pairs to estimate adjustment, read in from properties
    integer :: adj_min_stations
    ! max_passes includes the "Not sig" step
    integer, parameter :: max_passes = 2
    ! Inner-quartile scale filter
    real, parameter :: qscale = 1.46

    ! The major steps in determining the best adjustment value
    ! for each station/changepoint. Entire network undergoes each of
    ! the following processes. In order:
    !     1) Remove unusable data. Align moves with respect to non-missing
    !        data and compress out changes that are too close AND the data
    !        between them.
    !     2) step_index=1 processing begins the adjustment process by removing
    !         the Non-significant changepoints to lengthen segments.
    !     3) max_passes (:= step_index=2) finishes the adjustment process by testing
    !         for the minimum number of months in a segment and number
    !         of neighbors with which the difference series can be examined.
    !     4) Final adjusted output is written

    ! All of the merged_hits changepoint position alignment and
    ! data adjustment for too short segments has been done.

    call log_info('Inner-quartile filter scale: '//trim(log_string(qscale)))

    adj_est_method = get_property_chars(PROP_ADJUST_ESTIMATE_METHOD)
    call log_info('Method to estimate chgpt adjustment: '//adj_est_method)

    adj_filt_method = get_property_chars(PROP_ADJUST_FILTER_METHOD)
    call log_info('Method for estimate filter: '//adj_filt_method)

    ! Toggle to remove non-significant(NS) breakpoints then merge segments
    ! for longer segments to test
    insig_remove = get_property_logical(PROP_REMOVE_INSIGNIFICANT)
    call log_info('Toggle to remove Non-sig segments: '//trim(log_string(insig_remove)))

    adj_min_stations = get_property_int(PROP_ADJUST_MIN_NEIGHS)
    call log_info('Minimum station pairs to est adjustment: '//trim(log_string(adj_min_stations)))

    ! Toggle to remove outliers or not in Adjustment estimate
    adj_remove_outliers = get_property_logical(PROP_ADJUST_REMOVE_OUTLIERS)
    call log_info('Toggle to keep/remove chgpt outliers: '//trim(log_string(adj_remove_outliers)))

    ! Number of months to average for the adjusment estimate (0 = no limit)
    adj_win_months = get_property_int(PROP_ADJUST_WINDOW)
    call log_info('Months to avg for adj est(0=no limit): '//trim(log_string(adj_win_months)))

    targets_size = size(neighbors,1)
    neighbors_size = size(neighbors,2)

    allocate(num_changes(targets_size))
    allocate(unstable_dates(targets_size, MAX_CHANGEPOINTS))
    allocate(changepoints(targets_size, MAX_CHANGEPOINTS))

    min_seg_length = get_property_int(PROP_ADJUST_MIN_LENGTH)

    ! initialize metadata removal array
    meta_removal = 0

    call log_info("Alignmoves & Ndelete filter")

    ! generate the series monthly averages for all of the stations
    call create_series_monthly_averages(neighbors, element_data_process, first_data_sky, last_data_sky, monthly_average)

    ! remove all changepoints with correlations lower than corr_threshold
    corr_threshold = 0.0

    ! initialize work arrays
    num_changes(:) = 0
    adj_tested(:) = 0.0
    changepoints(:,:) = null_Changepoint()
    ! Set all unstable_dates to zero except for the first for each station = por_months
    unstable_dates(:,:) = 0

    call initialize_changpoint_arrays(neighbors, merged_hits, first_data_sky, last_data_sky, changepoints, num_changes)

    ! Structural Uncertainty Option - NS_LOOP
    ! if NS_loop toggle = 1 then go through loop to remove non-sig segments
    ! else skip loop
    if(insig_remove) then
      first_step = 1
    else
      first_step = 2
    endif

    ! The subnetwork processing became a multi-step process plus a
    !      "post-process pass" to manage:
    !       1) problems with the documented changepoints with NO undocumented
    !             support.
    !       2) restrict influence of large slope difference series
    !       3) determine the best amplitude estimation for each confirmed
    !             changepoint
    !       4) minimize the impact of local slopes

    !      ------------- Sig / Adj test ------------
    do step_index = first_step, 2

      ! used temporarily in the "steps" but kept in the "passes"
      change_status = 0

      ! ------------ pass_num loopback for step_index == 1 -------------
      if(step_index .eq. 1) then
        call log_info('-------------- NOT SIG REMOVAL ---------')
        step_str = 'Not sig: '
        pass_num = 1
        ! min_compare = adj_min_stations
        min_compare = 3
      else
        call log_info('-------------- ADJUST DISCONTINUITY STEP ---------')
        step_str = 'Dstep Dtrend: '
        pass_num = 2
        min_compare = adj_min_stations
        call log_info('Adjpass, min_seg_length, min_compare: '//log_string(pass_num)// &
                      log_string(min_seg_length)//" "//log_string(min_compare))
        call log_info('-------------------- MAX PASSES -----------------')

      endif

      ! process each subnetwork (candidate/target) one at a time
      do station_index = 1, targets_size
        ! target_index is the index of the sub-network (candidate)
        target_index = neighbors(station_index,1)%target_index
        target_id = neighbors(station_index,1)%target_id
        change_count = num_changes(target_index)

        ! initialize station chgpt estamt work arrays
        adjustments(:,:) = MISSING_REAL
        correlations(:,:) = MISSING_REAL
        unstable_count = 0

        if(neighbors(station_index,adj_min_stations+1)%neighbor_index .eq. 0) then
          call log_info('Neighbors < adj_min_stations: '//target_id)
          cycle
        endif

        ! List the confirmfilt output changepoints
        do change_num = 1, change_count
          sky_index = changepoints(target_index, change_num)%sky_month
          if(sky_index .le. 0) then
            call log_info('changepoints error: '//log_string(target_index)//' '// &
                   target_id//' '//log_string(change_num)// &
                   ' '//log_string(changepoints(target_index, change_num)%sky_month)//' No data segments')
            change_count = 1
            exit
          else
            call get_year_month_from_sky(year,month,sky_index,target_index)
            write(log_message, '(i3,i5,1x,a," Estamt chgin: ",2i5,i2.2,i10,2i6,f7.2)') &
                pass_num, target_index, target_id, change_num, &
                year, month, sky_index, merged_hits(sky_index), change_status(target_index,change_num), &
                changepoints(target_index,change_num)%amplitude
            call log_info(log_message)
          endif
        enddo

        ! ---------------- station chgpt loop --------------------
        ! find all of the "not done" changepoints (change_num) up to this point
        ! and the "not done" changepoints (change1 & change2) that bracket it
        last_changepoint = change_count
        change_num = change_count - 1
        segment_loop: do while (change_num .ge. 2)

          ! reset current chgpt by going backward from the current change_num
          ! to the first chgpt not done
          do change_index = last_changepoint-1, 2, -1
            if(change_status(target_index, change_index) .eq. 0) exit
          enddo
          change_num = change_index
          if(change_num .eq. 1) exit

          ! go forward to the latter chgpt or first chgpt not done (==0)
          do change_index = change_num+1, last_changepoint
            if(change_status(target_index, change_index) .eq. 0) exit
          enddo
          if(change_index .gt. last_changepoint) change_index = last_changepoint
          change2 = change_index

          ! finally, keep going back to the next earlier chgpt not done (==0)
          do change_index = change_num-1, 2, -1
            if(change_status(target_index, change_index) .eq. 0) exit
          enddo
          change1 = change_index
          ! set last changepoint to current changepoint for next iteration
          last_changepoint = change_num

          ! expanded IF to fix beg series UNSTBL problem
          if(change1 .eq. 1) then
            begin1_sky = changepoints(target_index, 1)%sky_month
          else
            begin1_sky = changepoints(target_index, change1)%sky_month + 1
          endif
          end1_sky = changepoints(target_index, change_num)%sky_month
          begin2_sky = end1_sky + 1
          end2_sky = changepoints(target_index, change2)%sky_month
          call log_info('Oriented: '//trim(log_string(change1))//' '//trim(log_string(change_num))// &
                 ' '//trim(log_string(change2))//' '//trim(log_string(begin1_sky))//' '// &
                 trim(log_string(end1_sky))//' '//trim(log_string(begin2_sky))//' '//trim(log_string(end2_sky)))

          ! get all of the yr/mth for print
          call get_year_month_from_sky(begin1_year,begin1_month,begin1_sky,target_index)
          call get_year_month_from_sky(end1_year,end1_month,end1_sky,target_index)
          call get_year_month_from_sky(begin2_year,begin2_month,begin2_sky,target_index)
          call get_year_month_from_sky(end2_year,end2_month,end2_sky,target_index)
          write(changepoint_str,'(" Win1: ", 2(i5, i2.2, i8)," to ", &
             &  "Win2: ", 2(i5, i2.2, i8))') &
                 begin1_year, begin1_month, begin1_sky, end1_year, end1_month, end1_sky, &
                 begin2_year, begin2_month, begin2_sky, end2_year, end2_month, end2_sky

          ! ---------- This section accumulates target-neighbor comparisons -------------

          ! see if there are enough homogenous data in the target
          ! first forward - process the after chgpt (second) window
          target_begin2_sky = begin2_sky
          target_end2_sky = 0
          seg2_count = 0
          do sky_index = begin2_sky, end2_sky
            if(element_data_process(sky_index)%value .gt. MISSING_REAL+1.) then
              ! set last used
              target_end2_sky = sky_index
              seg2_count = seg2_count + 1
            endif
          enddo

          ! if the segment length of the target station is too short for this
          ! adj pass, skip this chgpt - for now
          ! however, if at max_passes then remove segment and attempt to span
          if(seg2_count .lt. min_seg_length) then
            if(pass_num .lt. max_passes) then
              write(log_message,'("Adjpass seg2 short ",2i4,a," ",a,i5, &
                  &  " -skip")') pass_num,target_index,target_id,changepoint_str,seg2_count
              call log_info(log_message)
            else
              write(log_message,'("Adjpass seg2 short ",2i4,a," ",a,i5, &
                  &  " -remove")') pass_num,target_index,target_id,changepoint_str,seg2_count
              call log_info(log_message)

              ! Remove SECOND segment in this one case (begin2_sky/end2_sky)
              element_data_process(begin2_sky:end2_sky)%value = MISSING_REAL

              call update_removed_segment_indexes(adjustments, changepoints(target_index,:), change_status(target_index,:), &
                       merged_hits(end1_sky), num_changes(target_index), change_num, change_count)
            endif
            change_num = change_num - 1
            cycle segment_loop
          endif

          ! now go back in time - process the before chgpt (first window)
          target_begin1_sky = 0
          target_end1_sky = end1_sky
          seg1_count = 0
          do sky_index = end1_sky, begin1_sky, -1
            if(element_data_process(sky_index)%value .gt. MISSING_REAL+1.) then
              ! set first used
              target_begin1_sky = sky_index
              seg1_count = seg1_count + 1
            endif
          enddo

          ! if the segment length of the targ station is too short for this
          ! adj pass, skip this chgpt - for now
          ! however, if at max_passes then remove segment and attempt to span
          if(seg1_count .lt. min_seg_length) then
            if(pass_num .lt. max_passes) then
              write(log_message,'("Adjpass seg1 short ",2i4,a," ",a,i5, &
                  &  " -skip")') pass_num,target_index,target_id,changepoint_str,seg2_count
              call log_info(log_message)
            else
              write(log_message,'("Adjpass seg1 short ",2i4,a," ",a,i5, &
                  &  " -remove")') pass_num,target_index,target_id,changepoint_str,seg1_count
              call log_info(log_message)

              element_data_process(begin1_sky:end1_sky)%value = MISSING_REAL
              call update_removed_segment_indexes(adjustments, changepoints(target_index,:), change_status(target_index,:),  &
                       merged_hits(end1_sky), num_changes(target_index), change_num, change_count)
            endif
            change_num = change_num - 1
            cycle segment_loop
          endif

          ! number of neighbor pairs tested with adjustments
          num_pairs_tested = 0

          ! number of times adj_filt_method option test failed (if given)
          num_adj_filter = 0

          num_compare = 0

          ! go through all of the neighbors to make the before/after
          !  windows around the chgpt location
          do targ2neigh_index = 2, neighbors_size
            ! neighbor_index - the index of the "targ2neigh_index" station in the "target_index" subnet
            neighbor_index = neighbors(target_index, targ2neigh_index)%neighbor_index
            ! if there are no more neighbors, skip out
            if(neighbor_index .eq. 0) cycle

            ! find the neighbor skyindex of 2nd segment beginning
            pair_begin2_sky = get_sky_from_year_month(neighbor_index, begin2_year, begin2_month)
            if(pair_begin2_sky .lt. 0) then
              call log_debug('Seg2 begin '//log_string(begin2_year)//' '// &
                     log_string(begin2_month)//' Outside POR of Neighbor: '// &
                     target_id//' '//log_string(pair_begin2_sky))
              cycle
            endif

            ! determine the POR of the neighbor
            neigh_first_sky = get_first_sky(neighbor_index)
            neigh_last_sky = get_last_sky(neighbor_index)

            ! initialize paired diffs work arrays
            diffs = MISSING_REAL
            target_residuals = MISSING_REAL
            neigh_residuals = MISSING_REAL
            x_data = MISSING_REAL
            y_data = MISSING_REAL
            targ_first_diffs = MISSING_REAL
            neigh_first_diffs = MISSING_REAL

            ! first forward - process the after chgpt (second) window
            seg2_months = 0
            end2_month_index = 0
            ! accumulate paired data for the most recent window(2)
            ! ------------- SINE TEST Placeholder ------------
            call log_debug(target_id//'-'//neighbors(neighbor_index,1)%target_id// &
                ' begin2_sky, end2_sky: '//log_string(begin2_sky)//' '//log_string(end2_sky))

            ! start at the first month after the changepoint and go forward
            do target_sky = target_begin2_sky, target_end2_sky
              neigh_sky = pair_begin2_sky + target_sky - target_begin2_sky
              if(neigh_sky .gt. neigh_last_sky) exit
              call get_times_from_sky(target_index, month_index, year, month, target_sky)
              ! sum and count the differences
              !   the target-pair completeness is defined for every non-missing
              !   target month there is a pair month
              if(element_data_process(target_sky)%value .gt. MISSING_REAL+1. .and. &
                element_data_process(neigh_sky)%value .gt. MISSING_REAL+1. .and. &
                .not. delete_flags(targ2neigh_index,target_sky)) then
                ! increment number in segment - to test with MINLEN
                seg2_months = seg2_months + 1

                ! Structural Uncertainty Option - ADJ_WINDOW
                !   Reinstitute the time limit on the segments
                !   The Adjustment window may be smaller than the entire segment
                !   overlap length when ADJ_WINDOW is less than MINLEN
                !   so that not all the segment info is used for adjustment
                if(adj_win_months .eq. 0 .or. seg2_months .le. adj_win_months) then
                  ! set last month to use
                  end2_month_index = month_index
                  ! gen & use monthly anomalies for paired adjustment est
                  target_residuals(month_index) = element_data_process(target_sky)%value - monthly_average(month,target_index)
                  neigh_residuals(month_index) = element_data_process(neigh_sky)%value - monthly_average(month,neighbor_index)
                  diffs(month_index) = target_residuals(month_index) - neigh_residuals(month_index)
                  end2_sky = target_sky
                endif
              endif

              ! loop out if neighbor hits another chgpt
              ! this test is done after this month is used because when
              ! going forward the chgpt location is the last in this segment!
              if(merged_hits(neigh_sky).gt.0) exit

            enddo

            if(seg2_months .lt. min_seg_length) cycle

            ! find the neighbor skyindex of 2nd segment beginning
            neigh_end1_sky = get_sky_from_year_month(neighbor_index, end1_year, end1_month)
            if(neigh_end1_sky .lt. 0) then
              call log_debug('Seg1 end '//log_string(end1_year)//' '//log_string(end1_month)// &
                     ' Outside POR of Neighbor: '//neighbors(neighbor_index,1)%target_id//' '//log_string(neigh_end1_sky))
              cycle
            endif

            ! ------------- SINE TEST Placeholder ------------

            ! now go back in time - process the before chgpt (first window)
            seg1_months = 0
            end1_month_index = 0

            do target_sky = target_end1_sky, target_begin1_sky, -1
              neigh_sky = neigh_end1_sky + target_sky - end1_sky
              if(neigh_sky .lt. neigh_first_sky) exit

              ! loop out if either of the stations hit another chgpt
              if(target_sky.ne.target_end1_sky .and. merged_hits(target_sky).gt.0) exit

              ! Or neighbor has changepoint in target segment
              if(merged_hits(neigh_sky).gt.0) exit

              call get_times_from_sky(target_index, month_index, year, month, target_sky)
              ! sum and count the differences
              if(element_data_process(target_sky)%value .gt. MISSING_REAL+1. .and. &
                element_data_process(neigh_sky)%value .gt. MISSING_REAL+1. .and. &
                .not. delete_flags(targ2neigh_index,target_sky)) then
                ! set the first month used
                if(end1_month_index .eq. 0) end1_month_index = month_index
                ! increment number in segment - to test with MINLEN
                seg1_months = seg1_months + 1

                ! Structural Uncertainty Option - Reinstitute the time limit on the segments
                if(adj_win_months .eq. 0 .or. seg1_months .le. adj_win_months) then
                  ! gen & use monthly anomalies for paired adjustment est
                  target_residuals(month_index) = element_data_process(target_sky)%value - monthly_average(month,target_index)
                  neigh_residuals(month_index) = element_data_process(neigh_sky)%value - monthly_average(month,neighbor_index)
                  diffs(month_index) = target_residuals(month_index) - neigh_residuals(month_index)
                  begin1_sky = target_sky
                endif
              endif
            enddo

            if(seg1_months .lt. min_seg_length) cycle

            ! ------------- SINE TEST Placeholder ------------

            num_pairs_tested = num_pairs_tested + 1

            ! scan the difference array, pass segment into minbic (include missing data)
            data_index = 0
            diffs_count = 0
            diff_zero_count = 0

            do month_index = 1, por_months
              if(diffs(month_index) .ne. MISSING_REAL) then
                if(data_index .eq. 0) begin_month_index = month_index
                data_index = month_index - begin_month_index + 1

                ! count the number of zeros
                diffs_count = diffs_count + 1
                if(diffs(month_index) == 0.0) diff_zero_count = diff_zero_count + 1

                y_data(data_index) = diffs(month_index)
                x_data(data_index) = month_index
                if(month_index .le. end2_month_index) end2_data_index = data_index
                if(month_index .le. end1_month_index) end1_data_index = data_index
              endif
            enddo

            ! All differences between segments equal - must be duplicate
            if(diffs_count == diff_zero_count) then
              call log_info("Paired segments equal: "//target_id//"-"//neighbors(neighbor_index,1)%target_id)
              cycle
            end if

            call minbic(3, x_data, y_data, end1_data_index, end2_data_index, crit_thresh, test_stat, &
              schwarz_min, adj_tested(num_pairs_tested), y_intercepts_tested, slopes_tested, sse_tested, &
              models_tested(num_pairs_tested), values_tested)

            ! determine first difference correlations for pairs
            call calculate_paired_first_diffs(por_months, target_residuals, neigh_residuals, data_index,  &
                     targ_first_diffs, neigh_first_diffs)
            corr_tested(num_pairs_tested) = calculate_correlation(data_index, targ_first_diffs, neigh_first_diffs)

            write(log_message,'(2a, "-", 2a, 4f7.2, 2f7.3, 4i5)') &
                trim(step_str), target_id, neighbors(neighbor_index,1)%target_id, changepoint_str, crit_thresh, &
                test_stat, adj_tested(num_pairs_tested), corr_tested(num_pairs_tested), slopes_tested(1), &
                slopes_tested(2), models_tested(num_pairs_tested), values_tested, num_pairs_tested
            call log_info(log_message)

            ! Structural Uncertainty Option - ADJ_FILTER == bicf or both
            if(adj_filt_method .eq. OPT_FILTER_METHOD_BIC .or.  &
               adj_filt_method .eq. OPT_FILTER_METHOD_BOTH) then
              if(test_stat .lt. crit_thresh) then
                call log_info('ADJFILT: Bic sig test failed - skip pair')
                num_adj_filter = num_adj_filter + 1
                cycle
              endif
            endif

            ! number of comparisons
            num_compare = num_compare + 1

            ! keep adjustment for each neighbor/segment
            !   the first segment is the before (change_num) segment
            !   the second segment is the after (change_num+1) segment
            !   examining previous debug output indicates that since:
            !   the difference series is same, the output is same
            adjustments(targ2neigh_index,change_num) = adj_tested(num_pairs_tested)
            correlations(targ2neigh_index,change_num) = corr_tested(num_pairs_tested)

          enddo ! End of the neighbors loop

          ! Structural Uncertainty Option - ADJ_FILTER
          if(adj_filt_method .eq. OPT_FILTER_METHOD_BIC .or.  &
             adj_filt_method .eq. OPT_FILTER_METHOD_BOTH) then
            ! if the number of valid pairs is less than the minimum required
            ! due to the ADJ FILTER then assume chgpt is "Not Significant"
            if(num_compare .lt. min_compare .and. &
              num_compare+num_adj_filter .ge. min_compare) then
              ! remove changepoint BUT NOT DATA
              ! reduce total number of chgpts for current station
              call log_info("Remove Changepoint - not sig per ADJ_FILTER: "//trim(log_string(target_index))  &
                     //trim(log_string(change_num))//trim(log_string(num_compare))  &
                     //trim(log_string(num_adj_filter))//trim(log_string(min_compare)))
              meta_removal(target_index,change_num) = 1
              call log_info(target_id//" "//changepoint_str//" Set Remove chgpt "  &
                     //trim(log_string(changepoints(target_index,change_num)%sky_month)))
              change_num = change_num - 1
              cycle segment_loop
            endif
          endif

          ! ------------------- This section determines adjustment -----------------------

          num_neighs = 0

          ! go through all of the neighbors
          do targ2neigh_index = 2, neighbors_size
            ! neighbor_index - the index of the "targ2neigh_index" station in the "target_index" subnet
            neighbor_index = neighbors(target_index, targ2neigh_index)%neighbor_index
            ! if there are no more neighbors, skip out
            if(neighbor_index .eq. 0) exit

            ! save non-missing data for median calculations
            if(adjustments(targ2neigh_index,change_num) .eq. MISSING_REAL) then
              use_code(targ2neigh_index) = 'M'
            else
              num_neighs = num_neighs + 1
              adj_used(num_neighs) = adjustments(targ2neigh_index,change_num)
              corr_used(num_neighs) = correlations(targ2neigh_index,change_num)
              neigh_index_used(num_neighs) = targ2neigh_index
              num_neighs_used(num_neighs) = num_neighs
              use_code(targ2neigh_index) = 'U'
            endif
          enddo

          ! in step == 2, iadjlist(pass_num) determines the number of
          ! "Good Neigh" estimates needed for this pass to process the
          ! amplitude estimation for this chgpt.
          if(num_neighs .lt. min_compare) then
            if(pass_num .lt. max_passes) then
              write(log_message,'("Adjpass num_pairs_tested low ",2i6," ",a," ",4i8)') &
                pass_num, target_index, target_id, changepoints(target_index,change_num-1)%sky_month, &
                changepoints(target_index,change_num)%sky_month, changepoints(target_index,change2)%sky_month, num_neighs
              call log_info(log_message)
            else
              ! else there were not enough neighbors to make an evaluation
              ! THEREFORE THE TARGET HAS AN UNSTABLE NETWORK AND CANNOT BE COMPLETED
              write(log_message,'(i5, 1x, "-UNSTBL ", a, i2.2, a, " Neigh: ", i2, " Only est: ", f7.2)') &
                 target_index, target_id, 1, changepoint_str, num_pairs_tested, adj_tested(1)
              call log_info(log_message)

              ! keep track of unstable dates
              unstable_count = unstable_count + 1
              unstable_dates(target_index,unstable_count) = end1_sky
            endif
            change_num = change_num - 1
            cycle segment_loop
          endif

          ! 1st - remove both adjustment and slope outliers
          ! 2nd - calculate median adjustment
          ! filter around the inner-quartile range.....
          ! outlier_qscale is used for trimming outliers
          ! signif_qscale is used for significance testing
          ! for normal output - use (99%) clipping for outliers
          ! v20d - qscale from command line restored
          ! testing indicates that more outliers should be removed to tighten
          !   estimate distribution - use HOFN (95%) clipping
          ! outlier_qscale = 1.5 * qscale
          outlier_qscale = 1.0 * qscale
          signif_qscale = 1.0 * qscale

          ! sort the changepoint adjustments first
          call arrays_sort(num_neighs, adj_used, num_neighs_used)
          call tukey_median(num_neighs, adj_used, quartile_25, quartile_50, quartile_75)
          middle_50 = quartile_75 - quartile_25
          range_low = quartile_25 - (quartile_50 - quartile_25) * outlier_qscale
          range_high = quartile_75 + (quartile_75 - quartile_50) * outlier_qscale
          write(log_message,'(" TRIM p25, p75, quartile_50, middle_50, lo, hi:", &
              &  6f7.2)') quartile_25,quartile_75,quartile_50,middle_50,range_low,range_high
          call log_info(log_message)

          ! Count remaining stations
          stations_passed = 0
          do i = 1, num_neighs
            if(adj_used(i) .ge. range_low .and. adj_used(i) .le. range_high) then
              stations_passed = stations_passed + 1
            endif
          enddo

          ! Trim outliers from adjustments if enough stations
          ! Structural Uncertainty Option - Outlier removal
          if(adj_remove_outliers .and. stations_passed .ge. min_compare) then
            do i = 1, num_neighs
              if(adj_used(i) .lt. range_low .or. adj_used(i) .gt. range_high) then
                use_code(neigh_index_used(num_neighs_used(i))) = 'X'
              endif
            enddo
          endif

          num_neighs = 0
          do targ2neigh_index = 2, neighbors_size
            ! neighbor_index - the index of the "targ2neigh_index" station in the "target_index" subnet
            neighbor_index = neighbors(target_index, targ2neigh_index)%neighbor_index
            ! if there are no more neighbors, skip out
            if(neighbor_index .eq. 0) exit
            if(use_code(targ2neigh_index) .eq. 'U') then
              num_neighs = num_neighs + 1
              adj_used(num_neighs) = adjustments(targ2neigh_index,change_num)
              corr_used(num_neighs) = correlations(targ2neigh_index,change_num)
              neigh_index_used(num_neighs) = targ2neigh_index
            endif
          enddo

          if(num_neighs .lt. min_compare) then
            if(pass_num .lt. max_passes) then
              write(log_message,'("Insuff trimmed mean ",2i6,a," ",4i8)') &
                pass_num, target_index, target_id, changepoints(target_index,change_num-1)%sky_month, &
                changepoints(target_index,change_num)%sky_month, changepoints(target_index,change2)%sky_month, num_neighs
              call log_info(log_message)
            ! else there were not enough neighbors to make an evaluation
            ! THEREFORE THE TARGET HAS AN UNSTABLE NETWORK AND CANNOT BE COMPLETED
            else
              write(log_message,'(i5, 1x, "-UNSTB2 ", a, i2.2, a, " Neigh: ", i2, " Only est: ", f7.2)') &
                 target_index, target_id, 1, changepoint_str, num_pairs_tested, adj_tested(1)
              call log_info(log_message)

              unstable_count = unstable_count + 1
              unstable_dates(target_index,unstable_count) = end1_sky

              element_data_process(begin1_sky:end1_sky)%value = MISSING_REAL
              call update_removed_segment_indexes(adjustments, changepoints(target_index,:), change_status(target_index,:),  &
                       merged_hits(end1_sky), num_changes(target_index), change_num, change_count)
            endif
            change_num = change_num - 1
            cycle segment_loop
          endif

          valid_pair_count = num_neighs

          ! Count remaining stations
          stations_passed = 0
          corr_threshold = 0.0
          do neighbor_index = 1, valid_pair_count
            if(corr_used(neighbor_index) .gt. corr_threshold) then
              stations_passed = stations_passed + 1
            endif
          enddo

          ! (J4) Perform the correlation test ONLY if the number of
          ! neighbors is greater than the minimum threshold
          if(stations_passed .ge. min_compare) then

            ! sort wrt the correlation
            call arrays_sort(valid_pair_count, corr_used, neigh_index_used, adj_used, 0)

            do neighbor_index = 1, valid_pair_count
              if(corr_used(neighbor_index) .le. corr_threshold) then
                valid_pair_count = neighbor_index - 1
                if(valid_pair_count .ge. min_compare) then
                  exit
                else
                  if(pass_num .lt. max_passes) then
                    write(log_message,'("Insuff Neigh corr ",2i6,a," ",4i8)') &
                      pass_num, target_index, target_id, changepoints(target_index,change_num-1)%sky_month, &
                      changepoints(target_index,change_num)%sky_month, changepoints(target_index,change2)%sky_month,  &
                      valid_pair_count
                    call log_info(log_message)
                  else
                    write(log_message,'(i5, 1x, "-UNSTB3 ", a, i2.2, a, " Neigh: ", i2, " Only est: ", f7.2)') &
                       target_index, target_id, 1, changepoint_str, num_pairs_tested, adj_tested(1)
                    call log_info(log_message)

                    unstable_count = unstable_count + 1
                    unstable_dates(target_index,unstable_count) = end1_sky
                  endif

                  element_data_process(begin1_sky:end1_sky)%value = MISSING_REAL
                  call update_removed_segment_indexes(adjustments, changepoints(target_index,:), change_status(target_index,:),  &
                           merged_hits(end1_sky), num_changes(target_index), change_num, change_count)
                  change_num = change_num - 1
                  cycle segment_loop
                endif
              endif
            enddo
          end if

          ! sort adjustments to get 25-50-75 percentiles to
          ! generate adjustment and 95% conf using trimmed median array
          call arrays_sort(valid_pair_count, adj_used, neigh_index_used, corr_used, 1)
          call tukey_median(valid_pair_count, adj_used, signif_quartile_25, signif_quartile_50, signif_quartile_75)

          ! Structural Uncertainty Option - ADJ_EST == qav
          !   chgpt adjustment = average of 25% & 75% values
          if(adj_est_method .eq. OPT_EST_METHOD_MEDIAN) adj_estimate = signif_quartile_50
          if(adj_est_method .eq. OPT_EST_METHOD_QUART_AVG) adj_estimate = (signif_quartile_25 + signif_quartile_75) / 2.0
          ! Structural Uncertainty Option - ADJ_EST == avg
          if(adj_est_method .eq. OPT_EST_METHOD_AVG) adj_estimate = calculate_average(adj_used, valid_pair_count)

          ! Structural Uncertainty Option - ADJ_FILTER == Bic sig only or None
          !   Bypasses Collective 95% conf or Both filters
          if(adj_filt_method .eq. OPT_FILTER_METHOD_BIC .or.  &
             adj_filt_method .eq. OPT_FILTER_METHOD_NONE) then
            signif_quartile_75 = adj_estimate
            signif_quartile_25 = adj_estimate
          endif

          signif_range_low = signif_quartile_25 - (adj_estimate - signif_quartile_25) * signif_qscale
          signif_range_high = signif_quartile_75 + (signif_quartile_75 - adj_estimate) * signif_qscale

          do i = 1, valid_pair_count
            write(log_message,'("Adj: ", i4, f7.3, i4, f7.2)') &
              i, adj_used(i), neigh_index_used(i), corr_used(i)
            call log_info(log_message)
          enddo
          write(log_message,'("Amp ", i5," p25, p75, quartile_50, middle_50, lo, hi, adj-1st, adj-last:", &
              &  7f7.2)') valid_pair_count, signif_quartile_25, signif_quartile_75, adj_estimate, signif_range_low,  &
                 signif_range_high, adj_used(1), adj_used(valid_pair_count)
          call log_info(log_message)

          ! hold on to the lowest inner quartile range
          quartile_25 = signif_quartile_25
          quartile_75 = signif_quartile_75
          range_low = signif_range_low
          range_high = signif_range_high
          num_neighs = valid_pair_count

          ! ensure offset is significant for chgpt adj
          ! not significant (too small) if 0 is between lo and hi (see M&W 2009, p. 1706)
          ! Additionally use hi/lo offset of actual differences
          if(range_low * range_high .gt. 0.0 .or. adj_used(1) * adj_used(valid_pair_count) .gt. 0.0) then
            process_str = 'CONSHF'
            adj_signif = adj_estimate
          else
            ! too close to zero
            adj_signif = 0.0
            process_str = 'ZERSHF'
          endif
          change_status(target_index, change_num) = pass_num
          changepoints(target_index, change_num)%amplitude = adj_signif
          write(log_message,'(i5, 1x, a, "-", " ",a, i2.2, a, " AVG ADJ: ", f7.2, 3i4)') &
              target_index, target_id, process_str, 1, changepoint_str, adj_signif, num_neighs, &
              change_status(target_index, change_num), merged_hits(changepoints(target_index,change_num)%sky_month)
          call log_info(log_message)

          change_num = change_num - 1
          cycle segment_loop
        enddo segment_loop ! ICHG loop - end of station chgpt list

        ! List the confirmfilt output changepoints
        do change_num = 1, change_count
          sky_index = changepoints(target_index, change_num)%sky_month
          if(sky_index .le. 0) then
            call log_warn("changepoints error: "//trim(log_string(target_index))//target_id  &
              //trim(log_string(change_num))//trim(log_string(changepoints(target_index, change_num)%sky_month))  &
              //" No data segments")
            change_count = 1
            exit
          else
            call get_year_month_from_sky(year, month, sky_index, target_index)
            write(log_message,'(i3,i5,1x,a," Estamt chgout: ",2i5,i2.2,i10, &
                &  2i6,2f7.2,i5,3f7.3)') pass_num, target_index, target_id, &
                   change_num, year, month, sky_index, &
                   merged_hits(sky_index), change_status(target_index,change_num), &
                   changepoints(target_index,change_num)%amplitude
            call log_info(log_message)
          endif
        enddo

      enddo ! end of station loop

      ! Remove accumulated non-sig chgpts & unsupported metadata here
      !   remove changepoint in loop:
      !   1) target segment too short
      !   2) Unstable segment (too few trimmed mean)
      do station_index = 1, targets_size

        target_index = neighbors(station_index,1)%target_index
        target_id = neighbors(station_index,1)%target_id
        change_count = num_changes(target_index)
        is_removed = .false.
        ! Loop through the changepoints that aren't the first and last changepoints
        do change_num = change_count-1,2,-1
          remove_it = .false.
          if(step_index .eq. 1 .and. meta_removal(target_index,change_num) .eq. 1 .and. &
            (adj_filt_method .eq. OPT_FILTER_METHOD_BIC .or.  &
             adj_filt_method .eq. OPT_FILTER_METHOD_BOTH)) then
            ! Structural Uncertainty - remove chgpt in step=2 if required by adj_filt_method
            remove_it = .true.
          else if(changepoints(target_index,change_num)%amplitude .eq. 0.0 .and. &
            change_status(target_index,change_num) .gt. 0) then
            ! for all others, the adj and the seg1 and seg2 slopes must be 0.0
            remove_it = .true.
          endif

          if(is_removed) then
            change_status(target_index,change_num) = 0
            is_removed = .false.
          endif

          if(remove_it) then
            is_removed = .true.
            meta_removal(target_index, change_num) = 0
            end1_sky=changepoints(target_index,change_num)%sky_month
            call get_year_month_from_sky(year, month, end1_sky, target_index)
            call log_info(target_id//" "//trim(log_string(target_index))//" Remove chgpt "  &
                   //trim(log_string(change_num))//" "//trim(log_string(year))//" "//trim(log_string(month)))

            merged_hits(end1_sky) = -1 * merged_hits(end1_sky)
            ! reset the chgpt for recomputation
            change_status(target_index,change_num) = 0
            do i = change_num, change_count - 1
              changepoints(target_index,i) = changepoints(target_index,i+1)
            enddo
            num_changes(target_index) = num_changes(target_index)-1
            change_count = change_count - 1
          endif
        enddo
      enddo

    enddo ! end of step_index loop

  end subroutine estimate_changepoint_size

  !> For segments that are too short or are unstable (too few trimmed mean) and therefore
  !! being removed, update relevant indexes and counters, and move the items in relevant
  !! arrays into updated index locations.
  !!
  !! @param[inout] adjustments The array of adjustments for all pairs and changepoint numbers.
  !! @param[inout] target_changepoints Changepoints for the current target station.
  !! @param[inout] target_statuses An array of status indicators for changepoints for the current
  !!   target station.
  !! @param[inout] current_hit The value of merged_hits at a changepoint.
  !! @param[inout] target_change_count The number of changepoints for the current target station.
  !! @param[in] current_change_num The index of the changepoint currently being processed.
  !! @param[inout] change_count_alt A separate indicator of the number of changepoints for the
  !!   current target station that may be different from target_change_count in specific edge cases.
  subroutine update_removed_segment_indexes(adjustments, target_changepoints, target_statuses, current_hit, target_change_count, &
                                            current_change_num, change_count_alt)

    real, dimension(:,:) :: adjustments
    type(Changepoint), dimension(:) :: target_changepoints
    integer, dimension(:) :: target_statuses
    integer :: current_hit
    integer :: target_change_count
    integer, intent(in) :: current_change_num
    integer :: change_count_alt

    integer :: change_index
    integer :: station_index

    current_hit = -1 * current_hit ! merged_hits array at current skyline end index
    target_change_count = target_change_count-1
    change_count_alt = change_count_alt-1

    do change_index = current_change_num, change_count_alt
      target_statuses(change_index) = target_statuses(change_index+1)
      target_changepoints(change_index) = target_changepoints(change_index+1)
      do station_index = 1, size(adjustments,1)
        adjustments(station_index,change_index) = adjustments(station_index,change_index+1)
      enddo
    enddo

  end subroutine update_removed_segment_indexes

  !> Generates the series monthly averages for all of the stations.
  !!
  !! @param[in] neighbors The array of station neighbors and their IDs.
  !! @param[in] element_data_process An array for all skyline for the processed data values.
  !! @param[out] first_data_sky An array of first skyline with valid data for all stations.
  !! @param[out] last_data_sky An array of last skyline with valid data for all stations.
  !! @param[out] monthly_averages An array of monthly averages for each month and each station.
  subroutine create_series_monthly_averages(neighbors, element_data_process, first_data_sky, last_data_sky, monthly_averages)

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ElementValue), dimension(:), intent(in) :: element_data_process
    integer, dimension(:), intent(out) :: first_data_sky
    integer, dimension(:), intent(out) :: last_data_sky
    real, dimension(:,:), intent(out) :: monthly_averages

    real, dimension(:) :: month_sums(13)
    integer, dimension(:) :: month_counts(13)
    integer :: target_index
    integer :: target_first_sky
    integer :: target_last_sky
    integer :: station_index
    integer :: month
    integer :: sky_index

    do station_index = 1, size(neighbors,1)
      target_index = neighbors(station_index,1)%target_index
      target_first_sky = get_first_sky(target_index)
      target_last_sky = get_last_sky(target_index)
      ! initialize the beg/end period of record
      first_data_sky(target_index) = 0
      last_data_sky(target_index) = 0

      ! initialize sums and nums arrays for monthly averages
      month_sums(:) = 0.0
      month_counts(:) = 0

      ! accumulate sums and nums for monthly averages
      do sky_index = target_first_sky, target_last_sky
        if(element_data_process(sky_index)%value .gt. MISSING_REAL+1.) then
          month = get_month_from_sky(target_index, sky_index)
          if(first_data_sky(target_index) .eq. 0) first_data_sky(target_index) = sky_index
          last_data_sky(target_index) = sky_index
          month_sums(month) = month_sums(month) + element_data_process(sky_index)%value
          month_counts(month) = month_counts(month) + 1
        endif
      enddo

      ! calculate monthly averages
      do month = 1, 12
        monthly_averages(month,target_index) = month_sums(month) / month_counts(month)
      enddo
    enddo

  end subroutine create_series_monthly_averages

  !> Computes the median and upper and lower quartiles of a series of adjustments
  !!   using Tukey's median method.
  !!
  !! @param[in] adj_count The number of adjustments.
  !! @param[in] adj_used The array of adjustments.
  !! @param[out] quartile_25 The lower quartile adjustment value.
  !! @param[out] quartile_50 The median adjustment value.
  !! @param[out] quartile_75 The upper quartile adjustment value.
  subroutine tukey_median(adj_count, adj_used, quartile_25, quartile_50, quartile_75)

    integer :: adj_count
    real, dimension(:) :: adj_used
    real :: quartile_25
    real :: quartile_50
    real :: quartile_75

    real, dimension(:) :: adj_used_copy(size(adj_used))
    integer :: idx
    integer :: index_25
    integer :: index_50
    integer :: index_75
    integer :: num_high
    integer :: adj_count_copy

    do idx = 1, adj_count
      adj_used_copy(idx) = adj_used(idx)
    enddo
    adj_count_copy = adj_count

    ! calculate the median
    if(mod(adj_count_copy,2) .eq. 1) then
      index_50 = adj_count_copy / 2 + 1
      quartile_50 = adj_used_copy(index_50)
    else
      index_50 = adj_count_copy/2
      quartile_50 = (adj_used_copy(index_50) + adj_used_copy(index_50+1)) / 2.

      ! if median is an average, add back into the obs
      do idx = adj_count_copy, index_50+1, -1
        adj_used_copy(idx+1) = adj_used_copy(idx)
      enddo

      adj_used_copy(index_50+1) = quartile_50
      adj_count_copy = adj_count_copy + 1
      index_50 = index_50 + 1
    endif

    ! calculate the lower quartile (include median)
    if(mod(index_50,2) .eq. 1) then
      index_25 = index_50 / 2 + 1
      quartile_25 = adj_used_copy(index_25)
    else
      index_25 = index_50 / 2
      quartile_25 = (adj_used_copy(index_25) + adj_used_copy(index_25+1)) / 2.
    endif

    ! calculate the upper quartile (include median)
    num_high = adj_count_copy - index_50 + 1
    if(mod(num_high,2) .eq. 1) then
      index_75 = num_high/2 + index_50
      quartile_75 = adj_used_copy(index_75)
    else
      index_75 = num_high/2 + index_50 - 1
      quartile_75 = (adj_used_copy(index_75) + adj_used_copy(index_75+1)) / 2.
    endif

  end subroutine tukey_median

  !> Calculates a first difference time series of monthly mean temperatures
  !!   taking into account missing monthly values.
  !!
  !! @param values_count the number of values.
  !! @param values1 The monthly mean temperatures.
  !! @param values2 The monthly mean temperatures.
  !! @param good_count The number of shared non-missing values between values1 and values2.
  !! @param diffs1 The first differences for values1.
  !! @param diffs2 The first differences for values2.
  subroutine calculate_paired_first_diffs(values_count, values1, values2, good_count, diffs1, diffs2)

    integer :: values_count
    real, dimension(:) :: values1(values_count)
    real, dimension(:) :: values2(values_count)
    integer :: good_count
    real, dimension(:) :: diffs1(values_count)
    real, dimension(:) :: diffs2(values_count)

    integer :: counter
    integer :: idx
    integer :: prev_idx

    counter = 0
    good_count = 0

    diffs1(:) = MISSING_REAL
    diffs2(:) = MISSING_REAL

    ! do first difference filter accounting for missing data
    do idx = 1, values_count
      if (values1(idx) .gt. MISSING_REAL+1 .and. values2(idx) .gt. MISSING_REAL) then
        counter = counter + 1

        if (counter .gt. 1) then
          good_count = good_count + 1
          diffs1(good_count) = (values1(idx) - values1(prev_idx))/2
          diffs2(good_count) = (values2(idx) - values2(prev_idx))/2
        endif
        prev_idx = idx
      endif
    enddo

  end subroutine calculate_paired_first_diffs

  !> Initializes the changepoint arrays with the hits from the merged_hits array.
  !!
  !! @param[in] neighbors The array of station neighbors and their IDs.
  !! @param[in] merged_hits Merged UCP hits & SHF records; accumulated chgpt hits from Split/Merge algorithm.
  !! @param[in] first_data_sky An array of first skyline with valid data for all stations.
  !! @param[in] last_data_sky An array of last skyline with valid data for all stations.
  !! @param[out] changepoints The final changepoints, sky_month and amplitude, for the entire network.
  !! @param[out] change_counts The number of changepoints in each target station.
  subroutine initialize_changpoint_arrays(neighbors, merged_hits, first_data_sky, last_data_sky, changepoints, change_counts)

    type(StationNeighborPair), dimension(:,:) :: neighbors
    integer, dimension(:) :: merged_hits
    integer, dimension(:) :: first_data_sky
    integer, dimension(:) :: last_data_sky
    type(Changepoint), dimension(:,:) :: changepoints
    integer, dimension(:) :: change_counts

    integer :: station_index
    integer :: target_index
    integer :: target_first_sky
    integer :: target_last_sky
    integer :: sky_index
    integer :: change_index

    ! go through all of input chgpts (first/last are beg/end por)
    !   accumulating the output chgpts & adjustments
    do station_index = 1, size(neighbors,1)
      target_index = neighbors(station_index,1)%target_index
      target_first_sky = get_first_sky(target_index)
      target_last_sky = get_last_sky(target_index)

      ! First changepoints value
      change_index = 1
      changepoints(target_index, change_index) = new_Changepoint(first_data_sky(target_index), 0, 0.0)

      ! Convert merged_hits array to changepoints array
      do sky_index = target_first_sky, target_last_sky
        if(merged_hits(sky_index) .gt. 0) then
          change_index = change_index + 1
          changepoints(target_index, change_index) = new_Changepoint(sky_index, 0, 0.0)
        endif
      enddo

      ! Last changepoints value
      !   inserted if around following lines to fix end series UNSTBL problem
      if(changepoints(target_index, change_index)%sky_month .ne. last_data_sky(target_index)) then
        change_index = change_index + 1
        changepoints(target_index, change_index)%sky_month = last_data_sky(target_index)
      endif

      change_counts(target_index) = change_index
    enddo  ! end station_index loop

  end subroutine initialize_changpoint_arrays

  !> Sorts three arrays (two reals and one integer) together based on the values
  !!   of the first array using Shell's method.
  !!
  !! @param num_values The number of elements to sort in the arrays.
  !! @param sort_array The first array to be sorted.
  !! @param assoc_array1 The second (integer) array that is sorted according to the first array.
  !! @param assoc_array2 OPTIONAL, The third (real) array to be sorted according to the first array.
  !! @param scend OPTIONAL, 0 for descending, 1 for ascending.
  subroutine arrays_sort(num_values, sort_array, assoc_array1, assoc_array2, scend)

    integer :: num_values
    real, dimension(:) :: sort_array(num_values)
    integer, dimension(:) :: assoc_array1(num_values)
    real, dimension(:), optional :: assoc_array2(num_values)
    integer, optional :: scend

    integer :: i, j, k, l, m, n ! Indexes
    integer :: scend_opt
    integer :: lognb2
    real :: hold_val
    integer :: hold_assoc1
    real :: hold_assoc2
    real :: inv_ln2 ! Inverse of log2
    real :: eps

    ! If not specified, default is ascending
    if(.not. present(scend)) then
      scend_opt = 1
    else
      scend_opt = scend
    end if

    inv_ln2 = 1. / 0.69314718
    eps=1.E-5
    lognb2 = int(alog(float(num_values)) * inv_ln2+eps)
    m=num_values
    do n=1,lognb2
      m=m/2
      k=num_values-m
      do j=1,k
        i=j
        do
          l=i+m

          if((scend_opt .eq. 0 .and. sort_array(l) .gt. sort_array(i)) .or.  &
             (scend_opt .eq. 1 .and. sort_array(l) .lt. sort_array(i))) then
            ! Swap values for primary sort array
            hold_val=sort_array(i)
            sort_array(i)=sort_array(l)
            sort_array(l)=hold_val

            ! Swap values for first associated array
            hold_assoc1=assoc_array1(i)
            assoc_array1(i)=assoc_array1(l)
            assoc_array1(l)=hold_assoc1

            ! Swap values for second associated array, only if present
            if(present(assoc_array2)) then
              hold_assoc2=assoc_array2(i)
              assoc_array2(i)=assoc_array2(l)
              assoc_array2(l)=hold_assoc2
            end if

            i=i-m
            if(i >= 1) cycle
          endif
          exit ! Exit loop when i<1 or items(l)<items(i)
        enddo
      enddo
    enddo

  end subroutine arrays_sort

end module ChangepointSize

!> @file
!! Contains functionality for calculating the size of each change point.
