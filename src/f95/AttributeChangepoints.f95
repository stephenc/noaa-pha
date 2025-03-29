!> @brief
!! Attributes each potential change point to the station most likely
!! responsible using an accumulated “hit” count.
!! See Req. 1.5 for more information.
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
module AttributeChangepoints

  use Logger
  use PropertyReader
  use SkylineUtils
  use PropertyParameters
  use StationNeighborPairType
  use ChangepointSumsType
  use ChangepointSpanType
  use ChangepointHitType

  implicit none

contains

  !> Determines which station is responsible for each change point using the accumulated “hit” count.
  !!
  !! @param[in] neighbors A collection of pairs for each station with their ids.
  !! @param[inout] changepoint_spans For all skyline segments and for all neighbors of each target
  !!   shows the influence of neighbors on their targets (this is a large array in terms of memory).
  !! @param[inout] changepoint_sums For all skyline the cumulative adjustments due to changepoints
  !!   from station pairs.
  !! @param[out] changepoint_hits Attributed undocumented changepoints.
  subroutine attribute_changepoints(neighbors, changepoint_spans, changepoint_sums, changepoint_hits)
    use CommonVariables, only: skyline_months

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ChangepointSums), dimension(:) :: changepoint_sums
    type(ChangepointSpan), dimension(:,:) :: changepoint_spans
    type(ChangepointHit), dimension(:), intent(out), allocatable :: changepoint_hits

    call identify_culprits(neighbors, changepoint_spans)

    allocate(changepoint_hits(skyline_months))
    changepoint_hits(:) = null_ChangepointHit()

    call assign_changepoints(neighbors, changepoint_spans, changepoint_sums, changepoint_hits)

  end subroutine attribute_changepoints


  !> Identifies the station/times responsible for changepoints. The station/times
  !!   with the most hits retain those hits while they are eliminated from the
  !!   neighbors. During this process surrounding hits (in time) are cleared out of the span.
  !!
  !! @param[in] neighbors A collection of pairs for each station with their ids.
  !! @param[inout] changepoint_spans For all skyline segments and for all neighbors of each target
  !!   shows the influence of neighbors on their targets (this is a large array in terms of memory).
  subroutine identify_culprits(neighbors, changepoint_spans)
    use CommonVariables, only: skyline_months

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ChangepointSpan), dimension(:,:) :: changepoint_spans

    integer, dimension(:), allocatable :: network_hit_counts
    integer :: station_index
    integer :: sky_index
    integer :: target_index
    integer :: neighbor_index
    integer :: targ2neigh_index
    integer :: neigh2targ_index
    integer :: find_targ_index
    integer :: high_sky
    integer :: high_month_index
    integer :: first_month_index
    integer :: last_month_index
    integer :: targ_first_sky
    integer :: targ_last_sky
    integer :: neigh_first_sky
    integer :: neigh_last_sky
    integer :: neigh_first_year
    integer :: neigh_last_year
    integer :: neigh_high_sky
    integer :: high_target
    integer :: curr_span
    integer :: high_span
    integer :: high_found
    integer :: signif_found
    integer :: hit_count
    integer :: targets_size
    integer :: neighbors_size
    integer :: use_shf_meta
    logical :: stop_loop

    allocate(network_hit_counts(skyline_months))
    network_hit_counts(:) = 0

    targets_size = size(neighbors,1)
    neighbors_size = size(neighbors,2)

    ! If use_shf_meta is OPT_HISTORY_ONLY, then skip all this because we're only using SHF.
    use_shf_meta = get_property_int(PROP_USE_HISTORY_FILES)
    if(use_shf_meta == OPT_HISTORY_ONLY) return

    ! test the "higher count erasure" technique -
    !   target-pair-yr/mth-stat: final output changepoint_hits contain the
    !   total hits & est adj. parsed and attributed to the "Best Guess" stations
    call log_info('  NET  STN    FILT TECH    YEAR MTH            AVG   STD NUM ITFOUND')

    ! ------------------ pre-FILTER 1 ----------------------
    ! for each station and its sub-net in the network sum
    !   from the paired hit_count array:
    !   all the non-zero hits into the network_hit_counts array
    do station_index = 1, targets_size
      target_index = neighbors(station_index,1)%target_index
      targ_first_sky = get_first_sky(target_index)
      targ_last_sky = get_last_sky(target_index)
      do sky_index = targ_first_sky, targ_last_sky
        ! filter found by the paired confirm hits per technique
        hit_count = 0
        do targ2neigh_index = 2, neighbors_size
          neighbor_index = neighbors(target_index, targ2neigh_index)%neighbor_index
          if(neighbor_index == 0) exit
          ! used to find model types > 2 which are non-linear with steps
          signif_found = (changepoint_spans(targ2neigh_index,sky_index)%model_type) - 2
          if(signif_found > 0) then
            hit_count = hit_count + 1
          endif
        enddo
        ! filter found by the number of seperate pairs hit
        network_hit_counts(sky_index) = hit_count
      enddo
    enddo

    ! find the highest chgpt hit occurance for all targets/all yr-mths
    stop_loop = .false.
    do while (.not. stop_loop)
      high_found = 0
      ! for each target
      do station_index = 1, targets_size
        target_index = neighbors(station_index,1)%target_index
        targ_first_sky = get_first_sky(target_index)
        targ_last_sky = get_last_sky(target_index)
        if(targ_first_sky == MISSING_INT) cycle

        ! for each year/month
        do sky_index = targ_first_sky, targ_last_sky
          if (network_hit_counts(sky_index) > high_found) then
            high_found = network_hit_counts(sky_index)
            high_target = target_index
            high_sky = sky_index
            high_month_index = get_month_index_from_sky(target_index,high_sky)
          endif
        enddo ! end yr-mth search loop
      enddo ! end target search loop

      ! keep looping until high_found == 1
      if(high_found >= 2) then
        ! set the current highest hit station for this y/m/t
        target_index = high_target
        ! reset sky indices for "high net" target_index
        targ_first_sky = get_first_sky(target_index)
        targ_last_sky = get_last_sky(target_index)
        ! get the highest network_hit_counts out of the way, go thru the pairs
        do targ2neigh_index = 2, neighbors_size
          neighbor_index = neighbors(target_index, targ2neigh_index)%neighbor_index
          if(neighbor_index == 0) exit
          ! used to find model types > 2 which are non-linear with steps
          signif_found = (changepoint_spans(targ2neigh_index,high_sky)%model_type) - 2
          if(signif_found > 0) then
            ! fetch the chgpt number of the event
            high_span = changepoint_spans(targ2neigh_index,high_sky)%span_number
            if(high_span == 0) then
              call log_fatal("AttributeChangepoints::identify_culprits: model_number > 0;high_span == 0 "  &
                     //trim(log_string(signif_found))//" "//trim(log_string(signif_found)))
              stop 1
            endif

            ! go backward, clearing out peripherals
            do sky_index = high_sky-1, targ_first_sky, -1
              curr_span = changepoint_spans(targ2neigh_index,sky_index)%span_number
              if(curr_span /= high_span) exit
              changepoint_spans(targ2neigh_index,sky_index)%model_type = 0
              changepoint_spans(targ2neigh_index,sky_index)%span_number = 0
              network_hit_counts(sky_index) = network_hit_counts(sky_index) - 1
            enddo

            ! go forward, clearing out peripherals
            !   Note that high_sky is not cleared - this saves this station/time as a changepoint
            do sky_index = high_sky+1, targ_last_sky
              curr_span = changepoint_spans(targ2neigh_index,sky_index)%span_number
              if(curr_span /= high_span) exit
              changepoint_spans(targ2neigh_index,sky_index)%model_type = 0
              changepoint_spans(targ2neigh_index,sky_index)%span_number = 0
              network_hit_counts(sky_index) = network_hit_counts(sky_index) - 1
            enddo
          endif
        enddo ! end of paired station loop for target

        network_hit_counts(high_sky) = -1 * network_hit_counts(high_sky)
        ! go through all of the stations, skipping the highest
        !   (chosen) target
        do station_index = 1, targets_size
          neighbor_index = neighbors(station_index, 1)%target_index
          if(neighbor_index == target_index) cycle

          neigh_first_year = get_first_year(neighbor_index)
          neigh_last_year = get_last_year(neighbor_index)

          ! go through the neighbors of each paired station
          do neigh2targ_index = 2, neighbors_size
            find_targ_index = neighbors(neighbor_index, neigh2targ_index)%neighbor_index
            ! if one of the paired station neighbors is the chosen target
            if(find_targ_index == target_index) then
              ! retreive last and last neighbor station sky indices
              neigh_first_sky = get_first_sky(neighbor_index)
              if(neigh_first_year == MISSING_INT) cycle
              first_month_index = get_month_index(get_first_year(neighbor_index), 1)
              neigh_last_sky = get_last_sky(neighbor_index)
              if(neigh_last_year == MISSING_INT) cycle
              last_month_index = get_month_index(get_last_year(neighbor_index), 12)

              ! if this particular Target changepoint is NOT within the
              !   Neighbors POR then skip this neighbor!!!
              if(high_month_index < first_month_index .OR. high_month_index > last_month_index) cycle

              ! retreive neighbor station skyind index of the high_month_index
              neigh_high_sky = get_sky_from_month_index(neighbor_index,high_month_index)

              ! used to find model types > 2 which are non-linear with steps
              signif_found = (changepoint_spans(neigh2targ_index,neigh_high_sky)%model_type) - 2
              if(signif_found > 0) then
                ! fetch the chgpt number of the event
                high_span = changepoint_spans(neigh2targ_index,neigh_high_sky)%span_number
                if(high_span == 0) then
                  call log_info("AttributeChangepoints::identify_culprits: 2Nfound > 0;Nspan == 0 "  &
                         //trim(log_string(station_index))//" "//trim(log_string(target_index))//" "  &
                         //trim(log_string(neighbor_index))//" "//trim(log_string(high_month_index))  &
                         //" "//trim(log_string(high_span))//" "//trim(log_string(signif_found)))
                  stop 2
                endif

                ! go backward, clearing out peripherals
                do sky_index = neigh_high_sky-1, neigh_first_sky, -1
                  curr_span = changepoint_spans(neigh2targ_index,sky_index)%span_number
                  if(curr_span /= high_span) exit
                  changepoint_spans(neigh2targ_index,sky_index)%model_type = 0
                  changepoint_spans(neigh2targ_index,sky_index)%span_number = 0
                  network_hit_counts(sky_index) = network_hit_counts(sky_index) - 1
                enddo

                ! go forward, clearing out peripherals
                !   Note that neigh_high_sky is cleared - this removes it from being counted
                !   as a changepoint for that neighbor pair
                do sky_index = neigh_high_sky, neigh_last_sky
                  curr_span = changepoint_spans(neigh2targ_index,sky_index)%span_number
                  if(curr_span /= high_span) exit
                  changepoint_spans(neigh2targ_index,sky_index)%model_type = 0
                  changepoint_spans(neigh2targ_index,sky_index)%span_number = 0
                  network_hit_counts(sky_index) = network_hit_counts(sky_index) - 1
                enddo

              endif
              exit
            endif
          enddo ! end neighbors of pair loop
        enddo ! end of search for pairs to target
      else
        stop_loop = .true.
      endif
    enddo ! end of the highest occurance search for this tech

  end subroutine identify_culprits


  !> Based on previously retained hits for culprits (but eliminated from its neighbors)
  !!   assigns hits and offset information for each station/time of all targets.
  !!
  !! @param[in] neighbors A collection of pairs for each station with their ids.
  !! @param[in] changepoint_spans For all skyline segments and for all neighbors of each target
  !!   shows the influence of neighbors on their targets (this is a large array in terms of memory).
  !! @param[in] changepoint_sums For all skyline the cumulative adjustments due to changepoints
  !!   from station pairs.
  !! @param[out] changepoint_hits Attributed undocumented changepoints.
  subroutine assign_changepoints(neighbors, changepoint_spans, changepoint_sums, changepoint_hits)

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ChangepointSpan), dimension(:,:), intent(in) :: changepoint_spans
    type(ChangepointSums), dimension(:), intent(in) :: changepoint_sums
    type(ChangepointHit), dimension(:), intent(out) :: changepoint_hits

    integer :: target_index
    integer :: targ2neigh_index
    integer :: neighbor_index
    integer :: station_index
    integer :: sky_index
    integer :: targ_first_sky
    integer :: targ_last_sky
    integer :: min_neighbors
    integer :: hit_count
    integer :: sum_count
    integer :: year
    integer :: month
    real :: offset_avg
    real :: zscore_avg
    real :: offset_sum
    real :: zscore_sum
    character(len=1024) :: log_message

    ! follow all the "found" chgpts - if exactly equal to last
    min_neighbors = get_property_int(PROP_CONFIRM_MIN_NEIGHS)

    ! all of the peripheral "hits" in the changepoint_spans array associated
    !   with each filtered changepoint should be zeroed out by this
    !   point, leaving only the attributed chgpt-station data
    do station_index = 1, size(neighbors,1)
      target_index = neighbors(station_index,1)%target_index
      if(target_index == 0) cycle
      targ_first_sky = get_first_sky(target_index)
      targ_last_sky = get_last_sky(target_index)
      ! go thru the year/months again
      do sky_index = targ_first_sky, targ_last_sky
        hit_count = 0
        ! and its sub-net in the network
        do targ2neigh_index = 2, size(neighbors,2)
          neighbor_index = neighbors(target_index, targ2neigh_index)%neighbor_index
          if(neighbor_index == 0) exit
          ! accumulate the remaining hits
          if(changepoint_spans(targ2neigh_index,sky_index)%model_type /= 0) hit_count = hit_count + 1
        enddo ! end of paired station loop

        ! filter found by the number of seperate pairs hit
        if(hit_count >= min_neighbors) then
          ! generate adjustment around output (min_neighbors > 2 assumed)
          offset_sum = changepoint_sums(sky_index)%offset_sum
          zscore_sum = changepoint_sums(sky_index)%zscore_sum
          sum_count = changepoint_sums(sky_index)%sum_count

          ! save for the next filter
          offset_avg = offset_sum / sum_count
          ! zchgpt has the z-scores for the size of hit
          zscore_avg = zscore_sum / sum_count
          changepoint_hits(sky_index) = new_ChangepointHit(sum_count, zscore_avg)

          ! Logging
          call get_year_month_from_sky(year, month, sky_index, station_index) ! yr/mo needed only for log message
          write(log_message,1000) target_index, neighbors(target_index,1)%target_id, 1, sky_index, year, month, &
                    offset_avg, zscore_avg, sum_count, hit_count
     1000 format(i5,1x,a,'-CONFRM MW',i1,' at ',i10,i5,i3,' AVG ADJ: ',2f6.2,2i4)
          call log_info(trim(log_message))

        endif
      enddo ! end of yr/mth loop
    enddo ! end of target loop

  end subroutine assign_changepoints

end module AttributeChangepoints

!> @file
!! Contains functionality for attributing each potential change point to the station
!! most likely responsible.
