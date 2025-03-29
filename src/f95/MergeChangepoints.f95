!> @brief
!! Merges together changepoints that fall within a sufficiently small time window
!! from documented changepoints (See req. 1.7) and then changepoints that fall within
!! a sufficiently small time window from each other (See req. 1.8). “Sufficiently small”
!! can be anywhere from 0.4 to 59 months, depending on the result of the calculation
!! described in req. 1.6.1. See Reqs. 1.7 and 1.8 for more information.
!!
!! @copyright
!! THIS SOFTWARE and ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC
!! DOMAIN and THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE. THEY ARE
!! FURNISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
!! INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
!! EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE and
!! DOCUMENTATION FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR
!! THE USE OF THE SOFTWARE and DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL
!! SUPPORT TO USERS.
!!
!!
module MergeChangepoints
     
  use PropertyReader
  use SkylineUtils
  use MathUtils
  use FileUtils
  use StationType
  use StationNeighborPairType
  use ElementValueType
  use ChangepointSpanType
  use ChangepointType
  use ChangepointHitType
  use AlgorithmParameters
  use PropertyParameters

  implicit none

contains

  !> Merges together changepoints that fall within a sufficiently small time window
  !!   from documented changepoints (See req. 1.7) and then changepoints that fall within
  !!   a sufficiently small time window from each other (See req. 1.8). “Sufficiently small”
  !!   can be anywhere from 0.4 to 59 months, depending on the result of the calculation
  !!   described in req. 1.6.1.
  !!
  !! @param[in] neighbors A collection of pairs for each station with their ids.
  !! @param[in] element_data_process An array for all skyline for the processed data values.
  !! @param[in] history_changepoints The history records for stations.
  !! @param[in] changepoint_spans For all skyline segments and for all neighbors of each target
  !!   shows the influence of neighbors on their targets (this is a large array in terms of memory).
  !! @param[inout] changepoint_hits Attributed undocumented changepoints.
  !! @param[out] merged_hits Merged documented and undocumented changepoints.
  !! @param[out] delete_flags Target/skyline indexes indicating values to be deleted.
  subroutine merge_changepoints(neighbors, element_data_process, history_changepoints, changepoint_spans, changepoint_hits,  &
                                merged_hits, delete_flags)
    use CommonVariables, only: skyline_months

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ElementValue), dimension(:), intent(in) :: element_data_process
    type(Changepoint), dimension(:,:), intent(in) :: history_changepoints
    type(ChangepointSpan), dimension(:,:) :: changepoint_spans
    type(ChangepointHit), dimension(:) :: changepoint_hits
    integer, dimension(:), allocatable, intent(out) :: merged_hits
    logical(1), dimension(:,:), allocatable, intent(out) :: delete_flags

    integer, dimension(:), allocatable :: work_hits ! Work array of hits to be merged
    integer :: use_shf_meta

    allocate(delete_flags(size(neighbors,2), skyline_months))
    allocate(work_hits(skyline_months))
    allocate(merged_hits(skyline_months))
    work_hits(:) = 0
    merged_hits(:) = 0

    ! Do standard merging with found changepoints unles configured for history ONLY
    use_shf_meta = get_property_int(PROP_USE_HISTORY_FILES)
    if(use_shf_meta /= OPT_HISTORY_ONLY) then

      call merge_history_changepoints(neighbors, element_data_process, history_changepoints, changepoint_hits, work_hits)

      call merge_clustered_changepoints(neighbors, element_data_process, changepoint_hits, work_hits)

    ! Else if using ONLY station history files (SHF) then set changepoints to
    ! SHF metadata and skip pairwise segmentation and series attribution
    ! (confirmfilt) and go directly to chgpt amplitude estimation
    else

      call set_changepoints_to_history(neighbors, history_changepoints, work_hits)

    endif

    ! initialize temporary array for all station/neigh/months
    delete_flags = FALSE

    call flag_suspect_data(neighbors, element_data_process, changepoint_spans, delete_flags)

    call align_and_set_changepoints(neighbors, element_data_process, work_hits, merged_hits)

  end subroutine merge_changepoints


  !> Merges together changepoints that fall within a sufficiently small time window
  !!   from documented changepoints. (See req. 1.7)
  !!
  !! @param[in] neighbors A collection of pairs for each station with their ids.
  !! @param[in] element_data_process An array for all skyline for the processed data values.
  !! @param[in] history_changepoints The history records for stations.
  !! @param[inout] changepoint_hits Attributed undocumented changepoints.
  !! @param[out] work_hits Work array of merged documented and undocumented changepoints.
  subroutine merge_history_changepoints(neighbors, element_data_process, history_changepoints, changepoint_hits, work_hits)
    use CommonVariables, only: skyline_months

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ElementValue), dimension(:), intent(in) :: element_data_process
    type(Changepoint), dimension(:,:), intent(in) :: history_changepoints
    type(ChangepointHit), dimension(:) :: changepoint_hits
    integer, dimension(:) :: work_hits

    logical(1), dimension(:), allocatable :: history_hits
    integer :: station_index
    integer :: target_index
    integer :: bracket_end
    integer :: bracket_index
    integer :: bracket_sky_index
    integer :: sky_index
    integer :: expansion_index
    integer :: amploc_percent_index
    integer :: ampl_index
    integer :: target_first_year
    integer :: target_first_sky
    integer :: target_last_sky
    integer :: hit_count_sum
    integer :: up_miss
    integer :: down_miss
    integer :: year
    integer :: month
    integer :: i
    real :: hit_ampl
    real :: abs_hit_ampl
    logical :: do_next_bracket
    character(len=1024) :: log_message

    amploc_percent_index = get_amploc_percent_index(get_property_int(PROP_AMPLOC_PERCENT))

    allocate(history_hits(skyline_months))
    ! initialize Stn Hist array
    history_hits(:) = FALSE

    do station_index = 1, size(neighbors,1)
      target_index = neighbors(station_index,1)%target_index
      if(target_index == 0) cycle
      do i = 1, size(history_changepoints,2)
        if(history_changepoints(station_index,i)%sky_month == 0) exit
        sky_index = history_changepoints(station_index,i)%sky_month
        ! stn hist is locked into position by SHF (and UCP in filter 3)
        call log_info(" SHHITS set: "//trim(log_string(target_index))//" "//trim(log_string(sky_index)))
        history_hits(sky_index) = .true.
        work_hits(sky_index) = 100
      enddo
    enddo

    ! for each target station...
    do station_index = 1, size(neighbors,1)
      target_index = neighbors(station_index,1)%target_index
      if(target_index == 0) cycle
      target_first_year = get_first_year(target_index)
      target_first_sky = get_first_sky(target_index)
      target_last_sky = get_last_sky(target_index)
      ! for all the year/months
      do sky_index = target_first_sky, target_last_sky
        ! find the hits from statsubs
        if(changepoint_hits(sky_index)%hit_count > 0) then
          ! use amplitude to define mth range
          hit_ampl = changepoint_hits(sky_index)%amplitude
          hit_count_sum = changepoint_hits(sky_index)%hit_count
          ! standardize the size of the offset
          abs_hit_ampl = abs(hit_ampl)
          do ampl_index = 1,AMP_RANGE_COUNT
            if(abs_hit_ampl < AMP_RANGES(ampl_index)) exit
          enddo
          if(ampl_index > AMP_RANGE_COUNT) ampl_index = AMP_RANGE_COUNT

          ! make the search bracket twice the radius plus the current yr/mth
          bracket_end = MERGE_RADII(ampl_index, amploc_percent_index)*2 + 1

          ! Go through the bracket of months, look for SHF record start at the
          ! hit-point index and expand outward in the series 0, -1, 1, -2, 2, etc.
          ! This will merge the hits with the closest SHF record keep track of
          ! missing values in both directions
          up_miss = 0
          down_miss = 0
          do bracket_index = 1, bracket_end
            expansion_index = bracket_index/2
            if(mod(bracket_index,2) == 1) then
              call skip_missing(element_data_process, bracket_sky_index, sky_index, expansion_index, up_miss, 1,  &
                                target_last_sky, do_next_bracket)
              if (do_next_bracket) cycle
            else
              call skip_missing(element_data_process, bracket_sky_index, sky_index, expansion_index, down_miss, -1,  &
                                target_first_sky, do_next_bracket)
              if (do_next_bracket) cycle
            endif

            ! If there is an SHF record (at jmo) absorb the UCP hits (from imo)
            if(history_hits(bracket_sky_index))then
              work_hits(bracket_sky_index) = work_hits(bracket_sky_index) + hit_count_sum
              changepoint_hits(sky_index)%hit_count = 0
              changepoint_hits(sky_index)%amplitude = 0.0

              ! Logging
              call get_year_month_from_sky(year, month, sky_index, station_index) ! yr/mo needed only for log message
              write(log_message,"(i5,1x,a,'-SHFHIT KW ',i1,' at ',i10,i5,i3,i4,2f6.2,i9,2i5)")  &
                  target_index, neighbors(target_index,1)%target_id, 1, sky_index, year, month, hit_count_sum, &
                  hit_ampl, abs_hit_ampl, bracket_sky_index, MERGE_RADII(ampl_index, amploc_percent_index), &
                  work_hits(bracket_sky_index)
              call log_info(log_message)

              ! found what we wanted, break out
              exit
            endif ! end of SHF record occurence

          enddo ! end of the bracket loop
        endif
      enddo ! end of year/month loop
    enddo ! end of station loop

    deallocate(history_hits)

  end subroutine merge_history_changepoints


  !> Merges changepoints that fall within a sufficiently small time window from each other
  !!   (See req. 1.8).
  !!
  !! @param[in] neighbors A collection of pairs for each station with their ids.
  !! @param[in] element_data_process An array for all skyline for the processed data values.
  !! @param[inout] changepoint_hits Attributed undocumented changepoints.
  !! @param[inout] work_hits Work array of merged documented and undocumented changepoints.
  subroutine merge_clustered_changepoints(neighbors, element_data_process, changepoint_hits, work_hits)
    use CommonVariables, only: por_months

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ElementValue), dimension(:), intent(in) :: element_data_process
    type(ChangepointHit), dimension(:) :: changepoint_hits
    integer, dimension(:) :: work_hits

    ! Property indicating the hits in station pairs to confirm a chgpt
    ! Used to be env variable CONFIRM <2>,3,4,5
    integer :: min_neighbor_hits

    ! internal variables
    integer, dimension(:) :: high_hit_counts(por_months)
    integer :: station_index
    integer :: target_index
    integer :: bracket_end
    integer :: high_hit
    integer :: high_sky
    integer :: bracket_index
    integer :: ampl_index
    integer :: year
    integer :: month
    integer :: sky_index
    integer :: sky_offset
    integer :: first_month_index
    integer :: last_month_index
    integer :: num_months
    integer :: target_first_sky
    integer :: target_last_sky
    integer :: target_first_year
    integer :: up_miss
    integer :: down_miss
    integer :: hit_count
    integer :: expansion_index
    integer :: sky_index_expans
    integer :: sky_offset_expans
    integer :: hit_count_sum
    integer :: num_merge_months
    integer :: amploc_percent_index
    real :: high_ampl
    real :: abs_high_ampl
    real :: hit_ampl
    logical :: do_next_bracket
    logical :: hits_found
    logical :: no_more_highs
    character(len=256) :: log_message

    num_merge_months = -2
    call log_info('Merge months depend on chgpt amp :'//trim(log_string(num_merge_months)))
    min_neighbor_hits = get_property_int(PROP_CONFIRM_MIN_NEIGHS)

    amploc_percent_index = get_amploc_percent_index(get_property_int(PROP_AMPLOC_PERCENT))

    ! now the third filter.....(the old second filter)
    ! THESE SHOULD BE UCP'S WITH NO SHF RECORDS
    ! these are the rules:
    !   Input is all the hits that are leftover from the SHF filter in the
    !     changepoint_hits array
    !   go back through year/months.....
    !     for each technique...
    !       find the highest remaining hits (down to confirm)
    !        accumulate all of the "hit_counts" & "ntests" for each month
    !          +/- MERGE_RADII(from ChangepointHit%amplitude) while skipping missing data
    !     (all changepoint_hits used in accum are zeroed out)
    !     for all accumulations within num_merge_months
    !       when given month are greater or equal to ithres then add to
    !         the changepoint_hits array

    !     for each station...
    call log_info('NET  STN    FILT TECH     YR MTH  SUM  AVG  STD     RNG PR THRES')

    do station_index = 1, size(neighbors,1)
      target_index = neighbors(station_index,1)%target_index
      target_first_sky = get_first_sky(target_index)
      target_first_year = get_first_year(target_index)
      if(target_first_year == MISSING_INT) cycle
      first_month_index = get_month_index(target_first_year, 1)
      target_last_sky = get_last_sky(target_index)
      if(get_last_year(target_index) == MISSING_INT) cycle
      last_month_index = get_month_index(get_last_year(target_index), 12)
      num_months = last_month_index - first_month_index + 1

      !  initialize interim array...
      high_hit_counts(:) = 0

      ! iterate until there are no more high points
      no_more_highs = .false.
      do while (.not. no_more_highs)

        high_hit = 0
        high_ampl = 0.0
        ! find the "highest count"
        do sky_index = target_first_sky, target_last_sky
          hit_count = 0
          ! get hits for only those greater than min_neighbor_hits (:= 2)
          if(changepoint_hits(sky_index)%hit_count >= min_neighbor_hits)then
            hit_count = changepoint_hits(sky_index)%hit_count
            hit_ampl = changepoint_hits(sky_index)%amplitude
          endif
          ! find the highest chgpt hit station
          if (hit_count > high_hit) then
            high_hit = hit_count
            high_sky = sky_index
            high_ampl = hit_ampl
          endif
        enddo ! end find the highest count loop

        ! the highest hit value in the array is at high_sky, the sum of
        ! hits over all stat-tests is high_hit, and the est. adj. is high_ampl
        ! keep going until there are no more hits
        if(high_hit > 0) then
          ! bracket the highest hit +/- num_merge_months (from command line)
          if(num_merge_months /= -2) then
            bracket_end = num_merge_months * 2 + 1
          else
            ! else bracket using ampitude of chgpt to define mth range
            ! base the range on the standardized amplitude est.
            ! standardize the size of the offset
            abs_high_ampl = abs(high_ampl)
            do ampl_index = 1,AMP_RANGE_COUNT
              if(abs_high_ampl < AMP_RANGES(ampl_index)) exit
            enddo
            if(ampl_index > AMP_RANGE_COUNT) ampl_index = AMP_RANGE_COUNT
            bracket_end = MERGE_RADII(ampl_index, amploc_percent_index) * 2 + 1
          endif ! end of defining the search bracket

          ! Go through the bracket, look for the highest hits already found start at the
          ! hit-point index and expand outward in the series 0, -1, 1, -2, 2, etc.
          ! Keep track of missing values in both directions.
          up_miss = 0
          down_miss = 0
          hits_found = .FALSE.
          do bracket_index = 1, bracket_end
            expansion_index = bracket_index/2
            if(mod(bracket_index,2) == 1) then
              call skip_missing(element_data_process, sky_index_expans, high_sky, expansion_index,  &
                                up_miss, 1, target_last_sky, do_next_bracket)
              if (do_next_bracket) cycle
            else
              call skip_missing(element_data_process, sky_index_expans, high_sky, expansion_index,  &
                                down_miss, -1, target_first_sky, do_next_bracket)
              if (do_next_bracket) cycle
            endif ! end of finding the next non-missing datum

            ! absorb lesser hit into closest higher hit
            sky_offset_expans = sky_index_expans - target_first_sky + 1
            if(sky_offset_expans < 1 .OR. sky_offset_expans > num_months) then
              cycle
            endif
            if(high_hit_counts(sky_offset_expans) > 0) then
              high_hit_counts(sky_offset_expans) = high_hit_counts(sky_offset_expans) + high_hit
              hits_found = .TRUE.
              exit
            endif
          enddo ! end of bracket loop

          ! if no hits found, setup new hit
          ! NOTE: to keep the array sizes reasonable, use offset
          ! from target_first_sky (begin of record) for these arrays
          if (hits_found .eqv. .FALSE.) then
            sky_offset = high_sky - target_first_sky + 1
            high_hit_counts(sky_offset) = high_hit
          endif

          ! zero test array block for next iter
          changepoint_hits(high_sky)%hit_count = 0
          changepoint_hits(high_sky)%amplitude = 0.0
        else
          ! no hits >= min_neighbor_hits: stop
          no_more_highs = .true.
        endif ! end of working with this UCP
      enddo ! end of finding UCP for this station

      ! examine the interim high_hit_counts array for station's filtered chgpts
      do sky_index = target_first_sky, target_last_sky
        sky_offset = sky_index - target_first_sky + 1
        if(high_hit_counts(sky_offset) > 0) then
          hit_count_sum = high_hit_counts(sky_offset)
          ! test for minimum number of hits
          if(hit_count_sum >= min_neighbor_hits) then
            ! passed threshold test - put interim into final
            work_hits(sky_index) = work_hits(sky_index) + hit_count_sum
            call get_year_month_from_sky(year,month,sky_index,target_index)
            write(log_message,3000) target_index, neighbors(target_index,1)%target_id, 1, year,month,  &
                                    hit_count_sum, bracket_end, min_neighbor_hits
       3000 format(i5,1x,a,'-UCHGPT KW',i1,' at ',i4,i3,i4,2i3)
            call log_info(trim(log_message))
          endif
        endif ! end of using interim hit
      enddo ! end of examining interim hit array
    enddo ! end of station loop

  end subroutine merge_clustered_changepoints


  !> When undocumented changepoints are not considered, changepoints are derived purely
  !!   from the history files.
  !!
  !! @param[in] neighbors A collection of pairs for each station with their ids.
  !! @param[in] history_changepoints The history records for stations.
  !! @param[inout] work_hits Work array of only documented changepoints derived from history data.
  subroutine set_changepoints_to_history(neighbors, history_changepoints, work_hits)

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(Changepoint), dimension(:,:), intent(in) :: history_changepoints(size(neighbors,1), MAX_CHANGEPOINTS)
    integer, dimension(:) :: work_hits
    integer :: station_index
    integer :: move_index
    integer :: sky_index

    do station_index = 1, size(neighbors,1)
      if(neighbors(station_index,1)%target_index == 0) cycle ! if not a valid neighbor pair
      do move_index = 1, size(history_changepoints,2)
        if(history_changepoints(station_index, move_index)%sky_month == 0) exit
        sky_index = history_changepoints(station_index, move_index)%sky_month
        work_hits(sky_index) = 100
      enddo
    enddo

  end subroutine set_changepoints_to_history

  !> Increments the missing data counter for a particular direction and expansion index.
  !!
  !! @param[in] element_data_process An array for all skyline for the processed data values.
  !! @param[out] sky_index_expans The skyline value of the current expansion.
  !! @param[in] sky_index The central skyline index.
  !! @param[in] expans_index The amplitude of the expansion.
  !! @param[inout] miss_count The missing value counter.
  !! @param[in] direction The direction of the expansion (1 or -1).
  !! @param[in] high_low The high or low extent of the expansion.
  !! @param[out] do_next_bracket The flag to exit the expansion bracket process.
  subroutine skip_missing(element_data_process, sky_index_expans, sky_index, expans_index,  &
                          miss_count, direction, high_low, do_next_bracket)

    type(ElementValue), dimension(:), intent(in) :: element_data_process
    integer, intent(out) :: sky_index_expans
    integer, intent(in) :: sky_index
    integer, intent(in) :: expans_index
    integer :: miss_count
    integer, intent(in) :: direction
    integer, intent(in) :: high_low
    logical, intent(out) :: do_next_bracket

    do_next_bracket = .false.

    do
      sky_index_expans = sky_index + direction*expans_index + direction*miss_count
      ! negative direction effectively reverses greater than to less than
      if(direction*sky_index_expans > direction*high_low) then
        do_next_bracket = .true.
        exit
      endif
      if(element_data_process(sky_index_expans)%value < MISSING_REAL+1.) then
        ! this month is missing, skip to the next
        miss_count = miss_count + 1
      else
        exit
      endif
    enddo

  end subroutine skip_missing

  !> Flags suspect data where delete flags exceed the limit (now = 5).
  !!
  !! @param[in] neighbors The array of station neighbors and their IDs.
  !! @param[inout] element_data_process An array for all skyline for the processed data values.
  !! @param[in] changepoint_spans For all skyline segments and for all neighbors of each target
  !!   shows the influence of neighbors on their targets (this is a large array in terms of memory).
  !! @param[out] delete_flags Flag for data deletion.
  subroutine flag_suspect_data(neighbors, element_data_process, changepoint_spans, delete_flags)

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ElementValue), dimension(:) :: element_data_process
    type(ChangepointSpan), dimension(:,:), intent(in) :: changepoint_spans
    logical(1), dimension(:,:), intent(out) :: delete_flags

    integer :: station_index
    integer :: target_index
    integer :: targ2pair_index
    integer :: sky_index
    integer :: num_delete
    integer :: target_first_sky
    integer :: target_last_sky
    integer :: num_delete_limit

    ! threshold to removed data from amp est. Cannot be zero.
    num_delete_limit = 5
    call log_info('Suspect num_delete_limit: '//trim(log_string(num_delete_limit)))

    ! filter element value array to remove "num_delete < num_delete_limit" data
    do station_index = 1, size(neighbors,1)
      target_index = neighbors(station_index,1)%target_index
      target_first_sky = get_first_sky(target_index)
      target_last_sky = get_last_sky(target_index)

      do sky_index = target_first_sky, target_last_sky
        if(element_data_process(sky_index)%value .gt. MISSING_REAL+1.) then

          ! flag suspect data where sum(num_delete) >= num_delete_limit for adj est
          num_delete = 0
          do targ2pair_index = 2, size(changepoint_spans,1)
            if(changepoint_spans(targ2pair_index,sky_index)%is_deleted) then
              num_delete = num_delete + 1
              delete_flags(targ2pair_index,sky_index) = TRUE
            endif
          enddo

          if(num_delete .ge. num_delete_limit) then
            ! set to missing
            element_data_process(sky_index)%flag(1:2) = ' d'
          endif

        endif ! end of if data missing?
      enddo ! end of sky index loop
    enddo  ! end of station loop
  end subroutine flag_suspect_data


  !> Send changepoints, whether from history or found by algorithm,
  !!   through align_changepoints and set for final processing.
  !!
  !! @param[in] neighbors A collection of pairs for each station with their ids.
  !! @param[inout] element_data_process An array for all skyline for the processed data values.
  !! @param[in] work_hits Work array of merged documented and undocumented changepoints.
  !! @param[out] merged_hits Merged documented and undocumented changepoints.
  subroutine align_and_set_changepoints(neighbors, element_data_process, work_hits, merged_hits)

    type(StationNeighborPair), dimension(:,:), intent(in) :: neighbors
    type(ElementValue), dimension(:) :: element_data_process
    integer, dimension(:), intent(in) :: work_hits
    integer, dimension(:), intent(out) :: merged_hits

    type(Changepoint), dimension(:,:) :: changepoints(size(neighbors,1), MAX_CHANGEPOINTS)
    character(len=11) :: station_id
    integer :: station_index
    integer :: sky_index
    integer :: move_index
    integer :: move_count
    integer :: first_sky
    integer :: last_sky
    integer, dimension(:) :: move_nums(size(neighbors,1))

    ! The fourth filter
    ! Since the amplitude est MUST rely upon a minimum of MINLEN (=18) months
    ! to get even close to a reliable estimate at this point, it is assumed
    ! that the UCP are as good as the SHF -
    ! Therefore, align moves with respect to non-missing data and compress out
    ! changes that are too close and the data between them (i.e. less than MINLEN)

    changepoints(:,:) = null_Changepoint()

    ! Converts changepoints from skyline to station,changepoint_count format
    do station_index = 1, size(neighbors,1)
      station_id = neighbors(station_index,1)%target_id
      first_sky = get_first_sky(station_index)
      last_sky = get_last_sky(station_index)

      move_count = 0
      ! Generate arrays for alignment
      do sky_index = first_sky, last_sky
        if(work_hits(sky_index) .ne. 0) then
          move_count = move_count + 1
          changepoints(station_index, move_count) = new_Changepoint(sky_index, 31, (work_hits(sky_index)*1.0))
        endif
      enddo
      move_nums(station_index) = move_count
    enddo

    call align_changepoints(changepoints, element_data_process)

    ! Converts changepoints from station,changepoint_count to skyline format
    do station_index = 1, size(neighbors,1)
      first_sky = get_first_sky(station_index)
      last_sky = get_last_sky(station_index)

      ! put the data into a merged_hits array
      if(move_nums(station_index) == 0) cycle
      do move_index = 1, move_nums(station_index)
        sky_index = changepoints(station_index, move_index)%sky_month
        if(sky_index > 0) then
          merged_hits(sky_index) = changepoints(station_index, move_index)%amplitude
        else
          call log_debug("Station history changepoint has invalid sky_month value. station index: " &
                        //trim(log_string(station_index))//" move_index: "//trim(log_string(move_index))  &
                        //" "//changepoint_string(changepoints(station_index, move_index)))
        end if
      enddo
    enddo

  end subroutine align_and_set_changepoints

     
  !> Aligns changepoints based on the day of the month from the station history
  !!   (this not a factor after the merging process since all days are set to 31)
  !!   and then sets to missing any segments that are not long enough.
  !!
  !!   Steps:
  !!     A) see if the current month can be used, depending upon the day of the move
  !!       1) if the day > 25 the current month is used with the earlier segment
  !!       2) if the 5 < day < 25 the current month is unusable (make missing)
  !!            and use the previous month
  !!       3) if the day < 5 then the current month is used in the later segment
  !!            and use the previous month
  !!     B) Adjust the move backward to the first month with data
  !!     C) Remove any segments without sufficient data for adjustment calculation
  !!
  !! @param[inout] changepoints The documented or undocumented changepoints for stations.
  !! @param[inout] element_data_process An array for all skyline for the processed data values.
  subroutine align_changepoints(changepoints, element_data_process)

    type(Changepoint), dimension(:,:) :: changepoints
    type(ElementValue), dimension(:) :: element_data_process

    integer :: station_index
    integer :: first_por
    integer :: last_por
    integer :: first_sky
    integer :: last_sky
    integer :: first_index
    integer :: last_index
    integer :: sky_index
    integer :: change_sky
    integer :: target_first_sky
    integer :: target_last_sky
    integer :: target_first_year
    integer :: move_count
    integer :: move_index
    integer :: curr_move
    integer :: num_seg_points
    integer :: i

    ! local arrays
    integer :: aligned_sky(size(changepoints,2))
    real :: aligned_ampl(size(changepoints,2))

    ! minimum size of a station history (or SHF + "discovered") segments
    integer, parameter :: MIN_SHF_LENGTH = 24

    do station_index = 1, size(changepoints,1)
      ! If no known changepoints, don't process this station
      if(changepoint_is_null(changepoints(station_index, 1))) cycle

      ! initialize local arrays
      aligned_sky(:) = 0
      aligned_ampl(:) = 0.0

      ! fetch the begin and end month indices of the the first and last
      ! years of the stations data.
      target_first_sky = get_first_sky(station_index)
      target_first_year = get_first_year(station_index)
      target_last_sky = get_last_sky(station_index)

      if(target_first_year /= -9999) then
        ! re-adjust the sky & year_month indices to actual data (instead of Jan-Dec)
        do sky_index = target_first_sky, target_last_sky
          if(element_data_process(sky_index)%value > MISSING_REAL + 1) then
            first_sky = sky_index
            exit
          endif
        enddo

        do sky_index = target_last_sky, target_first_sky, -1
          if (element_data_process(sky_index)%value > MISSING_REAL + 1) then
            last_sky = sky_index
            exit
          endif
        enddo

        ! from this point on, the actual first and last dates are:
        ! first_sky and last_sky for the skyline indices

        ! Adjust for the day of change (the move is the last month of a segment)
        move_index = 1
        do while(.not. changepoint_is_null(changepoints(station_index, move_index)))
          if(changepoints(station_index, move_index)%sky_month >= first_sky  &
                 .and. changepoints(station_index, move_index)%sky_month <= last_sky) then

            ! if day <= 5 then set move back 1 month (this month will go with the next segment)
            if(changepoints(station_index, move_index)%day_of_month <= 5) then
              if(move_index > 1) then
                changepoints(station_index, move_index)%sky_month = changepoints(station_index, move_index)%sky_month-1
              endif
            ! if day > 5 && <= 25 then set move back 1 month and delete month data (data is bad)
            else if(changepoints(station_index, move_index)%day_of_month < 25  &
                        .and. changepoints(station_index, move_index)%sky_month > first_sky) then
              change_sky = changepoints(station_index, move_index)%sky_month
              if(element_data_process(change_sky)%value .ne. MISSING_REAL) &
                element_data_process(change_sky)%flag(1:2) = ' r'
              element_data_process(change_sky)%value = MISSING_REAL
              changepoints(station_index, move_index)%sky_month = changepoints(station_index, move_index)%sky_month-1
            endif
            ! if day > 25 then leave move were it is and keep data
          endif
          move_index = move_index + 1
        enddo
        move_count = move_index - 1
        ! go through the station history and data,
        ! adding moves that can be tested to the NEWMOVE arrays

        ! get stn hist move index up to start of the data
        move_index = 1
        do while (changepoints(station_index, move_index)%sky_month < first_sky .and. move_index < move_count)
          move_index = move_index + 1
        enddo

        ! now for the data period-of-record
        first_por = first_sky
        last_por = last_sky
        curr_move = 0
        first_index = 0
        last_index = -1
        num_seg_points = 0

        do sky_index = first_sky, last_sky
          if(element_data_process(sky_index)%value .ne. MISSING_REAL) then
            ! set the first value index for this segment
            if(first_index == 0) first_index = sky_index
            ! set the last value index
            last_index = sky_index
            ! number of data points in this segment
            num_seg_points = num_seg_points + 1
          endif

          ! see if we are at the end of a hist seg or the end of the POR
          if(sky_index == changepoints(station_index, move_index)%sky_month .or. sky_index == last_sky) then
            ! is there enough data in the segment
            if(num_seg_points >= MIN_SHF_LENGTH) then
              ! if enough, then set aligned_sky for usable segment
              curr_move = curr_move + 1
              ! if first move - (re)align first data point with first of segment
              if(curr_move == 1) first_por = first_index
              last_por = last_index
              ! if a stn hist move, add the data to aligned_sky
              if(sky_index == changepoints(station_index, move_index)%sky_month) then
                aligned_sky(curr_move) = changepoints(station_index, move_index)%sky_month
                aligned_ampl(curr_move) = changepoints(station_index, move_index)%amplitude
              ! if the end-of-record - generate last segment
              else
                aligned_sky(curr_move) = last_sky
                aligned_ampl(curr_move) = 0.0
              endif
            else
              if(.not. ((first_index .eq. last_index .and. first_index .eq. first_sky) .or. (first_index .eq. 0))) then
                ! if not enough, erase data
                do i = first_index, last_index
                  if(element_data_process(i)%value .ne. MISSING_REAL) then
                    element_data_process(i)%value = MISSING_REAL
                    element_data_process(i)%flag(1:2) = ' X'
                  endif
                enddo
              endif
            endif

            ! increment move and init segment variables
            ! watch out for multiple moves on the same date
            do while (changepoints(station_index, move_index)%sky_month == changepoints(station_index, move_index+1)%sky_month  &
                       .and. move_index <= move_count)
              move_index = move_index + 1
            enddo
            move_index = move_index + 1
            num_seg_points = 0
            first_index = 0
            last_index = -1
          endif
        enddo

        move_count = curr_move

      else
        call log_info('No data for station_index: '//trim(log_string(station_index)))
      endif ! target_first_year /= -9999

      ! initialize SHF input arrays for the alignmove output
      changepoints(station_index,:) = null_Changepoint()

      if(move_count == 0) then
        call log_warn("station_index: "//trim(log_string(station_index))//' No segments - DATA REMOVED')
        cycle
      endif

      do move_index = 1, move_count
        changepoints(station_index, move_index)%sky_month = aligned_sky(move_index)
        changepoints(station_index, move_index)%day_of_month = 31
        changepoints(station_index, move_index)%amplitude = aligned_ampl(move_index)
      enddo
    enddo

  end subroutine align_changepoints

end module MergeChangepoints

!> @file
!! Merges together changepoints that fall within a sufficiently small time window
!!   from documented changepoints from station history and then changepoints
!!   that fall within a sufficiently small time window from each other.
