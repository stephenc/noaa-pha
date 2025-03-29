!> @brief
!! Contains subroutines and functions used for determining the shape of a line
!! and how it fits to a model (described as M1 - M5 in requirements). This is
!! used to classify potential change points to determine their shape and
!! remove straight lines and slopes. See Req. 1.4 for more information.
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
module ModelFitUtils

  use Logger
  use PropertyReader
  use AlgorithmParameters
  use PropertyParameters

  implicit none

contains

  !> Selects the most appropriate model for a given single changepoint along with
  !!   the adjacent segments using the Bayesian Information Criteria (BIC). For the
  !!   "best" (lowest) model to be selected the BIC must be less than that of simpler
  !!   models. Also, models with slopes:
  !!     1) a minimum of 5 years (60 months) of raw data is required and
  !!     2) the t-test for the slope parameter(s) must be significant
  !!   Chgptmodels is a consolidation of all the statistical tests and their attendant routines
  !!   for the UCP. An effort has been made to standardize the tests for simplicity sake.
  !!   Model types:
  !!     Homogeneous linear types:
  !!     1    slr0 - simple flat line
  !!     2    slr1 - sloped straight line
  !!     Two phase regression types:
  !!     3    tpr0 - amplitude only, 0 sloped segments
  !!     4    tpr1 - amplitude shift with equal sloped segments
  !!     5    tpr2 - amplitude shift and non-equal sloped segments
  !!     6    tpr3 - amplitude shift with flat-to-slope segments
  !!     7    tpr4 - amplitude shift with slope-to-flat segments
  !!
  !! @param[in] process_opt Process option for choosing best model.
  !!   1 = used in split_and_merge_segments for model ID - SLR1 and all TPRs
  !!   2 = used on interim chgpts - all models
  !!   3 = used to estimate changepoint amplitude - SLR1 and TPR0 only
  !!   NOTE: for process_opt=3 min_slp_len == pha.adjust.min_length
  !!   otherwise min_slp_len == min_length (== 2)
  !! @param[in] x_data - serial input position (date) array
  !! @param[in] y_data - serial input data array (with missing)
  !! @param[in] seg1_end_index - end of the first segment
  !! @param[in] seg2_end_index - end of the second segment
  !! @param[out] crit_thresh_best 95% threshold of best model
  !! @param[out] test_stat Test statistic at changepoint
  !! @param[out] schwarz_min The lowest Schwarz statistic calculated for the model types
  !!             (the lower the value, the better the model fit)
  !! @param[out] best_offset Offset using best model
  !! @param[out] best_y_intercepts Intercept for each segment (2) of best model
  !! @param[out] best_slopes Slope for each segment (2) of best model
  !! @param[out] best_sse Sum square error of the best model
  !! @param[out] model_type Type of model
  !! @param[out] seg_values_count Number of obs in each segment (2)
  subroutine minbic(process_opt, x_data, y_data, seg1_end_index, seg2_end_index, crit_thresh_best, test_stat, schwarz_min,  &
                    best_offset, best_y_intercepts, best_slopes, best_sse, model_type, seg_values_count)
    use CommonVariables, only: por_months

    integer, intent(in) :: process_opt
    real, dimension(:), intent(in) :: x_data(por_months)
    real, dimension(:), intent(in) :: y_data(por_months)
    integer, intent(in) :: seg1_end_index
    integer, intent(in):: seg2_end_index
    real, intent(out) :: crit_thresh_best
    real, intent(out) :: test_stat
    real, intent(out) :: schwarz_min
    real, intent(out) :: best_offset
    real, dimension(:), intent(out) :: best_y_intercepts(2)
    real, dimension(:), intent(out) :: best_slopes(2)
    real, intent(out) :: best_sse
    integer, intent(out) :: model_type
    integer, dimension(:), intent(out) :: seg_values_count(2)

    real :: offset
    real :: f1n3
    real :: f2n4
    integer :: change_index
    integer :: seg2_valid_begin
    integer :: data_index
    integer :: min_slp_len
    integer :: min_length
    integer :: tot_obs
    integer :: seg1_count
    integer :: seg2_count
    integer :: num_values
    real :: schwarz_slope0
    real :: schwarz_slope1
    real :: schwarz
    real :: slope
    real :: slope1
    real :: slope2
    real :: slope_seg
    real :: sse_flat
    real :: sse_flat1
    real :: sse_flat2
    real :: sse_full
    real :: sse_reduced
    real :: sse_sloped
    real :: sse_sloped1
    real :: sse_sloped2
    real :: t_stat
    real :: crit_val
    real :: y1
    real :: y2
    real :: y_int
    real :: y_int1
    real :: y_int2
    real :: y_median
    real :: y_median1
    real :: y_median2
    logical :: is_straight_line
    logical :: did_error
    logical :: offset_gt10
    character(len=4) :: bic_type
    real, dimension(:) :: seg_y_intercepts(2)
    character(len=7) :: change_model
    character(len=256) :: log_message

    ! given mu + slp * xi = yi for any given segment, then the variables
    !   may be generalized such that:
    !   muq is the intecept for each segment
    !   best_slopes is the slope
    !   y_data is the yseged data series (that is: monotonically increasing
    !     WITH missing data as required)

    bic_type = get_property_chars(PROP_BIC_PENALTY)

    min_length = 2

    if(process_opt == 1 .OR. process_opt ==2) then
      min_slp_len = min_length
    else
      min_slp_len = get_property_int(PROP_ADJUST_MIN_LENGTH)
    endif

    offset_gt10 = .FALSE.

    call log_debug('process_opt (process option for choosing best model): '//log_string(process_opt))

    ! the last index is the end of the series
    num_values = seg2_end_index
    ! the chgpt index is the end of the first segment
    change_index = seg1_end_index
    do data_index = seg1_end_index + 1, seg2_end_index
      if(y_data(data_index) > MISSING_REAL + 1.0) exit
    enddo
    seg2_valid_begin = data_index

    ! run full series as one segment (straight line)
    best_offset = 0.0
    call kth_line(num_values, x_data, y_data, y_median, slope, y_int, sse_flat, sse_sloped, tot_obs)

    ! save the "reduced" SSE for the TPR models
    sse_reduced = sse_sloped
    schwarz_min = 9999.
    schwarz_slope0 = 9999.

    ! if process_opt == 2 then find the best of ANY model

    if(process_opt == 2) then
      ! The 2 straight line BIC models use the residual of the entire
      ! series as one segment - first the flat straight line
      ! use the kendall-theil method (without slope calc)
      change_model = 'KTHSLR0'
      if(tot_obs >= min_length) then
        schwarz_slope0 = bayes(tot_obs, sse_flat, 1, bic_type)
        schwarz_min = schwarz_slope0
        model_type = 1
        best_y_intercepts(1) = y_median
        best_y_intercepts(2) = y_median
        best_slopes(1) = 0.0
        best_slopes(2) = 0.0
        best_sse = sqrt(sse_flat/tot_obs)
        seg_values_count(1) = tot_obs
        seg_values_count(2) = 0
        test_stat = 0.0
        crit_thresh_best = 99.0
      else
        call log_debug(change_model//' - Unable to fit model - skipping')
      endif
    endif

    if (process_opt == 1 .OR. process_opt == 2) then
      ! for all other process_opt values, if TPR0 include the SLR1 test
      ! the sloped line (also gives back the Sum Square Error for 0 chngpts)
      ! use the kendall-theil method (with slope calc)
      change_model = 'KTHSLR1'
      if(tot_obs >= min_slp_len) then
        ! Use tot_obs for non-missing obs instead of num_values
        schwarz_slope1 = bayes(tot_obs, sse_reduced, 2, bic_type)

        ! following line replaced to reposition the t-test AFTER all BICs
        if(schwarz_slope1 < schwarz_min) then
          schwarz_min = schwarz_slope1
          model_type = 2
          best_y_intercepts(1) = y_int
          best_y_intercepts(2) = best_y_intercepts(1)
          best_slopes(1) = slope
          best_slopes(2) = best_slopes(1)
          best_sse = sqrt(sse_reduced/tot_obs)
          seg_values_count(1) = tot_obs
          seg_values_count(2) = 0
          test_stat = 0.0
          crit_thresh_best = 99.0
        endif
      else
        call log_debug(change_model//' - Unable to fit model - skipping')
      endif

    elseif(process_opt == 3) then
      model_type = 2
      best_y_intercepts(1) = y_int
      best_y_intercepts(2) = y_int
      best_slopes(1) = slope
      best_slopes(2) = slope
      best_sse = sqrt(sse_reduced/tot_obs)
      seg_values_count(1) = tot_obs
      seg_values_count(2) = 0
      test_stat = 0.0
      crit_thresh_best = 99.0
    endif

    ! for the TPR models - the series is divided into two segments
    !   from 1 to chgpt and from chgpt+1 to num_values - for all the models
    !   EXCEPT for model 4 (TPR1 = constant slope with one offset).

    ! first segment (starting at 1 for change_index)
    call kth_line(change_index, x_data(1:change_index), y_data(1:change_index), y_median1, slope1, y_int1, sse_flat1,  &
                  sse_sloped1, seg1_count)
    ! second segment (starting at change_index+1 for num_values-change_index)
    call kth_line(num_values-change_index, x_data(change_index+1:num_values), y_data(change_index+1:num_values), y_median2,  &
                  slope2, y_int2, sse_flat2, sse_sloped2, seg2_count)

    ! The BIC is the final arbiteur in deciding the data model of a given changepoint.
    ! However, the variance test (ttest) determines the significance. Therefore, AFTER
    ! the BIC decides the data model, the variance test for the chosen model keeps or
    ! drops the changepoint appropriately. (We hope the Kendall-Theil segment fit reduces
    ! the impact of outliers)

    ! Use the kendall-theil method (with 0 sloped segments)
    change_model = 'KTHTPR0'
    if(seg1_count >= min_length .AND. seg2_count >= min_length) then
      t_stat = calculate_ttest(y_data, seg1_end_index, seg2_end_index, min_length)
      crit_val = get_critval(tot_obs-2, 3)
      sse_full = sse_flat1 + sse_flat2
      schwarz = bayes(tot_obs, sse_full, 3, bic_type)
      offset = y_median1 - y_median2

      if(offset > 10. .OR. offset < -10.) then
        write(log_message,'(a,3f10.2,2(" -------"),3f8.2,2i5)') change_model, schwarz, y_median1, y_median2, t_stat, crit_val,  &
                   offset, seg1_count, seg2_count
        call log_info(log_message)
        if(offset > 10.) offset_gt10 = .TRUE.
      endif

      if(schwarz < schwarz_min) then
        schwarz_min = schwarz
        model_type = 3
        best_y_intercepts(1) = y_median1
        best_y_intercepts(2) = y_median2
        best_slopes(1) = 0.0
        best_slopes(2) = 0.0
        best_sse = sqrt(sse_full / (seg1_count + seg2_count))
        seg_values_count(1) = seg1_count
        seg_values_count(2) = seg2_count
        test_stat = t_stat
        crit_thresh_best = crit_val
        best_offset = offset
      endif
    endif

    ! for process_opt = 3 return only TPR0 solution
    if (process_opt == 1 .OR. process_opt == 2) then

      if(seg1_count + seg2_count >= min_slp_len) then
        change_model = 'KTHTPR1'
        call kth_tpr1(num_values, y_data, change_index, seg_y_intercepts, slope_seg, sse_full, seg1_count, seg2_count, did_error)
        if(.not. did_error) then
          tot_obs = seg1_count + seg2_count
          F1n3 = ((sse_reduced - sse_full)/1.)/(sse_full/(tot_obs-3))
          crit_val = get_critval(tot_obs-3, 4)
          schwarz = bayes(tot_obs, sse_full, 4, bic_type)
          y1 = seg_y_intercepts(1) + slope_seg * x_data(change_index)
          y2 = seg_y_intercepts(2) + slope_seg * x_data(seg2_valid_begin)
          offset = y1 - y2
          if(offset > 10. .OR. offset < -10.) then
            write(log_message,'(a,3f10.2,f8.3," -------",3f8.2,2i5)') change_model, schwarz, seg_y_intercepts, slope_seg, F1n3,  &
                      crit_val, offset, seg1_count, seg2_count
            call log_info(log_message)
            if(offset > 10.) offset_gt10 = .TRUE.
          endif
          if(schwarz < schwarz_min) then
            schwarz_min = schwarz
            model_type = 4
            best_y_intercepts(1) = seg_y_intercepts(1)
            best_y_intercepts(2) = seg_y_intercepts(2)
            best_slopes(1) = slope_seg
            best_slopes(2) = best_slopes(1)
            best_sse = sqrt(sse_full/tot_obs)
            seg_values_count(1) = seg1_count
            seg_values_count(2) = seg2_count
            test_stat = F1n3
            crit_thresh_best = crit_val
            best_offset = offset
          endif
        endif
      endif

      change_model = 'KTHTPR2'
      if(seg1_count >= min_slp_len .AND. seg2_count >= min_slp_len) then
        y1 = y_int1 + slope1 * x_data(change_index)
        y2 = y_int2 + slope2 * x_data(seg2_valid_begin)
        offset = y1 - y2
        tot_obs = seg1_count + seg2_count
        sse_full = sse_sloped1 + sse_sloped2
        F2n4 = ((sse_reduced - sse_full)/2.)/(sse_full/(tot_obs-4))
        crit_val = get_critval(tot_obs-4, 5)
        schwarz = bayes(tot_obs, sse_full, 5, bic_type)
        if(offset > 10. .OR. offset < -10.) then
          write(log_message,'(a,3f10.2,2f8.3,3f8.2,2i5)')  &
               change_model, schwarz, y_int1, y_int2, slope1, slope2, F2n4, crit_val, offset, seg1_count, seg2_count
          call log_info(log_message)
          if(offset > 10.) offset_gt10 = .TRUE.
        endif
        if(schwarz < schwarz_min) then
          schwarz_min = schwarz
          model_type = 5
          best_y_intercepts(1) = y_int1
          best_y_intercepts(2) = y_int2
          best_slopes(1) = slope1
          best_slopes(2) = slope2
          best_sse = sqrt(sse_full/tot_obs)
          seg_values_count(1) = seg1_count
          seg_values_count(2) = seg2_count
          test_stat = F2n4
          crit_thresh_best = crit_val
          best_offset = offset

        endif
      endif

      change_model = 'KTHTPR3'
      if(seg1_count >= min_length .AND. seg2_count >= min_slp_len) then
        y1 = y_median1
        y2 = y_int2 + slope2 * x_data(seg2_valid_begin)
        offset = y1 - y2
        tot_obs = seg1_count + seg2_count
        sse_full = sse_flat1 + sse_sloped2
        F1n3 = ((sse_reduced - sse_full)/1.)/(sse_full/(tot_obs-3))
        crit_val = get_critval(tot_obs, 4)
        schwarz = bayes(tot_obs, sse_full, 4, bic_type)
        if(offset > 10. .OR. offset < -10.) then
          write(log_message,'(a,3f10.2," -------",f8.3,3f8.2,2i5)') &
               change_model, schwarz, y_median1, y_int2, slope2, F1n3, crit_val, offset, seg1_count, seg2_count
          call log_info(log_message)
          if(offset > 10.) offset_gt10 = .TRUE.
        endif
        if(schwarz < schwarz_min) then
          schwarz_min = schwarz
          model_type = 6
          best_y_intercepts(1) = y_median1
          best_y_intercepts(2) = y_int2
          best_slopes(1) = 0.0
          best_slopes(2) = slope2
          best_sse = sqrt(sse_full/tot_obs)
          seg_values_count(1) = seg1_count
          seg_values_count(2) = seg2_count
          test_stat = F1n3
          crit_thresh_best = crit_val
          best_offset = offset

        endif
      endif

      ! step change with sloped-to-flat segments
      change_model = 'KTHTPR4'
      if(seg1_count >= min_slp_len .AND. seg2_count >= min_length) then
        y1 = y_int1 + slope1 * x_data(change_index)
        y2 = y_median2
        offset = y1 - y2
        tot_obs = seg1_count + seg2_count
        sse_full = sse_sloped1 + sse_flat2
        F1n3 = ((sse_reduced - sse_full)/1.)/(sse_full/(tot_obs-3))
        crit_val = get_critval(tot_obs, 4)
        schwarz = bayes(tot_obs, sse_full, 4, bic_type)
        if(offset > 10. .OR. offset < -10.) then
          write(log_message,'(a,3f10.2,f8.3," -------",3f8.2,2i5)') &
               change_model, schwarz, y_int1, y_median2, slope1, F1n3, crit_val, offset, seg1_count, seg2_count
          call log_info(log_message)
          if(offset > 10.) offset_gt10 = .TRUE.
        endif
        if(schwarz < schwarz_min) then
          schwarz_min = schwarz
          model_type = 7
          best_y_intercepts(1) = y_int1
          best_y_intercepts(2) = y_median2
          best_slopes(1) = slope1
          best_slopes(2) = 0.0
          best_sse = sqrt(sse_full/tot_obs)
          seg_values_count(1) = seg1_count
          seg_values_count(2) = seg2_count
          test_stat = F1n3
          crit_thresh_best = crit_val
          best_offset = offset
        endif
      endif
    endif ! end of process_opt == 1 .OR process_opt == 2  block

    ! calculate the offset for the best model
    ! if is_straight_line is true, then use the min SLR model
    ! else use the incoming model and calc offset
    is_straight_line = .false.

    if(model_type < 3) then
      ! if model_type == 1 or 2, there is NO MOVE!
      is_straight_line = .true.
    else if(process_opt == 1 .OR. process_opt == 3) then
      ! send back the TPR0 fit for any NON-SLR best model
      best_y_intercepts(1) = y_median1
      best_y_intercepts(2) = y_median2
      best_slopes(1) = 0.0
      best_slopes(2) = 0.0
    else
      ! used for model vs stat test comparison and interim chgpt tests
      ! check the critical variance threshold (t- or f-test) of proper model
      if(test_stat < crit_thresh_best) then
        is_straight_line = .true.
      endif
    endif
    if(is_straight_line) then
      ! either already a straight line or failed the process_opt defined test
      ! use the best SLR model
      best_offset = 0.0
      seg_values_count(1) = tot_obs
      seg_values_count(2) = 0
      test_stat = 0.0
      crit_thresh_best = 99.0
      if(schwarz_slope0 < schwarz_slope1) then
        schwarz_min = schwarz_slope0
        model_type = 1
        best_y_intercepts(1) = y_median
        best_y_intercepts(2) = y_median
        best_slopes(1) = 0.0
        best_slopes(2) = 0.0
        best_sse = sqrt(sse_flat/tot_obs)
      else
        schwarz_min = schwarz_slope1
        model_type = 2
        best_y_intercepts(1) = y_int
        best_y_intercepts(2) = best_y_intercepts(1)
        best_slopes(1) = slope
        best_slopes(2) = best_slopes(1)
        best_sse = sqrt(sse_reduced/tot_obs)
      endif
    else
      ! did not fail whichever test for the process_opt input -
      ! keep incoming model and calculate offset
      y1 = best_y_intercepts(1) + best_slopes(1) * x_data(change_index)
      y2 = best_y_intercepts(2) + best_slopes(2) * x_data(seg2_valid_begin)
      best_offset = y1 - y2
    endif

    ! redefine best_slopes to the kth_line solution no matter what MINBIC model
    best_slopes(1) = slope1
    best_slopes(2) = slope2

    ! DEBUG - if any model generated estimated changepoint > 10
    if(offset_gt10) then
      write(log_message,'("BEST:",i2,f10.2,2f10.2,2f8.3,3f8.2,  2i5)') model_type, schwarz_min, best_y_intercepts,  &
               best_slopes, test_stat, crit_thresh_best, best_offset, seg_values_count
      call log_info(log_message)
    endif

  end subroutine minbic

  !> Calculates the Kendall Theil linear regression line given an array of
  !!   dependent (y_data) assumed to have two same-sloped segments with a discontinuity.
  !!   First a single median slope is determined for both segments and then
  !!   the y intercept is determined for each segment using the median slope,
  !!   median x and median y values. This subroutine is only used for model 4 (TPR1).
  !!
  !! @param[in] num_values Number of values
  !! @param[in] y_data dependent data (predictand)
  !! @param[in] change_index last position in first segment
  !! @param[out] seg_y_intercepts y-intercept of regression line for each segment
  !! @param[out] slope Slopes of each segment
  !! @param[out] sse_tot Sum standard error of residual series
  !! @param[out] seg1_count Number of values in first segment
  !! @param[out] seg2_count Number of values in second segment
  !! @param[out] did_error True if kendall_theil returned an error. Otherwise, false.
  subroutine kth_tpr1(num_values, y_data, change_index, seg_y_intercepts, slope, sse_tot, seg1_count, seg2_count, did_error)

    integer, intent(in) :: num_values
    real, dimension(:), intent(in) :: y_data
    integer, intent(in) :: change_index
    real, dimension(:), intent(out) :: seg_y_intercepts(2)
    real, intent(out) :: slope
    real, intent(out) :: sse_tot
    integer, intent(out) :: seg1_count
    integer, intent(out) :: seg2_count
    logical, intent(out) :: did_error

    integer :: num_value1
    integer :: num_value2
    integer :: data_index
    integer :: tot_obs
    real, dimension(:) :: x_data_comp(size(y_data))
    real, dimension(:) :: y_data_comp(size(y_data))
    real, dimension(:) :: residual(num_values)
    real, dimension(:) :: sse_seg(2)
          
    did_error = .false.
          
    ! calculate the residuals for the "full model" with constant slope
    num_value1 = 0
    num_value2 = 0
    tot_obs = 0
    slope = 0.0
    seg_y_intercepts(1) = 0.0
    seg_y_intercepts(2) = 0.0
    sse_tot = 0.0
    sse_seg(1) = 0
    sse_seg(2) = 0
    y_data_comp(:) = MISSING_REAL
    x_data_comp(:) = MISSING_REAL

    do data_index = 1, num_values
      residual(data_index) = MISSING_REAL
    enddo

    ! Create array of non-missing y values for both segments
    do data_index = 1, change_index
      if(y_data(data_index) > MISSING_REAL+1) then
        num_value1 = num_value1 + 1
        y_data_comp(num_value1) = y_data(data_index)
        x_data_comp(num_value1) = data_index
      endif
    enddo
    do data_index = change_index+1, num_values
      if(y_data(data_index) > MISSING_REAL+1) then
        num_value2 = num_value2 + 1
        y_data_comp(num_value1 + num_value2) = y_data(data_index)
        x_data_comp(num_value1 + num_value2) = data_index
      endif
    enddo

    tot_obs = num_value1 + num_value2

    ! Use kendall-theil method with internal breakpoint and single slope
    call kendall_theil(tot_obs, x_data_comp, y_data_comp, seg_y_intercepts, slope, num_value1+1, did_error)
    if(did_error) return

    ! Calculate the residuals for each segment and add them
    do data_index = 1, change_index
      if(y_data(data_index) > MISSING_REAL+1) then
        residual(data_index) = y_data(data_index) - seg_y_intercepts(1) - slope*data_index
        sse_seg(1) = sse_seg(1) + residual(data_index)**2
      endif
    enddo
    do data_index = change_index+1, num_values
      if(y_data(data_index) > MISSING_REAL+1) then
        residual(data_index) = y_data(data_index) - seg_y_intercepts(2) - slope*data_index
        sse_seg(2) = sse_seg(2) + residual(data_index)**2
      endif
    enddo
    sse_tot = sse_seg(1) + sse_seg(2)

    seg1_count = num_value1
    seg2_count = num_value2

  end subroutine kth_tpr1

  !> Gets the SNHT significance threshold critical value for a normal distribution curve.
  !!   See req. 1.3.2.
  !!
  !! @param num_values The number of values in the curve.
  !! @param model_type The general shape of the curve (M1 - M5 in the requirements/paper).
  !! @return The "critical value" to use as the threshold for whether or not this is a
  !!   significant change point.
  function get_critval(num_values, model_type) result(critical_val)

    integer :: num_values
    integer :: model_type
    real :: critical_val

    integer :: len_seg
    integer :: value_index
    integer :: num_seg
    real :: value1
    real :: value2

    ! this set is at 95% confidence (two-tailed)
    real, dimension(34), parameter :: tvalue = (/ 12.71, 4.30, 3.18, 2.78, 2.57, 2.45, &
          2.37,2.31, 2.26, 2.23, 2.20, 2.18, 2.16, 2.15, 2.13, 2.12, 2.11, 2.10, 2.09, &
          2.09, 2.08, 2.07, 2.07, 2.06, 2.06, 2.06, 2.05, 2.05, 2.05, 2.04, 2.02, 2.00, 1.98, 1.96 /)
    ! this is the F1 column
    real, dimension(34), parameter :: f1value = (/ 161.4, 18.50, 10.1,  7.7,  6.6,  6.0, &
         5.6, 5.3,  5.1,  5.0, 4.8,  4.7,  4.7,  4.6,  4.5,  4.5,  4.5, 4.4,  4.4,  4.4, &
         4.3,  4.3, 4.3,  4.3,  4.2,  4.2,  4.2, 4.2,  4.2,  4.2,  4.1,  4.0,  3.9,  3.8 /)
    ! this is the F2 column
    real, dimension(34), parameter :: f2value = (/ 199.5, 19.00, 9.55, 6.94, 5.79, 5.14, &
         4.74, 4.46, 4.26, 4.10, 3.98, 3.89, 3.81, 3.74, 3.68, 3.63, 3.59, 3.55, 3.52, &
         3.49, 3.47, 3.44, 3.42, 3.40, 3.39, 3.37, 3.35, 3.34, 3.33, 3.32, 3.23, 3.15, 3.07, 3.00 /)

    ! threshold lengths for t & f values
    integer, dimension(34), parameter :: length = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, &
            13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 40, 60, 120, 9999 /)

    do value_index = 33,1,-1
      if(num_values >= length(value_index)) exit
    end do

    ! for model_type = 3 (TPR0) return the t-test stat
    if(model_type == 3) then
      value1 = tvalue(value_index)
      value2 = tvalue(value_index+1)
    else if(model_type == 4) then
      ! for the model_type = 4 (TPR1 & TPR3 & TPR4) return the F1 Stat
      value1 = f1value(value_index)
      value2 = f1value(value_index+1)
    else if(model_type == 5) then
      ! for the model_type = 5 (TPR2) return the F2 stat
      value1 = f2value(value_index)
      value2 = f2value(value_index+1)
    else
      call log_fatal('ModelFitUtils::get_critval: Unknown model_type in alpha')
      stop 1
    endif

    ! one-to-one correspondense at low value_index
    if(value_index < 30) then
      critical_val = value1
    else
      ! use ratio for larger value_index
      len_seg = length(value_index+1) - length(value_index)
      num_seg = num_values - length(value_index)
      critical_val = ((value2-value1) / len_seg) * num_seg + value1
    endif
     
  end function get_critval

  !> Calculates the Schwarz Bayesian Information Criterian statistic for a series and
  !!   its model fit.  The calculation is based on Schwarz (1978), Ann. Stat., 6, 461-464
  !!   and Seidel and Lanzante(2004), JGR-Atmos, 109, D14108.
  !!
  !! @param num_values Number of non-missing values of y_data.
  !! @param res_sum_sqr Sum square of residuals of model fit to y_data.
  !! @param num_free_params The number of free parameters used for model fitting.
  !! @param bic_type The run option for the type of Bayesian Information Criterian to use (bic, aic or none).
  !! @return schwarz Schwarz statistic (the lower the value, the better the model fit).
  function bayes(num_values, res_sum_sqr, num_free_params, bic_type) result(schwarz)

    integer num_values
    real :: res_sum_sqr
    integer :: num_free_params
    character(len=*) :: bic_type
    real :: schwarz

    real :: schwarz_1
    real :: schwarz_2

    ! The virtually impossible case of having res_sum_sqr of 0
    ! (which happens if both slopes are exactly 100% flat) causes an
    ! error when trying to take the log of 0, so change to tiny number.
    if(res_sum_sqr == 0) res_sum_sqr = FP_EPSILON

    if(trim(bic_type) .eq. OPT_PENALTY_BIC) then
      ! log10 - Schwarz
      schwarz_1 = num_values * log10(res_sum_sqr/real(num_values))
      schwarz_2 = num_free_params * log10(real(num_values))
    else if(trim(bic_type) .eq. OPT_PENALTY_AIC) then
      ! natural log - Akaika (does not penalize as heavily)
      schwarz_1 = num_values * log(res_sum_sqr/real(num_values))
      schwarz_2 = 2 * num_free_params
    else if(trim(bic_type) .eq. OPT_PENALTY_NONE) then
      schwarz_1 = num_values * log10(res_sum_sqr/real(num_values))
      schwarz_2 = 0.0
    else
      call log_fatal('ModelFitUtils::bayes: Invalid Bayesian Penalty function option')
      stop 1
    endif
    schwarz = schwarz_1 + schwarz_2
        
  end function bayes

  !> Calculate student's t-test, from Statistical Analysis in Climate Research.
  !!
  !! @param diff_array Array of realizations (may contain missing).
  !! @param end1_index End of the first realization, breakpoint between populations.
  !! @param end2_index End of the second realization.
  !! @param min_length Minimum number of non-missing values needed to calculate t_test.
  !! @return t_test T-test statistic.
  function calculate_ttest(diff_array, end1_index, end2_index, min_length) result(t_test)

    real, dimension(:) :: diff_array
    integer :: end1_index
    integer :: end2_index
    integer :: min_length
    real :: t_test

    real :: xsq1
    real :: xsq2
    real :: xsq_total
    real :: x_num1
    real :: x_num2
    real :: x_mean1
    real :: x_mean2
    integer :: data_index

    ! Calculate mean and residuals for the first population
    xsq1 = 0.0
    x_num1 = 0.0
    x_mean1 = 0.0
    do data_index = 1, end1_index
      if(diff_array(data_index) > MISSING_REAL + 1.) then
        x_mean1 = x_mean1 + diff_array(data_index)
        x_num1 = x_num1 + 1.0
      endif
    enddo
          
    if (x_num1 >= min_length) then
      x_mean1 = x_mean1 / x_num1
    else
      t_test = 0.0
      return
    end if

    do data_index = 1, end1_index
      if(diff_array(data_index) > MISSING_REAL + 1.) then
        xsq1 = xsq1 + (diff_array(data_index) - x_mean1)**2.
      endif
    enddo
    xsq1 = xsq1 / (x_num1-1.0) / x_num1
            
    ! Calculate mean and residuals for the second population
    x_num2 = 0.0
    xsq2 = 0.0
    x_mean2 = 0.0
    do data_index = end1_index+1, end2_index
      if(diff_array(data_index) > MISSING_REAL + 1.) then
        x_mean2 = x_mean2 + diff_array(data_index)
        x_num2 = x_num2 + 1.0
      endif
    enddo
          
    if (x_num2 >= min_length) then
      x_mean2 = x_mean2 / x_num2
    else
      t_test=0.0
      return
    end if
          
    do data_index = end1_index+1, end2_index
      if(diff_array(data_index) > MISSING_REAL + 1.) then
        xsq2 = xsq2 + (diff_array(data_index) - x_mean2)**2.
      endif
    enddo
    xsq2 = xsq2 / (x_num2-1.0) / x_num2
          
    ! Calculate t-test
    xsq_total = xsq1 + xsq2
    if(xsq_total < FP_EPSILON) xsq_total = FP_EPSILON
    t_test = abs(x_mean1-x_mean2) / sqrt(xsq_total)

  end function calculate_ttest

  !> Calculates the kendall_theil median slope (same for both segments) and y intercept
  !!   of both segments for model 4 (TPR1).
  !!
  !! @param[in] num_values Sample size.
  !! @param[in] x_data
  !! @param[in] y_data Dependent data (predictand).
  !! @param[out] seg_y_intercepts y-intercept of regression line for both segments.
  !! @param[out] slope_median Slope of linear regression line.
  !! @param[in] seg2_index The index correspnding to the start of the second segment.
  !! @param[out] did_error Algorithm unable to compute regression.
  subroutine kendall_theil(num_values, x_data, y_data, seg_y_intercepts, slope_median, seg2_index, did_error)

    integer, intent(in) :: num_values
    real, dimension(:), intent(in) :: y_data
    real, dimension(:), intent(in) :: x_data
    real, intent(out) :: seg_y_intercepts(2)
    real, intent(out) :: slope_median
    integer, intent(in) :: seg2_index
    logical, intent(out) :: did_error

    integer :: point1
    integer :: begin2_index
    integer :: end1_index
    integer :: index_median
    integer :: index_slope
    integer :: sum_bins
    integer :: point2
    integer :: lastgood
    integer :: num_slope
    integer :: num_values2
    integer :: num_comp
    integer, parameter :: max_slope_bin = 1000
    integer, parameter :: min_slope_bin = -1000
    real :: slope_local
    real :: slope_median_diff
    real :: half_slope_inc
    real :: slope_median_index
    real :: slopes(60*60)
    real, dimension(:) :: x_comp(size(y_data))
    real, dimension(:) :: y_comp(size(y_data))
    real, parameter :: slope_inc = .0002
    integer :: slope_bin(min_slope_bin:max_slope_bin)

    did_error = .false.
    num_comp = 0
    lastgood = 0
    end1_index = 0
    begin2_index = 0
    half_slope_inc = slope_inc * 0.5

    ! Create x and y arrays for non-missing data
    do point1 = 1, num_values
      ! .ne. test is used here to match original code. Not a good test for reals.
      if(y_data(point1) .ne. MISSING_REAL) then
        num_comp = num_comp + 1
        x_comp(num_comp) = x_data(point1)
        y_comp(num_comp) = y_data(point1)
        ! Define the end of the first segment and the beginning of the second segment
        if(begin2_index == 0 .AND. seg2_index <= point1)then
          end1_index = lastgood
          begin2_index = num_comp
        endif
      endif
      lastgood = num_comp
    enddo

    ! If not set or slope = 0 then set end of first segment
    if(end1_index == 0) end1_index = num_comp

    ! If not enough data return an error
    if(num_comp < 5) then
      did_error = .true.
      return
    endif

    ! If few enough to sort ascending
    if(num_comp < 60) then
      num_slope = 0
      ! For first segment, calculate slopes for all pairs of x,y points above a certain of months wide
      do point1 = 1, end1_index - 1
        do point2 = point1+1, end1_index
          if(abs(x_comp(point2) - x_comp(point1)) >= FP_EPSILON) then
            num_slope = num_slope + 1
            slopes(num_slope) = (y_comp(point2)-y_comp(point1))/(x_comp(point2)-x_comp(point1))
          endif
        enddo
      enddo
      ! For second segment continue to calculate slopes
      do point1 = begin2_index, num_comp - 1
        do point2 = point1+1, num_comp
          if(abs(x_comp(point2) - x_comp(point1)) >= FP_EPSILON) then
            num_slope = num_slope + 1
            slopes(num_slope) = (y_comp(point2)-y_comp(point1))/(x_comp(point2)-x_comp(point1))
          endif
        enddo
      enddo

      ! Sort all the slopes of combined segments
      call array_sort(slopes, num_slope)
      ! Determine median slope of combined segments
      index_median = num_slope/2 + mod(num_slope,2)

      if(mod(num_slope,2) == 1) then
        slope_median = slopes(index_median)
      else
        slope_median = (slopes(index_median) + slopes(index_median+1)) / 2.0
      end if

    else
      ! If too many values to sort, populate slope bins that are slope_inc (slope increment) wide
      slope_bin(:) = 0

      num_slope = 0
      ! Start with first segment
      do point1 = 1, end1_index - 1
        do point2 = point1+1, end1_index
          if(abs(x_comp(point2) - x_comp(point1)) >= FP_EPSILON) then
            num_slope = num_slope + 1
            slope_local = (y_comp(point2)-y_comp(point1))/(x_comp(point2)-x_comp(point1))
            if(slope_local > -1*half_slope_inc) then
              index_slope = (slope_local + half_slope_inc)/slope_inc
              if(index_slope > max_slope_bin) index_slope = max_slope_bin
            else
              index_slope = (slope_local - half_slope_inc)/slope_inc
              if(index_slope < min_slope_bin) index_slope = min_slope_bin
            endif
            slope_bin(index_slope) = slope_bin(index_slope) + 1
          endif
        enddo
      enddo
      ! Continue with second segment
      do point1 = begin2_index, num_comp - 1
        do point2 = point1+1, num_comp
          if(abs(x_comp(point2) - x_comp(point1)) >= FP_EPSILON) then
            num_slope = num_slope + 1
            slope_local = (y_comp(point2)-y_comp(point1))/(x_comp(point2)-x_comp(point1))
            if(slope_local > -1*half_slope_inc) then
              index_slope = (slope_local + half_slope_inc)/slope_inc
              if(index_slope > max_slope_bin) index_slope = max_slope_bin
            else
              index_slope = (slope_local - half_slope_inc)/slope_inc
              if(index_slope < min_slope_bin) index_slope = min_slope_bin
            endif
            slope_bin(index_slope) = slope_bin(index_slope) + 1
          endif
        enddo
      enddo

      ! Add slope bins until number exceeds median number of total slopes
      index_median = (num_slope + mod(num_slope,2))/2
      sum_bins = 0
      do index_slope = min_slope_bin, max_slope_bin
        sum_bins = sum_bins + slope_bin(index_slope)
        if(index_median <= sum_bins) exit
      enddo

      ! Median slope = bin slope of index_slope - slope of index_slope over median
      slope_median_diff = real(sum_bins-index_median)/real(slope_bin(index_slope))*slope_inc
      slope_median_index = real(index_slope)*slope_inc + half_slope_inc
      slope_median = slope_median_index - slope_median_diff
    endif

    seg_y_intercepts(1) = calculate_y_intercept(x_comp, y_comp, end1_index, slope_median)

    ! Calculate y-intercept of second segment (if it exists)
    if(begin2_index < num_comp) then
      num_values2 = num_comp - begin2_index + 1
      do point1 = begin2_index, num_comp
        x_comp(point1 - begin2_index + 1) = x_comp(point1)
        y_comp(point1 - begin2_index + 1) = y_comp(point1)
      enddo
      seg_y_intercepts(2) = calculate_y_intercept(x_comp, y_comp, num_values2, slope_median)
    endif

  end subroutine kendall_theil

  !> Calculates the y intercept of a regression line for an array of values. Used only
  !!   for model 4 (TPR1).
  !!
  !! @param x_comp The x values (missing removed) used to find the median x value.
  !! @param y_comp The y values (missing removed) used to find the median y value.
  !! @param num_values The number of values in the x and y arrays.
  !! @param slope_median The median slope of the data.
  !! @result y_intercept The y intercept computed from the x and y medians and the median slope.
  function calculate_y_intercept(x_comp, y_comp, num_values, slope_median) result(y_intercept)
    real, dimension(:) :: x_comp
    real, dimension(:) :: y_comp
    integer :: num_values
    real :: y_intercept

    integer :: index_median
    real :: x_median
    real :: y_median
    real :: slope_median

    ! Calculate median index of first segment
    index_median = num_values/2 + mod(num_values, 2)
    ! Calculate y-intercept from median x,y values of first segment
    x_median = x_comp(index_median)
    call array_sort(y_comp, num_values)
    y_median = y_comp(index_median)
    ! TODO: For an even number of values in the x_comp/y_comp arrays, the median values
    !   should be computed as the average of the two central values as they are in kth_line.
    y_intercept = y_median - slope_median * x_median

  end function calculate_y_intercept

  !> Calculates the Kendall Theil linear regression line given an array of
  !!   dependent (y_data) and independent (x_data) data values. First the median
  !!   slope is determined and then the y intercept is determined from the median slope,
  !!   median x and median y values.
  !! @param[in] num_in Number input values including missing.
  !! @param[in] x_data Independent data (predictor).
  !! @param[in] y_data Dependent data (predictand).
  !! @param[out] y_median Median of y data.
  !! @param[out] slope_median Slope of linear regression line.
  !! @param[out] y_int Y-intercept of regression line.
  !! @param[out] sse_flat Sum of squares of errors (residuals) from a flat (horizontal) line.
  !! @param[out] sse_slope Sum of squares of errors (residuals) from a sloped line.
  !! @param[out] num_values Number of of non-missing paired (x and y) coordinates.
  subroutine kth_line(num_in, x_data, y_data, y_median, slope_median, y_int, sse_flat, sse_slope, num_values)

    integer, intent(in) :: num_in
    real, dimension(:), intent(in) :: x_data(num_in)
    real, dimension(:), intent(in) :: y_data(num_in)
    real, intent(out) :: y_median
    real, intent(out) :: slope_median
    real, intent(out) :: y_int
    real, intent(out) :: sse_flat
    real, intent(out) :: sse_slope
    integer, intent(out) :: num_values

    real :: half_slope_inc
    integer :: point1
    integer :: point2
    integer :: index_median
    integer :: index_slope
    integer :: sum_bin
    real :: resid
    real :: slope_comp
    real :: slope_median_diff
    real :: slope_median_index

    integer, parameter :: min_slope_bin=-1000
    integer, parameter :: max_slope_bin=1000
    real, parameter :: slope_inc=.0002
    integer :: num_slope
    real :: x_median
    real, dimension(:) :: x_comp(num_in)
    real, dimension(:) :: y_comp(num_in)
    real, dimension(:) :: slope_array(60*60)
    integer, dimension(:,:) :: slope_bin(min_slope_bin:max_slope_bin)

    num_values = 0
    x_median = MISSING_REAL
    y_median = MISSING_REAL
    slope_median = MISSING_REAL
    y_int = MISSING_REAL
    sse_flat = 0.0
    sse_slope = 0.0
    half_slope_inc = slope_inc * 0.5

    ! Create x and y arrays for non-missing data
    do point1= 1, num_in
      if ((x_data(point1) > MISSING_REAL + 1) .AND. (y_data(point1) > MISSING_REAL + 1)) then
        num_values = num_values + 1
        x_comp(num_values) = x_data(point1)
        y_comp(num_values) = y_data(point1)
      endif
    enddo
    if(num_values < 2) return

    ! Calculate slopes for all pairs of x,y points with x diff > eps
    if(num_values < 60) then
      num_slope = 0
      do point1 = 1, num_values-1
        do point2 = point1+1, num_values
          if(abs(x_comp(point2) - x_comp(point1)) >= FP_EPSILON) then
            num_slope = num_slope + 1
            slope_array(num_slope) = (y_comp(point2)-y_comp(point1))/(x_comp(point2)-x_comp(point1))
          endif
        enddo
      enddo

      ! Sort the slopes
      call array_sort(slope_array, num_slope)

      ! Calculate median index and its slope value
      index_median = (num_slope + mod(num_slope,2))/2
      if(mod(num_slope,2) == 1) then
        slope_median = slope_array(index_median)
      else
        slope_median = (slope_array(index_median) + slope_array(index_median+1))/2
      endif

    else

      ! If too many values to sort, populate slope bins that are slope_inc (slope increment) wide
      do index_slope = min_slope_bin,max_slope_bin
        slope_bin(index_slope) = 0
      enddo
      num_slope = 0
      do point1 = 1, num_values-1
        do point2 = point1+1, num_values
          if(abs(x_comp(point2) - x_comp(point1)) >= FP_EPSILON) then
            num_slope = num_slope + 1
            slope_comp = (y_comp(point2)-y_comp(point1))/(x_comp(point2)-x_comp(point1))
            if(slope_comp > -1*half_slope_inc) then
              index_slope = (slope_comp + half_slope_inc)/slope_inc
              if(index_slope > max_slope_bin) index_slope = max_slope_bin
            else
              index_slope = (slope_comp - half_slope_inc)/slope_inc
              if(index_slope < min_slope_bin) index_slope = min_slope_bin
            endif
            slope_bin(index_slope) = slope_bin(index_slope) + 1
          endif
        enddo
      enddo

      ! Add slope bins until number exceeds median number of total slopes
      index_median = (num_slope + mod(num_slope,2))/2
      sum_bin = 0
      do index_slope = min_slope_bin, max_slope_bin
        sum_bin = sum_bin + slope_bin(index_slope)
        if(index_median <= sum_bin) exit
      enddo

      ! Median slope = bin slope of ind - slope of ind over median
      slope_median_diff = real(sum_bin-index_median)/real(slope_bin(index_slope))*slope_inc
      slope_median_index = real(index_slope)*slope_inc + half_slope_inc
      slope_median = slope_median_index - slope_median_diff
              
    endif

    ! Calculate y-intercept from median x,y values
    call array_sort(x_comp, num_values)
    call array_sort(y_comp, num_values)
    index_median = (num_values + mod(num_values,2)) / 2
    if(mod(num_values,2) == 1) then
      x_median = x_comp(index_median)
      y_median = y_comp(index_median)
    else
      x_median = (x_comp(index_median) + x_comp(index_median+1)) / 2
      y_median = (y_comp(index_median) + y_comp(index_median+1)) / 2
    endif
    y_int = y_median - slope_median * x_median

    ! Calculate residuals from flat and sloped lines
    do point1=1, num_in
      if ((x_data(point1) > MISSING_REAL + 1) .AND. (y_data(point1) > MISSING_REAL + 1)) then
        sse_flat = sse_flat + (y_median - y_data(point1)) ** 2
        resid = (y_int + slope_median*x_data(point1)) - y_data(point1)
        sse_slope = sse_slope + resid ** 2
      endif
    enddo
              
  end subroutine kth_line

  !> Sorts values in an array into ascending order using Shell's method.
  !!
  !! @param[inout] sort_array The array to be sorted.
  !! @param[in] num_values The number of values in the array to be sorted.
  subroutine array_sort(sort_array, num_values)

    real, dimension(:) :: sort_array(num_values)
    integer, intent(in) :: num_values

    integer :: i, j, k, l, m, n
    integer :: lognb2
    real :: inv_ln2
    real :: hold_val
    real :: eps

    inv_ln2 = 1./0.69314718
    eps = 1.E-5
    lognb2=int(alog(float(num_values))*inv_ln2+eps)
    m=num_values
    do n=1,lognb2
      m=m/2
      k=num_values-m

      do j=1,k
        i=j
        do
          l=i+m

          if(sort_array(l) < sort_array(i)) then
            hold_val=sort_array(i)
            sort_array(i)=sort_array(l)
            sort_array(l)=hold_val
            i=i-m
            if(i >= 1) cycle
          endif
          exit ! Exit loop when i<1 or sort_array(l)<sort_array(i)
        enddo
      enddo
    enddo
  end subroutine array_sort

end module ModelFitUtils
!> @file
!! Contains functionality for finding the best model shape for a potential change point
!! to determine its shape.
