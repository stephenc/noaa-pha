!> @brief Unit tests for ModelFitUtils module.
!!
!! @copyright COPYRIGHT
!! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC
!! DOMAIN AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE. THEY ARE
!! FURNISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
!! INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
!! EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE AND
!! DOCUMENTATION FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR
!! THE USE OF THE SOFTWARE AND DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL
!! SUPPORT TO USERS.
!!
module ModelFitUtilsTest

  use UnitTest
  use ModelFitUtils

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine model_fit_utils_tests()
 
    call test_array_sort()
    call test_minbic()
    call test_bayes()
    call test_calculate_y_intercept()
    call test_get_critval()

  end subroutine model_fit_utils_tests

  !> Tests the array_sort subroutine
  subroutine test_array_sort()

    real, dimension(:) :: items_even(10)
    real, dimension(:) :: items_odd(11)
    real, dimension(:) :: items_single(1)
    real, dimension(:) :: sorted_even(10)
    real, dimension(:) :: sorted_odd(11)
    real, dimension(:) :: sorted_single(1)

    items_even = (/ 6.0, 4.2, 6.0, 1.0, 0.2, 0.1, 8.2, 8.3, 8.4, 5.7 /)
    sorted_even = (/ 0.1, 0.2, 1.0, 4.2, 5.7, 6.0, 6.0, 8.2, 8.3, 8.4 /)
    call array_sort(items_even, 10)
    call assert_equals_within_tolerance(items_even, sorted_even, FP_EPSILON, "sort_ascending even items")

    items_odd = (/ 1.3, 6.0, 4.2, 6.0, 1.0, 0.2, 0.1, 8.2, 8.3, 8.4, 5.7 /)
    sorted_odd = (/ 0.1, 0.2, 1.0, 1.3, 4.2, 5.7, 6.0, 6.0, 8.2, 8.3, 8.4 /)
    call array_sort(items_odd, 11)
    call assert_equals_within_tolerance(items_odd, sorted_odd, FP_EPSILON, "sort_ascending odd items")

    items_single = (/ 2.0 /)
    sorted_single = (/ 2.0 /)
    call array_sort(items_single, 1)
    call assert_equals_within_tolerance(items_single, sorted_single, FP_EPSILON, "sort_ascending single item")

  end subroutine test_array_sort

  !> Tests the minbic subroutine
  subroutine test_minbic()

    real, dimension(:) :: x_data(10), y_data(10)
    integer data_index_end1, data_index_end2
    real :: crit_thresh_best
    real :: test_stat
    real :: schwarz_min
    real :: offset_best
    real y_int_best(2), slope_best(2)
    real :: sse_best
    integer :: model_type
    integer seg_obs(2)

    x_data = (/1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0/)
    data_index_end1 = 5
    data_index_end2 = 10

    y_data = (/0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.6/)
    call minbic(2, x_data, y_data, data_index_end1, data_index_end2, crit_thresh_best, test_stat, &
      schwarz_min, offset_best, y_int_best, slope_best, sse_best, model_type, seg_obs)
    call assert_equals(model_type, 1, "The flat single line model is chosen")
    call assert_equals_within_tolerance(offset_best, 0.0, .00001, 'The offset of a horizontal line is zero')

    y_data = (/0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0/)
    call minbic(2, x_data, y_data, data_index_end1, data_index_end2, crit_thresh_best, test_stat, &
      schwarz_min, offset_best, y_int_best, slope_best, sse_best, model_type, seg_obs)
    call assert_equals(model_type, 2, "The sloped single line model is chosen")
    call assert_equals_within_tolerance(offset_best, 0.0, .00001, 'The offset of a straight line is zero')

    y_data = (/0.5, 0.5, 0.5, 0.5, 0.5, 1.5, 1.5, 1.5, 1.5, 1.5/)
    call minbic(2, x_data, y_data, data_index_end1, data_index_end2, crit_thresh_best, test_stat, &
      schwarz_min, offset_best, y_int_best, slope_best, sse_best, model_type, seg_obs)
    call assert_equals(model_type, 3, "The two flat segments with offset model is chosen")
    call assert_equals_within_tolerance(offset_best, -1.0, .00001, 'The offset of model 3')

    y_data = (/0.1, 0.2, 0.3, 0.4, 0.5, 0.1, 0.2, 0.3, 0.4, 0.6/)
    call minbic(2, x_data, y_data, data_index_end1, data_index_end2, crit_thresh_best, test_stat, &
      schwarz_min, offset_best, y_int_best, slope_best, sse_best, model_type, seg_obs)
    call assert_equals(model_type, 4, "The two same sloped segments with offset model is chosen")
    call assert_equals_within_tolerance(offset_best, 0.4, .00001, 'The offset of model 4')

    y_data = (/0.1, 0.2, 0.3, 0.4, 0.5, 0.1, 0.3, 0.5, 0.7, 0.9/)
    call minbic(2, x_data, y_data, data_index_end1, data_index_end2, crit_thresh_best, test_stat, &
      schwarz_min, offset_best, y_int_best, slope_best, sse_best, model_type, seg_obs)
    call assert_equals(model_type, 5, "The two different sloped segments with offset model is chosen")
    call assert_equals_within_tolerance(offset_best, 0.4, .00001, 'The offset of model 5')

    y_data = (/0.5, 0.5, 0.5, 0.5, 0.5, 0.1, 0.3, 0.5, 0.7, 0.9/)
    call minbic(2, x_data, y_data, data_index_end1, data_index_end2, crit_thresh_best, test_stat, &
      schwarz_min, offset_best, y_int_best, slope_best, sse_best, model_type, seg_obs)
    call assert_equals(model_type, 6, "The flat to sloped segments with offset model is chosen")
    call assert_equals_within_tolerance(offset_best, 0.4, .00001, 'The offset of model 6')

    y_data = (/0.1, 0.3, 0.5, 0.7, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1/)
    call minbic(2, x_data, y_data, data_index_end1, data_index_end2, crit_thresh_best, test_stat, &
      schwarz_min, offset_best, y_int_best, slope_best, sse_best, model_type, seg_obs)
    call assert_equals(model_type, 7, "The sloped to flat segments with offset model is chosen")
    call assert_equals_within_tolerance(offset_best, 0.8, .00001, 'The offset of model 7')

  end subroutine test_minbic

  !> Tests the bayes function
  subroutine test_bayes()

    integer :: num_free_params
    integer num_values
    real :: res_sum_sqr
    real :: schwarz

    num_values = 17
    num_free_params = 4
    res_sum_sqr = 56.

    schwarz = bayes(num_values, res_sum_sqr, num_free_params, "bic")
    call assert_equals_within_tolerance(schwarz, 13.7233605, .00001, 'The schwarz statistic for bic bic_type')

    schwarz = bayes(num_values, res_sum_sqr, num_free_params, "aic")
    call assert_equals_within_tolerance(schwarz, 28.2663519, .00001, 'The schwarz statistic for aic bic_type')

    schwarz = bayes(num_values, res_sum_sqr, num_free_params, "none")
    call assert_equals_within_tolerance(schwarz, 8.8015648, .00001, 'The schwarz statistic for none bic_type')

  end subroutine test_bayes

  !> Tests the calculate_y_intercept function
  subroutine test_calculate_y_intercept()

    real, dimension(:) :: x_comp_odd(7)
    real, dimension(:) :: y_comp_odd(7)
    integer :: num_values
    real :: slope_median
    real :: y_intercept

    num_values = 7
    x_comp_odd = (/1.0, 2.0, 3.0, 7.0, 8.0, 9.0, 10./)
    y_comp_odd = (/3.1, 1.1, 9.0, 2.4, 7.0, 2.7, 4.5/)
    slope_median = 1.5

    y_intercept = calculate_y_intercept(x_comp_odd, y_comp_odd, num_values, slope_median)
    call assert_equals_within_tolerance(y_intercept, -7.4, .00001, 'The y intercept for model 4')

  end subroutine test_calculate_y_intercept

  !> Tests the get_critval function
  subroutine test_get_critval()

    call assert_equals_within_tolerance(get_critval(1, 3), 12.71, 0.00001, '1 obs, model type = 3')
    call assert_equals_within_tolerance(get_critval(6, 3), 2.45, 0.00001, '6 obs, model type = 3')
    call assert_equals_within_tolerance(get_critval(30, 3), 2.04, 0.00001, '30 obs, model type = 3')
    call assert_equals_within_tolerance(get_critval(35, 3), 2.03, 0.00001, '35 obs, model type = 3')
    call assert_equals_within_tolerance(get_critval(50, 3), 2.01, 0.00001, '50 obs, model type = 3')
    call assert_equals_within_tolerance(get_critval(120, 3), 1.98, 0.00001, '120 obs, model type = 3')

    call assert_equals_within_tolerance(get_critval(1, 4), 161.4, 0.00001, '1 obs, model type = 4')
    call assert_equals_within_tolerance(get_critval(6, 4), 6.0, 0.00001, '6 obs, model type = 4')
    call assert_equals_within_tolerance(get_critval(30, 4), 4.2, 0.00001, '30 obs, model type = 4')
    call assert_equals_within_tolerance(get_critval(35, 4), 4.15, 0.00001, '35 obs, model type = 4')
    call assert_equals_within_tolerance(get_critval(50, 4), 4.05, 0.00001, '50 obs, model type = 4')
    call assert_equals_within_tolerance(get_critval(120, 4), 3.9, 0.00001, '120 obs, model type = 4')

    call assert_equals_within_tolerance(get_critval(1, 5), 199.5, 0.00001, '1 obs, model type = 5')
    call assert_equals_within_tolerance(get_critval(6, 5), 5.14, 0.00001, '6 obs, model type = 5')
    call assert_equals_within_tolerance(get_critval(30, 5), 3.32, 0.00001, '30 obs, model type = 5')
    call assert_equals_within_tolerance(get_critval(35, 5), 3.275, 0.00001, '35 obs, model type = 5')
    call assert_equals_within_tolerance(get_critval(50, 5), 3.19, 0.00001, '35 obs, model type = 5')
    call assert_equals_within_tolerance(get_critval(120, 5), 3.07, 0.00001, '120 obs, model type = 5')

  end subroutine test_get_critval

end module ModelFitUtilsTest
