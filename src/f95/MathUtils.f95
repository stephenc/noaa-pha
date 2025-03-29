!> @brief
!! Generic subroutines related to generally useful mathematical calculations.
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
module MathUtils

  use AlgorithmParameters

implicit none

  private :: calculate_average_with_size, calculate_average_without_size

  !> Creates a new Changepoint instance.
  interface calculate_average
    module procedure calculate_average_with_size
    module procedure calculate_average_without_size
  end interface calculate_average

contains

  !> Computes the Pearson correlation coefficent
  !! between a candidate station and each of its neighbors.  account
  !! is taken of missing data in the candidate and each neighbor so
  !! that the mean, stnd dev, and cross-products are computed only
  !! for those months when both candidate and neighbor have good,
  !! as opposed to missing, data.  each correlation coefficient applies
  !! to all months of the year.
  !!
  !! @param good_values_count The number of months of good and missing
  !!                          data in each time series of monthly departures.
  !! @param target_values The array of first differences for the target station
  !! @param neighbor_values The array of first differences for the neighbor station
  !! @return coefficient The array of candidate-neighbor pearson correlation coefficients
  function calculate_correlation(good_values_count,target_values,neighbor_values) result(coefficient)
    integer :: good_values_count
    real, dimension(:) :: target_values
    real, dimension(:) :: neighbor_values
    real :: coefficient

    integer :: good_pair_count ! counter for good candidate-neighbor data
    real, dimension(:) :: good_target_values(good_values_count)
    real, dimension(:) :: good_neighbor_values(good_values_count)
    real :: target_standard_dev
    real :: neighbor_standard_dev
    real :: sum
    real :: target_mean
    real :: neighbor_mean
    integer :: i, j

    ! determine those months that both candidate and a neighbor have good data.
    ! They will vary with the particular candidate-neighbor combination.
    coefficient = 1.0
    good_pair_count = 0
    do i = 1,good_values_count
      if (.not. is_missing_real(target_values(i)) .and. &
          .not. is_missing_real(neighbor_values(i))) then
        good_pair_count = good_pair_count + 1
        good_target_values(good_pair_count) = target_values(i)
        good_neighbor_values(good_pair_count) = neighbor_values(i)
      endif
    end do

    ! calculate means for candidate and neighbor using only those months
    ! that both neighbor and candidate have good data
    target_mean = 0.0
    neighbor_mean = 0.0
    do j = 1,good_pair_count
      target_mean = target_mean + good_target_values(j)
      neighbor_mean = neighbor_mean + good_neighbor_values(j)
    end do

    if(good_pair_count .gt. 4) then
      target_mean = target_mean/good_pair_count
      neighbor_mean = neighbor_mean/good_pair_count

      ! calculate standard deviations for candidate and neigbhor using only
      ! those months that both neighbor and candifdate have good data
      target_standard_dev = 0.0
      neighbor_standard_dev = 0.0
      do j = 1, good_pair_count
        target_standard_dev = target_standard_dev + (good_target_values(j)-target_mean)**2
        neighbor_standard_dev = neighbor_standard_dev + (good_neighbor_values(j)-neighbor_mean)**2
      end do

      target_standard_dev = sqrt(target_standard_dev/(good_pair_count-1))
      neighbor_standard_dev = sqrt(neighbor_standard_dev/(good_pair_count-1))

      ! calculate pearson correlation coefficient for each candidate-neighbor combination
      sum = 0.0
      do j = 1, good_pair_count
        sum = sum + (good_target_values(j)-target_mean)*(good_neighbor_values(j)-neighbor_mean)
      end do

      if(neighbor_standard_dev .lt. FP_EPSILON) neighbor_standard_dev = FP_EPSILON
      if(target_standard_dev .lt. FP_EPSILON) target_standard_dev = FP_EPSILON
      coefficient = sum/((good_pair_count-1)*target_standard_dev*neighbor_standard_dev)
    else
      coefficient = MISSING_REAL
    endif

  end function calculate_correlation

  !> Calculates the average of an array of reals, using the array
  !! size to determine the number of values being averaged.
  !!
  !! @param values The array of real numbers to be averaged.
  !! @return The average of the values.
  function calculate_average_without_size(values) result(average)
    real, dimension(:) :: values
    real :: average
    integer :: values_count

    values_count = size(values)
    average = calculate_average(values, values_count)

  end function calculate_average_without_size

  !> Calculates the average of an array of reals, using a specified
  !! size to determine the number of values being averaged.
  !!
  !! @param values The array of real numbers to be averaged.
  !! @param values_count The number of values in the array to average.
  !! @return The average of the values.
  function calculate_average_with_size(values, values_count) result(average)
    real, dimension(:) :: values
    integer :: values_count

    real :: current_sum
    real :: average
    integer :: idx

    current_sum = 0.0
    do idx = 1, values_count
      current_sum = current_sum + values(idx)
    enddo
    average = current_sum / values_count

  end function calculate_average_with_size

  !> Tests a real value for MISSING.
  !!
  !! @param value The value to be tested.
  !! @return is_missing True if the value is considered MISSING. Otherwise, false.
  function is_missing_real(value) result(is_missing)
    real :: value
    logical :: is_missing

    is_missing = value <= MISSING_REAL+1.0

  end function is_missing_real

  !> Tests an integer value to determine if it represents a MISSING value.
  !! An integer must be exactly equal to AlgorithmParameters::MISSING_INT
  !! to be treated as missing.
  !!
  !! @param value The value to be tested against the missing value.
  !! @return is_missing True if the value is considered MISSING. Otherwise, false.
  function is_missing_int(value) result(is_missing)
    integer :: value
    logical :: is_missing

    ! @todo Was previously (value <= MISSING_INT+9) to cover -9990
    ! and various other large negative numbers that are invalid
    ! but changed to the below to match original code:
    is_missing = (value == MISSING_INT)

  end function is_missing_int

end module MathUtils
!> @file
!! Contains functionality for generally useful mathematical calculations.
