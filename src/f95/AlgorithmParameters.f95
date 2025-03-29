!> @brief
!! Holds all constant parameters used for algorithm options.
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
module AlgorithmParameters

  use Logger

  !> Convenience parameter for initializing or setting integer of size 1 to ZERO
  integer(1), parameter :: ZERO_SIZE1 = 0
  !> Convenience parameter for initializing or setting integer of size 2 to ZERO
  integer(2), parameter :: ZERO_SIZE2 = 0
  !> Convenience parameter for initializing or setting integer of default size to ZERO
  integer, parameter :: ZERO = 0
  !> Convenience parameter for initializing or setting real to ZERO
  real, parameter :: ZERO_REAL = 0.0
  !> Convenience parameter for setting logical of size 1 to FALSE
  logical(1), parameter :: FALSE = .false.
  !> Convenience parameter for setting logical of size 1 to TRUE
  logical(1), parameter :: TRUE = .true.

  !> An arbitrary tiny number.
  real, parameter :: FP_EPSILON = .000005

  !> Missing data constant for real value
  real, parameter :: MISSING_REAL = -99.99
  !> Missing data constant for integer value
  integer, parameter :: MISSING_INT = -9999

  !> Scale value used to convert between integer and actual real value
  real, parameter :: VALUE_SCALE = 100.

  !> Maximum number of changepoints allowed in a series
  integer, parameter :: MAX_CHANGEPOINTS = 80

  !> Minimum number of individual months in a raw series that can be tested
  integer, parameter :: MIN_MONTHS_RAW = 5

  !> Minimum number of years required to create a monthly average
  integer, parameter :: MIN_YEARS_FOR_MONTHLY_AVE = 5

  !> Number of amplitude ranges
  integer, parameter :: AMP_RANGE_COUNT = 7

  !> In the algorithm, the combination of the monthly merge and the hit thresholds set
  !! in the following arrays results in ~10% FAR with close to the maximum HSS skill score.
  !! range amplitude of estimated offsets to set the monthly merge size
  real, dimension(AMP_RANGE_COUNT), parameter :: AMP_RANGES = (/ 0.4,0.6,0.8,1.0,1.5,3.0,5.0 /)

  !> Structural Uncertainty Option - amploc_percent
  !! Uses varying percents inclusion from the "amp vs location uncertainty" spread
  integer, dimension(AMP_RANGE_COUNT, 3), parameter :: MERGE_RADII = &
      reshape((/ 29, 12, 7, 5, 3, 2, 1, 36, 18, 12, 8, 6, 5, 5, 59, 23, 12, 8, 6, 5, 5 /), shape(MERGE_RADII))

contains

  !> Index for the "amplitude vs location" percent inclusion for merging of pairwise hits
  !!
  !! @param amploc_percent Valid input values are 90, 92, 95. Default is 92.
  !! @return The arbitrary index for the given amploc percent: 1 (90%), <2> (92.5%), 3 (95%)
  function get_amploc_percent_index(amploc_percent) result(amploc_percent_index)
    integer :: amploc_percent
    integer :: amploc_percent_index

    if( amploc_percent == 90 ) then
      amploc_percent_index = 1
    else if ( amploc_percent == 92 ) then
      amploc_percent_index = 2
    else if ( amploc_percent == 95 ) then
      amploc_percent_index = 3
    else
      amploc_percent_index = 2
      call log_warn("amploc_percent property is set to an invalid option, "//  &
             trim(log_string(amploc_percent))//". Setting to default 92.")
    endif
  end function get_amploc_percent_index

end module AlgorithmParameters

!> @file
!! Contains parameters for constants for algorithm options.
