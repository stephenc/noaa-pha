!> @brief The module for running unit tests against the FindChangepoints
!! module subroutines.
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
module FindChangepointsTest

  use FindChangepoints
  use PropertyParameters
  use PropertyReader
  use UnitTest

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine find_changepoints_tests()

    call test_get_critical_value()

  end subroutine find_changepoints_tests

  !> Tests the snits subroutine.
  subroutine test_get_critical_value()

    real :: critval_90
    real :: critval_95
    real :: critval_975

    ! Just above the max num_values
    critval_90 = get_critical_value(251, OPT_SNHT_90)
    critval_95 = get_critical_value(251, OPT_SNHT_95)
    critval_975 = get_critical_value(251, OPT_SNHT_97_5)
    call assert_equals_within_tolerance(8.35, critval_90, .001, "snits lookup with 90% and 251 values")
    call assert_equals_within_tolerance(9.70, critval_95, .001, "snits lookup with 95% and 251 values")
    call assert_equals_within_tolerance(11.20, critval_975, .001, "snits lookup with 97.5% and 251 values")

    ! At the max num_values
    critval_90 = get_critical_value(250, OPT_SNHT_90)
    critval_95 = get_critical_value(250, OPT_SNHT_95)
    critval_975 = get_critical_value(250, OPT_SNHT_97_5)
    call assert_equals_within_tolerance(8.35, critval_90, .001, "snits lookup with 90% and 250 values")
    call assert_equals_within_tolerance(9.70, critval_95, .001, "snits lookup with 95% and 250 values")
    call assert_equals_within_tolerance(11.20, critval_975, .001, "snits lookup with 97.5% and 250 values")

    ! Right at a cut-off
    critval_90 = get_critical_value(100, OPT_SNHT_90)
    critval_95 = get_critical_value(100, OPT_SNHT_95)
    critval_975 = get_critical_value(100, OPT_SNHT_97_5)
    call assert_equals_within_tolerance(7.85, critval_90, .001, "snits lookup with 90% and 100 values")
    call assert_equals_within_tolerance(9.15, critval_95, .001, "snits lookup with 95% and 100 values")
    call assert_equals_within_tolerance(10.40, critval_975, .001, "snits lookup with 97.5% and 100 values")

    ! Just above a cut off
    critval_90 = get_critical_value(101, OPT_SNHT_90)
    critval_95 = get_critical_value(101, OPT_SNHT_95)
    critval_975 = get_critical_value(101, OPT_SNHT_97_5)
    call assert_equals_within_tolerance(7.854, critval_90, .001, "snits lookup with 90% and 101 values")
    call assert_equals_within_tolerance(9.154, critval_95, .001, "snits lookup with 95% and 101 values")
    call assert_equals_within_tolerance(10.408, critval_975, .001, "snits lookup with 97.5% and 101 values")

    ! Just below a cut-off
    critval_90 = get_critical_value(99, OPT_SNHT_90)
    critval_95 = get_critical_value(99, OPT_SNHT_95)
    critval_975 = get_critical_value(99, OPT_SNHT_97_5)
    call assert_equals_within_tolerance(7.845, critval_90, .001, "snits lookup with 90% and 99 values")
    call assert_equals_within_tolerance(9.14, critval_95, .001, "snits lookup with 95% and 99 values")
    call assert_equals_within_tolerance(10.39, critval_975, .001, "snits lookup with 97.5% and 99 values")

    ! Right at the minimum num_values
    critval_90 = get_critical_value(5, OPT_SNHT_90)
    critval_95 = get_critical_value(5, OPT_SNHT_95)
    critval_975 = get_critical_value(5, OPT_SNHT_97_5)
    call assert_equals_within_tolerance(4.27, critval_90, .001, "snits lookup with 90% and 5 values")
    call assert_equals_within_tolerance(4.54, critval_95, .001, "snits lookup with 95% and 5 values")
    call assert_equals_within_tolerance(4.71, critval_975, .001, "snits lookup with 97.5% and 5 values")

    ! Just below the minimum num_values
    critval_90 = get_critical_value(1, OPT_SNHT_90)
    critval_95 = get_critical_value(1, OPT_SNHT_95)
    critval_975 = get_critical_value(1, OPT_SNHT_97_5)
    call assert_equals_within_tolerance(99999.0, critval_90, .001, "snits lookup with 90% and 1 values")
    call assert_equals_within_tolerance(99999.0, critval_95, .001, "snits lookup with 95% and 1 values")
    call assert_equals_within_tolerance(99999.0, critval_975, .001, "snits lookup with 97.5% and 1 values")

    ! At zero and below the minimum num_values
    critval_90 = get_critical_value(0, OPT_SNHT_90)
    critval_95 = get_critical_value(0, OPT_SNHT_95)
    critval_975 = get_critical_value(0, OPT_SNHT_97_5)
    call assert_equals_within_tolerance(99999.0, critval_90, .001, "snits lookup with 90% and 0 values")
    call assert_equals_within_tolerance(99999.0, critval_95, .001, "snits lookup with 95% and 0 values")
    call assert_equals_within_tolerance(99999.0, critval_975, .001, "snits lookup with 97.5% and 0 values")

    ! Negative number and below the minimum num_values
    critval_90 = get_critical_value(-100, OPT_SNHT_90)
    critval_95 = get_critical_value(-100, OPT_SNHT_95)
    critval_975 = get_critical_value(-100, OPT_SNHT_97_5)
    call assert_equals_within_tolerance(99999.0, critval_90, .001, "snits lookup with 90% and negative")
    call assert_equals_within_tolerance(99999.0, critval_95, .001, "snits lookup with 95% and negative")
    call assert_equals_within_tolerance(99999.0, critval_975, .001, "snits lookup with 97.5% and negative")

  end subroutine test_get_critical_value

end module FindChangepointsTest
