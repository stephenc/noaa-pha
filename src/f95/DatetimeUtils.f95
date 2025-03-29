!> @brief
!! Generic subroutines related to date and time that may be useful to
!! multiple modules.
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
module DatetimeUtils

  use Logger

  implicit none

contains

  !> Gets the current year.
  !! 
  !! @return The current year.
  function get_current_year() result(year)
    integer :: year
    integer,dimension(8) :: values

    ! using keyword arguments
    call date_and_time(VALUES=values)

    ! values are [1=year 2=month 3=day]
    year = values(1)

  end function get_current_year

  !> Converts total seconds to hours, minutes, and seconds.
  !!
  !! @param[in] total_seconds The seconds to be converted.
  !! @param[out] hours The number of hours in total_seconds.
  !! @param[out] minutes The number of minutes in total_seconds left
  !!                over after hours are separated out.
  !! @param[out] seconds The number of seconds in total_seconds left
  !!                over after hours and minutes are separated out.
  subroutine seconds_to_hours_minutes(total_seconds, hours, minutes, seconds)
    integer, intent(out) :: hours, minutes
    real, intent(out) :: seconds
    real, intent(in) :: total_seconds

    hours = 0
    minutes = 0
    seconds = 0.0

    if(total_seconds .ge. 60.0) then
      minutes = total_seconds / 60
      seconds = mod(total_seconds, 60.0)

      if(minutes .ge. 60) then
        hours = minutes / 60
        minutes = mod(minutes,60)
      endif
    else
      seconds = total_seconds
    endif

  end subroutine seconds_to_hours_minutes

end module DatetimeUtils
!> @file 
!! Contains convenience subroutines for handling date and time information.
