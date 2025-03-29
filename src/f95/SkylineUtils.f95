!> @brief
!! Converts between and within three time reference frames: skyline index,
!! month index and calendar year/month.
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

module SkylineUtils
        
  use Logger

  implicit none

  private :: sky_year, sky_first

  ! NOTE: start month is ALWAYS January
  !> sky_year are the start/end year of each station
  integer, dimension(:,:), allocatable :: sky_year
  !> sky_first are the skyline index (year,mth) of the start of the station
  integer, dimension(:), allocatable :: sky_first

contains

  !> Allocates the skyline work arrays based on the number of stations.
  !!
  !! @param[in] station_count The total number of stations.
  subroutine allocate_skyline(station_count)

    integer, intent(in) :: station_count

    ! sky_year is the begin/end years of each of the stations
    allocate(sky_year(2, station_count))
    ! sky_first is the YEAR-MONTH offset of each station in the SKYLINE arrays
    allocate(sky_first(station_count))

  end subroutine allocate_skyline

  !> Converts year/month to the skyline index
  !!
  !! @param station_index the index of the station
  !! @param calendar_year the calendar year
  !! @param calendar_month the calendar month
  !! @return the skyline index
  function get_sky_from_year_month(station_index, calendar_year, calendar_month) result (sky_index)

    integer :: calendar_month
    integer :: calendar_year
    integer :: sky_index
    integer :: station_index

    if(calendar_year .lt. sky_year(1,station_index)) then
      sky_index = -1
    else if(calendar_year .gt. sky_year(2,station_index)) then
      sky_index = -2
    else
      sky_index = sky_first(station_index) + (calendar_year - sky_year(1,station_index))*12 + (calendar_month-1)
    endif

  end function get_sky_from_year_month


  !> Converts the monthly index to the skyline index
  !!
  !! @param station_index the index of the station
  !! @param month_index the monthly index
  !! @return the skyline index
  function get_sky_from_month_index(station_index, month_index) result (sky_index)

    integer :: month_index
    integer :: calendar_month
    integer :: calendar_year
    integer :: sky_index
    integer :: station_index

    call get_year_month_from_month_index(calendar_year, calendar_month, month_index)
    sky_index = sky_first(station_index) + (calendar_year - sky_year(1,station_index))*12 + (calendar_month-1)
    call check_sky(station_index, sky_index)

  end function get_sky_from_month_index


  !> Converts the skyline index to the monthly index
  !!
  !! @param station_index the index of the station
  !! @param sky_index the skyline index
  !! @return the monthly index
  function get_month_index_from_sky(station_index, sky_index) result (month_index)

    integer :: first_sky_index
    integer :: first_calendar_year
    integer :: first_month_index
    integer :: month_offset
    integer :: sky_index
    integer :: station_index
    integer :: month_index

    first_sky_index = get_first_sky(station_index)
    first_calendar_year = get_first_year(station_index)
    first_month_index = get_month_index(first_calendar_year, 1)
    month_offset = sky_index - first_sky_index
    month_index = first_month_index + month_offset
    call check_month_index(month_index)

  end function get_month_index_from_sky


  !> Converts the skyline index to the monthly index, the calendar year and calendar month
  !!
  !! @param[in] station_index the index of the station
  !! @param[in] sky_index the skyline index
  !! @param[out] calendar_year the calendar year
  !! @param[out] calendar_month the calendar month
  !! @param[out] month_index the monthly index
  subroutine get_times_from_sky(station_index, month_index, calendar_year, calendar_month, sky_index)

    integer :: month_index
    integer :: first_month_index
    integer :: calendar_month
    integer :: sky_index
    integer :: first_sky_index
    integer :: month_offset
    integer :: station_index
    integer :: calendar_year
    integer :: first_calendar_year

    first_sky_index = get_first_sky(station_index)
    first_calendar_year = get_first_year(station_index)
    first_month_index = get_month_index(first_calendar_year, 1)
    month_offset = sky_index - first_sky_index
    month_index = first_month_index + month_offset
    call check_month_index(month_index)
    call get_year_month_from_month_index(calendar_year, calendar_month, month_index)

  end subroutine get_times_from_sky


  !> Returns the first skyline index for a station
  !!
  !! @param station_index the index of the station
  !! @return the first skyline index for a station
  function get_first_sky(station_index) result(first_sky_index)

    integer :: first_sky_index
    integer :: station_index

    first_sky_index = sky_first(station_index)

  end function get_first_sky


  !> Returns the last skyline index for a station
  !!
  !! @param station_index the index of the station
  !! @return the last skyline index for a station
  function get_last_sky(station_index) result (last_sky_index)

    integer :: last_sky_index
    integer :: station_index

    last_sky_index = sky_first(station_index) + ((sky_year(2,station_index)-sky_year(1,station_index)+1)*12) - 1

  end function get_last_sky


  !> Returns first calendar year for a station
  !!
  !! @param station_index the index of the station
  !! @return the first calendar year for a station
  function get_first_year(station_index) result (first_calendar_year)

    integer :: first_calendar_year
    integer :: station_index

    first_calendar_year = sky_year(1,station_index)

  end function get_first_year


  !> Returns last calendar year for a station
  !!
  !! @param station_index the index of the station
  !! @return the last calendar year for a station
  function get_last_year(station_index) result (last_calendar_year)

    integer :: last_calendar_year
    integer :: station_index

    last_calendar_year = sky_year(2,station_index)

  end function get_last_year

        
  !> Converts the skyline index to the calendar year and month
  !!
  !! @param[in] station_index the index of the station
  !! @param[in] sky_index the skyline index
  !! @param[out] calendar_year the calendar year
  !! @param[out] calendar_month the calendar month
  subroutine get_year_month_from_sky(calendar_year, calendar_month, sky_index, station_index)

    integer :: station_index
    integer :: calendar_month
    integer :: month_offset
    integer :: first_sky_index
    integer :: sky_index
    integer :: calendar_year
    integer :: first_calendar_year

    first_sky_index = get_first_sky(station_index)
    first_calendar_year = get_first_year(station_index)
    month_offset = sky_index - first_sky_index + 1
    calendar_year = int((month_offset-1)/12) + first_calendar_year
    calendar_month = mod(month_offset-1, 12) + 1

  end subroutine get_year_month_from_sky


  !> Converts the skyline index to the calendar month
  !!
  !! @param station_index the index of the station
  !! @param sky_index the skyline index
  !! @return the calendar month
  function get_month_from_sky(station_index, sky_index) result(calendar_month)

    integer :: station_index
    integer :: calendar_month
    integer :: month_offset
    integer :: first_sky_index
    integer :: sky_index

    first_sky_index = get_first_sky(station_index)
    month_offset = sky_index - first_sky_index + 1
    calendar_month = mod(month_offset-1, 12) + 1

  end function get_month_from_sky


  !> Converts the monthly index to the calendar year and month
  !!
  !! @param[in] month_index the monthly index
  !! @param[out] calendar_year the calendar year
  !! @param[out] calendar_month the calendar month
  subroutine get_year_month_from_month_index(calendar_year, calendar_month, month_index)
    use CommonVariables, only: begin_year

    integer :: calendar_month
    integer :: month_index
    integer :: calendar_year

    calendar_year = int((month_index-1)/12) + (begin_year)
    calendar_month = mod(month_index-1, 12) + 1

  end subroutine get_year_month_from_month_index


  !> Converts the calendar year and month to the monthly index
  !!
  !! @param calendar_year the calendar year
  !! @param calendar_month the calendar month
  !! @return the monthly index
  function get_month_index(calendar_year, calendar_month) result(month_index)
    use CommonVariables, only: begin_year

    integer :: calendar_month
    integer :: month_index
    integer :: calendar_year

    month_index = (calendar_year-begin_year)*12 + calendar_month
    call check_month_index(month_index)

  end function get_month_index

  !> Sets the first skyline index for this station in the skyline arrays.
  !!
  !! @param[in] station_index the index of the station
  !! @param[in] skyline_index the the first skyline_index for this station
  subroutine set_station_sky_first(station_index, skyline_index)
    integer, intent(in) :: station_index
    integer, intent(in) :: skyline_index

    sky_first(station_index) = skyline_index

  end subroutine set_station_sky_first

  !> Sets the first year and last year for this station in the skyline arrays.
  !!
  !! @param[in] station_index the index of the station
  !! @param[in] first_year the first calendar year for a station
  !! @param[in] last_year the last calendar year for a station
  subroutine set_station_sky_years(station_index, first_year, last_year)
    integer, intent(in) :: station_index
    integer, intent(in) :: first_year
    integer, intent(in) :: last_year

    sky_year(1, station_index) = first_year
    sky_year(2, station_index) = last_year

  end subroutine set_station_sky_years

  !> Checks to make sure the skyline index is in bounds
  !!
  !! @param[in] station_index the index of the station
  !! @param[in] sky_index the skyline index
  subroutine check_sky(station_index, sky_index)

    integer :: station_index
    integer :: sky_index
    integer :: first_sky_index
    integer :: last_sky_index
    character(len=256) :: log_message

    first_sky_index = get_first_sky(station_index)
    last_sky_index = get_last_sky(station_index)
    if (sky_index < first_sky_index .or. sky_index > last_sky_index) then
      write(log_message,'("The sky_index of ", i8, " is out of bounds (", i8, ",", i8,")")') &
        sky_index, first_sky_index, last_sky_index
      call log_fatal("SkylineUtils::check_sky: "//log_message)
      stop 1
    endif

  end subroutine check_sky

  !> Checks to make sure the monthly index is in bounds
  !!
  !! @param[in] month_index the monthly index of the station
  subroutine check_month_index(month_index)
    use CommonVariables, only: por_months

    integer :: month_index
    character(len=256) :: log_message

    if (month_index < 1 .or. month_index > por_months) then
      write(log_message,'("The month_index of ", i8, " is out of bounds (", i8, ",", i8,")")') &
        month_index, 1, por_months
      call log_fatal("SkyUtils::check_month_index: "//log_message)
      stop 1
    endif

  end subroutine check_month_index

end module SkylineUtils
