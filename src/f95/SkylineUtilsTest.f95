!> @brief Unit tests for SkylineUtils module.
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
module SkylineUtilsTest

  use UnitTest
  use PropertyReader
  use SkylineUtils

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine skyline_utils_tests()
 
    call setup_skyline()
    call test_get_sky_from_year_month()
    call test_get_sky_from_month_index()
    call test_get_month_index_from_sky()
    call test_get_times_from_sky()
    call test_get_first_sky()
    call test_get_last_sky()
    call test_get_first_year()
    call test_get_last_year()
    call test_get_year_month_from_sky()
    call test_get_month_from_sky()
    call test_get_year_mo_from_mo_index()
    call test_get_month_index()

  end subroutine skyline_utils_tests

  !> Sets up the skyline arrays for test stations, which indirectly tests the allocate
  !! and setter subroutines.
  subroutine setup_skyline()

    call allocate_skyline(3)

    call set_station_sky_years(1, 1901, 1950)
    call set_station_sky_years(2, 1921, 1970)
    call set_station_sky_years(3, 1941, 1990)

    call set_station_sky_first(1, 1)
    call set_station_sky_first(2, 601)
    call set_station_sky_first(3, 1201)

  end subroutine setup_skyline

  !> Tests the get_sky_from_year_month() function.
  subroutine test_get_sky_from_year_month()

    call assert_equals(get_sky_from_year_month(1, 1901, 1), 1, "skyline of first station first year/month")
    call assert_equals(get_sky_from_year_month(1, 1950, 12), 600, "skyline of first station last year/month")
    call assert_equals(get_sky_from_year_month(1, 1925, 6), 294, "skyline of first station middle year/month")

    call assert_equals(get_sky_from_year_month(2, 1921, 1), 601, "skyline of second station first year/month")
    call assert_equals(get_sky_from_year_month(2, 1970, 12), 1200, "skyline of second station last year/month")
    call assert_equals(get_sky_from_year_month(2, 1960, 4), 1072, "skyline of second station middle year/month")

    call assert_equals(get_sky_from_year_month(3, 1941, 1), 1201, "skyline of third station first year/month")
    call assert_equals(get_sky_from_year_month(3, 1990, 12), 1800, "skyline of third station last year/month")
    call assert_equals(get_sky_from_year_month(3, 1965, 4), 1492, "skyline of third station middle year/month")

    call assert_equals(get_sky_from_year_month(1, 1890, 1), -1, "skyline of first station when year/month too low")
    call assert_equals(get_sky_from_year_month(1, 1960, 12), -2, "skyline of first station when year/month too high")

  end subroutine test_get_sky_from_year_month

  !> Tests the get_sky_from_month_index() function.
  subroutine test_get_sky_from_month_index()

    call assert_equals(get_sky_from_month_index(1, 601), 1, "skyline of first station first month_index")
    call assert_equals(get_sky_from_month_index(1, 1200), 600, "skyline of first station last month_index")
    call assert_equals(get_sky_from_month_index(1, 894), 294, "skyline of first station middle month_index")

    call assert_equals(get_sky_from_month_index(2, 841), 601, "skyline of second station first month_index")
    call assert_equals(get_sky_from_month_index(2, 1440), 1200, "skyline of second station last month_index")
    call assert_equals(get_sky_from_month_index(2, 1312), 1072, "skyline of second station middle month_index")

    call assert_equals(get_sky_from_month_index(3, 1081), 1201, "skyline of third station first month_index")
    call assert_equals(get_sky_from_month_index(3, 1680), 1800, "skyline of third station last month_index")
    call assert_equals(get_sky_from_month_index(3, 1372), 1492, "skyline of third station middle month_index")

  end subroutine test_get_sky_from_month_index

  !> Tests the get_month_index_from_sky() function.
  subroutine test_get_month_index_from_sky()

    call assert_equals(get_month_index_from_sky(1, 1), 601, "skyline of first station first month_index")
    call assert_equals(get_month_index_from_sky(1, 600), 1200, "skyline of first station last month_index")
    call assert_equals(get_month_index_from_sky(1, 294), 894, "skyline of first station middle month_index")

    call assert_equals(get_month_index_from_sky(2, 601), 841, "skyline of second station first month_index")
    call assert_equals(get_month_index_from_sky(2, 1200), 1440, "skyline of second station last month_index")
    call assert_equals(get_month_index_from_sky(2, 1072), 1312, "skyline of second station middle month_index")

    call assert_equals(get_month_index_from_sky(3, 1201), 1081, "skyline of third station first month_index")
    call assert_equals(get_month_index_from_sky(3, 1800), 1680, "skyline of third station last month_index")
    call assert_equals(get_month_index_from_sky(3, 1492), 1372, "skyline of third station middle month_index")

  end subroutine test_get_month_index_from_sky

  !> Tests the get_times_from_sky() subroutine.
  subroutine test_get_times_from_sky()

    integer :: year
    integer :: month
    integer :: month_index

    call get_times_from_sky(1, month_index, year, month, 1)
    call assert_equals(month_index, 601, "month_index of first station first skyline")
    call assert_equals(year, 1901, "year of first station first skyline")
    call assert_equals(month, 1, "month of first station first skyline")

    call get_times_from_sky(1, month_index, year, month, 600)
    call assert_equals(month_index, 1200, "month_index of first station last skyline")
    call assert_equals(year, 1950, "year of first station last skyline")
    call assert_equals(month, 12, "month of first station last skyline")

    call get_times_from_sky(1, month_index, year, month, 294)
    call assert_equals(month_index, 894, "month_index of first station middle skyline")
    call assert_equals(year, 1925, "year of first station middle skyline")
    call assert_equals(month, 6, "month of first station middle skyline")

    call get_times_from_sky(2, month_index, year, month, 601)
    call assert_equals(month_index, 841, "month_index of second station first skyline")
    call assert_equals(year, 1921, "year of second station first skyline")
    call assert_equals(month, 1, "month of second station first skyline")

    call get_times_from_sky(2, month_index, year, month, 1200)
    call assert_equals(month_index, 1440, "month_index of second station last skyline")
    call assert_equals(year, 1970, "year of second station last skyline")
    call assert_equals(month, 12, "month of second station last skyline")

    call get_times_from_sky(2, month_index, year, month, 1072)
    call assert_equals(month_index, 1312, "month_index of second station middle skyline")
    call assert_equals(year, 1960, "year of second station middle skyline")
    call assert_equals(month, 4, "month of second station middle skyline")

    call get_times_from_sky(3, month_index, year, month, 1201)
    call assert_equals(month_index, 1081, "month_index of third station first skyline")
    call assert_equals(year, 1941, "year of third station first skyline")
    call assert_equals(month, 1, "month of third station first skyline")

    call get_times_from_sky(3, month_index, year, month, 1800)
    call assert_equals(month_index, 1680, "month_index of third station last skyline")
    call assert_equals(year, 1990, "year of third station last skyline")
    call assert_equals(month, 12, "month of third station last skyline")

    call get_times_from_sky(3, month_index, year, month, 1492)
    call assert_equals(month_index, 1372, "month_index of third station middle skyline")
    call assert_equals(year, 1965, "year of third station middle skyline")
    call assert_equals(month, 4, "month of third station middle skyline")

  end subroutine test_get_times_from_sky

  !> Tests the get_first_sky() function, getting the first skyline index for stations.
  subroutine test_get_first_sky()

    call assert_equals(get_first_sky(1), 1, "first skyline of first station")
    call assert_equals(get_first_sky(2), 601, "first skyline of second station")
    call assert_equals(get_first_sky(3), 1201, "first skyline of last station")

  end subroutine test_get_first_sky

  !> Tests the get_last_sky() function, getting the last skyline index for stations.
  subroutine test_get_last_sky()

    call assert_equals(get_last_sky(1), 600, "last skyline of first station")
    call assert_equals(get_last_sky(2), 1200, "last skyline of second station")
    call assert_equals(get_last_sky(3), 1800, "last skyline of last station")

  end subroutine test_get_last_sky

  !> Tests the get_first_year() function.
  subroutine test_get_first_year()

    call assert_equals(get_first_year(1), 1901, "first year for first station")
    call assert_equals(get_first_year(2), 1921, "first year for second station")
    call assert_equals(get_first_year(3), 1941, "first year for third station")

  end subroutine test_get_first_year

  !> Tests the get_last_year() function.
  subroutine test_get_last_year()

    call assert_equals(get_last_year(1), 1950, "last year for first station")
    call assert_equals(get_last_year(2), 1970, "last year for second station")
    call assert_equals(get_last_year(3), 1990, "last year for third station")

  end subroutine test_get_last_year

  !> Tests the get_year_month_from_sky() subroutine.
  subroutine test_get_year_month_from_sky()
    integer :: year
    integer :: month

    call get_year_month_from_sky(year, month, 1, 1)
    call assert_equals(year, 1901, "year for first station, first skyline")
    call assert_equals(month, 1, "month for first station, first skyline")

    call get_year_month_from_sky(year, month, 200, 1)
    call assert_equals(year, 1917, "year for first station, first skyline")
    call assert_equals(month, 8, "year for first station, first skyline")

    call get_year_month_from_sky(year, month, 601, 2)
    call assert_equals(year, 1921, "year for second station, first skyline")
    call assert_equals(month, 1, "month for second station, first skyline")

    call get_year_month_from_sky(year, month, 1200, 2)
    call assert_equals(year, 1970, "year for second station, last skyline")
    call assert_equals(month, 12, "month for second station, last skyline")

    call get_year_month_from_sky(year, month, 1800, 3)
    call assert_equals(year, 1990, "year for third station, last skyline")
    call assert_equals(month, 12, "month for third station, last skyline")

  end subroutine test_get_year_month_from_sky

  !> Tests the get_month_from_sky() function.
  subroutine test_get_month_from_sky()

    call assert_equals(get_month_from_sky(1, 1), 1, "Month for first skyline of first station")
    call assert_equals(get_month_from_sky(1, 12), 12, "Month for 12th skyline of first station")
    call assert_equals(get_month_from_sky(1, 600), 12, "Month for last skyline of first station")
    call assert_equals(get_month_from_sky(2, 601), 1, "Month for first skyline of second station")
    call assert_equals(get_month_from_sky(2, 602), 2, "Month for second skyline of second station")
    call assert_equals(get_month_from_sky(3, 1400), 8, "Month for middle skyline of third station")
    call assert_equals(get_month_from_sky(3, 1800), 12, "Month for last skyline of third station")

  end subroutine test_get_month_from_sky

  !> Tests the get_year_month_from_month_index() subroutine. Truncated "month" in test
  !! subroutine name to comply with CDR-P Coding Standards (subroutine names < 32 characters)
  subroutine test_get_year_mo_from_mo_index()

    integer :: year
    integer :: month

    call get_year_month_from_month_index(year, month, 601)
    call assert_equals(year, 1901, "year of first station first month_index")
    call assert_equals(month, 1, "month of first station first month_index")

    call get_year_month_from_month_index(year, month, 1200)
    call assert_equals(year, 1950, "year of first station last month_index")
    call assert_equals(month, 12, "month of first station last month_index")

    call get_year_month_from_month_index(year, month, 894)
    call assert_equals(year, 1925, "year of first station middle month_index")
    call assert_equals(month, 6, "month of first station middle month_index")

    call get_year_month_from_month_index(year, month, 841)
    call assert_equals(year, 1921, "year of second station first month_index")
    call assert_equals(month, 1, "month of second station first month_index")

    call get_year_month_from_month_index(year, month, 1440)
    call assert_equals(year, 1970, "year of second station last month_index")
    call assert_equals(month, 12, "month of second station last month_index")

    call get_year_month_from_month_index(year, month, 1312)
    call assert_equals(year, 1960, "year of second station middle month_index")
    call assert_equals(month, 4, "month of second station middle month_index")

    call get_year_month_from_month_index(year, month, 1081)
    call assert_equals(year, 1941, "year of third station first month_index")
    call assert_equals(month, 1, "month of third station first month_index")

    call get_year_month_from_month_index(year, month, 1680)
    call assert_equals(year, 1990, "year of third station last month_index")
    call assert_equals(month, 12, "month of third station last month_index")

    call get_year_month_from_month_index(year, month, 1372)
    call assert_equals(year, 1965, "year of third station middle month_index")
    call assert_equals(month, 4, "month of third station middle month_index")

  end subroutine test_get_year_mo_from_mo_index

  !> Tests the get_month_index() function.
  subroutine test_get_month_index()

    call assert_equals(get_month_index(1901, 1), 601, "month_index of first station first year/month")
    call assert_equals(get_month_index(1950, 12), 1200, "month_index of first station last year/month")
    call assert_equals(get_month_index(1925, 6), 894, "month_index of first station middle year/month")

    call assert_equals(get_month_index(1921, 1), 841, "month_index of second station first year/month")
    call assert_equals(get_month_index(1970, 12), 1440, "month_index of second station last year/month")
    call assert_equals(get_month_index(1960, 4), 1312, "month_index of second station middle year/month")

    call assert_equals(get_month_index(1941, 1), 1081, "month_index of third station first year/month")
    call assert_equals(get_month_index(1990, 12), 1680, "month_index of third station last year/month")
    call assert_equals(get_month_index(1965, 4), 1372, "month_index of third station middle year/month")

  end subroutine test_get_month_index

end module SkylineUtilsTest
