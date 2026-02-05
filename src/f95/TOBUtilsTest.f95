!> @brief Unit tests for TOBUtils module.
!!
!! Tests the public interface: decode_obtime, tobchg, get_timezone.
!! Test data files for integration-level verification live in
!! src/test/resources/data/tob/.
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
module TOBUtilsTest

  use UnitTest
  use TOBUtils

  implicit none

contains

  !> Call each test subroutine for this module.
  subroutine tob_utils_tests()

    call test_decode_obtime_trid()
    call test_decode_obtime_hr()
    call test_decode_obtime_numeric()
    call test_decode_obtime_special_codes()
    call test_decode_obtime_9x_pattern()
    call test_tobchg_sr()
    call test_tobchg_ss()
    call test_tobchg_rs()
    call test_get_timezone()

  end subroutine tob_utils_tests

  ! ===========================================================================
  !  decode_obtime tests
  ! ===========================================================================

  !> TRID = traditional -> code 30
  subroutine test_decode_obtime_trid()
    call assert_equals(30, decode_obtime('TRID'), "TRID -> 30")
  end subroutine test_decode_obtime_trid

  !> xxHR pattern: positions 1:2 give the hour code.
  !! "07HR" -> 7,  "14HR" -> 14,  "00HR" -> 24 (midnight).
  subroutine test_decode_obtime_hr()
    call assert_equals( 7, decode_obtime('07HR'), "07HR -> 7")
    call assert_equals(14, decode_obtime('14HR'), "14HR -> 14")
    call assert_equals(24, decode_obtime('00HR'), "00HR -> 24 (midnight)")
    call assert_equals(12, decode_obtime('12HR'), "12HR -> 12")
  end subroutine test_decode_obtime_hr

  !> Standard numeric obs_time: positions 3:4 are the 2-digit hour code.
  !! "0007" -> 7,  "0014" -> 14,  "0000" -> 24.
  subroutine test_decode_obtime_numeric()
    call assert_equals( 7, decode_obtime('0007'), "0007 -> 7")
    call assert_equals(14, decode_obtime('0014'), "0014 -> 14")
    call assert_equals(24, decode_obtime('0000'), "0000 -> 24 (midnight)")
    call assert_equals(12, decode_obtime('0012'), "0012 -> 12")
    call assert_equals( 1, decode_obtime('0001'), "0001 -> 1")
  end subroutine test_decode_obtime_numeric

  !> Special keyword codes in positions 3:4.
  subroutine test_decode_obtime_special_codes()
    ! RS / VR / VA -> 26 (relative sunrise)
    call assert_equals(26, decode_obtime('00RS'), "00RS -> 26")
    call assert_equals(26, decode_obtime('00VR'), "00VR -> 26")
    call assert_equals(26, decode_obtime('00VA'), "00VA -> 26")
    ! SR -> 27 (sunrise)
    call assert_equals(27, decode_obtime('00SR'), "00SR -> 27")
    ! SS / PM -> 28 (sunset)
    call assert_equals(28, decode_obtime('00SS'), "00SS -> 28")
    call assert_equals(28, decode_obtime('00PM'), "00PM -> 28")
    ! Unknown codes -> 99
    call assert_equals(99, decode_obtime('0099'), "0099 -> 99")
    call assert_equals(99, decode_obtime('00UN'), "00UN -> 99")
    call assert_equals(99, decode_obtime('00DE'), "00DE -> 99")
  end subroutine test_decode_obtime_special_codes

  !> 9x09 / 9x19 exceptions: these extract positions 3:4 (normal path)
  !! rather than 2:3.  "9909" -> "09" -> 9.  "9919" -> "19" -> 19.
  !! Other 9x_9 patterns extract positions 2:3.
  subroutine test_decode_obtime_9x_pattern()
    call assert_equals( 9, decode_obtime('9909'), "9909 -> 9 (exception)")
    call assert_equals(19, decode_obtime('9919'), "9919 -> 19 (exception)")
    ! "9709" is a generic 9x_9 pattern (not 9909/9919): extracts pos 2:3 = "70"
    ! -> 70 > 30, not 99 -> warn + 99
    call assert_equals(99, decode_obtime('9709'), "9709 -> 99 (70 out of range)")
  end subroutine test_decode_obtime_9x_pattern

  ! ===========================================================================
  !  tobchg tests  (modifies tob in-place)
  ! ===========================================================================

  !> SR (code 27) resolves to the sunrise hour for each season.
  !! Jan/Feb/Mar/Oct -> 7;  Apr/Aug/Sep -> 6;  May/Jun/Jul -> 5;  Nov/Dec -> 8
  subroutine test_tobchg_sr()
    integer :: tob

    ! Winter months -> 8
    tob = 27;  call tobchg(11, tob);  call assert_equals(8, tob, "SR Nov -> 8")
    tob = 27;  call tobchg(12, tob);  call assert_equals(8, tob, "SR Dec -> 8")
    tob = 27;  call tobchg( 1, tob);  call assert_equals(8, tob, "SR Jan -> 8")
    ! Early spring / late fall -> 7
    tob = 27;  call tobchg( 2, tob);  call assert_equals(7, tob, "SR Feb -> 7")
    tob = 27;  call tobchg( 3, tob);  call assert_equals(7, tob, "SR Mar -> 7")
    tob = 27;  call tobchg(10, tob);  call assert_equals(7, tob, "SR Oct -> 7")
    ! Spring / late summer -> 6
    tob = 27;  call tobchg( 4, tob);  call assert_equals(6, tob, "SR Apr -> 6")
    tob = 27;  call tobchg( 8, tob);  call assert_equals(6, tob, "SR Aug -> 6")
    tob = 27;  call tobchg( 9, tob);  call assert_equals(6, tob, "SR Sep -> 6")
    ! Peak summer -> 5
    tob = 27;  call tobchg( 5, tob);  call assert_equals(5, tob, "SR May -> 5")
    tob = 27;  call tobchg( 6, tob);  call assert_equals(5, tob, "SR Jun -> 5")
    tob = 27;  call tobchg( 7, tob);  call assert_equals(5, tob, "SR Jul -> 5")
  end subroutine test_tobchg_sr

  !> SS (code 28) resolves to the sunset hour for each season.
  !! Jan/Nov/Dec -> 17;  Feb/Mar/Oct -> 18;  Apr/Aug/Sep -> 19;  May/Jun/Jul -> 20
  subroutine test_tobchg_ss()
    integer :: tob

    tob = 28;  call tobchg( 1, tob);  call assert_equals(17, tob, "SS Jan -> 17")
    tob = 28;  call tobchg(11, tob);  call assert_equals(17, tob, "SS Nov -> 17")
    tob = 28;  call tobchg(12, tob);  call assert_equals(17, tob, "SS Dec -> 17")
    tob = 28;  call tobchg( 2, tob);  call assert_equals(18, tob, "SS Feb -> 18")
    tob = 28;  call tobchg( 3, tob);  call assert_equals(18, tob, "SS Mar -> 18")
    tob = 28;  call tobchg(10, tob);  call assert_equals(18, tob, "SS Oct -> 18")
    tob = 28;  call tobchg( 4, tob);  call assert_equals(19, tob, "SS Apr -> 19")
    tob = 28;  call tobchg( 8, tob);  call assert_equals(19, tob, "SS Aug -> 19")
    tob = 28;  call tobchg( 9, tob);  call assert_equals(19, tob, "SS Sep -> 19")
    tob = 28;  call tobchg( 5, tob);  call assert_equals(20, tob, "SS May -> 20")
    tob = 28;  call tobchg( 6, tob);  call assert_equals(20, tob, "SS Jun -> 20")
    tob = 28;  call tobchg( 7, tob);  call assert_equals(20, tob, "SS Jul -> 20")
  end subroutine test_tobchg_ss

  !> RS (code 26) resolves to SR (Apr-Oct) or SS (Nov-Mar), then to the
  !! corresponding hourly code.
  subroutine test_tobchg_rs()
    integer :: tob

    ! Apr-Oct: RS -> SR -> sunrise hour
    tob = 26;  call tobchg( 7, tob);  call assert_equals(5, tob, "RS Jul -> SR -> 5")
    tob = 26;  call tobchg( 4, tob);  call assert_equals(6, tob, "RS Apr -> SR -> 6")
    tob = 26;  call tobchg(10, tob);  call assert_equals(7, tob, "RS Oct -> SR -> 7")
    ! Nov-Mar: RS -> SS -> sunset hour
    tob = 26;  call tobchg( 1, tob);  call assert_equals(17, tob, "RS Jan -> SS -> 17")
    tob = 26;  call tobchg(11, tob);  call assert_equals(17, tob, "RS Nov -> SS -> 17")
    tob = 26;  call tobchg( 3, tob);  call assert_equals(18, tob, "RS Mar -> SS -> 18")
  end subroutine test_tobchg_rs

  ! ===========================================================================
  !  get_timezone tests
  ! ===========================================================================

  !> Longitude -> US time-zone index.
  !! 1=Eastern (lon > -86), 2=Central (-103 < lon <= -86),
  !! 3=Mountain (-115 < lon <= -103), 4=Pacific (lon <= -115).
  subroutine test_get_timezone()
    ! Eastern: lon > -86
    call assert_equals(1, get_timezone(-75.0), "lon -75 -> Eastern (1)")
    call assert_equals(1, get_timezone(-85.9), "lon -85.9 -> Eastern (1)")
    ! Central: -103 < lon <= -86
    call assert_equals(2, get_timezone(-86.0), "lon -86 -> Central (2) (boundary)")
    call assert_equals(2, get_timezone(-100.0),"lon -100 -> Central (2)")
    ! Mountain: -115 < lon <= -103
    call assert_equals(3, get_timezone(-103.0),"lon -103 -> Mountain (3) (boundary)")
    call assert_equals(3, get_timezone(-110.0),"lon -110 -> Mountain (3)")
    ! Pacific: lon <= -115
    call assert_equals(4, get_timezone(-115.0),"lon -115 -> Pacific (4) (boundary)")
    call assert_equals(4, get_timezone(-122.4),"lon -122.4 -> Pacific (4)")
  end subroutine test_get_timezone

end module TOBUtilsTest
