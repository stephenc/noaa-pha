!> @brief Unit tests for ConfigurationUtils module.
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
module ConfigurationUtilsTest

  use UnitTest
  use ConfigurationUtils

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine configuration_utils_tests()
 
    call test_detokenize_property()
    call test_split_string()

  end subroutine configuration_utils_tests

  !> Tests the detokenize_property function.
  subroutine test_detokenize_property()
    character(len=1024) :: orig_property
    character(len=1024) :: expected_property

    ! Test typical property values with one or more tokens
    orig_property = "this/is/a/{pha.element}/filepath/with/tokens"
    expected_property = "this/is/a/tmax/filepath/with/tokens"
    call assert_equals(detokenize_property(orig_property), expected_property, "Replacing single pha.element token")

    orig_property = "this/is/a/{pha.element}/filepath/with/{pha.input-data-type}/tokens"
    expected_property = "this/is/a/tmax/filepath/with/raw/tokens"
    call assert_equals(detokenize_property(orig_property), expected_property, "Replacing two tokens in a property")

    orig_property = "this is a string without tokens"
    expected_property = "this is a string without tokens"
    call assert_equals(detokenize_property(orig_property), expected_property, "Detokenizing a string without tokens")

    orig_property = "multiple {pha.test.foo-foo-bar} string replacement {pha.test.foo-bar}"
    expected_property = "multiple foo-foo-bar string replacement foo-bar"
    call assert_equals(detokenize_property(orig_property), expected_property, "Detokenizing a complex foo-bar test string")

    ! Reads a property from the property file that references a later property as a token.
    ! We still expect that to work. Order of properties shouldn't matter.
    expected_property = "pre-foo"
    call assert_equals(get_property_chars("pha.test.pre-foo"), expected_property,  &
              "Read in property that reference a later property")

    ! Reads a property from the property file that references a later property with multiple tokens as a token.
    ! We don't expect that to work but it shouldn't break the run.
    expected_property = "pre-foo-foo-bar"
    call assert_equals(get_property_chars("pha.test.pre-foo-foo-bar"), expected_property,  &
              "Read in property that references a later property with multiple tokens")

    ! Reads a property from the property file that references a later property with multiple tokens as a token.
    ! We don't expect that to work but it shouldn't break the run.
    expected_property = "post-foo-foo-bar"
    call assert_equals(get_property_chars("pha.test.post-foo-foo-bar"), expected_property,  &
              "Read in property that references an earlier property with multiple tokens")

    ! Reads a cross-referencing property from the property file that would be impossible to parse fully
    ! yet we still expect it attempt the parse, return the original value, and not crash the system.
    call assert_true(get_property_chars("pha.test.cross-foo") .ne. "",  &
              "Read a cross-referenced property from the properties file. It can't be fully parsed but shouldn't break the run.")

    ! Testing a mix of property tokens and arg tokens
    ! This test is dependent on the Makefile 'test' target passing in -d 20160316
    orig_property = "{pha.input-data-type}{arg:d}{pha.element}{arg:q}"
    expected_property = "raw20160316tmax{arg:q}"
    call assert_equals(detokenize_property(orig_property), expected_property,  &
              "Detokenize a property with a mix of property and arg tokens.")

    ! Testing multiple command-line arguments without any other characters
    ! This test is dependent on the Makefile 'test' target passing in -d 20160316
    orig_property = "{arg:d}{arg:d}"
    expected_property = "2016031620160316"
    call assert_equals(detokenize_property(orig_property), expected_property,  &
              "Detokenize a property with only 2 command-line arg tokens in a row.")

    ! Testing multiple command-line arguments with other characters
    ! This test is dependent on the Makefile 'test' target passing in -d 20160316
    orig_property = "{arg:d}{arg:q}and{arg:d}"
    expected_property = "20160316{arg:q}and20160316"
    call assert_equals(detokenize_property(orig_property), expected_property,  &
              "Detokenize a property with 2 valid command-line arg tokens and 1 invalid.")

    ! Testing single command-line argument with other characters directly from the unit test properties file
    ! This test is dependent on the Makefile 'test' target passing in -d 20160316
    expected_property = "use-20160316"
    call assert_equals(get_property_chars("pha.test.use-arg"), expected_property,  &
              "Detokenize a property with a command-line arg token.")

    ! Testing multiple command-line arguments withother characters directly from the properties file
    ! This test is dependent on the Makefile 'test' target passing in -d 20160316
    expected_property = "use-20160316-20160316"
    call assert_equals(get_property_chars("pha.test.use-two-args"), expected_property,  &
              "Detokenize a property with two command-line arg tokens.")

    ! Testing badly formatted property values
    orig_property = "{arg:dwhatever goes here"
    call assert_equals(detokenize_property(orig_property), orig_property,  &
              "Attempt to detokenize badly formatted property")

    orig_property = "}{arg:dwhatever goes here"
    call assert_equals(detokenize_property(orig_property), orig_property,  &
              "Attempt to detokenize badly formatted property")

    orig_property = "arg:d{whatever goes here"
    call assert_equals(detokenize_property(orig_property), orig_property,  &
              "Attempt to detokenize badly formatted property")

    ! Test a property with an arg token that is longer than the standard single character flag
    call assert_equals(get_property_chars("pha.test.long-flag"), "test", "Detokenize a property with a long arg token")

  end subroutine test_detokenize_property

  !> Test the split_string subroutine
  subroutine test_split_string()
    character(len=100) :: input
    character(len(input)), dimension(:), allocatable :: result

    input = "how| are|yo,u|today | whatevs"
    call split_string(input, result, "|")
    call assert_equals(5, size(result), "split string into 5 words")
    call assert_equals("how", result(1), "split string - first string in array")
    call assert_equals("are", result(2), "split string - second string in array")
    call assert_equals("yo,u", result(3), "split string - third string in array")
    call assert_equals("today", result(4), "split string - fourth string in array")
    call assert_equals("whatevs", result(5), "split string - fifth string in array")

    input = "howareyoutoday?,"
    call split_string(input, result)
    call assert_equals(1, size(result), "split string into 1 word with extra delim at end")
    call assert_equals("howareyoutoday?", result(1), "split string - first and only word")

    input = "    "
    call split_string(input, result)
    call assert_equals(0, size(result), "split string that is an empty string")

    input = ",,,   , "
    call split_string(input, result)
    call assert_equals(0, size(result), "split string that is only delims")

  end subroutine test_split_string

end module ConfigurationUtilsTest
