!> @brief Unit tests for FileUtils module.
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
module FileUtilsTest

  use UnitTest
  use FileUtils

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine file_utils_tests()
 
    call test_get_available_file_unit()
    call test_does_file_exist()
    call test_does_directory_exist()
    call test_count_file_lines()
    call test_get_file_lines()
    call test_delete_file()
    call test_get_file_list()

  end subroutine file_utils_tests

  !> Tests the get_available_file_unit() function to ensure that
  !! it will not return a file unit that is already in use.
  subroutine test_get_available_file_unit()
    integer :: unit1
    integer :: unit2
 
    unit1 = get_available_file_unit()
    open(unit=unit1)
    unit2 = get_available_file_unit()
    open(unit=unit2)

    call assert_true(unit1 /= unit2, "file units should be different.")

    close(unit1)
    close(unit2)

  end subroutine test_get_available_file_unit

  !> Tests the does_file_exist function.
  subroutine test_does_file_exist()
    call assert_true(does_file_exist('build/ghcnm_20150121.inv'), "Testing existence of known good file.")
    call assert_false(does_file_exist('made_up_file'), "Testing existence of non-existent file.")
  end subroutine test_does_file_exist

  !> Tests the does_directory_exist function.
  subroutine test_does_directory_exist()
    call assert_true(does_directory_exist('src/main'), "Testing existence of known good path.")
    call assert_false(does_directory_exist('made/up/path'), "Testing existence of non-existent path.")
  end subroutine test_does_directory_exist

  !> Tests the count_lines_in_file function.
  subroutine test_count_file_lines()
    integer :: line_count

    line_count = count_file_lines('build/ghcnm_20150121.inv')
    call assert_equals(7280, line_count, 'Counting lines in ghcnm_20150121.inv')

    line_count = count_file_lines('build/empty_file.txt')
    call assert_equals(0, line_count, 'Counting lines in empty file')

  end subroutine test_count_file_lines

  !> Tests the get_file_lines function.
  subroutine test_get_file_lines()
    character(len=2000), dimension(:), allocatable :: file_lines
    character(len=256) :: file_path
    character(len=300) :: expected_line

    file_path = "./src/test/resources/data/test-station-meta-v3.txt"
    allocate(file_lines(count_file_lines(file_path)))
    file_lines = get_file_lines(file_path)

    expected_line = "USH00018323  31.8075  -85.9722  165.2 AL TROY" //&
      "                            USC00018323 ----------- ----------- ----------- +6"
    call assert_equals(expected_line, file_lines(1), "first file line")

    expected_line = "10160525000  34.8000    5.7300   87.0 BISKRA"// &
      "                           91U   91HIxxno-9A 3MED. GRAZING    B"
    call assert_equals(expected_line, file_lines(size(file_lines)), "last file line")

    expected_line = "10160518000  35.3000   -1.3500   68.0 BENI-SAF"// &
      "                        103R   -9HIDECO 1x-9WARM CROPS      B"
    call assert_equals(expected_line, file_lines(5), "middle file line")

  end subroutine test_get_file_lines

  !> Tests the delete_file function.
  subroutine test_delete_file()
    character(len=15), parameter :: file_path = "dummy.txt"
    logical :: file_exists
    integer :: file_unit

    ! Create a dummy file
    file_unit = get_available_file_unit()
    open (unit=file_unit, file=file_path)
    write(file_unit, *) "Test file"
    close(file_unit)

    ! Check that file does exist
    inquire(file=file_path, exist=file_exists)
    if(.not. file_exists) then 
      call fail("Dummy file for testing delete not created.")
    endif

    ! call delete_file(file_path)
    call delete_file(file_path)

    ! Check that file does not exist
    inquire(file=file_path, exist=file_exists)
    call assert_false(file_exists, "File should have been deleted.")
    
  end subroutine test_delete_file

  subroutine test_get_file_list()
    character(len=256) :: directory
    character(len=256), dimension(:), allocatable :: file_array
    character(len=256), dimension(8) :: expected_file_array

    expected_file_array = &
                         (/ "empty_file.txt            ",  &
                            "ghcnm_20150121.dat        ",  &
                            "ghcnm_20150121.inv        ",  &
                            "test-station-corr.txt     ",  &
                            "test-station-meta-v3.txt  ",  &
                            "test-station-meta-v4.txt  ",  &
                            "ushcn52i.northam.meta.tmax",  &
                            "ushcnv1.inv               " /)
    directory = "./src/test/resources/data"
    call get_file_list(directory, file_array)
    call assert_equals(file_array, expected_file_array, "getting filenames from directory "//directory)

  end subroutine test_get_file_list

end module FileUtilsTest
