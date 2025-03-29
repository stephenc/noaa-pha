!> @brief Program to run a test comparing actual PHA output to expected
!! output.
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
program PHATestOutput

  use Logger
  use PropertyReader
  use UnitTest
  use FileUtils
  use ConfigurationUtils
  use PropertyParameters

  implicit none

  real :: timer
  integer :: total
  integer :: successes
  integer :: failures

  character(len=1024) :: props_str
  character(len=1024), dimension(:), allocatable :: prop_files_array

  ! Test output for global data
  props_str = trim(read_command_arg('-p'))//',./build/ghcnm-pha.test.properties'
  call split_string(props_str, prop_files_array)
  call test_output_data(prop_files_array)

  ! Print out results of all tests.
  call report_test_results(total, successes, failures)

  ! Measure runtime of unit tests
  call cpu_time(timer)
  write(*,1) timer
  1 format ('File comparison tests completed in',f6.2,' seconds.')

  ! If any tests failed, exit with error.
  if(failures>0) then
    call exit(9)
  endif

contains

  !> Compares all lines of all produced files to the corresponding
  !! expected files.
  !!
  !! @param prop_files_array The array of properties files to use for this test.
  subroutine test_output_data(prop_files_array)
    character(len=*), dimension(:), intent(in) :: prop_files_array

    logical :: do_run_neighbors
    logical :: do_run_main

    character(len=256) :: neighbors_dist_output
    character(len=256) :: neighbors_dist_expected

    character(len=256) :: neighbors_corr_output
    character(len=256) :: neighbors_corr_expected

    character(len=256) :: main_output_dir
    character(len=256) :: main_expected_dir
    character(len=256), dimension(:), allocatable :: output_files
    integer :: i

    call properties_init(prop_files_array)
    call detokenize_all_properties()
    call log_init(get_property_chars(PROP_LOG_FILENAME),     &
                  get_property_chars(PROP_LOG_LEVEL),        &
                  get_property_logical(PROP_LOG_STDOUT),     &
                  get_property_logical(PROP_LOG_DATESTAMP),  &
                  get_property_logical(PROP_LOG_ROLLOVER))

    do_run_neighbors = get_property_logical("pha.do-run-neighbors")
    if(do_run_neighbors) then
      neighbors_dist_output = get_property_chars(PROP_PATH_NEIGH_DISTANCE)
      neighbors_dist_expected = get_property_chars("pha.path.neighbors-distance-expected")
      neighbors_corr_output = get_property_chars(PROP_PATH_NEIGH_CORRELATION)
      neighbors_corr_expected = get_property_chars("pha.path.neighbors-correlation-expected")

      call compare_files(neighbors_dist_output, neighbors_dist_expected)
      call compare_files(neighbors_corr_output, neighbors_corr_expected)

    else
      call log_info("Skipping neighbors test")
    endif ! End do_run_neighbors

    do_run_main = get_property_logical("pha.do-run-main")
    if(do_run_main) then

      ! Get the "network" directories, which are the non-USHCN data files
      main_output_dir = get_property_chars(PROP_PATH_ELEMENT_DATA_OUT)
      main_expected_dir = get_property_chars("pha.path.network-output-expected-dir")

      call get_file_list(main_output_dir, output_files)
      do i=1, size(output_files)
        call compare_files(trim(main_output_dir)//trim(output_files(i)), &
                           trim(main_expected_dir)//trim(output_files(i)))
      enddo

    else
      call log_info("Skipping main test")
    endif ! End do_run_main

  end subroutine test_output_data

  !> Given two files, loop through each line of file1 and compare to the
  !! same line of file2. Assert that each pair of lines is an exact match.
  !!
  !! @param[in] file1 The first file to compare.
  !! @param[in] file2 The second file to compare.
  subroutine compare_files(file1, file2)
    character(len=*), intent(in) :: file1
    character(len=*), intent(in) :: file2
    character(len=2000), dimension(:), allocatable :: file_lines1
    character(len=2000), dimension(:), allocatable :: file_lines2
    integer :: i ! file line index

    call log_info("Comparing files: "//trim(file1)//" AND "//trim(file2))

    allocate(file_lines1(count_file_lines(file1)))
    file_lines1 = get_file_lines(file1)
    allocate(file_lines2(count_file_lines(file2)))
    file_lines2 = get_file_lines(file2)

    call assert_equals(size(file_lines1), size(file_lines2), &
          "Files have different number of lines "//file1//" "//file2)

    do i=1, size(file_lines1)
      call assert_equals(trim(file_lines1(i)), trim(file_lines2(i)), &
                    "Mismatched lines for "//trim(file1)//" and "//trim(file2)//" line "//trim(log_string(i)))
    end do

  end subroutine compare_files

end program PHATestOutput
