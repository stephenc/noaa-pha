module ReadInputFilesTest

  use UnitTest
  use ReadInputFiles

  implicit none

contains

  !> Call each test subroutine for this module so that the main test
  !! program can conveniently call only this one subroutine.
  subroutine read_input_files_tests

!    call test_set_skyline_variables()

  end subroutine read_input_files_tests

  ! Test the set_skyline_variables subroutine for each properties file
  subroutine test_set_skyline_variables()
    use CommonVariables, only: skyline_months

    type(StationNeighborPair), dimension(:,:), allocatable :: neighbors

    call read_neighbors(neighbors)
    call set_skyline_variables(neighbors)
    call assert_equals(skyline_months, 33396, "Getting total skyline_months from files")

  end subroutine test_set_skyline_variables

end module ReadInputFilesTest
