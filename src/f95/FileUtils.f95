!> @brief
!! Generic subroutines related to file handling that may be useful
!! to multiple modules.
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
module FileUtils

  use Logger

  implicit none

contains

  !> Returns a valid file unit that is not currently in use.
  !!
  !! @return A valid, unused file unit.
  function get_available_file_unit() result(file_unit)
    integer :: file_unit
    logical :: is_open
    logical :: is_valid
    integer, parameter :: MIN_UNIT = 7
    integer, parameter :: MAX_UNIT = 99

    ! loop through possible integers until an unused unit is found.
    do file_unit=MIN_UNIT, MAX_UNIT
      inquire(unit=file_unit, exist=is_valid, opened=is_open)
      if(is_valid .and. .not. is_open) then
        return
      end if
    end do

    ! If it didn't return within do loop, then no units available.
    call log_error("FileUtils::get_available_file_unit: "//trim(log_string(MAX_UNIT))//  &
                   " units already open. No unit chosen.")

  end function get_available_file_unit

  !> Determines whether or not the file at the given
  !! location exists.
  !!
  !! @param file_path The file.
  !! @return True if the file exists. Otherwise, false.
  function does_file_exist(file_path) result(exists)
    character(len=*) :: file_path
    logical :: exists
    integer :: file_unit
    integer :: open_status

    file_unit = get_available_file_unit()

    open(unit=file_unit, file=file_path, status='old', iostat=open_status)
    if(open_status /= 0) then
      exists = .false.
    else
      exists = .true.
    endif

    close(file_unit)

  end function does_file_exist

  !> Determines whether or not the directory at the given
  !! location exists.
  !!
  !! @param directory The path to the directory.
  !! @return True if the directory exists. Otherwise, false.
  function does_directory_exist(directory) result(exists)
    character(len=*) :: directory
    logical :: exists

    inquire(file=trim(directory)//"/.", exist=exists)

  end function does_directory_exist

  !> Returns all lines in a file as an array. It does
  !! not trim or format the lines in any way.
  !! 
  !! @param file_path The file from which the lines are
  !!                  being read.
  !! @return An array of strings, each representing a line.
  function get_file_lines(file_path) result(lines_array)
    character(len=*), intent(in) :: file_path
    integer :: file_unit
    character(len=2000) :: line
    character(len=2000), dimension(:), allocatable :: lines_array
    integer :: read_status
    integer :: open_status
    integer :: line_index

    allocate(lines_array(count_file_lines(file_path)))
    file_unit = get_available_file_unit()

    open(unit=file_unit, file=file_path, status='old', iostat=open_status)
    if(open_status > 0) then
      call log_error("FileUtils::get_file_lines: Could not open file to get lines "//trim(file_path)//" status " &
                    //trim(log_string(open_status)))
    endif

    call log_debug("File open: "//trim(file_path)//  &
                   " with unit: "//log_string(file_unit))

    ! Read each line in the file and add to the array
    do line_index=1, size(lines_array)
      read(file_unit, '(a)', iostat=read_status) line
      if (read_status /= 0) exit

      lines_array(line_index) = line
    enddo

    close(file_unit)

  end function get_file_lines
 
  !> Returns the number of records in the specified file, based on the
  !! specified record pattern.
  !!
  !! @param file_path The file for which the records are being counted.
  !! @return The number of records in the file.
  function count_file_lines(file_path) result(line_count)
    character(len=*) :: file_path
    integer :: file_identifier
    integer :: line_count
    integer :: open_status
    integer :: read_status
    character(len=1) :: line

    file_identifier = get_available_file_unit()
    line_count = 0
    open(unit=file_identifier, file=file_path, status='old', iostat=open_status)
    if(open_status > 0) then
      call log_error("FileUtils::count_file_lines: Could not open file to count lines "//trim(file_path)//" status " &
                    //log_string(open_status))
    endif
    
    do
      read(file_identifier, '(a)', iostat=read_status) line
      if (read_status == -1) exit

      line_count = line_count + 1
    enddo

    close(file_identifier)

  end function count_file_lines

  !> Deletes a file. 
  !!
  !! @param[in] file_path The location of the file to delete.
  subroutine delete_file(file_path)
    character(len=*) :: file_path
    integer :: file_identifier
    integer :: open_status

    file_identifier = get_available_file_unit()

    open(unit=file_identifier, file=file_path, status='old', iostat=open_status)

    if (open_status.eq.0) then 
      call log_info("Deleting file "//trim(file_path))
      close(file_identifier, status='delete')
    else
      call log_warn("Unable to delete file "//trim(file_path)//" status " &
                    //log_string(open_status))
    endif

  end subroutine delete_file

  !> Gets the list of file names for a given directory, sorted in
  !! alphabetical order. Does not return full paths, only file names.
  !!
  !! @param[in] directory The path of the directory.
  !! @param[out] file_array The array of filenames in the directory.
  subroutine get_file_list(directory, file_array)
    character(len=*), intent(in) :: directory
    character(len=256), dimension(:), allocatable, intent(out) :: file_array
    character(len=22), parameter :: temp_file = "temp_pha_file_list.txt"

    call system('ls -p '//trim(directory)//' | grep -v / | sort > '//temp_file)

    allocate(file_array(count_file_lines(temp_file)))
    file_array = get_file_lines(temp_file)
    call delete_file(temp_file)

  end subroutine get_file_list

end module FileUtils

!> @file 
!! Contains functionality for generic file handling.
