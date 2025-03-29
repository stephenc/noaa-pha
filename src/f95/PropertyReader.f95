!> @brief Provides functionality for reading a text file containing 
!! properties with the format `key = value`, each separated by a new line. 
!!
!! @details Keys are unique.
!! If multiple entries contain the same key, later values will replace
!! earlier values. This can be handy if you want to use a common 
!! properties file for default values, but pass in a run-specific 
!! properties file with differing values for some keys to overwrite
!! the default properties.
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
module PropertyReader

implicit none

public :: properties_init
public :: get_property_chars, get_property_int, get_property_real,   &
          get_property_logical, props_list_print
public :: update_property

!! Subroutines for internal storage and retrieval of properties.
private :: props_list_init, props_list_add, props_list_get, props_list_update,    &
           props_list_contains_key
!! Initialization subroutines.
private :: properties_init_file, properties_init_filearray
!! Other helper functions.
private :: count_props_in_file, count_props_in_all_files,            &
           store_props_in_all_files, store_props_in_file, prop_to_lower,   &
           line_is_property, trim_whitespace
!! KeyValue type for internal property data
private :: new_KeyValue

!> Maximum length of strings for keys and values in properties files
integer, parameter   :: MAX_LENGTH = 240

!> A type to store a single key-value pair, representing a property.
type KeyValue
  character(len=MAX_LENGTH) :: key
  character(len=MAX_LENGTH) :: value
end type KeyValue

!> The current index in the properties array, used when adding properties
integer :: props_index

!> Stores the KeyValue instances
type(KeyValue), dimension(:), allocatable :: keys_values

!> Reads in the key-value pairs from the property file(s) specified.
interface properties_init
  ! Reads properties from a single file.
  module procedure properties_init_file
  ! Reads properties from an array of files.
  module procedure properties_init_filearray
end interface properties_init

contains

  !> Creates a new instance of a KeyValue.
  !!
  !! @param key The key use to identify the value.
  !! @param value The value associated with this key.
  !! @return The new KeyValue instance.
  function new_KeyValue(key, value) result(this)
    type(KeyValue) :: this
    character(len=*) :: key
    character(len=*) :: value
 
    this%key = key
    this%value = value

  end function new_KeyValue

  !> Converts a KeyValue instance to a string.
  !!
  !! @param key_value The KeyValue instance to convert.
  !! @return A string representation of the KeyValue.
  function key_value_string(key_value) result(kv_string)
    type(KeyValue) :: key_value
    character(len=484) :: kv_string

    if(trim(key_value%key) /= '' .and. trim(key_value%value) /= '') then
      write(kv_string,*) trim(trim_whitespace(key_value%key))//" = "//trim(trim_whitespace(key_value%value))
    else
      kv_string = ""
    end if

    kv_string = adjustl(kv_string)

  end function key_value_string

  !> Reads all the properties from a specified file and stores the key-value
  !! pairs for look-up.
  !!
  !! @param[in] file_path The path to the file containing the key-value properties.
  subroutine properties_init_file(file_path)
    character(len=*), intent(in) :: file_path
    ! Put the single file_path string into an array and call read_properties.
    character(len=len(file_path)), dimension(1) :: file_path_array
    file_path_array(1) = file_path
    call properties_init_filearray(file_path_array)
   
  end subroutine properties_init_file

  !> Reads all the properties from an array of specified files and stores
  !! the key-value pairs in the look-up table.
  !!
  !! @param[in] file_path_array The array of paths to the files containing the key-value properties.
  subroutine properties_init_filearray(file_path_array)
    character(len=*), dimension(:), intent(in) :: file_path_array
    character(7) :: record_pattern = '(a)=(a)'
    integer :: props_count

    ! Get the total number of properties in all files to read
    props_count = count_props_in_all_files(file_path_array, record_pattern)
    ! Initialize the props_list with the correct size
    call props_list_init(props_count)
    ! Read the properties in all files and store.
    call store_props_in_all_files(file_path_array, record_pattern)

  end subroutine properties_init_filearray

  !> Gets a character array (string) property by reading the value from the
  !! key-value pair, based on the specified key.
  !!
  !! @param key The name of the property
  !! @return The value of the character array (string) property
  function get_property_chars(key) result(val)
    character(len=*) :: key
    character(len=MAX_LENGTH) :: val

    ! Get the property from the property list.
    val = trim(props_list_get(key))

  end function get_property_chars

  !> Gets an integer property by reading the value from the
  !! key-value pair, based on the specified key.
  !! 
  !! @param key The name of the property
  !! @return The value of the integer property
  function get_property_int(key) result(val)
    character(len=*) :: key
    character(len=MAX_LENGTH) :: val_str
    integer :: val

    val_str = trim(props_list_get(key))
    read(val_str, *) val
 
  end function get_property_int

  !> Gets  property by reading the value from the
  !! key-value pair, based on the specified key.
  !! 
  !! @param key The name of the property
  !! @return The value of the real number property
  function get_property_real(key) result(val)
    character(len=*) :: key
    character(len=MAX_LENGTH) :: val_str
    real :: val

    val_str = trim(props_list_get(key))
    read(val_str, *) val

  end function get_property_real

  !> Gets a logical (boolean) property by reading the value from the
  !! key-value pair, based on the specified key.
  !!
  !! @param key The name of the property
  !! @return The value of the logical property
  function get_property_logical(key) result(val)
    character(len=*) :: key
    character(len=MAX_LENGTH) :: val_str
    logical :: val

    ! Get the value, trim, and make all lower case        
    val_str = prop_to_lower(trim(props_list_get(key)))

    ! Compare against possible case-insensitive "true" values
    if((val_str .eq. 'true') .or.     &
       (val_str .eq. '.true.') .or.   &
       (val_str .eq. 't') .or.        &
       (val_str .eq. 'yes') .or.      &
       (val_str .eq. '1')) then 
       val = .true.
    else if((val_str .eq. 'false') .or. &
       (val_str .eq. '.false.') .or.    &
       (val_str .eq. 'f') .or.          &
       (val_str .eq. 'no') .or.         &
       (val_str .eq. '0')) then
       val = .false.
    else
      write(*,*) "FATAL Expected logical value true/false, t/f, 1/0, or yes/no. " //  &
                   "Bad value assigned to property " // trim(key) // "."
      stop 1
    endif
          
  end function get_property_logical

  !> Updates an existing key-value pair with a new value.
  !!
  !! @param key The name of the property
  !! @param value The new value of the property
  subroutine update_property(key, value)
    character(len=*), intent(in)  :: key
    character(len=*), intent(in)  :: value
   
    call props_list_update(key, value)

  end subroutine update_property

  !> For all files in the array of file names, and with the specified 
  !! record pattern, count all records in all files.
  !! 
  !! @param file_path_array The array of file paths.
  !! @param record_pattern The regex string that constitues a single record.
  !! @return The total number of records found in all files in the array.
  function count_props_in_all_files(file_path_array, record_pattern) result(props_count)
    character(len=*), dimension(:) :: file_path_array
    character(len=1000) :: file_path
    character(len=*) :: record_pattern
    integer :: props_count
    integer :: file_idx

    ! Loop through each file in the array and count records.
    props_count = 0
    do file_idx = 1, size(file_path_array)
      file_path = trim(file_path_array(file_idx))
      props_count = props_count + count_props_in_file(file_path, record_pattern)
    enddo

  end function count_props_in_all_files

  !> Returns the number of records in the specified file, based on the
  !! specified record pattern.
  !!
  !! @param file_path The file for which the records are being counted.
  !! @param record_pattern The regex pattern used to define each record.
  !! @return The number of records in the file.
  function count_props_in_file(file_path, record_pattern) result(props_count)
    character(len=*) :: file_path
    character(len=*) :: record_pattern
    integer :: file_identifier = 20
    integer :: props_count 
    integer :: file_status
    character(len=1000) :: line

    props_count = 0
    open(unit=file_identifier, file=file_path, IOSTAT=file_status, status='old')
    ! Test if file can be found
    if (file_status/=0) then
      write(*,*) "ERROR Cannot read file to count properties: "//trim(file_path)
      return
    endif

    do
      read(file_identifier, record_pattern, IOSTAT=file_status) line
      if (file_status==-1) exit ! then we're done reading

      ! Increment count only if it's a property record
      if(line_is_property(line)) then
        props_count = props_count+1
      endif

    enddo
    close(file_identifier)

  end function count_props_in_file

  !> For all files in the array of file names, and with the specified
  !! record pattern, store the key-value pair properties in the props_list.
  !! NOTE: This subroutine is nearly identical to count_props_in_all_files
  !! since they are just looping through the array of files to do a
  !! specific thing. The loop cannot be consolidated easily because
  !! the properties total count has to be complete before the properties
  !! are read into the props_list, in order to initalize the props_list first.
  !!
  !! @param[in] file_path_array The array of file paths.
  !! @param[in] record_pattern The regex string that constitues a single record.
  subroutine store_props_in_all_files(file_path_array, record_pattern)
    character(len=*), dimension(:) :: file_path_array
    character(len=1000) :: file_path
    character(len=*) :: record_pattern
    integer :: file_idx

    ! Loop through each file in the array and count records.
    do file_idx = 1, size(file_path_array)
      file_path = trim(file_path_array(file_idx))
      call store_props_in_file(file_path, record_pattern)
    enddo

  end subroutine store_props_in_all_files

  !> For a specified file, store all the key-value pair properties
  !! in the props_list.
  !!
  !! @param[in] file_path The array of file paths.
  !! @param[in] record_pattern The regex string that constitues a single record.
  subroutine store_props_in_file(file_path, record_pattern)
    character(len=*), intent(in) :: file_path
    character(len=*), intent(in) :: record_pattern
    integer, parameter :: file_identifier = 30
    character(len=1000) :: line
    character(len=MAX_LENGTH) :: prop_key
    character(len=MAX_LENGTH) :: prop_val
    integer :: file_status
    integer :: line_idx
    integer :: eq_index

    open(unit=file_identifier, file=file_path, IOSTAT=file_status, status='old')
    ! Test if file can be found
    if (file_status/=0) then
      write(*,*) "ERROR Cannot read file to count properties: "//trim(file_path)
      return
    endif

    line_idx = 0

    ! Loop through each record in the current file and add each
    ! key-value pair to the property list.
    do
      read(file_identifier, record_pattern, IOSTAT=file_status) line
      if (file_status==-1) exit ! then we're done reading

      line_idx=line_idx+1

      ! Ignore comments that start with # symbol
      line = adjustl(line)
      if(.not. line_is_property(line)) then
        cycle
      endif

      eq_index = index(line, '=')
      prop_key = line(:eq_index-1) ! Get the substring before the '='
      prop_val = line(eq_index+1:) ! Get the substring after the '='
      if(props_list_contains_key(prop_key)) then
        call props_list_update(trim(trim_whitespace(prop_key)), trim(trim_whitespace(prop_val)))
      else
        call props_list_add(trim(trim_whitespace(prop_key)), trim(trim_whitespace(prop_val)))
      endif
    enddo
    close(file_identifier)

  end subroutine store_props_in_file

  !> Determines whether or not a line in a file is a
  !! key-value pair representing a property. It looks
  !! for the presence of '=' and absence of '#'.
  !!
  !! @param line The string line to check.
  !! @return True if it is a property, otherwise false.
  function line_is_property(line) result(is_prop)
    character(len=*) :: line
    logical :: is_prop

    if(index(line, '=')>0 .and. index(line, '#')==0) then
      is_prop = .true.
    else
      is_prop = .false.
    endif

  end function line_is_property

  !> Initialize an empty table.
  subroutine props_list_init(num_items)
    integer, intent(in)  :: num_items

    props_index = 0
    if(allocated(keys_values)) deallocate(keys_values)
    allocate(keys_values(num_items))
    keys_values = new_KeyValue("","")

  end subroutine props_list_init

  !> Add a new key-value pair to the table.
  subroutine props_list_add(key, value)
    character(len=*), intent(in)  :: key
    character(len=*), intent(in)  :: value

    props_index = props_index + 1
    if(props_index > size(keys_values, 1))   &
      write(*,*) "ERROR property list is already full"

    keys_values(props_index) = &
        new_KeyValue(trim(trim_whitespace(key)), trim(trim_whitespace(value)))

  end subroutine props_list_add

  !> Set a new value for an existing key-value pair in the table.
  subroutine props_list_update(key, value)
    character(len=*), intent(in)  :: key
    character(len=*), intent(in)  :: value
    integer                   :: local_index
    logical                   :: found

    found = .false.

    do local_index = 1,size(keys_values,1)
      if(trim(keys_values(local_index)%key) == trim(trim_whitespace(key))) then
        if(keys_values(local_index)%value /= value) then
          keys_values(local_index)%value = value
          write(*,*) "Overwriting property '"//trim(key)//"' with the value '"//trim(value)//"'."
        endif
        found = .true.
      endif
    enddo

    if(.not.found) then
      write(*,*) "Unknown key"
    endif

  end subroutine props_list_update

  !> Retrieve a value from the table for the specified key.
  function props_list_get(key) result(value)
    character(len=*)      :: key
    character(MAX_LENGTH) :: value
    integer               :: local_index
    logical               :: found

    found = .false.
    do local_index = 1,size(keys_values,1)
      if(trim(keys_values(local_index)%key) == trim(key)) then
        value = trim(keys_values(local_index)%value)
        found = .true.
      endif
    enddo

    if(.not.found) then
      write(*,*) "FATAL in property list: Attempt to retrieve value " //  &
                          "for unknown key: "//trim(key)
      stop 1
    endif

  end function props_list_get
  
  !> Determines whether the props_list contains a specified key.
  !!
  !! @param key The key to check for.
  !! @return True if the props_list contains the key, otherwise false.
  function props_list_contains_key(key) result(has_key)
    character(len=*) :: key
    logical :: has_key
    integer :: local_index
    has_key = .false.
    
    do local_index = 1, size(keys_values, 1)
      if(trim(trim_whitespace(keys_values(local_index)%key)) == trim(trim_whitespace(key))) then
        has_key = .true.
      endif
    enddo
 
  end function props_list_contains_key

  !> Print the key-value pairs in the table.
  subroutine props_list_print()
    integer  :: local_index

    write(*,*) "Contents of the property list:"
    do local_index = 1,size(keys_values)
      write(*,*) trim(key_value_string(keys_values(local_index)))
    enddo
    
  end subroutine props_list_print

  !> Convenience function to convert a string to all lower case.
  !!
  !! @param str_orig The character array to be converted
  !! @return str_lower The character array after being converted to lower case
  function prop_to_lower(str_orig) result(str_lower)

    character(len=*), intent(in) :: str_orig
    character(len=len(str_orig)) :: str_lower
    integer :: i,j

    do i = 1, len(str_orig)
      j = iachar(str_orig(i:i))
      if (j>= iachar("A") .and. j<=iachar("Z") ) then
        str_lower(i:i) = achar(iachar(str_orig(i:i))+32)
      else
        str_lower(i:i) = str_orig(i:i)
      end if
    end do

  end function prop_to_lower

  !> Trims tab characters from the beginning and end of a string.
  !!
  !! @param str The character string to trim.
  !! @return The trimmed character string.
  function trim_whitespace(str) result(trimmed_str)
    character(len=*) :: str
    character(len=len(str)) :: trimmed_str
    integer :: str_len
    integer :: char_idx
    integer, parameter :: IACHAR_TAB = 9
    integer, parameter :: IACHAR_SPACE = 32

    trimmed_str = trim(str)
    str_len = len_trim(str)

    ! Test if the beginning characters are the tab position in
    ! the ASCII collating sequence. 
    do char_idx=1, str_len
      ! If it's a tab, replace with a space for easy trim below.
      if(IACHAR(str(char_idx:char_idx)) == IACHAR_TAB) then
        trimmed_str(char_idx:char_idx) = ' '
      ! If it's a blank space, just leave it and cycle through.
      ! Otherwise, it's a non-blank, non-tab character, so we're done.
      else if(IACHAR(str(char_idx:char_idx)) /= IACHAR_SPACE) then
        exit
      end if
    end do

    ! Do the same for the last character
    do char_idx=str_len, 1, -1
      ! If it's a tab, replace with a space for easy trim below.
      if(IACHAR(str(char_idx:char_idx)) == IACHAR_TAB) then
        trimmed_str(char_idx:char_idx) = ' '
      ! If it's a blank space, just leave it and cycle through.
      ! Otherwise, it's a non-blank, non-tab character, so we're done.
      else if(IACHAR(str(char_idx:char_idx)) /= IACHAR_SPACE) then
        exit
      end if
    end do

    ! Now that tabs are converted to blanks, use trim() to remove.
    trimmed_str = trim(adjustl(trimmed_str))

  end function trim_whitespace

end module PropertyReader

!> @file 
!! Contains the PropertyReader module, which provides functionality for 
!! reading properties from a file.
