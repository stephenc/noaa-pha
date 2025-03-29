!> @brief
!! Contains utility subroutines for working with properties, command-line
!! arguments, and other configuration-related functionality.
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
module ConfigurationUtils

  use Logger
  use PropertyReader
  use PropertyParameters

  implicit none

  private :: replace_tokens

contains

  !> Read the argument following the requested flag.
  !! ex. -d yyyymmdd
  !!
  !! @param arg_flag The character expected before the desired argument.
  !! @return The argument passed in with the given flag.
  function read_command_arg(arg_flag) result(arg_value)

    character(len=1024) :: arg_value
    character(len=*) :: arg_flag

    character(len=(len_trim(arg_flag))) :: curr_arg
    integer :: arg_num
    integer :: i
    ! For Lahey compiler, must declare this even though it's a function
    integer :: iargc

    ! Initialize to -1 in case argument does not exist
    arg_num = -1
    arg_value = ""

    ! Loop through all args to find the specified flag.
    ! The desired arg is the next one.
    do i=1, iargc()
      call getarg(i, curr_arg)
      if(trim(curr_arg) .eq. trim(arg_flag)) arg_num = i+1
    end do

    ! Get the arg_value if the flag was found
    if(arg_num > 0) then
      call getarg(arg_num, arg_value)
    else
      print *, "WARN Argument with flag '", trim(arg_flag), "' not supplied."
    end if

    ! Check that the arg_value passed in isn't just the next flag; ex. '-x'
    ! Reset it to an empty string if so.
    if(index(trim(arg_value),'-') == 1) then
      print *, "WARN Flag '", trim(arg_flag), "' passed in without a value."
      arg_value = ""
    end if

  end function read_command_arg

  !> Detokenizes every property from the properties file(s) and updates
  !! the properties list.
  subroutine detokenize_all_properties()

    character(len=1024) :: prop_value
    integer :: i

    ! keys_values is a global array variable in the fortran-commons PropertyReader module
    ! Loop through all the properties in the properties file
    do i=1, size(keys_values)
      prop_value = keys_values(i)%value

      ! Now detokenize all the other properties.
      prop_value = detokenize_property(prop_value)

      ! Store the final property value for later use.
      call update_property(keys_values(i)%key, prop_value)

    end do

  end subroutine detokenize_all_properties

  !> Looks for tokens in a string that represent property keys or command-line args
  !! and replaces those tokens with the property value for each key. Allowed
  !! properties for tokens are pha.element, pha.version, pha.input-data-type,
  !! and pha.path.base. Example of a properly formatted property in a properties file
  !! intended to replace the 'pha.element' property with its value:
  !!   pha.logger.filename = logs/ghcnm-pha.{pha.element}.log
  !!
  !! @param property The string (property value) to be detokenized.
  !! @return parsed_property The string (property value) after its tokens have been replaced.
  function detokenize_property(property) result(parsed_property)

    character(len=*) :: property
    character(len=len(property)) :: parsed_property

    character(len=len(property)) :: prev_parsed_property
    integer :: i

    parsed_property = property
    prev_parsed_property = ""

    ! Detokenize the tokens that represent other properties.
    ! Loop through multiple times to attempt parsing of nested tokens.
    ! Only try 5 times to prevent infinite loop from cross-referenced tokens.
    do i=1, 5
      parsed_property = replace_tokens(parsed_property)
      ! If nothing changed since last time, we're done with this property.
      if(trim(parsed_property) .eq. trim(prev_parsed_property)) exit
      prev_parsed_property = parsed_property
    end do

  end function detokenize_property

  !> Replaces all valid tokens in a string with its corresponding property value or
  !! command-line argument.
  !!
  !! @param property The string (property value) to be parsed for tokens.
  !! @return parsed_property The property after its tokens have been replaced.
  function replace_tokens(property) result(parsed_property)

    character(len=*) :: property
    character(len=len(property)) :: parsed_property

    character(len=MAX_LENGTH) :: prop_key ! MAX_LENGTH is defined in PropertyReader module
    character(len=MAX_LENGTH+2) :: token
    character(len=1024) :: replace_value
    integer :: token_index_begin
    integer :: token_index_end
    integer :: token_length
    integer :: start_index

    character(len=*), parameter :: token_begin = "{"
    character(len=*), parameter :: token_end = "}"
    character(len=*), parameter :: token_arg_prefix = "arg:"

    token_index_begin = 0
    token_index_end = 0
    start_index = 1

    parsed_property = property

    do while (token_index_begin <= len_trim(parsed_property))

      ! Find the next token begin index, if any
      token_index_begin = index(parsed_property(start_index:len(parsed_property)), token_begin)
      if(token_index_begin <= 0) exit
      ! Find the next token end index that comes after the begin index
      token_index_end = index(parsed_property(start_index+token_index_begin:len(parsed_property)), token_end)
      if(token_index_end <= 0) then
        print *, "ERROR Token begin/end mismatch. No matching '",token_end,"' found for token begin '",token_begin,  &
                      "' for property value ",trim(property),"."
        exit
      end if

      ! Parse the property for the token key and get the corresponding replacement value
      token_index_begin = token_index_begin + start_index - 1
      token_index_end = token_index_end + token_index_begin
      prop_key = parsed_property(token_index_begin+len(token_begin):token_index_end-1)

      if(trim(prop_key) .ne. "") then

        if(index(prop_key, token_arg_prefix) > 0) then
          prop_key = prop_key(len(token_arg_prefix)+1:len(prop_key))
          replace_value = trim(read_command_arg("-"//trim(prop_key)))
          ! Replace the arg token in the property with the arg value
          token = token_begin//trim(prop_key)//token_end
          token_length = len_trim(token)+len(token_arg_prefix)
        else
          replace_value = get_property_chars(prop_key)
          ! Replace the key in the property with the replacement value
          token = token_begin//trim(prop_key)//token_end
          token_length = len_trim(token)
        end if

        if(trim(replace_value) /= "") then
          parsed_property = parsed_property(1:token_index_begin-1)//trim(replace_value)//  &
                                parsed_property(token_index_begin+token_length:len_trim(parsed_property))
        end if

      end if

      ! Reset the start index for the next token search (prevents infinite looping)
      start_index = token_index_end + 1

    end do

  end function replace_tokens

  !> Split a string into words separated by a delimeter
  !!
  !! @param str The input string to be split
  !! @param split_str An array of strings split out from the input string
  !! @param delim A 1-character delimeter. Default is "," if nothing passed in.
  subroutine split_string(str, split_str, delim)
    character(len=*) :: str
    character(len=len(str)), dimension(:), intent(out), allocatable :: split_str
    character(len=1), optional :: delim

    character(len=len(str)), dimension(:) :: split_str_temp(len(str))
    character(len=1) :: delim_opt
    integer :: delim_idx
    logical :: more_delims
    integer :: word_count

    if(.not. present(delim)) then
      delim_opt = ","
    else
      delim_opt = delim
    end if

    more_delims = .true.
    word_count = 0

    do while(more_delims)
      delim_idx = index(str, delim_opt)

      if(delim_idx == 0) then
        more_delims = .false.
        if(len_trim(str) > 0) then
          word_count = word_count+1
          split_str_temp(word_count) = trim(adjustl(str))
        end if
      else
        if(len_trim(str(1:delim_idx-1)) > 0) then
          word_count = word_count + 1
          split_str_temp(word_count) = trim(adjustl(str(1:delim_idx-1)))
        end if
        str = str(delim_idx+1:)
      end if
    end do

    allocate(split_str(word_count))
    split_str(1:word_count) = split_str_temp(1:word_count)

  end subroutine split_string

end module ConfigurationUtils

!> @file
!! Contains utility subroutines for working with properties, command-line
!! arguments, and other configuration-related functionality.
