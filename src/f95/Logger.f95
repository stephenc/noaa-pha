!> @brief Provides functionality for writing log messages of different importance 
!! levels to a specified log file and standard out.
!!
!! @details Allowed levels are FATAL, ERROR, WARN, INFO, DEBUG, or TRACE. All log messages 
!! will be printed with a timestamp (local time), log level indicator, and the 
!! specified message. Log levels allow a developer to limit the messages printed 
!! to the log without having to comment them out in the code. For instance, DEBUG 
!! messages can be included in the code but only enabled when troubleshooting is 
!! required.
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
module Logger

implicit none

! Specifying which subroutines should be public
public :: log_init
public :: log_fatal, log_error, log_warn, log_info, log_debug, log_trace
public :: get_log_file, get_log_level

! Setting helper methods to private
private :: write_to_log
private :: log_init_with_options
private :: string_to_level, level_to_string, log_to_upper
private :: current_datetime, current_date
private :: log_string_real, log_string_int, log_string_complex
private :: log_string_logical, log_string_int_1d, log_string_chars_1d

private :: FILE_OUT_LOG, log_file, log_level, do_stdout, do_datestamp
private :: do_rollover, start_datestamp

!> Initializes the Logger with filename, logging level, 
!! and whether or not to also write to stdout.
interface log_init
  !> Implementation of log_init that accepts specified values for options.
  module procedure log_init_with_options
end interface log_init

!> Converts a non-string to a string, so that a user can
!! easily append a non-string into a log message without
!! having to use complicated string formatting.
!! <br/>
!! ex. `call log_info("This number, "//log_string(my_real)//", will print to the log."))`
!!
!! @param val The value to be converted to a string.
!! @return A string representation of the val passed in. 
interface log_string
  module procedure log_string_real
  module procedure log_string_int
  module procedure log_string_complex
  module procedure log_string_logical
  module procedure log_string_chars_1d
  module procedure log_string_int_1d
end interface log_string


! Parameters that should be public
!> Severe errors that cause premature termination.
integer, parameter :: LEVEL_FATAL = 5
!> Other runtime errors or unexpected conditions.
integer, parameter :: LEVEL_ERROR = 4
!> Use of deprecated APIs, poor use of API, 'almost' errors, other runtime
!! situations that are undesirable or unexpected, but not necessarily "wrong".
integer, parameter :: LEVEL_WARN = 3
!> Interesting runtime events (startup/shutdown).
integer, parameter :: LEVEL_INFO = 2
!> Detailed information on the flow through the system.
integer, parameter :: LEVEL_DEBUG = 1
!> More detailed information.
integer, parameter :: LEVEL_TRACE = 0
!> Level was never set or was set to an invalid type. Logger will not work.
integer, parameter :: LEVEL_UNDEFINED = -1

! Parameters that should be private
integer, parameter :: FILE_OUT_LOG = 100

character(len=300), save :: log_file = ''
integer,            save :: log_level = LEVEL_UNDEFINED
logical,            save :: do_stdout = .false.
logical,            save :: do_datestamp = .false.
logical,            save :: do_rollover = .false.
character(len=10),  save :: start_datestamp = ''

contains

!> Sets the logging level and the log filename as well as specifying
!! options for whether or not to also print to stdout, whether to append 
!! a datestamp, and whether to rollover to a new datestamp at midnight 
!! for use by the log subroutines.
!!
!! @param[in] log_filename The filename or path to the log output file.
!! @param[in] min_log_level The minimum log level to print to the log.
!! @param[in] stdout True if log messages should also be printed to stdout.
!! @param[in] datestamp True if log filename should be appended with a datestatmp.
!! @param[in] rollover True if log filename datestamp should roll over to a new file after midnight.
subroutine log_init_with_options(log_filename, min_log_level, stdout, datestamp, rollover)
  character(len=*),  intent(in) :: log_filename
  character(len=*),  intent(in) :: min_log_level
  logical, optional, intent(in) :: stdout
  logical, optional, intent(in) :: datestamp
  logical, optional, intent(in) :: rollover

  log_file = log_filename
  log_level = string_to_level(trim(min_log_level))
  if(present(stdout))    do_stdout = stdout
  if(present(datestamp)) do_datestamp = datestamp
  if(present(rollover))  do_rollover = rollover

  start_datestamp = current_date()

end subroutine log_init_with_options

!> Writes a FATAL message. Use FATAL to log severe errors 
!! that cause premature termination. 
!!
!! @param[in] message The fatal message to be written to the log.
subroutine log_fatal(message)
  character(len=*), intent(in) :: message
  call write_to_log(message, LEVEL_FATAL)
end subroutine log_fatal

!> Writes an ERROR message if the minimum log level
!! specified is ERROR or less. Use ERROR to log runtime 
!! errors or unexpected conditions.  
!! 
!! @param[in] message The error message to be written to the log.
subroutine log_error(message)
  character(len=*), intent(in) :: message
  call write_to_log(message, LEVEL_ERROR)
end subroutine log_error

!> Writes a WARN message only if the minimum log level
!! specified is WARN or lower. Use WARN to log the use of 
!! deprecated APIs, poor use of API, "almost" errors, and other 
!! runtime situations that are undesirable or unexpected but not 
!! necessarily "wrong". 
!! 
!! @param[in] message The warn message to be written to the log.
subroutine log_warn(message)
  character(len=*), intent(in) :: message
  call write_to_log(message, LEVEL_WARN)
end subroutine log_warn

!> Writes an INFO message only if the minimum log level
!! specified is INFO or lower. Use INFO to log interesting 
!! runtime events (startup/shutdown). 
!! 
!! @param[in] message The info message to be written to the log.
subroutine log_info(message)
  character(len=*), intent(in) :: message
  call write_to_log(message, LEVEL_INFO)
end subroutine log_info

!> Writes a DEBUG log message only if the minimum log
!! level specified is DEBUG or lower. Use DEBUG to log detailed 
!! information on the flow through the system. 
!! 
!! @param[in] message The debug message to be written to the log.
subroutine log_debug(message)
  character(len=*), intent(in) :: message
  call write_to_log(message, LEVEL_DEBUG)
end subroutine log_debug

!> Writes a TRACE log message only if the minimum log
!! level specified is TRACE. Use TRACE to log very detailed information. 
!!
!! @param[in] message The trace message to be written to the log.
subroutine log_trace(message)
  character(len=*), intent(in) :: message
  call write_to_log(message, LEVEL_TRACE)
end subroutine log_trace

!> (Private) Writes the message with the current timestamp and level
!! to the configured log file. 
!!
!! @param[in] message The message to be written to the log.
!! @param[in] message_level The level of the message being written.
subroutine write_to_log(message, message_level)
  character(len=*), intent(in) :: message
  integer, intent(in) :: message_level
  character(len=300) :: filename
  integer :: min_level
  logical :: file_exists
  character(len=3) :: file_status
  character(len=len(message)+27) :: log_message
  character(len=19) :: now
  
  filename = get_log_file()
  min_level = get_log_level()

  ! If the log level of the message is lower than the configured
  ! logging level, don't write anything to the log.
  if(min_level > message_level) then
    return
  endif

  ! If log file already exists, append to it.
  ! If it doesn't exist, create a new file.
  inquire(file=filename, exist=file_exists)
  if (file_exists) then
    file_status = 'old'
  else
    file_status = 'new'
  end if

  ! Write to log file, filename specified during init
  now = current_datetime()
  open(unit=FILE_OUT_LOG,file=filename,position='append',status=file_status)
  write(log_message,*) now,' ',level_to_string(message_level),' ',trim(message)
  write(FILE_OUT_LOG,*) trim(log_message)

  ! Only write to stdout if configured to do so
  if (do_stdout) then
    write(*,*) trim(log_message)
  endif
    
  close(FILE_OUT_LOG)

end subroutine write_to_log

!> Gets the current date and time
!! @param current_time The current date and time
function current_datetime() result(now)
  character(19) :: now
  character(8)  :: date
  character(10) :: time
  character(5)  :: zone
  integer, dimension(8) :: values
  
  ! using keyword arguments
  call date_and_time(date, time, zone, values)

  ! format timestamp to be yyyy-mm-dd hh:mm:ss
  ! values are [1=year 2=month 3=day 5=hour 6=minutes 7=seconds]
  write(now, 1) values(1), values(2), values(3), values(5), values(6), values(7)
  1 format(i4,'-',i2.2,'-',i2.2,' ',i2.2,':',i2.2,':',i2.2)

end function current_datetime

!> Gets a string representation of the current date only (not the time)
!! @param today The current date in the form yyyy-mm-dd
function current_date() result(today)
  character(10) :: today
  character(8)  :: date
  character(10) :: time
  character(5)  :: zone
  integer, dimension(8) :: values

  ! using keyword arguments
  call date_and_time(date, time, zone, values)

  ! format date to be yyyy-mm-dd
  ! values are [1=year 2=month 3=day]
  write(today, 1) values(1), values(2), values(3)
  1 format(i4,'-',i2.2,'-',i2.2)
  
end function current_date

!> Gets the name of the log file from configuration with the current date
!! appeanded. This is a cheap way of simulating log4j's RollingFileAppender.
!! Ideally, the current log would just be the base filename and a log file 
!! from a previous day would be renamed to have the date of its last write 
!! appended to the base name.
!!
!! @return The base filename of the log file (without the current date).
function get_log_file() result(log_filename)
  character(len=300) :: log_filename

  if(trim(log_file)=='') then
    write(*,*) "Logger must be initialized with a log filename and a minimum desired " //  &
             & "log level using the log_init subroutine before calling a log subroutine. " //  &
             & "It is missing the log filename. Exiting."
    call exit(1)
  endif

  log_filename = trim(log_file)

  ! If rollover and datestamp is enabled, then append filename with the current date 
  if(do_rollover .and. do_datestamp) then
    log_filename = trim(log_filename)//"."//current_date() ! Append date to log file
  end if

  ! If only datestamp is enabled but not rollover, then append the first
  ! datestamp when this program started running.
  if(do_datestamp .and. .not. do_rollover) then
    log_filename = trim(log_filename)//"."//start_datestamp ! Append date that this Logger was initialized
  end if

end function get_log_file

!> Gets the minimum log level to print to the log.
!!
!! @return The desired minimum log level.
function get_log_level() result(level)
  integer :: level

  level = log_level

  ! Set the level_config value based on what is configured
  ! if it hasn't already been set.
  if(level == LEVEL_UNDEFINED) then
    write(*,*) "Logger must be initialized with a log filename and a minimum desired " //  &
             & "log level using the log_init subroutine before calling a log subroutine. " //  &
             & "It is missing a valid minimum desired log level (TRACE, DEBUG, INFO, " //  &
             & "WARN, ERROR, or FATAL). Exiting."
    call exit(1)
  endif

  ! Return the level_config value.
  level = log_level

end function get_log_level

!> Gets the minimum log level to print to the log, represented 
!! as a human-readable string.
!! 
!! @return The desired minimum log level written as a string.
function get_log_level_chars() result(level)
  character(len=5) :: level
  
  level = level_to_string(get_log_level())

end function get_log_level_chars

!> Gets the string equivalent of the log level.
!!
!! @param level The log level integer.
!! @return The string version of the specified log level.
function level_to_string(level) result(level_string)
  integer :: level
  character(len=5) :: level_string

  if(level == LEVEL_FATAL) then
    level_string = "FATAL"
  elseif(level == LEVEL_ERROR) then
    level_string = "ERROR"
  elseif (level == LEVEL_WARN) then
    level_string = "WARN"
  elseif (level == LEVEL_INFO) then
    level_string = "INFO"
  elseif (level == LEVEL_DEBUG) then
    level_string = "DEBUG"
  elseif (level == LEVEL_TRACE) then
    level_string = "TRACE"
  else
    level_string = "UNDEF"
  endif

end function level_to_string

!> Gets the log level integer constant for the given string.
!! Valid strings include FATAL, ERROR, WARN, INFO, DEBUG, and TRACE,
!! 
!! @param level_string The string representation of the logging level. Case-insensitive.
!! @return The log level integer constant.
function string_to_level(level_string) result(level)
  integer :: level
  character(len=*) :: level_string
  character(len=len(level_string)) :: level_upper

  level_upper = log_to_upper(level_string)

  if(level_upper == "FATAL") then
    level = LEVEL_FATAL
  elseif(level_upper == "ERROR") then
    level = LEVEL_ERROR
  elseif (level_upper == "WARN") then
    level = LEVEL_WARN
  elseif (level_upper == "INFO") then
    level = LEVEL_INFO
  elseif (level_upper == "DEBUG") then
    level = LEVEL_DEBUG
  elseif (level_upper == "TRACE") then
    level = LEVEL_TRACE
  else
    level = LEVEL_UNDEFINED
  endif

end function

!> Convenience function to convert a string to all upper case.
!!
!! @param str_orig The character array to be converted
!! @return The character array after being converted to upper case
function log_to_upper(str_orig) result(str_upper)

  character(len=*), intent(in) :: str_orig
  character(len=len(str_orig)) :: str_upper
  integer :: i,j

  do i = 1, len(str_orig)
    j = iachar(str_orig(i:i))
    if (j>= iachar("a") .and. j<=iachar("z") ) then
      str_upper(i:i) = achar(iachar(str_orig(i:i))-32)
    else
      str_upper(i:i) = str_orig(i:i)
    end if
  end do

end function log_to_upper

!> Converts a real to a string
!! @todo preserve the number of decimal places
function log_string_real(val) result (str)
  real :: val
  character(len=200) :: str

  write(str,'(f10.5)') val
  str = trim(adjustl(str))
  
end function log_string_real

!> Converts an integer to a string
function log_string_int(val) result (str)
  integer :: val
  character(len=200) :: str

  write(str,'(i10)') val
  str = trim(adjustl(str))

end function log_string_int

!> Converts a complex number to a string
function log_string_complex(val) result (str)
  complex :: val
  character(len=200) :: str

  write(str,*) val
  str = trim(adjustl(str))
  
end function log_string_complex

!> Converts a logical to a string
function log_string_logical(val) result (str)
  logical :: val
  character(len=5) :: str

  if(val) then
    str = 'True'
  else
    str = 'False'
  endif
  
end function log_string_logical

!> Converts a 1-D chararacters array to a string
function log_string_chars_1d(val) result (str)
  character(len=*), dimension(:) :: val
  character(len=2570) :: str
  integer :: i

  ! Convert array to comma-separated strings
  write(str,'(100a)') (trim(adjustl(val(i)))//', ', i=1, size(val))
  ! Remove the last comma
  str = str(1:len_trim(str)-1)

end function log_string_chars_1d

!> Converts a 1-D integer array to a string
function log_string_int_1d(val) result (str)
  integer, dimension(:) :: val
  character(len=2570) :: str
  integer :: i

  ! Convert array to comma-separated values
  write(str,'(100a)') (trim(log_string_int(val(i)))//', ', i=1, size(val))
  ! Remove the last comma
  str = str(1:len_trim(str)-1)
 
end function log_string_int_1d

end module Logger

!> @file 
!! Contains the Logger module which provides and easy way to write
!! log messages of different levels to a log file and stdout.
