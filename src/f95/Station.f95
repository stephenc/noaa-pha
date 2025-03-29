!> @brief
!! A type which represents a physical climate station.
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
module StationType

  use Logger
  use AlgorithmParameters

  implicit none

  private :: new_Station_Hcn, station_equals, station_array_equals, station_composite_ids_equal

  !> A type containing metadata about a climate station.
  type Station
    integer :: station_index
    character(len=11) :: station_id
    real :: latitude
    real :: longitude
    character(len=11), dimension(1:4) :: composite_ids
    logical(1) :: is_hcn
  end type Station

  !> Creates a new station instance.
  interface new_Station
    module procedure new_Station
    module procedure new_Station_Hcn
  end interface new_Station

  !> Test equality of Station instances.
  interface operator (.eq.)
    module procedure station_equals
    module procedure station_array_equals
  end interface

  !> A constant containing the value representing a non-existant 
  !! composite ID.
  character(len=11), parameter :: EMPTY_COMPOSITE_ID = '-----------'

contains

  !> Creates a new station without composite IDs.
  !!
  !! @param station_id The unique ID for this station.
  !! @param station_index Where in the input and output files this
  !!                          station appears.
  !! @param latitude The latitude of this station.
  !! @param longitude The longitude of this station.
  !! @return The Station instance being created.
  function new_Station(station_id, station_index, latitude, longitude) &
               result(this)
    type (Station) :: this
    character(len=11) :: station_id
    integer :: station_index
    real :: latitude
    real :: longitude
    character(len=11), dimension(:) :: composite_ids(4)

    ! Create empty composite_ids
    composite_ids(1:4) = EMPTY_COMPOSITE_ID

    ! Call constructor with composite_ids
    this = new_Station_Hcn(station_id, station_index, latitude,&
                         longitude, composite_ids)
    this%is_hcn = FALSE

  end function new_Station

  !> Creates a new station with composite IDs.
  !!
  !! @param station_id The unique ID for this station.
  !! @param station_index Where in the input and output files this
  !!                          station appears.
  !! @param latitude The latitude of this station.
  !! @param longitude The longitude of this station.
  !! @param composite_ids (USHCN stations only) Up to 4 station IDs
  !!                          of stations that were used to fill in 
  !!                          sparse data for this station.
  !! @return The Station instance being created.
  function new_Station_Hcn(station_id, station_index, latitude, &
               longitude, composite_ids) result(this)
    type (Station) :: this
    character(len=11) :: station_id
    integer :: station_index
    real :: latitude
    real :: longitude
    character(len=11), dimension(:) :: composite_ids(4)

    ! Validate lat and lon. If invalid, return without setting any values.
    if(.not. station_valid_latitude(latitude)) then
      call log_error('StationType::new_Station_Hcn: Invalid latitude for station '//station_id// &
                     ': '//log_string(latitude))
      return
    endif

    if(.not. station_valid_longitude(longitude)) then
      call log_error('StationType::new_Station_Hcn: Invalid longitude for station '//station_id// &
                     ': '//log_string(longitude))
      return
    endif
    
    ! Set values for Station  
    this%station_index = station_index
    this%station_id = station_id
    this%latitude = latitude
    this%longitude = longitude
    this%composite_ids = composite_ids
    this%is_hcn = TRUE

  end function new_Station_Hcn

  !> Returns a "null" Station. This is handy for
  !! setting array values to "null" so that junk isn't
  !! accidentally stored in the array.
  !!
  !! @return A Station with empty/zero values and
  !!         not_null set to false.
  function null_Station() result(this)
    type (Station) :: this

    this = new_Station("           ", ZERO, ZERO_REAL, ZERO_REAL)

  end function null_Station

  !> Outputs a string representation of a Station instance.
  !!
  !! @param this The Station instance.
  !! @return The string representation of this Station. 
  function station_string(this) result(station_str)
    type(Station) :: this
    character(len=145) :: station_str
    character(len=11) :: test_composite_id

    ! If the station object is null, skip the rest
    if(this%station_index == 0) then
      station_str = "[Station] NULL"
      return
    endif

    ! Create station string
    write(station_str, '(a,a,a,i5,a,f6.2,a,f7.2)') '[Station] id: ',this%station_id, &
          ' index: ',this%station_index,' lat: ',this%latitude,  &
          ' lon: ',this%longitude

    ! Include composite IDs if they exist for this station
    test_composite_id = this%composite_ids(1)
    if(test_composite_id .ne. EMPTY_COMPOSITE_ID .and. trim(test_composite_id) .ne. '') then
      write(station_str, '(a,a)') trim(station_str)//' composite ids: ', &
        trim(this%composite_ids(1))//' '//trim(this%composite_ids(2))//' '// &
        trim(this%composite_ids(3))//' '//trim(this%composite_ids(4))
    endif

  end function station_string

  !> Validates a latitude value.
  !!
  !! @param latitude The latitude value to be validated.
  !! @return True if the latitude is valid. Otherwise false.
  function station_valid_latitude(latitude) result(is_valid)
    real :: latitude
    logical :: is_valid

    is_valid = .not. (latitude .gt. 90.0 .or. latitude .lt. -90.0)

  end function station_valid_latitude

  !> Validates a longitude value.
  !!
  !! @param longitude The longitude value to be validated.
  !! @return True if the longitude is valid. Otherwise false.
  function station_valid_longitude(longitude) result(is_valid)
    real :: longitude
    logical :: is_valid

    is_valid =  .not. (longitude .gt. 360.0 .or. longitude .lt. -180.0)

  end function station_valid_longitude

  !> Determines if a record in a file represents a USHCN station.
  !! Assumes it is USHCN if it begins with "USH"
  !!
  !! @param record The file record to check.
  !! @result True if the record represents a USHCN station.
  !!         Otherwise false.
  function is_hcn_record(record) result(is_hcn)
    character(len=*) :: record
    logical :: is_hcn

    if(record(1:3) .eq. "USH") then
      is_hcn = .true.
    else
      is_hcn = .false.
    endif

  end function is_hcn_record

  !> Tests the equality of two Station instances.
  !!
  !! @param station1 The first Station instance to compare.
  !! @param station2 The second Station instance to compare.
  !! @return True if the Station instances have equal values
  !!         for their attributes. Otherwise false.
  function station_equals(station1, station2) result(is_equal)
    type(Station), intent(in) :: station1
    type(Station), intent(in) :: station2
    logical :: is_equal

    if((station1%station_id .eq. station2%station_id) .and. &
       (station1%station_index .eq. station2%station_index) .and. &
       (station1%latitude .eq. station2%latitude) .and. &
       (station1%longitude .eq. station2%longitude) .and. &
       (station_composite_ids_equal &
           (station1%composite_ids, station2%composite_ids))) then
      is_equal = .true.
    else
      is_equal = .false.
    endif  

  end function station_equals
 
  !> Tests the equality of two lists of composite IDs.
  !! 
  !! @param comp_ids1 The first array of composite IDs to compare.
  !! @param comp_ids2 The second array of composite IDs to compare.
  !! @return True if each value in the arrays are equal and in the
  !!         same order. Otherwise false.
  function station_composite_ids_equal(comp_ids1, comp_ids2) result(is_equal)
    character(len=11), dimension(:) :: comp_ids1
    character(len=11), dimension(:) :: comp_ids2
    logical :: is_equal
    integer :: i  

    is_equal = .true.

    ! If they are not the same size, then not equal
    if(size(comp_ids1) /= size(comp_ids2)) then
      is_equal = .false.
    ! Or if each element is not the same, then not equal
    else
      do i = 1, size(comp_ids1)
        if(.not. comp_ids1(i) .eq. comp_ids2(i)) then
          is_equal = .false.
        endif
      enddo
    endif
  
  end function station_composite_ids_equal

  !> Tests the equality of two arrays of Station instances.
  !! 
  !! @param station_array1 The first array of Station instances
  !!                       to compare.
  !! @param station_array2 The second array of Station instances
  !!                       to compare.
  !! @return True if the arrays are the same length and contain equal
  !!         Station instances in the same order. Otherwise false.
  function station_array_equals(station_array1, station_array2) &
      result(is_equal)
    type(Station), dimension(:), intent(in) :: station_array1
    type(Station), dimension(:), intent(in) :: station_array2
    logical :: is_equal
    integer :: i

    if(size(station_array1) /= size(station_array2)) then
      is_equal = .false.
      return
    endif

    do i=1, size(station_array1)
      if(.not. station_array1(i) .eq. station_array2(i)) then
        is_equal = .false.
        return
      endif
    end do

    is_equal = .true.

  end function station_array_equals

end module StationType

!> @file
!! The file containing the Station type and any related
!! subroutines.
