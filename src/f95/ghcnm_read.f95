module ghcnm_read

! Version 1.0, 08/12/2014

! National Climatic Data Center (NCDC)

! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC DOMAIN
! AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE.  THEY ARE FURNISHED 
! "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS INSTRUMENTALITIES, 
! OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY, EXPRESS OR IMPLIED, AS TO
! THE USEFULNESS OF THE SOFTWARE AND DOCUMENTATION FOR ANY PURPOSE. THEY
! ASSUME NO RESPONSIBILITY (1) FOR THE USE OF THE SOFTWARE AND DOCUMENTATION;
! OR (2) TO PROVIDE TECHNICAL SUPPORT TO USERS.

use stratus ! stratus version 1.4

implicit none

!*******************************************************************************
! Module Declarations

integer, public, parameter :: GHCNM_NUMBER_OF_ELEMENTS=4
integer, public, parameter :: GHCNM_BEGIN_YEAR=1690
integer, public, parameter :: GHCNM_TMAX=1
integer, public, parameter :: GHCNM_TAVG=2
integer, public, parameter :: GHCNM_TMIN=3
integer, public, parameter :: GHCNM_PRCP=4
real, public, parameter :: GHCNM_MISSING=-9999.9
integer, public, parameter :: GHCNM_MISSING_INTEGER=-9999
character(len=04), dimension(1:GHCNM_NUMBER_OF_ELEMENTS) :: GHCNM_ELEMENT=(/"TMAX","TAVG","TMIN","PRCP"/)
character(len=04) :: char_element

type ghcnm_structure
  integer :: number_of_stations
  integer, allocatable, dimension(:,:) :: first_year
  integer, allocatable, dimension(:,:) :: last_year
  real, allocatable, dimension(:) :: longitude  
  real, allocatable, dimension(:) :: latitude
  real, allocatable, dimension(:) :: elevation
  real, allocatable, dimension(:,:,:,:) :: data
  character(len=01), allocatable, dimension(:,:,:,:) :: data_measurement
  character(len=01), allocatable, dimension(:,:,:,:) :: data_quality
  character(len=01), allocatable, dimension(:,:,:,:) :: data_source
  character(len=11), allocatable, dimension(:) :: station_id
  character(len=30), allocatable, dimension(:) :: station_name
end type ghcnm_structure

contains

!*******************************************************************************

subroutine get_ghcnm(metadatafile,datafile,data_structure,begin_year,end_year)

!***************************************
! get_ghcnm Declarations

character(len=*), intent(in) :: metadatafile
character(len=*), intent(in) :: datafile
type(ghcnm_structure), intent(out) :: data_structure
integer, intent(in), optional :: begin_year
integer, intent(in), optional :: end_year
integer :: begin
integer :: counter
integer :: element
integer :: end
integer :: month
integer :: year
integer :: station
integer :: station_number
integer :: status
integer, dimension(1:12) :: value
character(len=01), dimension(1:12) :: flag1
character(len=01), dimension(1:12) :: flag2
character(len=01), dimension(1:12) :: flag3
character(len=08) :: date
real :: elevation
real :: latitude
real :: longitude
character(len=11) :: station_id
character(len=11) :: previous_station_id
character(len=30) :: station_name
logical :: file_found
logical :: station_not_found

!***************************************
! get_ghcnm Metadata file processing

inquire(file=metadatafile,exist=file_found)
if (.not.(file_found)) stop "ERROR: ghcnm_read module, metadatafile does not exist"

data_structure%number_of_stations=0
open(unit=22,file=metadatafile,status='old')
do
  read(22,*,IOSTAT=status) 
  if (status==END_OF_FILE) exit
  data_structure%number_of_stations=data_structure%number_of_stations+1
enddo
close(22)

allocate(data_structure%longitude(1:data_structure%number_of_stations),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"
allocate(data_structure%latitude(1:data_structure%number_of_stations),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"
allocate(data_structure%elevation(1:data_structure%number_of_stations),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"
allocate(data_structure%station_id(1:data_structure%number_of_stations),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"
allocate(data_structure%station_name(1:data_structure%number_of_stations),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"

counter=0
open(unit=22,file=metadatafile,status='old')
do
  read(22,'(a11,f9.4,1x,f9.4,f7.1,1x,a30)',IOSTAT=status) station_id,latitude,longitude,elevation,station_name
  if (status==END_OF_FILE) exit

  counter=counter+1
  data_structure%station_id(counter)=station_id
  data_structure%longitude(counter)=longitude
  data_structure%latitude(counter)=latitude
  data_structure%elevation(counter)=elevation
  data_structure%station_name(counter)=station_name 

enddo
close(22)

!***************************************
! get_ghcnm data file processing

if (present(end_year)) then
  end=end_year
else
  call date_and_time(DATE=date)
  read(date(1:4),'(i4)') end
endif
   
if (present(begin_year)) then
  if (begin_year > end) stop "ERROR: ghcnm module, begin_year > end_year in subroutine get_ghcnm"
  begin=begin_year
else
  begin=GHCNM_BEGIN_YEAR
  if (begin_year > end) stop "ERROR: ghcnm module, begin_year > end_year in subroutine get_ghcnm"
endif

allocate(data_structure%data(1:12,begin:end,1:GHCNM_NUMBER_OF_ELEMENTS,1:data_structure%number_of_stations),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"
allocate(data_structure%data_measurement(1:12,begin:end,1:GHCNM_NUMBER_OF_ELEMENTS,1:data_structure%number_of_stations),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"
allocate(data_structure%data_quality(1:12,begin:end,1:GHCNM_NUMBER_OF_ELEMENTS,1:data_structure%number_of_stations),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"
allocate(data_structure%data_source(1:12,begin:end,1:GHCNM_NUMBER_OF_ELEMENTS,1:data_structure%number_of_stations),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"
allocate(data_structure%first_year(1:data_structure%number_of_stations,1:GHCNM_NUMBER_OF_ELEMENTS),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"
allocate(data_structure%last_year(1:data_structure%number_of_stations,1:GHCNM_NUMBER_OF_ELEMENTS),STAT=status)
if (status > MEMORY_ERROR) stop "ERROR: ghcnm_read module, memory error"

data_structure%data(:,:,:,:)=GHCNM_MISSING
data_structure%data_measurement(:,:,:,:)=" "
data_structure%data_quality(:,:,:,:)=" "
data_structure%data_source(:,:,:,:)=" "
data_structure%first_year(:,:)=(GHCNM_MISSING_INTEGER)*(-1)
data_structure%last_year(:,:)=GHCNM_MISSING_INTEGER


inquire(file=datafile,exist=file_found)
if (.not.(file_found)) stop "ERROR: ghcnm_read module, datafile does not exist"

previous_station_id=""
open(unit=22,file=datafile,status='old')
do
  read(22,22,IOSTAT=status) station_id,year,char_element,(value(month),flag1(month),flag2(month),flag3(month),month=1,12)
  if (status==END_OF_FILE) exit
  if (status>0) stop "ERROR: reading in ghcnm data file!"

  if (year < begin .or. year > end ) cycle

  if (station_id/=previous_station_id) then 
    station_not_found=.true.
    do station=1,data_structure%number_of_stations
      if (station_id==data_structure%station_id(station)) then
        station_number=station
        previous_station_id=station_id
        station_not_found=.false.
        exit
      endif
    enddo
    if (station_not_found) cycle
  endif

  element=0
  if (char_element=="TMAX") element=GHCNM_TMAX
  if (char_element=="TAVG") element=GHCNM_TAVG
  if (char_element=="TMIN") element=GHCNM_TMIN
  if (char_element=="PRCP") element=GHCNM_PRCP
  if (element==0) cycle   ! skip, unrecognized element

  data_structure%first_year(station_number,element)=min(data_structure%first_year(station_number,element),year)
  data_structure%last_year(station_number,element)=max(data_structure%last_year(station_number,element),year)

  do month=1,12
    if (value(month)/=GHCNM_MISSING_INTEGER) data_structure%data(month,year,element,station_number)=real(value(month))
    data_structure%data_measurement(month,year,element,station_number)=flag1(month)
    data_structure%data_quality(month,year,element,station_number)=flag2(month)
    data_structure%data_source(month,year,element,station_number)=flag3(month)
  enddo

enddo
close(22)
22 format(a11,i4,a4,12(i5,3a1))

do element=1,GHCNM_NUMBER_OF_ELEMENTS
  where (data_structure%first_year(:,element)==GHCNM_MISSING_INTEGER*(-1)) &
         data_structure%first_year(:,element)=GHCNM_MISSING_INTEGER
enddo

end subroutine get_ghcnm

end module ghcnm_read
