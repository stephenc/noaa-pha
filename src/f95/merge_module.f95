module merge_module

implicit none 

!******************************************************************************
! merge_module.f95
! 
! **DESCRIPTION**
! This program, written in FORTRAN, are subroutines used in the main merge 
! program (merge_main.f95). A description of each subroutine is provided below
! 
! **COPYRIGHT**
! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC
! DOMAIN AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE. THEY ARE
! FURNISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
! INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
! EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE AND
! DOCUMENTATION FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR
! THE USE OF THE SOFTWARE AND DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL
! SUPPORT TO USERS.
!
! **REVISION HISTORY**
! 5/5/2014: For version 1.0.0 release of Databank
!
!******************************************************************************

! Define GLOBAL VARIABLES
integer  :: ioerror             ! IOERROR stat for opening / reading a file
character(len=1024) :: readline ! line currently being read in           
logical :: file_exists          ! boolean that checks if file exists
integer :: counter              ! generic incrimental counter
integer :: line_counter         ! counter for line position in file
integer :: element              ! current element position

! Define GLOBAL PARAMETERS (self explanatory) 
integer, parameter :: number_of_elements=3  
integer, parameter :: tmax=1
integer, parameter :: tmin=2
integer, parameter :: tavg=3
integer, parameter :: number_of_analysis=2
integer, parameter :: txn=1
integer, parameter :: tvg=2
real, parameter :: missing=-9999.0
integer, parameter :: high_miss = 9999
integer, parameter :: low_miss = -9999
integer, parameter :: dp = selected_real_kind(15,50)
real(dp), parameter :: PI=3.141592653589793

!Derived Data Type For Master/Candidate Data
type databank
  character(len=30), dimension(:), allocatable :: id
  character(len=8), dimension(:), allocatable :: key
  character(len=30), dimension(:), allocatable :: name
  character(len=20), dimension(:), allocatable :: country
  character(len=43), dimension(:,:,:), allocatable :: stage2_flag
  character(len=8), dimension(:,:,:,:), allocatable :: source_flag
  real, dimension(:), allocatable :: latitude
  real, dimension(:), allocatable :: longitude
  real, dimension(:), allocatable :: elevation
  integer, dimension(:,:), allocatable :: year_start
  integer, dimension(:,:), allocatable :: year_end
  integer :: number_of_stations
  real, dimension(:,:,:,:), allocatable :: data
end type databank

!Derived Data Type For CDF Lookup Table
type lookup_table
  integer, allocatable, dimension(:) :: min_month
  integer, allocatable, dimension(:) :: max_month
  real, allocatable, dimension(:) :: min_metric
  real, allocatable, dimension(:) :: max_metric
  real, allocatable, dimension(:) :: h1
  real, allocatable, dimension(:) :: h2
  integer :: max_nonmissing
end type lookup_table

!Derived Data Type For Blacklist Data
type blacklist
  character(len=30), dimension(:), allocatable :: input_source
  character(len=30), dimension(:), allocatable :: input_id
  character(len=30), dimension(:), allocatable :: input_name
  real, dimension(:), allocatable :: input_latitude
  real, dimension(:), allocatable :: input_longitude
  real, dimension(:), allocatable :: input_elevation
  character(len=30), dimension(:), allocatable :: decision
  character(len=30), dimension(:), allocatable :: final_name
  real, dimension(:), allocatable :: final_latitude
  real, dimension(:), allocatable :: final_longitude
  real, dimension(:), allocatable :: final_elevation
  integer :: max_blacklist
end type blacklist

contains

!*******************************************************************************
! **SUBROUTINE NAME**
! ID test
!
! **DESCRIPTION**
! Compare candidate / target ID's
!*******************************************************************************
subroutine id_test(SOURCE,MERGED_ID,CANDIDATE_ID,RESULT)

character(len=*), intent(in) :: SOURCE         ! Name of the source canidate station belongs to
character(len=*), intent(in) :: MERGED_ID      ! Full ID associated with merged/target station
character(len=*), intent(in) :: CANDIDATE_ID   ! Full ID associated with candidate station
logical, intent (out) :: RESULT                ! Boolean determining if an id match was made

character(len=30) :: converted_MERGED_ID    ! Merged/Target ID with control characters removed
character(len=30) :: converted_CANDIDATE_ID ! Candidate ID with control characters removed
integer :: length_CANDIDATE_ID              ! Length of converted_CANDIDATE_ID
integer :: length_MERGED_ID                 ! Length of converted_MERGED_ID

character(len=30) :: hadisd_id      ! HadISD ID's have 2 id's (First 6 digits USAF, last 5 WBAN)
character(len=30) :: hadisd_missing ! Missing indicator for id (999999 for USAF, 99999 for WBAN)
integer :: hadisd_length            ! length of id need for looping (6 USAF, 5 WBAN)

integer :: ii          ! counter for do loop 

RESULT = .false.
! If merged/target station is originally a GHCN-D station (source #01), pick off the id number embedded within the ID
if(trim(adjustl(MERGED_ID(1:2))) == "01") then

  ! Pick off network code of GHCN-D id, if COOP (C), WBAN, (W), or WMO (M), then we know which ID to compare
  select case (trim(adjustl(MERGED_ID(6:6))))
    case ("C")
      ! COOP ID, which is last 6 digits of GHCN-D ID
      converted_MERGED_ID=trim(adjustl(MERGED_ID(9:))) 
    case ("M" , "W")
      ! WMO/WBAN ID, which is last 5 digits of GHCN-D ID
      converted_MERGED_ID=trim(adjustl(MERGED_ID(10:)))
    case default
      ! Pick off entire ID (minus country code)
      converted_MERGED_ID=trim(adjustl(MERGED_ID(7:))) 
  end select

else

  ! ID is not GHCN-D, pick off the entire ID
  converted_MERGED_ID=trim(adjustl(MERGED_ID(4:)))      

endif

! Pick off entire candidate ID for comparison
converted_CANDIDATE_ID=trim(adjustl(CANDIDATE_ID(4:))) 

! Get length of the two ID's, for looping purposes
length_MERGED_ID=len_trim(converted_MERGED_ID)       
length_CANDIDATE_ID=len_trim(converted_CANDIDATE_ID) 

! Any realistic ID needs to be at least 5 characters in length
if(length_CANDIDATE_ID >= 5 .and. length_MERGED_ID >= 5) then

  ! First, If the source is from HadISD, then we have to compare with 3 ID's 
  if(trim(adjustl(SOURCE)) == "hadisd") then   
    do counter=1,3
      if(counter==1)then     ! WMO ID, First 5 digits
        hadisd_id=trim(adjustl(converted_CANDIDATE_ID(1:5))) 
        hadisd_length=5 
        hadisd_missing="99999"
      elseif(counter==2)then ! USAF ID, First 6 digits
        hadisd_id=trim(adjustl(converted_CANDIDATE_ID(1:6)))
        hadisd_length=6 
        hadisd_missing="999999"
      else                   ! WBAN ID, Last 5 digits
        hadisd_id=trim(adjustl(converted_CANDIDATE_ID(7:11))) 
        hadisd_length=5 
        hadisd_missing="99999"
      endif

      if(hadisd_id /= hadisd_missing) then
        do ii=1,len_trim(converted_MERGED_ID) 

          if (hadisd_id == converted_MERGED_ID(ii:ii+(hadisd_length-1))) then
            RESULT = .true.
          endif

        enddo
      endif

    enddo      

  else
    ! Source is not from HadISD, so only one comparison needed
    
    ! Compare Candidate with Merged
    do ii=1,len_trim(converted_MERGED_ID) 

      if (converted_CANDIDATE_ID == converted_MERGED_ID(ii:ii+(length_CANDIDATE_ID-1))) then
        RESULT = .true.
      endif

    enddo
   
    ! Compare Merged with Candidate (only if still false)  
    if(.not. RESULT) then 
      do ii=1,len_trim(converted_CANDIDATE_ID) 

        if (converted_MERGED_ID == converted_CANDIDATE_ID(ii:ii+(length_MERGED_ID-1))) then
          RESULT = .true.
        endif

      enddo
    endif
  endif
endif

end subroutine id_test

!*******************************************************************************
! **SUBROUTINE NAME**
! file_recs
!
! **DESCRIPTION**
! given a file name, check to see if the file exists, then count number
! of lines in the file
!*******************************************************************************
subroutine file_recs(FILE_NAME,NUM_UNIT,NUM_LINES)

character(len=*), intent(in) :: FILE_NAME ! name of file
integer, intent(in) :: NUM_UNIT           ! unit number used to open file
integer, intent(out) :: NUM_LINES         ! number of lines in file

inquire(file=FILE_NAME,exist=file_exists)
if (file_exists.eqv..false.) stop "ERROR: merge_module/file_recs: cannot find FILE"
line_counter=0
open(unit=NUM_UNIT,file=FILE_NAME,status='old')
do
  read(NUM_UNIT,*,iostat=ioerror)
  if (ioerror==-1) exit
  line_counter=line_counter+1
enddo
close(NUM_UNIT)
NUM_LINES=line_counter

end subroutine file_recs 

!*******************************************************************************
! **SUBROUTINE NAME**
! get_lookup_table
!
! **DESCRIPTION**
! read in the CDF lookup table required by the merge program
!   lookup_IA.txt
!*******************************************************************************
subroutine get_lookup_table(INFILE,OVERLAP,RESULT)

character(len=*), intent(in) :: INFILE    ! name of file
integer, intent(in) :: OVERLAP            ! overlap threshold (from config file)
type(lookup_table), intent(out) :: RESULT ! Derived Data Type For CDF Lookup Table

integer :: month_minimum
integer :: month_maximum
real :: metric_median

! Check to see if file exists
inquire(file=INFILE,exist=file_exists)
if (file_exists.eqv..false.) stop "ERROR: Cannot find lookup table"

! Find out how many lines in file
call file_recs(FILE_NAME=INFILE,NUM_UNIT=1,NUM_LINES=line_counter)
if (line_counter==0) stop "ERROR: lookup table has no data"

! Allocate the CDF arrays based upon the number of lines in the cdf infile. Set to blank / missing
allocate(RESULT%min_month(1:line_counter)); RESULT%min_month(:)=missing
allocate(RESULT%max_month(1:line_counter)); RESULT%max_month(:)=missing
allocate(RESULT%min_metric(1:line_counter)); RESULT%min_metric(:)=missing
allocate(RESULT%max_metric(1:line_counter)); RESULT%max_metric(:)=missing
allocate(RESULT%h1(1:line_counter)); RESULT%h1(:)=missing
allocate(RESULT%h2(1:line_counter)); RESULT%h2(:)=missing

! Go through file second time to collect data
line_counter = 0
counter = 0
open(unit=1,file=INFILE,status='OLD')
do
  read(1,'(a1024)',iostat=ioerror) readline
  if (ioerror==-1) exit
  line_counter = line_counter+1
  if(line_counter >= 5) then

    read (readline(1:4),'(i4)') month_minimum
    read (readline(6:9),'(i4)') month_maximum

    if(month_minimum >=OVERLAP .or. month_maximum >= OVERLAP) then
      counter = counter + 1
      read (readline(1:4),'(i4)') RESULT%min_month(counter)
      read (readline(6:9),'(i4)') RESULT%max_month(counter)
      read (readline(11:18),'(f8.6)') metric_median
      RESULT%min_metric(counter) = real(metric_median - 0.0025)
      RESULT%max_metric(counter) = real(metric_median + 0.0025)
      read (readline(20:27),'(f8.6)') RESULT%h1(counter)
      read (readline(29:36),'(f8.6)') RESULT%h2(counter)
    endif
  endif
enddo
close(1)

RESULT%max_nonmissing = count(RESULT%h1(:)/=missing)

end subroutine get_lookup_table

!*******************************************************************************
! **SUBROUTINE NAME**
! get_blacklist
!
! **DESCRIPTION**
! read in the blacklist file required by the merge program
!   databank_blacklist.txt
!*******************************************************************************
subroutine get_blacklist(INFILE,RESULT)

character(len=*), intent(in) :: INFILE ! name of file
type(blacklist), intent(out) :: RESULT ! Derived Data Type For Blacklist Data

! Check to see if file exists
inquire(file=INFILE,exist=file_exists)
if (file_exists.eqv..false.) stop "ERROR: Cannot find blacklist file"

! Find out how many lines in file
call file_recs(FILE_NAME=INFILE,NUM_UNIT=1,NUM_LINES=line_counter)
if (line_counter==0) stop "ERROR: blacklist file has no data"

! Allocate based upon # of cases in blacklist file. Set to blank / missing
allocate(RESULT%input_source(1:line_counter)); RESULT%input_source(:)=""
allocate(RESULT%input_id(1:line_counter)); RESULT%input_id(:)=""
allocate(RESULT%input_name(1:line_counter)); RESULT%input_name(:)=""
allocate(RESULT%input_latitude(1:line_counter)); RESULT%input_latitude(:)=missing
allocate(RESULT%input_longitude(1:line_counter)); RESULT%input_longitude(:)=missing
allocate(RESULT%input_elevation(1:line_counter)); RESULT%input_elevation(:)=missing
allocate(RESULT%decision(1:line_counter)); RESULT%decision(:)=""
allocate(RESULT%final_name(1:line_counter)); RESULT%final_name(:)=""
allocate(RESULT%final_latitude(1:line_counter)); RESULT%final_latitude(:)=missing
allocate(RESULT%final_longitude(1:line_counter)); RESULT%final_longitude(:)=missing
allocate(RESULT%final_elevation(1:line_counter)); RESULT%final_elevation(:)=missing

! Go through file second time to collect data
line_counter = 0
open(unit=1,file=INFILE,status='OLD')
do
  read(1,'(a1024)',iostat=ioerror) readline
  if (ioerror==-1) exit
  line_counter = line_counter+1

  RESULT%input_source(line_counter)=readline(1:25)
  RESULT%input_id(line_counter)=readline(27:41)
  RESULT%input_name(line_counter)=readline(43:73)
  read (readline(75:84),'(f10.4)') RESULT%input_latitude(line_counter)
  read (readline(86:95),'(f10.4)') RESULT%input_longitude(line_counter)
  read (readline(97:104),'(f10.4)') RESULT%input_elevation(line_counter)
  RESULT%decision(line_counter)=readline(106:113)

  if(trim(adjustl(RESULT%decision(line_counter))) .eq. "CHANGE") then
    RESULT%final_name(line_counter)=readline(127:156)
    read (readline(158:167),'(f10.4)') RESULT%final_latitude(line_counter)
    read (readline(169:178),'(f10.4)') RESULT%final_longitude(line_counter)
    read (readline(180:187),'(f10.4)') RESULT%final_elevation(line_counter)
  endif
enddo
close(1)

RESULT%max_blacklist = count(RESULT%input_id(:)/="")

end subroutine get_blacklist

!*******************************************************************************
! **SUBROUTINE NAME**
! get_data
!
! **DESCRIPTION**
! Reads in both metadata and data for a single source in the merge, and places
! it in a derived data type
!*******************************************************************************
subroutine get_data(GLOBAL_DATABANK_DIRECTORY,DATASET_NAME,DATASET_NUMBER,BEGIN_YEAR,END_YEAR,RESULT,MAXIMUM_STATIONS)

character(len=*), intent(in) :: GLOBAL_DATABANK_DIRECTORY ! location of Stage 2 Source
character(len=*), intent(in) :: DATASET_NAME              ! name of dataset
character(len=02), intent(in) :: DATASET_NUMBER           ! position in source hirearchy
integer, intent(in) :: BEGIN_YEAR                         ! begin year of por
integer, intent(in) :: END_YEAR                           ! end year of por
type(databank), intent(out) :: RESULT                     ! Derived Data Type For Master/Candidate Data
integer, intent(in), optional :: MAXIMUM_STATIONS         ! maximum number of stations (optional)

character(len=1024) :: infile           ! current file being read in
integer :: year                         ! year
integer :: month                        ! month 
character(len=1024) :: uppercase_source ! name of dataset (all uppercase letters)
character(len=1024) :: lowercase_source ! name of dataset (all lowercase letters)
character(len=1024) :: key_number       ! used for creating ID's
integer :: station                      ! current station position
integer :: char_position                ! current character position in line being read
integer :: allocation_limit             ! max number used when allocating derived data type
real :: value                           ! Temperature value currently being read in

call change_string_case(INPUT_STRING=DATASET_NAME,WHICH="UPPER",FINAL_STRING=uppercase_source)
call change_string_case(INPUT_STRING=DATASET_NAME,WHICH="LOWER",FINAL_STRING=lowercase_source)

! Read in INVENTORY first time to determine number of stations
infile=trim(GLOBAL_DATABANK_DIRECTORY)//trim(lowercase_source)//'/INVENTORY_'//trim(uppercase_source)//'_monthly_stage2'
inquire(file=infile,exist=file_exists)
if (file_exists.eqv..false.) stop "ERROR: merge_module/get_data: Cannot find global databank metadata file"

call file_recs(FILE_NAME=infile,NUM_UNIT=1,NUM_LINES=line_counter)
if (line_counter==0) stop "ERROR: merge_module/get_data: global databank metadata file has no data"
RESULT%number_of_stations=line_counter

! When allocating the target dataset (#1 in hirearchy), we do not know
! how many stations will end up at the end. So when MAXIMUM_STATIONS
! is present, we allocate the derived data type (merged) based upon 
! what we think will be the final number (defined in config file)
!
! OTHERWISE, allocate the derived data type (candidate) based upon
! how many stations there are in that source
if (present(MAXIMUM_STATIONS)) then
  allocation_limit = MAXIMUM_STATIONS
else
  allocation_limit = RESULT%number_of_stations
endif

write(*,'(a,i5)') "NUM STATIONS: ",RESULT%number_of_stations
write(*,'(a,i5)') "ALLOCATION NUMBER: ",  allocation_limit

! Allocate elements in derived data type based upon the allocation limit
allocate(RESULT%id(1:allocation_limit)); RESULT%id(:)=""
allocate(RESULT%key(1:allocation_limit)); RESULT%key(:)=""
allocate(RESULT%name(1:allocation_limit)); RESULT%name(:)=""
allocate(RESULT%country(1:allocation_limit)); RESULT%country(:)=""
allocate(RESULT%latitude(1:allocation_limit)); RESULT%latitude(:)=missing
allocate(RESULT%longitude(1:allocation_limit)); RESULT%longitude(:)=missing
allocate(RESULT%elevation(1:allocation_limit)); RESULT%elevation(:)=missing
allocate(RESULT%year_start(1:allocation_limit,1:number_of_elements)); RESULT%year_start(:,:)=high_miss
allocate(RESULT%year_end(1:allocation_limit,1:number_of_elements)); RESULT%year_end(:,:)=low_miss
allocate(RESULT%data(1:allocation_limit,1:number_of_elements,BEGIN_YEAR:END_YEAR,1:12)); RESULT%data(:,:,:,:)=missing
allocate(RESULT%stage2_flag(1:allocation_limit,BEGIN_YEAR:END_YEAR,1:12)); RESULT%stage2_flag(:,:,:)=""
allocate(RESULT%source_flag(1:allocation_limit,1:number_of_elements,BEGIN_YEAR:END_YEAR,1:12)) 
RESULT%source_flag(:,:,:,:)="XXXXXXXX"

! Read in INVENTORY second time to grab metadata
line_counter = 0
open(unit=1,file=INFILE,status='OLD')
do
  read(1,'(a1024)',iostat=ioerror) readline
  if (ioerror==-1) exit

  line_counter=line_counter+1

  RESULT%id(line_counter)=readline(1:12)
  RESULT%name(line_counter)=readline(14:43)
  RESULT%country(line_counter)=readline(45:64)
  read (readline(66:75),'(f10.4)') RESULT%latitude(line_counter)
  read (readline(77:86),'(f10.4)') RESULT%longitude(line_counter)
  if (RESULT%latitude(line_counter)==-9999.0) RESULT%latitude(line_counter)=missing
  if (RESULT%longitude(line_counter)==-9999.0) RESULT%longitude(line_counter)=missing
  read (readline(88:95),'(f8.2)') RESULT%elevation(line_counter)
  if (RESULT%elevation(line_counter)==-9999.0) RESULT%elevation(line_counter)=missing

enddo
close(1)

! Go through each station and read in data
do station=1,RESULT%number_of_stations
  infile=trim(GLOBAL_DATABANK_DIRECTORY)//trim(lowercase_source)//'/'//trim(lowercase_source)//'_'//                               &
         trim(RESULT%id(station))//'_monthly_stage2'
  inquire(file=infile,exist=file_exists)
  if (file_exists.eqv..false.) then
    write (*,*) trim(infile)
    stop "ERROR: merge_module/get_data: Cannot find global databank data file"
  endif

  ! Create 2 id's for station
  !   RESULT%id(station)  = ID given to us by source, appended in front by source #  
  !   RESULT%key(station) = Uniq Identifier, appended in front by source #  
  RESULT%id(station) = DATASET_NUMBER//"_"//RESULT%id(station)
  write (key_number,'(i6.6)') station
  RESULT%key(station) = DATASET_NUMBER//key_number

  open(unit=1,file=infile,status='OLD')
  do
    read(1,'(a1024)',iostat=ioerror) readline
    if (ioerror==-1) exit

    read (readline(63:66),'(i4)') year
    read (readline(67:68),'(i2)') month
    RESULT%stage2_flag(station,year,month)=readline(90:132)
    if (year < BEGIN_YEAR .or. year > END_YEAR) cycle
    char_position = 72 
    do element=1,number_of_elements
      read (readline(char_position:char_position+4),'(f6.0)') value
      if (value .ne. -9999.0) then
        RESULT%data(station,element,year,month)=value/100.0
        RESULT%source_flag(station,element,year,month)=RESULT%key(station)
        RESULT%year_start(station,element) = min(RESULT%year_start(station,element),year)
        RESULT%year_end(station,element) = max(RESULT%year_end(station,element),year)
      endif
      char_position = char_position + 6
    enddo
  enddo
  close(1)
enddo

end subroutine get_data

!*******************************************************************************
! **SUBROUTINE NAME**
! geographic_distance
!
! **DESCRIPTION**
! Calculate the distance from location pair (X1,Y1) to (X2,Y2) in kilometers
!*******************************************************************************
subroutine geographic_distance(X1,Y1,X2,Y2,DISTANCE)

real, intent(in) :: X1        ! Longitude of first station
real, intent(in) :: Y1        ! Latitude of first station
real, intent(in) :: X2        ! Longitude of second station
real, intent(in) :: Y2        ! Latitude of second station
real, intent(out) :: DISTANCE ! Geographic distance (km)
real :: RX1                   ! X1, converted to radians
real :: RY1                   ! Y1, converted to radians
real :: RX2                   ! X2, converted to radians
real :: RY2                   ! Y2, converted to radians
real :: x                     ! first part of calculating distance

! Make sure lat/lon values are not out of bounds
if (X1<-180.0.or.X1>360.0) & 
  stop "ERROR: merge_module/geographic_distance: one or more of X1 or X2 longitude out of range -180.0 <= X <= 360.0"
if (Y1<-90.0.or.Y1>180.0) & 
  stop "ERROR: merge_module/geographic_distance: one or more of Y1 or Y2 latitude out of range -90.0 <= Y <= 180.0"

! Convert degrees to radians
RX1=X1*PI/180.0
RX2=X2*PI/180.0
RY1=Y1*PI/180.0
RY2=Y2*PI/180.0

! Calculate first part of distance
x=(sqrt((sin((RY1-RY2)/2.0))**2.0 + cos(RY1)*cos(RY2)*(sin((RX1-RX2)/2.0))**2.0))
if (x>1.0) x=1.0
! Calculate second part (final distance)
DISTANCE=(2.0*asin(x))*180.0*60.0/PI*1.852

end subroutine geographic_distance

!*******************************************************************************
! **SUBROUTINE NAME**
! jaccard_index
!
! **DESCRIPTION**
! measure similarity of two strings
! 
! JACCARD INDEX (JI) = intersection divided by the union of two strings
!
! a = # of characters in common in both STRING1 and STRING2
! b = # of unique characters in STRING1 not in STRING2
! c = # of unique characters in STRING2 not in STRING1
!
! JI = (a) / (a+b+c)
!*******************************************************************************
subroutine jaccard_index(STRING1,STRING2,RESULT)

character(len=*), intent(in) :: STRING1 ! first string brought in
character(len=*), intent(in) :: STRING2 ! second string brought in
character(len=30) :: converted_STRING1  ! first string with control characters removed
character(len=30) :: converted_STRING2  ! second string with control characters removed
real, intent(out) :: RESULT              ! Final Jaccard Index: JI = (a) / (a+b+c)

integer, dimension(1:255) :: character_exists ! numerical 0/1 expression depicting if individual character exists in string
integer :: ii                                 ! counter for first do loop 
integer :: jj                                 ! counter for second do loop 
integer :: a                                  ! # of characters in common in both STRING1 and STRING2
integer :: b                                  ! # of unique characters in STRING1 not in STRING2
integer :: c                                  ! # of unique characters in STRING2 not in STRING1

! Remove spaces, tabs, and control characters in strings before running JI
call remove_odd_characters(INPUT_STRING=STRING1,FINAL_STRING=converted_STRING1)
call remove_odd_characters(INPUT_STRING=STRING2,FINAL_STRING=converted_STRING2)

! calculate a (# of characters in common in both STRING1 and STRING2)
character_exists(:)=0
do ii=1,len_trim(converted_STRING1)
  do jj=1,len_trim(converted_STRING2)
    if (converted_STRING1(ii:ii)==converted_STRING2(jj:jj)) character_exists(iachar(converted_STRING1(ii:ii)))=1
  enddo
enddo
a=count(character_exists(:)==1)

! calculate b (# of unique characters in STRING1 not in STRING2)
b=0
do ii=1,len_trim(converted_STRING1)
  if (character_exists(iachar(converted_STRING1(ii:ii)))==0) b=b+1
enddo

! calculate c (# of unique characters in STRING2 not in STRING1)
c=0
do ii=1,len_trim(converted_STRING2)
  if (character_exists(iachar(converted_STRING2(ii:ii)))==0) c=c+1
enddo

! with a,b,c, calculate JI
RESULT=real(a) / (real(a)+real(b)+real(c))

end subroutine jaccard_index

!*******************************************************************************
! **SUBROUTINE NAME**
! mean
!
! **DESCRIPTION**
! Calculate the mean of an array X
!*******************************************************************************
subroutine mean(X,RESULT)

real, intent(in) :: X(:)    ! Input data
real, intent(out) :: RESULT ! Final Mean Value
real :: data_sum            ! Non-Missing Data summed up
real :: data_counter        ! Number of Non-Missing Data
integer :: ii

data_sum=0.0
data_counter=0.0
do ii = 1,size(X)
  data_sum = data_sum + X(ii)
  data_counter=data_counter+1.0
enddo

if(data_counter == 0.0) stop "ERROR: merge_module/mean: denominator cannot be 0"

RESULT=(data_sum/data_counter)

end subroutine mean

!*******************************************************************************
! **SUBROUTINE NAME**
! index_of_agreement
!
! **DESCRIPTION**
! Given two stations with overlapping data (defined by config file), calculate
! the index of agreement (IA) between them. 
! 
! IA is a “goodness-of-fit” measure and is defined as the ratio between the 
! mean square error and the potential error. A modified version of IA 
! (Willmott et al., 1985; Legates and McCabe, 1999) is used where the squared 
! term is removed (PWR=1), and is the equation used here
!*******************************************************************************
subroutine index_of_agreement(X,Y,MEANX,PWR,RESULT)

real, dimension(:), intent(in) :: X ! Data for merged station
real, dimension(:), intent(in) :: Y ! Data for candidate station
real, intent(in) :: MEANX           ! Mean of merged station (NOTE: mean of candidate not used)
integer, intent(in) :: PWR          ! Power used in equation (see Legates and McCabe, 1999)
real, intent(out) :: RESULT         ! Final Index of Agreement

real :: mean_squared_error
real :: potential_error

real :: total_mean_squared_error
real :: total_potential_error
integer :: ii

total_mean_squared_error = 0.0
total_potential_error = 0.0

do ii=1,size(X)
  mean_squared_error= abs(X(ii)-Y(ii))**PWR  
  potential_error= (abs(Y(ii)-MEANX) + abs(X(ii)-MEANX))**PWR 

  total_mean_squared_error = total_mean_squared_error + mean_squared_error
  total_potential_error = total_potential_error + potential_error
enddo

if(total_potential_error == 0.0) stop "ERROR: merge_module/index_of_agreement: denominator cannot be 0"

RESULT = 1.0-(total_mean_squared_error/total_potential_error)

end subroutine index_of_agreement

!*******************************************************************************
! **SUBROUTINE NAME**
! merge_station
!
! **DESCRIPTION**
! Given two stations (one target and one candidate), look for missing data in
! The target data that is non-missing in the candidate data. If there is a 
! significant gap (defined by config file), then add the candidate data into
! the target station
!*******************************************************************************
subroutine merge_station(DATA_X,DATA_Y,STAGE2_X,STAGE2_Y,SOURCE_X,SOURCE_Y,THRESH,BEGIN_YEAR,END_YEAR,                             &
                         RESULT_MERGE,RESULT_DATA,RESULT_STAGE2,RESULT_SOURCE,RESULT_YRST,RESULT_YREND)

real, dimension(:,:), intent(in) :: DATA_X  ! Target Station Data
real, dimension(:,:), intent(in) :: DATA_Y  ! Candidate Station Data

character(len=43), dimension(:,:), intent(in) :: STAGE2_X ! Target Stage2 Provenance Tracking Flags
character(len=43), dimension(:,:), intent(in) :: STAGE2_Y ! Candidate Stage2 Provenance Tracking Flags

character(len=8), dimension(:,:), intent(in) :: SOURCE_X ! Target source of data
character(len=8), dimension(:,:), intent(in) :: SOURCE_Y ! Target source of data

integer, intent(in) :: THRESH ! gap_treshold (defined by configuration file)

integer, intent(in) :: BEGIN_YEAR ! begin year of entire databank por (defined in config file)
integer, intent(in) :: END_YEAR   ! end year of entire databank por (defined in config file)

logical, intent(out) :: RESULT_MERGE ! T/F switch indicating if candidate data is available to be added to target data

real, dimension(:,:), intent(out) :: RESULT_DATA                ! New Target Station Data
character(len=43), dimension(:,:), intent(out) :: RESULT_STAGE2 ! New Target Stage2 Provenance Tracking Flags
character(len=8), dimension(:,:), intent(out) :: RESULT_SOURCE  ! New Target source of data
integer, intent(out) :: RESULT_YRST                             ! New Target POR (start)
integer, intent(out) :: RESULT_YREND                            ! New Target POR (end)

integer :: num_years   ! Number of years in the data array
integer :: num_months  ! Number of months in the data array
integer :: actual_year ! Actual Year, since num_years is postiion in data array

integer :: year_count  ! Position of year in data array
integer :: month_count ! Position of month in data array

! Temporary arrays when running through data, at the end, these will become the final merged station
real, dimension(:), allocatable :: tmp_data                    ! Final merged data
character(len=8), dimension(:), allocatable :: tmp_source      ! Final merged sources
character(len=43), dimension(:), allocatable :: tmp_stage2_new ! Final merged Stage2 Provenance Tracking Flags
character(len=43), dimension(:), allocatable :: tmp_stage2_old ! Needed for when gap threshold isn't met

integer :: start_gap     ! month position where gap begins
integer :: non_missing   ! number of consecutive months non-missing
logical :: gap_occurs    ! Logical indicating if a gap is occurring
integer :: data_position ! position of data in array (year,month)
integer :: gap_period    ! position of gap in data array

num_years = ((END_YEAR-BEGIN_YEAR) + 1)
num_months = num_years * 12

! Allocate arrays based upon the number of months available
allocate(tmp_data(1:num_months)); tmp_data(:)=missing
allocate(tmp_stage2_old(1:num_months)); tmp_stage2_old(:)=""
allocate(tmp_stage2_new(1:num_months)); tmp_stage2_new(:)=""
allocate(tmp_source(1:num_months)); tmp_source(:)="XXXXXXXX"

data_position = 1
non_missing= 0
gap_occurs = .false.
RESULT_MERGE = .false.

! Run through target and candidate data arrays to check for gaps. 
! Set data/stage2/source information into a temporary array
!    if target is non-missing, then use target info
!    if target is missing and candidate is non-missing, then use candidate info
do year_count=1,num_years
  actual_year = year_count + (BEGIN_YEAR-1)
  do month_count=1,12
    tmp_stage2_old(data_position) = STAGE2_X(year_count,month_count)

    if (DATA_X(year_count,month_count)==missing) then 
      ! Target data missing 
      ! If gap hasn't started, then gap begins at this year/month position
      if(.not. gap_occurs) then
        gap_occurs = .true.
        start_gap = data_position                
      endif

      ! Since target data is missing, check to see if candidate data is non-missing
      if(DATA_Y(year_count,month_count)/=missing) then
        ! candidate data is non-missing, add candidate info to temporary arrays
        tmp_data(data_position) = DATA_Y(year_count,month_count)
        tmp_stage2_new(data_position) = STAGE2_Y(year_count,month_count)
        tmp_source(data_position) = SOURCE_Y(year_count,month_count)
        non_missing=non_missing+1     
      else
        ! candidate data is missing, add target info to temporary arrays
        tmp_data(data_position) = DATA_X(year_count,month_count)
        tmp_stage2_new(data_position) = STAGE2_X(year_count,month_count)
        tmp_source(data_position) = SOURCE_X(year_count,month_count)        
      endif
    else
      ! Target data is non-missing 
      ! add target info to temporary arrays
      tmp_data(data_position) = DATA_X(year_count,month_count)
      tmp_stage2_new(data_position) = STAGE2_X(year_count,month_count)
      tmp_source(data_position) = SOURCE_X(year_count,month_count)

      ! If a gap has occured prior to this position, see if number of non-missing data
      ! passes the gap threshold (defined in config file)
      if(gap_occurs) then
        if(non_missing < THRESH) then
          ! gap threshold not passed, need to reset temporary array from beginning of
          ! gap (start_gap) to current position(data_position-1)
          do gap_period=start_gap,data_position-1
            tmp_data(gap_period) = missing
            tmp_stage2_new(gap_period) = tmp_stage2_old(gap_period)
            tmp_source(gap_period) = "XXXXXXXX"
          enddo
        else
          ! gap threshold has been passed, switch boolean to true
          RESULT_MERGE = .true.
        endif           
      endif
      ! Reset some items since gap ended
      gap_occurs = .false.
      non_missing = 0
    endif    
    ! Move on to the next day month value  
    data_position = data_position+1 
  enddo 
enddo

! End of Data Stream Reached, Check for gap one more time
if(gap_occurs) then
  if(non_missing < THRESH) then
    do gap_period=start_gap,data_position-1
      tmp_data(gap_period) = missing
      tmp_stage2_new(gap_period) = tmp_stage2_old(gap_period)
      tmp_source(gap_period) = "XXXXXXXX"
    enddo
  else
    RESULT_MERGE = .true.
  endif      
endif  

! Merge using temporary array, result is NEW target station
data_position = 1
RESULT_YRST = high_miss
RESULT_YREND = low_miss
do year_count=1,num_years
  actual_year = year_count + (BEGIN_YEAR-1)
  do month_count=1,12

    RESULT_DATA(year_count,month_count)=tmp_data(data_position)
    if (RESULT_DATA(year_count,month_count)/=missing) then
      RESULT_YRST = min(actual_year,RESULT_YRST)
      RESULT_YREND = max(actual_year,RESULT_YREND)
    endif
    RESULT_STAGE2(year_count,month_count)=tmp_stage2_new(data_position)
    RESULT_SOURCE(year_count,month_count)=tmp_source(data_position)

    data_position = data_position+1 
  enddo 
enddo

end subroutine merge_station

!*******************************************************************************
! **SUBROUTINE NAME**
! withhold_station
!
! **DESCRIPTION**
! Take candidate station, that has been chosen to be withheld, and place
! the data / inventory file to the withheld bin
!*******************************************************************************
subroutine withhold_station(DATA_X,STAGE2_X,SOURCE_X,KEY_X,ID_X,NAME_X,COUNTRY_X,LON_X,LAT_X,ELEV_X,REASON,DIR,BEGIN_YEAR)

real, dimension(:,:,:), intent(in) :: DATA_X               ! Data for withheld station   
character(len=43), dimension(:,:), intent(in) :: STAGE2_X  ! Stage2 Provanance Flags for withheld station  
character(len=8), dimension(:,:,:), intent(in) :: SOURCE_X ! Source info for withheld station  
character(len=8), intent(in) :: KEY_X                      ! 8-Digit key for withheld station
character(len=30), intent(in) :: ID_X                      ! Source ID for withheld station
character(len=30), intent(in) :: NAME_X                    ! Name of withheld station
character(len=20), intent(in) :: COUNTRY_X                 ! Country of origin for withheld station
real, intent(in) :: LON_X                                  ! Latitude of withheld station
real, intent(in) :: LAT_X                                  ! Longitude of withheld station
real, intent(in) :: ELEV_X                                 ! Elevation of withheld station
integer, intent(in) :: REASON                              ! Reason station was withheld            
character(len=*),intent(in) :: DIR                         ! Withheld directory
integer, intent(in) :: BEGIN_YEAR                          ! begin year of entire databank por (defined in config file)

integer :: num_years   ! Number of years in the data array
integer :: actual_year ! Actual Year, since num_years is postiion in data array
integer :: year_count  ! Position of year in data array
integer :: month_count ! Position of month in data array

integer, dimension (1:number_of_elements) :: print_value ! Temperature value to be printed (in integer form)

! Open Inventory file and write out metadata
open(unit=21,file=trim(DIR)//'INVENTORY_monthly_withheld_stage3',status='old',access='append')
write(21,21) KEY_X,NAME_X,COUNTRY_X,LAT_X,LON_X,ELEV_X,ID_X,REASON     
call flush(21)
close(21)

num_years = int(size(DATA_X)/36)
actual_year = BEGIN_YEAR

! Open Data file and write out data
open(unit=19,file=trim(DIR)//'withheld_'//trim(KEY_X)//'_stage3',status='replace')

do year_count=1,num_years
  do month_count=1,12
    ! Don't print data if all 3 elements (tmax,tmin,tavg) is missing
    if (DATA_X(tmax,year_count,month_count)/=missing .or.                                                                          &
        DATA_X(tmin,year_count,month_count)/=missing .or.                                                                          &
        DATA_X(tavg,year_count,month_count)/=missing) then      

      ! Data needs to be converted from real to integer
      do element=1,number_of_elements
        if(DATA_X(element,year_count,month_count) == missing) then
          print_value(element) = int(-9999)
        else
          print_value(element) = int(DATA_X(element,year_count,month_count)*100)
        endif
      enddo

      write(19,19) NAME_X,LAT_X,LON_X,ELEV_X,actual_year,month_count,print_value(tmax),print_value(tmin),                          &
                   print_value(tavg),STAGE2_X(year_count,month_count),SOURCE_X(tmax,year_count,month_count),                       &
                   SOURCE_X(tmin,year_count,month_count),SOURCE_X(tavg,year_count,month_count)
      call flush(19)

    endif
  enddo
  actual_year=actual_year+1
enddo
close(19)

!Set Formats for withheld dat/inv file
19 format(a30,1x,f10.4,1x,f10.4,1x,f8.2,1x,i4,i2.2,"XX",1x,3(i5,1x),a43,1x,a8,"/",a8,"/",a8)
21 format(a12,1x,a30,1x,a20,1x,f10.4,1x,f10.4,1x,f8.2,31x,a20,1x,i3)

end subroutine withhold_station

!*******************************************************************************
! **SUBROUTINE NAME**
! change_string_case
!
! **DESCRIPTION**
! Convert a string to upper/lower case
!*******************************************************************************
subroutine change_string_case(INPUT_STRING,WHICH,FINAL_STRING)

character(len=*), intent(in) :: INPUT_STRING  ! Input string
character(len=05), intent(in) :: WHICH        ! Which type of conversion (UPPER / LOWER)
character(len=*), intent(out) :: FINAL_STRING ! Final string
character(len=01), dimension(1:26) :: cLower  ! array holding 26 lower case letters
character(len=01), dimension(1:26) :: cUpper  ! array holding 26 upper case letters
integer :: ii                                 ! counter for first do loop 
integer :: jj                                 ! counter for second do loop 

if(WHICH .ne. "UPPER" .and. WHICH .ne. "LOWER") stop "ERROR: merge_module/change_string_case: choose UPPER or LOWER"

! Define upper and lower case letters
cLower(:)=(/"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"/)
cUpper(:)=(/"A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"/)

FINAL_STRING=INPUT_STRING
do ii=1,len(FINAL_STRING)
  do jj=1,26
    if(WHICH .eq. "UPPER") then ! CONVERT LOWER TO UPPER CASE
      if (FINAL_STRING(ii:ii)==cLower(jj)) FINAL_STRING(ii:ii)=cUpper(jj)
    else ! CONVERT UPPER TO LOWER CASE
      if (FINAL_STRING(ii:ii)==cUpper(jj)) FINAL_STRING(ii:ii)=cLower(jj)
    endif
  enddo 
enddo  
end subroutine change_string_case

!*******************************************************************************
! **SUBROUTINE NAME**
! remove_odd_characters
!
! **DESCRIPTION**
! Given a string, remove any odd characters, including spaces, tabs and controls
!*******************************************************************************
subroutine remove_odd_characters(INPUT_STRING,FINAL_STRING)

character(len=*), intent(in) :: INPUT_STRING  ! Input string
character(len=*), intent(out) :: FINAL_STRING ! Final string
integer :: ii                                 ! position in INPUT_STRING
integer :: jj                                 ! position in FINAL_STRING
character(len=01) :: individual_char          ! individual character
integer :: ascii_value                        ! ascii value of character


FINAL_STRING=' '
jj=0
do ii=1,len_trim(INPUT_STRING)
  individual_char=INPUT_STRING(ii:ii)
  ascii_value=iachar(individual_char)
  select case(ascii_value)        
    case(48:57,65:90) ! 0-9, A-Z 
      jj=jj+1
      FINAL_STRING(jj:jj)=individual_char
  end select
end do

end subroutine remove_odd_characters

!*******************************************************************************
! **SUBROUTINE NAME**
! calculate_metadata_probability
!
! **DESCRIPTION**
! Given non-missing mertrics, calculate the metadata probability
!*******************************************************************************

subroutine calculate_metadata_probability(DISTANCE_PROB,HEIGHT_PROB,JACCARD_PROB,YEAR_PROB,                                        &
                                          DISTANCE_WEIGHT,HEIGHT_WEIGHT,JACCARD_WEIGHT,YEAR_WEIGHT,RESULT)

real, intent(in) :: DISTANCE_PROB
real, intent(in) :: HEIGHT_PROB
real, intent(in) :: JACCARD_PROB
real, intent(in), dimension(:) :: YEAR_PROB
real, intent(in) :: DISTANCE_WEIGHT
real, intent(in) :: HEIGHT_WEIGHT
real, intent(in) :: JACCARD_WEIGHT
real, intent(in) :: YEAR_WEIGHT
real, intent(out) :: RESULT
real :: numerator
real :: denominator

numerator=0.0
denominator=0.0
if(DISTANCE_PROB == missing) stop "ERROR: merge_module/calculate_metadata_probability: DISTANCE_PROB cannot be missing"

numerator=numerator+(DISTANCE_WEIGHT*DISTANCE_PROB)
denominator=denominator+DISTANCE_WEIGHT

if(HEIGHT_PROB /= missing) then
  numerator=numerator+(HEIGHT_WEIGHT*HEIGHT_PROB)
  denominator=denominator+HEIGHT_WEIGHT
endif

if(JACCARD_PROB /= missing) then
  numerator=numerator+(JACCARD_WEIGHT*JACCARD_PROB)
  denominator=denominator+JACCARD_WEIGHT
endif

! For TXN analysis, tmax and tmin cannot be missing
if(YEAR_PROB(tmax) /= missing .and. YEAR_PROB(tmin) /= missing) then
  numerator=numerator+(YEAR_WEIGHT*YEAR_PROB(tmax))+(YEAR_WEIGHT*YEAR_PROB(tmin))
  denominator=denominator+(2.0*YEAR_WEIGHT)
endif

! For TVG analysis, only tavg cannot be missing
if(YEAR_PROB(tavg) /= missing) then
  numerator=numerator+(YEAR_WEIGHT*YEAR_PROB(tavg))
  denominator=denominator+YEAR_WEIGHT
endif

if(denominator == 0.0) stop "ERROR: merge_module/calculate_metadata_probability: denominator cannot be 0"

RESULT = numerator / denominator

end subroutine calculate_metadata_probability

!*******************************************************************************
! **SUBROUTINE NAME**
! calculate_data_probability
!
! **DESCRIPTION**
! Given non-missing mertrics, calculate the data probabilities
!*******************************************************************************

subroutine calculate_data_probability(META_PROB,H1,H2,RESULT_SAME,RESULT_UNIQUE)

real, intent(in) :: META_PROB
real, intent(in), dimension(:) :: H1
real, intent(in), dimension(:) :: H2
real, intent(out) :: RESULT_SAME
real, intent(out) :: RESULT_UNIQUE
real :: numerator
real :: denominator

numerator=0.0
denominator=0.0
if(META_PROB == missing) stop "ERROR: merge_module/calculate_data_probability: META_PROB cannot be missing"

!*************************
! Calculate RESULT_SAME
!*************************
numerator=numerator+(1.0*META_PROB)
denominator=denominator+1.0

! For TXN analysis, tmax and tmin cannot be missing
if(H1(tmax) /= missing .and. H1(tmin) /= missing) then
  numerator=numerator+(1.0*H1(tmax))+(1.0*H1(tmin))
  denominator=denominator+2.0
endif

! For TVG analysis, only tavg cannot be missing
if(H1(tavg) /= missing) then
  numerator=numerator+(1.0*H1(tavg))
  denominator=denominator+1.0
endif

if(denominator == 0.0) stop "ERROR: merge_module/calculate_data_probability: denominator cannot be 0"

RESULT_SAME = numerator / denominator

!*************************
! Calculate RESULT_UNIQUE
!*************************
! For TXN analysis, tmax and tmin cannot be missing
if(H2(tmax) /= missing .and. H2(tmin) /= missing) then
  RESULT_UNIQUE = (1.0 - META_PROB) + H2(tmax) + H2(tmin)
endif

! For TVG analysis, only tavg cannot be missing
if(H2(tavg) /= missing) then
  RESULT_UNIQUE = (1.0 - META_PROB) + H2(tavg)
endif

end subroutine calculate_data_probability

!*******************************************************************************
! **SUBROUTINE NAME**
! correlation
!
! **DESCRIPTION**
! Calculate the Simple Correlation between array X and array Y
!*******************************************************************************

subroutine correlation(X,Y,VALUE)

real, dimension(:), intent(in) :: X ! Array X
real, dimension(:), intent(in) :: Y ! Array Y
real, intent(out) :: VALUE          ! Final Correlation value
real(dp) :: Sum_X                   ! Sum of Array X
real(dp) :: Sum_Y                   ! Sum of Array Y
real(dp) :: Sum_XY                  ! Sum of Array X * Array Y
real(dp) :: Sum_X_Squared           ! Sum of Array X ^ 2
real(dp) :: Sum_Y_Squared           ! Sum of Array Y ^ 2
integer :: ii                       ! counter for do loop

if (size(X)<2.or.size(Y)<2) then
  VALUE=-9999.0
else

  if (size(X)/=size(Y)) stop "ERROR: merge_module/correlation: X array size not equal to Y array size"

  Sum_X=0.0
  Sum_Y=0.0
  Sum_X_Squared=0.0
  Sum_Y_Squared=0.0
  Sum_XY=0.0
  do ii=1,size(X)
    Sum_X=Sum_X+X(ii)
    Sum_Y=Sum_Y+Y(ii)
    Sum_X_Squared=Sum_X_Squared+(X(ii)**2.0)
    Sum_Y_Squared=Sum_Y_Squared+(Y(ii)**2.0)
    Sum_XY=Sum_XY+(X(ii)*Y(ii))
  enddo
  if ( (Sum_X_Squared-(Sum_X**2.0/real(size(X))))*(Sum_Y_Squared-(Sum_Y**2.0/real(size(Y)))) <= 0.0 ) then
    VALUE=-9999.0
  else
    VALUE=(Sum_XY-((Sum_X*Sum_Y)/real(size(X))))                                                                                   &
          /sqrt((Sum_X_Squared-(Sum_X**2.0/real(size(X))))                                                                         &
          *(Sum_Y_Squared-(Sum_Y**2.0/real(size(Y)))))
  endif
  
endif

end subroutine correlation

!*******************************************************************************
! END OF MODULE
end module merge_module
