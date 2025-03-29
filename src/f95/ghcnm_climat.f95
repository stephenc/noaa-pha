program ghcnm_climat

!******************************************************************************
! ghcnm_climat.f95
! 
! **DESCRIPTION**
! This program, written in FORTRAN, looks for cases where "S" flag data
! Can be replaced by a more reliable CLIMAT source from the MetOffice 
!
! This program has a module associated with it (stratus.f95)
!   Using g95, the code can be compiled using the following command:
!     g95 stratus.f95 ghcnm_edit.f95
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
! 05/06/2018: First revision for GHCN-M v4.0.0
! 11/04/2019: Updated for GHCN-M v4.0.1 operational processing
!******************************************************************************

use stratus

character(len=1024) :: databank_directory
character(len=1024) :: version = "v1.1.1"

! Info for WMO cross reference file
character(len=1024) :: wmo_file
integer :: wmo_num_stations, wmo_counter
character(len=1024), allocatable, dimension(:) :: wmo_id1
character(len=1024), allocatable, dimension(:) :: wmo_id2

integer, parameter :: TMAX=1,TMIN=2,TAVG=3,BYR=1690,MISS=-9999, NUM_ELEMENTS=3
integer :: EYR,year_counter,month_counter,station_counter
integer, dimension(1:3) :: candidate_data
integer, dimension(1:12) :: iValue
integer, allocatable, dimension(:,:,:,:) :: iData
character(len=10000) :: cLine,readline
character(len=1000), allocatable, dimension(:) :: cL
character(len=01), allocatable, dimension(:,:,:,:) :: cDM_Flag,cQC_Flag,cDS_Flag
character(len=01), dimension(1:12) :: cDM,cQC,cDS,cDS_Temp
character(len=11), allocatable, dimension(:) :: cStn
character(len=11) :: cStation,cLastStation
character(len=04) :: cElem
character(len=200) :: cArg,cMetaDataFile,cDataFile,cOutMetaFile,cOutDataFile,cEditDataFile,cEditMetaFile
character(len=08) :: cDate
character(len=01) :: cFlag1,cFlag2,cFlag3
character(len=01) :: cCode
character(len=1024), allocatable, dimension(:) :: cName
real, allocatable, dimension(:) :: rLatitude,rLongitude,rElevation
real :: new_lat,new_lon,new_elev
character(len=1024) :: new_name,candidate_datafile
logical :: lExist, file_exists, wmo_boolean, has_climat


call getarg(1,cArg)
databank_directory=cArg

call getarg(2,cArg)
cMetaDataFile=cArg

call getarg(3,cArg)
cDataFile=cArg

call getarg(4,cArg)
cOutDataFile=cArg

if (databank_directory=="".or.cMetaDataFile=="".or.cDataFile=="" .or. cOutDataFile=="") then
  write (*,*) ""
  write (*,*) "GHCNM_CLIMAT, USAGE: <DATABANK_DIRECTORY> <IN_METAFILE> <IN_DATAFILE> <OUT_DATAFILE>" 
  write (*,*) ""
  stop
endif

call date_and_time(DATE=cDate)
read(cDate(1:4),'(i4)') EYR

!*******************************************************************************
! Read in WMO Cross-Reference File
write(*,'(a)') "READING WMO CROSS-REF FILE"

wmo_file=trim(databank_directory)//'resources/wmo_reference-'//trim(adjustl(version))//'.txt' 
inquire(file=wmo_file,exist=file_exists)
if (file_exists .eqv. .false.) stop "ERROR: cannot find WMO cross-reference file"

call file_recs(FILE=wmo_file,UNIT=20,NUM_RECS=wmo_num_stations)
allocate(wmo_id1(1:wmo_num_stations))
allocate(wmo_id2(1:wmo_num_stations))

line_counter = 0
open(unit=1,file=wmo_file,status='OLD')
do
  read(1,'(a1024)',IOSTAT=ioerror) readline
  if (ioerror==-1) exit

  line_counter=line_counter+1

  wmo_id1(line_counter)=readline(1:11)
  wmo_id2(line_counter)=readline(13:17)

enddo
close(1)

!*******************************************************************************
! READ IN GHCNM INVENTORY FILE
write(*,'(a)') "READING INVENTORY FILE"

NUM_STNS=0
open(unit=12,file=cMetaDataFile,status='old')
do
  read(12,*,IOSTAT=iStat)
  if (iStat==-1) exit
  NUM_STNS=NUM_STNS+1
enddo
close(12)

allocate(cL(1:NUM_STNS))
allocate(cStn(1:NUM_STNS))
allocate(rLatitude(1:NUM_STNS))
allocate(rLongitude(1:NUM_STNS))
allocate(rElevation(1:NUM_STNS))
allocate(cName(1:NUM_STNS))
iC=0
open(unit=12,file=cMetaDataFile,status='old')
do
  read(12,'(a)',IOSTAT=iStat) cLine
  if (iStat==-1) exit
  iC=iC+1
  cL(iC)=cLine(1:len(cL(iC)))
enddo
close(12)

call sort_char(cL(1:iC))

do iStn=1,iC
  cStn(iStn)=cL(iStn)(1:11)
  read (cL(iStn)(13:20),'(f8.4)') rLatitude(iStn)
  read (cL(iStn)(22:30),'(f9.4)') rLongitude(iStn)
  read (cL(iStn)(32:37),'(f6.1)') rElevation(iStn)
  cName(iStn)=trim(adjustl(cL(iStn)(39:68)))
enddo

!*******************************************************************************
! READ IN GHCNM DATA FILE
write(*,'(a)') "READING DATA FILE"
write(*,*) NUM_STNS

allocate(iData(1:NUM_ELEMENTS,1:NUM_STNS,BYR:EYR,1:12))
iData(:,:,:,:)=MISS
allocate(cDM_Flag(1:NUM_ELEMENTS,1:NUM_STNS,BYR:EYR,1:12))
allocate(cQC_Flag(1:NUM_ELEMENTS,1:NUM_STNS,BYR:EYR,1:12))
allocate(cDS_Flag(1:NUM_ELEMENTS,1:NUM_STNS,BYR:EYR,1:12))
cDM_Flag(:,:,:,:)=" "
cQC_Flag(:,:,:,:)=" "
cDS_Flag(:,:,:,:)=" "

cLastStation=""
open(unit=12,file=cDataFile,status='old')
do
  read(12,'(a11,i4,a4,12(i5,3a1))',IOSTAT=iStat) cStation,iYr,cElem, &
    (iValue(iMo),cDM(iMo),cQC(iMo),cDS(iMo),iMo=1,12)
  if (iStat==-1) exit

  if (iYr<BYR.or.iYr>EYR) cycle

  if (cLastStation/=cStation) then
    call binary_search(cStn(:),cStation,iStn)
    cLastStation=cStation
  endif
  if (iStn==0) cycle

  iElem=0
  !if (cElem=="PRCP") iElem=PRCP
  if (cElem=="TMAX") iElem=TMAX
  if (cElem=="TMIN") iElem=TMIN
  if (cElem=="TAVG") iElem=TAVG
  if (iElem==0) cycle

  do iMo=1,12
    iData(iElem,iStn,iYr,iMo)=iValue(iMo)
    cDM_Flag(iElem,iStn,iYr,iMo)=cDM(iMo)
    cQC_Flag(iElem,iStn,iYr,iMo)=cQC(iMo)
    cDS_Flag(iElem,iStn,iYr,iMo)=cDS(iMo)
  enddo
enddo
close(12)


!*******************************************************************************
! RUN ANALYSIS
write(*,'(a)') "RUN ANALYSIS"

do station_counter=1,NUM_STNS
  wmo_boolean=.false.
  has_climat=.false.
  do wmo_counter=1,wmo_num_stations
    if (cStn(station_counter) .eq.  wmo_id1(wmo_counter)) then
      wmo_boolean=.true.
      candidate_datafile=trim(databank_directory)//'tmp/climat-uk/climat-uk_'//trim(wmo_id2(wmo_counter))//'_monthly_stage2'
      inquire(file=candidate_datafile,exist=file_exists)

      if(wmo_boolean .and. file_exists) then
        ! V4 Station is in Cross Reference File, AND there is a CLIMAT-UK Version
        ! of this station. Open the file

        open(unit=1,file=candidate_datafile,status='OLD')
        do
          read(1,'(a1024)',IOSTAT=ioerror) readline
          if (ioerror==-1) exit
          
          read (readline(63:66),'(i4)') year_counter
          read (readline(67:68),'(i2)') month_counter
 
          read (readline(72:76),'(i5)') candidate_data(TMAX)
          read (readline(78:82),'(i5)') candidate_data(TMIN)
          read (readline(84:88),'(i5)') candidate_data(TAVG)
          
          do iElem=1,3
            if(cDS_Flag(iElem,station_counter,year_counter,month_counter) .eq. "S" .and. candidate_data(iElem) /= -9999) then
              
              ! Replace S Flag Data with UK ClIMAT Data ('|' Flag)
              iData(iElem,station_counter,year_counter,month_counter) = candidate_data(iElem)
              cDS_Flag(iElem,station_counter,year_counter,month_counter) = '|'
            endif           
          enddo

        enddo
      endif
    endif
  enddo  
enddo

!*******************************************************************************
! OUTPUT NEW DATA FILE
write(*,'(a)') "OUTPUT NEW DATA FILE"
open(unit=13,file=cOutDataFile,status='replace')
do iStn=1,NUM_STNS
  do iElem=1,NUM_ELEMENTS
    do iYr=BYR,EYR
      if (any(iData(iElem,iStn,iYr,:)/=MISS)) then
        !if (iElem==PRCP) cElem="PRCP"
        if (iElem==TAVG) cElem="TAVG"
        if (iElem==TMAX) cElem="TMAX"
        if (iElem==TMIN) cElem="TMIN"
        do iMo=1,12
          if (iData(iElem,iStn,iYr,iMo) > 99999) then
            iData(iElem,iStn,iYr,iMo)=MISS
          endif
        enddo
        if (any(iData(iElem,iStn,iYr,:)/=MISS)) then
          write (13,13) cStn(iStn),iYr,cElem,(iData(iElem,iStn,iYr,iMo), &
                                cDM_Flag(iElem,iStn,iYr,iMo),cQC_Flag(iElem,iStn,iYr,iMo),&
                                cDS_Flag(iElem,iStn,iYr,iMo),iMo=1,12)
        endif
      endif
    enddo
  enddo
enddo
close(13)
13 format(a11,i4,a4,12(i5,3a1))
end program ghcnm_climat
