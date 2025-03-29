program ghcnm_clean

!******************************************************************************
! ghcnm_clean.f95
! 
! **DESCRIPTION**
! This program, written in FORTRAN, has three functions. The first is to remove stations
! that have less than 10 years of data. Second, it removes stations found to be
! erroneous through expert assessment. Finally, it marks stations with an (*) that will be used
! in the Pairwise Homogeneity Algorithm (PHA)
!
! This program has a module associated with it (stratus.f95)
!   Using g95, the code can be compiled using the following command:
!     g95 stratus.f95 ghcnm_clean.f95
! 
! In addition,there are input files that must be present in the working 
! directory, or else the program will fail:
!   resources/stations_to_asterisk.txt
!   resources/stations_to_remove.txt
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
! 09/15/2016: First revision for GHCN-M v4.0.0
! 03/17/2017: Incorporated list of RBCN stations to keep, regardless of year threshold
! 10/05/2017: Uses year threshold as requirement for each month
! 11/04/2019: Updated for GHCN-M v4.0.1 operational processing
!******************************************************************************

use stratus

integer, parameter :: PRCP=1,TMAX=2,TMIN=3,TAVG=4,BYR=1690,MISS=-9999, NUM_ELEMENTS=4
integer :: EYR
integer, dimension(1:12) :: iValue
integer, dimension(1:12) :: monthly_matchcounter
integer, allocatable, dimension(:,:,:,:) :: iData
character(len=10000) :: cLine
character(len=1000), allocatable, dimension(:) :: cL
character(len=01), allocatable, dimension(:,:,:,:) :: cDM_Flag,cQC_Flag,cDS_Flag
character(len=01), dimension(1:12) :: cDM,cQC,cDS,cDS_Temp
character(len=11), allocatable, dimension(:) :: cStn,cRemStn,cAstkStn,cKeepStn
character(len=11) :: cStation,cLastStation
character(len=04) :: cElem
character(len=1024) :: cArg,cMetaDataFile,cDataFile,cOutDataFile,cOutMetaFile,cStationRemFile,cStationAstkFile,cStationKeepFile
character(len=08) :: cDate
character(len=01) :: cFlag1,cFlag2,cFlag3
character(len=01) :: cCode,asterisk
integer :: year_threshold, month_threshold, year_counter, month_counter, previous_year, counter, exempt_counter
logical :: lExist
logical, allocatable, dimension(:) :: match
character(len=30), allocatable, dimension(:) :: cName
real, allocatable, dimension(:) :: rLatitude,rLongitude,rElevation

call getarg(1,cArg)
cMetaDataFile=cArg

call getarg(2,cArg)
cDataFile=cArg

call getarg(3,cArg)
read (cArg,'(i5)') year_threshold

call getarg(4,cArg)
cStationRemFile=cArg

call getarg(5,cArg)
cStationAstkFile=cArg

call getarg(6,cArg)
cStationKeepFile=cArg

call getarg(7,cArg)
cOutMetaFile=cArg

call getarg(8,cArg)
cOutDataFile=cArg

if (cMetaDataFile=="".or.cDataFile=="".or.cStationRemFile=="".or.cStationAstkFile=="".or.cStationKeepFile=="" &
    .or. cOutMetaFile=="".or.cOutDataFile=="") then
  write (*,*) ""
  write (*,*) "GHCNM_CLEAN, USAGE: <IN_METAFILE> <IN_DATAFILE> <YEAR_THESHOLD> <IN_REM-FILE> <IN_ASTK-FILE> <IN_KEEP-FILE> &
              <OUT_METAFILE> <OUT_DATAFILE>, or --version, --help, --ehelp" 
  write (*,*) ""
  stop
endif

call date_and_time(DATE=cDate)
read(cDate(1:4),'(i4)') EYR
month_threshold=12

!*******************************************************************************
! READING LIST OF STATIONS TO REMOVE
write(*,'(a)') "READING LIST OF STATIONS TO REMOVE"

NUM_REM_STNS=0
open(unit=12,file=cStationRemFile,status='old')
do
  read(12,*,IOSTAT=iStat)
  if (iStat==-1) exit
  NUM_REM_STNS=NUM_REM_STNS+1
enddo
close(12)

allocate(cRemStn(1:NUM_REM_STNS))

iC=0
open(unit=12,file=cStationRemFile,status='old')
do
  read(12,'(a)',IOSTAT=iStat) cLine
  if (iStat==-1) exit
  iC=iC+1
  
  cRemStn(iC)=cLine(1:11)

enddo
close(12)

!*******************************************************************************
! READING LIST OF STATIONS TO ASTKERISK 
write(*,'(a)') "READING LIST OF STATIONS TO ASTKERISK (*) FOR QCF"

NUM_ASTK_STNS=0
open(unit=12,file=cStationAstkFile,status='old')
do
  read(12,*,IOSTAT=iStat)
  if (iStat==-1) exit
  NUM_ASTK_STNS=NUM_ASTK_STNS+1
enddo
close(12)

allocate(cAstkStn(1:NUM_ASTK_STNS))

iC=0
open(unit=12,file=cStationAstkFile,status='old')
do
  read(12,'(a)',IOSTAT=iStat) cLine
  if (iStat==-1) exit
  iC=iC+1
  
  cAstkStn(iC)=cLine(1:11)

enddo
close(12)

!*******************************************************************************
! READING LIST OF STATIONS TO KEEP, REGARDLESS OF YEAR THRESH 
write(*,'(a)') "READING LIST OF STATIONS TO KEEP"

NUM_KEEP_STNS=0
open(unit=12,file=cStationKeepFile,status='old')
do
  read(12,*,IOSTAT=iStat)
  if (iStat==-1) exit
  NUM_KEEP_STNS=NUM_KEEP_STNS+1
enddo
close(12)

allocate(cKeepStn(1:NUM_KEEP_STNS))

iC=0
open(unit=12,file=cStationKeepFile,status='old')
do
  read(12,'(a)',IOSTAT=iStat) cLine
  if (iStat==-1) exit
  iC=iC+1
  
  cKeepStn(iC)=cLine(1:11)

enddo
close(12)

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
  cName(iStn)=cL(iStn)(39:68)
enddo

!*******************************************************************************
! READ IN GHCNM DATA FILE
write(*,'(a)') "READING DATA FILE"

allocate(iData(1:NUM_ELEMENTS,1:NUM_STNS,BYR:EYR,1:12))
iData(:,:,:,:)=MISS
allocate(cDM_Flag(1:NUM_ELEMENTS,1:NUM_STNS,BYR:EYR,1:12))
allocate(cQC_Flag(1:NUM_ELEMENTS,1:NUM_STNS,BYR:EYR,1:12))
allocate(cDS_Flag(1:NUM_ELEMENTS,1:NUM_STNS,BYR:EYR,1:12))
cDM_Flag(:,:,:,:)=" "
cQC_Flag(:,:,:,:)=" "
cDS_Flag(:,:,:,:)=" "
allocate(match(1:NUM_STNS))
match(:) = .false.
monthly_matchcounter(:)=0

cLastStation=""
year_counter=0
open(unit=12,file=cDataFile,status='old')
do
  read(12,'(a11,i4,a4,12(i5,3a1))',IOSTAT=iStat) cStation,iYr,cElem, &
    (iValue(iMo),cDM(iMo),cQC(iMo),cDS(iMo),iMo=1,12)
  if (iStat==-1) exit

  if (iYr<BYR.or.iYr>EYR) cycle

  if (cLastStation/=cStation) then
    year_counter=0
    monthly_matchcounter(:)=0

    call binary_search(cStn(:),cStation,iStn)
    cLastStation=cStation
  endif
  if (iStn==0) cycle

  iElem=0
  if (cElem=="PRCP") iElem=PRCP
  if (cElem=="TMAX") iElem=TMAX
  if (cElem=="TMIN") iElem=TMIN
  if (cElem=="TAVG") iElem=TAVG
  if (iElem==0) cycle

  month_counter = 0
  do iMo=1,12
    iData(iElem,iStn,iYr,iMo)=iValue(iMo)
    if(iData(iElem,iStn,iYr,iMo) /=MISS) then
      month_counter =month_counter +1
      if(iElem==TAVG) then
        monthly_matchcounter(iMo)=monthly_matchcounter(iMo) +1
      endif
    endif
    cDM_Flag(iElem,iStn,iYr,iMo)=cDM(iMo)
    cQC_Flag(iElem,iStn,iYr,iMo)=cQC(iMo)
    cDS_Flag(iElem,iStn,iYr,iMo)=cDS(iMo)
  enddo

  ! Looks for ANY YEARS matching month threshold  (non-strict: only need to match threshold regardless of year)
  match(iStn) = .true.
  do iMo=1,12
    if(monthly_matchcounter(iMo) < year_threshold) then
      match(iStn) = .false.
    endif
  enddo

  previous_year=iYr

enddo
close(12)

!*******************************************************************************
! CHECK TO SEE IF STATIONS ARE ON EXEPMTION LIST
write(*,'(a,1x,i3,1x,a)') "FINDING STATIONS LESS THAN ",year_threshold," YEARS ON EXEMPT LIST"

do iStn=1,NUM_STNS
  if( .not. match(iStn)) then
    do exempt_counter=1,NUM_KEEP_STNS
      if (cStn(iStn) .eq. cKeepStn(exempt_counter)) then
        match(iStn) = .true.
      endif
    enddo
  endif  
enddo

!*******************************************************************************
! OUTPUT VALID DATA
write(*,'(a,1x,i3)') "OUTPUT DATA THAT MATCHES YEAR THRESHOLD OF ",year_threshold
write(*,'(a,1x,i3)') "      NUM_STNS REMOVING: ",NUM_REM_STNS
write(*,'(a,1x,i3)') "      NUM_STNS ADDING * AT END: ",NUM_ASTK_STNS

open(unit=15,file=cOutMetaFile,status='replace')
open(unit=13,file=cOutDataFile,status='replace')
do iStn=1,NUM_STNS

  ! Skip stations that were added to list to be removed (from manual assessment)
  do counter=1,NUM_REM_STNS
    if (cStn(iStn) .eq. cRemStn(counter)) then
      match(iStn) = .false.
    endif
  enddo

  if(match(iStn)) then
    asterisk=" "  
    do counter=1,NUM_ASTK_STNS
      if (cStn(iStn) .eq. cAstkStn(counter)) then
        asterisk="*"  
      endif
    enddo
    
    ! Write Metadata
    write (15,15) cStn(iStn),rLatitude(iStn),rLongitude(iStn),rElevation(iStn),cName(iStn),trim(asterisk)

    ! Write Data
    do iElem=1,NUM_ELEMENTS
      do iYr=BYR,EYR
        if (any(iData(iElem,iStn,iYr,:)/=MISS)) then
          if (iElem==PRCP) cElem="PRCP"
          if (iElem==TAVG) cElem="TAVG"
          if (iElem==TMAX) cElem="TMAX"
          if (iElem==TMIN) cElem="TMIN"
          write (13,13) cStn(iStn),iYr,cElem,(iData(iElem,iStn,iYr,iMo), &
            cDM_Flag(iElem,iStn,iYr,iMo),cQC_Flag(iElem,iStn,iYr,iMo), &
            cDS_Flag(iElem,iStn,iYr,iMo),iMo=1,12)
        endif
      enddo
    enddo
  endif
enddo
close(13)
close(15)
13 format(a11,i4,a4,12(i5,3a1))
15 format(a11,1x,f8.4,1x,f9.4,1x,f6.1,1x,a30,1x,a1)

end program ghcnm_clean
