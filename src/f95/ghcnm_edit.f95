program ghcnm_edit

!******************************************************************************
! ghcnm_edit.f95
! 
! **DESCRIPTION**
! This program, written in FORTRAN, has two functions. The first is to
! remove or change individual year/month points that were found (through
! expert assessment) to be incorrect. The second is to update station
! metadata information, including the stations lat/lon
!
! This program has a module associated with it (stratus.f95)
!   Using g95, the code can be compiled using the following command:
!     g95 stratus.f95 ghcnm_edit.f95
! 
! In addition,there are input files that must be present in the working 
! directory, or else the program will fail:
!   resources/edit.dat
!   resources/ghcnmv4_edit_meta.txt
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
! 11/04/2019: Updated for GHCN-M v4.0.1 operational processing
!******************************************************************************

use stratus

!integer, parameter :: PRCP=1,TMAX=2,TMIN=3,TAVG=4,BYR=1690,MISS=-9999, NUM_ELEMENTS=4
integer, parameter :: TMAX=1,TMIN=2,TAVG=3,BYR=1690,MISS=-9999, NUM_ELEMENTS=3
integer :: EYR
integer, dimension(1:12) :: iValue
integer, allocatable, dimension(:,:,:,:) :: iData
character(len=10000) :: cLine
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
character(len=1024) :: new_name
logical :: lExist

call getarg(1,cArg)
cMetaDataFile=cArg

call getarg(2,cArg)
cDataFile=cArg

call getarg(3,cArg)
cEditMetaFile=cArg

call getarg(4,cArg)
cEditDataFile=cArg

call getarg(5,cArg)
cOutMetaFile=cArg

call getarg(6,cArg)
cOutDataFile=cArg

if (cMetaDataFile=="".or.cDataFile=="".or.cEditMetaFile=="".or.cEditDataFile=="".or.cOutMetaFile=="".or.cOutDataFile=="") then
  write (*,*) ""
  write (*,*) "GHCNM_EDIT, USAGE: <IN_METAFILE> <IN_DATAFILE> <META_EDITFILE> <DATA_EDITFILE> <OUT_META> <OUT_DATA>" 
  write (*,*) ""
  stop
endif

call date_and_time(DATE=cDate)
read(cDate(1:4),'(i4)') EYR

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
! READ IN EDIT META FILE
write(*,'(a)') "READING EDIT (META) FILE"

open(unit=12,file=cEditMetaFile,status='old')
do
  read(12,'(a11,48x,a30,1x,f10.4,1x,f10.4,1x,f8.2)',IOSTAT=iStat) cStation,new_name,new_lat,new_lon,new_elev
  if (iStat==-1) exit
  if (iStat>0) then
    cycle
  endif
  
  call binary_search(cStn(:),cStation,iStn)
  if (iStn==0) then
    write (*,'(a,a)') "WARNING: GHCNM_EDIT: Reference to station not in metadata file, skipping: ",cStation
    cycle
  endif
  cName(iStn)=trim(adjustl(new_name))
  rLatitude(iStn)=new_lat
  rLongitude(iStn)=new_lon
  rElevation(iStn)=new_elev
enddo
close(12)

!*******************************************************************************
! READ IN EDIT DATA FILE
write(*,'(a)') "READING EDIT (DATA) FILE"

cLastStation=""
open(unit=12,file=cEditDataFile,status='old')
do
  read(12,'(a1,1x,a11,1x,a4,1x,i4,1x,i2,1x,i5,1x,a1,1x,a1,1x,a1)',IOSTAT=iStat) cCode,cStation,cElem, &
    iYr,iMo,iVal,cFlag1,cFlag2,cFlag3
  if (iStat==-1) exit
  if (iStat>0) then
    cycle
  endif

  if (iYr<BYR.or.iYr>EYR) cycle

  if (cLastStation/=cStation) then
    call binary_search(cStn(:),cStation,iStn)
    if (iStn==0) then
      !write (*,'(a,a)') "WARNING: GHCNM_EDIT: Reference to station not in metadata file, skipping: ",cStation
      cycle
    endif
    cLastStation=cStation
  endif
  if (iStn==0) cycle

  iElem=0
  !if (cElem=="PRCP") iElem=PRCP
  if (cElem=="TMAX") iElem=TMAX
  if (cElem=="TMIN") iElem=TMIN
  if (cElem=="TAVG") iElem=TAVG
  if (iElem==0) cycle

  if (cCode=="D".or.cCode=="d") then   ! DELAY UNTIL NEW VALUE
    if (iData(iElem,iStn,iYr,iMo)==iVal) then
      iData(iElem,iStn,iYr,iMo)=iVal
      cDM_Flag(iElem,iStn,iYr,iMo)=cFlag1
      cQC_Flag(iElem,iStn,iYr,iMo)=cFlag2
      cDS_Flag(iElem,iStn,iYr,iMo)=cFlag3
    endif
  endif
  if (cCode=="C".or.cCode=="c") then   ! CORRECT, ACCORDING TO NEW VALUE AND FLAGS
    iData(iElem,iStn,iYr,iMo)=iVal
    cDM_Flag(iElem,iStn,iYr,iMo)=cFlag1
    cQC_Flag(iElem,iStn,iYr,iMo)=cFlag2
    cDS_Flag(iElem,iStn,iYr,iMo)=cFlag3
  endif
    
enddo
close(12)

!*******************************************************************************
! OUTPUT NEW META FILE
write(*,'(a)') "OUTPUT NEW INVENTORY FILE"
open(unit=15,file=cOutMetaFile,status='replace')
do iStn=1,NUM_STNS
  write (15,15) cStn(iStn),rLatitude(iStn),rLongitude(iStn),rElevation(iStn),cName(iStn)
enddo
close(15)
15 format(a11,1x,f8.4,1x,f9.4,1x,f6.1,1x,a30)

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
                                cDM_Flag(iElem,iStn,iYr,iMo),cQC_Flag(iElem,iStn,iYr,iMo), &
                                cDS_Flag(iElem,iStn,iYr,iMo),iMo=1,12)
        endif
      endif
    enddo
  enddo
enddo
close(13)
13 format(a11,i4,a4,12(i5,3a1))

end program ghcnm_edit
