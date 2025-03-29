program ghcnm_qc

! Version 1.7, 12/12/2018

! National Climatic Data Center (NCDC)

! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC DOMAIN
! AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE.  THEY ARE FURNISHED 
! "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS INSTRUMENTALITIES, 
! OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY, EXPRESS OR IMPLIED, AS TO
! THE USEFULNESS OF THE SOFTWARE AND DOCUMENTATION FOR ANY PURPOSE. THEY
! ASSUME NO RESPONSIBILITY (1) FOR THE USE OF THE SOFTWARE AND DOCUMENTATION;
! OR (2) TO PROVIDE TECHNICAL SUPPORT TO USERS.

! PURPOSE: This program reads in a GHCNM v4.x.x input inventory and data
! file and runs a series of quality control steps on the data, and when a 
! particular datum fails, the data quality flag is set to a designated
! flag value. Two output files are produced:
!
! ghcnm_qc.out
! ghcnm_qc.diagnostics
!

use stratus     ! use stratus 1.4
use ghcnm_read  ! use ghcnm_read 1.0

implicit none

type(ghcnm_structure) :: ghcnm

! Parameters

integer, parameter :: BEGIN_YEAR=GHCNM_BEGIN_YEAR
integer, parameter :: NUM_PARAMETERS=14
real, dimension(1:NUM_PARAMETERS) :: rParam

! End Parameters

integer :: iPrevMo
integer :: END_YEAR
integer :: iPrevYr
integer :: iDup
integer :: iK
integer :: jj
integer :: iL
integer, allocatable, dimension(:,:,:,:) :: iMoMoDup
integer :: MIN_BUFFER
integer :: MIN_STATS
integer :: MIN_CORR
integer :: MAX_INTERDUP_MONTHS
integer :: MIN_INTERDUP_MONTHS
integer :: MIN_NEIGHBORS
integer :: STREAK_THRESHOLD
integer, dimension(1:12) :: iVal
integer, dimension(1:100000) :: iVals
integer, dimension(1:100000) :: iYear
integer, dimension(1:100000) :: iMonth
integer :: iSign
integer, allocatable, dimension(:) :: iLoc
integer, allocatable, dimension(:) :: zone
real :: WORLD_EXTREME_HIGH
real :: WORLD_EXTREME_LOW
real :: SPATIAL_ZSCORE_THRESHOLD
real :: MAX_DISTANCE
real :: DUP_DIFFERENCE
real :: BIWEIGHT_CENSOR_THRESHOLD
real :: rDiff
real :: ZSCORE_THRESHOLD
real, allocatable, dimension(:) :: ISO_MASK1, ISO_MASK2,ISO_MASK3
real, allocatable, dimension(:) :: rZScoreTemp
integer, allocatable, dimension(:) :: iIsoYr
integer, allocatable, dimension(:) :: iIsoMo
integer, allocatable, dimension(:) :: iStreak
integer, allocatable, dimension(:) :: iNumNeighbors
integer, allocatable, dimension(:,:) :: iNeighbor
real, allocatable, dimension(:,:,:) :: rMean,rStDev
real, allocatable, dimension(:,:,:,:) :: zscore
real, allocatable, dimension(:,:) :: rNeighDistLog
real, allocatable, dimension(:) :: rNeighborDist
real, dimension(1:100000) :: rTemp,rTempDist
real :: idwm
integer :: iStn,iElem,iYr1,iYr2,iYr,iMo,iC,ii,iStat,iStn1,iStn2,iF
real :: Z,rDist,rMult
character(len=50) :: cVal
character(len=200) :: cConfigFile
character(len=200) :: cLine
character(len=200) :: cMetaDataFile
character(len=200) :: cDataFile
character(len=200) :: cArg
character(len=20) :: cDate
character(len=100) :: cOutFile="ghcnm_qc.out"
character(len=100) :: cQCDiagnostics="ghcnm_qc.diagnostics"
character(len=25), dimension(1:num_parameters) :: cParameters
logical :: lExist
logical :: lStreak
logical :: READ_PREVIOUS_QC_FLAGS
logical :: READ_PREVIOUS_QC_FLAGS_ZSCORES

call date_and_time(DATE=cDate)
read (cDate(1:4),'(i4)') END_YEAR

!*******************************************************************************
! Assignments

cParameters(01)="MIN_BUFFER"
cParameters(02)="MIN_STATS"
cParameters(03)="MIN_NEIGHBORS"
cParameters(04)="MIN_CORR"
cParameters(05)="MAX_INTERDUP_MONTHS"
cParameters(06)="MIN_INTERDUP_MONTHS"
cParameters(07)="STREAK_THRESHOLD"
cParameters(08)="WORLD_EXTREME_HIGH"
cParameters(09)="WORLD_EXTREME_LOW"
cParameters(10)="BIWEIGHT_CENSOR_THRESHOLD"
cParameters(11)="SPATIAL_ZSCORE_THRESHOLD"
cParameters(12)="ZSCORE_THRESHOLD"
cParameters(13)="MAX_DISTANCE"
cParameters(14)="DUP_DIFFERENCE"

! User Input

call getarg(1,cArg)
cMetaDataFile=cArg

call getarg(2,cArg)
cDataFile=cArg

call getarg(3,cArg)
cConfigFile=cArg

if (cMetaDataFile(1:31)=="--auto-generate-parameter-file=") then
  if (cMetaDataFile(32:)=="") stop "No auto-generate-parameter-file=name specified!"
  open(unit=13,file=cMetaDataFile(32:),status='replace')
  write (13,'(a)') "MIN_BUFFER=5"
  write (13,'(a)') "MIN_STATS=10"
  write (13,'(a)') "MIN_NEIGHBORS=5"
  write (13,'(a)') "MIN_CORR=15"
  write (13,'(a)') "MAX_INTERDUP_MONTHS=12"
  write (13,'(a)') "MIN_INTERDUP_MONTHS=3"
  write (13,'(a)') "STREAK_THRESHOLD=5"
  write (13,'(a)') "WORLD_EXTREME_HIGH=4229.0"
  write (13,'(a)') "WORLD_EXTREME_LOW=-7500.0"
  write (13,'(a)') "BIWEIGHT_CENSOR_THRESHOLD=7.5"
  write (13,'(a)') "SPATIAL_ZSCORE_THRESHOLD=3.0"
  write (13,'(a)') "ZSCORE_THRESHOLD=5.0"
  write (13,'(a)') "MAX_DISTANCE=500.0"
  write (13,'(a)') "DUP_DIFFERENCE=1.50"
  close(13)
  stop
endif

if (cMetaDataFile=="".or.cDataFile=="".or.cMetaDatafile=="--help".or.cMetaDataFile=="--version") then
  write (*,*) "!"
  write (*,*) "GHCNM_QC, Version 1.7, 12/19/2018"
  write (*,*) ""
  write (*,*) "USAGE: REQUIRED: <METADATAFILE> <DATAFILE> <PARAMETER FILE>"
  write (*,*) ""
  write (*,*) "or optional"
  write (*,*) ""
  write (*,*) "USAGE: --version"
  write (*,*) "USAGE: --help"
  write (*,*) "USAGE: --auto-generate-parameter-file"
  write (*,*) ""
  write (*,*) "Example: ghcnm_qc input.inv input.dat" 
  write (*,*) ""
  stop
endif

if (cMetaDataFile=="--version") then
  write (*,*) " "
  write (*,*) "GHCNM_QC, Version 1.7, 12/19/2018"
  write (*,*) " "
  stop
endif

inquire(file=cConfigFile,exist=lExist)
if (lExist.eqv..false.) then
  write (*,*) ""
  write (*,*) "GHCNM_QC, Version 1.7, 12/19/2018"
  write (*,*) ""
  write (*,*) "USAGE: REQUIRED: <METADATAFILE> <DATAFILE> <PARAMETER FILE>"
  write (*,*) ""
  write (*,*) "or optional"
  write (*,*) ""
  write (*,*) "USAGE: --version"
  write (*,*) "USAGE: --help"
  write (*,*) "USAGE: --auto-generate-parameter-file"
  write (*,*) ""
  write (*,*) "Example: ghcnm_qc input.inv input.dat parameters.dat" 
  write (*,*) ""
  stop
endif

!*******************************************************************************
! Read in Thresholds

rParam(:)=0.0
open(unit=12,file=cConfigFile,status='old')
do
  read(12,'(a)',IOSTAT=iStat) cLine
  if (iStat==-1) exit

  do ii=1,num_parameters
    if (index(cLine,trim(cParameters(ii)))>0) then
      iSign=index(cLine,"=")
      if (iSign>0) then
        iL=iSign+1
        cVal=adjustl(trim(cLine(iL:)))
        read(cVal,*,IOSTAT=iStat) rParam(ii)
      endif
      if (iStat>0) stop "ERROR: ghcnm_qc, invalid threshold parameter!"
    endif
  enddo

enddo
close(12)

MIN_BUFFER=nint(rParam(01))
MIN_STATS=nint(rParam(02))
MIN_NEIGHBORS=nint(rParam(03))
MIN_CORR=nint(rParam(04))
MAX_INTERDUP_MONTHS=nint(rParam(05))
MIN_INTERDUP_MONTHS=nint(rParam(06))
STREAK_THRESHOLD=nint(rParam(07))
WORLD_EXTREME_HIGH=rParam(08)
WORLD_EXTREME_LOW=rParam(09)
BIWEIGHT_CENSOR_THRESHOLD=rParam(10)
SPATIAL_ZSCORE_THRESHOLD=rParam(11)
ZSCORE_THRESHOLD=rParam(12)
MAX_DISTANCE=rParam(13)
DUP_DIFFERENCE=rParam(14)

!*******************************************************************************
!  Get Data

call get_ghcnm(metadatafile=cMetaDataFile,datafile=cDataFile,data_structure=ghcnm,begin_year=BEGIN_YEAR,end_year=END_YEAR)

! Convert to whole deg C and whole mm

ghcnm%data_quality(:,:,:,:)=" "

open(unit=27,file=cQCDiagnostics,status='replace')

!*******************************************************************************
! Quality Control

! Inter Station Duplicate Check

do iStn1=1,ghcnm%number_of_stations
  do iStn2=iStn1+1,ghcnm%number_of_stations
    if (iStn1==iStn2) cycle
    do iYr=ghcnm%first_year(iStn1,GHCNM_TAVG),ghcnm%last_year(iStn1,GHCNM_TAVG)
      iDup=0
      iF=0
      do iMo=1,12
        rDiff=abs(ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn1)-ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn2))
        if (ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn1)/=GHCNM_MISSING) iF=iF+1
        if (rDiff<=DUP_DIFFERENCE) then
          iDup=iDup+1
        endif
      enddo
      if (iDup==MAX_INTERDUP_MONTHS.and.iF>=MIN_INTERDUP_MONTHS) then
        do iMo=1,12
          if (ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn1)/=GHCNM_MISSING) then
            ghcnm%data_quality(iMo,iYr,GHCNM_TAVG,iStn1)="E"
            write (27,'(a1,1x,a11,1x,i4,1x,i2.2,1x,a4,1x,f10.2)') "E",ghcnm%station_id(iStn1),iYr,iMo, &
                                                                  GHCNM_ELEMENT(GHCNM_TAVG),ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn1)
          endif
          if (ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn2)/=GHCNM_MISSING) then
            ghcnm%data_quality(iMo,iYr,GHCNM_TAVG,iStn2)="E"
            write (27,'(a1,1x,a11,1x,i4,1x,i2.2,1x,a4,1x,f10.2)') "E",ghcnm%station_id(iStn2),iYr,iMo, &
                                                                  GHCNM_ELEMENT(GHCNM_TAVG),ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn2)
          endif
        enddo
      endif
    enddo
  enddo
enddo

! Check for duplicate years

do iStn=1,ghcnm%number_of_stations
  do iElem=1,GHCNM_NUMBER_OF_ELEMENTS
    do iYr1=BEGIN_YEAR,END_YEAR-1
      do iYr2=BEGIN_YEAR+1,END_YEAR
        if (iYr1==iYr2) cycle
        if (count(ghcnm%data(:,iYr1,iElem,iStn)/=GHCNM_MISSING)>1) then
          if (iElem==GHCNM_PRCP.and.(count(ghcnm%data(:,iYr1,iElem,iStn)==0.0) +   &
             count(ghcnm%data(:,iYr1,iElem,iStn)==GHCNM_MISSING)==12)) cycle
          if (count(ghcnm%data(:,iYr1,iElem,iStn)==ghcnm%data(:,iYr2,iElem,iStn))==12) then
            ghcnm%data_quality(:,iYr1,iElem,iStn)="D"
            ghcnm%data_quality(:,iYr2,iElem,iStn)="D"
            write (27,'(a1,1x,a11,1x,i4,1x,i4,1x,a4,1x,12(f10.2))') "D",ghcnm%station_id(iStn),iYr1,iYr2,&
                                                                    ghcnm_element(iElem), ghcnm%data(:,iYr1,iElem,iStn)
          endif
        endif
      enddo
    enddo
  enddo
enddo

! Record Extremes Check (TAVG)

do iStn=1,ghcnm%number_of_stations
  do iYr=ghcnm%first_year(iStn,GHCNM_TAVG),ghcnm%last_year(iStn,GHCNM_TAVG)
    do iMo=1,12
      if (ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn)/=GHCNM_MISSING.and.ghcnm%data_quality(iMo,iYr,GHCNM_TAVG,iStn)==" ") then
        if (ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn) > world_extreme_high) then
          ghcnm%data_quality(iMo,iYr,GHCNM_TAVG,iStn)="R"
          write (27,'(a1,1x,a11,1x,i4,1x,i2.2,1x,a4,1x,f10.2)') "R",ghcnm%station_id(iStn),iYr,iMo, &
                                                                GHCNM_ELEMENT(GHCNM_TAVG),ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn)
        endif
        if (ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn) < world_extreme_low) then
          ghcnm%data_quality(iMo,iYr,GHCNM_TAVG,iStn)="R"
          write (27,'(a1,1x,a11,1x,i4,1x,i2.2,1x,a4,1x,f10.2)') "R",ghcnm%station_id(iStn),iYr,iMo, &
                                                                GHCNM_ELEMENT(GHCNM_TAVG),ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn)
        endif
      endif
    enddo
  enddo
enddo

! Streak Check

do iElem=1,GHCNM_NUMBER_OF_ELEMENTS
  if (iElem==GHCNM_PRCP) cycle      ! Skip PRCP
  do iStn=1,ghcnm%number_of_stations
    iC=0
    iVals(:)=GHCNM_MISSING_INTEGER
    do iYr=BEGIN_YEAR,END_YEAR
      do iMo=1,12
        iC=iC+1
        iVals(iC)=nint(ghcnm%data(iMo,iYr,iElem,iStn))
        iYear(iC)=iYr
        iMonth(iC)=iMo
      enddo
    enddo

    lStreak=.false.
    do ii=2,iC
      if (iVals(ii)==iVals(ii-1)) then
        if (lStreak.eqv..false.) then
          iK=2
          lStreak=.true.
        else
          iK=iK+1
        endif
      else
        if (lStreak.eqv..true.) then
          if (iK>=STREAK_THRESHOLD.and.iVals(ii-1)>GHCNM_MISSING_INTEGER) then
            do jj=ii-iK,ii-1
              ghcnm%data_quality(iMonth(jj),iYear(jj),iElem,iStn)="K"
              write (27,'(a1,1x,a11,1x,a4,1x,i4,1x,i2.2,f10.2)') "K",ghcnm%station_id(iStn),ghcnm_element(iElem), &
                                                                 iYear(jj),iMonth(jj), &
                                                                 ghcnm%data(iMonth(jj),iYear(jj),iElem,iStn)
            enddo
          endif
          lStreak=.false.
          iK=0
        endif
      endif
    enddo
  enddo
enddo

where(ghcnm%data(:,:,GHCNM_TAVG,:)/=GHCNM_MISSING) ghcnm%data(:,:,GHCNM_TAVG,:)=ghcnm%data(:,:,GHCNM_TAVG,:)*0.01
where(ghcnm%data(:,:,GHCNM_TMAX,:)/=GHCNM_MISSING) ghcnm%data(:,:,GHCNM_TMAX,:)=ghcnm%data(:,:,GHCNM_TMAX,:)*0.01
where(ghcnm%data(:,:,GHCNM_TMIN,:)/=GHCNM_MISSING) ghcnm%data(:,:,GHCNM_TMIN,:)=ghcnm%data(:,:,GHCNM_TMIN,:)*0.01
where(ghcnm%data(:,:,GHCNM_PRCP,:)/=GHCNM_MISSING) ghcnm%data(:,:,GHCNM_PRCP,:)=ghcnm%data(:,:,GHCNM_PRCP,:)*0.1

! Consecutive Duplicate Month Check

allocate(zone(1:ghcnm%number_of_stations))
allocate(iLoc(1:ghcnm%number_of_stations))
allocate(iMoMoDup(1:12,BEGIN_YEAR:END_YEAR,1:GHCNM_NUMBER_OF_ELEMENTS,1:ghcnm%number_of_stations))
iMoMoDup(:,:,:,:)=0
do iStn=1,ghcnm%number_of_stations
  if (ghcnm%latitude(iStn)<-30.0) zone(iStn)=1
  if (ghcnm%latitude(iStn)>=-30.0.and.ghcnm%latitude(iStn)<=30.0) zone(iStn)=2
  if (ghcnm%latitude(iStn)>30.0) zone(iStn)=3
  do iElem=1,GHCNM_NUMBER_OF_ELEMENTS
    if (iElem==GHCNM_PRCP) cycle      ! Skip PRCP
    do iYr=2000,END_YEAR
      do iMo=1,12
        if (iMo==1) then
          iPrevMo=12
          iPrevYr=iYr-1
        else
          iPrevMo=iMo-1
          iPrevYr=iYr
        endif

        if (ghcnm%data(iMo,iYr,iElem,iStn)/=GHCNM_MISSING.and.ghcnm%data(iPrevMo,iPrevYr,iElem,iStn)/=GHCNM_MISSING) then
!         Only apply this check to CLIMAT-related data 
          if (ghcnm%data_source(iMo,iYr,iElem,iStn)==",".or.ghcnm%data_source(iMo,iYr,iElem,iStn)=="}".or. &
              ghcnm%data_source(iMo,iYr,iElem,iStn)=="P".or.ghcnm%data_source(iMo,iYr,iElem,iStn)=="|".or. &
              ghcnm%data_source(iMo,iYr,iElem,iStn)=="D".or.ghcnm%data_source(iMo,iYr,iElem,iStn)=="3") then
            if (ghcnm%data_source(iPrevMo,iPrevYr,iElem,iStn)==",".or.ghcnm%data_source(iPrevMo,iPrevYr,iElem,iStn)=="}".or. &
                ghcnm%data_source(iPrevMo,iPrevYr,iElem,iStn)=="P".or.ghcnm%data_source(iPrevMo,iPrevYr,iElem,iStn)=="|".or. &
                ghcnm%data_source(iPrevMo,iPrevYr,iElem,iStn)=="D".or.ghcnm%data_source(iPrevMo,iPrevYr,iElem,iStn)=="3") then
              if (ghcnm%data(iMo,iYr,iElem,iStn)==ghcnm%data(iPrevMo,iPrevYr,iElem,iStn)) iMoMoDup(iMo,iYr,iElem,iStn)=1
            endif
          endif
       endif
      enddo
    enddo
  enddo
enddo

do iStn=1,ghcnm%number_of_stations
  do iElem=1,GHCNM_NUMBER_OF_ELEMENTS
    if (iElem==GHCNM_PRCP) cycle      ! Skip PRCP
    do iYr=2000,END_YEAR
      do iMo=1,12
        if (iMoMoDup(iMo,iYr,iElem,iStn)==1) then
          iC=0
          do iStn2=1,ghcnm%number_of_stations
            if (zone(iStn)/=zone(iStn2)) cycle
            if ((ghcnm%station_id(iStn)(1:3)==ghcnm%station_id(iStn2)(1:3)).and.(iMoMoDup(iMo,iYr,iElem,iStn2)==1)) then
              iC=iC+1
              iLoc(iC)=iStn2
            endif
          enddo
          if ((zone(iStn)==1.or.zone(iStn)==3).and.(iC>1)) then
            do ii=1,iC
              ghcnm%data_quality(iMo,iYr,iElem,iLoc(ii))="W"
              write (27,'(a1,1x,a11,1x,i4,1x,i2.2,1x,a4,1x,f10.2)') "W",ghcnm%station_id(iStn),iYr,iMo, &
                                                                    GHCNM_ELEMENT(iElem),ghcnm%data(iMo,iYr,iElem,iStn)
            enddo
          endif
          if (zone(iStn)==2.and.iC>2) then
            do ii=1,iC
              ghcnm%data_quality(iMo,iYr,iElem,iLoc(ii))="W"
              write (27,'(a1,1x,a11,1x,i4,1x,i2.2,1x,a4,1x,f10.2)') "W",ghcnm%station_id(iStn),iYr,iMo, &
                                                                    GHCNM_ELEMENT(iElem),ghcnm%data(iMo,iYr,iElem,iStn)
            enddo
          endif
        endif
      enddo
    enddo
  enddo
enddo

! TMAX < TMIN Check

do iStn=1,ghcnm%number_of_stations
  do iYr=BEGIN_YEAR,END_YEAR
    do iMo=1,12
      if (ghcnm%data(iMo,iYr,GHCNM_TMAX,iStn)/=GHCNM_MISSING.and.ghcnm%data(iMo,iYr,GHCNM_TMIN,iStn)/=GHCNM_MISSING) then
        if (ghcnm%data_quality(iMo,iYr,GHCNM_TMAX,iStn)==" ".and.ghcnm%data_quality(iMo,iYr,GHCNM_TMIN,iStn)==" ") then
          if (ghcnm%data(iMo,iYr,GHCNM_TMAX,iStn)<ghcnm%data(iMo,iYr,GHCNM_TMIN,iStn)) then
            ghcnm%data_quality(iMo,iYr,GHCNM_TMAX,iStn)="I"
            ghcnm%data_quality(iMo,iYr,GHCNM_TMIN,iStn)="I"
            write (27,'(a1,1x,a11,1x,i4,1x,i2.2)') "I",ghcnm%station_id(iStn),iYr,iMo
          endif
        endif
      endif
    enddo
  enddo
enddo
       
! Check for isolated values

allocate(iIsoYr(1:(( (END_YEAR+MIN_BUFFER)-(BEGIN_YEAR-MIN_BUFFER) )+1)*12))
allocate(iIsoMo(1:(( (END_YEAR+MIN_BUFFER)-(BEGIN_YEAR-MIN_BUFFER) )+1)*12))
allocate(iStreak(1:(( (END_YEAR+MIN_BUFFER)-(BEGIN_YEAR-MIN_BUFFER) )+1)*12))

allocate(ISO_MASK1(1:37)); allocate(ISO_MASK2(1:38)); allocate(ISO_MASK3(1:39))
ISO_MASK1(1:18)=GHCNM_MISSING; ISO_MASK1(19)=0.0; ISO_MASK1(20:37)=GHCNM_MISSING;
ISO_MASK2(1:18)=GHCNM_MISSING; ISO_MASK2(19:20)=0.0; ISO_MASK2(21:38)=GHCNM_MISSING;
ISO_MASK3(1:18)=GHCNM_MISSING; ISO_MASK3(19:21)=0.0; ISO_MASK3(22:39)=GHCNM_MISSING;
do iStn=1,ghcnm%number_of_stations
  do iElem=1,GHCNM_NUMBER_OF_ELEMENTS
    iC=0
    rTemp(:)=GHCNM_MISSING
    do iYr=BEGIN_YEAR-MIN_BUFFER,END_YEAR+MIN_BUFFER
      do iMo=1,12
        iC=iC+1
        if (iYr>=BEGIN_YEAR.and.iYr<=END_YEAR) then
          if (ghcnm%data(iMo,iYr,iElem,iStn)/=GHCNM_MISSING) rTemp(iC)=0.0
        endif
        iIsoYr(iC)=iYr
        iIsoMo(iC)=iMo
      enddo
    enddo
    do ii=21,iC-21
      if (all(ISO_MASK1(:)==rTemp(ii-18:ii+18))) then
        ghcnm%data_quality(iIsoMo(ii),iIsoYr(ii),iElem,iStn)="L"
        write (27,'(a1,1x,a11,1x,a4,1x,i4,1x,i2.2,f10.1)') "L",ghcnm%station_id(iStn), &
                                                           ghcnm_element(iElem),iIsoYr(ii),iIsoMo(ii), &
                                                           ghcnm%data(iIsoMo(ii),iIsoYr(ii),iElem,iStn)
      endif
      if (all(ISO_MASK2(:)==rTemp(ii-18:ii+19))) then
        ghcnm%data_quality(iIsoMo(ii),iIsoYr(ii),iElem,iStn)="L"
        ghcnm%data_quality(iIsoMo(ii+1),iIsoYr(ii+1),iElem,iStn)="L"
        write (27,'(a1,1x,a11,1x,a4,1x,i4,1x,i2.2,f10.1)') "L",ghcnm%station_id(iStn),ghcnm_element(iElem),iIsoYr(ii),iIsoMo(ii),&
                                                           ghcnm%data(iIsoMo(ii),iIsoYr(ii),iElem,iStn)
        write (27,'(a1,1x,a11,1x,a4,1x,i4,1x,i2.2,f10.1)') "L",ghcnm%station_id(iStn),ghcnm_element(iElem),iIsoYr(ii+1), &
                                                           iIsoMo(ii+1), ghcnm%data(iIsoMo(ii),iIsoYr(ii),iElem,iStn)
      endif
      if (all(ISO_MASK3(:)==rTemp(ii-18:ii+20))) then
        ghcnm%data_quality(iIsoMo(ii),iIsoYr(ii),iElem,iStn)="L"
        ghcnm%data_quality(iIsoMo(ii+1),iIsoYr(ii+1),iElem,iStn)="L"
        ghcnm%data_quality(iIsoMo(ii+2),iIsoYr(ii+2),iElem,iStn)="L"
        write (27,'(a1,1x,a11,1x,a4,1x,i4,1x,i2.2,f10.1)') "L",ghcnm%station_id(iStn),ghcnm_element(iElem),iIsoYr(ii+1), &
                                                           iIsoMo(ii+1), ghcnm%data(iIsoMo(ii),iIsoYr(ii),iElem,iStn)
        write (27,'(a1,1x,a11,1x,a4,1x,i4,1x,i2.2,f10.1)') "L",ghcnm%station_id(iStn),ghcnm_element(iElem),iIsoYr(ii+2), &
                                                           iIsoMo(ii+2), ghcnm%data(iISoMo(ii),iIsoYr(ii),iElem,iStn)
      endif
    enddo
  enddo
enddo

! Identify Climatological Outliers using z-scores

allocate(rMean(1:12,1:GHCNM_NUMBER_OF_ELEMENTS,1:ghcnm%number_of_stations))
allocate(rStDev(1:12,1:GHCNM_NUMBER_OF_ELEMENTS,1:ghcnm%number_of_stations))
allocate(zscore(1:12,BEGIN_YEAR:END_YEAR,1:GHCNM_NUMBER_OF_ELEMENTS,1:ghcnm%number_of_stations))
rMean(:,:,:)=GHCNM_MISSING
rStDev(:,:,:)=GHCNM_MISSING
zscore(:,:,:,:)=GHCNM_MISSING

do iStn=1,ghcnm%number_of_stations
  do iElem=1,GHCNM_NUMBER_OF_ELEMENTS
    if (iElem==GHCNM_PRCP) cycle
    do iMo=1,12
      iC=0
      do iYr=BEGIN_YEAR,END_YEAR
        if (ghcnm%data(iMo,iYr,iElem,iStn)/=GHCNM_MISSING.and.ghcnm%data_quality(iMo,iYr,iElem,iStn)==" ") then
          iC=iC+1
          rTemp(iC)=ghcnm%data(iMo,iYr,iElem,iStn)
        endif
      enddo
      if (iC>=MIN_STATS) then
        call mean(rTemp(1:iC),rMean(iMo,iElem,iStn),BIWEIGHT=BIWEIGHT_CENSOR_THRESHOLD)
        call standard_deviation(rTemp(1:iC),rStDev(iMo,iElem,iStn),BIWEIGHT=BIWEIGHT_CENSOR_THRESHOLD)
      endif
    enddo
    do iYr=BEGIN_YEAR,END_YEAR
      do iMo=1,12
        if (ghcnm%data(iMo,iYr,iElem,iStn)/=GHCNM_MISSING.and.ghcnm%data_quality(iMo,iYr,iElem,iStn)==" ") then
          if (rMean(iMo,iElem,iStn)/=GHCNM_MISSING.and.rStDev(iMo,iElem,iStn)/=GHCNM_MISSING) then
            if (ghcnm%data(iMo,iYr,iElem,iStn) < rMean(iMo,iElem,iStn)) then
              zscore(iMo,iYr,iElem,iStn)=((rMean(iMo,iElem,iStn)-ghcnm%data(iMo,iYr,iElem,iStn))/rStDev(iMo,iElem,iStn))*(-1.0)
            else
              zscore(iMo,iYr,iElem,iStn)=(ghcnm%data(iMo,iYr,iElem,iStn)-rMean(iMo,iElem,iStn))/rStDev(iMo,iElem,iStn)
            endif
            if (abs(zscore(iMo,iYr,iElem,iStn))>=ZSCORE_THRESHOLD) then
              ghcnm%data_quality(iMo,iYr,iElem,iStn)="O"
              write (27,'(a1,1x,a11,1x,a4,1x,i4,1x,i2.2,1x,4f12.3)') "O",ghcnm%station_id(iStn),ghcnm_element(iElem),iYr,iMo, &
                                                                     ghcnm%data(iMo,iYr,iElem,iStn),zscore(iMo,iYr,iElem,iStn), &
                                                                     rMean(iMo,iElem,iStn),rStDev(iMo,iElem,iStn)
            endif
          endif
        endif
      enddo
    enddo
  enddo
enddo

!*******************************************************************************
! GHCNM v2 Spatial Check I

! Find neighbors

  allocate(rNeighborDist(1:ghcnm%number_of_stations),STAT=iStat)
  allocate(iNeighbor(1:ghcnm%number_of_stations,1:ghcnm%number_of_stations),STAT=iStat)
  allocate(rNeighDistLog(1:ghcnm%number_of_stations,1:ghcnm%number_of_stations),STAT=iStat)
  allocate(iNumNeighbors(1:ghcnm%number_of_stations))
  iNumNeighbors(:)=0
  rNeighDistLog(:,:)=MAX_DISTANCE+1.0

  do iStn1=1,ghcnm%number_of_stations
    rNeighborDist(:)=MAX_DISTANCE+1.0   ! Initialize to larger than maximum distance to a neighbor
    do iStn2=1,ghcnm%number_of_stations
      if (iStn1==iStn2) cycle
      if (ghcnm%first_year(iStn1,GHCNM_TAVG)/=GHCNM_MISSING.and.ghcnm%first_year(iStn2,GHCNM_TAVG)/=GHCNM_MISSING) then
        call geographic_distance(ghcnm%longitude(iStn1),ghcnm%latitude(iStn1),ghcnm%longitude(iStn2),ghcnm%latitude(iStn2),rDist)
        rNeighborDist(iStn2)=rDist
        rNeighDistLog(iStn1,iStn2)=rDist
      else
        rNeighborDist(iStn2)=500000.0
        rNeighDistLog(iStn1,iStn2)=500000.0
      endif
    enddo
    call indices(rNeighborDist(:),iNeighbor(iStn1,:))
    iNumNeighbors(iStn1)=count(rNeighborDist(:)<=MAX_DISTANCE)
  enddo

! Spatial check

  allocate(rZScoreTemp(1:ghcnm%number_of_stations))

  do iStn=1,ghcnm%number_of_stations
    do iElem=1,GHCNM_NUMBER_OF_ELEMENTS
      if (iElem==GHCNM_PRCP) cycle
      do iYr=BEGIN_YEAR,END_YEAR
        do iMo=1,12
          if (zscore(iMo,iYr,iElem,iStn)>=ZSCORE_THRESHOLD) cycle 
          if (zscore(iMo,iYr,iElem,iStn)<=((-1.0)*ZSCORE_THRESHOLD)) cycle 
          if (zscore(iMo,iYr,iElem,iStn)>-2.5.and.zscore(iMo,iYr,iElem,iStn)<2.5) cycle
          if (iNumNeighbors(iStn)<1) cycle
          if (ghcnm%data_quality(iMo,iYr,iElem,iStn)/=" ") cycle
          iC=0
          do iStn2=1,iNumNeighbors(iStn)
            if (iC==MIN_NEIGHBORS) exit
            if (zscore(iMo,iYr,iElem,iNeighbor(iStn,iStn2))/=GHCNM_MISSING.and. &
                ghcnm%data_quality(iMo,iYr,iElem,iNeighbor(iStn,iStn2))==" ") then
              iC=iC+1
              rZScoreTemp(iC)=zscore(iMo,iYr,iElem,iNeighbor(iStn,iStn2))

              if (iElem==GHCNM_TAVG) then
                write (27,23) "S",iStn,ghcnm%station_id(iStn),iYr,iMo,ghcnm%longitude(iStn), &
                              ghcnm%latitude(iStn),ghcnm%data(iMo,iYr,iElem,iStn),zscore(iMo,iYr,iElem,iStn), &
                              iNumNeighbors(iStn),iStn2,ghcnm%station_id(iNeighbor(iStn,iStn2)), &
                              ghcnm%longitude(iNeighbor(iStn,iStn2)),ghcnm%latitude(iNeighbor(iStn,iStn2)), &
                              rNeighDistLog(iStn,iNeighbor(iStn,iStn2)),ghcnm%data(iMo,iYr,iElem,iNeighbor(iStn,iStn2)), &
                              zscore(iMo,iYr,iElem,iNeighbor(iStn,iStn2))
              endif
23 format(a1,1x,i5.5,1x,a11,1x,i4,1x,i2.2,1x,f10.4,1x,f10.4,1x,f12.4,1x,f12.4,1x,i5.5,1x,i5.5,1x,a11,1x,f10.4,1x,f10.4,1x,f12.2, &
          1x,f12.4,1x,f12.4)



            endif
          enddo
          if (iC>0) then
            iF=0
            do ii=1,iC
              Z=zscore(iMo,iYr,iElem,iStn) 
              if ((Z>=4.0.and.Z<5.0).and.(any(rZScoreTemp(1:iC)>=1.9))) iF=1
              if ((Z>=3.0.and.Z<4.0).and.(any(rZScoreTemp(1:iC)>=1.8))) iF=1
              if ((Z>=2.75.and.Z<3.0).and.(any(rZScoreTemp(1:iC)>=1.7))) iF=1
              if ((Z>=2.5.and.Z<2.75).and.(any(rZScoreTemp(1:iC)>=1.6))) iF=1
              if ((Z<=-4.0.and.Z>-5.0).and.(any(rZScoreTemp(1:iC)<=-1.9))) iF=1
              if ((Z<=-3.0.and.Z>-4.0).and.(any(rZScoreTemp(1:iC)<=-1.8))) iF=1
              if ((Z<=-2.75.and.Z>-3.0).and.(any(rZScoreTemp(1:iC)<=-1.7))) iF=1
              if ((Z<=-2.5.and.Z>-2.75).and.(any(rZScoreTemp(1:iC)<=-1.6))) iF=1
            enddo 
            if (iF==0) then
              ghcnm%data_quality(iMo,iYr,iElem,iStn)="S"
            endif
          endif
        enddo
      enddo
    enddo
  enddo

!*******************************************************************************
! Spatial Z-Score Check

! Note: this code subsection makes use of the 'Find Neighbors' subsection of code
! found above in the GHCNM V2 Spatial Check I

do iYr=BEGIN_YEAR,END_YEAR
  do iMo=1,12
    do iStn1=1,ghcnm%number_of_stations
      if (ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn1)/=GHCNM_MISSING.and.ghcnm%data_quality(iMo,iYr,GHCNM_TAVG,iStn1)==" ") then
        if (zscore(iMo,iYr,GHCNM_TAVG,iStn1)/=GHCNM_MISSING) then
          iC=0
          do iStn2=1,iNumNeighbors(iStn1)
            if (zscore(iMo,iYr,GHCNM_TAVG,iNeighbor(iStn1,iStn2))/=GHCNM_MISSING.and. &
                ghcnm%data_quality(iMo,iYr,GHCNM_TAVG,iNeighbor(iStn1,iStn2))==" ") then
              iC=iC+1
              rTemp(iC)=zscore(iMo,iYr,GHCNM_TAVG,iNeighbor(iStn1,iStn2))
              if (rNeighDistLog(iStn1,iNeighbor(iStn1,iStn2))==0.0) then
                rTempDist(iC)=0.0001  ! set to near 0.0, since 0.0 would cause divide by 0 error in inverse distance weighting
              else
                rTempDist(iC)=rNeighDistLog(iStn1,iNeighbor(iStn1,iStn2))
              endif
            endif
          enddo
          if (iC>1) then   ! must be at least two or more neighbors
            call inverse_distance_weighted_mean(rTemp(1:iC),rTempDist(1:iC),idwm)
            if (abs((zscore(iMo,iYr,GHCNM_TAVG,iStn1)-idwm))>=SPATIAL_ZSCORE_THRESHOLD) then
              ghcnm%data_quality(iMo,iYr,GHCNM_TAVG,iStn1)="T"
              do iStn2=1,iNumNeighbors(iStn1)
                if (zscore(iMo,iYr,GHCNM_TAVG,iNeighbor(iStn1,iStn2))/=GHCNM_MISSING.and. &
                    ghcnm%data_quality(iMo,iYr,GHCNM_TAVG,iNeighbor(iStn1,iStn2))==" ") then
                  write (27,23) "T",iStn1,ghcnm%station_id(iStn1),iYr,iMo,ghcnm%longitude(iStn1), &
                                ghcnm%latitude(iStn1),ghcnm%data(iMo,iYr,GHCNM_TAVG,iStn1),zscore(iMo,iYr,GHCNM_TAVG,iStn1), &
                                iNumNeighbors(iStn1),iStn2,ghcnm%station_id(iNeighbor(iStn1,iStn2)), &
                                ghcnm%longitude(iNeighbor(iStn1,iStn2)),ghcnm%latitude(iNeighbor(iStn1,iStn2)), &
                                rNeighDistLog(iStn1,iNeighbor(iStn1,iStn2)),ghcnm%data(iMo,iYr,GHCNM_TAVG,iNeighbor(iStn1,iStn2)), &
                                zscore(iMo,iYr,GHCNM_TAVG,iNeighbor(iStn1,iStn2))
                endif
              enddo
            endif
          endif
        endif
      endif
    enddo
  enddo
enddo

close(27)

!*******************************************************************************
!  Write out final data file with QC flags

! Convert back integer values express as hundredths deg C and tenths of a mm

where(ghcnm%data(:,:,GHCNM_TAVG,:)/=GHCNM_MISSING) ghcnm%data(:,:,GHCNM_TAVG,:)=ghcnm%data(:,:,GHCNM_TAVG,:)*100.0
where(ghcnm%data(:,:,GHCNM_TMAX,:)/=GHCNM_MISSING) ghcnm%data(:,:,GHCNM_TMAX,:)=ghcnm%data(:,:,GHCNM_TMAX,:)*100.0
where(ghcnm%data(:,:,GHCNM_TMIN,:)/=GHCNM_MISSING) ghcnm%data(:,:,GHCNM_TMIN,:)=ghcnm%data(:,:,GHCNM_TMIN,:)*100.0
where(ghcnm%data(:,:,GHCNM_PRCP,:)/=GHCNM_MISSING) ghcnm%data(:,:,GHCNM_PRCP,:)=ghcnm%data(:,:,GHCNM_PRCP,:)*10.0

! Convert real missing to integer missing (e.g. -9999.9 to -9999)

where(ghcnm%data(:,:,GHCNM_TAVG,:)==GHCNM_MISSING) ghcnm%data(:,:,GHCNM_TAVG,:)=real(GHCNM_MISSING_INTEGER)
where(ghcnm%data(:,:,GHCNM_TMAX,:)==GHCNM_MISSING) ghcnm%data(:,:,GHCNM_TMAX,:)=real(GHCNM_MISSING_INTEGER)
where(ghcnm%data(:,:,GHCNM_TMIN,:)==GHCNM_MISSING) ghcnm%data(:,:,GHCNM_TMIN,:)=real(GHCNM_MISSING_INTEGER)
where(ghcnm%data(:,:,GHCNM_PRCP,:)==GHCNM_MISSING) ghcnm%data(:,:,GHCNM_PRCP,:)=real(GHCNM_MISSING_INTEGER)

open(unit=13,file=cOutFile,status='replace')
do iStn=1,ghcnm%number_of_stations
  do iElem=1,GHCNM_NUMBER_OF_ELEMENTS
    do iYr=BEGIN_YEAR,END_YEAR
      if (count(ghcnm%data(:,iYr,iElem,iStn)/=real(GHCNM_MISSING_INTEGER))>0) then
        write (13,13) ghcnm%station_id(iStn),iYr,ghcnm_element(iElem),(nint(ghcnm%data(iMo,iYr,iElem,iStn)), &
                      ghcnm%data_measurement(iMo,iYr,iElem,iStn),ghcnm%data_quality(iMo,iYr,iElem,iStn), &
                      ghcnm%data_source(iMo,iYr,iElem,iStn),iMo=1,12)
      endif
    enddo
  enddo
enddo
close(13) 
13 format(a11,i4,a4,12(i5,3a1))

end program ghcnm_qc
