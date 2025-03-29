program stage3_to_ghcnm
use merge_module

 !******************************************************************************
 ! stage3_to_ghcnm.f95
 ! 
 ! **DESCRIPTION**
 ! This program, written in FORTRAN, Uses the latest recommended merge, and 
 ! converts to a format similar to GHCN-Monthly
 ! 
 ! This program has a module associated with it (merge_module.f95)
 !   Using g95, the code can be compiled using the following command:
 !     g95 merge_module.f95 stage3_to_ghcnm.f95
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
 ! 10/1/2015: First revision ready for beta release
 ! 09/29/2016: Updated for GHCN-M v4.0.0 release
 ! 10/31/2019: Updated for GHCN-M v4.0.1 processing
 !*******************************************************************************

integer, parameter :: BYR=1690,EYR=2025, NUM_ELEMENTS=3

integer, dimension(1:12) :: iVal
real, allocatable, dimension(:,:,:,:) :: rData
real, parameter :: rMiss=-99.9
character(len=01), allocatable, dimension(:,:,:,:) :: cDM_FLAG,cQC_FLAG,cDS_FLAG
character(len=01), dimension(1:12) :: cDM,cQC,cDS
character(len=04) :: cElement
character(len=04), dimension(1:NUM_ELEMENTS) :: cElem=(/"TMAX","TMIN","TAVG"/)
character(len=11) :: cStation,cStation2, cPrevStn
character(len=11), allocatable, dimension(:) :: cStn
real, allocatable, dimension(:) :: rLon,rLat,rElev

!******************************************************************************
!variables added / changed to accomidate databank

character(len=1024) dataline,metaline
integer :: iTmax,iTmin,iTavg
real, parameter :: MISS_LON=-999, MISS_LAT=-99
integer, parameter :: MISS=-9999
integer :: num_qc_D,num_qc_R,num_qc_I,num_qc_L,num_qc_O,num_qc_S
character(len=1024) :: cInMetaFile,cInDataDir,cOutMetaFile,cOutDataFile,cArg
character(len=1024) :: cName,infile,version
character(len=3) :: variant
character(len=8), dimension(1:NUM_ELEMENTS) :: source_info
integer :: NUM_STNS,NUM_LINES,number_of_lines, iYr, iMo, dataset_counter,flag_counter

!******************************************************************************
!variables added / changed to accomidate v4
character(len=1024) :: list_of_datasets, main_directory, ghcnd_directory,  ghcnd_infile, ghcndline
integer :: number_of_datasets
character(len=1024), allocatable, dimension(:) :: source_number, source_name, source_flag 
character(len=01), allocatable, dimension(:,:,:,:) :: ghcnd_flag

!******************************************************************************

call getarg(1,cArg)
main_directory=cArg

call getarg(2,cArg)
cInMetaFile=cArg

call getarg(3,cArg)
cInDataDir=cArg

call getarg(4,cArg)
cOutMetaFile=cArg

call getarg(5,cArg)
cOutDataFile=cArg

call getarg(6,cArg)
variant=cArg

call getarg(7,cArg)
version=cArg

if (cInMetaFile=="".or.cInDataDir=="".or.cOutMetaFile=="".or.cOutDatafile=="") then
  write (*,*) ""
  write (*,*) "USAGE: <MAIN_DIRECTORY> <IN_METADATAFILE> <IN_DATADIR> <OUT_METADATAFILE> <OUT_DATAFILE> <3-DIGIT-VARIANT> <VERSION>"
  write (*,*) ""
  stop
endif

!*******************************************************************************
! Read in Dataset File
write(*,*) "READING IN DATASET INFORMATION"

list_of_datasets = trim(main_directory)//'/update/resources/databank_sources-v1.1.1.txt'
inquire(file=list_of_datasets,exist=file_exists)
if (file_exists .eqv. .false.) stop "ERROR: cannot find dataset File"

call file_recs(FILE_NAME=trim(list_of_datasets),NUM_UNIT=20,NUM_LINES=number_of_datasets)

allocate(source_number(1:number_of_datasets))
allocate(source_name(1:number_of_datasets))
allocate(source_flag(1:number_of_datasets))

counter=0
open(unit=1,file=list_of_datasets,status='old')
do
  read(1,'(a1024)',IOSTAT=ioerror) readline
  if (ioerror==-1) exit

  counter=counter+1

  source_number(counter) = readline(1:2)
  source_name(counter)=readline(4:32)
  source_flag(counter)=readline(75:75)

enddo
close(1)

!*******************************************************************************
! Read in Metadata/Data

call system ('date')
write(*,*) "READING IN METADATA/DATA"

call file_recs(FILE_NAME=cInMetaFile,NUM_UNIT=12,NUM_LINES=NUM_STNS)

write(*,'(a,i5)') "    NUM_STNS: ",NUM_STNS

allocate(cStn(1:NUM_STNS)); allocate(rLon(1:NUM_STNS))
allocate(rLat(1:NUM_STNS)); allocate(rElev(1:NUM_STNS))
allocate(rData(1:NUM_STNS,1:NUM_ELEMENTS,BYR:EYR,1:12))
allocate(cQC_FLAG(1:NUM_STNS,1:NUM_ELEMENTS,BYR:EYR,1:12))
allocate(cDM_FLAG(1:NUM_STNS,1:NUM_ELEMENTS,BYR:EYR,1:12))
allocate(cDS_FLAG(1:NUM_STNS,1:NUM_ELEMENTS,BYR:EYR,1:12))
rData(:,:,:,:)=rMiss
cQC_FLAG(:,:,:,:)=" "
cDM_FLAG(:,:,:,:)=" "
cDS_FLAG(:,:,:,:)=" "
iCurrStn=1
allocate(ghcnd_flag(1:2,1:NUM_ELEMENTS,BYR:EYR,1:12))

open(unit=15,file=cOutMetaFile,status='replace')
open(unit=12,file=cInMetaFile,status='old')
do
  read(12,'(a)',IOSTAT=iStat) metaline
  if (iStat==-1) exit

  ghcnd_flag(:,:,:,:)=" "

  cStation=metaline(2:12)
  cStation2=metaline(130:140)
  infile=trim(cInDataDir)//trim(version)//'_'//trim(adjustl(cStation))//'_stage3'
  call file_recs(FILE_NAME=trim(infile),NUM_UNIT=17,NUM_LINES=number_of_lines)

  if(number_of_lines > 0 )then
    ! Data exists, GRAB METADATA 
    cName=metaline(14:43)
    read (metaline(66:75),'(f10.4)') rLatitude
    read (metaline(77:86),'(f10.4)') rLongitude
    read (metaline(89:95),'(f8.2)') rElevation

    if(metaline(127:128) .eq. "01") then
      ! Station originated from GHCN-D. Get Source Flags From Two Versions
      !   1= Version Used For v1.1.1
      !   2= Latest Version Used for NRT
      do flag_counter=1,2

        if (flag_counter == 1) then
          ghcnd_directory = trim(main_directory)//'/input_v1.1.1/ghcnd-dev'
          ghcnd_infile = trim(ghcnd_directory)//'/ghcnd-dev_'//trim(adjustl(cStation))//'_monthly_stage2'
        endif

        if (flag_counter == 2) then
          ghcnd_directory = trim(main_directory)//'/update/tmp/ghcnd'
          ghcnd_infile = trim(ghcnd_directory)//'/ghcnd_'//trim(adjustl(cStation))//'_monthly_stage2'
        endif

        !write(*,*) trim(ghcnd_infile)

        inquire(file=trim(ghcnd_infile),exist=file_exists)
        if (file_exists) then
          open(unit=25,file=trim(ghcnd_infile),status='old')
            do
              read(25,'(a)',IOSTAT=iStat) ghcndline
              if (iStat==-1) exit
              read (ghcndline(63:66),'(i4)') iYr
              read (ghcndline(67:68),'(i2)') iMo
              ghcnd_flag(flag_counter,tmax,iYr,iMo)=ghcndline(134:134)
              ghcnd_flag(flag_counter,tmin,iYr,iMo)=ghcndline(135:135)
              ghcnd_flag(flag_counter,tavg,iYr,iMo)=ghcndline(136:136)

             !write(*,'(i4,1x,i2,1x,3(a1,1x))') iYr,iMo,ghcnd_flag(flag_counter,tmax,iYr,iMo),ghcnd_flag(flag_counter,tmin,iYr,iMo),ghcnd_flag(flag_counter,tavg,iYr,iMo)
            enddo
          close(25)
        endif

      enddo
    endif

    if(rLatitude == 999.9 .or. rLatitude == -999.9 .or. rLatitude == -9999.0000) then  
      rLatitude = -99
    endif
    if(rLongitude == 999.9 .or. rLongitude == -999.9 .or. rLongitude == -9999.0000) then  
      rLongitude = -999
    endif

    write (15,15) cStation,rLatitude,rLongitude,rElevation,cName
    cStn(iCurrStn) = cStation

    !write(*,*) "NEW DATA"

    ! GRAB DATA
    open(unit=17,file=infile,status='old')
      do
        read(17,'(a)',IOSTAT=iStat) dataline
        if (iStat==-1) exit

        read (dataline(63:66),'(i4)') iYr
        read (dataline(67:68),'(i2)') iMo
        read (dataline(72:76),'(i5)') iTmax
        read (dataline(78:82),'(i5)') iTmin
        read (dataline(84:88),'(i5)') iTavg
        source_info(tmax)=dataline(134:141)
        source_info(tmin)=dataline(143:150)
        source_info(tavg)=dataline(152:159)

        if (iYr<BYR.or.iYr>EYR) cycle

        !tmax
        iElem=tmax
        if (iTmax/=MISS) then
          rData(iCurrStn,iElem,iYr,iMo)=real(iTmax)/100.0
        endif

        !tmin
        iElem=tmin
        if (iTmin/=MISS) then
          rData(iCurrStn,iElem,iYr,iMo)=real(iTmin)/100.0
        endif

        !tavg
        iElem=tavg
        if (iTavg/=MISS) then
          rData(iCurrStn,iElem,iYr,iMo)=real(iTavg)/100.0
        endif

        !source flag
        do iElem=1,NUM_ELEMENTS
          if(rData(iCurrStn,iElem,iYr,iMo)/=rMiss) then
            if(source_info(iElem)(1:2) .eq. "01") then

              if(source_info(iElem)(3:8) .eq. "UPDATE") then
                flag_counter=2
              else
                flag_counter=1
              endif

              ! If flag is average, and there was max/min data, use max/min flag
              if(iElem == tavg .and. iTmax /=MISS .and. iTmin /=MISS)then
                cDS_FLAG(iCurrStn,iElem,iYr,iMo)=ghcnd_flag(flag_counter,tmax,iYr,iMo)
              else
                ! Get srcflg from GHCN-D
                cDS_FLAG(iCurrStn,iElem,iYr,iMo)=ghcnd_flag(flag_counter,iElem,iYr,iMo)
              endif

              !write(*,'(i4,1x,i2,1x,a2,1x,a6,1x,i1,1x,i1,1x,2(a1,1x),2(i5,1x))') iYr,iMo,source_info(iElem)(1:2),source_info(iElem)(3:8),flag_counter,iElem,&
              !       cDS_FLAG(iCurrStn,iElem,iYr,iMo),ghcnd_flag(flag_counter,iElem,iYr,iMo),iTmax,iTmin

            else
              ! Search through ISTI sourceflags
              do dataset_counter=1,number_of_datasets
                if(source_info(iElem)(1:2) .eq. source_number(dataset_counter)) then 
                  cDS_FLAG(iCurrStn,iElem,iYr,iMo)=source_flag(dataset_counter)
                  exit
                endif
              enddo
            endif

            if(iElem == tavg .and. cDS_FLAG(iCurrStn,tavg,iYr,iMo) .eq. " ")then
              if(cDS_FLAG(iCurrStn,tmax,iYr,iMo) .ne. " ") then
                cDS_FLAG(iCurrStn,tavg,iYr,iMo) = cDS_FLAG(iCurrStn,tmax,iYr,iMo)
              elseif(cDS_FLAG(iCurrStn,tmin,iYr,iMo) .ne. " ") then
                cDS_FLAG(iCurrStn,tavg,iYr,iMo) = cDS_FLAG(iCurrStn,tmin,iYr,iMo)
              else
                cDS_FLAG(iCurrStn,tavg,iYr,iMo) = " "
              endif
            endif

          endif
        enddo

      enddo
    close(17)
    iCurrStn=iCurrStn+1
  endif
enddo
close(12)
close(15)
15 format(a11,1x,f8.4,1x,f9.4,1x,f6.1,1x,a30)

!*******************************************************************************
! Write out final data file without QC flags

call system ('date')
write(*,*) "OUTPUTTING DATA FILE"

open(unit=13,file=cOutDataFile,status='replace')
do iStn=1,NUM_STNS
  cStn(iStn)=trim(cStn(iStn))
  do iElem=1,NUM_ELEMENTS
    do iYr=BYR,EYR
      if (count(rData(iStn,iElem,iYr,:)/=rMiss)>0) then
        iVal(:)=-9999
        if (iElem==tmax) rMult=100.0
        if (iElem==tavg) rMult=100.0
        if (iElem==tmin) rMult=100.0
        do iMo=1,12
          if (rData(iStn,iElem,iYr,iMo)/=rMiss) iVal(iMo)=nint(rData(iStn,iElem,iYr,iMo)*rMult)
        enddo
        write (13,13) cStn(iStn),iYr,cElem(iElem),(iVal(iMo),cDM_FLAG(iStn,iElem,iYr,iMo), &
                      cQC_FLAG(iStn,iElem,iYr,iMo),cDS_FLAG(iStn,iElem,iYr,iMo),iMo=1,12)
      endif
    enddo
  enddo
enddo
close(13) 
13 format(a11,i4,a4,12(i5,3a1))
end program stage3_to_ghcnm
