 program genStnUncer

! use cirrus

 implicit none

 integer,parameter :: iMetaList=20, iUInMv2=21, iUOutMax=23, iUOutMin=26, iUMsg=27, iUMean=28
 integer,parameter :: iMinYr=1701, iMaxYr=2007
 integer,parameter :: iMiss=-9999
 integer,parameter :: TMAX=1,TMIN=2,TAVG=3,PRCP=4,iNumElem=4
 integer,parameter :: RAW=1, ADJ=2
 integer,parameter :: nStns = 8000

 integer, dimension(1:iNumElem) :: iUInFtp = (/29, 30, 31, 32/)
 integer,dimension(1:12) :: iTmpVals, iStDevs
 integer,dimension(iMinYr:iMaxYr,1:12) :: iDataD2M, iDiffs
 integer,dimension(1:nStns, 1:2, TMAX:PRCP, iMinYr:iMaxYr, 1:12) :: iDataMv2
 integer,dimension(1:2) :: iUXMG = (/24,25/)
 integer,allocatable,dimension(:) :: iMonVals
 integer, dimension(1:nStns, 1:iNumElem) :: ihaveMv2, ihaveFtp

 integer::iStn,iMon,iYear, iNumStns, iStat, iYr, iMo, iElem, iRun, iFirstYr, &
   iCnt, iFirstTime, iDiff, nArg
 integer :: ifound, iUMv2Bad

 real ::  rRawAvg, rTobAvg, rAdjAvg, rStdAvg, rUAdjAvg, rLAdjAvg
 real :: rLatitude, rLongitude
 real, dimension(1:nStns) :: rStnLats, rStnLons
 real, dimension(1:2,TMAX:TMIN,1:12) :: rMeans

 character(len=01),dimension(1:nStns,1:2,TMAX:PRCP,iMinYr:iMaxYr,1:12) :: cFlagsMv2
 character(len=01),dimension(1:12) :: cTmpFlags
 character(len=06),dimension(nStns) :: cIdD2Ms, cIdMv2
 character(len=06) :: cPrefix
 character(len=03),dimension(1:4) :: cElems = (/'max','min','avg','pcp'/)
 character(len=11) :: cIdD2M, cdTag, cRunid
 character(len=132) :: cPathList='/home/cwilliam/USHCN/v2/ghcnm/'
 character(len=132) :: cTestDir, cRawDataFile, cFtpDataFile, cPathRawData
 character(len=132) :: cRunString
 character(len=11) :: cdTagRegMv2, cdTagRegD2M, cElem, cProc
 character(len=132) :: cfile
 character(len=11) :: cStation
 character(len=06) :: cStnId
 character(len=06), dimension(nStns) :: cStnIds
 character(len=06), dimension(nStns) :: cIdsMv2  ! Assuming same size as cIdMv2

 logical :: lExist, lMax, lMin, lAvg, lPcp

 ihaveMv2 = 0

 ! get date tag for Mv2
 call getarg(1, cdTagRegMv2)
 print *,' Mv2 Data Date Tag/Region: ', cdTagRegMv2
 ! get date tag for D2M
 call getarg(1, cdTagRegD2M)
 print *,' D2M Data Date Tag/Region: ', cdTagRegD2M
 ! get runid
 call getarg(2, cRunid)
 print *,' Run ID:', cRunid
 ! get Element
 call getarg(3, cElem)
 ifound = 0
 do iElem = 1, iNumElem
   if (cElem == cElems(iElem)) then
     print *,' Element:', cElem
     ifound = 1
     exit
   end if
 end do
 if (ifound == 0) then
   print *,'Undefined Element:', cElem
   stop
 end if

 ! set run-string that distinguishes this run from others
 cRunString = trim(cElem) // '.' // trim(cdTagRegMv2) // '.' // trim(cdTagRegD2M) // &
   '.' // trim(cRunid)

 ! ----- setup log and output files
 cfile =  trim(cPathList) // 'diagnostics/' // trim(cRunString) // '.log'
 open(iUMsg,file=cfile, status='replace',iostat=iStat)
 if (iStat /= 0) then
   call perror('CANNOT open Diagnostic log file:'//trim(cfile))
   stop
 end if

 cfile =  trim(cPathList) // 'diagnostics/' // trim(cRunString) // 'Mv2.notprocessed'
 open(iUMv2Bad,file=cfile, status='unknown',iostat=iStat)
 if (iStat /= 0) then
   call perror('CANNOT open Mv2 Not Processed meta file:'//trim(cfile))
   stop
 end if

 ! ------ open and read the Mv2 meta data file (stnID, lat, lon) -------------------

 cfile = trim(cPathList) // 'meta/ghcnmv2.temp.meta'
 open(iMetaList, file=trim(cfile), status='old')
 if (iStat /= 0) then
   call perror('CANNOT open Mv2 metafile:' // trim(cfile))
   stop
 end if
 iStn=0
 do
   read(iMetaList,'(a11,f9.4,f10.4)',IOSTAT=iStat) cStation,rLatitude,rLongitude
   if (iStat == -1) exit
   iStn = iStn+1
   cStnIds(iStn) = cStation
   rStnLats(iStn) = rLatitude
   rStnLons(iStn) = rLongitude
 end do
 iNumStns = iStn

 ! ------ open, read, store, and close all of the Mv2 obs data files --------------
 cPathRawData = trim(cPathList) // 'monthv2/' // trim(cdTagRegMv2) // '/'
 do iStn = 1,iNumStns

   write(*,*) cIdsMv2(iStn)

   iDataMv2 = iMiss
   cFlagsMv2 = ' '

   iFirstYr = -1*iMiss

   ! Read in the Raw Mv2 data to be tested against
   cfile = trim(cPathRawData) // trim(cdTagRegMv2) // '/monthly_ushcn/raw/' //  &
     cStnIds(iStn) // '_' // cElems(iElem) // '.raw'

   inquire(file=trim(cfile),EXIST=lExist)
   if (lExist) then
     open(iUInMv2, file = trim(cfile), status='old')
     do
       read(iUInMv2,'(7x,i4,12(i6,a1))',IOSTAT=iStat) iYear,(iTmpVals(iMon), &
         cTmpFlags(iMon), iMon=1,12)
       if (iYear<iMinYr .or. iYear>iMaxYr) cycle
       if (iYear < iFirstYr) iFirstYr = iYear
       iDataMv2(iStn, RAW, iElem, iYear,:) = iTmpVals
       cFlagsMv2(iStn, RAW, iElem, iYear,:) = cTmpFlags
       ihaveMv2(iStn, iElem) = ihaveMv2(iStn, iElem) + 1
     end do
     close(iUInMv2)
   else
     write(iUMv2Bad,'("Mv2 data file does not exist: ",a)') trim(cfile)
   end if

   ! Test the input test data for inconsistencies -- min greater than max;
   iFirstTime = 0
   do iYr = iMinYr, iMaxYr
     do iMo = 1, 12
       if (iDataMv2(iStn,RAW,TMAX,iYr,iMo) /= iMiss .and. iDataMv2(iStn,RAW,TMIN,iYr,iMo) /= iMiss .and. &
         iDataMv2(iStn,RAW,TMAX, iYr, iMo) < iDataMv2(iStn,RAW,TMIN, iYr, iMo)) then
         if (iFirstTime == 0) then
           write(iUMsg,*) 'Station ' // cStnIds(iStn) // '  internal inconsistencies in raw data'
           write(iUMsg,*) 'iYr, iMo, Max-Min, Flags'
           iFirstTime = 1
         end if
         iDiff = iDataMv2(iStn,RAW,TMAX, iYr, iMo) - iDataMv2(Istn,RAW,TMIN, iYr, iMo)
         write(iUMsg,'(i4,1x,i2.2,i6,1x,a1,1x,a1)') iYr,iMo, iDiff, cFlagsMv2(iStn,RAW,TMAX,iYr,iMo), &
           cFlagsMv2(iStn,RAW,TMIN,iYr,iMo)
       end if
     end do
   end do

   ! Read in the FTP file and test
   do iElem = 1, iNumElem
     if (ihaveMv2(iStn, iElem) == 0 .or. (cProc == 'tob' .and. cElems(iElem) == 'pcp')) cycle

     do
       read(iUInFtp(iElem),'(a6,1x,i4,12(i6,a1))',IOSTAT=iStat) cStnId, iYear,(iTmpVals(iMon), &
         cTmpFlags(iMon), iMon=1,12)
!       print *,iElem, ' ', cStnId, ' ', iYear
       if (iStat == -1) exit
       if (iYear<iMinYr .or. iYear>iMaxYr) cycle
       if (cStnId == cStnIds(iStn)) then
         iDataMv2(iStn,ADJ,iElem,iYear,:) = iTmpVals
         cFlagsMv2(iStn,ADJ,iElem,iYear,:) = cTmpFlags
         iHaveFtp(iStn, iElem) = iHaveFtp(iStn, iElem) + 1
       else if (cStnId > cStnIds(iStn)) then
         exit
       end if
     end do
     if (iStat /= -1) then
       backspace(iUInFtp(iElem))
!       print *,' BackSpace ', iElem
     else
       call perror('I/O Error test FTP file Elem: ' // cElems(iElem))
     stop

     end if

     if (iHaveFtp(iStn, iElem) == 0) then
       write(iUMsg,*) cStnIds(iStn) // ' is not in the ',cElems(iElem),' ftp file'
       cycle
     end if
     if (count(cFlagsMv2(iStn,ADJ,iElem,:,:) /= ' ' .and. cFlagsMv2(iStn,ADJ,iElem,:,:) /= 'E' .and. &
       cFlagsMv2(iStn,ADJ,iElem,:,:) /= 'I' .and. cFlagsMv2(iStn,ADJ,iElem,:,:) /= 'Q' .and. &
       cFlagsMv2(iStn,ADJ,iElem,:,:) /= 'X') > 0) then
       write(iUMsg,*) 'There are invalid flags for station '//cStnIds(iStn)//' and element '// &
         cElems(iElem)
     end if

     if (cProc == 'raw' .or. cProc == 'tob') then
       iCnt = count(cFlagsMv2(iStn,ADJ,iElem,:,:) /= ' ' .and. cFlagsMv2(iStn,ADJ,iElem,:,:) /= 'Q' .and. &
         iDataMv2(iStn,ADJ,iElem,:,:) == iMiss)
     else
       iCnt = count(cFlagsMv2(iStn,ADJ,iElem,:,:) /= ' ' .and. iDataMv2(iStn,ADJ,iElem,:,:) == iMiss)
     end if
     if (iCnt > 0) then
       write(iUMsg,*) 'There are non blank flags for missing data at station '//&
       cStnIds(iStn)//' and element '//cElems(iElem)
       if (cProc == 'raw' .or. cProc == 'tob') then
         do iYr = iMinYr, iMaxYr
           do iMo = 1,12
             if (cFlagsMv2(iStn,ADJ,iElem,iYr,iMo) /= ' ' .and. cFlagsMv2(iStn,ADJ,iElem,iYr,iMo) /= 'Q' .and. &
               iDataMv2(iStn,ADJ,iElem,iYr,iMo) == iMiss) then
               write(iUMsg,*) cStnIds(iStn),' ', cElems(iElem), iYr, iMo,iDataMv2(iStn,ADJ,iElem,iYr,iMo), &
                 ':',cFlagsMv2(iStn,ADJ,iElem,iYr,iMo),':'
             end if
           end do
         end do
       else
         do iYr = iMinYr, iMaxYr
           do iMo = 1,12
             if (cFlagsMv2(iStn,ADJ,iElem,iYr,iMo) /= ' ' .and. iDataMv2(iStn,ADJ,iElem,iYr,iMo) == iMiss) then
               write(iUMsg,*) cStnIds(iStn),' ', cElems(iElem), iYr, iMo,iDataMv2(iStn,ADJ,iElem,iYr,iMo), &
                 ':',cFlagsMv2(iStn,ADJ,iElem,iYr,iMo),':'
             end if
           end do
         end do
       end if
     end if

     do iMo = 1,12
       do iRun = RAW,ADJ
          iCnt = count(iDataMv2(iStn,iRun,iElem,:,iMo) /= iMiss)
          allocate(iMonVals(1:iCnt))
          iMonVals(1:iCnt) = pack ( (iDataMv2(iStn,iRun,iElem,:,iMo)), (iDataMv2(iStn,iRun,iElem,:,iMo) /= iMiss) )
          rMeans(iRun,iElem,iMo) = real(sum(iMonVals))/iCnt
          deallocate(iMonVals)
       end do
     end do
   end do

   write(iUMean,'(f9.4,f10.4,1x," adj-raw",1x,a3,1x,12f6.1)') rStnLats(iStn),rStnLons(iStn),cElems(TMAX),&
        (rMeans(ADJ,TMAX,:) - rMeans(RAW,TMAX,:))/10.0
   write(iUMean,'(f9.4,f10.4,1x," adj-raw",1x,a3,1x,12f6.1)') rStnLats(iStn),rStnLons(iStn),cElems(TMIN),&
        (rMeans(ADJ,TMIN,:) - rMeans(RAW,TMIN,:))/10.0

   ! check that missing raw values have an 'E' flag

   do iElem = TMAX,PRCP
     if (iElem == TAVG .or. (cProc == 'tob' .and. cElems(iElem) == 'pcp')) cycle
      iCnt = count(iDataMv2(iStn,RAW,iElem,:,:) == iMiss .and. cFlagsMv2(iStn,RAW,iElem,:,:) /= 'f' .and. &
        iDataMv2(iStn,ADJ,iElem,:,:) /= iMiss .and. cFlagsMv2(iStn,ADJ,iElem,:,:) /= 'E')

      if (iCnt > 0) then
        write(iUMsg,*) 'Station '//cStnIds(iStn)//' has ',iCnt,' unidentified estimated values for ',cElems(iElem)//&
          ' in the following months'
        do iYr = iMinYr, iMaxYr
           do iMo = 1,12
             if (iDataMv2(iStn,RAW,iElem,iYr,iMo) == iMiss  .and. cFlagsMv2(iStn,RAW,iElem,iYr,iMo) /= 'f' .and. &
               iDataMv2(iStn,ADJ,iElem,iYr,iMo) /= iMiss .and. cFlagsMv2(iStn,ADJ,iElem,iYr,iMo) /= 'E') then
               write(iUMsg,*) cStnIds(iStn),' ', cElems(iElem), iYr, iMo,cFlagsMv2(iStn,ADJ,iElem,iYr,iMo)
            end if
         end do
        end do
      end if

   end do

   !Check for min > max inconsistencies
   iCnt = count(iDataMv2(iStn,ADJ,TMAX,:,:) /= iMiss .and. iDataMv2(iStn,ADJ,TMIN,:,:) /= iMiss .and. &
         iDataMv2(iStn,ADJ,TMAX,:,:) < iDataMv2(iStn,ADJ,TMIN,:,:))

   if (iCnt > 0) then
      write(iUMsg,*) 'Station '//cStnIds(iStn)//' has ',iCnt,' max/min internal inconsistencies in its adjusted data', &
        '(in the following year months)'
      do iYr = iMinYr,iMaxYr
        do iMo = 1,12
          if (iDataMv2(iStn,ADJ,TMAX,iYr,iMo) < iDataMv2(iStn,ADJ,TMIN,iYr,iMo) .and. iDataMv2(iStn,ADJ,TMAX,iYr,iMo) /= iMiss) then
            iDiff = iDataMv2(iStn,ADJ,TMAX,iYr,iMo) - iDataMv2(iStn,ADJ,TMIN,iYr,iMo)
            write(iUMsg,'(i4,1x,i2.2,i6,1x,a1,1x,a1)') iYr,iMo, iDiff, &
              cFlagsMv2(iStn,ADJ,TMAX,iYr,iMo), cFlagsMv2(iStn,ADJ,TMIN,iYr,iMo)
          end if
        end do
      end do
   end if

  !Check for failed qc values and flag codes of 'Q'
   do iElem = TMAX,PRCP
     if (cProc == 'tob' .and. cElems(iElem) == 'pcp') cycle
     iCnt = count(cFlagsMv2(iStn,RAW,iElem,:,:)/=' ' .and. (cFlagsMv2(iStn,ADJ,iElem,:,:)/='Q' .and. &
       iDataMv2(iStn,ADJ,iElem,:,:)/=iMiss))

     if (iCnt > 0) then
       write(iUMsg,*) 'Station '//cStnIds(iStn)//' has ',iCnt,' cases where the raw value was ', &
        'flagged but, the flag code is not Q (in the following year months) in ', cElems(iElem)
      do iYr = iMinYr,iMaxYr
        do iMo = 1,12
          if (cFlagsMv2(iStn,RAW,iElem,iYr,iMo) /= ' ' .and. cFlagsMv2(iStn,ADJ,iElem,iYr,iMo) /= 'Q') then
            write(iUMsg,'(a6,1x,i4,1x,i2.2,1x,a1,1x,a1)') cStnIds(iStn),iYr,iMo, &
              cFlagsMv2(iStn,RAW,iElem,iYr,iMo), cFlagsMv2(iStn,ADJ,iElem,iYr,iMo)
          end if
        end do
      end do
   end if

   end do

   !add check for max/min/tavg
   !add check for E flags for tavg when max or min were missing.

   call flush(iUMsg)

 end do !iStn

 do iElem = 1, iNumElem
   close(iUInFtp(ielem))
 end do

 contains
!-------------------------------------------------------------------
 subroutine readMeta(iUStnList,nStns,cStnIds,rStnLats,rStnLons,iNumStns)

 implicit none

 integer :: iStat, iStn, iNumStns, iUStnList, nStns

 real::rLongitude,rLatitude
 real, dimension(1:nStns) :: rStnLats, rStnLons

 character(6)::cStation
 character(6),dimension(nStns)::cStnIds

 write(*,*) 'iNumStns = ',iNumStns

 end subroutine readMeta
!----------------------------------------------------------------------

 end program genStnUncer
