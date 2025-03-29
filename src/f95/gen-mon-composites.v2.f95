! Expansion for the COOP to WBAN ID transition as the Petaby see fit
!   that is the Coop station is not specifically defined as a WBAN 
!   when in that network. The old USHCN (coop) ID is kept for consistency.
 program gen_mon_composites
 
 implicit none
 
! Declarations ---------------------------------------------
!    changed iMaxYr=2010 to 2020 
 
 integer, parameter :: iUInv=21, iUGhcndMonIn=22, iUGhcndMonOut=23, &
   iUV1File=24, iUMsg=6
 integer, parameter :: iMaxnHcnStns = 1218
 integer, parameter :: iMinYr = 1795, iMaxYr=2015
 integer, parameter :: iNElem = 3
 integer, parameter :: iMiss = -9999
 integer, parameter :: TMAX=1,TMIN=2,PRCP=3
 integer, parameter :: iComp=6

 ! Begin and End dates for each composite (last dim 1=yr, 2=mth, 3=day)
 integer, dimension(1:iMaxnHcnStns,1:iComp,1:3) :: iBegDates, iEndDates 
 ! iMonData is the current composite station, iV1MonData is stn's USHCNv1 data
 integer, dimension(1:iNElem,iMinYr:iMaxYr,1:12) :: iMonData, iV1MonData
 ! iTmpMonData is the data for all the GHCND2M composites data
 integer, dimension(1:iComp,1:iNElem,iMinYr:iMaxYr,1:12) :: iTmpMonData
 ! iTotalYr is the total POR for the HCNv2 data
 integer, dimension(1:iNElem) :: iTotalYr
 
 integer :: iStn, nStns, iStat, iCStn, iElem, iMo, iYr, iYear, &
   iBegYr, iEndYr, iBegMo, iEndMo, iBegDay, iEndDay, &
   iV1Count, iHcnCount, iVal, istrlen, ihcncomp, ifndcomp, &
   iformcomp, iformyr, iV1repl, iFirstYr, iLastYr
 
 ! cStnIds - First ID is the USHCN (COOP) number used for the official USHCNv2
 !           Second ID is the Coop or Wban number that is used in ingest
 !           Third through Fifth are the composited stations to extend the thread
 character(len=11), dimension(1:iMaxnHcnStns,1:iComp) :: cStnIds
 character(len=04), dimension(1:iNElem) :: cElems = (/'tmax','tmin','prcp'/)
 character(len=01), dimension(1:iNElem,iMinYr:iMaxYr,1:12) :: cF1, cF2, cF3
 character(len=01), dimension(1:iNElem,iMinYr:iMaxYr,1:12) :: cV1F1, cV1F2, &
   cV1F3 
 character(len=01), dimension(1:iComp,1:iNElem,iMinYr:iMaxYr,1:12) :: cTmpF1, &
   cTmpF2, cTmpF3
 character(len=01), dimension(1:iNElem,iMinYr:iMaxYr,1:12) :: cCompSrc

 character(len=132) :: cBaseDir, cHcnInv, cCompInv, cLine
 character(len=11) :: cStnId, cStnCW, cHcnId, cHcnComp, cCoopReg
 character(len=01) :: cSeg
 
 logical :: lExist
 
!-------------------------------------------------------------- 
 
 ! base data directory - input/output directories
 call getarg(1,cBaseDir)
 istrlen = len_trim(cBaseDir)
 if(cBaseDir(istrlen:istrlen) /= '/') cBaseDir(istrlen+1:istrlen+1) = '/'
 print *,'BaseDir: ',trim(cBaseDir)

 ! USHCN v2 station list ushcn-v2-stations.txt
 call getarg(2,cHcnInv)
 print *,'Hcn Station List: ', trim(cHcnInv)

 ! USHCN v2 composite list ushcn-v2-composites.txt
 call getarg(3,cCompInv)
 print *,'USHCN v2 Composite list: ', trim(cCompInv)
 
 ! Network Region to draw COOP data from (originally COOP)
 call getarg(4,cCoopReg)
 print *,'COOP Region network directory: ', trim(cCoopReg)
 
 cStnIds = ' '
 iV1MonData = iMiss
 iHcnCount = 0
 iV1Count = 0

 !Set begin date of first composite to January of iMinYr
 !Set end dates to December of iMaxYr

 iBegDates(:,:,1) = iMinYr
 iEndDates(:,:,1) = iMaxYr
 iBegDates(:,:,2) = 1
 iEndDates(:,:,2) = 12
 iBegDates(:,:,3) = 1
 iEndDates(:,:,3) = 31
 
 open(iUInv,file=trim(cHcnInv),status='old',IOSTAT=iStat)
 if(iStat /= 0) then
   print *,'Error in opening HcnInv: ',trim(cHcnInv)
   stop
 end if  
 
 iStn = 0
 
 do ! Fill in HCN stations id's
   read(iUInv,'(a11,62x,a11)',IOSTAT=iStat) cStnId, cStnCW
   if(iStat == -1) exit
   iStn = iStn + 1
   cStnIds(iStn,1) = cStnId
   cStnIds(iStn,2) = cStnCW
 end do
 
 nStns = iStn
 print *,'Number of HCN Stations: ', nStns
 close(iUinv)
 
 open(iUInv,file=trim(cCompInv),status='old',IOSTAT=iStat)
 if(iStat /= 0) then
   print *,'Error in opening CompInv: ',trim(cCompInv)
   stop
 end if  
 ifndcomp = 0
 ihcncomp = 0
 do ! read in composite list
   read(iUInv,'(2x,a11,1x,a11,2(2i3,i5))',IOSTAT=iStat) cHcnId, cHcnComp, &
      iBegMo, iBegDay, iBegYr, iEndMo, iEndDay, iEndYr
   if(iStat == -1) exit
   ihcncomp = ihcncomp + 1
   
   do iStn = 1,nStns
      ! found HCN station in list for whom this is a composite
      if(cStnIds(iStn,1) == cHcnId) then  
      
        do iCStn = 3,iComp
          ! see if any composite id is already stored, fill in next empty slot
          if(cStnIds(iStn,iCStn) == ' ') then
          
             ! print *,iStn, iCstn, 
	  
            cStnIds(iStn,iCStn) = cHcnComp
            iBegDates(iStn,iCStn,1) = iBegYr
            iBegDates(iStn,iCStn,2) = iBegMo
            iBegDates(iStn,iCStn,3) = iBegDay
	     
            iEndDates(iStn,iCStn,1) = iEndYr
            iEndDates(iStn,iCStn,2) = iEndMo
            iEndDates(iStn,iCStn,3) = iEndDay
            ifndcomp = ifndcomp + 1
            exit
          end if
        end do
      end if
   end do
   if(ifndcomp < 1) print *,'Composited HCN:', cHcnId, ' not found'
 end do
 print *,' Composite records: ', ihcncomp, ' found & stored: ', ifndcomp
 
! Set indefinite end dates to last day of iMaxYr

 where(iEndDates(:,:,1) == 9999) iEndDates(:,:,1) = iMaxYr
 where(iEndDates(:,:,2) == 99) iEndDates(:,:,2) = 12
 where(iEndDates(:,:,3) == 99) iEndDates(:,:,3) = 31
 where(iBegDates(:,:,2) == 99) iBegDates(:,:,2) = 1
 where(iBegDates(:,:,3) == 99) iBegDates(:,:,3) = 1

! Do station loop to fill a data array based on composites 
 iformcomp = 0
 iformyr = 0
 iV1repl = 0
 iTotalYr = 0
 do iStn = 1,nStns
 
   if(mod(iStn, 20) == 1) write(*,'(16x,25(i4,127x))') (iYr,iYr=iMaxYr,iMinYr,-10) 
   iMonData = iMiss
   iTmpMonData = iMiss
   iV1MonData = iMiss
   cF1 = ' '
   cF2 = ' '
   cF3 = ' '
   cCompSrc = ' '
 
   do iCStn = 2,iComp
   
      if(cStnIds(iStn,iCStn) == ' ') exit
      
      !See what data is available in GHCN Daily for the various composites
      do iElem=1,iNElem
   
        inquire(file=trim(cBaseDir)//trim(cCoopReg)//'/raw/'// &
          cStnIds(iStn,iCStn)//'.raw.'//cElems(iElem),EXIST=lExist)
	 
        if(.not. lExist) then
          print *,' Composite Coop missing: ',cStnIds(iStn,iCStn),cElems(iElem)
          cycle
        endif  
	 
        open(iUGhcndMonIn,file=trim(cBaseDir)//trim(cCoopReg)//'/raw/'// &
          cStnIds(iStn,iCStn)//'.raw.'//cElems(iElem),status='old')

        ! Read all the observations and flags into iTmpMonData, cTmpF1, cTmpF2, cTmpF3
        call readGhcndMon()
        close(iUGhcndMonIn)
	 
      end do
      
   end do
   
   !Deal with forming composites
   do iCStn = 2,iComp
   
      if(cStnIds(iStn,iCStn) == ' ') exit
      iformcomp = iformcomp + 1
      
      ! print *,'Comp: ',iCStn, cStnIds(iStn,1),' - ',cStnIds(iStn,iCStn),' : ', &
      !   iBegDates(iStn,iCStn,1), iBegDates(iStn,iCStn,2), iBegDates(iStn,iCStn,3), &
      !   ' to ', iEndDates(iStn,iCStn,1), iEndDates(iStn,iCStn,2), iEndDates(iStn,iCStn,3) 
      
      !deal with composites here
      do iYr = iBegDates(iStn,iCStn,1), iEndDates(iStn,iCStn,1)
	
        write(cSeg,'(i1)') iCStn-1
        iformyr = iformyr + 1
      
        if(iYr > iBegDates(iStn,iCStn,1) .and. iYr < iEndDates(iStn,iCStn,1)) then  
      
          iMonData(:,iYr,:) = iTmpMonData(iCStn,:,iYr,:)
          cF1(:,iYr,:) = cTmpF1(iCStn,:,iYr,:)
          cF2(:,iYr,:) = cTmpF2(iCStn,:,iYr,:)
          cF3(:,iYr,:) = cTmpF3(iCStn,:,iYr,:)
          where(iTmpMonData(iCStn,:,iYr,:) /= iMiss) cCompSrc(:, iYr, :) = cSeg
	  
        elseif(iYr == iBegDates(iStn,iCStn,1)) then
	  
          iBegMo = iBegDates(iStn,iCStn,2)
          iMonData(:,iYr,iBegMo:12) = iTmpMonData(iCStn,:,iYr,iBegMo:12)
          cF1(:,iYr,iBegMo:12) = cTmpF1(iCStn,:,iYr,iBegMo:12)
          cF2(:,iYr,iBegMo:12) = cTmpF2(iCStn,:,iYr,iBegMo:12)
          cF3(:,iYr,iBegMo:12) = cTmpF3(iCStn,:,iYr,iBegMo:12)
          where(iTmpMonData(iCStn,:,iYr,iBegMo:12) /= iMiss) &
            cCompSrc(:, iYr, iBegMo:12) = cSeg
	  
        elseif(iYr == iEndDates(iStn,iCStn,1)) then
	   
          iEndMo = iEndDates(iStn,iCStn,2)
          iMonData(:,iYr,1:iEndMo) = iTmpMonData(iCStn,:,iYr,1:iEndMo)
          cF1(:,iYr,1:iEndMo) = cTmpF1(iCStn,:,iYr,1:iEndMo)
          cF2(:,iYr,1:iEndMo) = cTmpF2(iCStn,:,iYr,1:iEndMo)
          cF3(:,iYr,1:iEndMo) = cTmpF3(iCStn,:,iYr,1:iEndMo)
          where(iTmpMonData(iCStn,:,iYr,1:iEndMo) /= iMiss) &
            cCompSrc(:, iYr, 1:iEndMo) = cSeg
	  
        endif
      end do
   end do
   
   !clean up cF3 for possible missing days
   
   where(iMonData == iMiss .and. cF1 /= ' ') cF1 = ' '
   where(iMonData == iMiss .and. cF2 /= ' ') cF2 = ' '
   where(iMonData == iMiss .and. cF3 /= ' ') cF3 = ' '
   
   ! Now we have all of the v2 that we can obtain from GHCN-Daily
   ! So read in the v1 data and see if it adds anything to the station record
   !  NOTE: the USHCNv1 data is not in the same format as the GHCN-Daily
   do iCStn = 1,iComp
   
      if(cStnIds(iStn,iCStn) == ' ') exit
      
      ! print *, iCStn, cStnIds(iStn,iCStn)
      
      do iElem=1,iNElem
     
         inquire(file=trim(cBaseDir)//'hcnv1/raw/USH00'// &
           cStnIds(iStn,iCStn)(6:11)//'.raw.'//cElems(iElem),EXIST=lExist)
	 
         if(.not. lExist) cycle
	 
         open(iUV1File,file=trim(cBaseDir)//'hcnv1/raw/USH00'// &
           cStnIds(iStn,iCStn)(6:11)//'.raw.'//cElems(iElem),status='old')
  	 
         ! Read in STATIC USHCNv1 into iV1MonData, cV1F1, cV1F2, cV1F3
         call readUshcnV1()
         close(iUV1File)
	 
      end do
   
      ! Fill iMonData with V1 data when there is no GHCN-Daily   
      iV1repl = iV1repl + count(iMonData == iMiss .and. iV1MonData /= iMiss)
      where(iMonData == iMiss .and. iV1MonData /= iMiss)
        iMonData = iV1MonData
        cF1 = cV1F1
        cF2 = cV1F2
        cF3 = cV1F3
        cCompSrc = 'V'
      end where
   end do
   
   iV1Count = iV1Count + count(iMonData /= iMiss .and. cF3 /= 'g' .and. &
     cF3 /= 'M' .and. cF3 /= '1')
   iHcnCount = iHcnCount + count(iMonData /= iMiss)

   do iElem = 1,3
      open(iUGhcndMonOut,file=trim(cBaseDir)//'hcnv2/raw/'// &
        cStnIds(iStn,1)//'.raw.'//cElems(iElem), status='replace')
      iFirstYr = 0
      iLastYr = 0
      
      do iYr = iMinYr, iMaxYr

        if(count(iMonData(iElem,iYr,:) /= iMiss) == 0) cycle
        
        ! set first and last years
        if(iFirstYr == 0) iFirstYr = iYr
        iLastYr = iYr
	
! MATT: Monday why???
!	if(count(cF3(iElem,iYr,:)=='1') + count(cF3(iElem,iYr,:)=='M') > 0) &
!          write(iUMsg,'(a11,1x,a4,i5,12(i6,3a1))') &
!          cStnIds(iStn,1),cElems(iElem),iYr,(iMonData(iElem,iYr,iMo), &
!            cF1(iElem,iYr,iMo),cF2(iElem,iYr,iMo),cF3(iElem,iYr,iMo), iMo=1,12)
	
        write(iUGhcndMonOut,'(a11,i5,12(i6,3a1))') cStnIds(iStn,1),iYr, &
          (iMonData(iElem,iYr,iMo),cF1(iElem,iYr,iMo), cF2(iElem,iYr,iMo), &
          cF3(iElem,iYr,iMo), iMo=1,12)
      end do
      iTotalYr(iElem) = iTotalYr(iElem) + (iLastYr - iFirstYr + 1)
      
      close(iUGhcndMonOut)
      
      write(*,'(a11,1x,a4,1x,25("-",10("-",12a1)))') cStnIds(iStn,1),cElems(iElem), &
        ((cCompSrc(iElem,iYr,iMo),iMo=12,1,-1),iYr=iMaxYr,iMinYr,-1) 

   end do
   
 end do !end iStn

 print *,' USHCNv1 Replacement data: ', iV1repl
 print *,' GHCND Composites: ', iformcomp, ' Total year: ', iformyr
 write(*,'("Percent V1 data in HCN = ",f8.2)') 100.0*real(iV1repl)/real(iHcnCount)
 write(*,'("export HMAXYRS=",i6)') iTotalYr(1)
 write(*,'("export HMINYRS=",i6)') iTotalYr(2)
 write(*,'("export HPCPYRS=",i6)') iTotalYr(3)
 
 contains
!----------------------------------------------------------------------- 
 subroutine readGhcndMon()
 ! Read all the observations and flags into iTmpMonData, cTmpF1, cTmpF2, cTmpF3

 do
   read(iUGhcndMonIn,'(12x,i4,12(i6,3a1))',IOSTAT=iStat) iYear, &
     (iTmpMonData(iCStn,iElem,iYear,iMo), cTmpF1(iCStn,iElem,iYear,iMo), &
     cTmpF2(iCStn,iElem,iYear,iMo), cTmpF3(iCStn,iElem,iYear,iMo), iMo = 1,12)
   if(iStat == -1) exit
 end do
 
 end subroutine readGhcndMon
!----------------------------------------------------------------------- 
 subroutine readUshcnV1()
 ! Read in STATIC USHCNv1 into iV1MonData, cV1F1, cV1F2, cV1F3
 
 do
   read(iUV1File,'(12x,i4,12(i6,3a1))',IOSTAT=iStat) iYear, &
     (iV1MonData(iElem,iYear,iMo), cV1F1(iElem,iYear,iMo), &
     cV1F2(iElem,iYear,iMo), cV1F3(iElem,iYear,iMo), iMo = 1,12)
   if(iStat == -1) exit  ! end of file or we have just read the "areal edit" data
 end do
 
 end subroutine readUshcnV1
 
!----------------------------------------------------------------------- 
! --- OnLine version (downloaded from CDIAC web format) CURRENTLY NOT USED ---
 subroutine readUshcnV1Online()
 
 real, dimension(1:12) :: rVals
 
 character(len=01), dimension(1:12) :: cV1F1Tmp, cV1F2Tmp, cV1F3Tmp
 character(len=01) :: cCode
 
 !First do tmax
 
! write(*,*) 'grep '//cStnIds(iStn,1)//' '//trim(cBaseDir)//'v1/hcn_doe_max_data > v1.tmp.file'

 call system('grep '//cStnIds(iStn,1)//' '//trim(cBaseDir)//'v1/hcn_doe_max_data > v1.tmp.file')
 
 open(iUV1File,file='v1.tmp.file')

 do
   read(iUV1File,'(7x,i4,2x,a1,12(f6.2,2a1,1x,a1))',IOSTAT=iStat) iYear, cCode, (rVals(iMo), &
     cV1F1Tmp(iMo), cV1F3Tmp(iMo), cV1F2Tmp(iMo), iMo = 1,12)
   if(iStat == -1) exit  ! end of file or we have just read the "areal edit" data
 
   if(cCode==' ') then
 
     where(rVals/=-99.99) 
       iV1MonData(TMAX,iYear,:) = int(rVals*10)
       cV1F1(TMAX,iYear,:) = cV1F1Tmp
       cV1F2(TMAX,iYear,:) = cV1F2Tmp
       cV1F3(TMAX,iYear,:) = cV1F3Tmp
     end where
   end if

 end do
 
 close(iUV1File)
 
 !Then do tmin

 call system('grep '//cStnIds(iStn,1)//' '//trim(cBaseDir)//'v1/hcn_doe_min_data > v1.tmp.file')
 
 
 open(iUV1File,file='v1.tmp.file')

 do
   read(iUV1File,'(7x,i4,2x,a1,12(f6.2,2a1,1x,a1))',IOSTAT=iStat) iYear, cCode, (rVals(iMo), & 
     cV1F1Tmp(iMo), cV1F2Tmp(iMo), cV1F3Tmp(iMo), iMo = 1,12)
   if(iStat == -1) exit  ! end of file or we have just read the "areal edit" data
 
   if(cCode==' ') then
 
     where(rVals/=-99.99) 
       iV1MonData(TMIN,iYear,:) = int(rVals*10)
       cV1F1(TMIN,iYear,:) = cV1F1Tmp
       cV1F2(TMIN,iYear,:) = cV1F2Tmp
       cV1F3(TMIN,iYear,:) = cV1F3Tmp
     end where
   end if

 end do
 
 close(iUV1File)

 !Finally do prcp

 call system('grep '//cStnIds(iStn,1)//' '//trim(cBaseDir)//'v1/hcn_doe_pcp_data > v1.tmp.file')
 
 open(iUV1File,file='v1.tmp.file')

 do
   read(iUV1File,'(7x,i4,2x,a1,12(f6.2,2a1,1x,a1))',IOSTAT=iStat) iYear, cCode, &
     (rVals(iMo), cV1F1Tmp(iMo), cV1F2Tmp(iMo), cV1F3Tmp(iMo), iMo = 1,12)
   if(iStat == -1) exit  ! end of file or we have just read the "areal edit" data
 
   if(cCode==' ') then
 
     where(rVals/=-99.99) 
       iV1MonData(PRCP,iYear,:) = int(rVals*10)
       cV1F1(PRCP,iYear,:) = cV1F1Tmp
       cV1F2(PRCP,iYear,:) = cV1F2Tmp
       cV1F3(PRCP,iYear,:) = cV1F3Tmp
     end where
   end if

 end do
 
 close(iUV1File)
 
 end subroutine readUshcnV1OnLine
 end program gen_mon_composites
