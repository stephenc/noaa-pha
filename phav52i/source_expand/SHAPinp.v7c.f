      subroutine read_hist(mmunit,mounit,ntstn,mmdates)
      
c     Read the history data for the current network from either 
c       the legacy USHCN format (irandom == 0)
c       or Matt's meta.txt file format (irandom != 0). 
c     Store these data into the UCP's Station History "Hit" array (sahist)
c
c   NOTE: CDMP history is the COOP data files has been moved from source = 3
c              to source = 1 and resorted.                      08aug05
c
c      UCP  Ver   Date     Description
c
c     (52g) v7 20100304   Merge with initial USHCN 52d tarball

c   verion      Description                                       date
c   ------  --------------------------------------------------  --------
c
c    v7c    Rework of the "alignmoves" code - it has become too
c             cumbersome even to debug                          18may11
c
c    v7b    Add Switchboard branch updates to process PW        02may11
c
c    v7a    Skyline modification                                10oct10
c
c    v6     Set "a" flag for short segment removals             05jan07
c              Needed in auto-update process
c
c    --------------------------- Official Version USHCNv2 ----------------------
c    ---------------------------------- 21 Dec 2006 ----------------------------
c
c    v5a    Set mmdates to MONTHLY index to adjust monthly      29nov06
c             values when annual (itimeres == 0)
c
c    v5     Alignmoves - Removed monthly values in TEMP array   13nov06
c
c              retro fix - mounit passed to inshp4              08nov06
c                     and meta record indices
c    v4f    Added mmdates for MMTS evaluation                   03nov06
c
c    v4e    Slight change in Last-dist-dir logic: if HCN and 
c             source = HCNSHF then "999 999" := MOVE            18may06
c
c    v4d    Set minimum segment length for the SHF "solid"      17nov05
c              suspect changepoints in alignmoves with minlenshf
c
c    v4c    make a more user (and data/meta) friendly alignmoves
c           by breaking up and simplifying the loops             28sep05
c
c    v4b    breakout the shf-data align wrt missing & minlen
c           so that it may be called for MM generated series     23aug05
c
c    v4a    includes changing sahist(yr,mth,stn) into            10aug05
c           sahist(ichgpt,stn)
c
c    v4     Rework the priorities for the COOP-3220 data and 
c           GPS revision.
c            1) Indentify probable GPS ONLY station history entry
c                as a NO MOVE
c            2) Remove mysterious Source == 0 from the COOP-3220
c                history
c                                                                04aug05
c
c    v3     Add the CDMP historical data to the mix (source = 3) 25may05
c             Use the CDMP with caution:
c             1) There is no obtime information
c             2) the previous distance fields are overwritten with
c                   the dist to P.O. if the previous dist is blank
c                   (see /home/cwilliam/CDMP_3220/stnhist)
c             3) the instrument strings are RIGHT justified
c
c    v1     Original insertion into UCPmonthly.v2.f             19jan05
c
c   Subroutine Parameters
c     mmunit - SHAP formatted input file unit
c     mounit - Output of compressed meta record's (MM's metadata format)
c     ntstn - Candidate stations sub-network
c     nht - Number of suspect changepoints (moved to restart)
c
      INCLUDE 'inhomog.parm.mthly.incl'  
      include 'inhomog.comm.mthly.incl'
      include 'inhomog.restart.mthly.incl'

c     setup the location and amount arrays
      real amt(ninh),slope(ninh)
      integer loc(ninh)

c     network station list (base station first)
      character*11 ntstn(maxstns), cin
      
c     input PTHORNE station
      character*11 pstn, lstn

c     MM's metadata to the alignment routine (move is Sky array)
      integer inmove(ninh), move(ninh), mday(ninh)
      
c     mmdates contains the dates of going to/from MMTS
      integer mmdates(maxstns,2)

      lin = 0
      
      if(irandom .ne. 0) then
        print *,'--------  Mmenne random series history --------'
c       since, in the realworld, there are no amounts - they are ignored...
c       note: readnet brings in data and generates Sky indices
      
c       go thru each station
        do istn = 1,maxstns
          itarg = nindx(istn,1)
          call firstsky(itarg, itmo1, ityr1, itmth1, itsky1)
          call lastsky(itarg, itmo2, ityr2, itmth2, itsky2)        

          move = 0
          amt = 0.0
          slope = 0
          mday = 31
          mday(1) = 1

c         setup the first move to begin at the beginning
          move(1) = itsky1

c         read the station history
          read(mmunit,*,end=999) in,is,nc,(inmove(ic),amt(ic),
     *      ic=2,nc+1)
          do ic = 2, nc+1
            move(ic) = inmove(ic) + itsky1 - 1
          enddo  
          
          write(cin,'(i11)') in
          
          move(nc+2) = itsky2

c         align SHF moves with missing and minlen data segments
c           and sets SAHIST for main processing
          call alignmoves(cin, istn, mounit, nc+2, move, amt, 
     *      mday)
        enddo
        
c      if mmunit == 0, defer to realworld histories if ishfmeta != 0
      else if(ishfmeta .ne. 0 .and. mmunit .ne. 0) then
        print *,'---------- Pthorne random series history ---------'
      
c       init last station
        lstn = ''
        nc = 0
        istn = 0
        itotmstn = 0
        itotmeta = 0
        
        do while (1 == 1)
          
c          print *, 'Read in the station history'
          read(mmunit,'(2x,a11,i7,i2)',end=90,err=100) pstn, iym, itemp
          if(idebug .ge. 2) print *,pstn, iym, itemp
            
c         see if station has changed
          if(pstn .ne. lstn) then
            if(nc .ge. 1) then
c             finish out the last station move with the end-of-period
              nc = nc + 1
              move(nc) = itsky2
c             process accumulated metadata for last station
c             align SHF moves with missing and minlen data segments
              if(idebug .ge. 2) 
     *          print *,' IN: ',cin, istn, (move(i),i=1,nc)
              call alignmoves(cin, istn, mounit, nc, move, amt, 
     *          mday)
              if(idebug .ge. 2) 
     *          print *,' OUT: ',cin, istn, (move(i),i=1,nc)
              itotmeta = itotmeta + nc
            endif
            
c           initialize metadata arrays for current station
            nc = 0
            do ichg = 1, ninh
              move(ichg) = 0
              amt(ichg) = amiss
              slope(ichg) = 0
              mday(ichg) = 31
            enddo  

c           find the current station
            ifound = 0
            cin = pstn
            do istn = 1, maxstns
              if(ntstn(istn) .eq. cin) then
                ifound = 1
                itotmstn = itotmstn + 1
                itarg = nindx(istn,1)
                call firstsky(itarg, itmo1, ityr1, itmth1, itsky1)
                call lastsky(itarg, itmo2, ityr2, itmth2, itsky2)        

c               setup the first move to begin at the beginning
                nc = 1
                move(nc) = itsky1
                mday(nc) = 1
c                print *,' Found: ',istn, itsky1, itsky2
                goto 80
              endif
            enddo
          
   80       if(ifound .eq. 0) then
              print *,'No station found for ',cin
            else  
              lstn = pstn
            endif
          endif
            
          if(ifound .eq. 1) then
            iy = iym/100
            im = mod(iym,100)
            call work2sky(itarg,-1,iy,im,iskymo,0)
            if(iskymo .gt. 0) then
              nc = nc + 1
              move(nc) = iskymo
c              print *,cin,iy,im,iskymo
            else
              if(idebug .ge. 2) print *,'Outside of POR:',cin,iy,im
            endif  
          endif  
        enddo

   90   if(nc .ge. 1) then
          itotmstn = itotmstn + 1
          nc = nc + 1
          move(nc) = itsky2
c         process accumulated metadata for last station
c         align SHF moves with missing and minlen data segments
c          print *,' IN: ',cin, istn, (move(i),i=1,nc)
          call alignmoves(cin, istn, mounit, nc, move, amt, 
     *      mday)
c          print *,' OUT: ',cin, istn, (move(i),i=1,nc)
          itotmeta = itotmeta + nc
        endif
        print *,'Number of PW-SHF stations, changes:',itotmstn,itotmeta

      else if(ishfmeta .ne. 0) then
        print *, '----------- Normals history format --------------'
        call inshp4(ntstn, inread, amt, mmdates, mounit)  
        if(inread .eq. 0) stop
      endif
      
c     temporary abort for printout
c     also set IHDBUG=2 in INSHP4
c      stop
      
      return
      
  100 call perror(' Unable to read Pthorne history: ' // mattmeta)
      stop
      
c     read error in Matt's meta.txt
  999 call perror(' Unable to read Matt meta.txt: ' // mattmeta)
      stop
      end

C***********************************************************************
C                          Subroutine INSHP4                           *
C                                                                      *
C                                                                      *
C VERSION     MODIFICATION                                     DATE    *
C -------     ------------                                  ---------- *
c
c INSHP4  incorporated into the UCPMONTHLY program 
c             NOTE: removes data arrays, HISTORY ONLY!!!!!
c                                                           19 jan 2005
c
C   1.1       SUN Workstation Version                       10/24/1995 *
C                                                                      *
C   1.0       Original Version                                         *
C                                                                      *
C AUTHOR:  Claude Williams                                             *
C                                                                      *
C MODIFIERS:  Pam Hughes                                               *
C             David Bowman                                             *
C                                                                      *
C USAGE:  INSHP4(character*6 ntstns, integer INEL, 
c                   integer sahist(ninh,nstns), integer inread)
C                                                                      *
C DESCRIPTION:  This subroutine is passed an array containing stations *
C               on which to get data, an array of the station          *
C               identification numbers of the candidate station and    *
C               its 20 most correlated closest neighbors, begin year,  *
C               end year, and element.  The subroutine reads in the    *
C                history information for   *
C               the appropriate stations, and determines station moves *
C               and move dates (day of month).                         *
C                                                                      *
C REQUIREMENTS:                                                        *
C                                                                      *
C   Input Files:  hcn_mmts_max_data                                    *
C                 hcn_mmts_mean_data                                   *
C                 hcn_mmts_min_data                                    *
C                 hcn_mmts_pcp_data                                    *
C                 hcn_station_history                                  *
C                                                                      *
C NOTES:  Fixed bugs in decoding of observation time, in determining   *
C         move using distance and direction from previous location,    *
C         and in initializing of IHDAY array. - D. Bowman: 09/24/93    *
C                                                                      *
C         Added an initialization loop for candidate station's flags   *
C         after realization that the absence of this reinitialization  *
C         during recursive SHAP runs jumbles the O/P flags, and        *
C         retains candidate station's flags on adjusted record after   *
C         grab of the data value. - P. Hughes:  09/16/93               *
C                                                                      *
C         Changed back to 20 neighbors (from 40). - P. Hughes:         *
C         04/09/93                                                     *
C                                                                      *
C         C. Williams originally let loop through 22, but NTR was only *
C         declared as 21.  Finagled NTR in calling program and         *
C         subroutine to go with larger indexing. - P. Hughes:          *
C         12/17/92                                                     *
C                                                                      *
C RESULTS:  Stores the history move *
C           dates (day of month) for the array of stations.     *
C                                                                      *
C PARAMETERS:                                                          *
C                                                                      *
C   nstns     Network of Stations Including Candidate                 *
C                                                                      *
C   numyr     Last Cell of Arrays that Hold Year Data; Currently Set  *
C              to Handle Through the Year 2025                         *
C                                                                      *
C FUNCTIONS:                                                           *
C                                                                      *
C   DIRTOI     Converts Character Direction to Numeric Value           *
C                                                                      *
C VARIABLES:                                                           *
C                                                                      *
C   BEGD       Begin Day from Station History Record                   *
C                                                                      *
C   BEGM       Begin Month from Station History Record                 *
C                                                                      *
C   BEGY       Begin Year from Station History Record                  *
C                                                                      *
C   BM         Begin Month                                             *
C                                                                      *
C   DREC       Number of Data Record to Read                           *
C                                                                      *
C   DYEAR      Year Read from Data Record                              *
C                                                                      *
C   ELEV       Elevation from Station History Record                   *
C                                                                      *
C   EM         End Month                                               *
C                                                                      *
C   ENDD       End Day from Station History Record                     *
C                                                                      *
C   ENDM       End Month from Station History Record                   *
C                                                                      *
C   ENDY       End Year from Station History Record                    *
C                                                                      *
C   FIRSTY     First Year of Data if Later Year Than begyr               *
C                                                                      *
C   HISTCD     History Code              0 => No Move                  *
C                                        1 => Real Move                *
C                                        2 => Observer Move Only       *
C                                                                      *
C   HREC       Number of History Record to Read                        *
C                                                                      *
C   I          Loop Counter                                            *
C                                                                      *
C   endyr        Latest Year to Run Program                              *
C                                                                      *
C   IDIRDF     Absolute Value of Difference in Directions to Determine *
C              if Post Office Direction Changed by 90 Degrees or More  *
C                                                                      *
C   IHDAY      Move Number Array; Holds Begin Day of Move When Begin   *
C   (30)       Day is Not = "1st"                                      *
C                                                                      *
C   begyr        Earliest Year to Begin Program                          *
C                                                                      *
C   INEL       Element                        1 => Maximum Temperature *
C                                             2 => Minimum Temperature *
C                                             3 => Mean Temperature    *
C                                             4 => Precipitation       *
C                                                                      *
C   INSHT      Instrument Height from Station History Record:          *
C                          (1:2) Holds Precipitation Instrument Height *
C                          (3:4) Holds Temperature Instrument Height   *
C                                                                      *
C   INSTR      Instruments from Station History Record                 *
C                                                                      *
C   LAT        Latitude from Station History Record                    *
C                                                                      *
C   LDIR       Direction from Previous Location from Station History   *
C              Record                                                  *
C                                                                      *
C   LDIS       Distance from Previous Location from Station History    *
C              Record                                                  *
C                                                                      *
C   LELEV      Previous Record's ELEV                                  *
C                                                                      *
C   LINSHT     Previous Record's INSHT                                 *
C                                                                      *
C   LINSTR     Previous Record's INSTR                                 *
C                                                                      *
C   LLAT       Previous Record's LAT                                   *
C                                                                      *
C   LLON       Previous Record's LON                                   *
C                                                                      *
C   LOBSER     Previous Record's OBSER                                 *
C                                                                      *
C   LON        Longitude from Station History Record                   *
C                                                                      *
C   LPODIB     Previous Record's PODIB                                 *
C                                                                      *
C   LPODIS     Previous Record's PODIS                                 *
C                                                                      *
C   LTMPOB     Previous Record's TMPOB                                 *
C                                                                      *
C   eyear     Minimum of ENDY and endyr                                 *
C                                                                      *
C   MTHIND     Index for Month Component of Arrays                     *
C                                                                      *
C   byear     Maximum of BEGY and begyr                                 *
C                                                                      *
C   MOVNUM     Number of Station Moves                                 *
C                                                                      *
C   NH         Net History                                             *
C   (nstns,numyr,12)  nstns Stations, numyr Years, 12 Months       *
C                                    0 => No Move                      *
C                                    1 => Real Move and Begin Day = 01 *
C                                    2 => Real Move and Begin Day > 01 *
C                                    9 => Missing                      *
C                                                                      *
C   NOTMOV     Determines if Station Moved:                            *
C              (1:1) = 0 => Change in Observation Time (Temperature)   *
C              (1:1) = 1 => No Change in Observation Time or Current/  *
C                           Previous Observation Time Unknown          *
C                           (Temperature)                              *
C              (1:1) = 1 => (Precipitation)                            *
C              (2:2) = 0 => Change in Instrument Height                *
C              (2:2) = 1 => No Change in Instrument Height, or         *
C                           Current/Previous Instrument Height Unknown *
C              (3:3) = 0 => Change in Instrument "CRS" or "NSS"        *
C                           (Temperature)                              *
C              (3:3) = 1 => No Change in Instrument "CRS" and "NSS"    *
C                           (Temperature)                              *
C              (3:3) = 0 => Change in Instrument "FP", "NSRG", "RRNG", *
C                           or "TB" (Precipitation)                    *
C              (3:3) = 1 => No Change in Instrument "FP" and "NSRG"    *
C                           and "RRNG" and "TB" (Precipitation)        *
C              (4:4) = 0 => Change in Distance and/or Direction from   *
C                           Previous Location                          *
C              (4:4) = 1 => No Change in Distance and Direction from   *
C                           Previous Location, or Unknown Distance     *
C                           from Previous Location                     *
C                                                                      *
C   OBSER      Observer from Station History Record                    *
C                                                                      *
C   OBTIM      Observation Time from Station History Record            *
C                                                                      *
C   iPASS       Flag to Check for Real Move:                            *
C                0 => First Record of Station History; Do Not Check    *
C                     for Move                                         *
C                1 => Other than First Record of Station History;      *
C                     Check for Move                                   *
C                                                                      *
C   PODIB      Direction (and Block Column) from Post Office from      *
C              Station History Record                                  *
C                                                                      *
C   PODIS      Distance from Post Office from Station History Record   *
C                                                                      *
C   POSCHG     Station Move                             0 => No Move   *
C                                                     > 0 => Real Move *
C                                                                      *
C   RECDAT     Monthly Data Value from Data Record                     *
C   (12)                                                               *
C                                                                      *
C   istn     Index for Station Component of Arrays                   *
C                                                                      *
C   TMPOB      Temperature Observation Time Decoded from OBTIM         *
C                                                                      *
C   YEAR       Index for Year to Look for Data/History                 *
C                                                                      *
C***********************************************************************

      SUBROUTINE INSHP4(ntstn, inread, amt, mmdates, mounit)

c      IMPLICIT INTEGER (A-Z)

      include 'inhomog.parm.mthly.incl'
      include 'inhomog.comm.mthly.incl'
      include 'inhomog.restart.mthly.incl'
      
c     instrument string parameters
      PARAMETER (ninstr = 38, maxnin = 11)

c     network station list (base station first)
      character*11 ntstn(maxstns)

      character*30 movstr
      DIMENSION IHDAY(30)
      
      integer mmdates(maxstns,2)

      CHARACTER*3 LDIR
      CHARACTER*4 INSHT,LTMPOB,NOTMOV,OBTIM,TMPOB
c      CHARACTER*4 LPODIB,PODIB
      character*4 linsht
      character*2 insth, linsth/'  '/
c      CHARACTER*6 LAT,LLAT
      CHARACTER*11 cstnstr
c      CHARACTER*7 LLON,LON
      character*11 ldisdir
c      CHARACTER*46 LOBSER,OBSER
      character*132 histfile
c      character*132 elemfile
      REAL RECDAT(12)
      integer src, move(ninh), mday(ninh)
      real amt(ninh)
      integer begm, begy, begd, endy, endm, endd, byear, eyear
      integer histcd, elev
c     because of the measurement changes in Lat/Lon there is a dramatic
c        change in lat/lon resolution as of 1998. In the new code, this 
c        will have to be dealt with better, probably with testing the
c        measurement type, then the amount of change (=45 arcseconds)
      real lleps /0.0125/
      real latdeg,latmin,latsec,londeg,lonmin,lonsec
      real alat,allat,alon,allon,aallat,aallon

      character*5 cstrng(ninstr) / 'AI   ','CRS  ','DT   ','EVA  ',
     *     'FP   ','HYTHG','MXMN ','NRIG ','NSRG ','NSS  ','RRIG ',
     *     'RRNG ','SDE  ','SG   ','SRG  ','SS   ','TG   ','DGT  ',
     *     'TB   ','EVO  ','MMTS ','TELSY','HYGRO','HY6  ','HY8  ',
     *     'SFP  ','SRRNG','SSG  ','SSRG ','STB  ','AMOS ','AUTOB',
     *     'PSY  ','ASOS ','PLAST','STO  ','HYGR ','NIMBS'/
      character*5 cstnin(maxnin)
      integer INSTR(ninstr),LINSTR(ninstr)
c      character*5 prcp1/'     '/, tmpr1/'     '/, 
c     *  lprcp1/'     '/, ltmpr1/'     '/

      print *,' INSHP4 - old style UHCNv1 station history files'

      inread = 1
c     temporary increment in printout
c     also set STOP in MAIN
c      ihdbug = 2
      ihdbug = 0
      
c     tolerance limit for lat/lon changes wrt GPS ONLY entry (45 arcseconds)
      tolgps = 45./60./60.
      
      maxmo = nmo
      if(itimeres .eq. 0) maxmo = numyr

C     INITIALIZE VARIABLES
      FIRSTY = 0
      DO 10 I = 1,30
        IHDAY(I) = 0
   10 CONTINUE

C     NOTE - HISTORY ARRAY INITIALIZED ELSEWHERE

C     GET STATION NETWORK HISTORY
      nohist = 0
      DO 150 istn = 1,numsubs
        cstnstr = ntstn(istn)
        if(iushcn .eq. 0) then
          istnlen = 11
        else
          istnlen = 6
        endif    
c       initialize instrument string
        do iin = 1, ninstr
          linstr(iin) = 0
          instr(iin) = 0
        end do
c       define paths to USHCN & CDMP-3220 datasets (and current type - icoop)
c       NOTE: ncand==0 assumes USHCN format for "COOP" stations
        if(istn .le. ncand .or. ncand .eq. 0) then
          histfile = incand(1:lnblnk(incand)) // 'his/' // 
     *      cstnstr(1:istnlen) // '.his'
          icoop = 0
        else
          histfile = incoop(1:lnblnk(incoop)) // 'his/' // 
     *      cstnstr(1:istnlen) // '.his'
          icoop = 1
        endif    

C       OPEN STATION HISTORY FILE
        if(idebug .gt. 1) print *,"History: ",histfile
        OPEN(9,FILE=histfile,STATUS='old',ERR=50)
        goto 55
          
   50   nohist = nohist + 1
        if(idebug .gt. 0) 
     *    print *, 'No history: ', histfile(1:lnblnk(histfile))
        goto 150
c       print *,' No history set (no file) for: ',istn,' ',cstnstr
c       print *, histfile
c       goto 150

C       INITIALIZE VARIABLES
   55   MOVNUM = 0
        iPASS = 0
c       last dates for the HCN source (=0)
        lys0 = 0
        lms0 = 0
        lds0 = 0

c       initialize incoming meta-move arrays 
        move = 0
        amt = 0.0
        mday = 31
 
C       STATION HISTORY FILE LOOP
C       READ STATION HISTORY RECORDS
        do while (1 .eq. 1)
c         initialize instrument string
          do iin = 1, ninstr
            linstr(iin) = instr(iin)
            instr(iin) = 0
          end do
              
   85     read(9,90,end = 120,err=220) src,begy,begm,begd,endy,endm,
     *       endd, latdeg, latmin, latsec,londeg, lonmin, lonsec,
     *       ldisdir,elev,insht,obtim,cstnin
   90     format(i1,7x,2(1x,i4,2i2),1x,f3.0,2f3.0,1x,f4.0,
     *           2f3.0,1x,a11,1x,i5,2x,a4,1x,a4,5x,13(a5,1x))
          histcd = 0
          if(ihdbug .ge. 2) write(6,90) src,begy,begm,begd,endy,endm,
     *       endd, latdeg, latmin, latsec,londeg, lonmin, lonsec,
     *       ldisdir,elev,insht,obtim,cstnin

c         The daily time of observation history records are not used here...
          if(src .eq. 1) go to 85
c         CDMP data should not be used with HCN stations!
          if(src .eq. 3 .and. icoop .eq. 0) goto 85
            
c         MSHR history records are used after the last begin date of the
c           USHCN & CDMP history records
          if(src .eq. 2 .and. (begy .lt. lys0 .or. 
     *      (begy.eq.lys0 .and. begm.lt.lms0) .or.
     *      (begy.eq.lys0 .and. begm.eq.lms0 .and. begd.le.lds0))) 
     *      goto 85
     
c         shift the instrument strings for the CDMP_3220 history
c          (Coop data (coop = 1: src=0)
          if(src .eq. 0 .and. icoop .eq. 1) then
c           there are only 10 max instruments
            do inin = 1, 10
              if(lnblnk(cstnin(inin)) .eq. 1) goto 92
              do while (cstnin(inin)(1:1).ne.' ')
                cstnin(inin)(1:4) = cstnin(inin)(2:5)
                cstnin(inin)(5:5) = ' '
              enddo
            enddo
          endif      
     
c         Get lat/lon ready
   92     alat = latdeg + ((latmin + latsec/60.0)/60.0)
          alon = londeg + ((lonmin + lonsec/60.0)/60.0)

c         decode last dir/dir from the USHCN
          if(src .eq. 0 .and. icoop .eq. 0) then
c           see if there is a decimal in distance
            if(ldisdir(1:1) .eq. '.' .or. ldisdir(2:2) .eq. '.' .or.
     *        ldisdir(3:3) .eq. '.') then
              read(ldisdir(1:3), '(f3.1)') dtemp
              ldis = int(dtemp * 10.)
            else
              read(ldisdir(1:3),'(i3)') ldis
            endif  
            ldir = ldisdir(5:7)
          endif  

c         find instruments
          do inin= 1, maxnin
            do iin = 1, ninstr
              if(cstnin(inin) .eq. cstrng(iin)) then
                instr(iin) = 1
                go to 91
              endif
            end do    
   91       continue
          end do  

c         find instrument height
C         POSITIONS 3 AND 4 HOLD TEMPERATURE INSTRUMENT HEIGHT
C         POSITIONS 1 AND 2 HOLD PRECIPITATION INSTRUMENT HEIGHT
          IF(INEL .LT. 4) THEN
            insth = insht(3:4)
          ELSE
            insth = insht(1:2)
          END IF

C         GET HISTORY FOR CORRECT TIME FRAME
          IF(ENDY .GE. begyr .AND. BEGY .LE. endyr) THEN

C           DECODE TEMPERATURE OBSERVATION TIME:  OBSERVATION TIME = "xxHR" OR
C           "TRID" => COPY TO TEMPERATURE TIME; OBSERVATION TIME = "9xx9" =>
C           COPY "xx" TO TEMPERATURE TIME; OTHERWISE, COPY TO TEMPERATURE
C           TIME.
            IF(INEL .LT. 4) THEN
              IF(OBTIM(3:4) .EQ. 'HR' .OR. OBTIM(3:4) .EQ. 'ID') THEN
                TMPOB = OBTIM
              ELSE IF(OBTIM(1:1) .EQ. '9' .AND.
     *                OBTIM(2:2) .NE. '9') THEN
                TMPOB = OBTIM(2:3)
              ELSE
                TMPOB = OBTIM(3:4)
              END IF
            END IF

C           FILL IN MISSING DATES
            IF(BEGM .EQ. 99) BEGM = 6
            IF(BEGD .EQ. 99) BEGD = 15
            IF(ENDM .EQ. 99 .AND. ENDD .EQ. 99 .AND.
     *         ENDY .EQ. 9999) THEN
              ENDM = 12
              ENDD = 31
              ENDY = endyr
            ELSE
              IF(ENDM .EQ. 99) ENDM = 6
              IF(ENDD .EQ. 99) ENDD = 15
            END IF

C           MERGE INSTRUMENTS

C*      *****************************BACKGROUND*******************************
C
C             MODIFIED THE INSTRUMENT TYPES "EQUATES" FOR "HYGROS" AND
C             "SHIELDS". - P. Hughes 07/15/91
C
C             SET ALL "EQUATES" TO INSTR(2) ("CRS") - "REFRESHES" SAVED LIST 
C             IN THE EVENT AN INSTRUMENT CHANGE IN "EQUATES" OCCURS.
C             - P. Hughes 12/16/91
C
C             "MMTS" HAVE BEEN ADJUSTED A PRIORI
C             "HYGROS" WILL BE EVALUATED BY "SHAP"
C             CHANGES FROM "DT" TO "CRS", ETC. ARE EVALUATED
C             CHANGES FROM "CRS", ETC. TO "HYGRO", ETC. ARE EVALUATED
C             - P. Hughes 04/09/93
C
C             DECISION MADE TO TREAT "DT"s WITH/WITHOUT SHELTERS AS SAME
C             - P. Hughes 04/09/93
c
c             ----------------------- The 2000 update ------------------------
c             Am changing back some of the instrumentation before to 90's.....
c             1) Setting the MMTS apart again (#21)
c             Changes for the Millenneum are:
c             1) Added ASOS as a instrument (#34)
c             2) For Precip also added 4"Plastic (#35) and STO in Hiwaii (#36)
c             3) Source 2 (the MSHR) generic Hygrothemometer (assumed HY8) (#37)
c
c             Master Station History Records (src=2) have two new fields:
c             1) the primary precip instrument and
c             2) the primary temperature instrument
c             USHCN SHF (src=1) primary instruments are determined the from the mix
c               of instruments at any given time.
c                                                            09 May 2001 cw

c           MXMN == HYTHG == SS == TG == DGT
c           E2 - modification remove MMTS from "equate"
c                  i.e. add back MMTS only record as changepoint
            IF(INSTR(7) .EQ. 1 .OR.
     *         INSTR(6) .EQ. 1 .OR. INSTR(16) .EQ. 1 .OR.
     *         INSTR(17) .EQ. 1 .OR. INSTR(18) .EQ. 1) then
              INSTR(2) = 1
            ELSE
              INSTR(2) = 0
            END IF
              
c           HYGR == HY8
            If(instr(37) .eq. 1) instr(25) = 1

C           ADDED "EQUATES" FOR THE SHIELDED PRECIPITATION INSTRUMENTS
c           SFP == FP : SRRNG == RRNG : SSRG == SRG
            IF(INSTR(26) .EQ. 1) INSTR(5) = 1
            IF(INSTR(27) .EQ. 1) INSTR(12) = 1
            IF(INSTR(29) .EQ. 1) INSTR(15) = 1

C           DETERMINE BEGIN AND END YEARS FOR STATION MOVES LOOP
            if(begy .lt. begyr) then
              begm = 1
              begd = 1
              byear = begyr
            else
              byear = begy
            end if

            if(endy .gt. endyr) then
              endm = 12
              endd = 31
              eyear = endyr
            else
              eyear = endy
            end if

C           LOOP FOR DETERMINING STATION MOVES
            movstr = ''
            byear = byear

C           TEST FOR REAL MOVE
            IF(iPASS .EQ. 1) THEN

C             INITIALIZE VARIABLE
              NOTMOV = '0000'

C             CHECK OBSERVATION TIME
C             NO OBSERVATION TIME CHANGE IF:  PRECIPITATION; UNKNOWN
C             OBSERVATION TIME; UNKNOWN PREVIOUS OBSERVATION TIME; OR
C             OBSERVATION TIME IS SAME AS PREVIOUS OBSERVATION TIME.
              IF(INEL.EQ.4 .OR. TMPOB.EQ.'99' .OR. tmpob.eq.'  '
     *            .or. LTMPOB.EQ.'99' .OR. TMPOB.EQ.LTMPOB) then
                NOTMOV(1:1) = '1'
              else
                movstr = movstr(1:lnblnk(movstr)) // ' OBT'
              endif  

C             CHECK INSTRUMENT HEIGHT
C             NO INSTRUMENT HEIGHT CHANGE IF:  UNKNOWN INSTRUMENT HEIGHT;
C             UNKNOWN PREVIOUS INSTRUMENT HEIGHT; OR INSTRUMENT HEIGHT IS SAME
C             AS PREVIOUS INSTRUMENT HEIGHT.

              if(ihdbug .ge. 2) then
                print *,' insth,linsth: ', insth, ' ', linsth
                write(6,'(40i1)') (instr(i),i=1,ninstr)
                write(6,'(40i1)') (linstr(i),i=1,ninstr)
              endif  
              IF(insth.EQ.'99' .OR. insth.eq.'  ' .or.
     *           linsth.EQ.'99' .OR. linsth.eq.'  ' .or.
     *           insth .EQ. linsth) THEN
                NOTMOV(2:2) = '1'
              ELSE
                HISTCD = 1
                movstr = movstr(1:lnblnk(movstr))  // ' IHT'
              END IF

C             TEST INSTRUMENTS
C             NO INSTRUMENT CHANGE IF:  TEMPERATURE, AND NO CHANGE IN "CRS" AND
C             NO CHANGE IN "NSS"; OR PRECIPITATION, AND NO CHANGE IN "SRG" OR NO
C             CHANGE IN "FP" AND NO CHANGE IN "NSRG" AND NO CHANGE IN "RRNG" AND
C             NO CHANGE IN "TB".
              IF(INEL .LT. 4) THEN
c               if current && last ASOS inst then NO MOVE
                if(instr(34).eq.1 .and. 
     *            linstr(34).eq.1) then
                  NOTMOV(3:3) = '1'
c               then CRS
                else if(instr(34).eq.linstr(34) .and.
     *            INSTR(2).eq.1.and.LINSTR(2).eq.1) then
c                 if ASOS not changed and current && last CRS inst then NO MOVE
                  NOTMOV(3:3) = '1'
c               last - everybody else
                else if(instr(34).eq.linstr(34) .and.
     *            INSTR(2).eq.LINSTR(2) .and.
     *            instr(21) .eq. linstr(21) .and.
     *            instr(22) .eq. linstr(22) .and.
     *            INSTR(10) .EQ. LINSTR(10)) THEN
                  NOTMOV(3:3) = '1'
                ELSE
                  HISTCD = 1
                  if(instr(35).eq.1.and.
     *              linstr(35).eq.0) then
                    movstr = movstr(1:lnblnk(movstr)) // ' ASOS'
                  else
                    movstr = movstr(1:lnblnk(movstr)) // ' INST'
                  endif
                END IF
              ELSE
                IF(INSTR(15).eq.1 .and. LINSTR(15) .eq. 1) then
                  NOTMOV(3:3) = '1'
                else if(INSTR(15) .eq. LINSTR(15) .and.
     *            INSTR(5).eq.1 .and. LINSTR(5).eq.1) then
                  NOTMOV(3:3) = '1'
                else if(INSTR(15) .eq. LINSTR(15) .and.
     *            INSTR(5) .eq. LINSTR(5) .and.
     *            INSTR(9) .EQ. LINSTR(9) .AND.
     *            INSTR(12) .EQ. LINSTR(12) .AND.
     *            INSTR(19) .EQ. LINSTR(19) .and.
     *            instr(24) .eq. linstr(24) .and.
     *            instr(25) .eq. linstr(25)) THEN
                  NOTMOV(3:3) = '1'
                ELSE
                  movstr = movstr(1:lnblnk(movstr)) // ' INST'
                  HISTCD = 1
                END IF
              END IF

C             TEST LAST DISTANCE AND DIRECTION
C             TEMPERATURE MOVED);
c             with the newer keyed sources (MSHR & CDMP) - 
c               nonblank ldisdir == MOVE
              if(src.eq.2 .or. (src.eq.0 .and. icoop.eq.1)) then
                if(ldisdir .ne. "           ") then
                  movstr = movstr(1:lnblnk(movstr)) // ' LDIS'
                  HISTCD = 1
                endif 
              else IF(src.eq.0 .and. icoop.eq.0) then
                if(ldis.eq.999) then
                  movstr = movstr(1:lnblnk(movstr)) // ' LDIS'
                  HISTCD = 1
                else if((LDIS .EQ. 0 .AND. LDIR .EQ. '000') .OR.
     *             (INEL .LT. 4 .AND. (LDIS / 100) .EQ. 8) .OR.
     *             (INEL .EQ. 4 .AND. (LDIS / 100) .EQ. 9)) THEN
c                 with the USHCN if ldis != 999 then
C                 NO DIS/DIR CHANGE IF:  NO CHANGE IN DISTANCE AND NO
C                 CHANGE IN DIRECTION; TEMP inst AND DIST only if "8XX" (I.E.,
C                 PRECIP MOVE); PREC inst AND DIST only if "9XX"
                  NOTMOV(4:4) = '1'
                ELSE
                  movstr = movstr(1:lnblnk(movstr)) // ' LDIS'
                  HISTCD = 1
                END IF
              END IF

C             INITIALIZE STATION MOVE VARIABLE
              POSCHG = 0

c -----------------Post Office Position removed 08 May 2001 -----------------

C             TEST ELEVATION, LATITUDE, AND LONGITUDE
C             MOVE IF:  CHANGE IN ELEVATION, LATITUDE, OR LONGITUDE
              IF(ELEV.NE.LELEV) then
                movstr = movstr(1:lnblnk(movstr)) // ' ELEV'
                POSCHG = POSCHG+1
              endif  
                    
              aallat = abs(aLAT-aLLAT)
              aallon = abs(aLON-aLLON)
              if(aallat.gt.lleps .OR. aallon.gt.lleps) then
                movstr = movstr(1:lnblnk(movstr)) // ' LALO'
                POSCHG = POSCHG+1
              endif  

              IF(POSCHG .EQ. 0) THEN
                NOTMOV(4:4) = '1'
              ELSE
C               REAL MOVE OCCURRED
                movstr = movstr(1:lnblnk(movstr)) // ' MOVE'
                HISTCD = 1
              END IF

              if(HISTCD .eq. 1) go to 110

C             NO MOVE OCCURRED
              HISTCD = 0

c ---------------Observer change removed 08 May 2001 -----------------
            END IF

c           last shot to affect history record - is this a GPS only entry
  110       if(byear .gt. 1995 .and. movstr .eq. ' LALO MOVE' .and.
     *        llatsec .eq. 0 .and. llonsec .eq. 0 .and. 
     *        aallat .le. tolgps .and. aallon .le. tolgps) then
              histcd = 0
              movstr = ' GPS'
            endif
            
c           get first and last skyline indices for current station
            itarg = nindx(istn,1)
            call firstsky(itarg, itmo1, ityr1, itmth1, itsky1)
            call lastsky(itarg, itmo2, ityr2, itmth2, itsky2)        
            
c           need to save off the beginning of the history records
            if(movnum .eq. 0) then
              movnum = movnum + 1
              call work2sky(itarg, -1, byear, begm, movskymo, 0)
c             if the stnhist is before the beginning, set to the beginning
              if(movskymo .le. itsky1) then
                move(movnum) = itsky1
                mday(movnum) = 1
              else  
                move(movnum) = movskymo
                mday(movnum) = begd
              endif  
              if(ihdbug .ge. 1) 
     *          write(6,1001)cstnstr,istn,movnum,byear,begm,begd,movstr
            end if              

            call work2sky(itarg, -1, byear, begm, movskymo, 0)
            if(histcd .eq. 0) then
              if(ihdbug .ge. 1) 
     *          write(6,1000)cstnstr,istn,histcd,byear,begm,movstr
 1000         format(a11,i4,' SHF STAY ',i5,1x,i4,1x,i2,1x,a)                    
            else
c             accumulate move dates from the shf records 
c              - use in next step with data
c             if there are more than one before the beginning, skip 
              if(movskymo .gt. itsky1 .and. movskymo .le. itsky2 .and.
     *          movskymo .ne. move(movnum)) then
                movnum = movnum + 1
                move(movnum) = movskymo
                if(itimeres .eq. 0) then
c                 scale annual as if monthly
                  mday(movnum) = (begm * 30 + begd) / 12
                else  
                  mday(movnum) = begd
                endif  
                if(ihdbug .ge. 1) write(6,1001)cstnstr,
     *            istn,movnum,byear,begm,begd,movstr
 1001           format(a11,i4,' SHF MOVE ',i5,i5,2i3,1x,a)
              endif
            end if
c           test for MMTS instr and whether to update mmdates
            if(instr(21) .eq. 1) then
c             MMTS in use - set begin date if not already set
              if(mmdates(istn,1) .eq. 9999) then
c               MMDATES MUST BE A MONTHLY INDEX
                ltimeres = itimeres
                itimeres = 1
                call work2sky(itarg, -1, byear, begm, movskymo, 0)
                itimeres = ltimeres
                mmdates(istn,1) = movskymo
                if(idebug .gt. 1) 
     *            print *,' Turn on MMTS',istn,byear,begm,movskymo
              endif
c            else
c             MMTS not in use - if begin date was set and end date is
c              not set then set end date
c              if(mmdates(istn,1) .ne. 9999 .and. 
c     *          mmdates(istn,2) .eq. 9999) then
c                mmdates(istn,2) = movskymo
c                print *,' Turn off MMTS',istn,movskymo
c              endif  
            endif  

c            print *,' pass,histcd: ', ipass, ' ', histcd
c            if(ipass .eq. 0 .or. histcd .eq. 1) then
C             PAST INITIAL STATION HISTORY RECORD:  INITIALIZE PASS VARIABLE SO
C             TEST FOR REAL MOVES
              iPASS = 1

C             SAVE HISTORY DATA
              LELEV = ELEV
              LINSHT = INSHT
c              LLAT = LAT
c              LLON = LON
c              LOBSER = OBSER
c              LPODIB = PODIB
c              LPODIS = PODIS
              LTMPOB = TMPOB
              linsth = insth
              allat = alat
              allon = alon
              llatsec = latsec
              llonsec = lonsec
              lbegy = begy
              lbegm = begm
              lbegd = begd
              lendy = endy
              lendm = endm
              lendd = endd
c             this is needed for both USHCN & CDMP
              if(src.eq.0) then
                lys0 = begy
                lms0 = begm
                lds0 = begd
              endif  
c            endif  

          END IF ! end of process record within begyr-endyr
        end do ! end of read station history data loop

c       align SHF moves with missing and minlen data segments
  120 if(movnum .gt. 0) then
        call alignmoves(cstnstr, istn, mounit, movnum, move, amt, mday)
      else
        if(idebug .gt. 0) 
     *    print *,'SHF: ',istn,' Station has no moves:', cstnstr
        nohist = nohist + 1
      endif  
        
  150 CONTINUE ! end of station loop
      print *,nohist, ' stations have no history'

C     CLOSE STATION HISTORY FILE
      CLOSE(9,ERR=240)

      RETURN

C     PRINT ERROR MESSAGE AND ABORT PROGRAM

c  160 call perror('CANNOT OPEN DATA FILE:' //  elemfile)
c      GO TO 260

c  180 call perror('CANNOT READ DATA FILE:' // elemfile )
c      GO TO 260

  200 call perror('CANNOT OPEN HCN FILE:' // histfile )
      GO TO 260

  220 call perror('CANNOT READ HCN FILE:' // histfile )
      GO TO 260

  240 call perror('CANNOT CLOSE HCN FILE:' // histfile )
  
  260 WRITE(6,270)
c     let calling program know that the data cannot be retrieved
      inread = 0
  270 FORMAT('SKIPPING STATION')

      return
      END

C***********************************************************************
C End of Subroutine INSHP3.                                            *
C***********************************************************************
 

      subroutine alignmoves(cstnstr, istn, mounit, movnum, move, amt, 
     *  mday)
c       SHF records have been interpreted for moves, align with the data
c         in the UCP routines, the changepoint location is the LAST MONTH
c         of the SEGMENT with data BEFORE (EARLIER) the change.
c       Rules:
c         A) see if the current month can be used, depending upon the day of the move
c           1) if the day > 25 the current month is used with the earlier segment
c           2) if the 5 < day < 25 the current month is unusable (make missing)
c                and use the previous month
c           3) if the day < 5 then the current month is used in the later segment
c                and use the previous month
c         B) Adjust the move backward to the first month with data
c         C) Remove any segments without sufficient data for adjustment calculation
c               LATER: MAY BE ABLE TO USE THE #HITS TO DECIDE ANOTHER METHOD

c     subroutine arguments
c     cstnstr - station identifier
c     istn - station index
c     mounit - output file number

c     Input (raw metadata values)
c     Output (compressed series)
c       movnum - total number of moves
c       move - "skymo" of each move
c       amt - amplitude of each move (unknown for SHF)
c       mday - day of month for each move

c     From restart.mod.f95 module for allocation - 09apr2009 cw
      use restart

      include 'inhomog.parm.mthly.incl'
      include 'inhomog.comm.mthly.incl'
      include 'inhomog.restart.mthly.incl'
      
c     passed arrays
      character*11 cstnstr
      integer move(ninh), mday(ninh)
      real amt(ninh)

c     local temp arrays
      integer newmove(ninh), newday(ninh)
      real newamt(ninh)
      
      maxmo = nmo
      if(itimeres .eq. 0) maxmo = numyr

c     initialize local temp arrays
      newmove = 0
      newday = 0
      newamt = 0.0
      
c     fetch the begin and end month indices of the the first and last
c       years of the stations data.
      call firstsky(istn, itmo1, ityr1, itmth1, itsky1)
      call lastsky(istn, itmo2, ityr2, itmth2, itsky2)
      
      if(ityr1 .eq. -9999) then
        print *,' No data for ', cstnstr
        goto 100
      endif  
      
      if(idebug .gt. 1) print *,istn, itmo1, ityr1, itmth1, itsky1
      if(idebug .gt. 1) print *,istn, itmo2, ityr2, itmth2, itsky2
      
c     re-adjust the sky & iym indices to actual data (instead of Jan-Dec)
      do iskymo = itsky1, itsky2
        if(temp(iskymo) .gt. amiss + 1) then
          ifsky = iskymo
          goto 5
        endif
      enddo
      
    5 do iskymo = itsky2, itsky1, -1
        if (temp(iskymo) .gt. amiss + 1) then
          ilsky = iskymo
          goto 7
        endif
      enddo   
          
c     from this point on, the actual first and last dates are:
c       ifsky and ilsky for the skyline indices
          
c     adjust for day of changepoint
    7 do imove = 1, movnum
        if(move(imove) .ge. ifsky .and. move(imove) .le. ilsky) then

c         ---- Adjust for the day of change ----
c         REMEMBER the move is the last month of a segment
c         if day <= 5 then set move back 1 month
c         if day > 5 && <= 25 then set move back 1 month and delete month data
c         if day > 25 then leave move were it is and keep data 
          if(mday(imove) .le. 5) then
            if(imove .gt. 1) then
c           ... if the move was within the first five days, this
c              month will go with the Next segment
              move(imove) = move(imove) - 1
c              print *,' day<5: ', move(imove)
            endif  
          else if(mday(imove).lt.25 .and. move(imove) .gt. ifsky) then
c           ... if the move was between the 5th and 25th - this
c                 month is no good to anyone, delete it
            indsky = move(imove)
            if(temp(indsky) .ne. amiss) tflg(indsky)(1:2) = ' r'
            temp(indsky) = amiss
            move(imove) = move(imove) - 1
c            print *,' day>5<25 ', move(imove)
c         ... else if the move is after the 25th - then 1) leave metadata
c               alone and 2) use this month in the THIS segment
          endif
        endif
      enddo
      
      if(idebug .gt. 1) print *, cstnstr, ifsky, ilsky

c     go through the station history and data
c       - adding moves that can be tested to the NEWMOVE arrays

c     get stn hist move index up to start of the data
      imove = 1
      do while (move(imove) .lt. ifsky .and. imove .lt. movnum)
        imove = imove + 1
      enddo
      
c     now for the data period-of-record
      ifpor = ifsky
      ilpor = ilsky
      jmove = 0
      ifirst = 0
      ilast = -1
      ntemp = 0
      do isky = ifsky, ilsky
        if(temp(isky) .ne. amiss) then
c         set the first value index for this segment
          if(ifirst .eq. 0) ifirst = isky
c         set the last value index
          ilast = isky
c         number of data points in this segment
          ntemp = ntemp + 1
        endif

c       see if we are at the end of a hist seg or the end of the POR
        if(isky .eq. move(imove) .or. isky .eq. ilsky) then
c         is there enough data in the segment
          if(ntemp .ge. minlenshf) then
c           if enough, then set newmove for usable segment
            jmove = jmove + 1
c           if first move - (re)align first data point with first of segment
            if(jmove .eq. 1) ifpor = ifirst
            ilpor = ilast
            if(isky .eq. move(imove)) then
c             if a stn hist move, add the data to newmove
              newmove(jmove) = move(imove)
              newday(jmove) = mday(imove)
              newamt(jmove) = amt(imove)
            else
c             if the end-of-record - generate last segment
              newmove(jmove) = ilsky
              newday(jmove) = 31
              newamt(jmove) = 0.0
            endif  
          else
c           if not enough, erase data (controversial---)
            do itemp = ifirst, ilast
              if(temp(itemp) .ne. amiss) then
                temp(itemp) = amiss
                tflg(itemp)(1:2) = ' X'
              endif
            enddo
          endif
          
c         increment move and init temp segment variables
c         watch out for multiple moves on the same date
          do while (move(imove) .eq. move(imove+1) 
     *      .and. imove .le. movnum)
            imove = imove + 1
          enddo  
          imove = imove + 1
          ntemp = 0
          ifirst = 0
          ilast = -1
        endif
      enddo
      
      movnum = jmove
      
c     initialize SHF input arrays for the alignmove output
  100 move = 0
      amt = 0.0
      mday = 0

      if(movnum .eq. 0) then
        print *,cstnstr,istn,' WARNING: No segments - DATA REMOVED'
        goto 150
      endif  

      if(idebug .gt. 1) then
        print *,cstnstr,istn,' SHF/DATA move summary: '
        call iskymo2iym(iy,im,ifpor,itsky1,ityr1)
        print *,' First data value: ', iy,im, ifpor
      endif  
      do imove = 1, movnum
        sahist(istn,imove) = newmove(imove)
        move(imove) = newmove(imove)
        amt(imove) = newamt(imove)
        mday(imove) = 31
        call iskymo2iym(iy,im,move(imove),itsky1,ityr1)
        if(idebug .gt. 1) then
          print *,' End seg: ',imove,' ym: ',iy,im,
     *      move(imove), amt(imove)
          if(imove .eq. movnum) then
            call iskymo2iym(iy,im,ilpor,itsky1,ityr1)            
            print *,'   Last data value: ', iy,im,ilpor
          endif
        endif  
      enddo
        
      if(mounit .gt. 0)
     *  write(mounit,1000) cstnstr, istn, movnum, 
     *    (move(i),amt(i),i=1,movnum)
 1000 format(a11,i6,i4,30(i9,f9.3))
      
  150 return
      end
