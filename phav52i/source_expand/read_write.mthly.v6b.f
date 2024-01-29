c     Copied from Skyline.0.9.0 (355)
c !! WARNING: read assumes data are sorted by year for filling skyline arrays!!

c      UCP  Ver   Date     Description
c
c     (52g) v6 20100304   Merge with initial USHCN 52d tarball

c     version  date     description
c     ------- -------   -------------------------------------------
c
c        6b   22mar11   Skyline modification
c
c        6a   24may10   Added back the USHCN (stn*6 & flag*1) format (-U option)
c
c        6    15mar10   Converted to GHCN format (stn*11 & flag*3)
c
c        5p   26aug08   added input for Peter's World (generated from NCEP)
c
c        5    21mar08   cleaned up the command line parameter routine
c                       (commline) and added H for HOFN graph output
c
c        4b   18may07   migrated removal of IMD(qc) bad data flag ('f')
c                         from an obscure AWK script to here. This flag
c                         should have been taken care of earlier, especially
c                         if the data was run through the distance/correlation
c                         algorithm where generation of sub-networks and
c                         removal of stations without sufficient data is
c                         accomplished.
c
c    --------------------------- Official Version USHCNv2 ----------------------
c    ---------------------------------- 21 Dec 2006 ----------------------------
c
c        4a   08jan07   all missing data with UCP "removed flag" are output
c        4    05dec06   added minslpknt to command line parms
c        3i   29nov06   added nindx for additional paired station loop backs
c        3h   03nov06   add MMTS (from USHCN v1) option, fixed input element
c                         error
c        3g   20oct06   added optional region input so perimeter stations
c                         are not output
c        3f   15may06   added ndelete array to the output ("RSTRTv7")
c        3e   17apr06   brought neglected annual timeres back to operational
c        3d   05apr06   reinstated Techinque ON/OFF option for itech
c        3c   08nov05   added confidence interval output
c        3b   11oct05   fix 3 & 3a for re-entry 2 ("RSTRTv6")
c        3    22sep05   Mod for restart journal version "RSTRTv5"
c        2    26jul05   iconfirm = 2
c        2a   30jun05   iconfirm = 3

c     =======================================================================
      subroutine commline()

c     read the command line parameters given as pairs of "-option value"
      integer iargc, narg
      character*132 argv

c     This include file contains the system parameters
      INCLUDE 'inhomog.parm.mthly.incl'
c     command line parameters
      include 'inhomog.comm.mthly.incl'

      character*6 iparm

c     initialize command line parameters
      do it = 1, ntech
        itech(it) = 0
      enddo
      inel = 0
      idebug = 0
      icorr = 0
      icoin = 1
      igra = 0
      irecurse = 0
      iregion = 0
      cmetafmt = 0
      rmetafmt = 0
      ncand = 0
      immts = 0
      iushcn = 0
      qscale = 0.0
      itscale = 0
      ihyear = 0
      ihtag = ''
      ctype = ''
      ntype = ''
      otype = ''
      incand = ''
      incoop = ''
      outcand = ''
      outcoop = ''
      odir = ''
      netfile = ''
      candfile = ''
      reffile = ''
      jrnlfile = ''
      mattdata = ''
      mattmeta = ''
      unique = ''
c     random gives number of generated random series network stations
      irandom = 0
c     incep identifies Peter Thorne's World input
      incep = 0
      minslpknt = 0
      itimeres = -1

c     parameters defined in the confirm-filter study
      print *,''
      print *,' ---- Parameters defined in Confirm-Filter Study ----'
      iconfirm = 2
      print *, ' Confirmation number: ', iconfirm
      nmrgyr = -2
      print *, ' Merge months depend on chgpt amp :', nmrgyr
      itech(1) = 1
      itech(2) = 0
      itech(3) = 0
c     NOTE: the SHAP output (indKW := ntech+1) is always turned ON!
      itech(indKW) = 1

      print *, ' T-test (+KW) Processing always enabled'

c     list of command line parms, in order
c     cC-d-e-F-g-H-j-lL-mM-nN-oO-pP-qQ-rR-sS-tT-u-W

      print *,''
      print *,' ---- Command line input ----'
c     get the number of command line arguments
      iarg = 1
      narg = iargc()

c     first parameter is the number of station-years (MaxSky)
      call getarg(iarg, argv)
      iarg = iarg + 1
      read(argv,fmt='(i7)') maxsky
      print *, ' MaxSky:', maxsky

c     go thru command line arguments - keep the valid ones
c        and ignoring the undefined ones.
      do while(iarg .le. narg)
        call getarg(iarg, argv)
        iarg = iarg + 1
        if(argv .eq. '-n') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          netfile = argv
          print *, ' Cand-Ref Network Input file :',
     *      netfile(1:lnblnk(netfile))
        else if(argv .eq. '-T') then
c          call getarg(iarg,argv)
c          print *,ntech, ':', argv(1:lnblnk(argv)), ':'
          iarg = iarg + 1
c          read(argv,fmt='(3i1)') (itech(i),i=1,ntech)
c          do i = 1,ntech
c            if(itech(i) .eq. 1) then
c              print *, ctech(i), ' processing option enabled'
c              iptech = iptech + 1
c            endif
c          enddo
        else if(argv .eq. '-m') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          mattmeta = argv
          print *,' Matt meta Input file :',mattmeta(1:lnblnk(mattmeta))
         else if(argv .eq. '-r') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          mattdata = argv
          print *,' Matt data Input file :',mattdata(1:lnblnk(mattdata))
          print *, ' Matt data Input file: ', mattdata
c        else if(argv .eq. '-p') then
c          iarg = iarg + 1
c          incep = 1
c          print *, ' Peter World data Input'
        else if(argv .eq. '-j') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          jrnlfile = argv
          print *, ' ----------- RESTART RUN --------------------'
          print *, ' Journal Input file: ', jrnlfile
        else if(argv .eq. '-c') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i4)') ncand
          print *, ' Number of candidate stations in meta: ', ncand
        else if(argv .eq. '-t') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i1)') itimeres
          print *, ' Time resolution (0-ann;1-mthly): ', itimeres
        else if(argv .eq. '-o') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          otype = argv
          print *, ' Processed Stage for Homog Output :',
     *      otype(1:lnblnk(otype))
        else if(argv .eq. '-p') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          ctype = argv
          print *, ' Processed Stage for Candidate Input :',
     *      ctype(1:lnblnk(ctype))
        else if(argv .eq. '-q') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          ntype = argv
          print *, ' Processed Stage for Network Input :',
     *      ntype(1:lnblnk(ntype))
        else if(argv .eq. '-u') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          unique = argv
          print *, ' Unique descriptor for this run/rerun :',
     *      unique(1:lnblnk(unique))
        else if(argv .eq. '-C') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          incand = argv
          ilen = lnblnk(incand)
          if(incand(ilen:ilen).ne.'/') incand(ilen+1:ilen+1)='/'
          print *, ' Base Candidate Input Directory ',
     *      INCAND(1:lnblnk(INCAND))
          call getarg(iarg, argv)
          iarg = iarg + 1
          outcand = argv
          ilen = lnblnk(outcand)
          if(outcand(ilen:ilen).ne.'/') outcand(ilen+1:ilen+1)='/'
          print *, ' Base Candidate Output Directory: ',
     *      OUTCAND(1:lnblnk(OUTCAND))
        else if(argv .eq. '-O') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          odir = argv
          ilen = lnblnk(odir)
          if(odir(ilen:ilen).ne.'/') odir(ilen+1:ilen+1)='/'
          print *, ' Directory for Test or HOFN output files :',
     *      odir(1:lnblnk(odir))
        else if(argv .eq. '-N') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          incoop = argv
          ilen = lnblnk(incoop)
          if(incoop(ilen:ilen).ne.'/') incoop(ilen+1:ilen+1)='/'
          print *, ' Base Coop Input Directory: ',
     *      INCOOP(1:lnblnk(INCOOP))
          call getarg(iarg, argv)
          iarg = iarg + 1
          outcoop = argv
          ilen = lnblnk(outcoop)
          if(outcoop(ilen:ilen).ne.'/') outcoop(ilen+1:ilen+1)='/'
          print *, ' Base Coop Output Directory: ',
     *      outcoop(1:lnblnk(outcoop))
        else if(argv .eq. '-e') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i3)') inel
          if(inel .eq. 4) then
            print *, ' Error: Use v2 only for temperature'
            print *, '        Use v3 for precipitation'
            go to 10
          else if(inel .gt. maxelem .or. inel .lt. 1) then
            print *, ' Element parameter out of range'
            go to 10
          endif
          icelem = celem(inel)
          iuelem = uelem(inel)
          print *, ' Processing Meteorological Element:', inel,
     *      ' : ', icelem, ' : ', iuelem
        else if(argv .eq. '-d') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i1)') idebug
          print *, ' Debug Level:', idebug
        else if(argv .eq. '-H') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i4)') ihyear
          itemp = ihyear
          if(ihyear .lt. 0) then
            itemp = -1 * ihyear
          else if(ihyear .eq. 0) then
            itemp = begyr
          endif
          if(itemp .lt. begyr) then
            print *,' Error: abs(ihyear):',itemp,
     *        ' is less than begyr:', begyr
            goto 10
          endif
          call getarg(iarg,argv)
          iarg = iarg + 1
          ihtag = argv
          print *, ' HOFN Input and Graph Output enabled start:',
     *      ihyear, ' Out DTAG: ', ihtag(1:lnblnk(ihtag))
        else if(argv .eq. '-S') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i2)') minslpknt
          print *, ' Min Slope Seg: ', minslpknt
        else if(argv .eq. '-s') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i3)') ndellim
          print *, ' Suspect ndellim: ', ndellim
        else if(argv .eq. '-R') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i3)') irandom
          print *, ' Random Series Test neighbors: ',
     *      irandom
        else if(argv .eq. '-F') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i6)') firstnet
          print *, ' First Network (by count) set to: ', firstnet
        else if(argv .eq. '-L') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i6)') lastnet
          print *, ' Last Network (by count) set to: ', lastnet
        else if(argv .eq. '-W') then
          irecurse = 1
          print *,' Recursion ENABLED with WMs series as input'
        else if(argv .eq. '-g') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i6)') iregion
          print *,' Output for US Region only: ', iregion
        else if(argv .eq. '-l') then
          netloop = 1
          print *, ' Network Looping ENABLED'
c          print *, ' Reference metafile in GHCN format'
        else if(argv .eq. '-M') then
          immts = 1
          print *, ' USHCN v1 MMTS adjustment enabled'
        else if(argv .eq. '-U') then
          iushcn = 1
          print *, ' Old USHCN (i.e. Old Normals) Input/output enabled'
        else if(argv .eq. '-x') then
          itscale = 1
          print *, ' Input data scaled in tenths'
        else if(argv .eq. '-P') then
          icoin = 0
          print *, ' Post Threshold test ENABLED'
        else if(argv .eq. '-Q') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(f5.2)') qscale
          print *, ' Inner-quartile filter scale: ', qscale
        else
          print *, ' Unknown argument: ', argv, ': skipping'
        endif
      enddo

      print *,''
      print *,'  --------- Peter"s World (Switchboard) ----------'

c     Initialize variables in Statcomm common to default
      ndist = 100
      nsnet = 40
      nstns = 40
      minstns = 7
      isnht = 5
      iconfirm = 2
      ishfmeta = 1
      iadjout = 1
      iadjwin = 0
      insloop = 1
      insopp = 0
      minsta = 2
      mypct = 92
      minlen = 18
      corr_type = '1diff'
      bic_type = 'bic'
      adjest = 'med'
      adjfilt = 'conf'

      call getchkenv('NEIGH_CLOSE ', iparm)
      read(iparm, '(i3)') ndist
      print *,' Number of closest neighbors input: ', ndist

      call getchkenv('NEIGH_FINAL ', iparm)
      read(iparm, '(i3)') nsnet
      if(nsnet .gt. maxnstns) then
        print *,' Reset PW neighbors to Incoming Network ', nsnet,
     *    ' to ', maxnstns
        nsnet = maxnstns
      else
        print *, ' Number of neighbor stations output: ', nsnet
      endif
c     increase maximum number of paired stations from the PW combo input
c      for nstns - add a buffer ~ 1/2 size of nstns (max == maxnstns)
      nstns = nsnet + nsnet/2
      if(nstns .gt. maxnstns) nstns = maxnstns
      print *,' nstns buffer limit set to: ', nstns

      call getchkenv('NEIGH_CORR  ', iparm)
      corr_type = iparm(1:5)
      print *, ' Correlation Type: ', corr_type

      call getchkenv('MIN_STNS    ', iparm)
      read(iparm, '(i3)') minstns
      print *, ' Minimum desired neighbors per year-mth: ', minstns

      call getchkenv('SNHT_THRES  ', iparm)
      read(iparm, '(i3)') isnht
      print *, ' SNHT significance threshold: ', isnht

      call getchkenv('BIC_PENALTY ', iparm)
      bic_type = iparm(1:4)
      print *, ' Bayesian penalty function: ', bic_type

      call getchkenv('CONFIRM     ', iparm)
      read(iparm, '(i3)') iconfirm
      print *, ' CONFIRM chgpt hit threshold: ', iconfirm

      call getchkenv('SHF_META    ', iparm)
      read(iparm, '(i3)') ishfmeta
      print *, ' Toggle to use SHF metadata: ', ishfmeta

      call getchkenv('ADJ_EST     ', iparm)
      adjest = iparm(1:3)
      print *, ' Method to estimate chgpt adjustment: ', adjest

      call getchkenv('ADJ_FILTER  ', iparm)
      adjfilt = iparm(1:4)
      print *, ' Method for estimate filter: ', adjfilt

      call getchkenv('ADJ_OUTLIER ', iparm)
      read(iparm, '(i3)') iadjout
      print *, ' Toggle to keep/remove chgpt outliers: ', iadjout

      call getchkenv('ADJ_WINDOW  ', iparm)
      read(iparm, '(i3)') iadjwin
      print *, ' Months to avg for adj est(0=no limit): ', iadjwin

      call getchkenv('NS_LOOP     ', iparm)
      read(iparm, '(i3)') insloop
      print *, ' Toggle to remove Non-sig segments: ', insloop

      call getchkenv('ADJ_MINLEN  ', iparm)
      read(iparm, '(i3)') minlen
      print *, ' Minimum data values in segment to est adj: ',minlen

      call getchkenv('ADJ_MINPAIR ', iparm)
      read(iparm, '(i3)') minsta
      print *, ' Minimum station pairs to est adjustment: ',minsta

      call getchkenv('AMPLOC_PCT  ', iparm)
      if( iparm .eq. '90' ) then
        mypct = 1
      else if ( iparm .eq. '92' ) then
        mypct = 2
      else
        mypct = 3
      endif
      print *, 'Amp vs Loc percent inclusion & index: ', iparm, mypct

      print *,''
      print *,'------------- Command line option Defaults ------------'

      if(iregion .eq. 0) then
        print *,' No Region specified - All stations will be output'
      endif

      if(ncand .eq. 0) then
        print *,' NOTE: NO CANDIDATE STATIONS - ALL COOP DATA'
      endif

      if(minslpknt .eq. 0) then
        minslpknt = minlen
        print *,' Minslpknt default (minlen):', minslpknt
      endif

      if(icorr .eq. 0) then
        print *,' All pairs equally weighted in adj. est.'
      endif

      if(icoin .eq. 1) then
        print *,' Co-incident Threshold testing ENABLED'
      endif

      if(immts .eq. 0) then
        print *,' USHCN v1 MMTS not enabled'
      endif

      if(itscale .eq. 0) then
        print *,' Default scaling is hundreths'
        ascale = scale(inel)
      else
c       Scaling is tenths
        ascale = tscale(inel)
      endif

      if(netfile.ne.'' .and. incand .ne. '' .and. itimeres .ge. 0 .and.
c     *   (unique .ne. '' .or. jrnlfile .ne. '') .and.
     *   outcand .ne. '' .and. outcoop .ne. '' .and.
     *   incoop.ne. '' .and. ctype.ne.'' .and. odir.ne.''.and.
     *   otype.ne.'' .and. ntype.ne.'' .and. qscale .ne. 0. .and.
     *   ndellim .ge. 0 .and.
     *   (itech(1).ne.0.or.itech(2).ne.0.or.itech(3).ne.0)) goto 11

   10 print *,' Apply Inhomogeneity techniques to the Monthly Data'
      print *,'     TEMPERATURE ONLY'
      print *,'   REQUIRED PARAMETERS'
      print *,'     -T            Techniques to use (TPR0,TPR2,TPR1)'
      print *,'     -l            Toggle network looping option ON'
      print *,'                   Default is CLASSIC no looping'
      print *,'     -c ncand      Num USHCN Stations in netfile'
      print *,'     -t itimeres   Time resolution (0-ann;1-mthly)'
      print *,'     -Q qscale    Scale for inner-quartile filter'
      print *,'                   = 1.46 (version 52d)'
      print *,'     -s ndellim    Ndelete threshold for suspect data'
      print *,'     -g reg ne=1,se=2,ct=3,nc=4,so=5,np=6,sw=7,nw=8,we=9'
      print *,'    Input Files ---'
      print *,'     -n netfile    Cand-Ref Network Stations file'
      print *,'     -o otype      Output process level'
      print *,'     -p ctype      Candidate Input process level'
      print *,'     -q ntype      Network Input process level'
      print *,'     -e elem       Met elem(1=max,2=min,3=avg)'
      print *,'    Input Directories ---'
      print *,'     -C incand outcand  I/O Candidate Data Directories'
      print *,'     -N incoop outcoop  I/O Coop Data Directories'
      print *,'If neither unique nor journal entered - RESTART UNAVAIL'
      print *,'     -u unique     descriptor for this run/rerun'
      print *,'     -j journal   Input the journal file and restart'
      print *,'   MISC PARAMETERS'
      print *,'     -U           Old USHCN format input'
      print *,'     -x           Temperature scale in .1 (default .01)'
      print *,'     -d           Debug level'
      print *,'     -S minslpknt Min seg length for sloped model'
      print *,'     -m metafile  Matt meta Input file'
      print *,'     -r mattdata  Matt data Input file'
      print *,'     -p           Peter Input data'
      print *,'     -M           MMTS v1 Adjustment (Off is default)'
      print *,'     -m metafile  Matt meta Input file'
      print *,'     -H ihyear ihtag Generate output for HOFN Graphs'
      print *,'                    ihyear is the first year to output'
      print *,'                    if <= 0, used HCN Normal input'
      print *,'                    if > 0, use HOFN 3220 input'
      print *,'                    ihtag is the datetag for files'
      print *,'     -O odir      Directory for Test&Graph output files'
      print *,'     -R nneigh    Random test (with #neigh) Mmenne fmt'
      print *,'     -P           Toggle Post/Coincident Threshold test'
      print *,'     -W           Recursion ENABLED w/ WMs series input'
      print *,'     -B begstn    first station number to process ',
     *  '(default - first in file)'
      print *,'     -E endstn    last station number to proces ',
     *  '(default - last in file)'
      print *,'     -F firstnet    first network to process ',
     *  '(default - first in file)'
      print *,'     -L lastnet    last network to proces ',
     *  '(default - last in file)'
      print *,' NOTE: Raw data files assumed to be parsed into '
      print *,'        sudirectories by state (01,02...,50)'
      print *,''
      print *,' ------------ What is needed -------------'
      if(netfile.eq.'') print *,' Netfile missing'
      if(incand .eq. '') print *,' Input Candir missing'
      if(outcand .eq. '') print *,' Output Candir missing'
      if(itimeres .lt. 0) print *,' Timeres negative'
c      if(unique .eq. '' .and. jrnlfile .eq. '')
c     *  print *,' Define Unique OR Journal'
      if(incoop.eq. '') print *,' Input Coopdir missing'
      if(outcoop.eq. '') print *,' Output Coopdir missing'
      if(ctype.eq.'') print *,' Ctype missing'
      if(odir.eq.'') print *,' Odir missing'
      if(otype.eq.'') print *,' Otype missing'
      if(ntype.eq.'') print *,' Ntype missing'
      if(qscale .eq. 0.) print *,' Qscale ne zero'
      if(itscale .eq. 0) print *,' Input data scaled to hundreths'
      if(ndellim .eq. 0.) print *,' Ndellim ne zero'
      if(itech(1).eq.0.and.itech(2).eq.0.and.itech(3).eq.0)
     *  print *,' At least one technique must be defined'

      stop

   11 continue

      return
      end

c     =======================================================================
      subroutine getchkenv(chrstr, iparm)
      character*12 chrstr
      character*6 iparm

      call getenv(chrstr(1:lnblnk(chrstr)), iparm)
      if(iparm .eq. '') then
        print *,'ENV: ',chrstr(1:lnblnk(chrstr)),' undefined'
        stop 555
      endif
      return
      end

c     =======================================================================
      subroutine openunits(version)
      INCLUDE 'inhomog.comm.mthly.incl'

      character*132 version
      character*132 outfile

c     Open the output files to accumulate the data for each station
c       for all the iterations
      do intech = 1, ntech + 1
        if(itech(intech) .eq. 1) then
          outfile = odir(1:lnblnk(odir)) // version(1:lnblnk(version))
     *      // '.' // c2tech(intech)
          iounit = bunit + intech
          open(iounit,file=outfile,status='unknown')
        endif
      enddo

      return
      end

c     =======================================================================
      subroutine closeunits()
      INCLUDE 'inhomog.comm.mthly.incl'

c     Close all the debug output files that accumulate data for each
c       station for all the iterations
      do intech = 1, ntech + 1
        if(itech(intech) .eq. 1) then
          iounit = bunit + intech
          close(iounit)
        endif
      enddo

      return
      end

c     =======================================================================
      subroutine readnet(mmunit, nnunit,idunit,rnunit,intr,ieof,
     *  ntstn)

c     WARNING: use only with inhomog.parm.incl & inhomog.comm.mthly.incl
c         AFTER 10Feb04

c     version 3 extends the 10 subnet limit for MM's simulations
c                                                     26 Apr 04 cw
c
c     readnet version 2 is implemented from octpart.v3.f and newer.
c       BE AWARE: ONLY USE SKILL SCORE ROUTINES VERSION 5 AND ABOVE --- as of
c         this version the pseudo station numbers for testing now begin with 830000
c         this removes the error of the **** in the last station, but also
c         offsets the station number by 10 for the older skill score programs!
c
c       The connection of the network-station read being one is removed
c       The target (candidate) station is the first subnet (neighbor) station
c       All of the other stations in the subnet must be a candidate in their
c         own subnet for the network to be closed (or circular) otherwise
c         the code won't work properly.                26sep03 cw
c
c     readnet reads in the candidate and its network of stations
c       nnunit is the unit number of the network definition file
c       idunit is the unit number for the data files
c       orig is the data array in stn,year,mth index order
c       ieof returns whether the end of file has been reached in the
c          network definition file (=1) or not (=0)

c     cstn is the desired station id
c     ista is the station's index

c     Reference restart.mod.f95 module for work array allocation
      use restart

c     This include file contains the system parameters
      INCLUDE 'inhomog.parm.mthly.incl'

c     command line parameters
      include 'inhomog.comm.mthly.incl'
      include 'inhomog.restart.mthly.incl'

      parameter(nmth = 13)
      parameter(iMaxVal = 100)

c     Indices of all the stations in each neighborhood
      integer indx(maxnstns)
c     Need to run, initially contains zeros and only when the candidate station
c       with that index is read in, does it change to 1
      integer intr(maxstns)
c     for annual computations
      integer num(maxstns),rnunit
      real sum(maxstns)

      integer indata(13), imiss /-9999/

c     station id and lat/lon/elev
      character*11 ntstn(maxstns),instn
      dimension rStnInfo(maxstns,3)

c     temporary 3220 input arrays
      integer iMonth(iMaxVal),iDay(iMaxVal),iValue(iMaxVal)
      character*3 cRecType
      character*4 cElmType

c      character*132 nfname, hcndir
      character*132 nfname
      character*3 inflag(13)
      character*1 ichar(21) /'0', '1', '2', '3', '4', '5', '6', '7',
     * '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'/

c     MM's missing value -999.999
      real ammis /-999./

c     variables for text output construction due to G77 inadequacies
      character*132 outstr, tmpstr, curcand
      character*4 curtyp

c     imxskymo is the index in the skyline matrix for filling the data
c       tested against maxskymo command line input for array size
      integer imxskymo /1/

      if(iushcn .eq. 0) then
        istnlen = 11
      else
        istnlen = 6
      endif

c     set internal = station-year-months
      maxskymo = maxsky * 12

c     initialize station ID array & original data array
      ntstn = ''
      orig = amiss

C     initialize incoming temperature flag array
      tflg = '   '

c     initialize need-to-run array for all subnetworks
      intr = 0

      ieof = 0

c     For Normals or HOFN type input, read in candidate & network indices
      if(ihyear .gt. 0 .or. irandom .eq. 0) then
c       read in all subnetworks from the correlation output
        itarg = 0
        print *,' nsnet: ', nsnet
        do while (1 .eq. 1)
          if(iushcn .eq. 0) then
            read(nnunit, '(a11)', end=80) instn
          else
            read(nnunit, '(a6)', end=80) instn
          endif
c          print *, 'instn ', instn
          read(nnunit, *, end = 80) (indx(i),i=1,nsnet)
          read(nnunit, *, end = 80) adum
          itarg = itarg + 1
          k = indx(1)
          if(k .gt. maxstns) then
            print *,' Number input stations > maxstns ',maxstns
            stop
          endif
          ntstn(k) = instn
          do i = 1,nsnet
            nindx(k,i) = indx(i)
          enddo
        enddo

   80   ieof = 1
c       if no subnets have been read, end it here....
        if(itarg .eq. 0) goto 200

c       total number of stations (& subnets) == last index
        numsubs = indx(1)
      endif

c     ---------------------- HOFN data input ------------------------
      if(ihyear .gt. 0) then
        print *,'##### WARNING: NOT TESTED FOR SKYLINE MOD ########'
        stop 999

c        print *,' Begyr: ',Begyr,' Endyr: ',Endyr,' IHyear: ',ihyear
c        print *,' icelem: ', icelem
c       read 3220 datafiles
        do iStn = 1, numsubs

          nfname = incand(1:lnblnk(incand)) //  ntstn(istn)(1:istnlen)
     *      // '.3220'
          open(idunit,FILE=nfname,status='old',err=92)

c         initialize station info
          intr(iStn) = 1
          ntot = 0
c         First and last year (for skyear)
          ibyear = 9999
          ieyear = 0
c         set the skyind of the first station datum (monthly)
          skyind(iStn) = imxskymo

          do while (1 .eq. 1)
c           incoming data in TD3220 format (originally for HOFN)
            read(idunit,1500,end=95) cRecType,dstn,cElmType,iYear,
     &        iTotlVal,(iMonth(iNVal),iDay(iNVal),iValue(iNVal),
     &        iNVal=1,iTotlVal)
 1500       format (a3,i6,2x,a4,2x,i4,6x,i3,12(2i2,i6,2x))

            if(cRecType .ne. 'MLY') then
              print *, nfname, ' is NOT monthly data'
              goto 95
            endif

            if(iYear .ge. BegYr .and. iYear .le. EndYr ) then
c             Keep track of begin/end years
              if(ibyear .eq. 9999) ibyear = iYear
              ieyear = iYear
              imxskymo = skyind(iStn) + (ieyear - ibyear) * 12
c             check the skyind - (room for another year of data?)
              if(imxskymo+11 .gt. maxskymo) then
                print *,'Incoming data exceeds Skyline Maximum:',
     *            istn, skyind(istn), maxskymo
                stop
              endif

              if((cElmType .eq. 'MMXT' .and. icelem .eq. '_max' ) .or.
     *          (cElmType .eq. 'MMNT' .and. icelem .eq. '_min')) then

                do iNVal=1,iTotlVal
                  iskymo = imxskymo + iMonth(iNVal) - 1
                  orig(iskymo) = iValue(iNVal)/ascale
                end do

              endif !End condition "TMAX or TMIN"
            endif !End condition "Year"
   90     end do

   92     call perror(' Cannot open HOFN(3200) data: ' // nfname)

c   95     print *, ntstn(istn), ntot
   95     close(idunit)
          if(ibyear .ne. 9999) then
            skyear(1, iStn) = ibyear
            skyear(2, iStn) = ieyear
          else
            skyear(1, iStn) = imiss
            skyear(2, iStn) = imiss
          endif
c         adjust working sky index for next station
          imxskymo = skyind(iStn) + (ieyear - ibyear + 1) * 12

        end do

  100   numsubs = iStn - 1
        print *, 'Number of stations read: ', numsubs

c     ---------------- Normals & Pthorne data format -----------------
      else if(irandom .eq. 0) then

c       for candidate & each reference station read in its data
c       changed for the GHCNMv2 station and GHCN-daily filename formats
        DO iStn = 1,numsubs
          itarg = nindx(iStn,1)
          if(iStn .le. ncand) then
            curtyp = ctype(1:lnblnk(ctype))
            curcand = incand
          else
            curtyp = ntype(1:lnblnk(ntype))
            curcand = incoop
          endif
          if(iushcn .eq. 0) then
            if(irecurse .eq. 0) then
              nfname = curcand(1:lnblnk(curcand)) //
     *          curtyp(1:lnblnk(curtyp)) // '/' //
     *          ntstn(itarg)(1:istnlen) // '.' //
     *          curtyp(1:lnblnk(curtyp)) // '.' // icelem
            else
              nfname = curcand(1:lnblnk(curcand)) // 'WMs.' //
     *          curtyp(1:lnblnk(curtyp)) // '/' //
     *          ntstn(itarg)(1:istnlen) // '.WMs.' //
     *          curtyp(1:lnblnk(curtyp)) // '.' // icelem
            endif
          else
            if(irecurse .eq. 0) then
              nfname = curcand(1:lnblnk(curcand)) //
     *          curtyp(1:lnblnk(curtyp)) // '/' //
     *          ntstn(itarg)(1:istnlen) // '_' // iuelem // '.' //
     *          curtyp(1:lnblnk(curtyp))
            else
              nfname = curcand(1:lnblnk(curcand)) // 'WMs.' //
     *          curtyp(1:lnblnk(curtyp)) // '/' //
     *          ntstn(itarg)(1:istnlen) // '_' // iuelem
     *          // '.WMs.' // curtyp(1:lnblnk(curtyp))
            endif
          endif
          if(idebug .ge. 2) print *,nfname

c         Open temporary i/o unit for data input
          open(idunit, FILE = nfname, err = 130)

c         initialize station info
          intr(iStn) = 1
          itot = 0
c         First and last year (for skyear)
          ibyear = 9999
          ieyear = 0
c         set the skyind of the first station datum (monthly)
          skyind(iStn) = imxskymo

          do while (1 .eq. 1)
            if(iushcn .eq. 0) then
              read(idunit, '(a11,i1,i4,12(i6,a3))', end = 140) instn,
     *          isrc, iyear,(indata(imth),inflag(imth),imth=1,12)
            else
              read(idunit, '(a6,i1,i4,12(i6,a1))', end = 140) instn,
     *          isrc, iyear,(indata(imth),inflag(imth),imth=1,12)
c              write(6, '(a6,i1,i4,12(i6,a1))') instn,
c     *          isrc, iyear,(indata(imth),inflag(imth),imth=1,12)
            endif
            if(iyear .ge. begyr .and. iyear .le. endyr) then
c             Keep track of begin/end years
              if(ibyear .eq. 9999) ibyear = iYear
              ieyear = iYear
              imxskymo = skyind(iStn) + (ieyear - ibyear) * 12
c             check the skyind
              if(imxskymo+11 .gt. maxskymo) then
                print *,'Incoming data exceeds Skyline Maximum:',
     *            istn, skyind(istn), maxskymo
                stop
              endif

              nval = 0
              do imth = 1, 12
                iskymo = imxskymo + imth - 1

c               convert external format and missing to internal values
c                   filter out the NEW Integrated Monthly Dataset (after QC)
                if(indata(imth) .eq. IMISS) then
                  orig(iskymo) = AMISS
                  inflag(imth)(1:2) = ' M'
                else if (iushcn .eq. 0 .and.
     *            inflag(imth)(2:2) .ne. " ")then
c                 -------  GHCN Input & flag decoding
                  orig(iskymo) = AMISS
                  inflag(imth)(1:2) = ' M'
                else if (iushcn .eq. 1 .and.
     *            inflag(imth)(1:1).eq."f") then
c                 -------  GHCN Input & flag decoding
                  orig(iskymo) = AMISS
                  inflag(imth)(1:2) = 'fM'
                else
                  orig(iskymo) = indata(imth)/ascale
                  nval = nval + 1
                endif
                tflg(iskymo) = inflag(imth)
              end do
              itot = itot + nval
            end if
          end do
  130     call perror(' Cannot open Normals data: ' // nfname)
c         make sure to close this unit!!!
  140     close(idunit)
          if(idebug .ge. 2)
     *      print *, istn, skyind(istn), ibyear, ieyear, imxskymo

c          print *,' Indata: ',instn, itot
          if(ibyear .ne. 9999) then
            skyear(1, iStn) = ibyear
            skyear(2, iStn) = ieyear
c           adjust working sky index for next station
            imxskymo = skyind(iStn) + (ieyear - ibyear + 1) * 12
          else
            skyear(1, iStn) = imiss
            skyear(2, iStn) = imiss
c           if no data AT ALL - leave a year worth of space......
            imxskymo = imxskymo + 12
          endif

  150   end do

      else
        numsubs = maxstns

c       read Mmenne random series data
  160   if(irandom .gt. 0) then
c         Annual series are positive ****DISABLED****
c         warning: assume 100 records per candidate-network
          print *,' Annual resolution is disabled'
          stop
        else
c         monthly series (irandom is negative)
c         warning: assume 1200 values per candidate-network
          itotmo = numyr * 12
          do iskyoff = 1, itotmo
            read(rnunit,*,end=180,err=200)
     *        imoff,(orig((iStn-1)*itotmo+iskyoff),iStn=1,numsubs)
            if(iskyoff .ne. imoff) then
              print *,'Monthly input and YYYY/MM out of sync'
              stop
            endif
c           convert MM's missing to internal missing
            do iskymo = 1, numsubs*itotmo
              if(orig(iskymo).le.ammis)then
                orig(iskymo)=amiss
              endif
            enddo
          enddo
        endif

c       read the stations and their nieghbors (1st diff corr sort)
  180   do istn = 1, numsubs
          intr(istn) = 1
c         set the skyind and skyear
          skyind(iStn) = (iStn-1) * itotmo + 1
          skyear(1,iStn) = begyr
          skyear(2,iStn) = endyr

          if(iushcn .eq. 0) then
            read(nnunit, '(a11)', end=190) ntstn(istn)
          else
            read(nnunit, '(a6)', end=190) ntstn(istn)
          endif
          read(nnunit, *, end = 190) (indx(i),i=1,nsnet)
          read(nnunit, *,end=190) adum
          k = indx(1)
          do i = 1,nsnet
            nindx(k,i) = indx(i)
          enddo
        enddo

c       test station number against begin and end stations
c        read(ntstn(1), fmt='(i6)') istn
c        if(istn .lt. begstn) goto 160
c        if(istn .gt. endstn) goto 190

      endif

c      print *,'Last station (subnet) index:',numsubs
c      print *,' readnet orig(49,1950,1):',orig(49,1950,1)
      return

  190 print *,' Candidate network past end station'
      ieof = 1
      return

  200 print *,' End of candidate network records'
      ieof = 1
      return

      end

c     =======================================================================
      subroutine writsta(itarg,cstn,dval,aval,cval,dflag,aflag,otag,
     *  iounit)

c     common output routine for the Inhomogeneity techniques
c     cstn   - input - candidate (base) station
c     dval   - input - adjusted series data
c     dflag  - input - adjusted series flags
c     aval   - input - adjustments used for series
c     cval   - input - error of adj for series
c     aflag  - input - flags for which segment series is adjusted
c     otag  - input - two letter identifier of Inhomo Tech (LV,EP,MD)
c     iounit - input - output device number

c     pulled from the stnhist subroutine in ucpmonthly.v5
c     fix up to write out results....
c     current data array for all stations
c      real   montemp(maxstns,begyr:endyr,13)
c     original input flags for all stations
c      character*1 monflag(maxstns,begyr:endyr,13)
c
c      character*6 ntstn(maxstns),shstn(maxnstns)
c      character*1 adjflag(begyr:endyr,13)
c      real outtemp(begyr:endyr,13),adjtemp(begyr:endyr,13)
c
c            do iy = begyr, endyr
c              do im = 1, 13
c                adjflag(iy,im) = ' '
c                adjtemp(iy,im) = amiss
c                if(iy .ge. istpyr .and.
c     *            montemp(itarg, iy, 13) .ne. amiss) then
c                  outtemp(iy,im) = montemp(itarg, iy, im)
c                  write(iounit,*) ' Cand: ', itarg, ntstn(itarg), iy, im,
c     *              outtemp(iy,im)
c                else
c                 outtemp(iy,im) = amiss
c                endif
c              enddo
c            enddo
c
c          call writsta(shstn(1), outtemp, monflag, adjtemp, adjflag,
c     *      'KW', idunit)

      include 'inhomog.comm.mthly.incl'
      include 'inhomog.parm.mthly.incl'
c      include 'inhomog.restart.mthly.incl'
      parameter (NMTH = 13)
      character*11 cstn
      character*8 ostr
      character*2 otag
      character*3 dflag(begyr:endyr,NMTH), aflag(begyr:endyr,NMTH)
      character*1 qflag
      real dval(begyr:endyr,NMTH), aval(begyr:endyr,NMTH),
     *  cval(begyr:endyr,NMTH)
      character*132 dfile, dpath, strpath
      integer idval(NMTH), iprnt(begyr:endyr)

      if(iushcn .eq. 0) then
        istnlen = 11
      else
        istnlen = 6
      endif

c     data file first
      if(itarg .le. ncand) then
        dpath = outcand(1:lnblnk(outcand))
        strpath = 'HCNOutDir: '
      else
        dpath = outcoop(1:lnblnk(outcoop))
        strpath = 'CoopOutDir: '
      endif
      ostr = otag // 's.' // otype(1:lnblnk(otype))
      if(iushcn .eq. 0) then
        dfile = dpath(1:lnblnk(dpath))// ostr(1:lnblnk(ostr)) //
     *    '/' //cstn(1:istnlen) // '.' // ostr(1:lnblnk(ostr)) //
     *    '.' // icelem
        strpath = strpath(1:lnblnk(strpath)) // ostr(1:lnblnk(ostr)) //
     *    '/' // cstn(1:istnlen) // '.' // ostr(1:lnblnk(ostr)) //
     *    '.' // icelem
      else
        dfile = dpath(1:lnblnk(dpath))// ostr(1:lnblnk(ostr)) //
     *    '/' // cstn(1:istnlen) // '_' // iuelem //
     *    '.' // ostr(1:lnblnk(ostr))
        strpath = strpath(1:lnblnk(strpath)) // ostr(1:lnblnk(ostr)) //
     *    '/' // cstn(1:istnlen) // '_' // iuelem //
     *    '.' // ostr(1:lnblnk(ostr))
      endif

c     open output file
      open(iounit, FILE=dfile, err= 300)

c     HOLD !!!!!!!!!!!!!!!!!!!!!!
c     write the data in HCN format
c      do iy = 1, endyr - begyr + 1
c        write(iounit,20,ERR=300) istn, iy + begyr - 1, inel,
c     *    (dval(im,iy),dflag(iy,im),im = 1,NMTH)
c   20   FORMAT(I6.6,1X,I4,1X,I1,'F',13(F6.2,1x,a1,2x))
c      end do

c     write the data in Normals format
      ishort = 0
      idelete = 0
      iunstbl = 0
      ichgptm = 0
      iunknown = 0
      do iy = begyr, endyr
        ndat = 0
        iprnt(iy) = 0
        do im = 1, NMTH
          qflag = dflag(iy,im)(2:2)
          if(qflag .ne. ' ') then
c           The PHA has deleted the data value - standardize qflag
            if(qflag .eq. 'a') then
              ishort = ishort + 1
            else if(qflag .eq. 'd') then
              idelete = idelete + 1
            else if(qflag .eq. 'g') then
              iunstbl = iunstbl + 1
            else if(qflag .eq. 'r') then
              ichgptm = ichgptm + 1
            else
              iunknown = iunknown + 1
            endif
            dflag(iy,im)(1:1) = ' '
            idval(im) = -9999
            ndat = ndat + 1
          else if(dval(iy,im) .lt. AMISS + 1.0 ) then
            idval(im) = -9999
            dflag(iy,im) = '   '
          else
            idval(im) = nint(dval(iy,im)*ascale)
            ndat = ndat + 1
          endif
        enddo
        if(ndat .gt. 0) then
          if(iushcn .eq. 0) then
            write(iounit,1000,ERR=300) cstn, inel, iy,
     *        (idval(im),dflag(iy,im), im=1,NMTH)
 1000       FORMAT(a11,i1,I4,13(i6,a3))
          else
            write(iounit,1001,ERR=300) cstn, inel, iy,
     *        (idval(im),dflag(iy,im)(2:2), im=1,NMTH)
 1001       FORMAT(a6,i1,I4,13(i6,a1))
          endif
          iprnt(iy) = 1
        endif
      enddo
      close(iounit)
      write(*,'(" Writing: ",a,5i4)') strpath(1:lnblnk(strpath)),
     *  ishort, idelete, iunstbl, ichgptm, iunknown

c     skip printing out addtional info
      goto 100

c     adjustment file next
      ostr = otag // 'a.' // otype(1:lnblnk(otype))
      dfile = dpath(1:lnblnk(dpath))// ostr(1:lnblnk(ostr)) //
     *    '/' // cstn // '.' // ostr(1:lnblnk(ostr)) // '.' // icelem
c     open output file
      open(iounit, FILE=dfile, err= 300)

c     HOLD !!!!!!!!!!!!!!!!!!!!!!
c     write the data in HCN format
c      do iy = 1, endyr - begyr + 1
c        write(iounit,20,ERR=300) istn, iy + begyr - 1, inel,
c     *    (dval(im,iy),dflag(iy,im),im = 1,NMTH)
c   20   FORMAT(I6.6,1X,I4,1X,I1,'F',13(F6.2,1x,a1,2x))
c      end do

c     write the data in Normals format
      do iy = begyr, endyr
        if(iprnt(iy) .eq. 1) then
          do im = 1, NMTH
            if(aval(iy,im) .lt. AMISS + 1.0 ) then
              idval(im) = -9999
            else
              idval(im) = nint(aval(iy,im)*ascale)
            endif
          enddo
          write(iounit,1000,ERR=300) cstn, inel, iy,
     *      (idval(im),aflag(iy,im), im=1,NMTH)
        endif
      enddo
      close(iounit)

c     err of adj file next
      ostr = otag // 'c.' // otype(1:lnblnk(otype))
      dfile = dpath(1:lnblnk(dpath))// ostr(1:lnblnk(ostr)) //
     *    '/' // cstn // '.' // ostr(1:lnblnk(ostr)) // '.' // icelem
c     open output file
      open(iounit, FILE=dfile, err= 300)

c     write the data in Normals format
      do iy = begyr, endyr
        if(iprnt(iy) .eq. 1) then
          do im = 1, NMTH
            if(cval(iy,im) .lt. AMISS + 1.0 ) then
              idval(im) = -9999
            else
              idval(im) = nint(cval(iy,im)*ascale)
            endif
          enddo
          write(iounit,1000,ERR=300) cstn, inel, iy,
     *      (idval(im),aflag(iy,im), im=1,NMTH)
        endif
      enddo
  100 close(iounit)
      return

  200 print *,' Perimeter station not output: ', cstn
      return

  300 call perror(' ERROR: writing output: ' // dfile)
      stop

      end

c     =======================================================================
      subroutine write_restart(icpos)

c     From restart.mod.f95 module for allocation - 09apr2009 cw
      use restart

      INCLUDE 'inhomog.parm.mthly.incl'
      include 'inhomog.comm.mthly.incl'
      include 'inhomog.restart.mthly.incl'

c     Current restart read-write compatible with resver = 'RSTRTv7 '
c                                                        15may06

c     icpos - indicates the position in the method/loop calling write_restart
c        1 = Beginning of the method/loop
c        2 = Just before the confirmfilt call
c        3 = Just before the Final stnhist call
      integer icpos
c     on a restart is equivalent to rentry

c     take out inhomog.comm.mthly.incl as soon as possible
c      integer nloop

      character*132 rsfile

c      character*64 unique

      write(rsfile,1000) unique(1:lnblnk(unique)), network, method,
     *  nloop, icpos
 1000 format(a,'.',i4.4,'_',i1,'_',i2.2,'_',i1)

c     In the following code, the currently commented lines are the
c       g77 implementation and the enabled lines are the LF95 implementation
c      open(40,file=rsfile,access='direct',recl=1,status='unknown')
      open(40,file=rsfile,FORM='UNFORMATTED',status='unknown')
c     resver is 8 bytes long
c      write(40, rec=1) resver
      write(40) resver
c     these are the variables that define the array sizes (checked on read)
c      write(40, rec=9) maxstns, nstns, begyr, endyr, ntech, ninh
      write(40) maxstns, nstns, begyr, endyr, ntech, ninh
c     these are the settings for the process variables and the arrays
c      write(40,rec=33) network, method, nloop, icpos, nht, lht, ntr,
c     *    itimeres, nhits, sahist, temp, schgpt, nchgpt, ntest, nfound,
c     *    nspan, ndelete, nindx, tflg
      write(40) network, method, nloop, icpos, nht, lht, ntr,
     *    itimeres, nhits, sahist, temp, schgpt, nchgpt, ntest, nfound,
     *    nspan, ndelete, nindx, tflg
      close(40)
      print *, ' JOURNAL WRITTEN: ', rsfile(1:lnblnk(rsfile))
      return
      end

c     =======================================================================
      subroutine read_restart(rsnet)

c     From restart.mod.f95 module for allocation - 09apr2009 cw
      use restart

c      subroutine read_restart(jrnlfile, rsnet, nloop, unique)
      INCLUDE 'inhomog.parm.mthly.incl'
      INCLUDE 'inhomog.comm.mthly.incl'
      include 'inhomog.restart.mthly.incl'

c     these are read from the restart journal and checked against the
c       values set in the current code

c     need network (output) seperated from rsnet (input)
      integer rsnet
      character*8 inver
      integer inmax, instns, inbeg, inend, intech, ininh

c     take out inhomog.comm.mthly.incl as soon as possible
c      integer nloop

      print *,' Unique identifier from Journal: ',
     *  jrnlfile(1:lnblnk(jrnlfile)-12)

c     In the following code, the currently commented lines are the
c       g77 implementation and the enabled lines are the LF95 implementation
c      open(40,file=jrnlfile,access='direct',recl=1,status='unknown')
      open(40,file=jrnlfile,FORM='UNFORMATTED',status='unknown')
c     Journal version is 8 bytes long
c      read(40,rec=1) inver
      read(40) inver
      if(inver .ne. resver) then
        print *,' ------------- Restart Error 1 ---------- '
        print *,' Journal vs. Current Version Mismatch'
        print *,' Journal: ',inver
        print *,' Current: ', resver
        stop
      endif
c     these are the variables that define the array sizes (checked on read)
c      read(40, rec=9) inmax, instns, inbeg, inend, intech, ininh
      read(40) inmax, instns, inbeg, inend, intech, ininh
      if(inmax .ne. maxstns .or. instns .ne. nstns .or.
     *  inbeg .ne. begyr .or. inend .ne. endyr .or.
     *  intech .ne. ntech .or. ininh .ne. ninh) then
        print *,' ------------- Restart Error 2 ---------- '
        print *,' Journal vs. Current Parameter Mismatch'
        print *,'            maxstn, nstns, begyr, endyr'
        print *,' Journal: ', inmax, instns, inbeg, inend, intech, ininh
        print *,' Current: ', maxstns, nstns, begyr, endyr, ntech, ninh
        stop
      endif
c     these are the settings for the process variables and the arrays
c      read(40,rec=33,err=100) network, method, nloop, rentry, nht, lht,
c     *     ntr, itimeres, nhits, sahist, temp, schgpt, nchgpt, ntest,
c     *     nfound, nspan, ndelete, nindx, tflg
      read(40) network, method, nloop, rentry, nht, lht,
     *     ntr, itimeres, nhits, sahist, temp, schgpt, nchgpt, ntest,
     *     nfound, nspan, ndelete, nindx, tflg
      close(40)
      print *, ' JOURNAL READ: ', jrnlfile(1:lnblnk(jrnlfile))
      rsnet = network
      print *,' Journal version: ',inver
      print *,'   maxstn, nstns, begyr, endyr, ntech, ninh',
     *  inmax, instns, inbeg, inend, ntech, ninh
      print *,' >>> Restart Network: ', rsnet
      print *,' >>> Method: ', method
      print *,' >>> Nloop: ', nloop
      print *,' >>> Rentry: ', rentry
      print *,' >>> Timeres: ', itimeres

c      print *,' nhits>0 read: ',maxstns, nmo
c      do istn = 1, maxstns
c        do imo = 1, nmo
c          if(nhits(istn, imo) .ne. 0) print *,istn, imo, nhits(istn,imo)
c        enddo
c      enddo

c      do k = 1, maxstns
c        do istn = 2, maxnstns
c          do imo = 1, nmo
c            do it = 1, intech
c              ichg = nspan(k,istn,imo,it)
c              if(ichg .gt. 0 .or. nfound(k,istn,imo,it) .ne. '0')
c     *          print *,'NSPAN:',k,istn,imo,it,ichg,
c     *            ' ',nfound(k,istn,imo,it)
c            enddo
c          enddo
c        enddo
c      enddo

   30 return

  100 print *,' Error in reading process variables and arrays'
      stop
      end


