c     Copied from Skyline.0.9.0 (355)
 
      subroutine splitmerge(idunit, rrtemp, subnet, iopt, istnpt,
     *  isigy1, isigy2, mtype, asigx, azscr, nsig, iwrite, itarg, 
     *  it2pair, ipair, ip2targ, rTraw, imob, imoe)
     
c      UCP  Ver   Date     Description
c
c          v22b 20110727   Addition of the fixes for errors pointed out
c                           by Daniel Rothenberger
c                          Mirror changes in GHCNMv3.0.0/splitmerge.v22r.f
c
c     (52g) v22 20100304   Merge with initial USHCN 52d tarball

c Vers   Date    Description
c  21f 19dec06  allow any tpr model to pass tpr0 model consistency test in testseg 
c
C     General note: During the splitting calls to TESTSEG there is a set of
c       work arrays used (with "ex" in their names) to accumulate the changes
c       to the changepoint arrays. All internal results in TESTSEG must be
c       accounted for in these arrays. 
c       During the merging calls to TESTSEG, changes are made only to the
c       incoming arrays (with "in" in their names)

c     subroutine parameters
c       idunit = output unit number for writing station data
c       rrtemp = cand & net temperature data, 12 monthly + annual
c       subnet = cand & net coop numbers
c       iopt definitions come from inhomog.comm.mthly.incl...
c       isigy1 = year-month changepoints (end of before (earlier) window)
c       isigy2 = year-month changepoints (begin of after (more current) window)
c       asigx = associated offsets for changepoint years
c       azscr = offset z-score wrt MINBIC segmented sum-square error
c       mtype = associated data models for changepoint years
c       nsig = number of changepoints
c       iwrite = toggle for writing out station data
c           :=0 for no output, else :=index of candidate for output
c       itarg = index of the first (target) station
c       ipair = index of the second station
c
c Vers   Date    Description
c
c  21q 24apr06   trying to understand differences between 21f & 21p
c                  removed SLR1 BIC vs TPR0 ONLY incompat
c                  left Split/Merge segment < 5obs removed & ndelete inc
c
c  21p 16apr06   changes generated for the structural uncertainty study
c                with Peter Thorne
c
c    --------------------------- Official Version USHCNv2 ----------------------
c    ---------------------------------- 21 Dec 2006 ----------------------------
c
c  21f 19dec06   TEST VERSION - ALLOW ANY TPR MODEL TO PASS TPR0 
c                                MODEL CONSISTENCY TEST IN TESTSEG 
c
c  21e 13Oct06   ERROR FIX - rsseq returned from MINBIC is actually the standard
c                  deviation - removed second - erroneous - computation of
c                  std dev
c
c  21d 30may06   redefined ndelete for individual targ-pair series
c
c  21c 29may06   added ifail to minbic call (place holder: not used)
c
c  21b 26may06   remove merging of segments when minbic returns SLR 
c
c  21a 24may06   remove complementary changepoint test and reinitialize the 
c                paired difference series after stat split/merge, before minbic
c
c  21  18may06   add complementary changepoint test in the minbic model testing
c
c  20h 11may06   changed minbic option to "2" for critvar test
c
c  20g 11may06   added "offsetting changepoints technique" to remove data and 
c                the later chgpt for a short segment (<= minlenshf) where the
c                estimated adjustments are within 20% (cratio) of each other. 
c
c  20f 09may06   rearranged decision tree in testseg for diverging philosophy
c                between split/merge and minbic proceedures
c
c  20e 09may06   all minbic calls use option 4 - no slopevar
c
c  20b 08may06   removed ntest increment, removed "CHGPT EQ MISS" abort at
c                start of segment, added inqtype=5 for BIC run in testseg
c
c  20a 21apr06   rename MLRTest and rewrote mlrtest & testseg for new philosophy
c
c  20  05apr06   reinstated attempt to estimated breakpoint (model=8) to
c                  minimize adjustments
c
c  19a 05apr06   SEE v18c - this is the 2nd problem
c                 2) must reinstate the MINBIC model test at the end of the
c                      split/merge step for each statistic, else too many
c                      erroneous changepoints (wrong stat model for data model)
c
c  19  03apr06   maybe I is stupid.... a stray comment from Matt "I don't see how
c                 to justify that" made me stop a realize that there MUST be NO
c                 data erasing for MINLEN in the testseg slipt/merge loop. I guess
c                 I just did not think it through.
c
c                 found two problems since the Betav6 (Atlanta AMS) MLRTest.v17e
c                 that have caused significant problems with the stability of
c                 the entire algorithm..... one inadvertant (mistake) the 
c                 other intentional (stupidity) 
c  18c 24mar06    1) overlooked adjusting the SPAN array at v18 when the 
c                      serial to compressed to serial was streamlined
c                 2) SEE v19a
c
c  18b 14mar06  skipping over stablized segments (i.e. no splits in split 
c                 mode and/or no merges in merge mode) save ~10% runtime
c
c  18a 03mar06  fixed change increment for compression 0 in testseg
c
c  18  12feb06  Revisited testseg to change input data arrays from compressed
c                 to serial with missing data. Problem with serial/compressed 
c                 confusion has probably been resident since early v16 or v17
c
c  17h 09feb06  Added sum-square error return from MINBIC
c
c  17g 22jan06  In split/merge (method=1) removed MINBIC for testing data 
c                 model type and call it only for best amplitude (iopt=2)
c
c  17f 22jan06  added inqtype to transfer the stat test used for the
c                 current minbic selection of data models
c
c  17e 21jan06  added input parameter istn21 to denote the index of stn1
c                 as a neighbor of stn2 (=-1 if not)
c
c  17d 15jan06  removed ntest increment - see statubs and calling routine
c                 in ucpmonthly.v9a.f
c
c  17c 13jan06  retained BRKPT model (=8) for changepoints and send model 
c                 types back to calling program as mtype(ninh)
c
c  17b 19dec05  fixed confusion about the "ex" and "in" work arrays in 
c               TESTSEG for changes in the changepoint arrays
c
c  17a 08dec05  pulled the merge for straight line segments out of the BIC
c               loop to remove any question of directional bias
c
c  17  06dec05  rearranged raw temp input to testseg so that segment lengths
c               less than minlen are removed 
c
c  16a 14nov05  remove legacy qsoft3 & monsd used for composite ref series
c               modified some debug statements to work at level 2 (from 3)
c               removed minlen gap for changepoints near edges
c
c  16  12oct05  another major shift during the beta-testing phase comes with
c               Matt's suggestion to put the BIC multi-model test BEFORE
c               the confirmfilt's unconfounding/conflation. See testseg calls.
c       BEWARE: the location vs amplitude and hits vs pairs decision
c               arrays in confirmfilt are based upon single statistic
c               parameter runs. NOT WITH THE MULTI-MODEL OUTPUT.
c               We must rerun the series of bracketing runs on the monthly
c               benchmark to regenerate these arrays.
c
c  15b 29sep05  add ioption parm to BIC code
c
c  15a 22Sep05  Remove "confused" data gaps from the ntest accumulation 
c               (used in the confirmfilt to set the hit threshold for 
c               undocumented chgpts)
c
c  14  16aug05 added isigy1-isigy2 to get span of possible yr/mths for
c              a given chgpt.                      
c
c  Version 13c - removed "not enough data" segments as defined in 
c           13b before the call to testseg       2 aug 05
c
c  Version 13b - Two confusing senarios need to be sorted out - 
c           These senarios should only happen in the t-test (2nd pass),
c              after confirmfilt with Station History added
c           1) The same station has two hits within minlen - this attempt
c              will be to collapse data (effectively using the earlier hit)
c           2) Each station has one each hit within minlen - this attempt
c              will be to NOT USE AT ALL (Alt: ascribe a hit to BOTH stations)
c
c  Version 13a - with the "paired" method the number of stations is
c           ALWAYS 2! Collapsed depave, etc. to give better clarity
c                                                9 may 05
c
c  Version 13 - added the T-test for when method=3. In the process
c           of removing the SHAP algorithm all-together!
c                                                17 mar 05
c
c  Version 12 - thought about including metadata - prematurely.....
c           did not do much of anything!
c                                                16 feb 05
c  Version 11b - feedback the number of pairwise tests for each of the
c           station-year-month for confirm filter
c                                                08 Mar 05
c
c  Version 11a - Removed all traces of the multi-linear regression and
c           the Durbin-Watson test (the LV routines and their progeny)
c                                                03 Feb 05
c
c  Version 11 - Removed dependence on LV's routine for the adjustment
c           amount. Reverted to a simple weighted average
c                                                21 Jan 05
c
c  Version 10 - Major rework of the algorithm to re-incorporate monthly 
c           data into the series - See testseg for more details
c                                                03 Nov 04
c
c  Version 9t - The pairing method should result in similar results from
c           any reference formulation to each stat-test. This is only the 
c           case for Durbin-Watson (PW, LW, MW). Extend printout to fix
c           inconsistency.
c                                                01 apr 04
c
c  Version 9pm - Parsing/merging re:Hawkins goes forth and back between modes
c           complimentary to LVwrapper.v3pm.f  
c                                                10 feb 04 cw
c
c  Version 8a - Rework the parsing loop for Lucie's Multi-Linear Regress
c    reference series
c                                                  02 Feb 04 
c
c  Version 8 - there is a full split in the ref/stat logic/algorithms....
c    see iscrit and ireftyp arrays below. Added capabilities for options:
c    LW, PW, MW. See inhomog.comm.mthly.incl for definitions
c                                                  26 Jan 04
c       
c  Version 7 - removed several routines to finish cleaning up for the 
c    removal of monthly calculations, depave and monsd are gone along
c    with several arrays.
c                                                  20 Sep 03
c
c  Version 6a - consideration of autocorrelation by several published
c    sources indicates that the statistical threshold level should be
c    increased. To test this, the critical values have been changed 
c    from the 95% to the 97.5% level. Hopefully, this will result in
c    a strong drop in the number of false positives but not correct hits.
c
c  Version 6 contains the expansion of the iopt codes to include
c    the mix&match tests LS, LT, PS, PT (see octpart main routine
c    for definitions)
c
c  Version 5a ensures identical series are bypassed (problem with 
c    random series generation)
c
c  Version 5 saves and returns all prospective changepoints
c
c  Version 4e3 is a gentler, simpler version to let the first pass
c   go through wether it is significantly inhomog or not.
C
c  Version 4 incorporates the Two Phase Regression test from MM.
c    Includes 1) new output tag: MM for TPR
c             2) new test statistic L&R Two phase
c             3) new output files using new output tag
c                                                  27 May 03
c
c  Version 3b contains an attempt to adjust at the changepoints indicated
c    by Menne/Duchon using the Lucie Vincent adjustment proceedure
c                                                  6 Feb 03 cw
c
c  Version 3 has been truncated to do annual evaluation
c                                                 22 Oct 02 cw
c
c  This version of MLRTest2.f has been modified for use with the
c   Integrated Homogeneity Test by:

c   1)  Making this a subroutine
c   2)  Removing references to Min and Max (the IHT processes one element
c          at a time)
c   3)  Passing the incoming data to getata which populates the arrays
c          used by MLRTest2
c
c  This version of MLRTest evaluates a time series for inhomogeneities 
c  starting with the last observation and working backwards.  In the
c  output, it searches for the first peak of the Tmax test statistic
c  and writes out the month that corresponds to that date.  
c
c  MLR Test is an abbreviation for the Maximum Likelihood Ratio Test.
c  The purpose of this program is to calculate inhomogeneity statistics
c  for a step-change (or shift) and a trend in a time series of monthly
c  mean temperatures from a candidate station.  The basis for this
c  program is the set of papers by Alexandersson and Moberg, the
c  references for which are given in the subroutines. The analysis is
c  intended to be performed on a monthly time step.
c
c  This version differs from our original version in that the q-array 
c  is scaled by an annual cyle of standard deviations of the monthly 
c  means.  All stations, candidate and neighbors, are scaled by the 
c  same monthly standard deviation.  In this sense we can think of 
c  this annual cycle as representative of each station but not unique 
c  to it.  The purpose for doing this is to minimize the effect that 
c  generally higher variance of monthly means in winter than in summer 
c  could have on producing a non-stationary q-time series.
c
c  The first step is to calculate the correlation coefficients between
c  the candidate and each of its neighbors.  These calculations are 
c  performed in subroutines associated with the formerly separate program
c  RHODRVR.  These subroutines, now a part of this program, include:
c  frstdif, depave, & correl.
c
c  A first difference filter is applied to the time series of monthly
c  mean temperatures.  Then the departures from the annual cycle of
c  monthly means of the differenced data for each station are computed.
c  Lastly, the candidate-neighbor correlation coefficients are calculated.
c  Missing data are taken into account in the calculations.
c
c  The MLR Test then makes use of the mean monthly temperatures and 
c  correlation coefficients between the candidate and neighbors. 
c  The latter effectively serve as station weights.
c  The annual cycle of monthly means is computed for each station
c  using an integer number of years.  When a new calendar year of 
c  monthly means becomes available, probably January or February,
c  a new set of correlation coefficients (from RHODRVR)and a new
c  annual cycle for each station (from SNIT) are computed.
c
c  The annual cycle of representive standard deviations of
c  monthly means is provided in subroutine MONSD.
c 
c  MLRTest computes the q and z time series in subroutine QZOFT2.
c  The q series is the monthly differences between the candidate
c  and the mean of its neighbors weighted by their respective
c  correlation coefficients and divided by the representative standard
c  deviations calculate in MONSD.  The z series is the standardized q
c  series.  MLRTest calls the subroutines that calculate the statistics
c  for a step-change or shift (SNITS) and a trend (SNITT).  The outputs
c  from these subtroutines are the step-change and trend statistics.
c  Their maximum values can be compared to a table of critical values
c  of these statistics taken from p. 31 of the 1997 paper and given in
c  subroutine LOOKUP.
c
c  Subroutine LOOKUP provides 90%,95%, and 97.5% critical values as a 
c  function of the number of months in the temperature time series.
c  The critical values are the same for both shifts and trends.  It
c  should be noted that the subroutine that calculates the trend
c  statistic (SNITT)includes the statistic for shifts calculated by
c  the shift subroutine (SNITS).  That both subroutines are included
c  is related to the way the SNIT program was developed.  At some
c  point in the future the latter subroutine (SNITS) can be removed.
c
c  Subroutine maxstep identifies relative peaks in the tmax test 
c  statistic.
c
c  21 December, 1999; 12 July, 2001; November 2001; March 2002; June 2002
c
c  June 2002 -- the date intervals were generalized for testing
c  at each relative peak in the Tmax test statistic and for reading
c  the TD3200 data format.  The rhodrvr.f program also has been 
c  integrated into the larger program.
c

c     From restart.mod.f95 module for allocation - 09apr2009 cw
      use restart

      INCLUDE 'inhomog.parm.mthly.incl' 
      INCLUDE 'inhomog.comm.mthly.incl'
      INCLUDE 'inhomog.restart.mthly.incl'
      INCLUDE 'inhomog.MDparm.mthly.incl'

      character*23 stnstr
      character*2 otag
      character*11 subnet(2)
      integer ounit
      integer job

c     paired difference temperature series
      dimension rTraw(nmo)
      dimension qx(nmo),qy(nmo)
      
c     inhomogeneous breakpoint array (including begin/end of series)
      integer inhyr(ninh), inhmod(ninh)
      integer lsplitin2(ninh), lsplitout(ninh), lmergein2(ninh),
     *  lmergeout(ninh)
      real offinh(ninh),segslp(2,ninh),sseseg(ninh)
c     keep track of significant year/months (isigy), models (mtype), 
c       and offset (asigx)
      integer isigy1(ninh), isigy2(ninh), mtype(ninh), istnpt(ninh)
      real asigx(ninh), azscr(ninh)
c     inhomog hits delineated by none = 0, stn1 = 1, stn2 = 2, both = 3
      integer inhstns(ninh)
c     current lowest imo for each type of inhomog hit (1,2,3)
      integer inhmo(3)
      integer end1,beg2
      real rrtemp(2,nmo)
      integer mknt2(2)
      real rslpq(2), rmuq(2)
      otag = c2tech(iopt)
      
      mintest = 5
c      mintest = minlen

c     >>>>>>>>>>>>> Initialize incoming inhomog arrays <<<<<<<<<<<<

c ------  Initialize data and reference series  ------
c     set up the temp arrays and the yr position array
c     imob & imoe are the begin and end indices of paired station's data
      imob = 0
      imoe = nmo
c      nmth = 0
      do imo = 1, nmo
        if(rrtemp(1, imo) .gt. amiss+1. .and. 
     *    rrtemp(2, imo) .gt. amiss+1. ) then
          if(imob .eq. 0) imob = imo
          imoe = imo
          rTraw(imo) = rrtemp(1,imo) - rrtemp(2,imo)
          if(idebug .ge. 3)
     *      print *,'rTraw: ',imo,rrtemp(1,imo),rrtemp(2,imo),rTraw(imo)

c          nmth = nmth + 1
        else
c          print *,' One of pair miss: ', imo
          rTraw(imo) = amiss  
        endif
c        if(mod(imo,12) .eq. 0) then
c          if(nmth .gt. 0) then
c            call imo2iym(iy,im,imo)
c            write(6,'(a,"-",a,i5,12f8.2)') subnet(1),subnet(2),iy,
c     *        (rTraw(im),im=imo-11,imo)
c          endif
c          nmth = 0
c        endif  
      enddo
      if(imob .eq. 0) then
        write(6,*)subnet(1),' Station has no data - skipping'
        goto 999
      endif
      call imo2iym(iyr,imth,imob)
      iBegYr = iyr
      iBegMth = imth
      call imo2iym(iyr,imth,imoe)
      iEndYr = iyr  
      iEndMth = imth
      
c     Initialize arguments to test segments:
c      iFirst requires the series to be split the first pass
c      numchgpt initializes number of change points found
C     Mindy determines if ifirst gets set to 1
      iFirst = 0
      ichange = 1
      ipass = 1

c     ARRAYS FOR CHGPTS FROM BIC and TESTSEG
c     inhyr is the month index of the chgpts
c     inhyr(1) is the begin of series, inhyr(mindy) is the end of series
c     inhmod is the model
c     inhstns is the station of the pair with the SHF "hit" (1,2,or 3=both)
c     offinh is the offset
c     BEFORE/AFTER ADJOINING SEGMENTS:
c     segslp are the slopes
c     sseseg are the sum-square error

      do inh = 1, ninh
        inhyr(inh) = 0
        inhstns(inh) = 0
      enddo  
          
c     number obs in current segment wrt inhomog date array (from station history)
      nobs = 0
c     first inhomog date array postion is beginning of data
      mindy = 1
      inhyr(mindy) = imob
c     initialize last obs value when nhit > 0
      lnhobs = inhyr(mindy)

c     initialize last yr/month with inhomog
      call imo2iym(lnhyr,lnhmth,imob)
c     set number of separate paired inhomog changepoints in series
      inhsum = 0

c      print *,' At MLRTest - nobs,yr,mth,rTraw1&2'
c     go through the current hits and the data for both stations
      inhadd = 0
      lnhadd = 0
c     inhmo holds the earliest chgpt from each and both stations
c       for aligning with the full period chgpts
      do inh = 1,3
        inhmo(inh) = 9999
      enddo
      
c     itarg and ipair are constant in the subroutine
c      calls for imoe are mainly index checking
      call work2sky(itarg, imob, ityr1, itmth1, itsky1, 1)
      call work2sky(itarg, imoe, ityr2, itmth2, itsky2, 1)
      call work2sky(ipair, imob, ipyr1, ipmth1, ipsky1, 1)
      call work2sky(ipair, imoe, ipyr2, ipmth2, ipsky2, 1)
      
      lhitmo = imob
      do imo = imob, imoe
        call imo2iym(iyr,im,imo)
        itskymo = itsky1 + imo - imob
        ipskymo = ipsky1 + imo - imob
c       if there are any station inhomog, set toggle and test with last
c         Stn 1 only inhomog: inhadd := 1 
c         Stn 2 only inhomog: inhadd := 2
c         Stn 2 & 3  inhomog: inhadd := 3 
    9   iredo = 0
        if(nhits(itskymo).gt.0) then
          if(idebug .ge. 3)  print *,' Stn1 - nhits ', subnet(1),
     *      itarg, iyr, im, nhits(itskymo)
          lhitmo = imo
          if(imo .lt. inhmo(1)) inhmo(1) = imo
          if(inhadd .ne. 1 .and. inhadd .ne. 3) inhadd = inhadd + 1
        endif  
        if(nhits(ipskymo).gt.0) then
          if(idebug .ge. 3) print *,' Stn2 - nhits ', subnet(2),
     *      ipair, iyr, im, nhits(ipskymo)
          lhitmo = imo
          if(imo .lt. inhmo(2)) inhmo(2) = imo
          if(inhadd .ne. 2 .and. inhadd .ne. 3) inhadd = inhadd + 2
        endif  
        if(inhadd .eq. 3 .and. imo .lt. inhmo(3)) inhmo(3) = imo
        if(imo .eq. imoe) then
          inhadd = 3
        endif  
        
        if(rTraw(imo) .gt. amiss+1.) then
c         increment number in segment
          nobs = nobs+1
c         if there has been a station inhomog
          if(inhadd .gt. 0 .and. nobs .gt. 1) then
c           adjust if the earliest chgpt is in the previous compressed
c             location
            if(inhmo(inhadd) .lt. imo) then
              nobs = nobs - 1
              iredo = 1
            endif  
c           check for more than minlen data since the last inhomog
            if(nobs .lt. mintest) then
c             if not, reset to last and merge stat inhomog type
              if(idebug .ge. 3) 
     *          print *,' Not enough data from: ', lnhyr, lnhmth,
     *            ' to ', iyr, im
              if(lnhadd.eq.1 .and. inhadd.eq.2) inhadd = 3
              if(lnhadd.eq.2 .and. inhadd.eq.1) inhadd = 3
              if(lnhadd.eq.3) inhadd = 3
c             v13c - removed not enough data segments from testseg input
c             backup and set data for testseg to missing
              lobs = lnhobs + 1
              if(mindy .eq. 1) lobs = lnhobs
              print *,' rTraw to miss: ', lobs, imo
              do iobs = lobs, imo
                rTraw(iobs) = amiss
              enddo  
            else
c             keep sum of metadata added
              if(idebug .ge. 3) 
     *          print *,' Enough data from: ', lnhyr, lnhmth,
     *            ' to ', iyr, im
              inhsum = inhsum + 1
c             compile inhomog array for segtest - check against the 
c               current earliest chgpt
              mindy = mindy + 1
c              inhyr(mindy) = imo
              inhyr(mindy) = lhitmo
              lnhobs = inhyr(mindy)
              lnhyr = iyr
              lnhmth = im
            endif 
c           reset the number of obs for current segment
            nobs = 0
c           Keep inhomog type for adjustment allocation
            inhstns(mindy) = inhadd
            lnhadd = inhadd
            inhadd = 0
            do inh = 1, 3
              inhmo(inh) = 9999
            enddo  
c           if current earliest is not current month, check current month
            if(iredo .eq. 1) goto 9
          endif  
        endif
      enddo

      if(idebug .ge. 3) then
        print *,' Inhomog data: mindy,inhyr,yr,mth,inhstns'
        do i = 1, mindy
          indx = inhyr(i)
          call imo2iym(iy,im,indx)
          write(6,'(5i5)') i,indx,iy,im,inhstns(i)
        enddo  
      endif  

c     The yr/mth index arrays have been set for all windows with enough
c       data for the stat tests, now set last yr/mth 
      do imo = imob, imoe
        if(rTraw(imo).gt.amiss+1.) lastmo = imo
      end do  ! end candidate/neighbor loop

c     if needed, readjust the last inhomog at the last paired data value
c      if(lastmo .lt. imoe) then
      if(inhyr(mindy) .lt. lastmo) then
        if(idebug .ge. 3) 
     *    print *,' Last month chg from: ',inhyr(mindy),' to ',lastmo
        inhyr(mindy) = lastmo
        imoe = lastmo
      endif
      
c     >>>>>>>>>>>>> Split/merge section <<<<<<<<<<<<
      
c    ------ Test segment until there are no changes ------
c        12oct05 - added looping through the BIC after all of 
c        the changes have been found by the Undoc split/merge

c     if there was no changepoints - break first series anyway
      if(mindy .eq. 2) iFirst = 1
      
c     set the station output string
      stnstr = subnet(1)(1:lnblnk(subnet(1))) // '-' // 
     *  subnet(2)(1:lnblnk(subnet(1)))

c     initialize number of chgpts in last split/merge testseg calls
      nsplitin2 = 0
      nsplitout = 0
      nmergein2 = 0
      nmergeout = 0

      do inh = 1, ninh
        lsplitin2(inh) = 0
        lmergein2(inh) = 0
        lsplitout(inh) = 0
        lmergeout(inh) = 0
        inhmod(inh) = 0
        offinh(inh) = 0.0
        sseseg(inh) = 0.0
        segslp(1,inh) = 0.0
        segslp(2,inh) = 0.0
      enddo

c     if this is the metadata inclusion method(=3) and neither the UCP
c      nor the histories contain any changepoints, or there where no
c      additional suspect changepoints in the metadata then skip segtest
      if((iFirst .eq. 1 .or. inhsum .eq. 0) .and. method .eq. 3) then
        print *,' skip segtest ifirst, inhsum, method: ',ifirst,inhsum,
     *    method
        iFirst = 0
        ichange = 0
        goto 20
      endif  

c     if this is the undocmented stats call, do merge/split and
c       start with the undocmented stats (not BIC)
      if(method .ne. 3) then
        iMerge = 0
        iBic = 0
        ioptseg = iopt
c       set inqtype for the duration of the looping
        if(iopt .eq. indMD) then
          inqtype = 3
        else if(iopt .eq. indMM) then
          inqtype = 5
        else if(iopt .eq. indXL) then
          inqtype = 4
        else
          print *,' Unknown MLRTest stat: ', iopt
          stop
        endif        
      else  
c       if this is the metadata inclusion method then only merge
c         using the BIC
        iMerge = 1
        iBic = 1
        ioptseg = iopt
        inqtype = 5
      endif  

C      For MINBIC inclusion in method=1
  10  do while (ichange .ne. 0 .or. ibic .eq. 0)
        ichange = 0
        imchange = 0
        ipchange = 0
        ierr = 0
        if(idebug .ge. 3)
     *    write(6,*)' Segtest pass: ',ipass, ' ibic: ',ibic, ' opt: ',
     *      ioptseg, ' imerge: ', iMerge


        if(iMerge .eq. 0) then
          if(idebug .ge. 2)
     *      print *,' Parse segments (isplit = 1), ipass:', ipass
          call testseg(rTraw, ioptseg, inqtype, inhmod, inhstns,
     *      iFirst, mindy, inhyr, nsplitin2, lsplitin2,
     *      offinh, segslp, sseseg, 1, ipchange, subnet, ierr)
c          print *,' ipchange ', ipchange
          if(ierr .ne. 0) goto 20
c         move the prior last split out to the next split input
c         keep this output as the last split out
          do inh = 1, ninh
            lsplitin2(inh) = lsplitout(inh)
            lsplitout(inh) = inhyr(inh)
          enddo
          nsplitin2 = nsplitout
          nsplitout = mindy
        endif  

c       If ifirst pass complete, loop back and test shorter segments
        if(iFirst .eq. 1) then
          iFirst = 0
          goto 10
        endif  
        
        if(idebug .ge. 2)
     *    print *,' Merge segments (isplit = 0), ipass:', ipass
        call testseg(rTraw, ioptseg, inqtype, inhmod, inhstns, 
     *    iFirst, mindy, inhyr, nmergein2, lmergein2,
     *    offinh, segslp, sseseg, 0, imchange, subnet, ierr)
c        print *,' imchange ', imchange
        if(ierr .ne. 0) goto 20
c       move the prior last merge out to the next merge input
c       keep this output as the last merge out
        do inh = 1, ninh
          lmergein2(inh) = lmergeout(inh)
          lmergeout(inh) = inhyr(inh)
        enddo
        nmergein2 = nmergeout
        nmergeout = mindy
        ichange = ipchange + imchange
        ipass = ipass + 1

c       test to see if the undoc stats are finished with the split/merge
        if(ibic .eq. 0 .and. (ichange .eq. 0 .or. ipass .gt. 10)) then
c         reset for the BIC runs
          ichange = 1
          ibic = 1
          ipass = 1
          iMerge = 1
          ioptseg = indKW
          
c         At this point, Merge sees all of the changepoints as valid.
c         For all of the segments less than Mintest(=5)
c           1) delete data in rTraw
c           2) increment nDelete
c           3) remove changepoint (at upper end of segment)
c         The number of chgpts may change, use variables for loop
c                                                added 16apr2009
          indy1 = 1
          indy2 = 2
          do while (indy2 .le. mindy)
            if(indy1 .eq. 1) then
              iob1 = inhyr(indy1)
            else
              iob1 = inhyr(indy1)+1
            endif
            iob2 = inhyr(indy2)
            mknt = 0
            do nx = iob1, iob2
              if(rTraw(nx).gt.amiss+1.) mknt = mknt + 1
            enddo

            if(mknt .lt. mintest) then
              call imo2iym(iyr1, imth1, iob1)
              call imo2iym(iyr2, imth2, iob2)
              if(idebug .ge. 1)
     *          write(6,'(a, " ", a, " Remove 0 Short Segment ", i4,
     *          i3, i5, " to ",i4,i3, i5, i5)') subnet(1), otag,
     *          iyr1, imth1, iob1, iyr2, imth2, iob2, mknt
              do nx = iob1, iob2
                if(rTraw(nx).gt.amiss+1.) then
                  call work2sky(itarg, nx, iyrd, imthd, iskymo, 1)
                  ndelete(it2pair,iskymo) = 'D'
                  rTraw(nx) = amiss
                endif  
              enddo  
              do indy = indy2, mindy
                inhyr(indy - 1) = inhyr(indy)
              enddo
              inhyr(mindy) = 0
              mindy = mindy - 1
            else
c             FIXED - was skipping sequential short segments 16Sep 2011
              indy2 = indy2 + 1
              indy1 = indy1 + 1
            endif  
          enddo

          
        else if(ibic .eq. 1 .and. ipass .le. 10) then
          iochange = 0
c         skip Complementary chgpts model at this time        24 May 06
          goto 15

c         Bringing "complementary chgpts" model earlier into the process
c           to incorporate short divergent sloped and multiple segments. 
c         Due to the ordering of the statistical tests, effectively 
c           making TPR2 the main workhorse

c         initialize divergence trigger (number of interim chgpts)
          intr = 0

          ichg = 2
          do while (ichg .le. mindy-1)
c           count the number of observations in the upcoming segment
            ilen = 0
            do imo = inhyr(ichg)+1, inhyr(ichg+1)
              if(rTraw(imo) .gt. amiss + 1.0) ilen = ilen + 1
            enddo
     
c           Complementary chgpt test triggered by upcoming seglen < minlenshf
c             and continues until upcoming seglen >= minlenshf
            if(ilen .lt. minlenshf) then
              if(intr .eq. 0) then
c               begin of comp chgpt test fragment
                indydiv1 = ichg
                imodiv1 = inhyr(ichg)+1
c               segment immediately before comp chgpt test
                imopre1 = inhyr(ichg-1)+1
                imopre2 = inhyr(ichg)
                idomain = 1
                qsign = offinh(ichg)
              endif
c             current end of comp chgpt test fragment
              imodiv2 = inhyr(ichg+1)
              intr = intr + 1
              if(qsign * offinh(ichg) .lt. 0.0) idomain = 0
            else if(intr .gt. 0) then
c             check last estimate
              if(qsign * offinh(ichg) .lt. 0.0) idomain = 0
              if(idomain .eq. 0) then
c               to get here, short fragments have been accumulated (intr > 0) 
c               and the est adj are on both sides of zero (idomain = 0)
c               and at the end of the short segments (2nd seg len >= minlenshf)
c               set end of the complementary chgpt test
                imodiv2 = inhyr(ichg)
c               segment immediately after comp chgpt test
                imopost1 = inhyr(ichg) + 1
                imopost2 = inhyr(ichg+1)
c               generate series and indices for minbic and erase comp chgpt data
                ix = 0
                mknt = 0
c               process from begin of pre segment to end of post segment
                do imo = imopre1, imopost2
                  ix = ix + 1
                  qx(ix) = imo
c                 catch and keep end of pre segment
                  if(imo .eq. imopre2) iqob1 = ix
c                 keep data outside of the comp chgpt test
                  if(imo .lt. imodiv1 .or. imo .gt. imodiv2) then
                    qy(ix) = rTraw(imo)
                  else
c                   erase data within the comp chgpt test
                    if(qy(ix) .gt. amiss+1) then
                      qy(ix) = amiss
                      call work2sky(itarg, imo, iyrd, imthd, iskymo, 1)
                      ndelete(it2pair,iskymo) = 'D'
                      rTraw(imo) = amiss
                    endif  
                  endif  
                enddo
                numx = imopost2 - imopre1 + 1
c               go back through MINBIC for finals
                call minbic(2, qx, qy, iqob1, numx, critval, curstat, 
     *            qmin, toff, rmuq, rslpq, rsseq, inqtype, iqtype, 
     *            mknt2, ifail)
                call imo2iym(iyr1, imth1, imopre1)
                call imo2iym(iyrb, imthb, imopre2)
                call imo2iym(iyrb2, imthb2, imopost1)
                call imo2iym(iyr2, imth2, imopost2)
                if(idebug .ge. 1) write(6,'( a, "-", a, " COMP: ", 
     *            2(2(i5, i2.2, i5), " : "), 3f7.2, 2f7.3, 3i5)')
     *            subnet(1),subnet(2), iyr1, imth1, imopre1,
     *            iyrb, imthb, imopre2, iyrb2, imthb2, imopost1, 
     *            iyr2, imth2, imopost2, curstat,  
     *            critval, toff, rslpq, mknt2, iqtype

c               go back to the first comp chgpt index (indydiv1)
                indyback = ichg - indydiv1
c               collapse the breakpoint array
                do jchg = ichg+1, mindy
                  inhyr(jchg - indyback) = inhyr(jchg)
                  inhmod(jchg - indyback) = inhmod(jchg)
                  inhstns(jchg - indyback) = inhstns(jchg)
                  offinh(jchg - indyback) = offinh(jchg)
                  sseseg(jchg - indyback) = sseseg(jchg)
                  segslp(1,jchg - indyback) = segslp(1,jchg)
                  segslp(2,jchg - indyback) = segslp(2,jchg)
                enddo
                mindy = mindy - indyback
                ichg = ichg - indyback

                iochange = iochange + 1
              endif
              intr = 0
            endif
c            print *,' iochange, ichg ',iochange, ichg
            ichg = ichg + 1
          enddo  

   15     ichange = ichange + iochange
        endif  
c        print *,' ichange ', ichange
c       if MINBIC has been turned ON and
c         either no changes have occured or there have been 10 passes
c         then leave testseg loop
        if(ibic .eq. 1 .and. (ichange .eq. 0 .or. ipass .gt. 10))
     *     goto 20  
        
      enddo             

c     >>>>>>>>>>> BIC model vs Split/merge stat section <<<<<<<<<<
   20 nsig = 0

c     initialize output arrays
      do inh = 1, ninh
        isigy1(inh) = 0
        isigy2(inh) = 0
        asigx(inh) = 0.0
        azscr(inh) = 0.0
        mtype(inh) = 0
        istnpt(inh) = 0
      enddo  
      do ichg = 2, mindy-1
        modtype = inhmod(ichg)
        wadj = offinh(ichg)
        end1 = inhyr(ichg)
        do indy = inhyr(ichg) + 1, inhyr(ichg+1)
          if(rTraw(indy) .gt. amiss+1.) then
            beg2 = indy
            goto 30
          endif
        enddo  
   30   continue
        rslp1 = segslp(1,ichg)
        rslp2 = segslp(2,ichg)
        
c       if the segment is "unconfounded" accumulate adjustments
c         and the model type has not been changed to slr0 or slr1

c       ---------------- REMOVED FOLLOWING OPTION v21q ---------------
c       V21p modification - Only consider SLR1 BIC output
c         incompatible with TPR0 input due to sensitivity
c         considerations
        iprocess = 0
        if(method .eq. 3) then
          iprocess = 1
        else if(inhstns(ichg) .ne. 3 .and. modtype .ge. 3) then
          iprocess = 1
c         test the incoming stattest (inqtype) against the best minbic 
c           data model (modtype). The following compatibilities are assumed:
c            inqtype    modtype
c            3 TPR0     3 TPR0
c            4 TPR1     3 TPR0 & 4 TPR1
c            5 TPR2     3 TPR0, 4 TPR1, 5 TPR2, 6 TPR3 & 7 TPR4
c           any other inqtype-modtype combinations are removed
c          if(inqtype .eq. 3 .and. modtype .ne. 3) then

c         V21F modification - ALLOW ALL TPR MODELS FOR TPR0
          if(inqtype .eq. 3 .and. modtype .lt. 3) then
            iprocess = 0
          else if(inqtype .eq. 4) then
            if(modtype .ne. 3 .and. modtype .ne. 4) iprocess = 0
          else if(inqtype .eq. 5 .and. modtype .lt. 3) then
            iprocess = 0
          endif  
        endif  
        if(iprocess .eq. 0) then
          if(idebug .gt. 2)
     *      print *,'Removed incompatible in/out qtype:',inqtype,
     *        modtype, inhstns(ichg)
          if(idebug .gt. 1) then
            call imo2iym(iye1,ime1,end1)
            call imo2iym(iyb2,imb2,beg2)
            write(6,'(a,2i5,1x,a," TESTSEG SKIP:",f7.2,2(2i5,i3),2i3)')
     *        stnstr(1:lnblnk(stnstr)), itarg, ipair, otag, wadj,
     *        end1, iye1, ime1,
     *        beg2, iyb2, imb2, modtype, inhstns(ichg)
          endif
        else
          nsig = nsig + 1
c         year/month of sig changepoint (beg-end of span)
          isigy1(nsig) = end1
          isigy2(nsig) = beg2-1
          asigx(nsig) = wadj
          azscr(nsig) = wadj / sseseg(ichg)
          mtype(nsig) = modtype
          istnpt(nsig) = inhstns(ichg)
          if(asigx(nsig) .gt. 10. .or. asigx(nsig) .lt. -10.) then
            print *, subnet(1),'-',subnet(2),' BIG ASIGX: ', 
     *        asigx(nsig),azscr(nsig),end1,beg2
          endif  

          if(idebug .ge. 1) then
            call imo2iym(iye1,ime1,end1)
            call imo2iym(iyb2,imb2,beg2)
            write(6,'(a,2i5,1x,a," TESTSEG ADJ: ",2f7.2,2f8.4,2(2i5,i3),
     *       2i3)') stnstr(1:lnblnk(stnstr)), itarg, ipair, otag, 
     *       asigx(nsig), azscr(nsig), rslp1, rslp2, end1, iye1, ime1,
     *       beg2, iyb2, imb2, modtype, istnpt(nsig)
          endif
          call imo2iym(iy, im, end1)
        endif  
   80 enddo  
      
  100 continue
     
  999 return
      end
c         
c     =======================================================================
c
      subroutine testseg(rTraw, ioptseg, inqtype, inhmod, inhstns,
     *  inFirst, mindy, inhyr, lindy, lnhyr, offinh, segslp, sseseg,
     *  iSplit, ichange, subnet, ierr)
c     loop through the segments - splitting at definitely inhomog,
c       keep when definitely homog, and restraining when questionable
      INCLUDE 'inhomog.parm.mthly.incl' 
      INCLUDE 'inhomog.comm.mthly.incl'
      INCLUDE 'inhomog.restart.mthly.incl'
      INCLUDE 'inhomog.MDparm.mthly.incl'

c      = cand & net temperature data, 12 monthly + annual
c     ioptseg is the Technique number (ref ser + stat test) 
c            See: inhomog.comm.mthly.incl for definitions
c     inqtype is the BIC model Q type for the inhomog splitting algorithm
c            (set as indMD=3,indMM=5,indXL=4) for minbic
c     inhyr is the inhomogeneous breakpoint array 
c            (including begin/end of series)
c     mindy is the total number of date indices in the chgpt array
c     iFirst indicates whether this is the Initial test of the series
c            currently this is used to force a chgpt at the peak whether
c            significant or not... to test the shorter series
c     iSplit indicates whether this is
c            Annual Merging call = 0
c            Annual Parsing call = 1
c            Monthly Merging call = 2
c            Monthly Parsing call = 3
c     ichange (output) indicates whether any change occured in the
c            segments
        
      integer inhyr(ninh), lnhyr(ninh), inhmod(ninh), inhstns(ninh),
     *   iexyr(ninh), iexmod(ninh), iexstns(ninh)
      real offinh(ninh), segslp(2,ninh), sseseg(ninh)
c      real offiex(ninh), segsex(2,ninh), ssesex(ninh)

c     raw input data with different array dimensions
      real rTraw(nmo)
      
c     difference between cand & composite neighbors (q) and 
c        standardized (z) series
      real qx(nmo),qy(nmo),z(nmo)
      
c     T-statistic and 9-point Binomial Average series derived from z
      real ts(nmo)

c      dimension tstat(nmo)
      integer mknt, mknt2(2)
      
      integer ounit

c     inhyr is the "inhomogeneous year" (chgpt) array, that is, the
c            segment array (including endpoints)
c     mindy is the total number of date indices in the chgpt array
      character*2 otag
      integer job
      character*11 subnet(2)
      character*9 limsense
      
      real acan(numyr), aref(numyr)
      integer ipyr(numyr), ifind(numyr)
      integer StatTest, end1
      
      real rmuq(2), rslpq(2)
      mintest = 5
c      mintest = minlen

      limsense = ' limit>: '

      job = 2
c     ioptseg can be negative at the end of the split/merge technique
c       to ONLY estimate the amplitudes
      iopt = abs(ioptseg)

c     set the outtag and out unit for this iopt version
      otag = c2tech(iopt)
      ounit = bunit + iopt

c     always start out with the first segment
      indy1 = 1
      if(isplit .eq. 1) then
        indy2 = 2
      else
        indy2 = 3
      endif    

      ichange = 0
      
c     Need a temporary variable that does not change the value in 
c     the calling program
      iFirst = inFirst
            
c     initialize inhomog expansion series array and indices
c     these are working arrays which are copied back to the incoming
c     arrays at the end of the split process
      if(isplit .eq. 1) then
        mexpn = mindy
        do ichg = 1, mindy
          iexyr(ichg) = inhyr(ichg)
          iexmod(ichg) = inhmod(ichg)
          iexstns(ichg) = inhstns(ichg)
c          offiex(ichg) = offinh(ichg)
c          ssesex(ichg) = sseseg(ichg)
c          segsex(1,ichg) = segslp(1,ichg)
c          segsex(2,ichg) = segslp(2,ichg)
        enddo 
      endif  
      iexy1 = indy1
      iexy2 = indy2
      
      lndy1 = 1
      lob1 = lnhyr(lndy1)

c ------    loop through all of the incoming segments (parse)  ------
c ------                or adjacent incoming segments (merge)  ------
      do while (indy2 .le. mindy)
      
        StatTest = inhomog
        if(indy1 .eq. 1) then
          iob1 = inhyr(indy1)
        else  
          iob1 = inhyr(indy1)+1
        endif  
        iob2 = inhyr(indy2)
c       if merge call - save interior chgpt
        if(isplit .eq. 0) iyrstat = inhyr(indy1 + 1)

c       generate yr/mth at beg, end, and brkpt of segments 
        call imo2iym(iyr1, imth1, iob1)
        call imo2iym(iyr2, imth2, iob2)
        if(ioptseg .lt. 0 .or. iscrit(iopt) .eq. iBstat) goto 10

c       is this segment stable (that is, is the segment the same as the 
c        last time testseg was called)
        do lndy1 = 1, lindy-1
          if(inhyr(indy1) .eq. lnhyr(lndy1)) then
            do inc = 1, indy2-indy1
              if(inhyr(indy1+inc) .ne. lnhyr(lndy1+inc)) goto 10
            enddo
            if(idebug .ge. 2)
     *        print *,'Stable segment: ',iob1,iyr1,imth1,iob2,iyr2,imth2
            goto 800
          endif
        enddo

c       initialize final stat series
   10   do im = 1, nmo
          qx(im) = amiss
          qy(im) = amiss
          z(im) = amiss
          ts(im) = 0.0
        enddo

c ------ Calculate/Recalculate ref series for current segment --------
c       estimate variance of each monthly time series
c       - also works for annual
c       mknt = number of non-missing months between iob1 & iob2
c       numx = number of serial months between iob1 & iob2
        mknt = 0
        ix = 0
        do nx = iob1,iob2
        ix = ix + 1
        qx(ix) = nx
        qy(ix) = rTraw(nx)
          if(rTraw(nx).gt.amiss+1.) mknt = mknt + 1
        enddo
        numx = iob2 - iob1 + 1      
        
        if(mknt .lt. mintest) then
          if(idebug .ge. 2)
     *      write(6,'(a, " ", a, " Skip 0 Segment too Short ", i4,
     *        i3, " to ",i4,i3,i5)') subnet(1), otag, iyr1, imth1,
     *        iyr2, imth2, mknt
c         Do nothing - goto next segment
          goto 800
        endif  
     
c       standardize series for split/merge stats
        if(iscrit(iopt) .ne. iBstat) then
          call standard(qy,z,1,numx,nmo)
        else
c         for estamt use full temperature series
          do ix = 1, numx
            z(ix) = qy(ix)
          enddo
        endif
              
c  ------ Generate test statistic for shift or step-change ----------
c       pass the window interval instead of nmo
c       see inhomog.comm.mthly.incl for ioption definitions
c          MLRTest.v17g modification - make minbic offset estimation call 
c          (iopt = 2) common for all of the split/merge calls (not indKW)
        if(ioptseg .gt. 0 .and. iscrit(iopt) .lt. iBstat) then

c         following fix thanks to D. Rothenberger 25Jun2011
c            for the stat routines here the relation between the
c              time-series (z & ts) and the count (last parameter)
c              should be:
c            if time-series not compressed, then numx (length of segment)
c            if time-series is compressed, then mknt (number of obs)
c         In this version - time-series are NOT COMPRESSED !!!
          if(iscrit(iopt) .eq. iTstat) then
c           generate statistic (and series for chgpt) for max-likely
c            call snits(nmo,z,ts,mknt)
            call snits(nmo,z,ts,numx)
            iqtype = 3  
          else if(iscrit(iopt) .eq. iFstat) then
c           generate statistic (and series for chgpt) for 2-phase
c            call twophreg(nmo,z,ts,mknt)
            call twophreg(nmo,z,ts,numx)
            iqtype = 5
          else if(iscrit(iopt) .eq. iXstat) then
c           generate statistic (and series for chgpt) for 2-phase w/const slope
c            call twophreg1(nmo,z,ts,mknt)
            call twophreg1(nmo,z,ts,numx)
            iqtype = 4
          endif

c    -------- Evaluate test statistic wrt threshold ---------------------
          iPeak = 0
          rPeak = 0.0
          do iknt = 2, numx - 1
            if(ts(iknt) .gt. rPeak) then
              iPeak = iknt
              rPeak = ts(iknt)
            endif  
          enddo  
                 
          if(iscrit(iopt) .eq. iXstat) then
c           Two Phase w/const slope critical value
            call tpr1table(mknt, critval)
          else  
c           Call lookup to get the 90%, 95%, and 97.5% critical values for
c            Tmax or Fmax test for a full segment
            call lookup(mknt,crit90,crit95,crit975,iscrit(iopt))
            if(isnht .eq. 1) then
              critval = crit975
            else if(isnht .eq. 5) then
              critval = crit95
            else    
              critval = crit95
            endif  
          endif  

          if(iPeak .le. 0) then
            if(idebug .ge. 1)
     *        write(6,'(a," ",a,"            No found peaks ",i4,i3,
     *        " to ",i4,i3)')subnet(1),otag,iyr1,imth1,iyr2,imth2
            StatTest = homog
            if(isplit .eq. 0) then
              if(idebug .ge. 2) then 
                call imo2iym(iyrb, imthb, iyrstat)
                write(6,'(a," ",a," Compress 1 out peak at ",2i5," ")')
     *            subnet(1), otag, iyrb, imthb
              endif
c             for merging, this is a change (collapse)
              goto 200
            else
c             for parsing, this is NOT a change
              goto 800
            endif  
          endif  
     
c         test the peak stat against the critical value
          if(rPeak.lt.critval) StatTest = homog
          curstat = rPeak
          end1 = qx(iPeak)
          call imo2iym(iyrb, imthb, end1)

c         Fragment First if either homog or inhomog
          if(iFirst .eq. 1) then
c           force first to split if homog, to test shorter segments
            if(StatTest .eq. homog) StatTest = inhomog
            if(idebug .ge. 2) write(6,'(a, "-", a, " ", a, 
     *        "             FIRST series ", i4, i3, " to ", i4, i3,
     *        " | at ", i4, i3, " ts: ", f7.2, a9, f6.2)')
     *        subnet(1), subnet(2), otag, iyr1, imth1, iyr2, 
     *        imth2, iyrb, imthb, curstat, limsense, critval
            goto 300
          else if(isplit .eq. 0) then
            if(StatTest .eq. homog)then
c             in a merge pass, collapse a homog chgpt
              if(idebug .ge. 2) write(6,'(a, "-", a, " ", a,
     *          " Compress 2 out peak at  ", i4, i3, " | ",
     *          f7.2, a, f7.2, " ")') subnet(1), subnet(2),
     *          otag, iyrb, imthb, curstat, limsense, critval
              goto 200
            else  
c             in a merge pass, leave an inhomog segment alone
              if(idebug .ge. 2) write(6,'(a, "-", a, " ",a,
     *          " Peak kept in merge at   ", i4, i3, " | ", 
     *          " ts: ", f7.2, a9, f6.2)')subnet(1), subnet(2),
     *          otag, iyrb, imthb, curstat, limsense, critval
              goto 800
            endif
          else
            if(StatTest .eq. homog) then
c             in a split pass, leave a homog segment alone
              if(idebug .ge. 2) write(6,'(a, "-", a, " ", a, 
     *          "       Homogeneous series ", i4, i3, " to ", i4, i3,
     *          " | at ", i4, i3, " ts: ", f7.2, a9, f6.2)')
     *          subnet(1), subnet(2), otag, iyr1, imth1, iyr2,
     *          imth2, iyrb, imthb, curstat, limsense, critval
              goto 800
            else
c             in a split pass, fragment an inhomog segment
              if(idebug .ge. 2) write(6,'(a, "-", a, " ", a,
     *          " Inhomogeneity for series ", i4, i3, " to ", i4, i3, 
     *          " | at ", i4, i3, " ts: ", f7.2, a9, f6.2)') 
     *          subnet(1), subnet(2), otag, iyr1, imth1, iyr2, 
     *          imth2, iyrb, imthb, curstat, limsense, critval
              goto 300
            endif  
          endif  

C         This is the ONLY parsing change point! These must NOT be
c         added back into the orginal chgpt array, but kept in an
c         array of their own
  300     iFirst = 0
          ichange = ichange + 1

c         update expansion year series array and indices
          mexpn = mexpn + 1
          do ichg = mexpn, iexy2+1, -1
            iexyr(ichg) = iexyr(ichg-1)
            iexmod(ichg) = iexmod(ichg-1)
            iexstns(ichg) = iexstns(ichg-1)
          enddo 
c         set the end of the series to the highest peak
          iexyr(iexy2) = end1
          iexmod(iexy2) = iqtype
          iexstns(iexy2) = 0
          iexy2 = iexy2 + 1
          iexy1 = iexy1 + 1
          goto 800
        
        else if(ioptseg .lt. 0 .or. iscrit(iopt) .eq. iBstat) then
        
c     -------------------------  run the BIC  -----------------------
          end1 = inhyr(indy1+1)
          call imo2iym(iyrb, imthb, end1)
          do ix = 1, numx
            if(end1 .lt. qx(ix)) then
              iqob1 = ix - 1
              goto 30
            endif
          enddo
        
c         determine the optimum Bayesian Info Criteria for found station/chgpt
   30     iyrstat = iqob1
          if(idebug .ge. 2)
     *      print *,' Entering MINBIC:',iyr1,imth1,iyrb,imthb,iyr2,imth2
          call minbic(2, qx, qy, iqob1, numx, critval, curstat,
     *      qmin, toff, rmuq, rslpq, rsseq, inqtype, iqtype, mknt2, 
     *      ifail)

c ------- tests for "break slope" model inconclusive, see versions before
c            v20e for brkpt model (=8)

c         save BIC data
          inhmod(indy1+1) = iqtype
          offinh(indy1+1) = toff
          sseseg(indy1+1) = rsseq
          segslp(1,indy1+1) = rslpq(1)
          segslp(2,indy1+1) = rslpq(2)
          call imo2iym(iyrb2, imthb2, end1 + 1)
          if(idebug .ge. 1) write(6,'( a, "-", a, " BIC: ", 
     *      2(2(i5, i2.2, i5), " : "), 3f7.2, 2f7.3, 3i5)')
     *      subnet(1),subnet(2), iyr1, imth1, iob1, iyrb, imthb, end1, 
     *      iyrb2, imthb2, end1 + 1, iyr2, imth2, iob2, curstat,  
     *      critval, toff, rslpq, mknt2, iqtype
            
          if(iqtype .eq. 2) then
            StatTest = homog
c           compress MINBIC segment here ONLY IF an SLR model is returned
c            goto 200
c           Because the BIC SLR0 test is less sensitive than the SNHT TPR0
c             model, ONLY set the SLR1 model to homogeneous AFTER THE FACT
c             Therefore, DO NOT collapse chgpt if SLR model - v21b
            goto 800
          else
            StatTest = inhomog
            goto 800
          endif

        else
          write(6,*)' Unknown statistical option in MLRTest, stopping'
          stop  
        endif    
     
c       collapse the breakpoint array
  200   do ichg = indy1+1, mindy-1
          inhyr(ichg) = inhyr(ichg+1)
          inhmod(ichg) = inhmod(ichg+1)
          inhstns(ichg) = inhstns(ichg+1)
          offinh(ichg) = offinh(ichg+1)
          sseseg(ichg) = sseseg(ichg+1)
          segslp(1,ichg) = segslp(1,ichg+1)
          segslp(2,ichg) = segslp(2,ichg+1)
        enddo
        mindy = mindy - 1
        indy2 = indy2 - 1
        indy1 = indy1 - 1

        ichange = ichange + 1
          
c ----- Update first and last chgpt date pointers for next segment -------
  800   indy2 = indy2 + 1
        indy1 = indy1 + 1
        iexy2 = iexy2 + 1
        iexy1 = iexy1 + 1
  810 enddo  ! end of series segmenting loop

  998 if(isplit .eq. 1 .and. ichange .gt. 0) then
c       repopulate the incoming array if this is a split pass with changes
        mindy = mexpn
        do ichg = 1, mexpn
          inhyr(ichg) = iexyr(ichg)
          inhmod(ichg) = iexmod(ichg)
          inhstns(ichg) = iexstns(ichg)
c          offinh(ichg) = offiex(ichg)
c          sseseg(ichg) = ssesex(ichg)
c          segslp(1,ichg) = segsex(1,ichg)
c          segslp(2,ichg) = segsex(2,ichg)
        enddo 
      endif          

  999 return
      end

c     =======================================================================
      subroutine standard(rRaw,rStd,nx1,nx2,iTotlObs)
c-----------------------------------------------------------------------
c     Standardize rRaw from nx1 to nx2, output results in rStd
c-----------------------------------------------------------------------      
      INCLUDE 'inhomog.parm.mthly.incl'  
        
      dimension rRaw(iTotlObs), rStd(iTotlObs)

      rSum = 0.0
      rNum = 0.0
      do i=nx1,nx2
        if(rRaw(i) .gt. amiss+1) then
          rSum = rRaw(i) + rSum
          rNum = rNum + 1
        endif  
      enddo
      
      rMean = rSum / rNum
      rVarSumm = 0.0
      do i = nx1,nx2
        if(rRaw(i).gt.amiss+1) rVarSumm = rVarSumm + (rRaw(i)-rMean)**2
      enddo
      
      rsqVar = sqrt(rVarSumm / (rNum-2))
      
      do i = nx1,nx2
        if(rRaw(i) .gt. amiss+1) then
          rStd(i) = (rRaw(i) - rMean) / rsqVar
        else
          rStd(i) = amiss
        endif    
      enddo
                
      return
      end

C     ******************************************************************
C     *  Subroutine acresid.
C     *  This subroutine obtains the autocorrelations of the residuals
C     ******************************************************************

      subroutine acresid (nr,resid,dwstat,alag1)
      INCLUDE 'inhomog.parm.mthly.incl'  

      dimension ac(2)
      real resid(nmo)
      real*8 z(nmo)

      do 100 i=1,nr
        z(i) = resid(i)
  100 continue
  
c     Use Claude Duchon's lag autocorrelation routine
      call uacovf(z, nr, amean, var, ac, 2)
      alag1 = ac(1) / var
 
      return
      end
 
