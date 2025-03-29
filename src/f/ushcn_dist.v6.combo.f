c***********************************************************************
c                          program normals_dist_2000.f
c                       from the ushcn program hcn_dist.f
c
c date of last change:  18 jan 2001
c 
c version     modification                                     date    *
c -------     ------------                                  ---------- *
c  v6.combo   Fixed two issues 
c               1) composites were not excluded in a USHCN station (fixed)
c               2) neighbor distance & ID pointer array needed to be
c                   re-initialized for each candidate (fixed)
c                   Also required changes in GHCNMv3.sh & USHCNv2.5.sh
c
c  v5a .combo  Retro-upgrade from solution in Rural-Urban study
c              Concatenated candfile & netfile into the metafile
c               to assist in recursion with ushcn_corr        23 aug 2011
c
c  v5.combo   includes skyline & attempt to keep both 6 & 11 char ID
c               w/1flg & 3flg resp.                           13jun11
c
c  v5         for the merge of USHCNv2 + composites from the 20100306
c               GHCND2M project plus the GHCNV2.5 project
c
c  v4         use GHCN-D metadata station format as reference 19 may 2009
c
c  2004.v3    slight change in format and modified test for enough
c                stations                                   18 Dec 2008
c
c  2004.v2    skip the composite & candidate USHCN in neighbors
c                                                           15 Jul 2005
c  
c  2004       catch the 100 nearest neighbors               6 apr 2005
c
c  2000       version for the linux server                   1 feb 2005
c
c   1.1       sun workstation version                       10/18/1995 *
c                                                                      *
c   1.0       original                                                 *
c                                                                      *
c usage:  hcn_dist                                                     *
c                                                                      *
c description:  this program determines the "numsrt" nearest neighbors *
c               to each normals candidate station.  this program is set up *
c               to handle up to "numstn" hcn stations, and only sorts  *
c               for the "numsrt" nearest neighbors.                    *
c               the major differences between this version and the 
c               hcn version are 
c             1) this routine uses seperate lists for the candidates 
c                   and the network neighbors
c             2) reads from lat,lon lists instead of the ushcn station
c                   history file
c                                                                      *
c requirements:                                                        *
c                                                                      *
c   input file:  candidate lat,lon list
c                prospective neighbor ll list
c                                                                      *
c         **************************************************************
c notes:  * changes to any parameters must be made to the same         *
c         * parameters in any of the functions and/or subroutines.     *
c         **************************************************************
c                                                                      *
c         changed from calculating distances and sorting one candidate *
c         station at a time to doing all stations at one time.         *
c         - d. bowman:  01/21/94                                       *
c
c         changed back to doing one station at a time for flexibility
c                                                                      *
c references:  sun fortran reference guide, rev. a, 22 feb 1991        *
c                                                                      *
c              sun fortran user's guide, rev. a, 22 feb 1991           *
c                                                                      *
c              understanding fortran - michel boillet - 1978           *
c              isbn 0-8299-0355-0                                      *
c                                                                      *
c results:  produces file containing number of hcn stations, ids of    *
c           candidate station and nearest "numsrt" neighbors, pointers *
c           to nearest "numsrt" neighbors, and distances from          *
c           candidate to nearest "numsrt" neighbors.                   *
c                                                                      *
c   output file:  normals_distances
c                                                                      *
c parameters:                                                          *
c                                                                      *
c   numsrt     number of nearest neighbors to sort                     *
c                                                                      *
c   numstn     number of hcn stations arrays can hold                  *
c                                                                      *
c functions:  gcd                                                      *
c                                                                      *
c subroutines:  sortn                                                  *
c                                                                      *
c variables:                                                           *
c                                                                      *
c   dist       holds distances between candidate station and the
c              neighbor stations within "degsqr" degrees 
c   (numstn,numstn)                                                    *
c
c   degsqr     initially set to the inideg parameter (10 at present)
c              is the initial pass for the nieghbor stations
c
c   deginc     if not enough neighbor stations are withing degsqr then
c              it is incremented by this amount (2 at present)
c                                                                      *
c   fildir     directory where files are located                       *
c                                                                      *
c   i          loop counter                                            *
c                                                                      *
c   j          loop counter                                            *
c                                                                      *
c   lat        latitude                                                *
c   (numstn)                                                           *
c                                                                      *
c   latdeg     latitude degrees                                        *
c                                                                      *
c   latmin     latitude minutes                                        *
c                                                                      *
c   lon        longitude                                               *
c   (numstn)                                                           *
c                                                                      *
c   londeg     longitude degrees                                       *
c                                                                      *
c   lonmin     longitude minutes                                       *
c                                                                      *
c   nstn       number of hcn stations                                  *
c                                                                      *
c   ptr        index of stations sorted by closest distance of         *
c   (numstn,numstn)     "numsrt" nearest neighbors                     *
c                                                                      *
c   rcount     number of records read from station history file        *
c                                                                      *
c   stnid      station ids:       1 => station id                      *
c   (numstn,2) numstn stations    2 => position of last history record *
c                                                                      *
c   stnidr     station id from history record                          *
c                                                                      *
c***********************************************************************

      program hcndis
      include 'prehomog.parm.incl'  

c     maxnets moved to prehomog.parm.incl - 12july2012
      integer distini, distinc, distmax, ncomp
      parameter (distini = 1000, lodist = 5,
     *  distinc = 500, distmax = 5000, ncomp = 4)

      integer candnets
      parameter (candnets = 2000)
      
      real gcd,clat(candnets),clon(candnets),curlat,curlon
      real lat(maxnets),lon(maxnets)
      real dist(maxnets)
      integer ptr(maxnets), dneigh(10)
      character*11 cstn(candnets), stnid(maxnets), curstn
      character*11 comp(ncomp,candnets), iparm, outid(maxnets)

c     command line variables
      character*256 argv, metafile, outfile
      integer cantype
c     input directory for default files (overridden with -i dirname)
c      originally - /'/sd4/dbowman/ushcn/files/'/
      integer iargc, narg
      
      print *,'Initial & Increment Distance: ',distini, distinc

c     initialize variables
c     use GHCN-D format as i/o reference 19 may 2009 cw

c     assume candidate format and neighbors are hcn
      cantype = 1

c     the distance from a station to itself is 0.0
      cdist = 0.0
      
      nc = 0
      idebug = 0
      
      metafile = ''
      outfile = ''

      print *,' generate nearest neighbor list - normals_dist_2004.v2'
      print *,' assume I/O in GHCN-D station metadata format'

c     begin command line code      
c     get the number of command line arguments
      narg = iargc()
      iarg = 1
c     go thru remaining command line arguments - keep the valid ones
c        and ignoring the undefined ones.
      do while(iarg .le. narg)
        call getarg(iarg, argv)
        iarg = iarg + 1
        if(argv .eq. '-m') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          metafile = argv
          print *, ' Meta cand-net station file :', metafile
        else if(argv .eq. '-o') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          outfile = argv
          print *, ' candidate w/network file :', outfile
        else if(argv .eq. '-u') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i4)') nc
          print *, ' Candidate-Neighbor merge - nc:',nc
        else if(argv .eq. '-h') then
          cantype = 0
          print *, ' enabled - non-hcn candidate file'
        else if(argv .eq. '-d') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i1)') idebug
          print *, ' debug level:', idebug
        else
          print *, ' unknown argument :', argv, ': skipping'
        endif
      end do
      
      if(metafile.ne.'' .and. outfile.ne.'') 
     *  goto 11
      
      print *,' normals network neighborhood generation routine'
      print *,
     *  ' usage: ushcn_dist.v5a.combo -m metafile -o outfile'
      print *,'   required parameters'
      print *,'     -u nc         number of candidate stations'
      print *,'    input files ---'
      print *,'     -m metafile   meta cand-net stations file'
      print *,'    output files ---'
      print *,'     -o outfile   candidate-network distance file'
      print *,'   optional parameters'
      print *,'     -h           non-hcn type candidates'
      print *,'     -d           debug log level'
      stop

   11 continue
      if(nc .eq. 0) print *, ' No Candidates in Metadata file'

C   ------------------ Peter's World ----------------
      
      call getenv('NEIGH_CLOSE', iparm)
      read(iparm, '(i3)') ndist
      print *,' Number of closest neighbors output', ndist
      if(ndist .eq. 0) then
        print *,' NEIGH_CLOSE not set: see combination!!!'
        stop 888
      endif  
      numsrt = ndist - 1

      print *, ' Read network station file'
      open(10,file=metafile(1:lnblnk(metafile)),status='old',err=235)

      print *,' Get list of station ids, lats & lons'
      do istn = 1, maxnets
        if(istn .le. nc) then
          if(cantype .eq. 0) then
c           read station list in GHCN-D format
            read(10,1000,end=125,err=240) cstn(istn),clat(istn),
     *        clon(istn)
            do ic = 1, ncomp
              comp(ic,istn) = '-----------'
            enddo  
          else
c           reading the candidates and composites in hcn type format
c           IF NCOMP CHANGES - THEN CHANGE THE NUMBER OF COMP ID'S READ 
            read(10,2000,end=125,err=245) cstn(istn), clat(istn), 
     *        clon(istn), comp(1,istn), comp(2,istn), comp(3,istn),
     *        comp(4,istn)
 2000       format(a11,f9.4,f10.4,43x,4(a11,1x))
            if(idebug .gt. 0) 
     *        print *,cstn(istn),' ',comp(1,istn),' ',comp(2,istn),' ',
     *          comp(3,istn),' ',comp(4,istn)
          endif
        else
          nstn = istn - nc
c         read station list in GHCN-D format
          read(10,1000,end=20,err=250) stnid(nstn),lat(nstn),lon(nstn)
 1000     format(a11, f9.4, f10.4)
        endif
      end do

      print *,'too many network stations - currently maxnets:',maxnets
      go to 360

c     save number of stations
   20 nstn = nstn - 1
      print *,' Number of Candidate stations read: ',nc
      print *,' Number of Network station read: ', nstn

c     close input file
      close(10,err=250)

c     open output file
      open(16,file=outfile,err=260)

c     Start with the Reference (USHCN type threaded) network
      do istn = 1, nc
c        print *, cstn(istn)
        if(clat(istn) .gt. 90. .or. clat(istn) .lt. -90. .or.
     *      clon(istn) .gt. 360. .or. clon(istn) .lt. -180.) then
          print *,' cstn: ', cstn(istn), ' lat/lon undefined: ', 
     *      clat(istn), clon(istn)
          go to 120
        endif  
          
c       compute distances between stations
        hidist = distini
        idn = 1
	
   60   is = 0

c       Initialize neighbor pointer and distance arrays
        ptr = 0
        dist = 0
        do 70 i = 1,nstn
c          print *, cstn, ' ', stnid(i)
c         The logic to exclude composite stations from the candidate
c           network list became simpler when the file format was 
c           updated to include WBAN ID's 
c         The COOP-WBAN ID of the candidate station is first composite ID
c         skip the composite station in the reference network
          do ic = 1,4
            if(stnid(i) .eq. comp(ic,istn)) then
              if(idebug .gt. 0) 
     *          print *,'Skip comp:',cstn(istn),' ',comp(ic,istn)
              goto 70
            endif  
          enddo
          adist = gcd(lat(i),lon(i),clat(istn),clon(istn))
c          print *, adist
          if(adist .lt. hidist) then
            is = is + 1
            dist(is) = adist
            ptr(is) = i
c            print *,stnid(i), adist, is
          endif
          if(idebug .gt. 0 .and. adist .lt. lodist) then
            print *,'Warning low dist ', adist, ' comp:',
     *        cstn(istn),' ',comp(ic,istn)
          endif  
   70   continue
   
c       see if there were enough stations, if not, crank up cut off
        dneigh(idn) = is
        if(is .lt. numsrt .and. hidist .lt. distmax) then
          hidist = hidist + distinc
          idn = idn + 1
          go to 60
        endif  

c        write(*,'(a, 10i5)') cstn(istn), (dneigh(i),i=1,idn)

c       sort distance and id arrays
        call sortn(dist,is,ptr)
        
c       output station ids, pointers, and distances
        do j = 1, numsrt
          if(ptr(j) .eq. 0) then
            outid(j) = "           "
          else
            outid(j) = stnid(ptr(j))
          endif
        enddo      
        
        write(16,90,err=260) cstn(istn),(outid(j),j=1,numsrt)
   90   format(500(a11,1x))

        write(16,100,err=260) istn,(ptr(j)+nc,j=1,numsrt)
  100   format(500(i11,1x))

        write(16,110,err=260) cdist,(dist(j),j=1,numsrt)
  110   format(500(f11.1,1x))
  120   continue
      end do

c     ----- Now go through the Neighbors, adding to the cand-neigh list -----
  125 do istn = 1, nstn
        curstn = stnid(istn)
        curlat = lat(istn)
        curlon = lon(istn)

c        print *, cstn
        if(curlat .gt. 90. .or. curlat .lt. -90. .or. curlon .gt. 360.
     *     .or. curlon .lt. -180.) then
          print *,' curstn: ', curstn, ' lat/lon undefined: ',
     *      curlat, curlon
          go to 220
        endif  
          
c       compute distances between stations
        hidist = distini
        idn = 1
        
  160   is = 0

c       Initialize neighbor pointer and distance arrays
        ptr = 0
        dist = 0
        do 170 i = 1,nstn
c         skip the candidate station in the reference network
          if(stnid(i) .eq. curstn) go to 170
          adist = gcd(lat(i),lon(i),curlat,curlon)
          if(adist .lt. hidist) then
            is = is + 1
            dist(is) = adist
            ptr(is) = i
c            print *,stnid(i), adist, is
          endif  
  170   continue
   
c       see if there were enough stations, if not, crank up cut off
        dneigh(idn) = is
        if(is .lt. numsrt .and. hidist .lt. distmax) then
          hidist = hidist + distinc
          idn = idn + 1
          go to 160
        endif  

        if(idebug .gt. 0) 
     *    write(*,'(a, 10i5)') curstn, (dneigh(i),i=1,idn)

c       sort distance and id arrays
        call sortn(dist,is,ptr)
        
c       output station ids, pointers, and distances
        do j = 1, numsrt
          if(ptr(j) .eq. 0) then
            outid(j) = "           "
          else
            outid(j) = stnid(ptr(j))
          endif
        enddo      

        write(16,90,err=260) curstn,(outid(j),j=1,numsrt)

        write(16,100,err=260) istn+nc,(ptr(j)+nc,j=1,numsrt)

        write(16,110,err=260) cdist,(dist(j),j=1,numsrt)
  220   continue
      end do

c     close output file
  225 close(16,err=260)

c     notify user of successful completion
      write(6,230)
  230 format('hcn_dist has completed successfully!')

c     skip error messages and end program
      go to 380

c     error messages and aborts
  235 call perror ( 'cannot open: ' // metafile )
      go to 360

  240 call perror ( 'cannot read(GHCN-D): ' // metafile )
      go to 360

  245 call perror ( 'cannot read(HCN): ' // metafile )
      go to 360

  250 call perror ( 'cannot read(stnlist): ' // metafile )
      go to 360

  260 call perror ( 'cannot open/read/close: ' // outfile )

  360 write(6,370)
  370 format('program is aborting.')

  380 stop
      end

c***********************************************************************
c end of program hcndis.                                               *
c***********************************************************************


c***********************************************************************
c                             function gcd                             *
c                                                                      *
c date of last change:  21 january 1994                                *
c                                                                      *
c modifier:  david bowman                                              *
c                                                                      *
c version     modification                                     date    *
c -------     ------------                                  ---------- *
c   1.1       sun workstation version                       01/21/1994 *
c                                                                      *
c   1.0       original                                                 *
c                                                                      *
c usage:  sortn(double precision dist(numstn,numstn), integer nstn,    *
c               integer nsort, integer ptr(numstn,numstn))             *
c                                                                      *
c description:  this subroutine determines and sorts the nsort nearest *
c               neighbors for each candidate stations.                  *
c                                                                      *
c notes:  changed from sorting one candidate station at a time to      *
c         sorting all candidates at one time.  subroutine now sorts    *
c         distance array as well as pointer array, and sorts only for  *
c         the nsort nearest neighbors. - d. bowman:  01/21/94          *
c        
c         changed back to one sort at a time, and sorts the whole 
c         array coming in.... 
c                                                                      *
c results:  sorts the distance and pointer arrays for the nsort        *
c           nearest neighbors for each candidate station.              *
c                                                                      *
c parameters:                                                          *
c                                                                      *
c   numstn     number of hcn stations for arrays to hold               *
c                                                                      *
c variables:                                                           *
c                                                                      *
c   dist       holds distances between all hcn stations                *
c   (numstn,numstn)                                                    *
c                                                                      *
c   distmp     holds distance value when sorting                       *
c                                                                      *
c   i          loop counter                                            *
c                                                                      *
c   j          loop counter                                            *
c                                                                      *
c   k          loop counter                                            *
c                                                                      *
c   min        index of minimum value                                  *
c                                                                      *
c   nsort      number of stations to sort                              *
c                                                                      *
c   nstn       number of hcn stations                                  *
c                                                                      *
c   ptr        index of stations sorted by closest distance of nsort   *
c   (numstn,numstn)     nearest neighbors                              *
c                                                                      *
c   ptrtmp     holds pointer value when sorting                        *
c                                                                      *
c***********************************************************************

      subroutine sortn(dist,nstn,ptr)

      real dist(nstn)
      integer ptr(nstn)
      
      l = nstn/2 + 1
      ir = nstn
      
   10 continue
      if(l .gt. 1) then      
        l = l - 1
        rdist = dist(l)
        rptr = ptr(l)
      else
        rdist = dist(ir)
        rptr = ptr(ir)  
        dist(ir) = dist(1)
        ptr(ir) = ptr(1)
        ir = ir - 1
        if(ir .eq. 1) then
          dist(1) = rdist
          ptr(1) = rptr
          return
        endif
      endif
      i = l
      j = l+l
   20 if(j .le. ir) then
        if(j .lt. ir) then
          if(dist(j) .lt. dist(j+1)) j = j + 1
        endif
        if(rdist .lt. dist(j)) then
          dist(i) = dist(j)
          ptr(i) = ptr(j)
          i = j
          j = j + j
        else
          j = ir + 1
        endif
        go to 20
      endif
      
      dist(i) = rdist
      ptr(i) = rptr
      go to 10
      end    
 
c***********************************************************************
c end of subroutine sortn.                                             *
c***********************************************************************
