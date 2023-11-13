c     Copied from Skyline.0.9.0 (355)
c    Ver     Date     Description
c
c     1   04nov2010   Initial implementation of Skyline Matrix solution
c                       to PHA work arrays

c     =========================== Functions =================================
c     Initially Allocate skyline main work arrays in restart module
c     allocsky()

c     Calculate skyind offset for istn & iyear
c     work2sky(istn, imo, iyear, imth, indsky, ierropt)

c     Calculate classic imo offset for istn, indsky
c     sky2work(istn, imo, iyear, imth, indsky, ierropt)

c     Return first year, sky index & classic imo for istn
c     firstsky(istn, imo, iyear, imth, indsky)

c     Return last year, sky index & classic imo for istn
c     lastsky(istn, imo, iyear, imth, indsky)

c     Return year/month for sky index, first index, first year
c     iskymo2iym(iy,im,iskymo,isky1,iyr1)

c     Return sky index for year/month, first index, first year
c     iym2iskymo(iy,im,iskymo,isky1,iyr1)


c     =======================================================================

      subroutine allocsky()
      
c     Reference restart.mod.f95 module for work array allocation
      use restart

c     This include file contains the system parameters
      INCLUDE 'inhomog.parm.mthly.incl'
      INCLUDE 'inhomog.comm.mthly.incl'
      
c     First command line parameter MAXSKY gives the sky index size of arrays
c       see inhomog.comm.mthly.incl
c     skyear is the begin/end years of each of the stations
      allocate(skyear(1:2, 1:maxstns))
c     skyrind is the YEAR-MONTH offset of each station in the SKYLINE arrays
      allocate(skyind(1:maxstns+1))

c     Initially Allocate skyline main work arrays in restart module
c      Due to the way all of the code has been created for the PHA
c      it may be easier to put all three variables together (yr-mo,stn)
      maxskymo = 12 * maxsky

      allocate(orig(1:maxskymo))
      allocate(temp(1:maxskymo))
      allocate(tflg(1:maxskymo))
      allocate(nhits(1:maxskymo))
      
      allocate(nfound(1:maxnstns, 1:maxskymo))
      allocate(nspan(1:maxnstns, 1:maxskymo))
      allocate(ndelete(1:maxnstns, 1:maxskymo))

      allocate(schgpt(1:maxskymo))
      allocate(zchgpt(1:maxskymo))
      allocate(nchgpt(1:maxskymo))
      allocate(ntest(1:maxskymo))

      return
      end

c     =======================================================================

      subroutine work2sky(istn, imo, iyear, imth, indsky, ierropt)

c     For:
c       istn = station index 
c       imo = year-mth index from PHA begin year (begyr)
c     Return 
c       iyear = year
c       imth = month
c       indsky = sky (station-year) index
c     Error handling
c       ierropt = 0 
c           return value indsky = -1 :== imo before station POR
c                        indsky = -2 :== imo after station POR
c       ierropt = 1
c           print error and exit

c     Additional functionality is that if the Classic IMO = -1
c       then istn, iyear, imth is given and the skyind offset is returned

c     Reference restart.mod.f95 module for work array allocation
      use restart

c     Calculate the offset of year and month from IMO
c        (from Classic) IMO = (iyear - Begyr)*12 + imth
c     Example: for imo = 1263 & Begyr = 1800
c        call IMO to year, month conversion routine imo2iym 
c        iyear = 1906, imth = 3
      if(imo .ge. 0) then
        call imo2iym(iyear, imth, imo)
      endif  
      
c     Retreive begin year (start of POR) for station from SKYEAR
c     Retreive skyline offset for begin year from SKYRIND
c     Calculate skyind offset for istn & iyear
c       Example: istn = 3, skyind(istn) = 590, skyear(istn) = 1899
c         that is, 3rd station starts in year 1899 at sky index of 590
c      nyrs = (skyind(istn + 1) - skyind(istn)) / 12
      nyrs = (skyear(2,istn) - skyear(1,istn)) + 1

c     add some QC - 
      if(iyear .lt. skyear(1,istn)) then
        if(ierropt .eq. 0) then
c         return -1 if iyear is before station begin
          indsky = -1
        else
          write(6,'("IMO(year,mth) ", i4.4, "(", i4.4, ",", i2.2,
     *     ") is before Station ", i5, " Begin:", i4.4)') imo, iyear,
     *     imth, istn, skyear(1,istn)
          stop
        endif  
      else if(iyear .gt. skyear(2,istn)) then
        if(ierropt .eq. 0) then
c         return -2 if iyear is greater than station end
          indsky = -2
        else
          write(6,'("IMO(year,mth)", i4.4, "(", i4.4, ",", i2.2,
     *     ") is after Station ", i5, "  End:", i4.4)') imo, iyear,
     *     imth, istn, skyear(2,istn)
          stop
        endif
      else
c       a positive return is valid
        indsky = skyind(istn) + (iyear - skyear(1,istn))*12 + (imth-1)
      endif  

      return
      end

c     =======================================================================

      subroutine sky2work(istn, imo, iyear, imth, indsky, ierropt)

c     For:
c       istn = station index 
c       indsky = sky (station-year) index
c     Return 
c       iyear = year
c       imth = month
c       imo = year-mth index from PHA begin year (begyr)
c     Error handling
c       ierropt = 0 
c           return value imo = -1 :== imo before PHA start year
c                        imo = -2 :== imo after PHA end year
c       ierropt = 1
c           print error and exit

c     This include file contains the system parameters
      INCLUDE 'inhomog.parm.mthly.incl'

c     Retreive start of POR year,month (iyr1, imth1) for station from SKYEAR
c              skyline (isky1) offset for begin year from SKYRIND
c              classic offset (imo1) for station begin
      call firstsky(istn, imo1, iyr1, imth1, isky1)
      
c     difference between isky1 and indsky is the offset for imo1 to imo
      iskyoff = indsky - isky1
      imo = imo1 + iskyoff

c     Calculate the offset of year and month from IMO
c        (from Classic) IMO = (iyear - Begyr)*12 + imth
      call imo2iym(iyear, imth, imo)

c     add some QC - 
      if(iyear .lt. begyr) then
        if(ierropt .eq. 0) then
c         return -1 if iyear is before station begin
          imo = -1
        else
          write(6,'("INDSKY(year,mth)",i8.8,"(",i4.4,",",i2.2,
     *     ") is before PHA Begin")') indsky, iyear, imth
          stop
        endif  
      else if(imo .gt. nmo) then
        if(ierropt .eq. 0) then
c         return -2 if iyear is greater than station end
          imo = -2
        else
          write(6,'("INDSKY(year,mth)",i5.5,"(",i4.4,",",i2.2,
     *     ") is after PHA End")') indsky, iyear, imth
          stop
        endif
      endif  

      return
      end

c     =======================================================================

      subroutine firstsky(istn, imo, iyr1, imth, isky1)

c     For the istn station, return all information about the first datum

c     Reference restart.mod.f95 module for work array allocation
      use restart

c     First month of all stations in Sky is ALWAYS Jan!
      imth = 1
c     First year of station comes from Readnet
      iyr1 = skyear(1,istn)
c     yr-mo index (old style)
      call iym2imo(iyr1, imth, imo)
c     Sky index of first year-month of istn
      isky1 = skyind(istn)
      return
      end

c     =======================================================================

      subroutine lastsky(istn, imo, iyr2, imth, isky2)

c     For the istn station, return all information about the last datum

c     Reference restart.mod.f95 module for work array allocation
      use restart

c     Last month of all stations in Sky is ALWAYS Dec!
      imth = 12
c     Last year of istn data
      iyr2 = skyear(2,istn)
c     yr-mo index (old style)
      call iym2imo(iyr2, imth, imo)
c     Sky index of last year-month of istn
      isky2 = skyind(istn) + ((iyr2-skyear(1,istn)+1)*12) - 1
      return
      end

c     =======================================================================

      subroutine iskymo2iym(iy,im,iskymo,isky1,iyr1)

c     Convert Sky month,station indx to year,month
c     NOTE: monthly time res ONLY!!!

c     Month offset from beginning of station index
      imoff = iskymo - isky1 + 1
c     Year of iskymo
      iy = int((imoff-1)/12) + iyr1
c     Month of iskymo
      im = mod(imoff, 12)
      if(im .eq. 0) im = 12
      return
      end
      
c     =======================================================================

      subroutine iym2iskymo(iy,im,iskymo,isky1,iyr1)

c     Convert year,month to Sky month,station indx
c     NOTE: monthly time res ONLY!!!

c     Offset from the beginning of station index to month 
      iskyoff = (iy-iyr1)*12 + im
c     Sky Index of year,month,station
      iskymo = iskyoff + isky1 - 1
      return
      end
      
