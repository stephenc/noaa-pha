!      Moved largest arrays from inhomog.restart.mthly.incl 
!        to an LF95 Module - 9apr2009 cw
       module restart
       
!      This include file contains the parameters, variables and arrays 
!        required to restart the inhomogeneity testing process at 
!        specific entry points.

!     BE AWARE: THE ARRAYS IN THIS ROUTINE ARE DEPENDENT UPON INHOMOG.COMM.INCL

!     skyear & skyrind - SKYLINE matrix base arrays
!     skyear are the start/end year of each station
!     skyind are the skyline index (year,mth) of the start of the station
!       NOTE: start month is ALWAYS January
      integer, dimension(:,:), allocatable :: skyear
      integer, dimension(:), allocatable :: skyind

!     orig & tflg - Incoming monthly temperature and flags 
!      From (maxstns,  maxyears, 12) to (skyind(stnind))
      real, dimension(:), allocatable :: orig
      character(3), dimension(:), allocatable :: tflg

!     temp - Original temp observations work array
!      From (maxstns,  maxyears, 12) to (skyind(stnind))
      real, dimension(:), allocatable :: temp

!     nhits - accumulated chgpt hits from Split/Merge algorithm
!      From (maxstns,  maxyears, 12) to (skyind(stnind))
      integer(2), dimension(:), allocatable :: nhits

!     nfound & nspan - working arrays for the number of "found" inhomogeneities
!        and the "span" of year-months for each paired difference series
!       as of ucpmonthly.v7.f nfound is changed from integer to char*1
!       need to hold onto the total number of chgpt per paired set
!      From (maxstns, maxneigh, nmo) to (maxneigh, skyind(stnind))
      integer(1), dimension(:,:), allocatable :: nfound
      integer(1), dimension(:,:), allocatable :: nspan
      character(1), dimension(:,:), allocatable :: ndelete

!     sums and counts for adjustment amounts for "found" ihomogeneities.
!     nchgpt counts are included in addition to the nfound counts because 
!     a given station may not be a neighbor of its neighbor
!      From (maxstns, nmo) to (skyind(stnind))
      real, dimension(:), allocatable :: schgpt
      real, dimension(:), allocatable :: zchgpt
      integer(1), dimension(:), allocatable :: nchgpt
      integer(1), dimension(:), allocatable :: ntest
     
      end module restart
