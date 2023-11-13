!   Copied from Skyline.0.9.0 (355)
!      Moved largest display arrays from ucpmonthly.v24p.f - confirmfilt 
!        routine to an LF95 Module - 10apr2009 cw
      module confirmdisp
       
!     Following arrays - restructured for Skyline
!      From (maxstns, nmo) to (skyind(stnind))
      integer, dimension(:),   allocatable :: jhits
      integer, dimension(:),   allocatable :: shhits
      real,    dimension(:),   allocatable :: amps
      
!      From (nmo, maxstns) to (skyind(stnind))
      integer, dimension(:),   allocatable :: ifndshow

!      From (nmo, maxstns, ntype) to (ntype, skyind(stnind))
      integer, dimension(:,:),   allocatable :: itypshow

      end module confirmdisp
