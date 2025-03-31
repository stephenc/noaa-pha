module stratus

implicit none

! National Climatic Data Center (NCDC)

! Version 1.4, 8/12/2014

! Stratus is a climatological programming library written in
! Fortran 95/2003 with an emphasis in geo/spatial, math, stat, and time/date
! routines. 

! all routines are subroutines

! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC DOMAIN
! AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE.  THEY ARE FURNISHED 
! "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS INSTRUMENTALITIES, 
! OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY, EXPRESS OR IMPLIED, AS TO
! THE USEFULNESS OF THE SOFTWARE AND DOCUMENTATION FOR ANY PURPOSE. THEY
! ASSUME NO RESPONSIBILITY (1) FOR THE USE OF THE SOFTWARE AND DOCUMENTATION;
! OR (2) TO PROVIDE TECHNICAL SUPPORT TO USERS.

type string_info
  integer :: number_of_strings
  integer, allocatable, dimension(:) :: length
  character(len=100000), allocatable, dimension(:) :: value
  character(len=20), allocatable, dimension(:) :: class
end type string_info
integer, public, parameter :: END_OF_FILE=-1               ! Note: compiler dependent value
integer, public, parameter :: MEMORY_ERROR=0               ! Note: compiler dependent value
integer, public, allocatable, dimension(:) :: SPLIT_INDICES, SORT_INDICES
real, public, allocatable, dimension(:) :: GLON,GLAT
real, public, allocatable, dimension(:,:) :: POLY_X
real, public, allocatable, dimension(:,:) :: POLY_Y
integer, parameter :: SCREEN=6
integer, public, parameter :: dp = selected_real_kind(15,50)
real(dp), public, parameter :: PI=3.141592653589793

contains

!*******************************************************************************
                                                                             
subroutine binary_search(X,SEARCH_STRING,LOCATION)

character(len=*), dimension(:), intent(in) :: X
character(len=*), intent(in) :: SEARCH_STRING
integer, intent(out) :: LOCATION
integer :: LEFT, RIGHT, MIDDLE, N

! SUMMARY:
!
! perform a binary search through a 1-D array (X) of character strings, and
! return the LOCATION of the SEARCH_STRING. If SEARCH_STRING not found, LOCATION=0

LOCATION=0; N=size(X); LEFT=lbound(X,1)
if (N < LEFT) return
RIGHT=N                           
do
  MIDDLE=(LEFT+RIGHT)/2                  
   if (SEARCH_STRING<X(MIDDLE) )then          
     if (LEFT>=RIGHT) EXIT       
     RIGHT=MIDDLE
  elseif (SEARCH_STRING>X(MIDDLE)) then         
    if (LEFT>=RIGHT) EXIT                    
    LEFT=MIDDLE+1 
  else                                   
    LOCATION=MIDDLE                        
    EXIT
  endif
enddo

end subroutine binary_search

!*******************************************************************************

subroutine correlation(X,Y,VALUE)

! Calculate the sample correlation coefficient

real, dimension(:), intent(in) :: X,Y
real, intent(out) :: VALUE
real(dp) :: rSumXY,rSumX,rSumY,rSSumX,rSSumY
integer :: ii

if (size(X)<2.or.size(Y)<2) stop "ERROR: stratus/correlation: X and/or Y array not large enough"
if (size(X)/=size(Y)) stop "ERROR: stratus/correlation: X array size not equal to Y array size"

rSumX=0.0
rSumY=0.0
rSSumX=0.0
rSSumY=0.0
rSumXY=0.0
do ii=1,size(X)
  rSumX=rSumX+X(ii)
  rSumY=rSumY+Y(ii)
  rSSumX=rSSumX+(X(ii)**2.0)
  rSSumY=rSSumY+(Y(ii)**2.0)
  rSumXY=rSumXY+(X(ii)*Y(ii))
enddo
if ( (rSSumX-(rSumX**2.0/real(size(x))))*(rSSumY-(rSumY**2.0/real(size(y)))) <= 0.0 ) then
  stop "ERROR: stratus/correlation: sqrt of negative number or divide by zero"
else
  VALUE=(rSumXY-((rSumX*rSumY)/real(size(X))))/sqrt((rSSumX-(rSumX**2.0/real(size(x))))*(rSSumY-(rSumY**2.0/real(size(y)))))
endif

end subroutine correlation

!*******************************************************************************

subroutine covariance(X,Y,VALUE,MISS,MIN_DATA)

real, intent(in) :: X(:)
real, intent(in) :: Y(:)
real, intent(out) :: VALUE
real, intent(in), optional :: MISS
integer, intent(in), optional :: MIN_DATA
real :: rSum,rC,rXMean,rYMean
integer :: ii

! SUMMARY:

! VALUE returns the covariance defined as:
!
! covariance = sigma[(x-x(bar))*(y-y(bar))] / n
!
! use optional MISS to filter missing data and optional MIN_DATA to set
! minimum missing data thresholds.
!
! If the number of non-missing X values < MIN_DATA then VALUE=MISS
! If one of MISS or MIN_DATA is present, then both must be present 

if (present(MISS).and..not.(present(MIN_DATA))) stop "ERROR: stratus/covariance: missing optional arg, MIN_DATA" 
if (present(MIN_DATA).and..not.(present(MISS))) stop "ERROR: stratus/covariance: missing optional arg, MISS"

if (present(MISS)) then
  VALUE=MISS
  if ((count(X(:)/=MISS)<MIN_DATA).or.(count(Y(:)/=MISS)<MIN_DATA)) return
  rSum=0.0
  rC=0.0
  call mean(X,rXMean,MISS=MISS,MIN_DATA=MIN_DATA)
  call mean(Y,rYMean,MISS=MISS,MIN_DATA=MIN_DATA)
  do ii=1,size(X)
    if (X(ii)==MISS.or.Y(ii)==MISS) cycle
    rSum=rSum+((X(ii)-rXMean)*(Y(ii)-rYMean))
    rC=rC+1.0
  enddo
  VALUE=rSum/rC
else
  rSum=0.0
  call mean(X,rXMean)
  call mean(Y,rYMean)
  do ii=1,size(X)
    rSum=rSum+((X(ii)-rXMean)*(Y(ii)-rYMean))
  enddo
  VALUE=rSum/real(size(X))
endif

end subroutine covariance

!*******************************************************************************
                                              
subroutine days_in_month(YEAR,MONTH,VALUE)

integer, intent(in) :: YEAR
integer, intent(in) :: MONTH
integer, intent(out) :: VALUE
integer :: iDays

! SUMMARY:
!
! VALUE returns the number of days in (MONTH,YEAR).
!

call days_in_year(YEAR,iDays)
if ((MONTH < 1) .or. (MONTH > 12)) stop "ERROR: stratus/days_in_month: MONTH out of range, 1 <= MONTH <= 12"
if ( (MONTH==1) .or. (MONTH==3) .or. (MONTH==5) .or. (MONTH==7) .or. (MONTH==8) .or. &
    (MONTH==10) .or. (MONTH==12) ) then
    VALUE = 31
endif
if (MONTH==2) then
  if (iDays==366) then
    VALUE = 29
  else
    VALUE = 28
  endif
endif
if ((MONTH==4) .or. (MONTH==6) .or. (MONTH==9) .or. (MONTH==11)) then
  VALUE = 30
endif

end subroutine days_in_month

!*******************************************************************************

subroutine days_in_year(YEAR,VALUE)

integer, intent(in) :: YEAR
integer, intent(out) :: VALUE

! SUMMARY:
!
! VALUE returns the number of days in YEAR
!
! YEAR must not be < 1600 or > 2300
!

VALUE=365
if (mod(YEAR,400)==0) then
  VALUE=366
else
  if (mod(YEAR,100)==0) then
    VALUE=365
  else
    if (mod(YEAR,4)==0) then
      VALUE=366
    else
      VALUE=365
    endif
  endif
endif

end subroutine days_in_year

!*******************************************************************************

subroutine degrees2radians(DEGREES,RADIANS)

real, intent(in) :: DEGREES
real, intent(out) :: RADIANS

! SUMMARY:
!
! Convert degrees into radians
!

RADIANS=DEGREES*PI/180.0

end subroutine degrees2radians

!*******************************************************************************

subroutine geographic_distance(X1,Y1,X2,Y2,DISTANCE)

real, intent(in) :: X1,X2,Y1,Y2
real :: rX1,rX2,rY1,rY2,x
real, intent(out) :: DISTANCE

! SUMMARY:
!
! Calculate the distance from location pair (X1,Y1) to (X2,Y2) in kilometers
!

!write(*,'(2(f10.4))') X1,Y1
if (X1<-180.0.or.X1>360.0) & 
  stop "ERROR: stratus/geographic_distance: one or more of X1 or X2 longitude out of range -180.0 <= X <= 360.0"
if (Y1<-90.0.or.Y1>180.0) & 
  stop "ERROR: stratus/geographic_distance: one or more of Y1 or Y2 latitude out of range -90.0 <= Y <= 180.0"

call degrees2radians(DEGREES=X1,RADIANS=RX1)
call degrees2radians(DEGREES=X2,RADIANS=RX2)
call degrees2radians(DEGREES=Y1,RADIANS=RY1)
call degrees2radians(DEGREES=Y2,RADIANS=RY2)

x=(sqrt((sin((RY1-RY2)/2.0))**2.0 + cos(RY1)*cos(RY2)*(sin((RX1-RX2)/2.0))**2.0))
if (x>1.0) x=1.0
DISTANCE=(2*asin(x))*180.0*60.0/PI*1.852

end subroutine geographic_distance

!*******************************************************************************

subroutine grid(W,E,S,N,X,Y,LON,LAT,STNS,GLON,GLAT,NUMGRIDS)

integer, intent(out) :: NUMGRIDS
integer, dimension(:), intent(out) :: STNS
integer :: iX,iY,iNumStns,iStn,iCurrGrid,iXX,iYY,iPoly
real :: rXCenter,rYCenter
real, intent(in) :: W,E,S,N,X,Y
real, dimension(1:5) :: rX,rY
real, dimension(:), intent(in) :: LON,LAT
real, allocatable, dimension(:) :: GLON,GLAT

! SUMMARY:
!
! Used to place a rectangular grid over a set of irregularly spaced stations
! and return the location of each station by its unique grid box number. 
! 
! Interpolate (grid) irregularly spaced data to a regularly spaced grid.
! W=Western longitudinal extent of rectangular grid (degrees)
! E=Eastern longitudinal extent of rectangular grid (degrees)
! S=Southern latitudinal extent of rectangular grid (degrees)
! N=Northern latitudinal extent of rectangular grid (degrees)
! LON=longitude width of grid boxes (degrees)
! LAT=latitude width of grid boxes (degrees)
! STNS=output list of stations (1....N) by unique grid box number 
! GLON=output list of grid box center coordinate longitudes by unique grid box number (degrees)
! GLAT=output list of grid box center coordinate latitudes by unique grid box number (degrees)
! NUMGRIDS=total number of unique grid box numbers

if (allocated(GLON)) deallocate(GLON)
if (allocated(GLAT)) deallocate(GLAT)

iX=nint(abs(E-W)/X)
iY=nint(abs(N-S)/Y)
iNumStns=size(LON)
NUMGRIDS=iX*iY
allocate(GLON(1:iX*iY))
allocate(GLAT(1:iX*iY))
STNS(:)=0

do iStn=1,iNumStns
  iCurrGrid=0
  OUTER: do iYY=1,iY
    do iXX=1,iX
      iCurrGrid=iCurrGrid+1
      rXCenter=(W+(X/2.0))+(real(iXX-1)*(X))
      rYCenter=(N-(Y/2.0))-(real(iYY-1)*(Y))
      rX(1)=rXCenter-(X/2.0); rY(1)=rYCenter+(Y/2.0)
      rX(2)=rXCenter+(X/2.0); rY(2)=rYCenter+(Y/2.0)
      rX(3)=rXCenter+(X/2.0); rY(3)=rYCenter-(Y/2.0)
      rX(4)=rXCenter-(X/2.0); rY(4)=rYCenter-(Y/2.0)
      rX(5)=rXCenter-(X/2.0); rY(5)=rYCenter+(Y/2.0)
      call point_in_polygon(LON(iStn),LAT(iStn),rX(:),rY(:),VALUE=iPoly)
      GLON(iCurrGrid)=rXCenter; GLAT(iCurrGrid)=rYCenter
      if (iPoly>=0) then
        STNS(iStn)=iCurrGrid
      endif
    enddo
  enddo OUTER
enddo

end subroutine grid

!*******************************************************************************

subroutine grid_polygon_fraction(LON,LAT,X,Y,XX,YY,POLYFILE,GRID_FRACTION)

real, intent(in) :: LON  ! Grid Node Center Longitude
real, intent(in) :: LAT  ! Grid Node Center Latitude
real, intent(in) :: X    ! Grid Spacing, X-Direction
real, intent(in) :: Y    ! Grid Spacing, Y-Direction
real, intent(in) :: XX   ! Desired Subsample Grid Spacing, X-Direction
real, intent(in) :: YY   ! Desired Subsample Grid Spacing, Y-Direction
real, intent(out) :: GRID_FRACTION
character(len=*), intent(in) :: POLYFILE
real, dimension(1:5) :: rX,rY
real :: W,E,S,N,rT,rC,rXCenter,rYCenter
integer :: iX,iY,iXX,iYY
integer :: VALUE

rX(1)=LON-(X/2.0); rY(1)=LAT+(Y/2.0)
rX(2)=LON+(X/2.0); rY(2)=LAT+(Y/2.0)
rX(3)=LON+(X/2.0); rY(3)=LAT-(Y/2.0)
rX(4)=LON-(X/2.0); rY(4)=LAT-(Y/2.0)
rX(5)=LON-(X/2.0); rY(5)=LAT+(Y/2.0)
W=rX(1)
E=rX(2)
S=rY(3)
N=rY(1)
iX=nint(abs(E-W)/XX)
iY=nint(abs(N-S)/YY)
rT=0.0
rC=0.0
do iYY=1,iY
  do iXX=1,iX
    rXCenter=(W+(XX/2.0))+(real(iXX-1)*(XX))
    rYCenter=(N-(YY/2.0))-(real(iYY-1)*(YY))
    call point_in_polyfile(rXCenter,rYCenter,POLYFILE,VALUE)
    rT=rT+1.0
    if (VALUE>=0) rC=rC+1.0
  enddo
enddo
if (rT==0.0) stop "ERROR: stratus/grid_polygon_fraction: Subsample Grid Error, no sub-grid boxes!"
GRID_FRACTION=rC/rT

end subroutine grid_polygon_fraction

!*******************************************************************************

subroutine indices(X,Y)                                                        

! modified version of http://www.netlib.org/napack/sort2.f
! lists indices in ascending order (i.e. y(1) is location of smallest value))

      real, intent(in), dimension(:) :: X
      integer, intent(out), dimension(:) :: Y
      REAL S,T
      INTEGER I,J,K,L,M,P,Q,W(size(X)),N

      if (size(X)/=size(Y)) stop "ERROR: stratus/indices: X and Y input arrays must be same size"
      N=size(X) 
      I = 1
      do
          K = I
          do
              J = I
              Y(I) = I
              I = I + 1
              IF (J .EQ. N) then
                  if (K .EQ. 1) return
                  exit
              END IF
              IF (X(I) .GE. X(J)) cycle
              W(K) = I
              exit
          end do
          IF (K .EQ. 1) RETURN
      end do
      W(K) = N + 1
  40  M = 1
      L = 1
  50  I = L
      IF ( I .GT. N ) GOTO 120
      P = Y(I)
      S = X(P)
      J = W(I)
      K = J
      IF ( J .GT. N ) GOTO 100
      Q = Y(J)
      T = X(Q)
      L = W(J)
      Y(I) = L
  60  IF ( S .GT. T ) GOTO 70
      W(M) = P
      M = M + 1
      I = I + 1
      IF ( I .EQ. K ) GOTO 80
      P = Y(I)
      S = X(P)
      GOTO 60
  70  W(M)= Q
      M = M + 1
      J = J + 1
      IF ( J .EQ. L ) GOTO 110
      Q = Y(J)
      T = X(Q)
      GOTO 60
  80  W(M) = Q
      K = M + L - J
      I = J - M
  90  M = M + 1
      IF ( M .EQ. K ) GOTO 50
      W(M) = Y(M+I)
      GOTO 90
  100 Y(I) = J
      L = J
  110 W(M) = P
      K = M + K - I
      I = I - M
      GOTO 90
  120 I = 1
  130 K = I
      J = Y(I)
  140 Y(I) = W(I)
      I = I + 1
      IF ( I .LT. J ) GOTO 140
      W(K) = I
      IF ( I .LE. N ) GOTO 130
      IF ( K .GT. 1 ) GOTO 40
      RETURN

end subroutine indices 

!*******************************************************************************

subroutine inverse_distance_weighted_mean(data_array,distances,idwm,MISSING,MIN_DATA,P)

real, dimension(:), intent(in) :: data_array
real, dimension(:), intent(in) :: distances
real, intent(out) :: idwm
real, optional, intent(in) :: MISSING
integer, optional, intent(in) :: MIN_DATA
integer :: iC,ii
real :: rP,rNumerator,rDenominator
real, optional, intent(in) :: P
real, allocatable, dimension(:) :: rTemp,rTempDist,weight

if (size(data_array)/=size(distances)) stop "ERROR: subroutine: inverse distance weighted mean, size data_array .ne. size distances"
if (present(MISSING).and..not.present(MIN_DATA)) stop "ERROR: subroutine: inverse distance weighted mean, MIN_DATA missing"
if (present(MIN_DATA).and..not.present(MISSING)) stop "ERROR: subroutine: inverse distance weighted mean, MISSING missing"
if (size(data_array)>=1) then
  allocate(rTemp(1:size(data_array)))
  allocate(rTempDist(1:size(data_array)))
  allocate(weight(1:size(data_array)))
else
  stop "ERROR: subroutine: inverse distance weighted mean, size of data_array = 0"
endif
if (present(MISSING).and.present(MIN_DATA)) then
  if (count(data_array(:)/=MISSING)<MIN_DATA) then
    idwm=MISSING
    return
  endif
  iC=0
  do ii=1,size(data_array)
    if (data_array(ii)/=MISSING.and.distances(ii)/=MISSING) then
      iC=iC+1
      rTemp(iC)=data_array(ii)
      rTempDist(iC)=distances(ii)
    endif
  enddo
else
  iC=0
  do ii=1,size(data_array)
    iC=iC+1
    rTemp(iC)=data_array(ii)
    rTempDist(iC)=distances(ii)
  enddo
endif

if (any(rTempDist(:)==0.0)) stop "ERROR: subroutine: inverse distance weighted mean, distance=0 therefore divide by 0"

if (present(P)) then
  rP=P
else
  rP=2.0
endif

! Calculate Weights

do ii=1,iC
  weight(ii)=1.0 / (rTempDist(ii)**rP)
enddo

! Calculate IDWM

rNumerator=0.0
rDenominator=0.0
do ii=1,iC
  rNumerator=rNumerator+(weight(ii)*rTemp(ii))
  rDenominator=rDenominator+(weight(ii))
enddo

if (rDenominator==0.0) stop "ERROR: subroutine: inverse distance weighted mean, denominator=0.0"

idwm=rNumerator/rDenominator

end subroutine inverse_distance_weighted_mean

!*******************************************************************************

subroutine jaccard_index(STRING1,STRING2,VALUE)

integer :: a,b,c,ii,jj
integer, dimension(1:255) :: iFound
character(len=*), intent(in) :: STRING1
character(len=*), intent(in) :: STRING2
real, intent(out) :: VALUE

! measure similarity of two strings
! 
! JACCARD INDEX (JI) = intersection divided by the union of two strings
!
! a = # of characters in common in both STRING1 and STRING2
! b = # of unique characters in STRING1 not in STRING2
! c = # of unique characters in STRING2 not in STRING1
!
! JI = (a) / (a+b+c)

iFound(:)=0
do ii=1,len_trim(STRING1)
  do jj=1,len_trim(STRING2)
    if (STRING1(ii:ii)==STRING2(jj:jj)) iFound(iachar(STRING1(ii:ii)))=1
  enddo
enddo
a=count(iFound(:)==1)

iFound(:)=0
do ii=1,len_trim(STRING1)
  do jj=1,len_trim(STRING2)
    if (STRING1(ii:ii)==STRING2(jj:jj)) iFound(iachar(STRING1(ii:ii)))=1
  enddo
enddo
b=0
do ii=1,len_trim(STRING1)
  if (iFound(iachar(STRING1(ii:ii)))==0) b=b+1
enddo
c=0
do ii=1,len_trim(STRING2)
  if (iFound(iachar(STRING2(ii:ii)))==0) c=c+1
enddo

VALUE=real(a) / (real(a)+real(b)+real(c))

end subroutine jaccard_index

!*******************************************************************************

subroutine kendall_theil(X,Y,SLOPE,INTERCEPT,P_VALUE,TAU,MISS,MIN_DATA)

! Source of method: http://pubs.usgs.gov/twri/twri4a3/html/pdf_new.html

real, dimension(:), intent(in) :: X,Y
real, allocatable, dimension(:) :: XX, YY
real, intent(out) :: SLOPE
real, intent(out), optional :: INTERCEPT
real, intent(out), optional :: P_VALUE
real, intent(out), optional :: TAU
real, dimension(:), allocatable :: rTemp
real, dimension(3:10,0:45) :: rT
real, allocatable, dimension(:) :: rStore
real, intent(in), optional :: MISS
real :: XXX,YYY
integer, intent(in), optional :: MIN_DATA
integer :: iPlus, iMinus,iS,iCurr
integer, dimension(size(Y)) :: iTie
integer, allocatable, dimension(:) :: SORT_INDICES
real(dp) :: rZSubS,rSum,rSigmaSubs
logical :: lEval
integer :: iC,ii,iN,iL,iStat,i,j

if (present(MISS).and..not.(present(MIN_DATA))) stop "ERROR: stratus/kendall_theil: missing optional arg, MIN_DATA" 
if (present(MIN_DATA).and..not.(present(MISS))) stop "ERROR: stratus/kendall_theil: missing optional arg, MISS"

if (present(MISS)) then
  SLOPE=MISS
  if (present(INTERCEPT)) INTERCEPT=MISS
  if (present(P_VALUE)) P_VALUE=MISS
  if (present(TAU)) TAU=MISS
  if ((count(X(:)/=MISS)<MIN_DATA).or.(count(Y(:)/=MISS)<MIN_DATA)) return
endif

if (size(X)/=size(Y)) stop "ERROR: stratus/kendall_theil: X array not equal in size to Y array"
if (size(X) < 4) stop "ERROR: stratus/kendall_theil: N < 4"

if (present(MISS)) then
  iC=0
  do ii=1,size(X)
    if (X(ii)==MISS.or.Y(ii)==MISS) cycle
    iC=iC+1
  enddo
  allocate(XX(1:iC))
  allocate(YY(1:iC))
  iC=0
  do ii=1,size(X)
    if (X(ii)==MISS.or.Y(ii)==MISS) cycle
    iC=iC+1
    XX(iC)=X(ii); YY(iC)=Y(ii)
  enddo
else
  iC=0
  do ii=1,size(X)
    iC=iC+1
  enddo
  allocate(XX(1:iC))
  allocate(YY(1:iC))
  iC=0
  do ii=1,size(X)
    iC=iC+1
    XX(iC)=X(ii); YY(iC)=Y(ii)
  enddo
endif

iN=size(XX)

iL=(iN*(iN-1))/2
allocate(rTemp(1:iL),stat=iStat) ; if (iStat>0) stop "ERROR: stratus/kendall_theil: not enough memory"
iC=0
do i=1,iN-1
  do j=i+1,iN
    if ( (XX(j)-XX(i)) /=0.0 ) then
      iC=iC+1
      rTemp(iC)=( (YY(j)-YY(i)) / (XX(j)-XX(i)) )
    endif
  enddo
enddo
if (iC == 0) then
  if (.not.present(MISS)) stop "ERROR: stratus/kendall_theil: not enough data to perform calculation, check denom. = 0.0"
endif
if (present(MISS).and.present(MIN_DATA)) then
  call median(X=rTemp(1:iC),VALUE=SLOPE,MISS=MISS,MIN_DATA=MIN_DATA)
  call median(X=YY,VALUE=YYY,MISS=MISS,MIN_DATA=MIN_DATA)
  call median(X=XX,VALUE=XXX,MISS=MISS,MIN_DATA=MIN_DATA)
  if (SLOPE==MISS) return 
else
  call median(rTemp(1:iC),SLOPE)
  call median(YY,YYY)
  call median(XX,XXX)
endif
if (present(INTERCEPT)) INTERCEPT = ( YYY - (SLOPE*XXX) )
if (present(P_VALUE)) then
  call sort(XX,SORT_INDICES)
  iPlus=0
  iMinus=0
  do i=1,iN-1
    do j=i+1,iN
      if ( YY(SORT_INDICES(i))==YY(SORT_INDICES(j)).or.XX(SORT_INDICES(i))==XX(SORT_INDICES(j)) ) then
      else
        if (YY(SORT_INDICES(i)) < YY(SORT_INDICES(j))) iPlus=iPlus+1
        if (YY(SORT_INDICES(i)) > YY(SORT_INDICES(j))) iMinus=iMinus+1
      endif
    enddo
  enddo
  iS=iPlus-iMinus
  if (iN <= 10) then
! rT is an array lookup of Kendall's S Statistic and tau correlation coefficient for N <= 10 and has 
! the lookup form of (N,S).  The probability is for a one-sided p = Prob(S >= x) or Prob(S <= -x)
    P_VALUE=-99.9
    rT=0.0
    rT(4,0)=0.625; rT(4,2)=0.375; rT(4,4)=0.167; rT(4,6)=0.042
 rT(5,0)=0.592; rT(5,2)=0.408; rT(5,4)=0.242; rT(5,6)=0.117; rT(5,8)=0.042; rT(5,10)=0.0083
    rT(8,0)=0.548; rT(8,2)=0.452; rT(8,4)=0.360; rT(8,6)=0.274; rT(8,8)=0.199; rT(8,10)=0.138; rT(8,12)=0.089
    rT(8,14)=0.054; rT(8,16)=0.031; rT(8,18)=0.0156; rT(8,20)=0.0071; rT(8,22)=0.0028; rT(8,24)=0.0009
    rT(8,26)=0.0002; rT(8,28)=0.0001; rT(9,0)=0.540; rT(9,2)=0.460; rT(9,4)=0.381; rT(9,6)=0.306
    rT(9,8)=0.238; rT(9,10)=0.179; rT(9,12)=0.130; rT(9,14)=0.090; rT(9,16)=0.060; rT(9,18)=0.038
    rT(9,20)=0.022; rT(9,22)=0.0124; rT(9,24)=0.0063; rT(9,26)=0.0029; rT(9,28)=0.0012; rT(9,30)=0.0004
    rT(9,32)=0.0001; rT(9,34)=0.0001; rT(9,36)=0.0001

    rT(3,1)=0.500; rT(3,3)=0.167; rT(6,1)=0.500; rT(6,3)=0.360; rT(6,5)=0.235; rT(6,7)=0.136; rT(6,9)=0.068
    rT(6,11)=0.028; rT(6,13)=0.0083; rT(6,15)=0.0014; rT(7,1)=0.500; rT(7,3)=0.386; rT(7,5)=0.281
    rT(7,7)=0.191; rT(7,9)=0.119; rT(7,11)=0.068; rT(7,13)=0.035; rT(7,15)=0.015; rT(7,17)=0.0054
    rT(7,19)=0.0014; rT(7,21)=0.0002; rT(10,1)=0.500; rT(10,3)=0.431; rT(10,5)=0.364; rT(10,7)=0.300
    rT(10,9)=0.242; rT(10,11)=0.190; rT(10,13)=0.146; rT(10,15)=0.108; rT(10,17)=0.078; rT(10,19)=0.054
    rT(10,21)=0.036; rT(10,23)=0.023; rT(10,25)=0.0143; rT(10,27)=0.0083; rT(10,29)=0.0046; rT(10,31)=0.0023
    rT(10,33)=0.0011; rT(10,35)=0.0005; rT(10,37)=0.0002; rT(10,39)=0.0001; rT(10,41)=0.0001
    rT(10,43)=0.0001; rT(10,45)=0.0001
    if (abs(iS)>45) then
      P_VALUE=0.0001*2.0
    else
      P_VALUE=(rT(iN,abs(iS)))*2.0
    endif
    if (P_VALUE == -99.9) stop "ERROR: stratus/kendall_theil: P_VALUE lookup value not found"
    if (P_VALUE>=0.000.and.P_VALUE<=0.0001) P_VALUE=0.0001
    if (P_VALUE>0.0001.and.P_VALUE<=0.0002) P_VALUE=0.0002
    if (P_VALUE>0.0002.and.P_VALUE<=0.0003) P_VALUE=0.0003
    if (P_VALUE>0.0003.and.P_VALUE<=0.0004) P_VALUE=0.0004
    if (P_VALUE>0.0004.and.P_VALUE<=0.0005) P_VALUE=0.0005
    if (P_VALUE>0.0005.and.P_VALUE<=0.0006) P_VALUE=0.0006
    if (P_VALUE>0.0006.and.P_VALUE<=0.0007) P_VALUE=0.0007
    if (P_VALUE>0.0007.and.P_VALUE<=0.0008) P_VALUE=0.0008
    if (P_VALUE>0.0008.and.P_VALUE<=0.0009) P_VALUE=0.0009
  else

!   Look for any ties

    allocate(rStore(1:size(XX)))
    rStore(:)=XX(:)
    call sort(rStore)
    iCurr=0
    iTie=0
    do i=2,size(XX)
      if (rStore(i-1)==rStore(i)) then
        iCurr=iCurr+1
      else
        if (iCurr/=0) then
          iCurr=iCurr+1
          iTie(iCurr)=iTie(iCurr)+1
        endif
        iCurr=0
      endif
    enddo
    rStore=YY
    call sort(rStore)
    iCurr=0
    do i=2,size(YY)
      if (rStore(i-1)==rStore(i)) then
        iCurr=iCurr+1
      else
        if (iCurr/=0) then
          iCurr=iCurr+1
          iTie(iCurr)=iTie(iCurr)+1
        endif
        iCurr=0
      endif
    enddo
    if (count(iTie(:)>0)>0) then
      rSum=real((iN*(iN-1)*((2*iN)+5)))
      do i=1,size(YY)
        rSum=rSum-( real(iTie(i))*real(i)*real(i-1)*real(2*i+5) )
      enddo
      if ((rSum/18.0)<0.0) stop "ERROR: stratus: kendall_theil, sqrt(q), where q < 0"
      rSigmaSubS = sqrt(rSum/18.0)
    else
      if ( ((real(iN)/18.0)*real((iN-1)*((2*iN)+5)))<0.0 ) stop "ERROR: stratus: kendall_theil, sqrt(q), where q < 0"
      rSigmaSubS = sqrt((real(iN)/18.0)*real((iN-1)*((2*iN)+5)))
    endif

    if (rSigmaSubS > 0.0) then
      rZSubS = (real(iS) - 1.0) / rSigmaSubS
    elseif(rSigmaSubS < 0.0) then
      rZSubS = (real(iS) + 1.0) / rSigmaSubS
    else
      rZSubS = 0.0
    endif

    lEval = .false.
    if (rZSubS >= 0.0) lEval = .true.
    call normal_dist(rZSubS,lEval,P_VALUE)
    P_VALUE = P_VALUE*2.0
    if (P_VALUE>=0.000.and.P_VALUE<=0.0001) P_VALUE=0.0001
    if (P_VALUE>0.0001.and.P_VALUE<=0.0002) P_VALUE=0.0002
    if (P_VALUE>0.0002.and.P_VALUE<=0.0003) P_VALUE=0.0003
    if (P_VALUE>0.0003.and.P_VALUE<=0.0004) P_VALUE=0.0004
    if (P_VALUE>0.0004.and.P_VALUE<=0.0005) P_VALUE=0.0005
    if (P_VALUE>0.0005.and.P_VALUE<=0.0006) P_VALUE=0.0006
    if (P_VALUE>0.0006.and.P_VALUE<=0.0007) P_VALUE=0.0007
    if (P_VALUE>0.0007.and.P_VALUE<=0.0008) P_VALUE=0.0008
    if (P_VALUE>0.0008.and.P_VALUE<=0.0009) P_VALUE=0.0009
  endif
endif
if (present(TAU)) then
  call sort(XX,SORT_INDICES)
  iPlus=0
  iMinus=0
  do i=1,iN-1
    do j=i+1,iN
      if ( YY(SORT_INDICES(i))==YY(SORT_INDICES(j)).or.XX(SORT_INDICES(i))==XX(SORT_INDICES(j)) ) then
      else
        if (YY(SORT_INDICES(i)) < YY(SORT_INDICES(j))) iPlus=iPlus+1
        if (YY(SORT_INDICES(i)) > YY(SORT_INDICES(j))) iMinus=iMinus+1
      endif
    enddo
  enddo
  iS=iPlus-iMinus
  TAU = real(iS) / (real(iN)*real(iN-1)/2.0)
endif
deallocate(rTemp)
deallocate(XX)
deallocate(YY)

end subroutine kendall_theil

!*******************************************************************************

subroutine kurtosis(X,VALUE,MISS,MIN_DATA)

integer, intent(in), optional :: MIN_DATA
real, intent(in) ::  X(:)
real, intent(out) :: VALUE
real, intent(in), optional :: MISS
real :: rNum,rTerm1,rXMean,rXStDev,rTerm2,rTerm3
integer :: iLoop

  
if (present(MISS).and..not.(present(MIN_DATA))) stop "ERROR: stratus/kurtosis: missing optional arg, MIN_DATA" 
if (present(MIN_DATA).and..not.(present(MISS))) stop "ERROR: stratus/kurtosis: missing optional arg, MISS"

if (present(MISS)) then
  VALUE=MISS
  if (count(X(:)/=MISS)<MIN_DATA) return
  rNum=real(count(X(:)/=MISS))
  if (size(X)<2) stop "ERROR: stratus/kurtosis: Input array size too small (e.g. < 2)"
  
  rTerm1= ( (rNum*(rNum+1.0)) / ((rNum-1.0)*(rNum-2.0)*(rNum-3.0)))
  call mean(X,rXMean,MISS=MISS,MIN_DATA=MIN_DATA)
  call standard_deviation(X,rXStDev,MISS=MISS,MIN_DATA=MIN_DATA)
  rTerm2=0.0
  do iLoop=1,size(X)
    if (X(iLoop)==MISS) cycle
    rTerm2 = rTerm2 + ( (((X(iLoop)-rXMean)/rXStDev)) **4.0)
  enddo
  rTerm3=(3.0*((rNum-1.0)**2.0))/((rNum-2.0)*(rNum-3.0))
  VALUE=(rTerm1*rTerm2)-rTerm3
else
  rNum=real(size(X))
  if (size(X)<2) stop "ERROR: stratus/kurtosis: Input array size too small (e.g. < 2)"
  
  rTerm1= ( (rNum*(rNum+1.0)) / ((rNum-1.0)*(rNum-2.0)*(rNum-3.0)))
  call mean(X,rXMean)
  call standard_deviation(X,rXStDev)
  rTerm2=0.0
  do iLoop=1,size(X)
    rTerm2 = rTerm2 + ( (((X(iLoop)-rXMean)/rXStDev)) **4.0)
  enddo
  rTerm3=(3.0*((rNum-1.0)**2.0))/((rNum-2.0)*(rNum-3.0))
  VALUE=(rTerm1*rTerm2)-rTerm3
endif

end subroutine kurtosis

!*******************************************************************************

subroutine linear_regression(X,Y,SLOPE,F_STATISTIC,INTERCEPT,P_VALUE,R,  &
           R_SQUARE,RESIDUALS,SE_INTERCEPT,SE_SLOPE,T_INTERCEPT,T_SLOPE, &
           MISS,MIN_DATA)

! simple linear regression 

integer, intent(in), optional :: MIN_DATA
real, intent(in) :: X(:),Y(:)
real, intent(out) :: SLOPE
real, intent(out), optional :: F_STATISTIC, INTERCEPT, R, R_SQUARE, SE_SLOPE, SE_INTERCEPT,  &
                               T_SLOPE, T_INTERCEPT, P_VALUE
real, intent(out), optional :: RESIDUALS(:)
real, intent(in), optional :: MISS
real(dp) :: rSxx,rSxy,rSyy,rSumX,rSumY,rSumXY,rSumX2,rSumY2,rC,rXbar,rYbar,rSE,rSSE,rSlope
integer :: i
real :: rSSREG,rTCRIT,rVal

if (size(X)/=size(Y)) stop "ERROR: stratus/linear_regression: size(X) /= size(Y)"
if (present(MISS).and..not.(present(MIN_DATA))) stop "ERROR: stratus/linear_regression: missing optional arg, MIN_DATA" 
if (present(MIN_DATA).and..not.(present(MISS))) stop "ERROR: stratus/linear_regression: MIN_DATA is present, MISS absent"

if (present(MISS)) SLOPE=MISS
if (present(MISS).and.present(F_STATISTIC)) F_STATISTIC=MISS
if (present(MISS).and.present(INTERCEPT)) INTERCEPT=MISS
if (present(MISS).and.present(R)) R=MISS
if (present(MISS).and.present(R_SQUARE)) R_SQUARE=MISS
if (present(MISS).and.present(SE_SLOPE)) SE_SLOPE=MISS
if (present(MISS).and.present(SE_INTERCEPT)) SE_INTERCEPT=MISS
if (present(MISS).and.present(T_SLOPE)) T_SLOPE=MISS
if (present(MISS).and.present(T_INTERCEPT)) T_INTERCEPT=MISS
if (present(MISS).and.present(P_VALUE)) P_VALUE=MISS
if (present(MISS).and.present(RESIDUALS)) RESIDUALS(:)=MISS
if (present(MISS)) then
  if (count(Y(:)==MISS)==size(Y)) return
endif

rC=0.0; rSumX=0.0; rSumY=0.0; rSumXY=0.0; rSumX2=0.0; rSumY2=0.0
do i=1,size(Y)
  if (present(MISS)) then
    if (X(i)==MISS.or.Y(i)==MISS) cycle
  endif
  rSumX=rSumX+X(i)
  rSumY=rSumY+Y(i)
  rSumXY=rSumXY+(X(i)*Y(i))
  rSumX2=rSumX2+(X(i)*X(i))
  rSumY2=rSumY2+(Y(i)*Y(i))
  rC=rC+1.0
enddo
if (present(MIN_DATA)) then
  if (nint(rC)<MIN_DATA) then
    return
  endif
endif

rXbar = rSumX/rC
rYbar = rSumY/rC
rSxx = rSumX2 - (((rSumX)**2.0)/rC)
rSyy = rSumY2 - (((rSumY)**2.0)/rC)
rSxy = rSumXY - ((rSumX*rSumY)/rC)

rSLOPE = rSxy/rSxx
SLOPE = rSlope


if (present(F_STATISTIC).or.present(INTERCEPT).or.present(R).or.present(R_SQUARE).or.present(SE_SLOPE) &
  .or.present(SE_INTERCEPT).or.present(T_SLOPE).or.present(T_INTERCEPT).or.present(RESIDUALS).or. &
  present(P_VALUE)) then
  rSSE = rSyy - (rSLOPE*(rSxy))
  if ( (rSSE/(rC-2.0))<0.0 ) then
    rSE=0.0
  else
    rSE = sqrt(rSSE/(rC-2.0))
  endif
  rSSREG = ((rSxy*rSxy)/rSxx)
  if (present(F_STATISTIC)) F_STATISTIC = (rSSREG/((rSE)**2.0))
  if (present(INTERCEPT)) INTERCEPT = (rYbar-(rSLOPE*rXbar))
  if (present(R)) R = (rSxy/(sqrt(rSxx*rSyy)))
  if (present(R_SQUARE)) R_SQUARE = (rSxy/(sqrt(rSxx*rSyy)))**2.0
  if (present(RESIDUALS)) then
    do i=1,size(X)
      RESIDUALS(i) = Y(i)-((rYbar-(rSLOPE*rXbar))+(rSLOPE*X(i)))
    enddo
  endif
  if (present(SE_INTERCEPT)) SE_INTERCEPT = rSE*(sqrt((rSumX2/(rC*rSxx))))
  if (present(SE_SLOPE)) SE_SLOPE = (rSE / (sqrt(rSxx)))
  if (present(T_SLOPE)) T_SLOPE = rSLOPE / (rSE/(sqrt(rSxx)))
  if (present(T_INTERCEPT)) T_INTERCEPT = (rYbar-(rSLOPE*rXbar))/(rSE*(sqrt((rSumX2/(rC*rSxx)))))
  if (present(P_VALUE)) then
    if (rSE==0.0) then
      P_VALUE=1.1
    else
      rTCRIT=(rSLOPE/(rSE/(sqrt(rSxx))))
      if (nint(rC-2.0)>4) then
        call students_t_dist(rTCRIT,nint(rC-2.0),rVal )
        P_VALUE=(rVal*2.0)
      else
        if (present(MISS)) then
          P_VALUE=MISS
        else
          stop "ERROR: stratus/linear_regression: cannot calculate P_VALUE, DF <= 4 .and. optional ARG MISS not present" 
        endif
      endif
    endif
    if (P_VALUE<0.0001) P_VALUE=0.0001
    if (P_VALUE>0.0001.and.P_VALUE<=0.0002) P_VALUE=0.0002
    if (P_VALUE>0.0002.and.P_VALUE<=0.0003) P_VALUE=0.0003
    if (P_VALUE>0.0003.and.P_VALUE<=0.0004) P_VALUE=0.0004
    if (P_VALUE>0.0004.and.P_VALUE<=0.0005) P_VALUE=0.0005
    if (P_VALUE>0.0005.and.P_VALUE<=0.0006) P_VALUE=0.0006
    if (P_VALUE>0.0006.and.P_VALUE<=0.0007) P_VALUE=0.0007
    if (P_VALUE>0.0007.and.P_VALUE<=0.0008) P_VALUE=0.0008
    if (P_VALUE>0.0008.and.P_VALUE<=0.0009) P_VALUE=0.0009
  endif
endif

end subroutine linear_regression

!*******************************************************************************

subroutine lowercase(STRING,LOWERSTRING)

! converts a string to all lower case

character(len=*), intent(in) :: STRING
character(len=*), intent(out) :: LOWERSTRING
character(len=01), dimension(1:26) :: cLower, cUpper
integer :: ii,jj

if (len(STRING)>len(LOWERSTRING)) stop "ERROR: stratus: lowercase, string length"

cLower(:)=(/"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r",&
            "s","t","u","v","w","x","y","z"/)
cUpper(:)=(/"A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R",&
            "S","T","U","V","W","X","Y","Z"/)

LOWERSTRING=STRING
do ii=1,len(LOWERSTRING)
  do jj=1,26
    if (LOWERSTRING(ii:ii)==cUpper(jj)) LOWERSTRING(ii:ii)=cLower(jj)
  enddo 
enddo

end subroutine lowercase

!*******************************************************************************

subroutine lowess(x, y, f, nsteps, delta, ys, rw, res)

! from www.netlib.org/go/lowess.f

! Dec. 30, 1985
! W. S. Cleveland
! Bell Laboratories
! Murray Hill NJ 07974

! reference:

! Cleveland, William S. (1979)
! "Robust Locally Weighted Regression and Smoothing Scatterplots".
! Journal of the American Statistical Association 74 (368): 829836

! Purpose:
! 
!        LOWESS computes the smooth of a scatterplot of Y  against  X
!        using  robust  locally  weighted regression.  Fitted values,
!        YS, are computed at each of the  values  of  the  horizontal
!        axis in X.
! 
!        Argument description
! 
!              X = Input; abscissas of the points on the
!                  scatterplot; the values in X must be ordered
!                  from smallest to largest.
!              Y = Input; ordinates of the points on the
!                  scatterplot.
!              F = Input; specifies the amount of smoothing; F is
!                  the fraction of points used to compute each
!                  fitted value; as F increases the smoothed values
!                  become smoother; choosing F in the range .2 to
!                  .8 usually results in a good fit; if you have no
!                  idea which value to use, try F = .5.
!         NSTEPS = Input; the number of iterations in the robust
!                  fit; if NSTEPS = 0, the nonrobust fit is
!                  returned; setting NSTEPS equal to 2 should serve
!                  most purposes.
!          DELTA = input; nonnegative parameter which may be used
!                  to save computations; if N is less than 100, set
!                  DELTA equal to 0.0; if N is greater than 100 you
!                  should find out how DELTA works by reading the
!                  additional instructions section.
!             YS = Output; fitted values; YS(I) is the fitted value
!                  at X(I); to summarize the scatterplot, YS(I)
!                  should be plotted against X(I).
!             RW = Output; robustness weights; RW(I) is the weight
!                  given to the point (X(I),Y(I)); if NSTEPS = 0,
!                  RW is not used.
!            RES = Output; residuals; RES(I) = Y(I)-YS(I).
! 
! 
!        Other programs called
! 
!               LOWEST
!               SSORT
! 
!        Additional instructions
! 
!        DELTA can be used to save computations.   Very  roughly  the
!        algorithm  is  this:   on the initial fit and on each of the
!        NSTEPS iterations locally weighted regression fitted  values
!        are computed at points in X which are spaced, roughly, DELTA
!        apart; then the fitted values at the  remaining  points  are
!        computed  using  linear  interpolation.   The  first locally
!        weighted regression (l.w.r.) computation is carried  out  at
!        X(1)  and  the  last  is  carried  out at X(N).  Suppose the
!        l.w.r. computation is carried out at  X(I).   If  X(I+1)  is
!        greater  than  or  equal  to  X(I)+DELTA,  the  next  l.w.r.
!        computation is carried out at X(I+1).   If  X(I+1)  is  less
!        than X(I)+DELTA, the next l.w.r.  computation is carried out
!        at the largest X(J) which is greater than or equal  to  X(I)
!        but  is not greater than X(I)+DELTA.  Then the fitted values
!        for X(K) between X(I)  and  X(J),  if  there  are  any,  are
!        computed  by  linear  interpolation  of the fitted values at
!        X(I) and X(J).  If N is less than 100 then DELTA can be  set
!        to  0.0  since  the  computation time will not be too great.
!        For larger N it is typically not necessary to carry out  the
!        l.w.r.  computation for all points, so that much computation
!        time can be saved by taking DELTA to be  greater  than  0.0.
!        If  DELTA =  Range  (X)/k  then,  if  the  values  in X were
!        uniformly  scattered  over  the  range,  the   full   l.w.r.
!        computation  would be carried out at approximately k points.
!        Taking k to be 50 often works well.
! 
!        Method
! 
!        The fitted values are computed by using the nearest neighbor
!        routine  and  robust locally weighted regression of degree 1
!        with the tricube weight function.  A few additional features
!        have  been  added.  Suppose r is FN truncated to an integer.
!        Let  h  be  the  distance  to  the  r-th  nearest   neighbor
!        from X(I).   All  points within h of X(I) are used.  Thus if
!        the r-th nearest neighbor is exactly the  same  distance  as
!        other  points,  more  than r points can possibly be used for
!        the smooth at  X(I).   There  are  two  cases  where  robust
!        locally  weighted regression of degree 0 is actually used at
!        X(I).  One case occurs when  h  is  0.0.   The  second  case
!        occurs  when  the  weighted  standard error of the X(I) with
!        respect to the weights w(j) is  less  than  .001  times  the
!        range  of the X(I), where w(j) is the weight assigned to the
!        j-th point of X (the tricube  weight  times  the  robustness
!        weight)  divided by the sum of all of the weights.  Finally,
!        if the w(j) are all zero for the smooth at X(I), the  fitted
!        value is taken to be Y(I).

! Source: www.netlib.org/go/lowess.f

! Note: Some modifications (to update with F95 features) have been made.


      integer n
      integer, intent(in) :: nsteps
      real, intent(in) :: x(:)
      real, intent(in) :: y(:)
      real, intent(in) :: f, delta
      real, intent(out) :: ys(:)
      real, intent(out) :: rw(:)
      real, intent(out) :: res(:)
      integer nright, min0, max0, i, j, ifix
      integer iter, last, m1, m2, ns, nleft
      real abs, cut, cmad, r, d1, d2
      real c1, c9, alpha, denom, float
      logical ok

      n=size(x)
      if (size(x)/=size(y)) stop "ERROR: Lowess, x and y arrays must be the same size"

      if (n .ge. 2) goto 1
         ys(1) = y(1)
         return
! at least two, at most n points
   1  ns = max0(min0(ifix(f*float(n)), n), 2)
      iter = 1
         goto  3
   2     iter = iter+1
   3     if (iter .gt. nsteps+1) goto  22
! robustness iterations
         nleft = 1
         nright = ns
! index of prev estimated point
         last = 0
! index of current point
         i = 1
   4        if (nright .ge. n) goto  5
! move nleft, nright to right if radius decreases
               d1 = x(i)-x(nleft)
! if d1<=d2 with x(nright+1)==x(nright), lowest fixes
               d2 = x(nright+1)-x(i)
               if (d1 .le. d2) goto  5
! radius will not decrease by move right
               nleft = nleft+1
               nright = nright+1
               goto  4
! fitted value at x(i)
   5        call lowest(x, y, x(i), ys(i), nleft, nright, res, iter.gt.1,rw,ok)
            if (.not. ok) ys(i) = y(i)
! all weights zero - copy over value (all rw==0)
            if (last .ge. i-1) goto 9
               denom = x(i)-x(last)
! skipped points -- interpolate
! non-zero - proof?
               j = last+1
                  goto  7
   6              j = j+1
   7              if (j .ge. i) goto  8
                  alpha = (x(j)-x(last))/denom
                  ys(j) = alpha*ys(i)+(1.0-alpha)*ys(last)
                  goto  6
   8           continue
! last point actually estimated
   9        last = i
! x coord of close points
            cut = x(last)+delta
            i = last+1
               goto  11
  10           i = i+1
  11           if (i .gt. n) goto  13
! find close points
               if (x(i) .gt. cut) goto  13
! i one beyond last pt within cut
               if (x(i) .ne. x(last)) goto 12
                  ys(i) = ys(last)
! exact match in x
                  last = i
  12           continue
               goto  10
! back 1 point so interpolation within delta, but always go forward
  13        i = max0(last+1, i-1)
  14        if (last .lt. n) goto  4
! residuals
         do  15 i = 1, n
            res(i) = y(i)-ys(i)
  15        continue
         if (iter .gt. nsteps) goto  22
! compute robustness weights except last time
         do  16 i = 1, n
            rw(i) = abs(res(i))
  16        continue
         call sort(rw)
         m1 = n/2+1
         m2 = n-m1+1
! 6 median abs resid
         cmad = 3.0*(rw(m1)+rw(m2))
         c9 = .999*cmad
         c1 = .001*cmad
         do  21 i = 1, n
            r = abs(res(i))
            if (r .gt. c1) goto 17
               rw(i) = 1.
! near 0, avoid underflow
               goto  20
  17           if (r .le. c9) goto 18
                  rw(i) = 0.
! near 1, avoid underflow
                  goto  19
  18              rw(i) = (1.0-(r/cmad)**2)**2
  19        continue
  20        continue
  21        continue
         goto  2
  22  return
end subroutine lowess

!*******************************************************************************

subroutine lowest(x, y, xs, ys, nleft, nright, w, userw,rw,ok)

! Purpose
! 
!        LOWEST is a support routine for the subroutine LOWESS 
!        and ordinarily  will not  be  called  by  the  user.
!        The  fitted  value, YS, is computed  at  the  value,  XS, 
!        of  the   horizontal   axis. Robustness  weights,  RW, 
!        can  be employed in computing the fit.
!
!        Argument description
! 
! 
!              X = Input; abscissas of the points on the
!                  scatterplot; the values in X must be ordered
!                  from smallest to largest.
!              Y = Input; ordinates of the points on the
!                  scatterplot.
!             XS = Input; value of the horizontal axis at which the
!                  smooth is computed.
!             YS = Output; fitted value at XS.
!          NLEFT = Input; index of the first point which should be
!                  considered in computing the fitted value.
!         NRIGHT = Input; index of the last point which should be
!                  considered in computing the fitted value.
!              W = Output; W(I) is the weight for Y(I) used in the
!                  expression for YS, which is the sum from
!              I = NLEFT to NRIGHT of W(I)*Y(I); W(I) is
!                  defined only at locations NLEFT to NRIGHT.
!          USERW = Input; logical variable; if USERW is .TRUE., a
!                  robust fit is carried out using the weights in
!                  RW; if USERW is .FALSE., the values in RW are
!                  not used.
!             RW = Input; robustness weights.
!             OK = Output; logical variable; if the weights for the
!                  smooth are all 0.0, the fitted value, YS, is not
!                  computed and OK is set equal to .FALSE.; if the
!                  fitted value is computed OK is set equal to
! 
! 
!        Method
! 
!        The smooth at XS is computed using (robust) locally weighted
!        regression of degree 1.  The tricube weight function is used
!        with h equal to the maximum of XS-X(NLEFT) and X(NRIGHT)-XS.
!        Two  cases  where  the  program  reverts to locally weighted
!        regression of degree 0 are described  in  the  documentation
!        for LOWESS.

      integer n
      integer, intent(in) :: nleft
      integer, intent(in) :: nright
      real, intent(in) :: x(:)
      real, intent(in) :: y(:) 
      real, intent(in) :: xs
      real, intent(out) :: ys
      real, intent(out) :: w(:)
      real, intent(in) :: rw(:)
      logical, intent(in) ::  userw
      logical, intent(out) :: ok
      integer nrt, j
      real abs, a, b, c, h, r
      real h1, sqrt, h9, amax1, range

      n=size(x)
      if (size(x)/=size(y)) stop "ERROR: lowest, x and y must be the same size arrays"

      range = x(n)-x(1)
      h = amax1(xs-x(nleft), x(nright)-xs)
      h9 = .999*h
      h1 = .001*h
! sum of weights
      a = 0.0
      j = nleft
         goto  2
   1     j = j+1
   2     if (j .gt. n) goto  7
! compute weights (pick up all ties on right)
         w(j) = 0.
         r = abs(x(j)-xs)
         if (r .gt. h9) goto 5
            if (r .le. h1) goto 3
               w(j) = (1.0-(r/h)**3)**3
! small enough for non-zero weight
               goto  4
   3           w(j) = 1.
   4        if (userw) w(j) = rw(j)*w(j)
            a = a+w(j)
            goto  6
   5        if (x(j) .gt. xs) goto  7
! get out at first zero wt on right
   6     continue
         goto  1
! rightmost pt (may be greater than nright because of ties)
   7  nrt = j-1
      if (a .gt. 0.0) goto 8
         ok = .false.
         goto  16
   8     ok = .true.
! weighted least squares
         do  9 j = nleft, nrt
! make sum of w(j) == 1
            w(j) = w(j)/a
   9        continue
         if (h .le. 0.) goto 14
            a = 0.0
! use linear fit
            do  10 j = nleft, nrt
! weighted center of x values
               a = a+w(j)*x(j)
  10           continue
            b = xs-a
            c = 0.0
            do  11 j = nleft, nrt
               c = c+w(j)*(x(j)-a)**2
  11           continue
            if (sqrt(c) .le. .001*range) goto 13
               b = b/c
! points are spread out enough to compute slope
               do  12 j = nleft, nrt
                  w(j) = w(j)*(b*(x(j)-a)+1.0)
  12              continue
  13        continue
  14     ys = 0.0
         do  15 j = nleft, nrt
            ys = ys+w(j)*y(j)
  15        continue
  16  return

end subroutine lowest

!*******************************************************************************

subroutine mean(X,VALUE,BIWEIGHT,MISS,MIN_DATA)

integer, intent(in), optional :: MIN_DATA
real, intent(in) :: X(:)
real :: rWeight(size(X))
real, intent(out) :: VALUE
real, intent(in), optional :: BIWEIGHT, MISS
real :: rMedian, rMad,rN,rD,rSum,rC
integer :: iC,iLoop

if (present(MISS)) then
  if (size(X)==0) then
    VALUE=MISS
    return
  endif
endif
if (present(MISS).and..not.(present(MIN_DATA))) stop "ERROR: stratus/mean: missing optional arg, MIN_DATA" 
if (present(MIN_DATA).and..not.(present(MISS))) stop "ERROR: stratus/mean: missing optional arg, MISS"

if (present(MISS)) then
  VALUE=MISS
  if (count(X(:)/=MISS)<MIN_DATA) return
  if (count(X(:)==MISS)==size(X).and.MIN_DATA==0) return
  if (present(BIWEIGHT)) then
    call median(X,rMedian,MISS=MISS,MIN_DATA=MIN_DATA)
    call median_absolute_deviation(X,rMad,MISS=MISS,MIN_DATA=MIN_DATA)
    iC=0
    do iLoop=1,size(X)
      if (X(iLoop)==MISS) cycle
      iC=iC+1
      if (rMad==0.0) then
        rWeight(iLoop)=0.0
      else
        rWeight(iLoop)=(X(iLoop)-rMedian)/(BIWEIGHT*rMad)
      endif
      if (abs(rWeight(iLoop))>1.0) then
        rWeight(iLoop)=1.0
      endif
    enddo
    rN=0.0
    rD=0.0
    do iLoop=1,size(X)
      rN=rN+(X(iLoop)-rMedian)*((1.0-rWeight(iLoop)**2.0)**2.0)
      rD=rD+((1.0-rWeight(iLoop)**2.0)**2.0)
    enddo
    if (iC>=MIN_DATA) VALUE=rMedian+(rN/rD)
  else
    rSum = 0.0
    rC=0.0
    do iLoop = 1,size(X)
      if (X(iLoop)==MISS) cycle
      rSum = rSum + X(iLoop)
      rC=rC+1.0
    enddo
    if (rC == 0.0) then
      stop "ERROR: stratus/mean: divide by 0.0 error"
    else
      if (nint(rC)>=MIN_DATA) VALUE=(rSum/rC)
    endif
  endif
else
  if (present(BIWEIGHT)) then
    call median(X,rMedian)
    call median_absolute_deviation(X,rMad)
    do iLoop=1,size(X)
      if (rMad==0.0) then
        rWeight(iLoop)=0.0
      else
        rWeight(iLoop)=(X(iLoop)-rMedian)/(BIWEIGHT*rMad)
      endif
      if (abs(rWeight(iLoop))>1.0) then
        rWeight(iLoop)=1.0
      endif
    enddo
    rN=0.0
    rD=0.0
    do iLoop=1,size(X)
      rN=rN+(X(iLoop)-rMedian)*((1.0-rWeight(iLoop)**2.0)**2.0)
      rD=rD+((1.0-rWeight(iLoop)**2.0)**2.0)
    enddo
    VALUE=rMedian+(rN/rD)
  else
    rSum = 0.0
    rC=0.0
    do iLoop = 1,size(X)
      rSum = rSum + X(iLoop)
      rC=rC+1.0
    enddo
    if (rC == 0.0) then
      stop "ERROR: stratus/mean: divide by 0.0 error"
    else
      VALUE=(rSum/rC)
    endif
  endif
endif
  
end subroutine mean

!*******************************************************************************

subroutine median(X,VALUE,MISS,MIN_DATA)

real, intent(in) :: X(:)
real, intent(out) :: VALUE
real, intent(in), optional :: MISS
integer, intent(in), optional :: MIN_DATA
real :: rXTemp(size(X))
integer :: iC,ii,iPos1,iPos2

! SUMMARY:
!
! VALUE returns the median of a real valued array X
!
! If one of the optional args MISS or MIN_DATA is present, then the other is
! required to be present.

if (present(MISS)) then
  if (size(X)==0) then
    VALUE=MISS
    return
  endif
endif
if (present(MISS).and..not.(present(MIN_DATA))) stop "ERROR: stratus/mean: missing optional arg, MIN_DATA" 
if (present(MIN_DATA).and..not.(present(MISS))) stop "ERROR: stratus/mean: missing optional arg, MISS"

if (present(MISS)) then
  VALUE=MISS
  if (count(X(:)/=MISS)<MIN_DATA) return
  if (count(X(:)==MISS)==size(X).and.MIN_DATA==0) return
  iC=0
  do ii=1,size(X)
    if (X(ii)/=MISS) then
      iC=iC+1
      rXTemp(iC)=X(ii)
    endif
  enddo
  if (iC==0) return
else
  rXTemp = X
  iC=size(X)
endif

call sort(rXTemp(1:iC))

if (mod(size(X(1:iC)),2)==0) then
  iPos1 = int(real(size(X(1:iC)))/2.0)
  iPos2 = iPos1 + 1
  VALUE = ((rXTemp(iPos1)+rXTemp(iPos2))/2.0)
else
  iPos1 = int((real(size(X(1:iC)))/2.0)+1.0)
  VALUE = rXTemp(iPos1)
endif

end subroutine

!*******************************************************************************

subroutine median_absolute_deviation(X,VALUE,MISS,MIN_DATA)

real, intent(in) :: X(:)
real, intent(out) :: VALUE
real, intent(in), optional :: MISS
integer, intent(in), optional :: MIN_DATA
real rTemp(1:size(X))
integer :: iNum,ii,iLoop
real :: rMedian

! SUMMARY:
!
! VALUE returns the median of the deviations from the median of a real valued
! array X.
!
! If one of the optional args MISS or MIN_DATA is present, then the other is
! required to be present.

if (present(MISS).and..not.(present(MIN_DATA))) stop "ERROR: stratus/mean: missing optional arg, MIN_DATA" 
if (present(MIN_DATA).and..not.(present(MISS))) stop "ERROR: stratus/mean: missing optional arg, MISS"

if (present(MISS)) then
  VALUE=MISS
  if (count(X(:)/=MISS)<MIN_DATA) return
  if (count(X(:)==MISS)==size(X).and.MIN_DATA==0) return
  iNum=size(X)
  call median(X,rMedian,MISS=MISS,MIN_DATA=MIN_DATA)
  if (rMedian==MISS) return
  iNum=0
  do ii=1,size(X)
    if (X(ii)/=MISS) then
      iNum=iNum+1
      rTemp(iNum)=X(ii)
    endif
  enddo 
else
  iNum=size(X)
  call median(X,rMedian)
endif

do iLoop=1,iNum
  rTemp(iLoop)=abs(X(iLoop)-rMedian)
enddo

if (present(MISS)) then
  call median(rTemp,VALUE,MISS=MISS,MIN_DATA=MIN_DATA)
else
  call median(rTemp,VALUE)
endif

end subroutine median_absolute_deviation

!*******************************************************************************

subroutine month2seasonal(DATA,TIME,STAT,MISS,MIN_DATA,VALUE,PTILE)

! given an array with monthly data (i.e. array[yr,mo] ), return a 1-D array (i.e. returnarray[yr]),
! which is a specified seasonal array of the monthly data.  The user has many options for handling
! the monthly data (e.g. average, sum, mean, etc.)

real, intent(in), dimension(:,:) :: DATA
character(len=*), intent(in) :: TIME
character(len=*), intent(in) :: STAT
real, intent(in) :: MISS
integer, intent(in) :: MIN_DATA
real, intent(out), dimension(:) :: VALUE
real, intent(in), optional :: PTILE
integer, dimension(1:24) :: iMon,iYrAdj
real, dimension(1:12) :: rTemp
integer :: iStat,iYrBegin,iF,iPos,iYr,iC,iComma,ii
real :: rResult

VALUE=MISS

! ERROR CHECKING

if (size(VALUE)/=size(DATA(:,:),dim=1)) stop "ERROR: stratus/month2seasonal: output array VALUE incorrect dimension size"
if (STAT=="PERCENTILE".or.STAT=="percentile") then 
  if (.not.present(PTILE)) stop "ERROR: No percentile chosen"
endif

iComma=0
do ii=1,len_trim(TIME)
  if (TIME(ii:ii)==",") iComma=iComma+1
enddo

if (iComma+1 < 2) stop "ERROR: stratus/month2seasonal: TIME must contain 2 or more months"
  
read(TIME,*,IOSTAT=iStat) iMon(1:iComma+1)

if (iStat/=0) stop "ERROR: stratus/month2seasonal: invalid TIME string"

do ii=1,iComma+1
  if (iMon(ii)<1.or.iMon(ii)>12) stop "ERROR: stratus/month2seasonal: integer TIME out of range (range = 1 to 12)"
enddo

! MAIN

iYrAdj(:)=0
iYrBegin=1
iF=0
do ii=2,iComma+1
  if (iMon(ii)<iMon(ii-1)) then
    if (iF==1) stop "ERROR: stratus/month2seasonal: TIME string syntax incorrect"
    iPos=ii-1 ; iF=1 ; iYrBegin=2
  endif
enddo
if (iF==1) iYrAdj(1:iPos)=1

do iYr=iYrBegin,size(DATA(:,:),dim=1)
  iC=0
  do ii=1,iComma+1
    if (DATA(iYr-iYrAdj(ii),iMon(ii))/=MISS) then
      iC=iC+1
      rTemp(iC)=DATA(iYr-iYrAdj(ii),iMon(ii))
    endif
  enddo
  if (iC>=MIN_DATA) then
    if (STAT=="MEAN".or.STAT=="mean") call mean(rTemp(1:iC),VALUE=rResult)
    if (STAT=="MEDIAN".or.STAT=="median") call median(rTemp(1:iC),VALUE=rResult)
    if (STAT=="MIN".or.STAT=="min") rResult=minval(rTemp(1:iC)) 
    if (STAT=="MAX".or.STAT=="max") rResult=maxval(rTemp(1:iC)) 
    if (STAT=="SUM".or.STAT=="sum") rResult=sum(rTemp(1:iC))
    if (STAT=="STDEV".or.STAT=="stdev") call standard_deviation(rTemp(1:iC),VALUE=rResult)
    if (STAT=="PERCENTILE".or.STAT=="percentile") call percentile(rTemp(1:iC),PTILE,VALUE=rResult)
    VALUE(iYr)=rResult
  endif
enddo
  
end subroutine month2seasonal

!*******************************************************************************

subroutine normal_dist(X,UPPER,VALUE)

   IMPLICIT NONE
   REAL(DP), INTENT(IN)   ::  X
   LOGICAL,   INTENT(IN)  ::  UPPER
   REAL, INTENT(OUT) :: VALUE

! SUMMARY:
! 
! VALUE returns the upper tail area (probability) (when UPPER=.true.) of the
! normal curve.  UPPER=.false. returns the lower tail area.
!
! Author: A. Miller

   !  Local variables
   REAL(DP), PARAMETER   ::  zero=0.0_DP, one=1.0_DP, half=0.5_DP, con=1.28_DP
   REAL(DP)              ::  z, y
   LOGICAL               ::  up

   !  Machine dependent constants
   REAL(DP), PARAMETER  ::  ltone = 7.0_DP, utzero = 18.66_DP
   REAL(DP), PARAMETER  ::  p = 0.398942280444_DP, q = 0.39990348504_DP,   &
                            r = 0.398942280385_DP, a1 = 5.75885480458_DP,  &
                            a2 = 2.62433121679_DP, a3 = 5.92885724438_DP,  &
                            b1 = -29.8213557807_DP, b2 = 48.6959930692_DP, &
                            c1 = -3.8052E-8_DP, c2 = 3.98064794E-4_DP,     &
                            c3 = -0.151679116635_DP, c4 = 4.8385912808_DP, &
                            c5 = 0.742380924027_DP, c6 = 3.99019417011_DP, &
                            d1 = 1.00000615302_DP, d2 = 1.98615381364_DP,  &
                            d3 = 5.29330324926_DP, d4 = -15.1508972451_DP, &
                            d5 = 30.789933034_DP

   up = upper
   z = x
   IF( z < zero ) THEN
      up = .NOT. up
      z = -z
   END IF
   IF( z <= ltone  .OR.  (up  .AND.  z <= utzero) ) THEN
      y = half*z*z
      IF( z > con ) THEN
         value = r*EXP( -y )/(z+c1+d1/(z+c2+d2/(z+c3+d3/(z+c4+d4/(z+c5+d5/(z+c6))))))
      ELSE
         value = half - z*(p-q*y/(y+a1+b1/(y+a2+b2/(y+a3))))
      END IF
   ELSE
      value = zero
   END IF

   IF( .NOT. up ) value = one - value 

   RETURN

end subroutine normal_dist

!*******************************************************************************

subroutine file_recs(FILE,UNIT,NUM_RECS)

character(len=*), intent(in) :: FILE
integer, intent(in) :: UNIT
integer, intent(out) :: NUM_RECS
integer :: iC,iStat
logical :: lExist

inquire(file=FILE,exist=lExist)
if (lExist.eqv..false.) stop "ERROR: stratus/file_recs: cannot find FILE"
iC=0
open(unit=UNIT,file=FILE,status='old')
do
  read(UNIT,*,IOSTAT=iStat)
  if (iStat==-1) exit
  iC=iC+1
enddo
close(UNIT)
NUM_RECS=iC

end subroutine file_recs 

!*******************************************************************************

subroutine percentile(X,PTILE,VALUE,MISS,MIN_DATA)

real, intent(in) :: X(:)
real, intent(in) :: PTILE
real, intent(out) :: VALUE
real, intent(in), optional :: MISS
integer, intent(in), optional :: MIN_DATA
integer :: iNum,ii,iC,iLow,iHigh
real :: rMemLoc,rFraction
real :: rTemp(size(X))

! SUMMARY:
!
! Calculate a specified percentile (PTILE) of a real values array X.
!
! If one of the optional args MISS or MIN_DATA is present, then the other is
! required to be present.

if (present(MISS).and..not.(present(MIN_DATA))) stop "ERROR: stratus/percentile: missing optional arg, MIN_DATA" 
if (present(MIN_DATA).and..not.(present(MISS))) stop "ERROR: stratus/percentile: missing optional arg, MISS"

if (present(MISS)) then
  VALUE=MISS
  if (count(X(:)/=MISS)<MIN_DATA) return
  iNum=(count(X(:)/=MISS))
  if (PTILE < 0.0 .or. PTILE > 100.0)  stop "ERROR: stratus/percentile: Input percentile out of range, 0 <= PTILE <= 100.0"

  if (iNum==1) then
    do ii=1,size(X)
      if (X(ii)==MISS) cycle
      VALUE = X(ii)
    enddo
  else
    iC=count(X(:)/=MISS)
    rTemp(1:iC) = pack(X(:),X(:)/=MISS)
    call sort(rTemp(1:iC))
    rMemLoc = (((real(iNum) - 1.0) / (100.0))*PTILE) + 1.0
    rFraction = rMemLoc - real(int(rMemLoc))
    iLow = int(rMemLoc)
    iHigh = int(rMemLoc)+1
    if (iHigh > iNum) then
      VALUE = rTemp(iLow)
    else
      VALUE = (((rTemp(iHigh) - rTemp(iLow))*rFraction)+rTemp(iLow))
    endif
  endif
else
  iNum=size(X)
  if (PTILE < 0.0 .or. PTILE > 100.0)  stop "ERROR: stratus/percentile: Input percentile out of range, 0 <= PTILE <= 100.0"

  if (iNum==1) then
    VALUE = X(iNum)
  else
    rTemp = X
    call sort(rTemp)
    rMemLoc = (((real(iNum) - 1.0) / (100.0))*PTILE) + 1.0
    rFraction = rMemLoc - real(int(rMemLoc))
    iLow = int(rMemLoc)
    iHigh = int(rMemLoc)+1
    if (iHigh > iNum) then
      VALUE = rTemp(iLow)
    else
      VALUE = (((rTemp(iHigh) - rTemp(iLow))*rFraction)+rTemp(iLow))
    endif
  endif
endif

end subroutine percentile

!*******************************************************************************

subroutine percent_rank(X,Y,VALUE,MISS,MIN_DATA)

real, dimension(:), intent(in) :: X
real, intent(in) :: Y
real, intent(out) :: VALUE
real, intent(in), optional :: MISS
integer, intent(in), optional :: MIN_DATA
integer :: iS,iL,iC,ii

if (present(MISS).and..not.(present(MIN_DATA))) stop "ERROR: stratus/percent_rank: missing optional arg, MIN_DATA" 
if (present(MIN_DATA).and..not.(present(MISS))) stop "ERROR: stratus/percent_rank: missing optional arg, MISS"

iS=0
iL=0
if (present(MISS)) then
  VALUE=MISS
  iC=0
  do ii=1,size(X)
    if (X(ii)/=MISS) then
      if (X(ii)>=Y) iL=iL+1
      if (X(ii)<Y) iS=iS+1
      iC=iC+1
    endif
  enddo
  if (iC>=MIN_DATA) then
    VALUE=(real(iS)/(real(iS)+real(iL)))*100.0
  endif
else
  do ii=1,size(X)
    if (X(ii)>=Y) iL=iL+1
    if (X(ii)<Y) iS=iS+1
  enddo 
  VALUE=(real(iS)/(real(iS)+real(iL)))*100.0
endif

end subroutine percent_rank

!*******************************************************************************

subroutine point_in_polygon(PX,PY,XX,YY,VALUE)

real, intent(in) :: PX
real, intent(in) :: PY
real, intent(in) :: XX(:)
real, intent(in) :: YY(:)
integer, intent(out) :: VALUE
integer :: N,I,J
real :: X(1:size(XX)),Y(1:size(YY))
logical ::  MX,MY,NX,NY

! SUMMARY:
!
! Given a "point" and a set of points that comprise a "polygon", return
!
! VALUE = -1 if point is outside of the polygon
! VALUE =  0 if point is on the polygon
! VALUE =  1 if point is inside polygon

      N=SIZE(YY)
6     DO I = 1, N
        X(I) = XX(I) - PX
        Y(I) = YY(I) - PY
      END DO
      VALUE=-1
      DO 2 I=1,N
      J=1+MOD(I,N)
      MX=X(I).GE.0.0
      NX=X(J).GE.0.0
      MY=Y(I).GE.0.0
      NY=Y(J).GE.0.0
      IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX)) GOTO 2
      IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX))) GOTO 3
      VALUE=-VALUE
      GOTO 2
!3     IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))) 2,4,5                       
3     IF(((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)))< 0.0) GOTO 2
      IF(((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)))== 0.0) GOTO 4
      IF(((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)))> 0.0) GOTO 5
4     VALUE=0
      RETURN
5     VALUE=-VALUE
2     CONTINUE
      RETURN
end subroutine point_in_polygon

!*******************************************************************************

subroutine point_in_polyfile(X,Y,POLYFILE,VALUE,FORCEREAD)

real, intent(in) :: X
real, intent(in) :: Y
character(len=*), intent(in) :: POLYFILE
integer, intent(out) :: VALUE
logical, optional, intent(in) :: FORCEREAD
integer, allocatable, dimension(:), save :: POLY_LENGTH
real, allocatable, dimension(:,:), save :: POLY_X
real, allocatable, dimension(:,:), save :: POLY_Y
real, parameter :: rMiss=-999.9
integer :: iF,iPoly,iNumVerts,iMaxVerts,iStat,ii,iVal
real :: rLati,rLong
character(len=200) :: cLine
character(len=200), save :: cPolyFileSave=""
logical :: lExist

iF=0
if (present(FORCEREAD)) then
  if (FORCEREAD.eqv..true.) iF=1 
endif
if (cPolyFileSave(1:len_trim(cPolyFileSave))/=POLYFILE(1:len_trim(POLYFILE))) iF=1

if (iF==1) then
  cPolyFileSave=POLYFILE
  inquire(file=POLYFILE,Exist=lExist)
  if (lExist.eqv..false.) stop "ERROR: stratus/point_in_polyfile: Cannot find Polyfile"
  iPoly=0
  iNumVerts=0
  iMaxVerts=0
  open(unit=44,file=POLYFILE)
  do
    read(44,'(a)',IOSTAT=iStat) cLine  
    if (iStat==-1) exit
    if (index(cLine(1:len_trim(cLine)),">")>0) then
      iPoly=iPoly+1
      iMaxVerts=max(iMaxVerts,iNumVerts)
      iNumVerts=0
    else
      if (iPoly==0) then
        iPoly=iPoly+1
        iNumVerts=0
      endif
      iNumVerts=iNumVerts+1
      iMaxVerts=max(iMaxVerts,iNumVerts)
      backspace(44)
      read(44,*) rLong,rLati
    endif 
  enddo
  close(44)
  if (index(cLine(1:len_trim(cLine)),">")>0) iPoly=iPoly-1
  if (iPoly==0.or.iMaxVerts==0) stop "ERROR: stratus/point_in_polyfile: polygon file contains no data"
  if (allocated(POLY_X)) deallocate(POLY_X)
  if (allocated(POLY_Y)) deallocate(POLY_Y)
  if (allocated(POLY_LENGTH)) deallocate(POLY_LENGTH)
  allocate(POLY_X(1:iPoly,1:iMaxVerts))
  allocate(POLY_Y(1:iPoly,1:iMaxVerts))
  allocate(POLY_LENGTH(1:iPoly))
  POLY_X=rMiss
  POLY_Y=rMiss
  iPoly=0
  iNumVerts=0
  open(unit=44,file=POLYFILE)
  do
    read(44,'(a)',IOSTAT=iStat) cLine  
    if (iStat==-1) exit
    if (index(cLine(1:len_trim(cLine)),">")>0) then
      if (iPoly>0) POLY_LENGTH(iPoly)=iNumVerts
      iPoly=iPoly+1
      iNumVerts=0
    else
      if (iPoly==0) then
        iPoly=iPoly+1
        iNumVerts=0
      endif
      iNumVerts=iNumVerts+1
      POLY_LENGTH(iPoly)=iNumVerts
      backspace(44)
      read(44,*) rLong,rLati
      POLY_X(iPoly,iNumVerts)=rLong
      POLY_Y(iPoly,iNumVerts)=rLati
    endif 
  enddo
  close(44)
endif

VALUE=-1
if (allocated(POLY_X).eqv..false.) stop "ERROR: stratus/point_in_polyfile: POLY_X array not allocated"
if (allocated(POLY_Y).eqv..false.) stop "ERROR: stratus/point_in_polyfile: POLY_Y array not allocated"
if (allocated(POLY_LENGTH).eqv..false.) stop "ERROR: stratus/point_in_polyfile: POLY_LENGTH array not allocated"
do ii=1,size(POLY_X(:,:),dim=1)
  call point_in_polygon(X,Y,POLY_X(ii,1:POLY_LENGTH(ii)),POLY_Y(ii,1:POLY_LENGTH(ii)),iVal)
  if (iVal==1) then
    VALUE=1
    exit
  endif
  if (iVal==0) then
    VALUE=0
    exit
  endif
enddo

end subroutine point_in_polyfile

!*******************************************************************************

subroutine radians2degrees(RADIANS,DEGREES)

real, intent(in) :: RADIANS
real, intent(out) :: DEGREES

! SUMMARY:
!
! convert real value RADIANS to DEGREES
!
! DEGREES = RADIANS*180.0/PI

DEGREES=RADIANS*180.0/PI

end subroutine radians2degrees

!*******************************************************************************

subroutine rank(X,RANKS)

real, dimension(:), intent(in) :: X
integer, dimension(:), intent(out) :: RANKS
integer, dimension(1:size(RANKS)) :: iIndex          
integer :: iLoop

! SUMMARY:
!
! rank the real valued array X in ascending order, and return the integer
! rank values back in the array RANKS

call indices(X,iIndex)
do iLoop = 1,size(X)
  RANKS(iIndex(iLoop)) = iLoop
enddo

return

end subroutine rank

!*******************************************************************************

subroutine skew(X,VALUE,MISS,MIN_DATA)

real, intent(in) :: X(:)
real, intent(out) :: VALUE
real, intent(in), optional :: MISS
integer, intent(in), optional :: MIN_DATA
real :: rNum,rTerm1,rXMean,rXStDev,rTerm2
integer :: iLoop

! SUMMARY:
!
! Calculate the skewness of a series of real valued numbers in array X
!
!              n
! skew = ------------- sigma( [(Xi - X(bar)) / s] )**3.0
!         (n-1)*(n-2)
!
! where "s" is the standard deviation.
!
! If one of the optional args MISS or MIN_DATA is present, then the other is
! required to be present.

if (present(MISS).and..not.(present(MIN_DATA))) stop "ERROR: stratus/skew: missing optional arg, MIN_DATA" 
if (present(MIN_DATA).and..not.(present(MISS))) stop "ERROR: stratus/skew: missing optional arg, MISS"

if (present(MISS)) then
  VALUE=MISS
  if (count(X(:)/=MISS)<MIN_DATA) return
  rNum=real(count(X(:)/=MISS))
  if (size(X)<2) stop "ERROR: stratus/skew: Input array size too small (e.g. < 2)"
  
  rTerm1= (rNum / ((rNum-1.0)*(rNum-2.0)))
  call mean(X,rXMean,MISS=MISS,MIN_DATA=MIN_DATA)
  call standard_deviation(X,rXStDev,MISS=MISS,MIN_DATA=MIN_DATA)

  rTerm2=0.0
  do iLoop=1,size(X)
    if (X(iLoop)==MISS) cycle
    rTerm2 = rTerm2 + ( (((X(iLoop)-rXMean)/rXStDev)) **3.0)
  enddo

  VALUE=rTerm1*rTerm2
else
  rNum=real(size(X))
  if (size(X)<2) stop "ERROR: stratus/skew: Input array size too small (e.g. < 2)"
  
  rTerm1= (rNum / ((rNum-1.0)*(rNum-2.0)))
  call mean(X,rXMean)
  call standard_deviation(X,rXStDev)

  rTerm2=0.0
  do iLoop=1,size(X)
    rTerm2 = rTerm2 + ( (((X(iLoop)-rXMean)/rXStDev)) **3.0)
  enddo

  VALUE=rTerm1*rTerm2
endif

end subroutine skew

!*******************************************************************************

! SOURCE(S): 
!   routine is in Public Domain, and available from 
!   www.netlib.org/napack/sort.f, author unknown

subroutine sort(X,SORT_INDICES)

real, intent(inout) :: X(:)
integer, allocatable, dimension(:), optional :: SORT_INDICES
real Y(size(X))
real S,T
integer I,J,K,L,M,N,P,Q,W(size(X))

! SUMMARY:
!
! Sort the real valued array X in ascending order and return the order in the
! integer array SORT_INDICES
!
! Public Domain (www.netlib.org/napack/sort.f), author unknown

N=size(X)

if (present(SORT_INDICES)) then

      allocate(SORT_INDICES(1:N))
      I = 1
1000    K = I
2000    J = I
      SORT_INDICES(I) = I
      I = I + 1
      IF ( J .EQ. N ) GOTO 3000
      IF ( X(I) .GE. X(J) ) GOTO 2000
      W(K) = I
      GOTO 1000
  3000  IF ( K .EQ. 1 ) RETURN
      W(K) = N + 1
  4000  M = 1
      L = 1
  5000  I = L
      IF ( I .GT. N ) GOTO 12000
      P = SORT_INDICES(I)
      S = X(P)
      J = W(I)
      K = J
      IF ( J .GT. N ) GOTO 10000
      Q = SORT_INDICES(J)
      T = X(Q)
      L = W(J)
      SORT_INDICES(I) = L
  6000  IF ( S .GT. T ) GOTO 7000
      W(M) = P
      M = M + 1
      I = I + 1
      IF ( I .EQ. K ) GOTO 8000
      P = SORT_INDICES(I)
      S = X(P)
      GOTO 6000
  7000  W(M)= Q
      M = M + 1
      J = J + 1
      IF ( J .EQ. L ) GOTO 11000
      Q = SORT_INDICES(J)
      T = X(Q)
      GOTO 6000
  8000  W(M) = Q
      K = M + L - J
      I = J - M
  9000  M = M + 1
      IF ( M .EQ. K ) GOTO 5000
      W(M) = SORT_INDICES(M+I)
      GOTO 9000
  10000 SORT_INDICES(I) = J
      L = J
  11000 W(M) = P
      K = M + K - I
      I = I - M
      GOTO 9000
  12000 I = 1
  13000 K = I
      J = SORT_INDICES(I)
  14000 SORT_INDICES(I) = W(I)
      I = I + 1
      IF ( I .LT. J ) GOTO 14000
      W(K) = I
      IF ( I .LE. N ) GOTO 13000
      IF ( K .GT. 1 ) GOTO 4000
      RETURN
else
      I = 1
  10  K = I
  20  J = I
      I = I + 1
      IF ( J .EQ. N ) GOTO 30
      IF ( X(I) .GE. X(J) ) GOTO 20
      Y(K) = REAL(I)
      GOTO 10
  30  IF ( K .EQ. 1 ) RETURN
      Y(K) = REAL(N + 1)
  40  M = 1
      L = 1
  50  I = L
      IF ( I .GT. N ) GOTO 120
      S = X(I)
      J = INT(Y(I))
      K = J
      IF ( J .GT. N ) GOTO 100
      T = X(J)
      L = INT(Y(J))
      X(I) = L
  60  IF ( S .GT. T ) GOTO 70
      Y(M) = S
      M = M + 1
      I = I + 1
      IF ( I .EQ. K ) GOTO 80
      S = X(I)
      GOTO 60
  70  Y(M)= T
      M = M + 1
      J = J + 1
      IF ( J .EQ. L ) GOTO 110
      T = X(J)
      GOTO 60
  80  Y(M) = T
      K = M + L - J
      I = J - M
  90  M = M + 1
      IF ( M .EQ. K ) GOTO 50
      Y(M) = X(M+I)
      GOTO 90
  100 X(I) = J
      L = J
  110 Y(M) = S
      K = M + K - I
      I = I - M
      GOTO 90
  120 I = 1
  130 K = I
      J = X(I)
  140 X(I) = Y(I)
      I = I + 1
      IF ( I .LT. J ) GOTO 140
      Y(K) = REAL(I)
      IF ( I .LE. N ) GOTO 130
      IF ( K .EQ. 1 ) RETURN
      GOTO 40
endif

end subroutine sort

!*******************************************************************************

subroutine sort_char(X,SORT_INDICES)

! Sort character strings

character(len=*), intent(inout), dimension (:) :: X
integer, allocatable, dimension(:), optional :: SORT_INDICES
character(len=len(X)) :: Temp                                        
integer :: LL,KK,JJ,II,i,K2

! SUMMARY:
!
! Sort character strings in ascending order and return the order in the
! integer array SORT_INDICES.

if (present(SORT_INDICES)) then
  if (allocated(SORT_INDICES)) deallocate(SORT_INDICES)
endif

if (present(SORT_INDICES)) then
   allocate(SORT_INDICES(1:size(X)))
   do i=1,size(SORT_INDICES)
     SORT_INDICES(i)=i
   enddo
   KK=ubound(X,1); LL=KK/2
   do
      if (LL<1) exit
      do  JJ=1,KK-LL
         if (X(JJ)>X(JJ+LL)) then
            Temp=X(JJ+LL)
            K2=SORT_INDICES(JJ+LL)
            do  II=JJ,1,-LL
               if (X(II)<Temp) exit
               X(II+LL)=X(II)
               SORT_INDICES(II+LL)=SORT_INDICES(II)
            end  do
            X(II+LL)=Temp
            SORT_INDICES(II+LL)=K2
         endif
      enddo
      LL=LL/2
      if (modulo(LL,2)==0) LL=LL-1
   enddo
else
 KK=ubound(X,1); LL=KK/2
   do
      if (LL<1) exit
      do  JJ=1,KK-LL
         if (X(JJ)>X(JJ+LL)) then
            Temp=X(JJ+LL)
            do  II=JJ,1,-LL
               if (X(II) < Temp) exit
               X(II+LL)=X(II)
            enddo
            X(II+LL)=Temp
         endif
      enddo
      LL=LL/2
      if (modulo(LL,2)==0) LL=LL-1
   end do
endif

end subroutine sort_char

!*******************************************************************************

subroutine spatial_mask(LON,LAT,POLYFILE,MASK)

real, dimension(:), intent(in) :: LON
real, dimension(:), intent(in) :: LAT
logical, dimension(:), intent(out) :: MASK
character(len=*), intent(in), optional :: POLYFILE
integer, parameter :: MAX_PTS=1000000                                 
character(len=01) :: cLine
real, dimension(1:MAX_PTS) :: rX,rY
integer, dimension(1:size(LON)) :: iDone
integer :: iPoly,iC,iStat,ii
logical :: lExist

! SUMMARY:
!
! Returns a logical mask array which is populated by finding all locations
! in each (LON,LAT) pair that are also contained within POLYFILE.

mask(:)=.false.

if ( (size(lon)/=size(lat)).or. (size(lon)/=size(mask)) ) stop "ERROR: stratus/spatial_mask: LON,LAT, and MASK different size(s)"
if (present(POLYFILE)) then
  inquire(file=POLYFILE,exist=lExist)
  if (lExist.eqv..false.) stop "ERROR: stratus/spatial_mask: POLYFILE not found"
endif

iC=0
iDone(:)=0
open(unit=48,file=POLYFILE)
do
  read(48,'(a)',IOSTAT=iStat) cLine
  if (iStat==-1) exit
  if (cLine(1:1)/=">") then
    backspace(48)
    iC=iC+1
    if (iC>MAX_PTS) stop "ERROR: stratus/spatial_mask: Number of vertices within a polygon exceeds 1,000,000"
    read(48,*) rX(iC),rY(iC)
  else
    do ii=1,size(LON) 
      if (iDone(ii)==1.or.iC==0) cycle
      call point_in_polygon(LON(ii),LAT(ii),rX(1:iC),rY(1:iC),VALUE=iPoly)
      if (iPoly>=0) then
        iDone(ii)=1
        mask(ii)=.true.
      endif
    enddo
    iC=0
  endif
enddo
close(48)
if (iC>0) then
  do ii=1,size(LON) 
    if (iDone(ii)==1.or.iC==0) cycle
    call point_in_polygon(LON(ii),LAT(ii),rX(1:iC),rY(1:iC),VALUE=iPoly)
    if (iPoly>=0) then
      iDone(ii)=1
      mask(ii)=.true.
    endif
  enddo
endif

end subroutine spatial_mask

!*******************************************************************************

subroutine split(STRING,DELIMITER,SPLIT_INDICES,PRESERVE_NULL)

character (len=*), intent(in) :: STRING
integer, allocatable, dimension(:) :: SPLIT_INDICES
character (len=01), intent(in) :: DELIMITER
character (len=*), intent(in), optional :: PRESERVE_NULL
integer, parameter :: MAXLENGTH = 100000
integer, dimension(1:MAXLENGTH) :: iLoc
integer :: iC,ii

if (allocated(SPLIT_INDICES)) deallocate(SPLIT_INDICES)
if (len(string)>MAXLENGTH) stop "ERROR: stratus/split1 STRING exceeds maximum length of 100000"
if (len(string)==0) stop "ERROR: stratus/split1: STRING has length=0"

iC=0
if (STRING(1:1)/=DELIMITER) then
  iC=1
  iLoc(iC)=1
endif
if (len(STRING)==1) then
  allocate(SPLIT_INDICES(1:2))
  SPLIT_INDICES(1)=1
  SPLIT_INDICES(2)=2
else
  do ii=2,len(STRING)
    if (STRING(ii-1:ii-1)/=DELIMITER.and.STRING(ii:ii)==DELIMITER) then
      iC=iC+1
      iLoc(iC)=ii-1
    endif
    if (STRING(ii-1:ii-1)==DELIMITER.and.STRING(ii:ii)/=DELIMITER) then
      iC=iC+1
      iLoc(iC)=ii
    endif
  enddo
  if (STRING(len(STRING):len(STRING))/=DELIMITER) then
    iC=iC+1
    iLoc(iC)=len(STRING)
  endif
  allocate(SPLIT_INDICES(1:iC))
  do ii=1,iC
    SPLIT_INDICES(ii)=iLoc(ii)
  enddo
endif

end subroutine split

!*******************************************************************************

subroutine standard_deviation(X,VALUE,BIWEIGHT,MISS,MIN_DATA)

real, intent(in) :: X(:)
real, intent(out) :: VALUE
real, intent(in), optional :: BIWEIGHT
real, intent(in), optional :: MISS
integer, intent(in), optional :: MIN_DATA
real :: rMedian,rMad,rN,rD,rC,rVariance
integer :: iLoop,iC

! SUMMARY:
!
! VALUE returns the standard deviation of series X.
!
!                              sigma[ Xi - X(bar) ]**2.0
! standard_deviation = sqrt [ --------------------------- ]
!                                      (n - 1)
!
! If one of the optional args MISS or MIN_DATA is present, then the other is
! required to be present.

real :: rWeight(size(X))                                             


if (present(MISS)) then
  VALUE=MISS
  if (count(X(:)/=MISS)<MIN_DATA) return
  if (present(BIWEIGHT)) then
    call median(X,rMedian,MISS=MISS,MIN_DATA=MIN_DATA)
    call median_absolute_deviation(X,rMad,MISS=MISS,MIN_DATA=MIN_DATA)
    do iLoop=1,size(X)
      if (X(iLoop)==MISS) cycle
      if (rMad==0.0) then
        rWeight(iLoop)=0.0
      else
        rWeight(iLoop)=(X(iLoop)-rMedian)/(BIWEIGHT*rMad)
      endif
      if (abs(rWeight(iLoop))>=1.0) then
        rWeight(iLoop)=1.0
      endif
    enddo
    rN=0.0
    rD=0.0
    rC=0.0
    do iLoop=1,size(X)
      if (X(iLoop)==MISS) cycle
      rC=rC+1.0
      rN=rN+((X(iLoop)-rMedian)**2.0)*((1.0-rWeight(iLoop)**2.0)**4.0)
      rD=rD+((1.0-(rWeight(iLoop)**2.0))**2.0)*(1.0-(5.0*(rWeight(iLoop)**2.0)))
    enddo
    rN=sqrt(rN*rC)
    rD=abs(rD)
    VALUE=rN/rD
  else
    call variance(X,rVariance,MISS=MISS,MIN_DATA=MIN_DATA)
    VALUE = sqrt(rVariance)
  endif
else
  if (present(BIWEIGHT)) then
    call median(X,rMedian)
    call median_absolute_deviation(X,rMad)
    do iLoop=1,size(X)
      if (rMad==0.0) then
        rWeight(iLoop)=0.0
      else
        rWeight(iLoop)=(X(iLoop)-rMedian)/(BIWEIGHT*rMad)
      endif
      if (abs(rWeight(iLoop))>=1.0) then
        rWeight(iLoop)=1.0
      endif
    enddo
    rN=0.0
    rD=0.0
    do iLoop=1,size(X)
      rN=rN+((X(iLoop)-rMedian)**2.0)*((1.0-rWeight(iLoop)**2.0)**4.0)
      rD=rD+((1.0-(rWeight(iLoop)**2.0))**2.0)*(1.0-(5.0*(rWeight(iLoop)**2.0)))
    enddo
    rN=sqrt(rN*size(X))
    rD=abs(rD)
    VALUE=rN/rD
  else
    call variance(X,rVariance)
    VALUE = sqrt(rVariance)
  endif
endif

end subroutine standard_deviation

!*******************************************************************************

subroutine string_class(string,class)

character(len=*), intent(in) :: string
character(len=*), intent(out) :: class
character(len=11) :: cNegativeInteger = "-0123456789"
character(len=10) :: cPositiveInteger = "0123456789"
character(len=12) :: cNegativeFloat = "-0123456789."
character(len=11) :: cPositiveFloat = "0123456789."
character(len=62) :: cAlphanumeric = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
character(len=52) :: cAlphabetic = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
character(len=26) :: cAlphabeticUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
character(len=26) :: cAlphabeticLower = "abcdefghijklmnopqrstuvwxyz"

if (len(class)<20) stop "ERROR: subroutine: string_class, output variable class must be char length >= 20"

class="any"
if (verify(string,cAlphanumeric)==0) class="alphanumeric"
if (verify(string,cNegativeFloat)==0) class="negative_float"
if (verify(string,cPositiveFloat)==0) class="positive_float"
if (verify(string,cNegativeInteger)==0) class="negative_integer"
if (verify(string,cPositiveInteger)==0) class="positive_integer"
if (verify(string,cAlphabetic)==0) class="alphabetic"
if (verify(string,cAlphabeticUpper)==0) class="alphabetic_upper"
if (verify(string,cAlphabeticLower)==0) class="alphabetic_lower"

end subroutine string_class

!*******************************************************************************

subroutine string_split(STRING,DELIMITER,STRING_TYPE,PRESERVE_NULL)

type(string_info) :: string_type
character (len=*), intent(in) :: STRING
integer, allocatable, dimension(:) :: SPLIT_INDICES
character (len=01), intent(in) :: DELIMITER
character (len=*), intent(in), optional :: PRESERVE_NULL
integer, parameter :: MAXLENGTH = 100000
integer, dimension(1:MAXLENGTH) :: iLoc
integer :: iC,ii

if (allocated(SPLIT_INDICES)) deallocate(SPLIT_INDICES)
if (len(string)>MAXLENGTH) stop "ERROR: stratus/split1 STRING exceeds maximum length of 100000"
if (len(string)==0) stop "ERROR: stratus/split1: STRING has length=0"

call split(string,delimiter,SPLIT_INDICES)

string_type%number_of_strings=0
if (size(SPLIT_INDICES(:))>=2) string_type%number_of_strings=size(SPLIT_INDICES(:))/2

if (string_type%number_of_strings>0) then
  if (allocated(string_type%length)) deallocate(string_type%length)
  if (allocated(string_type%value)) deallocate(string_type%value)
  if (allocated(string_type%class)) deallocate(string_type%class)
  allocate(string_type%length(1:string_type%number_of_strings))
  allocate(string_type%value(1:string_type%number_of_strings))
  allocate(string_type%class(1:string_type%number_of_strings))
  iC=1
  do ii=1,string_type%number_of_strings
    string_type%value(ii)=string(SPLIT_INDICES(iC):SPLIT_INDICES(iC+1))
    string_type%length(ii)=len_trim(string_type%value(ii))
    call string_class(string(SPLIT_INDICES(iC):SPLIT_INDICES(iC+1)),string_type%class(ii))
    iC=iC+2 
  enddo
endif 

end subroutine string_split

!*******************************************************************************

subroutine students_t_dist(TCRIT,DF,VALUE)

! DESCRIPTION
!   calculate the upper (if T >= 0) or lower tail (if T < 0) area under 
!   Student's t-distribution

real, intent(in) :: TCRIT
integer, intent(in) :: DF
real, intent(out) :: VALUE

! SUMMARY:
!
! VALUE returns the upper (if TCRIT >= 0) or lower tail (if TCRIT < 0) area
! under Student's t-distribution.
!
! Original F90 code by A. Miller
!
!     Local variables

REAL     :: v, x, tt
REAL, PARAMETER  :: four = 4.0, one = 1.0, half = 0.5
REAL, PARAMETER  :: a1 = 0.09979441, a2 = -0.581821, a3 = 1.390993,  &
                    a4 = -1.222452, a5 = 2.151185
REAL, PARAMETER  :: b1 = 5.537409, b2 = 11.42343
REAL, PARAMETER  :: c1 = 0.04431742, c2 = -0.2206018, c3 = -0.03317253,  &
                    c4 = 5.679969, c5 = -12.96519
REAL, PARAMETER  :: d1 = 5.166733, d2 = 13.49862
REAL, PARAMETER  :: e1 = 0.009694901, e2 = -0.1408854, e3 = 1.88993,  &
                    e4 = -12.75532, e5 = 25.77532
REAL, PARAMETER  :: f1 = 4.233736, f2 = 14.3963
REAL, PARAMETER  :: g1 = -9.187228E-5, g2 = 0.03789901, g3 = -1.280346,  &
                    g4 = 9.249528, g5 = -19.08115
REAL, PARAMETER  :: h1 = 2.777816, h2 = 16.46132
REAL, PARAMETER  :: i1 = 5.79602E-4, i2 = -0.02763334, i3 = 0.4517029,  &
                    i4 = -2.657697, i5 = 5.127212
REAL, PARAMETER  :: j1 = 0.5657187, j2 = 21.83269

!     Check that number of degrees of freedom > 4.

if (real(DF) <= four) stop "ERROR: stratus/students_t: degrees of freedom <= 4"

!     Evaluate series.

v = one / real(DF)
tt = ABS(TCRIT)
x = half*(one +   &
    tt*(((a1 + v*(a2 + v*(a3 + v*(a4 + v*a5)))) / (one - v*(b1 - v*b2))) +  &
    tt*(((c1 + v*(c2 + v*(c3 + v*(c4 + v*c5)))) / (one - v*(d1 - v*d2))) +  &
    tt*(((e1 + v*(e2 + v*(e3 + v*(e4 + v*e5)))) / (one - v*(f1 - v*f2))) +  &
    tt*(((g1 + v*(g2 + v*(g3 + v*(g4 + v*g5)))) / (one - v*(h1 - v*h2))) +  &
    tt*((i1 + v*(i2 + v*(i3 + v*(i4 + v*i5)))) / (one - v*(j1 - v*j2))) ))))) ** (-8)
VALUE = x

end subroutine students_t_dist

!*******************************************************************************

subroutine t_test(X,Y,MEAN1,MEAN2,VARIANCE1,VARIANCE2,DF,T_CRITICAL,T_DISTRIBUTION,PVALUE)

real, dimension(:), intent(in) :: X
real, dimension(:), intent(in) :: Y
real, intent(in) :: MEAN1
real, intent(in) :: MEAN2
real, intent(in) :: VARIANCE1
real, intent(in) :: VARIANCE2
integer, intent(out) :: DF
real, intent(out) :: T_CRITICAL
real, intent(out) :: T_DISTRIBUTION
real, intent(out) :: PVALUE

real :: x_error
real :: y_error
real :: standard_error

if(MEAN1 == -9999 .or. &
   MEAN2 == -9999 .or. &
   VARIANCE1 == -9999 .or. &
   VARIANCE2 == -9999) then

  T_CRITICAL = -9999
  PVALUE = -9999
else
  !Calculate master standard error, candidate standard error, and then total
  !standard error
  if ( (VARIANCE1/(size(X)-1)) .ge. 0.0) then
    x_error = sqrt(VARIANCE1 / (size(X)-1))
  else
    x_error=0.0
  endif
  if ( (VARIANCE2 / (size(Y)-1)) .ge. 0.0) then
    y_error = sqrt(VARIANCE2 / (size(Y)-1))
  else
    y_error = 0.0
  endif
  standard_error = sqrt((x_error**2) + (y_error**2))

  !calculate degrees of freedom from above standard errors
  DF = standard_error**4 / ((x_error**4 / (size(X)-2))  + (y_error**4 /(size(Y)-2)))

  !calculate t-test / p-value
  T_CRITICAL = (MEAN1 - MEAN2) /  standard_error
  if (DF > 4) then
    call students_t_dist(T_CRITICAL,DF,T_DISTRIBUTION)
    PVALUE = 2*T_DISTRIBUTION
  else
    PVALUE=-999.9
  endif
endif

end subroutine t_test


!*******************************************************************************

subroutine uppercase(STRING,UPPERSTRING)

! converts a string to all upper case

character(len=*), intent(in) :: STRING
character(len=*), intent(out) :: UPPERSTRING
character(len=01), dimension(1:26) :: cLower, cUpper
integer :: ii,jj

if (len(STRING)>len(UPPERSTRING)) stop "ERROR: stratus: uppercase length"

cLower(:)=(/"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r",&
            "s","t","u","v","w","x","y","z"/)
cUpper(:)=(/"A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R",&
            "S","T","U","V","W","X","Y","Z"/)

UPPERSTRING=STRING
do ii=1,len(UPPERSTRING)
  do jj=1,26
    if (UPPERSTRING(ii:ii)==cLower(jj)) UPPERSTRING(ii:ii)=cUpper(jj)
  enddo 
enddo

end subroutine uppercase

!*******************************************************************************

subroutine variance(X,VALUE,MISS,MIN_DATA)

real, intent(in) :: X(:)
real, intent(out) :: VALUE
real, intent(in), optional :: MISS
integer, intent(in), optional :: MIN_DATA
integer :: iNum,ii
real :: rMeanValue,rSum

! SUMMARY:
!
! VALUE returns the variance of series X
!
!             sigma[ Xi - X(bar) ]**2.0
! variance = ---------------------------
!                     (n - 1)
! 
! If one of the optional args MISS or MIN_DATA is present, then the other is
! required to be present.

if (present(MISS).and..not.present(MIN_DATA)) stop "ERROR: stratus/variance: optional arg MIN_DATA missing"

if (present(MISS)) then
  VALUE=MISS
  if (count(X(:)/=MISS)<MIN_DATA) return
  iNum=(count(X(:)/=MISS))
  if (iNum.eq.1) then
    VALUE=X(1)
  else
    call mean(X,rMeanValue,MISS=MISS,MIN_DATA=MIN_DATA)
    rSum=0.0
    do ii=1,size(X)
      if (X(ii)==MISS) cycle
      rSum=rSum+((X(ii)-rMeanValue)**2.0)
    enddo
    VALUE = ( rSum / real(iNum-1) )
  endif
else
  iNum=size(X)
  if (iNum.eq.1) then
    VALUE=X(1)
  else
    call mean(X,rMeanValue)
    rSum=0.0
    do ii=1,size(X)
      rSum=rSum+((X(ii)-rMeanValue)**2.0)
    enddo
    VALUE = ( rSum / real(iNum-1) )
  endif
endif

end subroutine variance

!*******************************************************************************

function Compass(rLat1,rLon1,rLat2,rLon2)

     ! Given the geographical coordinates of two points on the Earth's
     ! surface, compute the compass heading from the central point 
     ! (rLat1, rLon1) to the secondary point (rLat2, rLon2).  
     ! Angles given in degrees.
     !
     ! Uses: degrees_to_radians in cirrus.f95
     !       radians_to_degrees in cirrus.f95 
     ! 
     ! John Caesar, Hadley Centre, 31/8/05 

     ! Declarations

        real, intent(in) :: rLat1
        real, intent(in) :: rLon1
        real, intent(in) :: rLat2
        real, intent(in) :: rLon2
        real :: zz,xx,yy,az,y1,y2,x1,x2,compass

      ! Convert degrees to radians

      call degrees2radians(90.0-rLat1,y1)
      call degrees2radians(90.0-rLat2,y2)

      call degrees2radians(rLon1,x1)
      call degrees2radians(rLon2,x2)

!      zz=cos(y1)*cos(y2)+sin(y1)*sin(y2)*cos(x2-x1)     
      xx=sin(y2)*sin(x2-x1)
      yy=sin(y1)*cos(y2)-cos(y1)*sin(y2)*cos(x2-x1)

      ! Determine angle

      if (xx==0.0.and.yy==0.0) then
        write (*,*) rLat1,rLon1,rLat2,rLon2
        stop
      endif
      az=atan2(xx,yy)

      ! Convert result back to degrees    

      call radians2degrees(az,compass)

  end function Compass


end module stratus
