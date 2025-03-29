program climat_decoder

! National Climatic Data Center (NCDC)

! Version 1.0, 05/31/2014

! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC DOMAIN
! AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE.  THEY ARE FURNISHED 
! "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS INSTRUMENTALITIES, 
! OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY, EXPRESS OR IMPLIED, AS TO
! THE USEFULNESS OF THE SOFTWARE AND DOCUMENTATION FOR ANY PURPOSE. THEY
! ASSUME NO RESPONSIBILITY (1) FOR THE USE OF THE SOFTWARE AND DOCUMENTATION;
! OR (2) TO PROVIDE TECHNICAL SUPPORT TO USERS.

use stratus ! stratus 1.4

implicit none

type(string_info) :: string
integer, parameter :: EOF=-1
integer :: EOL                     ! END OF LINE
integer :: MISS=-9999
integer :: iMonth
integer :: iYear
integer :: iStat
integer :: i111,i222,i333,i444,i03,i04,i06,iMax,iMin,iPrcp,iMean
real :: rPrcp,rMin, rMax, rMean, rSign
real, parameter :: rMiss=-999.9
character(len=02), dimension(1:12) :: months = (/"01","02","03","04","05","06","07","08","09","10","11","12"/)
character(len=05) :: cWMO
character(len=200) :: cInfile,cOutfile
character(len=10000) :: cLine

call getarg(1,cInfile)
call getarg(2,cOutfile)

if (cInfile=="--version") stop "climat_decoder: version 1.0, 05/31/2014"

if (cInfile=="".or.cOutfile=="") stop "USAGE: climat_decode: <infile> <outfile>"
 
iMonth=MISS
iYear=MISS

open(unit=13,file=cOutfile,status='replace')
open(unit=12,file=cInfile,status='old')
do
  read(12,'(a)',IOSTAT=iStat) cLine
  if (iStat==EOF) exit

  call string_split(cLine," ",string)

  if (len_trim(cLine)==5) then 
    if (string%class(1)=="positive_integer".and.string%length(1)==5) then
      cWMO=trim(string%value(1))
      cycle
    endif
  endif
 

  if (index(cLine,"CLIMAT ")>0) then
    if (string%class(2)=="positive_integer".and.string%length(2)==5) then
      read(string%value(2)(1:2),'(i2)',IOSTAT=iStat) iMonth
      if (iStat>0) then
        iMonth=MISS
        iYear=MISS
        cycle
      endif
      if (iMonth>=1.and.iMonth<=12) then
        read(string%value(2)(3:5),'(i3)',IOSTAT=iStat) iYear
        if (iStat>0) then
          iYear=MISS 
          iMonth=MISS
          cycle
        else
          iYear=2000+iYear
          if (iYear < 2000 .or. iYear > 2030) then 
            iYear=MISS
            iMonth=MISS
            cycle
          endif 
        endif
      endif
    endif
    if (string%class(3)=="positive_integer".and.string%length(3)==5) cWMO=string%value(3)
  endif

  if (index(cLine,"NIL")>0) then
    cycle
  endif

  i111=index(cLine," 111 ")  ! monthly averaged meteorlogical values
  if (i111>0) i222=index(cLine(i111:)," 222 ")  ! normal climatological values for the month
  if (i111>0) i333=index(cLine(i111:)," 333 ")  ! number of days with parameters beyond certain thresholds
  if (i111>0) i444=index(cLine(i111:)," 444 ")  ! extreme values and frequencies
  if (cLine(1:4)=="111 ") i111=1

  if (i111==0) then
    if (string%number_of_strings>=4) then
      if (string%length(1)==5.and.string%class(1)=="positive_integer") then
        if (index(cLine(6:)," 3")>0.and.index(cLine(6:)," 4")>0.and.index(cLine(6:)," 6")>0) i111=6
      endif
    endif
  endif

  if (i111>0) then
    if (string%class(1)=="positive_integer".and.string%length(1)==5) then
      cWMO=trim(string%value(1))
    endif
    EOL=0
    if (any( (/i222,i333,i444/) /= 0)) EOL=minval( (/i222,i333,i444/), (/i222,i333,i444/)>0 )
    if (EOL<i111) EOL=len_trim(cLine)

    i03=index(cLine(i111:EOL)," 3")    ! mean monthly temperature and standard deviation	
    i04=index(cLine(i111:EOL)," 4")    ! mean daily maximum and minimum temperature
    i06=index(cLine(i111:EOL)," 6")    ! precipitation

    rMean=rMiss; rMax=rMiss; rMin=rMiss; rPrcp=rMiss   ! Initialize all output data to missing

    if (i03>0) then    ! Process Section 111, Group 3 (monthly mean temperature and standard deviation data)
      rMean=rMiss
      read(cLine(i111+(i03+2):i111+(i03+4)),'(i4)',IOSTAT=iStat) iMean
      if (iStat==0) then 
        rSign=-1.0
        if (cLine(i111+(i03+1):i111+(i03+1))=="0") rSign=1.0
        rMean=rSign*(real(iMean)/10.0)
      endif
    endif

    if (i04>0) then    ! Process Section 111, Group 4 (mean daily maximum temperature)
      rMax=rMiss
      read(cLine(i111+(i04+2):i111+(i04+4)),'(i3)',IOSTAT=iStat) iMax
      if (iStat==0) then
        rSign=-1.0
        if (cLine(i111+(i04+1):i111+(i04+1))=="0") rSign=1.0
        rMax=rSign*(real(iMax)/10.0)
      endif
    endif

    if (i04>0) then    ! Process Section 111, Group 4 (mean daily minimum temperature)
      rMin=rMiss
      read(cLine(i111+(i04+6):i111+(i04+8)),'(i3)',IOSTAT=iStat) iMin
      if (iStat==0) then
        rSign=-1.0
        if (cLine(i111+(i04+5):i111+(i04+5))=="0") rSign=1.0
        rMin=rSign*(real(iMin)/10.0)
      endif
    endif

    if (i06>0) then    ! Process Section 111, Group 5 (precipitation)
      rPrcp=rMiss
      read(cLine(i111+(i06+1):i111+(i06+4)),'(i4)',IOSTAT=iStat) iPrcp
      if (iStat==0) then
        rPrcp=real(iPrcp)
      endif
    endif

    if (any((/rMean,rMax,rMin,rPrcp/)/=rMiss).and.(iYear/=MISS.and.iMonth/=MISS)) then
      write (13,'(a5,1x,i4,1x,i2.2,1x,4f10.1)') cWMO,iYear,iMonth,rMax,rMean,rMin,rPrcp
    endif
  endif
enddo
close(12)
close(13)

end program climat_decoder
