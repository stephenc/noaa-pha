C***********************************************************************
C                     Program ushcn_tobs_2000.f                      *
C
C DATE OF LAST CHANGE:  25 July 2000
C
C MODIFIER:  Claude Williams
C
C VERSION     MODIFICATION                                     DATE    *
C -------     ------------                                  ---------- *
c
c   4a         Expand format to GHCNM 11-char ID & 3-char Flag  10 Mar 2010
c              Corrected TOB adjustment for GHCN Celcius!!!   18 May 2010
c              Fixed TAD output index problem!!! 05may2011
c
c   4         Rearranged directory structure for Process level (raw, tob)
c               from the state directory structure          18 may 2009
c
c   3b        1) Removed TD3200 for Station History input
c               Use Only MSI/MSHR (src==2) for COOP/CDMP
c               and original SHF(src==0) for USHCN
c             2) Modified GETOBT to account for the HSI 
c               unknown obcodes:
c                  code  nrec  interpretation
c                   00   4630  Midnight obs (24)
c                   VA     19  Variable (VR)
c           (DE,MI,31,73,78,83,89) 25 total - unknown (99)
c                                                           03 Jul 2008
c
c   3a        Currently removed Alaska & Hawaii from TOB 
c             processing. TOB calibrations not been done,   05 Feb 2008
c
c   3         Slight metadata format change                 14 Nov 2006
c
c   2.1       Added the gmt offset from the SHF for a more
c               precise adjustment
c                                                           11 Sep 02
c             Fixed error in getobt - 9909 & 9919 decoded improperly
c                                                           12jul05
c
c   2.0a      For the TOB study brought on by the Balling
c             question, redesigned the CSTN variable to be
c             a character string                            02jul2002
c
c   2.0       The only diference between this version       06sep2001
c             and that used for the 1971-2000 Normals
c             is that the begin year is changed to 1895
c             and the station history input begins with
c             the USHCN SHF instead of Jim Owenby's TOBs
c
c   1.4       From TD3200 documentation - all records       07/25/2001
c             have obtime SET to 18hr. Must use MSHR
c             pre 1982
c
c   1.3       Bring in the metadata from the modified       04/10/2001
c             Master Station History Report from David B.
c             Use the Normals data file structure
c
C   1.2       Blank annual flags 1,2, and 4 on              10/24/1995 *
C             precipitation records                                    *
C                                                                      *
C   1.1       SUN Workstation Version                       01/06/1994 *
C                                                                      *
C   1.0       Original                                                 *
C                                                                      *
C AUTHOR:  Claude Williams                                             *
C                                                                      *
C MODIFIERS:  Pam Hughes                                               *
C             David Bowman                                             *
C                                                                      *
C USAGE:  hcn_tobs                                                     *
C                                                                      *
C DESCRIPTION:  This program adjusts the raw data based on the time of *
C               observation, and sets the record flag to either "F" if *
C               the data is "F"laky, or "G" if the data is "G"ood.     *
C                                                                      *
C REQUIREMENTS:                                                        *
C                                                                      *
C   Input Files:   hcn_station_history                                 *
C                  hcn_xs_data                                         *
C                                                                      *
C         **************************************************************
C NOTES:  * CHANGES TO ANY PARAMETERS MUST BE MADE TO THE SAME         *
C         * PARAMETERS IN ANY OF THE FUNCTIONS AND/OR SUBROUTINES.     *
C         **************************************************************
C                                                                      *
C         This program originally used an array of flags returned by   *
C         the subroutine GETOBT.  These flags denoted if the observer  *
C         at the time of an observation change was the "WEATHER        *
C         BUREAU".  If the observer was the "WEATHER BUREAU", this     *
C         program set the data quality flag to "G" (good) and made the *
C         observation time = 24.  This part of the logic was           *
C         incomplete in that some observers were listed as "USWB", or  *
C         some other variation.  Also, over periods of history when    *
C         the observation time does not change, there are times when   *
C         the observer changed from or to the "WEATHER BUREAU".        *
C         - D. Bowman:  03/08/94                                       *
C                                                                      *
C         Modified 06/27/91 to use "improved" COBOL subroutines GETMO2 *
C         and PUTMO2.  GETMO2 returns exactly what data is there;      *
C         i.e., no evaluation of the # of days missing.  However, at   *
C         this point, the ISAM has been through FINAL EDIT and values  *
C         with more than 10 days are already missing. - P. Hughes      *
C                                                                      *
C         Objective of program is to make adjustments for observation  *
C         time bias to long term historical climate network            *
C         temperature elements.  Program relies on various C.          *
C         Williams' subprograms to fulfill this objective.  The first  *
C         step in this process is to extract a given station's data    *
C         from the HCN (call GETSTN).  (Only monthly values with less  *
C         than 10 missing days are returned.)  Step 2 is to consult    *
C         the Station History File (call GETOBT) for changes affecting *
C         the input parameters of the observation time adjustment      *
C         routine (call OBTDIF).  From this consultation,              *
C         "appropriate" latitude and longitude are returned and an     *
C         array of the dates of changes in time of temperature         *
C         observations beginning with where max/min instrumentation is *
C         initially in use along with several flags.  Step 3 is to     *
C         calculate monthly means of the most recent (approximately)   *
C         40 years of data (e.g., all Januarys from 1950 to 1990, all  *
C         February's from 1950 to 1990, etc.) but with no less than 25 *
C         months used for a monthly mean held in LTIX.  LTIX data is   *
C         long term means of IX(YR,MO,3) where 3 = Average (1 =        *
C         Maximum, 2 = Minimum, and 4 = Precipitation).  These monthly *
C         means will be passed to the OBTDIF routine as the necessary  *
C         previous and subsequent monthly means solicited as AMT1 and  *
C         AMT2.  Step 4 is to make adjustments for observation time    *
C         bias to serve as the data base for future passes by programs *
C         developed to make other adjustments (e.g., station moves,    *
C         instrument height changes, etc.).  Step 5 is to put the      *
C         station (call PUTSTN) back into the HCN.                     *
C                                                                      *
C         Unit 11 holds the observation time codes directly from C.    *
C         Williams subroutine (i.e., HCN station history).             *
C         Unit 12 holds the "massaged" observation time codes with     *
C         flags used to actually adjust.                               *
C                                                                      *
C         Year ranges from 1985 onward have been added for updating    *
C         the DOE HCN data set.  The variables IBYEAR, IEYEAR, and     *
C         ILYEAR define the beginning and end of the period to be      *
C         updated and the beginning of the period to use as the        *
C         monthly means, respectively.  The periods as of this update  *
C         are 1950 - 1990 for calculating the monthly means and 1985 - *
C         1987 for correcting the data for observation time.           *
C         - C. Williams:  12/05/1988                                   *
C                                                                      *
C         CHARACTER*1 DLTH,ESTOB(40),PREVP,SKIP,IXF(1940:1990,12)      *
C         CHARACTER*4 FLAG(1940:1990,12,4)                             *
C         CHARACTER*8 CSTN                                        *
C         CHARACTER*28 CNAME(300)                                      *
C         CHARACTER*30 COMPID(1940:1990)                               *
C         INTEGER ADJ(1940:1990,12,4),BDAYH(40),BEGDAY,BMOD(300)       *
C         INTEGER BMOH(4),BYRD(300),BYRH(40),IBYCTL(300),IEYCTL(300),P *
C         INTEGER IX(1940:1990,12,4)                                   *
C         EXTERNAL GETSTN(ACOB),GNAME(ACOB),GETOBT(ACOB),PUTSTN(ACOB)  *
C                                                                      *
C         A combination of the 1984 and 1987 methods:  In 1987, C.     *
C         Williams limited the input period of record including        *
C         subroutines for 51 years (1940 - 1990) and apparently did    *
C         not use the portion of original code which calculated a long *
C         term average based on the "current" 25 - 30 months because   *
C         of the limits on the incoming data.                          *
C                                                                      *
C         C. Williams introduced variables to control the begin and    *
C         end years (e.g., IBYCTL, IEYCTL, IBYDEF, IEYDEF, IFYEAR, and *
C         ILYEAR) by station.  For 1990, I changed the original        *
C         hard coded begin (1785) and end (1984) years to parameter    *
C         variables BEGYR and ENDYR; however not to the extent of      *
C         flexibility C. Williams allowed.  I brought back the         *
C         original code to determine the most current 25 - 30 months   *
C         to compute the long term averages.  The COBOL subroutines    *
C         differ from the original 1984, in that while the arrays      *
C         passed must be defined with dimensions 216 x 13 x 4          *
C         (1785:2000), portions can be transmitted using begin and end *
C         years as arguments.                                          *
C                                                                      *
C         Due to processing changes from regions to all 1219 stations  *
C         at a pass, I upped the array indices from 500 as originally  *
C         used in 1984 (300 was used in 1987) to 1500.  Also C.        *
C         Williams' PUTSTN accepted two flag arrays:  FLAG and IXF.    *
C         IXF then overwrote the third character position of the       *
C         temperature element flags in FLAG.  I eliminated the         *
C         CHARACTER*1 FLAG array (e.g., IXF) and renamed "FLAG" as     *
C         "IXF" and used substringing to initial and assign this flag. *
C         This program uses the monthly values, never the annuals.     *
C         Corrections included:                                        *
C         1) declaring "COMPID" as 32 characters rather than 30;       *
C         2) upping CNAME array to 1500 rather than original 20        *
C         3) fixed leap year logic so as to exclude 1800 as a leap     *
C            year (i.e., of the century years 1600, 1700, 1800, 1900,  *
C            and 2000, only 1600 and 2000 are actually leap years).    *
C         - P. Hughes:  06/27/91                                       *
C                                                                      *
C         Never noticed only the first 20 station's names ever         *
C         printed. - P. Hughes                                         *
C                                                                      *
C REFERENCES:  Sun FORTRAN Reference Guide, Rev. A, 22 Feb 1991        *
C                                                                      *
C              Sun FORTRAN User's Guide, Rev. A, 22 Feb 1991           *
C                                                                      *
C              Understanding FORTRAN - Michel Boillet - 1978           *
C              ISBN 0-8299-0355-0                                      *
C                                                                      *
C RESULTS:  This program adjusts the raw data based on the time of     *
C           observation, sets the record flag to either "F" if the     *
C           data is "F"laky or "G" if the data is "G"ood, and writes   *
C           the adjusted data and flags to a file.                     *
C                                                                      *
C   Output File:  hcn_tobs_data                                        *
C                                                                      *
C PARAMETERS:                                                          *
C                                                                      *
C   ibegyr     First Year of Data to Process                           *
C                                                                      *
C   ilstyr     Last Year of Data Arrays Can Store                      *
C                                                                      *
C   imxchg     Maximum Number of Observation Time Changes Over a       *
C              Station's Period of Record                              *
C                                                                      *
C   MAXNETS     Number of Stations Arrays Can Store                     *
C                                                                      *
C SUBROUTINES:  GETDAT                                                 *
C               GETOBT                                                 *
C               GETREC                                                 *
C               MSGDAT                                                 *
C               OBTDIF                                                 *
C               TOBCHG                                                 *
C                                                                      *
C VARIABLES:                                                           *
C                                                                      *
C   A          Time of Observation Bias Adjustments for 12 Months, 24  *
C   (12,24,3)  Hours Each Day, 3 Elements                              *
C                                                                      *
C   AAV        Adjusted Mean Value from "A" for a Specific Month and   *
C   (31)       Observation Time                                        *
C                                                                      *
C   ADJ        Adjusted Values for Each Monthly Value                  *
C   (ibegyr:ilstyr,12,4)  Years ibegyr - ilstyr, 12 Values, 4 Elements *
C                                                                      *
C   ADJAV      Sum of Adjusted Values for a Monthly Mean               *
C                                                                      *
C   ADJMN      Sum of Adjusted Values for a Monthly Minimum            *
C                                                                      *
C   ADJMX      Sum of Adjusted Values for a Monthly Maximum            *
C                                                                      *
C   AIDIF      Inter-Daily Temperature Difference (See Article.)       *
C                                            -1 => Calculate the Value *
C                                                                      *
C   ALATZ      Latitude of Station                                     *
C                                                                      *
C   ALONZ      Longitude of Station                                    *
C                                                                      *
C   AMN        Adjusted Minimum Value from A for a Specific Month and  *
C   (31)       Observation Time                                        *
C                                                                      *
C   AMX        Adjusted Maximum Value from A for a Specific Month and  *
C   (31)       Observation Time                                        *
C                                                                      *
C   AOBAV      Estimate of Mean Temperature Observation Time Bias in   *
C   (24)       Degrees for Each Hour                                   *
C                                                                      *
C   AOBMN      Estimate of Minimum Temperature Observation Time Bias   *
C   (24)       in Degrees for Each Hour                                *
C                                                                      *
C   AOBMX      Estimate of Maximum Temperature Observation Time Bias   *
C   (24)       in Degrees for Each Hour                                *
C                                                                      *
C   BDAYH      Begin Days Where Time of Observation Changes            *
C   (imxchg)                                                           *
C                                                                      *
C   BEGDAY     Begin Day of History Used in Adjusting Data             *
C                                                                      *
C   BMOD       Month of Earliest Max/Min Data for Each Station         *
C                                                              *
C                                                                      *
C   BMOH       Begin Months Where Time of Observation Changes          *
C   (imxchg)                                                           *
C                                                                      *
C   BY         Year Counter for Finding LTIX Values                    *
C                                                                      *
C   BYRD       Year of Earliest Max/Min Data for Each Station          *
C                                                              *
C                                                                      *
C   BYRH       Begin Years Where Time of Observation Changes           *
C   (imxchg)                                                           *
C                                                                      *
C   CNAME      Name of HCN Station                                     *
C                                                              *
C                                                                      *
C   CSTN       ID of HCN Station                                       *
C                                                              *
C                                                                      *
C   DAYS       Same as DAYSMO Except Allows for February to Have 29    *
C              Days for Leap Years                                     *
C                                                                      *
C   DAYSMO     Number of Days in Each Month                            *
C   (12)                                                               *
C                                                                      *
C   DEBUG      Debug Variable:       0 => Do Not Debug                 *
C                                    1 => Debug; Print Out Information *
C                                                                      *
C   DFILE      Name of Data File to Access/Index                       *
C                                                                      *
C   DLTH       History/Data Flag:                                      *
C                                 "N" => History Does Not Precede Data *
C                                 "Y" => History Precedes Data         *
C                                                                      *
C   ENDDAY     End Day of History Used in Adjusting Data               *
C                                                                      *
C   ENDM       End Month of History Used in Adjusting Data             *
C                                                                      *
C   ENDY       End Year of History Used in Adjusting Data              *
C                                                                      *
C   ENDYR      End Year of Data/Flag Search                            *
C                                                                      *
C   ESTOB      Data Quality Flag:                 "F" => Data is Flaky *
C   (imxchg)                                      "G" => Data is Good  *
C                                                                      *
C   FILDIR     Directory Where Input and Output Files are Located      *
C                                                                      *
C   ICT        Count of Monthly Nonmissing Data Values for a 30 Year   *
C   (12)       Period                                                  *
C                                                                      *
C   IDFLG      Drift Algorithm Flag:  0 => Estimate the Drift Using    *
C                                          Mean Monthly Temperatures   *
C                                     1 => Calculate Exact Drift Using *
C                                          Differences Between First   *
C                                          and Last Days               *
C                                                                      *
C   IFLAG      Read Error Flag:        0 => No Error                   *
C                                      1 => Error in Getting Data/Flag *
C                                                                      *
C   IMONTH     Used to Find Earlier Month of Maximum/Minimum Data      *
C                                                                      *
C   ITUNIT     Temperature Units:                      0 => Centigrade *
C                                                      1 => Fahrenheit *
C                                                                      *
C   ITZ        Time Zone of Station:                     1 => Eastern  *
C                                                        2 => Central  *
C                                                        3 => Mountain *
C                                                        4 => Pacific  *
C                                                                      *
C   IS         Loop Counter                                            *
C                                                                      *
C   IX         Raw Monthly and Annual Data                             *
C   (ibegyr-1:ilstyr,13,4)  Years ibegyr-1 to ilstyr, 13 Values, 4     *
C                           Elements                                   *
C                                                                      *
C   IXF        Raw Monthly and Annual Data Flags                       *
C   (ibegyr-1:ilstyr,13,4)  Years ibegyr-1 to ilstyr, 13 Values, 4     *
C                           Elements                                   *
C                                                                      *
C   IYEAR      Used to Find Earlier Year of Maximum/Minimum Data       *
C                                                                      *
C   J          Loop Counter                                            *
C                                                                      *
C   K          Loop Counter                                            *
C                                                                      *
C   L          Loop Counter                                            *
C                                                                      *
C   LL         Loop Counter                                            *
C                                                                      *
C   LTIX       Long Term Average Monthly Data Values Over the Most     *
C   (0:13)     Recent 30 Year Span (of Which No More than 5 Values Are *
C              Missing), or Possibly For the Entire Period of Record   *
C                                0      Holds December Value           *
C                                1 - 12 Holds January - December Value *
C                               13      Holds January Value            *
C                                                                      *
C   M          Loop Counter; Holds Start Month for Adjusting Data      *
C                                                                      *
C   MM         Loop Counter                                            *
C                                                                      *
C   N          Loop Counter                                            *
C                                                                      *
C   NA         Number of Days to Make Adjustments for During a         *
C   (31)       Specific Month                                          *
C                                                                      *
C   NCHGS      Number of Times Observation Time Changes Over the       *
C              Station's Period of Record                              *
C                                                                      *
C   NP         Number of Adjustments to Make for a Specific Month      *
C                                                                      *
C   NSTNR      Total Number of HCN Stations                            *
C                                                                      *
C   P          Loop Counter                                            *
C                                                                      *
C   PREVP      Data/History Flag:                                      *
C                                 "N" => Data Does Not Precede History *
C                                 "Y" => Data Precedes History         *
C                                                                      *
C   RFILE      Array of Station IDs and Record Begin Numbers of Each   *
C   (48,75,2)  HCN Station for Accessing a Data File as Direct:        *
C              48 States, 75 Stations,        1 => Station Id          *
C                                             2 => Record Begin Number *
C                                                                      *
C   SAVMO      Holds Month When Adjusting Data so Know When Doing Next *
C              Month                                                   *
C                                                                      *
C   SAVTOB     Holds Observation Time When Adjusting Data              *
C                                                                      *
C   SAVYR      Holds Year When Adjusting Data so Know When Doing Next  *
C              Year                                                    *
C                                                                      *
C   SKIP       Flag to Indicate Whether Missing Date was Filled In:    *
C                                    "N" => Missing Date Not Filled In *
C                                    "Y" => Missing Date Filled In     *
C                                                                      *
C   STARTM     Begin Month of History Used When Adjusting the Data     *
C                                                                      *
C   STARTY     Begin Year of History Used When Adjusting the Data      *
C                                                                      *
C   SUMNA      Total Number of Days in Month for Which to Adjust Data  *
C                                                                      *
C   TOB        Observation Time Used in Adjusting Data                 *
C                                                                      *
C   TOBTMP     Code for Time of Observation                            *
C   (0:imxchg)                                   01 - 24 => 01 - 24    *
C                                                     25 => "xxHR"     *
C                                                     26 => "RS"       *
C                                                     27 => "SR"       *
C                                                     28 => "SS"       *
C                                                     29 => All Others *
C                                                     30 => "TRID"     *
C                                                     99 => Unknown    *
C                                                                      *
C   Y          Loop Counter; Holds Start Year for Adjusting Data       *
C                                                                      *
C***********************************************************************

      PROGRAM NRMTOB

      include 'prehomog.parm.incl'

      PARAMETER (imxchg = 200, minyear = 20)

      COMMON/MDBLK1/BDAYH,BMOH,BYRH
      INTEGER BDAYH(imxchg),BMOH(imxchg),BYRH(imxchg)
      INTEGER DAYSMO(12) /31,28,31,30,31,30,31,31,30,31,30,31/

      CHARACTER*1 DLTH,PREVP,SKIP
      CHARACTER*1 ESTOB(imxchg)
      CHARACTER*132 ARGV, metafile, odfile, otfile
      CHARACTER*3 IXF(ibegyr-1:ilstyr,13,4)
c     Originally - from TYPHOON ...
c      CHARACTER*25 FILDIR /'/sd4/dbowman/ushcn/files/'/
      CHARACTER*44 CNAME
      CHARACTER*132 DFILE /''/, dbgfile /''/
      DIMENSION ICT(12),NA(31)
      DIMENSION AAV(31),AMN(31),AMX(31),AOBAV(24),AOBMN(24),AOBMX(24)
      dimension aoblast(12)
      DIMENSION ADJ(ibegyr:ilstyr,12,4)
      DIMENSION A(12,24,3)
      INTEGER BEGDAY,BY,DAYS,DEBUG,ENDDAY,ENDM,ENDY,ENDYR,P
      INTEGER SAVMO,SAVTOB,SAVYR,STARTM,STARTY,SUMNA,GMToff
      INTEGER TOB,Y,normrun/0/
      INTEGER BMOD,BYRD,TOBTMP(0:imxchg)
      REAL LTIX(0:13)
      REAL IX(ibegyr-1:ilstyr,13,3)
      CHARACTER*11 CSTN
      character*3 netype
      character*2 country
      character*3 ctype, otype, atype
      character*128 cmd

c     metatypes is just the input type 
      character*7 metatype(3)/'Normals','USHCN02','NWS4803'/
      call subpon(" ============ NRMTOB entry", 438, 0)
C     INITIALIZE VARIABLES
      print *,' Generate TOB corrections for USHCN v2'

      debug = 0

c     first parameter is required and decoded as follows:
c       four digit year (>= ibegyr+minyear)

c     BEGIN command line code      
c     get the number of command line arguments
      narg = iargc()
      if(narg .lt. 1) then
        print *,' Not all required parameters given'
        goto 10
      endif
      
      iarg = 1
      
      atype = ''
      otype = ''
      ctype = ''
      ingmt = 1

c     get last year of data to process
      call getarg(iarg,argv)
      iarg = iarg + 1
      read(argv,fmt='(i4)') endyr
      if(endyr .lt. ibegyr + minyear) then
        print *, 'COMMAND LINE INPUT ENDYR:', endyr, 
     *    ' TOO LOW, MUST BE AT LEAST:', ibegyr+minyear
        stop
      endif
      print *, ' Highest year to process: ', endyr

c     go thru remaining command line arguments - keep the valid ones
c        and ignoring the undefined ones.
      do while(iarg .le. narg)
        call getarg(iarg, argv)
        iarg = iarg + 1
        if(argv .eq. '-m') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          metafile = argv
          print *, ' (-m) Stations Metadata Input file :', 
     *      metafile(1:lnblnk(metafile))
        else if(argv .eq. '-D') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          dfile = argv
          print *, ' (-D) Base Normals Monthly Data directory:', 
     *      dfile(1:lnblnk(dfile))
        else if(argv .eq. '-p') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          ctype = argv
          print *, ' (-p) Processed Stage for Input files:', ctype
        else if(argv .eq. '-o') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          otype = argv
          print *, ' (-o) Processed Stage for TOB ouput files:', otype
        else if(argv .eq. '-a') then
          call getarg(iarg, argv)
          iarg = iarg + 1
          atype = argv
          print *, ' (-a) Processed Stage for TOB correction ouput :', 
     *      atype
        else if(argv .eq. '-n') then
          normrun = 1
          print *, ' (-n) Normals Run - no HCN SHF used'  
        else if(argv .eq. '-x') then
          ingmt = 0
          print *, ' (-x) No GMT offset input - estimate'  
        else if(argv .eq. '-d') then
          call getarg(iarg,argv)
          iarg = iarg + 1
          read(argv,fmt='(i1)') debug
          print *, ' (-d) Debug Level:', debug
        else
          print *, ' Unknown argument :', argv, ': skipping'
        endif
      enddo
      
      if(metafile.ne.'' .and. dfile .ne. '' .and.
     *   atype .ne. '' .and. otype.ne.'' .and. ctype.ne.'') goto 11
      
   10 print *,' Time of Observation Bias generation routine'
      print *,
     *  ' USAGE: normals_tobs_2000 -m metafile -h distfile -o outfile'
      print *,'   REQUIRED PARAMETERS'
      print *,'     Last year to process'
      print *,'    I/O process types ---'
      print *,'     -p ctype    Input process level'
      print *,'     -o otype    TOB Output process level'
      print *,'     -a atype    Adustment Output process level'
      print *,'    Input Files ---'
      print *,'     -m metafile   Station Metadata file'      
      print *,'     -D data_dir   Data directory for i/o.'
      print *,'     -n  if present then NORMALS RUN - no HCN SHF used'
      print *,'     -x  if present then estimate the GMT offset'
      print *,''
      print *,' NOTE: Data files with .tob extension are parsed into '
      print *,'        sudirectories by state (01,02...,50)'
      stop

   11 continue

      if(normrun .eq. 0) print *,' HCN Run - Use HCN SHF data'
      if(ingmt .eq. 1) print *,' GMT offset is input in metafile'
      
      print *,''
      print *,'---Parameters from prehomog.parm.incl---'
      print *,'Begin year for the USHCN series: ', ibegyr
      print *,'End year for the USHCN series: ', ilstyr
      print *,'Number of stations in nearest neighbor file: ', ndist
      print *,'Number of network stations: ', nstns
      print *,'Total Number of years: ', numyr
      print *,''

c     read station file list
      print *,' Opening: ', metafile
      OPEN(12,FILE=metafile,STATUS='old',ERR=820)

C     OPEN DEBUG FILE

      iflen = lnblnk(dfile)
      if(dfile(iflen:iflen) .ne. '/') then
        iflen = iflen + 1
        dfile(iflen:iflen) = '/'
      endif 

      DO while(1.eq.1)
C       GET STATION ID, lat/lon, GMT, and name
c        print *,' Reading: ', metafile
        READ(12,2000,ERR=830,end=785) cstn,alatz,alonz,cname,gmtoff
 2000   format(a11,f9.4,f10.4,8x,a35,47x,i3)
c        print *,cstn,alatz,alonz,gmtoff,cname
c 2000   format(a6,2f8.2,i4,a44)

        if(debug .gt. 0) 
     *    print *,' -------- Processing ', cstn,alatz,alonz,gmtoff

C       GET ALL Elements and station history FOR THE STATION FROM 
C       "ibegyr" TO "ilstyr" FOR ALL MONTHS AND ANNUALS

C       INITIALIZE READ ERROR FLAG
        IFLAG = 0

C       GET DATA AND FLAGS
   30   CALL GETDAT(IX,dfile,IXF,IFLAG,cstn,ctype,debug)
   
C       PRINT OUT MESSAGE IF NO DATA FOUND, AND SKIP TO NEXT STATION
        IF(IFLAG .EQ. 1) THEN
          WRITE(6,40) CSTN
   40     FORMAT('NO DATA FOR STATION ',a)
          GO TO 780
        END IF

C       INITIALIZE ADJ ARRAY AND DATA FLAG FOR TEMPERATURE
        DO 60 J = ibegyr,ENDYR
          DO 60 L = 1,3
            DO 50 K = 1,12
              ADJ(J,K,L) = amiss
   50       CONTINUE

C           SET ANNUAL VALUE TO MISSING AND ANNUAL FLAGS TO BLANKS FOR
C           TEMPERATURE
            IX(J,13,L) = amiss
   60   CONTINUE

C       INITIALIZE VARIABLES
        LTIX(0) = 0.
        LTIX(13) = 0.
        NCHGS = 0

        DO 80 K = 1,24
          DO 70 L = 1,3
            DO 70 J = 1,12
              A(J,K,L) = 0.
   70     CONTINUE

          AOBAV(K) = 0.
          AOBMN(K) = 0.
          AOBMX(K) = 0.
   80   CONTINUE

C       INITIALIZE TIME OF OBSERVATION TO Sunset
        TOBTMP(0) = 28

        DO 90 I = 1,imxchg
          BDAYH(I) = 0
          BMOH(I) = 0
          BYRH(I) = 0
          ESTOB(I) = ' '
          TOBTMP(I) = 99
   90   CONTINUE

c       BEWARE: Assumes US?00ssnnnn station history file name format!!!
c       skip all of the TOB computation for anybody not in CONUS (01-48)
c         nor in a WBAN (80-89) USW00023400 USC00300023
        read(cstn,'(a3)') netype
        if(netype .ne. 'USC' .and. netype .ne. 'USW'
     *     .and. netype .ne. 'USH') then
          print *,' Not USC/USW/USH - skipping: ', cstn
          nchgs = 1
          tobtmp(nchgs) = 24
          goto 680
        endif
        read(cstn,'(a2,3x,i2)') country, istate
        read(cstn,'(a3,3x,i5)') netype, iwstn
        if((netype .eq. 'USC' .and. istate.gt. 48) .or. 
     *     (netype .eq. 'USW' .and.
     *     (alatz .gt. 49.1 .or. alatz .lt. 24.5 .or. 
     *     alonz .gt. -66.8 .or. alonz .lt. -124.8))) then
          print *,' US Not CONUS - skipping: ', cstn
          nchgs = 1
          tobtmp(nchgs) = 24
          goto 680
        endif  

        IF(DEBUG .ge. 2) THEN
          write(6,*) ' Monthly input data'
          do i = 1, 2
            write(6,*) ' Elem: ', celem(i)
            DO J = ibegyr, endyr
              WRITE(6,'(i4,1x,12f7.2)') J,(IX(J,M,3),M=1,12)
            enddo
          enddo    
        END IF

C       GET LATITUDE, LONGITUDE, AND NUMBER OF CHANGES WITH BEGIN DATES
C       AND TIME OF OBSERVATION CODES
        CALL GETOBT(BDAYH,BMOH,BYRH,dfile,cstn,NCHGS,TOBTMP,NORMRUN,
     *    debug)

C       GO TO PRINT OUT INFORMATION IF NO TIME OF OBSERVATION CHANGES
        IF(NCHGS .EQ. 0) THEN

C         DEBUG PRINTOUT
          IF(DEBUG .ge. 1) WRITE(6,110) CSTN
  110                      FORMAT('STATION ',a,' NEVER HAD MAX-MIN ',
     *                            'INSTRUMENTATION.')
          GO TO 680
        END IF

        itz = 0
C       DETERMINE TIME ZONE:
        IF(ALONZ .LE. -115.000001) THEN
C         PACIFIC TIME ZONE        
          ITZ = 4
        ELSE IF(ALONZ .LE. -103.000001) THEN
C         MOUNTAIN TIME ZONE
          ITZ = 3
        ELSE IF(ALONZ .LE. -86.000001) THEN
C         CENTRAL TIME ZONE
          ITZ = 2
        ELSE
C         EASTERN TIME ZONE
          ITZ = 1
        END IF
        
c       if the GMT offset is not given in the SHF then approximate
        if(GMToff .ne. 0 .and. GMToff .ne. 99 .and.
     *    GMToff .ne. -99 .and. ingmt .eq. 1) then
          if(gmtoff-4 .ne. itz) then
            print *,' Station GMT time zone: ', gmtoff-4, 
     *        ' different than Lon derived: ', itz
          endif
          itz = gmtoff - 4
        else
          if(debug .gt. 0) 
     *      print *,' Station time zone is Lon derived: ', itz  
        endif    
        if(itz .lt. 1 .or. itz .gt. 4) then
          print *,' Station time zone outside of Cont US - skip'
          goto 780
        endif  

C       DEBUG PRINTOUT
        IF(DEBUG .ge. 1) WRITE(6,120) CSTN,CNAME,ALATZ,
     *                               ALONZ,ITZ
  120                    FORMAT(a,1X,A44,1X,2(F7.2,1X),I1)

C       DETERMINE EARLIER BEGIN DATE BETWEEN MAXIMUM AND MINIMUM DATA
C       INITIALIZE VARIABLES
        BMOD = 12
        BYRD = ENDYR

        DO 150 I = 1,2
          DO 130 Y = ibegyr,ENDYR
            DO 130 M = 1,12
C             GET MONTH AND YEAR OF FIRST NONMISSING DATA VALUE
              IF(IX(Y,M,I) .NE. amiss) THEN
                IMONTH = M
                IYEAR = Y
                GO TO 140
              END IF
  130     CONTINUE

C         DETERMINE EARLIER BEGIN DATE
  140     IF(IYEAR .LT. BYRD .OR. (IYEAR .EQ. BYRD .AND.
     *       IMONTH .LT. BMOD)) THEN
            BMOD = IMONTH
            BYRD = IYEAR
          END IF

  150   CONTINUE

C       CALCULATE MONTHLY MEANS USING MOST RECENT 30 YEAR PERIOD IN WHICH
C       NO MORE THAN 20 YEARS HAVE MISSING MONTHLY VALUES
C       INITIALIZE VARIABLE

        BY = ENDYR + 1
  160   BY = BY - 1
  
        mict = 0

C       NEED AT LEAST 10 "CURRENT" YEARS FOR EACH MONTH
        DO 240 M = 1,12

C         INITIALIZE MONTHLY COUNT AND SUM
          ICT(M) = 0
          LTIX(M) = 0.

c          Expanded to entire record 15 Aug 2001
c          DO 230 Y = BY,BY-29,-1
          DO 230 Y = BY,ibegyr,-1
            if(ix(y,m,3) .eq. amiss .and.
     *        (ix(y,m,1) .ne. amiss .and. ix(y,m,2) .ne. amiss)) then
              ix(y,m,3) = (ix(y,m,1) + ix(y,m,2)) / 2.0
            endif  
C           COUNT AND SUM NONMISSING MONTHLY VALUES
            IF(IX(Y,M,3) .NE. amiss) THEN
              ICT(M) = ICT(M) + 1
              LTIX(M) = LTIX(M) + IX(Y,M,3)
            END IF

  230     CONTINUE

          if(ict(m) .gt. mict) mict = ict(m)  
  240   CONTINUE

C         IF NOT AT LEAST 5 YEARS of data - abort!
          IF(MICT .LT. 5) then
            print *,' Warning station ', cstn, 
     *       ' - has only ', mict, ' years'
            write(6,*) ' Warning station ', cstn, 
     *       ' - has only ', mict, ' years'
            go to 780
          endif  

C       DEBUG PRINTOUT
  250   IF(DEBUG .ge. 2) THEN
          WRITE(6,260) endyr-30
  260     FORMAT('30 YEARS OF AVERAGE TEMPERATURE USED FOR LONG TERM ',
     *           'MONTHLY MEANS STARTING WITH YEAR ',I4)

          DO 270 J = endyr-30,endyr
            WRITE(6,210) J,(IX(J,M,3),M=1,12)
  210       format(i4,1x,12f7.2)
  270     CONTINUE
        END IF

C     CALCULATE LONG TERM AVERAGE

        DO 280 M = 1,12
          LTIX(M) = LTIX(M) / FLOAT(ICT(M))
  280   CONTINUE

C       SET "FINAL VALUE" OF HISTORY BEGIN DATE ARRAY TO DECEMBER OF LAST
C       YEAR:  DAY MUST BE ONE DAY PAST END (TO USE JANUARY 01 IS MORE
C       TROUBLE)
        BDAYH(NCHGS+1) = 32
        BMOH(NCHGS+1) = 12
        BYRH(NCHGS+1) = ENDYR

C       DEBUG PRINTOUT
        IF(DEBUG .ge. 1) WRITE(6,290)
  290                    FORMAT('   Stn NCHG #CHG TOB DY MTH YR')

        DO 330 J = 1,NCHGS
C         INITIALIZE VARIABLE
          SKIP = 'N'

C         THIS CODE IS TO GIVE THE STATUS OF DATA
C         FILL IN DATE IF ANY PART OF BEGIN DATE IS MISSING
          IF(BYRH(J) .EQ. 9999 .OR. BMOH(J) .EQ. 99 .OR.
     *       BDAYH(J) .EQ. 99) CALL MSGDAT(CSTN,DEBUG,ibegyr,J,SKIP)

C         SKIP TO NEXT STATION IF NOT ABLE TO FILL IN MISSING PART(S) OF
C         BEGIN DATE
          IF(SKIP .EQ. 'Y') GO TO 780

C         SET BEGIN DATE TO JANUARY 01 OF "ibegyr" IF HISTORY BEGIN YEAR IS
C         BEFORE "ibegyr"
          IF(BYRH(J) .LT. ibegyr) THEN

C           DEBUG PRINTOUT
            IF(DEBUG .ge. 1) WRITE(6,300) CSTN,ibegyr,BMOH(J),
     *                                    BDAYH(J),BYRH(J)
  300                        FORMAT('HISTORY OF STATION ',a,
     *                              ' BEGINS BEFORE ',I4,'.  BEGIN ',
     *                              'DATE IS ',2(I2.2,1X),I4)
            BMOH(J) = 1
            BDAYH(J) = 1
            BYRH(J) = ibegyr
          END IF

C         SET BEGIN DATE TO 32 DECEMBER OF ENDYR IF HISTORY END YEAR IS
C         AFTER ENDYR
          IF(BYRH(J) .GT. ENDYR) THEN

C           DEBUG PRINTOUT
            IF(DEBUG .ge. 1) WRITE(6,310) CSTN,J,BMOH(J),BDAYH(J),
     *                                    BYRH(J)
  310                        FORMAT('STATION ',a,' AT NCHG = ',I2,
     *                              ' HISTORY SUCCEEDS DATA.  BEGIN ',
     *                              'DATE IS ',2(I2.2,1X),I4)
            BMOH(J) = 12
            BDAYH(J) = 32
            BYRH(J) = ENDYR
          END IF

C         DEBUG PRINTOUT
          IF(DEBUG .ge. 1) WRITE(6,320) CSTN,NCHGS,J,TOBTMP(J),
     *                                  BMOH(J),BDAYH(J),BYRH(J)
  320                      FORMAT(a,1X,3(I2,1X),1X,2(I2.2,1X),
     *                            I4)
  330   CONTINUE

C       DEBUG PRINTOUT
        IF(DEBUG .ge. 1) WRITE(6,340) CSTN,ALATZ,ALONZ,BMOH(1),
     *                                BDAYH(1),BYRH(1),BMOD,
     *                                BYRD,ITZ
  340                    FORMAT(a,1X,2(F7.2,1X),'HIST BEGAN',
     *                          ' = ',2(I2.2,1X),I4,'  DATA BEGAN',
     *                          ' = ',I2.2,1X,I4,' TIME ZONE = ',I1)

C       DEBUG PRINTOUT
        IF(DEBUG .ge. 2) WRITE(6,350) (LTIX(M),ICT(M),M=1,12)
  350                    FORMAT('LONG TERM MONTHLY MEANS = ',
     *                          12(F6.2,1X,I2,1X))

C       PLACE DECEMBER'S LONG TERM AVERAGE IN CELL 0 AND JANUARY'S LONG
C       TERM AVERAGE IN CELL 13
        LTIX(0) = LTIX(12)
        LTIX(13) = LTIX(1)

C       CALCULATE ESTIMATES OF TIME OF OBSERVATION BIAS ADJUSTMENTS
C     J INCREMENTS LTIX:  NEVER CHANGE 12 TO 13
        DO 370 J = 1,12
C         INITIALIZE VARIABLES PASSED TO SUBROUTINE OBTDIF
          AIDIF = -1.
          IDFLG = 0
c         USHCNv2 in the GHCN format is CELCIUS - changed 1 to 0
c                                              18 May 2010 cw
          ITUNIT = 0

C         CALL SUBROUTINE OBTDIF
          CALL OBTDIF(AIDIF,ALATZ,ALONZ,AOBAV,AOBMN,AOBMX,IDFLG,ITUNIT,
     *                ITZ,J,LTIX(J-1),LTIX(J+1),debug)

C         STORE TIME OF OBSERVATION BIAS ADJUSTMENTS
          DO 360 L = 1,24
            A(J,L,1) = AOBMX(L)
            A(J,L,2) = AOBMN(L)
            A(J,L,3) = AOBAV(L)
  360     CONTINUE
  370   CONTINUE

C       DEBUG PRINTOUT
        IF(DEBUG .ge. 2) THEN
          WRITE(6,380)
  380     FORMAT('ESTIMATES OF OBSERVATION TIME BIAS RETURNED FROM ',
     *           'OBTDIF.',/,' MONTHS: JAN       FEB       MAR       ',
     *           'APR       MAY       JUN       JUL       AUG       ',
     *           'SEP       OCT       NOV       DEC')

          DO 400 L = 1,24
            WRITE(6,390) L,((A(J,L,I),J=1,12),I=1,3)
  390       FORMAT(I3,3(12F10.4,/,3X))
  400     CONTINUE
        END IF

C       PRINT ALL APPLICABLE MESSAGES
        DO 470 N = 1,NCHGS

C         OBSERVATION TIME IS OTHER THAN 01 - 24
          IF(TOBTMP(N) .GE. 25) THEN
C           OBSERVATION TIME IS "TRID"
            IF(TOBTMP(N) .EQ. 30) THEN
C             SET OBSERVATION TIME TO PREVIOUS OBSERVATION TIME
              TOBTMP(N) = TOBTMP(N-1)
C             SET FLAG TO "F"LAKY
              ESTOB(N) = 'F'

C             DEBUG PRINTOUT
              IF(DEBUG .ge. 1) WRITE(6,410) BMOH(N),BDAYH(N),BYRH(N),
     *                                      BMOH(N+1),BDAYH(N+1),
     *                                      BYRH(N+1)
  410                          FORMAT('TRID WITH MAX/MIN INSTRUMENTS ',
     *                                'FROM ',I2.2,I2.2,I4,' THRU ',
     *                                I2.2,I2.2,I4)

C             SET ALL PREVIOUS FLAGS TO "F"LAKY
              DO 430 L = 1,N-1
                ESTOB(L) = 'F'

C               DEBUG PRINTOUT
                IF(DEBUG .ge. 1) WRITE(6,420)
  420                            FORMAT('ENCOUNTERED TRID:  ANY PRIOR ',
     *                                  'OBSERVATION/ADJUSTMENT ',
     *                                  'CONSIDERED SUSPECT.')
  430         CONTINUE

C             OBSERVATION TIME IS "XXHR"
            ELSE IF(TOBTMP(N) .EQ. 25) THEN
C             SET OBSERVATION TIME TO 24 AND FLAG TO "F"LAKY
              TOBTMP(N) = 24
              ESTOB(N) = 'F'
C             DEBUG PRINTOUT
              IF(DEBUG .ge. 1) WRITE(6,440) BMOH(N),BDAYH(N),BYRH(N),
     *                                      BMOH(N+1),BDAYH(N+1),
     *                                      BYRH(N+1)
  440                          FORMAT('SEE 530:  MULTIPLE HOURS FROM ',
     *                                I2.2,I2.2,I4,' THRU ',I2.2,I2.2,
     *                                I4)

            ELSE IF(TOBTMP(N) .EQ. 99 .OR. TOBTMP(N) .EQ. 29) THEN
C           OBSERVATION TIME IS UNKNOWN OR OTHER
C             DEBUG PRINTOUT
              IF(DEBUG .ge. 1 .AND. TOBTMP(N) .EQ. 29)
     *           WRITE(6,450) TOBTMP(N-1),BMOH(N),BDAYH(N),BYRH(N),
     *                        BMOH(N+1),BDAYH(N+1),BYRH(N+1)
  450            FORMAT('OBSERVATION TIME  = 29.  SEE 530:  TREATED AS',
     *                  ' OBSERVATION TIME MISSING.  ASSUMED PREVIOUS ',
     *                  'TOB (= ',I2.2,') STILL APPLICABLE FROM ',I2.2,
     *                  I2.2,I4,' THRU ', I2.2,I2.2,I4)

C             SET OBSERVATION TIME TO PREVIOUS OBSERVATION TIME AND FLAG TO
C             "F"LAKY
              TOBTMP(N) = TOBTMP(N-1)
              ESTOB(N) = 'F'
              IF(N .EQ. NCHGS) WRITE(6,460) CSTN,TOBTMP(N)
  460                          FORMAT('NO CURRENT OBSERVATION TIME FOR',
     *                                ' STATION ',a, 'Assuming ',i2)
            ELSE
C             OBSERVATION TIME IS "RS", "SR", OR "SS":  SET FLAG TO "G"OOD
              ESTOB(N) = 'G'
            END IF
          ELSE
C           OBSERVATION TIME IS 01 - 24:  SET FLAG TO "G"OOD
            ESTOB(N) = 'G'
          END IF
  470   CONTINUE

C       DEBUG PRINTOUT
        IF(DEBUG .ge. 1) THEN
          DO 490 J = 1,NCHGS
            WRITE(6,480) CSTN,NCHGS,J,TOBTMP(J),ESTOB(J),BMOH(J),
     *                   BDAYH(J),BYRH(J)
  480       FORMAT(a,3(I4,1X),A1,1X,2(I2.2,1X),I4)
  490     CONTINUE
        END IF

C       INITIALIZE VARIABLES
        DLTH = 'N'
        PREVP = 'N'
        SAVMO = 13
        SAVTOB = 99
        TOB = 99

C       DETERMINE IF DATA PRECEDES HISTORY
        IF(BYRD .LT. BYRH(1) .OR. (BYRD .EQ. BYRH(1) .AND.
     *     BMOD .LT. BMOH(1))) THEN

          write(6,*) cstn, ' TOB = sunset from ', byrd, ' to ', byrh(1)

C         SET FLAG TO INDICATE THAT DATA PRECEDES HISTORY
          PREVP = 'Y'

C         DEBUG PRINTOUT
          IF(DEBUG .ge. 1) WRITE(6,500)
  500                      FORMAT('DATA PRECEDES HISTORY.  ASSUME ',
     *                            'OBSERVATION TIME IS SUNSET.')

C         LOOP FROM BEGINNING OF DATA TO BEGINNING OF HISTORY
          DO 540 Y = BYRD,BYRH(1)
            DO 530 M = 1,12

C             SKIP MONTHS BEFORE DATA BEGINS
              IF(Y .EQ. BYRD .AND. M .LT. BMOD) GO TO 530

C             INITIALIZE OBSERVATION TIME TO SUNSET
              TOB = TOBTMP(0)

C             CONVERT SUNSET ("SS") TO CODE FROM 01 TO 24
              CALL TOBCHG(M,TOB)

C             SAVE MONTH AND EXIT LOOP WHEN AT BEGINNING OF STATION HISTORY
              IF(Y .EQ. BYRH(1) .AND. M .EQ. BMOH(1)) THEN
                SAVMO = M
                GO TO 550
              END IF

C             SAVE ADJUSTMENT AND ADJUST DATA IF NOT MISSING
              DO 520 I = 1,3
                ADJ(Y,M,I) = A(M,TOB,I)
                IF(IX(Y,M,I) .NE. amiss) IX(Y,M,I) =
     *             IX(Y,M,I) - ADJ(Y,M,I)
  520         CONTINUE
  530       CONTINUE
  540     CONTINUE

C       DATA DOES NOT PRECEDE HISTORY
        ELSE

C         SET FLAG IF HISTORY PRECEDES DATA
          IF(BYRD .GT. BYRH(1) .OR. (BYRD .EQ. BYRH(1) .AND.
     *       BMOD .GT. BMOH(1))) DLTH = 'Y'
        END IF

C       AT THIS POINT COULD HAVE A "PARTIAL" MONTH AWAITING ADJUSTMENT IF
C       DATA PRECEDED HISTORY (OR SIMPLY WAITING FOR ADJUSTMENT WITH
C       OBSERVATION TIME = "SS" IF BEGIN DATE OF HISTORY IS A NEW MONTH),
C       OR COULD SKIP OVER SOME CHANGES ("NCHGS") IF HISTORY PRECEDED
C       DATA, OR COULD BE "NORMAL" WAY WITH DATA = HISTORY
C       INITIALIZE ADJUSTMENT VARIABLES
  550   DO 560 J = 1,31
          NA(J) = 0
  560   CONTINUE

        NP = 0

C       LOOP THROUGH ALL OBSERVATION TIME CHANGES
        DO 640 N = 1,NCHGS

C         HISTORY PRECEDES DATA
          IF(DLTH .EQ. 'Y') THEN
  
C           SKIP OVER ALL CHANGES UNTIL DATA BEGINS BETWEEN CHANGES
            IF(BYRD .GT. BYRH(N+1) .OR. (BYRD .EQ. BYRH(N+1)
     *         .AND. BMOD .GT. BMOH(N+1))) GO TO 640
C           RESET FLAG
            DLTH = 'N'

C           DEBUG PRINTOUT
            IF(DEBUG .ge. 1) WRITE(6,570) BMOD,BYRD, TOBTMP(N)
  570                        FORMAT('DATA BEGINS AFTER HISTORY.  BEGIN',
     *                              ' ADJUSTMENTS AT ',I2.2,I4,
     *                              ' WITH OBSERVATION TIME = ',I2)

C           INITIALIZE START DATE TO BEGIN DATE OF DATA
            BEGDAY = 1
            STARTM = BMOD
            STARTY = BYRD

C         HISTORY DOES NOT PRECEDE DATA:  INITIALIZE START DATE TO HISTORY
C         DATE OF CHANGE
          ELSE
            BEGDAY = BDAYH(N)
            STARTM = BMOH(N)
            STARTY = BYRH(N)
          END IF

C         INITIALIZE END DATE TO HISTORY DATE OF NEXT CHANGE
          ENDDAY = BDAYH(N+1)
          ENDM = BMOH(N+1)
          ENDY = BYRH(N+1)

C         SAVE START MONTH AND YEAR
          M = STARTM
          Y = STARTY

C         COPY FLAG FOR LONGEST PERIOD OF PARTIAL MONTH
c  580     IF(NP .GT. 1 .AND. 
c     *       SAVMO .NE. M .AND. NA(1) .GT. NA(2)) THEN
c            IXF(SAVYR,SAVMO,1)(3:3) = ESTOB(N-1)
c            IXF(SAVYR,SAVMO,2)(3:3) = ESTOB(N-1)
c            IXF(SAVYR,SAVMO,3)(3:3) = ESTOB(N-1)
c          END IF

C         COPY FLAG FOR START MONTH AND YEAR FROM HISTORY CHANGE FLAG
c          IXF(Y,M,1)(3:3) = ESTOB(N)
c          IXF(Y,M,2)(3:3) = ESTOB(N)
c          IXF(Y,M,3)(3:3) = ESTOB(N)

C         FINISHED HISTORY OF MONTH
  580     IF(M .NE. SAVMO) THEN

C           FIRST TIME THROUGH, DATA DID NOT PRECEDE HISTORY
            IF(SAVMO .NE. 13) THEN

C             SAVE YEAR FOR ADJUSTING (SINCE YEAR HAS ALREADY BEEN INCREMENTED)
              IF(SAVMO .EQ. 12) THEN
                SAVYR = Y - 1
              ELSE
                SAVYR = Y
              END IF

C             INITIALIZE ADJUSTMENT VARIABLES
              ADJAV = 0.
              ADJMN = 0.
              ADJMX = 0.
              SUMNA = 0.

C             CALCULATE NUMBER OF DAYS TO ADJUST FOR
              DO 590 P = 1,NP
                SUMNA = SUMNA + FLOAT(NA(P))
  590         CONTINUE

C             CALCULATE ADJUSTED VALUES
              DO 600 P = 1,NP
                ADJAV = ADJAV + FLOAT(NA(P)) * AAV(P) / SUMNA
                ADJMN = ADJMN + FLOAT(NA(P)) * AMN(P) / SUMNA
                ADJMX = ADJMX + FLOAT(NA(P)) * AMX(P) / SUMNA
  600         CONTINUE

C             IF MORE THAN TWO CHANGES IN ANY MONTH, SET FLAG TO "F"LAKY
              IF(NP .GT. 2) THEN
                DO 610 L = 1,3
c                  IXF(Y,M,L)(3:3) = 'F'
                  IXF(Y,M,L)(2:2) = 'F'
  610           CONTINUE
              END IF

C             SAVE ADJUSTED VALUES
              ADJ(SAVYR,SAVMO,1) = ADJMX
              ADJ(SAVYR,SAVMO,2) = ADJMN
              ADJ(SAVYR,SAVMO,3) = ADJAV

C             ADJUST NONMISSING VALUES
              DO 620 I = 1,3
                IF(IX(SAVYR,SAVMO,I) .NE. amiss) IX(SAVYR,SAVMO,I) =
     *             IX(SAVYR,SAVMO,I) - ADJ(SAVYR,SAVMO,I)
  620         CONTINUE

C             REINITIALIZE ADJUSTMENT VARIABLES
              DO 630 J = 1,31
                NA(J) = 0
  630         CONTINUE

              NP = 0

            END IF
  
C           SAVE MONTH AND SET OBSERVATION TIME TO UNKNOWN
            SAVMO = M
            TOB = 99
          END IF

C         SAVE OBSERVATION TIME AND SET OBSERVATION TIME TO HISTORY
C         OBSERVATION TIME
          SAVTOB = TOB
          TOB = TOBTMP(N)

C          CONVERT OBSERVATION TIMES OF "RS", "SR", AND "SS" TO HOURS
C         NOTE:  PREVIOUSLY TOBTMP VALUES OF 25 WERE CHANGED TO 24
          IF(TOBTMP(N) .GE. 26 .AND. TOBTMP(N) .LE. 28)
     *       CALL TOBCHG(M,TOB)

C         DETERMINE IF AT END DATE
          IF(Y .EQ. ENDY .AND. M .EQ. ENDM) THEN

C           DETERMINE IF CHANGE OCCURS WITHIN SAME MONTH
            IF(Y .EQ. STARTY .AND. M .EQ. STARTM) THEN

C             DATA PRECEDES HISTORY
              IF(PREVP .EQ. 'Y') THEN

C               COUNT NUMBER OF TIMES AND STORE NUMBER OF DAYS TO ADJUST
                NP = NP + 1
                NA(NP) = BDAYH(N) - 1

C               GET ADJUSTMENT VALUES
                AMX(NP) = A(M,SAVTOB,1)
                AMN(NP) = A(M,SAVTOB,2)
                AAV(NP) = A(M,SAVTOB,3)

C               RESET FLAG
                PREVP = 'N'
              END IF

              NP = NP + 1
              NA(NP) = ENDDAY - BEGDAY

C           CHANGE DOES NOT OCCUR WITHIN SAME MONTH
            ELSE

C             COUNT TIMES AND STORE NUMBER OF DAYS TO ADJUST
              NP = NP + 1
              NA(NP) = ENDDAY - 1
            END IF

C           GET ADJUSTMENT VALUES AND SAVE MONTH
            AMX(NP) = A(M,TOB,1)
            AMN(NP) = A(M,TOB,2)
            AAV(NP) = A(M,TOB,3)
            SAVMO = M

C         NOT AT END DATE
          ELSE

C           GET NUMBER OF DAYS IN MONTH
            DAYS = DAYSMO(M)

C           ADJUST NUMBER OF DAYS IN FEBRUARY FOR LEAP YEAR
            IF(M .EQ. 2 .AND.
     *         ((MOD(Y,4) .EQ. 0 .AND. MOD(Y,100) .NE. 0) .OR.
     *          MOD(Y,400) .EQ. 0)) DAYS = 29

C           CHECK FOR PARTIAL MONTH
            IF(Y .EQ. BYRH(N) .AND. M .EQ. BMOH(N)) THEN

C             DATA PRECEDES HISTORY
              IF(PREVP .EQ. 'Y') THEN

C               COUNT TIMES AND STORE NUMBER OF DAYS TO ADJUST
                NP = NP + 1
                NA(NP) = BEGDAY - 1

C               GET ADJUSTMENT VALUES
                AMX(NP) = A(M,SAVTOB,1)
                AMN(NP) = A(M,SAVTOB,2)
                AAV(NP) = A(M,SAVTOB,3)

C               RESET FLAG
                PREVP = 'N'
              END IF

C             COUNT TIMES AND STORE NUMBER OF DAYS TO ADJUST
              NP = NP + 1
              NA(NP) = DAYS - BEGDAY + 1

C           NOT PARTIAL MONTH
            ELSE

C             COUNT TIMES AND STORE NUMBER OF DAYS TO ADJUST
              NP = NP + 1
              NA(NP) = DAYS
            END IF

C           GET ADJUSTMENT VALUES
            AMX(NP) = A(M,TOB,1)
            AMN(NP) = A(M,TOB,2)
            AAV(NP) = A(M,TOB,3)

C           NEXT MONTh
            M = M + 1

C           ROLL TO NEXT YEAR IF MONTH IS JANUARY
            IF(M .GT. 12) THEN
              M = 1
              Y = Y + 1
            END IF

C           LOOP BACK THROUGH UNTIL NEXT CHANGE
            GO TO 580
          END IF

  640   CONTINUE

C       ADJUST FOR LAST MONTH
C       REINITIALIZE ADJUSTMENT VALUES
        ADJAV = 0.
        ADJMN = 0.
        ADJMX = 0.
        SUMNA = 0.

C       CALCULATE TOTAL NUMBER OF DAYS
        DO 650 P = 1,NP
          SUMNA = SUMNA + FLOAT(NA(P))
  650   CONTINUE

C       CALCULATE ADJUSTMENT VALUES
        DO 660 P = 1,NP
          ADJAV = FLOAT(NA(P)) * AAV(P) / SUMNA + ADJAV
          ADJMN = FLOAT(NA(P)) * AMN(P) / SUMNA + ADJMN
          ADJMX = FLOAT(NA(P)) * AMX(P) / SUMNA + ADJMX
  660   CONTINUE

C       SAVE ADJUSTMENT VALUES
        ADJ(Y,M,1) = ADJMX
        ADJ(Y,M,2) = ADJMN
        ADJ(Y,M,3) = ADJAV

C       ADJUST NONMISSING VALUES
        DO 670 I = 1,3
          IF(IX(Y,M,I) .NE. amiss) IX(Y,M,I) = IX(Y,M,I) - ADJ(Y,M,I)
  670   CONTINUE

C       WRITE TIME OF OBSERVATION ADJUSTED DATA AND FLAGS TO OUTPUT FILE
  680   DO 740 I = 1,3
  
c         save the normals TOB adjustment line for the current obtime
          atob = 0.0
          do im = 1,12
            tob = tobtmp(nchgs)
            if(tobtmp(n) .ge. 26 .and. tobtmp(n) .le. 28)
     *        call tobchg(im,tob)
            aoblast(im) = a(im,tob,i)
            atob = atob + a(im,tob,i)
          enddo
          atob = atob / 12  
          otfile = dfile(1:iflen) // atype // '/' // 
     *      cstn // '.' // atype // '.' // celem(i)
c          cmd = 'rm ' // otfile
c          call system(cmd)
c          print *,cmd
          open(17, file=otfile, err=870)
          write(17,1000) cstn,i,
     *      (nint(aoblast(im)*scale(i)),im=1,12),nint(atob*scale(i))
 1000     format(a,i1,'19', 12(i6,'   '),i7,'   ')
          close(17)

          odfile = dfile(1:iflen) // otype // '/' // 
     *      cstn // '.' // otype // '.' // celem(i)
c          cmd = 'rm ' // odfile
c          call system(cmd)
c          print *,cmd
          open(16, file=odfile, err=850)

          if(debug .ge. 1) write(6,*) 'ELEM:', celem(i)
          
          DO 730 Y = ibegyr,ENDYR

C           COMPUTE TEMPERATURE ANNUAL VALUE
            IX(Y,13,I) = 0.

              DO 690 M = 1,12
                IF(IX(Y,M,I) .NE. amiss) THEN
                  IX(Y,13,I) = IX(Y,13,I) + IX(Y,M,I)
                ELSE
                  IX(Y,13,I) = amiss
                  GO TO 700
                END IF
  690         CONTINUE

              IX(Y,13,I) = IX(Y,13,I) / 12. + 0.00002

  700       DO 720 M = 1,12

C             WRITE OUT ONLY IF SOME VALUE IS NOT MISSING - Normals format
              IF(IX(Y,M,I) .NE. amiss) THEN
                if(debug .ge. 2)
     *           WRITE(6,'("dat ",i4,1x,12f7.2)')Y,(IX(Y,MM,i),MM=1,12)
                WRITE(16,710,ERR=880) CSTN,i,y,
     *             (nint(IX(Y,MM,I)*scale(i)),IXF(Y,MM,I),MM=1,13)
  710           FORMAT(a,i1,I4,13(i6,A3))
                GO TO 730
              END IF
  720       CONTINUE
  730     CONTINUE
          close(16)
  740   CONTINUE

C       DEBUG PRINTOUT
        IF(DEBUG .ge. 2) THEN
          J = BYRD

          DO 770 I = 1,3
c            WRITE(6,750) CSTN,J,I,(IX(J,K,I),IXF(J,K,I)(3:3),K=1,12)
            WRITE(6,750) CSTN,J,I,(IX(J,K,I),IXF(J,K,I),K=1,12)
  750       FORMAT(a,1X,I4,1X,I1,1X,12(F6.2,1X,A3,1X))
            WRITE(6,760) CSTN,J,I,(ADJ(J,K,I),K=1,12)
  760       FORMAT(a,1X,I4,1X,I1,1X,12(F6.2,5X))
  770     CONTINUE
        END IF

  780 end do

C     CLOSE FILES
  785 continue
c      IF(DEBUG .ge. 1) CLOSE(6)
      CLOSE(12)
      CLOSE(11)
      CLOSE(16,ERR=900)

C     NOTIFY USER OF SUCCESSFUL COMPLETION
      WRITE(6,790)
  790 FORMAT("hcn_tobs HAS COMPLETED SUCCESSFULLY!")
      call subpon(" ============ NRMTOB exit 790", 438, 1)
C     SKIP ERROR MESSAGES AND END PROGRAM
      GO TO 940

C     PRINT ERROR MESSAGE AND ABORT PROGRAM
  800 print *,'CANNOT OPEN debug FILE: ',dbgfile
      GO TO 920

  820 print *,'CANNOT OPEN metaFILE:', metafile
      GO TO 920

  830 call perror('CANNOT read metaFILE:' // metafile)
      GO TO 920

  840 print *,'CANNOT OPEN hcn_xs_data FILE.'
      GO TO 920

  850 call perror('Cannot write to: ' // odfile)
      go to 920
      
  870 call perror('Cannot write to: ' // otfile)
      go to 920    

  860 print *,'CANNOT OPEN hcn_tobs_data FILE.'
      GO TO 920

  880 print *,'CANNOT WRITE TO hcn_tobs_data FILE.'
      GO TO 920

  900 print *,'CANNOT CLOSE hcn_tobs_data FILE.'
  920 WRITE(6,930)
  930 FORMAT('PROGRAM IS ABORTING.')

  940 STOP

      END
      subroutine subpon(txt, iu, iw)
      character*80 txt
      character*24 greeting
      call fdate( greeting ) 
      print *, txt, " ", greeting
      if (iw .eq. 0) then
	open(iu, FILE = '~/ponfile8.txt', STATUS = 'old', ERR = 150)
      endif
      write(iu, 1) txt, " ", greeting
    1 format(1x, A, A, A)
      if (iw .eq. 1) then 
	close(iu)
      endif
      go to 160
  150 print *, txt, " OPEN ERROR ", iu, " ", greeting
  160 return
      end
C***********************************************************************
C End of Program HCNTOB.                                               *
C***********************************************************************


C***********************************************************************
C                           Function FNDIFF                            *
C                                                                      *
C DATE OF LAST CHANGE:  20 October 1993                                *
C                                                                      *
C MODIFIER:  David Bowman                                              *
C                                                                      *
C VERSION     MODIFICATION                                      DATE   *
C -------     ------------                                    -------- *
C   1.1       SUN Workstation Version                         10/20/93 *
C                                                                      *
C   1.0       Original                                        02/XX/85 *
C                                                                      *
C AUTHOR:  Claude Williams                                             *
C                                                                      *
C MODIFIERS:  Pam Hughes                                               *
C             David Bowman                                             *
C                                                                      *
C USAGE:  FNDIFF(real ALATZ, real ALONZ, integer IMO)                  *
C                                                                      *
C DESCRIPTION:  Calculates inter-daily temperature difference.         *
C                                                                      *
C NOTES:  None.                                                        *
C                                                                      *
C RESULTS:  Returns inter-daily temperature difference.                *
C                                                                      *
C SUBROUTINES:  QUAD                                                   *
C                                                                      *
C VARIABLES:                                                           *
C                                                                      *
C   ALAT       Not Used by this Function                               *
C   (130)                                                              *
C                                                                      *
C   ALATZ      Latitude of Station in Degrees                          *
C                                                                      *
C   ALON       Not Used by this Function                               *
C   (130)                                                              *
C                                                                      *
C   ALONZ      Longitude of Station in Degrees                         *
C                                                                      *
C   BASAV      Not Used by this Function                               *
C   (12,-12:11)                                                        *
C                                                                      *
C   BASMN      Not Used by this Function                               *
C   (12,-12:11)                                                        *
C                                                                      *
C   BASMX      Not Used by this Function                               *
C   (12,-12:11)                                                        *
C                                                                      *
C   BLATZ      Same as ALATZ                                           *
C                                                                      *
C   BLONZ      Same as ALONZ                                           *
C                                                                      *
C   CFAV       Not Used by this Function                               *
C   (12)                                                               *
C                                                                      *
C   CFMN       Not Used by this Function                               *
C   (12)                                                               *
C                                                                      *
C   CFMX       Not Used by this Function                               *
C   (12)                                                               *
C                                                                      *
C   DTDIF      Inter-Daily Temperature Differences for Stations Used   *
C   (12,130)   in the Analysis                                         *
C                                                                      *
C   DZ         Distances for the Closest Station Within Each Quadrant  *
C   (4)                                                                *
C                                                                      *
C   I          Loop Counter                                            *
C                                                                      *
C   IMO        Month                                                   *
C                                                                      *
C   IZ         Index Numbers for the Closest Station Within Each       *
C   (4)        Quadrant                                                *
C                                                                      *
C   NSTA       Not Used by this Function                               *
C                                                                      *
C   RELEV      Not Used by this Function                               *
C   (12)                                                               *
C                                                                      *
C   RHO        Not Used by this Function                               *
C   (130)                                                              *
C                                                                      *
C   SDZ        Calculation for Weighted Average                        *
C                                                                      *
C   STDIF      Calculation for Weighted Average                        *
C                                                                      *
C***********************************************************************

      FUNCTION FNDIFF(ALATZ,ALONZ,IMO)

      COMMON /BIAS/ALAT,ALON,BASAV,BASMN,BASMX,CFAV,CFMN,CFMX,DTDIF,
     *             NSTA,RELEV,RHO
      DIMENSION ALAT(130),ALON(130)
      DIMENSION BASAV(12,-12:11),BASMN(12,-12:11),BASMX(12,-12:11)
      DIMENSION CFAV(12),CFMN(12),CFMX(12),RELEV(12),RHO(130)
      DIMENSION DTDIF(12,130)

      COMMON /QUADR/BLATZ,BLONZ,DZ,IZ
      DIMENSION DZ(4),IZ(4)

C     ESTIMATE THE INTER-DAILY TEMPERATURE DIFFERENCE FROM THE CLOSEST
C     STATIONS IN EACH QUADRANT

C     FIND CLOSEST STATIONS IN EACH QUADRANT IF FIRST CALL OF THIS
C     FUNCTION FOR A PARTICULAR STATION

      IF(BLATZ .NE. ALATZ .AND. BLONZ .NE. ALONZ) THEN

C     INITIALIZE VARIABLES

        BLATZ = ALATZ
        BLONZ = ALONZ
        DO 10 I = 1,4
          DZ(I) = 999.
          IZ(I) = 999
   10   CONTINUE

C     FIND CLOSEST STATIONS IN EACH QUADRANT

        CALL QUAD(ALATZ,ALONZ,DZ,IZ)

      END IF

C     FOUND STATION WITHIN 25 KILOMETERS; USE STATION'S DTDIF VALUE AS
C     INTER-DAILY TEMPERATURE DIFFERENCE

      IF(DZ(1) .EQ. 0.) THEN
        FNDIFF = DTDIF(IMO,IZ(1))
      ELSE

C     USE STATION IN EACH QUADRANT TO DETERMINE INTER-DAILY TEMPERATURE
C     DIFFERENCE

        SDZ = 0.
        STDIF = 0.

C     CALCULATE USING WEIGHTED AVERAGE

        DO 20 I = 1,4
          IF(DZ(I) .NE. 999.) THEN
            SDZ = SDZ + (1. / DZ(I))
            STDIF = STDIF + (DTDIF(IMO,IZ(I)) / DZ(I))
          END IF
   20   CONTINUE

        FNDIFF = STDIF / SDZ
      END IF

      RETURN

      END

C***********************************************************************
C End of Function FNDIFF.                                              *
C***********************************************************************


C***********************************************************************
C                            Function FNRNG                            *
C                                                                      *
C DATE OF LAST CHANGE:  20 October 1993                                *
C                                                                      *
C MODIFIER:  David Bowman                                              *
C                                                                      *
C VERSION     MODIFICATION                                      DATE   *
C -------     ------------                                    -------- *
C   1.1       SUN Workstation Version                         10/20/93 *
C                                                                      *
C   1.0       Original                                        02/XX/85 *
C                                                                      *
C AUTHOR:  Claude Williams                                             *
C                                                                      *
C MODIFIERS:  Pam Hughes                                               *
C             David Bowman                                             *
C                                                                      *
C USAGE:  FNRNG(real ALATZ, real ALONZ)                                *
C                                                                      *
C DESCRIPTION:  Calculates hour range for December.                    *
C                                                                      *
C NOTES:  None.                                                        *
C                                                                      *
C RESULTS:  Returns hour range for December.                           *
C                                                                      *
C SUBROUTINES:  QUAD                                                   *
C                                                                      *
C VARIABLES:                                                           *
C                                                                      *
C   ALAT       Not Used by this Function                               *
C   (130)                                                              *
C                                                                      *
C   ALATZ      Latitude of Station in Degrees                          *
C                                                                      *
C   ALON       Not Used by this Function                               *
C   (130)                                                              *
C                                                                      *
C   ALONZ      Longitude of Station in Degrees                         *
C                                                                      *
C   BASAV      Not Used by this Function                               *
C   (12,-12:11)                                                        *
C                                                                      *
C   BASMN      Not Used by this Function                               *
C   (12,-12:11)                                                        *
C                                                                      *
C   BASMX      Not Used by this Function                               *
C   (12,-12:11)                                                        *
C                                                                      *
C   BLATZ      Same as ALATZ                                           *
C                                                                      *
C   BLONZ      Same as ALONZ                                           *
C                                                                      *
C   CFAV       Not Used by this Function                               *
C   (12)                                                               *
C                                                                      *
C   CFMN       Not Used by this Function                               *
C   (12)                                                               *
C                                                                      *
C   CFMX       Not Used by this Function                               *
C   (12)                                                               *
C                                                                      *
C   DTDIF      Not Used by this Function                               *
C   (12,130)                                                           *
C                                                                      *
C   DZ         Distances for the Closest Station Within Each Quadrant  *
C   (4)                                                                *
C                                                                      *
C   I          Loop Counter                                            *
C                                                                      *
C   IZ         Index Numbers for the Closest Station Within Each       *
C   (4)        Quadrant                                                *
C                                                                      *
C   NSTA       Not Used by this Function                               *
C                                                                      *
C   RELEV      Not Used by this Function                               *
C   (12)                                                               *
C                                                                      *
C   RHO        Hour Range for December                                 *
C   (130)                                                              *
C                                                                      *
C   SDZ        Calculation for Weighted Average                        *
C                                                                      *
C   SRNG       Calculation for Weighted Average                        *
C                                                                      *
C***********************************************************************

      FUNCTION FNRNG(ALATZ,ALONZ)

      COMMON /BIAS/ALAT,ALON,BASAV,BASMN,BASMX,CFAV,CFMN,CFMX,DTDIF,
     *             NSTA,RELEV,RHO
      DIMENSION ALAT(130),ALON(130)
      DIMENSION BASAV(12,-12:11),BASMN(12,-12:11),BASMX(12,-12:11)
      DIMENSION CFAV(12),CFMN(12),CFMX(12),RELEV(12),RHO(130)
      DIMENSION DTDIF(12,130)

      COMMON /QUADR/BLATZ,BLONZ,DZ,IZ
      DIMENSION DZ(4),IZ(4)

C     ESTIMATE DIURNAL TEMPERATURE RANGE FROM CLOSEST STATIONS IN EACH
C     QUADRANT

C     FIND CLOSEST STATIONS IN EACH QUADRANT IF FIRST CALL OF THIS
C     FUNCTION FOR A PARTICULAR STATION

      IF(BLATZ .NE. ALATZ .AND. BLONZ .NE. ALONZ) THEN

C     INITIALIZE VARIABLES

        BLATZ = ALATZ
        BLONZ = ALONZ
        DO 10 I = 1,4
          DZ(I) = 999.
          IZ(I) = 999
   10   CONTINUE

C     FIND CLOSEST STATIONS IN EACH QUADRANT

        CALL QUAD(ALATZ,ALONZ,DZ,IZ)

      END IF

C     FOUND STATION WITHIN 25 KILOMETERS; USE STATION'S RHO VALUE AS
C     HOUR RANGE VALUE

      IF(DZ(1) .EQ. 0.) THEN
        FNRNG = SQRT(RHO(IZ(1)))

C     USE STATION IN EACH QUADRANT TO DETERMINE HOUR RANGE VALUE

      ELSE
        SDZ = 0.
        SRNG = 0.

C     CALCULATE USING WEIGHTED AVERAGE

        DO 20 I = 1,4
          IF(DZ(I) .NE. 999.) THEN
            SDZ = SDZ + (1. / DZ(I))
            SRNG = SRNG + (RHO(IZ(I)) / DZ(I))
          END IF
   20   CONTINUE

        FNRNG = SQRT(SRNG / SDZ)
      END IF

      RETURN

      END

C***********************************************************************
C End of Function FNRNG.                                               *
C***********************************************************************

c***********************************************************************
C     Subroutine GETDAT.
C***********************************************************************
      SUBROUTINE GETDAT(IX, dfile, IXF, IFLAG, cstn, ctype, debug)

      include 'prehomog.parm.incl'
      PARAMETER (imxchg = 200, minyear = 20)
     
      INTEGER IFLAG, debug
      CHARACTER*3 IXF(ibegyr-1:ilstyr,13,3)
      REAL IX(ibegyr-1:ilstyr,13,3)
      character*11 istn, cstn
      character*3 ctype
      
      character*132 dfile, efile

      integer idata(13), iflen
      character*3 cflag(13)
      
      iflen = lnblnk(dfile)
      
c     initialize
      do iy = ibegyr, ilstyr
        do im = 1,13
          do ie =  1,3
            ix(iy, im, ie) = amiss
            ixf(iy, im, ie) = '   '
          end do
        end do
      end do      
      
      do ie = 1, 2
c       open station data file
        efile = dfile(1:iflen) // ctype // '/' // cstn // '.' //
     *    ctype // '.' // celem(ie)
        if(debug .gt. 0) print *, efile
        open(17, file=efile, err=110)
    
C       READ FILE UNTIL CORRECT STATION
        DO WHILE(1 .eq. 1)
C         GET DATA VALUES AND DAY FLAGS
          READ(17,40,END=100,ERR=110) iSTN,ielem,iYEAR,
     *      (iDATA(I),cFLAG(i),I=1,13)
   40     FORMAT(a11,i1,I4,13(i6,a3))
c          print *,iSTN,ielem,iYEAR,iData

c         skip if not in year range
          if(iyear .ge. ibegyr .and. iyear .le. ilstyr) then
            asum = 0.0
            nsum = 0
            do im = 1, 12
              ixf(iyear, im, ie) = cflag(im)
              if(idata(im) .ne. inmiss) then
                ix(iyear, im, ie) = float(idata(im))/scale(ie)
                asum = asum + ix(iyear, im, ie)
                nsum = nsum + 1
              else
                ix(iyear, im, ie) = amiss
              endif  
            enddo  
c           generate annual
            if(nsum .eq. 12) ix(iyear, 13, ie) = asum / nsum      
          endif
        end do
        
  100   close(17)
      end do
      
c      print *,' IX1: ', (ix(iy,12,2),iy=2000,2007)
      
c     generate average
      do iyear = ibegyr, ilstyr
        do im = 1, 13
          if(ix(iyear,im,1).ne.amiss .and. ix(iyear,im,2).ne.amiss)
     *      ix(iyear,im,3) = (ix(iyear,im,1)+ix(iyear,im,2))/2.0
        enddo
      enddo  
      
      return
      
  110 call perror(' Error in reading: ' // efile)
      iflag = 1
      return
      END

C***********************************************************************
C End of Subroutine GETDAT.                                            *
C***********************************************************************


C***********************************************************************
C                          Subroutine GETOBT                           *
C                                                                      *
C DATE OF LAST CHANGE:  08 March 1994                                  *
C                                                                      *
C MODIFIER:  David Bowman                                              *
C                                                                      *
C VERSION     MODIFICATION                                      DATE   *
C -------     ------------                                    -------- *
C   1.1       SUN Workstation Version                         03/08/94 *
C                                                                      *
C   1.0       Original                                                 *
C                                                                      *
C AUTHOR:  Claude Williams                                             *
C                                                                      *
C MODIFIERS:  Pam Hughes                                               *
C             David Bowman                                             *
C                                                                      *
C USAGE:  GETOBT(integer BDAYH(imxchg),        *
C                integer BMOH(imxchg), integer BYRH(imxchg),           *
C                integer STNID, integer NCHGS,         *
C                integer TOBTMP(0:imxchg))             *
C                                                                      *
C DESCRIPTION:  This subroutine reads the station history for one HCN  *
C               station to determine the station's most current        *
C               latitude and longitude, the number of times the time   *
C               of observation changed along with the date of change,  *
C               and the time of observation encoded.                   *
C                                                                      *
C NOTES:  This subroutine originally checked to see if the observer    *
C         was the "WEATHER BUREAU" whenever there was a change in the  *
C         observation time.  If so, a flag was set to "1"; otherwise,  *
C         to "0".  See the main program's "NOTES:" section for more    *
C         information on how this flag was used.                       *
C         - D. Bowman:  03/08/94                                       *
C                                                                      *
C RESULTS:  Returns station's most current latitude and longitude, the *
C           number of times the time of observation changed, the dates *
C           of change, the time of observations encoded, and whether   *
C           the observer was the "WEATHER BUREAU".                     *
C                                                                      *
C PARAMETERS:                                                          *
C                                                                      *
C   imxchg     Maximum Number of Observation Time Changes Over a       *
C              Station's Period of Record                              *
C                                                                      *
C VARIABLES:                                                           *
C                                                                      *
C   BDAY       Begin Day of History Record                             *
C                                                                      *
C   BDAYH      Begin Days Where Time of Observations Change            *
C   (imxchg)                                                           *
C                                                                      *
C   BMOH       Begin Months Where Time of Observations Change          *
C   (imxchg)                                                           *
C                                                                      *
C   BMONTH     Begin Month of History Record                           *
C                                                                      *
C   BYEAR      Begin Year of History Record                            *
C                                                                      *
C   BYRH       Begin Years Where Time of Observations Change           *
C   (imxchg)                                                           *
C                                                                      *
C   I          Loop Counter                                            *
C                                                                      *
C   INSTFL     Flag:    0 => None of the Instruments Below Present     *
C                       1 => One of the Following Instruments Present: *
C                               (6) Hygrothermograph                   *
C                               (7) Minimum Thermometer                *
C                               (8) Maximum Thermometer                *
C                              (18) Thermograph                        *
C                              (19) Digital Thermometer                *
C                              (22) Maximum/Minimum Temperature System *
C                                                                      *
C   INSTR      Station Instruments                                     *
C   (36)                                                               *
C                                                                      *
C   LATDEG     Degrees of Latitude                                     *
C                                                                      *
C   LATMIN     Minutes of Latitude                                     *
C                                                                      *
C   LOBCOD     Previous Observation Time Code                          *
C                                                                      *
C   LONDEG     Degrees of Longitude                                    *
C                                                                      *
C   LONMIN     Minutes of Longitude                                    *
C                                                                      *
C   NCHGS      Number of Times Observation Time Changes                *
C                                                                      *
C   OBCODE     Observation Time Code                                   *
C                                                                      *
C   OBTIME     Observation Time from Station History Record:           *
C                           (1:2) => Precipitation Time of Observation *
C                           (3:4) => Temperature Time of Observation   *
C                                                                      *
C   STNID      ID of Station on Which to Get Observation Times         *
C                                                                      *
C   STNIDR     ID of Station Read from Record                          *
C                                                                      *
C   TMPCOD     ASCII Conversion of Character Temperature Time of "01"  *
C              through "24"                                            *
C                                                                      *
C   TOBTMP     Code for Time of Observation:     01 - 24 => 01 - 24    *
C   (0:imxchg)                                        25 => "xxHR"     *
C                                                     26 => "RS"       *
C                                                     27 => "SR"       *
C                                                     28 => "SS"       *
C                                                     29 => All Others *
C                                                     30 => "TRID"     *
C                                                     99 => Unknown    *
C                                                                      *
C***********************************************************************
c
c     NOTE: ALATZ and ALONZ are no longer retreived from the Station
c           History file, they now come form the Meta File!

      SUBROUTINE GETOBT(BDAYH,BMOH,BYRH,dfile,cstn,NCHGS,TOBTMP,NORMRUN,
     *  debug)

c     normrun indicates that the HCN SHF records will not be used,
c     even if avaialble because composite stations histories can
c     only be used with composite station data.

      include 'prehomog.parm.incl'
      PARAMETER (imxchg = 200, minyear = 20)

      INTEGER BDAYH(imxchg),BMOH(imxchg),BYRH(imxchg)
      INTEGER TOBTMP(0:imxchg), obcode
      integer src, lhcn, ltd, debug

      CHARACTER*4 obtime
      character*6 shfid
      character*11 cstn
      character*2 obtim
      character*132 dfile, hfile
      INTEGER BDAY,BMONTH,BYEAR,eday,emonth,eyear
      REAL LATDEG,LATMIN,LONDEG,LONMIN

C     INITIALIZE VARIABLES

      INSTFL = 0
      LOBCOD = -1
      NCHGS = 0
      leyear = 0
      lbyear = 0
      ltd = 0
      
      iflen = lnblnk(dfile)      
      hfile = dfile(1:iflen) // 'his/' // cstn // '.his'
      open(10, file=hfile, err=35)

c     set the offset from the end of the file name that the station
c         name starts (used for history files)
      isolen = 10

C     GET STATION ID, RECORD BEGIN DATE, LATITUDE, LONGITUDE,
C     INSTRUMENTS, AND OBSERVATION TIME
      DO WHILE(1 .eq. 1)

   10   READ(10,20,ERR=35,end=40) src,shfid,BYEAR,BMONTH,BDAY,eyear,
     *                                emonth, eday, LATDEG,
     *                                LATMIN,LONDEG,LONMIN,
     *                                OBTIME
   20   FORMAT(i1,1x,a6,1x,I4,2I2,1X,I4,2I2,1x,F3.0,1X,F2.0,4X,F4.0,1X,
     *         F2.0,28X,A4)
     
c       For the COOP/CDMP runs, do not use src=0
        if(normrun .eq. 1 .and. src .eq. 0) goto 10
        
c       do not use daily (TD3200) records
        if(src .eq. 1) goto 10
        
c       if(leyear == 9999) reset when the source changes
        if(leyear .eq. 9999 .and. lsrc .ne. src) leyear = lbyear
        
c       the source data is in highest to lowest priority
c       as of 30 July 2001 that order is
c       Source 0  Jim Owenby's 1961-1990 tob file (for normals)
c                 USHCN station History file (for USHCN)
c       Source 1  TD3200 - warning - only good AFTER 1982
c                 Removed 09Jul2008 due to conflicts with NEW
c                 MSI (MSHR) data
c       Source 2  Master Station History Report - most detailed yet
c                   hardest to keep upto date
c        print *, byear, eyear, ibegyr, leyear
        if(eyear .lt. ibegyr .or. byear .lt. leyear) go to 30

c        print *,src,shfid,BYEAR,BMONTH,BDAY,eyear,
c     *                                emonth, eday, LATDEG,
c     *                                LATMIN,LONDEG,LONMIN,
c     *                                OBTIME 

C       INITIALIZE OBSERVATION TIME CODE
        OBCODE = 0

C       DECODE OBSERVATION TIME
        IF(OBtime .EQ. 'TRID') THEN
          OBCODE = 30
          go to 25
        endif 
c       Error in decoding 9909 & 9919 for 18 years!!!!! - 12Jul05 cw
        if(obtime(1:1) .eq. '9' .and. obtime(4:4) .eq. '9' .and.
     *    obtime .ne. '9909' .and. obtime .ne. '9919') then
          obtim = obtime(2:3)
        else
          obtim = obtime(3:4)
        endif

        if(obtim .eq. '00' .or. obtim .eq. '0 ') then 
          obcode = 24
        else IF(OBTIM .EQ. 'HR') THEN 
          OBCODE = 25
        else IF(OBTIM .EQ. 'RS' .or. OBTIM .EQ. 'VR' .or. 
     *    obtim .eq. 'VA') THEN 
            OBCODE = 26
        ELSE IF(OBTIM .EQ. 'SR') THEN 
            OBCODE = 27
        ELSE IF(OBTIM .EQ. 'SS' .or. OBTIM .EQ. 'PM') THEN 
            OBCODE = 28
        ELSE IF(OBTIM .EQ. '99' .or. obtim .eq. '  ' .or. 
     *    OBTIM .EQ. 'DE' .or. obtim .eq. 'MI' .or. 
     *    OBTIM .EQ. '31' .or. obtim .eq. '73' .or. 
     *    OBTIM .EQ. '78' .or. obtim .eq. '83' .or. 
     *    OBTIM .EQ. '89' .or. obtim .eq. 'UN') THEN
            OBCODE = 99
        ENDIF
        
C       CONTINUE DECODING OBSERVATION TIME
   25   if(obcode .eq. 0) then
          READ(OBTIM,FMT='(i2)') obcode
        END IF
        
        if (obcode .gt. 30 .and. obcode .ne. 99) then
          print *, 'Input Error ', shfid,' Bad obtime:',obtime,' abort'
          stop 999
        endif  

C       COUNT CHANGE AND SAVE BEGIN DATE, AND OBSERVATION CODE; SET
C       PREVIOUS OBSERVATION CODE
c       Station History file has three different sources.
c         From highest quality to lowest - 
c        src = 0 is the Highly QC'd HCN station history
c  >>>>>>>>>>> UNFORTUNATELY, DUE TO COMPOSITE STATIONS, THIS SOURCE
C  >>>>>>>>>>> CANNOT BE USED WITH THE NORMALS
c              1 is the obtime from the daily records (TD3200)
C  >>>>>>>>>>> UNFORTUNATELY, DUE TO ALL THE OBTIME VALUES BEING SET
C  >>>>>>>>>>> TO 18HR BEFORE 1982, 
C  >>>>>>>>>>> ONLY RECORDS AFTER JAN 1, 1982 MAY BE USED
c              2 is the Modified Master Station History Report
c       NOTE: HISTORY FILE MUST HAVE RECORDS IN HIGHEST TO LOWEST
c              QUALITY SOURCE ORDER EACH SORTED BY TIME!!!!
        IF(OBCODE .NE. LOBCOD) THEN
c         if the first record, or the newest, use it. 
          if(nchgs .eq. 0 .or. byear .gt. byrh(nchgs) .or.
     *      (byear .eq. byrh(nchgs) .and. bmonth .gt. bmoh(nchgs)) .or.
     *      (byear .eq. byrh(nchgs) .and. bmonth .eq. bmoh(nchgs) .and.
     *      bday+1 .ge. bdayh(nchgs))) then
            IF(NCHGS .GE. imxchg) THEN
              WRITE(6,100) imxchg,NCHGS+1
  100         FORMAT('PARAMETER imxchg HAS TO BE INCREASED FROM ',I2,
     *           ' TO AT LEAST ',I2)
              stop 100
            END IF

c           DO NOT RELY ON THE TD3200 before 1982!
            if(src .eq. 1 .and. byear .lt. 1982) then
              byear = 1982
              if(byear .ge. eyear) go to 30
              bmonth = 1
              bday = 1
            endif  

            NCHGS = NCHGS + 1
            BDAYH(NCHGS) = BDAY
            BMOH(NCHGS) = BMONTH
            BYRH(NCHGS) = BYEAR
            TOBTMP(NCHGS) = OBCODE
            if(debug .gt. 0) write(6,*) nchgs, src, bdayh(nchgs), 
     *        bmoh(nchgs), byrh(nchgs), tobtmp(nchgs)
            LOBCOD = OBCODE
c           Keep tabs on sources
            if(src .eq. 0) then
c             Accept HCN or Jim Owenby's data as truth
              lhcn = nchgs
            else if(src .eq. 1) then
c             Use TD3200 after the last record from the HCN, usually
c               somwhere around 1994. In Normals there is no HCN
              ltd = nchgs
            else
c             Only use the MSHR data if it is later than both the HCN and TD
              lmshr = nchgs
            endif  
          else
c           for Normals, use ONLY the MSHR
            lmshr = nchgs
          endif
        END IF

c       set last begin and end years & source
        lbyear = byear
        leyear = eyear
        lsrc = src

   30 END DO
      goto 40
      
   35 call perror(' Error: Cannot Open/Read Hfile: ' // hfile)   
  
   40 close(10)
      if(nchgs .gt. 0) then
        write(6,'(a6,i4,200(2x,i4.4"/"i2.2"/"i2.2":"i2.2))')shfid,
     *    nchgs,(BYRH(iC),BMOH(iC),BDAYH(iC),TOBTMP(iC),ic=1,nchgs)
      else
        print *,cstn,' - no history file'
      endif  
      RETURN

C     PRINT ERROR MESSAGE AND ABORT PROGRAM
   50 call perror(' Error in open/read: ' // hfile)
      print *,'PROGRAM IS ABORTING.'
      STOP

      END

C***********************************************************************
C End of Subroutine GETOBT.                                            *
C***********************************************************************


C***********************************************************************
C                          Subroutine MSGDAT                           *
C                                                                      *
C DATE OF LAST CHANGE:  15 October 1993                                *
C                                                                      *
C MODIFIER:  David Bowman                                              *
C                                                                      *
C VERSION     MODIFICATION                                      DATE   *
C -------     ------------                                    -------- *
C   1.1       SUN Workstation Version                         10/15/93 *
C                                                                      *
C   1.0       Original                                                 *
C                                                                      *
C AUTHOR:  Claude Williams                                             *
C                                                                      *
C MODIFIERS:  Pam Hughes                                               *
C             David Bowman                                             *
C                                                                      *
C USAGE:  MSGDAT(integer STNID, integer DEBUG, integer BEGYR,          *
C                integer J, character*1 SKIP)                          *
C                                                                      *
C DESCRIPTION:  This subroutine fills in dates for missing days,       *
C               months, and/or years.                                  * 
C                                                                      *
C NOTES:  Added additional code to handle other missing date           *
C         situations. - D. Bowman:  10/15/93                           *
C                                                                      *
C RESULTS:  Fills in dates for missing days, months, and/or years.     *
C                                                                      *
C PARAMETERS:                                                          *
C                                                                      *
C   imxchg     Maximum Number of Observation Time Changes Over a       *
C              Station's Period of Record                              *
C                                                                      *
C VARIABLES:                                                           *
C                                                                      *
C   BDAYH      Begin Days Where Time of Observations Change            *
C   (imxchg)                                                           *
C                                                                      *
C   BEGYR      Begin Year of Data/Flag Search                          *
C                                                                      *
C   BMOH       Begin Months Where Time of Observations Change          *
C   (imxchg)                                                           *
C                                                                      *
C   BYRH       Begin Years Where Time of Observations Change           *
C   (imxchg)                                                           *
C                                                                      *
C   DAYSMO     Number of Days in Each Month                            *
C   (12)                                                               *
C                                                                      *
C   DEBUG      Debug Variable:       0 => Do Not Debug                 *
C                                    1 => Debug; Print Out Information *
C                                                                      *
C   J          Observation Change Number                               *
C                                                                      *
C   LASTDY     Last Day of a Particular Month:  Used to Account for    *
C              Leap Years                                              *
C                                                                      *
C   SKIP       Flag to Indicate Whether Missing Date was Filled In:    *
C                                    "N" => Missing Date Not Filled In *
C                                    "Y" => Missing Date Filled In     *
C                                                                      *
C   STNID      ID of Current HCN Station                               *
C                                                                      *
C***********************************************************************

      SUBROUTINE MSGDAT(STNID,DEBUG,BEGYR,J,SKIP)

      PARAMETER (imxchg = 200)

      CHARACTER*1 SKIP
      INTEGER BEGYR,DEBUG
      character*6 STNID

      COMMON/MDBLK1/BDAYH,BMOH,BYRH
      INTEGER BDAYH(imxchg),BMOH(imxchg),BYRH(imxchg)
      INTEGER DAYSMO(12) /31,28,31,30,31,30,31,31,30,31,30,31/

C     DEBUG PRINTOUT
      IF(DEBUG .ge. 1) WRITE(6,10) STNID,BDAYH(J),BMOH(J),BYRH(J)
   10                   FORMAT('STATION ',A6,' ENCOUNTERED MISSING ',
     *                         'DATE OF ',2(I2.2,1X),I4)

C     DETERMINE SUITABLE YEAR, MONTH, AND/OR DAY WHERE MISSING
C     YEAR IS MISSING
      IF(BYRH(J) .EQ. 9999) THEN
C       SET YEAR TO BEGIN YEAR IF FIRST TIME OF OBSERVATION
        IF(J .EQ. 1) THEN
          BYRH(J) = BEGYR

C       SET YEAR TO MIDDLE YEAR IF PREVIOUS YEAR AND NEXT YEAR ARE NOT
C       MISSING
        ELSE IF(BYRH(J-1) .NE. 9999 .AND. BYRH(J+1) .NE. 9999) THEN
          BYRH(J) = (BYRH(J-1) + BYRH(J+1) + 1) / 2
        END IF

C       CANNOT FILL IN MISSING YEAR:  PRINT TO SCREEN AND SET FLAG
        IF(BYRH(J) .EQ. 9999) THEN
          WRITE(6,20) STNID
   20     FORMAT('****************** NO ADJUSTMENTS ******************',
     *           /,'MORE THAN ONE MISSING YEAR IN A ROW.  STATION ',
     *           A6,' SKIPPED.')
          SKIP = 'Y'

C       SET MONTH AND DAY TO JANUARY 1
        ELSE
          BMOH(J) = 1
          BDAYH(J) = 1
        END IF

C     MONTH IS MISSING
      ELSE IF(BMOH(J) .EQ. 99) THEN

C       SET MONTH TO JANUARY IF FIRST TIME OF OBSERVATION CHANGE
        IF(J .EQ. 1) THEN
          BMOH(J) = 1

C       BEGIN YEAR OF PREVIOUS CHANGE SAME AS BEGIN YEAR OF NEXT CHANGE
        ELSE IF(BYRH(J-1) .EQ. BYRH(J+1)) THEN

C         SET BEGIN MONTH TO MIDDLE MONTH IF NEXT MONTH NOT MISSING
          IF(BMOH(J+1) .NE. 99) THEN
            BMOH(J) = (BMOH(J-1) + BMOH(J+1) + 1) / 2
          ELSE

C           SET BEGIN MONTH TO 1/3 MONTH IF NEXT MONTH MISSING
            BMOH(J) = ((2 * BMOH(J-1)) + 12) / 3
          END IF

C         SET BEGIN MONTH TO MIDDLE MONTH IF BEGIN YEAR OF PREVIOUS CHANGE
C         SAME AS BEGIN YEAR OF CURRENT CHANGE
        ELSE IF(BYRH(J-1) .EQ. BYRH(J)) THEN
          BMOH(J) = (BMOH(J-1) + 13) / 2

C       SET BEGIN MONTH TO JANUARY
        ELSE
          BMOH(J) = 1
        END IF

C       SET DAY TO 1
        BDAYH(J) = 1

C       DAY IS MISSING
      ELSE IF(BDAYH(J) .EQ. 99) THEN

C       DETERMINE LAST DAY OF MONTH:  ACCOUNT FOR FEBRUARY IN LEAP YEARS
        IF(BMOH(J) .EQ. 2 .AND. ((MOD(BYRH(J),4) .EQ. 0 .AND.
     *     MOD(BYRH(J),100) .NE. 0) .OR. MOD(BYRH(J),400) .EQ. 0)) THEN
          LASTDY = 29
        ELSE
          LASTDY = DAYSMO(J)
        END IF

C       SET DAY TO 1 IF FIRST TIME OF OBSERVATION CHANGE
        IF(J .EQ. 1) THEN
          BDAYH(J) = 1

C       BEGIN YEAR AND MONTH OF PREVIOUS CHANGE SAME AS BEGIN YEAR OF NEXT
C       CHANGE
        ELSE IF(BYRH(J-1) .EQ. BYRH(J+1) .AND.
     *          BMOH(J-1) .EQ. BMOH(J+1)) THEN

C         SET BEGIN DAY TO MIDDLE DAY OF MONTH IF NEXT BEGIN DAY NOT MISSING
          IF(BDAYH(J+1) .NE. 99) THEN
            BDAYH(J) = (BDAYH(J-1) + BDAYH(J+1) + 1) / 2

C         SET BEGIN DAY TO 1/3 MONTH IF NEXT DAY MISSING
          ELSE
            BDAYH(J) = (2 * BDAYH(J-1) + LASTDY) / 3
          END IF

C       SET BEGIN DAY TO MIDDLE DAY IF BEGIN YEAR AND MONTH OF PREVIOUS
C       CHANGE SAME AS BEGIN YEAR AND MONTH OF CURRENT CHANGE
        ELSE IF(BYRH(J-1) .EQ. BYRH(J) .AND.
     *          BMOH(J-1) .EQ. BMOH(J)) THEN
          BDAYH(J) = (BDAYH(J-1) + LASTDY + 1) / 2

        ELSE
C         SET BEGIN DAY TO 1
          BDAYH(J) = 1
        END IF
      END IF

      RETURN

      END

C***********************************************************************
C End of Subroutine MSGDAT.                                            *
C***********************************************************************


C***********************************************************************
C                          Subroutine OBTDIF                           *
C                                                                      *
C DATE OF LAST CHANGE:  21 October 1993                                *
C                                                                      *
C MODIFIER:  David Bowman                                              *
C                                                                      *
C VERSION     MODIFICATION                                      DATE   *
C -------     ------------                                    -------- *
C   1.1       SUN Workstation Version                         10/21/93 *
C                                                                      *
C   1.0       Original                                        02/XX/85 *
C                                                                      *
C AUTHOR:  Claude Williams                                             *
C                                                                      *
C MODIFIERS:  Pam Hughes                                               *
C             David Bowman                                             *
C                                                                      *
C USAGE:  OBTDIF(real AIDIF, real ALATZ, real ALONZ, real AOBAV(24),   *
C                real AOBMN(24), real AOBMX (24), integer IDFLG,       *
C                integer ITUNIT, integer ITZ, integer IMO,             *
C                real AMT1, real AMT2)                                 *
C                                                                      *
C DESCRIPTION:  Calculates the estimated observation time bias for any *
C               point within the contiguous USA using the algorithm    *
C               developed by  T. Karl, et. al., and published in the   *
C               "Journal of Climate and Applied Meteorology."          *
C                                                                      *
C NOTES:  If the inter-daily temperature difference (AIDIF) is         *
C         unknown, input a negative number and the value will be       *
C         estimated from the closest four stations in the data base.   *
C                                                                      *
C         ***WARNING*** The estimated value is returned through the    *
C         variable AIDIF; therefore, this variable must always be      *
C         reset in the calling routine and must never be a constant.   *
C                                                                      *
C RESULTS:  Returns the 24 hourly values for maximum, minimum, and     *
C           mean temperature.                                          *
C                                                                      *
C SUBROUTINES:  SOLAR                                                  *
C                                                                      *
C VARIABLES:                                                           *
C                                                                      *
C   AIDIF      Inter-Daily Temperature Difference (See Article.)       *
C                                                                      *
C   ALAT       Latitudes of NSTA Number of Stations Used in the        *
C   (130)      Analysis                                                *
C                                                                      *
C   ALATZ      Latitude of Station in Degrees                          *
C                                                                      *
C   ALON       Longitudes of NSTA Number of Stations Used in the       *
C   (130)      Analysis                                                *
C                                                                      *
C   ALONZ      Longitude of Station in Degrees                         *
C                                                                      *
C   AMT1       IDFLG = 0 => Mean Monthly Average Temperature for       *
C                           Previous Month in Degrees                  *
C              IDFLG = 1 => Last Daily Maximum Temperature of Previous *
C                           Month - Last Daily Maximum Temperature of  *
C                           Current Month                              *
C                                                                      *
C   AMT2       IDFLG = 0 => Mean Monthly Average Temperature for       *
C                           Subsequent Month in Degrees                *
C              IDFLG = 1 => Last Daily Minimum Temperature of Previous *
C                           Month - Last Daily Minimum Temperature of  *
C                           Current Month                              *
C                                                                      *
C   AOBAV      Estimate of Mean Temperature Observation Time Bias in   *
C   (24)       Degrees                                                 *
C                                                                      *
C   AOBMN      Estimate of Minimum Temperature Observation Time Bias   *
C   (24)       in Degrees                                              *
C                                                                      *
C   AOBMX      Estimate of Maximum Temperature Observation Time Bias   *
C   (24)       in Degrees                                              *
C                                                                      *
C   BASAV      Correction Factor for Monthly Mean Temperature Derived  *
C   (12,-12:11)  From Station Solar Elevation and Station Sunrise      *
C                                                                      *
C   BASMN      Correction Factor for Monthly Minimum Temperature       *
C   (12,-12:11)  Derived From Station Solar Elevation and Station      *
C                Sunrise                                               *
C                                                                      *
C   BASMX      Correction Factor for Monthly Maximum Temperature       *
C   (12,-12:11)  Derived From Station Solar Elevation and Station      *
C                Sunrise                                               *
C                                                                      *
C   CFAV       Correction Factor for Monthly Mean Temperature          *
C   (12)                                                               *
C                                                                      *
C   CFMN       Correction Factor for Monthly Minimum Temperature       *
C   (12)                                                               *
C                                                                      *
C   CFMX       Correction Factor for Monthly Maximum Temperature       *
C   (12)                                                               *
C                                                                      *
C   DAYS       Number of Days in Each Month                            *
C   (12)                                                               *
C                                                                      *
C   DRIFTA     Effects of Month End Temperatures on the Monthly Mean   *
C              Temperature                                             *
C                                                                      *
C   DRIFTN     Effects of Month End Temperatures on the Monthly        *
C              Minimum Temperature                                     *
C                                                                      *
C   DRIFTX     Effects of Month End Temperatures on the Monthly        *
C              Maximum Temperature                                     *
C                                                                      *
C   DTDIF      Inter-Daily Temperature Differences for Stations Used   *
C   (12,130)   in the Analysis                                         *
C                                                                      *
C   ELEV       Mid-Month Solar Elevations for Each Hour of the Day in  *
C   (24)       Degrees                                                 *
C                                                                      *
C   ELEVHI     Highest Mid-Month Solar Elevation for the Day in        *
C              Degrees                                                 *
C                                                                      *
C   I          Loop Counter                                            *
C                                                                      *
C   IBC        Loop Counter; Column in Base Curve for Station Solar    *
C              Elevation                                               *
C                                                                      *
C   IDFLG      Drift Algorithm:                                        *
C                         0 => Estimate the Drift Using Mean Monthly   *
C                              Temperatures                            *
C                         1 => Calculate Exact Drift Using Differences *
C                              Between First and Last Days             *
C                                                                      *
C   IHR        Loop Counter                                            *
C                                                                      *
C   IMO        Month                                                   *
C                                                                      *
C   ISR        Base Curve Row According to Station Sunrise             *
C                                                                      *
C   ITUNIT     Temperature Units:                      0 => Centigrade *
C                                                      1 => Fahrenheit *
C                                                                      *
C   ITZ        Not Used by this Subroutine                             *
C                                                                      *
C   J          Loop Counter                                            *
C                                                                      *
C   M          Loop Counter                                            *
C                                                                      *
C   NSTA       Number of Stations Used in the Analysis                 *
C                                                                      *
C   RATIO      Fractional Part of a Day; X/24ths of a day; Used in     *
C              Drift Calculation                                       *
C                                                                      *
C   RELEV      Base Curve Values for Station Solar Elevation           *
C   (12)                                                               *
C                                                                      *
C   RHO        Hour Range for December                                 *
C   (130)                                                              *
C                                                                      *
C   SR         Mean Monthly Sunrise Time in Local Standard Time        *
C                                                                      *
C   SS         Not Used by this Subroutine                             *
C                                                                      *
C   TAMT1      Same as AMT1 for Manipulation Purposes                  *
C                                                                      *
C   TAMT2      Same as AMT2 for Manipulation Purposes                  *
C                                                                      *
C***********************************************************************

      SUBROUTINE OBTDIF(AIDIF,ALATZ,ALONZ,AOBAV,AOBMN,AOBMX,IDFLG,
     *                  ITUNIT,ITZ,IMO,AMT1,AMT2,debug)

      DIMENSION AOBAV(24),AOBMN(24),AOBMX(24)

      COMMON /BIAS/ALAT,ALON,BASAV,BASMN,BASMX,CFAV,CFMN,CFMX,DTDIF,
     *             NSTA,RELEV,RHO
      real ALAT(130),ALON(130)
      real BASAV(12,-12:11),BASMN(12,-12:11),BASMX(12,-12:11)
      real CFAV(12),CFMN(12),CFMX(12),RELEV(12),RHO(130)
      real DTDIF(12,130)

      real DAYS(12) /31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,
     *                   31./
     
      integer debug
     
      DIMENSION ELEV(24)


C     CONVERT FAHRENHEIT TEMPERATURE TO CENTIGRADE

      IF(ITUNIT .EQ. 1) THEN
        TAMT1 = (AMT1 - 32.) / 1.8
        TAMT2 = (AMT2 - 32.) / 1.8

C     LEAVE TEMPERATURE IN CENTIGRADE

      ELSE
        TAMT1 = AMT1
        TAMT2 = AMT2
      END IF

C     GET MID-MONTH SOLAR ELEVATIONS AND SUNRISE/SUNSET TIMES

      CALL SOLAR(ALATZ,ALONZ,ELEV,IMO,ITZ,SR,SS)

C     FIND HIGHEST MID-MONTH SOLAR ELEVATION

      ELEVHI = 0.

      DO 10 M = 1,24
        IF(ELEV(M) .GT. ELEVHI) ELEVHI = ELEV(M)
   10 CONTINUE

C     FIND CORRECT COLUMN IN BASE CURVE FOR STATION SOLAR ELEVATION

      DO 20 IBC = 1,11
        IF(RELEV(IBC) .GT. ELEVHI) GO TO 30
   20 CONTINUE

      IBC = 12

C     SET BASE CURVE ROW ACCORDING TO STATION SUNRISE

   30 ISR = -1 * NINT(SR) + 1

C     INTERPOLATE FOR INTER-DAILY TEMPERATURE DIFFERENCE

      IF(AIDIF .LT. 0.000001) AIDIF = FNDIFF(ALATZ,ALONZ,IMO)

C     TAKE HOUR RANGE INTO ACCOUNT FOR DECEMBER

      IF(IMO .EQ. 12) AIDIF = AIDIF * FNRNG(ALATZ,ALONZ)

C     CALCULATE DRIFT USING MONTHLY MEAN TEMPERATURES

      IF(IDFLG .EQ. 0) THEN
        DRIFTA = (TAMT2 - TAMT1) / (2. * DAYS(IMO))
        DRIFTN = DRIFTA
        DRIFTX = DRIFTA

C     CALCULATE DRIFT USING DIFFERENCES BETWEEN FIRST AND LAST DAYS

      ELSE
        DRIFTN = TAMT2 / DAYS(IMO)
        DRIFTX = TAMT1 / DAYS(IMO)
        DRIFTA = (DRIFTN + DRIFTX) / 2.
      END IF
      
      if(debug .ge. 2) then
        write(6,2000) imo, aidif, DRIFTA, DRIFTN, DRIFTX
 2000   format(' imo: ', i3,
     *    ' OBTDIF: aidif, DRIFTA, DRIFTN, DRIFTX:',4f6.2)
        write(6,1500) isr, sr, elevhi, ibc, relev(ibc)
 1500   format(' isr, sr, elevhi, ibc, relev(ibc)',
     *    i4,2f6.2,i4,f6.2)       
      endif

C   CALCULATE ESTIMATES OF MEAN, MINIMUM, AND MAXIMUM TEMPERATURE
C   OBSERVATION TIME BIAS

      DO 40 IHR = 1,23
        RATIO = (FLOAT(IHR) - 24.) / 24.
        AOBAV(IHR) = CFAV(IMO) * BASAV(IBC,ISR) * AIDIF + DRIFTA * RATIO
        AOBMN(IHR) = CFMN(IMO) * BASMN(IBC,ISR) * AIDIF + DRIFTN * RATIO
        AOBMX(IHR) = CFMX(IMO) * BASMX(IBC,ISR) * AIDIF + DRIFTX * RATIO
        
        if(debug .ge. 2) then
          write(6,1000) ihr, ratio,
     *     CFAV(IMO),BASAV(IBC,ISR),CFAV(IMO)*BASAV(IBC,ISR)*AIDIF,
     *     DRIFTA * RATIO, AOBAV(IHR)
c     *     ,CFMN(IMO),BASMN(IBC,ISR),CFMN(IMO)*BASMN(IBC,ISR)*AIDIF,
c     *     DRIFTN * RATIO, AOBMN(IHR),
c     *     CFMX(IMO),BASMX(IBC,ISR),CFMX(IMO)*BASMX(IBC,ISR)*AIDIF,
c     *     DRIFTX * RATIO, AOBMX(IHR)
 1000     format( 'OBTDIF: ihr,ratio:',i3,f6.2,    
     *      ' cf,bas,cf*bas*aidif,drift*ratio,aob',
     *      ' avg: ', 5f6.2)
c     *      ,' max:', 5f6.2,' min:', 5f6.2)
        endif

C     CONVERT CELSIUS TEMPERATURE TO FAHRENHEIT

        IF(ITUNIT .EQ. 1) THEN
          AOBAV(IHR) = AOBAV(IHR) * 1.8
          AOBMN(IHR) = AOBMN(IHR) * 1.8
          AOBMX(IHR) = AOBMX(IHR) * 1.8
        END IF

C     NEXT BASE CURVE ROW

        ISR = ISR + 1

C     RESET BASE CURVE ROW IF AT ROW 12

        IF(ISR .EQ. 12) ISR = -12
   40 CONTINUE

      AOBAV(24) = 0.
      AOBMN(24) = 0.
      AOBMX(24) = 0.

      RETURN

      END

C***********************************************************************
C End of Subroutine OBTDIF.                                            *
C***********************************************************************


C***********************************************************************
C                           Subroutine QUAD                            *
C                                                                      *
C DATE OF LAST CHANGE:  19 October 1993                                *
C                                                                      *
C MODIFIER:  David Bowman                                              *
C                                                                      *
C VERSION     MODIFICATION                                      DATE   *
C -------     ------------                                    -------- *
C   1.1       SUN Workstation Version                         10/19/93 *
C                                                                      *
C   1.0       Original                                        02/XX/85 *
C                                                                      *
C AUTHOR:  Claude Williams                                             *
C                                                                      *
C MODIFIERS:  Pam Hughes                                               *
C             David Bowman                                             *
C                                                                      *
C USAGE:  QUAD(real ALATZ, real ALONZ, real DZ(4), integer IZ(4))      *
C                                                                      *
C DESCRIPTION:  Determines closest stations to a given station within  *
C               each quadrant.                                         *
C                                                                      *
C NOTES:  None.                                                        *
C                                                                      *
C RESULTS:  Returns closest stations to and their distances from a     *
C           given station within each quadrant.                        *
C                                                                      *
C VARIABLES:                                                           *
C                                                                      *
C   ALAT       Latitudes of NSTA Number of Stations Used in the        *
C   (130)      Analysis                                                *
C                                                                      *
C   ALATZ      Latitude of Station in Degrees                          *
C                                                                      *
C   ALON       Longitudes of NSTA Number of Stations Used in the       *
C   (130)      Analysis                                                *
C                                                                      *
C   ALONZ      Longitude of Station in Degrees                         *
C                                                                      *
C   BASAV      Not Used by this Subroutine                             *
C   (12,-12:11)                                                        *
C                                                                      *
C   BASMN      Not Used by this Subroutine                             *
C   (12,-12:11)                                                        *
C                                                                      *
C   BASMX      Not Used by this Subroutine                             *
C   (12,-12:11)                                                        *
C                                                                      *
C   CFAV       Not Used by this Subroutine                             *
C   (12)                                                               *
C                                                                      *
C   CFMN       Not Used by this Subroutine                             *
C   (12)                                                               *
C                                                                      *
C   CFMX       Not Used by this Subroutine                             *
C   (12)                                                               *
C                                                                      *
C   DIST       Distance in Kilometers Between Station and One of the   *
C              Stations Used in the Analysis                           *
C                                                                      *
C   DLAT       Intermediate Calculation for DIST                       *
C                                                                      *
C   DLON       Intermediate Calculation for DIST                       *
C                                                                      *
C   DTDIF      Not Used by this Subroutine                             *
C   (12,130)                                                           *
C                                                                      *
C   DZ         Distances for the Closest Station Within Each Quadrant  *
C   (4)                                                                *
C                                                                      *
C   I          Loop Counter                                            *
C                                                                      *
C   IZ         Index Numbers for the Closest Station Within Each       *
C   (4)        Quadrant                                                *
C                                                                      *
C   NSTA       Number of Stations Used in the Analysis                 *
C                                                                      *
C   OLAT       Intermediate Calculation for DIST                       *
C                                                                      *
C   RELEV      Not Used by this Subroutine                             *
C   (12)                                                               *
C                                                                      *
C   RHO        Not Used by this Subroutine                             *
C   (130)                                                              *
C                                                                      *
C***********************************************************************

      SUBROUTINE QUAD(ALATZ,ALONZ,DZ,IZ)

      DIMENSION DZ(4),IZ(4)

      COMMON /BIAS/ALAT,ALON,BASAV,BASMN,BASMX,CFAV,CFMN,CFMX,DTDIF,
     *             NSTA,RELEV,RHO
      DIMENSION ALAT(130),ALON(130)
      DIMENSION BASAV(12,-12:11),BASMN(12,-12:11),BASMX(12,-12:11)
      DIMENSION CFAV(12),CFMN(12),CFMX(12),RELEV(12),RHO(130)
      DIMENSION DTDIF(12,130)

C     DETERMINE THE CLOSEST STATIONS WITHIN EACH QUADRANT

      DO 10 I = 1,NSTA

C     CALCULATE DISTANCE FROM EACH STATION TO DESIRED LATITUDE AND
C     LONGITUDE

        DLAT = (ALAT(I) - ALATZ) / 57.29578
        DLON = (ALON(I) - ALONZ) / 57.29578
        OLAT = ((ALAT(I) + ALATZ) / 2.) / 57.29578
        DIST = ABS(6371. * ACOS(COS(DLAT) * COS(ABS(DLON) * COS(OLAT))))

C     DO NOT USE IF DISTANCE GREATER THAN 500 KILOMETERS

        IF(DIST .GE. 500.000001) GO TO 10

C     IF DESIRED STATION IS WITHIN 25 KILOMETERS OF A STATION USED IN
C     THE ANALYSIS, SET FIRST QUADRANT TO DATA BASE VALUE AND RETURN

        IF(DIST .LT. 25.000001) THEN
          DZ(1) = 0.
          IZ(1) = I
          GO TO 20

C     FIND NEIGHBORS IN FIRST OR SECOND QUADRANT

        ELSE IF(DLAT .GE. 0.000001) THEN

C     FIND NEIGHBOR IN FIRST QUADRANT

          IF(DLON .GE. 0.000001) THEN

C     SAVE CLOSEST NEIGHBOR

            IF(DIST .LT. DZ(1)) THEN
              DZ(1) = DIST
              IZ(1) = I
            END IF

C     FIND NEIGHBOR IN SECOND QUADRANT

          ELSE

C     SAVE CLOSEST NEIGHBOR

            IF(DIST .LT. DZ(2)) THEN
              DZ(2) = DIST
              IZ(2) = I
            END IF
          END IF

C     FIND NEIGHBORS IN THIRD OR FOURTH QUADRANT

        ELSE

C     FIND NEIGHBOR IN FOURTH QUADRANT

          IF(DLON .GE. 0.000001) THEN

C     SAVE CLOSEST NEIGHBOR

            IF(DIST .LT. DZ(4)) THEN
              DZ(4) = DIST
              IZ(4) = I
            END IF

C     FIND NEIGHBOR IN THIRD QUADRANT

          ELSE

C     SAVE CLOSEST NEIGHBOR

            IF(DIST .LT. DZ(3)) THEN
              DZ(3) = DIST
              IZ(3) = I
            END IF
          END IF
        END IF

   10 CONTINUE

   20 RETURN

      END

C***********************************************************************
C End of Subroutine QUAD.                                              *
C***********************************************************************


C***********************************************************************
C                           Subroutine SOLAR                           *
C                                                                      *
C DATE OF LAST CHANGE:  19 October 1993                                *
C                                                                      *
C MODIFIER:  David Bowman                                              *
C                                                                      *
C VERSION     MODIFICATION                                      DATE   *
C -------     ------------                                    -------- *
C   1.1       SUN Workstation Version                         10/19/93 *
C                                                                      *
C   1.0       Original                                        02/XX/85 *
C                                                                      *
C AUTHOR:  Claude Williams                                             *
C                                                                      *
C MODIFIERS:  Pam Hughes                                               *
C             David Bowman                                             *
C                                                                      *
C USAGE:  SOLAR(real ALATZ, real ALONZ, real ELEV(24), integer IMO,    *
C               integer ITZ, real SR, real SS)                         *
C                                                                      *
C DESCRIPTION:  Calculates mid-month solar elevations and sunrise/     *
C               sunset times.                                          *
C                                                                      *
C NOTES:  None.                                                        *
C                                                                      *
C RESULTS:  Returns mid-month solar elevations and sunrise/sunset      *
C           times.                                                     *
C                                                                      *
C VARIABLES:                                                           *
C                                                                      *
C   ALATZ      Latitude of Station in Degrees                          *
C                                                                      *
C   ALONZ      Longitude of Station in Degrees                         *
C                                                                      *
C   CALAT      Latitude of Station in Radians                          *
C                                                                      *
C   CGAMMA     Intermediate Calculation for Declination in Radians     *
C                                                                      *
C   CPHI       Angular Fraction of Year in Radians                     *
C                                                                      *
C   CRH        Solar Hour Angle in Degrees                             *
C                                                                      *
C   ELEV       Mid-Month Solar Elevations for Each Hour of the Day in  *
C   (24)       Degrees                                                 *
C                                                                      *
C   GAMMA      Intermediate Calculation for Declination in Degrees     *
C                                                                      *
C   GMT        Mean Greenwich Time                                     *
C                                                                      *
C   H          Intermediate Calculation for Mid-Month Solar Elevations *
C              for Each Hour of the Day                                *
C                                                                      *
C   IM         Loop Counter                                            *
C                                                                      *
C   IMO        Month                                                   *
C                                                                      *
C   ITZ        Time Zone of Station                                    *
C                                                                      *
C   PHI        Angular Fraction of Year in Degrees                     *
C                                                                      *
C   RD         Declination                                             *
C                                                                      *
C   RH         Solar Hour Angle in Radians                             *
C                                                                      *
C   RM         Solar Noon                                              *
C                                                                      *
C   RMO        Number of Days from Beginning of Year to First of Month *
C   (12)                                                               *
C                                                                      *
C   SR         Mean Monthly Sunrise Time in Local Standard Time        *
C                                                                      *
C   SS         Mean Monthly Sunset Time in Local Standard Time         *
C                                                                      *
C   TSR        Mean Monthly Sunrise Time in Mean Greenwich Time        *
C                                                                      *
C   TSS        Mean Monthly Sunset Time in Mean Greenwich Time         *
C                                                                      *
C   X          Intermediate Calculation for Mid-Month Solar Elevations *
C              for Each Hour of the Day                                *
C                                                                      *
C   Y          Intermediate Calculation for Solar Hour Angle           *
C                                                                      *
C   Z          Intermediate Calculation for Solar Noon                 *
C                                                                      *
C   ZONE       Time of Each US Time Zone With Respect to Mean          *
C   (4)        Greenwich Time:           (1) => Eastern Standard Time  *
C                                        (2) => Central Standard Time  *
C                                        (3) => Mountain Standard Time *
C                                        (4) => Pacific Standard Time  *
C                                                                      *
C***********************************************************************

      SUBROUTINE SOLAR(ALATZ,ALONZ,ELEV,IMO,ITZ,SR,SS)

      real ELEV(24)

      real RMO(12) /0.,31.,59.,90.,120.,151.,181.,212.,243.,273.,
     *                  304.,334./
      real ZONE(4) /5.,6.,7.,8./

C     CANNOT HAVE LATITUDE OF EXACTLY 90 DEGREES

      IF(ALATZ .EQ. 90.) ALATZ = 89.99
      IF(ALATZ .EQ. -90.) ALATZ = -89.99

C     CONVERT LATITUDE FROM DEGREES TO RADIANS

      CALAT = ALATZ / 57.29578

C     CALCULATE ANGULAR FRACTION OF YEAR

      PHI = 360. * (RMO(IMO) + 15.) / 365.242

C     CONVERT ANGULAR FRACTION OF YEAR FROM DEGREES TO RADIANS

      CPHI = PHI / 57.29578

C     CALCULATE TRUE SOLAR NOON

      Z = 2. * CPHI
      RM = 12. + 0.123570 * SIN(CPHI) - 0.004289 * COS(CPHI) +
     *     0.153809 * SIN(Z) + 0.060783 * COS(Z)

C     CALCULATE GAMMA

      GAMMA = 279.9348 + 1.914827 * SIN(CPHI) - 0.079525 * COS(CPHI) +
     *        0.019938 * SIN(Z) - 0.001620 * COS(Z) + PHI

C     CONVERT GAMMA FROM DEGREES TO RADIANS

      CGAMMA = GAMMA / 57.29578

C     CALCULATE DECLINATION

      RD = ASIN(SIN(0.409172) * SIN(CGAMMA))

C     CALCULATE SOLAR HOUR ANGLE.  FIRST SINE ARGUMENT:
C     -0.014544 FOR SUNSET/SUNRISE; -0.017453 FOR CIVIL TWILIGHT

      Y = (SIN(-0.014544) - SIN(CALAT) * SIN(RD)) /
     *    (COS(CALAT) * COS(RD))

C     INSERT DUMMY SUNRISE/SUNSET TIMES IF SUN NEVER RISES NOR SETS

      IF(Y .LE. -1.000001 .OR. Y .GE. 1.000001) THEN
        TSR = 0.
        TSS = 24.

C     DETERMINE SUNRISE/SUNSET TIMES

      ELSE
        RH = ACOS(Y)

C     CONVERT SOLAR ANGLE HOUR FROM RADIANS TO DEGREES

        CRH = RH * 57.29578

C     CALCULATE MEAN SUNRISE/SUNSET TIMES FOR MONTH

        TSR = (ABS(ALONZ) / 15.) + RM - (CRH / 15.)
        TSS = (ABS(ALONZ) / 15.) + RM + (CRH / 15.)
      END IF

C     CONVERSION FOR LOCAL STANDARD TIME ZONES

      SR = TSR - ZONE(ITZ)
      SS = TSS - ZONE(ITZ)

C     ASSURE HOURS ARE BETWEEN 0 AND 24

      IF(SR .LE. -0.000001) THEN
        SR = SR + 24.
      ELSE IF(SR .GE. 24.000001) THEN
        SR = SR - 24.
      END IF

      IF(SS .LE. -0.000001) THEN
        SS = SS + 24.
      ELSE IF(SS .GE. 24.000001) THEN
        SS = SS - 24.
      END IF

C     CONVERT MID-MONTH SOLAR ELEVATION FOR EACH HOUR OF THE DAY TO
C     INTEGER

      DO 10 IM = 0,23

C     CONVERT LOCAL STANDARD TIME TO MEAN GREENWICH TIME

        GMT = IM + ZONE(ITZ) + 0.5
        IF(GMT .GE. 24.000001) GMT = GMT - 24.

C     CALCULATE HIGHEST MID-MONTH SOLAR ELEVATION

        H = (15. * (GMT - RM) - ABS(ALONZ)) / 57.29578
        X = SIN(CALAT) * SIN(RD) + COS(CALAT) * COS(RD) * COS(H)
        ELEV(IM+1) = ASIN(X) * 57.29578
   10 CONTINUE

      RETURN

      END

C***********************************************************************
C End of Subroutine SOLAR.                                             *
C***********************************************************************


C***********************************************************************
C                          Subroutine TOBCHG                           *
C                                                                      *
C DATE OF LAST CHANGE:  21 October 1993                                *
C                                                                      *
C MODIFIER:  David Bowman                                              *
C                                                                      *
C VERSION     MODIFICATION                                      DATE   *
C -------     ------------                                    -------- *
C   1.1       SUN Workstation Version                         10/21/93 *
C                                                                      *
C   1.0       Original                                                 *
C                                                                      *
C AUTHOR:  Claude Williams                                             *
C                                                                      *
C MODIFIERS:  Pam Hughes                                               *
C             David Bowman                                             *
C                                                                      *
C USAGE:  TOBCHG(integer M, integer TOB)                               *
C                                                                      *
C DESCRIPTION:  Changes "RS", "SR", and "SS" times of observation into *
C               codes from 05 - 08 or 17 - 20.                         *
C                                                                      *
C NOTES:  None.                                                        *
C                                                                      *
C RESULTS:  Returns time of observation from 05 - 08 or 17 - 20.       *
C                                                                      *
C   26 = Rotating Schedule (RS)                                        *
C        SR for Months of April Through October                        *
C        SS for Months of November Through March                       *
C                                                                      *
C   27 = Sunrise (SR)                                                  *
C        SR = 0800 LST and TOB = 08 for Months November, December, and *
C                                       January                        *
C        SR = 0700 LST AND TOB = 07 for Months February, March, and    *
C                                       October                        *
C        SR = 0600 LST AND TOB = 06 for Months April, August, and      *
C                                       September                      *
C        SR = 0500 LST AND TOB = 05 for Months May, June, and July     *
C                                                                      *
C   28 = Sunset (SS)                                                   *
C        SS = 1700 LST and TOB = 17 for Months November, December, and *
C                                       January                        *
C        SS = 1800 LST AND TOB = 18 for Months February, March, and    *
C                                       October                        *
C        SS = 1900 LST AND TOB = 19 for Months April, August, and      *
C                                       September                      *
C        SS = 2000 LST AND TOB = 20 for Months May, June, and July     *
C                                                                      *
C VARIABLES:                                                           *
C                                                                      *
C   M          Month of Year                                           *
C                                                                      *
C   TOB        Time of Observation Code                                *
C                                                                      *
C***********************************************************************

      SUBROUTINE TOBCHG(M,TOB)

      INTEGER TOB

C     TIME OF OBSERVATION IS "RS"

      IF(TOB .EQ. 26) THEN
        IF(M .GE. 4 .AND. M .LE. 10) THEN
          TOB = 27
        ELSE
          TOB = 28
        END IF
      END IF

C     TIME OF OBSERVATION IS "SR"

      IF(TOB .EQ. 27) THEN
        IF(M .EQ. 11 .OR. M .EQ. 12 .OR. M .EQ. 1) THEN
          TOB = 8
        ELSE IF(M .EQ. 2 .OR. M .EQ. 3 .OR. M .EQ. 10) THEN
          TOB = 7
        ELSE IF(M .EQ. 4 .OR. M .EQ. 8 .OR. M .EQ. 9) THEN
          TOB = 6
        ELSE
          TOB = 5
        END IF

C     TIME OF OBSERVATION IS "SS"

      ELSE IF(TOB .EQ. 28) THEN
        IF(M .EQ. 11 .OR. M .EQ. 12 .OR. M .EQ. 1) THEN
          TOB = 17
        ELSE IF(M .EQ. 2 .OR. M .EQ. 3 .OR. M .EQ. 10) THEN
          TOB = 18
        ELSE IF(M .EQ. 4 .OR. M .EQ. 8 .OR. M .EQ. 9) THEN
          TOB = 19
        ELSE
          TOB = 20
        END IF

      END IF

      RETURN

      END

C***********************************************************************
C End of Subroutine TOBCHG.                                            *
C***********************************************************************

      BLOCK DATA
      
      COMMON /BIAS/ALAT,ALON,BASAV,BASMN,BASMX,CFAV,CFMN,CFMX,DTDIF,
     *             NSTA,RELEV,RHO

      real ALAT(130),ALON(130)
      real BASAV(12,-12:11),BASMN(12,-12:11),BASMX(12,-12:11)
      real CFAV(12),CFMN(12),CFMX(12),RELEV(12),RHO(130)
      real DTDIF(12,130)

       DATA ALAT/40.70,41.93,41.78,42.90,44.48,48.57,37.70,36.08,33.93,
     *          33.43,38.88,36.12,43.12,44.38,33.65,34.90,40.15,36.77,
     *          32.13,29.18,29.65,25.82,35.87,33.95,33.57,35.18,41.80,
     *          47.48,47.45,43.42,35.10,33.23,35.27,30.38,39.90,40.50,
     *          29.98,25.90,35.05,32.47,30.30,39.50,43.57,35.40,38.52,
     *          43.12,38.05,37.77,42.42,46.47,39.12,46.43,40.77,47.62,
     *          45.60,27.97,38.75,35.23,37.50,43.20,32.12,35.33,46.60,
     *          39.07,44.47,43.87,42.92,44.77,32.43,39.77,38.07,46.77,
     *          41.13,43.65,31.80,35.05,32.90,35.43,35.82,47.00,45.00,
     *          42.00,40.50,39.00,38.00,36.50,35.50,34.00,35.50,33.50,
     *          32.50,28.50,28.50,30.00,30.50,39.00,40.00,45.00,46.00,
     *          45.00,44.50,44.00,32.70,36.08,36.88,37.32,35.03,30.42,
     *          32.30,37.23,40.22,41.73,40.00,41.00,43.13,42.08,46.90,
     *          41.45,41.53,42.38,39.37,35.42,34.65,32.73,41.15,45.80,
     *          45.68,42.37,46.57,38.03/

      DATA ALON/ -74.17, -72.68, -87.75, -85.67, -88.13, -93.38,-112.15,
     *          -115.17,-118.38,-112.02, -79.85, -86.68, -77.67, -98.22,
     *          -101.83,-120.45,-122.25,-119.72, -81.20, -81.05, -95.28,
     *           -80.28, -78.78, -83.32, -86.75,-103.60,-107.20,-111.37,
     *          -122.30,-124.25,-108.80,-107.27, -75.55, -84.37, -84.22,
     *           -80.22, -90.25, -97.43, -89.98, -93.82, -97.70,-119.78,
     *          -116.22, -97.60,-121.50, -76.12, -87.53, -99.97, -83.02,
     *           -84.37,-108.53,-105.87,-111.97,-117.52,-122.60, -82.53,
     *           -90.38,-101.70, -77.33, -71.50,-110.93, -94.37,-112.00,
     *           -95.63, -73.15, -91.25,-112.60,-106.97, -99.68,-104.88,
     *          -117.08,-100.75,-100.68, -70.32,-106.40,-106.62, -80.03,
     *           -82.55, -83.98,-123.50,-123.50,-124.00,-124.00,-123.50,
     *          -122.00,-121.50,-120.50,-117.00,-116.50,-115.00,-113.50,
     *           -99.50, -97.00, -92.50, -87.50, -75.50, -74.50, -68.00,
     *           -69.00, -70.50, -84.00, -86.00, -83.65, -79.95, -76.20,
     *           -79.97, -85.20, -81.65, -86.40, -93.38, -76.85, -71.43,
     *           -82.88, -85.22, -89.33, -80.18, -96.80, -90.52, -93.65,
     *           -96.37,-101.70,-119.05,-112.43,-117.17,-104.82,-108.53,
     *          -118.85,-122.87,-120.53, -84.60/

      DATA (BASAV(I,-12),I=1,12)/ 0.40, 0.32, 0.35, 0.36, 0.38, 0.43,
     *                            0.47, 0.54, 0.49, 0.51, 0.54, 0.52/
      DATA (BASAV(I,-11),I=1,12)/ 0.31, 0.25, 0.27, 0.28, 0.28, 0.31,
     *                            0.34, 0.39, 0.37, 0.39, 0.43, 0.41/
      DATA (BASAV(I,-10),I=1,12)/ 0.22, 0.18, 0.19, 0.21, 0.21, 0.23,
     *                            0.25, 0.28, 0.26, 0.27, 0.31, 0.32/
      DATA (BASAV(I, -9),I=1,12)/ 0.14, 0.11, 0.12, 0.14, 0.15, 0.16,
     *                            0.19, 0.20, 0.18, 0.19, 0.22, 0.23/
      DATA (BASAV(I, -8),I=1,12)/ 0.06, 0.04, 0.05, 0.07, 0.09, 0.10,
     *                            0.13, 0.14, 0.13, 0.14, 0.15, 0.17/
      DATA (BASAV(I, -7),I=1,12)/-0.03,-0.02,-0.02, 0.00, 0.02, 0.04,
     *                            0.07, 0.09, 0.09, 0.09, 0.10, 0.13/
      DATA (BASAV(I, -6),I=1,12)/-0.10,-0.09,-0.09,-0.07,-0.05,-0.03,
     *                            0.00, 0.03, 0.04, 0.05, 0.06, 0.08/
      DATA (BASAV(I, -5),I=1,12)/-0.17,-0.15,-0.16,-0.14,-0.12,-0.10,
     *                           -0.07,-0.03,-0.01, 0.00, 0.01, 0.02/
      DATA (BASAV(I, -4),I=1,12)/-0.25,-0.22,-0.23,-0.21,-0.19,-0.17,
     *                           -0.15,-0.11,-0.07,-0.06,-0.05,-0.04/
      DATA (BASAV(I, -3),I=1,12)/-0.32,-0.28,-0.29,-0.28,-0.26,-0.24,
     *                           -0.22,-0.18,-0.13,-0.13,-0.12,-0.12/
      DATA (BASAV(I, -2),I=1,12)/-0.38,-0.33,-0.35,-0.34,-0.33,-0.31,
     *                           -0.29,-0.26,-0.21,-0.20,-0.20,-0.21/
      DATA (BASAV(I, -1),I=1,12)/-0.43,-0.37,-0.39,-0.39,-0.39,-0.37,
     *                           -0.36,-0.33,-0.28,-0.27,-0.28,-0.30/
      DATA (BASAV(I,  0),I=1,12)/-0.45,-0.38,-0.41,-0.41,-0.43,-0.42,
     *                           -0.41,-0.39,-0.34,-0.33,-0.35,-0.38/
      DATA (BASAV(I,  1),I=1,12)/-0.30,-0.19,-0.23,-0.24,-0.26,-0.26,
     *                           -0.25,-0.26,-0.22,-0.22,-0.25,-0.28/
      DATA (BASAV(I,  2),I=1,12)/ 0.01, 0.10, 0.05, 0.04, 0.02, 0.02,
     *                            0.01, 0.01, 0.02, 0.02,-0.01,-0.01/
      DATA (BASAV(I,  3),I=1,12)/ 0.27, 0.28, 0.25, 0.23, 0.17, 0.17,
     *                            0.15, 0.13, 0.12, 0.12, 0.11, 0.13/
      DATA (BASAV(I,  4),I=1,12)/ 0.51, 0.45, 0.42, 0.39, 0.30, 0.27,
     *                            0.24, 0.20, 0.16, 0.16, 0.15, 0.17/
      DATA (BASAV(I,  5),I=1,12)/ 0.73, 0.61, 0.61, 0.55, 0.44, 0.39,
     *                            0.34, 0.28, 0.21, 0.21, 0.20, 0.19/
      DATA (BASAV(I,  6),I=1,12)/ 0.91, 0.77, 0.78, 0.72, 0.60, 0.54,
     *                            0.46, 0.39, 0.28, 0.28, 0.26, 0.23/
      DATA (BASAV(I,  7),I=1,12)/ 1.01, 0.88, 0.89, 0.84, 0.74, 0.69,
     *                            0.60, 0.52, 0.39, 0.37, 0.35, 0.31/
      DATA (BASAV(I,  8),I=1,12)/ 1.00, 0.87, 0.90, 0.89, 0.82, 0.80,
     *                            0.72, 0.64, 0.49, 0.48, 0.45, 0.41/
      DATA (BASAV(I,  9),I=1,12)/ 0.85, 0.75, 0.80, 0.81, 0.80, 0.82,
     *                            0.77, 0.72, 0.59, 0.57, 0.56, 0.51/
      DATA (BASAV(I, 10),I=1,12)/ 0.65, 0.56, 0.61, 0.65, 0.69, 0.75,
     *                            0.74, 0.73, 0.62, 0.62, 0.62, 0.59/
      DATA (BASAV(I, 11),I=1,12)/ 0.51, 0.42, 0.45, 0.49, 0.52, 0.60,
     *                            0.63, 0.66, 0.59, 0.59, 0.61, 0.59/

      DATA (BASMN(I,-12),I=1,12)/ 0.51, 0.43, 0.46, 0.47, 0.43, 0.41,
     *                            0.41, 0.38, 0.31, 0.31, 0.31, 0.35/
      DATA (BASMN(I,-11),I=1,12)/ 0.41, 0.35, 0.37, 0.39, 0.38, 0.37,
     *                            0.38, 0.36, 0.30, 0.30, 0.30, 0.34/
      DATA (BASMN(I,-10),I=1,12)/ 0.30, 0.26, 0.28, 0.31, 0.31, 0.31,
     *                            0.34, 0.33, 0.28, 0.28, 0.28, 0.32/
      DATA (BASMN(I, -9),I=1,12)/ 0.19, 0.17, 0.19, 0.21, 0.23, 0.24,
     *                            0.28, 0.28, 0.24, 0.25, 0.26, 0.30/
      DATA (BASMN(I, -8),I=1,12)/ 0.08, 0.07, 0.08, 0.11, 0.14, 0.16,
     *                            0.20, 0.22, 0.20, 0.21, 0.22, 0.26/
      DATA (BASMN(I, -7),I=1,12)/-0.04,-0.04,-0.03, 0.00, 0.03, 0.06,
     *                            0.11, 0.14, 0.14, 0.15, 0.16, 0.21/
      DATA (BASMN(I, -6),I=1,12)/-0.15,-0.14,-0.14,-0.11,-0.08,-0.04,
     *                            0.00, 0.05, 0.07, 0.08, 0.10, 0.14/
      DATA (BASMN(I, -5),I=1,12)/-0.26,-0.24,-0.26,-0.22,-0.20,-0.16,
     *                           -0.12,-0.06,-0.02, 0.00, 0.01, 0.04/
      DATA (BASMN(I, -4),I=1,12)/-0.36,-0.34,-0.36,-0.34,-0.32,-0.29,
     *                           -0.26,-0.19,-0.13,-0.11,-0.09,-0.07/
      DATA (BASMN(I, -3),I=1,12)/-0.46,-0.44,-0.47,-0.46,-0.45,-0.42,
     *                           -0.39,-0.32,-0.25,-0.23,-0.22,-0.22/
      DATA (BASMN(I, -2),I=1,12)/-0.56,-0.53,-0.56,-0.56,-0.57,-0.55,
     *                           -0.52,-0.46,-0.38,-0.36,-0.36,-0.38/
      DATA (BASMN(I, -1),I=1,12)/-0.63,-0.60,-0.64,-0.66,-0.68,-0.67,
     *                           -0.65,-0.61,-0.52,-0.51,-0.52,-0.56/
      DATA (BASMN(I,  0),I=1,12)/-0.66,-0.63,-0.67,-0.70,-0.74,-0.75,
     *                           -0.74,-0.72,-0.64,-0.63,-0.66,-0.72/
      DATA (BASMN(I,  1),I=1,12)/-0.36,-0.24,-0.32,-0.35,-0.40,-0.43,
     *                           -0.41,-0.44,-0.38,-0.40,-0.44,-0.51/
      DATA (BASMN(I,  2),I=1,12)/ 0.20, 0.28, 0.21, 0.18, 0.12, 0.12,
     *                            0.11, 0.09, 0.09, 0.08, 0.05, 0.04/
      DATA (BASMN(I,  3),I=1,12)/ 0.54, 0.52, 0.49, 0.46, 0.37, 0.36,
     *                            0.35, 0.32, 0.29, 0.29, 0.28, 0.32/
      DATA (BASMN(I,  4),I=1,12)/ 0.71, 0.62, 0.61, 0.57, 0.47, 0.45,
     *                            0.43, 0.39, 0.34, 0.34, 0.34, 0.38/
      DATA (BASMN(I,  5),I=1,12)/ 0.79, 0.66, 0.65, 0.62, 0.51, 0.48,
     *                            0.46, 0.41, 0.35, 0.35, 0.35, 0.39/
      DATA (BASMN(I,  6),I=1,12)/ 0.81, 0.67, 0.67, 0.63, 0.53, 0.48,
     *                            0.46, 0.42, 0.35, 0.35, 0.36, 0.39/
      DATA (BASMN(I,  7),I=1,12)/ 0.81, 0.66, 0.66, 0.63, 0.53, 0.48,
     *                            0.46, 0.42, 0.35, 0.35, 0.35, 0.39/
      DATA (BASMN(I,  8),I=1,12)/ 0.80, 0.65, 0.65, 0.63, 0.53, 0.48,
     *                            0.46, 0.41, 0.34, 0.34, 0.35, 0.38/
      DATA (BASMN(I,  9),I=1,12)/ 0.75, 0.62, 0.63, 0.61, 0.52, 0.47,
     *                            0.46, 0.41, 0.33, 0.33, 0.34, 0.38/
      DATA (BASMN(I, 10),I=1,12)/ 0.69, 0.57, 0.59, 0.58, 0.50, 0.46,
     *                            0.45, 0.40, 0.33, 0.33, 0.33, 0.37/
      DATA (BASMN(I, 11),I=1,12)/ 0.61, 0.51, 0.53, 0.53, 0.47, 0.44,
     *                            0.44, 0.39, 0.32, 0.32, 0.32, 0.36/

      DATA (BASMX(I,-12),I=1,12)/ 0.29, 0.22, 0.24, 0.26, 0.32, 0.46,
     *                            0.53, 0.70, 0.67, 0.71, 0.76, 0.70/
      DATA (BASMX(I,-11),I=1,12)/ 0.22, 0.15, 0.16, 0.17, 0.19, 0.26,
     *                            0.29, 0.43, 0.44, 0.48, 0.55, 0.50/
      DATA (BASMX(I,-10),I=1,12)/ 0.15, 0.10, 0.10, 0.11, 0.12, 0.15,
     *                            0.17, 0.23, 0.24, 0.27, 0.34, 0.33/
      DATA (BASMX(I, -9),I=1,12)/ 0.09, 0.06, 0.06, 0.06, 0.07, 0.09,
     *                            0.10, 0.12, 0.13, 0.13, 0.17, 0.17/
      DATA (BASMX(I, -8),I=1,12)/ 0.04, 0.02, 0.02, 0.03, 0.04, 0.05,
     *                            0.05, 0.07, 0.07, 0.07, 0.08, 0.08/
      DATA (BASMX(I, -7),I=1,12)/-0.02,-0.01,-0.01, 0.00, 0.01, 0.02,
     *                            0.02, 0.03, 0.04, 0.04, 0.04, 0.05/
      DATA (BASMX(I, -6),I=1,12)/-0.06,-0.04,-0.04,-0.03,-0.02,-0.01,
     *                            0.00, 0.01, 0.01, 0.02, 0.02, 0.02/
      DATA (BASMX(I, -5),I=1,12)/-0.10,-0.07,-0.07,-0.06,-0.04,-0.03,
     *                           -0.02,-0.01, 0.00, 0.00, 0.00, 0.01/
      DATA (BASMX(I, -4),I=1,12)/-0.13,-0.09,-0.09,-0.08,-0.06,-0.04,
     *                           -0.04,-0.02,-0.02,-0.01,-0.01,-0.01/
      DATA (BASMX(I, -3),I=1,12)/-0.17,-0.11,-0.11,-0.09,-0.08,-0.06,
     *                           -0.05,-0.04,-0.02,-0.02,-0.02,-0.02/
      DATA (BASMX(I, -2),I=1,12)/-0.20,-0.12,-0.13,-0.11,-0.09,-0.07,
     *                           -0.06,-0.05,-0.03,-0.03,-0.03,-0.03/
      DATA (BASMX(I, -1),I=1,12)/-0.22,-0.14,-0.14,-0.12,-0.10,-0.07,
     *                           -0.07,-0.05,-0.04,-0.04,-0.04,-0.03/
      DATA (BASMX(I,  0),I=1,12)/-0.25,-0.14,-0.15,-0.13,-0.11,-0.08,
     *                           -0.08,-0.06,-0.05,-0.04,-0.05,-0.04/
      DATA (BASMX(I,  1),I=1,12)/-0.24,-0.14,-0.15,-0.12,-0.11,-0.08,
     *                           -0.08,-0.07,-0.05,-0.05,-0.05,-0.04/
      DATA (BASMX(I,  2),I=1,12)/-0.18,-0.09,-0.11,-0.09,-0.09,-0.07,
     *                           -0.08,-0.07,-0.05,-0.05,-0.06,-0.05/
      DATA (BASMX(I,  3),I=1,12)/ 0.01, 0.04, 0.00, 0.00,-0.03,-0.03,
     *                           -0.05,-0.05,-0.05,-0.05,-0.06,-0.06/
      DATA (BASMX(I,  4),I=1,12)/ 0.32, 0.27, 0.24, 0.20, 0.12, 0.09,
     *                            0.04, 0.01,-0.02,-0.02,-0.03,-0.05/
      DATA (BASMX(I,  5),I=1,12)/ 0.69, 0.57, 0.56, 0.49, 0.38, 0.30,
     *                            0.21, 0.15, 0.07, 0.06, 0.04,-0.02/
      DATA (BASMX(I,  6),I=1,12)/ 1.02, 0.88, 0.89, 0.81, 0.67, 0.59,
     *                            0.46, 0.36, 0.21, 0.21, 0.16, 0.06/
      DATA (BASMX(I,  7),I=1,12)/ 1.22, 1.09, 1.11, 1.06, 0.94, 0.88,
     *                            0.74, 0.63, 0.42, 0.40, 0.35, 0.23/
      DATA (BASMX(I,  8),I=1,12)/ 1.20, 1.10, 1.15, 1.15, 1.11, 1.11,
     *                            0.97, 0.87, 0.64, 0.62, 0.56, 0.44/
      DATA (BASMX(I,  9),I=1,12)/ 0.95, 0.88, 0.96, 1.02, 1.09, 1.18,
     *                            1.09, 1.03, 0.83, 0.81, 0.78, 0.64/
      DATA (BASMX(I, 10),I=1,12)/ 0.62, 0.55, 0.64, 0.73, 0.88, 1.05,
     *                            1.03, 1.06, 0.91, 0.90, 0.91, 0.81/
      DATA (BASMX(I, 11),I=1,12)/ 0.41, 0.33, 0.38, 0.44, 0.58, 0.77,
     *                            0.83, 0.94, 0.85, 0.87, 0.90, 0.82/

      DATA CFAV/0.31922,0.37431,0.44731,0.50498,0.56846,0.56295,0.52204,
     *          0.48322,0.44204,0.42144,0.36685,0.11857/

      DATA CFMN/0.32202,0.38046,0.45206,0.47658,0.55423,0.55926,0.52539,
     *          0.49406,0.41774,0.40619,0.36537,0.11671/

      DATA CFMX/0.31180,0.36147,0.43151,0.50968,0.55989,0.56033,0.53689,
     *          0.49398,0.46986,0.44970,0.36621,0.12074/

      DATA (DTDIF(J,  1),J=1,12)/2.91,3.20,2.54,2.82,2.77,2.29,1.87,
     *                           1.83,2.29,2.60,2.71,2.72/
      DATA (DTDIF(J,  2),J=1,12)/3.48,3.66,2.38,2.75,2.70,2.43,1.85,
     *                           2.20,2.61,2.84,3.07,3.18/
      DATA (DTDIF(J,  3),J=1,12)/3.35,3.22,2.49,3.30,3.33,2.62,1.89,
     *                           1.96,2.52,2.73,3.07,3.25/
      DATA (DTDIF(J,  4),J=1,12)/2.93,3.27,2.32,2.90,2.97,2.16,1.74,
     *                           1.84,2.58,2.71,2.72,2.93/
      DATA (DTDIF(J,  5),J=1,12)/3.45,3.53,2.68,2.94,3.06,2.44,1.99,
     *                           2.37,2.80,2.66,2.97,3.33/
      DATA (DTDIF(J,  6),J=1,12)/4.22,4.04,2.98,2.84,3.01,2.30,2.03,
     *                           2.31,2.96,2.90,2.95,4.30/
      DATA (DTDIF(J,  7),J=1,12)/2.51,2.55,2.37,2.17,1.75,1.45,1.16,
     *                           1.21,1.52,1.54,2.04,2.25/
      DATA (DTDIF(J,  8),J=1,12)/1.92,1.73,1.84,2.13,1.71,1.52,1.29,
     *                           1.31,1.57,1.52,1.60,1.49/
      DATA (DTDIF(J,  9),J=1,12)/1.59,1.21,1.22,1.20,0.74,0.61,0.65,
     *                           0.76,1.14,1.28,1.40,1.46/
      DATA (DTDIF(J, 10),J=1,12)/1.62,1.44,1.44,1.58,1.37,1.08,1.18,
     *                           1.39,1.14,1.28,1.41,1.25/
      DATA (DTDIF(J, 11),J=1,12)/3.80,4.01,3.12,2.86,2.49,1.76,1.31,
     *                           1.41,1.92,2.41,3.19,3.61/
      DATA (DTDIF(J, 12),J=1,12)/3.97,3.95,3.01,2.81,2.07,1.55,1.15,
     *                           1.29,1.69,2.22,3.08,3.33/
      DATA (DTDIF(J, 13),J=1,12)/3.26,3.69,2.92,3.30,3.26,2.58,1.96,
     *                           2.09,2.88,3.03,2.98,2.98/
      DATA (DTDIF(J, 14),J=1,12)/4.17,3.90,3.16,3.25,3.04,2.54,2.10,
     *                           2.66,3.85,3.40,3.44,4.25/
      DATA (DTDIF(J, 15),J=1,12)/3.69,3.49,3.44,3.11,2.35,1.89,1.52,
     *                           1.34,2.07,2.49,3.52,3.39/
      DATA (DTDIF(J, 16),J=1,12)/1.62,1.37,1.38,1.66,1.33,1.02,0.89,
     *                           1.04,1.40,1.56,1.70,1.54/
      DATA (DTDIF(J, 17),J=1,12)/2.33,1.63,1.69,1.75,1.88,2.07,1.67,
     *                           1.61,1.89,1.71,1.73,1.90/
      DATA (DTDIF(J, 18),J=1,12)/1.63,1.31,1.52,1.77,1.52,1.86,1.26,
     *                           1.29,1.65,1.49,1.31,1.38/
      DATA (DTDIF(J, 19),J=1,12)/3.29,3.21,2.91,2.36,1.66,1.19,0.93,
     *                           1.07,1.51,1.90,2.73,3.10/
      DATA (DTDIF(J, 20),J=1,12)/3.36,2.96,2.85,2.03,1.16,0.87,0.78,
     *                           0.72,0.91,1.37,2.03,2.78/
      DATA (DTDIF(J, 21),J=1,12)/3.66,3.00,2.63,1.75,1.19,0.92,0.70,
     *                           0.78,0.99,1.57,2.71,3.01/
      DATA (DTDIF(J, 22),J=1,12)/2.50,2.31,1.96,1.34,0.90,0.62,0.61,
     *                           0.68,0.67,0.91,1.34,1.96/
      DATA (DTDIF(J, 23),J=1,12)/3.57,3.65,2.91,2.84,2.38,1.75,1.17,
     *                           1.36,1.95,2.34,3.09,3.31/
      DATA (DTDIF(J, 24),J=1,12)/2.97,3.00,2.58,2.38,1.81,1.40,1.02,
     *                           1.02,1.61,1.92,2.53,2.90/
      DATA (DTDIF(J, 25),J=1,12)/3.40,3.26,2.91,2.57,1.73,1.26,0.98,
     *                           1.07,1.45,1.90,2.81,3.06/
      DATA (DTDIF(J, 26),J=1,12)/3.71,3.30,3.35,3.12,2.50,1.85,1.59,
     *                           1.56,2.48,2.79,3.45,3.53/
      DATA (DTDIF(J, 27),J=1,12)/4.07,3.08,3.10,2.92,2.18,1.72,1.50,
     *                           1.72,2.26,2.75,3.32,3.28/
      DATA (DTDIF(J, 28),J=1,12)/4.69,3.73,3.09,3.29,2.58,2.25,2.24,
     *                           2.58,3.40,3.61,4.35,4.14/
      DATA (DTDIF(J, 29),J=1,12)/1.89,1.37,1.35,1.53,1.61,1.74,1.65,
     *                           1.48,1.40,1.57,1.69,1.92/
      DATA (DTDIF(J, 30),J=1,12)/1.71,1.41,1.13,1.26,1.05,0.82,0.93,
     *                           1.06,1.28,1.56,1.70,1.65/
      DATA (DTDIF(J, 31),J=1,12)/2.36,2.04,2.19,2.08,1.67,1.27,1.13,
     *                           1.10,1.19,1.37,1.93,1.79/
      DATA (DTDIF(J, 32),J=1,12)/2.24,2.20,2.26,2.22,1.58,1.20,1.28,
     *                           1.14,1.28,1.47,1.98,1.93/
      DATA (DTDIF(J, 33),J=1,12)/3.51,3.59,2.81,2.49,1.93,1.53,1.15,
     *                           1.33,1.35,2.13,2.98,3.17/
      DATA (DTDIF(J, 34),J=1,12)/3.11,2.77,2.52,1.92,1.35,1.00,0.79,
     *                           0.80,1.01,1.75,2.50,2.80/
      DATA (DTDIF(J, 35),J=1,12)/3.64,3.63,2.97,3.05,2.79,2.10,1.54,
     *                           1.78,2.31,2.59,3.07,3.51/
      DATA (DTDIF(J, 36),J=1,12)/3.54,3.74,2.98,3.05,2.74,2.29,1.68,
     *                           1.82,2.21,2.51,3.25,3.49/
      DATA (DTDIF(J, 37),J=1,12)/3.47,2.99,2.86,1.79,1.26,0.79,0.74,
     *                           0.82,1.05,1.53,2.70,3.31/
      DATA (DTDIF(J, 38),J=1,12)/3.55,3.29,2.67,1.60,0.95,0.73,0.51,
     *                           0.62,0.97,1.51,2.45,3.08/
      DATA (DTDIF(J, 39),J=1,12)/3.87,3.53,2.94,2.56,1.88,1.46,1.05,
     *                           1.18,1.72,2.08,3.10,3.18/
      DATA (DTDIF(J, 40),J=1,12)/3.76,3.20,2.74,2.32,1.67,1.11,0.90,
     *                           1.13,1.34,1.94,2.93,3.13/
      DATA (DTDIF(J, 41),J=1,12)/3.54,3.31,3.04,2.35,1.55,1.06,0.70,
     *                           0.94,1.29,1.76,3.04,3.01/
      DATA (DTDIF(J, 42),J=1,12)/2.40,1.97,2.03,1.99,1.81,1.72,1.36,
     *                           1.44,1.57,1.74,2.09,2.08/
      DATA (DTDIF(J, 43),J=1,12)/2.42,1.80,2.07,2.60,2.42,2.53,2.12,
     *                           2.21,2.34,2.29,2.48,2.09/
      DATA (DTDIF(J, 44),J=1,12)/3.84,3.57,3.63,2.99,2.14,1.56,1.37,
     *                           1.59,2.19,2.32,3.27,3.28/
      DATA (DTDIF(J, 45),J=1,12)/1.76,1.28,1.41,1.65,1.54,1.91,1.61,
     *                           1.65,1.67,1.36,1.56,1.42/
      DATA (DTDIF(J, 46),J=1,12)/3.54,3.91,2.83,3.05,3.16,2.56,1.77,
     *                           2.02,2.50,3.02,3.01,3.38/
      DATA (DTDIF(J, 47),J=1,12)/3.91,3.71,3.28,2.94,2.36,1.87,1.44,
     *                           1.72,2.06,2.64,3.31,3.44/
      DATA (DTDIF(J, 48),J=1,12)/3.72,3.53,3.38,3.28,2.78,2.08,1.95,
     *                           2.15,2.87,3.03,3.50,3.50/
      DATA (DTDIF(J, 49),J=1,12)/2.91,3.26,2.69,3.04,3.11,2.54,1.89,
     *                           2.00,2.59,2.75,2.90,2.77/
      DATA (DTDIF(J, 50),J=1,12)/3.13,3.70,2.58,2.41,2.88,2.44,2.01,
     *                           2.09,2.52,2.57,2.57,3.53/
      DATA (DTDIF(J, 51),J=1,12)/1.97,1.87,2.15,2.56,1.96,1.70,1.22,
     *                           1.62,1.75,1.72,1.81,1.77/
      DATA (DTDIF(J, 52),J=1,12)/3.87,3.44,2.84,3.11,2.79,2.61,2.49,
     *                           2.80,3.38,3.02,3.53,3.74/
      DATA (DTDIF(J, 53),J=1,12)/2.57,2.30,2.40,2.71,2.36,2.13,1.75,
     *                           1.96,2.33,2.49,2.30,2.03/
      DATA (DTDIF(J, 54),J=1,12)/2.44,1.85,1.58,2.06,1.92,2.15,1.88,
     *                           2.01,2.12,2.06,2.34,2.12/
      DATA (DTDIF(J, 55),J=1,12)/1.97,1.49,1.44,1.74,1.59,1.69,1.72,
     *                           1.55,1.61,1.79,1.77,1.85/
      DATA (DTDIF(J, 56),J=1,12)/2.74,2.36,2.15,1.40,0.90,0.66,0.74,
     *                           0.70,0.91,1.19,1.83,2.38/
      DATA (DTDIF(J, 57),J=1,12)/3.71,3.71,3.12,3.21,2.43,2.10,1.61,
     *                           1.79,2.30,2.71,3.17,3.48/
      DATA (DTDIF(J, 58),J=1,12)/3.86,3.67,3.34,3.21,2.51,1.91,1.55,
     *                           1.49,2.46,2.76,3.54,3.62/
      DATA (DTDIF(J, 59),J=1,12)/3.69,3.68,2.94,3.23,2.75,2.01,1.39,
     *                           1.63,2.06,2.33,3.10,3.17/
      DATA (DTDIF(J, 60),J=1,12)/3.72,4.05,2.40,2.73,2.83,2.48,1.97,
     *                           2.19,2.85,3.00,3.02,3.23/
      DATA (DTDIF(J, 61),J=1,12)/2.10,1.76,2.01,1.82,1.63,1.19,1.26,
     *                           1.14,1.17,1.38,1.78,1.56/
      DATA (DTDIF(J, 62),J=1,12)/3.58,2.93,2.97,2.57,1.83,1.34,1.40,
     *                           1.58,1.61,1.96,2.85,3.01/
      DATA (DTDIF(J, 63),J=1,12)/4.02,3.39,2.58,2.67,2.08,1.99,1.73,
     *                           1.87,2.46,2.50,3.64,3.73/
      DATA (DTDIF(J, 64),J=1,12)/3.96,3.20,2.97,3.20,2.30,1.89,1.76,
     *                           2.06,2.57,2.71,3.40,3.62/
      DATA (DTDIF(J, 65),J=1,12)/4.17,4.49,2.99,2.67,2.99,2.63,1.92,
     *                           2.16,2.57,3.17,3.08,3.74/
      DATA (DTDIF(J, 66),J=1,12)/3.45,3.45,2.60,2.82,2.83,2.26,1.73,
     *                           2.19,2.56,2.78,3.14,3.44/
      DATA (DTDIF(J, 67),J=1,12)/3.22,2.30,1.99,2.56,2.14,1.89,1.76,
     *                           1.80,1.94,2.17,2.58,2.55/
      DATA (DTDIF(J, 68),J=1,12)/4.01,3.29,2.69,2.99,2.42,1.99,1.76,
     *                           2.16,3.00,2.96,3.57,3.70/
      DATA (DTDIF(J, 69),J=1,12)/4.18,4.04,3.80,3.18,2.48,1.51,1.20,
     *                           1.19,1.86,2.46,3.59,3.61/
      DATA (DTDIF(J, 70),J=1,12)/4.14,3.40,3.16,3.13,2.51,2.23,1.76,
     *                           1.94,2.73,2.69,3.86,3.81/
      DATA (DTDIF(J, 71),J=1,12)/1.99,1.89,1.69,2.18,1.96,1.58,1.19,
     *                           1.19,1.46,1.60,1.63,1.72/
      DATA (DTDIF(J, 72),J=1,12)/4.17,4.03,2.92,3.11,2.95,2.38,2.20,
     *                           2.84,3.16,3.31,3.62,4.19/
      DATA (DTDIF(J, 73),J=1,12)/3.55,3.23,2.91,2.94,2.72,2.37,1.99,
     *                           2.31,3.35,2.72,3.37,3.57/
      DATA (DTDIF(J, 74),J=1,12)/3.51,4.06,2.47,2.46,2.69,2.53,1.89,
     *                           2.07,2.85,2.99,2.98,3.06/
      DATA (DTDIF(J, 75),J=1,12)/2.65,2.52,2.58,2.25,1.79,1.28,1.44,
     *                           1.08,1.29,1.47,2.24,2.30/
      DATA (DTDIF(J, 76),J=1,12)/2.26,1.98,2.42,2.39,1.81,1.27,1.28,
     *                           1.22,1.46,1.57,1.97,1.94/
      DATA (DTDIF(J, 77),J=1,12)/3.22,3.32,3.00,2.43,1.78,1.38,1.10,
     *                           1.08,1.45,1.92,2.91,3.25/
      DATA (DTDIF(J, 78),J=1,12)/3.30,3.26,2.55,2.74,2.08,1.42,1.16,
     *                           1.24,1.75,2.08,2.76,3.14/
      DATA (DTDIF(J, 79),J=1,12)/3.32,3.18,2.57,2.64,1.79,1.32,1.00,
     *                           1.04,1.40,1.86,2.70,2.82/
      DATA (DTDIF(J, 80),J=1,12)/1.80,1.10,1.20,1.30,1.00,1.10,1.55,
     *                           1.25,1.30,1.60,1.60,1.60/
      DATA (DTDIF(J, 81),J=1,12)/1.75,1.25,1.30,1.45,1.00,1.00,1.25,
     *                           1.25,1.40,1.60,1.60,1.60/
      DATA (DTDIF(J, 82),J=1,12)/1.70,1.20,1.25,1.50,1.40,1.00,1.00,
     *                           1.20,1.30,1.60,1.50,1.60/
      DATA (DTDIF(J, 83),J=1,12)/1.65,1.20,1.20,1.50,1.40,1.10,0.90,
     *                           0.95,1.20,1.55,1.50,1.50/
      DATA (DTDIF(J, 84),J=1,12)/1.50,1.00,1.00,1.50,1.20,1.00,0.90,
     *                           0.90,1.10,1.40,1.50,1.20/
      DATA (DTDIF(J, 85),J=1,12)/1.60,1.10,1.20,1.45,1.20,1.00,1.00,
     *                           1.00,1.50,1.40,1.50,1.20/
      DATA (DTDIF(J, 86),J=1,12)/1.60,1.10,1.30,1.50,1.10,1.20,1.00,
     *                           1.00,1.30,1.50,1.50,1.20/
      DATA (DTDIF(J, 87),J=1,12)/1.70,1.20,1.20,1.70,1.20,1.20,1.10,
     *                           1.10,1.50,1.50,1.50,1.20/
      DATA (DTDIF(J, 88),J=1,12)/1.80,1.30,1.35,1.40,1.20,0.90,1.10,
     *                           1.20,1.30,1.30,1.40,1.50/
      DATA (DTDIF(J, 89),J=1,12)/1.85,1.40,1.65,2.10,1.70,1.10,1.25,
     *                           1.30,1.60,1.50,1.60,1.70/
      DATA (DTDIF(J, 90),J=1,12)/1.40,1.35,1.35,1.35,1.00,0.80,0.75,
     *                           1.15,1.20,1.30,1.30,1.30/
      DATA (DTDIF(J, 91),J=1,12)/1.40,1.20,1.40,1.40,0.90,0.90,0.80,
     *                           1.15,1.10,1.30,1.30,1.30/
      DATA (DTDIF(J, 92),J=1,12)/3.60,3.20,3.00,1.80,1.50,1.00,0.75,
     *                           0.85,1.30,1.70,2.60,3.20/
      DATA (DTDIF(J, 93),J=1,12)/3.60,3.10,2.70,1.65,1.25,0.80,0.50,
     *                           0.70,0.90,1.40,2.60,3.10/
      DATA (DTDIF(J, 94),J=1,12)/3.60,2.90,2.60,1.60,1.25,0.80,0.50,
     *                           0.80,0.80,1.50,2.60,3.10/
      DATA (DTDIF(J, 95),J=1,12)/3.25,2.60,2.50,1.60,1.25,0.90,0.65,
     *                           0.80,0.80,1.50,2.50,2.90/
      DATA (DTDIF(J, 96),J=1,12)/2.60,3.20,2.20,2.70,2.70,2.20,1.49,
     *                           1.60,2.20,2.40,2.80,2.60/
      DATA (DTDIF(J, 97),J=1,12)/2.60,3.10,2.20,2.70,2.75,2.30,1.70,
     *                           1.80,2.30,2.55,2.80,2.60/
      DATA (DTDIF(J, 98),J=1,12)/3.70,4.20,2.75,2.70,2.80,2.70,1.80,
     *                           2.20,2.70,3.10,3.00,3.20/
      DATA (DTDIF(J, 99),J=1,12)/4.20,4.50,3.20,2.80,3.30,2.80,1.90,
     *                           2.25,2.75,3.30,3.25,3.60/
      DATA (DTDIF(J,100),J=1,12)/4.20,4.50,3.20,2.85,3.30,2.70,1.80,
     *                           2.20,2.70,3.30,3.25,3.60/
      DATA (DTDIF(J,101),J=1,12)/2.65,3.40,2.40,2.60,3.45,2.20,1.90,
     *                           2.25,2.70,2.60,2.80,3.30/
      DATA (DTDIF(J,102),J=1,12)/2.85,3.50,2.45,2.70,3.30,2.20,1.80,
     *                           2.20,2.70,2.70,2.90,3.30/
      DATA (DTDIF(J,103),J=1,12)/2.91,3.19,2.70,2.43,1.73,1.23,0.97,
     *                           0.97,1.52,1.82,2.60,3.01/
      DATA (DTDIF(J,104),J=1,12)/3.44,3.57,2.84,2.82,2.29,1.69,1.39,
     *                           1.33,1.90,2.30,2.83,3.10/
      DATA (DTDIF(J,105),J=1,12)/3.64,3.59,3.02,3.06,2.80,2.11,1.40,
     *                           1.52,1.77,2.08,2.99,3.07/
      DATA (DTDIF(J,106),J=1,12)/3.59,3.75,2.93,2.91,2.48,1.91,1.46,
     *                           1.49,2.05,2.37,2.80,3.10/
      DATA (DTDIF(J,107),J=1,12)/3.13,3.18,2.41,2.54,1.84,1.25,1.02,
     *                           0.99,1.42,1.79,2.51,2.79/
      DATA (DTDIF(J,108),J=1,12)/3.51,3.22,2.92,2.29,1.45,1.07,0.89,
     *                           0.97,1.13,1.55,2.54,2.98/
      DATA (DTDIF(J,109),J=1,12)/3.18,3.13,2.69,2.11,1.49,0.99,0.86,
     *                           0.73,1.26,1.82,2.70,2.98/
      DATA (DTDIF(J,110),J=1,12)/4.17,3.71,3.38,3.16,2.10,1.64,1.44,
     *                           1.66,2.00,2.53,3.49,3.57/
      DATA (DTDIF(J,111),J=1,12)/2.87,3.06,2.42,2.79,2.59,2.16,1.52,
     *                           1.87,2.24,2.44,2.70,2.69/
      DATA (DTDIF(J,112),J=1,12)/3.22,3.41,2.37,2.75,2.63,2.34,1.73,
     *                           1.89,2.42,2.86,3.09,2.80/
      DATA (DTDIF(J,113),J=1,12)/3.55,3.50,3.04,3.09,2.75,2.20,1.75,
     *                           1.81,2.35,2.57,3.20,3.66/
      DATA (DTDIF(J,114),J=1,12)/3.33,3.39,2.62,3.12,2.89,2.22,1.65,
     *                           1.91,2.42,2.75,3.07,3.24/
      DATA (DTDIF(J,115),J=1,12)/3.61,3.50,2.66,2.95,3.07,2.37,1.88,
     *                           2.32,2.81,2.90,3.33,3.51/
      DATA (DTDIF(J,116),J=1,12)/3.31,4.01,3.11,3.62,3.53,2.85,2.00,
     *                           2.07,2.58,2.76,2.97,3.00/
      DATA (DTDIF(J,117),J=1,12)/4.06,3.82,3.33,3.24,3.28,2.45,2.05,
     *                           2.60,3.30,3.33,3.22,4.20/
      DATA (DTDIF(J,118),J=1,12)/3.64,3.41,2.76,3.07,2.80,2.31,1.74,
     *                           2.09,2.44,2.90,3.41,3.40/
      DATA (DTDIF(J,119),J=1,12)/3.65,3.39,2.73,3.08,2.44,2.13,1.59,
     *                           2.09,2.65,2.78,3.37,3.65/
      DATA (DTDIF(J,120),J=1,12)/3.62,3.64,2.44,2.85,2.67,2.21,1.80,
     *                           1.92,2.81,2.91,3.23,3.77/
      DATA (DTDIF(J,121),J=1,12)/4.15,3.59,3.22,3.10,2.91,2.52,1.95,
     *                           2.22,3.27,3.02,3.72,3.87/
      DATA (DTDIF(J,122),J=1,12)/1.77,1.53,1.65,2.05,1.67,1.83,1.32,
     *                           1.26,1.83,1.73,1.55,1.52/
      DATA (DTDIF(J,123),J=1,12)/1.97,1.75,1.89,2.00,1.64,1.24,1.09,
     *                           1.07,1.30,1.30,1.80,1.66/
      DATA (DTDIF(J,124),J=1,12)/1.29,1.08,1.10,1.06,0.78,0.63,0.67,
     *                           0.66,0.97,1.10,1.12,1.24/
      DATA (DTDIF(J,125),J=1,12)/4.42,3.66,3.21,3.11,2.60,2.27,1.83,
     *                           2.16,2.81,3.01,3.91,3.83/
      DATA (DTDIF(J,126),J=1,12)/4.19,3.39,2.91,3.20,2.40,2.35,2.10,
     *                           2.29,3.18,3.24,3.90,3.68/
      DATA (DTDIF(J,127),J=1,12)/2.84,2.02,1.63,2.09,1.89,2.17,1.92,
     *                           2.04,2.01,2.11,2.61,2.48/
      DATA (DTDIF(J,128),J=1,12)/2.26,1.82,1.71,1.88,1.79,1.87,1.62,
     *                           1.47,1.84,1.72,2.06,2.04/
      DATA (DTDIF(J,129),J=1,12)/2.47,1.89,1.93,2.08,1.99,2.12,2.19,
     *                           1.94,1.84,1.88,2.23,1.96/
      DATA (DTDIF(J,130),J=1,12)/4.09,3.78,3.31,3.05,2.63,1.92,1.22,
     *                           1.56,1.94,2.61,3.22,3.66/

      DATA NSTA/130/

      DATA RELEV/22.5,28.5,34.5,40.5,46.5,52.5,58.5,64.5,70.5,76.5,82.5,
     *           90.0/

      DATA RHO/ 4.50, 5.77, 4.43, 3.66, 4.77, 5.54,13.70,10.97, 8.22,
     *         12.65, 6.08, 6.61, 3.49, 6.60,11.38,12.39, 8.12, 8.58,
     *          9.84, 7.97, 7.16, 6.86, 8.66, 8.64, 7.43,12.19, 6.69,
     *          4.15, 2.64, 4.53,13.63,12.10, 3.76, 9.53, 4.80, 4.41,
     *          6.72, 7.12, 6.36, 7.33, 8.11,12.91, 5.34, 7.66, 6.84,
     *          3.55, 5.82, 8.84, 3.29, 3.81, 9.11, 6.37, 6.70, 2.82,
     *          3.38, 8.77, 5.77,10.26, 8.36, 7.21,12.09, 8.57, 5.17,
     *          7.47, 3.81, 4.71, 6.86, 6.62, 9.07,10.45,13.32, 6.41,
     *         11.11, 6.61,11.66,10.17, 9.48, 8.40, 6.77, 2.50, 3.50,
     *          5.00, 6.00, 6.50, 7.00, 8.00, 8.50,10.00,11.00,13.00,
     *         12.50, 8.50, 6.50, 7.00, 8.00, 7.00, 6.00, 4.50, 5.50,
     *          5.50, 3.00, 3.00,10.12, 8.39, 5.49, 6.30, 7.69, 8.90,
     *          8.71, 7.52, 5.20, 5.20, 4.92, 4.77, 5.28, 2.56, 4.75,
     *          5.33, 5.87, 6.91,10.49, 8.77,13.74, 8.51, 7.16, 4.98,
     *          2.87, 5.56, 5.02, 5.59/

      END    
      
