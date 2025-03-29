program merge_update
use merge_module

implicit none 

 !******************************************************************************
 ! merge_update.f95
 ! 
 ! **DESCRIPTION**
 ! This program, written in FORTRAN, Uses the latest recommended merge, and 
 ! Appends the latest data to it from the following four sources:
 !     ghcnd, climat-ncdc, climat-uk, climat-prelim, climat-bufr mcdw-unpublished
 ! 
 ! This program has a module associated with it (merge_module.f95)
 !   Using g95, the code can be compiled using the following command:
 !     g95 merge_module.f95 merge_update.f95
 ! 
 ! In addition,there are input files that must be present in the working 
 ! directory, or else the program will fail:
 !   resources/databank_sources.txt
 !   resources/update_sources.txt
 !
 ! **COPYRIGHT**
 ! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC
 ! DOMAIN AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE. THEY ARE
 ! FURNISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
 ! INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
 ! EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE AND
 ! DOCUMENTATION FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR
 ! THE USE OF THE SOFTWARE AND DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL
 ! SUPPORT TO USERS.
 !
 ! **REVISION HISTORY**
 ! 07/23/2014: For first iteration of databank update system
 ! 08/08/2014: To incorporate v1.0.1 of databank
 ! 09/08/2014: Made change in monthly_difference value. More robust and accurate
 ! 10/20/2014: Re-Structured Master data into multi-dimensional array
 ! 10/21/2014: Added WMO cross-reference table from GHCN-Daily
 ! 07/06/2015: To incorporate v1.1.0 of databank
 ! 09/18/2015: Removing Meta comparisons and relying solely on WMO Cross-Reference
 ! 09/22/2015: Forcing GHCN-D source "S" to only be appeneded as last resort
 ! 09/21/2016: Removing Hard Coded Directory Paths via getarg
 ! 10/05/2017: Removes 5 year threshold
 ! 10/31/2019: Updated for GHCNM-v4.0.1 processing
 !*******************************************************************************

 !*******************************************************************************
 ! Initialize Varibles
 !*******************************************************************************
 ! Required Directories / variables
 character(len=1024) :: version = "v1.1.1"
 character(len=1024) :: main_directory,source_directory,recommended_directory
 
 ! Booleans 
 logical :: station_match
 logical :: write_log

 ! Info for WMO cross reference file
 character(len=1024) :: wmo_file
 integer :: wmo_num_stations
 character(len=1024), allocatable, dimension(:) :: wmo_id1
 character(len=1024), allocatable, dimension(:) :: wmo_id2

 ! Info for candidate datasets to be read in
 character(len=1024) :: dataset_update_file
 character(len=1024) :: dataset_recommended_file
 integer :: number_of_datasets
 integer :: dataset_counter
 character(len=1024), allocatable, dimension(:) :: dataset_name_lower
 character(len=1024), allocatable, dimension(:) :: dataset_name_upper
 character(len=1024), allocatable, dimension(:) :: dataset_position

 ! Info for Master Metafile (Need to store entire file)
 character(len=1024) :: master_metafile

 integer :: master_num_stations
 integer :: master_counter

 character(len=1024), dimension(:), allocatable :: master_meta_key
 character(len=1024), dimension(:), allocatable :: master_meta_name
 character(len=1024), dimension(:), allocatable :: master_meta_country
 character(len=1024), dimension(:), allocatable :: master_meta_id
 character(len=1024), dimension(:), allocatable :: master_meta_recid
 character(len=1024), dimension(:), allocatable :: master_meta_wmo

 real, dimension(:), allocatable :: master_meta_lat
 real, dimension(:), allocatable :: master_meta_lon
 real, dimension(:), allocatable :: master_meta_elev
 
 integer, dimension(:), allocatable :: master_meta_start_tmax
 integer, dimension(:), allocatable :: master_meta_end_tmax
 integer, dimension(:), allocatable :: master_meta_start_tmin
 integer, dimension(:), allocatable :: master_meta_end_tmin
 integer, dimension(:), allocatable :: master_meta_start_tavg
 integer, dimension(:), allocatable :: master_meta_end_tavg
 
 ! Info for Master Datafile (Need to store entire file)
 character(len=1024) :: master_datafile

 character(len=1024), dimension(1700:2030,1:12) :: master_data_name
 character(len=4), dimension(1700:2030,1:12) :: master_data_year
 character(len=2), dimension(1700:2030,1:12) :: master_data_month
 character(len=2), dimension(1700:2030,1:12) :: master_data_day
 character(len=1024), dimension(1700:2030,1:12) :: master_data_source_update
 character(len=1024), dimension(1700:2030,1:12) :: master_data_stage2_flag
 real, dimension(1700:2030,1:12) :: master_data_lat
 real, dimension(1700:2030,1:12) :: master_data_lon
 real, dimension(1700:2030,1:12) :: master_data_elev

 integer, dimension(1700:2030,1:12,1:3) :: master_data_value
 character(len=8), dimension(1700:2030,1:12,1:3) :: master_data_source
 character(len=6) :: master_data_yrmo
 integer :: master_latest_month

 ! Info for Candidate Metafile (Need to store entire file)
 character(len=1024) :: candidate_metafile
 
 integer, dimension(:), allocatable :: candidate_num_stations
 integer :: candidate_counter
 
 character(len=1024), dimension(:,:), allocatable :: candidate_meta_id
 character(len=1024), dimension(:,:), allocatable :: candidate_meta_name
 
 real, dimension(:,:), allocatable :: candidate_meta_lat
 real, dimension(:,:), allocatable :: candidate_meta_lon
 real, dimension(:,:), allocatable :: candidate_meta_elev

 ! Info for Candidate Datafile (Only store one line at a time)
 character(len=1024) :: candidate_datafile
 character(len=4) :: candidate_data_year
 character(len=2) :: candidate_data_month
 character(len=2) :: candidate_data_day
 character(len=6) :: candidate_data_yrmo
 character(len=1024) :: candidate_data_stage2_flag
 character(len=1024) :: candidate_data_source_flag
 
 integer :: candidate_data_tmax
 integer :: candidate_data_tmin
 integer :: candidate_data_tavg
 integer :: candidate_latest_month
 integer :: candidate_latest_year

 ! Other Stuff
 character(len=1024) :: sourceline
 character(len=1024) :: out_datafile
 character(len=1024) :: out_metafile
 character(len=1024) :: argument
 character(len=1024) :: log_file
 character(len=8) :: current_date
 integer,dimension(8) :: current_time
 
 integer :: number_of_lines
 integer :: current_month
 integer :: current_year
 integer :: monthly_difference
 integer :: match_counter 
 integer :: append_data

 real :: distance_value
 real :: distance_probability
 real :: height_value
 real :: height_probability
 real :: jaccard_probability
 real :: metadata_probability
 logical :: id_match 

 integer :: year_counter
 integer :: month_counter
 integer :: element_counter

!*******************************************************************************
! GET ARGUMENTS
!*******************************************************************************
call getarg(1,argument)
main_directory=argument

call getarg(2,argument)
read (argument,'(i6)') current_month

if (main_directory=="".or. argument=="") then
  write (*,*) ""
  write (*,*) "USAGE: <MAIN_DIRECTORY> <APPEND_MONTH>"
  write (*,*) ""
  stop
endif

 !*******************************************************************************
 ! Grab Current Time and BEGIN!!!
 !*******************************************************************************
 call date_and_time(DATE=current_date,VALUES=current_time)

 if(current_month < 200000) then
   stop "ERROR: Current Date Not Valid"
 endif
 read (argument(1:4),'(i4)') current_year 

 ! Create LOG file
 call date_and_time(DATE=current_date)
 log_file = trim(main_directory)//'update/log/log_UPDATE_'//current_date

 open(unit=11,file=log_file,status='replace')

 write(11,'(a,1x,a8,1x,i2.2,a,i2.2,a,i2.2)') "BEGIN:",current_date,current_time(5),":",current_time(6),":",current_time(7)

 !*******************************************************************************
 ! Read in WMO Cross-Reference File
 !*******************************************************************************
 write(11,*) "READING IN WMO CROSS-REFERENCE FILE"
 call flush(11)

 wmo_file = trim(main_directory)//'update/resources/wmo_reference-'//trim(adjustl(version))//'.txt'
 inquire(file=wmo_file,exist=file_exists)
 if (file_exists .eqv. .false.) stop "ERROR: cannot find WMO cross-reference file"

 call file_recs(FILE_NAME=wmo_file,NUM_UNIT=20,NUM_LINES=wmo_num_stations)
 allocate(wmo_id1(1:wmo_num_stations))
 allocate(wmo_id2(1:wmo_num_stations))

 line_counter = 0
 open(unit=1,file=wmo_file,status='OLD')
 do
   read(1,'(a1024)',IOSTAT=ioerror) readline
   if (ioerror==-1) exit
   line_counter=line_counter+1

   wmo_id1(line_counter)=readline(1:11)
   wmo_id2(line_counter)=readline(13:17)
 
 enddo
 close(1)

 !*******************************************************************************
 ! Read in Master Dataset
 !*******************************************************************************
 write(11,*) "READING IN MASTER INVENTORY"
 call flush(11)
 
 recommended_directory = trim(adjustl(main_directory))//"/input_"//trim(adjustl(version))//"/" 

 master_metafile=trim(recommended_directory)//'results_merged/INVENTORY_monthly_merged_stage3'
 inquire(file=master_metafile,exist=file_exists)
 if (file_exists .eqv. .false.) stop "ERROR: cannot find MASTER INVENTORY infile"
 call file_recs(FILE_NAME=master_metafile,NUM_UNIT=20,NUM_LINES=master_num_stations)

 ! Allocate Stage 3 data depending on number of stations
 allocate(master_meta_recid(1:master_num_stations))
 allocate(master_meta_name(1:master_num_stations))
 allocate(master_meta_country(1:master_num_stations))
 allocate(master_meta_lat(1:master_num_stations))
 allocate(master_meta_lon(1:master_num_stations))
 allocate(master_meta_elev(1:master_num_stations))
 allocate(master_meta_start_tmax(1:master_num_stations))
 allocate(master_meta_end_tmax(1:master_num_stations))
 allocate(master_meta_start_tmin(1:master_num_stations))
 allocate(master_meta_end_tmin(1:master_num_stations))
 allocate(master_meta_start_tavg(1:master_num_stations))
 allocate(master_meta_end_tavg(1:master_num_stations))
 allocate(master_meta_id(1:master_num_stations))
 allocate(master_meta_key(1:master_num_stations))
 allocate(master_meta_wmo(1:master_num_stations))

 line_counter = 0
 open(unit=1,file=master_metafile,status='OLD')
 do
   read(1,'(a1024)',IOSTAT=ioerror) readline
   if (ioerror==-1) exit
   line_counter=line_counter+1

   master_meta_recid(line_counter)=readline(1:12)
   master_meta_name(line_counter)=readline(14:43)
   master_meta_country(line_counter)=readline(45:64)
   read (readline(66:75),'(f10.4)') master_meta_lat(line_counter)
   read (readline(77:86),'(f10.4)') master_meta_lon(line_counter)
   read (readline(88:95),'(f8.2)') master_meta_elev(line_counter)
   read (readline(97:100),'(i4)') master_meta_start_tmax(line_counter)
   read (readline(102:105),'(i4)') master_meta_end_tmax(line_counter)
   read (readline(107:110),'(i4)') master_meta_start_tmin(line_counter)
   read (readline(112:115),'(i4)') master_meta_end_tmin(line_counter)
   read (readline(117:120),'(i4)') master_meta_start_tavg(line_counter)
   read (readline(122:125),'(i4)') master_meta_end_tavg(line_counter)
   master_meta_id(line_counter)=readline(127:146)
   master_meta_key(line_counter)=readline(151:158)
   master_meta_wmo(line_counter)="XXXXX"

   do counter=1,wmo_num_stations 
     if(trim(adjustl(master_meta_recid(line_counter))) .eq. trim(wmo_id1(counter))) then
       master_meta_wmo(line_counter)= trim(wmo_id2(counter))
       exit
     endif
   enddo
 enddo
 close(1)

!*******************************************************************************
! Read in List of Datasets that need to be updated
!*******************************************************************************
 write(11,*) "READING IN LIST OF DATASETS"
 call flush(11)

 dataset_update_file = trim(main_directory)//'update/resources/update_sources-'//trim(adjustl(version))//'.txt'
 inquire(file=dataset_update_file,exist=file_exists)
 if (file_exists .eqv. .false.) stop "ERROR: cannot find file with list of UPDATED datasets"

 dataset_recommended_file = trim(main_directory)//'update/resources/databank_sources-'//trim(adjustl(version))//'.txt'
 inquire(file=dataset_recommended_file,exist=file_exists)
 if (file_exists .eqv. .false.) stop "ERROR: cannot find file with list of MERGE datasets"

 call file_recs(FILE_NAME=dataset_update_file,NUM_UNIT=20,NUM_LINES=number_of_datasets)

 allocate(dataset_name_lower(1:number_of_datasets))
 allocate(dataset_name_upper(1:number_of_datasets))
 allocate(dataset_position(1:number_of_datasets))

 counter=0
 open(unit=12,file=dataset_update_file,status='old')
 do
   read(12,'(a)',IOSTAT=ioerror) readline
   if (ioerror==-1) exit

   counter=counter+1
   
   call change_string_case(INPUT_STRING=readline,WHICH="LOWER",FINAL_STRING=dataset_name_lower(counter)) 
   call change_string_case(INPUT_STRING=readline,WHICH="UPPER",FINAL_STRING=dataset_name_upper(counter))

   ! Find position in source hierarchy
   open(unit=14,file=dataset_recommended_file,status='old')
   do
     read(14,'(a)',IOSTAT=ioerror) sourceline
     if (ioerror==-1) exit

     if(trim(adjustl(sourceline(4:25))) .eq. dataset_name_lower(counter)) then
       dataset_position(counter) = sourceline(1:2)
     endif
    
   enddo
   close(14)
 enddo
 close(12)

!*******************************************************************************
! Read in CANDIDATE Inventories
!*******************************************************************************
 write(11,*) "READING IN CANDIDATE INVENTORIES"
 call flush(11)

 source_directory = trim(adjustl(main_directory))//"update/tmp/" 

 ! Go through Inventories once to allocate
 allocate(candidate_num_stations(1:number_of_datasets))
 do dataset_counter=1,number_of_datasets
    
   candidate_metafile=trim(source_directory)//trim(dataset_name_lower(dataset_counter))//'/INVENTORY_'//                           &
                      trim(dataset_name_upper(dataset_counter))//'_monthly_stage2'
                      
   inquire(file=candidate_metafile,exist=file_exists)
   if (file_exists .eqv. .false.) stop "ERROR: cannot find CANDIDATE INVENTORY infile"
   call file_recs(FILE_NAME=candidate_metafile,NUM_UNIT=20,NUM_LINES=number_of_lines)

   candidate_num_stations(dataset_counter) = number_of_lines
 enddo

 allocate(candidate_meta_id(1:number_of_datasets,1:maxval(candidate_num_stations(:))))
 allocate(candidate_meta_name(1:number_of_datasets,1:maxval(candidate_num_stations(:))))
 allocate(candidate_meta_lat(1:number_of_datasets,1:maxval(candidate_num_stations(:))))
 allocate(candidate_meta_lon(1:number_of_datasets,1:maxval(candidate_num_stations(:))))
 allocate(candidate_meta_elev(1:number_of_datasets,1:maxval(candidate_num_stations(:))))

 ! Go through Inventories a second time to collect
 do dataset_counter=1,number_of_datasets
    
   candidate_metafile=trim(source_directory)//trim(dataset_name_lower(dataset_counter))//'/INVENTORY_'//                           &
                      trim(dataset_name_upper(dataset_counter))//'_monthly_stage2'
                      
   inquire(file=candidate_metafile,exist=file_exists)
   if (file_exists .eqv. .false.) stop "ERROR: cannot find CANDIDATE INVENTORY infile"

   line_counter=0
   open(unit=1,file=candidate_metafile,status='OLD')
   do
     read(1,'(a1024)',IOSTAT=ioerror) readline
     if (ioerror==-1) exit
     line_counter=line_counter+1

     candidate_meta_id(dataset_counter,line_counter)=trim(dataset_position(dataset_counter))//'_'//readline(1:12)
     candidate_meta_name(dataset_counter,line_counter)=readline(14:43)
     read (readline(66:75),'(f10.4)') candidate_meta_lat(dataset_counter,line_counter)
     read (readline(77:86),'(f10.4)') candidate_meta_lon(dataset_counter,line_counter)
     read (readline(88:95),'(f8.2)') candidate_meta_elev(dataset_counter,line_counter)
     
   enddo
   close(1)

 enddo

!*******************************************************************************
! Run Analysis
!*******************************************************************************
 write(11,*) "RUNNING ANALYSIS AND WRITING UPDATED STAGE3 DATAFILES"
 call flush(11)

 do master_counter=1,master_num_stations

   master_datafile=trim(recommended_directory)//'results_merged/merge_'//trim(adjustl(master_meta_recid(master_counter)))//'_stage3'

   ! Read in data from master infile
   inquire(file=master_datafile,exist=file_exists)
   if (file_exists.eqv..false.) stop "ERROR: Cannot find MASTER DATA file"
   call file_recs(FILE_NAME=master_datafile,NUM_UNIT=20,NUM_LINES=number_of_lines)

   ! Reset Master Station Data/Metadata before reading in file
   do year_counter=1700,2030
     do month_counter=1,12

       master_data_name(year_counter,month_counter)=""
       master_data_lat(year_counter,month_counter)=-999.9
       master_data_lon(year_counter,month_counter)=-999.9
       master_data_elev(year_counter,month_counter)=-999.9
       master_data_year(year_counter,month_counter)=""
       master_data_month(year_counter,month_counter)=""
       master_data_day(year_counter,month_counter)=""
       master_data_source_update(year_counter,month_counter)=""     
       master_data_stage2_flag(year_counter,month_counter)=""  

       do element_counter=1,3
         master_data_value(year_counter,month_counter,element_counter)=-9999.  
         master_data_source(year_counter,month_counter,element_counter)="XXXXXXXX"  
       enddo
     enddo
   enddo

   line_counter = 0
   open(unit=1,file=master_datafile,status='OLD')
   do
     read(1,'(a1024)',IOSTAT=ioerror) readline
     if (ioerror==-1) exit
     line_counter=line_counter+1

     read (readline(63:66),'(i4)') year_counter
     read (readline(67:68),'(i2)') month_counter

     ! METADATA (By Month)
     master_data_name(year_counter,month_counter) = readline(1:30)
     read (readline(32:41),'(f10.4)') master_data_lat(year_counter,month_counter)
     read (readline(43:52),'(f10.4)') master_data_lon(year_counter,month_counter)
     read (readline(54:61),'(f8.2)') master_data_elev(year_counter,month_counter)
     master_data_year(year_counter,month_counter)=readline(63:66)
     master_data_month(year_counter,month_counter)=readline(67:68)
     master_data_day(year_counter,month_counter)=readline(69:70)
     master_data_yrmo=master_data_year(year_counter,month_counter)//master_data_month(year_counter,month_counter)
     read (master_data_yrmo,'(i6)') master_latest_month
     master_data_stage2_flag(year_counter,month_counter) = readline(90:132)

     !TMAX DATA (By Month)
     read (readline(72:76),'(i5)') master_data_value(year_counter,month_counter,tmax)
     master_data_source(year_counter,month_counter,tmax) = readline(134:141) 

     !TMIN DATA (By Month) 
     read (readline(78:82),'(i5)') master_data_value(year_counter,month_counter,tmin)
     master_data_source(year_counter,month_counter,tmin) = readline(143:150) 

     !TAVG DATA (By Month)
     read (readline(84:88),'(i5)') master_data_value(year_counter,month_counter,tavg)
     master_data_source(year_counter,month_counter,tavg) = readline(152:159) 

   enddo
   close(1)
   
   ! Go through each source that needs to be updated and see if there is a match between a candidate and master
   do dataset_counter=1,number_of_datasets
     
     station_match = .false.
     do candidate_counter=1,candidate_num_stations(dataset_counter)
       
       if(trim(master_meta_id(master_counter)(1:2)) .eq. "01" .and. trim(dataset_name_lower(dataset_counter)) .eq. "ghcnd") then

         ! Master station originated from GHCN-D. Look for an identical match
         if(trim(master_meta_id(master_counter)(4:20)) .eq. trim(candidate_meta_id(dataset_counter,candidate_counter)(4:20))) then

           !GHCND is a one-to-one match with ID's
           station_match = .true.
           match_counter = candidate_counter
           candidate_datafile = trim(source_directory)//trim(dataset_name_lower(dataset_counter))//'/'//                           &
                                trim(dataset_name_lower(dataset_counter))//'_'//                                                   &
                                trim(candidate_meta_id(dataset_counter,candidate_counter)(4:20))//'_monthly_stage2'
           exit
         endif

       elseif(trim(master_meta_wmo(master_counter)) .eq. trim(candidate_meta_id(dataset_counter,candidate_counter)(4:8))) then 

         !one to one match with WMO ID from Cross Reference File
         station_match = .true.
         match_counter = candidate_counter
         candidate_datafile = trim(source_directory)//trim(dataset_name_lower(dataset_counter))//'/'// &
                              trim(dataset_name_lower(dataset_counter))//'_'// &
                              trim(candidate_meta_id(dataset_counter,candidate_counter)(4:20))//'_monthly_stage2'
         
         exit

       else
         station_match = .false.
       endif
     enddo

     if(station_match) then ! look for new data to append
       ! Read in data from candidate infile
       inquire(file=candidate_datafile,exist=file_exists)
       if (file_exists.eqv..false.) then
         write(*,*) "ERROR: Cannot find CANDIDATE DATA file"
         write(*,*) candidate_datafile
         stop
       endif
       open(unit=1,file=candidate_datafile,status='OLD')
       do
         read(1,'(a1024)',IOSTAT=ioerror) readline
         if (ioerror==-1) exit

         read (readline(63:66),'(i4)') year_counter
         read (readline(67:68),'(i2)') month_counter
  
         candidate_data_year=readline(63:66)
         candidate_data_month=readline(67:68)
         candidate_data_day=readline(69:70)
         candidate_data_yrmo=candidate_data_year//candidate_data_month
         read (candidate_data_year,'(i4)') candidate_latest_year
         read (candidate_data_yrmo,'(i6)') candidate_latest_month
         read (readline(72:76),'(i5)') candidate_data_tmax
         read (readline(78:82),'(i5)') candidate_data_tmin
         read (readline(84:88),'(i5)') candidate_data_tavg
         candidate_data_stage2_flag = readline(90:132)
         candidate_data_source_flag = readline(136:136)

         monthly_difference = (current_month - candidate_latest_month) - ( (current_year - candidate_latest_year) * 88)

         append_data=1 
         ! First Run of GHCN-D, All Flags but "S"
         if(dataset_counter==1 .and. trim(adjustl(candidate_data_source_flag)) .eq. "S") then 
           append_data=0
         endif

         ! Second Run of GHCN-D, Only "S"
         if(dataset_counter==7 .and. trim(adjustl(candidate_data_source_flag)) .ne. "S") then
           append_data=0
         endif
         
         if(monthly_difference > 0 .and. append_data==1) then

           write_log = .false.

           ! Add Metadata (only if respective month is missing) 
           if(master_data_name(year_counter,month_counter) .eq. "") then
             master_data_name(year_counter,month_counter) = candidate_meta_name(dataset_counter,match_counter)
           endif
           if(master_data_lat(year_counter,month_counter) == -999.9) then
             master_data_lat(year_counter,month_counter) = candidate_meta_lat(dataset_counter,match_counter)
           endif
           if(master_data_lon(year_counter,month_counter) == -999.9) then
             master_data_lon(year_counter,month_counter) = candidate_meta_lon(dataset_counter,match_counter)
           endif
           if(master_data_elev(year_counter,month_counter) == -999.9) then
             master_data_elev(year_counter,month_counter) = candidate_meta_elev(dataset_counter,match_counter)
           endif
           if(master_data_year(year_counter,month_counter) .eq. "") then
             master_data_year(year_counter,month_counter) = candidate_data_year
           endif
           if(master_data_month(year_counter,month_counter) .eq. "") then
             master_data_month(year_counter,month_counter) = candidate_data_month
           endif
           if(master_data_day(year_counter,month_counter) .eq. "") then
             master_data_day(year_counter,month_counter) = candidate_data_day
           endif
           if(master_data_stage2_flag(year_counter,month_counter) .eq. "") then
             master_data_stage2_flag(year_counter,month_counter) = candidate_data_stage2_flag
           endif

           ! TMAX
           if(candidate_data_tmax /= -9999 .and. master_data_value(year_counter,month_counter,tmax) == -9999) then
             write_log = .true.
             master_data_value(year_counter,month_counter,tmax)=candidate_data_tmax
             master_data_source(year_counter,month_counter,tmax)=trim(adjustl(dataset_position(dataset_counter)))//"UPDATE"
             master_data_stage2_flag(year_counter,month_counter)(17:19)=candidate_data_stage2_flag(17:19) 
             master_data_stage2_flag(year_counter,month_counter)(29:31)=candidate_data_stage2_flag(29:31) 
           endif
     
           if(master_data_value(year_counter,month_counter,tmax) /= -9999) then
             read (master_data_year(year_counter,month_counter),'(i4)') master_meta_end_tmax(master_counter)
           else
             master_data_source(year_counter,month_counter,tmax)="XXXXXXXX"
           endif

           ! TMIN
           if(candidate_data_tmin /= -9999 .and. master_data_value(year_counter,month_counter,tmin) == -9999) then
             write_log = .true.
             master_data_value(year_counter,month_counter,tmin)=candidate_data_tmin
             master_data_source(year_counter,month_counter,tmin)=trim(adjustl(dataset_position(dataset_counter)))//"UPDATE"
             master_data_stage2_flag(year_counter,month_counter)(21:23)=candidate_data_stage2_flag(21:23) 
             master_data_stage2_flag(year_counter,month_counter)(33:35)=candidate_data_stage2_flag(33:35) 
           endif
     
           if(master_data_value(year_counter,month_counter,tmin) /= -9999) then
             read (master_data_year(year_counter,month_counter),'(i4)') master_meta_end_tmin(master_counter)
           else
             master_data_source(year_counter,month_counter,tmin)="XXXXXXXX"
           endif

           ! TAVG
           if(master_data_value(year_counter,month_counter,tmax) /= -9999 .and.                                         & 
              master_data_value(year_counter,month_counter,tmin) /= -9999) then
             master_data_value(year_counter,month_counter,tavg) =((master_data_value(year_counter,month_counter,tmax) + & 
                                                                  master_data_value(year_counter,month_counter,tmin)) / 2)
             master_data_source(year_counter,month_counter,tavg)="XXUPDATE"
           elseif(candidate_data_tavg /= -9999 .and. master_data_value(year_counter,month_counter,tavg) == -9999) then
             write_log = .true.
             master_data_value(year_counter,month_counter,tavg)=candidate_data_tavg
             master_data_source(year_counter,month_counter,tavg)=trim(adjustl(dataset_position(dataset_counter)))//"UPDATE"
             master_data_stage2_flag(year_counter,month_counter)(25:27)=candidate_data_stage2_flag(25:27) 
             master_data_stage2_flag(year_counter,month_counter)(37:39)=candidate_data_stage2_flag(37:39) 
           else
             if(master_data_value(year_counter,month_counter,tavg) == -9999) then
               master_data_value(year_counter,month_counter,tavg)=-9999
             endif
           endif

           if(master_data_value(year_counter,month_counter,tavg) /= -9999) then
             read (master_data_year(year_counter,month_counter),'(i4)') master_meta_end_tavg(master_counter)
           else
             master_data_source(year_counter,month_counter,tavg)="XXXXXXXX"
           endif

           if(write_log) then ! Only write to log cases where TMAX/TMIN/TAVG are appended
             write(11,'(a8,1x,a12,1x,a20,1x,a25,1x,a20,1x,i6,1x,i6,1x,i3,1x,i3)') trim(adjustl(master_meta_key(master_counter))), &
                                                                         trim(adjustl(master_meta_recid(master_counter))),        &
                                                                         trim(master_meta_id(master_counter)),                    &
                                                                         dataset_name_lower(dataset_counter),                     &
                                                                         trim(candidate_meta_id(dataset_counter,match_counter)),  &
                                                                         master_latest_month,candidate_latest_month,              &
                                                                         monthly_difference,dataset_counter
             call flush(11)
           endif
         endif
       enddo
       close(1)
     endif
   enddo

   ! Output Updated DATA
   out_datafile=trim(main_directory)//'update/results_merged/merge_'//trim(adjustl(master_meta_recid(master_counter)))//'_stage3'
   open(unit=23,file=out_datafile,status='replace')

   do year_counter=1700,2030
     do month_counter=1,12

       if(master_data_value(year_counter,month_counter,tmax) /= -9999 .or. &
         master_data_value(year_counter,month_counter,tmin) /= -9999 .or. &
         master_data_value(year_counter,month_counter,tavg) /= -9999) then

         write(23,23) master_data_name(year_counter,month_counter),master_data_lat(year_counter,month_counter), &
                      master_data_lon(year_counter,month_counter),master_data_elev(year_counter,month_counter), &
                      master_data_year(year_counter,month_counter),master_data_month(year_counter,month_counter), &
                      master_data_day(year_counter,month_counter),master_data_value(year_counter,month_counter,tmax), &
                      master_data_value(year_counter,month_counter,tmin),master_data_value(year_counter,month_counter,tavg), &
                      master_data_stage2_flag(year_counter,month_counter),master_data_source(year_counter,month_counter,tmax), &
                      master_data_source(year_counter,month_counter,tmin),master_data_source(year_counter,month_counter,tavg)

       endif

     enddo
   enddo

   close(23)
   23 format(a30,1x,f10.4,1x,f10.4,1x,f8.2,1x,a4,a2,a2,1x,3(i5,1x),a43,1x,a8,"/",a8,"/",a8)
 enddo

!*******************************************************************************
! Output Updated METADATA
!*******************************************************************************
 write(11,*) "WRITING UPDATED STAGE3 INVENTORY"
 call flush(11)

 out_metafile=trim(main_directory)//'update/results_merged/INVENTORY_monthly_merged_stage3'
 open(unit=25,file=out_metafile,status='replace')
 do line_counter=1,master_num_stations

    write(25,25) master_meta_recid(line_counter),master_meta_name(line_counter),master_meta_country(line_counter),                 &
                 master_meta_lat(line_counter),master_meta_lon(line_counter),master_meta_elev(line_counter),                       &
                 master_meta_start_tmax(line_counter),master_meta_end_tmax(line_counter),master_meta_start_tmin(line_counter),     &
                 master_meta_end_tmin(line_counter),master_meta_start_tavg(line_counter),master_meta_end_tavg(line_counter),       &
                 master_meta_id(line_counter),"REC",master_meta_key(line_counter)
 enddo
 close(25)
 25 format(a12,1x,a30,1x,a20,1x,f10.4,1x,f10.4,1x,f8.2,1x,6(i4,1x),a20,1x,a,a8)
 
!*******************************************************************************
! Program Completed
!*******************************************************************************
write (11,*) "DONE!"
call date_and_time(DATE=current_date,VALUES=current_time)
write(11,'(a,1x,a8,1x,i2.2,a,i2.2,a,i2.2)') "END:",current_date,current_time(5),":",current_time(6),":",current_time(7)
close(11)
close(13)
end program merge_update
