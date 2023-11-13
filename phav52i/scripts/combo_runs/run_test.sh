#!/bin/sh

echo "-------------------------------"
echo ">>>>>  `date` Entering:" $0 $@

# WARNING After 1/17/2011 running RAW for MAX/MIN
# WARNING Until 1/18/2011 Running TOB and TAVG 

# set number of required parameters
reqparm=`expr 6`

if [ $# -lt $reqparm ]
then
  echo "Not all command line input defined. Please enter"
  echo '  UCP routine and data output version defined in calling script'
  echo ''
  echo '  $1 is the data ID for the current test run (world6, us120217, etc.)'
  echo '  $2 is the element (tmax/tmin/tavg)'
  echo '  $3 is the process level (raw/tob)'
  echo '  $4 is the first combo to process'
  echo '  $5 is the last combo to process'
  echo '  $6 - I or P or N processing options'
  echo '        I parses & converts GHCNMv2 meta/data, dist & corr, UCP & Fill'
  echo '        P regenerates the dist & corr, UCP & Fill'
  echo '        N just UCP & Fill'
  echo '       Process        I     P    N'
  echo '       Parse&Convert  Y     -    -'
  echo '       Dist & Corr    Y     Y    -'
  echo '       UCP & Fill     Y     Y    Y'
  exit
fi

# ---- Check all environment variable dependencies here -----
echo "TESTBASE: ${TESTBASE?" undefined: see project script: stop"}"
echo "TESTDATA: ${TESTDATA?" undefined: see project script: stop"}"
echo "phamods: ${phamods?" undefined: see project script: stop"}"
echo "ver: ${ver?" undefined: see project script: stop"}"

datatag=$1
elem=$2
proc=$3
begcombo=$4
endcombo=$5
IPN=$6

dtag=`date +%y%m%d%H%M`
cwd=`pwd`

# read in the testdata configuation file
source ${TESTDATA}/$datatag.conf

# check for available elements
ifound=0
maxelem=0
minelem=0
genelem="none"
for ielem in $elems
  do 
  if [ $elem == $ielem ]
    then 
    ifound=1
    break
  fi
  if [ $ielem == "tmax" ]; then maxelem=1; fi
  if [ $ielem == "tmin" ]; then minelem=1; fi
done
if [ $ifound == 0 ]
  then
  # if the individual elements DTR or AVG have been requested but not found
  #   then if tmax & tmin exist - use them to calculate requested element
  if [[ $elem == "tdtr" && $maxelem -eq 1 && $minelem -eq 1 ]]
    then
    echo "Generate DTR from MAX/MIN"
    genelem=DTR
  elif [[ $elem == "tavg" && $maxelem -eq 1 && $minelem -eq 1 ]]
    then
    echo "Generate AVG from MAX/MIN"
    genelem=AVG
  else
    echo "Element: $elem not defined for Testdata: $datatag"
    echo " Only elements available are: $elems"
    exit
  fi  
fi

# check for available processing levels
ifound=0
for iproc in $procs
  do 
  if [ $proc == $iproc ]
    then 
    ifound=1
    break
  fi
done
if [ $ifound == 0 ]
  then
  echo "Process: $proc not defined for Testdata: $datatag"
  echo " Only processes available are: $procs"
  exit
fi

# --------------- setup local output directory definitions ---------------
outdir=$basedir/output
mkdir -p $outdir
corrdir=$basedir/corr
mkdir -p $corrdir
metadir=$basedir/meta
mkdir -p $metadir

# PHA executable definitions and defaults
pharun=${phamods}.FAST.MLY.${phaext}
echo "PHA exec: $pharun"

if [ $proc == "tob" ]
  then
  # Incoming Peter World or US TOB 
  preproc=c
else  
  # Incoming US RAW
  preproc=r
fi

bin=$TESTBASE/bin
lib=$TESTBASE/scripts
combodir=$TESTDATA/mixboards

if [ $elem == "tmax" ]
  then
  ielem=1
elif [ $elem == "tmin" ]
  then
  ielem=2
elif [ $elem == "tavg" ]
  then
  ielem=3
else
  echo "Unknown Element: $elem"  
fi
echo "Element: $elem Elnum: $ielem"

if [ $datatype == "P" ]
  then
  # Peters Worlds datatype may be used for any dataset that
  #   has already been converted into the PHA input format
  #   and directory structure.
  #  That is:
  #   $TESTDATA
  #   |-- $dataset/
  #     |-- $datatag.conf
  #     `-- $datatag/
  #         |-- meta/
  #           |   `-- $datatag_stnlist
  #           `-- monthly/
  #               `-- raw/
  #                   |-- $stn1.raw.$elem
  #                   `-- ....
  #
  # Peters Worlds may also have an input history file
  mattmeta=$corrdir/pwmeta.$datatag.$elem.$dtag
  ln -s $histfile $mattmeta
  EXTopt="-m  $mattmeta"
elif [ $datatype == "G" ]
  then
  # GHCNMv3 datatype consists of two files
  #  .dat contains all of the monthly data in GHCNMv3 form sorted by
  #            station, year, element
  #  .inv contains the station list in GHCNMv3 form
  # These files are found at:
  #   $TESTDATA
  #   |-- $dataset/
  #       |-- $datatag.conf
  #       `-- $datatag/
  #           |-- ingest/
  #           |   `-- temp/
  #           |       |-- ~.dat
  #           |       `-- ~.inv
  #
  #           The following process will fill out
  #           |-- meta/
  #           |   `-- $datatag_stnlist
  #           `-- monthly/
  #               `-- raw/
  #                   |-- $stn1.raw.$elem
  #                   `-- ....

  if [ $IPN == "I" ]
    then
    incoming=$basedir/ingest
    # check to see if there is more than one (or none)
    currdats=( `find $incoming/temp -name "*.dat" -print` )
    currinvs=( `find $incoming/temp -name "*.inv" -print` )
    ncurrdats=${#currdats[*]}
    ncurrinvs=${#currinvs[*]}
    if [[ $ncurrdats != 1 || $ncurrinvs != 1 ]]
      then 
      echo -e "\nMust be one and only one dat and inv files: $ncurrdats $ncurrinvs\n"
      exit 1
    fi

    currinv=${currinvs[0]}
    currdat=${currdats[0]}
  
    echo -e "\n Convert data to GHCNMv2 format"
    # 20091019 - Byron file has 3-flags and all elements in one file
    gawk -f $lib/convert_mv2_d2m.awk -v PROC=raw -v ODIR=$candata \
      -v DEBUG=$outdir/convert_mv2_d2m.$dataset.log ${currdat}
    
    echo -e "\n Convert\split GHCNMv3 Station list into Element meta files"
    #  use data files to split into MXMN, TAVG, & PRCP files
    rm -f $metafile
  
    invlog=$corrdir/gen_meta.$datatag.$elem.$dtag.inv
    rm -f $invlog
    outlog=$outdir/gen_meta.$datatag.$elem.$dtag.log
    rm -f $outlog

    #NOTE: gen_meta generates the number of POR years for each element 
    gawk -f $lib/gen_meta.awk -v META=$stnlist -v PROC=raw -v IDIR=$candata \
    -v REG=$region -v DEBUG=$invlog ${currinv} > $outlog
  else
    echo -e "\n GHCNMv2 Parsing and conversion skipped"
    # use data from previous run
    outlog=$outdir/gen_meta.$datatag.$elem.*.log
  fi  

  # set the POR years per element
  `tail -4 $outlog`
    
  echo -e "\n From data TMAXYRS: $TMAXYRS"
  echo "From config maxyrs: $maxyrs"
     
elif [ $datatype == "U" ]
  then
  # Ensure USHCN candidates reset each combo (may change in dist-corr loop)
  EXTopt=""
else
  echo -e "\nDatatype: $datatype undefined - stopping"
  exit 1
fi

# --------------------- Generate DTR or TAVG -----------------
if [ $genelem != "none" ]
  then
  echo -e "\n`date` ----   generate $genelem files and from MAX/MIN"
  gawk -f $lib/gen_avg_dtr.3flg.awk -v byear=$begyr -v eyear=$endyr \
    -v ddir=$candata -v vproc=raw -v otype=$genelem -v ometa=$stnlist.$elem \
    $stnlist.tmax >> $outdir/gen_avg_dtr.log 2>&1
fi

  
jcand=$icand
for (( icombo=$begcombo ; $icombo <= $endcombo; icombo=`expr $icombo + 1` ))
  do
  oproc=$preproc`printf %02d $icombo`

  # ensure the output directories are created
  mkdir -p $candata/WMs.$oproc 
  mkdir -p $netdata/WMs.$oproc 
  mkdir -p $candata/FLs.$oproc 
  mkdir -p $netdata/FLs.$oproc

  # define mixboard parameter combination as environment variables
  gawk '{print"export "$0}' $combodir/combination$icombo.txt > $outdir/test.$oproc
  source $outdir/test.$oproc
  
  # check the Processing option for Dist & Corr processing
  if [ $IPN == "I" -o $IPN == "P" ]
    then

    # the distance & correlation routine "loop" until there are at least
    #   8 years of data is a station, and each station has at least 5 
    #   neighbors. Other stations are removed from processing.
    metafile=$corrdir/meta.$datatag.$elem.$oproc.$dtag
    corrfile=$corrdir/corr.$datatag.$elem.$oproc.$dtag
    distfile=$corrdir/dist.$datatag.$elem.$oproc.$dtag
    stnfile=$corrdir/stnlist.$datatag.$elem.$oproc.$dtag

    # initialize first metafile using all incoming stations
    cat $stnlist.$elem > $metafile.1
    sleep 2

    # number of initial stations
    numstn=`wc -l $metafile.1 | cut -f 1,1 -d " " -`

    # take the current metafile and see if all the stations are usable
    cd $corrdir
    for (( iloop=1;iloop <= 10;iloop=$iloop+1 ))
      do
      echo "Dist-Corr Iteration: $iloop"
      echo "Generate distance network file"
      distlog=$outdir/dist.$datatag.$elem.$oproc.$dtag.log
      $bin/ushcn_dist.v5a.combo -u $jcand -m $metafile.$iloop \
        -o ${distfile}.$iloop > $distlog.$iloop
      echo "Generate correlation network file"
      corrlog=$outdir/corr.$datatag.$elem.$oproc.$dtag.log
      $bin/ushcn_corr.v5a.combo $endyr $ielem -u $jcand -p $iproc -n 0 \
        -D 1 -d ${distfile}.$iloop -m $metafile.$iloop -o $corrfile.$iloop \
        -C $candata -N $netdata > $corrlog.$iloop
      echo "Make list of usable stations"
      goodstns=$stnfile.$iloop
      gawk -f $lib/min_neigh.awk -v MINSTN=6 -v ICAND=$jcand $corrfile.$iloop > \
        $goodstns
      source  $corrfile.$iloop.cmd 
      newstn=`wc -l $goodstns | cut -f 1,1 -d " " -`
      if [ $numstn == $newstn ]
        then
        echo "Final Network generated - loop: $iloop  Cand: $jcand  Total: $newstn"
        break
      fi
      echo "Remove unusable stations and loop back Cand: $jcand  Total: $newstn"
      filtlog=$outdir/stnlist.$datatag.$elem.$oproc.$dtag.filter
      gawk -f $lib/filter_a_with_b.ghcn.awk -v STNLIST=$goodstns \
        $metafile.$iloop > $filtlog.$iloop
      (( nloop=$iloop+1 ))   
      ln -s $metafile.$iloop.input_in_stnlist $metafile.$nloop
      numstn=$newstn
    done

    cp $metafile.$iloop $metafile
    cp $distfile.$iloop $distfile
    cp $corrfile.$iloop $corrfile
    cp $distlog.$iloop $distlog
    cp $corrlog.$iloop $corrlog
    # keep all Filter logs showing removed stations
    # cp $filtlog.$iloop $filtlog

    rm $metafile.?
    rm $distfile.?
    rm $corrfile.?
    rm $corrfile.?.cmd
    rm $stnfile.?
    rm $metafile.?.input_in_stnlist
    rm $distlog.?
    rm $corrlog.?
    # rm $filtlog.?
    cd $cwd
    
  else

    echo -e "\n Skip Dist-Corr Iteration - use previous run"
    # the distance & correlation routine "loop" until there are at least
    #   8 years of data is a station, and each station has at least 5 
    #   neighbors. Other stations are removed from processing.
    metafile=$corrdir/meta.$datatag.$elem.$oproc.*
    corrfile=$corrdir/corr.$datatag.$elem.$oproc.*
    distfile=$corrdir/dist.$datatag.$elem.$oproc.*
    stnfile=$corrdir/stnlist.$datatag.$elem.$oproc.*
  
  fi
  
  logfile=$outdir/$pharun.$dtag.$elem.$datatag.$oproc.out
  echo "FULL UCP: $pharun world:$datatag oproc:$oproc dtag:$dtag"
  nice $bin/$pharun $maxyrs -Q 1.46 -S 18 -s 5 -P -o $oproc -d 1 -T 100 -l -t 1 \
    -e $ielem -p $iproc -q $iproc -c $jcand -n $corrfile -C $candata $candata \
    $EXTopt -N $netdata $netdata -O . > $logfile
  gzip $logfile  
    
  logfile=$outdir/fill2004p.$dtag.$elem.$datatag.$oproc.log
  echo "FULL FILL: $phamods world:$datatag elem:$ielem oproc:$oproc dtag:$dtag currtag:$currtag"
  nice $bin/ushcn_fill.v4p -d 0 -u $jcand -e $ielem -p WMs.$oproc -q WMs.$oproc \
    -o FLs.$oproc -n $corrfile -c $metafile -C $candata -N $netdata > $logfile
  gzip $logfile  
done
