#!/bin/sh

# All directories are relative to 

# Version   Date     Modules                     Description
#   52d   20090520  UCPM24pM21qC6pL1Si6pR5p      Final GHCNMv2five cron
#   52g   20100304  UCPM25M22aC7L1Si7R6           Merge with initial USHCN 52d tarball
#   52h   20110613  UCPM25cM22aC7L1Si7cR6b.lf95   Skyline adaptation
#   52i   20120815  UCPM25dM22bC7aL1Si7cR6b.lf95  RothenBerger fixes

# 9Mar2012 - change script (and compile) names to PHA version

lf95opt=0
# one of the few dependencies around...
# CO2 does NOT have GFORTRAN
# Must use LF95....
if [ $HOSTNAME == "co2.ncdc.noaa.gov" ]
  then
  lf95opt=1
fi  

reqparm=`expr 2`
optparm1=`expr $reqparm + 1`

if [ $# -lt $reqparm ]
then
  echo "Compile the USHCN inhomog application"
  echo "  USAGE: current_version.csh binfile comptype"
  echo "    where binfile is the output binary executable"
  echo "          comptype is FAST or DBUG"
  echo ""
  exit
fi

cwd=`pwd`
echo "Entering :" $0 $*
scripts=`dirname $0`
echo "Compile scripts directory: $scripts"
cd $scripts
cd ../..
sbase=`pwd`
cd $cwd

echo "CODEBASE: ${sbase}"
src=$sbase/source_expand
# MAKE binary location is this directory
bin=.

binfile=$1
comptype=$2

cwd=`pwd`

# expand the source code with the include code
cd $src
rm -f *.for
for file in *.f
  do
  gawk -f $scripts/expand_source.awk $file > ${file}or
  echo $file
done

# set the option for FAST or DBUG compilation
if [ $comptype == "DBUG" ]
  then
    copt="-g -fbounds-check"
  if [ $lf95opt == 1 ]
    then
   copt="-g --trap --chk --chkglobal "
  fi  
#   copt="--chk s "
elif [ $comptype == "FAST" ]
  then 
#  set copt = "-O2 -funroll-loops"
  copt="-O2 "
else
  echo "Invalid comtype: $comptype - STOPPING"
  exit  
fi

# use --info when desired
dopt=""
# lf95 --
#  dopt=" --fix --co --trace"
# dopt=" --fix"

if [ $lf95opt == 1 ]
  then
  lf95 $dopt $copt --model medium restart.skymod.f95 confirmdisp.skymod.f95 \
    ucpmonthly.v25d.for splitmerge.v22b.for chgptmodels.v7a.for \
    SHAPinp.v7c.for read_write.mthly.v6b.for acovf.for \
    skyline.v1.for -o $bin/$binfile 2> lf95.err
else    
  gfortran $dopt $copt -fno-sign-zero -mcmodel=medium restart.skymod.f95 confirmdisp.skymod.f95 \
    ucpmonthly.v25d.for splitmerge.v22b.for chgptmodels.v7a.for \
    SHAPinp.v7c.for read_write.mthly.v6b.for acovf.for \
    skyline.v1.for -o $bin/$binfile 2> lf95.err
fi    

echo "Leaving :" $0
cd $cwd

