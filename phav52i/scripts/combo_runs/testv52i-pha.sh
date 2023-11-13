#!/bin/sh

# IMPORTANT - this is the self-defining script for the HCN PHA runs
#  Link this script in the GHCNM deployment directory containing
#  TESTBASE and TESTDATA
#  then it can be run from anywhere!!!
#   i.e.
#  in the deployment directory as "./testv52i-pha.sh"
#  from anywhere else as "$deploydir/testv52i-pha.sh"

# Figure out where the GHCNM is deployed
echo "Command: $0 $*"
export proj=`basename $0 ".sh"`
echo "Current project: $proj"

# Save current location & ensure baselocation is full path
cwd=`pwd`
cd `dirname $0`
export projpath=`pwd`
echo "Current project installed directory: $projpath"

# from here on - all directories are relative to PROJPATH

# Location of Scripts and Codes
export TESTBASE=$projpath/code
echo "Location of scripts & codes: $TESTBASE"

# Directory Structure
#   $testdata/$outtag/$ireg/WMs.$ver
#      "         "       " /FLs.$ver  
export TESTDATA=$projpath/data
echo "Location of data base: $TESTDATA"

# SKYLINE sparse matrix indexing for better resource usage
# RB fix version for impact analysis
export phamods=PHAv52i
export ver=52i
echo "Current version of TEST PHA: $phamods"
echo "Current version tag: $ver"

# pass on the command line parameters to the main run:
$projpath/code/scripts/run_test.sh $*

echo -e "\nDONE!"
echo "`date` <<<<< Leaving: " $0
