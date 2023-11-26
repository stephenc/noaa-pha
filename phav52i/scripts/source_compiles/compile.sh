
#!/bin/bash

# upgraded parameter list to include ANNUAL as well as MONTHLY - see inhomog.parm.system.....incl

reqparm=`expr 4`
optparm1=`expr $reqparm + 1`

if [ $# -lt $reqparm ]
  then
  echo "Compile the USHCN inhomog application"
  echo "  USAGE: compile.csh UCPver comptype time_def net_def"
  echo "     where UCPver is the UCP version to compile"
  echo "           comptype is the compilation options for debugging or fast"
  echo "               Current comptype are:"
  echo "                DBUG - for symbolic links for debugging"
  echo "                FAST - for fast execution options"
  echo "           time_def is the time resolution of the data"
  echo "                ANN - for annual data"
  echo "                MLY - for monthly data"
  echo "           net_def is the network name used to define "
  echo "               the Network dependent array settings"
  echo "               Current net_def are:"
  echo "                GHCN"
  echo "                MATT"
  echo "                NORMALS"
  echo "                USHCN"
  echo ""
  exit
fi

echo "Entering :" $0 $*

version=$1
comptype=$2
time_def=$3
net_def=$4
echo " ----------- CCCCC"
echo "version: " $version
echo "comptype: " $comptype
echo "time_def: " $time_def
echo "net_def: " $net_def
echo " ----------- CCCCC"

cwd=`pwd`
echo "Entering :" $0 $*
scripts=`dirname $0`
echo "Compile scripts directory: $scripts"
cd $scripts
cd ../..
sbase=`pwd`
cd $cwd
  
echo "codebase set to: $sbase"
src=$sbase/source_expand
incl=$src/parm_includes
scripts=$sbase/scripts/source_compiles

# TRY to remember to update each time the most
#  up-to-date version of the USHCN 2002 update
#  routine is changed!!!!!

cp -v $incl/inhomog.parm.MTHLY.${net_def}.incl $src/inhomog.parm.mthly.incl

cp -v $incl/inhomog.parm.system.${time_def}.incl $src/inhomog.parm.system.mthly.incl

binfile=${version}.${comptype}.${time_def}.${net_def}

echo "Begin $binfile Compilation"
$scripts/$version.sh $binfile $comptype
echo "End $binfile Compilation"

# clean up
if [ "$comptype" = "FAST" ]
  then
  rm -f $src/*.o
  rm -f $src/*.for
  rm -f $src/*.mod
fi  

echo "Leaving :" $0
