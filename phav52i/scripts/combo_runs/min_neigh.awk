BEGIN {
  if( MINSTN == "" || ICAND == "" )
  {
    print "Test Corr-Neigh file for min stns and monitor number of candidates"
    print "  Not all parameters entered:"
    print ""
    print " -v MINSTN=minimum number of neighbors required"
    print " -v ICAND=number of USHCN type candidates in network"
    print ""
    print " WARNING - script resets ICAND environment variable"
    exit
  }
  minstn = MINSTN + 0
  icand = ICAND + 0
  remcand = 0
}
   
{
  if(NR%3 == 1)
  {
    if(NF >= minstn)
    {
      # print station if number of neighbors >= minstn
      print $1
    }
    else
    {
      # else do not print (i.e. remove from station list)
      #   and if a candidate, then reduce number of candidates
      istn = ((NR-1)/3)+1
      if(istn <= icand)
      {
        # print "Xcand: "$1
        remcand++
      }
      else
      {
        # print "Remove: "$1
      }  
    }  
  }
}

END {
  cmdfile=FILENAME".cmd"
  icand-=remcand
  # cmd="export icand="icand
  # print "Command: "cmd
  print "export icand="icand > cmdfile
}
