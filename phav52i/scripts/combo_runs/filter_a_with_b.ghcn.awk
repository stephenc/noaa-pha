BEGIN {
  if(STNLIST == "")
  {
    print "Filter Input file through STNLIST"
    print " Assumes:"
    print "  1) the key is the first field"
    print "  2) the files are sorted in first field order"
    print "  EXAMPLE: gawk -f filter_a_with_b.awk -v STNLIST=NWS_subset \\ "
    print "            pe6_pe7.pe7.min_6.sada > pe6_pe7.pe7.min_NWS.sada"
    exit
  }  
  stn2 = 0
  dbug = 0
  ninpnot = 0
  nfiltin = 0
  nfiltnot = 0
  print "STNLIST:",STNLIST
}

{
  if(NR == 1) 
  {
    errfile = FILENAME ".err"
    filtin = FILENAME".input_in_stnlist"
    filtnot = FILENAME".stnlist_not_input"
    inpnot = FILENAME".input_not_stnlist"
  }
  # the candidate station in the first field
  stn1 = substr($1,1,11)
  if(dbug > 0) print "stn1", stn1
  rec1 = $0
  
  while(1==1)
  {
    if(stn2 > stn1)
    {
      if(dbug > 0) print "stn2 > stn1"
      print rec1 > inpnot
      ninpnot++
      break
    }      
    else if(stn2 == stn1)
    {
      if(dbug > 0) print "stn2 == stn1"
      print rec1 > filtin
      nfiltin++
      if((getline < STNLIST) <= 0)
      {
        stn2 = "ZZZZZZZZZZZ"
        if(dbug > 0) print "EOF stn2", stn2
      }
      else  
      {
        stn2 = substr($1,1,11)
        rec2 = $0
        if(dbug > 0) print "stn2", stn2
      }  
      break
    }  
    else if(stn2 != 0)
    {
      if(dbug > 0) print "stn2 < stn1"
      print rec2 > filtnot
      nfiltnot++
    }  
    if((getline < STNLIST) > 0)
    {
      stn2 = substr($1,1,11)
      rec2 = $0
      if(dbug > 0) print "stn2", stn2
    }
    else
    {  
      stn2 = "ZZZZZZZZZZZ"
      if(dbug > 0) print "EOF stn2", stn2
    }  
  }
}  

END {
  if(stn1 > stn2)
  {
    if(dbug > 0) print "END stn2 < stn1"
    print rec1 > inpnot
    ninpnot++
  }
  else if(stn2 > stn1 && stn2 != "ZZZZZZZZZZZ")
  {  
    if(dbug > 0) print "END stn2 > stn1"
    print rec2 > filtnot
    nfiltnot++
  }  
  while((getline < STNLIST) > 0)
  {
    stn2 = $1
    rec2 = $0
    if(dbug > 0) print "END stn2", stn2
    print rec1 > filtnot
    nfiltnot++
  }
  print "Stations in Final station list: ", nfiltin
  print "            input not stnlist:  ", ninpnot
  print "            stnlist not input:  ", nfiltnot
}    
