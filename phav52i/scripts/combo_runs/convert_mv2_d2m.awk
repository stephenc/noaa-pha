BEGIN {
  # simple routine to generate the 3-flag GHCNM format from
  #   GHCN-Mv2 (with 3-flag) from Byron
  
  print "PROC: "PROC
  print "ODIR: "ODIR
  print "DEBUG: "DEBUG
  
  if( PROC == "" || ODIR == "")
  {
    print "Convert GHCNMv2 to GHCND2M 3-flag data format"
    print ""
    print " where PROC is the output process (raw, tob, adj, etc.)"
    print "       ODIR is the output directory"
    print " Example:"
    print "  gawk -f convert_mv2_d2m.awk -v PROC=raw"
    print "    -v ODIR=../global v2.mean_cone.nodups"
    exit
  }

  # data format of GHCNMv2 and GHCNMv2five
  istrt = 20
  iinc = 8
  ilen = 5
  lstn = ""
  debug = 0
}  

{
  # parse the GHCNMv2 data records
  stn=substr($0, 1, 11)
  yr=substr($0, 12, 4)
  elem=substr($0, 16, 4)
  # determine element file for the record
  if(elem == "PRCP")
  {
    iel = "prcp"
  }
  else if(elem == "TMAX")
  {
    iel = "tmax"
  }
  else if(elem == "TMIN")
  {
    iel = "tmin"
  }
  else if(elem == "TAVG")
  {
    iel = "tavg"
  }
  else
  {
    print "Unknown element: ", stn, yr, elem
    next
  }
  
  # if new station or new element, make output file
  if(stn != lstn || iel != lel)
  {
    if(lstn != "")
    {
      close(ofile)
    }
    else
    {
      # first time through - ensure process directory is ready
      cmd = "mkdir -p "ODIR"/"PROC
      system(cmd)
    }
    lstn = stn
    lel = iel
    ofile = ODIR"/"PROC"/"stn"."PROC"."iel
    if(debug > 0) print PROC"/"stn"."PROC"."iel > DEBUG
    # initialize station data file
    cmd = "rm -f "ofile
    system(cmd)
    system("")
  }
  
  # fix the spacing (add blank character to beginning of data value field)
  printf("%s %s", stn, yr) >> ofile
  for(im = 1; im <= 12; im++)
  {
    ic = istrt + ((im - 1) * iinc)
    printf(" %s", substr($0, ic, iinc)) >> ofile
  }
  printf("\n") >> ofile
}
