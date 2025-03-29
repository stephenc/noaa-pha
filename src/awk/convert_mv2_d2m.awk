BEGIN {
  # Updated for more flexible input.
  #  Originally only station-elem-year sort
  #  Expanded to include station-year-elem sort
  #
  # simple routine to generate the 3-flag GHCNM format from
  #   GHCN-Mv2 (with 3-flag)
  
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
    ielext = "prcp"
    iel = 4
  }
  else if(elem == "TMAX")
  {
    ielext = "tmax"
    iel = 1
  }
  else if(elem == "TMIN")
  {
    ielext = "tmin"
    iel = 2
  }
  else if(elem == "TAVG")
  {
    ielext = "tavg"
    iel = 3
  }
  else
  {
    print "Unknown element: ", stn, yr, elem
    next
  }
  
  # if new station or new element, make output file
  if(stn != lstn)
  {
    # close up all files from last station
    if(lstn != "")
    {
      for(ie = 1; ie <= 4; ie++)
      {
        if(nelem[ie] > 0)
        {
          close(ofile[ie])
          if(debug > 0) printf("close: %s %1d %3d\n", lstn, ie, nelem[ie])
        }
      }
    }
    else
    {
      # first time through - ensure process directory is ready
      cmd = "mkdir -p "ODIR"/"PROC
      system(cmd)
    }
    # initialize for new station
    lstn = stn
    for(ie = 1; ie <= 4; ie++)
    {
      nelem[ie] = 0
    }  
  }

  # define output file for this station/element
  ofile[iel] = ODIR"/"PROC"/"stn"."PROC"."ielext
  if(nelem[iel] == 0)
  {
    if(debug > 0) printf("remove: %s %1d %3d\n", lstn, iel, nelem[iel])
    # initialize station data file
    cmd = "rm -f "ofile[iel]
    system(cmd)
    system("")
  }
  
  nelem[iel] ++
  # fix the spacing (add blank character to beginning of data value field)
  printf("%s %s", stn, yr) >> ofile[iel]
  for(im = 1; im <= 12; im++)
  {
    ic = istrt + ((im - 1) * iinc)
    printf(" %s", substr($0, ic, iinc)) >> ofile[iel]
  }
  printf("\n") >> ofile[iel]
}
