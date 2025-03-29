BEGIN {
  # Conus only - 16mar2010
  # Expanded for NorthAm test to USHCNv2.5
  if(OUTDIR == "" || PROC == "")
  {
    print "Enter output directory (OUTDIR)"
    print "       and process type (PROC)"
    STOPIT = 1
    exit 1
  }
  istrt = 20
  ilen = 5
  iflg = 3
  iinc = 8
  
  lstn = ""
  lelem = ""
  
  # number of unknown network stations
  # that is: inside US but netcode not C for COOP or W for Wban 
  unknet = 0
}

{
  # read in the data (both USHCNv1 & GHCN COOP in same format)
  cntry = substr($1, 1, 2)
  stn = substr($1, 1, 11)
  if(TYPE == "northam")
  {
    if(cntry != "US" && cntry != "CA" && cntry != "MX" && \
      cntry != "RQ" && cntry != "VQ")
    { next }
  }
  else if(TYPE == "hcnv1") 
  {
    # HCN v1 are only conus
    stn = "USH00"substr($1, 6, 6)
  }
  else if(TYPE == "coop")
  {
    # COOP must be filtered for conus only
    netcode = substr($1, 3, 1)
    # For US netcode can be C for COOP; W for Wban (assume all CONUS)
    if(netcode == "C")
    {
      st = substr($1, 6, 2) + 0
      if(st > 48) {next}
    }
    else if(netcode == "W")
    {
      wstn = substr($1, 7, 5) + 0
      # WBAN numbers from 21500 to 45800 are not in CONUS
      #  Changed from 21500 to 25300 as of WBAN expansion on 16 May 2011 -cw
      if(wstn >= 25300 && wstn <= 45800) {next}
    }
    else
    {
      unknet++
    }
  }  
  else
  {
    print "Unknown Region type"
    exit
  } 

  elem = tolower(substr($0, 16, 4))

  if(stn != lstn || elem != lelem) 
  {
    if(lstn != "") 
    {
      close(fname)
    }  
    fname = OUTDIR"/"TYPE"/"PROC"/"stn"."PROC"."elem
    lstn = stn
    lelem = elem
    stnelem ++
  }  

  year = substr($0, 12, 4)
  printf("%s %s", stn, year) >> fname
  for(im = 0; im <= 11; im++)
  {
    mstr = substr($0, istrt + (im * iinc), iinc)
    printf(" %s", mstr) >> fname
  }
  printf("\n") >> fname
  nrec++
}

END {
  if(STOPIT == 1) exit 1
  if( unknet > 0)
  {
    print "WARNING: Unknown US netcode stations - Skipped: ",unknet
  }  
  print "Parse_ghcnd_stns: ",TYPE, " Summary: ", stnelem, " station-elements ", \
    nrec, " records"
}
