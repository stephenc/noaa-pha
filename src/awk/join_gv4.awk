BEGIN {
  # simple routine to generate the 3-flag GHCNM format from
  #   GHCN-Mv2 (with 3-flag) 
  
  print "PROC: "PROC
  print "GDIR: "GDIR
  print "OUTDAT:  "OUTDAT
  print "OUTINV:  "OUTINV
  print "ELEM: "ELEM
  print "QAEF: "QAEF
  if(QAEF == "qaf") {print "QAF not implemented yet"; DOEND=0;exit}
  print "INTYPE: "INTYPE
  if(INTYPE != "INV" && INTYPE != "META"){print "UNKNOWN INTYPE"; DOEND=0;exit} 
  DOEND = 1
  
  if( PROC == "" || ELEM == "" || GDIR == "" || OUTDAT == "" || OUTINV == "" || QAEF == "" || INTYPE == "")
  {
    print "Join Process level GHCN & USHCN files using the log file"
    print "   made in the gen_meta.awk"
    print ""
    print " where PROC is the process (raw, WMs.52g, FLs.52g)"
    print "       ELEM is the element to process (tavg, tmax, tmin)"
    print "       GDIR is the GHCN input directory"
    print "       OUTDAT is the summary output"
    print "       OUTINV is the station list output"
    print "       QAEF is the QAE string for post output"
    print "       INTYPE is the input type of the input file"
    print "           =INV is the GHCNM QC INV file"
    print "           =META is the Gen-Meta log from GHCNMv4"
    print " Example:"
    print "  gawk -f join_gv4.awk -v PROC=raw -v GDIR=gdata -v ELEM=tavg"
    print "    -v QAEF=qcf -v INTYPE=INV -v OUTDAT=ghcnm.52d.dat "
    print "    -v OUTINV=ghcnm.52d.inv ghcnm.inv"
    DOEND = 0
    exit 1
  }

  debug = 0
  amiss = -9999 + 0

  # data format of GHCNMv2 and GHCNMv2five
  # istrt = 20
  # iinc = 8
  # ilen = 5
  # lstn = ""

  # data format of HCN-3flag Normals style
  istrt = 17
  iinc = 9
  ilen = 6
  lstn = ""

  for(im = 1; im <= 12; im++)
  {
    nmiss[im] = 0
    nfill[im] = 0
    ndata[im] = 0
  }  
  cmd = "rm -f "OUTDAT
  system(cmd)
  system("")
  cmd = "rm -f "OUTINV
  system(cmd)
  system("")
  ninvel = split("tmax tmin tavg prcp tdtr", invels, " ")
  indmax = 1
  indmin = 2
  inddtr = 5
# Define output element
#  nelem = split("tavg", elems, " ")
  elems[1] = ELEM
  nelem = 1
  istrt1 = 12
  inc1 = 15
  
  debug = 0
}

{
  # get station from gen_meta output log file
  stn = substr($0, 1, 11)
  finedit = $NF
  
  # two types of station list files are usable
  if (INTYPE == "INV")
  {
    metarec = substr($0, 13)
  }
  else if (INTYPE == "META")
  {
    metarec = substr($0, 74)
  }
   
  for (ielem = 1; ielem <= nelem; ielem++)
  {
    if(debug > 0) print stn, elemstr[ielem]

    if( finedit == "*" && QAEF == "qcf" )
    {
      file = GDIR"/tob/"stn".tob."elems[ielem]
    }
    else
    {
      file = GDIR"/"PROC"/"stn"."PROC"."elems[ielem]
    }

    if(debug > 1)  print file, begyr, endyr
    while( (getline < file) > 0)
    {
      yr = substr($0, 13, 4) + 0
      # limit QFE output to 1961-2010 for 3 Normals period
      # i.e. 61-90, 71-00, 81-10
      if(QAEF == "qfe")
      {
        if (yr < 1961) {continue}
        else if(yr > 2010) {break}
      }
      # output monthly
      printf("%11s%4d%4s", stn, yr, toupper(elems[ielem])) >> OUTDAT
      for(im = 1; im <= 12; im++)
      {
        ic = istrt + (im-1)*iinc
        rdata = substr($0, ic, ilen) + 0
        rflag = substr($0, ic+ilen, 3)
        ic = ic + ilen
        # data measurement flag
        mflg = substr(rflag, 1, 1)
        if(mflg == ".") {mflg = "i"}
        # QC flag
        qflg = substr(rflag, 2, 1)
        # Source flag
        sflg = substr(rflag, 3, 1)
 
        if(debug > 2) 
        {
          print "mflag:"mflg
          print "qflag:"qflg
          print "sflag:"sflg
        }
         
        if(PROC == "raw" || PROC == "tob")
        {
          if(mflg != "T") {mflg = tolower(mflg)}
          if(rdata == amiss)
          {
            mflg = " "
            sflg = " "
          }
          if(debug > 2) 
          {
            print "raw: ",stn, yr, im, rdata, ":"rflag":"mflg qflg sflg":"
          }  
        }
        else if(PROC ~ "WMs")
        {
          if(mflg != "T") {mflg = tolower(mflg)}
          # ANY non-blank QC flag data and it is not == X then this is 
          #   RAW or TOB data for the QCF and should be set to missing
          if(qflg != " " )
          {
            if(qflg != "X")
            { 
              rdata = amiss
              qflg = "Q"
            }
          }  
          if(debug > 2) 
          {
            print "WMs: ",stn, yr, im, rdata, ":"rflag":"mflg,qflg,sflg":"
          }  
        }
        else if(PROC ~ "FLs")
        {
          if(mflg != "E" && mflg != "T") {mflg = tolower(mflg)}
          # check ANY non-blank QC flag data
          if(qflg != " ")
          {
            if(rdata == amiss)
            {
              # data NOT filled (estimated) - use WMs flags
              if(qflg ~ /[[:lower:]]/)
              {
                # lower case QC flags indicate PHA removal
                mflg = " "
                qflg = "X"
                sflg = " "
              }
              else
              {
                # Upper case QC flags are from raw QC routines
                mflg = " "
                qflg = "M"
                sflg = " "
              }
            }
            else
            {  
              mflg = "Q"
              qflg = " "
              sflg = " "
            }
          }
          if(debug > 2) 
          {
            print "FLs: ",stn, yr, im, rdata, ":"rflag":"mflg,qflg,sflg":"
          }  
        }
          
        if(rdata == amiss)
        {
          nmiss[im]++
        }
        else if(mflg == "E" || mflg == "Q")
        {
          nfill[im]++
          ifound++
        }
        else
        {
          ndata[im]++
          ifound++
        }
        printf("%5d%c%c%c", rdata, mflg, qflg, sflg) >> OUTDAT
      }
      printf("\n") >> OUTDAT
    }
    close(file)

  }
  if(ifound > 0) {print stn, metarec >> OUTINV}
}

END {
  if(DOEND == 0) exit 1
  printf("Total Number of Data Values:   ")
  for(im = 1; im <= 12; im++){printf("%8d",ndata[im])}
  printf("\n")
  printf("Total Number of Filled Values: ")
  for(im = 1; im <= 12; im++){printf("%8d",nfill[im])}
  printf("\n")
  printf("Total Number of Missing Values:")
  for(im = 1; im <= 12; im++){printf("%8d",nmiss[im])}
  printf("\n")
}
