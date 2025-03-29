BEGIN {
  # from GHCNMv3/run_scripts/join_gv3_usv2.awk
  #   simple routine to generate CAS "the 3-flag GHCNM format from
  #   GHCN-Mv2 (with 3-flag)

  print "PROC: "PROC
  print "GDIR: "GDIR
  print "UDIR: "UDIR
  print "OUTDAT:  "OUTDAT
  print "OUTINV:  "OUTINV
  print "ELEM: "ELEM
  doend = 1

  if( PROC == "" || ELEM == "" || GDIR == "" || UDIR == "" || OUTDAT == "" || OUTINV == "")
  {
    print "Join Process level GHCN & USHCN files using the log file"
    print "   made in the gen_meta.awk"
    print ""
    print " where PROC is the process (raw, WMs.52g, FLs.52g)"
    print "       ELEM is the element to process (tavg, tmax, tmin)"
    print "       GDIR is the GHCN input directory"
    print "       UDIR is the USHCN input directory"
    print "       OUTDAT is the summary output"
    print "       OUTINV is the station list output"
    print " Example:"
    print "  gawk -f join_gv3_usv2.awk -v PROC=raw -v GDIR=gdata -v UDIR=udata"
    print "    -v OUTDAT=ghcnm.52d.dat -v ELEM=tavg -v OUTINV=ghcnm.52d.inv ghcnm.inv"
    doend = 0
    exit
  }

  # data format of GHCNMv2 and GHCNMv2five
  istrt = 20
  iinc = 8
  ilen = 5
  lstn = ""
  debug = 0
  amiss = -9999 + 0

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
  ninvel = split("tmax tmin tavg prcp", invels, " ")
# Define output element
#  nelem = split("tavg", elems, " ")
  elems[1] = ELEM
  nelem = 1
  istrt1 = 12
  inc1 = 15
}

{
  # get station from gen_meta output log file
  stn = substr($0, 1, 11)
  metarec = substr($0,74)
  ifound = 0
  for (invel = 1; invel <= ninvel; invel++)
  {
    for (ielem = 1; ielem <= nelem; ielem++)
    {
      if(invels[invel] == elems[ielem])
      {
        elemstr[ielem] = substr($0, istrt1 + (invel - 1) * inc1, inc1)
      }
    }
  }
  for (ielem = 1; ielem <= nelem; ielem++)
  {
    if(debug > 0) print stn, elemstr[ielem]
    if(substr(elemstr[ielem], 2, 4) != "----")
    {
      begyr = substr(elemstr[ielem], 7, 4) + 0
      endyr = substr(elemstr[ielem], 12, 4) + 0
      # fill in the US HCN data from the USHCNv2 data set
      #   defined by "42500" at the start of the station ID
      if(substr(stn, 1, 5) == "42500")
      {
        file = UDIR"/"PROC"/USH00"substr(stn,6,6)"."PROC"."elems[ielem]
        hcnfile = 1
      }
      else
      {
        file = GDIR"/"PROC"/"stn"."PROC"."elems[ielem]
        hcnfile = 0
      }
      # print file, begyr, endyr
      while( (getline < file) > 0)
      {
        yr = substr($0, 13, 4) + 0
        if(yr < begyr || yr > endyr) {continue}
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
          # print "mflag:"mflg
          # QC flag
          qflg = substr(rflag, 2, 1)
          # print "qflag:"qflg
          # Source flag
          sflg = substr(rflag, 3, 1)
          # print "sflag:"sflg

          if(PROC == "raw")
          {
            if(mflg != "T") {mflg = tolower(mflg)}
            if(rdata == amiss)
            {
              mflg = " "
              qflg = " "
              sflg = " "
            }
            else
            {
              if(hcnfile == 1) {sflg = "U"}
           }
            # print "raw: ",stn, yr, im, rdata, ":"rflag":"mflg qflg sflg":"
          }
          else if(PROC ~ "WMs")
          {
            if(mflg != "T") {mflg = tolower(mflg)}
            if(hcnfile == 1 && rdata != amiss) {sflg = "U"}
            # ANY non-blank QC flag data should be set to missing
            if(qflg != " ")
            {
              rdata = amiss
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
            # print "WMs: ",stn, yr, im, rdata, ":"rflag":"mflg,qflg,sflg":"
          }
          else if(PROC ~ "FLs")
          {
            if(hcnfile == 1 && rdata != amiss)
            {
              if(mflg == "E")
              {
                sflg = " "
              }
              else
              {
                sflg = "U"
              }
            }
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
            # print "FLs: ",stn, yr, im, rdata, ":"rflag":"mflg,qflg,sflg":"
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
  }
  if(ifound > 0) {print stn, metarec >> OUTINV}
}

END {
  if(doend == 0) exit
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
