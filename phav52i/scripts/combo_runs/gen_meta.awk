BEGIN {
  # generate the separate element metadata files for
  #  the GHCNM Normal format (that is, after converted 
  #  from Byron's big DAT file
  
  print "META: "META
  print "IDIR: "IDIR
  print "PROC: "PROC
  print "REG: "REG
  print "DEBUG: "DEBUG
  
  if( PROC == "" || IDIR == "" || META == "" || REG == "" )
  {
    print "Generate the GHCNM meta element files"
    print ""
    print " where PROC is the process (raw, tob, adj, etc.)"
    print "       IDIR is the input data directory"
    print "       META is the output meta file base"
    print " Example:"
    print "  gawk -f gen-meta.awk -v PROC=raw -v META=ghcn.meta"
    print "    -v IDIR=../global ghcnm.inv"
    exit
  }  

  # data format of HCN-3flag Normals style
  istrt = 17
  iinc = 9
  ilen = 6
  lstn = ""
  nelem = split("tmax tmin tavg prcp", elem, " ")
  
  # ithres is the minimum threshold for the number of each month
  if(REG == "coop" || REG == "northam")
  {
    ithres = 8
    ibegyr = 1795
  }
  else
  {  
    ithres = 6
    ibegyr = 1701
  }  
  
  # missing obs value
  amiss = -9999
  
  # debug option
  debug = 0
  
  # number of non-Conus US stations
  notconus = 0
  ymax = 0
  ymin = 0
  yavg = 0
  ypcp = 0
}  

{
  # read the GHCNMv2 station list (mis-named "inv")
  stn = $1
  lat = $2 + 0
  lon = $3 + 0
  if(REG == "coop")
  {
    if(lat > 50 || lat < 23.5 || lon > -65 || lon < -125)
    {
      notconus++
      next
    }
  }
  if (lat == -99.0 || lon == -999.0)
  {
    print "Lat-Lon missing: "$0
    nolatlon++
    next
  }    
  elev = $4 + 0
  name = substr($0, 39, 30)
  metarec = substr($0, 12)
  # surf = substr($0, 64)
  # orec = sprintf("%s %8.4f %9.4f %6.1f       %s | %s\n", stn, lat, lon, elev, name, surf)
  orec = sprintf("%s %8.4f %9.4f %6.1f       %s", stn, lat, lon, elev, name)

  tavgstr = "---- ---- ----"
  tmaxstr = "---- ---- ----"
  tminstr = "---- ---- ----"
  prcpstr = "---- ---- ----"
  split("0 0 0 0", begyr, " ")
  split("0 0 0 0", endyr, " ")
  elnum = 0
  
  # find station in data directory
  for (ielem = 1; ielem <= nelem; ielem++)
  {
    num[ielem] = 0
    file = IDIR"/"PROC"/"stn"."PROC"."elem[ielem]
    for(im = 1; im <= 12; im++) {mthnum[im] = 0}     
    if(debug > 0) print file > DEBUG
    while( (getline < file) > 0)
    {
      yr = substr($0, 13, 4) + 0
      # print yr > DEBUG
      if(yr < ibegyr) {continue}
      if(begyr[ielem] == 0) begyr[ielem] = yr
      endyr[ielem] = yr
      # get monthly
      for(im = 1; im <= 12; im++)
      {
        ic = istrt + (im-1)*iinc
        rdata = substr($0, ic, ilen) + 0
        rflag = substr($0, ic+ilen, 3)
        ic = ic + ilen
        if( rdata != amiss && substr(rflag,2,1) == " " )
        {
          mthnum[im]++
          if(debug > 1) print yr, im, mthnum[im], ic, rdata >> DEBUG
        }
      }
    }
    close(file)
    ipass = 1
    for(im = 1; im <= 12; im++)
    {
      if(debug > 0) print im, mthnum[im] >> DEBUG
      if(mthnum[im] < ithres) {ipass = 0; break}
    }
    if(ipass == 1) { num[ielem]++ } 
  }
  if(num[1] >= 1)
  {
    tmaxstr = sprintf("tmax %4d %4d", begyr[1], endyr[1])
    ofile = META".tmax"
    print orec > ofile
    tmax++
    ymax += endyr[1] - begyr[1] + 1
    elnum += 8 
  }
  if(num[2] >= 1)
  {
    tminstr = sprintf("tmin %4d %4d", begyr[2], endyr[2])
    ofile = META".tmin"
    print orec > ofile
    tmin++
    ymin += endyr[2] - begyr[2] + 1
    elnum += 4
  }
  if(num[3] >= 1) 
  {
    tavgstr = sprintf("tavg %4d %4d", begyr[3], endyr[3])
    ofile = META".tavg"
    print orec > ofile
    tavg++
    yavg += endyr[3] - begyr[3] + 1 
    elnum += 2
  }
  if(num[4] >= 1)
  {
    prcpstr = sprintf("prcp %4d %4d", begyr[4], endyr[4])
    ofile = META".prcp"
    print orec > ofile
    prcp++
    ypcp += endyr[4] - begyr[4] + 1 
    elnum += 1
  }  
   
  print stn, tmaxstr, tminstr, tavgstr, prcpstr, metarec >> DEBUG
}

END {
  if(notconus > 0)
  {
    print " WARNING: Not CONUS stations - Skipped: ", notconus
  }
  if(nolatlon > 0)
  {
    print " WARNING: Stations without Lat/Lon - Skipped: ", nolatlon
  }  
  print "number of tmax:",tmax, " tmin:",tmin," tavg:",tavg, " prcp:",prcp
  # Export the number of POR years for each element
  print "export TMAXYRS="ymax
  print "export TMINYRS="ymin
  print "export TAVGYRS="yavg
  print "export TPCPsYRS="ypcp
}
  
