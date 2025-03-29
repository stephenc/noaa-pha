BEGIN {
  # simple routine to generate the 3-flag GHCNM format from
  #   GHCN-Mv2 (with 3-flag) 
  
  # data format of GHCNMv2 and GHCNMv2five
  istrt = 20
  iinc = 8
  ilen = 5
  lstn = ""
  
  byear = 1700
  eyear = 2012
  amiss = -9999
}  

{
  # parse the GHCNMv2 data records
  stn=substr($0, 1, 11)

  # check for new station 
  if(stn != lstn)
  {
    if(lstn != "")
    {
      # compare old station (max+min)/2 == avg
      for(iy = byear; iy <= eyear; iy++)
      {
        for(im = 1; im <= 12; im++)
        {
          for(ie = 1; ie <= 3; ie++)
          {
            if(vdata[ie, iy, im] != amiss)
            {
              igood[ie]++
            }
          }    
          if(vdata[1, iy, im] != amiss && vdata[2, iy, im] != amiss &&
             vdata[3, iy, im] != amiss)
          {
            vavg = int((vdata[1, iy, im] + vdata[2, iy, im]) / 2.0) + 0
            iadd = 1
            if(vavg < 0) iadd = -1
            if( vavg != vdata[3, iy, im] && vavg+iadd != vdata[3, iy, im])
            {
              printf("%s %4d %2d %8.1f %8.1f %7.0f %7.0f %7.0f\n", lstn,
                iy, im, vavg - vdata[3, iy, im], vavg, vdata[3, iy, im], 
                vdata[1, iy, im], vdata[2, iy, im])
            }
            else
            {
              igood[0]++
            }  
          }
        }
      }
      printf("%s %5d %5d %5d %5d\n", lstn, igood[0], igood[1], igood[2], igood[3])
    }
    # initialize for new station
    lstn = stn
    for(ie = 0; ie <= 3; ie++) igood[ie] = 0
    for(ie = 1; ie <= 3; ie++)
    {
      for(iy = byear; iy <= eyear; iy++)
      {
        for(im = 1; im <= 12; im++)
        {
          vdata[ie, iy, im] = amiss
          vflag[ie, iy, im] = "   "
        }    
      }  
    }  
  }
  
  yr=substr($0, 12, 4)
  elem=substr($0, 16, 4)
  # determine element file for the record
  if(elem == "PRCP")
  {
    iprcp++
    next
  }
  else if(elem == "TMAX")
  {
    ie = 1
  }
  else if(elem == "TMIN")
  {
    ie = 2
  }
  else if(elem == "TAVG")
  {
    ie = 3
  }
  else
  {
    print "Unknown element: ", stn, yr, elem
    next
  }
  
  # fill in the max/min/avg element array for this station
  for(im = 1; im <= 12; im++)
  {
    ic = istrt + ((im - 1) * iinc)
    vdata[ie, yr, im] = substr($0, ic, ilen) + 0
  }
}
