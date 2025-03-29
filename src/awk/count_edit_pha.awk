BEGIN {
  # count the number of QC flag2 (X & Q) in the QCA file
  amiss = -9999
  
  # GHCN 11-char ID 3-flag monthly data
  # column start, length, increment and missing value definition
  istrt = 17
  ilen = 6
  incr = 9
  yrstrt = 13

  # QCA ?
  # column start, length, increment and missing value definition
  # istrt = 20
  # ilen = 5
  # incr = 8
  # yrstrt = 12
  
  debug = 0
  loyr = 9999 + 0
}

{
  year = substr($0, yrstrt, 4) + 0
  # print year
  if(year < loyr) loyr = year
  
  for(im = 1; im <= 12; im++)
  {
    iqc = istrt + (im-1)*incr + ilen + 1
    ivc = istrt + (im-1)*incr
    vflag = substr($0, iqc, 1)
    val = substr($0,ivc,ilen) + 0
    if(vflag == "X")
    {
      phax[year, im]++
    }
    else if(vflag == "Q")
    {
      qcx[year, im]++
    }
    else if(val > -9999)
    {
      nval[year, im] ++
    }  
  }
} 

END {
  for(iy = loyr; iy <= 2015; iy++)
  {
    for(im = 1; im <= 12; im++)
    {
      if(qcx[iy,im] != 0 || phax[iy,im] != 0 || nval[iy,im] != 0)
      {
        printf("%7.2f %5d %5d %5d\n", \
          iy+(im-1.0)/12.0, qcx[iy,im], phax[iy,im], nval[iy,im])
      }
    }  
  }  
}  
