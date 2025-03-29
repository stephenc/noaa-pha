BEGIN {
  # count the number of QC flag2 (X & Q) in the QCA file
  amiss = -9999
  
  # GHCN 11-char ID 3-flag monthly data
  # column start, length, increment and missing value definition
  # istrt = 17
  # ilen = 6
  # incr = 9
  # yrstrt = 13

  # QCA ?
  # column start, length, increment and missing value definition
  istrt = 20
  ilen = 5
  incr = 8
  yrstrt = 12
  
  debug = 0
  loyr = 9999 + 0
}

{
  year = substr($0, yrstrt, 4) + 0
  if(year < 2014) {next}
  # print year
  if(year < loyr) loyr = year
  
  for(im = 1; im <= 12; im++)
  {
    iqc = istrt + (im-1)*incr + ilen + 1
    ivc = istrt + (im-1)*incr
    vflag = substr($0, iqc, 1)
    if(vflag == " "){continue}
    flags[vflag]++
    itot++
    iflg[year, im, vflag]++
    iymflg[year, im]++
  }
} 

END {
  printf("  Flags")
  for(vflag in flags)
  {
    printf("    %s  ", vflag)
  }
  printf("   Sum\n")
  printf("  Total")
  for(vflag in flags)
  {
    printf(" %6d", flags[vflag])
  }
  printf(" %6d\n", itot)    
  for(iy = loyr; iy <= 2015; iy++)
  {
    for(im = 1; im <= 12; im++)
    {
      printf("%7.2f", iy+(im-1.0)/12.0)
      for(vflag in flags)
      {
        printf(" %6d", iflg[iy, im, vflag])
      }
      printf(" %6d\n", iymflg[iy, im])
    }  
  }  
}  
