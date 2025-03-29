BEGIN {

  if(BYEAR == "" || EYEAR == "" || THRES == "")
  {
    print "Summ Number of monthly observations in file"
    print "Where:"
    print "   BYEAR = begin year"
    print "   EYEAR = end year"
    print "   THRES = threshold months to output"
    exit
  }  
  
  # count the number of QC flag2 (X & Q) in the QCA file
  amiss = -9999 + 0
  
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
  byr = BYEAR + 0
  eyr = EYEAR + 0
}

{
  if(NR == 1) {stn = substr($0, 1, 11)}
  year = substr($0, yrstrt, 4) + 0
  # print "Y "year
  if(year < byr || year > eyr) {next}
  
  for(im = 1; im <= 12; im++)
  {
    iqc = istrt + (im-1)*incr + ilen + 1
    ivc = istrt + (im-1)*incr
    vflag = substr($0, iqc, 1)
    val = substr($0,ivc,ilen) + 0
    if((vflag == " " || vflag == "E") && val != amiss)
    {
      summth[im]++
      # print val" "vflag
    }
  }
} 

END {
  prtrec = sprintf("%s", stn)
  iprt = 1
  for(im = 1; im <= 12; im++)
  {
    if(summth[im] < THRES) {iprt = 0; break}
    prtrec = ( prtrec sprintf(" %3d", summth[im]) )
  }
  if(iprt == 1)
  {
    printf("%s\n", prtrec)
  }
}  
