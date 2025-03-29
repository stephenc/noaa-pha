 
BEGIN {
  stopit = 0
  # Simple averaging - good idea to do the "Raw fill" (RB3.2 - F option)
  if(BYEAR =="" || EYEAR =="" || ELEM== "" || PROC == "" || DIV == "" || DDIR == "")
  {
    print "Average all of the stations in the input file ($1 == stations)"
    print "  Pathname down to PROC is the first line in the file"
    print "  Example: nawk -f gen_stnanom.awk -v ELEM=tavg \\ "
    print "    -v BYEAR=1875 -v EYEAR=2011 -v PROC=raw \\ "
    print "    -v DIV=100 -v DDIR=~/data iceland.stns"
    print " BYEAR: ",BYEAR
    print " EYEAR: ",EYEAR
    print " ELEM: ",ELEM
    print " PROC: ",PROC
    print " DIV: ",DIV
    print " DDIR: ",DDIR
    stopit = 1
    exit
  }
  
  amiss = -9999
  datapath=DDIR
  
  # Turn ON(=1)/OFF(=0) debug print
  DEBUG = 0
  
  # Normals monthly data
  # column start, length, increment and missing value definition
  istrt = 17
  ilen = 6
  incr = 9
  
  # initialize data arrays
  init_data()
}
    
{
#  if(NR == 1) {datapath = $0; next}
  fullpath = datapath"/"PROC"/"$1"."PROC"."ELEM 
   if(DEBUG >= 1) print fullpath
  read_data(fullpath)
}

END {
  if(stopit == 1) exit
  for(iy = BYEAR; iy <= EYEAR; iy++)
  {
    yrsum = 0
    yrcnt = 0
    for(im = 1; im <= 12; im++)
    {
      netavg = data[im, iy] / count[im, iy]
      yrsum += netavg
      yrcnt ++
    }
    printf("%4d %8.2f\n", iy, yrsum/yrcnt)
  }  
}

function read_data(fullpath)
{
    if(DEBUG >= 1)  print " read " fullpath
    
    # read station data
    while(1==1)
    {
      if((getline < fullpath) <= 0) break
      line = $0
      if(DEBUG >= 2)  print line
      iy = substr(line, 13, 4) + 0
      if(DEBUG >= 3) print iy

      # filter for desired data 
      if(iy >= BYEAR && iy <= EYEAR)
      {
        for(im = 1; im <= 12; im++)
        {
          ic = istrt + (im-1)*incr
          idat = substr(line, ic, ilen) + 0
          iflg = substr(line, ic+ilen, 3)
#          if(idat <= amiss || substr(iflg, 2, 1) != " ") continue
          if(idat <= amiss) 
          {
            if(DEBUG >=1) print "MISSING: "iy, im
          }
          else
          {
            cdat = idat / DIV
            data[im, iy] += cdat
            count[im, iy] ++
            if(DEBUG >= 3) print iy, im, data[im,iy], count[im, iy]
          }
        }
      }
    } # end of read file loop  
    close(fullpath)
}

function init_data()
{
  for(im = 1; im <=12; im++)
  {  
    for(iy = BYEAR; iy <= EYEAR; iy++)
    {
      data[im, iy] = 0
      count[im, iy] = 0
    }
  }
}
