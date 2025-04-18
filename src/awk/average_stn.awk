 
BEGIN {
  stopit = 0
  # Simple averaging - good idea to do the "Raw fill" (RB3.2 - F option)
  if(BYEAR =="" || EYEAR =="" || ELEM== "" || PROC == "" || DIV == "" || DDIR == "")
  {
    print "Average all of the stations in the input file ($1 == stations)"
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
}
    
{
#  if(NR == 1) {datapath = $0; next}
  stnid = $1
  fullpath = datapath"/"PROC"/"stnid"."PROC"."ELEM 
  if(DEBUG >= 1) print fullpath

  # initialize data arrays
  init_data()

  # read data file
  read_data(fullpath)
  
  # print month avg 
  printavg()
}

function printavg()
{
  printf("%s ", stnid)
  for(im = 1; im <= 12; im++)
  {
    if(count[im] .gt. 0)
    {
      avg = data[im] / count[im]
    }
    else
    {
      avg = -99.99
    }    
    printf("%8.2f 2d", avg, count[im])
  }
  printf("\n")
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
            data[im] += cdat
            count[im] ++
            if(DEBUG >= 3) print iy, im, data[im], count[im]
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
    data[im] = 0
    count[im] = 0
  }
}
