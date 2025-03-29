BEGIN {
  
  if( byear == "" || eyear == "" || ddir == "" || vproc == "" || \
   ometa == "" || otype == "")
  {
    print "Generate the avgerage as (max+min)/2"
    print "   where byear = begin year"
    print "         eyear = end year"
    print "         ddir = the data directory where MONTHLY is"
    print "         vproc = version of the process"
    print "         otype = output is DTR or AVG"
    print "   and the input file is the list of stations to process"
    print "     Example: "
    print "       gawk -f gen_avg.3flg.awk -v byear=1895 -v eyear=2005  "
    print "         -v ddir=~/CDMP_3220.2/monthly -v vproc=FLs.52d  " 
    print "         ../to_nws/nws.49_05.final > gen_avg.nws.49_05.log"
    print "byear "byear
    print "eyear "eyear
    print "ddir "ddir
    print "vproc "vproc
    print "otype "otype
    print "ometa "ometa
    exit
  }
  
  amiss = -9999
  
  # GHCN 11-char ID 3-flag monthly data
  # column start, length, increment and missing value definition
  istrt = 17
  ilen = 6
  incr = 9
  
  velem[1] = "tmax"
  velem[2] = "tmin"

  if(otype == "AVG")
  {
    velout = "tavg" 
  }
  else if(otype == "DTR")  
  {
    velout = "tdtr"
  }
  else
  {
    print "Unknown output type: ",otype
    exit
  }
  nometa = ometa".rem"
  
  debug = 0
}

{
  instn = $1
  metarec = $0
  
  for(ie = 1; ie <= 2; ie++)
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
  
  for(ie = 1; ie <= 2; ie++)
  {
    if(vfile[ie] != "") close(vfile[ie])
    vfile[ie] = ddir "/" vproc "/" instn "." vproc "." velem[ie]
    if (debug == 1) print "Input:" vfile[ie]
    while(1==1)
    {
      if((getline < vfile[ie]) <= 0) break
      line = $0
      #print line
      iy = substr(line, 13, 4) + 0
      #print iy

      # filter for desired data  
      if(iy >= byear && iy <= eyear)
      {
        for(im = 1; im <= 12; im++)
        {
          ic = istrt + (im-1)*incr
          vdata[ie, iy, im] = substr(line, ic, ilen) + 0
          ic = ic + ilen
          vflag[ie, iy, im] = substr(line, ic, 3)
        }
      }
    } # end of read file loop 
    close(vfile[ie])
  }  

  irec = 0
  for(iy = byear; iy <= eyear; iy++) found[iy] = 0;
  for(im = 0; im <= 12; im++) omth[im] = 0;
    
  # generate avg and count each month with data
  for(iy = byear; iy <= eyear; iy++)
  {
    for(im = 1; im <= 12; im++)
    {
      if(vdata[1, iy, im] != amiss && vdata[2, iy, im] != amiss)
      {
        # if both max/min have data - keep this year
        found[iy] = 1
        break
      }
      else
      {
        qflag1 = substr(vflag[1, iy, im], 2, 1)
        qflag2 = substr(vflag[2, iy, im], 2, 1)
        if(qflag1 != " " || qflag2 != " ")
        {
          found[iy] = 1
          break
        }
      }  
    }
    if(found[iy] == 1)
    {
      for(im = 1; im <= 12; im++)
      {
        if(vdata[1, iy, im] != amiss && vdata[2, iy, im] != amiss)
        {
          # generate avg or dtr
          if(otype == "AVG")
          {
            odat[iy, im] = (vdata[1, iy, im] + vdata[2, iy, im] + 1)/2
          }
          else
          {
            odat[iy, im] = vdata[1, iy, im] - vdata[2, iy, im]
          }  
          # accum monthly total numbe of obs
          omth[im]++
        }
        else
        {
          odat[iy, im] = amiss
        }    

        oflag[iy, im] = ""
        for(iflg = 1; iflg <= 3; iflg++)
        {
          oflag1 = substr(vflag[1, iy, im], iflg, 1)
          oflag2 = substr(vflag[2, iy, im], iflg, 1)
          if(odat[iy, im] == amiss && iflg == 1)
          {
            iflag = " "
          }  
          else if(vproc ~ "FLs" && iflg == 1 &&
            (oflag1 == "E" || oflag2 == "E"))
          {  
            iflag = "E"   
          }  
          else if((vproc ~ "WMs" || vproc ~ "FLs") && iflg == 2)
          {
            if(oflag1 == "Q" || oflag2 == "Q") {iflag = "Q"}
            else if(oflag1 == "X" || oflag2 == "X") {iflag = "X"}
            else {iflag = " "}
          }
          else
          {
            iflag = oflag1
            if(oflag2 > oflag1) iflag = oflag2
          }
          oflag[iy, im] = ( oflag[iy, im] iflag )
        }
      }  
      irec ++
    }
  }
  
  # skip station if there is less than 8 obs values for any month
  for(im = 1; im <= 12; im++)
  {
    if(omth[im] < 8) 
    {
      print metarec >> nometa
      next
    }
  }  

  # Write data and meta files
  ofile = ddir "/" vproc "/" instn "." vproc "." velout
  if (debug == 1) print "Output:" ofile
  system("rm -f " ofile)
  system("touch " ofile)

  print metarec >> ometa
  for(iy = byear; iy <= eyear; iy++)
  {
    if(found[iy] == 1)
    {
      printf("%s %4.4d", instn, iy) >> ofile
      for(im = 1; im <= 12; im++)
      {
        printf("%6d%s", odat[iy, im], oflag[iy, im]) >> ofile
      }  
      printf("\n") >> ofile
      irec ++
    }
  }  
  close(ofile)
  if(irec == 0)
  {
    print "No data written to "instn " " velout " " vproc " #AVG"
  }  
}  
