BEGIN {

  if( byear == "" || eyear == "" || ddir == "" || vproc =="")
  {
    print "Generate the temp daily range (tdr) as (max-min)"
    print "   where byear = begin year"
    print "         eyear = end year"
    print "         ddir = input data directory read /vproc"
    print "         vproc = version of the process"
    print "   and the input file is the list of stations to process"
    print "     Example: "
    print "       gawk -f gen_tdr.3flg.awk -v byear=1895 -v eyear=2011  "
    print "         -v ddir=hcnv2 -v vproc=tob  " 
    print "         cdmp.meta.nws > gen_avg.nws.1949_2007.log"
    print "byear "byear
    print "eyear "eyear
    print "ddir "ddir
    print "vproc "vproc
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

  velout = "tdtr"
  
  debug = 0
}

{
  instn = $1
  
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

  ofile = ddir "/" vproc "/" instn "." vproc "." velout
  if (debug == 1) print "Output:" ofile
  system("rm -f " ofile)
  system("touch " ofile)
  irec = 0
  # print results
  for(iy = byear; iy <= eyear; iy++)
  {
    found = 0
    for(im = 1; im <= 12; im++)
    {
      if(vdata[1, iy, im] != amiss && vdata[2, iy, im] != amiss)
      {
        found = 1
        break
      }
      else
      {
        qflag1 = substr(vflag[1, iy, im], 2, 1)
        qflag2 = substr(vflag[2, iy, im], 2, 1)
        if(qflag1 != " " || qflag2 != " ")
        {
          found = 1
          break
        }  
      }
    }
    if(found == 1) 
    {    
      printf("%s %4.4d", instn, iy) >> ofile
      for(im = 1; im <= 12; im++)
      {
        if(vdata[1, iy, im] != amiss && vdata[2, iy, im] != amiss)
        {
          odat = vdata[1, iy, im] - vdata[2, iy, im]
        }  
        else
        {
          odat = amiss
        }
        oflag = ""
        for(iflg = 1; iflg <= 3; iflg++)
        {
          oflag1 = substr(vflag[1, iy, im], iflg, 1)
          oflag2 = substr(vflag[2, iy, im], iflg, 1)
          if(odat == amiss && iflg == 1)
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
          oflag = ( oflag iflag )
        }
        printf("%6d%s", odat, oflag) >> ofile
      }  
      printf("\n") >> ofile
      irec ++
    }
  }  
  close(ofile)
  if(irec == 0)
  {
    print "No data written to "instn " " velout " " vproc " #DTR"
  }
  else
  {
    print instn " " velout " " vproc " " irec
  }  
}  
