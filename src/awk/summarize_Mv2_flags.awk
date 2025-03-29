BEGIN {
# script=/home/cwilliam/USHCN/branches/GHCN/scripts
# cd /home/cwilliam/USHCN/v2/ghcnm/monthv2/20091019/ingest
# gawk -f $script/summarize_Mv2_flags.awk ghcnm_10_19_2009_00000.dat | sort -k 1,2 -t ":" -o ghcnm_10_19_2009_00000.flg.sum
# cd /home/cwilliam/USHCN/v2/ghcnm/monthv2/20091216/ingest
# gawk -f $script/summarize_Mv2_flags.awk ghcnm_12_16_2009_00001.dat | sort -k 1,2 -t ":" -o ghcnm_12_16_2009_00001.flg.sum

  if(FMT == "USHCN")
  {
    print "USHCN format"
    # data format for USHCN data start, value length, month string length
    istrt = 17
    ilen = 6
    iinc = 9
  }
  else
  {  
    print "GHCN format"
    # data format for GHCNM data start, value length, month string length
    istrt = 20
    ilen = 5
    iinc = 8
  }
  
  ilev = 0
  if(LEVEL == "detailed")
  {
    print "Detailed QC eval enabled - include data value with flag"
    ilev = 1
  }  
  
  iflag = 3
  iflg = 1
  amiss = -9999 + 0
}

{
#  if(NR%1000 == 1) print NR
  # meta data
  stn = substr($0, 1, 11)
  year = substr($0, 12, 4)
  if(FMT == "USHCN")
  {
    elem = "ALL "
  }
  else
  {  
    elem = substr($0, 16, 4)
  }
  
  pneg = 0
  for (im = 1; im <= 12; im++)
  {
    # start of monthly data column
    dcol = istrt + (iinc * (im - 1))
    # start of monthly flags column
    fcol = istrt + (iinc * (im - 1)) + ilen
    
    # all 3 flags together
    flag3 = substr($0, fcol, iflag)
    # each separately
    flg1 = substr($0, fcol, iflg)
    flg2 = substr($0, fcol+1, iflg)
    flg3 = substr($0, fcol+2, iflg)
    
    data = substr($0, dcol, ilen) + 0
    if(flag3 == "\0D ")
    {
      print im, fcol, ":"$0
    }
    else if(data == amiss)
    {
      mbin[elem, flag3]++
    }
    else
    {
      bin[elem, flag3]++
      # Detailed eval (data + flag)
      if(ilev == 1 && flg2 != " ")
      {
        dbin[elem, flg2, data]++
      }  
    }
    bin1[flg1]++
    bin2[flg2]++
    bin3[flg3]++
    if(elem == "PRCP" && data < 0 && data != amiss && pneg == 0)
    {
      pneg = 1
      print $0
    }  
  }
}

END {
  print "a Elem :Flg:   N"
  for (dims in bin)
  {
    split(dims, indices, SUBSEP)
    printf("A %4s :%3s: %9d\n", indices[1], indices[2], bin[dims])
  }
   for (dims in mbin)
  {
    split(dims, indices, SUBSEP)
    printf("M %4s :%3s: %9d\n", indices[1], indices[2], mbin[dims])
  }
  print "n :F:       N"
  for (indx in bin1)
  {
    printf("1 :%1s: %9d\n", indx, bin1[indx])
  }  
  for (indx in bin2)
  {
    printf("2 :%1s: %9d\n", indx, bin2[indx])
  }  
  for (indx in bin3)
  {
    printf("3 :%1s: %9d\n", indx, bin3[indx])
  }  
  if(ilev == 1)
  {
    print "DV: Elem: F: DATFLG:   N"
    for (dims in dbin)
    {
      split(dims, inds, SUBSEP)
      printf("DV: %4s: %s: %7s: %9d\n", inds[1], inds[2], inds[3], dbin[dims])
    }
  }    
}
