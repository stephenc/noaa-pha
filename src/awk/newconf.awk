BEGIN {
  if(NCOMBO == "")
  {
    print "Please enter NCOMBO number"
    exit
  }
  ncombo = NCOMBO + 100 
  FS = "="
  ofile = "combination"ncombo".txt"
}

{
  if($1 == "CONFIRM")
  {
    nc = $2-1
    print $1"="nc >> ofile
  }
  else
  {
    print $0 >> ofile
  }
}
