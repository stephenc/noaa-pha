# Lib-src Makefile
# dynamically figure out all of the *.f files.
# Compile into the source directory - the project Makefile/build will
#   manage placement

.PHONY: all clean build compile 
.DEFAULT_GOAL := all

objects = ushcn_dist.v5a.combo ushcn_corr.v5a.combo ushcn_tobs.v4a \
	ushcn_fill.v4p gen-mon-composites.v2
	
all: compile build

compile: ushcn_dist.v5a.combo ushcn_corr.v5a.combo \
	ushcn_tobs.v4a ushcn_fill.v4p gen-mon-composites.v2

ushcn_dist.v5a.combo: ushcn_dist.v5a.combo.f prehomog.parm.incl
	gfortran -fno-sign-zero ushcn_dist.v5a.combo.f -o ushcn_dist.v5a.combo

ushcn_corr.v5a.combo: ushcn_corr.v5a.combo.f prehomog.parm.incl \
	prehomog.comm.incl prehomog.corr.incl
	gfortran -fno-sign-zero ushcn_corr.v5a.combo.f -o ushcn_corr.v5a.combo

ushcn_tobs.v4a: ushcn_tobs.v4a.f prehomog.parm.incl
	gfortran -fno-sign-zero ushcn_tobs.v4a.f -o ushcn_tobs.v4a

ushcn_fill.v4p: ushcn_fill.v4p.f filnet_subs.v4p.f posthomog.parm.incl \
	posthomog.fill.incl posthomog.comm.incl
	gfortran -fno-sign-zero ushcn_fill.v4p.f filnet_subs.v4p.f -o ushcn_fill.v4p

gen-mon-composites.v2: gen-mon-composites.v2.f95
	gfortran -fno-sign-zero gen-mon-composites.v2.f95 -o gen-mon-composites.v2

clean:
	rm -f $(objects)

build: 
	cp -v $(objects) ../build/code/bin
