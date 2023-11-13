# Major compile of the PHA code

.PHONY: all compile clean build 
.DEFAULT_GOAL := all

objects = PHAv52i.FAST.MLY.*
objclean = lf95.err inhomog.parm.mthly.incl inhomog.parm.system.mthly.incl
 
all: compile build

compile: restart.skymod.f95 confirmdisp.skymod.f95 \
	ucpmonthly.v25d.f splitmerge.v22b.f chgptmodels.v7a.f \
	SHAPinp.v7c.f read_write.mthly.v6b.f acovf.f skyline.v1.f \
	inhomog.comm.mthly.incl  inhomog.MDparm.mthly.incl \
	inhomog.file.mthly.incl inhomog.restart.mthly.incl \
	parm_includes/inhomog.parm.MTHLY.TEST.incl \
	parm_includes/inhomog.parm.system.MLY.incl \
	inhomog.combo.mthly.incl
	../scripts/source_compiles/compile.sh PHAv52i FAST MLY TEST

clean:
	rm -f $(objclean)
	rm -f $(objects)

build:
	cp -v PHAv52i.FAST.MLY.TEST ../build/code/bin
