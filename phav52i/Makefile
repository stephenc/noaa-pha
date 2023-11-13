# ---------------------- PHA_v52i Public Release ---------------------
# @Updated by: Claude Williams
# @Date: December 2011
# @Date: March 2012
# @Date: September 2012
#
# This Makefile Template defines the basic steps required to make a
# project.  An operational process must contain these steps 
# to successfully be installed.
#
# NOTE: Public Make files assume NETDEF=TEST
#

.PHONY: clean check compile build install uninstall
.DEFAULT_GOAL := all

$(info --- Target to be processed '$(MAKECMDGOALS)')

PROJECT := pha_v52i
installscrpt := $(PROJECT).sh

ifeq ($(strip $(INSTALLDIR)),)
  INSTALLDIR := ~/$(PROJECT)
endif

# Use default if test data directory is undefined
ifeq ($(strip $(TESTDATA)),)
  # Default test data comes from the exported directory
  TESTDATA := ../testdata/benchmark.tar.gz
endif

all: clean compile build

help: 
$(info               ----Currently defined INSTALL parameters----)
$(info  )
$(info                    Default Installation directory)
$(info  )
$(info      Install the test run and insert test data)
$(info      INSTALLDIR: $(INSTALLDIR))
$(info  )
$(info      Path and file to the compressed PHA test input data)
$(info      including the Configuration file for this data)
$(info      and the Default Parameter Combination)
$(info      TESTDATA: $(TESTDATA))

clean:
	# Clean compile and build actions
	make clean -C lib_src/
	make clean -C source_expand/
	rm -rf build/

check:
	# Check dependancies here

compile: check
  # Compile apps with the Make in each source dir
	make compile -C lib_src
	make compile -C source_expand

build: 
	# Collect apps and scripts for processing
	mkdir -p build/code/bin
	mkdir -p build/data
	mkdir -p build/runlogs
	make build -C lib_src
	make build -C source_expand
	make build -C scripts/combo_runs
	cp -v readme.quick_start build

install: uninstall all
	# Install the Scripts & Codes here
	$(info INSTALLDIR: $(INSTALLDIR))
	mkdir -p $(INSTALLDIR)
        # Path and file to the compressed PHA test input data
        #   including the Configuration file specific to this data
	$(info TESTDATA: $(TESTDATA))
        # Path and file to the Combinations to be used in the test
	$(info MIXDATA: $(MIXDATA))
	mkdir -p $(INSTALLDIR)
	cp -vr build/* $(INSTALLDIR)
	tar -xzf $(TESTDATA) -C $(INSTALLDIR)/data
	cp -v $(INSTALLDIR)/data/*.conf $(INSTALLDIR)

uninstall:
	# Remove installation directory
	if test -d $(INSTALLDIR); \
	  then \
	  rm -rf $(INSTALLDIR)/code; \
	  rm -rf $(INSTALLDIR)/data; \
	  rm -rf $(INSTALLDIR)/runlogs; \
	  rm -f $(INSTALLDIR)/*.sh; \
	  rm -f $(INSTALLDIR)/*.conf; \
	  rm -f $(INSTALLDIR)/readme.*; \
	  rmdir $(INSTALLDIR); \
	else \
	  echo "Nothing to do"; \
	fi
