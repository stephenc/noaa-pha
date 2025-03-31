# Compiler settings
FC = gfortran
FCFLAGS = -O2 # -Wall -Wextra -Wno-unused-parameter -fPIC
F77FLAGS = -ffixed-form -fno-range-check -fno-sign-zero -std=legacy
F95FLAGS = -cpp

# Directories (use absolute paths internally for robustness, though not strictly required)
# CURDIR is a Make built-in variable representing the current working directory.
# Using $(CURDIR)/ ensures paths are absolute, reducing ambiguity.
# Note: This doesn't directly solve whitespace, quoting does, but it's good practice.
ABS_SRC_DIR = $(CURDIR)/src
ABS_INC_DIR = $(ABS_SRC_DIR)/incl
ABS_F77_DIR = $(ABS_SRC_DIR)/f
ABS_F95_DIR = $(ABS_SRC_DIR)/f95
ABS_AWK_DIR = $(ABS_SRC_DIR)/awk
ABS_OBJ_DIR = $(CURDIR)/obj
ABS_BIN_DIR = $(CURDIR)/bin

# --- Cross-Platform Python ---
PYTHON := $(shell command -v python3 || command -v python)
ifeq ($(PYTHON),)
$(error "Python interpreter not found (tried 'python3' and 'python'). Please install Python 3.")
endif

# --- F77 Programs ---
F77_PROGRAMS = ushcn_corr.v5a.combo ushcn_dist.v6.combo ushcn_fill.v4p

# --- AWK Scripts ---
# Find source files (assuming .awk extension, adjust if needed)
AWK_SOURCES = $(wildcard $(ABS_AWK_DIR)/*.awk)
# Define target executable names (same basename, no extension, in bin dir)
AWK_TARGETS = $(patsubst $(ABS_AWK_DIR)/%.awk, $(ABS_BIN_DIR)/%, $(AWK_SOURCES))

# --- Prerequisites ---
# Ensure directories exist before trying to use them
DIRS := $(ABS_OBJ_DIR) $(ABS_BIN_DIR)
$(DIRS):
	@mkdir -p "$@"

F77_SOURCES = $(wildcard $(ABS_F77_DIR)/*.f)
F95_SOURCES = $(wildcard $(ABS_F95_DIR)/*.f95)

# Use CURDIR for potentially space-containing path to the script/Makefile
DEPS_SCRIPT = $(CURDIR)/generate_deps.py
MAKEFILE_PATH = $(firstword $(MAKEFILE_LIST)) # Get path to this Makefile

deps.mk: $(F77_SOURCES) $(F95_SOURCES) $(DEPS_SCRIPT) $(MAKEFILE_PATH) | $(DIRS)
	@cd $(CURDIR) && $(PYTHON) "$(DEPS_SCRIPT)" # Run python script from project root

.PHONY: all
all: deps.mk $(PROGRAMS:%=$(ABS_BIN_DIR)/%) $(AWK_TARGETS) ## Build all programs (F77 and F95) defined in deps.mk

.PHONY: test
test: unit-test output-test ## Run all tests

.PHONY: unit-test
unit-test: $(ABS_BIN_DIR)/PHATestUnits ## Run unit tests
	"$<" -d 20160316 -longflag test

.PHONY: output-test
output-test: $(ABS_BIN_DIR)/PHATestOutput ## Run output tests
	"$<"

##@ Utility
.PHONY: help
help:  ## Display this help
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[a-zA-Z0-9_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' "$(MAKEFILE_PATH)"

.PHONY: clean
clean: ## Remove generated object files, module files, binaries, and dependency file
	-rm -f "$(ABS_OBJ_DIR)"/*.o "$(ABS_OBJ_DIR)"/*.mod "$(ABS_BIN_DIR)"/* deps.mk fort.*

.DEFAULT_GOAL := all

# --- Generic Compilation Rules ---

# Rule to create executable awk scripts in bin/ from src/awk/
# Assumes source files have .awk extension. Target has no extension.
$(ABS_BIN_DIR)/%: $(ABS_AWK_DIR)/%.awk | $(DIRS)
	@echo '#!/usr/bin/env sh' > "$@"
	@echo 'dir=$$(CDPATH= cd -- "$$(dirname -- "$$0")" && pwd)' >> "$@"
	@echo 'awk -f "$${dir}/../src/awk/$(notdir $<)" "$$@"' >> "$@"
	@chmod +x "$@"

# Generic rule for F77 object files
# Quote paths for compiler arguments and automatic variables
$(ABS_OBJ_DIR)/%.o: $(ABS_F77_DIR)/%.f | $(DIRS)
	$(FC) $(FCFLAGS) $(F77FLAGS) -I"$(ABS_INC_DIR)" -c "$<" -o "$@"

# Generic F95 rule is not used as specific rules are generated in deps.mk

$(ABS_BIN_DIR)/ushcn_corr.v5a.combo: $(ABS_OBJ_DIR)/ushcn_corr.v5a.combo.o | $(DIRS)
	$(FC) $(FCFLAGS) $(F77FLAGS) "$<" -o "$@"

$(ABS_BIN_DIR)/ushcn_dist.v6.combo: $(ABS_OBJ_DIR)/ushcn_dist.v6.combo.o | $(DIRS)
	$(FC) $(FCFLAGS) $(F77FLAGS) "$<" -o "$@"

# For multiple object files, list them explicitly quoted instead of using $^
$(ABS_BIN_DIR)/ushcn_fill.v4p: $(ABS_OBJ_DIR)/ushcn_fill.v4p.o $(ABS_OBJ_DIR)/filnet_subs.v4p.o | $(DIRS)
	$(FC) $(FCFLAGS) $(F77FLAGS) "$(ABS_OBJ_DIR)/ushcn_fill.v4p.o" "$(ABS_OBJ_DIR)/filnet_subs.v4p.o" -o "$@"

-include deps.mk
