#!/usr/bin/env python3
import os
import re
import sys

# Define directories
SRC_DIR = "./src"
INC_DIR = os.path.join(SRC_DIR, "incl")
F77_DIR = os.path.join(SRC_DIR, "f")
F95_DIR = os.path.join(SRC_DIR, "f95")
ABS_OBJ_DIR = "./obj"
BIN_DIR = "./bin"

# Regex for F77 INCLUDE statements (case-insensitive)
F77_INCLUDE_RE = re.compile(r"^\s*INCLUDE\s+['\"]([^'\"]+)['\"]", re.IGNORECASE)
# Regex for F95 program/module/use (case-insensitive)
F95_PROGRAM_RE = re.compile(r"^\s*program\s+(\w+)", re.IGNORECASE)
F95_MODULE_RE = re.compile(r"^\s*module\s+(\w+)", re.IGNORECASE)
F95_USE_RE = re.compile(r"^\s*use\s+(\w+)", re.IGNORECASE)


def parse_f77_file(file_path):
    """Parse an F77 file to find INCLUDE dependencies."""
    includes_set = set() # Use a set to store unique includes
    try:
        with open(file_path, "r") as f:
            for line in f:
                # Simple check for fixed-form comment (C, c, *)
                if line.startswith(("*", "c", "C")):
                    continue
                match = F77_INCLUDE_RE.match(line)
                if match:
                    include_file = match.group(1).strip()
                    # Ensure we only add relative paths if found
                    if "/" not in include_file and "\\" not in include_file:
                        includes_set.add(include_file) # Add to set (handles duplicates)
    except FileNotFoundError:
        print(f"Warning: Could not find file {file_path}", file=sys.stderr)
        return []
    except Exception as e:
        print(
            f"Warning: Error reading file {file_path}: {e}", file=sys.stderr
        )
        return []
    return list(includes_set) # Convert back to list before returning


def parse_f95_file(file_path):
    """Parse an F95 file for program/module info and dependencies."""
    is_program = False
    program_name = None
    module_name = None
    used_modules = []
    try:
        with open(file_path, "r") as f:
            lines = f.readlines()
    except FileNotFoundError:
        print(f"Warning: Could not find file {file_path}", file=sys.stderr)
        return None, None, []
    except Exception as e:
        print(
            f"Warning: Error reading file {file_path}: {e}", file=sys.stderr
        )
        return None, None, []

    for line in lines:
        line = line.strip()
        if line.startswith("!"):  # Skip comments
            continue
        # Match 'program' statement
        match = F95_PROGRAM_RE.match(line)
        if match:
            is_program = True
            program_name = match.group(1)
        # Match 'module' statement (take the first one)
        match = F95_MODULE_RE.match(line)
        if match and module_name is None:
            module_name = match.group(1)
        # Match 'use' statement
        match = F95_USE_RE.match(line)
        if match:
            used_module = match.group(1)
            if used_module not in used_modules:  # Avoid duplicates
                used_modules.append(used_module)

    if is_program:
        return "program", program_name, used_modules
    elif module_name:
        return "module", module_name, used_modules
    return None, None, []  # File is neither a program nor a module


def get_all_deps(module, module_uses, memo=None):
    """Recursively compute all modules that a given module depends on."""
    if memo is None:
        memo = {}
    if module in memo:
        return memo[module]

    deps = set(module_uses.get(module, []))
    for used_module in module_uses.get(module, []):
        deps.update(get_all_deps(used_module, module_uses, memo))

    memo[module] = deps
    return deps


def generate_deps():
    """Generate deps.mk with F77 and F95 dependencies."""
    # --- F95 Processing ---
    f95_files = []
    if os.path.isdir(F95_DIR):
        f95_files = [
            f for f in os.listdir(F95_DIR) if f.lower().endswith(".f95")
        ]
    else:
        print(f"Warning: F95 directory not found: {F95_DIR}", file=sys.stderr)

    module_to_file = {}  # Maps module name to defining file base
    file_to_used_modules = {}  # Maps file base to list of used modules
    f95_programs = []  # List of program file bases

    for file in f95_files:
        base = os.path.splitext(file)[0]
        file_path = os.path.join(F95_DIR, file)
        type_, name, used_modules = parse_f95_file(file_path)

        if type_ == "program":
            f95_programs.append(base)
            file_to_used_modules[base] = used_modules
        elif type_ == "module":
            if name in module_to_file:
                print(
                    f"Warning: Module '{name}' defined in multiple files:"
                    f" '{module_to_file[name]}.f95' and '{base}.f95'",
                    file=sys.stderr,
                )
            module_to_file[name] = base
            file_to_used_modules[base] = used_modules

    module_uses = {
        module: file_to_used_modules[file]
        for module, file in module_to_file.items()
        if file in file_to_used_modules
    }

    # --- F77 Processing ---
    f77_files = []
    if os.path.isdir(F77_DIR):
        f77_files = [
            f for f in os.listdir(F77_DIR) if f.lower().endswith(".f")
        ]
    else:
        print(f"Warning: F77 directory not found: {F77_DIR}", file=sys.stderr)

    f77_includes = {}  # Maps f77 file base to list of include files

    for file in f77_files:
        base = os.path.splitext(file)[0]
        file_path = os.path.join(F77_DIR, file)
        includes = parse_f77_file(file_path)
        if includes:
            f77_includes[base] = includes

    # --- Generate deps.mk ---
    try:
        with open("deps.mk", "w") as f:
            f.write(
                "# Automatically generated dependencies by generate_deps.py\n"
            )
            f.write("# DO NOT EDIT THIS FILE MANUALLY\n\n")

            # Combine F77 and F95 programs (use existing F77_PROGRAMS from Makefile)
            f.write("# List of all program executables to be built\n")
            f.write(
                f'PROGRAMS = $(F77_PROGRAMS) {" ".join(f95_programs)}\n\n'
            )

            f.write("# Default target: build all programs\n")
            f.write("all: $(PROGRAMS:%=$(ABS_BIN_DIR)/%)\n\n")

            # --- F95 Rules ---
            f.write("# --- F95 Dependencies ---\n\n")
            # Linking rules for each F95 program
            for program in f95_programs:
                direct_modules = file_to_used_modules.get(program, [])
                all_modules = set(direct_modules)
                memo_deps = {} # Memoization for get_all_deps
                for module in direct_modules:
                    if module in module_uses:
                        all_modules.update(get_all_deps(module, module_uses, memo_deps))
                    elif module not in module_to_file:
                        print(f"Warning: Module '{module}' used by program '{program}' but not found.", file=sys.stderr)


                # Map modules to their object files
                module_o_files = [
                    f"$(ABS_OBJ_DIR)/{module_to_file[m]}.o"
                    for m in all_modules
                    if m in module_to_file
                ]
                f.write(
                    f"$(ABS_BIN_DIR)/{program}: $(ABS_OBJ_DIR)/{program}.o {' '.join(module_o_files)}\n"
                )
                f.write(
                    "\t$(FC) $(FCFLAGS) $(F95FLAGS) $^ -o $@\n\n"
                )

            # Compilation rules for all F95 source files
            for file in f95_files:
                base = os.path.splitext(file)[0]
                used_modules = file_to_used_modules.get(base, [])
                # Map used modules to their .mod files
                used_mod_files = [
                    f"$(ABS_OBJ_DIR)/{m}.mod"
                    for m in used_modules
                    if m in module_to_file # Only depend on mods we know how to build
                ]
                f.write(
                    f"$(ABS_OBJ_DIR)/{base}.o: $(ABS_F95_DIR)/{file} {' '.join(used_mod_files)}\n"
                )
                # Use -J to specify where .mod files are created/found
                f.write(
                    "\t$(FC) $(FCFLAGS) $(F95FLAGS) -I$(ABS_OBJ_DIR) -J$(ABS_OBJ_DIR) -c $< -o $@\n\n"
                )

            # Rules for .mod files (depend on the .o file that creates them)
            for module, file_base in module_to_file.items():
                f.write(f"$(ABS_OBJ_DIR)/{module}.mod: $(ABS_OBJ_DIR)/{file_base}.o\n")

            # --- F77 Rules ---
            f.write("# --- F77 Dependencies ---\n\n")
            # Compilation rules for F77 files with specific INCLUDE dependencies
            for base, includes in f77_includes.items():
                include_paths = [
                    f"$(ABS_INC_DIR)/{inc}" for inc in includes
                ]
                f.write(
                    f"$(ABS_OBJ_DIR)/{base}.o: $(ABS_F77_DIR)/{base}.f {' '.join(include_paths)}\n"
                )
                # The actual compilation command comes from the generic rule in Makefile

    except IOError as e:
        print(f"Error writing deps.mk: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    # Ensure required directories exist before proceeding
    if not os.path.isdir(ABS_OBJ_DIR):
        os.makedirs(ABS_OBJ_DIR)
        print(f"Created directory: {ABS_OBJ_DIR}")
    if not os.path.isdir(BIN_DIR):
        os.makedirs(BIN_DIR)
        print(f"Created directory: {BIN_DIR}")

    generate_deps()
