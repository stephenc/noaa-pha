# Documentation Index

Complete guide to all documentation in this repository.

## Documentation Structure

```
GHCNM v4 (Reconstructed)
│
├── START HERE
│   ├── README.md                           Main entry point, project overview
│   └── GETTING_STARTED.md                  Step-by-step first analysis
│
├── UNDERSTANDING THE SYSTEM
│   ├── WORKFLOW.md                         Complete pipeline diagrams
│   └── QUICK_REFERENCE.md                  One-page cheat sheet
│
├── TECHNICAL REFERENCE
│   ├── PROPERTIES_REFERENCE.md             Complete properties configuration
│   ├── examples/DATA_FORMAT_EXAMPLES.md    File format specifications
│   └── README.md (Programs section)        All program documentation
│
└── PRACTICAL TOOLS
    └── examples/prepare_sample_data.sh     Automated data setup script
```

## Where to Start

### I just want to run this...
[GETTING_STARTED.md](GETTING_STARTED.md) - Follow the 5-step quick start

### I want to understand what's happening
[WORKFLOW.md](WORKFLOW.md) - See visual diagrams of the complete process

### I need to look something up quickly
[QUICK_REFERENCE.md](QUICK_REFERENCE.md) - Commands and settings at a glance

### I need to configure properties
[PROPERTIES_REFERENCE.md](PROPERTIES_REFERENCE.md) - Complete properties reference

### I'm migrating from v52i
[V52I_TO_V4_COMPARISON.md](V52I_TO_V4_COMPARISON.md) - Parameter mapping and migration guide

### I'm having data format issues
[examples/DATA_FORMAT_EXAMPLES.md](examples/DATA_FORMAT_EXAMPLES.md) - Format specs with examples

### I want to automate data preparation
[examples/prepare_sample_data.sh](examples/prepare_sample_data.sh) - Run this script

## Document Descriptions

### [README.md](README.md)
**Main project documentation**
- What: 76KB comprehensive reference
- Covers: All programs, all file formats, all configuration options
- Use when: You need detailed technical specifications
- Sections:
  - Project changes from NOAA release
  - Build instructions (make, Docker)
  - Testing procedures
  - Inferred program functionality (PHAMain, ghcnm_qc, etc.)
  - Complete program reference (40+ programs)
  - Data format specifications (10+ formats)

### [GETTING_STARTED.md](GETTING_STARTED.md)
**Your first PHA analysis**
- What: Practical guide to running the software
- Covers: Build → Data → Config → Run
- Use when: You're new or need a reminder how to set up
- Topics:
  - Quick start (5 steps)
  - Where to get data
  - Directory structure
  - Configuration file creation
  - Understanding the workflow
  - Working with small datasets
  - Complete worked example
  - Troubleshooting common issues

### [WORKFLOW.md](WORKFLOW.md)
**Understanding the complete pipeline**
- What: Visual workflow diagrams and explanations
- Covers: Full GHCNM production pipeline, PHA internals
- Use when: You want to understand what happens under the hood
- Diagrams:
  - Full production pipeline (7 stages)
  - Simplified PHA-only workflow
  - File flow through PHAMain
  - Single station processing example
  - Configuration decision tree
  - Validation workflow
  - Troubleshooting flowcharts

### [QUICK_REFERENCE.md](QUICK_REFERENCE.md)
**One-page cheat sheet**
- What: Quick lookup for common operations
- Covers: Commands, settings, file formats, troubleshooting
- Use when: You know what you want but need syntax
- Includes:
  - Build commands
  - File naming patterns
  - Essential properties
  - Running programs
  - AWK one-liners
  - Performance tuning presets
  - Quick diagnosis table

### [examples/DATA_FORMAT_EXAMPLES.md](examples/DATA_FORMAT_EXAMPLES.md)
**File format reference with real examples**
- What: Detailed format specs with sample data
- Covers: All input/output file formats
- Use when: You're creating/parsing data files
- Formats covered:
  - Station inventory (.inv)
  - Monthly data (3-flag format)
  - Neighbor distance files
  - Neighbor correlation files
  - Properties files
  - GHCNM v4 official format
- Includes: Parsing code (Python, AWK, shell)

### [examples/prepare_sample_data.sh](examples/prepare_sample_data.sh)
**Automated data preparation script**
- What: Downloads and prepares sample data automatically
- Covers: Download → Extract → Format → Configure
- Use when: You want to test with real NOAA data
- What it does:
  - Downloads official GHCNM v4 data
  - Extracts regional subset
  - Converts to proper format
  - Creates directory structure
  - Generates configuration file

## Learning Paths

### Path 1: Quick Start (1 hour)
```
1. README.md (just Prerequisites & Building sections)
2. GETTING_STARTED.md (Quick Start section)
3. examples/prepare_sample_data.sh (run it)
4. Run PHAMain
5. QUICK_REFERENCE.md (bookmark for later)
```

### Path 2: Deep Understanding (4-6 hours)
```
1. README.md (full read)
2. WORKFLOW.md (study diagrams)
3. GETTING_STARTED.md (work through example)
4. examples/DATA_FORMAT_EXAMPLES.md (understand formats)
5. Run small test with your own data
6. Review log files and outputs
```

### Path 3: Data Wrangling Focus
```
1. examples/DATA_FORMAT_EXAMPLES.md (read all)
2. GETTING_STARTED.md (Data Preparation sections)
3. README.md (Programs & Data Formats sections)
4. WORKFLOW.md (Data Flow diagrams)
5. Practice converting custom data
```

### Path 4: Configuration & Tuning
```
1. GETTING_STARTED.md (Configuration Reference section)
2. QUICK_REFERENCE.md (Essential Properties)
3. README.md (Properties file format & Required Keys)
4. build/ghcnm-pha.test.properties (examine real config)
5. Experiment with parameter changes
6. WORKFLOW.md (Performance Considerations)
```

## Documentation by Task

### Building the Software
- [README.md § Building the Project](README.md#building-the-project)
- [README.md § Running Tests](README.md#running-tests)
- [GETTING_STARTED.md § Quick Start § Build](GETTING_STARTED.md#1-build-the-software)

### Getting Data
- [GETTING_STARTED.md § Obtain Input Data](GETTING_STARTED.md#2-obtain-input-data)
- [examples/prepare_sample_data.sh](examples/prepare_sample_data.sh)
- [examples/DATA_FORMAT_EXAMPLES.md § Converting from GHCNM v4](examples/DATA_FORMAT_EXAMPLES.md#converting-from-ghcnm-v4-official-format)

### Understanding File Formats
- [examples/DATA_FORMAT_EXAMPLES.md](examples/DATA_FORMAT_EXAMPLES.md) (start here)
- [README.md § Data Formats](README.md#data-formats)
- [QUICK_REFERENCE.md § Data Format Quick Reference](QUICK_REFERENCE.md#data-format-quick-reference)

### Setting Up Directories
- [GETTING_STARTED.md § Set Up Directory Structure](GETTING_STARTED.md#3-set-up-directory-structure)
- [QUICK_REFERENCE.md § Directory Setup](QUICK_REFERENCE.md#directory-setup)

### Creating Configuration Files
- [GETTING_STARTED.md § Create a Configuration File](GETTING_STARTED.md#4-create-a-configuration-file)
- [GETTING_STARTED.md § Configuration Reference](GETTING_STARTED.md#configuration-reference)
- [README.md § Properties File Format](README.md#properties-file-format-properties)
- [QUICK_REFERENCE.md § Essential Properties](QUICK_REFERENCE.md#essential-properties)

### Running the Analysis
- [GETTING_STARTED.md § Run the Analysis](GETTING_STARTED.md#5-run-the-analysis)
- [WORKFLOW.md § Simplified Workflow](WORKFLOW.md#simplified-workflow-for-running-pha-only)
- [QUICK_REFERENCE.md § Running Programs](QUICK_REFERENCE.md#running-programs)

### Understanding What PHAMain Does
- [GETTING_STARTED.md § Understanding the Workflow](GETTING_STARTED.md#understanding-the-workflow)
- [WORKFLOW.md § File Flow Through PHAMain](WORKFLOW.md#file-flow-through-phamain)
- [WORKFLOW.md § Data Flow: Single Station Example](WORKFLOW.md#data-flow-single-station-example)
- [README.md § PHAMain](README.md#phamain)

### Troubleshooting
- [GETTING_STARTED.md § Troubleshooting](GETTING_STARTED.md#troubleshooting)
- [WORKFLOW.md § Troubleshooting Workflow](WORKFLOW.md#troubleshooting-workflow)
- [QUICK_REFERENCE.md § Troubleshooting Commands](QUICK_REFERENCE.md#troubleshooting-commands)
- [QUICK_REFERENCE.md § Quick Diagnosis](QUICK_REFERENCE.md#quick-diagnosis)

### Using Other Programs
- [README.md § Programs](README.md#programs) (complete reference for 40+ programs)
- [QUICK_REFERENCE.md § Running Programs](QUICK_REFERENCE.md#running-programs)
- [WORKFLOW.md § Full Production Pipeline](WORKFLOW.md#full-production-pipeline)

### Performance Tuning
- [WORKFLOW.md § Performance Considerations](WORKFLOW.md#performance-considerations)
- [QUICK_REFERENCE.md § Tuning for Performance vs Quality](QUICK_REFERENCE.md#tuning-for-performance-vs-quality)

### Working with Small Datasets
- [GETTING_STARTED.md § Working with Small Datasets](GETTING_STARTED.md#working-with-small-datasets)
- [WORKFLOW.md § Configuration Decision Tree](WORKFLOW.md#configuration-decision-tree)

## FAQ: Which Document?

**Q: Where do I start if I'm completely new?**
A: [GETTING_STARTED.md](GETTING_STARTED.md) - Read through Quick Start section

**Q: How do I download real climate data?**
A: Run [examples/prepare_sample_data.sh](examples/prepare_sample_data.sh) or see [GETTING_STARTED.md § Obtain Input Data](GETTING_STARTED.md#2-obtain-input-data)

**Q: What does each program do?**
A: [README.md § Programs](README.md#programs) has detailed descriptions of all 40+ programs

**Q: How do I format my data files?**
A: [examples/DATA_FORMAT_EXAMPLES.md](examples/DATA_FORMAT_EXAMPLES.md) shows exact format with examples

**Q: What are all these properties settings?**
A: [GETTING_STARTED.md § Configuration Reference](GETTING_STARTED.md#configuration-reference) explains them

**Q: Why isn't it working?**
A: [QUICK_REFERENCE.md § Quick Diagnosis](QUICK_REFERENCE.md#quick-diagnosis) table shows common problems

**Q: What happens inside PHAMain?**
A: [WORKFLOW.md](WORKFLOW.md) has detailed diagrams and explanations

**Q: I need a quick command syntax...**
A: [QUICK_REFERENCE.md](QUICK_REFERENCE.md) is your one-page cheat sheet

**Q: How long will this take to run?**
A: [WORKFLOW.md § Performance Considerations](WORKFLOW.md#performance-considerations)

**Q: Can I process just a few stations?**
A: Yes! [GETTING_STARTED.md § Working with Small Datasets](GETTING_STARTED.md#working-with-small-datasets)

## File Formats Quick Finder

| Format | Example File | Documentation |
|--------|--------------|---------------|
| Station inventory | `ghcnm.inv` | [DATA_FORMAT_EXAMPLES § Inventory](examples/DATA_FORMAT_EXAMPLES.md#station-inventory-file-format) |
| Monthly data | `USC00084731.raw.tmax` | [DATA_FORMAT_EXAMPLES § Monthly Data](examples/DATA_FORMAT_EXAMPLES.md#monthly-temperature-data-format-3-flag) |
| Properties | `ghcnm-pha.properties` | [DATA_FORMAT_EXAMPLES § Properties](examples/DATA_FORMAT_EXAMPLES.md#properties-file-format) |
| Neighbors (distance) | `neighbors-distance.txt` | [DATA_FORMAT_EXAMPLES § Distance](examples/DATA_FORMAT_EXAMPLES.md#neighbor-distance-file-format) |
| Neighbors (correlation) | `neighbors-correlation.txt` | [DATA_FORMAT_EXAMPLES § Correlation](examples/DATA_FORMAT_EXAMPLES.md#neighbor-correlation-file-format) |

## Getting Additional Help

1. **Check the documentation** (you're reading the index!)
2. **Check the test files** in `build/` and `src/test/resources/`
3. **Check the log file** after running PHAMain
4. **Check the original NOAA release** at ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4/
5. **Check the GHCNM v4 papers** for methodology details

## Contributing to Documentation

If you find errors or want to add clarifications:

1. The main files are Markdown (.md)
2. Keep examples realistic and tested
3. Update this index if adding new documents
4. Cross-reference related sections
5. Include code examples where helpful

## Document Maintenance

| Document | Last Updated | Status |
|----------|--------------|--------|
| README.md | 2026-01-26 | Reconstructed from NOAA release |
| GETTING_STARTED.md | 2026-01-26 | New, comprehensive guide |
| WORKFLOW.md | 2026-01-26 | New, visual diagrams |
| QUICK_REFERENCE.md | 2026-01-26 | New, cheat sheet |
| DATA_FORMAT_EXAMPLES.md | 2026-01-26 | New, practical examples |
| prepare_sample_data.sh | 2026-01-26 | New, automation script |
| DOCUMENTATION_INDEX.md | 2026-01-26 | This file |

---

Ready to start? See [GETTING_STARTED.md](GETTING_STARTED.md)

Need quick lookup? See [QUICK_REFERENCE.md](QUICK_REFERENCE.md)

Want to understand it all? See [WORKFLOW.md](WORKFLOW.md)
