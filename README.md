# COMA Science Backend

Jan Kleyna's COMA Science Backend - A comprehensive astronomical image processing and analysis web service.

## Overview

This repository contains the **coma-sci-backend** web service and Jan Kleyna's complete astronomical software library. The repository is self-contained with integrated Docker support for easy deployment while preserving all scientific functionality.

## Credits

**All code in this repository was written by Jan Kleyna** at the University of Hawaii Institute for Astronomy. This is his comprehensive lisp-lib collection with Docker-specific modifications applied for the COMA project deployment.

## Architecture

### Self-Contained Design

This repository includes everything needed for deployment:
- Source code for all packages
- Dockerfile and docker-compose.yml
- Build scripts and configuration
- Small data files (compiled into executable)
- Docker entrypoint script for runtime initialization

### Build-Time vs. Runtime Loading

**Compile-Time Loading** (via eval-when blocks):
- Observatory coordinates
- Landolt photometric standards
- Small body name mappings
- Comet orbital data
- Orbit element templates

These small datasets (~few MB) are compiled into the executable for optimal performance.

**Runtime Loading** (in main.lisp):
- **lparallel kernel**: Cannot be saved in buildapp core (threads limitation)
- **ASTORB database**: Large dataset (~200MB compressed, ~1GB FASL)
- **Orbit elements**: Derived from ASTORB

### Key Modifications for Docker

**Critical changes** (must be preserved):
1. `astro/small-body-identify/sbid-search.lisp` - lparallel initialization commented out
2. `astro/astorb/astorb.lisp` - ASTORB loading commented out
3. `coma-json-server/main.lisp` - Entry point with runtime initialization
4. `coma-json-server/dispatcher.lisp` - Modern YASON API (no `*yason-float-type*`)

**Removed for cleanliness**:
- Development tools (slime/, asdf/, INIT-FILES/)
- Local quicklisp-systems/ directory
- Build artifacts and symbolic links

## Repository Structure

```
coma-sci-backend/
├── Dockerfile               # Docker build configuration
├── docker-compose.yml       # Docker Compose orchestration
├── docker-entrypoint.sh     # Container startup script
├── coma-sci-backend.asd     # ASDF system definition
├── build.lisp               # Build script
├── BUILD.md                 # Build documentation
├── DOCKER_BUILD.md          # Docker-specific build notes
├── PACKAGE_LIST.md          # Complete package inventory
├── README.md                # This file
├── coma-json-server/        # Main JSON web service
│   ├── main.lisp            # Runtime initialization entry point
│   ├── dispatcher.lisp      # JSON request routing
│   ├── web-service.lisp     # Hunchentoot server setup
│   └── ...                  # API endpoint implementations
├── astro/                   # Astronomy-specific packages (42 packages)
├── jlib/                    # General utility libraries (29 packages)
├── cl-pgplot/               # PGPLOT plotting bindings
├── external-packages/       # Third-party packages
└── www-utils/               # Web utilities (HTML parsing)
```

## Package Inventory

### Main Application
- **coma-json-server**: JSON web service providing astronomical computations

### Astronomy Packages (38 packages)
Core functionality:
- **imutils**: Image processing utilities
- **instrument-id**: Camera/instrument identification
- **imred**: Image reduction pipeline
- **cfitsio**: FITS file I/O bindings
- **wcs**: World Coordinate System support
- **terapix**: Astrometry and photometry (SExtractor, SCAMP wrappers)

Ephemeris and orbital mechanics:
- **slalib**: SLALIB positional astronomy library bindings
- **slalib-ephem**: Ephemeris calculations using SLALIB
- **kepler-orbit**: Keplerian orbit calculations
- **orbital-elements**: Orbital element representations
- **orbital-elements-parse**: Parsing orbital elements
- **orbital-mech**: Orbital mechanics utilities
- **jpl-horizons**: JPL Horizons ephemeris service client
- **mpc**: Minor Planet Center data access

Photometry and calibration:
- **phot-calib**: Photometric calibration
- **phot-transforms**: Color transformations
- **landolt-standards**: Landolt standard star data

Object identification and catalogs:
- **small-body-name**: Small body name parsing
- **small-body-identify**: Object identification
- **astorb**: Asteroid orbital database
- **asteroids**: Asteroid utilities
- **astro-catalog**: Star catalog access
- **id-ss-object-at-pos**: Identify solar system objects at position
- **brute-force-comets**: Comet identification

Coordinate systems:
- **astro-coords**: Coordinate transformations
- **astro-time**: Time system conversions
- **ra-dec**: Right ascension/declination utilities
- **precess**: Precession calculations
- **sky-project**: Sky projection algorithms

Image analysis:
- **shift-and-add**: Co-addition of moving object images
- **fits-stamp**: FITS image stamp creation
- **isochrones-isodynes**: Syndyne/synchrone modeling
- **simple-comet-activity-detector**: Comet activity detection
- **a-f-rho**: Afρ dust production calculations

Supporting:
- **observatories**: Observatory coordinates database
- **astro-obj**: Astronomical object representations
- **magnitude-of-sun**: Solar magnitudes
- **mpc-packed-desig**: MPC packed designation format

Instrument-specific:
- **gmos-proc**: Gemini GMOS processing
- **suprime-cam-fix**: Subaru SuprimeCam fixes
- **hypersuprime-cam-fix**: Subaru HyperSuprimeCam fixes

### Utility Libraries (26 packages in jlib/)
Mathematical, image processing, data structures, parsing, and system utilities.

### Plotting
- **cl-pgplot**: Common Lisp bindings for PGPLOT graphics library

### External Packages
- **lparallel**: Parallel processing library
- **re** (massung-regexp): Regular expression library
- **cl-html-parse-walker**: HTML parsing utilities

## Dependencies

### Quicklisp Dependencies
See `qlfile` for complete list. Key dependencies:
- **yason**: JSON encoding/decoding (version 20250622+)
- **alexandria**: Common utilities
- **hunchentoot**: Web server
- **drakma**: HTTP client
- **cl-ppcre**: Perl-compatible regular expressions
- **cffi**: Foreign function interface
- **bordeaux-threads**: Portable threading
- And more...

### System Dependencies
The application uses FFI bindings to several C libraries:
- **CFITSIO**: FITS file I/O (libcfitsio) - must be thread-safe (--enable-reentrant)
- **SLALIB**: Positional astronomy library
- **FFTW3**: Fast Fourier transforms
- **PGPLOT**: Plotting library (optional, for diagnostics)
- **libsolkep.so**, **libkdtree.so**, **libconcaveman.so**, **libtsnnls.so**, etc.

These shared libraries (.so/.dylib) must be available in the system library path.

## Building

### Using Docker (Recommended)

```bash
cd /path/to/coma-sci-backend
docker-compose up --build
```

The Docker build:
1. Installs all native library dependencies
2. Builds custom libraries (CFITSIO with thread support, SLALIB, nrwavelets, simple-kepler-solver)
3. Installs SBCL, Quicklisp, and buildapp
4. Copies the repository (COPY . .)
5. Builds nrwavelets C library
6. Loads all Quicklisp dependencies
7. Downloads latest ASTORB database from Lowell Observatory
8. Builds executable with buildapp (4GB heap)
9. Creates `/usr/local/bin/coma-sci-backend`

The built image includes:
- All native libraries in `/usr/local/lib`
- SBCL executable with all code compiled
- ASTORB database ready for volume initialization
- Entry point script for runtime setup

### Manual Build

See `BUILD.md` for detailed manual build instructions.

## Running

### With Docker

```bash
cd /path/to/coma-sci-backend
docker-compose up -d
```

The container will:
1. Copy ASTORB database from image to named volume (first run only)
2. Launch executable with 8GB heap
3. Initialize lparallel kernel
4. Load ASTORB database (compile FASL on first run)
5. Start web server on port 5054

Check status:
```bash
docker logs sci-backend
docker exec sci-backend curl -f http://localhost:5054/health
```

### Standalone Executable

```bash
# Default settings (port 8080, host 0.0.0.0)
./coma-sci-backend

# Docker deployment settings
COMA_PORT=5054 COMA_HOST=0.0.0.0 ./coma-sci-backend --dynamic-space-size 8192
```

The `main()` function in `coma-sci-backend:main`:
1. Parses environment variables (COMA_PORT, COMA_HOST)
2. Initializes lparallel kernel (4 worker threads)
3. Loads ASTORB database (compiles FASL on first run)
4. Initializes orbit elements for object identification
5. Launches the Hunchentoot web server on configured port
6. Runs indefinitely

## Configuration

### Environment Variables

- **COMA_PORT** - Web server port (default: 8080, Docker uses 5054)
- **COMA_HOST** - Web server host (default: 0.0.0.0)
- **COMA_SERVER_HOST** - External hostname for callback URLs
- **VIZQUERY_PROGRAM** - Path to vizquery program for CDS catalog access
- **VIZQUERY_SITE** - VizieR mirror site to use
- **TERAPIX_DIRECTORY** - Directory for TERAPIX tools output
- **LISP_LIB** - Path to source code (set by Docker)
- **LISP_LIB_DATADIR** - Path to runtime data (set by Docker)
- **LD_LIBRARY_PATH** - Must include `/usr/local/lib`

### Volume Mounts

The `docker-compose.yml` defines:
- **sci-backend-data** - Named volume for ASTORB database and FASL cache
- Optional FITS data directories (commented out by default for portability)

### Data Files

**Compiled into executable:**
- Small datasets in `astro/*/data/` directories (~few MB total)
- Loaded at compile-time via eval-when blocks

**Loaded at runtime:**
- ASTORB database downloaded during Docker build
- Copied to volume at container startup
- FASL cache persisted for fast restarts

## Maintaining Docker Compatibility

### Critical Requirements

These changes MUST be preserved:

1. **lparallel initialization** - `sbid-search.lisp` eval-when stays commented out
2. **ASTORB loading** - `astorb.lisp` eval-when stays commented out
3. **Runtime initialization** - `main.lisp` calls lparallel and ASTORB init
4. **YASON compatibility** - Don't use deprecated `*yason-float-type*`

### What Works Correctly

- All small dataset eval-when blocks load at compile-time
- No need to comment out observatory, Landolt, small-body-name, or comet data loading
- These are efficiently compiled into the executable

## License

Please refer to Jan Kleyna for licensing information. This code is part of his research work at the University of Hawaii Institute for Astronomy.

## Contact

For questions about the code, please contact Jan Kleyna at the University of Hawaii Institute for Astronomy.

## Acknowledgments

This entire codebase is the work of **Jan Kleyna**. The Docker-specific modifications were made to enable containerized deployment for the COMA project while preserving all original functionality.
