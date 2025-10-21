# Building coma-sci-backend

Jan Kleyna's COMA Science Backend - Astronomical image processing and analysis web service

## Important Note

This is a self-contained repository with integrated Docker support. The codebase includes all source code, build scripts, and Docker configuration in one location.

**Key features:**
- Self-contained build (Dockerfile and source in same directory)
- Runtime initialization for large datasets (ASTORB)
- Modern YASON API compatibility
- Optimized for containerized deployment

## Prerequisites

- SBCL (Steel Bank Common Lisp)
- Quicklisp
- buildapp (installed via Quicklisp)
- Native libraries installed (see below)

## Native Library Dependencies

The following native libraries must be installed and available in `/usr/local/lib`:

### Required Libraries

1. **libcfitsio.so** - FITS file I/O (with --enable-reentrant)
2. **libslalib.so** - SLALIB astronomical library
3. **libsolkep.so** - Simple Kepler orbit solver
4. **libkdtree.so** - KD-tree spatial indexing
5. **libconcaveman.so** - Concave hull computation
6. **libtsnnls.so** - Sparse non-negative least squares
7. **libxpa.so** - XPA inter-process communication
8. **libwcs.so** - WCS (World Coordinate System)
9. **PGPLOT** - Graphics library
10. **FFTW3** - Fast Fourier Transform library

### Optional Libraries

- **libdigest2.so** - MPC NEO orbit classifier (if digest2 features enabled)

## Quick Build

```bash
cd /path/to/coma-sci-backend

# Load and verify system
sbcl --load build.lisp

# Or build executable directly with buildapp
buildapp --output coma-sci-backend \
         --dynamic-space-size 4096 \
         --asdf-tree . \
         --load-system coma-sci-backend \
         --entry coma-sci-backend:main
```

## Docker Build (Recommended)

The recommended way to build is using Docker, which handles all dependencies:

```bash
cd /path/to/coma-sci-backend
docker-compose up --build
```

The Docker build will:
1. Install all native library dependencies
2. Build custom libraries (CFITSIO, SLALIB, nrwavelets, simple-kepler-solver)
3. Install SBCL, Quicklisp, and buildapp
4. Install Quicklisp dependencies
5. Download latest ASTORB database from Lowell Observatory
6. Build the executable with buildapp
7. Create `/usr/local/bin/coma-sci-backend`

## Running the Server

### Standalone Executable

```bash
# Default settings (port 8080, host 0.0.0.0)
# Note: Docker deployment uses port 5054
./coma-sci-backend

# Custom settings via environment variables
COMA_PORT=5054 COMA_HOST=0.0.0.0 ./coma-sci-backend

# With increased heap size for ASTORB compilation (8GB)
./coma-sci-backend --dynamic-space-size 8192
```

### From SBCL REPL

```lisp
(ql:quickload :coma-sci-backend)
(coma-sci-backend:launch-coma-json-server-web-interface :port 5054 :host "0.0.0.0")
```

## Runtime Initialization

When the server starts, `coma-sci-backend:main` performs runtime initialization:

1. **Initializes lparallel kernel** (4 worker threads)
   - MUST be at runtime: buildapp cannot save core with multiple threads
2. **Initializes ASTORB file list**
   - Finds the latest ASTORB database file
3. **Loads ASTORB asteroid database**
   - Large dataset (~200MB compressed) loaded at runtime
   - Compiles to FASL cache on first run for faster subsequent startups
4. **Initializes orbit elements** for small body identification
   - Depends on ASTORB data

**Note:** Most smaller datasets (observatory data, Landolt standards, small body names, comet data) are now loaded at compile-time via `eval-when` blocks. Only lparallel and ASTORB require runtime initialization.

## Configuration

### Environment Variables

- `COMA_PORT` - Server port (default: 8080, Docker uses 5054)
- `COMA_HOST` - Server host (default: 0.0.0.0)
- `COMA_SERVER_HOST` - External hostname for callback URLs
- `VIZQUERY_PROGRAM` - Path to vizquery program (CDS catalog access)
- `VIZQUERY_SITE` - VizieR mirror site to use
- `TERAPIX_DIRECTORY` - Directory for TERAPIX tools output
- `LISP_LIB` - Path to Lisp library source inside container (default: /opt/lisp-lib)
- `LISP_LIB_DATADIR` - Path to runtime data directory (default: /data/support/sci-backend)
- `LD_LIBRARY_PATH` - Should include `/usr/local/lib` for native libraries

### Data Directories

The server expects the following directory structure:

```
/data/
├── support/
│   ├── sci-backend/
│   │   ├── astorb/        # ASTORB database (downloaded at build, persisted via volume)
│   │   ├── cache/         # Runtime cache
│   │   ├── orbits/        # Cached orbit data
│   │   ├── work/          # Working directory
│   │   ├── catalogs/      # Star catalogs
│   │   └── config/        # Configuration files
└── [telescope_data]/      # Telescope FITS data directories (optional, configured via volumes)
```

## Troubleshooting

### Missing Native Libraries

```bash
# Check which libraries are missing
ldd ./coma-sci-backend

# Verify library paths
ldconfig -p | grep -E "slalib|cfitsio|kdtree|xpa|nrwavelets|solkep"
```

### ASDF Can't Find Systems

Ensure `coma-sci-backend.asd` is loading correctly and registering all source paths. The .asd file uses ASDF's `:tree` feature to recursively find all packages.

### Data Files Not Loading

If you see warnings about missing data files at runtime:
1. Ensure data files are in `astro/*/data/` directories
2. Check that volume mounts (in Docker) point to the correct locations
3. Verify file permissions

### Build Errors

Check that all Quicklisp dependencies are installed:

```bash
sbcl --eval '(ql:quickload :yason)' \
     --eval '(ql:quickload :hunchentoot)' \
     --eval '(ql:quickload :cffi)' \
     --eval '(quit)'
```

## Testing the Build

```bash
# Start server (with 8GB heap for ASTORB)
./coma-sci-backend --dynamic-space-size 8192 &

# Test JSON submission (adjust port if using Docker)
curl -X POST http://localhost:5054/submit-json \
     -H "Content-Type: application/json" \
     -d '{"TYPE":"REQUEST","COMMAND":"HELLO","ID":"test123"}'

# Or test with Docker
docker exec sci-backend curl -f http://localhost:5054/health
```

## Docker Architecture Notes

This repository is optimized for Docker deployment with the following key design decisions:

### Self-Contained Build
- Dockerfile and docker-compose.yml are in the repository root
- Build context is the current directory (context: .)
- No need for separate build/source directories

### Runtime vs Build-Time Initialization
- **Build-time** (via eval-when): Small datasets compiled into executable
- **Runtime** (in main.lisp): lparallel threads and ASTORB database
- ASTORB downloaded during Docker build, copied to volume at container startup

### Entry Point Script
- `docker-entrypoint.sh` handles:
  - ASTORB database copying from build location to volume
  - Passing --dynamic-space-size 8192 flag for adequate heap space
  - Container initialization logging

### Volume Strategy
- Named volume `sci-backend-data` persists ASTORB database and FASL cache
- FITS data directories are commented out by default for portability
- Uncomment volume mounts in docker-compose.yml for site-specific FITS sources

### YASON Compatibility
- Code uses modern YASON API (removed `*yason-float-type*`)
- Compatible with YASON 20250622 and later
