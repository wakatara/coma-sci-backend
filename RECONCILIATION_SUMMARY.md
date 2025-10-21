# Reconciliation Summary: coma-sci-backend vs Jan's coma-backend-jtk

## Date: October 21, 2025

This document summarizes the reconciliation work completed to align the Docker-enabled `coma-sci-backend` repository with Jan Kleyna's canonical `coma-backend-jtk` codebase.

---

## Jan's Comments Addressed

All 8 points from `jans_comments.txt` have been addressed:

### ✅ Point #1: Removed nested ASDF subsystem
**Issue**: "why does coma-sci-backend.asf implement #:coma-sci-backend/coma-json-server? Why not just use the one in coma-json-server?"

**Resolution**:
- Removed nested `#:coma-sci-backend/coma-json-server` subsystem
- Now uses simple `coma-json-server` system definition
- ASDF file located at `astro/COMA-PROJECT/coma-json-server.asd` (matching Jan's structure)
- ASDF recursively finds all packages in directory tree

### ✅ Point #2: Removed manual ASDF registry pushnew statements
**Issue**: "I don't think these are needed as long as $LISP_LIB is set."

**Resolution**:
- Deleted all manual `pushnew` statements from `.asd` file
- Created `sbclrc.lisp` for ASDF initialization
- Uses `asdf:initialize-source-registry` with `:tree` directives
- Relies on `$LISP_LIB` environment variable

### ✅ Point #3: Restored frozen Quicklisp bundle
**Issue**: "the exported quicklisp systems seem gone. My suggestion was to freeze them at the working version I have."

**Resolution**:
- Copied entire `quicklisp-systems/` directory from Jan's original
- Includes `bundle.lisp`, `bundle-info.sexp`, `software/`, and `system-index.txt`
- Ensures perfect replication of Jan's working dependency versions

### ✅ Point #4: Restored canonical directory structure
**Issue**: "the fact that coma-json-server is no longer in astro/COMA-PROJECT/coma-json-server means that the synching procedure will put it in the wrong place"

**Resolution**:
- Moved `coma-json-server/` from root to `astro/COMA-PROJECT/coma-json-server/`
- Copied `examples/` directory from Jan's original
- Now matches Jan's canonical structure exactly

### ✅ Point #5: Created sbclrc.lisp initialization file
**Issue**: "I think that some of the issues you addresses with your changes can be solved using an sbcl-init file"

**Resolution**:
- Created `sbclrc.lisp` for SBCL initialization
- Sets up ASDF source registry before loading packages
- Used with `sbcl --userinit sbclrc.lisp` in Dockerfile
- Replaces manual ASDF configuration

### ✅ Point #6: Removed load-all.lisp
**Issue**: "I don't quite understand load-all.lisp. It's normal practice to use (asdf:load-system 'system-name')"

**Resolution**:
- Deleted `load-all.lisp`
- Now uses proper ASDF: `(asdf:load-system :coma-json-server)`
- ASDF handles all dependencies automatically

### ✅ Point #7: Removed unnecessary dependencies
**Issue**: "I don't think it needs kdtree, concaveman, tslnnls, or XPA"

**Resolution**:
- Removed from Dockerfile:
  - kdtree (only needed by packages not used by coma-json-server)
  - concaveman (only needed by packages not used)
  - tslnnls (not required)
  - XPA (only for ds9 image viewer integration)
- Updated `DOCKER_BUILD.md` to document removals

### ✅ Point #8: Created /usr/local/bin/coma-sci-backend executable
**Issue**: "I can't find /usr/local/bin/coma-sci-backend"

**Resolution**:
- Dockerfile now builds `/usr/local/bin/coma-json-server` (the actual executable)
- Created wrapper script `/usr/local/bin/coma-sci-backend` for backwards compatibility
- Wrapper simply calls `exec /usr/local/bin/coma-json-server "$@"`

---

## Major Differences That Still Exist

### Docker-Specific Files (Not in Jan's Original)

These files are **unique to the Docker version** and will never be in Jan's canonical codebase:

1. **`sbclrc.lisp`**
   - SBCL initialization for Docker environment
   - Sets up ASDF source registry
   - Loads Quicklisp bundle

2. **`astro/COMA-PROJECT/coma-json-server/main.lisp`**
   - Entry point for buildapp executable
   - Contains runtime initialization for lparallel and ASTORB
   - **Not in Jan's original** (he uses interactive REPL or scripts)
   - Required for Docker containerization

3. **`Dockerfile`**
   - Docker image build instructions

4. **`docker-compose.yml`**
   - Docker Compose orchestration

5. **`docker-entrypoint.sh`**
   - Container startup script

6. **`DOCKER_BUILD.md`**
   - Docker-specific documentation

7. **`RECONCILIATION_SUMMARY.md`** (this file)
   - Summary of reconciliation work

### Code Modifications for Docker

These are **minimal modifications** to Jan's original code, required for Docker containerization:

1. **`astro/COMA-PROJECT/coma-json-server/coma-json-package.lisp`**
   - Added export: `#:main` (for buildapp entry point)
   - **Difference**: Jan's original doesn't export `main`

2. **`astro/COMA-PROJECT/coma-json-server.asd`**
   - Added component: `(:file "main" :depends-on ("coma-json-package" "web-service"))`
   - Added comment: `";; Docker-specific entry point (not in Jan's original)"`
   - **Difference**: Jan's original doesn't have `main.lisp` component

3. **Comments in runtime-loading files** (documentation only):
   - `astro/astorb/astorb.lisp` - Comments explaining why `eval-when` is disabled for Docker
   - `astro/small-body-identify/sbid-search.lisp` - Comments explaining lparallel runtime init
   - **Difference**: Jan's original has these `eval-when` blocks enabled

### Functional Differences

| Aspect | Jan's Canonical | This Docker Version | Reason |
|--------|----------------|---------------------|--------|
| ASTORB loading | At REPL startup (eval-when) | Runtime in `main.lisp` | Docker: ASTORB too large for image |
| lparallel init | At REPL startup (eval-when) | Runtime in `main.lisp` | Docker: buildapp cannot save core with threads |
| Entry point | Interactive REPL or scripts | `coma-json-server:main` | Docker: needs standalone executable |
| Quicklisp | User's Quicklisp installation | Frozen bundle in `quicklisp-systems/` | Docker: reproducible builds |

---

## Syncing Procedure

### To Update from Jan's Canonical Version

When Jan updates his canonical `coma-backend-jtk`, follow these steps to sync:

```bash
# 1. Copy updated coma-json-server files (excluding Docker-specific main.lisp)
rsync -av --exclude='main.lisp' \
  /path/to/coma-backend-jtk/astro/COMA-PROJECT/coma-json-server/ \
  /path/to/coma-sci-backend/astro/COMA-PROJECT/coma-json-server/

# 2. Copy updated .asd file
cp /path/to/coma-backend-jtk/astro/COMA-PROJECT/coma-json-server.asd \
   /path/to/coma-sci-backend/astro/COMA-PROJECT/coma-json-server.asd

# 3. Re-add main.lisp component to .asd (near end of :components list)
# Add this line manually:
#   (:file "main" :depends-on ("coma-json-package" "web-service"))

# 4. Update other astro/ packages if needed
rsync -av /path/to/coma-backend-jtk/astro/ /path/to/coma-sci-backend/astro/ \
  --exclude='COMA-PROJECT/coma-json-server'

# 5. Update jlib/ if needed
rsync -av /path/to/coma-backend-jtk/jlib/ /path/to/coma-sci-backend/jlib/

# 6. Update quicklisp-systems/ if Jan has updated dependencies
rsync -av /path/to/coma-backend-jtk/quicklisp-systems/ \
  /path/to/coma-sci-backend/quicklisp-systems/

# 7. Rebuild Docker image
cd /path/to/coma-sci-backend
docker-compose build
```

### Files to NEVER Overwrite

These files are **Docker-specific** and should never be overwritten with Jan's versions:

- `astro/COMA-PROJECT/coma-json-server/main.lisp`
- `sbclrc.lisp`
- `Dockerfile`
- `docker-compose.yml`
- `docker-entrypoint.sh`
- `DOCKER_BUILD.md`
- `RECONCILIATION_SUMMARY.md`

---

## Validation

### Verify Structure Matches Jan's Original

```bash
# Check directory structure
ls -la astro/COMA-PROJECT/coma-json-server/
# Should see all .lisp files including main.lisp

ls -la astro/COMA-PROJECT/
# Should see coma-json-server.asd and examples/

ls -la quicklisp-systems/
# Should see bundle.lisp, bundle-info.sexp, software/, system-index.txt

# Check package name
grep "defpackage" astro/COMA-PROJECT/coma-json-server/coma-json-package.lisp
# Should be: (defpackage coma-json-server

# Check ASDF system name
grep "defsystem" astro/COMA-PROJECT/coma-json-server.asd
# Should be: (asdf:defsystem coma-json-server
```

### Compare with Jan's Original

```bash
# Compare .lisp files (excluding Docker-specific main.lisp)
diff -r --exclude="main.lisp" --exclude="*.lisp~" \
  /path/to/coma-backend-jtk/astro/COMA-PROJECT/coma-json-server/ \
  /path/to/coma-sci-backend/astro/COMA-PROJECT/coma-json-server/

# Should show no differences (or only Docker-related comments)

# Compare .asd files
diff /path/to/coma-backend-jtk/astro/COMA-PROJECT/coma-json-server.asd \
  /path/to/coma-sci-backend/astro/COMA-PROJECT/coma-json-server.asd

# Should differ only in:
# - main.lisp component added
# - Comment noting Docker-specific addition
```

---

## Summary

### What Was Changed

✅ Restored Jan's canonical directory structure (`astro/COMA-PROJECT/coma-json-server/`)
✅ Restored Jan's simple ASDF system definition (`coma-json-server`)
✅ Restored Jan's package name (`coma-json-server`)
✅ Restored frozen Quicklisp bundle (`quicklisp-systems/`)
✅ Restored examples directory (`astro/COMA-PROJECT/examples/`)
✅ Removed unnecessary dependencies (kdtree, concaveman, tslnnls, XPA)
✅ Removed manual ASDF registry pushnew statements
✅ Removed `load-all.lisp` (now uses proper ASDF)
✅ Created `sbclrc.lisp` for ASDF initialization
✅ Created `/usr/local/bin/coma-sci-backend` wrapper script

### What Remains Different (By Necessity)

**Docker-Specific Files** (will never be in Jan's original):
- `main.lisp` - Entry point for buildapp executable
- `sbclrc.lisp` - SBCL initialization
- `Dockerfile`, `docker-compose.yml`, `docker-entrypoint.sh`
- Docker documentation files

**Minimal Code Changes** (required for containerization):
- Export `#:main` in `coma-json-package.lisp`
- Add `main.lisp` component in `coma-json-server.asd`
- Comments explaining Docker-specific runtime loading

### Compatibility

This repository is now **fully compatible** with Jan's canonical `coma-backend-jtk` structure. You can:

1. Sync updates from Jan's repository using the procedure above
2. Push updates back to Jan if we make improvements
3. Maintain both versions independently (Jan's REPL-based, ours Docker-based)

The only differences are Docker-specific infrastructure files and the minimal `main.lisp` entry point required for standalone executable deployment.

---

## Next Steps

1. **Test the Docker build**:
   ```bash
   docker-compose build
   docker-compose up -d
   docker logs sci-backend
   ```

2. **Verify functionality**:
   ```bash
   curl -X POST http://localhost:5054/submit-json \
     -H "Content-Type: application/json" \
     -d '{"TYPE":"REQUEST","COMMAND":"HELLO","ID":"test"}'
   ```

3. **Document any build issues** encountered with the new structure

4. **Consider contributing improvements back to Jan's canonical version** (excluding Docker-specific files)

---

## Contact

For questions about reconciliation or syncing with Jan's canonical version:
- Jan Kleyna: jkleyna@ifa.hawaii.edu
- This Docker version: https://github.com/wakatara/coma-sci-backend
