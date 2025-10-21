#!/bin/bash
set -e

echo "====================================="
echo "COMA Sci-Backend Starting..."
echo "====================================="

# Copy ASTORB database from build location to runtime location if needed
echo "Checking ASTORB database..."
mkdir -p /data/support/sci-backend/astorb

if [ -d "/opt/astorb" ] && [ "$(ls -A /opt/astorb 2>/dev/null)" ]; then
    # Copy any .gz files from /opt/astorb/ that don't exist in target
    for file in /opt/astorb/*.gz; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            if [ ! -f "/data/support/sci-backend/astorb/$filename" ]; then
                echo "  Copying ASTORB database from image: $filename"
                cp "$file" "/data/support/sci-backend/astorb/$filename"
                echo "  ASTORB database ready ($(du -h /data/support/sci-backend/astorb/$filename | cut -f1))"
            else
                echo "  ASTORB database already exists in volume: $filename"
            fi
        fi
    done
else
    echo "  Warning: No ASTORB database found at /opt/astorb/"
    echo "  The application will attempt to download it at runtime"
fi

echo "====================================="
echo "Starting COMA Sci-Backend executable"
echo "====================================="

# Launch the executable with increased heap size for ASTORB FASL compilation
# The author normally uses 8GB for ASTORB compilation
# The --dynamic-space-size flag must be passed at runtime to the SBCL-based executable
if [[ "$1" == "/usr/local/bin/coma-sci-backend" ]] || [[ "$1" == "/usr/local/bin/coma-json-server" ]]; then
    echo "Launching with --dynamic-space-size 8192 (8GB heap)"
    exec "$1" --dynamic-space-size 8192 "${@:2}"
else
    # Execute other commands as-is
    exec "$@"
fi
