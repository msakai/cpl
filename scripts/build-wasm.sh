#!/bin/bash
set -e

# CPL WebAssembly Build Script
# Builds CPL interpreter for WebAssembly using GHC's WASM backend
# Output goes to _site/ directory for local testing and deployment

echo "================================================"
echo "Building CPL for WebAssembly"
echo "================================================"
echo ""

# Check for required tools
echo "Checking toolchain..."

if ! command -v wasm32-wasi-ghc &> /dev/null; then
    echo "Error: wasm32-wasi-ghc not found"
    echo ""
    echo "Please install the GHC WebAssembly cross-compiler:"
    echo "  https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html"
    echo ""
    echo "For macOS/Linux, you can download pre-built binaries:"
    echo "  https://downloads.haskell.org/~ghc/"
    exit 1
fi

if ! command -v wasm32-wasi-cabal &> /dev/null; then
    echo "Error: wasm32-wasi-cabal not found"
    echo ""
    echo "Please ensure the GHC WASM toolchain is properly installed."
    exit 1
fi

echo "✓ wasm32-wasi-ghc found: $(wasm32-wasi-ghc --version | head -n1)"
echo "✓ wasm32-wasi-cabal found: $(wasm32-wasi-cabal --version | head -n1)"
echo ""

# Get project root (script is in scripts/)
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "Project root: $PROJECT_ROOT"
echo ""

# Output directory
OUTPUT_DIR="$PROJECT_ROOT/_site"

# Clean previous build artifacts (optional)
if [ "$1" = "--clean" ]; then
    echo "Cleaning previous build artifacts..."
    rm -rf dist-newstyle
    echo "✓ Clean complete"
    echo ""
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"

echo "Generating sample files module..."
"$PROJECT_ROOT/scripts/generate-samples-js.sh"
echo ""

echo ""
echo "Configuring CPL for WebAssembly..."
wasm32-wasi-cabal configure -fWeb -f-Haskeline

echo ""
echo "Building CPL..."
wasm32-wasi-cabal build

echo ""
echo "Locating WASM binary..."

# Find the compiled WASM binary
WASM_BIN=$(wasm32-wasi-cabal list-bin cpl)

if [ -z "$WASM_BIN" ]; then
    echo "Error: Could not find compiled WASM binary"
    echo "Expected to find 'cpl.wasm' in dist-newstyle/build/wasm32-wasi/..."
    exit 1
fi

echo "✓ Found WASM binary: $WASM_BIN"

# Copy WASM binary to output directory
echo ""
echo "Copying WASM binary to $OUTPUT_DIR/cpl.wasm..."
cp "$WASM_BIN" "$OUTPUT_DIR/cpl.wasm"
echo "✓ Copied WASM binary"

# Post-link processing (if available)
echo ""
echo "Checking for post-link processor..."
LIBDIR=$(wasm32-wasi-ghc --print-libdir)

if [ -f "$LIBDIR/post-link.mjs" ]; then
    echo "Running post-link processor..."
    node "$LIBDIR/post-link.mjs" -i "$OUTPUT_DIR/cpl.wasm" -o "$OUTPUT_DIR/cpl.js"
    echo "✓ Post-link processing complete"
else
    echo "ℹ No post-link processor found (this is usually fine)"
fi

# Copy source files from web/ to output directory
echo ""
echo "Copying web files to $OUTPUT_DIR/..."
cp "$PROJECT_ROOT/web/index.html" "$OUTPUT_DIR/"
cp "$PROJECT_ROOT/web/cpl-terminal.js" "$OUTPUT_DIR/"
cp "$PROJECT_ROOT/web/favicon.ico" "$OUTPUT_DIR/"
cp "$PROJECT_ROOT/web/favicon-16x16.png" "$OUTPUT_DIR/"
cp "$PROJECT_ROOT/web/favicon-32x32.png" "$OUTPUT_DIR/"
cp "$PROJECT_ROOT/web/apple-touch-icon.png" "$OUTPUT_DIR/"
cp "$PROJECT_ROOT/web/icon-192x192.png" "$OUTPUT_DIR/"
cp "$PROJECT_ROOT/web/icon-512x512.png" "$OUTPUT_DIR/"
cp "$PROJECT_ROOT/web/manifest.json" "$OUTPUT_DIR/"
echo "✓ Copied web files"

# Get file size
WASM_SIZE=$(du -h "$OUTPUT_DIR/cpl.wasm" | cut -f1)

echo ""
echo "================================================"
echo "Build complete!"
echo "================================================"
echo ""
echo "Output directory: $OUTPUT_DIR"
echo "WASM binary size: $WASM_SIZE"
echo ""
echo "To test locally, run:"
echo "  python3 -m http.server -d _site 8000"
echo "Then open: http://localhost:8000"
echo ""
echo "For full site with tutorials, also run:"
echo "  ./scripts/build-tutorial.sh"
echo ""

# Verify required files exist
echo "Checking output files..."
MISSING_FILES=0

for f in index.html cpl-terminal.js cpl.wasm cpl.js samples.js favicon.ico favicon-16x16.png favicon-32x32.png apple-touch-icon.png icon-192x192.png icon-512x512.png manifest.json; do
    if [ -f "$OUTPUT_DIR/$f" ]; then
        echo "  ✓ $f"
    else
        echo "  ✗ $f (missing)"
        MISSING_FILES=1
    fi
done

if [ $MISSING_FILES -eq 0 ]; then
    echo ""
    echo "✓ All required files present"
else
    echo ""
    echo "⚠ Some files are missing. The web interface may not work correctly."
fi

echo ""
