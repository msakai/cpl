#!/bin/bash
set -e

# CPL WebAssembly Build Script
# Builds CPL interpreter for WebAssembly using GHC's WASM backend

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

# Clean previous build artifacts (optional)
if [ "$1" = "--clean" ]; then
    echo "Cleaning previous build artifacts..."
    rm -rf dist-newstyle
    echo "✓ Clean complete"
    echo ""
fi

# Update package index
echo "Updating package index..."
wasm32-wasi-cabal update

echo "Generating sample files module..."
"$PROJECT_ROOT/scripts/generate-samples-js.sh"
echo ""

echo ""
echo "Configuring CPL for WebAssembly..."
wasm32-wasi-cabal configure -fWASM -f-Readline -f-Haskeline

echo ""
echo "Building CPL..."
wasm32-wasi-cabal build

echo ""
echo "Locating WASM binary..."

# Find the compiled WASM binary
WASM_BIN=$(find dist-newstyle -name "cpl.wasm" -type f | grep wasm32-wasi | head -n1)

if [ -z "$WASM_BIN" ]; then
    echo "Error: Could not find compiled WASM binary"
    echo "Expected to find 'cpl.wasm' in dist-newstyle/build/wasm32-wasi/..."
    exit 1
fi

echo "✓ Found WASM binary: $WASM_BIN"

# Create wasm directory if it doesn't exist
mkdir -p wasm

# Copy WASM binary
echo ""
echo "Copying WASM binary to wasm/cpl.wasm..."
cp "$WASM_BIN" wasm/cpl.wasm

echo "✓ Copied to wasm/cpl.wasm"

# Post-link processing (if available)
echo ""
echo "Checking for post-link processor..."
LIBDIR=$(wasm32-wasi-ghc --print-libdir)

if [ -f "$LIBDIR/post-link.mjs" ]; then
    echo "Running post-link processor..."
    node "$LIBDIR/post-link.mjs" -i wasm/cpl.wasm -o wasm/cpl.js
    echo "✓ Post-link processing complete"
else
    echo "ℹ No post-link processor found (this is usually fine)"
fi

# Get file size
WASM_SIZE=$(du -h wasm/cpl.wasm | cut -f1)

echo ""
echo "================================================"
echo "Build complete!"
echo "================================================"
echo ""
echo "WASM binary: wasm/cpl.wasm ($WASM_SIZE)"
echo ""
echo "To test locally, run:"
echo "  cd wasm && python3 -m http.server 8000"
echo "Then open: http://localhost:8000"
echo ""
echo "Required files in wasm/:"
echo "  - cpl.wasm (compiled binary)"
echo "  - index.html (web interface)"
echo "  - cpl-terminal.js (terminal controller)"
echo ""

# Verify required files exist
MISSING_FILES=0

if [ ! -f "wasm/index.html" ]; then
    echo "⚠ Warning: wasm/index.html not found"
    MISSING_FILES=1
fi

if [ ! -f "wasm/cpl-terminal.js" ]; then
    echo "⚠ Warning: wasm/cpl-terminal.js not found"
    MISSING_FILES=1
fi

if [ $MISSING_FILES -eq 0 ]; then
    echo "✓ All required files present"
else
    echo ""
    echo "⚠ Some required files are missing. The web interface may not work."
fi

echo ""
