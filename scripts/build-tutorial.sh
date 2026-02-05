#!/bin/bash
set -euo pipefail

# Script to build tutorial HTML pages from Markdown using Pandoc
# Usage: ./scripts/build-tutorial.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OUTPUT_DIR="${PROJECT_ROOT}/_site"

echo "=== Building CPL Tutorial Pages ==="

# Check if pandoc is installed
if ! command -v pandoc &> /dev/null; then
    echo "Error: pandoc is not installed"
    echo "Install with: sudo apt-get install pandoc  (Ubuntu/Debian)"
    echo "          or: brew install pandoc           (macOS)"
    exit 1
fi

# Create output directory if it doesn't exist
mkdir -p "${OUTPUT_DIR}"
mkdir -p "${OUTPUT_DIR}/doc-images"

# Copy images
echo "Copying images..."
cp -r "${PROJECT_ROOT}/doc-images/"*.png "${OUTPUT_DIR}/doc-images/"

# Build English tutorial
echo "Building English tutorial (tutorial.html)..."
pandoc "${PROJECT_ROOT}/TUTORIAL.md" \
    --from markdown \
    --to html5 \
    --standalone \
    --toc \
    --toc-depth=3 \
    --css=tutorial.css \
    --metadata title="CPL Tutorial" \
    --metadata lang="en" \
    --include-before-body="${PROJECT_ROOT}/web/tutorial_header.txt" \
    --output "${OUTPUT_DIR}/tutorial.html"

# Build Japanese tutorial
echo "Building Japanese tutorial (tutorial_ja.html)..."
pandoc "${PROJECT_ROOT}/TUTORIAL_ja.md" \
    --from markdown \
    --to html5 \
    --standalone \
    --toc \
    --toc-depth=3 \
    --css=tutorial.css \
    --metadata title="CPL チュートリアル" \
    --metadata lang="ja" \
    --include-before-body="${PROJECT_ROOT}/web/tutorial_ja_header.txt" \
    --output "${OUTPUT_DIR}/tutorial_ja.html"

IMAGE_COUNT=$(ls -1 "${OUTPUT_DIR}/doc-images/"*.png 2>/dev/null | wc -l | sed 's/^[[:space:]]*//; s/[[:space:]]*$//')

echo "✓ Tutorial pages built successfully:"
echo "  - ${OUTPUT_DIR}/tutorial.html"
echo "  - ${OUTPUT_DIR}/tutorial_ja.html"
echo "  - ${OUTPUT_DIR}/doc-images/ ($IMAGE_COUNT images)"
