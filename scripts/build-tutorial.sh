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
    --variable header-includes='<meta name="viewport" content="width=device-width, initial-scale=1.0"><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github-dark.min.css"><script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script><script>hljs.highlightAll();</script>' \
    --output "${OUTPUT_DIR}/tutorial.html"

# Add navigation header to English tutorial
sed -i.bak '/<body>/a\
<div class="nav-header">\
  <a href="index.html">← Back to CPL WASM</a> | \
  <a href="tutorial_ja.html">日本語版</a> | \
  <a href="https://github.com/msakai/cpl">GitHub</a>\
</div>' "${OUTPUT_DIR}/tutorial.html"
rm -f "${OUTPUT_DIR}/tutorial.html.bak"

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
    --variable header-includes='<meta name="viewport" content="width=device-width, initial-scale=1.0"><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github-dark.min.css"><script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script><script>hljs.highlightAll();</script>' \
    --output "${OUTPUT_DIR}/tutorial_ja.html"

# Add navigation header to Japanese tutorial
sed -i.bak '/<body>/a\
<div class="nav-header">\
  <a href="index.html">← CPL WASM に戻る</a> | \
  <a href="tutorial.html">English</a> | \
  <a href="https://github.com/msakai/cpl">GitHub</a>\
</div>' "${OUTPUT_DIR}/tutorial_ja.html"
rm -f "${OUTPUT_DIR}/tutorial_ja.html.bak"

echo "✓ Tutorial pages built successfully:"
echo "  - ${OUTPUT_DIR}/tutorial.html"
echo "  - ${OUTPUT_DIR}/tutorial_ja.html"
echo "  - ${OUTPUT_DIR}/doc-images/ (7 images)"
