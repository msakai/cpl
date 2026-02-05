#!/bin/bash
set -euo pipefail

# Script to generate favicon files from source logo images
# Usage: ./scripts/generate-favicons.sh
#
# Requires: ImageMagick (magick command)
#
# Source images:
#   - logo-with-diagram.png: Used for larger icons (180x180+)
#   - logo-without-diagram.png: Used for smaller icons (32x32 and below)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OUTPUT_DIR="${PROJECT_ROOT}/web"

echo "=== Generating Favicon Files ==="

# Check if ImageMagick is installed
if ! command -v magick &> /dev/null; then
    echo "Error: ImageMagick is not installed"
    echo "Install with: brew install imagemagick  (macOS)"
    echo "          or: sudo apt-get install imagemagick  (Ubuntu/Debian)"
    exit 1
fi

echo "ImageMagick found: $(magick --version | head -n1)"

# Check source images exist
LOGO_WITH_DIAGRAM="${PROJECT_ROOT}/logo-with-diagram.png"
LOGO_WITHOUT_DIAGRAM="${PROJECT_ROOT}/logo-without-diagram.png"

if [ ! -f "$LOGO_WITH_DIAGRAM" ]; then
    echo "Error: $LOGO_WITH_DIAGRAM not found"
    exit 1
fi

if [ ! -f "$LOGO_WITHOUT_DIAGRAM" ]; then
    echo "Error: $LOGO_WITHOUT_DIAGRAM not found"
    exit 1
fi

echo "Source images found:"
echo "  - $LOGO_WITH_DIAGRAM"
echo "  - $LOGO_WITHOUT_DIAGRAM"
echo ""

# Create output directory if needed
mkdir -p "$OUTPUT_DIR"

# Generate small icons from logo-without-diagram.png
# (The diagram is not visible at small sizes)
echo "Generating small icons from logo-without-diagram.png..."
magick "$LOGO_WITHOUT_DIAGRAM" -resize 16x16 "$OUTPUT_DIR/favicon-16x16.png"
echo "  Created favicon-16x16.png"

magick "$LOGO_WITHOUT_DIAGRAM" -resize 32x32 "$OUTPUT_DIR/favicon-32x32.png"
echo "  Created favicon-32x32.png"

# Generate favicon.ico (multi-resolution)
TMPDIR=$(mktemp -d)
magick "$LOGO_WITHOUT_DIAGRAM" -resize 16x16 "$TMPDIR/16.png"
magick "$LOGO_WITHOUT_DIAGRAM" -resize 32x32 "$TMPDIR/32.png"
magick "$TMPDIR/16.png" "$TMPDIR/32.png" "$OUTPUT_DIR/favicon.ico"
rm -rf "$TMPDIR"
echo "  Created favicon.ico (16x16 + 32x32)"

# Generate large icons from logo-with-diagram.png
# (The diagram is visible at larger sizes)
echo ""
echo "Generating large icons from logo-with-diagram.png..."

magick "$LOGO_WITH_DIAGRAM" -resize 180x180 "$OUTPUT_DIR/apple-touch-icon.png"
echo "  Created apple-touch-icon.png (180x180)"

magick "$LOGO_WITH_DIAGRAM" -resize 192x192 "$OUTPUT_DIR/icon-192x192.png"
echo "  Created icon-192x192.png (192x192)"

magick "$LOGO_WITH_DIAGRAM" -resize 512x512 "$OUTPUT_DIR/icon-512x512.png"
echo "  Created icon-512x512.png (512x512)"

echo ""
echo "=== Favicon generation complete ==="
echo ""
echo "Generated files in $OUTPUT_DIR/:"
ls -la "$OUTPUT_DIR"/favicon* "$OUTPUT_DIR"/apple-touch-icon.png "$OUTPUT_DIR"/icon-*.png 2>/dev/null | awk '{print "  " $9 " (" $5 " bytes)"}'
