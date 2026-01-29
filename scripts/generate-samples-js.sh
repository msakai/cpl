#!/bin/bash
set -e

# Generate wasm/samples.js from samples/ directory
# This script converts .cpl and .cdt files into a JavaScript module
# that can be imported by the WASM frontend.

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUTPUT="$PROJECT_ROOT/wasm/samples.js"

echo "// Auto-generated from samples/ directory. Do not edit manually." > "$OUTPUT"
echo "export const sampleFiles = {" >> "$OUTPUT"

for f in "$PROJECT_ROOT"/samples/*.cpl "$PROJECT_ROOT"/samples/*.cdt; do
  [ -f "$f" ] || continue
  name="samples/$(basename "$f")"
  # Use node for safe JSON escaping
  content=$(node -e "process.stdout.write(JSON.stringify(require('fs').readFileSync('$f', 'utf8')))")
  echo "  $(node -e "process.stdout.write(JSON.stringify('$name'))"):  $content," >> "$OUTPUT"
done

echo "};" >> "$OUTPUT"
echo "Generated $OUTPUT"
