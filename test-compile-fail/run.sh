#!/usr/bin/env bash
set -euo pipefail

# Compile-fail tests for client derivation type safety.
# Each .purs file in cases/ must FAIL to compile.

SOURCES=$(bunx spago sources 2>/dev/null | grep -v 'test/')
OUTPUT_DIR="output-compile-fail"
CASES_DIR="test-compile-fail/cases"
PASS=0
FAIL=0

for f in "$CASES_DIR"/*.purs; do
  name=$(basename "$f" .purs)
  if bunx purs compile --output "$OUTPUT_DIR" $SOURCES "$f" >/dev/null 2>&1; then
    echo "FAIL: $name compiled but should not have"
    FAIL=$((FAIL + 1))
  else
    echo "  ok: $name correctly rejected"
    PASS=$((PASS + 1))
  fi
done

echo ""
echo "$PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
