#!/bin/bash
# C1 link-time frame-table bake tool (task #63, see runtime/frame_descriptors.c).
#
# Given a linked (unpatched) native executable whose runtime reserves the
# .caml_frametable_baked PROGBITS section, produce a copy in which that section
# holds the fully-built frame-descriptor table. At startup the baked binary then
# points [current_frame_descrs] straight at the in-image table and skips the
# ~7.5 ms per-process rebuild. A freshly-linked (unbaked) binary carries the
# reserve as zeros, so it safely falls back to rebuilding.
#
# Mechanism: run the binary once with CAML_FRAME_BAKE set -- its own runtime
# builds the table (via the unchanged add_frame_descriptors) and dumps the exact
# section image, then _exit(0)s before main. objcopy --update-section patches
# that image in place: the dump is exactly the reserve size, so the update is a
# pure byte-overwrite with no ELF relayout and the .note.gnu.build-id preserved.
#
# usage: bake_frame_table.sh <in-binary> <out-binary>
set -u
BIN="${1:?usage: bake_frame_table.sh <in-binary> <out-binary>}"
OUT="${2:?usage: bake_frame_table.sh <in-binary> <out-binary>}"
SEC=.caml_frametable_baked
# Resolve BIN so a bare name (no ./ , not on PATH) still runs.
case "$BIN" in */*) RUN=$BIN ;; *) RUN=./$BIN ;; esac
TBL=$(mktemp "${TMPDIR:-/tmp}/frametbl.XXXXXX.bin")
trap 'rm -f "$TBL"' EXIT

echo "=== 1. bake dump (binary's own runtime builds the table, then dumps it) ==="
CAML_FRAME_BAKE="$TBL" "$RUN" -version >/dev/null 2>&1
echo "dump exit=$? size=$(stat -c%s "$TBL" 2>/dev/null)"

echo "=== 2. reserve section size vs dump size (must be equal for in-place update) ==="
SECSZ=$(readelf -SW "$BIN" \
  | awk -v s="$SEC" 'index($0,s){for(i=1;i<=NF;i++) if($i==s){print strtonum("0x"$(i+4)); exit}}')
echo "section=$SECSZ dump=$(stat -c%s "$TBL")"
if [ "$SECSZ" != "$(stat -c%s "$TBL")" ]; then
  echo "SIZE MISMATCH -- objcopy would refuse; aborting" >&2; exit 1
fi

echo "=== 3. objcopy --update-section (same-size, in-place, build-id preserved) ==="
objcopy --update-section ${SEC}="$TBL" "$BIN" "$OUT"
echo "objcopy exit=$?"
chmod u+x "$OUT"

echo "=== 4. build-id before vs after (same-size update preserves the note) ==="
echo -n "in : "; readelf -n "$BIN" | awk '/Build ID/{print $3}'
echo -n "out: "; readelf -n "$OUT" | awk '/Build ID/{print $3}'
echo "BAKE_DONE"
