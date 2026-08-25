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
# Failure policy: a binary WITHOUT the reserve section did not opt in, so it
# gets an UNBAKED passthrough copy with a warning and exit 0 (a build that
# wires this step in never fails on such a binary). A binary WITH the reserve
# that cannot be baked -- typically the frame table outgrew
# FRAME_BAKED_CAP_MAX, or objcopy broke -- FAILS LOUDLY (exit 1): silently
# installing a passthrough there would revert the startup win with no signal
# until someone re-measures. Baking is idempotent: in CAML_FRAME_BAKE mode
# the runtime always rebuilds the table fresh before dumping, so re-baking an
# already-baked binary reproduces the same image.
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

# Passthrough: only for binaries that lack the reserve section (did not opt
# in). cp -p preserves the build-id and every other byte.
fallback() {
  echo "[frame] bake: $1 -- installing UNBAKED passthrough copy" >&2
  cp -p "$BIN" "$OUT" && chmod u+x "$OUT"
  echo "BAKE_FALLBACK"
  exit 0
}

# Loud failure: the binary carries the reserve, so an un-bakeable table is a
# real regression (most likely num_descr outgrew FRAME_BAKED_CAP_MAX in
# runtime/frame_descriptors.c -- bump it and caml_frame_bake_reserve.c
# together). Do NOT silently install a passthrough here.
bakefail() {
  echo "[frame] bake: FAILED: $1" >&2
  echo "[frame] bake: reserve present but table not baked; refusing to" \
       "install a silent passthrough (see FRAME_BAKED_CAP_MAX)" >&2
  echo "BAKE_FAILED"
  exit 1
}

echo "=== 1. reserve section present? (absent = did not opt in) ==="
SECHEX=$(readelf -SW "$BIN" \
  | awk -v s="$SEC" 'index($0,s){for(i=1;i<=NF;i++) if($i==s){print $(i+4); exit}}')
# readelf prints the size in hex without 0x; convert in bash (strtonum is gawk-only
# and its absence would silently install the unbaked fallback on every build).
SECSZ=""
[ -n "$SECHEX" ] && SECSZ=$((16#$SECHEX))
echo "section=${SECSZ:-<absent>}"
[ -n "$SECSZ" ] || fallback "reserve section $SEC absent"

echo "=== 2. bake dump (binary's own runtime builds the table, then dumps it) ==="
CAML_FRAME_BAKE="$TBL" "$RUN" -version >/dev/null 2>&1
DUMPSZ=$(stat -c%s "$TBL" 2>/dev/null || echo 0)
echo "dump exit=$? size=$DUMPSZ"
[ "${DUMPSZ:-0}" -gt 0 ] 2>/dev/null \
  || bakefail "bake dump empty (table exceeds reserve, or dump failed)"
[ "$SECSZ" = "$DUMPSZ" ] || bakefail "reserve size $SECSZ != dump size $DUMPSZ"

echo "=== 3. objcopy --update-section (same-size, in-place, build-id preserved) ==="
if ! objcopy --update-section ${SEC}="$TBL" "$BIN" "$OUT"; then
  bakefail "objcopy --update-section failed"
fi
echo "objcopy exit=0"
chmod u+x "$OUT"

echo "=== 4. build-id before vs after (same-size update preserves the note) ==="
echo -n "in : "; readelf -n "$BIN" | awk '/Build ID/{print $3}'
echo -n "out: "; readelf -n "$OUT" | awk '/Build ID/{print $3}'
echo "BAKE_DONE"
