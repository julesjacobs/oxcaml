#!/usr/bin/env bash
# Hash-freeze guard for the frozen core interfaces (DESIGN.md §10, §11; AGENTS.md).
#
# The frozen .mlis are the master's stable working-set view of the core data
# types. Changing one requires the unfreeze ritual: an updated FROZEN.sha256, an
# unfreeze ADR in decisions/, and an adversarial review by a fresh agent. This
# script is the mechanical enforcement — `make check-frozen` (wired into
# `make test`) goes red on any drift.
#
# Subcommands:
#   check     (default) recompute sha256 of the frozen files and diff against
#             FROZEN.sha256; exit nonzero + loud message on mismatch.
#   generate  (re)write FROZEN.sha256 — only as step 3 of the unfreeze ritual.
#   spine     write SPINE.md: the concatenated frozen .mlis (the master's view).
#
# The frozen-file list below is the single source of truth for both the freeze
# check and the spine, so the two can never cover different sets.
set -euo pipefail

cd "$(dirname "$0")/.."   # repo root (tools/ is one level down)

MANIFEST=FROZEN.sha256
SPINE=SPINE.md

# Frozen at the M0-core merge (ADR-0003): sort/symbol/term/context/iarr.
# Frozen at the M1 THEORY freeze (ADR-0005 Tranche A): env/rank/theory_view (the
# scheduled three) + the new THEORY vocabulary atom/lit/explanation/theory.
# Frozen at the M2 EUF-adapter merge (ADR-0005 Tranche B): model.mli — its [value]
# variant's Uninterp witness (open q3) is now pinned by the EUF adapter's first real
# model (Uninterp = congruence-class representative id). sat.mli (Tranche C, M4)
# freezes later — see decisions/adr-0005-freeze-plan.md.
FROZEN_FILES=(
  smt/core/sort.mli
  smt/core/symbol.mli
  smt/core/term.mli
  smt/core/context.mli
  smt/core/iarr.mli
  smt/core/env.mli
  smt/core/rank.mli
  smt/core/theory_view.mli
  smt/core/atom.mli
  smt/core/lit.mli
  smt/core/explanation.mli
  smt/core/theory.mli
  smt/core/model.mli
)

case "${1:-check}" in
  generate)
    sha256sum "${FROZEN_FILES[@]}" > "$MANIFEST"
    echo "wrote $MANIFEST (${#FROZEN_FILES[@]} frozen interfaces)"
    ;;

  check)
    if [ ! -f "$MANIFEST" ]; then
      echo "frozen interface check: manifest $MANIFEST missing" >&2
      exit 1
    fi
    if sha256sum -c --status "$MANIFEST"; then
      echo "frozen: ${#FROZEN_FILES[@]} interface(s) match $MANIFEST"
    else
      {
        echo ""
        echo "frozen interface changed — unfreeze ritual required (see AGENTS.md / DESIGN §10)."
        echo "A frozen .mli no longer matches $MANIFEST. If the change is intended:"
        echo "  1. write an unfreeze ADR in decisions/,"
        echo "  2. get an adversarial review by a fresh agent (grill-me brief),"
        echo "  3. regenerate the manifest: tools/check_frozen.sh generate"
        echo "offending file(s):"
        sha256sum -c "$MANIFEST" 2>&1 | grep -v ': OK$' || true
      } >&2
      exit 1
    fi
    ;;

  spine)
    {
      echo "# SPINE.md — frozen core interfaces (GENERATED — regenerate with \`make spine\`)"
      echo ""
      echo "The master's working-set view of the frozen core data types (DESIGN.md §11)."
      echo "Do NOT edit by hand. Frozen per ADR-0003; hash-checked via $MANIFEST"
      echo "(\`make check-frozen\`). Changing any of these requires the unfreeze ritual."
      echo ""
      for f in "${FROZEN_FILES[@]}"; do
        echo "=== $f ==="
        echo ""
        cat "$f"
        echo ""
      done
    } > "$SPINE"
    echo "wrote $SPINE (${#FROZEN_FILES[@]} frozen interfaces)"
    ;;

  *)
    echo "usage: $0 [check|generate|spine]" >&2
    exit 2
    ;;
esac
