#!/usr/bin/env bash
# Mutation-testing runner (DESIGN.md §10: "routinely inject seeded faults and
# require the tiered suite catches them; a surviving mutant halts feature work").
#
# For each mutant in tools/mutants/registry/*.patch (+ .meta sidecar): create a
# throwaway git worktree off HEAD, apply the patch, run the mutant's declared
# suite, and REQUIRE it goes red. A KILLED mutant is the system working; a
# SURVIVOR is a real oracle gap (a loud, exit-1 failure — do NOT weaken the
# mutant, fix the oracle). PATCH-FAILED means the patch no longer applies (the
# target drifted) — refresh it.
#
# Usage:  run.sh [MODULE]        # MODULE filters by the meta's module= field
# Scratch worktrees live under ../worktrees/scratch-mutant-* and are ALWAYS
# removed on exit (trap). Never mutates main/ or the task worktree.

set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
REGISTRY="$REPO/tools/mutants/registry"
WORKTREES="$(cd "$REPO/.." && pwd)"
LOGS="${OXSMT_MUTANT_LOGS:-$(cd "$REPO/.." && pwd)/logs}"
FILTER="${1:-}"

mkdir -p "$LOGS"
STAMP="$(date +%Y%m%d-%H%M%S)"
LOG="$LOGS/mutants-$STAMP.log"

# Frozen .mlis must never be a mutation target (they are hash-frozen; a patch to
# one is a registry error, not a fault). Source of truth: FROZEN.sha256.
FROZEN="$(awk '{print $2}' "$REPO/FROZEN.sha256" 2>/dev/null || true)"

SCRATCHES=()
cleanup() {
  for s in "${SCRATCHES[@]:-}"; do
    [ -n "$s" ] || continue
    git -C "$REPO" worktree remove --force "$s" >/dev/null 2>&1 || rm -rf "$s"
  done
  git -C "$REPO" worktree prune >/dev/null 2>&1 || true
}
trap cleanup EXIT

echo "oxsmt mutation run $STAMP" | tee "$LOG"
echo "repo=$REPO  filter=${FILTER:-<all>}  log=$LOG" | tee -a "$LOG"
echo "" | tee -a "$LOG"

meta_get() { sed -n "s/^$2=//p" "$1" | head -1; }

declare -a ROWS=()
n_kbuild=0 n_ktest=0 n_survived=0 n_patchfail=0 n_lint=0

for patch in "$REGISTRY"/*.patch; do
  [ -e "$patch" ] || continue
  name="$(basename "$patch" .patch)"
  meta="$REGISTRY/$name.meta"
  if [ ! -f "$meta" ]; then
    ROWS+=("$name|?|(no .meta)|PATCH-FAILED")
    n_patchfail=$((n_patchfail + 1))
    continue
  fi
  module="$(meta_get "$meta" module)"
  suite="$(meta_get "$meta" suite)"
  [ -n "$FILTER" ] && [ "$FILTER" != "$module" ] && continue

  {
    echo "=== $name  (module=$module suite=$suite) ==="
    echo "--- $(meta_get "$meta" description)"
  } >>"$LOG"

  # Registry lint: refuse a patch that touches a frozen interface.
  lint_bad=""
  for f in $FROZEN; do
    if grep -qE "^\+\+\+ b/$f$|^--- a/$f$" "$patch"; then lint_bad="$f"; fi
  done
  if [ -n "$lint_bad" ]; then
    echo "LINT-REJECT: patch touches frozen interface $lint_bad" >>"$LOG"
    ROWS+=("$name|$suite|frozen:$lint_bad|LINT-REJECT")
    n_lint=$((n_lint + 1))
    continue
  fi

  scratch="$WORKTREES/scratch-mutant-$name"
  git -C "$REPO" worktree remove --force "$scratch" >/dev/null 2>&1 || true
  if ! git -C "$REPO" worktree add --detach "$scratch" HEAD >>"$LOG" 2>&1; then
    echo "PATCH-FAILED: could not create scratch worktree" >>"$LOG"
    ROWS+=("$name|$suite|worktree add failed|PATCH-FAILED")
    n_patchfail=$((n_patchfail + 1))
    continue
  fi
  SCRATCHES+=("$scratch")

  if ! git -C "$scratch" apply "$patch" >>"$LOG" 2>&1; then
    echo "PATCH-FAILED: git apply rejected (target drifted — refresh the patch)" >>"$LOG"
    ROWS+=("$name|$suite|git apply rejected — refresh patch|PATCH-FAILED")
    n_patchfail=$((n_patchfail + 1))
    git -C "$REPO" worktree remove --force "$scratch" >/dev/null 2>&1 || true
    continue
  fi

  # Build FIRST, then run the suite — so a mutant caught by the compiler
  # (KILLED-BUILD) is distinguished from one caught by a suite oracle on a clean
  # build (KILLED-TEST). Both are kills, but only KILLED-TEST exercises a test
  # oracle; a KILLED-BUILD certifies the type-checker, not the registry's oracle,
  # so the table surfaces the difference for registry-quality auditing. `make
  # build` is `dune build @@default`, which compiles the mutated library AND every
  # suite executable, so the subsequent `make <suite>` build step is a cache hit
  # and any nonzero there is a genuine test failure.
  echo "--- building: make build (dune @@default)" >>"$LOG"
  ( cd "$scratch" && make build ) >>"$LOG" 2>&1
  build_rc=$?
  if [ "$build_rc" -ne 0 ]
  then
    echo "--- build failed (exit $build_rc): KILLED-BUILD" >>"$LOG"
    ROWS+=("$name|$suite|mutant does not compile (exit $build_rc)|KILLED-BUILD")
    n_kbuild=$((n_kbuild + 1))
  else
    # Hermetic: private cache/logs so the run never touches the shared ../cache.
    echo "--- running: make $suite" >>"$LOG"
    ( cd "$scratch" \
        && OXSMT_CACHE="$scratch/.mutant-cache" OXSMT_LOGS="$scratch/.mutant-logs" \
           make "$suite" ) >>"$LOG" 2>&1
    rc=$?
    echo "--- make $suite exit=$rc" >>"$LOG"
    if [ "$rc" -ne 0 ]
    then
      ROWS+=("$name|$suite|suite red on clean build (exit $rc)|KILLED-TEST")
      n_ktest=$((n_ktest + 1))
    else
      ROWS+=("$name|$suite|suite stayed GREEN — oracle gap|SURVIVED")
      n_survived=$((n_survived + 1))
    fi
  fi

  git -C "$REPO" worktree remove --force "$scratch" >/dev/null 2>&1 || true
done

# ---- digest ----
{
  echo ""
  printf '%-32s %-16s %-13s %s\n' MUTANT SUITE RESULT DETAIL
  printf '%-32s %-16s %-13s %s\n' "------" "-----" "------" "------"
  for row in "${ROWS[@]:-}"; do
    [ -n "$row" ] || continue
    IFS='|' read -r m s d r <<<"$row"
    printf '%-32s %-16s %-13s %s\n' "$m" "$s" "$r" "$d"
  done
  echo ""
  echo "totals: $n_ktest KILLED-TEST, $n_kbuild KILLED-BUILD, $n_survived SURVIVED, \
$n_patchfail PATCH-FAILED, $n_lint LINT-REJECT"
} | tee -a "$LOG"

if [ "$n_survived" -gt 0 ]; then
  echo "MUTANTS SURVIVED — oracle gap; halt feature work on the affected module(s) (DESIGN §10). full log: $LOG"
  exit 1
elif [ "$((n_patchfail + n_lint))" -gt 0 ]; then
  echo "registry needs attention (PATCH-FAILED/LINT-REJECT). full log: $LOG"
  exit 2
else
  if [ "$n_kbuild" -gt 0 ]; then
    echo "note: $n_kbuild mutant(s) killed at BUILD, not TEST — these certify the compiler, \
not a test oracle; prefer a compiling variant so an oracle is exercised."
  fi
  echo "all mutants killed ($n_ktest KILLED-TEST, $n_kbuild KILLED-BUILD). full log: $LOG"
  exit 0
fi
