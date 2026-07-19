# Task #18 — LRA flip riders (R1 expected-Real `-`; R2 tri-state garbage + DESIGN-LRA)

Branch `task/lra-riders`, off trunk `f75366318f`. Riders from the LRA default-ON flip
review (`logs/lra-flip-review.md`, APPROVE-WITH-HEDGE).

## R1 (MEDIUM) — expected-Real propagation through the unified `-`

**Bug.** The unified subtraction arm (`smt/smtlib/parser/parser.ml`, `read_op`'s `"-"`
case) read its operands with no expected sort and only took the Real path when an operand
was *already* Real. In a Real context (e.g. this subtraction as an operand of a Real `=` or
comparison, which `same_sort_terms` re-reads with `expected = Real`), an all-Int subtraction
whose operands are Int-widenable (an Int `ite`/const) stayed Int and then mixed with the
Real context, degrading to `unknown`. Reproducer `(= (- (ite p 1 2) 3) 0.0)` → `unknown` vs
z3 `unsat`. SOUND completeness loss; the 156–160 headline already reflected it.

**Fix.** Honor an enclosing Real `expected` in the `-` arm: when `expected = Some Real`
(and LRA enabled), enter the Real path so the existing `coerce_to_sort st Sort.real` calls
widen each operand. Only fires under `expected = Real`; the all-Int (non-Real) operand reads
and their intern order — hence byte-identity — are untouched (pure insertion + one `if`
condition line; `git diff -w` == `git diff`).

RED→GREEN (built at pin, `QF_LRA` logic):
- `(= (- (ite p 1 2) 3) 0.0)` — trunk `unknown` → fix **`unsat`** (RED confirmed by
  stash+rebuild on trunk).
- sat variant `(= (- (ite p 1 3) 3) 0.0)` → `sat`.
- multi-arg `(= (- (ite p 5 6) 1 2) 3.0)` → `sat`.
- comparison context `(<= (- (ite p 1 2) 3) (- 0.5))` → `sat` (also driven through
  `same_sort_terms`).
- non-const Int in Real minus `(= (- x 3) 0.0)` (x:Int) → `unknown` (graceful degrade,
  unchanged; coerce of a non-const Int is a sound mixed-Int/Real degrade, same as before).

## R2 (LOW) — tri-state garbage direction + DESIGN-LRA doc

**Finding.** `lra_config.ml` (LRA): unset→ON, `0/false/no/off`→OFF, garbage→ON (= default).
`manager.ml` (FAIR, also default-ON): unset→ON, `1/true/yes`→ON, **garbage→OFF** (opposite
of its own default) — and its comment claims this "matches the `OXSMT_LRA` flip lever," which
is factually wrong (LRA takes garbage→ON). That is the inconsistency.

**Reconcile decision (fleet-wide convention).** Adopt *"an unrecognized value resolves to the
lever's DEFAULT direction (garbage ≡ unset); the recognized opt-out/opt-in tokens are the only
switch to the non-default side."* This is least-surprising (a typo cannot silently flip a
reviewed, verified default) and unifies both levers: default-ON ⇒ garbage→ON, dark ⇒
garbage→OFF, i.e. "garbage takes the default." LRA already conforms. FAIR is the outlier;
aligning it is a one-line change in its own (active) lane — recorded as a cross-lane reconcile
item, NOT edited here.

**Code (LRA-local, byte-id-safe).** `lra_config.ml`: match the opt-out tokens after
`String.trim` so a value picked up from a script (`OXSMT_LRA="off\n"`, a trailing space)
still opts out; a genuine typo still resolves to the default (ON) per the convention. Trim
does not affect the clean values used by the gates (`0`/unset/`1`), so byte-id is unaffected.
Verified: unset/`1`/typo→ON; `0`/`off`/`"off "`/`" FALSE "`→OFF.

**Doc.** `DESIGN-LRA.md` §2: replaced the stale positive-allowlist sentence (which still
described the pre-flip default-OFF dark flag) with the current default-ON reality, and added
§2.1 "Environment value convention (fleet-wide)" recording the tri-state rule, the
garbage-takes-default principle, and the FAIR consistency note.

## Gates

- `make test`: exit 0, 0 hard failure(s) / 0 soft miss(es) (re-run after the lra_config change).
- `check-frozen`: 14/14 (only non-frozen `parser.ml`, `lra_config.ml` `.ml`, and `DESIGN-LRA.md` touched).
- non-Real byte-id (mine vs trunk `f75366318f` binary, default env / LRA ON): **0 diffs**
  on every non-Real file that completed on both binaries. Three runs — an 80-file mixed set
  (QF_UF/AX/LIA/UFDT/UFLIA) and a 110-file QF_UF/AX/UFDT/UFLIA set both gave 0 diffs (skips =
  files neither binary solves within the wall under box load = not byte-id datapoints), and a
  tiny 13-file set gave compared=10, diffs=0, skipped=3. Confirms R1 is Real-only (it fires
  only under `expected = Some Real`, which no non-Real logic ever supplies to the `-` arm) and
  R2's trim is inert under the clean default env.
- QF_LRA 300 (first-300 by path) vs `:status`: **145 solved / 0 disagreements**. (Solved is a
  load-floor: this ran under load ~97 with a 3s wall, and R1 is solve-monotone — it can only
  turn a Real-context `unknown` into a verdict, never remove a solve — so no regression is
  possible; the flip review's 156–160 was a different sample on a quieter box.)
- quant-LRA degrade (mine vs trunk, 60-file `LRA/` sample): **60/60 degrade to `unknown`,
  0 verdict mismatch vs trunk** — quantified-Real handling unchanged.

## Files
- `smt/smtlib/parser/parser.ml` (R1, +13/-1)
- `smt/core/lra_config.ml` (R2 code, +14/-6)
- `DESIGN-LRA.md` (R2 doc, +36/-5)
