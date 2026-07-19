# vox2 IDE proof-pane correctness audit — catalog (2026-07-17)

Instrument: `tools/voxide-pane.js` (tip 01458f1525) over committed offline fixtures
in `tests/fixtures/`. Ground truth: the raw `-vox-dump-vc-json` per-VC dump
(`location` / `program_point` / `goal.source_span` / `goal.text` vs `goal.display`)
from `worktrees/refinement-codex/_install/bin/ocamlc.opt`, and the source semantics.
Correctness is judged against the dump + semantics, never against the browser.

All repros are offline and deterministic:
`node tools/voxide-pane.js examples/<F>.ml --vcs-json tests/fixtures/<F>.vcs.json --line L --col C --section body --no-file --stdout`
(1-based L/C, exactly as the editor shows the caret). The workspace fixture is
`tests/fixtures/xmod.workspace.json` (add `--file Client.ml`).

## Counts
Confirmed-reproducible: 14 (3 honesty, 4 correctness/attribution, 3 display, 4 clarity).
Suspected / compiler-completeness (pane cannot fix alone): 3.
Two of the five seed bugs (attribution, `--file`) are confirmed and generalized;
the seed's DISPLAY claim is **partially wrong** and is broadened below (D1).
The attribution criterion is sharpened to per-caret (ch=0..n) granularity in the
dedicated section below, which adds two new attribution bugs (B1 end-boundary,
B2 nested-winner) beyond the seed's program_point bug (C1).

## Top 6 (ranked)
1. H1 — state-at-cursor invents false facts at the wrong scope (sibling defs, and a
   concrete call-site value mislabeled as a parameter on the definition line).
2. C1 — every contract-argument obligation is attributed to the whole call incl. the
   function name, not the argument value (seed #2, now shown uniform + cross-module).
3. B1 (NEW) — the caret at a span's END edge (`ch == end`) never shows the obligation,
   so the line-end caret right after the argument goes blank on EVERY example. One-char
   fix (`<` → `<=`). Sharpened caret audit below.
4. H3 — inside an `else` branch the pane shows the `then` branch's obligation **and a
   hypothesis that is false in the else branch** (`n <= 1`).
5. D1 — the dump's "source-like" `goal.display`/hypothesis display still leaks raw AST
   (`app[...]`, `constructor[...]`) for `if`/`let`/unit — it reaches the pane, not just `-i`.
6. H2 — state-at-cursor label collisions: two facts printed with the same label
   (`a : a > 0` and `a : 3 > 0`), the concrete one masquerading as the parameter.

---

## CONFIRMED — HONESTY

### H1 (honesty). State-at-cursor reports facts outside the caret's scope
`stateAtCursor` (pane_model.js:223) pools **every** named hypothesis from **every**
VC in the file and places each by the text position of its origin span, with no notion
of which definition the caret is in. Three distinct false-fact manifestations:

- **Concrete call value mislabeled as a parameter, hoisted to the definition.**
  Repro: `multi_arg.ml` L8 C45 (caret on param `b`'s annotation):
  ```
  known at this point
  a : a > 0
  a : 3 > 0      <-- FALSE here
  b : b > 0
  ```
  SHOWN: `a : 3 > 0`. EXPECTED: nothing about the value `3` on the definition line.
  Why: VC2 (`4 > 0`, the call `add_pos 3 4` on L10) carries a hypothesis
  `name="a" display="3 > 0"` whose origin span is param `a`'s annotation `_ > 0` on
  **L8** (dump: fact origin span L7C22-27, 0-based). state-at-cursor places the fact by
  that L8 span, so a concrete argument from the L10 call is asserted on the L8
  definition and labeled with the parameter name. On the definition, `a` is a bound
  variable, not `3`.

- **Sibling-definition leakage.** Repro: `nested_call.ml` L10 C1 (top-level, after
  `let use = pos (pos 5)`): shows `x : x > 0`, `x : 5 > 0`, `pos : pos 5 > 0`.
  `x` is `pos`'s *internal* parameter (defined L7); it is not in scope at the
  top-level `use`. EXPECTED: `use`'s own facts only (here, none).

- **Internal VC hypotheses shown as program facts at a sibling binding.** Repro:
  `xmod.workspace.json` `--file Client.ml` L8 C1 (`let bad = ...`): shows
  `good : good > 0`, `pos : pos (...) > 0`, `_ : one (...) > 0`, `one : one (...) > 0`
  — proof-context hypotheses of L7's `good` call, presented as "known at this point"
  for the `bad` binding.

Root cause: pane_model.js:223 `stateAtCursor` uses only `posCmp(h.span.end, cursor)`;
no enclosing-definition gate, no de-dup by content. The pane labels the view
"Approximate … a binding introduced inside a branch or other nested scope may still
appear below that scope", which covers *under*-scoped branch locals but NOT (a) cross-
definition leakage nor (b) a concrete argument mislabeled as a parameter. Seed #4 is
the sibling-def subset; the mislabeled-argument case is new and worse (it is a
false statement, not merely a stale-scope one).
Proposed fix: scope state-at-cursor to the enclosing top-level definition (drop facts
whose origin span lies outside the def containing the caret); drop hypotheses whose
`display` is a value fact about a *different* subterm (or whose origin is a callee
param annotation, i.e. a contract-argument fact) rather than a binder live here; then
de-dup by label+display. Verify the "known here" view is empty where it cannot be sound
(the honest under-report), matching the guard.ml behavior (which is correct today).

### H2 (honesty/clarity). State-at-cursor label collisions
Two facts print with the same label so the reader cannot tell them apart, and the
concrete one impersonates the abstract parameter.
Repro: `multi_arg.ml` L8 C45 → `a : a > 0` and `a : 3 > 0`;
`nested_call.ml` L9 C15 → `x : x > 0` and `x : 5 > 0`.
SHOWN: duplicate labels. EXPECTED: distinct, honest labels (or, per H1, the concrete-
value fact should not appear here at all). Root cause: `hypLabel` (pane_model.js:196)
returns the origin `name` verbatim; several hypotheses legitimately carry the same
callee-parameter name. Fix travels with H1.

### H3 (honesty/correctness). Else-branch caret shows the then-branch obligation with a false hypothesis
Repro: `recursion.ml` L9 C37 (caret on the `+` inside the `else` expression
`fib (n - 1) + fib (n - 2)`):
```
obligation [proved]
n : n >= 0
h1 : n <= 1     <-- FALSE in the else branch
⊢ n >= 0
```
SHOWN: the **then**-branch obligation (`n >= 0` under `n <= 1`). EXPECTED: the
else-branch obligation `fib (n - 1) + fib (n - 2) >= 0` under the negation `n > 1`.
The displayed hypothesis `n <= 1` is exactly false at this caret. Same class in
`abs.ml` (caret in `0 - x` shows the then-branch `x >= 0`).
Why: the two branch annotation VCs both carry the **whole-if** span as `location` and
`program_point` (dump: VC0 goal `n >= 0`, VC3 goal `fib (n-1)+fib (n-2) >= 0`, both
L9C2-C49). `proofPaneModel` (pane_model.js:371-373) sorts overlapping VCs by
`spanSize`; on the tie it keeps array order (then before else), so the then VC wins
across the entire body, including the else region.
Root cause is the compiler's coarse `location` (both branch VCs share the if-span);
the pane amplifies it by presenting one as authoritative. Fix: compiler should give
each branch VC the branch sub-expression as `location`; pane mitigation — when equal-
span VCs overlap and disagree, do not present one branch's hypotheses as if they held
at an arbitrary caret (e.g. show a disambiguation / all overlapping goals, not a
single silently-chosen one).

### H4 (honesty). Foreign-unit data pollutes the buffer without `--file`/active (seed #3)
Repro: `xmod.workspace.json` on `Client.ml` — the ruler for L7
(`let good = Lib.pos (Lib.one ())`) is `000111111111111111111112` **with**
`--file Client.ml` but `001222222222222222222223` **without** it: the Lib.ml VCs and
hypotheses enter the Client.ml pane pool. `adaptAndFilter` (voxide-pane.js:316) filters
by unit only when `--file` is passed and otherwise uses every VC, ignoring the payload's
`active` field; `contains` (pane_model.js:164) compares line/col only, never `file`, so a
foreign span can match by coordinates. Fix (already tracked, task #139): default the
active unit to `payload.active` and route both obligations and state-at-cursor facts by
`file`. Severity honesty (a Client caret can surface a Lib obligation/fact).

---

## CONFIRMED — CORRECTNESS / ATTRIBUTION

### C1 (correctness). Contract-argument obligations cover the function name / whole call (seed #2, generalized)
`_vc_anchor_span` (compiler.py:407) iterates `("program_point", "location")` and takes
the first non-ghost — i.e. it prefers `program_point` (the whole call) over `location`
(the argument value). So the obligation surfaces on the callee name and surrounding
call syntax, not the value it constrains. Uniform across every contract-argument VC:

| file | caret shows obligation on | should be | dump location |
|---|---|---|---|
| overview.ml | `positive 7` (L11 C13-22) | `7` | L11 C22 |
| counterexample.ml | `need_one 2` (L11 C13-22) | `2` | L11 C22 |
| proof_tour.ml | `need_pos 0` (L18 C17-26) | `0` | L18 C26 |
| dependent.ml | `dep 3 3` (L8 C15-21) | 2nd `3` | L8 C21 |
| Client.ml | `Lib.pos (Lib.one ())` (L7 C12-31) | `(Lib.one ())` | L7 C20 |

Repro: `node tools/voxide-pane.js examples/overview.ml --vcs-json tests/fixtures/overview.vcs.json --line 11 --col 13 --section body --no-file --stdout` → shows `⊢ 7 > 0` while the caret is on `positive`.
Proposed fix: iterate `("location", "program_point")` (still skipping ghost).
**Verified safe**: annotation VCs' `location` is ghost in abs.ml and binder.ml (falls
through to `program_point`, the correct sub-expression); in recursion.ml the annotation
`location` is non-ghost and already equals `program_point`. So the swap narrows only the
contract-argument marks and leaves annotation marks unchanged.

---

## CONFIRMED — CURSOR-POSITION GRANULARITY (per-caret, boundary/space)

Method: a line of n chars has n+1 carets (ch=0..n). For every caret I compute
**shown** (the current tool: anchor = `program_point`-first, `contains` = `[start,end)`
— pane_model.js:164-171) and **optimal** (the target semantics: anchor = `location`-first
skipping ghost = the proposed C1 fix, `contains` = `[start,end]` inclusive, innermost-
wins). "optimal" is judged against the raw dump `location` spans + source, never the
browser. The harness's shown column was spot-checked byte-identical to
`voxide-pane --line/--col` at overview ch20/ch22, nested_call ch14, multi_arg ch20.
`[x|y]` denotes the caret between source chars x (left) and y (right); ⏎ = line end.

Aggregate (via `review/caret_attribution_audit.py`, reproducible offline):
**150 / 861 carets (17%) deviate** from optimal attribution across the 15 units.
Worst: overview/counterexample 10/23 (43%), guard 10/34, dependent 7/22, Client.ml
18/52. Annotation-only units deviate least and only via B1 (abs 1/48, unproved 1/45,
binder 2/53). Run: `VOX2_OCAMLC=… TMPDIR=… python3 review/caret_attribution_audit.py`.

Two NEW attribution bugs fall out, independent of C1:

### B1 (correctness, NEW). The END-edge caret of a span never shows its obligation
`contains` (pane_model.js:170) tests `position.ch < range.end.col` — the end boundary is
**exclusive**. The target semantics want `[start,end]` inclusive (a caret at a token's
right edge is "on" it). Because the constrained value is usually the LAST token on its
line, the **line-end caret** — where the cursor sits right after you finish typing the
argument — shows the placeholder instead of the obligation. Confirmed on EVERY example
(overview ch22, counterexample ch22, dependent ch21, guard ch26, proof_tour ch26/47,
multi_arg ch21/72, multi_param ch22/62, nested_call ch21/45, unproved ch44, abs ch30,
binder ch12/4, recursion ch35/49/41, xmod ch31/19). For the annotation-only examples
(abs, binder, unproved, multi_param result) this is the **sole** attribution deviation —
their anchoring is otherwise correct. Fix: `<` → `<=` in `contains` (see B/ambiguity
note before shipping). One char, flips ~20 boundary carets across the corpus.

### B2 (correctness, NEW). For nested calls, program_point shows the WRONG (inner) obligation
Because `program_point` nesting differs from `location` nesting, the inner call's
program_point can be *smaller* than the outer's and win where the outer obligation is
optimal. `nested_call.ml` L9 `let use = pos (pos 5)`, caret ch14-18 (`(pos ` — the outer
argument's open-paren and the inner callee name): **shown `5 > 0`** (inner), **optimal
`pos 5 > 0`** (outer, whose `location` is the whole `(pos 5)`). So program_point
anchoring is not merely "too wide" — it silently swaps which of two nested obligations
is presented. The C1 anchor swap (to `location`) fixes B2 too (location nesting matches
subterm nesting: `5`⊂`(pos 5)`).

### Per-example caret tables (interesting carets: token/arg boundaries, spaces, line ends)

overview.ml — `let seven = positive 7` (n=22):
| caret | on | shown | optimal | verdict |
|---|---|---|---|---|
| ch12 | `[ \|p]` start `positive` | `7 > 0` | `·` | DEVIATION (C1: on fn name) |
| ch20 | `[e\| ]` end `positive` | `7 > 0` | `·` | DEVIATION (C1) |
| ch21 | `[ \|7]` start `7` | `7 > 0` | `7 > 0` | ok |
| ch22 | `[7\|]` end `7` / ⏎ | `·` | `7 > 0` | DEVIATION (B1: end edge) |

guard.ml — `  if y > 0 then need_pos y else 0` (need_pos y):
| caret | on | shown | optimal | verdict |
|---|---|---|---|---|
| ch16 | `[ \|n]` start `need_pos` | `y > 0` | `·` | DEVIATION (C1) |
| ch24 | `[s\| ]` end `need_pos` | `y > 0` | `·` | DEVIATION (C1) |
| ch25 | `[ \|y]` start `y` | `y > 0` | `y > 0` | ok |
| ch26 | `[y\| ]` end `y` | `·` | `y > 0` | DEVIATION (B1) |
| ch27 | `[ \|e]` on `else` | `·` | `·` | ok (unrelated token) |

multi_arg.ml — `let use = add_pos 3 4` (two args, one line):
| caret | on | shown | optimal | verdict |
|---|---|---|---|---|
| ch10-17 | `add_pos`+space | `3 > 0` | `·` | DEVIATION (C1) |
| ch18 | `[ \|3]` start `3` | `3 > 0` | `3 > 0` | ok |
| ch19 | `[3\| ]` end `3` | `3 > 0` | `3 > 0` | ok |
| ch20 | `[ \|4]` start `4` | `3 > 0` | `4 > 0` | DEVIATION (C1: wrong arg — `3` covers `4`) |
| ch21 | `[4\|]` end `4` / ⏎ | `·` | `4 > 0` | DEVIATION (B1) |

nested_call.ml — `let use = pos (pos 5)` (nesting + B2):
| caret | on | shown | optimal | verdict |
|---|---|---|---|---|
| ch10-13 | outer `pos `+space | `pos 5 > 0` | `·` | DEVIATION (C1) |
| ch14 | `[ \|(]` open paren | `5 > 0` | `pos 5 > 0` | DEVIATION (B2: inner shown, outer optimal) |
| ch15-18 | inner `pos `+space | `5 > 0` | `pos 5 > 0` | DEVIATION (B2) |
| ch19 | `[ \|5]` start `5` | `5 > 0` | `5 > 0` | ok (innermost) |
| ch20 | `[5\|)]` end `5` | `5 > 0` | `5 > 0` | ok |
| ch21 | `[)\|]` end `)` / ⏎ | `·` | `pos 5 > 0` | DEVIATION (B1: outer arg end) |

recursion.ml — `  if n <= 1 then n else fib (n - 1) + fib (n - 2)` (branch + nested + C1 + B1):
| caret | on | shown | optimal | verdict |
|---|---|---|---|---|
| ch24-27 | first `fib `+space | `n - 1 >= 0` | `n >= 0` | DEVIATION (C1: fn name shows arg; opt is the annotation) |
| ch35 | `[)\| ]` end `(n - 1)` | `n >= 0` | `n - 1 >= 0` | DEVIATION (B1: arg end falls back to annotation) |
| ch49 | end `(n - 2)` / ⏎ | `·` | `n - 2 >= 0` | DEVIATION (B1) |
Caveat: both branch annotation VCs (`n >= 0` then, `fib(n-1)+fib(n-2) >= 0` else) carry
the SAME whole-if `location`, so even "optimal" here shows the then goal in the else
region — that residue is H3 (compiler-side branch spans), not fixable by anchor/boundary.

Client.ml (xmod) — `let good = Lib.pos (Lib.one ())` and `let bad = Lib.pos 0`:
| caret | on | shown | optimal | verdict |
|---|---|---|---|---|
| ch11-18 (L7) | `Lib.pos `+space | `one (…) > 0` | `·` | DEVIATION (C1: on outer fn) |
| ch19 (L7) | `[ \|(]` start `(Lib.one ())` | `one (…) > 0` | `one (…) > 0` | ok |
| ch31 (L7) | `[)\|]` end / ⏎ | `·` | `one (…) > 0` | DEVIATION (B1) |
| ch10-17 (L8) | `Lib.pos `+space | `0 > 0` | `·` | DEVIATION (C1) |
| ch18-19 (L8) | `0` then ⏎ | `0 > 0`, then `·` | `0 > 0`, `0 > 0` | ch19 DEVIATION (B1) |

### Boundary-ambiguity analysis + recommended convention
- **No natural shared-edge exists in the corpus.** Adjacent arguments are space-
  separated (`add_pos 3 4`: `3`=[18,19], `4`=[20,21], the space char is col19), so the
  two tokens' boundary carets are ch19 (end of `3`) and ch20 (start of `4`) — distinct,
  never the same caret. The only span meeting is NESTING (`5`⊂`(pos 5)`), resolved by
  innermost-wins, which the optimal column already does correctly (nested_call ch19-20).
- **The separating space.** Under the inclusive rule the space's two boundary carets
  coincide with the neighbouring tokens' edges (ch19→`3`, ch20→`4`); a multi-char gap
  would leave its interior carets at `·`. This is acceptable: a caret touching a token is
  "on" it. No interior-space obligation was observed.
- **Recommended convention for a genuine shared edge (end A == start B), which B1's
  `<=` change WILL create between two same-level adjacent spans:** innermost (smaller)
  wins; on equal size, the span that STARTS at the caret wins (you are entering the new
  subterm, not lingering on the old one). Concretely, implement `contains` as
  `start <= ch <= end` but, when selecting, break ties by (smaller span, then
  start==ch). This keeps B1's line-end fix while making the created shared-edge carets
  deterministic and intuitive. Without the tiebreak, `<=` alone would let the left
  token's end-caret and the right token's start-caret both match a shared caret and the
  current `spanSize`+array-order tie-break would be arbitrary.
- **Line start / end.** ch=0 correctly shows `·` everywhere (no span starts at col 0 in
  the corpus; a span that did should show at ch=0). ch=n is the B1 case above.

---

## CONFIRMED — DISPLAY (reaches the pane, not only `-i`)

### D1 (honesty/clarity). The dump's `display` renderer is incomplete — raw AST leaks into the pane goal AND hypotheses
The seed assumed `goal.display` is always source-like ("proves a source-like renderer
exists"). It does not: the renderer handles application / comparison / `&&` but leaks
raw AST for `if`, `let`, and the unit literal, and this text is what the pane prints
(via `goal.display` and hypothesis `display`), independently of the `-i` printer.

- `if/then/else` — `predicate_forms.ml`, caret on the `ite 5` argument:
  `⊢ (if (app[Stdlib!.>] 5 0) then constructor[bool/6!.true] else constructor[bool/6!.false])`.
- `let..in` — `predicate_forms.ml` `lin 5`: `⊢ (let y = 5 in (app[Stdlib!.>] y 0))`.
- unit literal `()` — `xmod` Client.ml VC for `Lib.pos (Lib.one ())`:
  `⊢ one (constructor[unit/7!.()]) > 0`.
- These leak in **state-at-cursor hypotheses** too (predicate_forms context pane shows
  `x : (if (app[Stdlib!.>] 5 0) then constructor[bool/6!.true] …`).
Root cause: the compiler source-like renderer does not recurse into `let`/`if` bodies
and has no case for the unit constructor. Fix belongs with task #141 but its scope must
include the DUMP `display` field, not only `-i`; add `let`/`if`/unit (and see D3).

### D2 (correctness/clarity). Cross-module display drops the module qualifier
`xmod` — the goal for `Lib.pos (Lib.one ())` prints `one (…) > 0` and hypotheses print
`pos : pos (…) > 0`; the `Lib.` path is gone, so the displayed goal does not match the
source term. Root cause: renderer prints the value identifier without its module path.

### D3 (clarity). Keyword-infix operators render prefix
Probe (offline, no committed fixture): `let f (x:int{ _ mod 2 = 0 }) = x  let g = f 4`
→ `goal.display = "mod 4 2 = 0"` (should be `4 mod 2 = 0`). Same expected for
`land`/`lor`/`lxor`/`lsl`/`lsr`/`asr`. Renderer treats keyword-infix operators as
prefix application. Reproduced via the real compiler; not committed because it produces
an ordinary contract-arg VC that would duplicate predicate_forms' role.

---

## CONFIRMED — CLARITY / FORMATTING

### CL1 (clarity/leak). Solver detail leaks an absolute temp path
Every failing/unproved obligation's `solver detail` starts with an internal throwaway
path, e.g. `unproved.ml`:
`/usr/local/home/jujacobs/tmp/vox2-vcf46c69.lean:4:2: error: `grind` failed`.
Also in counterexample.ml, proof_tour.ml, xmod. The pane shows `discharge.detail`
verbatim (pane_model.js:539); the compiler embeds the temp Lean filename in grind's
diagnostic. Fix: strip/relabel the temp path (compiler or adapter) before display.

### CL2 (clarity). `+N more obligation here` is not inspectable
Where obligations overlap, the pane shows the smallest and a bare count of the rest:
`multi_arg.ml` L10 (`add_pos 3 4`) shows `⊢ 3 > 0` + `+1 more obligation here` (the
hidden one is `4 > 0`); same in recursion / nested_call. The user cannot see or cycle
to the other obligation(s) from the pane. Fix: list the other goals (one line each) or
make them selectable.

### CL3 (clarity). Hypotheses never fade; irrelevant context is shown as relevant
`used` is hard-coded True in the adapter (compiler.py:474, "compiler tracks no per-fact
usage"), so fade-unused never fires. In `xmod` the disproved `0 > 0` obligation lists
four hypotheses about the *previous* binding `good` (`one`, `_`, `pos`, `good`), none
relevant to `0 > 0`, all shown unfaded. Fix needs a compiler usage signal, or drop
hypotheses that share no free variable with the goal.

### CL4 (clarity). Anonymous `_` hypothesis labels
`xmod` shows `_ : one (constructor[unit/7!.()]) > 0` — a hypothesis literally labeled
`_`. `hypLabel` (pane_model.js:196) uses the origin name `_` verbatim; a positional
`h<i>` would read better than a bare underscore. (Note: branch-condition facts already
have null names and become `h<i>` correctly; this is the explicit-`_`-binder case.)

---

## SUSPECTED / COMPILER-COMPLETENESS (pane cannot fix alone)

### S1 (suspected honesty). Refined arguments that emit NO VC
Probe: `type r = { a : int }  let f (x : r{ _.a > 0 }) = x  let g = f { a = 1 }`
emits **zero** verification conditions; likewise `let f (x:string{ String.length _ > 0 }) = x  let g = f "hi"`.
The pane then shows "No obligations reported" / "verified" for a program that carries a
refined precondition. If the compiler is silently accepting these unverified, the pane's
"verified" is a false-green (soundness, compiler-side); if it is merely declining to
model them, the pane should still say so. Needs compiler-side triage. (Also: the record
predicate leaks a stamp in `-i`: `(_).field[r/279[1].a]`.) No fixture committed (no VCs
to capture).

### S2 (note, not a bug). Disproved never yields a witness — the witness path is dead
Across every disproved case constructible from the real compiler (`counterexample.ml`,
`proof_tour.ml` `need_pos 0`, `at_least 5 2`, `f () : int{_>0} = 0`),
`discharge.counterexample` is null; the pane always shows the honest "refuted, no
concrete witness" note. The `candidate counterexample (unbounded-int model)` +
witness-rendering branch (pane_model.js:288-302, `WITNESS_PREFIX_LINE`) is therefore
unreachable from real dumps and untestable end-to-end. This is why the spec's
"disproved WITH a witness" corpus item could not be produced from the compiler; it is
recorded here rather than fabricated. The honesty behavior itself is correct.

---

## Corpus added (examples/ + tests/fixtures/)
New example programs (each a real program the compiler accepts or rejects) with
committed `/vcs` fixtures, chosen to cover cases the original 8 examples did not:

- `multi_arg.ml` — two refined args on one call + refined result (multi-obligation
  line; carriers for H1/H2/C1/CL2).
- `multi_param.ml` — refined result contract that mentions a parameter (`_ >= n`).
- `nested_call.ml` — `pos (pos 5)`: nested calls, obligation on a deep subterm
  (`pos 5 > 0`); carriers for H1/H2.
- `predicate_forms.ml` — `&&` / `if`/`then`/`else` / `let`..`in` predicates (D1 carriers).
- `unproved.ml` — `x * x >= 0`: an open (UNPROVED, no counterexample) goal, distinct
  from disproved.
- `Lib.ml` + `Client.ml` — cross-module (`Lib.pos (Lib.one ())` proved + `Lib.pos 0`
  disproved); fixture `xmod.workspace.json` from `check_workspace` (carriers for
  H4/D1/D2/C1/CL3/CL4).

Not producible from the real compiler and therefore documented, not committed:
disproved-with-witness (S2), match-in-predicate (rejected: "A match expression is not
yet supported in refinements"), record/string-length refined args (S1, no VCs).

## Fix-phase ordering (for the lead)
0. B1 boundary `<` → `<=` (pane_model.js:170) + start-wins tiebreak — one-line + a small
   selection tiebreak; flips ~20 line-end/edge carets; also close B2 via C1 below.
1. C1 anchor swap (compiler.py:407) — one-line, verified safe; flips 5 attribution repros
   and B2 (nested wrong-winner).
2. D1/D2/D3 renderer completion — compiler-side (task #141), must also fix the dump
   `display` field, re-run predicate_forms + xmod fixtures.
3. H1/H2 state-at-cursor scoping + de-dup (pane_model.js:223) — the biggest honesty win.
4. H4 `--file`/active default (voxide-pane.js:316, app.js) — seed #3 / task #139.
5. H3 branch-span (compiler location) + pane disambiguation; CL1 temp-path strip;
   CL2/CL3/CL4 clarity.
Each fix gets a regression fixture; re-run this audit; the anti-drift lock
(`node tests/test_pane_fidelity.js`, 22 checks) must stay green.
