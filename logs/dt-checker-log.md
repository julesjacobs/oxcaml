# DT checker DAG-awareness — task #28 (Dt_model_check.inhabits/v_eq/ev)

Builder: dt-checker-fable. Worktree `worktrees/dt-checker`, branch `task/dt-checker`, off
trunk `8e056625e6`. Files touched:
- `smt/interface/dt_model_check.ml` (impl: the fix + the post-review O(1) sort guard).
- `smt/interface/dt_model_check.mli` (post-review: single-Context precondition doc; still NOT
  in FROZEN.sha256, so no unfreeze ritual — `check-frozen` = 14 match, unchanged).
- `tests/solver/dt_sat_gate.ml` (new `run_dag_blowup` discrimination test).
- `tests/dt-goldens-sat/dt_embedded_diamond_sat.smt2` (end-to-end RED golden).

## Post-review riders (dual review APPROVE @8566be295c → byte-identical @4ca011a0be; codex
## APPROVE-under-contract, `logs/codex-review/dt-checker-8566be295c.md`). Lead adjudication:
1. **mli single-Context precondition (codex CRITICAL, contract-only).** codex noted `ev_memo`
   keys on `Term.tag`, unique only within one `Context`; two terms from different `Context`s
   colliding on a tag would alias. This is pre-existing in surface (the tag-keyed `env`
   `Term.Table` already required single-Context) and production-unreachable (`Session.commit_sat`
   always passes one `Context`) — NOT a wrong-sat. Rider = state the single-`Context` precondition
   explicitly in `check`'s own doc (the TCB entry point should not merely inherit it from
   `term.mli`). No code change.
2. **O(1) sort guard (codex HIGH, fragment-gated).** `Inhabits_key.hash` called `Sort.hash`
   (which recurses over `Array` sorts) BEFORE the sort match, so a deeply-nested `Array`-sorted
   position risked `Stack_overflow` in the hash. Rider = restructure `make_inhabits` so the memo
   (hence `Sort.hash`/`Sort.equal`) is consulted ONLY in the `Sort.Datatype _, Ctor _` arm
   (`Sort.hash` on a `Datatype` is O(1) identity); every non-datatype `(sort, tree)` — including
   any `Array` sort — returns `false` in O(1) via the catch-all WITHOUT hashing. Verdict-identical
   (same logic; leaf/array cases were pointlessly memoized before) and the datatype recursion —
   the only DAG-sharing path — is still memoized, so the diamond still collapses (gate 26/0,
   run_dag_blowup + embedded golden still GREEN). Cannot accept-more (codex agrees).

## Outcome
Made `Dt_model_check`'s recursive re-derivation DAG-aware so a diamond-shaped sort graph no
longer costs ~2^depth checker visits. Verdict-preserving (0 flips over the full 8700-file
QF_DT corpus). The blowup is REAL and END-TO-END REACHABLE through the product path (trunk
HANGS on the `dt_embedded_diamond_sat` golden; fix returns checked-`sat` instantly) — codex+
fable dt-spine finding 2, VERIFIED, pre-existing on trunk. It is not present in the current
8700-file corpus (hence 0-flip), so the golden is a constructed reachability witness.

## Root cause (confirmed against source)
The model BUILDER memoizes `base_tree` per sort (`dt.ml:1075-1109`, comment "codex (e)"), so
the value it assigns a free datatype variable is a SHARED DAG: for `Si = ci(S(i+1), S(i+1))`
bottoming at a nullary `end`, both fields of every level point at the SAME physical
`base_tree(S(i+1))` object — `N` distinct physical `Ctor` nodes but `2^N` root-to-leaf paths.
The checker (`inhabits` at `dt_model_check.ml:51`, and `v_eq`/`ev`) re-derived structurally
with NO memoization, so it unfolded the sharing and visited ~`2^N` nodes. A trivially-SAT
input could therefore TIME OUT at the sat authority even though the builder finished
instantly (builder ~N ticks, checker ~2^N visits).

## Fix
Physical-identity memoization of the three recursive re-derivations, all per-`check`-call:
- `inhabits` (now `make_inhabits reg`, a closure over a fresh table): memo keyed on
  `(Sort.t, physical tree)`.
- `v_eq` (moved inside `ev_with`): memo keyed on the physical pair `(tree, tree)`.
- `ev` (inside `ev_with`): memo keyed on `Term.t` identity (`Term.Table`).

Key module: `equal` is physical `==` on the tree component (+ `Sort.equal` for the sort);
`hash` is the stdlib bounded structural hash `Hashtbl.hash` (`hash_param` examines a bounded
prefix, so it terminates on an arbitrarily large / shared DAG). Physically-equal trees are
structurally identical ⇒ hash-equal ⇒ a genuine hit lands; two distinct objects that
hash-collide are separated by `==` (a missed hit → recompute, never a wrong merge).

## Soundness argument (TCB-adjacent)
- Each memoized function is a PURE function of its (physical) arguments within one `check`
  call: `reg` and the model `env` are fixed for the call and no tree is mutated. A memo hit
  is the SAME physical object, hence returns EXACTLY what recomputation would. Verified there
  is no context threaded through the recursion other than the fixed `reg`/`env`.
- Therefore memoization can only make the checker FASTER; it can NEVER make it accept a model
  it would otherwise reject. The "no wrong verdict" firewall (`Session.commit_sat` gates every
  DT sat through this checker; builder cannot produce unsat) is preserved. `ev`/`v_eq` cache
  only successful (non-`Bad`) results, so a `Bad`-raising path is recomputed and raises again
  — identical fail-closed behavior.
- Tables are created per `check` call (`make_inhabits` closes over fresh tables; `ev_with`
  creates `v_eq_memo`/`ev_memo` per call). Nothing survives across calls
  (per-term-cache-per-context lesson).
- `mli` unchanged and still exposes only `check`; no builder state imported; the checker
  remains a total independent re-derivation.

## RED evidence — TWO independent discriminators

### 1. End-to-end `.smt2` golden (product path) — `tests/dt-goldens-sat/dt_embedded_diamond_sat.smt2`
Datatype `A = a0 | a(rec:A, d:S0)` (has a base case `a0`) over a pure-diamond field-sort
chain `S0 = c0(S1,S1) … S39 = c39(S40,S40)`, `S40 = end`. Asserting `((_ is a) t)` forces the
ACCEPTED model `t = a(base_tree A, base_tree S0)`; the `d:S0` field is `base_tree S0` — a
SHARED diamond DAG (41 distinct physical nodes, 2^40 paths, builder memoizes `base_tree` per
sort). `Session.commit_sat → Dt_model_check` walks that embedded field.
- FIX: `make dt-sat-gate` = `26 checks, 0 failures`, this golden is CHECKED `sat` INSTANTLY.
- TRUNK: the trunk gate binary scanning the same goldens dir is SIGKILL'd at 25s (exit 137) —
  it HANGS on this golden (2^40 checker traversal). Same via the CLI: trunk `oxsmt_cli` on the
  embedded shape hangs (timeout, exit 124) at depth 30 AND 45; the fix returns `sat` in ~0.00s.
- Order-robust: CHECKED `sat` whether the golden sorts first OR last in the gate's dir scan
  (verified `aaa_embed`/`zzz_embed` variants, both pass) — no order dependence.

### 2. Direct-checker unit test — `tests/solver/dt_sat_gate.ml:run_dag_blowup`
Builds a depth-60 sort chain in the registry, hand-builds a SHARED diamond `ctor_tree` (`sub`
bound once per level, placed in both fields), and asserts `Dt_model_check.check` (a) ACCEPTS
the well-formed diamond, (b) REJECTS a bogus-root-name tree, (c) REJECTS an ill-arity shared
BOTTOM (proving the memo still validates the shared leaf, not a rubber-stamp — the accept
cases alone can't prove non-rubber-stamp). FIX: instant. Un-memoized checker (trunk file
copied in, gate rebuilt): SIGKILL'd at 20s (exit 137), hangs in the accept case.

## Empirical gates (by EXIT CODE)
| gate | result | exit |
|---|---|---|
| `make test` | all suites green (harness 69/0, dt-sat-gate incl. golden) | 0 |
| `make dt-sat-gate` | `26 checks, 0 failures` (+1 golden, +3 run_dag_blowup) | 0 |
| `make check-frozen` | 14 interfaces match | 0 |
| `dt_test.exe` | `8 checks, 0 failures` | 0 |
| `dune build @fmt` (my files) | clean (promoted) | 0 |

## 0-flip A/B — full 8700 QF_DT, sha-stamped both sides
- Trunk binary `oxsmt_cli.exe` built in `main` @ `8e056625e6`, sha `4e021551103ed2b3…`.
- Fix binary `oxsmt_cli.exe` from `worktrees/dt-checker`, sha `8f07549a685c86d4…`.
- Driver `/tmp/ab_qfdt.py` (10s/file timeout, verdict-token diff over every `(check-sat)`).
- Result: **TOTAL=8700, same=8700, FLIPS=0.** Verdict-preserving, as expected — the change
  is latent (no current corpus file embeds a deep-enough diamond to blow up at 10s; the
  golden is a CONSTRUCTED reachability witness, not present in the corpus).

## RETRACTION — no process-global cache (my earlier flag #51 was WRONG)
An earlier draft of this log flagged a "process-global cache" degrading the 2nd+ in-process DT
solve to `unknown`. I could NOT reproduce it and it is FALSE. What actually happens:
- A PURE diamond (`x : S0` where S0 has NO base case, only `ci`) degrades to `unknown`
  end-to-end — but ORDER-INDEPENDENTLY (verified: same `unknown` whether it sorts first or
  last; trunk CLI on it returns `unknown` in ~0.00s, i.e. the checker is never even invoked —
  the theory/builder declines the shape because there is no accepted model). My one-time
  "passed when first" observation was a misread of a run that then crashed on a missing file.
- The shape that IS accepted and DOES reach the checker is the EMBEDDED one (§1 golden): a
  datatype with a base case whose model embeds a diamond field. That is order-independent
  (always `sat`) and is the genuine end-to-end reachability witness.
No global/cross-`Session` mutable state is involved. bs-dt-fable's audit (no global cache in
non-test `smt/`; multi-solve green) is correct; #51 should be closed as not-reproducible. My
fix and both RED discriminators stand on their own and do not depend on that (retracted) claim.

## Review ask
TCB-adjacent (independent DT sat authority) ⇒ blocking DUAL review (codex + fable, ADR-0007).
Freeze tip at the task/dt-checker head; review the soundness argument above against the code
(esp.: memo key equivalence, per-call table lifetime, fail-closed on `Bad` unchanged).
Coordinated with bs-dt-fable (they own `dt.ml` L2b; I own `dt_model_check.ml` — no overlap).
