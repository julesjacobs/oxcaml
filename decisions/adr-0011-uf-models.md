# ADR (ACCEPTED): Uninterpreted-function-table models (the QF_UF model gap)

- **Status:** ACCEPTED — implemented and landed on `task/uf-models` (trunk `e41b126672`); **this land IS its acceptance.** Promoted verbatim from `logs/adr-uf-models-draft.md`; the DRAFT status line and all sections below are preserved as-ratified for the record. Review chain: core dual-review APPROVED-WITH-CHANGES (`logs/adr-uf-models-review.md` + `logs/codex-review/adr-uf-models.md`) → C1/F4/F2 fold → codex TCB round (1 HIGH + 2 MEDIUM, all fixed) → F3 + uniform-gating → uf-reviewer FINAL APPROVED at `a31fe786a7` (codex fixes empirically verified; gate GREEN, honeypot flips on dropped rows). Implementation review + suites in the `uf-models` board row.

- **Status:** DRAFT — design-only. **REVISED per the reconciled review order R1-R11** (`logs/adr-uf-models-reconciliation.md`, converging `logs/adr-uf-models-review.md` (nonown) + `logs/codex-review/adr-uf-models.md` (codex); both CHANGES-NEEDED, no genuine disagreements). Substrate grounded on the LANDED wiring (trunk `6a0b29de68`) per `logs/uf-models-scout.md`. §2 freeze fork RULED: **fork (b), the unfrozen `Cdclt` output currency — no frozen-surface change.** All sections normative.
- **Owning gap:** of 7,503 QF_UF corpus files, **7,269 (96.9%) declare a non-nullary function/predicate** and so need function TABLES to produce a verified `sat`; only **234 (3.1%) are nullary-only** (the slice the function-free pipeline can reach today). The solver degrades the rest to `unknown`. Biggest solved-rate lever on the board (R11).
- **Scope (R3, normative):** **first cut = QF_UF only** (uninterpreted sorts + functions/predicates). **QF_UFLIA mixed-sort tables (Int-keyed / Int-valued) are DEFERRED** behind a concrete-ℤ realization algorithm (§10 stub). No change to the unsat path, the SAT core, the combination algorithm, or EUF engine internals beyond model read-out.
- **Sequencing prerequisite (O6):** this is downstream of the M4 session↔combination wiring, now **LANDED (trunk `6a0b29de68`)**: the combinator is threaded into `Session` via the `Cdclt` driver and the combined model is consumed. R1's promotion checker and R9's transport build on that seam; the Bool-const-forwarding gap (#46) is folded in. Implementation builds directly on current trunk.

## 0. Prior fact — the WIRE format is already pinned and dual-validated (R6: which "Model")

The `.model` sidecar sexp grammar is already parsed AND consumed by BOTH independent N-version tools:
- **§8 evaluator** (`tests/eval/`, `oxsmt_core`-only, its own reader): App→table lookup+default, `eval.ml:106-131`; grammar `eval_model.ml:66-99` (n-ary + mixed-sort + default-only all parsed).
- **Gate** (`tests/gate/`, Lean): sort→`Fin n`, function→total nested-`if` table, `encoder.ml:210-273`.

```
(model (sort S 2) (const a 0) (fun f (default 0) (case (0) 0) (case (1) 0)))
```

**R6 — two "Model" types, stated to avoid the trap:** the table-capable Model those consumers read is the **sidecar reader type** (`tests/gate/model.ml`, `tests/eval/eval_model.ml`), **NOT** `Oxsmt_core.Model.t` — which is abstract with only `value`/`of_alist` and carries NO tables (`of_alist` builds empty sort/function metadata). So §0 de-risks the **wire** format (accurate and real); the genuinely-new work is (i) EXTRACTING tables from the e-graph and (ii) carrying them through a model currency to that wire format. Both N-version consumers being ready is the central de-risk: format-conformance is checkable against two pre-existing validators.

## 1. Extraction — ONE spec: recursive closure over LIVE roots (R4, R2, R10)

Finite model from the congruence closure. **Normative construction:**

- **Universe per uninterpreted sort S (R2):** `T_S/≡` — the distinct `class_of` representatives of registered terms of sort S — **if nonempty; else `{★_S}`, one anonymous adjoined element.** SMT-LIB uninterpreted sorts are inhabited, so **cardinality ≥ 1 always** (`(sort S 0)` is malformed / rejected by both readers). Elements are 0-based indices.
- **Function/predicate table per symbol `f` (arity ≥ 1):** every `App(f,·)` node contributes a row `(elt(class arg₁),…) → elt(class result)`. **Well-defined by congruence** (equal argument tuples are already one class → one result), not an approximation.
- **Default:** any element of the codomain universe is sound (§6); use the **least index** (for an adjoined-only sort, `★_S`) for determinism.

**R4a — recursive subterm closure, not shallow group-by.** `atom_terms` stores registration ROOTS, not every App node (`euf_adapter.ml:38,68-95`); a shallow pass MISSES nested apps (`f(g(x))` omits `g(x)`'s row → silently `default` → mis-model). Extraction MUST recursively traverse each root's full subterm closure, grouping ALL `App(f,·)` including nested.

**R4b — LIVE roots only (dissolves the grow-only/pop hazard).** `atom_terms` is grow-only / not trailed; after a `pop` it retains stale roots, and `class_of` **re-registers** a truncated term as a fresh singleton (`euf.ml:326-368,604-605`) → spurious universe elements/rows (inflates the universe → gate `decide` cost + determinism risk; same family as #42/M1). Extraction MUST range only over terms LIVE in the current post-pop asserted problem. **Chosen mechanism (spec one, per lead):** ride the model snapshot `Cdclt` already takes at the accepting `Final→Sat` (`cdclt.mli:5,43-44`), enumerating the atoms interned via `Cdclt.intern_atom` that are ACTIVE under the satisfying assignment — those are definitionally live. **No `euf.mli` change and no no-open-push restriction needed** — the snapshot-scoped path is the first cut, and per §2 rider 2 it is ENFORCED frozen-surface-free. If implementation finds that liveness genuinely cannot be read off the active-atom set and a non-mutating `euf.mli` `class_of_opt`/`is_registered` primitive is required, that is a **STOP-AND-REPORT to the lead** (it would touch the frozen surface), never a silent addition.

**R4c — Bool-codomain / predicate cells degrade when buried.** A predicate `p:…→Bool` is a `Fun`-kind Bool-sorted e-node (`euf.ml:33-35`). A row's result is a genuine `true`/`false` ONLY if that `p(args)` node is bound to `true_const`/`false_const` in EUF — i.e. it SURFACED as a SAT atom (K_bool). A BURIED `p(args)` (`g(p(a))`, never a top-level atom) is the undetermined H2 class (EUF does not 2-value uninterpreted Bool terms). **Normative:** extraction detects this with the SAME predicate the combinator already uses (`model_eval ma node = Some (Model.Bool _)`); an undetermined buried Bool result **degrades the model to `unknown` via the combinator's `Incomplete` class — never a guessed/opaque cell.** (Int- and uninterpreted-codomain cells have no such hazard: any class id is a valid element.)

**R4 net rule (synthesis, normative).** Extraction = a recursive subterm-closure walk rooted at the LIVE active interned atoms (R4b: the `Cdclt` accepting-`Final→Sat` snapshot's active atoms), descending each into its full closure (R4a) so that a nested application under an active atom — `g(x)` inside an active `f(g(x))` — IS enumerated and gets its own row; grouping ALL `App(f,·)` reached this way by symbol; universes/defaults per R2; Bool-codomain undetermined cells degrade per R4c. The active-atom set supplies liveness; the per-root recursion supplies nested-app completeness — codex's shallow-pass gap is closed precisely because the recursion is applied to each active root, not a flat group-by of the atom list.

**R10 — byte-determinism recipe (full).** Ascending class-id element numbering is necessary but not sufficient. After the live recursive walk: (a) number each sort's elements by ascending class-id gathered over the walk; then canonically sort (b) the sorts, (c) the functions, and (d) each function's case tuples — a total deterministic order independent of `Hashtbl`/registration iteration. The rendered bytes are then a pure function of the accepted model.

## 2. Model currency — RULED: fork (b), the unfrozen Cdclt output currency (R5)

**RULING (lead, normative):** function tables ride the **unfrozen `Cdclt` output currency**; `Oxsmt_core.Model.t` (FROZEN) is **NOT** touched. Rationale (nonown addendum, adopted): a UF function table is an **OUTPUT-ONLY artifact** — the internal model-based combination compares per-term values / class-ids and **never consumes tables**, so forcing them into the frozen internal currency (`Theory.model : t -> Model.t`) is the real incoherence; all three consumers (the CLI renderer, the R1 checker, the sidecar) are `Cdclt`-natural. So the combinator exposes tables via a **non-`THEORY` side accessor** that `Cdclt` calls directly (it already binds concrete `Oxsmt_combine.Combine`), and `Cdclt` gains the table-bearing output type. **No frozen-file change; no §10 unfreeze.**

```ocaml
(* cdclt.mli — NOT frozen; additive *)
type fun_table = { default : value; cases : (value list * value) list }
type binding = Const of string * value | Fun of string * fun_table    (* NEW *)
type sort_card = { sort_name : string; card : int }
val model : t -> (sort_card list * binding list) option   (* extends model_bindings *)
```

Four normative riders:

1. **Checker currency.** The R1 in-process checker (§3) reads the **`Cdclt` OUTPUT model** — the very artifact that gets rendered to the sidecar. This is a feature: the check covers extraction AND carry (it validates what we actually ship, not an internal precursor).
2. **No frozen touch, ENFORCED.** The first cut uses ONLY the snapshot-scoped extraction mechanism (§1 R4b, active interned atoms) — **NO `euf.mli` change.** If implementation discovers the non-mutating `class_of_opt`/`is_registered` fallback is genuinely needed, that is a **STOP-AND-REPORT to the lead** (it would break the frozen-surface-free property), **never a silent addition.**
3. **Future-proof clause.** Fork (b) does NOT preclude extending `Model.t` later: if/when a future theory (datatypes, arrays) needs structured values in the INTERNAL combination currency, that is a separate theory-driven §10 decision. (b) means "tables are output-only *today*," not "`Model.t` never grows."
4. **Encoding pin + firewall's second job.** The `Uninterp`/element wire encoding is pinned at the extraction/rendering layer (fork-independent: element = 0-based class-index, §0 grammar). Lifting the `model_bindings` table-free firewall (§3) must **PRESERVE its second job** — degrading a model with a function application appearing ONLY inside an arithmetic atom (the no-purification guard nonown flagged; the combination is incomplete/unsound there without a purification pass). The firewall stops being "any table ⇒ None" and becomes "a table we cannot soundly extract-and-self-check ⇒ None", but the arithmetic-buried-application degrade stays.

## 3. Session / CLI — re-grounded on wiring2; the R1 promotion checker (R1, R9)

**Substrate (LANDED trunk `6a0b29de68`).** The session is theory-wired via the `Cdclt` driver (`Cdclt.create` instantiates `Combine (Uflia_router) (Euf_adapter) (Lia_adapter)`, `cdclt.mli:1-2`). `build_model` (`session.ml:222-244`) already reconstructs the nullary-symbol theory model AND unions Boolean propositional assignments — **this subsumes the #46 Bool-const gap.** The REAL firewall to lift is narrow: `Cdclt.model_bindings` returns `None` "if … no table-free model is reconstructable" (`cdclt.mli:43-46`), and `check_sat`'s SOUNDNESS RULE degrades to `Unknown` precisely then (`session.ml:261-273`). Corpus CLI path: CLI → `Session.check_sat`/`get_model` → `Cdclt` → `Combine`. So the session-side work is: **extend `Cdclt`'s output (per §2 fork (b)) to build tables instead of returning `None`; extend `build_model` to emit `(sort …)`/`(const …)`/`(fun …)`; lift the table-free clause of the SOUNDNESS RULE — only behind R1, and PRESERVING the firewall's second job (§2 rider 4): a function application appearing ONLY inside an arithmetic atom still degrades to `unknown` (the no-purification guard). The lifted rule is "a table we cannot soundly extract-and-self-check ⇒ `unknown`", not "any table ⇒ `sat`".**

**R1 (CRITICAL, normative) — a solver-side in-process model checker gates every function-model `sat`.** Today `session.ml check_sat` promotes a theory `sat` with NO in-process model evaluation; the §8 evaluator gates only the external harness (`run_harness.ml:261-280`). So the "self-certifying" argument does not hold on the shipped path. **Ruling:** `Session` gets an **obligatory candidate→verified promotion step** — it CANNOT emit `sat` for a function model unless the candidate model passes an internal checker; **fail-closed to `unknown`** otherwise. Checker spec (this merits a LIGHT design confirm):
- lives in `smt/` (NOT `tests/`); **`oxsmt_core`-only**; it does **NOT** import `tests/eval` — the N-version firewall stays intact, `tests/eval` remains the independent external validator;
- input: the candidate model (the §1 tables + universes + const/Bool bindings) and the ORIGINAL asserted formula (the session has the asserted terms);
- evaluates **EVERY original assertion** (not a subset) to `Bool true` under the model (App→table lookup+default, arithmetic, equality, connectives — the same evaluation shape as `eval.ml`, re-implemented solver-side, not shared);
- result: all-true → promote to `sat`; any-false / any-unevaluable → **`unknown`** (fail-closed). It is a witness/self-cert guard, not the verdict authority (§6).
- **VOCABULARY PIN (nonown R1-confirm delta):** the in-process checker, the emitted witness, and the external `tests/eval` evaluator MUST all reference the SAME vocabulary — the ORIGINAL asserted terms (pre-preprocessing). The session stores the original term (before `Preprocess.run`; implemented), so the internal check evaluates exactly what the external evaluator reads back from the `.smt2`; a future non-trivial preprocessing (Bool-`Ite` lift, QF_UFLIA div/mod) cannot silently diverge the internal check from the external one. For QF_UF preprocessing is ≈ identity so this is moot today; fail-closed contains any divergence regardless. (Spec clarity, not a behavior change.)

**R9 — harness model-transport.** The solver→harness channel is flat-constant-only (`harness.ml:120-130,425-464`). Function-table output needs the **sidecar file** as the richer carrier (both N-version consumers already parse it) — an explicit acceptance-path prerequisite. The CLI renders the §0 grammar (quoting via `Printer.quote_symbol`, fail-closed on an unrepresentable name).

## 4. §8 evaluator — spec is the §0 grammar; completeness contract (R7)

Independent per N-version (its own reader, `oxsmt_core`-only); the §0 grammar + §1 element semantics are the complete spec (implement-from without reading solver code). The App-table evaluation already exists (`eval.ml:106-131`); the eval-side agent's task is confirm-and-harden (arity ≥ 2, all codomains, default-fallthrough, empty/adjoined sort R2).

**R7 — define & align the completeness contract.** `eval_model.ml:101-147` validates supplied entries but does NOT require "every declared symbol assigned" (a missing UNUSED symbol passes); the Lean encoder demands ALL declared. Define **formula-complete** (every symbol the assertions USE is assigned) vs **signature-complete** (every declared symbol/sort assigned). **Normative choice: the emitted model must be FORMULA-complete** (the assertions' used symbols; this is what soundness needs and what the R1 checker verifies), and every used uninterpreted sort still gets a `(sort …)` entry (R2, incl. an adjoined-only sort). The evaluator and the gate encoder must AGREE on this contract — align the gate's "all declared" demand to formula-complete, or have the CLI emit signature-complete supersets, but pick one and make both readers accept it.

## 5. Gate sat-direction — trust tier + timeout policy (R8)

Feasibility is established: the encoder already emits `Fin n` + function tables and proves goals `by decide`, falling back to `native_decide` (`encoder.ml:202-273`, `gate.ml:52-74`). **R8 (normative, honesty/provenance):** `native_decide` adds compiler + `Lean.ofReduceBool` trust (weaker than kernel `decide`; adr-0006:288-294), and the fallback fires only on a *classified tactic failure* — a wall-clock TIMEOUT returns `INCONCLUSIVE` with no native retry (`gate.ml:52-73`, `lean_runner.ml:56-67`). So: `certify_sat` must RECORD which tactic succeeded (label compiler-trusted vs kernel-certified), and the ADR states a benchmark/timeout policy. Already sound (timeout → INCONCLUSIVE, never false CERTIFY; missing assignment → loud `Encode_error`). Coverage gap to close in acceptance: no honeypot drives a non-nullary `declare-fun` through the sat path.

## 6. Soundness (R1 framing)

The **verdict** rests on the combination: a theory-certified `sat` is genuine by Nelson-Oppen soundness (the internalization combination, modulo its known documented gaps). The **R1 in-process checker is a fail-closed witness / self-certification guard**: a wrong or incomplete table makes the check fail → `unknown`, never a wrong verdict. The residual same-lineage correlated-blind-spot risk (the solver-side checker sharing the solver's lineage) is covered at corpus time by the UNCORRELATED external `tests/eval` (independent reader/impl) + the Lean gate. So the risk class introduced here is **completeness/format — a bad table degrades to `unknown`** — NOT wrong verdicts. Fail-closed rendering (unrepresentable name / undetermined buried Bool / incomplete model → `unknown`) keeps the gate asymmetry intact.

## 7. Acceptance (R3 scope, R11 grounded target)

- **Target logic = QF_UF ONLY (R3).** The ~140 QF_UFLIA unknowns are OUT of scope (deferred, §10); acceptance must not conflate logics.
- **Grounded structural breakdown (R11), fold in:** of 7,503 QF_UF files — 7,269 (96.9%) declare a non-nullary fn/predicate (need tables); 234 (3.1%) nullary-only (today's reachable slice); 690 (9.2%) declare an applied predicate (`…→Bool`, the R4c surface, a buried subset degrades). **This is structural (declare-fun arity), NOT the runtime sat/unknown/timeout split** — run `corpus_run`/status tooling to ground the "6,712 internally-sat" and "532 SAT-search timeouts" runtime numbers before fixing the numeric target.
- **Acceptance:** a QF_UF slice flips `unknown → sat` with the R1 checker green in-process AND 0 external `tests/eval` mismatches AND the gate green on the certified slice; determinism byte-identical ×2; a function-table `sat` gate honeypot + witness added (§5 gap); no regression on unsat / the nullary fast path / existing goldens; **`check-frozen` unchanged — fork (b) touches no frozen surface** (a `check-frozen` diff is a regression / the §2-rider-2 STOP-AND-REPORT tripwire).
- **Named residual (R11, replaces "out-of-fragment shapes"):** buried-Bool `Incomplete` degrades (subset of the 690), oversized universes → gate `decide` cost, and the separate SAT-search timeouts (~532, runtime).

## 8. What does NOT change

The unsat path + its Lean `grind` encoding; the SAT core; the internalization combination algorithm + interface set; **the FROZEN surface — `model.mli`/`theory.mli` untouched (§2 fork (b)); `check-frozen` stays 13/13**; EUF engine internals (only `class_of` / registered-term read-out — a non-mutating `euf.mli` query would be a §2-rider-2 STOP-AND-REPORT, not taken in the first cut); LIA numeric authority; the sidecar wire grammar (reused verbatim); the `tests/eval` N-version independence (R1 does not import it).

## 9. Open questions (post-reconciliation; most resolved above)

Resolved into normative text: O1→§10 deferral; O3→R4b (snapshot-scoped live walk); O6→§0 sequencing + §3 substrate; default→§1/§6; determinism→R10; two-Model→R6; the §2 freeze fork→RULED (b). **Genuinely open, each getting a LIGHT design confirm when its spec is written (not a full dual round):** (i) the R1 in-process checker interface (§3) — confirm the evaluation surface + fail-closed contract; (ii) the R3 concrete-ℤ realization stub (§10). Nothing else blocks.

## 10. Deferred — QF_UFLIA mixed-sort tables (R3 stub, gets a light confirm)

Int-sorted EUF classes need a **concrete-ℤ realization algorithm**: a class used as a function argument/result must be realized as an actual integer consistent with LIA's model, and the merged model can OMIT pure-EUF Int terms (`combine.ml:606-649`, `lia_adapter.ml:151-155`). So a mixed-sort table (Int-keyed / Int-valued) needs: (1) for each Int-sorted class touched by a table, obtain LIA's integer value (numeric authority) — or, for a pure-EUF Int class LIA does not value, pick a fresh integer distinct from all constrained ones; (2) key/value table cells by those integers (not element indices); (3) re-run the R1 checker (which evaluates Int arithmetic) to catch any inconsistency. This is its own design item with genuine algorithmic content (the fresh-integer choice must respect all LIA (dis)equalities) — deferred, with this stub as the seed. QF_UFLIA unknowns stay OUT of the QF_UF acceptance target until it lands.

---

*Revised per R1-R11; §2 freeze fork RULED (b). Dispatchable for QF_UF implementation after light confirms on the R1 checker spec (§3) and the R3 realization stub (§10); the implementation PR's standard dual review covers the rest (reconciliation §Dispatchability).*
