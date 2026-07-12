# ADR-0013 — Certificates + Lean replay: the end-to-end path (M5, elaboration of ADR-0006)

Status: **RATIFIED (Rev 5) 2026-07-12** — design-only, no code in `main/`; implementation is a tracked post-M4/M5 dispatch. Promoted verbatim from `logs/adr-certificates-draft.md` (the original DRAFT Rev-5 status line and all sections below preserved as-ratified for the record); the §Appendix D3-erratum block was additionally applied to `decisions/adr-0006-certificates.md` as its Revision 5 at this promotion. Dual adversarial review complete — codex Rev-4 final leg (3 HIGH + 1 MEDIUM, all folded into Rev 5) + same-model legs; master-ratified. Elaborates ADR-0006.

_Original draft status line (preserved):_ **DRAFT Rev 5 — revised against the codex Rev-4 final leg**
(`logs/codex-review/adr-certificates-rev4.md`: 3 HIGH + 1 MEDIUM, all verified with
triggers; the same-model source-path leg CLEARED Rev 4 but its clear is superseded — its
walk missed the `Theory_prop` branch in `analyze_final` at `sat.ml:711`). Author:
cert-adr-author-2. Rev 4 folded RRR1–RRR4 + two post-audit folds; the codex leg then
confirmed **the RRR2 bypass call** (with the deferred alternative named) and **the E3
dropped-clause seam** (via the negative-only-selector invariant), and found four residual
holes in the terminal-exit enumeration (§4.0) + the A5 accounting identity. **No AST or
architecture change** (RR1/RR2/RR5, RR3 option (b), R5, and the whole AST still stand); the
Rev-5 residual is confined to §4.0's four-exit enumeration + §5 A5. New/changed vs Rev 4:
**[Rev-5 H#/M#]**.

**Rev-5 changelog (this revision).**
- **H1 [the load-bearing fold]** — the E3 `analyze_final` hook must materialize **both**
  `Implied_by` **and** `Theory_prop` reasons (the latter via `theory_reason_clause`,
  emitted as `Theory` propagation-leaf intros), or the terminal hints omit the
  theory-propagation ancestors that dominate the EUF/LIA production path and a genuine
  theory `unsat` session is uncertifiable. §4.0 E3 mechanism.
- **H2** — E1's Rev-4 "the empty clause is itself an `Input`" was wrong: `t.ok` falls when
  level-0 simplification filters an added clause to `[]` (`sat.ml:767–769`; e.g. two
  nonempty `Input`s `[a]`,`[¬a]`). E1's terminal step is now an explicit level-0-RUP
  `Resolution{[||]}`, not an `Input`. §4.0 E1.
- **H3** — a **fourth** in-search `R_unsat` route (E4): a Final-effort `T_lemma` that
  simplifies to empty at level 0 (`sat.ml:980–984`, sets `result` directly). It cannot be
  laundered as an empty `Input` (`origin = `Theory_lemma``); its terminal step is a
  `Valid_lemma` `Theory` intro + level-0-RUP `Resolution{[||]}`. §4.0 now says **FOUR
  exits**.
- **M1** — §5 A5's accounting identity omitted `checker/infra-error` and `timeout/killed`;
  both now roll up into `uncertified` so every `unsat` has a mandated bucket.
- **Citations for all four folds re-read from source** (R9): `sat.ml:711` (analyze_final's
  `Theory_prop` branch) / `:537–542` (`theory_reason_clause`, the materialization helper;
  `:515–531` is the `theory_explain_checked` CONTRACT-EX check it calls), `:767–769` (E1
  filter-to-empty), `:980–984` (E4), `:624`/`:626` (analyze's parallel materialization).

**Rev-4 changelog (prior revision).**
- **RRR1** — §4.0 now enumerates **all three** `Sat` `Unsat` exits (E1 solve-entry, E2
  level-0 conflict, E3 failed assumption), each with a defined terminal `[||]` step, and
  specs the **E3 mechanism** (the universal production exit) precisely.
- **RRR2** — §1.4(a) hint order corrected to `[rₙ..r₁; conflict]`; **unminimized-clause
  emission** adopted (option (b)); the Rev-3 `sat.ml` minimization-reason-id delta is
  **deleted** (net simplification). Minimized-with-dependency-ordered-hints recorded as a
  post-M5 size optimization, hazard noted.
- **RRR3** — §3.3 `invalid-certificate` bucket redefined to cover **every
  artifact-attributable rejection**; §4.1 honeypot routing reconciled.
- **RRR4** — §1.0 strip rule stated precisely (drop-if-any-inactive / strip-if-all-active).
- **Citations re-attested against the moving trunk** (R9): `session.ml` and
  `cdclt.ml`/adapter line numbers had drifted since Rev 3 and are corrected below; the two
  frozen-core cites (`explanation.mli`, `theory.mli`) were re-read and are unchanged.
- **Post-audit folds (source-path leg cleared Rev 4):** §1.4(b) now addresses the
  **solved-set divergence** of the minimization-off traced config (generous-CI-budget
  resolution, option (i), with uncertified-isolation fallback (ii)) + an A3 acceptance
  line; §4.0's E3 spec gains a **fail-closed guard** (assert the `analyze_final` core is
  all-selectors, loud uncertified otherwise).

**Successor note (Rev 5 completed by author-2).** This text is written to be carried from
the documents alone. Every load-bearing fact below was re-read from source for this
revision (R9 discipline) and is cited to `file:line` so the scoped re-review (focused on
the four §4.0 exits — especially the E3 `analyze_final` `Theory_prop` materialization (H1),
E1's filter-to-empty trigger (H2), and the E4 route (H3)) can verify it directly. The
**accepted `sat.ml`/session behavior deltas** the cert work requires are collected in §4.0
— they are the crux of freeze prerequisite (ii).

**Master ruling folded — RR3 = option (b):** the certificate is over the **unguarded
active CNF**; the emission point strips selectors (§1.0). Prior rulings still in force:
**R5** (`omega` M5 bootstrap → reflected multiplier checker target; D3 erratum at
promotion), **R4** (resolution-first staging; EUF is the first leaf).

Cites (re-read line-by-line for this revision, current trunk ~a8185aa87e+):
`smt/interface/session.ml` (selector guard — `Sat.add_clause (¬sel :: …)` :198, comment
:197; `current_selector = List.hd t.frames` :154; `frames` selector stack :65–66;
active-selector assumptions `List.map Sat.pos t.frames` :302 + `Sat.solve ~assumptions`
:304), `smt/interface/cdclt.ml` (`Split`→`T_lemma` :199–202; `split_lit`/CONTRACT-SPLIT
:176; mapping comment :15–16), `smt/solver/sat.ml` (unit reason = `Decision` :47–56 +
level-0 unit enqueue :770; antecedent prepend :626 + `List.rev` return :679; 1UIP clause
fixed at :628 **before** the minimization loop :629–658; `analyze_final` :684–723; three
`Unsat` exits — solve-entry `if not t.ok` :1015, level-0 conflict :912–915, failed
assumption `analyze_final` at :944 in the assumption loop :939–946; `add_theory_lemmas`
reuses `add_clause` :846–849 + :848), `smt/core/explanation.mli` (`Trivial`:18,
`Lia_bound`:20, `Lia_farkas`:21, `Lia_branch`:22; 34 lines), `smt/core/theory.mli`
(`Split`:34; 81 lines), `smt/theories/{euf/euf_adapter.ml` (propagate :146–151,
`explain`/`explain_implied` :154–168)`, lia/lia_adapter.ml` (Farkas conflict
`conflict_explanation` :80–85, `propagations`/`Lia_bound` :113–121)`}`,
`smt/solver/sat.mli` (`trace`/`on_learned` :87–98). ADRs: DESIGN §7/§8/§10/§12; ADR-0006
(D3 amended per R5); ADR-0005 D5/D7 + Freeze plan Tranche C; ADR-0010 §3.3a/§3.4; ADR-0003
(N2 gcd tightening).

---

## 0. What a certificate is, what it is about, and what it certifies

An `unsat` certificate is a self-contained artifact refuting the **unguarded active
CNF** of a query: (i) a **structural atom dictionary** giving every propositional
variable a version-stable meaning; (ii) the active, selector-stripped clauses as `Input`
intros; (iii) an ordered list of derivations terminating in the empty clause; (iv) a
`query_digest` binding it to exactly this query. The checker accepts iff it re-derives
`[||]`, every theory leaf's witness verifies, **and** the three-way digest equality holds
(§1). Independent of the solver.

**Two grades of trust [R5/P2].** *certifies-the-theorem* — the leaf proposition is
kernel-true (e.g. `omega` proves the premises unsat); sufficient for **soundness**.
*certifies-the-witness* — the emitted witness (multipliers, proof tree) is what is
checked; catches an emitter that produced a wrong-but-true witness. The OCaml checker
always certifies-the-witness; Lean does for EUF, and for LIA at the **target** (reflected
multiplier checker), with `omega` the **M5 bootstrap** (theorem only, OCaml the sole
witness-checker in the interim). Stated, not hidden.

**Scope boundary (unchanged).** The certificate is about the *clausified active CNF +
theory atoms* — matching today's post-normalization Lean-from-dump oracle (ADR-0003 N2).
Certificates **match, not widen** the TCB: preprocessing / Tseitin / div-mod elimination
/ internalization / **preprocess-time gcd tightening** stay trusted. Certificates remove
trust from **search** (SAT, EUF, LIA, combination; at M6, instantiation). Only `unsat` is
certified; `sat` stays self-certifying by model evaluation, so the M6 `Instance` step
appears only inside an `unsat` certificate.

---

## 1. Certificate format — self-contained, structural, query-bound

### 1.0 The active CNF: selectors stripped at emission [RR3 — ruled option (b); strip rule RRR4]
**The hole (source-verified).** `session.ml:198` stores every asserted clause as
`Sat.add_clause t.sat (Sat.neg sel :: List.map lit_of clause)`, i.e. `(¬sel ∨ clause)`
where `sel = current_selector t = List.hd t.frames` (`session.ml:154`); the `frames`
field is the selector stack, innermost first (`session.ml:65–66`). `session.ml:302`
builds `assumptions = List.map Sat.pos t.frames` and `session.ml:304` solves
`Sat.solve ~assumptions t.sat` — the active frame selectors are **solve-time
assumptions**. So the real refuted formula is `⋀ᵢ(selᵢ → clauseᵢ) ∧ {selⱼ=true : j active}`
— the `Unsat` is **conditional on the active-selector assumption set**, which is neither
an `Input` clause nor a level-0 `Unit`. The guarded CNF is trivially SAT with all
selectors false, so a cert embedding it **cannot derive `[||]`**. (This is DESIGN §7's
"each top-level assertion gets a selector literal; the final conflict over selectors is
the core" — the cert must turn that selector-conditioned core into an assumption-free
refutation.)

**Ruling (master): option (b) — translate the proof to the unguarded active CNF.** The
emission point dumps the **post-activation** clause set. **The strip rule, stated over the
clause's full selector-literal set [RRR4]:**
- A clause containing **ANY inactive-selector literal** is **DROPPED** — it is dormant for
  this solve (guarded by a selector assumed false / not assumed), so it is not part of the
  active problem.
- A clause whose selector literals are **all active** is **stripped** of them (each `¬selⱼ`
  removed) and **kept** as an `Input` (or, for a learned clause, resolved/emitted with the
  strip applied). The atom dictionary is stripped in lockstep.

This covers the **mixed active/inactive learned-clause case**: learned clauses persist
across `pop` (learnts are not frame-scoped), so a single learned clause can carry both an
active and an inactive selector literal — the any-inactive → drop rule handles it (such a
learnt is vacuous for the active problem and needs no emission).

**Determinism:** the strip is a total, order-preserving pass over the clause set in
emission order; the active-selector set is `t.frames` (a deterministic list). **Soundness
invariant (state it):** an active selector is assumed **true for the entire solve**, so
`¬selⱼ` is **false** throughout; deleting a false literal from a clause **weakens
nothing** under the assumption set — the stripped clause is entailed by the guarded
clause + `selⱼ`. A clause carrying an inactive `¬selₖ` is entailed with no active content
(its `selₖ` is not assumed), so dropping it removes nothing from the active problem.
**Fail-closed:** if the translation meets a selector literal that is neither a
known-active nor a known-inactive frame (e.g. a malformed frame state), it emits **no
guessed clause** → the query is **loud uncertified**, never a fabricated `Input`.

### 1.1 The AST — structural throughout [RR1], provenance-split [RR5]
```
(* smt/certificate/cert.ml — depends on `core` (for reading only) + checker-local bignum *)

(* [RR1] STRUCTURAL, version-stable encodings — never live Symbol.t/Sort.t (process-global
   hash-cons ids, per-binary). This is what makes the whole artifact replayable by a
   different binary. *)
type sort_enc   = Bool_s | Int_s | Uninterp_s of string          (* uninterpreted sort by NAME *)
type symbol_enc = { name : string; arg_sorts : sort_enc array; res_sort : sort_enc }
type term_enc =                                                  (* structural term tree *)
  | T_app  of symbol_enc * term_enc array                        (* incl. nullary = const/var *)
  | T_int  of string                                             (* arbitrary-precision numeral *)
  | T_le   of term_enc                                           (* Le-normal: arg <= 0 *)
  | T_eq   of term_enc * term_enc
  (* … the frozen Term node set, mirrored structurally; NO hash-cons tag anywhere *)

type atom_id   = int                         (* ARTIFACT-LOCAL index into [atoms] *)
type clause_id = int                         (* ARTIFACT-LOCAL, emission order *)
type lit = { atom : atom_id; positive : bool }   (* no live Lit.t in the artifact *)

type atom_denot =                            (* each propositional var's structural meaning *)
  | Atom_term of term_enc
  | Tseitin   of bool_comb                   (* clausifier proxy → defining boolean combination *)
and bool_comb = And of atom_id array | Or of atom_id array | Not of atom_id | Var of atom_id

type euf_proof =
  | Assumed of lit | Refl
  | Trans of euf_proof * euf_proof
  | Cong  of symbol_enc * euf_proof list     (* [RR1] structural symbol, NOT Symbol.t *)

type farkas_mult = Le_mult of Rational.t | Eq_mult of Rational.t   (* L6 sign rule *)

type theory_leaf =
  | Euf_chain   of euf_proof
  | Lia_farkas  of { concl : lit option; combo : (lit * farkas_mult) list }   (* None=conflict; Some p=propagation Γ⊢p [R3] *)
  | Valid_lemma of valid_kind                (* UNCONDITIONAL ℤ-tautology only; Split lemmas land here [R3/RR5] *)
  | Shared_eq   of { gamma : lit list; eq : lit }        (* explicit N-O clause ¬Γ ∨ (s=t) *)
  | Cut         of cut_witness               (* RESERVED; not emitted in v1/M5 *)
  | Instance    of instance_witness          (* M6; checker+honeypot at M5 *)
and valid_kind = Trichotomy of lit * lit * lit | Bb_split of lit * lit
and instance_witness = { lemma_id : int; subst : term_enc array }
and cut_witness = unit

type lemma_entry = { id : int; qvar_sorts : sort_enc array; body : term_enc }   (* [RR1] sort_enc *)

type intro =
  | Input      of { id : clause_id; clause : lit array }                 (* [RR5] ONLY genuine query clauses (active, stripped §1.0) *)
  | Unit       of { id : clause_id; lit : lit }                          (* standing level-0 unit; checker re-derives closure (§1.3) *)
  | Resolution of { id : clause_id; clause : lit array; hints : clause_id list }  (* hints ordered-RUP [rₙ..r₁; conflict]; M5 emits UNMINIMIZED clauses, so no minimization reasons [RRR2] *)
  | Theory     of { id : clause_id; clause : lit array; leaf : theory_leaf }      (* clause MAY carry a positive implied literal [R3] *)

type t =
  { version      : int                       (* guards the GRAMMAR, not atom-numbering *)
  ; query_digest : string                    (* [RR2] over the CLAUSIFIED ACTIVE CNF + structural dictionary *)
  ; atoms        : atom_denot array          (* the structural dictionary; every lit.atom indexes it *)
  ; lemmas       : lemma_entry list          (* [] for QF (M5); populated with the lemma tier (M6) *)
  ; intros       : intro list }              (* Input(active,stripped) … then derivations; last reaches [||] *)
```
**Structural-dictionary well-formedness [RR1]:** the reader validates the `atoms` array
and every `term_enc`/`bool_comb` as a well-formed DAG — indices in bounds, **acyclic**,
operands in **canonical order** (tag-free: sort then symbol name then arity then recursive
operand order), no dangling `atom_id`. A malformed dictionary → uncertified (fail-closed).

### 1.2 Query binding — three-way digest equality [RR2]
Rev 2's binding checked only `digest(replayed query CNF) == query_digest`; it **never**
checked `digest(embedded Inputs) == query_digest`, so an artifact could embed a proof of
CNF **A** while claiming `query_digest = digest(B)` and certify **B** — a false `unsat`
with **no hash collision**. **Fix — require the three-way equality:**
```
digest(embedded Input intros)  ==  query_digest  ==  digest(replayed active query CNF)
```
with **exact canonical-byte equality** (not just hash-equal) for the Lean bridge. **The
digest is over the CLAUSIFIED ACTIVE CNF + the structural proxy dictionary — NOT source
ASTs.** `canonical.ml:86` canonicalizes source-query ASTs; the cert operates one layer
down (post-clausification, post-selector-strip), so §2 defines a **new clausified-CNF
canonicalizer** over the structural dictionary + `Input` clause set (sorted clauses,
canonical atom numbering by structural order), reusing the netstring *scheme* only.

### 1.3 Level-0 units — the checker re-derives the closure [H3, stands]
`Input` = active stripped query clauses; `Unit { id; lit }` = a standing level-0 unit,
**no antecedents**. The checker **re-derives the level-0 unit closure by confluent
BCP-to-fixpoint over the `Input` clauses** — the same unit-propagation engine it runs for
RUP (standard for verified LRAT checkers, e.g. cake_lpr; not the LRAT-vs-DRAT "search",
which is for a learned clause's *order*). Re-derived, not trusted; cannot spuriously
reject (BCP confluence: checker closure ⊇ any declared unit given the same `Input`). This
is why `on_unit : id:int -> lit:lit -> unit` needs **no** level-0 forcing-clause
provenance (which the core drops, `sat.ml:47–56` — a level-0 unit's reason is `Decision`,
enqueued with no clause id at `sat.ml:770`). **This face of the freeze gate stands;
RR4 (below) is the rest of the contract.**

### 1.4 The `on_learned` / resolution contract — PINNED [RR4 + RRR2, the long pole]
Rev 2's "the `on_learned` antecedents *are* the LRAT hints at zero cost" is **false as it
stands** (source-verified). The freeze gate is this **contract**, not the signature type.

- **(a) Order [RRR2 — corrected].** `analyze` accumulates reason ids during the 1UIP
  resolution loop (`sat.ml:626` `ants := !c.id :: !ants`) and returns `List.rev !ants`
  (`sat.ml:679`), so the returned `antecedents` are `[confl; r₁; …; rₙ]` (**conflict
  first**, then reason clauses in resolution order). Ordered RUP needs each cited clause
  to be **unit at its turn**, which is the **opposite** order: the reason clauses in
  reverse-resolution order, conflict last. **Pinned — the correct ordered-RUP hint order
  is `[rₙ..r₁; conflict]`.** (Rev 3's §1.4(a) was self-contradictory — it demanded both
  "the reverse of `analyze`'s order" *and* "the conflict clause first"; those are
  incompatible, and an implementer following it builds the chain wrong and every cert
  false-rejects. This is the fix.) The emitter (or a thin transform at emission) presents
  `hints` in exactly `[rₙ..r₁; conflict]`. The checker does **ordered** RUP (no search).
- **(b) Emit the UNMINIMIZED 1UIP clause [RRR2 — option (b), adopted; replaces the Rev-3
  minimization delta].** Rev 3 tried to make the *minimized* learned clause certifiable by
  appending each self-subsumption reason id to `ants`. That is both incomplete (the dropped
  literals form a **transitive dependency chain** — dropping A via reason `rc_A` whose body
  mentions B, where B was itself dropped, needs `rc_B` sequenced **before** `rc_A`, i.e. a
  topological order, not "one `rc` per literal") and unnecessary. **Adopt instead: on the
  traced path, use the pre-minimization 1UIP clause.** Concretely — `analyze` fixes the
  asserting literal in slot 0 at `sat.ml:628` and only then runs the local minimization
  loop (`sat.ml:629–658`); capture `out` as of `:628` and, **when a trace is active, bypass
  the minimization loop so the emitted-and-stored learned clause is the unminimized one.**
  With no dropped literals, the accumulated `ants` (complete, from the full 1UIP resolution
  chain) is *exactly* the hint chain `[rₙ..r₁; conflict]` — no minimization-reason problem
  at all, and a simpler checker + soundness theorem.
  - **Why bypass, not just capture-and-emit:** the solver's stored clause is used to
    propagate downstream, and its id appears in *later* learned clauses' antecedents. If we
    emitted the unminimized clause under an id whose *stored* clause were still minimized,
    a downstream **ordered, hint-restricted** RUP step citing that id would need the
    minimized clause's dropped-literal reasons (level>0, not level-0, so checker BCP §1.3
    does not recover them) → stall. Storing **and** emitting the unminimized clause keeps
    every downstream antecedent chain over unminimized clauses, so hint-restricted RUP is
    clean throughout. An unminimized 1UIP clause is a valid asserting clause (minimization
    only shrinks it), so search stays sound, terminating, and deterministic on the traced
    binary; the backjump-level computation (`sat.ml:659–676`) runs on the same `out`.
  - **Cost:** larger learned clauses ⇒ larger certs (bounded: certs are streamed + size-
    capped, §2). Minimization is a **search-speed** optimization irrelevant to the proof;
    forgoing it *only when tracing* (trace is OFF by default, §2) is free for the
    uncertified corpus.
  - **Solved-set divergence (the traced config is a weaker solver).** Bypassing
    minimization slows the traced solve, so a **budget-marginal** query could time out
    *traced* that grind solved *untraced* — which would break A3's "superset of grind"
    comparison for a reason unrelated to emission. **Resolution (option (i)):** the
    certificate gate runs as a **CI job with a generous budget** (not a per-compile-latency
    path), sized so that minimization-off is **performance-only** and does not change the
    solved set. **Fallback (option (ii), documented):** if practice falsifies (i) — a query
    the untraced solver closes but the traced one cannot within budget — route that query's
    *traced* run to **uncertified** (a traced-config budget-timeout, isolated from the
    grind-comparison numerator), never to a disagreement or a soundness signal.
  - **Post-M5 size optimization (recorded, not adopted):** emit the *minimized* clause with
    its self-subsumption reason ids in **dependency (topological) order**. **Hazard:** it
    adds a topological-ordering emission pass and complicates the soundness theorem (the
    checker must consume the dependency-ordered minimization reasons); defer until cert
    size is shown to matter.
- **(c) Terminal `[||]` step — see §4.0.** Reaching the empty clause is **not** a single
  case: there are **four** distinct `Sat` `Unsat` exits (Rev 5 H2/H3), and only one emits
  an `on_learned`. §4.0 enumerates all four (E1 solve-entry filter-to-empty, E2 level-0
  conflict, E3 failed assumption, E4 Final-effort theory-lemma-empty) with a defined
  terminal step each; E3 (the universal production exit, incl. its `Theory_prop`
  materialization) is the one genuinely new emission mechanism. (Rev 3's §1.4(c) covered
  only E2; Rev 4 covered E1/E2/E3 but mis-stated E1 and missed E4.)

**Known limitation (RAT):** oxsmt learns 1UIP-RUP clauses only; RAT (from inprocessing it
does not do) is out of scope — documented, not a gap.

### 1.5 EUF / LIA / Valid_lemma / Shared_eq / Instance leaves
- **EUF** `Euf_chain` — structural proof tree (`Assumed|Refl|Trans|Cong`), replayed
  link-by-link into a fresh union-find. No existing N-version reference (`euf.ml`'s
  closure is set-based/flattened — H2), so this is new code and a leaf against the proven
  skeleton (§4). `Cong` now carries `symbol_enc` (RR1). Emission: `euf_adapter.ml:146–151`
  (`propagate`) and `:154–168` (`explain`/`explain_implied`) for implied-equality leaves; a
  new additive `explain_proof` accessor for conflicts.
- **LIA** `Lia_farkas { concl; combo }` — `concl=None` conflict (`Σ multᵢ·rowᵢ` cancels
  to a strictly positive constant); `concl=Some p` **propagation** `Γ⊢p` (combo over
  `Γ∪{¬p}` cancels; `Theory` clause is `¬Γ∨p`, a positive implied literal). L6 sign rule
  (`Le`→nonneg, `Eq`→free-sign). Emission: Farkas conflict `conflict_explanation`
  (`lia_adapter.ml:80–85`), propagation `propagations` (`lia_adapter.ml:113–121`).
  `Lia_bound` tag already
  frozen (`explanation.mli:20`) — format-side + additive off-seam accessor, no core
  unfreeze.
- **Valid_lemma** — **unconditional ℤ-tautology ONLY** (`Trichotomy`, `Bb_split`). OCaml
  rule: the clause is exactly the three/two atoms over the same `x,y`/`x,k` (structural,
  via the dictionary) and ℤ-exhaustive. **A Gomory/MIR cut (W6) is conditionally valid,
  NOT a tautology and NOT pure Farkas — never `Valid_lemma`; it needs the reserved `Cut`
  leaf, and until that exists cut-closed unsat is loud-uncertified.** v1 emits no cuts
  (DESIGN §6), so v1 is complete via split+Farkas+resolution (`2x=1`: split `x≤0∨x≥1`,
  each branch Farkas-closed). gcd tightening is **preprocess-time** (ADR-0003 N2), trusted
  bucket, not a search step (v1 does no dynamic gcd tightening — stated).
- **Shared_eq** `{ gamma; eq }` — explicit N-O clause `¬Γ∨(s=t)`.
- **Instance** `{ lemma_id; subst }` (M6) — `lemma_id` indexes `lemmas`; `subst` a
  positional `term_enc` vector. Certified at the **formula level** (`∀x̄.φ ⊢ φ[σ]`,
  capture-free substitution recomputed); clausification of `φ[σ]` rides the preprocessing
  TCB via `Input`, not re-certified. Lean `specialize`. Off-core, no `Rule_tag.Instance`.
  Format + checker + wrong-substitution honeypot at M5; emission at M6.
- **`Trivial`/`Lia_branch` tags** map to no leaf by design: `Trivial` facts add no
  constraint (dropped at emission); branching is the resolution skeleton, not a
  `Lia_branch` leaf.

---

## 2. Emission, serialization, cost

**Streaming at witness-time (ADR-0006 D2).** Emission points (cited): `on_learned`
(+ the RR4/RRR2 order + unminimized-clause contract, §1.4); `on_input`/`on_unit`
(§1.3/§4.0); the four terminal-`[||]` exits (§4.0); EUF `propagate`
(`euf_adapter.ml:146–151`) / `explain`+`explain_implied` (`euf_adapter.ml:154–168`); LIA
Farkas conflict (`lia_adapter.ml:80–85`) / propagation (`lia_adapter.ml:113–121`); engine
`Split` (`theory.mli:34`) → `Valid_lemma`; `Combine` seam → `Shared_eq`.

**`on_input` provenance split [RR5].** `add_theory_lemmas` (`sat.ml:846–849`) adds
`T_lemma` clauses — the theory `Split` lemmas (`cdclt.ml:199–202`, via `split_lit`
`cdclt.ml:176`) — through the **same `add_clause`** (`sat.ml:848`) as query inputs. An
`on_input` hook at that common path would record a split lemma as a **trusted `Input`**,
laundering a buggy split into an axiom. **Pinned fix:** `add_clause` gains an
`~origin:[ `Query | `Theory_lemma ]` tag (a `sat.ml` delta, §4.0). `on_input` fires
**only** for `` `Query ``; `` `Theory_lemma `` clauses route to a `Valid_lemma` `Theory`
intro (certified as a ℤ-tautology) and are **never** embedded as `Input` CNF.
Session/clausifier pass `` `Query ``; `add_theory_lemmas` passes `` `Theory_lemma ``.

**Serialization [R7].** Reuse the netstring *scheme*, not `canonical.ml` code (it is a
serializer only — `ser`, `:34–37`, in-memory `String.concat`). M5 builds a **streaming
serializer** and a **fail-closed reader** (parse error / unknown tag / version mismatch /
truncation / trailing bytes → uncertified) with **its own malformation honeypots**. The
clausified-CNF canonicalizer (§1.2) lives here.

**Cost — OFF by default** via `set_trace` (unset ⇒ ~zero corpus cost). ON for the
certified-`unsat` gate + debug builds. Streamed to disk, per-query size cap; on exceed →
**uncertified run + loud counter**, a first-class gate-accounting bucket.

---

## 3. Replay — soundness theorem, trust tiers, taxonomy, transition

### 3.1 Reflected checker + kernel-proved soundness theorem [R2/P3 — CRITICAL]
`decide (checkProof cnf proof)` proves nothing without a **once-proved kernel theorem**
```
checkProof cnf leaves proof = true → (∀ c ∈ leaves, ClauseValid c) → ¬ Satisfiable cnf
```
ranging over the intro kinds (`Input`/`Unit`/`Resolution`/`Theory`), **incl. the RR4
minimization/final-step contract**, with the **atom-index ↔ Lean-proposition bridge = the
RR1 structural dictionary** (one construction serves RR2 binding + P3 denotation).
Standing `Unit`s are justified by a **BCP-closure lemma** (a literal propagated to
fixpoint from `Input` is entailed by it), not trusted. Per-query goal: `(∧ leaves valid,
kernel-proved) ∧ (checkProof=true by decide) ∧ (three-way digest) → False`. A checker
that accepts a satisfiable CNF cannot *have* this proof (won't type-check) — that is what
audits the checker function (P3c), plus the N-version OCaml checker. **Scale honesty:**
kernel `decide` over ~32k clauses is not the steady state; `@Compiler` (`native_decide` +
`ofReduceBool`) is expected — keep the per-query `Kernel`/`Compiler` tier accounting;
STATUS reports the real distribution (drop "Kernel is common" until measured, open q2).

### 3.2 Leaf checkers, R5 bootstrap→target
Resolution skeleton → reflected checker + §3.1 theorem (step 1, LRAT/cake_lpr precedent).
EUF → explicit proof term. **LIA Farkas/Valid_lemma [R5 ratified]:** `omega` = **M5
bootstrap** (kernel proof term, certifies theorem); **reflected multiplier checker** =
**target** (clear denominators, check signs, kernel `Int` sum to positive constant by
`decide` + own soundness theorem; certifies the witness, closing P2's "omega ignores the
multipliers" gap). Amends ADR-0006 D3 via labeled erratum at promotion. Instance →
`specialize`.

### 3.3 Replay-outcome taxonomy — the invalid-certificate bucket [RR6/R6/P4 + RRR3]
The checker cannot adjudicate sat/unsat a priori, so an **artifact that fails for any
reason attributable to the certificate itself** (a wrong cert of an actually-`unsat`
query — exactly the A1 honeypots — but also a truncated/unreadable one) must have a home
that is **not** a soundness alarm and **not** an infra alarm. **`invalid-certificate` is
redefined [RRR3] to cover EVERY artifact-attributable rejection**, at any stage of the
pipeline:

| bucket | meaning | routing |
|---|---|---|
| **certified** | checker green + leaves valid + three-way digest | pass (`@Kernel`/`@Compiler` tier) |
| **invalid-certificate** | **any artifact-attributable rejection** — (i) **reader-reject**: parse error / unknown tag / version mismatch / truncation / trailing bytes / malformed dictionary (§1.1), i.e. fails at the *reader* before any check; (ii) **digest mismatch** (RR2 three-way binding fails); (iii) **well-formed-but-fails-check** (leaf/RUP fails — the A1 honeypots). Emitter, serializer, or checker bug. | **loud, NOT a soundness alarm**; phase-blocking; fix emitter/serializer/checker |
| **rejected-as-unsound** | identified as certifying an actually-`sat` query **only** via the honeypot's known-`sat` construction or an independent `sat` witness — never from the checker alone | **soundness alarm, ship-stopping** |
| **checker/infra-error** | Lean elaboration/encoder error, OOM, replay-infra bug (`Elab_error`, not conflated with a wrong cert) | loud, not a soundness alarm |
| **unsupported-rule** | well-formed leaf this checker version can't check | **uncertified** (coverage gap) |
| **timeout/killed** | resource exhaustion | tier-escalate-or-finding |

**Why the widening [RRR3].** Rev 3 defined `invalid-certificate` as a "**well-formed** cert
that FAILS its check," but §4.1 also routes truncated/malformed/unreadable artifacts there
— which are **not** well-formed and fail at the *reader*, before any check runs. That was
self-inconsistent: the reader-level malformations had no well-defined bucket. The three
sub-cases above make every §4.1 honeypot's landing bucket well-defined. The distinctions
that matter are preserved: `invalid-certificate` (artifact's fault, loud, phase-blocking)
vs `rejected-as-unsound` (a genuine soundness alarm, only via a known-`sat` construction or
independent witness) vs `checker/infra-error` (Lean OOM / encoder bug, the current
`Elab_error` vs `Tactic_failed` distinction the gate keeps).

**A1 honeypots land in `invalid-certificate`** (they mutate a valid cert of an *unsat*
query, so they are not "unsound" in the sat sense). **Disagreement triage:** OCaml vs Lean
are N-version — a cert OCaml accepts but Lean rejects localizes to Lean/encoding;
both-reject localizes to the emitter. The correlated-blind-spot residual is defeated only
by §3.1's kernel theorem (a wrong cert about a *satisfiable* CNF cannot yield a kernel
proof of False — leaves won't discharge / digest won't match), so a false certification
requires a bug in the tiny audited TCB, not in search.

### 3.4 Transition + marker-risk retirement
Phase A (shadow) → B (replay-first) → C (grind retired from `unsat`). **Phase A exit
requires BOTH** (a) replay certifies a superset of grind's `unsat` with zero
disagreements **AND (b)** the wrong-cert honeypots run **end-to-end through the LIVE
pipeline** (strip → canonicalize → serialize → read → both checkers → digest → classifier)
and all land in `invalid-certificate` (§3.3) — not offline. Marker-risk retired when the
`unsat` path reads only structural outcome (no `substring_mem` over Lean stderr;
`tactic_gaveup_markers` survive only on the grind `sat`/refutation path), `gate selftest`
asserts it.

---

## 4. Staging — resolution-first [R4], the sat.ml deltas, freeze

### 4.0 The accepted `sat.ml` / session behavior deltas + the terminal-exit enumeration (freeze prerequisite (ii)) [RRR1/RRR2/RR5/RR3 + Rev-5 H1/H2/H3]

**The four `Sat` `Unsat` exits — each must reach `[||]` [RRR1 + Rev-5 H2/H3].** The format
must emit a terminal empty clause on **whichever exit fires**. Source-verified, `Sat.solve`
has **exactly four** `Unsat` exits (Rev 4 enumerated three and mis-stated E1; the codex
final leg found E1's real trigger (H2) and a fourth route (H3)):

- **E1 — `t.ok` false at solve entry** (`sat.ml:1015` `if not t.ok then Unsat`). **H2
  correction:** `t.ok` does **not** require an *embedded* empty clause. It goes false in
  `add_clause` (`sat.ml:767–769`) when level-0 simplification **filters an added clause down
  to `[]`** — `ls = List.filter (fun l -> lit_val t l <> -1) ls` drops already-false level-0
  literals, and `| [] -> t.ok <- false`. So two **nonempty** `Input`s (`[a]` enqueues `a` as
  a level-0 unit; then `[¬a]` filters to `[]`) trip it with **no `[||]` among the Inputs**.
  **Terminal step (corrected):** E1 emits an explicit `Resolution { clause = [||]; hints =
  [id of the clause that filtered to `[]`, then its level-0 unit reasons] }` — `[||]` follows
  by RUP of that clause against the checker's level-0 BCP closure (§1.3), *not* an `Input`.
- **E2 — level-0 conflict in search** (`sat.ml:912–915`: `if decision_level t = 0 then
  (t.ok <- false; result := Some R_unsat)`, no `analyze`). **Terminal step:** the conflict
  clause `confl` is falsified by the level-0 assignment, so a final
  `Resolution { clause = [||]; hints = [confl.id] }` follows by RUP of `confl` against the
  checker's level-0 BCP closure (§1.3). (E2 also absorbs the `propagate_theory` T_lemma-to-
  empty path, which funnels a `transient_clause [||]` through `handle_confl`, `sat.ml:880`.)
- **E3 — failed assumption** (`sat.ml:939–946`: when an active selector assumption `pa`
  is found false, `lit_val t pa = -1` at `:943` → `t.failed <- analyze_final t (neg_lit
  pa)` at `:944`; `result := Some R_unsat`). **This is the production/session exit**, and
  as written it emits **no `on_learned` and no `[||]` step**; `analyze_final`
  (`sat.ml:684–723`) is the site the terminal step must hook. (Mechanism below, incl. the
  H1 Theory_prop fix.)
- **E4 — Final-effort theory lemma empty at level 0 [Rev-5 H3, NEW]** (`sat.ml:980–984`:
  at a full Boolean model the Final check returns `T_lemma clauses`; `add_theory_lemmas`
  (`sat.ml:846–849`) unwinds to level 0 and re-adds them via `add_clause`; a lemma all of
  whose literals are already false at level 0 filters to `[]`, `t.ok <- false`, and the
  Final-check site sets `result := Some R_unsat` **directly** — no `confl`, no `analyze`,
  distinct from E2). The empty clause came from a `T_lemma` (`origin = `Theory_lemma``,
  RR5), so it **cannot** be laundered as an empty `Input`. **Terminal step:** emit the
  offending `T_lemma` as its **`Valid_lemma` `Theory` intro** (certified as a ℤ-tautology /
  split disjunction, §1.5) and a final `Resolution { clause = [||]; hints = [that Theory
  intro's id, then its level-0 unit reasons] }` — `[||]` by RUP against the level-0 closure.

**Production reality (the reviewers proved this — state it plainly).** Under session
selector-guarding, `session.ml:198` prepends `¬sel` to **every** asserted clause, so:
- even `(assert false)` clausifies-and-guards to `add_clause [¬sel]` — a **unit**, not the
  empty clause — so `t.ok` never goes false on the session path and **E1 essentially never
  fires** for a session solve;
- guarded clauses `(¬sel ∨ …)` are never unit at level 0 (the selector assumptions are
  applied as decisions at level ≥1 inside `search`, `sat.ml:939–988`, not as level-0
  units), so a level-0 conflict cannot arise from them and **E2 essentially never fires**
  for a session solve.
- **E3 is the UNIVERSAL production exit.** Every session `Unsat` returns through the
  failed-assumption path: BCP over the guarded active clauses forces some active selector
  `selⱼ` false, and the assumption loop reports it via `analyze_final`.
E1/E2 remain reachable for raw-`Sat`/degenerate use (a client calling `Sat` directly with
unguarded clauses); **E4 is reachable on the theory session path** (a split lemma refuted
by the level-0 closure at a full model) but is subsumed under E3 whenever the empty lemma
also forces a selector false — still, the emitter must not assume so. The emitter lives at
the `Sat` level and **must handle all four**.

**The E3 mechanism — the RR3-strip / terminal-step seam (the one genuinely new
mechanism).** At the `analyze_final` site (`sat.ml:944`) the solver has a failed
assumption `selⱼ`: BCP forced `¬selⱼ` true. `analyze_final` (`sat.ml:684–723`) already
walks the trail back from `¬selⱼ` through the `Implied_by` reason clauses that propagated
it, collecting the assumption core. The certificate hook at that site translates **that
same forcing derivation** into a terminal step:
1. Take the forcing chain that derived `¬selⱼ` (the trail walk `analyze_final` performs),
   accumulating a reason id per crossed literal in RUP-consumption order `[rₙ..r₁]` — the
   E3 delta adds this accumulation at the `analyze_final` site, mirroring `analyze`'s
   `sat.ml:626`. (`analyze_final` currently accumulates no ids; this is the new emission.)
   The chain crosses **two** reason kinds, and both must be materialized (H1, below):
   `Implied_by cc` → `cc.id`; `Theory_prop` → the materialized theory reason clause's id.
2. Apply the **§1.0 selector strip**: every active selector's `¬sel` literal is removed
   from the reason clauses and from the derived resolvent. Because each active `selⱼ` is
   assumed **true throughout the solve**, its `¬selⱼ` literal is **false throughout**;
   stripping it weakens nothing.
3. After the strip, the forcing chain that derived `¬selⱼ` derives, over the
   selector-stripped active CNF, a clause with **no remaining literals** — the empty
   clause. The hook emits a terminal `Resolution { clause = [||]; hints = [rₙ..r₁] }`
   (checked by ordered RUP against the stripped `Input` clauses and the theory intros of
   step 1's `Theory_prop` reasons).

**E3 must materialize `Theory_prop` reasons, not only `Implied_by` [Rev-5 H1 — the
load-bearing fold].** `analyze_final`'s trail walk recurses through **theory-propagated**
literals too: its `Theory_prop` branch (`sat.ml:711–719`) calls `theory_explain_checked`
and marks the premises, but — **unlike** `analyze`, which at `sat.ml:624` materializes the
lazy reason via `theory_reason_clause t pl` and ids it at `:626` — `analyze_final`
materializes **no reason clause and no id**. So a naive E3 hook that accumulated only
`Implied_by` ids would **omit exactly the theory-propagation ancestors that dominate the
EUF/LIA production path**, and a genuine theory `unsat` session would be **uncertifiable**
(the hint chain would skip the steps that actually forced `¬selⱼ`). **Fix:** the E3 hook
materializes each crossed `Theory_prop` literal's reason with the **same
`theory_reason_clause` (`sat.ml:537–542`)** `analyze` uses — the clause `[p ∨ ¬p₁ ∨ … ∨
¬pₖ]` over its validated premises (CONTRACT-EX) — emits it as a **`Theory` intro** (the
EUF/LIA propagation leaf `¬Γ ∨ p`, §1.5), and places that intro's id in the terminal hints
in **propagation order**. This is precisely the parallel of `analyze`'s :624 materialization,
lifted to the assumption-core path; it is the second half of the E3 emission delta.

**E3 hints never cite a strip-DROPPED clause (load-bearing soundness).** A strip-DROPPED
clause (§1.0/RRR4) carries an inactive selector, is **dormant for this solve**, and was
**never on the trail** — hence no `Implied_by` reason in the `analyze_final` forcing chain
can reference one. The E3 hints are exactly the **active** reason clauses that propagated
`¬selⱼ`, each of which is kept (all-active → stripped-and-kept), not dropped. So the
terminal derivation cites only clauses present as stripped `Input`/`Resolution` intros,
and ordered RUP over the emitted CNF is well-defined.

**E3 fail-closed guard (cheap hardening).** The resolvent reached after the strip must be
literally empty — i.e. every literal `analyze_final` collected into the core is an
**active selector**. The hook **asserts** the core is all-selectors; if any non-selector
literal survives the strip (a malformed frame state, or an unexpected assumption reaching
`Sat.solve` beyond `List.map Sat.pos t.frames`), the derived clause is **not** `[||]` and
the hook emits **no guessed terminal step** → the query is **loud uncertified**, never a
fabricated empty clause. Same fail-closed discipline as the §1.0 strip.
This is exactly where RR3-strip (§1.0) and the RR4c terminal step meet: the selector whose
forcing derivation is a *near-empty* resolvent `[¬selⱼ]` becomes a genuine `[||]` once
the assumed-true selector literal is stripped. (DESIGN §7: "the final conflict over
selectors is the core" — the cert turns that conditional core into an unconditional `[||]`.)

**The accepted `sat.ml`/session behavior deltas.** These are step-1 implementation and
must land + survive the source-path re-review before the Tranche-C signature freeze:
1. **Unminimized learned-clause emission [RRR2b — replaces the Rev-3 minimization delta].**
   When a trace is active, capture the 1UIP clause at `sat.ml:628` (before the minimization
   loop) and **bypass the minimization loop (`sat.ml:629–658`)** so the emitted-and-stored
   learned clause is the unminimized one; `on_learned` fires with it and with the complete,
   correctly-ordered `ants`. **This deletes the Rev-3 `sat.ml:629` minimization-reason-id
   delta** (net simplification). (§1.4(b).)
2. **Terminal `[||]` step for each of the FOUR `Unsat` exits [RRR1 + Rev-5 H2/H3].**
   E1 → an explicit `Resolution{[||]}` by level-0 RUP of the clause that filtered to `[]`
   (`sat.ml:767–769`), **not** an `Input` (H2 correction); E2 → final
   `Resolution{[||]; hints=[confl.id]}` at the level-0-conflict site (`sat.ml:912–915`);
   **E3 → the `analyze_final` hook (`sat.ml:684–723`/`:944`) translating the
   assumption-forcing derivation to a stripped `[||]`, materializing BOTH `Implied_by` and
   `Theory_prop` reasons** (H1; mechanism above), **with the E3 fail-closed guard** (assert
   the core is all-selectors; loud uncertified otherwise); E4 → the offending `T_lemma` as
   a `Valid_lemma` `Theory` intro + a final level-0-RUP `Resolution{[||]}` at the
   Final-effort site (`sat.ml:980–984`) (H3, NEW). Add `analyze_final`, the solve-entry
   check, and the Final-effort T_lemma site to the delta list; E3 (with its Theory_prop
   materialization) is the longest pole.
3. **`add_clause ~origin`** — provenance tag (`sat.ml:756`/threaded to `:848`) so
   `on_input` fires only for `` `Query ``; `T_lemma`/`Split` clauses route to `Valid_lemma`
   (RR5).
4. **Selector-strip at emission** — `session.ml` dumps the unguarded active CNF (RR3 §1.0;
   drop-if-any-inactive / strip-if-all-active, RRR4); a session/emitter pass, not a
   `sat.ml` change.
Plus the trace signatures: `on_input : id:int -> clause:lit array -> origin:[`Query|`Theory_lemma] -> unit`,
`on_unit : id:int -> lit:lit -> unit`, existing `on_learned` (`sat.mli:87–98`; contract
pinned §1.4; learned units fire it). **Freeze prerequisite (ii) = this contract text
pinned in the ADR AND surviving the source-path re-review** (the signature types alone are
insufficient — cert-reviewer's concession is on record).

### 4.1 Staging (resolution-first)
1. **Step 1 — resolution vertical (freeze/serialization point):** freeze the AST (§1.1,
   structural throughout); build the streaming serializer + fail-closed reader + its
   honeypots; the clausified-CNF canonicalizer + three-way digest (§1.2); the selector-
   strip pass (§1.0); the OCaml resolution checker (with level-0 BCP, §1.3); **state +
   kernel-prove the §3.1 soundness theorem**; land the §4.0 engine deltas **incl. the E3
   `analyze_final` terminal-step hook with `Theory_prop` materialization (RRR1 + Rev-5 H1),
   the E1 filter-to-empty and E4 Final-effort-lemma terminal steps (Rev-5 H2/H3), and
   unminimized-clause emission (RRR2b)**. Validate vs hand-built certs + LRAT precedent.
   Honeypots — all land in `invalid-certificate` (§3.3), driven through the **live
   pipeline**, spanning its three sub-cases (RRR3): reader-reject (truncated skeleton,
   malformed dictionary (RR1)), digest mismatch (**mis-bound `query_digest` (RR2)**,
   **embedded-Inputs≠digest (RR2)**), and well-formed-but-fails-check (permuted multipliers,
   broken `Trans`/`Cong`, hint citing a non-unit, wrong-sign eq multiplier, non-exhaustive
   `Trichotomy`, **unstripped-selector / active-CNF mismatch (RR3)**,
   **split-lemma-as-Input (RR5)**, wrong-substitution `Instance`, **a missing terminal
   `[||]` step on any of E1–E4 / no-empty-clause skeleton (RRR1 + H2/H3 regression)**, **an
   E3 hint chain dropping a `Theory_prop` ancestor so a theory-`unsat` session stalls (H1
   regression)**, **a minimized learned clause slipping past the trace bypass so its hints
   stall (RRR2b regression)**).
2. **Step 2 — leaves against the proven skeleton:** EUF first leaf (new tree checker) ∥
   LIA Farkas+propagation ∥ Valid_lemma ∥ Shared_eq.
3. **Step 3 — Lean replay (master-only):** reflected resolution checker + soundness
   theorem ∥ EUF term emitter ∥ LIA `omega` bootstrap → reflected multiplier target.
   Drives Phase A→C.

---

## 5. Frozen-surface, open questions, acceptance

### Frozen-surface impact
| module | frozen? | impact |
|---|---|---|
| `core/*.mli` (explanation/theory/atom/lit/term/model/sort/symbol) | **FROZEN** | **NONE.** Cert uses structural encodings, not live ids (RR1); `Lia_bound`(:20) already present; `Rule_tag` payload-free; no `Rule_tag.Instance`. |
| `solver/sat.mli` | not frozen; Tranche C | `+ on_input(~origin)`, `+ on_unit`, `on_learned` **contract pinned** (§1.4). **Engine deltas (§4.0)** — this is prerequisite (ii); pinnable + must survive source-path re-review. |
| `interface/session.ml`, `cdclt.ml` | not frozen | selector-strip emission (§1.0) + `~origin` threading (§4.0/RR5). Behavior additions, no `.mli` freeze impact. |
| `theories/{euf,lia}` (pre-adapter) | — | `+ explain_proof` (EUF); emitter-side signed-multiplier/propagation translation (LIA). Additive. |
| `certificate/` (new) | new | AST + structural encodings + streaming serde + fail-closed reader + clausified canonicalizer + OCaml checker + bignum; `core`-read-only; ≤~600 lines target (reader + dictionary + canonicalizer push toward it — watch, may warrant a split). |

### Open questions
1. Cert persistence in the cache (leaning: store trimmed cert).
2. `native_decide` threshold **+** honest `@Compiler`-fraction measurement (RR4/R2 scale).
3. ~~Farkas overflow~~ resolved (checker-local bignum); confirm bignum vs `omega` sign agreement on large multipliers.
4. ~~`Shared_eq` shape~~ pinned as explicit `¬Γ∨(s=t)`; confirm vs M4 `Combine`.
5. Trimming aggressiveness.
6. ~~`Instance` interface~~ CLOSED (lemma-ADR §L6/O6).
7. ~~decide-timeout fallthrough~~ SPLIT OUT (board #86).
8. Emission-order determinism a *proven* function of input (total tie-breaks; no
   hash-table/pivot/thread order leak) — A6 is a test, not a guarantee; audit before claiming.
9. `Cut` leaf shape — W6; cut-closed unsat loud-uncertified until then. Confirm no dynamic
   search-time gcd tightening (v1: none).
10. **[RR3]** Selector-strip completeness: the active-selector set is exactly `t.frames`
    and the only assumptions reaching `Sat.solve` are `List.map Sat.pos t.frames`
    (`session.ml:302–304`) — **confirmed at source this revision**. Re-pin against
    `session.ml` at the emitter's implementation each time the session path changes, so the
    strip can never miss a live guard.

### Acceptance (M5)
- **A1 (checker audited, end-to-end).** All §4.1 honeypots land in `invalid-certificate`
  via the **live pipeline** (incl. selector-strip, three-way digest, provenance-split,
  malformed-dictionary), not offline.
- **A2 (emit+check on real solves).** Every CI `unsat` emits all five QF leaves; OCaml
  checker passes; seeded emitter mutants (incl. a missing terminal `[||]` on any of E1–E4,
  an E3 hint chain dropping a `Theory_prop` ancestor, a minimized clause slipping past the
  trace bypass, a split-lemma-as-Input) caught. Coverage note: E3 (with `Theory_prop`
  materialization) is the production exit, but the A2 corpus must include cases that fire
  E1/E2/E4 (raw-`Sat` degenerate inputs, level-0 theory conflicts, a split refuted at level
  0) so every exit's terminal step is exercised.
- **A3 (Lean Phase C).** Replay ⊇ grind's `unsat`, zero disagreements, live rejection
  honeypots red; grind retired from `unsat`; tier distribution honest in STATUS. The
  cert gate runs at a generous CI budget so the minimization-off traced config is
  performance-only (§1.4(b)); any traced-config budget-timeout is booked `uncertified`,
  not a disagreement, so the "superset of grind" comparison is not confounded by the trace.
- **A4 (marker risk retired).** No `substring_mem`-over-stderr on the `unsat` path; `gate
  selftest` asserts.
- **A5 (accounting closes) [Rev-5 M1].** Every `unsat` maps to exactly one accounting
  bucket and the sum identity closes: `{certificate-checked (`@Kernel`/`@Compiler`),
  uncertified, invalid-certificate, rejected-as-unsound}`, where **`uncertified` is the
  roll-up of every non-certified, non-invalid, non-unsound outcome** — the §3.3
  `unsupported-rule`, `checker/infra-error`, **and** `timeout/killed` sub-buckets all book
  as `uncertified` for the identity (their §3.3 distinctions are kept for triage but do not
  open a hole in the count). No `unsat` can fall outside the four accounting buckets.
- **A6 (determinism — a TEST, R1).** Byte-identical across two runs, **same binary**;
  cross-version stability from structural encoding + three-way digest, not byte-equality;
  emission order audited (open q8).

---

## Firm vs provisional (RR round-2 + RRR round-3 items)
| # | Item | Status |
|---|---|---|
| RRR1 + Rev-5 H1/H2/H3 | §4.0 enumerates all **four** `Unsat` exits (E1 filter-to-empty, E2 level-0 conflict, E3 failed assumption, E4 Final-effort theory-lemma-empty), each with a terminal `[||]`; E3 (`analyze_final` → stripped `[||]`, materializing both `Implied_by` and `Theory_prop` reasons) is the universal production exit and the one new mechanism | **FIRM** (HIGH; the long pole; §4.0) |
| RRR2 | Hint order `[rₙ..r₁; conflict]`; **unminimized-clause emission** (option (b)) — the Rev-3 minimization-reason-id delta is deleted; dep-ordered minimized emission is a post-M5 opt | **FIRM** (HIGH; §1.4/§4.0) |
| RRR3 | `invalid-certificate` redefined = every artifact-attributable rejection (reader-reject ∪ digest-mismatch ∪ well-formed-but-fails-check); §4.1 routing reconciled | **FIRM** (HIGH; §3.3) |
| RRR4 | Strip rule: any-inactive-selector-literal → DROP; all-active → strip and keep | **FIRM** (MEDIUM; §1.0) |
| RR1 | Structural `symbol_enc`/`sort_enc`/`term_enc` throughout (no live ids); proxy-DAG bounds/acyclicity/canonical-order validation | **FIRM** (CRITICAL, cleared ROUND 3) |
| RR2 | Three-way digest equality; digest over the clausified active CNF + dictionary, not source ASTs | **FIRM** (CRITICAL, cleared ROUND 3) |
| RR3 | Selectors stripped at emission → unguarded active CNF; option (b) ruled; fail-closed on unstrippable | **FIRM** (CRITICAL, master-ruled) |
| RR4 | `on_learned` contract pinned: RUP-order hints (`[rₙ..r₁; conflict]`, RRR2), unminimized emission (RRR2 supersedes the Rev-3 minimization delta), terminal `[||]` step (→ RRR1's three-exit enumeration) | **FIRM** (HIGH; §1.4/§4.0) |
| RR5 | `on_input ~origin` provenance split — query→`Input`, `T_lemma`/`Split`→`Valid_lemma`, never Input | **FIRM** (HIGH) |
| RR6 | `invalid-certificate` bucket added (RRR3 widens its definition); A1 honeypots land in it | **FIRM** (HIGH) |
| R5 | `omega` M5 bootstrap → reflected multiplier target; D3 erratum at promotion | RATIFIED |
| R4 | Resolution-first staging; EUF first leaf | FIRM (ruled) |
| H3 | Level-0 closure re-derived by checker BCP (`on_unit` needs no provenance) | FIRM (stands, scoped) |
| §4.0 | Accepted `sat.ml`/session behavior deltas = freeze prerequisite (ii) | **FIRM** |

---

## Appendix — D3 erratum wording, FOR ADR-0006 AT PROMOTION TIME (R5, master-ratified)

*Not part of this design draft's body. This is the ready-to-commit erratum block for
`decisions/adr-0006-certificates.md`, to be appended (as the next Revision) by the
integrator when this ADR is promoted — house erratum style, per the L6 (Rev 4) and
DESIGN A2 precedent. Master ruling on record (R5): adopt as a **staged** amendment.*

---

**Revision 5 (2026-07-11) — D3 LIA-leaf-checker erratum (staged amendment; adds a
ratified target, no frozen-type change; master-approved per reconciliation R5):**
Decision 3 named Lean core **`omega` the FIRM default LIA leaf checker**. A
cross-model finding that post-dates D3 (codex Rev-2 P2) shows `omega` validates **the
theorem, not the certificate's witness**: `omega` re-derives its own refutation and
**ignores the emitted Farkas multipliers**, so (a) an emitter that produced *wrong*
multipliers is **masked in Lean** whenever the leaf goal is independently `omega`-true
(the multiplier error is then caught only by the N-version OCaml checker), and (b)
`omega`-on-a-leaf is Lean *re-searching*, in tension with D3's own thesis that Lean
"stops searching (`grind`) and merely checks." D3 was ratified before this insight
existed; this is the correction mechanism working, not decision churn.

Amendment — **staged, both parts ratified:**
- **`omega` remains the M5 BOOTSTRAP** LIA-leaf checker. D3's operational content is
  intact for M5: `omega` is Lean *core* (no Mathlib), emits a **kernel-checked** proof
  term (no extra axiom, unlike `native_decide`), is complete and deterministic, and —
  unlike `grind` — never gives up. LIA replay works day one and is **kernel-sound on
  the theorem**. In the bootstrap interim the OCaml checker is the sole *witness*
  checker for LIA; this is stated, not hidden.
- **A reflected multiplier checker is the ratified TARGET.** Represent the
  `Le`-normal rows + the emitted `(lit, farkas_mult)` combo as **closed data**; a Lean
  *function* clears denominators, checks each multiplier's sign (`Le`→nonnegative,
  `Eq`→free per the L6 rule), sums the scaled rows by **kernel `Int` arithmetic**, and
  checks the result cancels every variable to a **strictly positive constant**;
  discharge `combineAndCheck rows combo = true` by `decide`, backed by a **once-proved
  soundness theorem** `combineAndCheck rows combo = true → (∧ rowsᵢ with signs) → False`.
  This **certifies the witness** (the emitted multipliers), closing (a), and is a
  *check* not a *search*, closing (b). It is the **same reflected-checker + once-proved-
  soundness-theorem discipline** as the M5 resolution skeleton (this ADR's R2/§3.1), so
  the Lean side unifies around one construction.

**Distinct from the route ADR-0006 already rejected.** The earlier "explicit-multiplier"
Farkas route was rejected because it still called `omega` for the `Σ = k` algebraic
identity (so it added trust rather than removing it). The target here is a **fully
reflected, `omega`-free** computation over closed integer data discharged by `decide` —
the ring-free normalizer that route lacked, now realized as `combineAndCheck`.

**No frozen-type change.** The `Lia_farkas` leaf shape (`{ concl; combo }`, signed
`farkas_mult`) is unchanged; both the bootstrap and target consume the identical leaf.
`Explanation`/`Rule_tag`/`THEORY` seam unchanged and still frozen. The trust-grade
distinction this erratum turns on — **certifies-the-theorem** (`omega`, bootstrap) vs
**certifies-the-witness** (reflected checker, target) — is recorded in the trust story.

**M5 acceptance criterion (added by this erratum):** the LIA leaf replays under `omega`
at M5 (kernel-checked); the reflected multiplier checker + its soundness theorem is a
tracked post-M5 target whose landing flips the LIA leaf from theorem-trust to
witness-trust in Lean, and until it lands STATUS records that the LIA multiplier check
is OCaml-only. (Rejected alternative, on record: making the reflected checker a
*blocking* M5 deliverable — rejected because `omega` already gives kernel soundness at
M5, so blocking M5 on the reflected checker trades no soundness for schedule risk.)
