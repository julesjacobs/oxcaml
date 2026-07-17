# Flush-testing is complete for stateless PIFO-tree schedulers

**A theorem and its proof.** For stateless packet schedulers over PIFO trees of arbitrary finite
topology, agreement on all *flush* words (push a batch, then drain) implies agreement on *all*
interleaved words. The two black-box test regimes coincide.

*Version v2 — definitive document. The proof of §§1–4 is the elegant synthesis proof
(`pifo-elegant-proof-report.md`, sha256 `8f236381…`), reproduced as the primary text with the
referee-verified expansions folded in at their sites: **four bracketed parenthetical additions** (the
timestamp-scoping clarification §1; the degenerate-cycle remark §3; the restriction fact §4; the (18)
instantiation §4) and **one expanded paragraph** (the substitution coupling §4, whose original
one-sentence proof is replaced by the referee-supplied coupling invariant). The four brackets preserve
the original text verbatim; the substitution paragraph is a disclosed replacement, verified correct by
the Fable delta review. The expansion sites are from the Fable delta referee (`pifo-referee-elegant.md`
§D) and the codex second signature (§1 timestamp scope). Front matter (§0) and the falsified-routes
appendix (§A) are added for the Cornell-facing edition; they surround, and do not alter, the proof.*

---

## 0. Front matter

### 0.1 The theorem

The model, the two equivalences, and the statement are fixed by a frozen, dual-reviewed Lean
definitions file, `PifoStatement.lean` (sha256 `fe9475eb…`), which contains only definitions — no
theorems, no `sorry`. The claim is its final definition, `statelessFlushImpliesInterleaved`:

> **Theorem.** For every finite packet alphabet `Fin k` and every two *valid stateless* schedulers
> `S₁, S₂` over PIFO trees (arbitrary, possibly different topologies): if `S₁` and `S₂` produce
> identical outputs on every flush word, then they produce identical outputs on every word of
> operations (pushes and pops arbitrarily interleaved). The converse is trivial, flush words being
> operation words.

Here a **flush word** of `w ∈ A*` pushes the letters of `w` in order and then pops `|w|` times;
**flush-equivalence** is identity of outputs on all flush words; **interleaved-equivalence** is
identity of outputs on all operation words, with over-popping observable as a `none` emitted
identically whenever both trees are simultaneously packet-free.

### 0.2 Model recap (paper Defs 3.1–3.7; flush: Defs 4.3–4.4; Mohan/Liu/Foster/Kappé/Kozen, OOPSLA'23, arXiv 2211.11659)

A **PIFO** is a priority queue whose `pop` removes the lowest-`rank` entry, ties broken FIFO by
earliest arrival. A **PIFO tree** holds a PIFO at every node: leaf PIFOs hold packets, internal-node
PIFOs hold ranked child indices. A **push** walks one root-to-leaf path, enqueuing one entry
(child-index/rank, or packet/rank) at each node on the walk; every entry also carries a global
arrival index, so FIFO tie-breaking is unambiguous (within any one queue all arrival indices are
distinct). A **pop** pops the root PIFO to get a child index, recurses into that child, and continues
to a leaf, which emits the observable packet; pop is **atomic** (it either fully succeeds or leaves
the state unchanged and emits `none`, an observable definedness bit). A scheduler is **stateless**:
every push of type `i` uses the *same* fixed annotated path `assign i` — no state, no dependence on
arrival number. The observable of a pop is the emitted packet's *type*; for stateless schedulers this
carries the same information as its identity, since same-type packets share a path and hence tie on
rank in every queue, so FIFO releases them in arrival order.

### 0.3 The boundary: statelessness is necessary

The theorem is sharp. For **stateful** schedulers the two equivalences genuinely differ:
flush-equivalence does *not* imply interleaved-equivalence. A stateful scheduler may give two
same-type packets different paths or ranks, letting them overtake one another, so the type-level and
identity-level observables come apart and a batch drain can hide a distinction that an interleaving
exposes. A concrete counterexample is machine-checked in Lean (`pifo-lean/Pifo.lean`, with
`Extended.lean`): two stateful schedulers, flush-equivalent yet interleaved-distinguishable, with
outputs recorded as arrival numbers to make the overtaking visible. Statelessness is thus exactly the
hypothesis under which flush-testing is complete — which is what makes the theorem worth stating.

### 0.4 Provenance and verification methodology

This theorem was established by **four refereed proofs — two independent general architectures, their
synthesis, and the flat base** — of which the proof below (the synthesis) is the definitive
distillation:

1. **Flat base case** (`pifo-flat-proof.md`, v2.2 FROZEN, sha256 `607b76f6…`): flush ⇒ interleaved
   for height-1 trees, via a canonical form (obstruction-component partition + effective-comparison
   table) that both determines behavior and is recoverable from ≤3-type drains. Twice refereed
   (independent codex and Fable hostile reviews, both SOUND). Formalized in Lean **twice** (below).
2. **General lift** (`pifo-lift-proof.md`, v1.3, sha256 `ed11e747…`): reduces arbitrary topology to
   the flat base by a token-stream induction on the alphabet, consuming the flat base through triple
   projections (height enters only through auxiliary lemmas), carrying child sub-schedulers driven by
   interleaved contexts. Dual hostile review; CONFIRMED-SOUND after v1.3 repairs.
3. **General blind proof** (`pifo-pen-ultra2-report.md`): an independent full-conjecture proof (the
   prover saw only the frozen statement) via common refinement of root partitions and colored
   congruence. Refereed SOUND; it is the architecture the proof below distills.
4. **The elegant synthesis** (§§1–4 here): the pair-dependent meet architecture, self-contained,
   proved SOUND under a maximum-hostility review conducted on the explicit assumption that elegance
   was hiding a hole (`pifo-referee-elegant.md`).

**Two complete Lean formalizations of the flat theorem** exist, by different routes: a **canon route**
(`pifo-lean-flat/`) that discharges only the finite readability/invariance certificate by
`native_decide`; and a **certificate-free pair-dependent route** (`pifo-lean-flat-pairdep/`, ~3,200
lines) that uses standard core axioms *only* — no `native_decide`, no finite oracle. Height-0 is
Lean-proved (`pifo-lean-h0/`); general-case formalization is in progress on several lanes.

**Methodology.** Every claim was subjected to (i) **dual hostile review** — two independent referee
agents, maximum-hostility posture, each re-deriving every step by hand; (ii) **executable
construction testing** — the proof's own construction implemented literally and machine-checked, not
merely spot-tested (the elegant proof: 170,846 checks, 0 failures, including 80 fully-audited pairs
with genuinely crossing decompositions and each coupling step checked separately; across lanes,
millions of exhaustive and random scheduler pairs with zero counterexamples); and (iii) **sha
discipline** — documents are frozen and pinned by sha256, and referees verify the sha before and
after review so a verdict provably attaches to an exact text.

*Editorial note (colored-PIFO lemma).* An alternative argmin-coupling argument for the colored-PIFO
lemma, discovered during the height-1 Lean formalization (`pifo-lean-h1-fable/Kernel.lean`), was
evaluated for inclusion. It is kernel-checked and elegant in Lean, but in prose it introduces extra
vocabulary (the lane's `Tied` classes and `Rel` invariant) without shortening the argument, and
the referee independently judged the profile-class argument below the cleanest of four verified
guises of this lemma. It is therefore *not* adopted here; the §1 proof stands as written.

---

## 1. Operational facts

At every reachable valid state and every internal-node child \(c\),

\[
\#\{\text{references to }c\}
=
\#\{\text{packets below }c\}. \tag{1}
\]

A valid push increments both sides; a successful pop decrements both. Hence a selected reference always points to a nonempty subtree, so the partial-failure branch of atomic `treePop` is unreachable. A pop fails exactly when the total packet count is zero. Therefore the `none`/`some` pattern depends only on the operation word.

Only comparisons of arrival stamps matter. Replacing any increasing subsequence of global timestamps by \(1,2,\ldots\) preserves every queue decision. *(Scoped precisely, per the codex second signature: the subsequence renumbered is all timestamps participating in the projected child run — a complete run's worth, in original relative order — not an arbitrary subset that could reorder against retained stamps; since only within-queue relative arrival order matters, every decision is preserved.)* Thus embedded child runs with timestamp gaps equal their standalone runs after timestamp compression.

We may ghost-tag an entry by its source push. This tag is not observable and is not stored in `Entry.val`; erasing it commutes with every transition because queue ordering uses only rank and arrival.

### Two-type lemma

For distinct \(x,y\), follow their paths to the first internal node where their child indices differ, or to their common leaf if they never differ. Before that decisive node, every reference has the same child value, so those nodes merely forward one anonymous service per successful pop.

At the decisive node:

- if it is internal, its selected child identifies \(x\) or \(y\), and that child’s restricted subtree is monochromatic;
- if it is a leaf, the queue directly emits \(x\) or \(y\).

Thus every operation word over \(\{x,y\}\) behaves as one stable PIFO with the decisive comparison

\[
e(x,y)\in\{x<y,\ x=y,\ y<x\}.
\]

The two binary flushes recover it exactly:

\[
\begin{array}{c|cc}
e(x,y)&F(xy)&F(yx)\\ \hline
x<y&xy&xy\\
x=y&xy&yx\\
y<x&yx&yx.
\end{array} \tag{2}
\]

This includes FIFO ties.

### Colored-PIFO lemma

Let \(p,q\) be two total preorders on types, colored by \(\gamma\), and suppose

\[
\gamma(x)\ne\gamma(y)
\implies
\operatorname{cmp}_p(x,y)=\operatorname{cmp}_q(x,y), \tag{3}
\]

including equality. Then queues ordered by \(p,q\), receiving identical pushes and pops, emit identical color traces.

For the proof, partition each color into cross-profile classes:

\[
x\sim y
\iff
\gamma(x)=\gamma(y)
\ \land\
\forall z\text{ of another color},\
\operatorname{cmp}_p(x,z)=\operatorname{cmp}_p(y,z).
\]

These classes are common to \(p,q\). Distinct classes have a common quotient preorder:

- differently colored classes are ordered identically by (3);
- if same-colored classes have different profiles, a witnessing outside type places one class strictly before the other in both preorders.

If a class ties another class, the other has a different color, and transitivity forces every member of the first class to tie every other member under both \(p,q\).

Maintain equal pending counts in every profile class, and identical pending occurrence sets in every class tied with another class. A pop chooses the same least quotient class. If several classes tie, FIFO chooses the same globally earliest occurrence. If the least class is an isolated monochromatic class, the queues may remove different types inside it, but emit the same color and leave equal counts. The invariant survives later pushes and pops. Empty pops agree as well. ∎

## 2. Flush decompositions

For \(|A|>1\), follow the unique child used by all types until reaching either the first node with at least two active children or a leaf.

Nodes skipped this way are online-inert: every reference has the same child value. At a terminal leaf, replace it by a root carrying the old leaf ranks and one FIFO singleton child per type. The expanded root mirrors the old leaf queue occurrence-for-occurrence.

Consequently every scheduler is online-equivalent to

\[
M=(P,r,(M_B)_{B\in P}), \tag{4}
\]

where \(P\) is a proper partition of \(A\) into active root children and \(r\) is the root preorder.

Let \(F_M(w)\) be the type word produced by flushing \(w\). Then

\[
F_M(w)|_B=F_{M_B}(w|_B)=F_M(w|_B), \tag{5}
\]

because child \(B\) receives all of \(w|_B\) before any service and is called exactly \(|w|_B\) times. Also,

\[
\pi_P(F_M(w))
=
\pi_P(\operatorname{sort}_r(w)), \tag{6}
\]

because the root drains its references in stable \(r\)-order.

Call \((P,r)\) a decomposition of \(F\) when (5)–(6) hold.

## 3. The meet lemma

The central fact is

\[
\boxed{
(P,r)\text{ and }(Q,s)\text{ decompose }F
\Longrightarrow
(P\wedge Q,t)\text{ decomposes }F
}
\tag{7}
\]

for some total preorder \(t\).

Let

\[
R=P\wedge Q
=\{B\cap C\ne\varnothing:B\in P,\ C\in Q\}.
\]

### Cell autonomy

For \(D=B\cap C\),

\[
\begin{aligned}
F(w)|_D
&=(F(w)|_B)|_C\\
&=F(w|_B)|_C\\
&=F((w|_B)|_C)\\
&=F(w|_D).
\end{aligned} \tag{8}
\]

Thus every meet cell is autonomous.

### Cross-cell comparisons

For types in distinct \(R\)-cells, prescribe the binary effective comparison \(e(x,y)\) from (2).

If \(P\) separates \(x,y\), then their pair restriction is selected at the \(P\)-root, so

\[
e(x,y)=\operatorname{cmp}_r(x,y). \tag{9}
\]

Likewise, if \(Q\) separates them,

\[
e(x,y)=\operatorname{cmp}_s(x,y). \tag{10}
\]

Hence the prescriptions agree whenever both partitions separate the pair.

### The three-cell diamond

Take one type from each of three distinct cells, viewed as points in the \(P\times Q\) incidence grid.

- If their \(P\)-coordinates are distinct, all comparisons come from \(r\).
- If their \(Q\)-coordinates are distinct, all come from \(s\).
- Otherwise the three points form an L. After naming its common corner \(x\), the restricted transformer has both decompositions

\[
\{x,y\}\mid\{z\},
\qquad
\{x,z\}\mid\{y\}. \tag{11}
\]

We prove that \(e\) on this triple is a total preorder. Otherwise its non-strict relation is not transitive, so there are distinct \(a,b,c\) with

\[
a\le_e b,\qquad b\le_e c,\qquad c<_e a.
\]

Push them in the single arrival order \(\sigma=abc\). This orients any tie on \(a,b\) or \(b,c\) forward, producing a stable precedence cycle. Rotate its names around the common corner so that

\[
x\prec_\sigma y\prec_\sigma z\prec_\sigma x. \tag{12}
\]

Crucially, \(\sigma\) is retained; it need not be the word \(xyz\).

For the first decomposition in (11), the outer occurrence order is \(y,z,x\), giving slots

\[
\{x,y\},z,\{x,y\}.
\]

The inner pair outputs \(x,y\), so the flush output is

\[
x,z,y.
\]

For the second decomposition, the outer order is \(x,y,z\), giving slots

\[
\{x,z\},y,\{x,z\}.
\]

Its inner pair outputs \(z,x\), so the result is

\[
z,y,x.
\]

This contradicts the fact that both are decompositions of the same \(F\). Thus every three-cell restriction is a total preorder.

### Global extension

Build a directed constraint graph on types:

- \(x<e y\) contributes a marked edge \(x\to y\);
- \(x=e y\) contributes unmarked edges both ways.

These comparisons extend to a total preorder exactly when there is no directed cycle containing a marked edge. Indeed, if no such cycle exists, no marked edge lies inside a strongly connected component. Collapse SCCs, linearly extend the resulting DAG, and assign successive natural ranks; ties lie inside SCCs and strict comparisons go between them.

Suppose a shortest marked closed walk existed. It may be taken simple. If vertices two steps apart belonged to distinct cells, the total preorder on those three cells would provide a shortcut, marked whenever either replaced edge was marked. This would give a shorter marked closed walk.

Therefore every second vertex lies in the same cell. An odd cycle is then impossible, and an even cycle alternates between two cells. But all comparisons between two fixed cells come from one genuine preorder:

- from \(r\) if their \(P\)-blocks differ;
- otherwise from \(s\), since their \(Q\)-blocks differ.

No genuine preorder contains such a cycle. Hence the required total preorder \(t\) exists and is realizable by natural ranks.

*(Degenerate lengths are subsumed, not special cases. A marked cycle of length 2 is impossible because \(e\) is single-valued — a strict comparison yields exactly one directed edge — and it also falls to the alternation branch. A marked cycle of length 3 has pairwise-distinct cells, since every edge is cross-cell, and dies directly by the diamond. The shortcut edge used above lies in the graph and inherits marking, because composing a strict comparison with a \(\le\) in a total preorder is strict.)*

Moreover,

\[
t=r\quad\text{across distinct \(P\)-blocks},\qquad
t=s\quad\text{across distinct \(Q\)-blocks}. \tag{13}
\]

### The meet-cell control word

By colored congruence, with colors \(P\),

\[
\pi_P(F(w))
=\pi_P(\operatorname{sort}_r(w))
=\pi_P(\operatorname{sort}_t(w)). \tag{14}
\]

Fix \(B\in P\). Inside \(B\), its \(R\)-cells are its intersections with \(Q\)-blocks. Therefore

\[
\begin{aligned}
\pi_Q(F(w)|_B)
&=\pi_Q(F(w|_B))\\
&=\pi_Q(\operatorname{sort}_s(w|_B))\\
&=\pi_Q(\operatorname{sort}_t(w|_B))\\
&=\pi_Q(\operatorname{sort}_t(w)|_B).
\end{aligned} \tag{15}
\]

The middle equality is colored congruence using (13). The last is stable-sort projection: deleting occurrences outside \(B\) changes no retained pairwise order; compressing timestamps is strictly increasing.

The global \(P\)-word together with every per-\(B\) \(Q\)-subword uniquely determines the \(R\)-word. Scanning the \(P\)-word, at each \(B\)-position take the next \(Q\)-symbol for that \(B\). Thus

\[
\pi_R(F(w))
=
\pi_R(\operatorname{sort}_t(w)). \tag{16}
\]

Equations (8) and (16) prove the meet lemma. Equivalently, \(F\) can be realized by a \(t\)-root routing to the \(R\)-cells, with child transformer \(F|_D\) in cell \(D\). A word is uniquely reconstructed from its cell skeleton and its subsequence in every cell. ∎

## 4. Strong induction on the alphabet

We prove the theorem simultaneously for all finite alphabets.

For \(|A|=0\), every operation is a failed pop. For \(|A|=1\), each successful pop emits the sole type, and success is determined by the packet count.

Let \(|A|>1\), and normalize two flush-equivalent schedulers \(S,T\) as above. Let their proper top decompositions be

\[
(P,r,(S_B)),\qquad (Q,s,(T_C)),
\]

with common flush transformer \(F\).

Apply the meet lemma, obtaining \(R=P\wedge Q\) and \(t\). For each \(D\in R\), choose a valid scheduler \(U_D\) realizing \(F|_D\), for example the restriction \(S|D\). *(This restriction is legitimate: `run(S, w) = run(S|_D, w)` for every word \(w\) over \(D\), because no reference to a type outside \(D\) is ever created, so no other subtree is entered; hence \(S|D\) realizes \(F|_D\).)* Let \(U\) be the scheduler with a \(t\)-root routing directly to the \(U_D\).

By (8) and (16),

\[
F_U=F. \tag{17}
\]

### Transforming \(S\) into \(U\)

For \(B\in P\), let \(V_B\) be a \(t|_B\)-root routing to the cells \(D\subseteq B\), with children \(U_D\). Then

\[
F_{S_B}=F|_B=F_{V_B}. \tag{18}
\]

*(The right equality \(F_{V_B}=F|_B\) is (8) and (16) instantiated at \(B\)-words: for \(w\) over \(B\) one has \(\operatorname{sort}_t(w)=\operatorname{sort}_{t|_B}(w)\), and the meet cells lying inside \(B\) are exactly the \(D\subseteq B\), so the \(t|_B\)-root over those cells with children \(U_D\) reproduces \(\pi_R(F|_B)\) and, cell by cell, \(F|_D\) — i.e. \(F|_B\). The left equality is (5) for the \(P\)-decomposition.)*

Since \(B\subsetneq A\), the induction hypothesis gives

\[
S_B\simeq_{\mathrm{inter}}V_B. \tag{19}
\]

Substitute every \(V_B\) for \(S_B\). The substitution is an online coupling with invariant: the two schedulers hold identical states at the root and at every child other than \(B\), and child \(B\) has received the same *driven word* on both sides — namely its projected pushes together with one anonymous service per \(B\)-token popped by the root. This invariant is preserved by every operation: a push routes identically at the root (whose reference queue is untouched by the swap) and appends the same letter to the same child's driven word; a pop selects the same root reference, hence drives the same child, and by (19) the swapped child \(B\) emits identically under its driven word (its timestamps are a sparse subsequence, which compresses monotonically, so (19) applies verbatim). Hence the whole scheduler is preserved online.

The resulting scheduler has two selector levels:

- an outer \(r\)-selector routing to \(P\);
- an inner \(t\)-selector routing to \(R\).

Replace the outer \(r\) by \(t\). By (13) and colored congruence, the entire online \(P\)-token trace is unchanged. The children therefore receive identical driven words.

Now both selector levels use exactly \(t\). They collapse occurrence-for-occurrence. More precisely, maintain a pending set \(\Omega\) of history-tagged pushes such that:

- the two-level outer queue is \(\Omega\), labeled by \(P\)-block;
- \(U\)’s root queue is \(\Omega\), labeled by \(R\)-cell;
- the inner queue for \(B\) is \(\Omega|_B\), labeled by \(R\)-cell;
- corresponding \(U_D\) states are identical.

All selector keys are the same \((t,\text{arrival})\). If the outer queue selects \(o\in B\), then \(o\) is also the minimum of \(\Omega|_B\), so the inner queue selects the same occurrence, including FIFO ties. Both schedulers then call the same \(U_D\) state. A successful lower pop deletes \(o\) from all corresponding selectors; an atomic lower failure rolls both sides back identically.

Thus the two levels collapse exactly to \(U\), and

\[
S\simeq_{\mathrm{inter}}U. \tag{20}
\]

The symmetric argument, using \(Q,s\), gives

\[
T\simeq_{\mathrm{inter}}U. \tag{21}
\]

Therefore \(S\simeq_{\mathrm{inter}}T\). Restoring the universally identical `none` positions proves equality of the exact Lean `run` outputs on every operation word. ∎

## 5. What was discarded and why

- Canonical forms, obstruction components, cell-feasibility machinery, and finite certificates are unnecessary. The pair-dependent meet uses the two given decompositions without reconstructing either from observations.
- Forced-pair closures, repeated promotion, termination potentials, grandchild graphs, and global triple flattening disappear. Only the single L-shaped three-type diamond remains.
- Pop-cancellation and residual batch-response reconstruction are avoided entirely; those routes encounter the identifiability impossibility (§A).
- No occurrence-labelled lifting of flush equivalence or observable token decomposition is required. Ghost occurrences are used only inside direct couplings of explicitly constructed queues.

## 6. Self-check

- **F1/F2 and the meta-theorem:** the proof never claims that \(F\) identifies \(P\) or \(Q\). It reads them from the two given schedulers and constructs the pair-dependent refinement \(P\wedge Q\). Tie-merges and aligned strict merges are therefore legitimate, not ambiguities to recover.

- **FIFO ties:** binary recovery uses both arrival orders; colored congruence keeps exact cross-color ties; the diamond uses one globally consistent arrival permutation; tie constraints are bidirectional graph edges; tandem collapse uses identical global timestamps.

- **Hijacking and residue drift:** no parent reference is identified with the packet it services. Recoloring couples only block colors. Exact occurrence equality is asserted only for the deliberately duplicated \(t\)-selectors, where their pending occurrence sets are explicitly equal.

- **Higher-arity obstructions:** any alleged longer obstruction has a three-cell shortcut or alternates between two cells governed by one genuine preorder. Equality-chain inconsistencies are included through marked cycles.

- **Atomicity and over-pops:** balance excludes partial failure on reachable valid states, and an empty pop is a common `none` leaving state unchanged.

- **Timestamp gaps and repeated types:** strictly increasing timestamp compression preserves all comparisons; stable-sort projection is occurrence-based and remains valid with repetitions.

- **Induction measure:** only the original top blocks \(B\in P\) and \(C\in Q\) must be proper. A meet cell may equal an entire block, making \(V_B\) unary, but the induction is still on \(|B|<|A|\), not on the child’s size or tree height.

- **Known false completion principle:** no interleaved divergence is extended into a flush, and no residual state is treated as “the old drain minus its first output.”

---

## A. Appendix — falsified routes (the boundaries of the proof space)

Three plausible strategies were pursued and *disproved*; each falsification is machine-witnessed and
delimits the design of the proof above. They are recorded so the space is not re-searched.

**A.1 Push-projection completion ("hijack rescue") — FALSE.** The tempting shortcut is: if two
schedulers first diverge at a pop of some interleaved word \(\sigma\), then the flush word of
\(\sigma\)'s push-projection already distinguishes them. It does not. A push occurring *after* the
diverging pop can contribute a low-rank reference that, in the all-at-once flush, is popped early and
*hijacks* the disputed packet, repairing the disagreement. Flush distinguishability is therefore
**not monotone** under appending pushes (minimal witnesses verified, including both-side-flat
instances requiring multiplicity ≥ 3). Consequence, honored throughout §§1–4: no step may "extend a
word until a difference shows"; the analysis is strictly local (the two-type table, the three-cell
diamond) and pair-dependent.

**A.2 The probe-identifiability meta-theorem — an impossibility.** Every probe an observation-based
proof can perform (insertion clones, \(N\)-fold floods) is a function of the flush drain family. But
two schedulers can share the *entire* drain family while their internal decompositions differ — the
canonical witnesses F1 (a \(\lambda\)-tied block vs. rank-tied singletons) and F2 (an aligned
co-leafed pair vs. separate leaves with a strictly-between witness) have identical probe families and
different structure. Hence **no probe-based argument can pin \(P\) or \(Q\) from flush behavior.**
This killed the two "recover the structure, then reconstruct the residual" pen proofs (the cloning
identifiability step and the insertion-probe-to-deletion-effect step are both provably
underdetermined). Consequence: the winning proof never recovers structure from observation — it reads
\(P, r\) and \(Q, s\) from the two *given* schedulers and takes their meet. The impossibility is
exactly why the pair-dependent architecture, not a canonical-form recovery, is the right shape.

**A.3 Residual-drain determinism — FALSE.** One might hope the state left after a pop is a function
of "the old drain minus its first output," so a bisimulation could be run purely on drain families.
It is not: orphaned references (a served reference whose packet was hijacked) make the residual state
depend on hidden structure that the drain family does not determine. This is the residual-state form
of the same identifiability gap. Consequence: the collapse in §4 couples *explicitly constructed*
queues with equal pending occurrence sets and never treats a residual as a derived drain.

---

## B. Files and artifacts

- **This document:** `/tmp/pifo-theorem-final.md`. Base proof: `pifo-elegant-proof-report.md`
  (sha256 `8f236381…`); its hostile review `pifo-referee-elegant.md` (SOUND; 170,846 checks, 0
  failures; scripts `pifo-referee-elegant-check.js`, `pifo-referee-elegant-crossing.js`).
- **Frozen statement:** `pifo-statement/PifoStatement.lean` (sha256 `fe9475eb…`), dual-reviewed
  (`review-codex.md`, `review-fable.md`).
- **Stateful boundary counterexample:** `pifo-lean/Pifo.lean`, `pifo-lean/Extended.lean`
  (machine-checked).
- **Other refereed proofs:** `pifo-flat-proof.md` (v2.2 FROZEN, sha256 `607b76f6…`; reviews
  `pifo-flatproof-review-codex-final.md`, `pifo-flatproof-review-v2.md`); `pifo-lift-proof.md` (v1.3,
  sha256 `ed11e747…`; reviews `pifo-referee-lift-codex.md`, `pifo-referee-lift-ultra.md`,
  `pifo-lift-v13-confirm.md`); `pifo-pen-ultra2-report.md` (review `pifo-referee-ultra2.md`).
- **Flat Lean formalizations:** `pifo-lean-flat/` (canon route, `native_decide` for the certificate
  only); `pifo-lean-flat-pairdep/` (certificate-free, standard axioms only). Height-0:
  `pifo-lean-h0/`.
- **Falsified-route witnesses:** `pifo-referee-report.md` (identifiability impossibility, F1/F2
  probe-family analysis); flat-proof Appendix A and `pifo-proof4-reconcile.js`,
  `pifo-proof4-flatviol.js` (hijack rescue); clean-room reports (residual-drain determinism).
