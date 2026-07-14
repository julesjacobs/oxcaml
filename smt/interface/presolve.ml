(* W1b unconditional equality-elimination presolve. See presolve.mli for the contract and
   logs/w1b-design.md for the diagnosis / micro-spike evidence and the four
   build-soundness constraints implemented here. Pure over Term.t: a term rewrite through
   the session Context's smart constructors. *)

open Oxsmt_core

type def =
  { name : string
  ; sort : Sort.t
  ; value : Term.t
  }

type result =
  { reduced : Term.t list
  ; defs : def list
  }

(* Flatten a root [And] into its top-level conjuncts, descending NO further (constraint 1:
   an [Eq] under [Or]/[Ite]/[Not] — or under a nested non-[And] node — is never a
   top-level conjunct). A nested root [And] is flattened recursively; anything else is a
   single conjunct. Order-preserving (I6). *)
let rec flatten (c : Term.t) =
  match c.node with
  | And xs -> List.concat_map flatten (Iarr.to_list xs)
  | _ -> [ c ]
;;

(* Structural occurs-check: does the (nullary-variable) term [x] appear as a subterm of
   [t]? Terms are hash-consed, so equality is O(1) by tag. *)
let rec occurs (x : Term.t) (t : Term.t) =
  Term.equal x t
  ||
  match t.node with
  | App (_, args) -> Iarr.exists (occurs x) args
  | Arith lin -> Iarr.exists (fun (tm, _c) -> occurs x tm) lin.coeffs
  | Le a | Not a -> occurs x a
  | Eq (a, b) -> occurs x a || occurs x b
  | And xs | Or xs -> Iarr.exists (occurs x) xs
  | Ite (c, a, b) -> occurs x c || occurs x a || occurs x b
  | Bool_const _ | Int_const _ -> false
;;

(* [t] contains no applied (arity >= 1) function/predicate — i.e. no EUF-owned node. A
   candidate alias's RHS must be UF-free: this keeps substitution from moving a UF
   application into new positions (perturbing the interface set) and keeps model
   re-derivation over Int-only values (constraint 3 / model reconstruction). Vacuously
   true for pure QF_LIA. *)
let rec uf_free (t : Term.t) =
  match t.node with
  | App (_, args) -> Iarr.length args = 0
  | Arith lin -> Iarr.for_all (fun (tm, _c) -> uf_free tm) lin.coeffs
  | Le a | Not a -> uf_free a
  | Eq (a, b) -> uf_free a && uf_free b
  | And xs | Or xs -> Iarr.for_all uf_free xs
  | Ite (c, a, b) -> uf_free c && uf_free a && uf_free b
  | Bool_const _ | Int_const _ -> true
;;

(* The nullary variables that occur UNDER an uninterpreted-function application anywhere
   in the assertion set — the ADR-0010 §3.1 boundary source that makes a variable an
   interface member. Eliminating such a variable would perturb the combinator's
   boundary/shared-set computation (constraint 3), so the pass skips it. Computed by a
   single structural walk that carries an [under_uf] flag, set true for the whole subtree
   of every UF argument. Empty for pure QF_LIA (no UF applications) — the guard is then
   vacuous. *)
let under_uf_vars (assertions : Term.t list) =
  let acc = ref Term.Set.empty in
  (* MEMOIZED over the hash-cons DAG. The assertion set is maximally shared (the [let]
     reader reuses each bound value by reference), so the naive per-path recursion
     re-walks a shared subterm once per path to it — exponential on let-heavy inputs. A
     node's contribution depends on the [under_uf] flag it was reached with (an [App] leaf
     is only recorded when it appears under a function argument), so the visited key
     carries that flag. *)
  let visited : (int * bool, unit) Hashtbl.t = Hashtbl.create 256 in
  let rec walk ~under_uf (t : Term.t) =
    let key = t.Term.tag, under_uf in
    if not (Hashtbl.mem visited key)
    then (
      Hashtbl.replace visited key ();
      match t.node with
      | App (_, args) when Iarr.length args > 0 -> Iarr.iter (walk ~under_uf:true) args
      | App (_, _) -> if under_uf then acc := Term.Set.add t !acc
      | Arith lin -> Iarr.iter (fun (tm, _c) -> walk ~under_uf tm) lin.coeffs
      | Le a | Not a -> walk ~under_uf a
      | Eq (a, b) ->
        walk ~under_uf a;
        walk ~under_uf b
      | And xs | Or xs -> Iarr.iter (walk ~under_uf) xs
      | Ite (c, a, b) ->
        walk ~under_uf c;
        walk ~under_uf a;
        walk ~under_uf b
      | Bool_const _ | Int_const _ -> ())
  in
  List.iter (walk ~under_uf:false) assertions;
  !acc
;;

(* A bare Int variable eligible to be an alias LHS: a nullary application of a
   non-reserved Int symbol. (Bool aliases — iff connectives — and uninterpreted-sort
   variables are out of scope; v1 eliminates Int vars only, matching the SMPT/Dartagnan
   corpus.) *)
let is_bare_int_var (t : Term.t) =
  match t.node with
  | App (sym, args) ->
    Iarr.length args = 0
    && Sort.equal t.sort Sort.int
    && not (Env.is_reserved_name (Symbol.name sym))
  | _ -> false
;;

(* If [c] is a top-level atom-equality with a bare-Int-var side that does not occur in the
   other side, return [(x, t)] (x the variable, t its definition). A Bool [Eq] (iff) is
   not an atom equality and is skipped. When both sides are eligible bare vars
   ([(= x y)]), the tag-lower side [a] is chosen — a deterministic function of the term
   (I6). *)
let candidate (c : Term.t) =
  match c.node with
  | Eq (a, b) when Sort.equal a.sort Sort.int ->
    if is_bare_int_var a && not (occurs a b)
    then Some (a, b)
    else if is_bare_int_var b && not (occurs b a)
    then Some (b, a)
    else None
  | _ -> None
;;

(* The set of variables reachable from [t] by following the current substitution [sigma]
   (the transitive closure of the alias graph rooted at [t]'s variables). Adding [x ↦ t]
   would close a cycle exactly when [x] is in this set; such an alias is rejected (kept as
   an ordinary constraint) so [sigma] stays acyclic. *)
let reachable sigma (t : Term.t) =
  let acc = ref Term.Set.empty in
  let rec go (u : Term.t) =
    match u.node with
    | App (_, args) when Iarr.length args = 0 ->
      if not (Term.Set.mem u !acc)
      then (
        acc := Term.Set.add u !acc;
        match Term.Map.find_opt u sigma with
        | Some rhs -> go rhs
        | None -> ())
    | App (_, args) -> Iarr.iter go args
    | Arith lin -> Iarr.iter (fun (tm, _c) -> go tm) lin.coeffs
    | Le a | Not a -> go a
    | Eq (a, b) ->
      go a;
      go b
    | And xs | Or xs -> Iarr.iter go xs
    | Ite (c, a, b) ->
      go c;
      go a;
      go b
    | Bool_const _ | Int_const _ -> ()
  in
  go t;
  !acc
;;

let name_of (x : Term.t) =
  match x.node with
  | App (sym, _) -> Symbol.name sym
  | _ -> invalid_arg "Presolve.name_of: not an application"
;;

(* --- Pass A: entailed-equality extraction from top-level disjunctions (task #7) ------
   For each top-level conjunct [(or D_1 … D_k)], add the equalities entailed by EVERY
   disjunct: if [e] is in the induced equality-closure of every [D_j] then [D_j ⊨ e] for
   all j, so [(or D_j) ⊨ e] — adding [e] is equisatisfiable (both directions), eliminates
   no variable, and introduces no new term. On eq_diamond each diamond
   [(or (and (=xi yi)(=yi x{i+1})) (and (=xi zi)(=zi x{i+1})))] entails [xi=x{i+1}]; the
   added units chain [x0=…=xn] at the EUF level 0, refuting [x0≠xn] with no search.

   Reviewer-pinned contracts (logs/codex-review/preprocess-design.md,
   preprocess-design-review-fable.md); each has a mutation test in [presolve_test]:
   - ATOMIC-LEAF GRAMMAR (codex BLOCKER-1 / fable #5): within a disjunct, descend through
     [And] ONLY; a leaf is a positive [Eq(a,b)] between NON-Bool terms; OPAQUE at
     Eq-operands, [Not], [Or], [Ite], and Bool/iff equalities. Recursing into any of those
     can extract a non-entailed equality ⇒ wrong-Unsat.
   - INDEPENDENT PER-BRANCH INTERSECTION (codex BLOCKER-2): each disjunct gets its OWN
     union-find; the intersection is "same class in EVERY branch" via signature vectors;
     the branch union-finds are NEVER merged. Emit a spanning FOREST (chain per
     intersection-class), never the O(n²) materialized closure.
   - SINGLE PASS, no fixpoint (fable #4): the EUF core does the transitive chaining.
   - BUDGET / neutral abort (codex MED-6 / fable #3): bail an [Or] as soon as a disjunct
     has no equality leaves (its closure is all-singletons ⇒ empty intersection), or a
     hard term/leaf cap is exceeded — skip that [Or], never fail.
   - DETERMINISM (fable #6): term-tag order throughout (universe via [Term.Set]; groups
     via an ordered [Map]; edges in ascending index order).

   Soundness backstop note: the win direction is UNSAT, where R1 (model self-check) does
   NOT run — so these contracts ARE the soundness margin (fable #2). Gated + cert-OFF at
   the session; a wrong (non-entailed) addition is caught only by the ON-vs-OFF 0-mismatch
   sweep, so the discriminating mutants are mandatory. *)

let pass_a_max_terms = 512
let pass_a_max_leaves = 1024

(* Atomic equality leaves of one disjunct, under the pinned grammar. *)
let eq_leaves (d : Term.t) =
  let acc = ref [] in
  let rec go (t : Term.t) =
    match t.node with
    | And xs -> Iarr.iter go xs
    | Eq (a, b) when not (Sort.equal a.sort Sort.bool) -> acc := (a, b) :: !acc
    (* OPAQUE: Not, Or, Ite, Bool/iff Eq, App, Le, Arith, consts — no recursion. *)
    | _ -> ()
  in
  go d;
  !acc
;;

(* Equalities entailed by [(or ds)] as a spanning forest of new [Eq] terms (built through
   [ctx]). [] (neutral) when a disjunct has no eq leaves or a cap is exceeded. *)
let or_entailed_eqs ctx (ds : Term.t list) =
  let branches = List.map eq_leaves ds in
  (* fire-condition: an equality-free disjunct ⇒ its closure is all singletons ⇒ the
     intersection is empty ⇒ this [Or] entails nothing. Bail. *)
  if
    List.exists
      (function
        | [] -> true
        | _ -> false)
      branches
  then []
  else (
    let univ_set =
      List.fold_left
        (List.fold_left (fun s (a, b) -> Term.Set.add a (Term.Set.add b s)))
        Term.Set.empty
        branches
    in
    let universe = Array.of_list (Term.Set.elements univ_set) in
    let n = Array.length universe in
    let too_big =
      n > pass_a_max_terms
      || List.exists (fun l -> List.length l > pass_a_max_leaves) branches
    in
    if too_big
    then [] (* neutral abort — skip this Or, never fail *)
    else (
      let idx = Term.Table.create (2 * n) in
      Array.iteri (fun i t -> Term.Table.replace idx t i) universe;
      let k = List.length branches in
      (* one INDEPENDENT union-find per branch (never merged across branches). *)
      let ufs = Array.init k (fun _ -> Array.init n (fun i -> i)) in
      let rec find uf i =
        let p = uf.(i) in
        if p = i then i else find uf p
      in
      let union uf i j =
        let ri = find uf i
        and rj = find uf j in
        if ri <> rj then uf.(ri) <- rj
      in
      List.iteri
        (fun b leaves ->
           let uf = ufs.(b) in
           List.iter
             (fun (a, c) -> union uf (Term.Table.find idx a) (Term.Table.find idx c))
             leaves)
        branches;
      (* signature vector = class rep in each branch; equal signatures ⇒ same class in
         every branch ⇒ entailed equal. *)
      let sig_of i = List.init k (fun b -> find ufs.(b) i) in
      let module SM = Map.Make (struct
          type t = int list

          let compare = compare
        end)
      in
      let groups = ref SM.empty in
      for i = 0 to n - 1 do
        let s = sig_of i in
        groups
        := SM.update
             s
             (function
               | None -> Some [ i ]
               | Some l -> Some (i :: l))
             !groups
      done;
      let out = ref [] in
      SM.iter
        (fun _ idxs ->
           match List.sort compare idxs with
           | [] | [ _ ] -> ()
           | x :: rest ->
             ignore
               (List.fold_left
                  (fun prev cur ->
                     out := Context.eq ctx universe.(prev) universe.(cur) :: !out;
                     cur)
                  x
                  rest
                : int))
        !groups;
      List.rev !out))
;;

(* Pass A entry: entailed equalities to ADD as top-level units, over the flattened
   top-level conjuncts. Pure; deterministic. The caller (Session, gated + cert-OFF)
   internalizes these but does NOT record them in the R1 [asserted] set (they are derived
   consequences, not original assertions). *)
let entailed_equalities ctx assertions =
  let conjuncts = List.concat_map flatten assertions in
  List.concat_map
    (fun (c : Term.t) ->
       match c.node with
       | Or ds -> or_entailed_eqs ctx (Iarr.to_list ds)
       | _ -> [])
    conjuncts
;;

let run ctx assertions =
  let conjuncts = List.concat_map flatten assertions in
  (* Interface variables to skip (constraint 3, defense-in-depth): vars occurring under a
     UF application. NOT a soundness requirement — substituting a variable by an equal
     term is equisatisfiable IN ANY THEORY (codex H2 / same-model adjudication), and R1
     gates every reported Sat — so this only ever forgoes an optimization, never affects a
     verdict. *)
  let uf_vars = under_uf_vars assertions in
  (* Pass 1 (first-wins, cycle-guarded): scan conjuncts in order, building the alias map
     [sigma] and the elimination [order]. Tag each conjunct [`Def] (its defining equality,
     dropped) or [`Keep] (retained, later substituted). Eligibility: a bare-Int-var alias
     whose variable is not already defined (first-wins), not an interface variable
     (constraint 3), whose RHS is UF-free, and which introduces no cycle. *)
  let sigma = ref Term.Map.empty in
  let order = ref [] in
  let tagged =
    List.map
      (fun c ->
         match candidate c with
         | Some (x, t)
           when (not (Term.Map.mem x !sigma))
                && (not (Term.Set.mem x uf_vars))
                && uf_free t
                && not (Term.Set.mem x (reachable !sigma t)) ->
           sigma := Term.Map.add x t !sigma;
           order := x :: !order;
           `Def
         | _ -> `Keep c)
      conjuncts
  in
  if Term.Map.is_empty !sigma
  then
    (* No elimination: return the ORIGINAL assertions unchanged (byte-for-byte neutral —
       no flattening, no rebuild), so a zero-alias file is solved identically to trunk. *)
    { reduced = assertions; defs = [] }
  else (
    let sigma = !sigma in
    (* Substitute to a fixpoint through the smart constructors: replace each defined
       variable by its (recursively resolved) definition. [sigma] is acyclic, so this
       terminates; memoized on the hash-cons tag so each distinct subterm is rebuilt once.
       The result mentions only surviving variables. *)
    let memo = Term.Table.create 256 in
    let rec subst (t : Term.t) =
      match Term.Table.find_opt memo t with
      | Some r -> r
      | None ->
        let r =
          match Term.Map.find_opt t sigma with
          | Some rhs -> subst rhs
          | None -> rebuild t
        in
        Term.Table.add memo t r;
        r
    and rebuild (t : Term.t) =
      match t.node with
      | Bool_const b -> Context.bool_const ctx b
      | Int_const n -> Context.int_const_big ctx n
      | App (sym, args) ->
        if Iarr.length args = 0
        then t (* a surviving variable — unchanged (preserves hash-cons identity) *)
        else Context.app ctx sym (List.map subst (Iarr.to_list args))
      | Arith lin ->
        Context.linear_combination_big
          ctx
          (List.map (fun (tm, co) -> co, subst tm) (Iarr.to_list lin.coeffs))
          lin.const
      | Le a -> Context.le ctx (subst a) (Context.int_const ctx 0)
      | Eq (a, b) -> Context.eq ctx (subst a) (subst b)
      | Not a -> Context.not_ ctx (subst a)
      | And xs -> Context.and_ ctx (List.map subst (Iarr.to_list xs))
      | Or xs -> Context.or_ ctx (List.map subst (Iarr.to_list xs))
      | Ite (c, a, b) -> Context.ite ctx (subst c) (subst a) (subst b)
    in
    let reduced =
      List.filter_map
        (function
          | `Def -> None
          | `Keep c -> Some (subst c))
        tagged
    in
    (* Defs in ELIMINATION order (order is newest-first; [rev_map] restores oldest-first).
       Each value is [subst x] = the definition resolved over surviving variables only, so
       model re-derivation needs no ordering between defs. *)
    let defs =
      List.rev_map
        (fun x -> { name = name_of x; sort = x.Term.sort; value = subst x })
        !order
    in
    { reduced; defs })
;;

(* --- Contextual simplification (task #13) ---------------------------------------------
   Within an [(ite c a b)] the condition [c] may be ASSUMED true while rewriting the
   then-branch [a] and false while rewriting the else-branch [b] — a scope-local,
   model-PRESERVING (logically equivalent) rewrite that eliminates NO variable. Two kinds
   of fact are recorded per branch:
   - BOOLEAN-ATOM: the atom (after stripping a leading [Not], flipping the truth) has a
     known truth value in the branch, so a literal reoccurrence of it folds to that
     constant. Sound in BOTH polarities (true in the then-branch, false in the
     else-branch).
   - EQUALITY→CONSTANT: when the (peeled) atom is [(= v k)] with [v] a bare non-reserved
     variable and [k] a literal, the THEN-branch — the only place [v = k] holds — also
     substitutes [v ↦ k], which drives the smart constructors' folding. The else-branch
     knows only [v ≠ k], which establishes NO substitution. Restricting [k] to a literal
     keeps the rewrite size-non-increasing. The {!Node} smart constructors already fold
     constants, collapse [(ite true|false …)] and [(not const)], flatten [and]/[or] at
     construction, so a substitution that turns a condition constant collapses the
     enclosing [ite] for free — this is the whole win: it reaches ITEs whose conditions
     are genuine non-constant subterms that local folding (all applied already at
     construction) cannot touch.

   SOUNDNESS (the win direction is UNSAT, where the R1 model self-check does NOT run — so
   these scoping rules ARE the soundness margin; each has a discriminating registry
   mutant, module=presolve, suite=wiring-test):
   - the then-assumption is applied ONLY under [a], the else-assumption ONLY under [b],
     and the condition [c] is rewritten under the PARENT env (leaking any of these to a
     sibling branch, to the condition, or above the [ite] is wrong-Unsat);
   - the equality substitution is added ONLY for the TRUE polarity of an [Eq] (a
     disequality condition establishes no substitution);
   - each branch env owns its OWN memo table: a term rewritten under one branch's
     assumptions is never reused under a different env (a shared cache would leak a
     branch-local fold to a sibling/parent — wrong-Unsat). Model reconstruction:
     unaffected — no variable is eliminated, and the R1 checker evaluates the ORIGINAL
     asserted terms, not these. Determinism (I6): a deterministic function of term
     structure; [Term.Map]/[Term.Table] and the [Context] constructors are
     order-insensitive. Hard node-visit budget with NEUTRAL ABORT: on cap the pass returns
     the ORIGINAL terms unchanged (never a partial/asymmetric rewrite). *)

let ctx_max_steps = 12_000_000

exception Ctx_budget

(* An assumption scope: the boolean atoms known true/false and the variable→literal
   substitutions in force, plus this scope's OWN memo (so a fold under these assumptions
   is physically unreachable from any other scope). *)
type ctx_env =
  { atoms : bool Term.Map.t
  ; subst : Term.t Term.Map.t
  ; memo : Term.t Term.Table.t
  }

(* An Int or Bool literal — the only right-hand side admitted for equality substitution. *)
let is_literal (t : Term.t) =
  match t.node with
  | Int_const _ | Bool_const _ -> true
  | App _ | Arith _ | Le _ | Eq _ | Not _ | And _ | Or _ | Ite _ -> false
;;

(* A bare non-reserved nullary variable — the only admissible substitution left-hand side.
   Reserved [.oxsmt.*] symbols are internal witnesses and never substituted. *)
let is_bare_var (t : Term.t) =
  match t.node with
  | App (sym, args) ->
    Iarr.length args = 0 && not (Env.is_reserved_name (Symbol.name sym))
  | Int_const _ | Arith _ | Le _ | Eq _ | Not _ | And _ | Or _ | Ite _ | Bool_const _ ->
    false
;;

let simplify_contextual ctx assertions =
  (* One node-visit budget shared across BOTH walks below (discovery + rewrite). On the
     cap the top-level [List.map] raises [Ctx_budget] and the whole simplified list is
     discarded for the ORIGINAL [assertions] — a neutral abort, never a partial/asymmetric
     rewrite. *)
  let steps = ref 0 in
  let tick () =
    incr steps;
    if !steps > ctx_max_steps then raise Ctx_budget
  in
  (* Observability-only counters (OXSMT_PRESOLVE_CTX_STATS): [engaged] = the rewrite ran
     on an ITE-bearing assertion; [subst_hits] = variable→literal substitutions applied;
     [atom_folds] = assumed-atom occurrences folded to a truth constant (the other, more
     common, way the pass rewrites — most nec-style collapse is atom-folding, not variable
     substitution). [engaged] with [subst_hits] + [atom_folds] = 0 is fired-but-no-effect.
     Counts are per distinct (scope, term) — memoization means a reused fold is tallied
     once, so these are coarse effect indicators, not exact rewrite totals. Emitted once
     per call to stderr, default-silent; never read back into the solve path. *)
  let engaged = ref false in
  let subst_hits = ref 0 in
  let atom_folds = ref 0 in
  (* Does [t]'s subtree contain any [Ite]? Memoized over the hash-cons DAG. An assertion
     with no [Ite] cannot be contextually simplified (the assumption env is only ever
     extended at an [ite]), so it is passed through untouched — the pass is then exactly
     neutral for a cheap non-allocating walk, which matters for the propagation-bound /
     large-arith files where the win never fires. The table is LOCAL to this call: term
     identity ([Term.equal]/[Term.hash]) is the per-[Context] hash-cons tag, so a table
     shared across [Context]s (one session per corpus file) would collide tags and return
     stale answers — a module-global table silently disables the pass. Discovery visits
     count against the shared budget (memoized ⇒ DAG-linear, but still bounded). *)
  let has_ite_table : bool Term.Table.t = Term.Table.create 4096 in
  let rec has_ite (t : Term.t) =
    match Term.Table.find_opt has_ite_table t with
    | Some b -> b
    | None ->
      tick ();
      let b =
        match t.node with
        | Ite _ -> true
        | Bool_const _ | Int_const _ -> false
        | App (_, args) -> Iarr.exists has_ite args
        | Arith lin -> Iarr.exists (fun (tm, _c) -> has_ite tm) lin.coeffs
        | Le a | Not a -> has_ite a
        | Eq (a, b) -> has_ite a || has_ite b
        | And xs | Or xs -> Iarr.exists has_ite xs
      in
      Term.Table.add has_ite_table t b;
      b
  in
  let root =
    { atoms = Term.Map.empty; subst = Term.Map.empty; memo = Term.Table.create 4096 }
  in
  (* Extend [env] with [atom = truth]: strip a leading [Not] (flipping [truth]) so a
     positive atom is recorded, and add the variable→literal substitution when the peeled
     atom is [(= v k)] AND [truth] is true. A FRESH scope (its own memo). *)
  let rec assume env (atom : Term.t) ~truth =
    match atom.node with
    | Not d -> assume env d ~truth:(not truth)
    | Bool_const _ | Int_const _ | App _ | Arith _ | Le _ | Eq _ | And _ | Or _ | Ite _ ->
      let atoms = Term.Map.add atom truth env.atoms in
      let subst =
        if truth
        then (
          match atom.node with
          | Eq (a, b) ->
            if is_bare_var a && is_literal b
            then Term.Map.add a b env.subst
            else if is_bare_var b && is_literal a
            then Term.Map.add b a env.subst
            else env.subst
          | Bool_const _
          | Int_const _
          | App _
          | Arith _
          | Le _
          | Not _
          | And _
          | Or _
          | Ite _ -> env.subst)
        else env.subst
      in
      { atoms; subst; memo = Term.Table.create 64 }
  in
  let rec simp env (t : Term.t) : Term.t =
    tick ();
    match Term.Table.find_opt env.memo t with
    | Some r -> r
    | None ->
      let r =
        match Term.Map.find_opt t env.subst with
        | Some v ->
          incr subst_hits;
          v
        | None ->
          (match Term.Map.find_opt t env.atoms with
           | Some b ->
             incr atom_folds;
             Context.bool_const ctx b
           | None ->
             (* Re-fold after rebuild: an outer substitution may have rewritten the atom
                that [assume] recorded (it records the REWRITTEN [c'], not the original
                [c]), so a branch occurrence of the ORIGINAL atom matches [env.atoms] only
                once its children are substituted. Check the rebuilt result against the
                assumed atoms too, else the promised recurrence-fold is missed
                (completeness only — equivalence holds either way). *)
             let r' = rebuild env t in
             (match Term.Map.find_opt r' env.atoms with
              | Some b ->
                incr atom_folds;
                Context.bool_const ctx b
              | None -> r'))
      in
      Term.Table.add env.memo t r;
      r
  and rebuild env (t : Term.t) : Term.t =
    match t.node with
    | Bool_const b -> Context.bool_const ctx b
    | Int_const n -> Context.int_const_big ctx n
    | App (sym, args) ->
      if Iarr.length args = 0
      then t (* a variable — unchanged (preserves hash-cons identity) *)
      else Context.app ctx sym (List.map (simp env) (Iarr.to_list args))
    | Arith lin ->
      Context.linear_combination_big
        ctx
        (List.map (fun (tm, co) -> co, simp env tm) (Iarr.to_list lin.coeffs))
        lin.const
    | Le a -> Context.le ctx (simp env a) (Context.int_const ctx 0)
    | Eq (a, b) -> Context.eq ctx (simp env a) (simp env b)
    | Not a -> Context.not_ ctx (simp env a)
    | And xs -> Context.and_ ctx (List.map (simp env) (Iarr.to_list xs))
    | Or xs -> Context.or_ ctx (List.map (simp env) (Iarr.to_list xs))
    | Ite (c, a, b) ->
      let c' = simp env c in
      (match c'.node with
       | Bool_const true -> simp env a
       | Bool_const false -> simp env b
       | App _ | Int_const _ | Arith _ | Le _ | Eq _ | Not _ | And _ | Or _ | Ite _ ->
         let env_then = assume env c' ~truth:true in
         let env_else = assume env c' ~truth:false in
         let a' = simp env_then a in
         let b' = simp env_else b in
         Context.ite ctx c' a' b')
  in
  let emit ~aborted =
    match Sys.getenv_opt "OXSMT_PRESOLVE_CTX_STATS" with
    | Some ("1" | "true" | "yes") ->
      Printf.eprintf
        "ctx: fired=%d substitutions=%d atomfolds=%d steps=%d aborted=%d\n%!"
        (if !engaged then 1 else 0)
        !subst_hits
        !atom_folds
        !steps
        (if aborted then 1 else 0)
    | Some _ | None -> ()
  in
  match
    List.map
      (fun a ->
         if has_ite a
         then (
           engaged := true;
           simp root a)
         else a)
      assertions
  with
  | exception Ctx_budget ->
    emit ~aborted:true;
    assertions
  | simplified ->
    emit ~aborted:false;
    simplified
;;

(* --- Equality-over-ITE projection (task #34) ----------------------------------------- A
   LOCAL, model-PRESERVING rewrite on the shared hash-consed DAG — a DIFFERENT algorithm
   from [simplify_contextual] above (which tracks a per-branch assumption context and was
   measured net-negative on these families: it under-fired the [(= v_i v_j)]/[(not bool)]
   conditions and its per-ITE contextual descent exceeded the wall). This pass tracks NO
   context; it applies three pure ITE identities bottom-up and memoizes over the DAG:

   1. EQUALITY-OVER-ITE PROJECTION (the nec-smt lever): [(= (ite c x y) d)] ->
      [(ite c (= x d) (= y d))] when [d] is a leaf (constant or bare variable) — symmetric
      in the operands. Exact ITE equivalence: total ITE semantics give
      [(= (c?x:y) d) = (c ? (x=d) : (y=d))] in every model. Restricting [d] to a leaf
      keeps each distributed equality size-1-bounded (no duplication of a large RHS). This
      is precisely what oxsmt's diagnosed failure needs: a nec condition
      [(= chain_ite literal)] — which oxsmt would otherwise Tseitin into a FRESH aux
      boolean disconnected from the real variables — becomes a boolean function of the
      ORIGINAL condition atoms (each leaf [(= const literal)] folds, [(ite c true false)]
      collapses to [c]), so the SAT core sees atoms over the real variables instead of
      thousands of opaque aux vars. (Confirmed on the exemplars: z3 4.8.5 collapses these
      to mk-bool-var 1 in preprocessing; the projection reproduces that collapse for
      oxsmt.)

   2. BOOLEAN-ITE COLLAPSE: the {!Node} [ite] constructor folds a constant condition and
      equal branches but NOT [(ite c true false) -> c] / [(ite c false true) -> (not c)] /
      the one-constant-branch and/or forms, which are exactly what a projected Bool-sorted
      ITE produces once its branch equalities fold to constants. [bool_ite] applies them
      so the projection actually collapses to a boolean over [c] rather than leaving an
      opaque Bool-sorted [ite] node (which would get its own Tseitin var).

   3. SELECTOR COLLAPSE (local, depth-1): [(ite c a b)] where a branch is itself an [ite]
      on the SAME condition [c] (or its complement [(not c)]) drops the dead sub-branch:
      [(ite c (ite c a2 _) b) -> (ite c a2 b)], [(ite c a (ite c _ b2)) -> (ite c a b2)],
      and the complement variants. Purely local (no assumption tracking) — the branch of
      [c] fixes the nested same-condition [ite]'s selector.

   SOUNDNESS: all three are logical EQUIVALENCES (not merely equisatisfiable) and
   eliminate NO variable, so — like [simplify_contextual] — no [def]/model-reconstruction
   machinery is needed and the R1 self-check (over the ORIGINAL asserted terms) is
   untouched. The win direction is UNSAT (R1 does not run), so the discriminating tests
   are the margin; but unlike the contextual pass there is no scope-leak surface (these
   are context-free local identities). All terms are built through [ctx]'s smart
   constructors (well-sorted, hash-consed). Determinism (I6): a deterministic function of
   term structure.

   TERMINATION / BUDGET: memoized on the hash-cons tag (main walk) and on the
   [(ite-tag, leaf-tag)] pair (the projection), so each distinct node/projection is done
   once. A deep single ITE spine compared against many distinct literals still makes the
   count of distinct [(subterm, literal)] projections super-linear (O(depth^2) new terms
   on the pathological single-chain shape), which — even collapsed for search — is too
   large to clausify within the wall. A hard step budget with NEUTRAL ABORT (return the
   input list unchanged — never a partial rewrite) caps that worst case at OFF-equivalent
   behaviour: on abort the projected terms are discarded BEFORE any is internalized, so
   the only ON cost on an aborted file is the bounded projection work (about
   [proj_max_steps] hash-cons ops), never a blown-up clausification. Measured knee: the
   med nec exemplar wins at ~9K steps; the 15,847-ITE bftpd worst case needs ~3.7M (and
   clausifies for >90s if allowed to complete) — 500K separates them, admitting the bushy
   wins and aborting the deep-chain losses (which are losses at the 2s wall regardless). *)

let proj_max_steps_default = 500_000

(* Env-overridable (OXSMT_PRESOLVE_PROJ_MAX_STEPS) for budget tuning / deep-chain
   profiling without a recompile; falls back to [proj_max_steps_default] on
   absent/unparseable. Read once per call (cheap). *)
let proj_max_steps () =
  match Sys.getenv_opt "OXSMT_PRESOLVE_PROJ_MAX_STEPS" with
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n > 0 -> n
     | Some _ | None -> proj_max_steps_default)
  | None -> proj_max_steps_default
;;

exception Proj_budget

(* A leaf admissible as the non-ITE side of a projected equality: a constant, or a bare
   nullary variable. Restricting the distributed side to a leaf keeps [(= x d)]/[(= y d)]
   size-1-bounded so the projection cannot blow up term size. *)
let proj_is_leaf (t : Term.t) =
  match t.node with
  | Int_const _ | Bool_const _ -> true
  | App (_, args) -> Iarr.length args = 0
  | Arith _ | Le _ | Eq _ | Not _ | And _ | Or _ | Ite _ -> false
;;

(* Build a Bool-sorted [(ite c x y)] collapsing the forms the {!Node} constructor leaves
   alone: constant condition, constant branches ([(ite c true false) -> c] etc.), and a
   single-constant branch (to [or]/[and]). [c], [x], [y] are already simplified. *)
let bool_ite ctx (c : Term.t) (x : Term.t) (y : Term.t) =
  match c.node with
  | Bool_const true -> x
  | Bool_const false -> y
  | _ ->
    (match x.node, y.node with
     | Bool_const true, Bool_const false -> c
     | Bool_const false, Bool_const true -> Context.not_ ctx c
     | Bool_const true, _ -> Context.or_ ctx [ c; y ]
     | Bool_const false, _ -> Context.and_ ctx [ Context.not_ ctx c; y ]
     | _, Bool_const true -> Context.or_ ctx [ Context.not_ ctx c; x ]
     | _, Bool_const false -> Context.and_ ctx [ c; x ]
     | _ -> Context.ite ctx c x y)
;;

let simplify_projection ctx assertions =
  let steps = ref 0 in
  let max_steps = proj_max_steps () in
  let tick () =
    incr steps;
    if !steps > max_steps then raise Proj_budget
  in
  (* Observability-only counters (OXSMT_PRESOLVE_PROJ_STATS): [engaged] = the walk ran on
     an ITE-bearing assertion; [projections] = equality-over-ITE projections applied
     (distinct (ite,leaf) pairs); [collapses] = local selector collapses. Emitted once per
     call to stderr, default-silent; never read back into the solve path. *)
  let engaged = ref false in
  let projections = ref 0 in
  let collapses = ref 0 in
  (* Main walk memo (per-Context hash-cons tag) and the projection memo keyed on the
     (ite-node tag, leaf tag) pair. Both LOCAL to this call: the tag is the per-[Context]
     hash-cons identity, so a table shared across [Context]s (one session per file) would
     collide tags and return stale answers (a module-global table silently corrupts the
     pass — the same footgun the contextual pass documents). *)
  let memo : Term.t Term.Table.t = Term.Table.create 4096 in
  let proj_memo : (int * int, Term.t) Hashtbl.t = Hashtbl.create 4096 in
  let rec go (t : Term.t) : Term.t =
    match Term.Table.find_opt memo t with
    | Some r -> r
    | None ->
      tick ();
      let r = rewrite t in
      Term.Table.add memo t r;
      r
  and rewrite (t : Term.t) : Term.t =
    match t.node with
    | Bool_const _ | Int_const _ -> t
    | App (sym, args) ->
      if Iarr.length args = 0
      then t (* a variable — unchanged (preserves hash-cons identity) *)
      else Context.app ctx sym (List.map go (Iarr.to_list args))
    | Arith lin ->
      Context.linear_combination_big
        ctx
        (List.map (fun (tm, co) -> co, go tm) (Iarr.to_list lin.coeffs))
        lin.const
    | Le a -> Context.le ctx (go a) (Context.int_const ctx 0)
    | Not a -> Context.not_ ctx (go a)
    | And xs -> Context.and_ ctx (List.map go (Iarr.to_list xs))
    | Or xs -> Context.or_ ctx (List.map go (Iarr.to_list xs))
    | Eq (a, b) -> rewrite_eq (go a) (go b)
    | Ite (c, a, b) -> rewrite_ite (go c) (go a) (go b)
  and rewrite_eq (a : Term.t) (b : Term.t) : Term.t =
    (* Project when one side is an [Ite] and the other a leaf; else the plain equality. *)
    match a.node, b.node with
    | Ite _, _ when proj_is_leaf b -> project a b
    | _, Ite _ when proj_is_leaf a -> project b a
    | _ -> Context.eq ctx a b
  and project (ite_t : Term.t) (d : Term.t) : Term.t =
    let key = ite_t.tag, d.tag in
    match Hashtbl.find_opt proj_memo key with
    | Some r -> r
    | None ->
      tick ();
      incr projections;
      let r =
        match ite_t.node with
        | Ite (c, x, y) -> bool_ite ctx c (rewrite_eq x d) (rewrite_eq y d)
        | _ -> Context.eq ctx ite_t d (* unreachable: caller guarantees an Ite *)
      in
      Hashtbl.add proj_memo key r;
      r
  and rewrite_ite (c : Term.t) (a : Term.t) (b : Term.t) : Term.t =
    match c.node with
    | Bool_const true -> a
    | Bool_const false -> b
    | _ ->
      (* Local selector collapse: within the [c]-true (then) branch a nested [ite] on the
         same [c] takes its then-branch and on [(not c)] its else-branch; symmetrically in
         the else branch. A depth-1 rewrite (no assumption tracking). *)
      let notc = Context.not_ ctx c in
      let a' =
        match a.node with
        | Ite (c2, a2, _b2) when c2.tag = c.tag ->
          incr collapses;
          a2
        | Ite (c2, _a2, b2) when c2.tag = notc.tag ->
          incr collapses;
          b2
        | _ -> a
      in
      let b' =
        match b.node with
        | Ite (c2, _a2, b2) when c2.tag = c.tag ->
          incr collapses;
          b2
        | Ite (c2, a2, _b2) when c2.tag = notc.tag ->
          incr collapses;
          a2
        | _ -> b
      in
      Context.ite ctx c a' b'
  in
  let emit ~aborted =
    match Sys.getenv_opt "OXSMT_PRESOLVE_PROJ_STATS" with
    | Some ("1" | "true" | "yes") ->
      Printf.eprintf
        "proj: fired=%d projections=%d collapses=%d steps=%d aborted=%d\n%!"
        (if !engaged then 1 else 0)
        !projections
        !collapses
        !steps
        (if aborted then 1 else 0)
    | Some _ | None -> ()
  in
  (* Only walk assertions that contain an [Ite] (the projection can only fire under one);
     an ITE-free assertion is returned untouched, so the pass is exactly neutral (a cheap
     structural check) on pure-arith / propagation-bound files where it never wins.
     MEMOIZED over the DAG (the assertion is a maximally-shared [let]-DAG; a naive
     per-path walk is exponential), and its visits count against the shared budget. Table
     LOCAL to this call (per-[Context] tag identity). *)
  let has_ite_table : bool Term.Table.t = Term.Table.create 4096 in
  let rec has_ite (t : Term.t) =
    match Term.Table.find_opt has_ite_table t with
    | Some b -> b
    | None ->
      tick ();
      let b =
        match t.node with
        | Ite _ -> true
        | Bool_const _ | Int_const _ -> false
        | App (_, args) -> Iarr.exists has_ite args
        | Arith lin -> Iarr.exists (fun (tm, _c) -> has_ite tm) lin.coeffs
        | Le a | Not a -> has_ite a
        | Eq (a, b) -> has_ite a || has_ite b
        | And xs | Or xs -> Iarr.exists has_ite xs
      in
      Term.Table.add has_ite_table t b;
      b
  in
  match
    List.map
      (fun a ->
         if has_ite a
         then (
           engaged := true;
           go a)
         else a)
      assertions
  with
  | exception Proj_budget ->
    emit ~aborted:true;
    assertions
  | simplified ->
    emit ~aborted:false;
    simplified
;;

(* --- Symmetry breaking (task #25, quf-symmetry-experiment.md §6) ----------------------
   Detect interchangeable same-sort constants — constants whose pairwise transposition
   maps the asserted set to itself — and emit sound, full-action generator-based
   LEX-LEADER symmetry-breaking constraints over the ORIGINAL vocabulary (theory
   equalities only). NO value precedence: the offline experiment PROVED value precedence
   unsound here (the constants appear as both function ARGUMENTS and VALUES — index+value
   symmetry — so value precedence can eliminate every representative of a SAT orbit,
   producing SAT→UNSAT flips). See [[symmetry-unsat-losses-mask-unsoundness]].

   {b General / structural — no family or filename logic.} Detection is a bounded
   graph-automorphism-style invariant: constants of one sort are refined by a cheap
   occurrence signature, then each candidate transposition is CONFIRMED EXACTLY by
   rebuilding every conjunct under the swap through [ctx]'s smart constructors (which
   flatten/dedup/tag-sort [and]/[or] and tag-order [eq], so the hash-consed tag IS the
   AC-canonical form) and comparing the resulting tag multiset to the original. Only a
   confirmed transposition is ever used. Interchangeable classes are the connected
   components of the confirmed-transposition graph (adjacent transpositions generate the
   symmetric group on a component).

   {b Soundness of the emission.} For a class {c_0..c_{k-1}} and each adjacent generator
   g = (c_i c_{i+1}) (a confirmed symmetry), we require the assignment A to be
   lexicographically <= g(A) over a fixed-ordered atom sequence a_1..a_n (the equalities
   [(= cell c_v)] for application cells of the class sort and class values c_v), with
   b_j = g(a_j) computed by rebuilding a_j under g. false<true, encoded with an O(n)
   prefix-equal chain whose "prefix so far" is carried by fresh reserved [.oxsmt.sym.*]
   nullary Bool aux vars ([fresh_bool]). The aux var is OPAQUE, so [and (aux) atom] is a
   2-input node that the AC-flattening smart constructor does NOT re-expand — a shared
   [and]-term chain would instead re-flatten to O(n^2) clauses (measured: it tanks the
   A/B). A ⪯ g(A) keeps >=1 representative per orbit ⇒ SAT-preserving; adding constraints ⇒
   UNSAT-preserving; sound for any fixed atom sequence (closure under g is NOT required —
   b_j is used as a plain Bool term).

   {b Size cap (sat-safe).} Emission is SKIPPED for classes of size >= [symbreak_emit_max]
   (default 6): the offline A/B showed size-6/7 classes regress the satisfiable instances
   they touch (heavy encoding slows model-finding) while contributing ~0 conversions. The
   captured win is the size-2..5 UNSAT tail.

   {b Equisatisfiable, not equivalent.} These constraints REMOVE (symmetric) models, so —
   like {!entailed_equalities} — the caller must NOT record them in the R1 self-check set
   (which evaluates the ORIGINAL assertions; any found model still satisfies them) and must
   gate the pass cert-OFF (a lex-leader clause is not resolution-derivable). Pure and
   deterministic (I6): all iteration is in term-tag order. Neutral-abort (returns [[]],
   i.e. no breaking) on any hard budget. All terms are built through [ctx]. *)

(* Detection: bound the total exact-check work (each check rebuilds the conjunct DAG). *)
let symbreak_max_steps = 4_000_000

(* Skip a same-signature bucket larger than this before running pairwise exact checks —
   O(bucket^2) checks would dominate on large symmetric sets (e.g. eq_diamond). A bucket
   this large cannot yield an emittable class anyway (emit cap below). *)
let symbreak_consider_max = 8

(* Size cap (sat-safe): emit lex-leader only for classes of size in [2, symbreak_emit_max). *)
let symbreak_emit_max = 6

(* Cap the lex atom sequence per class (a longer sequence only strengthens the break; we
   truncate deterministically in tag order — still sound). *)
let symbreak_atoms_max = 512

(* Cap total emitted breaking constraints across all classes; neutral-abort beyond. *)
let symbreak_constraints_max = 200_000

exception Symbreak_budget

(* Rebuild every term in [terms] under the leaf substitution [sigma] (constant-term ->
   constant-term), through [ctx] so the result is AC-canonical hash-consed. A substituted
   leaf is returned directly (NOT re-substituted — [sigma] is an involution on leaves, so
   recursion would not terminate). Memoized on the hash-cons tag. [bump] charges the DAG
   walk against the global budget. *)
let symbreak_rebuild ctx sigma bump terms =
  let memo = Term.Table.create 256 in
  let rec go (t : Term.t) =
    match Term.Table.find_opt memo t with
    | Some r -> r
    | None ->
      bump ();
      let r =
        match Term.Map.find_opt t sigma with
        | Some target -> target
        | None ->
          (match t.node with
           | Bool_const b -> Context.bool_const ctx b
           | Int_const n -> Context.int_const_big ctx n
           | App (sym, args) ->
             if Iarr.length args = 0
             then t
             else Context.app ctx sym (List.map go (Iarr.to_list args))
           | Arith lin ->
             Context.linear_combination_big
               ctx
               (List.map (fun (tm, co) -> co, go tm) (Iarr.to_list lin.coeffs))
               lin.const
           | Le a -> Context.le ctx (go a) (Context.int_const ctx 0)
           | Eq (a, b) -> Context.eq ctx (go a) (go b)
           | Not a -> Context.not_ ctx (go a)
           | And xs -> Context.and_ ctx (List.map go (Iarr.to_list xs))
           | Or xs -> Context.or_ ctx (List.map go (Iarr.to_list xs))
           | Ite (c, a, b) -> Context.ite ctx (go c) (go a) (go b))
      in
      Term.Table.add memo t r;
      r
  in
  List.map go terms
;;

(* Multiset key of a conjunct list: the sorted tag list (hash-cons tag = AC-canonical id). *)
let symbreak_tag_multiset (terms : Term.t list) =
  List.sort Int.compare (List.map (fun (t : Term.t) -> t.Term.tag) terms)
;;

(* [symmetry_break ctx assertions] — see the block comment. Returns extra top-level Bool
   constraints (the lex-leader clauses) to internalize alongside the assertions; [[]] when
   nothing is broken (byte-neutral to a no-symmetry input). *)
let symmetry_break ~counter cap env ctx assertions =
  let conjuncts = List.concat_map flatten assertions in
  let base_key = symbreak_tag_multiset conjuncts in
  let steps = ref 0 in
  let bump () =
    incr steps;
    if !steps > symbreak_max_steps then raise Symbreak_budget
  in
  (* Fresh reserved nullary Bool aux var for the lex prefix-equal chain (an O(n) encoding
     — the aux var is opaque, so [and (aux) atom] is a 2-input And that the AC-flattening
     smart constructor does NOT re-expand, unlike a shared [and]-term which would blow up
     to O(n^2) clauses). Kind ["sym"] keeps the namespace disjoint from preprocessing's
     fresh symbols. [counter] is a PER-SESSION monotone ref (F2): a second call must not
     reuse [.oxsmt.sym.0] — [Env.declare_reserved] is idempotent, so reuse would rebind
     one name to a second, conflicting definition (wrong-unsat). Deterministic (I6): a
     fixed call sequence yields identical names. *)
  let fresh_bool () =
    let name = Printf.sprintf "%ssym.%d" Env.reserved_prefix !counter in
    incr counter;
    let sym = Env.declare_reserved cap env name (Rank.create [] Sort.bool) in
    Context.app ctx sym []
  in
  (* is (a b) a genuine symmetry? rebuild all conjuncts under the swap and compare. *)
  let is_symmetry a b =
    let sigma = Term.Map.add a b (Term.Map.singleton b a) in
    let swapped = symbreak_rebuild ctx sigma bump conjuncts in
    List.equal Int.equal (symbreak_tag_multiset swapped) base_key
  in
  try
    (* 1. Harvest distinct nullary constants (non-Bool sort) and application cells; record
          a cheap per-constant occurrence signature (parent App symbol-hash + arg
          position). *)
    let const_sig = Term.Table.create 256 in
    (* term -> (sig accumulator) *)
    let consts = ref Term.Set.empty in
    let cells = ref Term.Set.empty in
    let add_sig c key =
      let prev =
        try Term.Table.find const_sig c with
        | Not_found -> []
      in
      Term.Table.replace const_sig c (key :: prev)
    in
    let seen = Term.Table.create 512 in
    let rec harvest (t : Term.t) =
      if not (Term.Table.mem seen t)
      then (
        Term.Table.add seen t ();
        bump ();
        match t.node with
        | App (_, args) when Iarr.length args = 0 ->
          if not (Sort.equal t.sort Sort.bool) then consts := Term.Set.add t !consts
        | App (sym, args) ->
          if not (Sort.equal t.sort Sort.bool) then cells := Term.Set.add t !cells;
          Iarr.iteri
            (fun i a ->
               (match a.Term.node with
                | App (_, aargs) when Iarr.length aargs = 0 ->
                  if not (Sort.equal a.Term.sort Sort.bool)
                  then add_sig a (Symbol.hash sym, i)
                | _ -> ());
               harvest a)
            args
        | Arith lin -> Iarr.iter (fun (tm, _c) -> harvest tm) lin.coeffs
        | Le a | Not a -> harvest a
        | Eq (a, b) ->
          harvest a;
          harvest b
        | And xs | Or xs -> Iarr.iter harvest xs
        | Ite (c, a, b) ->
          harvest c;
          harvest a;
          harvest b
        | Bool_const _ | Int_const _ -> ())
    in
    List.iter harvest conjuncts;
    let sig_of c =
      List.sort
        compare
        (try Term.Table.find const_sig c with
         | Not_found -> [])
    in
    (* 2. Group constants by sort, then refine each sort-group by signature into buckets.
       Grouping is by [Sort.equal], NOT [Sort.hash] alone (F3): [Sort.hash] is
       non-injective (e.g. distinct Uninterpreted/BitVec sorts can collide), and a
       cross-sort candidate pair would drive [Context.eq] to raise [Term.Sort_error] out
       of the pass. #distinct sorts is small, so equal-grouping is cheap. *)
    let const_list = Term.Set.elements !consts in
    (* term-tag order is Term.Set.elements order (Set uses Term.compare = tag). *)
    let sorts =
      List.fold_left
        (fun acc c ->
           if List.exists (Sort.equal c.Term.sort) acc then acc else acc @ [ c.Term.sort ])
        []
        const_list
    in
    let cell_list = Term.Set.elements !cells in
    let out = ref [] in
    let n_constraints = ref 0 in
    (* one group per distinct sort (tag order preserved by [filter]); deterministic
       sort-group order by the min constant tag in the group. *)
    let sort_groups =
      List.map (fun s -> List.filter (fun c -> Sort.equal c.Term.sort s) const_list) sorts
      |> List.sort (fun g1 g2 ->
        match g1, g2 with
        | c1 :: _, c2 :: _ -> Int.compare c1.Term.tag c2.Term.tag
        | _ -> 0)
    in
    List.iter
      (fun group ->
         (* refine by signature *)
         let buckets = Hashtbl.create 16 in
         List.iter
           (fun c ->
              let key = sig_of c in
              let prev =
                try Hashtbl.find buckets key with
                | Not_found -> []
              in
              Hashtbl.replace buckets key (c :: prev))
           group;
         let bucket_list =
           Hashtbl.fold (fun _ v acc -> List.rev v :: acc) buckets []
           |> List.sort (fun b1 b2 ->
             match b1, b2 with
             | c1 :: _, c2 :: _ -> Int.compare c1.Term.tag c2.Term.tag
             | _ -> 0)
         in
         List.iter
           (fun bucket ->
              let n = List.length bucket in
              if n >= 2 && n <= symbreak_consider_max
              then (
                let arr = Array.of_list bucket in
                (* arr is in tag order (bucket built from tag-ordered group, rev preserves) *)
                Array.sort (fun a b -> Int.compare a.Term.tag b.Term.tag) arr;
                (* union-find over confirmed-transposition graph *)
                let parent = Array.init n (fun i -> i) in
                let rec find i = if parent.(i) = i then i else find parent.(i) in
                let union i j =
                  let ri = find i
                  and rj = find j in
                  if ri <> rj then parent.(ri) <- rj
                in
                for i = 0 to n - 1 do
                  for j = i + 1 to n - 1 do
                    if find i <> find j && is_symmetry arr.(i) arr.(j) then union i j
                  done
                done;
                (* collect components in tag order *)
                let comp = Hashtbl.create 16 in
                for i = 0 to n - 1 do
                  let r = find i in
                  let prev =
                    try Hashtbl.find comp r with
                    | Not_found -> []
                  in
                  Hashtbl.replace comp r (arr.(i) :: prev)
                done;
                let classes =
                  Hashtbl.fold (fun _ v acc -> List.rev v :: acc) comp []
                  |> List.filter (fun cs -> List.length cs >= 2)
                  |> List.sort (fun a b ->
                    match a, b with
                    | c1 :: _, c2 :: _ -> Int.compare c1.Term.tag c2.Term.tag
                    | _ -> 0)
                in
                List.iter
                  (fun consts_in_class ->
                     let k = List.length consts_in_class in
                     (* SIZE CAP (sat-safe): emit only for 2 <= k < symbreak_emit_max. *)
                     if k >= 2 && k < symbreak_emit_max
                     then (
                       let cs = Array.of_list consts_in_class in
                       (* class sort *)
                       let csort = cs.(0).Term.sort in
                       (* atoms = (= cell c_v) for cells of the class sort, values in class;
                       fixed order: (cell tag, value tag), truncated to the atom cap. *)
                       let class_cells =
                         List.filter (fun c -> Sort.equal c.Term.sort csort) cell_list
                       in
                       let atoms = ref [] in
                       let natoms = ref 0 in
                       (try
                          List.iter
                            (fun cell ->
                               Array.iter
                                 (fun v ->
                                    if !natoms >= symbreak_atoms_max then raise Exit;
                                    atoms := Context.eq ctx cell v :: !atoms;
                                    incr natoms)
                                 cs)
                            class_cells
                        with
                        | Exit -> ());
                       let atoms = List.rev !atoms in
                       (* full-action lex-leader for each adjacent generator (c_i c_[{i+1}]).
                       The lex constraint A <= g(A) over the atom sequence, false<true,
                       encoded with an O(n) prefix-equal chain of fresh aux vars: p_0 =
                       true; for each moved atom a_j (image b_j): (p_j /\ a_j) -> b_j
                       [lex step] p_[{j+1}] <-> (p_j /\ (a_j = b_j)) [prefix advance] *)
                       for i = 0 to k - 2 do
                         let a = cs.(i)
                         and b = cs.(i + 1) in
                         let sigma = Term.Map.add a b (Term.Map.singleton b a) in
                         let images = symbreak_rebuild ctx sigma bump atoms in
                         (* the atoms actually MOVED by g (a_j <> b_j); fixed atoms
                         contribute a vacuous step and no prefix change, so they are
                         dropped soundly. *)
                         let seq =
                           List.rev
                             (List.fold_left2
                                (fun acc atom image ->
                                   if Term.equal atom image
                                   then acc
                                   else (atom, image) :: acc)
                                []
                                atoms
                                images)
                         in
                         let m = List.length seq in
                         let prefix = ref (Context.bool_const ctx true) in
                         List.iteri
                           (fun j (atom, image) ->
                              (* (prefix /\ atom) -> image *)
                              let lhs = Context.and_ ctx [ !prefix; atom ] in
                              out
                              := Context.or_ ctx [ Context.not_ ctx lhs; image ] :: !out;
                              incr n_constraints;
                              if !n_constraints > symbreak_constraints_max
                              then raise Symbreak_budget;
                              (* advance the prefix through a fresh aux var, except after the
                             last step (its value is never read). *)
                              if j < m - 1
                              then (
                                let p = fresh_bool () in
                                let def =
                                  Context.and_ ctx [ !prefix; Context.eq ctx atom image ]
                                in
                                out := Context.eq ctx p def :: !out;
                                incr n_constraints;
                                prefix := p))
                           seq
                       done))
                  classes))
           bucket_list)
      sort_groups;
    List.rev !out
  with
  | Symbreak_budget -> []
;;
