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
      | Int_const n -> Context.int_const ctx n
      | App (sym, args) ->
        if Iarr.length args = 0
        then t (* a surviving variable — unchanged (preserves hash-cons identity) *)
        else Context.app ctx sym (List.map subst (Iarr.to_list args))
      | Arith lin ->
        Context.linear_combination
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
