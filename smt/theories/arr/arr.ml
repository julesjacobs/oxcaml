(* The arrays theory as an e-graph client. See arr.mli for the contract.

   The theory owns a [prem Euf.t] engine: select/store are App nodes, so the engine gives
   congruence closure over them for free. On top of that substrate this module enforces
   the read-over-write and extensionality axioms. The select/store symbol classification
   (which App head is a select vs a store, over which index/element sorts) is read from
   {!Oxsmt_core.Array_defs}; the theory grows its own copy when it mints a fresh [select]
   for a ROW step.

   THEORY boilerplate (atom<->term bookkeeping, watches, propagation-reason caching,
   pop-frame reason dropping) mirrors {!Oxsmt_dt.Dt} / Euf_adapter. *)

open Oxsmt_core
module Euf = Oxsmt_euf.Euf
module Defs = Array_defs

(* The engine's ['p]. A real assertion carries [P_lit]; the standing [true <> false]
   disequality carries [P_axiom]; a ROW-derived equality carries [P_derived reasons] — the
   trail literals whose conjunction entails the trigger. [P_axiom] is dropped when
   building an Explanation so a returned premise set holds only asserted literals. *)
type prem =
  | P_lit of Lit.t
  | P_axiom
  | P_derived of Lit.t list

type kind =
  | K_eq of Term.t * Term.t
  | K_bool
  | K_foreign

type info =
  { term : Term.t
  ; kind : kind
  }

type t =
  { ctx : Context.t
  ; env : Env.t
  ; cap : Env.reserved_cap
    (* ADR-0012 R1 reserved-minting capability for [env], threaded from the session (the
         sole holder). The arrays theory is a legitimate reserved-symbol minter — like
         preprocessing and the lemma tier — because its extensionality rule introduces
         FRESH witness indices mid-solve, which MUST be unforgeable (see [witness_index]). *)
  ; mutable reg : Defs.t (* select/store classification; grows as we mint fresh selects *)
  ; engine : prem Euf.t
  ; true_const : Term.t
  ; false_const : Term.t
  ; atoms : info Atom.Table.t
  ; watched : Atom.t Term.Table.t (* watched Eq/predicate term -> atom, for propagation *)
  ; mutable atom_terms : Term.t list (* registration order; model enumeration *)
  ; seen_cat : unit Term.Table.t
  ; mutable store_terms : Term.t list (* registered store applications, rev order *)
  ; mutable select_terms : Term.t list (* registered select applications, rev order *)
  ; select_syms :
      (string, Symbol.t) Hashtbl.t (* minted select symbols, by canonical name *)
  ; ensured_reads : (int * int, unit) Hashtbl.t
    (* (store term tag, index term tag) pairs for which [select store index] has already
         been materialized by the upward read-propagation rule ([ensure_store_reads]).
         Guards termination: each (store, index) pair triggers at most one fresh select,
         and a fresh select's index is drawn from an existing select, so no new index is
         ever created. Monotonic like [select_terms] (terms persist across pop; only
         e-graph merges are backtracked). *)
  ; ext_witness : (int * int, Term.t) Hashtbl.t
    (* fresh extensionality witness index per asserted array-diseq pair (by term tags,
         orientation-normalized), reused so re-assertion after a pop is stable *)
  ; mutable fresh_counter : int
  ; mutable explain_cache : Explanation.t Lit.Map.t
  ; mutable frames : Lit.t list list
  }

let max_iters = 1_000_000

let create ctx env cap reg =
  let engine = Euf.create ctx in
  let true_const = Context.bool_const ctx true in
  let false_const = Context.bool_const ctx false in
  Euf.register_term engine true_const;
  Euf.register_term engine false_const;
  Euf.assert_neq engine ~premise:P_axiom true_const false_const;
  { ctx
  ; env
  ; cap
  ; reg
  ; engine
  ; true_const
  ; false_const
  ; atoms = Atom.Table.create 64
  ; watched = Term.Table.create 64
  ; atom_terms = []
  ; seen_cat = Term.Table.create 128
  ; store_terms = []
  ; select_terms = []
  ; select_syms = Hashtbl.create 16
  ; ensured_reads = Hashtbl.create 64
  ; ext_witness = Hashtbl.create 32
  ; fresh_counter = 0
  ; explain_cache = Lit.Map.empty
  ; frames = [ [] ]
  }
;;

(* --- small helpers --- *)

let head_args (term : Term.t) : (Symbol.t * Term.t array) option =
  match term.Term.node with
  | Term.App (s, args) -> Some (s, Array.of_list (Iarr.to_list args))
  | _ -> None
;;

let role_of t (term : Term.t) : Defs.entry option =
  match head_args term with
  | Some (sym, _) -> Defs.role_of_sym t.reg sym
  | None -> None
;;

let array_sort (sort : Sort.t) : (Sort.t * Sort.t) option =
  match sort with
  | Sort.Array (i, e) -> Some (i, e)
  | Sort.Bool | Sort.Int _ | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.BitVec _ ->
    None
;;

let rec lits_of_prem = function
  | P_lit l -> [ l ]
  | P_axiom -> []
  | P_derived ls -> ls

and lits_of_prems ps = List.concat_map lits_of_prem ps

let dedup_lits (ls : Lit.t list) : Lit.t list =
  let seen = ref Lit.Map.empty in
  List.filter
    (fun l ->
       if Lit.Map.mem l !seen
       then false
       else (
         seen := Lit.Map.add l () !seen;
         true))
    ls
;;

(* --- minting fresh terms (select for a ROW step, a witness index for extensionality) --- *)

(* Get-or-mint the [select] symbol for one (index, element) instantiation. Uses the SAME
   canonical name the parser uses ({!Array_defs.op_symbol_name}), so a fresh [select] the
   theory builds hash-conses with any [select] already in the problem; records it in [reg]
   so [role_of] classifies it. *)
let select_sym t ~index ~element : Symbol.t =
  let name = Defs.op_symbol_name Defs.Select ~index ~element in
  match Hashtbl.find_opt t.select_syms name with
  | Some sym -> sym
  | None ->
    let arr = Sort.array_ ~index ~element in
    (* board #58: the op name is a reserved [.oxsmt.arr.*] symbol with [|] sort-key
       separators, so it must be minted through the cap door (the same [cap] the
       ext-witness Skolem uses), not [Env.declare_fun] — the public door rejects both the
       prefix and the byte class. *)
    let sym =
      Env.declare_reserved t.cap t.env name (Rank.create [ arr; index ] element)
    in
    Hashtbl.replace t.select_syms name sym;
    t.reg <- Defs.add t.reg sym Defs.Select ~index ~element;
    sym
;;

(* --- registration: catalog select/store while registering into the e-graph --- *)

let rec catalog t (term : Term.t) =
  if not (Term.Table.mem t.seen_cat term)
  then (
    Term.Table.replace t.seen_cat term ();
    match head_args term with
    | Some (_, args) ->
      (match role_of t term with
       | Some { Defs.role = Defs.Store; _ } -> t.store_terms <- term :: t.store_terms
       | Some { Defs.role = Defs.Select; _ } -> t.select_terms <- term :: t.select_terms
       | None -> ());
      Array.iter (catalog t) args
    | None ->
      (match term.Term.node with
       | Term.Eq (a, b) ->
         catalog t a;
         catalog t b
       | Term.Not a | Term.Le a -> catalog t a
       | Term.And xs | Term.Or xs -> List.iter (catalog t) (Iarr.to_list xs)
       | Term.Ite (a, b, c) ->
         catalog t a;
         catalog t b;
         catalog t c
       | Term.Arith lin ->
         List.iter (fun (c, _) -> catalog t c) (Iarr.to_list lin.Term.coeffs)
       | Term.Bool_const _ | Term.Int_const _ -> ()
       | Term.App _ -> ()))
;;

(* [build_select t base j] is the (registered, cataloged) term [select base j] for a ROW
   step, where [base] has an array sort. *)
let build_select t (base : Term.t) (j : Term.t) : Term.t option =
  match array_sort base.Term.sort with
  | None -> None
  | Some (index, element) ->
    let sym = select_sym t ~index ~element in
    let sel = Context.app t.ctx sym [ base; j ] in
    Euf.register_term t.engine sel;
    catalog t sel;
    Some sel
;;

let classify (term : Term.t) : kind =
  if not (Theory_view.is_atom term)
  then invalid_arg "Arr.register_atom: term is not a theory atom";
  match Theory_view.atom term with
  | Theory_view.Equality (a, b) -> K_eq (a, b)
  | Theory_view.Predicate (_, _) | Theory_view.Bool_lit _ -> K_bool
  | Theory_view.Le_zero _ -> K_foreign
;;

let register_atom t atom term =
  Euf.register_term t.engine term;
  catalog t term;
  if not (Atom.Table.mem t.atoms atom)
  then (
    let kind = classify term in
    Atom.Table.replace t.atoms atom { term; kind };
    t.atom_terms <- term :: t.atom_terms;
    match kind with
    | K_eq _ -> Term.Table.replace t.watched term atom
    | K_bool ->
      (match term.Term.node with
       | Term.App (_, args) when Iarr.length args >= 1 ->
         Term.Table.replace t.watched term atom;
         Euf.rearm_watch t.engine term
       | _ -> ())
    | K_foreign -> ())
;;

(* --- extensionality: a <> b (arrays) => fresh k, select a k <> select b k --- *)

let witness_index t (a : Term.t) (b : Term.t) (index : Sort.t) : Term.t =
  let ta = a.Term.tag
  and tb = b.Term.tag in
  let key = if ta <= tb then ta, tb else tb, ta in
  match Hashtbl.find_opt t.ext_witness key with
  | Some w -> w
  | None ->
    (* A fresh nullary index constant for the extensionality Skolem. It MUST be
       unforgeable: if the same symbol could be named elsewhere, an equality asserted at
       this very index would turn our [select a k <> select b k] into a false conflict — a
       wrong-UNSAT. The reserved [".oxsmt.arr.ext."] namespace closes the EXTERNAL door
       (parsed input / public [declare_fun] / raw [Env] can never produce a [.oxsmt.*]
       symbol with a rank), NOT lexical illegality — [@arr.ext.N] would have been
       forgeable since [@]/[.] are legal simple-symbol characters.

       board #58 (codex critical, freshness by CONSTRUCTION not by counter trust): the
       minting door [Session.internal_minter] is PUBLIC, so a trusted in-process caller
       could pre-mint [.oxsmt.arr.ext.N] (giving that name a rank) and then our counter,
       trusting itself, would hand the extensionality rule the CALLER's symbol -> capture
       -> wrong verdict. So advance past any index whose name already carries a rank in
       [env] (i.e. is already declared/minted), and only then mint. This makes the witness
       fresh against the actual env state, not merely against our own counter. The counter
       also keeps distinct diseq pairs on distinct witnesses. *)
    let rec fresh_name () =
      let name = Printf.sprintf ".oxsmt.arr.ext.%d" t.fresh_counter in
      t.fresh_counter <- t.fresh_counter + 1;
      match Env.rank t.env (Symbol.intern name) with
      | (_ : Rank.t) -> fresh_name () (* already declared/minted — skip it *)
      | exception Not_found -> name
    in
    let name = fresh_name () in
    let sym = Env.declare_reserved t.cap t.env name (Rank.create [] index) in
    let w = Context.const t.ctx sym in
    Euf.register_term t.engine w;
    Hashtbl.replace t.ext_witness key w;
    w
;;

let extensionality t lit (a : Term.t) (b : Term.t) =
  match array_sort a.Term.sort with
  | None -> ()
  | Some (index, _element) ->
    let k = witness_index t a b index in
    (match build_select t a k, build_select t b k with
     | Some sa, Some sb -> Euf.assert_neq t.engine ~premise:(P_lit lit) sa sb
     | _ -> ())
;;

let assert_lit t lit =
  let atom = Lit.atom lit in
  let positive = Lit.sign lit in
  match Atom.Table.find_opt t.atoms atom with
  | None -> invalid_arg "Arr.assert_lit: atom was not registered"
  | Some { kind = K_eq (a, b); _ } ->
    if positive
    then Euf.assert_eq t.engine ~premise:(P_lit lit) a b
    else (
      Euf.assert_neq t.engine ~premise:(P_lit lit) a b;
      (* An array disequality demands an extensionality witness: a <> b is satisfiable
         only if a and b differ at some index, so introduce a fresh one. Sound
         (Skolemization); it drives ROW to close read-over-write refutations. *)
      extensionality t lit a b)
  | Some { kind = K_bool; term } ->
    let target = if positive then t.true_const else t.false_const in
    Euf.assert_eq t.engine ~premise:(P_lit lit) term target
  | Some { kind = K_foreign; _ } ->
    invalid_arg "Arr.assert_lit: a foreign (non-array/EUF) atom must not be asserted"
;;

(* --- read-over-write saturation --- *)

(* Upward read propagation. The read-over-write rules ([row_round] / [row_split]) relate a
   read of a STORE, [select (store base i v) j], to a read of its BASE, [select base j].
   But they only fire once BOTH reads exist as terms, and the only term-introduction they
   do is the base read ([build_select ... base]) given a store read. Nothing introduces
   the store read given only a base read — so a read of an array [a] never reaches the
   stores built over [a]. Two symptoms, one cause:

   - the storeinv shape (store(a,i,select(b,i)) = store(b,i,select(a,i)), a<>b): unsat,
     but the disequality's witness reads [select a k]/[select b k] never reach the two
     stores, so the store equality is never exercised and the engine wrongly saturates
     (unsat -> unknown);
   - satisfiable queries whose model the self-check REJECTS: the engine saturates on an
     under-constrained arrangement (reads on the declared arrays were never propagated up
     to the store terms mentioned by the assertions), so the extracted model does not in
     fact satisfy the assertions.

   This closes both: for every store [st = store base i v] and every read [select arr j]
   whose array [arr] is congruent to the store's [base], materialize [select st j]. The
   existing round/split then relates it to [select base j] (i = j entailed => select st j
   = v, else the guarded [row_split]).

   Cost is controlled by WHEN this runs, not by restricting which indices it fires at:
   [check] calls it only at [Final], once ordinary ROW saturation is otherwise clean and
   quiet (see [final_introduce_reads]). A query that refutes during propagation never
   reaches that point, so its fast refutation is untouched; the reads (and the
   [row_split]s they spawn) are paid for only on a branch that survives to a full,
   otherwise-consistent assignment — exactly where a store equality would otherwise go
   unexercised. Running this eagerly inside [saturate] at every effort instead was
   measured to flood deep store chains (swap/storecomm: ~22 nested stores) with case
   splits and net out to a wash.

   Termination: each (store term, index term) pair fires at most once ([ensured_reads]); a
   fresh read's index [j] is an existing index term, so no new index — hence no new
   (store,index) pair — is ever created. The introduction walks up a store chain across
   rounds: reading a base introduces a read of the store over it, which is itself the base
   of the next store, and so on. *)
let ensure_store_reads t ~changed =
  List.iter
    (fun st ->
       match head_args st with
       | Some (_, [| base; _i; _v |])
         when match role_of t st with
              | Some { Defs.role = Defs.Store; _ } -> true
              | _ -> false ->
         List.iter
           (fun sel ->
              match head_args sel with
              | Some (_, [| arr; j |])
                when role_of t sel <> None && Euf.are_equal t.engine arr base ->
                let key = st.Term.tag, j.Term.tag in
                if not (Hashtbl.mem t.ensured_reads key)
                then (
                  Hashtbl.replace t.ensured_reads key ();
                  match build_select t st j with
                  | Some _ -> changed := true
                  | None -> ())
              | _ -> ())
           t.select_terms
       | _ -> ())
    t.store_terms
;;

(* One ROW pass: for each select term [sel = select arr j] and each store term
   [st = store base i v] with [arr] congruent to [st], propagate the ENTAILED ROW
   equality. Only the definite direction ([i = j] known ⇒ sel = v) is propagated here; the
   open case is deferred to a [Split] at [Final] ([row_split]). Sets [changed] on a
   propagation. Never reports a conflict directly (Euf.check does). *)
let row_round t ~changed =
  List.iter
    (fun sel ->
       match head_args sel with
       | Some (_, [| arr; j |]) when role_of t sel <> None ->
         List.iter
           (fun st ->
              match head_args st with
              | Some (_, [| base; i; v |])
                when (match role_of t st with
                      | Some { Defs.role = Defs.Store; _ } -> true
                      | _ -> false)
                     && Euf.are_equal t.engine arr st ->
                (* definite ROW1: i = j entailed ⇒ sel = v *)
                if Euf.are_equal t.engine i j && not (Euf.are_equal t.engine sel v)
                then (
                  let prem =
                    P_derived
                      (dedup_lits
                         (lits_of_prems
                            (Euf.explain t.engine arr st @ Euf.explain t.engine i j)))
                  in
                  Euf.assert_eq t.engine ~premise:prem sel v;
                  changed := true);
                ignore base
              | _ -> ())
           t.store_terms
       | _ -> ())
    t.select_terms
;;

(* Saturate ROW definite propagations to a fixpoint, interleaving [Euf.check] (a merge can
   expose a diseq violation or a new congruence). [Some prems] on the first conflict. *)
let saturate t : prem list option =
  let rec loop n =
    if n > max_iters then failwith "Arr.saturate: fixpoint did not converge (bug)";
    match Euf.check t.engine with
    | Euf.Conflict prems -> Some prems
    | Euf.Consistent ->
      let changed = ref false in
      row_round t ~changed;
      if !changed then loop (n + 1) else None
  in
  loop 0
;;

(* At [Final]: find the tag-least open ROW pair (select arr j, store base i v) with
   [arr ~ st], [i = j] not entailed, and [sel] not already equal to [select base j], and
   return the theory-valid split disjunction. Building [select base j] introduces it into
   the e-graph so the split's second disjunct is a real atom. The disjunction:

   (guard ⇒) i = j ∨ select (store base i v) j = select base j

   where [guard] is [arr <> st] when [arr] is only congruent to (not syntactically) [st]
   (so the clause is unconditionally array-valid). Two/three distinct atoms, not a
   propositional tautology, so the SAT core keeps it. *)
let row_split t : Term.t list option =
  let cand = ref None in
  let by_tag a b = compare a.Term.tag b.Term.tag in
  List.iter
    (fun sel ->
       if !cand = None
       then (
         match head_args sel with
         | Some (_, [| arr; j |]) when role_of t sel <> None ->
           List.iter
             (fun st ->
                if !cand = None
                then (
                  match head_args st with
                  | Some (_, [| base; i; v |])
                    when (match role_of t st with
                          | Some { Defs.role = Defs.Store; _ } -> true
                          | _ -> false)
                         && Euf.are_equal t.engine arr st
                         && not (Euf.are_equal t.engine i j) ->
                    (match build_select t base j with
                     | Some selbase when not (Euf.are_equal t.engine sel selbase) ->
                       let idx_eq = Context.eq t.ctx i j in
                       let read_eq = Context.eq t.ctx sel selbase in
                       let disjuncts =
                         if Term.equal arr st
                         then [ idx_eq; read_eq ]
                         else
                           [ Context.not_ t.ctx (Context.eq t.ctx arr st)
                           ; idx_eq
                           ; read_eq
                           ]
                       in
                       ignore v;
                       cand := Some disjuncts
                     | _ -> ())
                  | _ -> ()))
             (List.sort by_tag t.store_terms)
         | _ -> ()))
    (List.sort by_tag t.select_terms);
  !cand
;;

(* --- propagation reason caching (mirrors Dt / Euf_adapter) --- *)

let reason_of_implied t (imp : Euf.implied) : Explanation.t =
  let premises = dedup_lits (lits_of_prems (Euf.explain_implied t.engine imp)) in
  let rule =
    if premises = []
    then Explanation.Rule_tag.Trivial
    else Explanation.Rule_tag.Euf_congruence
  in
  { Explanation.premises; rule }
;;

let cache_reason t lit expl =
  if not (Lit.Map.mem lit t.explain_cache)
  then (
    t.explain_cache <- Lit.Map.add lit expl t.explain_cache;
    match t.frames with
    | fr :: rest -> t.frames <- (lit :: fr) :: rest
    | [] -> t.frames <- [ [ lit ] ])
;;

let collect_propagations t : Lit.t list =
  List.filter_map
    (fun (imp : Euf.implied) ->
       match Term.Table.find_opt t.watched imp.Euf.atom with
       | None -> None
       | Some atom ->
         let lit = Lit.make atom imp.Euf.value in
         cache_reason t lit (reason_of_implied t imp);
         Some lit)
    (Euf.propagate t.engine)
;;

let conflict_of prems =
  let premises = dedup_lits (lits_of_prems prems) in
  if premises = [] then failwith "Arr: empty conflict premise set (unsound) [tripwire]";
  Theory.Conflict { Explanation.premises; rule = Explanation.Rule_tag.Euf_congruence }
;;

(* At [Final] only, after ordinary ROW saturation is clean and quiet: introduce the upward
   witness reads ([ensure_store_reads]) and re-saturate. Deferring the introduction to
   [Final] — rather than running it eagerly in [saturate] at every effort — is the
   difference between +completeness with no regression and a net wash: a query that
   refutes during propagation never reaches here, so its fast refutation is untouched; the
   witness reads (and the [row_split]s they spawn) are paid for only on a branch that
   survives to a full, otherwise-consistent assignment, which is exactly where the store
   equality would otherwise go unexercised. Returns a re-saturation verdict (conflict /
   propagations) if the new reads forced one; otherwise [None] to fall through to
   [row_split]/[Sat]. *)
let final_introduce_reads t : Theory.check_result option =
  let changed = ref false in
  ensure_store_reads t ~changed;
  if not !changed
  then None
  else (
    match saturate t with
    | Some prems -> Some (conflict_of prems)
    | None ->
      (match collect_propagations t with
       | _ :: _ as lits -> Some (Theory.Propagations lits)
       | [] -> None))
;;

let check t effort =
  match saturate t with
  | Some prems -> conflict_of prems
  | None ->
    let lits = collect_propagations t in
    (match lits, effort with
     | _ :: _, _ -> Theory.Propagations lits
     | [], Theory.Propagate -> Theory.Propagations []
     | [], Theory.Final ->
       (match final_introduce_reads t with
        | Some result -> result
        | None ->
          (match row_split t with
           | Some terms -> Theory.Split terms
           | None -> Theory.Sat)))
;;

let explain t lit =
  match Lit.Map.find_opt lit t.explain_cache with
  | Some expl -> expl
  | None ->
    failwith
      "Arr.explain: no cached reason for literal (not theory-propagated, or its frame \
       was popped)"
;;

let push t =
  Euf.push t.engine;
  t.frames <- [] :: t.frames
;;

let pop t n =
  Euf.pop t.engine n;
  let rec drop k frames =
    if k = 0
    then frames
    else (
      match frames with
      | fr :: rest ->
        List.iter (fun l -> t.explain_cache <- Lit.Map.remove l t.explain_cache) fr;
        drop (k - 1) rest
      | [] -> [])
  in
  t.frames
  <- (match drop n t.frames with
      | [] -> [ [] ]
      | fs -> fs)
;;

(* --- models (never trusted for a client verdict: array sat degrades to unknown) --- *)

let children (term : Term.t) : Term.t list =
  match term.Term.node with
  | Term.Bool_const _ | Term.Int_const _ -> []
  | Term.App (_, args) -> Iarr.to_list args
  | Term.Arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
  | Term.Le a | Term.Not a -> [ a ]
  | Term.Eq (a, b) -> [ a; b ]
  | Term.And a | Term.Or a -> Iarr.to_list a
  | Term.Ite (c, a, b) -> [ c; a; b ]
;;

let model t =
  let seen = Term.Table.create 64 in
  let acc = ref [] in
  let rec walk (term : Term.t) =
    if not (Term.Table.mem seen term)
    then (
      Term.Table.replace seen term ();
      let v =
        match term.Term.node with
        | Term.Eq (a, b) -> Model.Bool (Euf.are_equal t.engine a b)
        | _ ->
          if Sort.equal term.Term.sort Sort.bool
          then
            if Euf.are_equal t.engine term t.true_const
            then Model.Bool true
            else if Euf.are_equal t.engine term t.false_const
            then Model.Bool false
            else Model.Uninterp (Euf.class_of t.engine term)
          else Model.Uninterp (Euf.class_of t.engine term)
      in
      acc := (term, v) :: !acc;
      List.iter walk (children term))
  in
  List.iter walk (List.rev t.atom_terms);
  Model.of_alist !acc
;;

(* --- array sat models (§8 self-check via Array_model_check) --- *)

type value =
  | Scalar of Model.value
  | Array of
      { entries : (value * value) list
      ; default : value
      }

let array_model t : (Term.t * value) list option =
  (* A per-e-class scalar for a NON-array leaf (an index/element value). Uninterpreted and
     Int classes take a per-class-distinct witness (distinct classes => distinct values,
     so an index/element disequality holds); Bool is a bounded 2-value domain (like the DT
     Bool completion); an Int class carrying a literal takes it. *)
  let bool_memo : (int, bool) Hashtbl.t = Hashtbl.create 16 in
  let bool_next = ref 0 in
  let scalar_of (x : Term.t) : Model.value =
    if Sort.equal x.Term.sort Sort.bool
    then
      if Euf.are_equal t.engine x t.true_const
      then Model.Bool true
      else if Euf.are_equal t.engine x t.false_const
      then Model.Bool false
      else (
        let k = Euf.class_of t.engine x in
        match Hashtbl.find_opt bool_memo k with
        | Some b -> Model.Bool b
        | None ->
          let b = !bool_next = 1 in
          incr bool_next;
          Hashtbl.replace bool_memo k b;
          Model.Bool b)
    else (
      match x.Term.sort with
      | Sort.Int _ ->
        let members = Euf.class_members t.engine x in
        (match
           List.find_map
             (fun (m : Term.t) ->
                match m.Term.node with
                | Term.Int_const n -> Some n
                | _ -> None)
             members
         with
         | Some n ->
           (match Bigint.to_int_opt n with
            | Some i -> Model.Int i
            | None -> Model.Uninterp (Euf.class_of t.engine x))
         | None -> Model.Uninterp (Euf.class_of t.engine x))
      | _ -> Model.Uninterp (Euf.class_of t.engine x))
  in
  (* Group every registered [select b j] by the e-class of its array argument [b], so an
     array class's model map lists (value of j -> value of that select) for every read of
     a congruent array. This is the finite part of the weak-array value; unread indices
     take the element sort's base default. *)
  let by_class : (int, (Term.t * Term.t) list) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun sel ->
       match head_args sel, role_of t sel with
       | Some (_, [| arr; j |]), Some { Defs.role = Defs.Select; _ } ->
         let k = Euf.class_of t.engine arr in
         Hashtbl.replace
           by_class
           k
           ((j, sel) :: Option.value (Hashtbl.find_opt by_class k) ~default:[])
       | _ -> ())
    (List.rev t.select_terms);
  let memo : (int, value) Hashtbl.t = Hashtbl.create 64 in
  let rec default_of (element : Sort.t) (depth : int) : value =
    if depth > 1_000
    then Scalar (Model.Uninterp 0)
    else (
      match element with
      | Sort.Array (_, e2) -> Array { entries = []; default = default_of e2 (depth + 1) }
      | Sort.Bool -> Scalar (Model.Bool false)
      | Sort.Int _ -> Scalar (Model.Int 0)
      | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.BitVec _ ->
        Scalar (Model.Uninterp 0))
  and value_of (x : Term.t) (depth : int) : value =
    if depth > 1_000
    then Scalar (Model.Uninterp 0)
    else (
      match x.Term.sort with
      | Sort.Array (_index, element) ->
        let k = Euf.class_of t.engine x in
        (match Hashtbl.find_opt memo k with
         | Some v -> v
         | None ->
           (* seed a base value first (cycle guard; array sorts are non-recursive so this
              is only defensive) *)
           Hashtbl.replace
             memo
             k
             (Array { entries = []; default = default_of element (depth + 1) });
           let sels = Option.value (Hashtbl.find_opt by_class k) ~default:[] in
           let entries =
             List.map
               (fun (j, sel) -> value_of j (depth + 1), value_of sel (depth + 1))
               sels
           in
           let v = Array { entries; default = default_of element (depth + 1) } in
           Hashtbl.replace memo k v;
           v)
      | _ -> Scalar (scalar_of x))
  in
  (* one value per registered subterm of an asserted atom (array leaves + index/element
     leaves; the checker computes select/store/equality itself) *)
  let seen = Term.Table.create 128 in
  let acc = ref [] in
  let rec collect (term : Term.t) =
    if not (Term.Table.mem seen term)
    then (
      Term.Table.replace seen term ();
      acc := (term, value_of term 0) :: !acc;
      List.iter collect (children term))
  in
  List.iter collect (List.rev t.atom_terms);
  Some !acc
;;
