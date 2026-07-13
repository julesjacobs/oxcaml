(* The datatypes theory as an e-graph client. See dt.mli for the contract.

   The theory owns a [prem Euf.t] engine: constructors/selectors/testers are App nodes, so
   the engine gives congruence closure over them for free (a genuine e-graph client). On
   top of that substrate this module enforces the four DT axioms and constructor case
   splits. The datatype {e shape} — which constructors a sort has, the selector/field
   correspondence, the tester map — is read from {!Oxsmt_core.Datatype_defs}.

   The THEORY boilerplate (atom<->term bookkeeping, watches, precedence-valid propagation
   reason caching, pop-frame reason dropping) mirrors Euf_adapter — the engine does the
   congruence reasoning and self-checks its explanations; this layer relabels premise
   tokens and adds the DT rules in [check]. *)

open Oxsmt_core
module Euf = Oxsmt_euf.Euf
module Defs = Datatype_defs

(* The engine's ['p]. A real assertion carries [P_lit]; the standing [true <> false]
   disequality carries [P_axiom]; a DT-derived equality (field equality, selector
   evaluation, constructor instantiation) carries [P_derived reasons] — the trail literals
   whose conjunction entails the trigger. [P_axiom] rides along and is dropped when we
   build an Explanation, so a returned premise set holds only asserted literals. *)
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
  ; reg : Defs.t
  ; engine : prem Euf.t
  ; true_const : Term.t
  ; false_const : Term.t
  ; atoms : info Atom.Table.t
  ; watched : Atom.t Term.Table.t (* watched Eq/predicate term -> atom, for propagation *)
  ; mutable atom_terms : Term.t list (* registration order; model enumeration *)
  ; seen_cat : unit Term.Table.t
  ; mutable ctor_terms : Term.t list (* registered constructor applications, rev order *)
  ; mutable selector_terms : Term.t list (* registered selector applications, rev order *)
  ; mutable tester_terms : Term.t list (* registered tester applications, rev order *)
  ; mutable dt_terms : Term.t list (* every registered datatype-sorted term, rev order *)
  ; split_relevant : unit Term.Table.t (* DT terms in a diseq / under a selector-tester *)
  ; diseq_relevant : unit Term.Table.t
    (* DT terms that are an operand of a disequality (or a field of one, transitively) —
         the seed set for field-relevance propagation; distinctness, not equality, is what
         forces a field to be case-split. *)
  ; input_dt : unit Term.Table.t
    (* datatype terms from the ORIGINAL problem (not theory-introduced constructor
         instantiations) — the field-relevance cascade only crosses these, bounding it to
         the finite input structure (no runaway down a recursive spine of split-born
         selector terms). *)
  ; mutable explain_cache : Explanation.t Lit.Map.t
  ; mutable frames : Lit.t list list
  }

let max_iters = 1_000_000

let create ctx _env reg =
  let engine = Euf.create ctx in
  let true_const = Context.bool_const ctx true in
  let false_const = Context.bool_const ctx false in
  Euf.register_term engine true_const;
  Euf.register_term engine false_const;
  Euf.assert_neq engine ~premise:P_axiom true_const false_const;
  { ctx
  ; reg
  ; engine
  ; true_const
  ; false_const
  ; atoms = Atom.Table.create 64
  ; watched = Term.Table.create 64
  ; atom_terms = []
  ; seen_cat = Term.Table.create 128
  ; ctor_terms = []
  ; selector_terms = []
  ; tester_terms = []
  ; dt_terms = []
  ; split_relevant = Term.Table.create 64
  ; diseq_relevant = Term.Table.create 64
  ; input_dt = Term.Table.create 64
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

(* The datatype sort symbol of [sort], if it is a datatype sort. *)
let dt_sort_sym (sort : Sort.t) : Symbol.t option =
  match sort with
  | Sort.Datatype s -> Some s
  | Sort.Bool | Sort.Int _ | Sort.Uninterpreted _ | Sort.Array _ | Sort.BitVec _ -> None
;;

let is_dt_sort t (sort : Sort.t) =
  match dt_sort_sym sort with
  | Some s -> Defs.is_datatype_sym t.reg s
  | None -> false
;;

let datatype_of_sort t (sort : Sort.t) : Defs.datatype option =
  match dt_sort_sym sort with
  | Some s -> Defs.datatype_of_sort t.reg s
  | None -> None
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

let derived_premise t a b =
  P_derived (dedup_lits (lits_of_prems (Euf.explain t.engine a b)))
;;

(* --- registration: catalog DT structure while registering into the e-graph --- *)

(* [~input] is true when cataloging a term from the ORIGINAL problem (an asserted atom's
   closure), false for a term the theory itself introduced mid-solve (a constructor
   instantiation [C (sel_1 x, ..)] from a split/tester). Only input datatype terms enter
   [input_dt], which BOUNDS the field-relevance cascade: it may march over the finite
   input structure but never down a recursive spine of split-born selector terms (which
   would spuriously exhaust the split budget on a trivially-sat recursive disequality). *)
let rec catalog t ~input (term : Term.t) =
  if not (Term.Table.mem t.seen_cat term)
  then (
    Term.Table.replace t.seen_cat term ();
    if is_dt_sort t term.Term.sort
    then (
      t.dt_terms <- term :: t.dt_terms;
      if input then Term.Table.replace t.input_dt term ());
    match head_args term with
    | Some (sym, args) ->
      if Defs.constructor_of_sym t.reg sym <> None
      then t.ctor_terms <- term :: t.ctor_terms
      else if Defs.selector_of_sym t.reg sym <> None
      then (
        t.selector_terms <- term :: t.selector_terms;
        if Array.length args >= 1 then Term.Table.replace t.split_relevant args.(0) ())
      else if Defs.tester_of_sym t.reg sym <> None
      then (
        t.tester_terms <- term :: t.tester_terms;
        if Array.length args >= 1 then Term.Table.replace t.split_relevant args.(0) ());
      Array.iter (catalog t ~input) args
    | None ->
      (match term.Term.node with
       | Term.Eq (a, b) ->
         catalog t ~input a;
         catalog t ~input b
       | Term.Not a | Term.Le a -> catalog t ~input a
       | Term.And xs | Term.Or xs -> List.iter (catalog t ~input) (Iarr.to_list xs)
       | Term.Ite (a, b, c) ->
         catalog t ~input a;
         catalog t ~input b;
         catalog t ~input c
       | Term.Arith lin ->
         List.iter (fun (c, _) -> catalog t ~input c) (Iarr.to_list lin.Term.coeffs)
       | Term.Bool_const _ | Term.Int_const _ -> ()
       | Term.App _ -> () (* unreachable: head_args returned None *)))
;;

let classify (term : Term.t) : kind =
  if not (Theory_view.is_atom term)
  then invalid_arg "Dt.register_atom: term is not a theory atom";
  match Theory_view.atom term with
  | Theory_view.Equality (a, b) -> K_eq (a, b)
  | Theory_view.Predicate (_, _) | Theory_view.Bool_lit _ -> K_bool
  | Theory_view.Le_zero _ -> K_foreign
;;

let register_atom t atom term =
  Euf.register_term t.engine term;
  catalog t ~input:true term;
  if not (Atom.Table.mem t.atoms atom)
  then (
    let kind = classify term in
    Atom.Table.replace t.atoms atom { term; kind };
    t.atom_terms <- term :: t.atom_terms;
    match kind with
    | K_eq _ ->
      (* watch for propagation only. Split-relevance is NOT seeded here: an equality atom
         does not by itself demand a case split (a positive equality merely merges; only a
         DISEQUALITY creates distinctness pressure), so relevance is seeded at assert time
         for the negative polarity — see [assert_lit]. Marking both sides here (polarity
         unknown at register time) would case-split a positive-equality don't-care. *)
      Term.Table.replace t.watched term atom
    | K_bool ->
      (match term.Term.node with
       | Term.App (_, args) when Iarr.length args >= 1 ->
         Term.Table.replace t.watched term atom;
         Euf.rearm_watch t.engine term
       | _ -> ())
    | K_foreign -> ())
;;

let assert_lit t lit =
  let atom = Lit.atom lit in
  let positive = Lit.sign lit in
  match Atom.Table.find_opt t.atoms atom with
  | None -> invalid_arg "Dt.assert_lit: atom was not registered"
  | Some { kind = K_eq (a, b); _ } ->
    if positive
    then Euf.assert_eq t.engine ~premise:(P_lit lit) a b
    else (
      Euf.assert_neq t.engine ~premise:(P_lit lit) a b;
      (* a DISEQUALITY over datatype-sorted sides is the ONLY equality-shaped split demand
         (a positive equality merely merges — never a split). Its operands become
         split-relevant (so a bare-variable enum diseq like [distinct v1..v4] case-splits)
         AND diseq-relevant (so field-relevance cascades to constructor fields for the
         bounded-enum field pigeonhole). Neither is seeded by [assert_eq]. *)
      if is_dt_sort t a.Term.sort
      then (
        Term.Table.replace t.split_relevant a ();
        Term.Table.replace t.split_relevant b ();
        Term.Table.replace t.diseq_relevant a ();
        Term.Table.replace t.diseq_relevant b ()))
  | Some { kind = K_bool; term } ->
    let target = if positive then t.true_const else t.false_const in
    Euf.assert_eq t.engine ~premise:(P_lit lit) term target
  | Some { kind = K_foreign; _ } ->
    invalid_arg "Dt.assert_lit: a foreign (non-DT) atom must not be asserted"
;;

(* --- the DT saturation fixpoint --- *)

(* [x = C (sel_1 x, .., sel_n x)] (nullary C: the constant), the constructor a true tester
   forces. *)
let instantiate_ctor t (c : Defs.constructor) (x : Term.t) : Term.t =
  match c.Defs.selectors with
  | [] -> Context.const t.ctx c.Defs.sym
  | selectors ->
    let sels =
      List.map
        (fun (sel : Defs.selector) -> Context.app t.ctx sel.Defs.sym [ x ])
        selectors
    in
    Context.app t.ctx c.Defs.sym sels
;;

(* class-id -> canonical (tag-least) witness constructor term; [Some prems] clash. *)
let build_witnesses t : (int, Term.t) Hashtbl.t * prem list option =
  let witnesses = Hashtbl.create 64 in
  let conflict = ref None in
  List.iter
    (fun cterm ->
       if !conflict = None
       then (
         let k = Euf.class_of t.engine cterm in
         match Hashtbl.find_opt witnesses k with
         | None -> Hashtbl.replace witnesses k cterm
         | Some wterm ->
           let cs = fst (Option.get (head_args cterm)) in
           let ws = fst (Option.get (head_args wterm)) in
           if not (Symbol.equal cs ws)
           then conflict := Some (Euf.explain t.engine cterm wterm)
           else if cterm.Term.tag < wterm.Term.tag
           then Hashtbl.replace witnesses k cterm))
    (List.rev t.ctor_terms);
  witnesses, !conflict
;;

let witness_of t witnesses x = Hashtbl.find_opt witnesses (Euf.class_of t.engine x)

(* One saturation round: inject field equalities (injectivity), selector evaluations, and
   constructor instantiations from asserted testers; check testers against witnesses.
   Returns conflict tokens if a DT axiom is violated; sets [changed] on an injection. *)
let saturate_round t ~changed : prem list option =
  let witnesses, clash = build_witnesses t in
  match clash with
  | Some _ as c -> c
  | None ->
    let conflict = ref None in
    (* injectivity: same constructor in one class => field equalities *)
    List.iter
      (fun cterm ->
         if !conflict = None
         then (
           let k = Euf.class_of t.engine cterm in
           match Hashtbl.find_opt witnesses k with
           | Some wterm when not (Term.equal wterm cterm) ->
             let cs, ca = Option.get (head_args cterm) in
             let ws, wa = Option.get (head_args wterm) in
             if Symbol.equal cs ws
             then
               Array.iteri
                 (fun i cai ->
                    let wai = wa.(i) in
                    if not (Euf.are_equal t.engine cai wai)
                    then (
                      Euf.assert_eq
                        t.engine
                        ~premise:(derived_premise t cterm wterm)
                        cai
                        wai;
                      changed := true))
                 ca
           | _ -> ()))
      (List.rev t.ctor_terms);
    (* selector evaluation: sel_i (C a..) = a_i once the argument's class is a C *)
    List.iter
      (fun st ->
         if !conflict = None
         then (
           match head_args st with
           | Some (sym, sargs) when Array.length sargs = 1 ->
             (match Defs.selector_of_sym t.reg sym with
              | Some (_dt, c, sel) ->
                let x = sargs.(0) in
                (match witness_of t witnesses x with
                 | Some wterm ->
                   let ws, wa = Option.get (head_args wterm) in
                   if Symbol.equal ws c.Defs.sym
                   then (
                     let target = wa.(sel.Defs.index) in
                     if not (Euf.are_equal t.engine st target)
                     then (
                       Euf.assert_eq
                         t.engine
                         ~premise:(derived_premise t x wterm)
                         st
                         target;
                       changed := true))
                 | None -> ())
              | None -> ())
           | _ -> ()))
      (List.rev t.selector_terms);
    (* testers: is-C x asserted true/false vs the class's witness *)
    List.iter
      (fun tt ->
         if !conflict = None
         then (
           match head_args tt with
           | Some (sym, targs) when Array.length targs = 1 ->
             (match Defs.tester_of_sym t.reg sym with
              | Some (_dt, c) ->
                let x = targs.(0) in
                let is_true = Euf.are_equal t.engine tt t.true_const in
                let is_false = Euf.are_equal t.engine tt t.false_const in
                (match witness_of t witnesses x with
                 | Some wterm ->
                   let ws, _ = Option.get (head_args wterm) in
                   let same = Symbol.equal ws c.Defs.sym in
                   if is_true && not same
                   then
                     conflict
                     := Some
                          (Euf.explain t.engine tt t.true_const
                           @ Euf.explain t.engine x wterm)
                   else if is_false && same
                   then
                     conflict
                     := Some
                          (Euf.explain t.engine tt t.false_const
                           @ Euf.explain t.engine x wterm)
                 | None ->
                   if is_true
                   then (
                     let cterm = instantiate_ctor t c x in
                     if not (Euf.are_equal t.engine x cterm)
                     then (
                       Euf.assert_eq
                         t.engine
                         ~premise:(derived_premise t tt t.true_const)
                         x
                         cterm;
                       catalog t ~input:false cterm;
                       changed := true)))
              | None -> ())
           | _ -> ()))
      (List.rev t.tester_terms);
    (* single-constructor forcing: a value of a 1-constructor datatype IS that constructor
       (a type tautology, hence an empty premise), so force [x = C (sel_1 x, ..)] for a
       split-relevant term whose class has no witness. This is the injectivity DUAL that
       lets congruence equate two records/tuples with equal fields
       ([fst p = fst q ∧ snd p = snd q ⟹ p = q]); a multi-constructor choice is a guess
       and stays a Split. Bounded: only split-relevant terms are forced, and an introduced
       field is forced only if it is itself split-relevant. *)
    List.iter
      (fun x ->
         if !conflict = None && Term.Table.mem t.split_relevant x
         then (
           match datatype_of_sort t x.Term.sort with
           | Some { Defs.constructors = [ c ]; _ } ->
             if witness_of t witnesses x = None
             then (
               let cterm = instantiate_ctor t c x in
               if not (Euf.are_equal t.engine x cterm)
               then (
                 Euf.assert_eq t.engine ~premise:(P_derived []) x cterm;
                 catalog t ~input:false cterm;
                 changed := true))
           | Some _ | None -> ()))
      (List.rev t.dt_terms);
    !conflict
;;

(* Saturate to a fixpoint, interleaving [Euf.check] (a congruence merge / diseq violation
   can arise from an injection). [Some prems] on the first conflict. *)
let saturate t : prem list option =
  let rec loop i =
    if i > max_iters then failwith "Dt.saturate: fixpoint did not converge (bug)";
    match Euf.check t.engine with
    | Euf.Conflict prems -> Some prems
    | Euf.Consistent ->
      let changed = ref false in
      (match saturate_round t ~changed with
       | Some _ as c -> c
       | None -> if !changed then loop (i + 1) else None)
  in
  loop 0
;;

(* Acyclicity (occurs check): a constructor value cannot contain itself. Build the
   directed graph class -> class-of-each-DT-arg-of-its-witness and look for a cycle; a
   cycle's premises are the equalities placing each argument in the next class. *)
let occurs_check t witnesses : prem list option =
  let color = Hashtbl.create 64 in
  (* class -> `Gray | `Black *)
  let parent = Hashtbl.create 64 in
  (* class -> (parent_class, arg_term_into_this_class) *)
  let result = ref None in
  let succs k =
    match Hashtbl.find_opt witnesses k with
    | None -> []
    | Some wterm ->
      let _, wargs = Option.get (head_args wterm) in
      List.filter_map
        (fun a ->
           if is_dt_sort t a.Term.sort then Some (Euf.class_of t.engine a, a) else None)
        (Array.to_list wargs)
  in
  let edge_prems arg =
    match Hashtbl.find_opt witnesses (Euf.class_of t.engine arg) with
    | Some w_next -> Euf.explain t.engine arg w_next
    | None -> []
  in
  let rec dfs k =
    if !result = None
    then (
      Hashtbl.replace color k `Gray;
      List.iter
        (fun (k', arg) ->
           if !result = None
           then (
             match Hashtbl.find_opt color k' with
             | Some `Gray ->
               let prems = ref (edge_prems arg) in
               let cur = ref k in
               while !cur <> k' do
                 match Hashtbl.find_opt parent !cur with
                 | Some (pc, parg) ->
                   prems := edge_prems parg @ !prems;
                   cur := pc
                 | None -> cur := k' (* safety: broken chain, stop *)
               done;
               result := Some !prems
             | Some `Black -> ()
             | None ->
               Hashtbl.replace parent k' (k, arg);
               dfs k'))
        (succs k);
      Hashtbl.replace color k `Black)
  in
  (* Start the DFS from each witness class in a DETERMINISTIC order (registration order of
     the constructor terms), never [Hashtbl.iter] over [witnesses] — the iteration order
     of a Hashtbl is unspecified, so which cycle (and thus which conflict premise set) is
     found would vary run-to-run, breaking I6 and cert reproducibility even though the
     verdict is stable. *)
  List.iter
    (fun cterm ->
       let k = Euf.class_of t.engine cterm in
       if Hashtbl.mem witnesses k && not (Hashtbl.mem color k) then dfs k)
    (List.rev t.ctor_terms);
  !result
;;

(* --- propagation reason caching (mirrors Euf_adapter) --- *)

let reason_of_implied t (imp : Euf.implied) : Explanation.t =
  let premises = dedup_lits (lits_of_prems (Euf.explain_implied t.engine imp)) in
  (* Unlike EUF, DT propagates {b tautologies}: [sel_i (C a…) = a_i] holds with no
     hypothesis, so when the watched atom's constructor argument is a literal term the
     reason is legitimately empty. Tag it [Trivial] (an unconditional theory-valid unit)
     rather than tripping the "empty reason = unsound" guard EUF uses. A non-empty reason
     is an ordinary congruence/injectivity chain. *)
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

(* Propagate split-relevance through constructor fields, to a fixpoint: if a
   split-relevant class has a constructor witness [C (a1..an)], its datatype-sorted fields
   become split-relevant too. Without this, a bounded-enum FIELD pigeonhole
   ([distinct (C x) (C y) (C z)] over a 2-value field type) never case-splits the fields
   [x y z], so the split loop terminates and [Final] wrongly reports Sat — the fields must
   be split so congruence surfaces the pigeonhole (two equal fields ⟹ their [C _] are
   congruent ⟹ violate a distinctness). Non-datatype fields (e.g. Int) are left alone. *)
let mark_field_relevance t witnesses =
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun x ->
         if Term.Table.mem t.diseq_relevant x
         then (
           match witness_of t witnesses x with
           | Some wterm ->
             let _, wargs = Option.get (head_args wterm) in
             Array.iter
               (fun a ->
                  (* [input_dt] guard BOUNDS the cascade: cross only fields that are terms
                   of the original problem, never a split-born selector term — else a
                   recursive disequality ([x <> y] over a recursive datatype) would march
                   down the spine (tail, tail-of-tail, ..) forever and spuriously exhaust
                   the split budget on a trivially-sat query. Input structure is finite. *)
                  if
                    is_dt_sort t a.Term.sort
                    && Term.Table.mem t.input_dt a
                    && not (Term.Table.mem t.diseq_relevant a)
                  then (
                    (* into [split_relevant] so the field is case-split, and
                     [diseq_relevant] so a nested field cascades further *)
                    Term.Table.replace t.split_relevant a ();
                    Term.Table.replace t.diseq_relevant a ();
                    changed := true))
               wargs
           | None -> ()))
      t.dt_terms
  done
;;

(* At [Final] with no propagation pending: force a constructor for the tag-least
   split-relevant datatype term whose class has no constructor yet, as the exhaustiveness
   clause [is-C1 x ∨ .. ∨ is-Ck x]. Only fires for >= 2 constructors (a genuine guess
   among distinct atoms, CONTRACT-SPLIT); a single-constructor type is forced in
   saturation (an entailed fact, not a guess), and a 0-constructor type (parser-rejected;
   total here for safety) never splits. *)
let exhaustiveness_split t witnesses : Term.t list option =
  let cand = ref None in
  List.iter
    (fun x ->
       if !cand = None && Term.Table.mem t.split_relevant x
       then (
         match datatype_of_sort t x.Term.sort with
         | Some dt when List.length dt.Defs.constructors >= 2 ->
           if witness_of t witnesses x = None then cand := Some (x, dt)
         | _ -> ()))
    (List.sort (fun a b -> compare a.Term.tag b.Term.tag) t.dt_terms);
  match !cand with
  | None -> None
  | Some (x, dt) ->
    Some
      (List.map
         (fun (c : Defs.constructor) -> Context.app t.ctx c.Defs.tester [ x ])
         dt.Defs.constructors)
;;

let conflict_of prems =
  let premises = dedup_lits (lits_of_prems prems) in
  if premises = [] then failwith "Dt: empty conflict premise set (unsound) [tripwire]";
  Theory.Conflict { Explanation.premises; rule = Explanation.Rule_tag.Euf_congruence }
;;

let check t effort =
  match saturate t with
  | Some prems -> conflict_of prems
  | None ->
    let witnesses, clash = build_witnesses t in
    (match clash with
     | Some prems -> conflict_of prems
     | None ->
       (match occurs_check t witnesses with
        | Some prems -> conflict_of prems
        | None ->
          let lits = collect_propagations t in
          (match lits, effort with
           | _ :: _, _ -> Theory.Propagations lits
           | [], Theory.Propagate -> Theory.Propagations []
           | [], Theory.Final ->
             mark_field_relevance t witnesses;
             (match exhaustiveness_split t witnesses with
              | Some terms -> Theory.Split terms
              | None -> Theory.Sat))))
;;

let explain t lit =
  match Lit.Map.find_opt lit t.explain_cache with
  | Some expl -> expl
  | None ->
    failwith
      "Dt.explain: no cached reason for literal (not theory-propagated, or its frame was \
       popped)"
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

(* --- models --- *)

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

type ctor_tree =
  | Ctor of string * ctor_tree list
  | Leaf of Model.value

(* A value for a non-datatype leaf: a bound Bool/Int if the class carries one, else an
   opaque uninterpreted witness (Int defaults to 0 when unconstrained — no arithmetic
   theory is present in a pure-DT problem). [Bool] is a FINITE 2-element sort, so it must
   NEVER flow through the unbounded [Uninterp] bucket (codex B1: that admitted three
   pairwise-distinct Bool datatype fields as sat): an unconstrained Bool leaf defaults to
   [Model.Bool false]. Diseq-respecting bounded completion (so two disequal Bool classes
   get DISTINCT values) is layered on in {!check_model}; this default keeps the standalone
   / public path sound (never [Uninterp] in a Bool position). *)
let leaf_value t (x : Term.t) : Model.value =
  if Sort.equal x.Term.sort Sort.bool
  then
    if Euf.are_equal t.engine x t.true_const
    then Model.Bool true
    else if Euf.are_equal t.engine x t.false_const
    then Model.Bool false
    else Model.Bool false
  else (
    match x.Term.sort with
    | Sort.Int _ ->
      let members = Euf.class_members t.engine x in
      let int_const =
        List.find_map
          (fun (m : Term.t) ->
             match m.Term.node with
             | Term.Int_const n -> Some n
             | _ -> None)
          members
      in
      (* An Int-sorted class value is the class's [Int_const] literal —
         arbitrary-precision [Bigint.t] (core-bignum W2). [Model.Int] is native [int]
         (model.mli is frozen), so a value beyond [int] range has no faithful scalar
         model: degrade to an opaque [Uninterp] marker (the bignum R1 int-projection-sink
         pattern) rather than a silently truncated [Model.Int] — never claim a wrong
         value. A satisfiable query whose model needs a >int63 field then reads unknown,
         matching sat-DT-degrades-to-unknown. FOLLOW-UP (constructor-tree sat models) owes
         real >int63 representation or must keep degrading here. *)
      (match int_const with
       | None -> Model.Int 0
       | Some n ->
         (match Bigint.to_int_opt n with
          | Some i -> Model.Int i
          | None -> Model.Uninterp (Euf.class_of t.engine x)))
    (* BitVec is defensively unreachable here: a bit-vector term is resolved by the eager
       bit-blaster before the combinator, and any BV term that reached the combinator
       degrades via [require_no_bitvec_terms] before model extraction. Fold into the
       generic opaque-class fallback for exhaustiveness. *)
    | Sort.Bool | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.Array _ | Sort.BitVec _ ->
      Model.Uninterp (Euf.class_of t.engine x))
;;

(* Shared model builder, parametrized on [leaf : Term.t -> Model.value] so a caller can
   inject diseq-respecting completion for a finite scalar sort (e.g. [Bool]); the public
   {!constructor_model} passes the plain per-class {!leaf_value}, {!check_model} passes a
   Bool-completing wrapper (codex B1 fix). *)
let constructor_model_gen t ~(leaf : Term.t -> Model.value)
  : (Term.t * ctor_tree) list option
  =
  let witnesses, clash = build_witnesses t in
  if clash <> None
  then None
  else (
    let memo : (int, ctor_tree) Hashtbl.t = Hashtbl.create 64 in
    (* A sort-correct default scalar for a SYNTHESIZED (term-less) base field: a [Bool]
       field must be [Model.Bool] (never [Uninterp]/[Int] — the checker's sort-inhabitance
       would reject those), an [Int] field [0], an uninterpreted field element [0]. An
       [Array]/[BitVec] field has no scalar model value here (a DT+arrays/BV mix is out of
       the pure-DT fragment): return [Uninterp 0], which the checker's inhabitance rejects
       for those sorts -> the sat degrades to [unknown] (sound). *)
    let base_leaf (sort : Sort.t) : Model.value =
      match sort with
      | Sort.Bool -> Model.Bool false
      | Sort.Int _ -> Model.Int 0
      | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.Array _ | Sort.BitVec _ ->
        Model.Uninterp 0
    in
    (* a terminating constructor for a datatype: prefer a nullary one, else fewest DT
       fields *)
    let dt_field_count (c : Defs.constructor) =
      List.fold_left
        (fun n (s : Defs.selector) -> if is_dt_sort t s.Defs.field_sort then n + 1 else n)
        0
        c.Defs.selectors
    in
    let base_ctor (dt : Defs.datatype) : Defs.constructor option =
      match dt.Defs.constructors with
      | [] -> None (* 0-constructor type (parser-rejected); stay total *)
      | c0 :: rest ->
        (match
           List.find_opt (fun (c : Defs.constructor) -> c.Defs.selectors = []) (c0 :: rest)
         with
         | Some c -> Some c
         | None ->
           Some
             (List.fold_left
                (fun best c -> if dt_field_count c < dt_field_count best then c else best)
                c0
                rest))
    in
    (* Model COMPLETION for an unconstrained class. Distinct e-classes are never
       asserted-equal (a positive equality would have MERGED them), so giving distinct
       unconstrained classes DISTINCT witness values is always sound and, crucially,
       satisfies disequalities ([x <> y] over an infinite/large-enough sort). Each
       unconstrained class draws a fresh [idx] from [next_idx]; [distinct_base dt idx]
       realizes the idx-th distinct value of the sort:
       - [idx = 0]: the terminating base constructor (nil / zero / the fewest-DT-fields
         constructor), recursive fields themselves based — the finite witness;
       - enum sort (every constructor nullary): the idx-th constructor when [idx < k];
       - a self-recursive sort (some constructor has a field of THIS sort): a spine of
         that recursive constructor of length [idx] over the base (distinct lengths =>
         distinct trees: nil, cons(_,nil), cons(_,cons(_,nil)), …);
       - otherwise fall back to the base — the §8 checker then fails closed (a distinct
         value it could not realize => [unknown], never a wrong sat). Determinism (I5/I6):
         idx is assigned in the deterministic tag-ordered traversal of [dt_terms]. *)
    let next_idx = ref 0 in
    let rec tree_of (x : Term.t) (depth : int) : ctor_tree =
      let k = Euf.class_of t.engine x in
      match Hashtbl.find_opt memo k with
      | Some tr -> tr
      | None ->
        Hashtbl.replace memo k (Leaf (Model.Uninterp k));
        let tr =
          match Hashtbl.find_opt witnesses k with
          | Some wterm ->
            let sym, wargs = Option.get (head_args wterm) in
            let _, c = Option.get (Defs.constructor_of_sym t.reg sym) in
            Ctor
              ( Symbol.name c.Defs.sym
              , Array.to_list (Array.map (fun a -> field_tree a depth) wargs) )
          | None ->
            (match datatype_of_sort t x.Term.sort with
             | Some dt ->
               let idx = !next_idx in
               incr next_idx;
               distinct_base dt idx depth
             | None -> Leaf (leaf x))
        in
        Hashtbl.replace memo k tr;
        tr
    and field_tree (a : Term.t) depth =
      if is_dt_sort t a.Term.sort then tree_of a (depth + 1) else Leaf (leaf a)
    and base_tree (dt : Defs.datatype) depth : ctor_tree =
      if depth > 10_000
      then Leaf (Model.Uninterp 0)
      else (
        match base_ctor dt with
        | None -> Leaf (Model.Uninterp 0)
        | Some c ->
          Ctor
            ( Symbol.name c.Defs.sym
            , List.map
                (fun (s : Defs.selector) ->
                   if is_dt_sort t s.Defs.field_sort
                   then (
                     match datatype_of_sort t s.Defs.field_sort with
                     | Some d -> base_tree d (depth + 1)
                     | None -> Leaf (Model.Uninterp 0))
                   else Leaf (base_leaf s.Defs.field_sort))
                c.Defs.selectors ))
    and distinct_base (dt : Defs.datatype) (idx : int) depth : ctor_tree =
      if idx = 0 || depth > 10_000
      then base_tree dt depth
      else (
        let ctors = dt.Defs.constructors in
        let is_enum =
          List.for_all (fun (c : Defs.constructor) -> c.Defs.selectors = []) ctors
        in
        if is_enum
        then (
          (* the idx-th nullary constructor, if the domain is large enough; else base (the
             §8 checker fails closed on the remaining collision) *)
          match List.nth_opt ctors idx with
          | Some c -> Ctor (Symbol.name c.Defs.sym, [])
          | None -> base_tree dt depth)
        else (
          (* a constructor with a field of THIS datatype's sort — build a spine of length
             [idx] over it, so distinct idx give distinct-length (distinct) trees *)
          let self_rec =
            List.find_opt
              (fun (c : Defs.constructor) ->
                 List.exists
                   (fun (s : Defs.selector) ->
                      match dt_sort_sym s.Defs.field_sort with
                      | Some a -> Symbol.equal a dt.Defs.sort_sym
                      | None -> false)
                   c.Defs.selectors)
              ctors
          in
          match self_rec with
          | Some c ->
            Ctor
              ( Symbol.name c.Defs.sym
              , List.map
                  (fun (s : Defs.selector) ->
                     match dt_sort_sym s.Defs.field_sort with
                     | Some a when Symbol.equal a dt.Defs.sort_sym ->
                       distinct_base dt (idx - 1) (depth + 1)
                     | _ ->
                       if is_dt_sort t s.Defs.field_sort
                       then (
                         match datatype_of_sort t s.Defs.field_sort with
                         | Some d -> base_tree d (depth + 1)
                         | None -> Leaf (Model.Uninterp 0))
                       else Leaf (base_leaf s.Defs.field_sort))
                  c.Defs.selectors )
          | None -> base_tree dt depth))
    in
    let dt_terms = List.sort_uniq (fun a b -> compare a.Term.tag b.Term.tag) t.dt_terms in
    Some (List.map (fun x -> x, tree_of x 0) dt_terms))
;;

let constructor_model t = constructor_model_gen t ~leaf:(fun x -> leaf_value t x)

(* Diseq-respecting bounded completion for the finite [Bool] sort (codex B1). [Bool] has
   two inhabitants, so like an enum it must get a BOUNDED-domain assignment: a class equal
   to the true/false constant takes that value; every other class draws the next of
   [{false, true}] (deterministically, in first-seen order) and is memoized by class so
   equal terms share it. Distinct disequal Bool classes thus get DISTINCT values (a 2-box
   [distinct] stays sat), while a THIRD mutually-distinct Bool class necessarily reuses a
   value — the trees then collide and the checker's structural [distinct] fails closed to
   [unknown] (never the wrong-sat B1 admitted). A [Bool] leaf never becomes [Uninterp]. *)
let bool_completion t : Term.t -> Model.value =
  let memo : (int, bool) Hashtbl.t = Hashtbl.create 16 in
  let next = ref 0 in
  fun x ->
    if Euf.are_equal t.engine x t.true_const
    then Model.Bool true
    else if Euf.are_equal t.engine x t.false_const
    then Model.Bool false
    else (
      let k = Euf.class_of t.engine x in
      match Hashtbl.find_opt memo k with
      | Some b -> Model.Bool b
      | None ->
        (* 0 -> false, 1 -> true, >=2 -> false (collision; checker rejects a diseq) *)
        let b = !next = 1 in
        incr next;
        Hashtbl.replace memo k b;
        Model.Bool b)
;;

(* The full checker model: a [Term.t -> ctor_tree] assignment for every registered subterm
   the §8 DT self-check needs (Dt_model_check). It is the union of

   - [constructor_model_gen] — a constructor tree for every registered DATATYPE term (so a
     datatype variable, a nested field, AND an underspecified selector term all have a
     value the evaluator can look up); and

   - a [Leaf] SCALAR for every registered NON-datatype atomic (nullary [App]) subterm — an
     Int/Bool/uninterpreted-sort variable or nullary symbol — so a top-level Int/uninterp
     equality (decided here by congruence) has operand values.

   Both share ONE [leaf] function so a Bool field and a Bool scalar of the same class get
   the same value: Bool leaves flow through the diseq-respecting {!bool_completion} (a
   finite sort, bounded to 2 values — codex B1), all other leaves through {!leaf_value}.
   Compound non-datatype terms (testers, equalities, connectives, constructor/selector
   applications) are NOT listed: the evaluator computes them structurally. [None] iff the
   tree build degrades (a cross-class constructor clash), so the caller fails closed to
   [unknown]. Snapshotted at the accepting Final->Sat (see {!Cdclt}).

   POSTURE (review F2): this extraction runs OUTSIDE the CONTRACT-POISON firewall (like
   [Session.build_model] — the firewall wraps only [Sat.solve]), so a bug here surfaces as
   a crash, never a silently-swallowed [unknown]. The tree walk needs NO explicit
   recursion-depth guard for termination: assertion ASTs are finite hash-consed DAGs and
   the model trees are finite ([base_tree]/[distinct_base] carry [depth > 10_000] caps,
   and the occurs check refutes a cyclic assignment as UNSAT before any sat model is
   built), so it always terminates. *)
let check_model t : (Term.t * ctor_tree) list option =
  let bool_of = bool_completion t in
  let leaf (x : Term.t) : Model.value =
    if Sort.equal x.Term.sort Sort.bool then bool_of x else leaf_value t x
  in
  match constructor_model_gen t ~leaf with
  | None -> None
  | Some trees ->
    let seen = Term.Table.create 128 in
    let scalars = ref [] in
    let rec walk (term : Term.t) =
      if not (Term.Table.mem seen term)
      then (
        Term.Table.replace seen term ();
        (match term.Term.node with
         | Term.App (_, args) when Iarr.length args = 0 ->
           (* a nullary applied symbol: a datatype leaf is already covered by [trees];
              collect a non-datatype leaf (Int/Bool/uninterpreted var) as a scalar *)
           if not (is_dt_sort t term.Term.sort)
           then scalars := (term, Leaf (leaf term)) :: !scalars
         | _ -> ());
        List.iter walk (children term))
    in
    List.iter walk (List.rev t.atom_terms);
    Some (trees @ !scalars)
;;
