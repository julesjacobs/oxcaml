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
  | Sort.Bool | Sort.Int _ | Sort.Uninterpreted _ -> None
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

let rec catalog t (term : Term.t) =
  if not (Term.Table.mem t.seen_cat term)
  then (
    Term.Table.replace t.seen_cat term ();
    if is_dt_sort t term.Term.sort then t.dt_terms <- term :: t.dt_terms;
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
  catalog t term;
  if not (Atom.Table.mem t.atoms atom)
  then (
    let kind = classify term in
    Atom.Table.replace t.atoms atom { term; kind };
    t.atom_terms <- term :: t.atom_terms;
    match kind with
    | K_eq (a, b) ->
      Term.Table.replace t.watched term atom;
      if is_dt_sort t a.Term.sort
      then (
        Term.Table.replace t.split_relevant a ();
        Term.Table.replace t.split_relevant b ())
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
    else Euf.assert_neq t.engine ~premise:(P_lit lit) a b
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
                       catalog t cterm;
                       changed := true)))
              | None -> ())
           | _ -> ()))
      (List.rev t.tester_terms);
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
  Hashtbl.iter (fun k _ -> if not (Hashtbl.mem color k) then dfs k) witnesses;
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

(* At [Final] with no propagation pending: force a constructor for the tag-least
   split-relevant datatype term whose class has no constructor yet (>= 2 constructors), as
   the exhaustiveness clause [is-C1 x ∨ .. ∨ is-Ck x]. *)
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
   theory is present in a pure-DT problem). *)
let leaf_value t (x : Term.t) : Model.value =
  if Sort.equal x.Term.sort Sort.bool
  then
    if Euf.are_equal t.engine x t.true_const
    then Model.Bool true
    else if Euf.are_equal t.engine x t.false_const
    then Model.Bool false
    else Model.Uninterp (Euf.class_of t.engine x)
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
      Model.Int (Option.value int_const ~default:0)
    | Sort.Bool | Sort.Uninterpreted _ | Sort.Datatype _ ->
      Model.Uninterp (Euf.class_of t.engine x))
;;

let constructor_model t : (Term.t * ctor_tree) list option =
  let witnesses, clash = build_witnesses t in
  if clash <> None
  then None
  else (
    let memo : (int, ctor_tree) Hashtbl.t = Hashtbl.create 64 in
    (* a terminating constructor for a datatype: prefer a nullary one, else fewest DT
       fields *)
    let dt_field_count (c : Defs.constructor) =
      List.fold_left
        (fun n (s : Defs.selector) -> if is_dt_sort t s.Defs.field_sort then n + 1 else n)
        0
        c.Defs.selectors
    in
    let base_ctor (dt : Defs.datatype) : Defs.constructor =
      let cs = dt.Defs.constructors in
      match List.find_opt (fun (c : Defs.constructor) -> c.Defs.selectors = []) cs with
      | Some c -> c
      | None ->
        List.fold_left
          (fun best c -> if dt_field_count c < dt_field_count best then c else best)
          (List.hd cs)
          (List.tl cs)
    in
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
             | Some dt -> base_tree dt depth
             | None -> Leaf (leaf_value t x))
        in
        Hashtbl.replace memo k tr;
        tr
    and field_tree (a : Term.t) depth =
      if is_dt_sort t a.Term.sort then tree_of a (depth + 1) else Leaf (leaf_value t a)
    and base_tree (dt : Defs.datatype) depth : ctor_tree =
      if depth > 10_000
      then Leaf (Model.Uninterp 0)
      else (
        let c = base_ctor dt in
        Ctor
          ( Symbol.name c.Defs.sym
          , List.map
              (fun (s : Defs.selector) ->
                 if is_dt_sort t s.Defs.field_sort
                 then (
                   match datatype_of_sort t s.Defs.field_sort with
                   | Some d -> base_tree d (depth + 1)
                   | None -> Leaf (Model.Uninterp 0))
                 else Leaf (Model.Int 0))
              c.Defs.selectors ))
    in
    let dt_terms = List.sort_uniq (fun a b -> compare a.Term.tag b.Term.tag) t.dt_terms in
    Some (List.map (fun x -> x, tree_of x 0) dt_terms))
;;
