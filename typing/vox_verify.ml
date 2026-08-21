(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree
module Ir = Vox_lower.Ir

type error =
  | Unrepresentable of string
  | Dependent_arrow
  | Stacked_refinement_heads
  | Predicate_reads_mutable
  | Not_verified of int
  | Plan_error of string

exception Error of Location.t * error

(* A pending obligation, as the walk produces them in source order; each
   becomes one [Vox_logic.Obligation.t] through the lowering.  The subject
   is lowered when the obligation is finalized, so the constants its
   lowering mints (and the facts they deposit) land in this obligation's
   own snapshot. *)
type pending =
  { subject : Typedtree.expression
  ; subject_ir : Ir.t
  ; imposed : Types.type_expr  (* refined; head = the predicate *)
  ; facts : Vox_fact.t  (* in scope at the subject *)
  ; seen_idents : Path.t list ref
        (* the idents whose value-description facts this obligation
           already holds (deposited while the subject lowered); goal
           assembly deposits through the same set, so a predicate and a
           subject mentioning one ident yield one fact.  Each pending owns
           its ref: two obligations on one subject share the lowering's
           history but not each other's assembly-time deposits *)
  ; loc : Location.t
  }

type state =
  { symbols : Vox_lower.Symbols.t
  ; mutable pendings : pending list  (* newest first *)
  ; mutable seen : (Typedtree.expression * Types.type_expr) list
        (* consumer-side dedup: one pending per (subject node, imposed type
           up to [Ctype.is_equal]) -- markers and apply-arrow domains
           overlap (annotated arguments, the %ignore path) *)
  ; mutable admissions : (Path.t * Types.type_expr) list
        (* newest first, deduplicated by path: the refined contracts of
           externals and imported declarations that deposited facts --
           axioms nothing in this unit checked, reported at exit so a
           Proved is visibly conditional on them *)
  }

(* The walk threads a scope value down, so branch scoping is the data
   structure's behaviour: whatever a branch adds vanishes when the walk
   resumes with the parent's value. *)
type scope =
  { facts : Vox_fact.t
  ; mutable_decls : (Ident.t * Types.type_expr) list
        (* [let mutable] binders in scope, with their declared types: a
           [Texp_mutvar] read deposits the declared predicate at its
           per-read constant *)
  ; total_locals : Ident.t list
        (* local binders whose right-hand side was checked at totality
           Total (the recorded [Texp_mode] annotation of [let f @ total]):
           the stability gate's occurrence read cannot see this — the
           annotation caps the checking mode without pinning the binder's
           mode variable *)
  }

(* Whether a binding's right-hand side carries a [@ total] mode
   annotation (recorded as a [Texp_mode] extra by the constraint
   desugaring). *)
let rhs_annotated_total (e : Typedtree.expression) =
  List.exists
    (fun (extra, _, _) ->
       match extra with
       | Texp_mode modes ->
         (match modes.mode_modes.totality with
          | Some Mode.Totality.Const.Total -> true
          | Some Partial | None -> false)
       | _ -> false)
    e.exp_extra

let is_total_local scope id =
  List.exists (Ident.same id) scope.total_locals

(* The annotated-total binders of a binding group, folded into a scope:
   used by [walk_bindings] at a [let] and by [walk_structure] across
   items. *)
let add_total_binders scope vbs =
  List.fold_left
    (fun scope (vb : value_binding) ->
       match vb.vb_pat.pat_desc with
       | Tpat_var { id; _ } when rhs_annotated_total vb.vb_expr ->
         { scope with total_locals = id :: scope.total_locals }
       | _ -> scope)
    scope vbs

let mono ty =
  match Types.get_desc ty with
  | Tpoly _ -> Btype.tpoly_get_mono ty
  | _ -> ty

(* Dependent arrows reach the walk from valid source (higher-order solving
   instantiates them into applications), so a predicate whose free
   variables include an arrow binder is a located rejection, never a
   fatal error.  Stacked heads are fail-closed pending the owner-level
   semantics question. *)
let rec rexp_has_free_var ~bound (r : Types.refinement_expression) =
  let free = rexp_has_free_var in
  match r.rexp_desc with
  | Rexp_var id -> not (List.exists (Ident.same id) bound)
  | Rexp_hole | Rexp_ident _ | Rexp_constant _ -> false
  | Rexp_apply (f, { rapp_source_args = args; rapp_completion = _ }) ->
    free ~bound f || List.exists (fun (_, a) -> free ~bound a) args
  | Rexp_format (_, expansion) -> free ~bound expansion
  | Rexp_tuple comps -> List.exists (fun (_, c) -> free ~bound c) comps
  | Rexp_construct (_, _, arg) ->
    (match arg with Some a -> free ~bound a | None -> false)
  | Rexp_field (r, _, _, _) -> free ~bound r
  | Rexp_ifthenelse (c, a, b) ->
    free ~bound c || free ~bound a
    || (match b with Some x -> free ~bound x | None -> false)
  | Rexp_let ({ rb_ident; rb_expr }, body) ->
    free ~bound rb_expr || free ~bound:(rb_ident :: bound) body
  | Rexp_fun (id, body) -> free ~bound:(id :: bound) body
  | Rexp_match (scrut, cases) ->
    free ~bound scrut
    || List.exists
         (fun (c : Types.refinement_case) ->
            let rec pat_idents (p : Types.refinement_pattern) acc =
              match p.rpat_desc with
              | Rpat_any | Rpat_constant _ -> acc
              | Rpat_var id -> id :: acc
              | Rpat_tuple ps ->
                List.fold_left (fun acc (_, p) -> pat_idents p acc) acc ps
              | Rpat_construct (_, _, po) ->
                Option.fold ~none:acc ~some:(fun p -> pat_idents p acc) po
              | Rpat_alias (p, id) -> pat_idents p (id :: acc)
            in
            let bound = pat_idents c.rc_lhs bound in
            (match c.rc_guard with Some g -> free ~bound g | None -> false)
            || free ~bound c.rc_rhs)
         cases
  | Rexp_constraint (r, _) -> free ~bound r

let check_imposable env ty loc =
  match Types.get_desc (Ctype.expand_head env ty) with
  | Trefine { ref_payload; ref_pred; _ } ->
    (match Types.get_desc (Ctype.expand_head env ref_payload) with
     | Trefine _ -> raise (Error (loc, Stacked_refinement_heads))
     | _ -> ());
    if rexp_has_free_var ~bound:[] !ref_pred
    then raise (Error (loc, Dependent_arrow))
  | _ ->
    (* every imposition source checked its head; reaching here is a
       walker defect, not a user error *)
    Misc.fatal_error "Vox_verify: imposed type without a refined head"

(* Fail-open predicate fact: instantiate a declared refined type's
   predicate at a subject term, extending [facts_ref]; [true] when a fact
   was deposited.  A source declines what it cannot lower (a completeness
   gap, never a soundness gap) — except a predicate over mutable state,
   which has no single denotation and propagates as a located rejection.
   [on_resolved] receives the idents the predicate's own lowering
   resolves, so nested declared facts ride the same rule. *)
let add_predicate_fact st ~env ~label ~loc ty ~subject_ir ~on_resolved
    facts_ref =
  match Types.get_desc (Ctype.expand_head env ty) with
  | Trefine { ref_payload; ref_pred; _ } ->
    (try
       let hole_sort =
         Vox_lower.sort_of_type st.symbols ~loc env ref_payload
       in
       let pred =
         Vox_lower.lower_predicate st.symbols ~on_resolved ~env ~hole_sort
           !ref_pred
       in
       facts_ref :=
         Vox_fact.add !facts_ref
           (Vox_lower.substitute_hole pred ~hole:subject_ir)
           ~label ~loc;
       true
     with Vox_lower.Unsupported _ -> false)
  | _ -> false

(* The codomain contract of an application: the funct's solved arrow spine
   after the supplied arguments.  Reading the spine (a declared source)
   rather than the apply's [exp_type] (residue). *)
let apply_codomain (e : Typedtree.expression) =
  match e.exp_desc with
  | Texp_apply (funct, args, _, _, _, _) ->
    let rec spine ty = function
      | [] -> Some ty
      | (_, Omitted _) :: _ -> None
      | (_, Arg _) :: rest ->
        (match Types.get_desc (Ctype.expand_head funct.exp_env ty) with
         | Tarrow (_, _, cod, _) -> spine cod rest
         | _ -> None)
    in
    spine funct.exp_type args
  | _ -> None

(* An assumed contract: a refined declaration nothing in this unit
   checked — an external's, or an imported unit's.  Same-unit
   declarations had their own obligations discharged here (or refused
   with the unit), so they are not axioms. *)
let is_axiom_source path (vd : Types.value_description) =
  (match vd.val_kind with Val_prim _ -> true | _ -> false)
  || Ident.is_global_or_predef (Path.head path)

let record_admission st ~env path (vd : Types.value_description) =
  if is_axiom_source path vd
     && not (List.exists (fun (p, _) -> Path.same p path) st.admissions)
  then begin
    (* an occurrence's description instantiates the scheme at the use's
       ground type; the report names the generic declaration — one line
       describing the whole trust surface, instead of whichever
       specialization happened to deposit first *)
    let declared =
      match Env.find_value path env with
      | vd -> (Subst.Lazy.force_value_description vd).val_type
      | exception Not_found -> vd.val_type
    in
    st.admissions <- (path, declared) :: st.admissions
  end

(* The declaration behind an application's funct, when it is one. *)
let funct_declaration (e : Typedtree.expression) =
  match e.exp_desc with
  | Texp_apply (funct, _, _, _, _, _) ->
    (match funct.exp_desc with
     | Texp_ident { path; desc; _ } -> Some (path, desc)
     | _ -> None)
  | _ -> None

(* The fact sources that ride on lowering — either front end's.  Deposits
   go to [facts_ref]: the obligation's own snapshot at finalization and
   goal assembly, or the scope under construction for a let equality /
   path condition.  [seen_idents] makes value-description facts
   once-per-obligation; finalization and goal assembly share one set
   through the pending record. *)
let make_deposit st scope ~env ?(seen_idents = ref []) facts_ref =
  let rec deposit (r : Vox_lower.resolved) (ir : Ir.t) =
    match r with
    | Vox_lower.Resolved_ident (path, vd) ->
      (* module-level and imported values: the occurrence is
         payload-typed, the value description still declares the refined
         type; once per obligation.  KNOWN HOLE (owner-deferred to a later
         piece): a recursive value binding resolves its own ident here
         while its right-hand side lowers, so its declared predicate
         becomes a hypothesis of its own obligation — [let rec x :
         t{ false } = C x] self-justifies, and the false fact then
         propagates through later deposits.  Excluding the recursive
         group's own idents needs recursive-group infrastructure this
         piece does not carry; the sentinel fixtures in vc-printing.ml and
         vc-z3.ml (recursive-knot-hole) pin the accepting behaviour. *)
      if not (List.exists (Path.same path) !seen_idents)
      then begin
        seen_idents := path :: !seen_idents;
        if add_predicate_fact st ~env
             ~label:("value " ^ Path.name path)
             ~loc:ir.Ir.loc vd.val_type ~subject_ir:ir ~on_resolved:deposit
             facts_ref
        then record_admission st ~env path vd
      end
    | Resolved_apply e ->
      (match apply_codomain e with
       | Some cod ->
         if add_predicate_fact st ~env ~label:"apply codomain"
              ~loc:e.exp_loc cod ~subject_ir:ir ~on_resolved:deposit
              facts_ref
         then (
           match funct_declaration e with
           | Some (path, vd) -> record_admission st ~env path vd
           | None -> ())
       | None -> ())
    | Resolved_field (e, lbl) ->
      ignore
        (add_predicate_fact st ~env
           ~label:("field " ^ lbl.lbl_name)
           ~loc:e.exp_loc lbl.lbl_arg ~subject_ir:ir ~on_resolved:deposit
           facts_ref
         : bool)
    | Resolved_mutvar id ->
      (match
         List.find_opt (fun (i, _) -> Ident.same i id) scope.mutable_decls
       with
       | Some (_, ty) ->
         ignore
           (add_predicate_fact st ~env
              ~label:("read of " ^ Ident.name id)
              ~loc:ir.Ir.loc ty ~subject_ir:ir ~on_resolved:deposit
              facts_ref
            : bool)
       | None -> ())
  in
  deposit

(* Binder facts, unconditional: every bound ident whose recorded pattern
   type has a refined head deposits the instantiated predicate.  Variable
   and alias (sub-)patterns are exactly the idents [pat_bound_idents_full]
   reports; a refined head on a wildcard, constant, constructor or
   or-pattern node binds no single name and deposits nothing. *)
let add_binder_facts st scope ~env pat facts =
  let facts_ref = ref facts in
  let deposit = make_deposit st scope ~env facts_ref in
  List.iter
    (fun (id, (name : string Location.loc), ty, _, _) ->
       (* arrow domains are [Tpoly]-wrapped, and synthetic binders (the
          optional-elimination eta pattern) record them unmono'ed *)
       let ty = mono ty in
       match Types.get_desc (Ctype.expand_head env ty) with
       | Trefine _ ->
         (try
            let sort =
              Vox_lower.sort_of_type st.symbols ~loc:name.loc env ty
            in
            let sym = Vox_lower.Symbols.value (Path.Pident id) in
            let subject_ir = { Ir.desc = Var sym; sort; loc = name.loc } in
            ignore
              (add_predicate_fact st ~env
                 ~label:("binder " ^ Ident.name id)
                 ~loc:name.loc ty ~subject_ir ~on_resolved:deposit facts_ref
               : bool)
          with Vox_lower.Unsupported _ -> ())
       | _ -> ())
    (pat_bound_idents_full pat);
  !facts_ref

(* Transparency for path conditions: the condition lowered without
   abstracting anything to an opaque constant.  Minted constants are the
   only symbols containing '/', so the term itself carries the answer --
   counting mints would misjudge a re-lowering that hits the per-node
   memo. *)
let rec has_opaque (ir : Ir.t) =
  match ir.desc with
  | Var name -> String.length name > 6 && String.sub name 0 7 = "result/"
  | Const _ | Hole -> false
  | App (_, args) | Call (_, args) | Construct (_, args) ->
    List.exists has_opaque args
  | Ite (a, b, c) -> has_opaque a || has_opaque b || has_opaque c
  | Select (_, _, t) -> has_opaque t

(* Lower a condition for a path-condition fact.  [None] when the gate
   declines: not transparently lowerable (or not Bool-sorted, which
   cannot happen for a typechecked condition but fails open anyway).
   Returns the lowered condition and the facts its lowering deposited. *)
let transparent_condition st scope (cond : Typedtree.expression) =
  let facts_ref = ref scope.facts in
  match
    Vox_lower.lower_subject st.symbols
      ~on_resolved:(make_deposit st scope ~env:cond.exp_env facts_ref)
      ~is_total_local:(is_total_local scope) cond
  with
  | exception Vox_lower.Unsupported _ -> None
  | ci ->
    if has_opaque ci || not (Vox_logic.Sort.equal ci.Ir.sort Bool)
    then None
    else Some (ci, !facts_ref)

let negate (ci : Ir.t) : Ir.t = { ci with desc = App (Not, [ci]) }

let add_condition scope (ci, facts) ~sense ~loc =
  let ci = if sense then ci else negate ci in
  { scope with
    facts = Vox_fact.add facts ci ~label:"path condition" ~loc
  }

(* Imposition bookkeeping: markers on the node plus whatever the parent
   pushed, deduplicated per (node, type up to [Ctype.is_equal]) --
   refinement-flow's own rule extended across sources at the consumer. *)
let impositions st (e : Typedtree.expression) ~imposed =
  let markers =
    List.filter_map
      (fun (extra, loc, _) ->
         match extra with
         | Texp_refinement_obligation ty -> Some (ty, loc)
         | _ -> None)
      e.exp_extra
  in
  List.filter
    (fun ((ty : Types.type_expr), loc) ->
       let dup =
         List.exists
           (fun (n, ty') ->
              n == e && Ctype.is_equal e.exp_env false [ty] [ty'])
           st.seen
       in
       if dup then false
       else begin
         check_imposable e.exp_env ty loc;
         st.seen <- (e, ty) :: st.seen;
         true
       end)
    (markers @ imposed)

(* Finalize: this node is the subject of every imposition that reached
   it.  Lowering the subject may mint constants and deposit facts (value
   descriptions, codomain contracts, per-read mutable facts); those land
   in this obligation's own snapshot.  Obligations fail closed: a subject
   that cannot be represented is a located error, never a skip. *)
let finalize st scope ~imposed (e : Typedtree.expression) =
  match imposed with
  | [] -> ()
  | imposed ->
    let facts_ref = ref scope.facts in
    let seen_idents = ref [] in
    (match
       Vox_lower.lower_subject st.symbols
         ~on_resolved:(make_deposit st scope ~env:e.exp_env ~seen_idents
                         facts_ref)
         ~is_total_local:(is_total_local scope) e
     with
     | subject_ir ->
       List.iter
         (fun (ty, loc) ->
            st.pendings <-
              { subject = e
              ; subject_ir
              ; imposed = ty
              ; facts = !facts_ref
                (* an independent snapshot per pending: assembly mutates
                   the set, and a second obligation on the same subject
                   must still deposit the facts its own goal needs *)
              ; seen_idents = ref !seen_idents
              ; loc
              }
              :: st.pendings)
         imposed
     | exception Vox_lower.Unsupported { loc; reason } ->
       raise (Error (loc, Unrepresentable reason)))

(* A pushed obligation reports at the result position that receives it:
   the innermost arm/body/tail a failure would name, not the annotation
   that started the push. *)
let retarget imposed (target : Typedtree.expression) =
  List.map (fun (ty, _) -> ty, target.exp_loc) imposed

let rec walk_expr st scope ~imposed (e : Typedtree.expression) : unit =
  let imposed = impositions st e ~imposed in
  match e.exp_desc with
  (* ---- result positions: control shapes push the obligation into the
     positions that produce their value, each checked in its own fact
     scope ---- *)
  | Texp_ifthenelse (c, a, b_opt) when b_opt <> None || imposed = [] ->
    walk_expr st scope ~imposed:[] c;
    let condition = transparent_condition st scope c in
    let branch sense =
      match condition with
      | Some cf -> add_condition scope cf ~sense ~loc:c.exp_loc
      | None -> scope
    in
    walk_expr st (branch true) ~imposed:(retarget imposed a) a;
    Option.iter
      (fun b -> walk_expr st (branch false) ~imposed:(retarget imposed b) b)
      b_opt
  | Texp_match (scrut, _, cases, eff_cases, _) ->
    walk_expr st scope ~imposed:[] scrut;
    List.iter (fun c -> walk_case st scope ~imposed c) cases;
    List.iter (fun c -> walk_case st scope ~imposed c) eff_cases
  | Texp_try (body, cases, eff_cases) ->
    walk_expr st scope ~imposed:(retarget imposed body) body;
    List.iter (fun c -> walk_case st scope ~imposed c) cases;
    List.iter (fun c -> walk_case st scope ~imposed c) eff_cases
  | Texp_let (rec_flag, vbs, body) ->
    let scope = walk_bindings st scope rec_flag vbs in
    walk_expr st scope ~imposed:(retarget imposed body) body
  | Texp_letmutable (vb, body) ->
    walk_expr st scope ~imposed:[] vb.vb_expr;
    (* no binder fact (no stable symbol for it to be about), no let
       equality (stale after the first write); reads deposit instead *)
    let scope =
      match vb.vb_pat.pat_desc with
      | Tpat_var { id; _ } ->
        { scope with
          mutable_decls = (id, vb.vb_pat.pat_type) :: scope.mutable_decls
        }
      | _ -> scope
    in
    walk_expr st scope ~imposed:(retarget imposed body) body
  | Texp_sequence (e1, _, e2) ->
    walk_expr st scope ~imposed:[] e1;
    let scope =
      match e1.exp_desc with
      | Texp_assert (cond, _) ->
        (* under -noassert, translcore erases the assert
           (lambda/translcore.ml, [Texp_assert]) — except syntactic
           [assert false], which it keeps raising — so a fact from a test
           that never runs would be unsound and the arm is gated on the
           flag, mirroring translcore's own syntactic split *)
        let erased_under_noassert =
          !Clflags.noassert
          && (match cond.exp_desc with
              | Texp_construct (_, { cstr_name = "false"; _ }, _, _, _) ->
                false
              | _ -> true)
        in
        if erased_under_noassert
        then scope
        else (
          match transparent_condition st scope cond with
          | Some cf -> add_condition scope cf ~sense:true ~loc:cond.exp_loc
          | None -> scope)
      | _ -> scope
    in
    walk_expr st scope ~imposed:(retarget imposed e2) e2
  | Texp_open (od, body) ->
    walk_module_expr st scope od.open_expr;
    walk_expr st scope ~imposed:(retarget imposed body) body
  | Texp_letmodule (_, _, _, mexpr, body) ->
    walk_module_expr st scope mexpr;
    walk_expr st scope ~imposed:(retarget imposed body) body
  | Texp_letexception (_, body) ->
    walk_expr st scope ~imposed:(retarget imposed body) body
  (* ---- collection: applications pair the solved arrow spine with the
     argument list; &&/|| additionally give the right operand the left
     operand as a path condition ---- *)
  | Texp_apply (funct, args, _, _, _, _) ->
    finalize st scope ~imposed e;
    (match funct.exp_desc, args with
     | ( Texp_ident
           { desc = { val_kind = Val_prim { prim_name; _ }; _ }; _ },
         [(_, Arg (l, _)); (_, Arg (r, _))] )
       when prim_name = "%sequand" || prim_name = "%sequor" ->
       walk_expr st scope ~imposed:[] l;
       let r_scope =
         match transparent_condition st scope l with
         | Some cf ->
           add_condition scope cf ~sense:(prim_name = "%sequand")
             ~loc:l.exp_loc
         | None -> scope
       in
       walk_expr st r_scope ~imposed:[] r
     | _ ->
       walk_expr st scope ~imposed:[] funct;
       let rec go spine_ty = function
         | [] -> ()
         | (_, arg) :: rest ->
           let dom, cod =
             match
               Types.get_desc (Ctype.expand_head funct.exp_env spine_ty)
             with
             | Tarrow (_, dom, cod, _) -> mono dom, cod
             | _ ->
               (* a pairing failure is a walker defect: it must crash,
                  never become a dropped obligation *)
               Misc.fatal_error
                 (Format.asprintf
                    "Vox_verify: %a: application has more arguments than \
                     its function's arrow spine"
                    Location.print_loc e.exp_loc)
           in
           (match arg with
            | Omitted _ ->
              (* the apply's own type retains this arrow; the later
                 application that supplies the argument collects it *)
              ()
            | Arg (ae, _) ->
              let imposed_arg =
                match
                  Types.get_desc (Ctype.expand_head ae.exp_env dom)
                with
                | Trefine _ -> [dom, ae.exp_loc]
                | _ -> []
              in
              walk_expr st scope ~imposed:imposed_arg ae);
           go cod rest
       in
       go funct.exp_type args)
  (* ---- scopes ---- *)
  | Texp_function { params; body; _ } ->
    finalize st scope ~imposed e;
    (* own fact scope: facts never outlive the scope of the one
       evaluation they describe *)
    let fscope = { scope with facts = Vox_fact.empty } in
    let fscope =
      List.fold_left
        (fun fscope param ->
           match param.fp_kind with
           | Tparam_pat pat ->
             { fscope with
               facts =
                 add_binder_facts st fscope ~env:pat.pat_env pat
                   fscope.facts
             }
           | Tparam_optional_default (pat, default, _) ->
             walk_expr st fscope ~imposed:[] default;
             { fscope with
               facts =
                 add_binder_facts st fscope ~env:pat.pat_env pat
                   fscope.facts
             })
        fscope params
    in
    (match body with
     | Tfunction_body b -> walk_expr st fscope ~imposed:[] b
     | Tfunction_cases fc ->
       List.iter (fun c -> walk_case st fscope ~imposed:[] c) fc.fc_cases)
  | Texp_letop { let_; ands; body; _ } ->
    finalize st scope ~imposed e;
    walk_expr st scope ~imposed:[] let_.bop_exp;
    List.iter (fun bop -> walk_expr st scope ~imposed:[] bop.bop_exp) ands;
    walk_case st scope ~imposed:[] body
  | Texp_while { wh_cond; wh_body; _ } ->
    finalize st scope ~imposed e;
    walk_expr st scope ~imposed:[] wh_cond;
    (* loop bodies run many times: fresh fact scope, same rule as
       function bodies; while-condition facts are deferred *)
    walk_expr st { scope with facts = Vox_fact.empty } ~imposed:[] wh_body
  | Texp_for { for_from; for_to; for_body; _ } ->
    finalize st scope ~imposed e;
    walk_expr st scope ~imposed:[] for_from;
    walk_expr st scope ~imposed:[] for_to;
    walk_expr st { scope with facts = Vox_fact.empty } ~imposed:[] for_body
  (* ---- everything else: this node is a subject; children are visited
     generically in the current scope ---- *)
  | _ ->
    finalize st scope ~imposed e;
    walk_children st scope e

and walk_case :
  type k. state -> scope -> imposed:(Types.type_expr * Location.t) list ->
  k case -> unit =
  fun st scope ~imposed c ->
  let scope =
    { scope with
      facts =
        add_binder_facts st scope ~env:c.c_rhs.exp_env c.c_lhs scope.facts
    }
  in
  Option.iter (walk_expr st scope ~imposed:[]) c.c_guard;
  walk_expr st scope ~imposed:(retarget imposed c.c_rhs) c.c_rhs

(* Bindings: rhs walked in the outer scope; the returned scope carries the
   binder facts, the deposits the rhs lowering makes, and (immutable
   variable patterns only) the let equality.  The rhs is lowered for every
   binding pattern — a wildcard read [let _ = w in ...] deposits w's
   declared fact exactly as a named read does — while the equality
   additionally needs a stable symbol, which only a variable pattern
   binds.  The equality admits the opaque tier: the constant names exactly
   this evaluation, and the fact dies with the scope; the deposits
   (notably the apply-codomain fact) landing in the same scope is how a
   codomain contract reaches the bound name. *)
and walk_bindings st scope rec_flag vbs =
  (* a recursive peer may call an annotated-total binder of its own group,
     so a recursive group's evidence is folded in before its right-hand
     sides are walked and lowered; a nonrecursive right-hand side sees only
     the outer scope, where the group's binders do not exist *)
  let scope =
    match (rec_flag : Asttypes.rec_flag) with
    | Recursive -> add_total_binders scope vbs
    | Nonrecursive -> scope
  in
  List.iter (fun vb -> walk_expr st scope ~imposed:[] vb.vb_expr) vbs;
  List.fold_left
    (fun sc vb ->
       let sc =
         match vb.vb_pat.pat_desc with
         | Tpat_var { id; _ } when rhs_annotated_total vb.vb_expr ->
           { sc with total_locals = id :: sc.total_locals }
         | _ -> sc
       in
       let facts =
         add_binder_facts st sc ~env:vb.vb_pat.pat_env vb.vb_pat sc.facts
       in
       let facts =
         let facts_ref = ref facts in
         (match
            Vox_lower.lower_subject st.symbols
              ~on_resolved:
                (make_deposit st sc ~env:vb.vb_expr.exp_env facts_ref)
              ~is_total_local:(is_total_local sc) vb.vb_expr
          with
          | exception Vox_lower.Unsupported _ -> ()
          | rhs_ir ->
            (match vb.vb_pat.pat_desc, rec_flag with
             | Tpat_var { id; _ }, Asttypes.Nonrecursive ->
               let sort = rhs_ir.Ir.sort in
               let sym = Vox_lower.Symbols.value (Path.Pident id) in
               let x : Ir.t =
                 { desc = Var sym; sort; loc = vb.vb_pat.pat_loc }
               in
               facts_ref :=
                 Vox_fact.add !facts_ref
                   { desc = App (Eq, [x; rhs_ir])
                   ; sort = Bool
                   ; loc = vb.vb_pat.pat_loc
                   }
                   ~label:("let " ^ Ident.name id)
                   ~loc:vb.vb_pat.pat_loc
             | _ -> ()));
         !facts_ref
       in
       { sc with facts })
    scope vbs

and walk_children st scope e =
  let it = iterator st scope in
  Tast_iterator.default_iterator.expr it e

and walk_module_expr st scope mexpr =
  let it = iterator st scope in
  Tast_iterator.default_iterator.module_expr it mexpr

(* Structure items are walked left to right with total-binder evidence
   threaded across items — the one piece of a binding's scope a later item
   needs: a recursive [let rec f @ total]'s occurrences cannot read Total
   from their mode (the annotation caps the checking mode without pinning
   a rec binder's mode variable; the toplevel pins bindings at phrase end,
   the batch compiler does not), so the recorded [Texp_mode] extra is the
   only evidence, exactly as in [walk_bindings].  A recursive item is
   visited with its own group's evidence already folded in, for the same
   reason as in [walk_bindings]: a peer's right-hand side may call the
   group's total binder.  Facts and let equalities deliberately stay
   per-item: module-level equalities are a recorded completeness gap. *)
and walk_structure st scope (s : structure) =
  ignore
    (List.fold_left
       (fun scope (item : structure_item) ->
          let scope_after =
            match item.str_desc with
            | Tstr_value (_, vbs) -> add_total_binders scope vbs
            | _ -> scope
          in
          let scope_during =
            match item.str_desc with
            | Tstr_value (Recursive, _) -> scope_after
            | _ -> scope
          in
          let it : Tast_iterator.iterator = iterator st scope_during in
          it.structure_item it item;
          scope_after)
       scope s.str_items
     : scope)

(* The generic descent: everything the explicit arms do not shape
   (modules, classes, comprehensions, ...) is still visited, so an
   obligation can never be skipped by omission; only fact precision is
   lost there. *)
and iterator st scope =
  { Tast_iterator.default_iterator with
    expr = (fun _ ce -> walk_expr st scope ~imposed:[] ce)
  ; case = (fun (type k) _ (c : k case) -> walk_case st scope ~imposed:[] c)
  ; structure = (fun _ s -> walk_structure st scope s)
  }

let report_error ~loc = function
  | Unrepresentable reason ->
    Location.errorf ~loc
      "This expression cannot yet be represented in a verification \
       condition:@ %s."
      reason
  | Dependent_arrow ->
    Location.errorf ~loc
      "This application involves a dependent function type that cannot \
       yet be verified."
  | Stacked_refinement_heads ->
    Location.errorf ~loc
      "Consecutive refinement heads cannot yet be verified."
  | Predicate_reads_mutable ->
    Location.errorf ~loc
      "This predicate reads mutable state, which cannot yet be verified."
  | Not_verified n ->
    Location.errorf ~loc
      "%d refinement obligation%s not verified." n
      (if n = 1 then " was" else "s were")
  | Plan_error message -> Location.errorf ~loc "%s" message

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (report_error ~loc err)
    | _ -> None)

(* A best-effort rendering of counterexample values (term-valued models,
   see [Vox_backend.model]); [Bitvec] literals print as the OCaml ints
   they are. *)
let rec term_to_string : Vox_logic.Term.t -> string = function
  | Var name -> name
  | Const (Bool b) -> string_of_bool b
  | Const (Int digits) -> digits
  | Const (Bitvec { width; value }) ->
    let signed =
      if width >= 64
      then value
      else
        let m = Int64.shift_left 1L width in
        let masked = Int64.logand value (Int64.sub m 1L) in
        if Int64.equal (Int64.logand masked (Int64.shift_left 1L (width - 1)))
             0L
        then masked
        else Int64.sub masked m
    in
    Int64.to_string signed
  | Construct (c, []) -> c
  | Construct (c, args) ->
    Printf.sprintf "%s (%s)" c (String.concat ", " (List.map term_to_string args))
  | (App _ | Call _ | Ite _ | Select _ | Test _) as t ->
    (* not literal-shaped; models should not contain these, but a reader
       deserves something over nothing *)
    (match t with
     | App (_, args) | Call (_, args) ->
       Printf.sprintf "(%s)" (String.concat " " (List.map term_to_string args))
     | _ -> "<term>")

(* Per-obligation failure reports are emitted as they are found, so the
   editor's readout shows the state of the buffer; the one final refusal
   error is what fails the build. *)
let print_failure ~loc fmt =
  Format.kasprintf
    (fun message ->
       Location.print_report Format.std_formatter
         (Location.error ~loc message))
    fmt

let model_lines = function
  | None | Some [] -> ""
  | Some assignments ->
    String.concat ""
      (List.map
         (fun (name, term) ->
            Printf.sprintf "\n  %s = %s" name (term_to_string term))
         assignments)

(* One obligation through the backend: [true] when it failed.  In [Dump]
   the printing backend's expected non-verdict ([Ok (Unknown _)]) is
   suppressed; everything else — including a renderer [Error], a defect in
   this pass's output — reports and counts in both modes. *)
let discharge_outcome ~dump_only ~loc (outcome : Vox_backend.outcome) =
  match outcome with
  | Ok (Proved _) -> false
  | Ok (Unknown _) when dump_only -> false
  | Ok (Refuted model) ->
    print_failure ~loc
      "Refinement verification failed: the predicate is refutable.%s"
      (model_lines model);
    true
  | Ok (Unknown Timeout) ->
    print_failure ~loc
      "This refinement obligation could not be verified: the solver \
       timed out.";
    true
  | Ok (Unknown (Incomplete reason)) ->
    print_failure ~loc
      "This refinement obligation could not be verified (%s)." reason;
    true
  | Error (Unavailable message) ->
    (* cannot occur per obligation -- availability was checked at
       selection -- but a failure to run is still a failure *)
    print_failure ~loc "The solver backend is unavailable: %s." message;
    true
  | Error (Error { cause; raw = _ }) ->
    print_failure ~loc "The solver backend failed: %s." cause;
    true

(* Goal assembly: the imposed head's predicate, lowered and instantiated
   at the subject.  The predicate lowering deposits through the same hook
   as the subject's did (sharing the pending's once-per-obligation ident
   set), so a goal may lean on a declared value only its predicate
   mentions.  Obligations fail closed: anything the front end rejects is a
   located user error here. *)
let assemble st (p : pending) : Vox_logic.Obligation.t =
  let env = p.subject.exp_env in
  match Types.get_desc (Ctype.expand_head env p.imposed) with
  | Trefine { ref_payload; ref_pred; _ } ->
    let facts_ref = ref p.facts in
    (* only [Resolved_ident] can fire from the predicate front end (a
       mutable read in a predicate is a rejection, applications and field
       reads do not lower there), so the scope is inert *)
    let assembly_scope =
      { facts = Vox_fact.empty; mutable_decls = []; total_locals = [] }
    in
    let deposit =
      make_deposit st assembly_scope ~env ~seen_idents:p.seen_idents
        facts_ref
    in
    let goal_ir =
      try
        let hole_sort =
          Vox_lower.sort_of_type st.symbols ~loc:p.loc env ref_payload
        in
        let pred =
          Vox_lower.lower_predicate st.symbols ~on_resolved:deposit ~env
            ~hole_sort !ref_pred
        in
        Vox_lower.substitute_hole pred ~hole:p.subject_ir
      with
      | Vox_lower.Unsupported { loc; reason } ->
        raise (Error (loc, Unrepresentable reason))
    in
    let facts = !facts_ref in
    let signature =
      try
        Vox_lower.to_signature st.symbols ~loc:p.loc
          ~terms:(Vox_fact.terms facts @ [goal_ir])
      with Vox_lower.Unsupported { loc; reason } ->
        raise (Error (loc, Unrepresentable reason))
    in
    Vox_lower.canonicalise
      { signature
      ; hypotheses = Vox_fact.hypotheses facts
      ; goal = Vox_lower.emit goal_ir
      ; location = p.loc
      }
  | _ ->
    Misc.fatal_error "Vox_verify: imposed type lost its refined head"

let print_admissions st =
  match List.rev st.admissions with
  | [] -> ()
  | admissions ->
    Format.printf
      "Refinement verdicts are conditional on %d assumed contract%s:@."
      (List.length admissions)
      (if List.length admissions = 1 then "" else "s");
    List.iter
      (fun (path, ty) ->
         Format.printf "  %s : %a@." (Path.name path) Printtyp.type_expr ty)
      admissions

let implementation ~backend:(module B : Vox_backend.BACKEND) ~dump_only
    ~config (str : Typedtree.structure) =
  let st =
    { symbols = Vox_lower.Symbols.create ()
    ; pendings = []
    ; seen = []
    ; admissions = []
    }
  in
  let base =
    { facts = Vox_fact.empty; mutable_decls = []; total_locals = [] }
  in
  (try
     let it = iterator st base in
     it.structure it str
   with Vox_lower.Reads_mutable_state { loc } ->
     raise (Error (loc, Predicate_reads_mutable)));
  let pendings = List.rev st.pendings in
  (* Sequentially, in source order; the walk continues past a failure (a
     failed goal's spec already became a fact at walk time -- fact sources
     read declarations, not verdicts) and the unit is refused at exit,
     which is what carries the soundness that localisation spent. *)
  let failures =
    List.filter_map
      (fun (p : pending) ->
         let obligation =
           try assemble st p
           with Vox_lower.Reads_mutable_state { loc } ->
             raise (Error (loc, Predicate_reads_mutable))
         in
         if discharge_outcome ~dump_only ~loc:p.loc
              (B.discharge ~config obligation)
         then Some p.loc
         else None)
      pendings
  in
  if not dump_only then print_admissions st;
  match failures with
  | [] -> ()
  | first_loc :: _ ->
    raise (Error (first_loc, Not_verified (List.length failures)))

(* The driver glue behind [-vox-backend], shared by the batch compilers
   (Compile_common) and the toplevels (Topcommon): under the default
   ([none]) the pass does not run at all; otherwise the flags build the
   config -- [-vox-z3] falling back to the resolution order the test gate
   uses -- and [Vox_backend.plan] selects the backend, failing once, at
   selection.  A selection or configuration error has no source position:
   it reports at the unit's file-level ghost location ([Location.in_file],
   the convention of other whole-unit errors), never at a fabricated
   [_none_] position. *)
let run_if_enabled (str : Typedtree.structure) =
  match !Clflags.vox_backend with
  | "none" -> ()
  | backend_name ->
    let fail message =
      raise
        (Error (Location.in_file !Location.input_name, Plan_error message))
    in
    (* the name is validated before any solver-command resolution: a PATH
       search on behalf of a backend that does not exist, or that never
       reads the command, is work and failure surface for nothing *)
    (match Vox_backend.select backend_name with
     | Error message -> fail message
     | Ok (module Backend : Vox_backend.BACKEND) ->
       let config =
         { Vox_backend.Config.timeout_seconds = Some !Clflags.vox_timeout
         ; z3_command =
             (match !Clflags.vox_z3 with
              | Some _ as command -> command
              | None ->
                if String.equal Backend.name "z3"
                then Vox_backend.resolve_z3 ()
                else None)
         }
       in
       (match Vox_backend.plan ~backend_name ~config with
        | Error message -> fail message
        | Ok No_discharge -> ()
        | Ok (Dump backend) ->
          implementation ~backend ~dump_only:true ~config str
        | Ok (Discharge backend) ->
          implementation ~backend ~dump_only:false ~config str))
