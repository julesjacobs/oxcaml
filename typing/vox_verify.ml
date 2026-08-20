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
  | Discharge_not_implemented of int

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
  ; loc : Location.t
  }
[@@warning "-69"] (* [subject]/[subject_ir] feed the goal (next stage) *)

type state =
  { symbols : Vox_lower.Symbols.t
  ; mutable pendings : pending list  (* newest first *)
  ; mutable seen : (Typedtree.expression * Types.type_expr) list
        (* consumer-side dedup: one pending per (subject node, imposed type
           up to [Ctype.is_equal]) -- markers and apply-arrow domains
           overlap (annotated arguments, the %ignore path) *)
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
  }

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
  | Rexp_apply (f, args) ->
    free ~bound f || List.exists (fun (_, a) -> free ~bound a) args
  | Rexp_tuple comps -> List.exists (fun (_, c) -> free ~bound c) comps
  | Rexp_construct (_, _, arg) ->
    (match arg with Some a -> free ~bound a | None -> false)
  | Rexp_field (r, _) -> free ~bound r
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
  | Trefine { ref_payload; ref_pred } ->
    (match Types.get_desc (Ctype.expand_head env ref_payload) with
     | Trefine _ -> raise (Error (loc, Stacked_refinement_heads))
     | _ -> ());
    if rexp_has_free_var ~bound:[] ref_pred
    then raise (Error (loc, Dependent_arrow))
  | _ ->
    (* every imposition source checked its head; reaching here is a
       walker defect, not a user error *)
    Misc.fatal_error "Vox_verify: imposed type without a refined head"

(* Fail-open predicate fact: instantiate a declared refined type's
   predicate at a subject term.  Needs the rexp front end, which is not
   yet implemented, so every source built on this declines today; the
   rules and their call sites are what this stage pins down. *)
let add_predicate_fact st ~env ~label ~loc ty ~subject_ir facts =
  match Types.get_desc (Ctype.expand_head env ty) with
  | Trefine { ref_payload; ref_pred } ->
    (try
       let hole_sort = Vox_lower.sort_of_type ~loc env ref_payload in
       let pred =
         Vox_lower.lower_predicate st.symbols ~env ~hole_sort ref_pred
       in
       Vox_fact.add facts
         (Vox_lower.substitute_hole pred ~hole:subject_ir)
         ~label ~loc
     with Vox_lower.Unsupported _ -> facts)
  | _ -> facts

(* Binder facts, unconditional: every bound ident whose recorded pattern
   type has a refined head deposits the instantiated predicate.  Variable
   and alias (sub-)patterns are exactly the idents [pat_bound_idents_full]
   reports; a refined head on a wildcard, constant, constructor or
   or-pattern node binds no single name and deposits nothing. *)
let add_binder_facts st ~env pat facts =
  List.fold_left
    (fun facts (id, (name : string Location.loc), ty, _, _) ->
       match Types.get_desc (Ctype.expand_head env ty) with
       | Trefine _ ->
         (try
            let sort = Vox_lower.sort_of_type ~loc:name.loc env ty in
            let sym =
              Vox_lower.Symbols.value st.symbols (Path.Pident id) ~sort
            in
            let subject_ir = { Ir.desc = Var sym; sort; loc = name.loc } in
            add_predicate_fact st ~env
              ~label:("binder " ^ Ident.name id)
              ~loc:name.loc ty ~subject_ir facts
          with Vox_lower.Unsupported _ -> facts)
       | _ -> facts)
    facts
    (pat_bound_idents_full pat)

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

(* The fact sources that ride on the subject lowering.  Deposits go to
   [facts_ref]: the obligation's own snapshot at finalization, or the
   scope under construction for a let equality / path condition. *)
let make_deposit st scope ~env facts_ref =
  let seen_idents = ref [] in
  fun (r : Vox_lower.resolved) (ir : Ir.t) ->
    match r with
    | Resolved_ident (path, vd) ->
      (* module-level and imported values: the occurrence is
         payload-typed, the value description still declares the refined
         type; once per obligation *)
      if not (List.exists (Path.same path) !seen_idents)
      then begin
        seen_idents := path :: !seen_idents;
        facts_ref :=
          add_predicate_fact st ~env
            ~label:("value " ^ Path.name path)
            ~loc:ir.Ir.loc vd.val_type ~subject_ir:ir !facts_ref
      end
    | Resolved_apply e ->
      (match apply_codomain e with
       | Some cod ->
         facts_ref :=
           add_predicate_fact st ~env ~label:"apply codomain"
             ~loc:e.exp_loc cod ~subject_ir:ir !facts_ref
       | None -> ())
    | Resolved_field (e, lbl) ->
      facts_ref :=
        add_predicate_fact st ~env
          ~label:("field " ^ lbl.lbl_name)
          ~loc:e.exp_loc lbl.lbl_arg ~subject_ir:ir !facts_ref
    | Resolved_mutvar id ->
      (match
         List.find_opt (fun (i, _) -> Ident.same i id) scope.mutable_decls
       with
       | Some (_, ty) ->
         facts_ref :=
           add_predicate_fact st ~env
             ~label:("read of " ^ Ident.name id)
             ~loc:ir.Ir.loc ty ~subject_ir:ir !facts_ref
       | None -> ())

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
  | Select (_, _, t) | Test (_, t) -> has_opaque t
  | Let (_, a, b) -> has_opaque a || has_opaque b
  | Lambda (_, t) -> has_opaque t

(* Lower a condition for a path-condition fact.  [None] when the gate
   declines: not transparently lowerable (or not Bool-sorted, which
   cannot happen for a typechecked condition but fails open anyway).
   Returns the lowered condition and the facts its lowering deposited. *)
let transparent_condition st scope (cond : Typedtree.expression) =
  let facts_ref = ref scope.facts in
  match
    Vox_lower.lower_subject st.symbols
      ~on_resolved:(make_deposit st scope ~env:cond.exp_env facts_ref)
      cond
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
    (match
       Vox_lower.lower_subject st.symbols
         ~on_resolved:(make_deposit st scope ~env:e.exp_env facts_ref)
         e
     with
     | subject_ir ->
       List.iter
         (fun (ty, loc) ->
            st.pendings <-
              { subject = e
              ; subject_ir
              ; imposed = ty
              ; facts = !facts_ref
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
        (match transparent_condition st scope cond with
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
                 add_binder_facts st ~env:pat.pat_env pat fscope.facts
             }
           | Tparam_optional_default (pat, default, _) ->
             walk_expr st fscope ~imposed:[] default;
             { fscope with
               facts =
                 add_binder_facts st ~env:pat.pat_env pat fscope.facts
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
      facts = add_binder_facts st ~env:c.c_rhs.exp_env c.c_lhs scope.facts
    }
  in
  Option.iter (walk_expr st scope ~imposed:[]) c.c_guard;
  walk_expr st scope ~imposed:(retarget imposed c.c_rhs) c.c_rhs

(* Bindings: rhs walked in the outer scope; the returned scope carries the
   binder facts and (immutable variable patterns only) the let equality.
   The equality admits the opaque tier -- the constant names exactly this
   evaluation, and the fact dies with the scope -- and its lowering's
   deposits (notably the apply-codomain fact) land in the same scope,
   which is how a codomain contract reaches the bound name. *)
and walk_bindings st scope rec_flag vbs =
  List.iter (fun vb -> walk_expr st scope ~imposed:[] vb.vb_expr) vbs;
  List.fold_left
    (fun sc vb ->
       let facts =
         add_binder_facts st ~env:vb.vb_pat.pat_env vb.vb_pat sc.facts
       in
       let facts =
         match vb.vb_pat.pat_desc, rec_flag with
         | Tpat_var { id; _ }, Asttypes.Nonrecursive ->
           (try
              let facts_ref = ref facts in
              let rhs_ir =
                Vox_lower.lower_subject st.symbols
                  ~on_resolved:
                    (make_deposit st sc ~env:vb.vb_expr.exp_env facts_ref)
                  vb.vb_expr
              in
              let sort = rhs_ir.Ir.sort in
              let sym =
                Vox_lower.Symbols.value st.symbols (Path.Pident id) ~sort
              in
              let x : Ir.t =
                { desc = Var sym; sort; loc = vb.vb_pat.pat_loc }
              in
              Vox_fact.add !facts_ref
                { desc = App (Eq, [x; rhs_ir])
                ; sort = Bool
                ; loc = vb.vb_pat.pat_loc
                }
                ~label:("let " ^ Ident.name id)
                ~loc:vb.vb_pat.pat_loc
            with Vox_lower.Unsupported _ -> facts)
         | _ -> facts
       in
       { sc with facts })
    scope vbs

and walk_children st scope e =
  let it = iterator st scope in
  Tast_iterator.default_iterator.expr it e

and walk_module_expr st scope mexpr =
  let it = iterator st scope in
  Tast_iterator.default_iterator.module_expr it mexpr

(* The generic descent: everything the explicit arms do not shape
   (modules, classes, comprehensions, ...) is still visited, so an
   obligation can never be skipped by omission; only fact precision is
   lost there. *)
and iterator st scope =
  { Tast_iterator.default_iterator with
    expr = (fun _ ce -> walk_expr st scope ~imposed:[] ce)
  ; case = (fun (type k) _ (c : k case) -> walk_case st scope ~imposed:[] c)
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
  | Discharge_not_implemented n ->
    Location.errorf ~loc
      "Refinement verification is not yet implemented:@ %d refinement \
       obligation%s collected but not discharged."
      n
      (if n = 1 then "" else "s")

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (report_error ~loc err)
    | _ -> None)

let implementation ~backend:_ ~dump_only ~config:_
    (str : Typedtree.structure) =
  let st =
    { symbols = Vox_lower.Symbols.create (); pendings = []; seen = [] }
  in
  let base = { facts = Vox_fact.empty; mutable_decls = [] } in
  let it = iterator st base in
  it.structure it str;
  let pendings = List.rev st.pendings in
  if dump_only
  then
    (* Scaffolding output: the pending stream and each obligation's
       hypothesis snapshot, replaced by the real query dump when the
       predicate front end and discharge land (next stage). *)
    List.iter
      (fun (p : pending) ->
         Format.printf "@[<2>%a:@ pending obligation:@ %a@]@."
           Location.print_loc p.loc Printtyp.type_expr p.imposed;
         List.iter
           (fun (h : Vox_logic.Obligation.hypothesis) ->
              Format.printf "  h%d: %s@." h.id h.origin.label)
           (Vox_fact.hypotheses p.facts))
      pendings
  else
    match pendings with
    | [] -> ()
    | { loc; _ } :: _ ->
      raise (Error (loc, Discharge_not_implemented (List.length pendings)))
