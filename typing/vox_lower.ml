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

module Ir = struct
  type t =
    { desc : desc
    ; sort : Vox_logic.Sort.t
    ; loc : Location.t
    }

  and desc =
    | Var of string
    | Const of Vox_logic.Literal.t
    | App of Vox_logic.Op.t * t list
    | Call of string * t list
    | Ite of t * t * t
    | Construct of string * t list
    | Select of string * int * t
    | Test of string * t
    | Hole
    | Let of string * t * t
    | Lambda of string list * t
end

exception Unsupported of { loc : Location.t; reason : string }

let unsupported ~loc reason = raise (Unsupported { loc; reason })

type resolved =
  | Resolved_ident of Path.t * Types.value_description
  | Resolved_apply of Typedtree.expression
  | Resolved_field of Typedtree.expression * Data_types.label_description
  | Resolved_mutvar of Ident.t

module Symbols = struct
  type t =
    { mutable next_opaque : int
    ; variables : (string, Vox_logic.Sort.t) Hashtbl.t
    ; functions : (string, Vox_logic.Sort.t list * Vox_logic.Sort.t) Hashtbl.t
    ; mutable node_memo : (Typedtree.expression * Ir.t) list
          (* tier-1 opaque constants, memoized per node (physical identity):
             re-lowering a node yields the constant its first lowering
             minted, which is what lets a codomain fact and a let equality
             agree on one constant.  Reads of mutable variables are their
             own nodes, so per-node is per-read. *)
    }

  let create () =
    { next_opaque = 0
    ; variables = Hashtbl.create 16
    ; functions = Hashtbl.create 16
    ; node_memo = []
    }

  (* Resolved identity: the stamp keeps shadowed locals distinct
     ([Path.name] would drop it); module paths are already unambiguous as
     their dotted spelling. *)
  let symbol_of_path (path : Path.t) =
    match path with
    | Pident id -> Ident.unique_name id
    | _ -> Path.name path

  let value t path ~sort =
    let name = symbol_of_path path in
    Hashtbl.replace t.variables name sort;
    name

  (* The key is identity plus the ground sort signature of the use, so a
     polymorphic function used at two ground instantiations yields two
     declarations; the signature is mangled into the name in
     [Signature.instantiate]'s [name<key,...>] shape. *)
  let func t path ~params ~result =
    let keys = List.map Vox_logic.Sort.key (params @ [result]) in
    let name =
      Printf.sprintf "%s<%s>" (symbol_of_path path) (String.concat "," keys)
    in
    Hashtbl.replace t.functions name (params, result);
    name

  let fresh_opaque t ~sort =
    let n = t.next_opaque in
    t.next_opaque <- n + 1;
    let name = Printf.sprintf "result/%d" n in
    Hashtbl.replace t.variables name sort;
    name

  let to_signature _t =
    Misc.fatal_error "Vox_lower.Symbols.to_signature: not yet implemented"
end

let bv63 : Vox_logic.Sort.t = Bitvec 63

let ir desc sort loc : Ir.t = { Ir.desc; sort; loc }

let const_int n loc = ir (Ir.Const (Vox_logic.Literal.ocaml_int n)) bv63 loc

let rec sort_of_type ~loc env ty : Vox_logic.Sort.t =
  let ty = Ctype.expand_head env ty in
  match Types.get_desc ty with
  | Tconstr (p, _, _) when Path.same p Predef.path_bool -> Bool
  | Tconstr (p, _, _) when Path.same p Predef.path_int -> Bitvec 63
  | Tconstr (p, args, _) ->
    let name =
      match args with
      | [] -> Symbols.symbol_of_path p
      | args ->
        let keys =
          List.map
            (fun a -> Vox_logic.Sort.key (sort_of_type ~loc env a))
            args
        in
        Printf.sprintf "%s<%s>" (Symbols.symbol_of_path p)
          (String.concat "," keys)
    in
    (match Env.find_type p env with
     | { type_kind = Type_variant _ | Type_record _; _ } -> Datatype name
     | { type_kind = Type_abstract _; _ } -> Uninterpreted name
     | { type_kind = Type_open; _ } ->
       unsupported ~loc "its type is an open (extensible) variant"
     | { type_kind = Type_record_unboxed_product _; _ } ->
       unsupported ~loc "its type is an unboxed record"
     | exception Not_found -> Uninterpreted name)
  | Ttuple elts ->
    let keys =
      List.map
        (fun (_, t) -> Vox_logic.Sort.key (sort_of_type ~loc env t))
        elts
    in
    Datatype (Printf.sprintf "tuple<%s>" (String.concat "," keys))
  | Trefine { ref_payload; _ } -> sort_of_type ~loc env ref_payload
  | Tpoly _ -> sort_of_type ~loc env (Btype.tpoly_get_mono ty)
  | Tarrow _ -> unsupported ~loc "it has a function type"
  | Tvar _ | Tunivar _ -> unsupported ~loc "its type is not fully determined"
  | Tpackage _ -> unsupported ~loc "it has a first-class module type"
  | Tobject _ | Tfield _ | Tnil -> unsupported ~loc "it has an object type"
  | _ -> unsupported ~loc "its type cannot yet be given a sort"

(* The interpreted-operator table: (primitive, operand sorts) -> [Op].
   Drawn from the [primitive_is_total] set intersected with what [Op]
   expresses, plus comparisons and equality at the [int]/[bool] carriers
   (stable by construction: the lowered term contains no [Call]).  No
   Boolean ordering ([Op] has none); shifts are guarded because OCaml
   leaves out-of-range counts unspecified while the SMT primitives are
   total.  [/] and [mod] are deliberately absent (they raise). *)
let interpreted symbols ~prim ~(args : Ir.t list) ~result ~loc :
  Ir.t option =
  let open Vox_logic in
  let is_bv (a : Ir.t) = Sort.equal a.sort bv63 in
  let is_bool (a : Ir.t) = Sort.equal a.sort Sort.Bool in
  let app op args sort = Some (ir (Ir.App (op, args)) sort loc) in
  match prim, args with
  (* the sort guard keeps an identity that coerces (an [Obj.magic]-like
     use) out of the table; it falls to the gate like anything else *)
  | "%identity", [a] when Sort.equal a.sort result -> Some a
  | "%boolnot", [a] when is_bool a -> app Op.Not args Sort.Bool
  | "%sequand", [a; b] when is_bool a && is_bool b -> app Op.And args Sort.Bool
  | "%sequor", [a; b] when is_bool a && is_bool b -> app Op.Or args Sort.Bool
  | "%negint", [a] when is_bv a -> app Op.Bv_neg args bv63
  | "%succint", [a] when is_bv a -> app Op.Bv_add [a; const_int 1 loc] bv63
  | "%predint", [a] when is_bv a -> app Op.Bv_sub [a; const_int 1 loc] bv63
  | "%addint", [a; b] when is_bv a && is_bv b -> app Op.Bv_add args bv63
  | "%subint", [a; b] when is_bv a && is_bv b -> app Op.Bv_sub args bv63
  | "%mulint", [a; b] when is_bv a && is_bv b -> app Op.Bv_mul args bv63
  | "%andint", [a; b] when is_bv a && is_bv b -> app Op.Bv_and args bv63
  | "%orint", [a; b] when is_bv a && is_bv b -> app Op.Bv_or args bv63
  | "%xorint", [a; b] when is_bv a && is_bv b -> app Op.Bv_xor args bv63
  | ("%lslint" | "%lsrint" | "%asrint"), [x; n] when is_bv x && is_bv n ->
    (* Interpreted exactly where the two semantics provably coincide,
       opaque outside it: [Ite (0 <= n && n <= 62, shift, c)]. *)
    let op : Op.t =
      match prim with
      | "%lslint" -> Bv_shl
      | "%lsrint" -> Bv_lshr
      | _ -> Bv_ashr
    in
    let cmp op a b = ir (Ir.App (op, [a; b])) Sort.Bool loc in
    let in_range =
      ir
        (Ir.App
           ( Op.And,
             [ cmp Op.Bv_sle (const_int 0 loc) n;
               cmp Op.Bv_sle n (const_int 62 loc) ] ))
        Sort.Bool loc
    in
    let out_of_range =
      ir (Ir.Var (Symbols.fresh_opaque symbols ~sort:bv63)) bv63 loc
    in
    Some (ir (Ir.Ite (in_range, ir (Ir.App (op, args)) bv63 loc, out_of_range))
            bv63 loc)
  | "%equal", [a; b] when (is_bv a && is_bv b) || (is_bool a && is_bool b) ->
    app Op.Eq args Sort.Bool
  | "%notequal", [a; b] when (is_bv a && is_bv b) || (is_bool a && is_bool b)
    ->
    app Op.Distinct args Sort.Bool
  | "%lessthan", [a; b] when is_bv a && is_bv b -> app Op.Bv_slt args Sort.Bool
  | "%lessequal", [a; b] when is_bv a && is_bv b -> app Op.Bv_sle args Sort.Bool
  | "%greaterthan", [a; b] when is_bv a && is_bv b ->
    app Op.Bv_sgt args Sort.Bool
  | "%greaterequal", [a; b] when is_bv a && is_bv b ->
    app Op.Bv_sge args Sort.Bool
  | _ -> None

(* The stability gate's occurrence half: the funct is a path whose totality
   projection at this occurrence is [Total].  Conservative reading, no
   constraining: an unresolved totality is an absence of promise. *)
let occurrence_is_total (funct : Typedtree.expression) =
  match funct.exp_desc with
  | Texp_ident { mode; _ } ->
    (match
       Mode.Totality.Guts.check_const_conservative
         (Mode.Value.proj_comonadic Totality mode)
     with
     | Some Total -> true
     | Some Partial | None -> false)
  | _ -> false

(* The gate's argument half: every argument type crosses totality (no
   arrows -- the impure-parameter case) and logicality (no mutable parts --
   the mutable-read case).  A [Call] over an argument failing either could
   equate two calls straddling a write or an effect. *)
let crosses_totality_and_logicality env ty =
  let crossing = Ctype.crossing_of_ty env ty in
  let crosses (type a) (ax : a Mode.Crossing.Axis.t) =
    Mode.Crossing.Per_axis.le ax
      (Mode.Crossing.Per_axis.max ax)
      (Mode.Crossing.proj ax crossing)
  in
  crosses (Comonadic Totality) && crosses (Monadic Logicality)

let lower_subject symbols ?on_resolved (expr : Typedtree.expression) : Ir.t =
  let resolved r t =
    match on_resolved with None -> () | Some f -> f r t
  in
  let rec lower (e : Typedtree.expression) : Ir.t =
    let loc = e.exp_loc in
    let node_sort () = sort_of_type ~loc e.exp_env e.exp_type in
    let opaque () =
      match
        List.find_opt (fun (n, _) -> n == e) symbols.Symbols.node_memo
      with
      | Some (_, t) -> t
      | None ->
        let sort = node_sort () in
        let t = ir (Ir.Var (Symbols.fresh_opaque symbols ~sort)) sort loc in
        symbols.Symbols.node_memo <- (e, t) :: symbols.Symbols.node_memo;
        t
    in
    match e.exp_desc with
    | Texp_ident { path; desc; _ } ->
      let sort = node_sort () in
      let t = ir (Ir.Var (Symbols.value symbols path ~sort)) sort loc in
      resolved (Resolved_ident (path, desc)) t;
      t
    | Texp_constant (Const_int n) -> const_int n loc
    | Texp_construct (_, cstr, _, [], _)
      when Path.same (Data_types.cstr_res_type_path cstr) Predef.path_bool ->
      ir (Ir.Const (Vox_logic.Literal.Bool (String.equal cstr.cstr_name "true")))
        Bool loc
    | Texp_construct (_, cstr, _, args, _) ->
      let sort = node_sort () in
      ir
        (Ir.Construct (cstr.cstr_name, List.map (fun (_, a) -> lower a) args))
        sort loc
    | Texp_tuple (comps, _) ->
      (match node_sort () with
       | Datatype name as sort ->
         ir
           (Ir.Construct
              ("mk_" ^ name, List.map (fun (_, c) -> lower c) comps))
           sort loc
       | _ -> opaque ())
    | Texp_field { record; label; _ } ->
      (match label.lbl_mut with
       | Mutable _ -> opaque ()
       | Immutable ->
         (match
            sort_of_type ~loc:record.exp_loc record.exp_env record.exp_type
          with
          | Datatype record_name ->
            let t =
              ir
                (Ir.Select ("mk_" ^ record_name, label.lbl_pos, lower record))
                (node_sort ()) loc
            in
            resolved (Resolved_field (e, label)) t;
            t
          | _ -> opaque ()))
    | Texp_ifthenelse (c, a, Some b) ->
      let ci = lower c in
      if Vox_logic.Sort.equal ci.sort Bool
      then
        let ai = lower a in
        let bi = lower b in
        ir (Ir.Ite (ci, ai, bi)) ai.sort loc
      else opaque ()
    | Texp_sequence (_, _, tail) -> lower tail
    | Texp_open (_, body) -> lower body
    | Texp_letmodule (_, _, _, _, body) -> lower body
    | Texp_mutvar id ->
      let t = opaque () in
      resolved (Resolved_mutvar id.txt) t;
      t
    | Texp_apply (funct, args, _, _, _, _) ->
      let supplied =
        (* [None] when an argument is [Omitted]: the node is a partial
           application, function-sorted, and falls to the sort check. *)
        List.fold_right
          (fun (_, (arg : Typedtree.apply_arg)) acc ->
             match arg, acc with
             | Arg (ae, _), Some rest -> Some (ae :: rest)
             | Omitted _, _ | _, None -> None)
          args (Some [])
      in
      (match supplied with
       | None -> opaque ()
       | Some arg_exprs ->
         let stable () =
           occurrence_is_total funct
           && List.for_all
                (fun (a : Typedtree.expression) ->
                   crosses_totality_and_logicality a.exp_env a.exp_type)
                arg_exprs
         in
         let call path lowered =
           let params = List.map (fun (a : Ir.t) -> a.Ir.sort) lowered in
           let sort = node_sort () in
           let name = Symbols.func symbols path ~params ~result:sort in
           ir (Ir.Call (name, lowered)) sort loc
         in
         let t =
           match funct.exp_desc with
           | Texp_ident { path; desc = { val_kind = Val_prim p; _ }; _ } ->
             let lowered = List.map lower arg_exprs in
             (match
                interpreted symbols ~prim:p.prim_name ~args:lowered
                  ~result:(node_sort ()) ~loc
              with
              | Some t -> t
              | None -> if stable () then call path lowered else opaque ())
           | Texp_ident { path; _ } ->
             if stable () then call path (List.map lower arg_exprs)
             else opaque ()
           | _ -> opaque ()
         in
         (match t.desc with
          | Ir.App _ -> ()  (* interpreted rows never carry contracts *)
          | _ -> resolved (Resolved_apply e) t);
         t)
    | _ -> opaque ()
  in
  lower expr

let lower_predicate _symbols ~env:_ ~hole_sort:_
    (rexp : Types.refinement_expression) =
  (* The rexp front end (the located predicate sort checker) is the next
     stage; a located rejection keeps obligations fail-closed and lets
     fact sources decline fail-open until it lands. *)
  unsupported ~loc:rexp.rexp_loc
    "predicate lowering is not yet implemented"

let rec substitute_hole (ir : Ir.t) ~hole =
  let subst t = substitute_hole t ~hole in
  let desc : Ir.desc =
    match ir.desc with
    | Hole -> hole.Ir.desc
    | (Var _ | Const _) as desc -> desc
    | App (op, args) -> App (op, List.map subst args)
    | Call (f, args) -> Call (f, List.map subst args)
    | Ite (c, a, b) -> Ite (subst c, subst a, subst b)
    | Construct (c, args) -> Construct (c, List.map subst args)
    | Select (c, i, t) -> Select (c, i, subst t)
    | Test (c, t) -> Test (c, subst t)
    | Let (x, e, body) -> Let (x, subst e, subst body)
    | Lambda (xs, body) -> Lambda (xs, subst body)
  in
  { ir with desc }

let rec emit (ir : Ir.t) : Vox_logic.Term.t =
  match ir.desc with
  | Var name -> Var name
  | Const literal -> Const literal
  | App (op, args) -> App (op, List.map emit args)
  | Call (f, args) -> Call (f, List.map emit args)
  | Ite (c, a, b) -> Ite (emit c, emit a, emit b)
  | Construct (c, args) -> Construct (c, List.map emit args)
  | Select (c, i, t) -> Select (c, i, emit t)
  | Test (c, t) -> Test (c, emit t)
  | Hole -> Misc.fatal_error "Vox_lower.emit: residual hole"
  | Let _ -> Misc.fatal_error "Vox_lower.emit: residual let binder"
  | Lambda _ -> Misc.fatal_error "Vox_lower.emit: residual lambda binder"

let canonicalise _obligation =
  Misc.fatal_error "Vox_lower.canonicalise: not yet implemented"
