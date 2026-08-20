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
    | Hole
end

exception Unsupported of { loc : Location.t; reason : string }

let unsupported ~loc reason = raise (Unsupported { loc; reason })

exception Ill_sorted of { loc : Location.t; message : string }

let ill_sorted ~loc fmt =
  Format.kasprintf (fun message -> raise (Ill_sorted { loc; message })) fmt

exception Reads_mutable_state of { loc : Location.t }

type resolved =
  | Resolved_ident of Path.t * Types.value_description
  | Resolved_apply of Typedtree.expression
  | Resolved_field of Typedtree.expression * Data_types.label_description
  | Resolved_mutvar of Ident.t

module Symbols = struct
  type t =
    { mutable next_opaque : int
    ; mutable node_memo : (Typedtree.expression * Ir.t) list
          (* tier-1 opaque constants, memoized per node (physical identity):
             re-lowering a node yields the constant its first lowering
             minted, which is what lets a codomain fact and a let equality
             agree on one constant.  Reads of mutable variables are their
             own nodes, so per-node is per-read. *)
    ; datatype_decls : (string, Vox_logic.Datatype.decl) Hashtbl.t
          (* parametric declarations by declaration name, fed to
             [Signature.instantiate] at signature assembly *)
    ; datatype_roots :
        (string,
         string * Vox_logic.Datatype.ty list * Vox_logic.Sort.t list)
          Hashtbl.t
          (* ground instance name -> (declaration name, arguments, argument
             sorts): how a [Sort.Datatype] mentioned by a term recovers its
             root for [Signature.instantiate], and how a constructor use
             recovers the instance suffix [instantiate] mangles into member
             names *)
    ; mutable registering : string list
          (* declaration names whose fields are being built: a recursive
             field reference must not re-enter *)
    }

  let create () =
    { next_opaque = 0
    ; node_memo = []
    ; datatype_decls = Hashtbl.create 16
    ; datatype_roots = Hashtbl.create 16
    ; registering = []
    }

  (* Resolved identity: the stamp keeps shadowed idents distinct where
     [Path.name] would drop it — the head of a dotted path included, so
     two local modules [M] with different [t]s stay two symbols.  Globals
     carry [Ident.unique_name]'s fake [_0] stamp; the canonical
     renumbering gives locals and globals alike their per-obligation
     number. *)
  let rec symbol_of_path (path : Path.t) =
    match path with
    | Pident id -> Ident.unique_name id
    | Pdot (p, s) | Pextra_ty (p, Pcstr_ty s) -> symbol_of_path p ^ "." ^ s
    | Papply (p1, p2) ->
      symbol_of_path p1 ^ "(" ^ symbol_of_path p2 ^ ")"
    | Pextra_ty (p, Pext_ty) -> symbol_of_path p
    | Pextra_ty (p, Punboxed_ty) -> symbol_of_path p ^ "#"

  let value path = symbol_of_path path

  (* The key is identity plus the ground sort signature of the use, so a
     polymorphic function used at two ground instantiations yields two
     declarations; the signature is mangled into the name in
     [Signature.instantiate]'s [name<key,...>] shape. *)
  let func path ~params ~result =
    let keys = List.map Vox_logic.Sort.key (params @ [result]) in
    Printf.sprintf "%s<%s>" (symbol_of_path path) (String.concat "," keys)

  let fresh_opaque t =
    let n = t.next_opaque in
    t.next_opaque <- n + 1;
    Printf.sprintf "result/%d" n
end

let bv63 : Vox_logic.Sort.t = Bitvec 63

let ir desc sort loc : Ir.t = { Ir.desc; sort; loc }

let const_int n loc = ir (Ir.Const (Vox_logic.Literal.ocaml_int n)) bv63 loc

(* [Signature.instantiate]'s [name<key,...>] shape: one convention, two
   producers (the allocator here, [instantiate] for datatype instances). *)
let mangle name (sorts : Vox_logic.Sort.t list) =
  match sorts with
  | [] -> name
  | sorts ->
    Printf.sprintf "%s<%s>" name
      (String.concat "," (List.map Vox_logic.Sort.key sorts))

(* A ground [Datatype.ty] back from a sort; datatype sorts recover their
   root from the registry. *)
let ty_of_sort st (s : Vox_logic.Sort.t) : Vox_logic.Datatype.ty =
  match s with
  | Bool -> Bool
  | Int -> Int
  | Bitvec w -> Bitvec w
  | Uninterpreted n -> Uninterpreted n
  | Datatype n ->
    (match Hashtbl.find_opt st.Symbols.datatype_roots n with
     | Some (decl, args, _) -> Apply (decl, args)
     | None ->
       Misc.fatal_error
         ("Vox_lower: datatype sort without a registered root: " ^ n))

let record_root st instance_name decl_name args arg_sorts =
  if not (Hashtbl.mem st.Symbols.datatype_roots instance_name)
  then Hashtbl.replace st.Symbols.datatype_roots instance_name
         (decl_name, args, arg_sorts)

(* The member (constructor or selector) of a datatype instance, named the
   way [Signature.instantiate] names it: the member is qualified with the
   stamped declaration name — members of two datatypes share the solver's
   one namespace, so a bare [A] or a bare label would collide across
   declarations — and mangled with the instance's argument sorts. *)
let instance_member st ~instance member =
  match Hashtbl.find_opt st.Symbols.datatype_roots instance with
  | Some (decl_name, _, arg_sorts) ->
    mangle (decl_name ^ "." ^ member) arg_sorts
  | None ->
    Misc.fatal_error
      ("Vox_lower: datatype sort without a registered root: " ^ instance)

(* A record whose every value can change under a datatype's extensional
   equality is not one: a mutable record becomes an uninterpreted sort
   (its reads already abstract), never a datatype whose constructor would
   equate two states. *)
let record_is_immutable (labels : Types.label_declaration list) =
  List.for_all
    (fun (l : Types.label_declaration) ->
       match l.ld_mutable with
       | Immutable -> true
       | Mutable _ -> false)
    labels

(* The resolved identity of [Stdlib.Bigint.t]: the head of the normalized
   path must be the [Stdlib__Bigint] compilation unit itself
   ([normalize_type_path] resolves the [Stdlib.Bigint] alias to it).  A
   spelling comparison would also catch a user module literally named
   [Stdlib__Bigint], whose [t] is not this type. *)
let is_bigint env p =
  match Env.normalize_type_path None env p with
  | Pdot (Pident id, "t") ->
    Ident.is_global id && String.equal (Ident.name id) "Stdlib__Bigint"
  | _ -> false
  | exception Not_found -> false

(* SMT datatypes must be well-founded: every declared sort needs a value
   built from finitely many constructor applications, so a recursive
   variant group with no reachable base constructor ([type t = C of t]) is
   a declaration the solver rejects — and a strictly inductive reading
   would make the sort empty, turning every fact about its values vacuous.
   The OCaml type is inhabited (via cycles), so such a group lowers to a
   declared uninterpreted sort instead: its values stay sound opaque
   constants, and constructor reasoning over cyclic data is deferred with
   the rest of cyclic-data reasoning.  The check is a fixpoint over the
   registered declarations: a declaration is well-founded once some
   constructor has every field at an already-well-founded type.  A name
   with no registered declaration counts as well-founded — it is either
   not a datatype at all or a back-reference inside the group being
   registered, and the first query after registration completes settles
   the group for every caller that can mention it in a term. *)
let well_founded st name =
  match Hashtbl.find_opt st.Symbols.datatype_decls name with
  | None -> true
  | Some _ ->
    let wf : (string, unit) Hashtbl.t = Hashtbl.create 16 in
    let ty_wf : Vox_logic.Datatype.ty -> bool = function
      | Bool | Int | Bitvec _ | Uninterpreted _ | Param _ | Arrow _ -> true
      | Apply (n, _) ->
        Hashtbl.mem wf n
        || not (Hashtbl.mem st.Symbols.datatype_decls n)
    in
    let grounded (d : Vox_logic.Datatype.decl) =
      List.exists
        (fun (c : Vox_logic.Datatype.constructor) ->
           List.for_all (fun (_, ty) -> ty_wf ty) c.fields)
        d.constructors
    in
    let changed = ref true in
    while !changed do
      changed := false;
      Hashtbl.iter
        (fun n d ->
           if not (Hashtbl.mem wf n) && grounded d
           then begin
             Hashtbl.add wf n ();
             changed := true
           end)
        st.Symbols.datatype_decls
    done;
    Hashtbl.mem wf name

let rec sort_of_type st ~loc env ty : Vox_logic.Sort.t =
  let ty = Ctype.expand_head env ty in
  match Types.get_desc ty with
  | Tconstr (p, _, _) when Path.same p Predef.path_bool -> Bool
  | Tconstr (p, _, _) when Path.same p Predef.path_int -> Bitvec 63
  | Tconstr (p, [], _) when is_bigint env p -> Int
  | Tconstr (p, args, _) ->
    let name = Symbols.symbol_of_path p in
    let arg_sorts () = List.map (sort_of_type st ~loc env) args in
    (match Env.find_type p env with
     | { type_kind = Type_record (labels, _, _); _ }
       when not (record_is_immutable labels) ->
       Uninterpreted (mangle name (arg_sorts ()))
     | { type_kind = Type_variant _ | Type_record _; _ } as decl ->
       register_datatype st ~loc env p decl;
       if not (well_founded st name)
       then Uninterpreted (mangle name (arg_sorts ()))
       else begin
         let sorts = arg_sorts () in
         let instance = mangle name sorts in
         record_root st instance name
           (List.map (datatype_ty st ~loc env ~params:[]) args)
           sorts;
         Datatype instance
       end
     | { type_kind = Type_abstract _; _ } ->
       Uninterpreted (mangle name (arg_sorts ()))
     | { type_kind = Type_open; _ } ->
       unsupported ~loc "its type is an open (extensible) variant"
     | { type_kind = Type_record_unboxed_product _; _ } ->
       unsupported ~loc "its type is an unboxed record"
     | exception Not_found -> Uninterpreted (mangle name (arg_sorts ())))
  | Ttuple elts ->
    (match
       List.find_opt (fun (lbl, _) -> Option.is_some lbl) elts
     with
     | Some _ -> unsupported ~loc "its type is a labeled tuple"
     | None ->
       tuple_sort st ~loc
         (List.map (fun (_, t) -> sort_of_type st ~loc env t) elts))
  | Trefine { ref_payload; _ } -> sort_of_type st ~loc env ref_payload
  | Tpoly _ -> sort_of_type st ~loc env (Btype.tpoly_get_mono ty)
  | Tarrow _ -> unsupported ~loc "it has a function type"
  | Tvar _ | Tunivar _ -> unsupported ~loc "its type is not fully determined"
  | Tpackage _ -> unsupported ~loc "it has a first-class module type"
  | Tobject _ | Tfield _ | Tnil -> unsupported ~loc "it has an object type"
  | _ -> unsupported ~loc "its type cannot yet be given a sort"

(* Tuples are single-constructor datatypes, registered ground: the decl
   [tuple<keys>] with constructor [mk_tuple<keys>] and selectors
   [tuple<keys>.<i>]. *)
and tuple_sort st ~loc (sorts : Vox_logic.Sort.t list) : Vox_logic.Sort.t =
  let name = mangle "tuple" sorts in
  if not (Hashtbl.mem st.Symbols.datatype_decls name)
  then
    Hashtbl.replace st.Symbols.datatype_decls name
      { Vox_logic.Datatype.decl_name = name
      ; params = []
      ; constructors =
          [ { constructor_name = "mk_" ^ name
            ; fields =
                List.mapi
                  (fun i s -> Printf.sprintf "%s.%d" name i, ty_of_sort st s)
                  sorts
            } ]
      };
  record_root st name name [] [];
  ignore loc;
  Datatype name

(* The parametric declaration for a concrete variant or (immutable)
   record, fed to [Signature.instantiate] at signature assembly.  The
   subset is the doc's: regular closed variants with at least one
   constructor, records, tuples; the rest (GADT constructors, inline
   records, empty variants) is a located rejection, never a silent
   abstraction. *)
and register_datatype st ~loc env p (decl : Types.type_declaration) =
  let name = Symbols.symbol_of_path p in
  if not (Hashtbl.mem st.Symbols.datatype_decls name)
     && not (List.mem name st.Symbols.registering)
  then begin
    st.Symbols.registering <- name :: st.Symbols.registering;
    Fun.protect
      ~finally:(fun () ->
        st.Symbols.registering <- List.tl st.Symbols.registering)
      (fun () ->
         let params =
           List.mapi
             (fun i ty -> Types.get_id ty, Printf.sprintf "a%d" i)
             decl.type_params
         in
         let field_ty = datatype_ty st ~loc env ~params in
         (* Members (constructors, selectors) share the solver's one
            namespace across all datatypes, so every member is qualified
            with the stamped declaration name — the tuple-selector pattern
            — rather than emitted bare: two datatypes sharing a
            constructor or label name must not collide. *)
         let constructors =
           match decl.type_kind with
           | Type_record (labels, _, _) ->
             [ { Vox_logic.Datatype.constructor_name = "mk_" ^ name
               ; fields =
                   List.map
                     (fun (l : Types.label_declaration) ->
                        name ^ "." ^ Ident.name l.ld_id,
                        field_ty l.ld_type)
                     labels
               } ]
           | Type_variant ([], _, _) ->
             unsupported ~loc "its type is an empty variant"
           | Type_variant (cstrs, _, _) ->
             List.map
               (fun (cd : Types.constructor_declaration) ->
                  (match cd.cd_res with
                   | Some _ ->
                     unsupported ~loc "its type has a GADT constructor"
                   | None -> ());
                  let cname = name ^ "." ^ Ident.name cd.cd_id in
                  let fields =
                    match cd.cd_args with
                    | Cstr_tuple args ->
                      List.mapi
                        (fun i (ca : Types.constructor_argument) ->
                           Printf.sprintf "%s.%d" cname i,
                           field_ty ca.ca_type)
                        args
                    | Cstr_record _ ->
                      unsupported ~loc
                        "its type has an inline-record constructor"
                  in
                  { Vox_logic.Datatype.constructor_name = cname; fields })
               cstrs
           | Type_abstract _ | Type_open | Type_record_unboxed_product _ ->
             assert false
         in
         Hashtbl.replace st.Symbols.datatype_decls name
           { Vox_logic.Datatype.decl_name = name
           ; params = List.map snd params
           ; constructors
           })
  end

(* Field types of a declaration under registration: type parameters map to
   [Param]; arrows are represented only so [Signature.instantiate] can
   reject them; anything ground goes through the sort vocabulary. *)
and datatype_ty st ~loc env ~params ty : Vox_logic.Datatype.ty =
  let ty = Ctype.expand_head env ty in
  match Types.get_desc ty with
  | Tvar _ ->
    (match List.assoc_opt (Types.get_id ty) params with
     | Some name -> Param name
     | None -> unsupported ~loc "its type is not fully determined")
  | Trefine { ref_payload; _ } -> datatype_ty st ~loc env ~params ref_payload
  | Tarrow _ -> Arrow (Bool, Bool)
  | Tconstr (p, args, _) when args <> [] && params <> [] ->
    (match Env.find_type p env with
     | { type_kind = Type_record (labels, _, _); _ }
       when not (record_is_immutable labels) ->
       unsupported ~loc
         "its type has a mutable field at a type parameter"
     | { type_kind = Type_variant _ | Type_record _; _ } as decl ->
       register_datatype st ~loc env p decl;
       if well_founded st (Symbols.symbol_of_path p)
       then
         Apply
           (Symbols.symbol_of_path p,
            List.map (datatype_ty st ~loc env ~params) args)
       else
         (* the non-well-founded field type falls to the sort vocabulary,
            which grounds it as an uninterpreted sort (or rejects it,
            located, when its arguments mention the enclosing parameters) *)
         ty_of_sort st (sort_of_type st ~loc env ty)
     | _ -> ty_of_sort st (sort_of_type st ~loc env ty)
     | exception Not_found -> ty_of_sort st (sort_of_type st ~loc env ty))
  | _ -> ty_of_sort st (sort_of_type st ~loc env ty)

(* Value symbols are sort-sensitive, the discipline function symbols
   already follow: a polymorphic value used at two ground sorts in one
   obligation must be two SMT constants — declaring [nil] once at
   [list<Bv63>] and using it at [list<Bool>] is a query the solver
   rejects.  A value whose declared type already grounds to the
   occurrence's sort keeps its bare stamped name: the binder-fact and
   let-equality sites build exactly that name from the declared type, so
   the identities agree.  Only an occurrence the declaration cannot name
   on its own (an instantiated type variable) mangles the occurrence sort
   in, [Symbols.func]-style.  The declared type is read from the
   environment: an occurrence's own description is already instantiated
   at the use's type and cannot tell the two cases apart. *)
let value_symbol st ~loc env path sort =
  let name = Symbols.value path in
  let declares_sort =
    match
      sort_of_type st ~loc env
        (Subst.Lazy.force_value_description (Env.find_value path env))
          .val_type
    with
    | declared -> Vox_logic.Sort.equal declared sort
    | exception (Unsupported _ | Not_found) -> false
  in
  if declares_sort then name else mangle name [sort]

(* Close an obligation's signature over exactly the symbols its terms
   mention, in first-occurrence order (hypotheses, then goal), plus the
   datatype declarations reachable from any mentioned sort, run through
   [Signature.instantiate].  Determinism matters: the declaration order in
   the rendered script is this order. *)
let to_signature st ~loc ~(terms : Ir.t list) : Vox_logic.Signature.t =
  let vars = ref [] in
  let funcs = ref [] in
  let uninterp = ref [] in
  let roots = ref [] in
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let seen_sort : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let note_uninterpreted n =
    if not (Hashtbl.mem seen_sort n)
    then begin
      Hashtbl.add seen_sort n ();
      uninterp := n :: !uninterp
    end
  in
  let note_sort (s : Vox_logic.Sort.t) =
    match s with
    | Uninterpreted n -> note_uninterpreted n
    | Datatype n ->
      if not (Hashtbl.mem seen_sort n)
      then begin
        Hashtbl.add seen_sort n ();
        match Hashtbl.find_opt st.Symbols.datatype_roots n with
        | Some (decl, args, _) -> roots := (decl, args) :: !roots
        | None ->
          Misc.fatal_error
            ("Vox_lower: datatype sort without a registered root: " ^ n)
      end
    | Bool | Int | Bitvec _ -> ()
  in
  let rec walk (t : Ir.t) =
    note_sort t.sort;
    match t.desc with
    | Var name ->
      if not (Hashtbl.mem seen name)
      then begin
        Hashtbl.add seen name ();
        vars := (name, t.sort) :: !vars
      end
    | Const _ | Hole -> ()
    | Call (name, args) ->
      if not (Hashtbl.mem seen name)
      then begin
        Hashtbl.add seen name ();
        funcs :=
          (name, List.map (fun (a : Ir.t) -> a.Ir.sort) args, t.sort)
          :: !funcs
      end;
      List.iter walk args
    | App (_, args) | Construct (_, args) -> List.iter walk args
    | Ite (a, b, c) -> walk a; walk b; walk c
    | Select (_, _, x) -> walk x
  in
  List.iter walk terms;
  let decls =
    Hashtbl.fold (fun _ d acc -> d :: acc) st.Symbols.datatype_decls []
  in
  match Vox_logic.Signature.instantiate decls (List.rev !roots) with
  | Error message -> unsupported ~loc message
  | Ok (datatypes, _) ->
    (* uninterpreted sorts reachable only through datatype fields still
       need declaring; instantiate already produced every reachable
       datatype *)
    List.iter
      (fun (d : Vox_logic.Signature.datatype) ->
         List.iter
           (fun (c : Vox_logic.Signature.constructor) ->
              List.iter
                (fun (_, s) ->
                   match (s : Vox_logic.Sort.t) with
                   | Uninterpreted n -> note_uninterpreted n
                   | _ -> ())
                c.fields)
           d.constructors)
      datatypes;
    { sorts = List.rev !uninterp
    ; datatypes
    ; variables = List.rev !vars
    ; functions = List.rev !funcs
    }

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
     use) out of the table; it falls to the gate like anything else.
     [result] is [None] in the predicate front end, which has no expected
     result sort to guard with. *)
  | "%identity", [a]
    when (match result with
          | Some r -> Sort.equal a.sort r
          | None -> false) ->
    Some a
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
      ir (Ir.Var (Symbols.fresh_opaque symbols)) bv63 loc
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
(* Local [let f @ total] bindings do not pin the binder's mode variable
   (the annotation caps the expected mode the right-hand side is checked
   at), so their occurrences never read Total conservatively; the walker
   supplies [is_total_local] from the binding's recorded [Texp_mode]
   annotation instead. *)
let occurrence_is_total ~is_total_local (funct : Typedtree.expression) =
  match funct.exp_desc with
  | Texp_ident { path; mode; _ } ->
    (match
       Mode.Totality.Guts.check_const_conservative
         (Mode.Value.proj_comonadic Totality mode)
     with
     | Some Total -> true
     | Some Partial | None ->
       (match path with
        | Pident id -> is_total_local id
        | _ -> false))
  | _ -> false

(* The gate's argument half: every argument type crosses totality (no
   arrows -- the impure-parameter case) and logicality (no mutable parts --
   the mutable-read case).  A [Call] over an argument failing either could
   equate two calls straddling a write or an effect. *)
(* In the crossing lattice, smaller means more crossing (typing/mode.ml,
   [Crossing]): a type fully crosses an axis iff its projection is [min]. *)
let crosses_axis (type a) env ty (ax : a Mode.Crossing.Axis.t) =
  let crossing = Ctype.crossing_of_ty env ty in
  Mode.Crossing.Per_axis.le ax
    (Mode.Crossing.proj ax crossing)
    (Mode.Crossing.Per_axis.min ax)

let crosses_totality_and_logicality env ty =
  crosses_axis env ty (Comonadic Totality)
  && crosses_axis env ty (Monadic Logicality)

let crosses_logicality env ty = crosses_axis env ty (Monadic Logicality)

let lower_subject symbols ?on_resolved ?(is_total_local = fun _ -> false)
    (expr : Typedtree.expression) : Ir.t =
  let resolved r t =
    match on_resolved with None -> () | Some f -> f r t
  in
  let rec lower (e : Typedtree.expression) : Ir.t =
    let loc = e.exp_loc in
    let node_sort () = sort_of_type symbols ~loc e.exp_env e.exp_type in
    let opaque () =
      match
        List.find_opt (fun (n, _) -> n == e) symbols.Symbols.node_memo
      with
      | Some (_, t) -> t
      | None ->
        let sort = node_sort () in
        let t = ir (Ir.Var (Symbols.fresh_opaque symbols)) sort loc in
        symbols.Symbols.node_memo <- (e, t) :: symbols.Symbols.node_memo;
        t
    in
    match e.exp_desc with
    | Texp_ident { path; desc; _ } ->
      let sort = node_sort () in
      let t =
        ir (Ir.Var (value_symbol symbols ~loc e.exp_env path sort)) sort loc
      in
      resolved (Resolved_ident (path, desc)) t;
      t
    | Texp_constant (Const_int n) -> const_int n loc
    | Texp_construct (_, cstr, _, [], _)
      when Path.same (Data_types.cstr_res_type_path cstr) Predef.path_bool ->
      ir (Ir.Const (Vox_logic.Literal.Bool (String.equal cstr.cstr_name "true")))
        Bool loc
    | Texp_construct (_, cstr, _, args, _) ->
      (* the constructor is named as the instantiated declaration names it
         (qualified, instance-mangled), so the term and the signature
         agree *)
      (match node_sort () with
       | Datatype instance as sort ->
         ir
           (Ir.Construct
              (instance_member symbols ~instance cstr.cstr_name,
               List.map (fun (_, a) -> lower a) args))
           sort loc
       | _ -> opaque ())
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
            sort_of_type symbols ~loc:record.exp_loc record.exp_env
              record.exp_type
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
           occurrence_is_total ~is_total_local funct
           && List.for_all
                (fun (a : Typedtree.expression) ->
                   crosses_totality_and_logicality a.exp_env a.exp_type)
                arg_exprs
         in
         let call path lowered =
           let params = List.map (fun (a : Ir.t) -> a.Ir.sort) lowered in
           let sort = node_sort () in
           let name = Symbols.func path ~params ~result:sort in
           ir (Ir.Call (name, lowered)) sort loc
         in
         let t =
           match funct.exp_desc with
           | Texp_ident { path; desc = { val_kind = Val_prim p; _ }; _ } ->
             let lowered = List.map lower arg_exprs in
             (match
                interpreted symbols ~prim:p.prim_name ~args:lowered
                  ~result:(Some (node_sort ())) ~loc
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

(* The primitives the operator table knows: an application of one of these
   whose operand sorts fit no row is a predicate sort error ([int{ 1 + true }]
   dies here, as an error the user can read), while a primitive outside the
   table is an unsupported-construct rejection. *)
let table_prims =
  [ "%boolnot"; "%sequand"; "%sequor"; "%negint"; "%succint"; "%predint"
  ; "%addint"; "%subint"; "%mulint"; "%andint"; "%orint"; "%xorint"
  ; "%lslint"; "%lsrint"; "%asrint"; "%equal"; "%notequal"; "%lessthan"
  ; "%lessequal"; "%greaterthan"; "%greaterequal" ]

let sort_key = Vox_logic.Sort.key

(* Predicate front end: rexp -> IR.  A located sort checker (rexp is
   untyped and nothing upstream or downstream checks predicate sorts) and
   a normaliser to the quantifier-free fragment: [let]s substitute (as
   binder-environment entries), applied lambdas beta-reduce, [match]
   lowers to [Ite]/[Select] with equality tests (the day-one matchable
   subjects — tuples, integer and Boolean patterns — need no
   [Term.Test]); any residual binder form is a located rejection.  A free mention of a mutable variable, or of a value whose
   type does not cross logicality, is [Reads_mutable_state]: no predicate
   over mutable state has one denotation, so this rejection is fail-closed
   even for facts. *)
let lower_predicate symbols ?on_resolved ~env ~hole_sort
    (rexp : Types.refinement_expression) : Ir.t =
  let resolved r t =
    match on_resolved with None -> () | Some f -> f r t
  in
  let require_sort ~loc ~what (t : Ir.t) sort =
    if not (Vox_logic.Sort.equal t.Ir.sort sort)
    then
      ill_sorted ~loc "%s has sort %s where %s was expected" what
        (sort_key t.Ir.sort) (sort_key sort)
  in
  let conjunction ~loc = function
    | [] -> ir (Ir.Const (Vox_logic.Literal.Bool true)) Bool loc
    | [c] -> c
    | cs -> ir (Ir.App (And, cs)) Bool loc
  in
  (* The ground constructors of a datatype a predicate matches on;
     day one only ground declarations (tuples, monomorphic records and
     variants) are matchable. *)
  let ground_constructors ~loc name =
    let fail () =
      unsupported ~loc
        "matching on this subject is not yet supported in predicates"
    in
    match Hashtbl.find_opt symbols.Symbols.datatype_roots name with
    | Some (decl_name, [], _) ->
      (match Hashtbl.find_opt symbols.Symbols.datatype_decls decl_name with
       | Some { params = []; constructors; _ } ->
         List.map
           (fun (c : Vox_logic.Datatype.constructor) ->
              ( c.constructor_name,
                List.map
                  (fun (sel, ty) ->
                     let sort : Vox_logic.Sort.t =
                       match (ty : Vox_logic.Datatype.ty) with
                       | Bool -> Bool
                       | Int -> Int
                       | Bitvec w -> Bitvec w
                       | Uninterpreted n -> Uninterpreted n
                       | Apply (n, []) -> Datatype n
                       | Apply _ | Param _ | Arrow _ -> fail ()
                     in
                     sel, sort)
                  c.fields ))
           constructors
       | Some _ | None -> fail ())
    | Some _ | None -> fail ()
  in
  let bool_constructor path =
    match (path : Path.t) with
    | Pextra_ty (p, Pcstr_ty name) when Path.same p Predef.path_bool ->
      Some (String.equal name "true")
    | _ -> None
  in
  let rec lower binders (r : Types.refinement_expression) : Ir.t =
    let loc = r.rexp_loc in
    match r.rexp_desc with
    | Rexp_hole -> ir Ir.Hole hole_sort loc
    | Rexp_var id ->
      (match
         List.find_opt (fun (i, _) -> Ident.same i id) binders
       with
       | Some (_, t) -> t
       | None ->
         (* an arrow binder that escaped the upstream rejection through
            higher-order solving; the caller words the dependent-arrow
            error, this is only reached from fact sources *)
         unsupported ~loc
           "this predicate depends on a function-argument binder")
    | Rexp_ident (path, _) ->
      (match Subst.Lazy.force_value_description (Env.find_value path env) with
       | vd ->
         (match vd.val_kind with
          | Val_mut _ -> raise (Reads_mutable_state { loc })
          | _ ->
            if not (crosses_logicality env vd.val_type)
            then raise (Reads_mutable_state { loc });
            let sort = sort_of_type symbols ~loc env vd.val_type in
            let t =
              ir (Ir.Var (value_symbol symbols ~loc env path sort)) sort loc
            in
            (* the same deposit rule as the subject front end: resolving a
               free ident whose declared type is refined deposits the
               instantiated fact — a goal's predicate may lean on a
               declared value the subject never mentions *)
            resolved (Resolved_ident (path, vd)) t;
            t)
       | exception Not_found ->
         unsupported ~loc "this name cannot be resolved at verification time")
    | Rexp_constant { pconst_desc = Pconst_integer (digits, None); _ } ->
      (match int_of_string_opt digits with
       | Some n -> const_int n loc
       | None -> unsupported ~loc "this integer literal cannot be read")
    | Rexp_constant _ ->
      unsupported ~loc "this literal cannot yet appear in a predicate"
    | Rexp_apply (f, args) ->
      let args =
        List.map
          (fun ((lbl : Asttypes.arg_label), a) ->
             match lbl with
             | Nolabel -> a
             | Labelled _ | Optional _ ->
               unsupported ~loc:a.Types.rexp_loc
                 "labeled arguments cannot yet appear in a predicate")
          args
      in
      (match f.rexp_desc with
       | Rexp_fun _ ->
         let lowered = List.map (lower binders) args in
         let rec beta binders (f : Types.refinement_expression) = function
           | [] -> lower binders f
           | a :: rest ->
             (match f.rexp_desc with
              | Rexp_fun (id, body) -> beta ((id, a) :: binders) body rest
              | _ ->
                unsupported ~loc
                  "this application cannot yet be verified")
         in
         beta binders f lowered
       | Rexp_ident (path, _) ->
         (match
            Subst.Lazy.force_value_description (Env.find_value path env)
          with
          | { val_kind = Val_prim p; _ } ->
            let lowered = List.map (lower binders) args in
            (match
               interpreted symbols ~prim:p.prim_name ~args:lowered
                 ~result:None ~loc
             with
             | Some t -> t
             | None ->
               if List.mem p.prim_name table_prims
               then
                 ill_sorted ~loc "%s is applied to operand(s) of sort %s"
                   (Path.name path)
                   (String.concat ", "
                      (List.map
                         (fun (a : Ir.t) -> sort_key a.Ir.sort)
                         lowered))
               else
                 unsupported ~loc
                   (Printf.sprintf
                      "the primitive %s cannot yet appear in a predicate"
                      (Path.name path)))
          | _ ->
            unsupported ~loc
              "calling a function in a predicate is not yet supported"
          | exception Not_found ->
            unsupported ~loc
              "this name cannot be resolved at verification time")
       | _ -> unsupported ~loc "this application cannot yet be verified")
    | Rexp_tuple comps ->
      let comps =
        List.map
          (fun (lbl, c) ->
             match lbl with
             | None -> c
             | Some _ ->
               unsupported ~loc "labeled tuples cannot yet appear here")
          comps
      in
      let lowered = List.map (lower binders) comps in
      let sort =
        tuple_sort symbols ~loc (List.map (fun (t : Ir.t) -> t.Ir.sort) lowered)
      in
      let name =
        match (sort : Vox_logic.Sort.t) with
        | Datatype n -> n
        | _ -> assert false
      in
      ir (Ir.Construct ("mk_" ^ name, lowered)) sort loc
    | Rexp_construct (path, _, None) ->
      (match bool_constructor path with
       | Some b -> ir (Ir.Const (Vox_logic.Literal.Bool b)) Bool loc
       | None ->
         unsupported ~loc
           "constructors cannot yet appear in a predicate")
    | Rexp_construct _ ->
      unsupported ~loc "constructors cannot yet appear in a predicate"
    | Rexp_field _ ->
      unsupported ~loc "field access cannot yet appear in a predicate"
    | Rexp_ifthenelse (c, a, Some b) ->
      let ci = lower binders c in
      require_sort ~loc ~what:"this condition" ci Bool;
      let ai = lower binders a in
      let bi = lower binders b in
      require_sort ~loc ~what:"the else branch" bi ai.Ir.sort;
      ir (Ir.Ite (ci, ai, bi)) ai.Ir.sort loc
    | Rexp_ifthenelse (_, _, None) ->
      unsupported ~loc "an if without an else cannot appear in a predicate"
    | Rexp_let ({ rb_ident; rb_expr }, body) ->
      lower ((rb_ident, lower binders rb_expr) :: binders) body
    | Rexp_fun _ ->
      unsupported ~loc
        "an unapplied function cannot appear in a verification condition"
    | Rexp_match (scrut, cases) ->
      let s = lower binders scrut in
      let rec case_chain = function
        | [] ->
          unsupported ~loc "this predicate match may be inexhaustive"
        | (c : Types.refinement_case) :: rest ->
          let conds, bindings = lower_pattern s c.rc_lhs in
          let binders = bindings @ binders in
          let conds =
            conds
            @ (match c.rc_guard with
               | None -> []
               | Some g ->
                 let gi = lower binders g in
                 require_sort ~loc:g.rexp_loc ~what:"this guard" gi Bool;
                 [gi])
          in
          let rhs = lower binders c.rc_rhs in
          (match conds with
           | [] -> rhs (* irrefutable, unguarded: later cases unreachable *)
           | conds ->
             let rest_ir = case_chain rest in
             require_sort ~loc ~what:"this match case" rest_ir rhs.Ir.sort;
             ir (Ir.Ite (conjunction ~loc conds, rhs, rest_ir))
               rhs.Ir.sort loc)
      in
      case_chain cases
    | Rexp_constraint (r, ty) ->
      let t = lower binders r in
      let sort = sort_of_type symbols ~loc env ty in
      require_sort ~loc ~what:"this constrained expression" t sort;
      t
  (* A pattern against a scrutinee term: the tests that select the case
     and the binder instantiations it makes, both as terms about the one
     scrutinee. *)
  and lower_pattern (scrut : Ir.t) (p : Types.refinement_pattern) :
    Ir.t list * (Ident.t * Ir.t) list =
    let loc = p.rpat_loc in
    match p.rpat_desc with
    | Rpat_any -> [], []
    | Rpat_var id -> [], [id, scrut]
    | Rpat_alias (p, id) ->
      let conds, bindings = lower_pattern scrut p in
      conds, (id, scrut) :: bindings
    | Rpat_constant { pconst_desc = Pconst_integer (digits, None); _ } ->
      require_sort ~loc ~what:"this pattern's subject" scrut bv63;
      (match int_of_string_opt digits with
       | Some n ->
         [ir (Ir.App (Eq, [scrut; const_int n loc])) Bool loc], []
       | None -> unsupported ~loc "this integer literal cannot be read")
    | Rpat_constant _ ->
      unsupported ~loc "this pattern cannot yet appear in a predicate"
    | Rpat_tuple ps ->
      let ps =
        List.map
          (fun (lbl, p) ->
             match lbl with
             | None -> p
             | Some _ ->
               unsupported ~loc "labeled tuples cannot yet appear here")
          ps
      in
      (match (scrut.Ir.sort : Vox_logic.Sort.t) with
       | Datatype name ->
         (match ground_constructors ~loc name with
          | [(cname, fields)] when List.length fields = List.length ps ->
            List.fold_left2
              (fun (conds, bindings) (i, (_, field_sort)) p ->
                 let sub =
                   ir (Ir.Select (cname, i, scrut)) field_sort loc
                 in
                 let c, b = lower_pattern sub p in
                 conds @ c, bindings @ b)
              ([], [])
              (List.mapi (fun i f -> i, f) fields)
              ps
          | _ ->
            ill_sorted ~loc
              "a tuple pattern is matched against a subject of sort %s"
              (sort_key scrut.Ir.sort))
       | _ ->
         ill_sorted ~loc
           "a tuple pattern is matched against a subject of sort %s"
           (sort_key scrut.Ir.sort))
    | Rpat_construct (path, _, arg) ->
      (match bool_constructor path, arg with
       | Some b, None ->
         require_sort ~loc ~what:"this pattern's subject" scrut Bool;
         ( [ ir
               (Ir.App
                  (Eq, [scrut; ir (Ir.Const (Bool b)) Bool loc]))
               Bool loc ],
           [] )
       | _ ->
         unsupported ~loc
           "constructor patterns cannot yet appear in a predicate")
  in
  let t = lower [] rexp in
  require_sort ~loc:rexp.rexp_loc ~what:"this predicate" t Bool;
  t

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
  | Hole -> Misc.fatal_error "Vox_lower.emit: residual hole"

(* Renumber an obligation's symbols deterministically so baselines do not
   churn when unrelated edits shift [Ident] stamps: [base_<stamp>] (with an
   optional [<...>] instance suffix) becomes [base_<n>] with [n] assigned
   per base in first-occurrence order (the same stamp keeps one [n] across
   its instance suffixes), and [result/<counter>] renumbers in
   first-occurrence order.  Names renumber per delimiter-separated
   segment — dots, and the [< > , ( ) #] of instance suffixes and functor
   paths — so a qualified datatype member ([box_<stamp>.first_pos]) stays
   consistent with its datatype's own renaming, and a stamp inside an
   instance suffix ([wrap_<n><leaf_<stamp>>]) renumbers with the sort it
   names; unstamped segments (predef names, tuple instances, sort keys)
   pass through.  Occurrence order is hypotheses, then goal, then the
   signature — which is itself in first-occurrence term order, so the two
   agree. *)
let canonicalise (ob : Vox_logic.Obligation.t) : Vox_logic.Obligation.t =
  let renames : (string, string) Hashtbl.t = Hashtbl.create 16 in
  let stamp_numbers : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let base_counters : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let opaque_counter = ref 0 in
  let is_digits s =
    s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s
  in
  let rename_segment segment =
    match String.rindex_opt segment '_' with
    | Some i
      when i > 0
           && is_digits
                (String.sub segment (i + 1)
                   (String.length segment - i - 1)) ->
      let base = String.sub segment 0 i in
      let stamp = String.sub segment i (String.length segment - i) in
      let key = base ^ "\000" ^ stamp in
      let n =
        match Hashtbl.find_opt stamp_numbers key with
        | Some n -> n
        | None ->
          let n =
            1
            + (Option.value ~default:0
                 (Hashtbl.find_opt base_counters base))
          in
          Hashtbl.replace base_counters base n;
          Hashtbl.add stamp_numbers key n;
          n
      in
      Printf.sprintf "%s_%d" base n
    | _ -> segment
  in
  let is_delimiter = function
    | '.' | '<' | '>' | ',' | '(' | ')' | '#' -> true
    | _ -> false
  in
  (* The delimiter characters also occur inside operator identifiers
     ([let ( +> ) ...] lowers to [+>_319<Bv63,Bv63,Bv63>]), where they are
     name material, not structure.  A delimiter is structural only where
     the grammar of generated names can put one: after a completed token —
     a segment of name/key characters, or a segment ending in a [_stamp]
     (which admits an operator base) — or directly after a closing
     [> ) #] (nested suffixes, functor applications).  Anywhere else
     ([+>], [.>>=_2], the leading [<] of [( < )]'s symbol) the character
     belongs to an operator identifier and stays in its segment, so the
     stamp that follows renumbers with its base instead of leaking raw. *)
  let is_closer = function '>' | ')' | '#' -> true | _ -> false in
  let token_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '/' -> true
    | _ -> false
  in
  let ends_with_stamp s =
    match String.rindex_opt s '_' with
    | Some i ->
      i > 0
      && i < String.length s - 1
      && is_digits (String.sub s (i + 1) (String.length s - i - 1))
    | None -> false
  in
  let rename name =
    match Hashtbl.find_opt renames name with
    | Some r -> r
    | None ->
      let renamed =
        if String.length name > 7
           && String.equal (String.sub name 0 7) "result/"
           && is_digits (String.sub name 7 (String.length name - 7))
        then begin
          incr opaque_counter;
          Printf.sprintf "result/%d" !opaque_counter
        end
        else begin
          let buf = Buffer.create (String.length name) in
          let flush start stop =
            Buffer.add_string buf
              (rename_segment (String.sub name start (stop - start)))
          in
          let start = ref 0 in
          let previous_delimiter = ref None in
          String.iteri
            (fun i c ->
               if is_delimiter c
               then begin
                 let structural =
                   if i > !start
                   then begin
                     let segment = String.sub name !start (i - !start) in
                     String.for_all token_char segment
                     (* the two operator-named datatype members ([::] and
                        [[]], the list constructors) are completed tokens
                        too: their instance suffix is structural and its
                        keys may hold stamps of their own *)
                     || String.equal segment "::"
                     || String.equal segment "[]"
                     || ends_with_stamp segment
                   end
                   else
                     match !previous_delimiter with
                     | Some d -> is_closer d
                     | None -> false
                 in
                 if structural
                 then begin
                   flush !start i;
                   Buffer.add_char buf c;
                   previous_delimiter := Some c;
                   start := i + 1
                 end
               end)
            name;
          flush !start (String.length name);
          Buffer.contents buf
        end
      in
      Hashtbl.add renames name renamed;
      renamed
  in
  let sort (s : Vox_logic.Sort.t) : Vox_logic.Sort.t =
    match s with
    | Uninterpreted n -> Uninterpreted (rename n)
    | Datatype n -> Datatype (rename n)
    | Bool | Int | Bitvec _ -> s
  in
  let rec term (t : Vox_logic.Term.t) : Vox_logic.Term.t =
    match t with
    | Var n -> Var (rename n)
    | Const _ -> t
    | App (op, args) -> App (op, List.map term args)
    | Call (f, args) -> Call (rename f, List.map term args)
    | Ite (a, b, c) -> Ite (term a, term b, term c)
    | Construct (c, args) -> Construct (rename c, List.map term args)
    | Select (c, i, x) -> Select (rename c, i, term x)
    | Test (c, x) -> Test (rename c, term x)
  in
  let hypotheses =
    List.map
      (fun (h : Vox_logic.Obligation.hypothesis) ->
         { h with term = term h.term })
      ob.hypotheses
  in
  let goal = term ob.goal in
  let signature =
    { Vox_logic.Signature.sorts = List.map rename ob.signature.sorts
    ; datatypes =
        List.map
          (fun (d : Vox_logic.Signature.datatype) ->
             { Vox_logic.Signature.datatype_name = rename d.datatype_name
             ; constructors =
                 List.map
                   (fun (c : Vox_logic.Signature.constructor) ->
                      { Vox_logic.Signature.constructor_name =
                          rename c.constructor_name
                      ; fields =
                          List.map
                            (fun (sel, s) -> rename sel, sort s)
                            c.fields
                      })
                   d.constructors
             })
          ob.signature.datatypes
    ; variables =
        List.map (fun (n, s) -> rename n, sort s) ob.signature.variables
    ; functions =
        List.map
          (fun (n, params, result) ->
             rename n, List.map sort params, sort result)
          ob.signature.functions
    }
  in
  { ob with signature; hypotheses; goal }
