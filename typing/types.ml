(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Representation of types and declarations *)

open Allowance
open Asttypes

module Rigid_name = struct
  type unknown_id = Shape.Uid.t

  type t =
    | Atom of
        { constr : Path.t;
          arg_index : int
        }
    | KAtom of Path.t
    | Param of int
    | Unknown of unknown_id

  let compare a b =
    if a == b
    then 0
    else
      match a, b with
      | Atom a1, Atom a2 ->
        let h = Path.compare a1.constr a2.constr in
        if h != 0 then h else Int.compare a1.arg_index a2.arg_index
      | KAtom p1, KAtom p2 -> Path.compare p1 p2
      | Param x, Param y -> Int.compare x y
      | Atom _, _ -> -1
      | _, Atom _ -> 1
      | KAtom _, _ -> -1
      | _, KAtom _ -> 1
      | Unknown x, Unknown y -> Shape.Uid.compare x y
      | Unknown _, _ -> 1
      | _, Unknown _ -> -1

  let to_string = function
    | Atom { constr; arg_index } ->
      let constr_s = Format_doc.asprintf "%a" Path.print constr in
      Printf.sprintf "%s.%d" constr_s arg_index
    | KAtom path ->
      let path_s = Format_doc.asprintf "%a" Path.print path in
      Printf.sprintf "katom[%s]" path_s
    | Param i -> Printf.sprintf "param[%d]" i
    | Unknown id ->
      Format.asprintf "unknown[%a]" Shape.Uid.print id

  let atomic constr arg_index = Atom { constr; arg_index }

  let katom path = KAtom path

  let param i = Param i

  let unknown uid = Unknown uid
end

module Ldd = struct
  module Name = Rigid_name

  include (Ldd.Make (Rigid_name) :
             Ldd_intf.S with module Name := Rigid_name)
end

type constructor_ikind =
  { base : Ldd.node;
    coeffs : Ldd.node array;
  }

type constructor_ikind_entry =
  | Constructor_ikind of constructor_ikind
  | No_constructor_ikind of string

type type_ikind = constructor_ikind_entry

let ikinds_todo (message : string) : type_ikind =
  if !Clflags.ikinds_debug then
    Format.eprintf "[ikinds-todo] %s@." message;
  No_constructor_ikind message

type atomic =
  | Nonatomic
  | Atomic

type mutability =
  | Immutable
  | Mutable of
      { mode : Mode.Value.Comonadic.lr
      ; atomic : atomic
      }

let is_mutable = function
  | Immutable -> false
  | Mutable _ -> true

let is_atomic = function
  | Immutable -> false
  | Mutable { atomic = Atomic; mode = _ } -> true
  | Mutable { atomic = Nonatomic; mode = _ } -> false

(** Takes [m0] which is the parameter of [let mutable], returns the
    mode of new values in future writes. *)
let mutable_mode m0 : _ Mode.Value.t =
  { comonadic = m0
  ; monadic = Mode.Value.Monadic.(min |> allow_left |> allow_right)
  }

(* Type expressions for the core language *)

type mod_bounds =
  { crossing : Mode.Crossing.t;
    externality: Jkind_axis.Externality.t;
  }

(* Shared by [Typedtree.expression] and [refinement_expression]. *)
type constant =
    Const_int of int
  | Const_char of char
  | Const_untagged_char of char
  | Const_string of string * Location.t * string option
  | Const_float of string
  | Const_float32 of string
  | Const_unboxed_float of string
  | Const_unboxed_float32 of string
  | Const_int8 of int
  | Const_int16 of int
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
  | Const_untagged_int of int
  | Const_untagged_int8 of int
  | Const_untagged_int16 of int
  | Const_unboxed_int32 of int32
  | Const_unboxed_int64 of int64
  | Const_unboxed_nativeint of nativeint

module With_bounds_type_info = struct
  type t = {relevant_axes : Jkind_axis.Axis_set.t } [@@unboxed]

  let join { relevant_axes = axes1 } { relevant_axes = axes2 } =
    { relevant_axes = Jkind_axis.Axis_set.union axes1 axes2 }
end

type transient_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable scope: scope_field;
    id: int }

and scope_field = int
  (* bit field: 27 bits for scope (Ident.highest_scope = 100_000_000)
     and at least 4 marks *)

and type_expr = transient_expr

and type_desc =
  | Tvar of { name : string option; jkind : jkind_lr }
  | Tarrow of arrow_desc * type_expr * type_expr * commutable
  | Ttuple of (string option * type_expr) list
  | Tunboxed_tuple of (string option * type_expr) list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tquote of type_expr
  | Tsplice of type_expr
  | Tquote_eval of type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr * type_expr option
  | Tvariant of row_desc
  | Tunivar of { name : string option; jkind : jkind_lr }
  | Tpoly of type_expr * type_expr list
  | Trepr of type_expr * Jkind_types.Sort.univar list
  | Tpackage of package
  | Tof_kind of jkind_lr
  | Tbox of type_expr
  | Trefine of refinement_desc

(* Keep this constructor list mechanically aligned with the supported subset
   of [Typedtree.expression_desc].  The payload differences are exactly the
   three refinement deltas: no environment, no elaboration metadata, and
   resolved free references. *)
and refinement_expression =
  { rexp_desc : refinement_expression_desc;
    rexp_type : type_expr;
    rexp_loc : Location.t;
  }

and refinement_expression_desc =
  | Rexp_ident of refinement_identifier
  | Rexp_constant of constant
  | Rexp_let of refinement_binding list * refinement_expression
  | Rexp_function of
      { arg_label : arg_label;
        param : refinement_binder;
        body : refinement_expression;
      }
  | Rexp_apply of
      refinement_expression *
        (arg_label * refinement_expression) list
  | Rexp_tuple of (string option * refinement_expression) list
  | Rexp_construct of
      refinement_constructor * refinement_expression list
  | Rexp_field of refinement_expression * refinement_field
  | Rexp_ifthenelse of
      refinement_expression * refinement_expression *
        refinement_expression option

and refinement_identifier =
  | Rbound of Ident.t
  | Rfree of refinement_reference

and refinement_reference =
  | Rfun of string
  | Rsibling of string
  | Rapp of Path.t
  | Rglobal of Path.t

and refinement_binder =
  { rb_id : Ident.t;
    rb_type : type_expr;
  }

and refinement_binding =
  { rbind_binder : refinement_binder;
    rbind_expr : refinement_expression;
  }

and refinement_constructor =
  { rconstr_type_path : Path.t;
    rconstr_name : string;
  }

and refinement_field =
  { rfield_type_path : Path.t;
    rfield_name : string;
  }

and refinement_desc =
  { ref_skeleton : type_expr;
    ref_view : refinement_binder;
    ref_pred : refinement_expression;
  }

and arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
  | Position of string

and arrow_desc =
  arg_label * Mode.Alloc.lr * Mode.Alloc.lr

and package =
    { pack_path : Path.t;
      pack_cstrs : (string list * type_expr) list }

and row_desc =
    { row_fields: (label * row_field) list;
      row_more: type_expr;
      row_closed: bool;
      row_fixed: fixed_explanation option;
      row_name: (Path.t * type_expr list) option }
and fixed_explanation =
  | Univar of type_expr
  | Fixed_private
  | Reified of Path.t
  | Rigid
  | Fixed_existential
and row_field = [`some] row_field_gen
and row_field_cell = [`some | `none] row_field_gen ref
and _ row_field_gen =
    RFpresent : type_expr option -> [> `some] row_field_gen
  | RFeither :
      { no_arg: bool;
        arg_type: type_expr list;
        matched: bool;
        ext: row_field_cell} -> [> `some] row_field_gen
  | RFabsent : [> `some] row_field_gen
  | RFnone : [> `none] row_field_gen

and abbrev_memo =
    Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and any = [`some | `none | `var]
and field_kind = [`some|`var] field_kind_gen
and _ field_kind_gen =
    FKvar : {mutable field_kind: any field_kind_gen} -> [> `var] field_kind_gen
  | FKprivate : [> `none] field_kind_gen  (* private method; only under FKvar *)
  | FKpublic  : [> `some] field_kind_gen  (* public method *)
  | FKabsent  : [> `some] field_kind_gen  (* hidden private method *)

and commutable = [`some|`var] commutable_gen
and _ commutable_gen =
    Cok      : [> `some] commutable_gen
  | Cunknown : [> `none] commutable_gen
  | Cvar : {mutable commu: any commutable_gen} -> [> `var] commutable_gen

(* jkinds *)

and jkind_history =
  | Interact of
      { reason : Jkind_intf.History.interact_reason;
        jkind1 : jkind_desc_packed;
        history1 : jkind_history;
        jkind2 : jkind_desc_packed;
        history2 : jkind_history
      }
  | Creation of Jkind_intf.History.creation_reason

(* See [With_bounds_types] for more information on this abstract type. *)
and with_bounds_types

and 'd with_bounds =
  | No_with_bounds : ('l * 'r) with_bounds
  | With_bounds : with_bounds_types -> ('l * Allowance.disallowed) with_bounds

and 'layout jkind_base =
  | Layout of 'layout
  | Kconstr of Path.t * Jkind_types.Scannable_axes.t

and ('layout, 'd) base_and_axes =
  { base : 'layout jkind_base;
    mod_bounds : mod_bounds;
    with_bounds : 'd with_bounds
  }
  constraint 'd = 'l * 'r

and 'd jkind_const_desc = (Jkind_types.Layout.Const.t, 'd) base_and_axes
  constraint 'd = 'l * 'r
and jkind_const_desc_lr = (allowed * allowed) jkind_const_desc

and 'd jkind_desc = (Jkind_types.Sort.t Jkind_types.Layout.t, 'd) base_and_axes
  constraint 'd = 'l * 'r

and jkind_desc_packed = Pack_jkind_desc : ('l * 'r) jkind_desc -> jkind_desc_packed

and 'd jkind_quality =
  | Best : ('l * disallowed) jkind_quality
  | Not_best : ('l * 'r) jkind_quality

and 'd jkind =
  { jkind : 'd jkind_desc;
    annotation : Parsetree.jkind_annotation option;
    history : jkind_history;
    has_warned : bool;
    ran_out_of_fuel_during_normalize : bool;
    quality : 'd jkind_quality;
  }
  constraint 'd = 'l * 'r

and jkind_l = (allowed * disallowed) jkind
and jkind_r = (disallowed * allowed) jkind
and jkind_lr = (allowed * allowed) jkind
and jkind_packed = Pack_jkind : ('l * 'r) jkind -> jkind_packed

and jkind_declaration =
  {
    (* CR layouts: Though it's semantically correct to have a const jkind for
       the manifest, it's not obvious if this is the right choice from a
       performance perspective. See internal ticket 5719. *)
    jkind_manifest : jkind_const_desc_lr option;
    jkind_attributes : Parsetree.attributes;
    jkind_uid : Shape.Uid.t;
    jkind_loc : Location.t
  }

module TransientTypeOps = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

module TransientTypeHash = Hashtbl.Make(TransientTypeOps)

(* *)

module Uid = Shape.Uid

(* Maps of methods and instance variables *)

module MethSet = Misc.Stdlib.String.Set
module VarSet = Misc.Stdlib.String.Set

module Meths = Misc.Stdlib.String.Map
module Vars = Misc.Stdlib.String.Map


(* Value descriptions *)

type value_kind =
    Val_reg of Jkind_types.Sort.t       (* Regular value *)
  | Val_mut of Mode.Value.Comonadic.lr * Jkind_types.Sort.t
                                        (* Mutable value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of
      class_signature * self_meths * Ident.t Vars.t * string
                                        (* Self *)
  | Val_anc of class_signature * Ident.t Meths.t * string
                                        (* Ancestor *)

and self_meths =
  | Self_concrete of Ident.t Meths.t
  | Self_virtual of Ident.t Meths.t ref

and class_signature =
  { csig_self: type_expr;
    mutable csig_self_row: type_expr;
    mutable csig_vars: (mutable_flag * virtual_flag * type_expr) Vars.t;
    mutable csig_meths: (method_privacy * virtual_flag * type_expr) Meths.t; }

and method_privacy =
  | Mpublic
  | Mprivate of field_kind

(* Variance *)
(* Variance forms a product lattice of the following partial orders:
     0 <= may_pos <= pos
     0 <= may_weak <= may_neg <= neg
     0 <= inj
   may_pos/may_neg mean possible positive/negative occurrences;
     thus, may_pos + may_neg = invariant
   Additionally, the following implications are valid
     pos => inj
     neg => inj
   Examples:
     type 'a t        : may_pos + may_neg
     type +'a t       : may_pos
     type -'a t       : may_neg
     type +-'a t      : null (no occurrence of 'a assured)
     type !'a t       : may_pos + may_neg + inj
     type +!'a t      : may_pos + inj
     type -!'a t      : may_neg + inj
     type +-!'a t     : inj
     type 'a t = 'a   : pos
     type 'a t = 'a -> unit : neg
     type 'a t = ('a -> unit) -> unit : pos + may_weak
     type 'a t = A of (('a -> unit) -> unit) : pos
     type +'a p = ..  : may_pos + inj
     type 'a t = A    : inj
 *)

module Variance = struct
  type t = int
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  let single = function
    | May_pos -> 1
    | May_neg -> 2 + 4
    | May_weak -> 4
    | Inj -> 8
    | Pos -> 16 + 8 + 1
    | Neg -> 32 + 8 + 4 + 2
    | Inv -> 63
  let union v1 v2 = v1 lor v2
  let inter v1 v2 = v1 land v2
  let subset v1 v2 = (v1 land v2 = v1)
  let eq (v1 : t) v2 = (v1 = v2)
  let set x v = union v (single x)
  let set_if b x v = if b then set x v else v
  let mem x = subset (single x)
  let null = 0
  let unknown = 7
  let full = single Inv
  let covariant = single Pos
  let contravariant = single Neg
  let swap f1 f2 v v' =
    set_if (mem f2 v) f1 (set_if (mem f1 v) f2 v')
  let conjugate v =
    let v' = inter v (union (single Inj) (single May_weak)) in
    swap Pos Neg v (swap May_pos May_neg v v')
  let compose v1 v2 =
    if mem Inv v1 && mem Inj v2 then full else
    let mp =
      mem May_pos v1 && mem May_pos v2 || mem May_neg v1 && mem May_neg v2
    and mn =
      mem May_pos v1 && mem May_neg v2 || mem May_neg v1 && mem May_pos v2
    and mw = mem May_weak v1 && v2 <> null || v1 <> null && mem May_weak v2
    and inj = mem Inj v1 && mem Inj v2
    and pos = mem Pos v1 && mem Pos v2 || mem Neg v1 && mem Neg v2
    and neg = mem Pos v1 && mem Neg v2 || mem Neg v1 && mem Pos v2 in
    List.fold_left (fun v (b,f) -> set_if b f v) null
      [mp, May_pos; mn, May_neg; mw, May_weak; inj, Inj; pos, Pos; neg, Neg]
  let strengthen v =
    if mem May_neg v then v else v land (full - single May_weak)
  let get_upper v = (mem May_pos v, mem May_neg v)
  let get_lower v = (mem Pos v, mem Neg v, mem Inj v)
  let unknown_signature ~injective ~arity =
    let v = if injective then set Inj unknown else unknown in
    Misc.replicate_list v arity
end

module Separability = struct
  type t = Ind | Sep | Deepsep
  type signature = t list
  let eq (m1 : t) m2 = (m1 = m2)
  let rank = function
    | Ind -> 0
    | Sep -> 1
    | Deepsep -> 2
  let compare m1 m2 = compare (rank m1) (rank m2)
  let max m1 m2 = if rank m1 >= rank m2 then m1 else m2

  let print ppf = function
    | Ind -> Format.fprintf ppf "Ind"
    | Sep -> Format.fprintf ppf "Sep"
    | Deepsep -> Format.fprintf ppf "Deepsep"

  let print_signature ppf modes =
    let pp_sep ppf () = Format.fprintf ppf ",@," in
    Format.fprintf ppf "@[(%a)@]"
      (Format.pp_print_list ~pp_sep print) modes

  let default_signature ~arity =
    let default_mode = if Config.flat_float_array then Deepsep else Ind in
    Misc.replicate_list default_mode arity
end

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_decl_kind;
    type_jkind: jkind_l;
    type_ikind: constructor_ikind_entry;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    type_separability: Separability.t list;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_unboxed_default: bool;
    type_uid: Uid.t;
    type_unboxed_version : type_declaration option;
 }

and type_decl_kind =
  (label_declaration, label_declaration, constructor_declaration) type_kind

and unsafe_mode_crossing =
  { unsafe_mod_bounds : Mode.Crossing.t
  ; unsafe_with_bounds : (allowed * disallowed) with_bounds
  }

and ('lbl, 'lbl_flat, 'cstr) type_kind =
    Type_abstract of type_origin
  | Type_record of
      'lbl list * record_representation * unsafe_mode_crossing option
  | Type_record_unboxed_product of
      'lbl_flat list *
      record_unboxed_product_representation *
      unsafe_mode_crossing option
  | Type_variant of
      'cstr list * variant_representation * unsafe_mode_crossing option
  | Type_open

and tag = Ordinary of {src_index: int;     (* Unique name (per type) *)
                       runtime_tag: int}   (* The runtime tag *)
        | Extension of Path.t
        | Null

and type_origin =
    Definition
  | Rec_check_regularity
  | Existential of string

and mixed_block_element =
  | Scannable of Jkind_types.Scannable_axes.t
  | Float_boxed
  | Float64
  | Float32
  | Bits8
  | Bits16
  | Untagged_immediate
  | Bits32
  | Bits64
  | Vec128
  | Vec256
  | Vec512
  | Word
  | Product of mixed_product_shape
  | Void

and mixed_product_shape = mixed_block_element array

and module_representation = Jkind_types.Sort.t array

and record_representation =
  | Record_unboxed
  | Record_inlined of tag * constructor_representation * variant_representation
  | Record_boxed
  | Record_float
  | Record_ufloat
  | Record_mixed of mixed_product_shape
  | Record_dummy of { represent_as_float_array : bool; flatten_floats : bool }
  | Record_variable

and record_unboxed_product_representation =
  | Record_unboxed_product
  | Record_unboxed_product_variable

and variant_representation =
  | Variant_unboxed
  | Variant_boxed of cstr_layout array
  | Variant_extensible
  | Variant_with_null

and cstr_layout =
  | Cstr_layout_known of
      { shape : constructor_representation;
        sorts : Jkind_types.Sort.Const.t array;
      }
  | Cstr_layout_variable

and constructor_representation =
  | Constructor_uniform_value
  | Constructor_mixed of mixed_product_shape
  | Constructor_variable

and label_declaration =
  {
    ld_id: Ident.t;
    ld_mutable: mutability;
    ld_modalities: Mode.Modality.Const.t;
    ld_type: type_expr;
    ld_sort: Jkind_types.Sort.Const.t option;
    ld_loc: Location.t;
    ld_attributes: Parsetree.attributes;
    ld_uid: Uid.t;
  }

and constructor_declaration =
  {
    cd_id: Ident.t;
    cd_args: constructor_arguments;
    cd_res: type_expr option;
    cd_loc: Location.t;
    cd_attributes: Parsetree.attributes;
    cd_uid: Uid.t;
  }

and constructor_argument =
  {
    ca_modalities: Mode.Modality.Const.t;
    ca_type: type_expr;
    ca_sort: Jkind_types.Sort.Const.t option;
    ca_loc: Location.t;
  }

and constructor_arguments =
  | Cstr_tuple of constructor_argument list
  | Cstr_record of label_declaration list

type extension_constructor =
  { ext_type_path: Path.t;
    ext_type_params: type_expr list;
    ext_args: constructor_arguments;
    ext_shape: constructor_representation;
    ext_constant: bool;
    ext_ret_type: type_expr option;
    ext_private: private_flag;
    ext_loc: Location.t;
    ext_attributes: Parsetree.attributes;
    ext_uid: Uid.t;
  }

and type_transparence =
    Type_public      (* unrestricted expansion *)
  | Type_new         (* "new" type *)
  | Type_private     (* private type *)

let tys_of_constr_args = function
  | Cstr_tuple tl -> List.map (fun ca -> ca.ca_type) tl
  | Cstr_record lbls -> List.map (fun l -> l.ld_type) lbls

(* Type expressions for the class language *)

type class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

type class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: Variance.t list;
    cty_loc: Location.t;
    cty_attributes: Parsetree.attributes;
    cty_uid: Uid.t;
 }

type class_type_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_hash_type: type_declaration;
    clty_variance: Variance.t list;
    clty_loc: Location.t;
    clty_attributes: Parsetree.attributes;
    clty_uid: Uid.t;
  }

(* Type expressions for the module language *)

type visibility =
  | Exported
  | Hidden

type rec_status =
  Trec_not                   (* first in a nonrecursive group *)
| Trec_first                 (* first in a recursive group *)
| Trec_next                  (* not first in a recursive/nonrecursive group *)

type ext_status =
  Text_first                     (* first constructor of an extension *)
| Text_next                      (* not first constructor of an extension *)
| Text_exception                 (* an exception *)

type module_presence =
  | Mp_present
  | Mp_absent

module Aliasability = struct
  type t = Not_aliasable | Aliasable

  let aliasable b = if b then Aliasable else Not_aliasable

  let is_aliasable = function
    | Aliasable -> true
    | Not_aliasable -> false
end

module type Wrap = sig
  type 'a t
end

module Lpoly = struct
  type state =
    | Pending of Location.t
    | Determined of Jkind_types.Sort.var list

  type t = state ref

  let get_exn t = match !t with
    | Pending _ -> Misc.fatal_error "layout is pending generalization"
    | Determined l -> l

  let is_empty_exn t = List.is_empty @@ get_exn t

  let determined l = ref (Determined l)
  let pending ~loc = ref (Pending loc)

  let generalize ~on_determined ~on_to_generalize t =
    match !t with
    | Pending loc -> t := Determined (on_to_generalize loc)
    | Determined _ -> on_determined ()
end

module type Wrapped = sig
  type 'a wrapped

  type value_description =
    { val_type: type_expr wrapped;                (* Type of the value *)
      val_modalities : Mode.Modality.t;     (* Modalities on the value *)
      val_kind: value_kind;
      val_lpoly: Lpoly.t wrapped;
      val_loc: Location.t;
      val_zero_alloc: Zero_alloc.t;
      val_attributes: Parsetree.attributes;
      val_uid: Uid.t;
    }

  type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type * Mode.Alloc.lr
  | Mty_alias of Path.t
  | Mty_strengthen of module_type * Path.t * Aliasability.t
      (* See comments about the aliasability of strengthening in mtype.ml *)

  and functor_parameter =
  | Unit
  | Named of Ident.t option * module_type * Mode.Alloc.lr

  and signature = signature_item list wrapped

  and persistent_signature = signature * Mode.Staticity.Const.t

  and signature_item =
    Sig_value of Ident.t * value_description * visibility
  | Sig_type of Ident.t * type_declaration * rec_status * visibility
  | Sig_typext of Ident.t * extension_constructor * ext_status * visibility
  | Sig_module of
      Ident.t * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype of Ident.t * modtype_declaration * visibility
  | Sig_class of Ident.t * class_declaration * rec_status * visibility
  | Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility
  | Sig_jkind of Ident.t * jkind_declaration * visibility

  and module_declaration =
  {
    md_type: module_type;
    md_modalities: Mode.Modality.t;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
    md_uid: Uid.t;
  }

  and modtype_declaration =
  {
    mtd_type: module_type option;  (* Note: abstract *)
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
    mtd_uid: Uid.t;
  }

  val sort_of_signature_item :
    signature_item -> Jkind_types.Sort.t option
end

module Make_wrapped(Wrap : Wrap) = struct
  (* Avoid repeating everything in Wrapped *)
  module rec M : Wrapped with type 'a wrapped = 'a Wrap.t = M
  include M

  let sort_of_signature_item = function
    | Sig_value(_, decl, _) ->
      begin match decl.val_kind with
      | Val_reg sort -> Some sort
      | Val_ivar _ ->
        Some Jkind_types.Sort.(of_const Const.for_instance_var)
      | Val_self _ | Val_anc _ ->
        Some Jkind_types.Sort.(of_const Const.for_object)
      | Val_prim _ -> None (* Primitives are not stored in modules *)
      | Val_mut _ ->
        Misc.fatal_error "Mutable variable found at the structure level"
      end
    | Sig_typext _ ->
      Some Jkind_types.Sort.(of_const Const.for_type_extension)
    | Sig_module(_, pres, _, _, _) ->
      begin match pres with
      | Mp_present ->
        Some Jkind_types.Sort.(of_const Const.for_module)
      | Mp_absent -> None
      end
    | Sig_class _ ->
        Some Jkind_types.Sort.(of_const Const.for_class)
    | Sig_type _ | Sig_modtype _ | Sig_class_type _ | Sig_jkind _ -> None
end

module Map_wrapped(From : Wrapped)(To : Wrapped) = struct
  open From
  type mapper =
    {
      map_signature: mapper -> signature -> To.signature;
      map_type_expr: mapper -> type_expr wrapped -> type_expr To.wrapped;
      map_value_description:
        mapper -> value_description -> To.value_description;
    }

  let signature m = m.map_signature m

  let rec module_type m = function
    | Mty_ident p -> To.Mty_ident p
    | Mty_alias p -> To.Mty_alias p
    | Mty_functor (parm,mty,mm) ->
        To.Mty_functor (functor_parameter m parm, module_type m mty, mm)
    | Mty_signature sg -> To.Mty_signature (signature m sg)
    | Mty_strengthen (mty,p,aliasable) ->
        To.Mty_strengthen (module_type m mty, p, aliasable)

  and functor_parameter m = function
      | Unit -> To.Unit
      | Named (id,mty,mm) -> To.Named (id, module_type m mty,mm)

  let value_description m vd = m.map_value_description m vd

  let module_declaration m {md_type; md_modalities; md_attributes;
    md_loc; md_uid} =
    To.{
      md_type = module_type m md_type;
      md_modalities;
      md_attributes;
      md_loc;
      md_uid;
    }

  let modtype_declaration m {mtd_type; mtd_attributes; mtd_loc; mtd_uid} =
    To.{
      mtd_type = Option.map (module_type m) mtd_type;
      mtd_attributes;
      mtd_loc;
      mtd_uid;
    }

  let signature_item m = function
    | Sig_value (id,vd,vis) ->
        To.Sig_value (id, value_description m vd, vis)
    | Sig_type (id,td,rs,vis) ->
        To.Sig_type (id,td,rs,vis)
    | Sig_module (id,pres,md,rs,vis) ->
        To.Sig_module (id, pres, module_declaration m md, rs, vis)
    | Sig_modtype (id,mtd,vis) ->
        To.Sig_modtype (id, modtype_declaration m mtd, vis)
    | Sig_typext (id,ec,es,vis) ->
        To.Sig_typext (id,ec,es,vis)
    | Sig_class (id,cd,rs,vis) ->
        To.Sig_class (id,cd,rs,vis)
    | Sig_class_type (id,ctd,rs,vis) ->
        To.Sig_class_type (id,ctd,rs,vis)
    | Sig_jkind (id,jkd,vis) ->
        To.Sig_jkind (id,jkd,vis)
end

include Make_wrapped(struct type 'a t = 'a end)

let equal_tag t1 t2 =
  match (t1, t2) with
  | Ordinary {src_index=i1}, Ordinary {src_index=i2} ->
    i2 = i1 (* If i1 = i2, the runtime_tags will also be equal *)
  | Extension path1, Extension path2 -> Path.same path1 path2
  | Null, Null -> true
  | (Ordinary _ | Extension _ | Null), _ -> false

let compare_tag t1 t2 =
  match (t1, t2) with
  | Ordinary {src_index=i1}, Ordinary {src_index=i2} ->
    Int.compare i1 i2
  | Extension path1, Extension path2 -> Path.compare path1 path2
  | Null, Null -> 0
  | Ordinary _, (Extension _ | Null) -> -1
  | (Extension _ | Null), Ordinary _ -> 1
  | Extension _, Null -> -1
  | Null, Extension _ -> 1

let rec equal_mixed_block_element_up_to_scannable_axes e1 e2 =
  match e1, e2 with
  | Scannable _, Scannable _
  | Float64, Float64 | Float32, Float32 | Float_boxed, Float_boxed
  | Word, Word | Untagged_immediate, Untagged_immediate
  | Bits8, Bits8 | Bits16, Bits16
  | Bits32, Bits32 | Bits64, Bits64
  | Vec128, Vec128 | Vec256, Vec256 | Vec512, Vec512
  | Void, Void
    -> true
  | Product es1, Product es2
    -> Misc.Stdlib.Array.equal
         equal_mixed_block_element_up_to_scannable_axes es1 es2
  | ( Scannable _ | Float64 | Float32 | Float_boxed | Word | Untagged_immediate
    | Bits8 | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512
    | Product _ | Void ), _
    -> false

let rec compare_mixed_block_element e1 e2 =
  match e1, e2 with
  | Scannable sa1, Scannable sa2 -> (
    match Jkind_types.Scannable_axes.less_or_equal sa1 sa2 with
    | Less -> -1
    | Equal -> 0
    | Not_le -> 1)
  | Float_boxed, Float_boxed
  | Float64, Float64 | Float32, Float32
  | Word, Word | Untagged_immediate, Untagged_immediate
  | Bits8, Bits8 | Bits16, Bits16 | Bits32, Bits32 | Bits64, Bits64
  | Vec128, Vec128 | Vec256, Vec256 | Vec512, Vec512
  | Void, Void
    -> 0
  | Product es1, Product es2
    -> Misc.Stdlib.Array.compare compare_mixed_block_element es1 es2
  | Scannable _, _ -> -1
  | _, Scannable _ -> 1
  | Float_boxed, _ -> -1
  | _, Float_boxed -> 1
  | Float64, _ -> -1
  | _, Float64 -> 1
  | Float32, _ -> -1
  | _, Float32 -> 1
  | Word, _ -> -1
  | _, Word -> 1
  | Untagged_immediate, _ -> -1
  | _, Untagged_immediate -> 1
  | Bits8, _ -> -1
  | _, Bits8 -> 1
  | Bits16, _ -> -1
  | _, Bits16 -> 1
  | Bits32, _ -> -1
  | _, Bits32 -> 1
  | Bits64, _ -> -1
  | _, Bits64 -> 1
  | Vec128, _ -> -1
  | _, Vec128 -> 1
  | Vec256, _ -> -1
  | _, Vec256 -> 1
  | Vec512, _ -> -1
  | _, Vec512 -> 1
  | Void, _ -> -1
  | _, Void -> 1

let equal_mixed_product_shape_up_to_scannable_axes r1 r2 = r1 == r2 ||
  Misc.Stdlib.Array.equal equal_mixed_block_element_up_to_scannable_axes r1 r2

let equal_constructor_representation_up_to_scannable_axes r1 r2 = r1 == r2 ||
  match r1, r2 with
  | Constructor_uniform_value, Constructor_uniform_value -> true
  | Constructor_mixed mx1, Constructor_mixed mx2 ->
      equal_mixed_product_shape_up_to_scannable_axes mx1 mx2
  | Constructor_variable, Constructor_variable -> true
  | (Constructor_mixed _ | Constructor_uniform_value | Constructor_variable), _
    -> false

let equal_variant_representation_up_to_scannable_axes r1 r2 = r1 == r2 ||
  match r1, r2 with
  | Variant_unboxed, Variant_unboxed ->
      true
  | Variant_boxed layouts1, Variant_boxed layouts2 ->
      Misc.Stdlib.Array.equal
        (fun l1 l2 -> match l1, l2 with
           | Cstr_layout_variable, Cstr_layout_variable -> true
           | Cstr_layout_known { shape = s1; sorts = ss1 },
             Cstr_layout_known { shape = s2; sorts = ss2 } ->
             equal_constructor_representation_up_to_scannable_axes s1 s2
             && Misc.Stdlib.Array.equal Jkind_types.Sort.Const.equal ss1 ss2
           | (Cstr_layout_known _ | Cstr_layout_variable), _ -> false)
        layouts1
        layouts2
  | Variant_extensible, Variant_extensible ->
      true
  | Variant_with_null, Variant_with_null -> true
  | (Variant_unboxed | Variant_boxed _ | Variant_extensible | Variant_with_null), _ ->
      false

let equal_record_representation_up_to_scannable_axes r1 r2 = match r1, r2 with
  | Record_unboxed, Record_unboxed ->
      true
  | Record_inlined (tag1, cr1, vr1), Record_inlined (tag2, cr2, vr2) ->
      (* Equality of tag and variant representation imply equality of
         constructor representation. *)
      ignore (cr1 : constructor_representation);
      ignore (cr2 : constructor_representation);
      equal_tag tag1 tag2 &&
        equal_variant_representation_up_to_scannable_axes vr1 vr2
  | Record_boxed, Record_boxed ->
      true
  | Record_float, Record_float ->
      true
  | Record_ufloat, Record_ufloat ->
      true
  | Record_mixed mx1, Record_mixed mx2 ->
      equal_mixed_product_shape_up_to_scannable_axes mx1 mx2
  | Record_dummy { represent_as_float_array = a1; flatten_floats = b1 },
    Record_dummy { represent_as_float_array = a2; flatten_floats = b2 } ->
      Bool.equal a1 a2 && Bool.equal b1 b2
  | Record_variable, Record_variable -> true
  | (Record_unboxed | Record_inlined _ | Record_boxed | Record_float
    | Record_ufloat | Record_mixed _ | Record_dummy _ | Record_variable), _ ->
      false

let equal_record_unboxed_product_representation_up_to_scannable_axes r1 r2 =
  match r1, r2 with
  | Record_unboxed_product, Record_unboxed_product
  | Record_unboxed_product_variable, Record_unboxed_product_variable -> true
  | (Record_unboxed_product | Record_unboxed_product_variable), _ -> false

(* The scannable axes in the resulting [mixed_block_element] are always [max] *)
let rec mixed_block_element_of_const_sort (sort : Jkind_types.Sort.Const.t) =
  match sort with
  (* CR layouts-scannable: since sorts do not store scannable axis information,
     we are forced to default to max. It would be good to store the scannable
     axis information, but doing so takes a sizable refactor. See the comment
     on [Sort] in [jkind_intf.ml] *)
  | Base Scannable -> Scannable Jkind_types.Scannable_axes.max
  | Base Bits8 -> Bits8
  | Base Bits16 -> Bits16
  | Base Bits32 -> Bits32
  | Base Bits64 -> Bits64
  | Base Float32 -> Float32
  | Base Float64 -> Float64
  | Base Untagged_immediate -> Untagged_immediate
  | Base Vec128 -> Vec128
  | Base Vec256 -> Vec256
  | Base Vec512 -> Vec512
  | Base Word -> Word
  | Product sorts ->
    Product (Array.map mixed_block_element_of_const_sort (Array.of_list sorts))
  | Base Void -> Void
  | Univar _ -> Misc.fatal_error "mixed_block_element_of_const_sort: Univar"
  | Genvar _ -> Misc.fatal_error "mixed_block_element_of_const_sort: Genvar"

let find_unboxed_type decl =
  match decl.type_kind with
    Type_record
      ([{ld_type = arg; ld_modalities = ms; _}],
       Record_unboxed, _)
  | Type_record
      ([{ld_type = arg; ld_modalities = ms; _ }],
       Record_inlined (_, _, Variant_unboxed), _)
  | Type_record_unboxed_product
      ([{ld_type = arg; ld_modalities = ms; _ }],
       (Record_unboxed_product | Record_unboxed_product_variable), _)
  | Type_variant ([{cd_args = Cstr_tuple [{ca_type = arg; ca_modalities = ms; _}]; _}], Variant_unboxed, _)
  | Type_variant ([{cd_args = Cstr_record [{ld_type = arg; ld_modalities = ms; _}]; _}], Variant_unboxed, _) ->
    Some (arg, ms)
  | Type_record (_, ( Record_inlined _ | Record_unboxed
                    | Record_boxed | Record_float | Record_ufloat
                    | Record_mixed _ | Record_dummy _ | Record_variable), _)
  | Type_record_unboxed_product
      (_, (Record_unboxed_product | Record_unboxed_product_variable), _)
  | Type_variant (_, ( Variant_boxed _ | Variant_unboxed
                     | Variant_extensible | Variant_with_null), _)
  | Type_abstract _ | Type_open ->
    None

let item_visibility = function
  | Sig_value (_, _, vis)
  | Sig_type (_, _, _, vis)
  | Sig_typext (_, _, _, vis)
  | Sig_module (_, _, _, _, vis)
  | Sig_modtype (_, _, vis)
  | Sig_class (_, _, _, vis)
  | Sig_class_type (_, _, _, vis)
  | Sig_jkind (_, _, vis) -> vis

let rec bound_value_identifiers = function
    [] -> []
  | Sig_value(id, {val_kind = Val_reg _}, _) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_typext(id, _, _, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_module(id, Mp_present, _, _, _) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_class(id, _, _, _) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem

let signature_item_id = function
  | Sig_value (id, _, _)
  | Sig_type (id, _, _, _)
  | Sig_typext (id, _, _, _)
  | Sig_module (id, _, _, _, _)
  | Sig_modtype (id, _, _)
  | Sig_class (id, _, _, _)
  | Sig_class_type (id, _, _, _)
  | Sig_jkind (id, _, _)
    -> id

let signature_item_representation sg =
  match sort_of_signature_item sg with
  | None -> None
  | Some sort -> Some (signature_item_id sg, sort)

let bound_value_identifiers_and_sorts sigs =
  List.filter_map signature_item_representation sigs

let rec mixed_block_element_to_string = function
  | Scannable _ -> "Scannable"
  | Float_boxed -> "Float_boxed"
  | Float32 -> "Float32"
  | Float64 -> "Float64"
  | Bits8 -> "Bits8"
  | Bits16 -> "Bits16"
  | Bits32 -> "Bits32"
  | Bits64 -> "Bits64"
  | Vec128 -> "Vec128"
  | Vec256 -> "Vec256"
  | Vec512 -> "Vec512"
  | Word -> "Word"
  | Untagged_immediate -> "Untagged_immediate"
  | Product es ->
    "Product ["
    ^ (String.concat ", "
         (Array.to_list (Array.map mixed_block_element_to_string es)))
    ^ "]"
  | Void -> "Void"

let mixed_block_element_to_lowercase_string = function
  | Scannable _ -> "scannable"
  | Float_boxed -> "float"
  | Float32 -> "float32"
  | Float64 -> "float64"
  | Bits8 -> "bits8"
  | Bits16 -> "bits16"
  | Bits32 -> "bits32"
  | Bits64 -> "bits64"
  | Vec128 -> "vec128"
  | Vec256 -> "vec256"
  | Vec512 -> "vec512"
  | Word -> "word"
  | Untagged_immediate -> "untagged_immediate"
  | Product es ->
    "product ["
    ^ (String.concat ", "
         (Array.to_list (Array.map mixed_block_element_to_string es)))
    ^ "]"
  | Void -> "void"

(**** Definitions for backtracking ****)

type change =
    Ctype : type_expr * type_desc -> change
  | Ccompress : type_expr * type_desc * type_desc -> change
  | Clevel : type_expr * int -> change
  | Cscope : type_expr * int -> change
  | Cname :
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option -> change
  | Crow : [`none|`some] row_field_gen ref -> change
  | Ckind : [`var] field_kind_gen -> change
  | Ccommu : [`var] commutable_gen -> change
  | Cuniv : type_expr option ref * type_expr option -> change
  | Cmodes : Mode.changes -> change
  | Csort : Jkind_types.Sort.change -> change
  | Czero_alloc : Zero_alloc.change -> change

type changes =
    Change of change * changes ref
  | Unchanged
  | Invalid

let trail = Local_store.s_table ref Unchanged

let log_change ch =
  let r' = ref Unchanged in
  !trail := Change (ch, r');
  trail := r'

let () =
  Mode.set_append_changes (fun changes -> log_change (Cmodes !changes));
  Jkind_types.Sort.set_change_log (fun change -> log_change (Csort change));
  Zero_alloc.set_change_log (fun change -> log_change (Czero_alloc change))

(* constructor and accessors for [field_kind] *)

type field_kind_view =
    Fprivate
  | Fpublic
  | Fabsent

let rec field_kind_internal_repr : field_kind -> field_kind = function
  | FKvar {field_kind = FKvar _ | FKpublic | FKabsent as fk} ->
      field_kind_internal_repr fk
  | kind -> kind

let field_kind_repr fk =
  match field_kind_internal_repr fk with
  | FKvar _ -> Fprivate
  | FKpublic -> Fpublic
  | FKabsent -> Fabsent

let field_public = FKpublic
let field_absent = FKabsent
let field_private () = FKvar {field_kind=FKprivate}

(* Constructor and accessors for [commutable] *)

let rec is_commu_ok : type a. a commutable_gen -> bool = function
  | Cvar {commu} -> is_commu_ok commu
  | Cunknown -> false
  | Cok -> true

let commu_ok = Cok
let commu_var () = Cvar {commu=Cunknown}

(**** Representative of a type ****)

let rec repr_link (t : type_expr) d : type_expr -> type_expr =
 function
   {desc = Tlink t' as d'} ->
     repr_link t d' t'
 | {desc = Tfield (_, k, _, t') as d'}
   when field_kind_internal_repr k = FKabsent ->
     repr_link t d' t'
 | t' ->
     log_change (Ccompress (t, t.desc, d));
     t.desc <- d;
     t'

let repr_link1 t = function
   {desc = Tlink t' as d'} ->
     repr_link t d' t'
 | {desc = Tfield (_, k, _, t') as d'}
   when field_kind_internal_repr k = FKabsent ->
     repr_link t d' t'
 | t' -> t'

let repr t =
  match t.desc with
   Tlink t' ->
     repr_link1 t t'
 | Tfield (_, k, _, t') when field_kind_internal_repr k = FKabsent ->
     repr_link1 t t'
 | _ -> t

(* scope_field and marks *)

let scope_mask = (1 lsl 27) - 1
let marks_mask = (-1) lxor scope_mask
let () = assert (Ident.highest_scope land marks_mask = 0)

type type_mark =
  | Mark of {mark: int; mutable marked: type_expr list}
  | Hash of {visited: unit TransientTypeHash.t}
let type_marks =
  (* All the bits in marks_mask *)
  List.init (Sys.int_size - 27) (fun x -> 1 lsl (x + 27))
let available_marks = Local_store.s_ref type_marks
let with_type_mark f =
  match !available_marks with
  | mark :: rem as old ->
      available_marks := rem;
      let mk = Mark {mark; marked = []} in
      Misc.try_finally (fun () -> f mk) ~always: begin fun () ->
        available_marks := old;
        match mk with
        | Mark {marked} ->
            (* unmark marked type nodes *)
            List.iter
              (fun ty -> ty.scope <- ty.scope land ((-1) lxor mark))
              marked
        | Hash _ -> ()
      end
  | [] ->
      (* When marks are exhausted, fall back to using a hash table *)
      f (Hash {visited = TransientTypeHash.create 1})

(* getters for type_expr *)

let get_desc t = (repr t).desc
let get_level t = (repr t).level
let get_scope t = (repr t).scope land scope_mask
let get_id t = (repr t).id
let not_marked_node mark t =
  match mark with
  | Mark {mark} -> (repr t).scope land mark = 0
  | Hash {visited} -> not (TransientTypeHash.mem visited (repr t))

(* transient type_expr *)

module Transient_expr = struct
  let create desc ~level ~scope ~id = {desc; level; scope; id}
  let set_desc ty d = ty.desc <- d
  let set_stub_desc ty d =
    (match ty.desc with
    | Tvar {name = None; _} -> ()
    | _ -> assert false);
    ty.desc <- d
  let set_level ty lv = ty.level <- lv
  let set_var_jkind ty jkind' =
    match ty.desc with
    | Tvar { name; _ } ->
      set_desc ty (Tvar { name; jkind = jkind' })
    | _ -> Misc.fatal_error "set_var_jkind called on non-var"
  let get_scope ty = ty.scope land scope_mask
  let get_marks ty = ty.scope lsr 27
  let set_scope ty sc =
    if (sc land marks_mask <> 0) then
      invalid_arg "Types.Transient_expr.set_scope";
    ty.scope <- (ty.scope land marks_mask) lor sc
  let try_mark_node mark ty =
    match mark with
    | Mark ({mark} as mk) ->
        (ty.scope land mark = 0) && (* mark type node when not marked *)
        (ty.scope <- ty.scope lor mark; mk.marked <- ty :: mk.marked; true)
    | Hash {visited} ->
        not (TransientTypeHash.mem visited ty) &&
        (TransientTypeHash.add visited ty (); true)
  let coerce ty = ty
  let repr = repr
  let type_expr ty = ty
end

(* setting marks *)
let try_mark_node mark t = Transient_expr.try_mark_node mark (repr t)

(* Comparison for [type_expr]; cannot be used for functors *)

let eq_type t1 t2 = t1 == t2 || repr t1 == repr t2
let compare_type t1 t2 = compare (get_id t1) (get_id t2)

module Refinement = struct
  type t = refinement_expression

  type validation_error =
    | Root_type_mismatch
    | Unbound_identifier of Ident.t
    | Bound_identifier_type_mismatch of Ident.t
    | Duplicate_binder of Ident.t
    | Global_binder of Ident.t
    | Empty_let
    | Invalid_name of string
    | Function_type_mismatch
    | Apply_type_mismatch
    | Let_type_mismatch
    | If_type_mismatch
    | Tuple_type_mismatch

  let create ~loc ~type_ rexp_desc =
    { rexp_desc; rexp_type = type_; rexp_loc = loc }

  let fold_types f init expression =
    let fold_binder init binder = f init binder.rb_type in
    let rec loop init expression =
      let init = f init expression.rexp_type in
      match expression.rexp_desc with
      | Rexp_ident _ | Rexp_constant _ -> init
      | Rexp_let (bindings, body) ->
        let init =
          List.fold_left
            (fun init binding ->
              let init = fold_binder init binding.rbind_binder in
              loop init binding.rbind_expr)
            init bindings
        in
        loop init body
      | Rexp_function { param; body; arg_label = _ } ->
        loop (fold_binder init param) body
      | Rexp_apply (function_, arguments) ->
        List.fold_left
          (fun init (_, argument) -> loop init argument)
          (loop init function_) arguments
      | Rexp_tuple fields ->
        List.fold_left (fun init (_, field) -> loop init field) init fields
      | Rexp_construct (_, arguments) ->
        List.fold_left loop init arguments
      | Rexp_field (record, _) -> loop init record
      | Rexp_ifthenelse (condition, ifso, ifnot) ->
        let init = loop init condition in
        let init = loop init ifso in
        Option.fold ~none:init ~some:(loop init) ifnot
    in
    loop init expression

  let iter_types f expression =
    fold_types (fun () type_ -> f type_) () expression

  let map_types f expression =
    let map_binder binder = { binder with rb_type = f binder.rb_type } in
    let rec map expression =
      let rexp_desc =
        match expression.rexp_desc with
        | (Rexp_ident _ | Rexp_constant _) as desc -> desc
        | Rexp_let (bindings, body) ->
          Rexp_let
            ( List.map
                (fun binding ->
                  { rbind_binder = map_binder binding.rbind_binder;
                    rbind_expr = map binding.rbind_expr;
                  })
                bindings,
              map body )
        | Rexp_function { arg_label; param; body } ->
          Rexp_function
            { arg_label; param = map_binder param; body = map body }
        | Rexp_apply (function_, arguments) ->
          Rexp_apply
            (map function_,
             List.map
               (fun (label, argument) -> label, map argument)
               arguments)
        | Rexp_tuple fields ->
          Rexp_tuple (List.map (fun (label, field) -> label, map field) fields)
        | Rexp_construct (constructor, arguments) ->
          Rexp_construct (constructor, List.map map arguments)
        | Rexp_field (record, field) -> Rexp_field (map record, field)
        | Rexp_ifthenelse (condition, ifso, ifnot) ->
          Rexp_ifthenelse
            (map condition, map ifso, Option.map map ifnot)
      in
      { expression with rexp_desc; rexp_type = f expression.rexp_type }
    in
    map expression

  let map_locs f expression =
    let rec map expression =
      let rexp_desc =
        match expression.rexp_desc with
        | Rexp_constant (Const_string (value, loc, delimiter)) ->
          (* [Const_string] is the only constant carrying a location; every
             other embedded [Location.t] in the predicate AST is [rexp_loc],
             handled below. Normalizing both closes the marshaling-digest
             leak class rather than a single instance. *)
          Rexp_constant (Const_string (value, f loc, delimiter))
        | (Rexp_ident _ | Rexp_constant _) as desc -> desc
        | Rexp_let (bindings, body) ->
          Rexp_let
            ( List.map
                (fun binding ->
                  { binding with rbind_expr = map binding.rbind_expr })
                bindings,
              map body )
        | Rexp_function { arg_label; param; body } ->
          Rexp_function { arg_label; param; body = map body }
        | Rexp_apply (function_, arguments) ->
          Rexp_apply
            (map function_,
             List.map
               (fun (label, argument) -> label, map argument)
               arguments)
        | Rexp_tuple fields ->
          Rexp_tuple (List.map (fun (label, field) -> label, map field) fields)
        | Rexp_construct (constructor, arguments) ->
          Rexp_construct (constructor, List.map map arguments)
        | Rexp_field (record, field) -> Rexp_field (map record, field)
        | Rexp_ifthenelse (condition, ifso, ifnot) ->
          Rexp_ifthenelse (map condition, map ifso, Option.map map ifnot)
      in
      { expression with rexp_desc; rexp_loc = f expression.rexp_loc }
    in
    map expression

  let map_paths ?sibling_prefix ~value_path ~type_path expression =
    (* [sibling_prefix] is set only when the enclosing signature is being
       projected under a module path (see [Subst]/[Env.components_of_module]).
       A sibling reference is signature-relative -- a bare name in whatever
       module instance the refinement lives in -- so on projection it must be
       requalified to that instance's path, exactly as [Rglobal]/[Rapp] paths
       are; otherwise two instances of one signature (or two signatures sharing
       a value name) conflate their siblings into a single symbol.  Bare when
       there is no projection prefix (in-instance verification), where
       single-context name-keying is already sound. *)
    let map_reference = function
      | Rapp path -> Rapp (value_path path)
      | Rglobal path -> Rglobal (value_path path)
      | Rsibling name ->
        (match sibling_prefix with
         | Some root -> Rglobal (Path.Pdot (root, name))
         | None -> Rsibling name)
      | Rfun name ->
        (match sibling_prefix with
         | Some root -> Rapp (Path.Pdot (root, name))
         | None -> Rfun name)
    in
    let rec map expression =
      let rexp_desc =
        match expression.rexp_desc with
        | Rexp_ident (Rfree reference) ->
          Rexp_ident (Rfree (map_reference reference))
        | (Rexp_ident (Rbound _) | Rexp_constant _) as desc -> desc
        | Rexp_let (bindings, body) ->
          Rexp_let
            ( List.map
                (fun binding ->
                  { binding with rbind_expr = map binding.rbind_expr })
                bindings,
              map body )
        | Rexp_function ({ body; _ } as function_) ->
          Rexp_function { function_ with body = map body }
        | Rexp_apply (function_, arguments) ->
          Rexp_apply
            (map function_,
             List.map
               (fun (label, argument) -> label, map argument)
               arguments)
        | Rexp_tuple fields ->
          Rexp_tuple (List.map (fun (label, field) -> label, map field) fields)
        | Rexp_construct (constructor, arguments) ->
          Rexp_construct
            ( { constructor with
                rconstr_type_path =
                  type_path constructor.rconstr_type_path
              },
              List.map map arguments )
        | Rexp_field (record, field) ->
          Rexp_field
            ( map record,
              { field with
                rfield_type_path = type_path field.rfield_type_path
              } )
        | Rexp_ifthenelse (condition, ifso, ifnot) ->
          Rexp_ifthenelse
            (map condition, map ifso, Option.map map ifnot)
      in
      { expression with rexp_desc }
    in
    map expression

  let free_bound_identifiers expression =
    let rec loop bound free expression =
      match expression.rexp_desc with
      | Rexp_ident (Rbound id) ->
        if Ident.Set.mem id bound then free else Ident.Set.add id free
      | Rexp_ident (Rfree _) | Rexp_constant _ -> free
      | Rexp_let (bindings, body) ->
        let free =
          List.fold_left
            (fun free binding -> loop bound free binding.rbind_expr)
            free bindings
        in
        let bound =
          List.fold_left
            (fun bound binding ->
              Ident.Set.add binding.rbind_binder.rb_id bound)
            bound bindings
        in
        loop bound free body
      | Rexp_function { param; body; arg_label = _ } ->
        loop (Ident.Set.add param.rb_id bound) free body
      | Rexp_apply (function_, arguments) ->
        List.fold_left
          (fun free (_, argument) -> loop bound free argument)
          (loop bound free function_) arguments
      | Rexp_tuple fields ->
        List.fold_left
          (fun free (_, field) -> loop bound free field)
          free fields
      | Rexp_construct (_, arguments) ->
        List.fold_left (loop bound) free arguments
      | Rexp_field (record, _) -> loop bound free record
      | Rexp_ifthenelse (condition, ifso, ifnot) ->
        let free = loop bound free condition in
        let free = loop bound free ifso in
        Option.fold ~none:free ~some:(loop bound free) ifnot
    in
    loop Ident.Set.empty Ident.Set.empty expression

  let with_desc expression rexp_desc = { expression with rexp_desc }

  let rec rename_free ~from ~to_ expression =
    let rename expression = rename_free ~from ~to_ expression in
    let rexp_desc =
      match expression.rexp_desc with
      | Rexp_ident (Rbound id) when Ident.same id from ->
        Rexp_ident (Rbound to_)
      | (Rexp_ident _ | Rexp_constant _) as desc -> desc
      | Rexp_let (bindings, body) ->
        let bindings =
          List.map
            (fun binding ->
              { binding with rbind_expr = rename binding.rbind_expr })
            bindings
        in
        let shadows =
          List.exists
            (fun binding -> Ident.same binding.rbind_binder.rb_id from)
            bindings
        in
        Rexp_let (bindings, if shadows then body else rename body)
      | Rexp_function ({ param; body; _ } as function_) ->
        let body = if Ident.same param.rb_id from then body else rename body in
        Rexp_function { function_ with body }
      | Rexp_apply (function_, arguments) ->
        Rexp_apply
          (rename function_,
           List.map
             (fun (label, argument) -> label, rename argument)
             arguments)
      | Rexp_tuple fields ->
        Rexp_tuple
          (List.map (fun (label, field) -> label, rename field) fields)
      | Rexp_construct (constructor, arguments) ->
        Rexp_construct (constructor, List.map rename arguments)
      | Rexp_field (record, field) -> Rexp_field (rename record, field)
      | Rexp_ifthenelse (condition, ifso, ifnot) ->
        Rexp_ifthenelse
          (rename condition, rename ifso, Option.map rename ifnot)
    in
    with_desc expression rexp_desc

  let fresh_id id =
    Ident.create_scoped ~scope:(Ident.scope id) (Ident.name id)

  let rec subst ~id ~by expression =
    let free_in_by = free_bound_identifiers by in
    let recurse = subst ~id ~by in
    let freshen_capturing_binders bindings body =
      List.fold_left
        (fun (bindings, body) binding ->
          let binder = binding.rbind_binder in
          if Ident.Set.mem binder.rb_id free_in_by
          then
            let fresh = fresh_id binder.rb_id in
            let binder = { binder with rb_id = fresh } in
            ( { binding with rbind_binder = binder } :: bindings,
              rename_free ~from:binding.rbind_binder.rb_id ~to_:fresh body )
          else binding :: bindings, body)
        ([], body) bindings
      |> fun (bindings, body) -> List.rev bindings, body
    in
    let rexp_desc =
      match expression.rexp_desc with
      | Rexp_ident (Rbound occurrence) when Ident.same occurrence id ->
        by.rexp_desc
      | (Rexp_ident _ | Rexp_constant _) as desc -> desc
      | Rexp_let (bindings, body) ->
        let bindings =
          List.map
            (fun binding ->
              { binding with rbind_expr = recurse binding.rbind_expr })
            bindings
        in
        let shadows =
          List.exists
            (fun binding -> Ident.same binding.rbind_binder.rb_id id)
            bindings
        in
        if shadows
        then Rexp_let (bindings, body)
        else
          let bindings, body = freshen_capturing_binders bindings body in
          Rexp_let (bindings, recurse body)
      | Rexp_function ({ param; body; _ } as function_) ->
        if Ident.same param.rb_id id
        then Rexp_function function_
        else
          let param, body =
            if Ident.Set.mem param.rb_id free_in_by
            then
              let fresh = fresh_id param.rb_id in
              ( { param with rb_id = fresh },
                rename_free ~from:param.rb_id ~to_:fresh body )
            else param, body
          in
          Rexp_function { function_ with param; body = recurse body }
      | Rexp_apply (function_, arguments) ->
        Rexp_apply
          (recurse function_,
           List.map
             (fun (label, argument) -> label, recurse argument)
             arguments)
      | Rexp_tuple fields ->
        Rexp_tuple
          (List.map (fun (label, field) -> label, recurse field) fields)
      | Rexp_construct (constructor, arguments) ->
        Rexp_construct (constructor, List.map recurse arguments)
      | Rexp_field (record, field) -> Rexp_field (recurse record, field)
      | Rexp_ifthenelse (condition, ifso, ifnot) ->
        Rexp_ifthenelse
          (recurse condition, recurse ifso, Option.map recurse ifnot)
    in
    match expression.rexp_desc with
    | Rexp_ident (Rbound occurrence) when Ident.same occurrence id -> by
    | _ -> with_desc expression rexp_desc

  let collect_binder_stamps avoid expression =
    let add id = Hashtbl.replace avoid (Ident.stamp id) () in
    let rec collect expression =
      match expression.rexp_desc with
      | Rexp_ident (Rbound id) -> add id
      | Rexp_ident (Rfree _) | Rexp_constant _ -> ()
      | Rexp_let (bindings, body) ->
        List.iter
          (fun binding ->
            add binding.rbind_binder.rb_id;
            collect binding.rbind_expr)
          bindings;
        collect body
      | Rexp_function { param; body; arg_label = _ } ->
        add param.rb_id;
        collect body
      | Rexp_apply (function_, arguments) ->
        collect function_;
        List.iter (fun (_, argument) -> collect argument) arguments
      | Rexp_tuple fields ->
        List.iter (fun (_, field) -> collect field) fields
      | Rexp_construct (_, arguments) -> List.iter collect arguments
      | Rexp_field (record, _) -> collect record
      | Rexp_ifthenelse (condition, ifso, ifnot) ->
        collect condition;
        collect ifso;
        Option.iter collect ifnot
    in
    collect expression

  let fresh_id_avoiding avoid id =
    let rec create () =
      let fresh = fresh_id id in
      if Hashtbl.mem avoid (Ident.stamp fresh) then create () else fresh
    in
    let fresh = create () in
    Hashtbl.add avoid (Ident.stamp fresh) ();
    fresh

  let freshen_binders_with avoid initial_env expression =
    let rec lookup id = function
      | [] -> id
      | (old_id, fresh_id) :: rest ->
        if Ident.same id old_id then fresh_id else lookup id rest
    in
    let rec freshen env expression =
      let rexp_desc =
        match expression.rexp_desc with
        | Rexp_ident (Rbound id) -> Rexp_ident (Rbound (lookup id env))
        | (Rexp_ident (Rfree _) | Rexp_constant _) as desc -> desc
        | Rexp_let (bindings, body) ->
          let bindings =
            List.map
              (fun binding ->
                { binding with rbind_expr = freshen env binding.rbind_expr })
              bindings
          in
          let bindings, env =
            List.fold_left
              (fun (bindings, env) binding ->
                let binder = binding.rbind_binder in
                let fresh = fresh_id_avoiding avoid binder.rb_id in
                let binding =
                  { binding with
                    rbind_binder = { binder with rb_id = fresh }
                  }
                in
                binding :: bindings, (binder.rb_id, fresh) :: env)
              ([], env) bindings
          in
          Rexp_let (List.rev bindings, freshen env body)
        | Rexp_function ({ param; body; _ } as function_) ->
          let fresh = fresh_id_avoiding avoid param.rb_id in
          let body = freshen ((param.rb_id, fresh) :: env) body in
          Rexp_function
            { function_ with param = { param with rb_id = fresh }; body }
        | Rexp_apply (function_, arguments) ->
          Rexp_apply
            (freshen env function_,
             List.map
               (fun (label, argument) -> label, freshen env argument)
               arguments)
        | Rexp_tuple fields ->
          Rexp_tuple
            (List.map
               (fun (label, field) -> label, freshen env field)
               fields)
        | Rexp_construct (constructor, arguments) ->
          Rexp_construct (constructor, List.map (freshen env) arguments)
        | Rexp_field (record, field) ->
          Rexp_field (freshen env record, field)
        | Rexp_ifthenelse (condition, ifso, ifnot) ->
          Rexp_ifthenelse
            ( freshen env condition,
              freshen env ifso,
              Option.map (freshen env) ifnot )
      in
      with_desc expression rexp_desc
    in
    freshen initial_env expression

  let freshen_binders expression =
    let avoid = Hashtbl.create 17 in
    collect_binder_stamps avoid expression;
    freshen_binders_with avoid [] expression

  let freshen_desc_binders refinement =
    let avoid = Hashtbl.create 17 in
    Hashtbl.add avoid (Ident.stamp refinement.ref_view.rb_id) ();
    collect_binder_stamps avoid refinement.ref_pred;
    let fresh_view =
      fresh_id_avoiding avoid refinement.ref_view.rb_id
    in
    { refinement with
      ref_view = { refinement.ref_view with rb_id = fresh_view };
      ref_pred =
        freshen_binders_with avoid
          [refinement.ref_view.rb_id, fresh_view]
          refinement.ref_pred;
    }

  (* Freshen the free references to bare local [Pident]s in a predicate.  Such a
     reference is a function parameter (or other unit-local value) that a
     structure-level predicate mentions; it is lowered as [Rfree (Rglobal/Rapp
     (Pident id))] carrying that unit's local stamp.  Local stamps are only
     unique within a compilation unit, so when a refinement is imported from
     another unit its foreign parameter stamp can coincide with a caller-local
     binder stamp -- and because [Ident.same] compares locals by stamp, the two
     distinct values would be conflated (a cross-unit soundness hole).  Renaming
     each such reference to a globally fresh [Scoped] ident on import makes the
     foreign parameter a unique opaque symbol that cannot be [Ident.same] to any
     current-unit binder (a [Scoped] ident is never [same] as a [Local] one, and
     its stamp is fresh regardless), so it can neither be rewritten to a caller
     binder nor conflated by [Path.same].  [Rsibling]/[Rfun] (name-keyed) and
     module-qualified [Pdot] references are left untouched.  Renaming is
     consistent within the predicate (every mention of one foreign parameter
     maps to the same fresh ident), and it is applied once at import, where the
     loaded signature is cached, so a unit's parameter reference is stable across
     all uses in the importing compilation. *)
  let freshen_free_local_refs refinement =
    let renaming = ref [] in
    let fresh_for id =
      match List.find_opt (fun (old, _) -> Ident.same old id) !renaming with
      | Some (_, fresh) -> fresh
      | None ->
        let fresh = fresh_id id in
        renaming := (id, fresh) :: !renaming;
        fresh
    in
    let rename_reference = function
      | Rglobal (Path.Pident id) -> Rglobal (Path.Pident (fresh_for id))
      | Rapp (Path.Pident id) -> Rapp (Path.Pident (fresh_for id))
      | (Rglobal _ | Rapp _ | Rfun _ | Rsibling _) as reference -> reference
    in
    let rec walk expression =
      let rexp_desc =
        match expression.rexp_desc with
        | Rexp_ident (Rfree reference) ->
          Rexp_ident (Rfree (rename_reference reference))
        | (Rexp_ident (Rbound _) | Rexp_constant _) as desc -> desc
        | Rexp_let (bindings, body) ->
          Rexp_let
            ( List.map
                (fun binding ->
                  { binding with rbind_expr = walk binding.rbind_expr })
                bindings,
              walk body )
        | Rexp_function function_ ->
          Rexp_function { function_ with body = walk function_.body }
        | Rexp_apply (function_, arguments) ->
          Rexp_apply
            (walk function_,
             List.map (fun (label, argument) -> label, walk argument) arguments)
        | Rexp_tuple fields ->
          Rexp_tuple (List.map (fun (label, field) -> label, walk field) fields)
        | Rexp_construct (constructor, arguments) ->
          Rexp_construct (constructor, List.map walk arguments)
        | Rexp_field (record, field) -> Rexp_field (walk record, field)
        | Rexp_ifthenelse (condition, ifso, ifnot) ->
          Rexp_ifthenelse (walk condition, walk ifso, Option.map walk ifnot)
      in
      with_desc expression rexp_desc
    in
    { refinement with ref_pred = walk refinement.ref_pred }

  let equal_constant left right =
    match left, right with
    | ( Const_string (left, _, left_delimiter),
        Const_string (right, _, right_delimiter) ) ->
      String.equal left right && left_delimiter = right_delimiter
    | _ -> left = right

  let alpha_equal ~equal_type ?(binders = []) left right =
    let add_pair pairs left right =
      match
        List.find_opt (fun (id, _) -> Ident.same id left) pairs,
        List.find_opt (fun (_, id) -> Ident.same id right) pairs
      with
      | None, None -> Some ((left, right) :: pairs)
      | Some (_, paired_right), Some (paired_left, _)
        when Ident.same paired_right right && Ident.same paired_left left ->
        Some pairs
      | Some _, _ | _, Some _ -> None
    in
    let paired pairs left right =
      match
        List.find_opt (fun (id, _) -> Ident.same id left) pairs,
        List.find_opt (fun (_, id) -> Ident.same id right) pairs
      with
      | Some (_, paired_right), Some (paired_left, _) ->
        Ident.same paired_right right && Ident.same paired_left left
      | Some (_, paired_right), None -> Ident.same paired_right right
      | None, Some _ -> false
      | None, None -> Ident.same left right
    in
    let equal_reference left right =
      match left, right with
      | Rfun left, Rfun right | Rsibling left, Rsibling right ->
        String.equal left right
      | Rapp left, Rapp right | Rglobal left, Rglobal right ->
        Path.same left right
      | (Rfun _ | Rsibling _ | Rapp _ | Rglobal _), _ -> false
    in
    let rec equal pairs left right =
      equal_type left.rexp_type right.rexp_type
      &&
      match left.rexp_desc, right.rexp_desc with
      | Rexp_ident (Rbound left), Rexp_ident (Rbound right) ->
        paired pairs left right
      | Rexp_ident (Rfree left), Rexp_ident (Rfree right) ->
        equal_reference left right
      | Rexp_constant left, Rexp_constant right ->
        equal_constant left right
      | Rexp_let (left_bindings, left_body),
        Rexp_let (right_bindings, right_body) ->
        let rec bindings pairs left right =
          match left, right with
          | [], [] -> Some pairs
          | left :: left_rest, right :: right_rest ->
            let left_binder = left.rbind_binder in
            let right_binder = right.rbind_binder in
            if equal_type left_binder.rb_type right_binder.rb_type
               && equal pairs left.rbind_expr right.rbind_expr
            then
              Option.bind
                (add_pair pairs left_binder.rb_id right_binder.rb_id)
                (fun pairs -> bindings pairs left_rest right_rest)
            else None
          | [], _ :: _ | _ :: _, [] -> None
        in
        Option.fold
          ~none:false
          ~some:(fun pairs -> equal pairs left_body right_body)
          (bindings pairs left_bindings right_bindings)
      | Rexp_function left, Rexp_function right ->
        left.arg_label = right.arg_label
        && equal_type left.param.rb_type right.param.rb_type
        && Option.fold
             ~none:false
             ~some:(fun pairs -> equal pairs left.body right.body)
             (add_pair pairs left.param.rb_id right.param.rb_id)
      | Rexp_apply (left_function, left_arguments),
        Rexp_apply (right_function, right_arguments) ->
        equal pairs left_function right_function
        && List.length left_arguments = List.length right_arguments
        && List.for_all2
             (fun (left_label, left) (right_label, right) ->
               left_label = right_label && equal pairs left right)
             left_arguments right_arguments
      | Rexp_tuple left, Rexp_tuple right ->
        List.length left = List.length right
        && List.for_all2
             (fun (left_label, left) (right_label, right) ->
               left_label = right_label && equal pairs left right)
             left right
      | Rexp_construct (left_constructor, left_arguments),
        Rexp_construct (right_constructor, right_arguments) ->
        Path.same
          left_constructor.rconstr_type_path
          right_constructor.rconstr_type_path
        && String.equal
             left_constructor.rconstr_name right_constructor.rconstr_name
        && List.length left_arguments = List.length right_arguments
        && List.for_all2 (equal pairs) left_arguments right_arguments
      | Rexp_field (left_record, left_field),
        Rexp_field (right_record, right_field) ->
        equal pairs left_record right_record
        && Path.same left_field.rfield_type_path right_field.rfield_type_path
        && String.equal left_field.rfield_name right_field.rfield_name
      | Rexp_ifthenelse (left_condition, left_ifso, left_ifnot),
        Rexp_ifthenelse (right_condition, right_ifso, right_ifnot) ->
        equal pairs left_condition right_condition
        && equal pairs left_ifso right_ifso
        &&
        begin match left_ifnot, right_ifnot with
        | None, None -> true
        | Some left, Some right -> equal pairs left right
        | None, Some _ | Some _, None -> false
        end
      | ( (Rexp_ident _ | Rexp_constant _ | Rexp_let _ | Rexp_function _ |
           Rexp_apply _ | Rexp_tuple _ | Rexp_construct _ | Rexp_field _ |
           Rexp_ifthenelse _),
          _ ) ->
        false
    in
    let rec add_binders pairs = function
      | [] -> Some pairs
      | (left, right) :: rest ->
        if equal_type left.rb_type right.rb_type
        then
          Option.bind
            (add_pair pairs left.rb_id right.rb_id)
            (fun pairs -> add_binders pairs rest)
        else None
    in
    Option.fold
      ~none:false
      ~some:(fun pairs -> equal pairs left right)
      (add_binders [] binders)

  let equal_desc ~equal_type left right =
    equal_type left.ref_skeleton right.ref_skeleton
    && alpha_equal
         ~equal_type
         ~binders:[left.ref_view, right.ref_view]
         left.ref_pred right.ref_pred

  let print_constant ppf = function
    | Const_int value -> Format.fprintf ppf "%d" value
    | Const_char value -> Format.fprintf ppf "%C" value
    | Const_untagged_char value -> Format.fprintf ppf "#%C" value
    | Const_string (value, _, delimiter) ->
      Format.fprintf ppf "%S%a" value
        (fun ppf -> function
          | None -> ()
          | Some delimiter -> Format.fprintf ppf "[%s]" delimiter)
        delimiter
    | Const_float value -> Format.pp_print_string ppf value
    | Const_float32 value -> Format.fprintf ppf "%ss" value
    | Const_unboxed_float value -> Format.fprintf ppf "#%s" value
    | Const_unboxed_float32 value -> Format.fprintf ppf "#%ss" value
    | Const_int8 value -> Format.fprintf ppf "%ds" value
    | Const_int16 value -> Format.fprintf ppf "%dS" value
    | Const_int32 value -> Format.fprintf ppf "%ldl" value
    | Const_int64 value -> Format.fprintf ppf "%LdL" value
    | Const_nativeint value -> Format.fprintf ppf "%ndn" value
    | Const_untagged_int value -> Format.fprintf ppf "#%d" value
    | Const_untagged_int8 value -> Format.fprintf ppf "#%ds" value
    | Const_untagged_int16 value -> Format.fprintf ppf "#%dS" value
    | Const_unboxed_int32 value -> Format.fprintf ppf "#%ldl" value
    | Const_unboxed_int64 value -> Format.fprintf ppf "#%LdL" value
    | Const_unboxed_nativeint value -> Format.fprintf ppf "#%ndn" value

  let print_path = Format_doc.compat Path.print

  let print_reference ppf = function
    | Rfun name -> Format.fprintf ppf "fun[%s]" name
    | Rsibling name -> Format.fprintf ppf "sibling[%s]" name
    | Rapp path -> Format.fprintf ppf "app[%a]" print_path path
    | Rglobal path -> Format.fprintf ppf "global[%a]" print_path path

  let print_bound_identifier ppf id =
    Format.pp_print_string ppf (Ident.name id)

  let print_identifier ppf = function
    | Rbound id -> print_bound_identifier ppf id
    | Rfree reference -> print_reference ppf reference

  let print_label ppf = function
    | Nolabel -> ()
    | Labelled label -> Format.fprintf ppf "~%s:" label
    | Optional label -> Format.fprintf ppf "?%s:" label
    | Position label -> Format.fprintf ppf "@%s:" label

  let rec print ppf expression =
    match expression.rexp_desc with
    | Rexp_ident identifier -> print_identifier ppf identifier
    | Rexp_constant constant -> print_constant ppf constant
    | Rexp_let (bindings, body) ->
      Format.fprintf ppf "(@[<2>let %a in@ %a@])"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ and ")
           (fun ppf binding ->
             Format.fprintf ppf "%a =@ %a"
               print_bound_identifier binding.rbind_binder.rb_id
               print binding.rbind_expr))
        bindings print body
    | Rexp_function { arg_label; param; body } ->
      Format.fprintf ppf "(@[<2>fun %a%a ->@ %a@])"
        print_label arg_label print_bound_identifier param.rb_id print body
    | Rexp_apply (function_, arguments) ->
      Format.fprintf ppf "(@[<2>%a%a@])" print function_
        (fun ppf arguments ->
          List.iter
            (fun (label, argument) ->
              Format.fprintf ppf "@ %a%a" print_label label print argument)
            arguments)
        arguments
    | Rexp_tuple fields ->
      Format.fprintf ppf "(@[<hov>%a@])"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           (fun ppf (label, field) ->
             match label with
             | None -> print ppf field
             | Some label -> Format.fprintf ppf "~%s:%a" label print field))
        fields
    | Rexp_construct (constructor, arguments) ->
      Format.fprintf ppf "@[<2>constructor[%a.%s]%a@]"
        print_path constructor.rconstr_type_path constructor.rconstr_name
        (fun ppf arguments ->
          List.iter (fun argument -> Format.fprintf ppf "@ %a" print argument)
            arguments)
        arguments
    | Rexp_field (record, field) ->
      Format.fprintf ppf "(%a).field[%a.%s]" print record
        print_path field.rfield_type_path field.rfield_name
    | Rexp_ifthenelse (condition, ifso, ifnot) ->
      Format.fprintf ppf "(@[<2>if %a@ then %a%a@])"
        print condition print ifso
        (fun ppf -> function
          | None -> ()
          | Some ifnot -> Format.fprintf ppf "@ else %a" print ifnot)
        ifnot

  exception Invalid of validation_error

  let validate ~equal_type ~bool_type ?unit_type ?(binders = []) expression =
    let invalid error = raise (Invalid error) in
    let same_type left right = equal_type left right in
    let strip_mono type_ =
      match get_desc type_ with
      | Tpoly (type_, []) -> type_
      | _ -> type_
    in
    let add_binder bound seen binder =
      if Ident.is_global binder.rb_id then invalid (Global_binder binder.rb_id);
      if Ident.Set.mem binder.rb_id seen
      then invalid (Duplicate_binder binder.rb_id);
      ( Ident.Map.add binder.rb_id binder.rb_type bound,
        Ident.Set.add binder.rb_id seen )
    in
    let require_name kind name =
      if String.length name = 0 then invalid (Invalid_name kind)
    in
    let validate_reference = function
      | Rfun name -> require_name "function" name
      | Rsibling name -> require_name "sibling" name
      | Rapp _ | Rglobal _ -> ()
    in
    let rec loop bound seen expression =
      match expression.rexp_desc with
      | Rexp_ident (Rbound id) ->
        begin match Ident.Map.find_opt id bound with
        | None -> invalid (Unbound_identifier id)
        | Some binder_type ->
          if not (same_type binder_type expression.rexp_type)
          then invalid (Bound_identifier_type_mismatch id)
        end;
        seen
      | Rexp_ident (Rfree reference) ->
        validate_reference reference;
        seen
      | Rexp_constant _ -> seen
      | Rexp_let (bindings, body) ->
        if bindings = [] then invalid Empty_let;
        let seen =
          List.fold_left
            (fun seen binding ->
              let seen = loop bound seen binding.rbind_expr in
              if not
                   (same_type
                      binding.rbind_binder.rb_type
                      binding.rbind_expr.rexp_type)
              then invalid Let_type_mismatch;
              seen)
            seen bindings
        in
        let body_bound, seen =
          List.fold_left
            (fun (bound, seen) binding ->
              add_binder bound seen binding.rbind_binder)
            (bound, seen) bindings
        in
        let seen = loop body_bound seen body in
        if not (same_type expression.rexp_type body.rexp_type)
        then invalid Let_type_mismatch;
        seen
      | Rexp_function { arg_label; param; body } ->
        let bound, seen = add_binder bound seen param in
        let seen = loop bound seen body in
        begin match get_desc expression.rexp_type with
        | Tarrow ((type_label, _, _), argument_type, result_type, _)
          when type_label = arg_label
               && same_type (strip_mono argument_type) param.rb_type
               && same_type result_type body.rexp_type ->
          ()
        | _ -> invalid Function_type_mismatch
        end;
        seen
      | Rexp_apply (function_, arguments) ->
        if arguments = [] then invalid Apply_type_mismatch;
        let seen = loop bound seen function_ in
        let function_type, seen =
          List.fold_left
            (fun (function_type, seen) (label, argument) ->
              let seen = loop bound seen argument in
              match get_desc function_type with
              | Tarrow ((type_label, _, _), argument_type, result_type, _)
                when type_label = label
                     && same_type
                          (strip_mono argument_type) argument.rexp_type ->
                result_type, seen
              | _ -> invalid Apply_type_mismatch)
            (function_.rexp_type, seen) arguments
        in
        if not (same_type function_type expression.rexp_type)
        then invalid Apply_type_mismatch;
        seen
      | Rexp_tuple fields ->
        let seen =
          List.fold_left
            (fun seen (_, field) -> loop bound seen field)
            seen fields
        in
        begin match get_desc expression.rexp_type with
        | Ttuple field_types
          when List.length fields >= 2
               && List.length fields = List.length field_types
               && List.for_all2
                    (fun (label, field) (type_label, field_type) ->
                      label = type_label
                      && same_type field.rexp_type field_type)
                    fields field_types ->
          ()
        | _ -> invalid Tuple_type_mismatch
        end;
        seen
      | Rexp_construct (constructor, arguments) ->
        require_name "constructor" constructor.rconstr_name;
        List.fold_left (loop bound) seen arguments
      | Rexp_field (record, field) ->
        require_name "field" field.rfield_name;
        loop bound seen record
      | Rexp_ifthenelse (condition, ifso, ifnot) ->
        let seen = loop bound seen condition in
        let seen = loop bound seen ifso in
        let seen =
          Option.fold ~none:seen ~some:(loop bound seen) ifnot
        in
        if not (same_type condition.rexp_type bool_type)
           || not (same_type ifso.rexp_type expression.rexp_type)
           || not
                (Option.fold
                   ~none:
                     (Option.fold
                        ~none:false
                        ~some:(same_type expression.rexp_type)
                        unit_type)
                   ~some:(fun ifnot ->
                     same_type ifnot.rexp_type expression.rexp_type)
                   ifnot)
        then invalid If_type_mismatch;
        seen
    in
    try
      if not (same_type expression.rexp_type bool_type)
      then invalid Root_type_mismatch;
      let bound, seen =
        List.fold_left
          (fun (bound, seen) binder -> add_binder bound seen binder)
          (Ident.Map.empty, Ident.Set.empty)
          binders
      in
      ignore (loop bound seen expression : Ident.Set.t);
      Ok ()
    with Invalid error -> Error error

  let print_validation_error ppf = function
    | Root_type_mismatch ->
      Format.pp_print_string ppf "root expression does not have type bool"
    | Unbound_identifier id ->
      Format.fprintf ppf "unbound refinement identifier %s" (Ident.name id)
    | Bound_identifier_type_mismatch id ->
      Format.fprintf ppf "type mismatch for refinement identifier %s"
        (Ident.name id)
    | Duplicate_binder id ->
      Format.fprintf ppf "duplicate refinement binder %s" (Ident.name id)
    | Global_binder id ->
      Format.fprintf ppf "refinement binder %s is global" (Ident.name id)
    | Empty_let -> Format.pp_print_string ppf "empty refinement let"
    | Invalid_name kind ->
      Format.fprintf ppf "empty refinement %s name" kind
    | Function_type_mismatch ->
      Format.pp_print_string ppf "refinement function type mismatch"
    | Apply_type_mismatch ->
      Format.pp_print_string ppf "refinement application type mismatch"
    | Let_type_mismatch ->
      Format.pp_print_string ppf "refinement let type mismatch"
    | If_type_mismatch ->
      Format.pp_print_string ppf "refinement conditional type mismatch"
    | Tuple_type_mismatch ->
      Format.pp_print_string ppf "refinement tuple type mismatch"
end

(* with-bounds *)

(* Compare types roughly semantically, to allow best-effort deduplication of the types
   inside of with-bounds.

   This function might compare two types as inequal that are actually equal, but should
   /never/ compare two types as equal that are not semantically equal. It may go without
   saying but it also needs to expose a total order.

   Someday, it's probably desirable to merge this, and make it compatible, with
   [Ctype.eqtype], though that seems quite hard.
*)
(* CR layouts v2.8: this will likely loop infinitely on rectypes. Internal
   ticket 5086. *)
(* CR layouts v2.8: this whole approach is probably /quite/ wrong, since
   type_expr is fundamentally mutable, and using mutable things in the keys of
   maps is a recipe for disaster. We haven't found a way that this can break
   /yet/, but it is likely that one exists. We should rethink this whole
   approach soon. Internal ticket 5086. *)
let best_effort_compare_type_expr te1 te2 =
  let max_depth = 50 in
  let rank_by_id ty =
    (* This negation is important! We want all these types to compare strictly /less/
       than the structural ones - the easiest way to make that happen is to make the
       id negative, and ensure the ranks of all the other variants are positive *)
    -ty.id
  in
  let rec aux depth te1 te2 =
    if te1 == te2 || repr te1 == repr te2 then 0
    else if depth >= max_depth
    then (rank_by_id te1) - (rank_by_id te2)
    else
      let rank ty =
        match get_desc ty with
        (* Types which must be compared by id *)
        | Tvar _
        | Tunivar _
        | Tobject (_, _)
        | Tfield (_, _, _, _)
        | Tnil
        | Tvariant _
        | Tpackage _
        | Tarrow (_, _, _, _)
        | Tquote _
        | Tsplice _
        | Tquote_eval _
        | Tbox _
        | Trefine _
        (* CR layouts v2.8: we can actually see Tsubst here in certain cases, eg during
           [Ctype.copy] when copying the types inside of with_bounds. We also can't
           compare Tsubst structurally, because the Tsubsts that are created in
           Ctype.copy are cyclic (?). So the best we can do here is compare by id.
           this is almost definitely wrong, primarily because of the mutability - we
           should fix that. Internal ticket 5086. *)
        | Tsubst (_, _)
          -> rank_by_id ty
        (* Types which we know how to compare structurally*)
        | Ttuple _ -> 2
        | Tunboxed_tuple _ -> 3
        | Tconstr (_, _, _) -> 5
        | Tpoly (_, _) -> 6
        | Tof_kind _ -> 7
        | Trepr (_, _) -> 8
        (* Types we should never see *)
        | Tlink _ -> Misc.fatal_error "Tlink encountered in With_bounds_types"
      in
      match get_desc te1, get_desc te2 with
      | Ttuple elts1, Ttuple elts2
      | Tunboxed_tuple elts1, Tunboxed_tuple elts2 ->
        List.compare
          (fun (l1, te1) (l2, te2) ->
             let l = Option.compare String.compare l1 l2 in
             if l = 0 then aux (depth + 1) te1 te2 else l
          )
          elts1
          elts2
      | Tconstr (p1, args1, _), Tconstr (p2, args2, _) ->
        let p = Path.compare p1 p2 in
        if p = 0
        then List.compare (aux (depth + 1)) args1 args2
        else p
      | Tpoly (t1, ts1), Tpoly (t2, ts2) ->
        (* NOTE: this is mostly broken according to the semantics of type_expr, but probably
           fine for the particular "best-effort" comparison we want. *)
        List.compare (aux (depth + 1)) (t1 :: ts1) (t2 :: ts2)
      | Trepr (t1, sort_vars1), Trepr (t2, sort_vars2) ->
        (* Compare by establishing correspondence between univars and comparing
           the inner types with that correspondence. *)
        (match List.combine sort_vars1 sort_vars2 with
         | exception Invalid_argument _ ->
           Int.compare (List.length sort_vars1) (List.length sort_vars2)
         | pairs ->
           Jkind_types.Sort.enter_repr pairs (fun () -> aux (depth + 1) t1 t2))
      | _, _ -> rank te1 - rank te2
  in
  aux 0 te1 te2

(* A map from [type_expr] to [With_bounds_type_info.t], specifically defined with a
   (best-effort) semantic comparison function on types to be used in the with-bounds of a
   jkind.

   This module is defined internally to be equal (via two uses of [Obj.magic]) to the
   abstract type [with_bound_types] to break the circular dependency between with-bounds
   and type_expr. The alternative to this approach would be mutually recursive modules,
   but this approach creates a smaller diff with upstream and makes rebasing easier.
*)
module With_bounds_types : sig
  (* Note that only the initially needed bits of [Stdlib.Map.S] are exposed here; feel
     free to expose more functions if you need them! *)
  type t = with_bounds_types
  type info := With_bounds_type_info.t

  val empty : t
  val is_empty : t -> bool
  val to_seq : t -> (type_expr * info) Seq.t
  val of_list : (type_expr * info) list -> t
  val of_seq : (type_expr * info) Seq.t -> t
  val singleton : type_expr -> info -> t
  val map : (info -> info) -> t -> t
  val map_with_key : (type_expr -> info -> type_expr * info) -> t -> t
  val merge
    : (type_expr -> info option -> info option -> info option) ->
    t -> t -> t
  val update : type_expr -> (info option -> info option) -> t -> t
  val find_opt : type_expr -> t -> info option
  val for_all : (type_expr -> info -> bool) -> t -> bool
  val exists : (type_expr -> info -> bool) -> t -> bool
end = struct
  module M = Map.Make(struct
      (* CR layouts v2.8: A [Map] with mutable values (of which [type_expr] is
         one) as keys is deeply problematic. And in fact we never actually use
         this map structure for anything other than deduplication (indeed we
         can't, because of its best-effort nature). Instead of this structure,
         we should store the types inside of with-bounds as a (morally
         immutable) array, and write a [deduplicate] function, private to
         [Jkind], which uses this map structure to deduplicate the with-bounds,
         but only during construction and after normalization. Internal ticket
         5086.*)
      type t = type_expr

      let compare = best_effort_compare_type_expr
    end)
  include M

  type map = With_bounds_type_info.t M.t
  type t = with_bounds_types

  let of_map : map -> with_bounds_types = Obj.magic
  let to_map : with_bounds_types -> map = Obj.magic

  let empty = empty |> of_map
  let is_empty t = t |> to_map |> is_empty
  let to_seq t = t |> to_map |> to_seq
  let of_seq s = of_seq s |> of_map
  let of_list l = l |> List.to_seq |> of_seq
  let singleton ty i = add ty i (to_map empty) |> of_map
  let map f t = map f (to_map t) |> of_map
  let merge f t1 t2 = merge f (to_map t1) (to_map t2) |> of_map
  let update te f t = update te f (to_map t) |> of_map
  let find_opt te t = find_opt te (to_map t)
  let for_all f t = for_all f (to_map t)
  let exists f t = exists f (to_map t)
  let map_with_key f t =
    fold (fun key value acc ->
      let key, value = f key value in
      M.add key value acc) (to_map t) M.empty |> of_map
end

let equal_unsafe_mode_crossing
      ~type_equal
      { unsafe_mod_bounds = mc1; unsafe_with_bounds = wb2 }
      umc2 =
  Misc.Le_result.equal ~le:Mode.Crossing.le mc1 umc2.unsafe_mod_bounds
  && (match wb2, umc2.unsafe_with_bounds with
    | No_with_bounds, No_with_bounds -> true
    | No_with_bounds, With_bounds _ | With_bounds _, No_with_bounds -> false
    | With_bounds wb1, With_bounds wb2 ->
      (* It's tough (impossible?) to do better than a double subset check here because of
         the fact that these maps are best-effort. But in practice these will usually not
         be huge, and the attribute triggering this check is (hopefully) rare. *)
      With_bounds_types.for_all
        (fun ty1 _info ->
           With_bounds_types.exists
             (fun ty2 _info -> type_equal ty1 ty2)
             wb2)
        wb1
      && With_bounds_types.for_all
        (fun ty2 _info ->
           With_bounds_types.exists
             (fun ty1 _info -> type_equal ty1 ty2)
             wb1)
        wb2)

(* Constructor and accessors for [row_desc] *)

let create_row ~fields ~more ~closed ~fixed ~name =
  { row_fields=fields; row_more=more;
    row_closed=closed; row_fixed=fixed; row_name=name }

(* [row_fields] subsumes the original [row_repr] *)
let rec row_fields row =
  match get_desc row.row_more with
  | Tvariant row' ->
    row.row_fields @ row_fields row'
  | _ ->
    row.row_fields

let rec row_repr_no_fields row =
  match get_desc row.row_more with
  | Tvariant row' -> row_repr_no_fields row'
  | _ -> row

let row_more row = (row_repr_no_fields row).row_more
let row_closed row = (row_repr_no_fields row).row_closed
let row_fixed row = (row_repr_no_fields row).row_fixed
let row_name row = (row_repr_no_fields row).row_name

let rec get_row_field tag row =
  let rec find = function
    | (tag',f) :: fields ->
        if tag = tag' then f else find fields
    | [] ->
        match get_desc row.row_more with
        | Tvariant row' -> get_row_field tag row'
        | _ -> RFabsent
  in find row.row_fields

let set_row_name row row_name =
  let row_fields = row_fields row in
  let row = row_repr_no_fields row in
  {row with row_fields; row_name}

type row_desc_repr =
    Row of { fields: (label * row_field) list;
             more:type_expr;
             closed:bool;
             fixed:fixed_explanation option;
             name:(Path.t * type_expr list) option }

let row_repr row =
  let fields = row_fields row in
  let row = row_repr_no_fields row in
  Row { fields;
        more = row.row_more;
        closed = row.row_closed;
        fixed = row.row_fixed;
        name = row.row_name }

type row_field_view =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

let rec row_field_repr_aux tl : row_field -> row_field = function
  | RFeither ({ext = {contents = RFnone}} as r) ->
      RFeither {r with arg_type = tl@r.arg_type}
  | RFeither {arg_type;
              ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      row_field_repr_aux (tl@arg_type) rf
  | RFpresent (Some _) when tl <> [] ->
      RFpresent (Some (List.hd tl))
  | RFpresent _ as rf -> rf
  | RFabsent -> RFabsent

let row_field_repr fi =
  match row_field_repr_aux [] fi with
  | RFeither {no_arg; arg_type; matched} -> Reither (no_arg, arg_type, matched)
  | RFpresent t -> Rpresent t
  | RFabsent -> Rabsent

let rec row_field_ext (fi : row_field) =
  match fi with
  | RFeither {ext = {contents = RFnone} as ext} -> ext
  | RFeither {ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      row_field_ext rf
  | _ -> Misc.fatal_error "Types.row_field_ext "

let rf_present oty = RFpresent oty
let rf_absent = RFabsent
let rf_either ?use_ext_of ~no_arg arg_type ~matched =
  let ext =
    match use_ext_of with
      Some rf -> row_field_ext rf
    | None -> ref RFnone
  in
  RFeither {no_arg; arg_type; matched; ext}

let rf_either_of = function
  | None ->
      RFeither {no_arg=true; arg_type=[]; matched=false; ext=ref RFnone}
  | Some ty ->
      RFeither {no_arg=false; arg_type=[ty]; matched=false; ext=ref RFnone}

let eq_row_field_ext rf1 rf2 =
  row_field_ext rf1 == row_field_ext rf2

let changed_row_field_exts l f =
  let exts = List.map row_field_ext l in
  f ();
  List.exists (fun r -> !r <> RFnone) exts

let match_row_field ~present ~absent ~either (f : row_field) =
  match f with
  | RFabsent -> absent ()
  | RFpresent t -> present t
  | RFeither {no_arg; arg_type; matched; ext} ->
      let e : row_field option =
        match !ext with
        | RFnone -> None
        | RFeither _ | RFpresent _ | RFabsent as e -> Some e
      in
      either no_arg arg_type matched (ext,e)

(**** Some type creators ****)

let new_id = Local_store.s_ref (-1)

let create_expr = Transient_expr.create

let proto_newty3 ~level ~scope desc  =
  incr new_id;
  create_expr desc ~level ~scope ~id:!new_id

                  (**********************************)
                  (*  Utilities for backtracking    *)
                  (**********************************)

let undo_change = function
    Ctype  (ty, desc) -> Transient_expr.set_desc ty desc
  | Ccompress (ty, desc, _) -> Transient_expr.set_desc ty desc
  | Clevel (ty, level) -> Transient_expr.set_level ty level
  | Cscope (ty, scope) -> Transient_expr.set_scope ty scope
  | Cname  (r, v)    -> r := v
  | Crow   r         -> r := RFnone
  | Ckind  (FKvar r) -> r.field_kind <- FKprivate
  | Ccommu (Cvar r)  -> r.commu <- Cunknown
  | Cuniv  (r, v)    -> r := v
  | Cmodes c          -> Mode.undo_changes c
  | Csort change -> Jkind_types.Sort.undo_change change
  | Czero_alloc c -> Zero_alloc.undo_change c

type snapshot = changes ref * int
let last_snapshot = Local_store.s_ref 0

let log_type ty =
  if ty.id <= !last_snapshot then log_change (Ctype (ty, ty.desc))
let link_type ty ty' =
  let ty = repr ty in
  let ty' = repr ty' in
  if ty == ty' then () else begin
  log_type ty;
  let desc = ty.desc in
  Transient_expr.set_desc ty (Tlink ty');
  (* Name is a user-supplied name for this unification variable (obtained
   * through a type annotation for instance). *)
  match desc, ty'.desc with
    Tvar { name }, Tvar { name = name'; jkind = jkind' } ->
      begin match name, name' with
      | Some _, None ->
        log_type ty';
        Transient_expr.set_desc ty' (Tvar { name; jkind = jkind' })
      | None, Some _ -> ()
      | Some _, Some _ ->
        if ty.level < ty'.level then begin
          log_type ty';
          Transient_expr.set_desc ty' (Tvar { name; jkind = jkind' })
        end
      | None, None   -> ()
      end
  | _ -> ()
  end
  (* ; assert (check_memorized_abbrevs ()) *)
  (*  ; check_expans [] ty' *)
(* TODO: consider eliminating set_type_desc, replacing it with link types *)
let set_type_desc ty td =
  let ty = repr ty in
  if td != ty.desc then begin
    log_type ty;
    Transient_expr.set_desc ty td
  end
(* TODO: separate set_level into two specific functions: *)
(*  set_lower_level and set_generic_level *)
let set_level ty level =
  let ty = repr ty in
  if level <> ty.level then begin
    if ty.id <= !last_snapshot then log_change (Clevel (ty, ty.level));
    Transient_expr.set_level ty level
  end

(* TODO: introduce a guard and rename it to set_higher_scope? *)
let set_scope ty scope =
  let ty = repr ty in
  let prev_scope = ty.scope land scope_mask in
  if scope <> prev_scope then begin
    if ty.id <= !last_snapshot then log_change (Cscope (ty, prev_scope));
    Transient_expr.set_scope ty scope
  end
let set_var_jkind ty jkind =
  let ty = repr ty in
  log_type ty;
  Transient_expr.set_var_jkind ty jkind
let set_univar rty ty =
  log_change (Cuniv (rty, !rty)); rty := Some ty
let set_name nm v =
  log_change (Cname (nm, !nm)); nm := v

let rec link_row_field_ext ~(inside : row_field) (v : row_field) =
  match inside with
  | RFeither {ext = {contents = RFnone} as e} ->
      let RFeither _ | RFpresent _ | RFabsent as v = v in
      log_change (Crow e); e := v
  | RFeither {ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      link_row_field_ext ~inside:rf v
  | _ -> invalid_arg "Types.link_row_field_ext"

let rec link_kind ~(inside : field_kind) (k : field_kind) =
  match inside with
  | FKvar ({field_kind = FKprivate} as rk) as inside ->
      (* prevent a loop by normalizing k and comparing it with inside *)
      let FKvar _ | FKpublic | FKabsent as k = field_kind_internal_repr k in
      if k != inside then begin
        log_change (Ckind inside);
        rk.field_kind <- k
      end
  | FKvar {field_kind = FKvar _ | FKpublic | FKabsent as inside} ->
      link_kind ~inside k
  | _ -> invalid_arg "Types.link_kind"

let rec commu_repr : commutable -> commutable = function
  | Cvar {commu = Cvar _ | Cok as commu} -> commu_repr commu
  | c -> c

let rec link_commu ~(inside : commutable) (c : commutable) =
  match inside with
  | Cvar ({commu = Cunknown} as rc) as inside ->
      (* prevent a loop by normalizing c and comparing it with inside *)
      let Cvar _ | Cok as c = commu_repr c in
      if c != inside then begin
        log_change (Ccommu inside);
        rc.commu <- c
      end
  | Cvar {commu = Cvar _ | Cok as inside} ->
      link_commu ~inside c
  | _ -> invalid_arg "Types.link_commu"

let set_commu_ok c = link_commu ~inside:c Cok

let snapshot () =
  let old = !last_snapshot in
  last_snapshot := !new_id;
  (!trail, old)

let rec rev_log accu = function
    Unchanged -> accu
  | Invalid -> assert false
  | Change (ch, next) ->
      let d = !next in
      next := Invalid;
      rev_log (ch::accu) d

let backtrack ~cleanup_abbrev (changes, old) =
  match !changes with
    Unchanged -> last_snapshot := old
  | Invalid -> failwith "Types.backtrack"
  | Change _ as change ->
      cleanup_abbrev ();
      let backlog = rev_log [] change in
      List.iter undo_change backlog;
      changes := Unchanged;
      last_snapshot := old;
      trail := changes

let undo_first_change_after (changes, _) =
  match !changes with
  | Change (ch, _) ->
      undo_change ch
  | _ -> ()

let rec rev_compress_log log r =
  match !r with
    Unchanged | Invalid ->
      log
  | Change (Ccompress _, next) ->
      rev_compress_log (r::log) next
  | Change (_, next) ->
      rev_compress_log log next

let undo_compress (changes, _old) =
  match !changes with
    Unchanged
  | Invalid -> ()
  | Change _ ->
      let log = rev_compress_log [] changes in
      List.iter
        (fun r -> match !r with
          Change (Ccompress (ty, desc, d), next) when ty.desc == d ->
            Transient_expr.set_desc ty desc; r := !next
        | _ -> ())
        log

let class_mode =
  let hint : _ Mode.Hint.const = Legacy Class in
  Mode.Value.(of_const ~hint_monadic:hint ~hint_comonadic:hint Const.legacy)

let toplevel_mode =
  let hint : _ Mode.Hint.const = Legacy Toplevel in
  Mode.Value.(of_const ~hint_monadic:hint ~hint_comonadic:hint Const.legacy)
