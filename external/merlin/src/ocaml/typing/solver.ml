(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Stephen Dolan, Jane Street, London                   *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Allowance
open Solver_intf
module Fmt = Format_doc

module Solver_mono (H : Hint) (C : Lattices_mono) = struct
  exception Exact_zap_impossible

  exception Exact_resource_limit

  exception Exact_inconsistent_state

  type quantifier =
    | Outer
    | Universal of int
    | Existential of int

  type ('a, 'd) hint =
    | Apply :
        'd H.Morph.t * ('b, 'a, 'd) C.morph * ('b, 'd) ahint
        -> ('a, 'd) hint
    | Const : 'd H.Const.t -> ('a, 'd) hint
    | Branch : 'd branch * ('a, 'd) ahint * ('a, 'd) ahint -> ('a, 'd) hint
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  and ('a, 'd) ahint = 'a * ('a, 'd) hint constraint 'd = _ * _

  type 'a error =
    { left : ('a, left_only) ahint;
      right : ('a, right_only) ahint
    }

  (** Hint used internally by the solver. This can be converted to [ahint] by
      [populate]. *)
  module Comp_hint = struct
    module Morph_hint = struct
      (** [('a, 'b, 'd) t] is a hint for a morphism from ['a] to ['b] with
          allowance ['d] *)
      type ('a, 'b, 'd) t =
        | Base : 'd H.Morph.t * ('a, 'b, 'd) C.morph -> ('a, 'b, 'd) t
        | Compose :
            ('b, 'c, 'l * 'r) t * ('a, 'b, 'l * 'r) t
            -> ('a, 'c, 'l * 'r) t
        | Id : ('a, 'a, 'l * 'r) t
            (** Short-hand for [Base (H.id, C.id)] to save memory *)
        | Adjoint_l :
            ('a, 'b, 'l2 * allowed) t * ('b, 'a, allowed * disallowed) C.morph
            -> ('b, 'a, 'l * disallowed) t
            (** [Adjoint_l (h, m)] is the left adjoint of [h], deferred; [m] is
                the (already computed) left adjoint of [h]'s morphism. *)
        | Adjoint_r :
            ('a, 'b, allowed * 'r2) t * ('b, 'a, disallowed * allowed) C.morph
            -> ('b, 'a, disallowed * 'r) t
            (** Right-adjoint analogue of [Adjoint_l]. *)
        constraint 'd = _ * _
      [@@ocaml.warning "-62"]

      (** Composes two hint morphisms [f] and [g]. If [f] is [Id] we return [g]
          and vice-versa. Otherwise we return [Compose (f, g)]. *)
      let compose : type a b c l r.
          (b, c, l * r) t -> (a, b, l * r) t -> (a, c, l * r) t =
       fun m1 m2 ->
        match m1, m2 with Id, m -> m | m, Id -> m | _, _ -> Compose (m1, m2)

      include Magic_allow_disallow (struct
        type ('a, 'b, 'd) sided = ('a, 'b, 'd) t constraint 'd = 'l * 'r

        let rec allow_left : type l a b r.
            (a, b, allowed * r) t -> (a, b, l * r) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.allow_left morph_hint, C.allow_left morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (allow_left a_morph_hint, allow_left b_morph_hint)
          | Adjoint_l (h, m) -> Adjoint_l (h, m)

        let rec allow_right : type a b l r.
            (a, b, l * allowed) t -> (a, b, l * r) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.allow_right morph_hint, C.allow_right morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (allow_right a_morph_hint, allow_right b_morph_hint)
          | Adjoint_r (h, m) -> Adjoint_r (h, m)

        let rec disallow_left : type a b l r.
            (a, b, l * r) t -> (a, b, disallowed * r) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.disallow_left morph_hint, C.disallow_left morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (disallow_left a_morph_hint, disallow_left b_morph_hint)
          | Adjoint_l (h, m) -> Adjoint_l (h, m)
          | Adjoint_r (h, m) -> Adjoint_r (h, m)

        let rec disallow_right : type a b l r.
            (a, b, l * r) t -> (a, b, l * disallowed) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.disallow_right morph_hint, C.disallow_right morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (disallow_right a_morph_hint, disallow_right b_morph_hint)
          | Adjoint_l (h, m) -> Adjoint_l (h, m)
          | Adjoint_r (h, m) -> Adjoint_r (h, m)
      end)

      let rec left_adjoint : type a b l.
          H.Pinpoint.t ->
          b C.obj ->
          (a, b, l * allowed) t ->
          H.Pinpoint.t * a C.obj * (b, a, allowed * disallowed) t =
       fun pp b_obj -> function
        | Id -> pp, b_obj, Id
        | Adjoint_r (h, m) -> pp, C.src b_obj m, disallow_right h
        | Base (small_morph_hint, morph) ->
          let pp, small_morph_hint = H.Morph.left_adjoint pp small_morph_hint in
          ( pp,
            C.src b_obj morph,
            Base (small_morph_hint, C.left_adjoint b_obj morph) )
        | Compose (f_morph_hint, g_morph_hint) ->
          let mid_pp, mid, f_morph_hint_adj =
            left_adjoint pp b_obj f_morph_hint
          in
          let src_pp, src, g_morph_hint_adj =
            left_adjoint mid_pp mid g_morph_hint
          in
          src_pp, src, Compose (g_morph_hint_adj, f_morph_hint_adj)

      let rec right_adjoint : type a b r.
          H.Pinpoint.t ->
          b C.obj ->
          (a, b, allowed * r) t ->
          H.Pinpoint.t * a C.obj * (b, a, disallowed * allowed) t =
       fun pp b_obj -> function
        | Id -> pp, b_obj, Id
        | Adjoint_l (h, m) -> pp, C.src b_obj m, disallow_left h
        | Base (small_morph_hint, morph) ->
          let pp, small_morph_hint =
            H.Morph.right_adjoint pp small_morph_hint
          in
          ( pp,
            C.src b_obj morph,
            Base (small_morph_hint, C.right_adjoint b_obj morph) )
        | Compose (f_morph_hint, g_morph_hint) ->
          let mid_pp, mid, f_morph_hint_adj =
            right_adjoint pp b_obj f_morph_hint
          in
          let src_pp, src, g_morph_hint_adj =
            right_adjoint mid_pp mid g_morph_hint
          in
          src_pp, src, Compose (g_morph_hint_adj, f_morph_hint_adj)

      let rec populate : type b a l r.
          a C.obj ->
          (b, a, l * r) t ->
          (b C.obj -> (b, l * r) ahint) ->
          (a, l * r) ahint =
       fun obj_a hint cont ->
        match hint with
        | Id -> cont obj_a
        | Adjoint_l (h, m) ->
          let obj_b = C.src obj_a m in
          let _, _, h' = left_adjoint H.Pinpoint.unknown obj_b h in
          populate obj_a (allow_left h') cont
        | Adjoint_r (h, m) ->
          let obj_b = C.src obj_a m in
          let _, _, h' = right_adjoint H.Pinpoint.unknown obj_b h in
          populate obj_a (allow_right h') cont
        | Base (morph_hint, morph) ->
          let obj_b = C.src obj_a morph in
          let ahint = cont obj_b in
          let a = C.apply obj_a morph (fst ahint) in
          a, Apply (morph_hint, morph, ahint)
        | Compose (h1, h2) ->
          populate obj_a h1 (fun obj_mid -> populate obj_mid h2 cont)
    end

    type ('a, 'd) t =
      | Apply : ('b, 'a, 'd) Morph_hint.t * ('b, 'd) t -> ('a, 'd) t
      | Const : 'd H.Const.t * 'a -> ('a, 'd) t
      | Branch : 'd branch * ('a, 'd) t * ('a, 'd) t -> ('a, 'd) t
      | Min : ('a, 'l * disallowed) t
          (** Short-hand for [Const (H.Const.min, C.min) to save memory] *)
      | Max : ('a, disallowed * 'r) t
          (** Short-hand for [Const (H.Const.max, C.max) to save memory] *)
      | Unknown : 'a -> ('a, 'l * 'r) t
          (** Short-hand for [Const (H.Const.unknown, a) to save memory] *)
      constraint 'd = _ * _
    [@@ocaml.warning "-62"]

    include Magic_allow_disallow (struct
      type ('a, _, 'd) sided = ('a, 'd) t constraint 'd = 'l * 'r

      let rec allow_left : type a l r. (a, allowed * r) t -> (a, l * r) t =
        function
        | Apply (f_hint, h) -> Apply (Morph_hint.allow_left f_hint, allow_left h)
        | Const (h, c) -> Const (H.Const.allow_left h, c)
        | Branch (Join, h1, h2) -> Branch (Join, allow_left h1, allow_left h2)
        | Min -> Min
        | Unknown c -> Unknown c

      let rec allow_right : type a l r. (a, l * allowed) t -> (a, l * r) t =
        function
        | Apply (f_hint, h) ->
          Apply (Morph_hint.allow_right f_hint, allow_right h)
        | Const (h, c) -> Const (H.Const.allow_right h, c)
        | Branch (Meet, h1, h2) -> Branch (Meet, allow_right h1, allow_right h2)
        | Max -> Max
        | Unknown c -> Unknown c

      let rec disallow_left : type a l r. (a, l * r) t -> (a, disallowed * r) t
          = function
        | Apply (f_hint, h) ->
          Apply (Morph_hint.disallow_left f_hint, disallow_left h)
        | Const (h, c) -> Const (H.Const.disallow_left h, c)
        | Branch (Join, h1, h2) ->
          Branch (Join, disallow_left h1, disallow_left h2)
        | Branch (Meet, h1, h2) ->
          Branch (Meet, disallow_left h1, disallow_left h2)
        | Min -> Min
        | Max -> Max
        | Unknown c -> Unknown c

      let rec disallow_right : type a l r. (a, l * r) t -> (a, l * disallowed) t
          = function
        | Apply (f_hint, h) ->
          Apply (Morph_hint.disallow_right f_hint, disallow_right h)
        | Const (h, c) -> Const (H.Const.disallow_right h, c)
        | Branch (Join, h1, h2) ->
          Branch (Join, disallow_right h1, disallow_right h2)
        | Branch (Meet, h1, h2) ->
          Branch (Meet, disallow_right h1, disallow_right h2)
        | Min -> Min
        | Max -> Max
        | Unknown c -> Unknown c
    end)

    (** This is for removing compositions. This function doesn't contain any
        [assert false] as it is just for a straightforward transformation *)
    let rec populate : type a l r. a C.obj -> (a, l * r) t -> (a, l * r) ahint =
     fun obj_a -> function
      | Min -> C.min obj_a, Const H.Const.min
      | Max -> C.max obj_a, Const H.Const.max
      | Unknown c -> c, Const H.Const.unknown
      | Const (const_hint, const) -> const, Const const_hint
      | Apply (morph_hint, hint) ->
        Morph_hint.populate obj_a morph_hint (fun src -> populate src hint)
      | Branch (b, hint1, hint2) -> (
        let ahint1 = populate obj_a hint1 in
        let ahint2 = populate obj_a hint2 in
        match b with
        | Join ->
          let a = C.join obj_a (fst ahint1) (fst ahint2) in
          a, Branch (Join, ahint1, ahint2)
        | Meet ->
          let a = C.meet obj_a (fst ahint1) (fst ahint2) in
          a, Branch (Meet, ahint1, ahint2))
  end

  (* All keys in a particular [VarMap] should have the same destination object,
     but the key type does not encode that invariant.  The comparator orders by
     variable id first, only checking object equality when equal ids force it to
     compare morphisms. *)
  type key = Key : 'b C.obj * int * ('a, 'b, 'd) C.morph -> key

  module VarMap = Map.Make (struct
    type t = key

    let compare (Key (obj1, id1, m1)) (Key (obj2, id2, m2)) =
      let c = Int.compare id1 id2 in
      if c <> 0
      then c
      else
        match C.equal_obj obj1 obj2 with
        | Misc.Is_eq -> C.compare_morph obj1 m1 m2
        | Misc.Is_not_eq ->
          Misc.fatal_error
            "Solver.VarMap.compare: inconsistent destination objects"
  end)

  (** Map the function to the list, and returns the first [Error] found; Returns
      [Ok ()] if no error. *)
  let find_error (f : 'x -> ('a, 'b) Result.t) (t : 'x VarMap.t) :
      ('a, 'b) Result.t =
    let r = ref (Ok ()) in
    let _ =
      VarMap.for_all
        (fun _ x ->
          match f x with
          | Ok () -> true
          | Error _ as e ->
            r := e;
            false)
        t
    in
    !r

  let var_map_to_list t = VarMap.fold (fun _ a xs -> a :: xs) t []

  type 'a var =
    { mutable level : int;
      quantifier : quantifier option;
      scope_order : int option;
          (** The level of the variable. This has the same meaning as the level
              field of a [type_expr]. *)
      mutable vlower : 'a lmorphvar VarMap.t;
          (** A list of variables directly under the current variable. Each is a
              pair [f] [v], and we have [f v <= u] where [u] is the current
              variable. *)
      mutable vupper : 'a rmorphvar VarMap.t;
          (** A list of variables directly above the current variable. Each is a
              pair [f] [v], and we have [u <= f v] where [u] is the current
              variable. *)
      mutable lower : 'a;
          (** The *conservative* lower bound of the variable. Why conservative:
              if a user calls [submode c u] where [c] is some constant and [u]
              some variable, we can modify [u.lower] of course. Idealy we should
              also modify all [v.lower] where [v] is variable above [u].
              However, variables at a higher [level] are not reachable from [u].
              Therefore, the [lower] of higher variables are not updated
              immediately, hence conservative. Those [lower] of higher variables
              can be made precise later on demand, see [zap_to_floor_var_aux].
          *)
      mutable lower_hint : ('a, left_only) Comp_hint.t;
          (** Hints for [lower] *)
      mutable upper : 'a;
          (** The conservative upper bound of the variable. See [lower] to
              understand why the bound is conservative *)
      mutable upper_hint : ('a, right_only) Comp_hint.t;
          (** Hints for [upper] *)
      (* To summarize, INVARIANT:
         - For any variable [v], we have [v.lower <= v.upper].
         - Variables that have been fully constrained will have
         [v.lower = v.upper]. Note that adding a boolean field indicating that
         won't help much.
         - For any [v] and [f u \in v.vlower], [u.level <= v.level]
         - For any [v] and [f u \in v.vupper], [u.level < v.level]
         - For any [v] and [f u \in v.vlower], we have [f u.upper <= v.upper],
           but not necessarily [f u.lower <= v.lower].
         - For any [v] and [f u \in v.vupper], we have [v.lower <= f u.lower],
           but not necessarily [v.upper <= f u.upper].
         - for all [f u \in v.vlower] and [g w \in v.vupper] we
           have either [g'f \in w.vlower] or [f'g \in u.vupper]. *)
      id : int;  (** For identification/printing *)
      mutable subst : 'a var option;
          (** Similar to Tsubst in a type, the [subst] field holds an optional
              copy used during instantiation *)
      mutable gencopy : 'a var option;
          (** Calls to [generalize_structure] creates additional copies to make
              sure the bounds of a generalize structure is rigid: [gencopy]
              caches such copies *)
      mutable exact : exact_component list
    }

  and exact_component =
    { members : anyvar list;
      guard_rows : anyvalue list list;
      witness_rows : anyvalue list list;
      graph_stamp : (Obj.t * Obj.t * Obj.t * Obj.t) list;
      symbolic : exact_symbolic option
    }

  and exact_symbolic =
    { manager : Solver_mdd.manager;
      guard : Solver_mdd.node;
      witness : Solver_mdd.node
    }

  and anyvar = Var : 'a C.obj * 'a var -> anyvar

  and anyvalue = Value : 'a C.obj * 'a -> anyvalue

  and 'b lmorphvar = ('b, left_only) morphvar

  and 'b rmorphvar = ('b, right_only) morphvar

  and ('b, 'd) morphvar =
    | Amorphvar :
        'a var * ('a, 'b, 'd) C.morph * ('a, 'b, 'd) Comp_hint.Morph_hint.t
        -> ('b, 'd) morphvar
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  let get_key dst (Amorphvar (v, m, _)) = Key (dst, v.id, m)

  module VarSet = Set.Make (Int)

  let vars = ref []

  type change =
    | Cupper : 'a var * 'a * ('a, right_only) Comp_hint.t -> change
    | Clower : 'a var * 'a * ('a, left_only) Comp_hint.t -> change
    | Cvlower : 'a var * 'a lmorphvar VarMap.t -> change
    | Cvupper : 'a var * 'a rmorphvar VarMap.t -> change
    | Clevel : 'a var * int -> change
    | Cgencopy : 'a var * 'a var option -> change
    | Cexact : 'a var * exact_component list -> change
    | Cvars of anyvar list

  type changes = change list

  let undo_change = function
    | Cupper (v, upper, upper_hint) ->
      v.upper <- upper;
      v.upper_hint <- upper_hint
    | Clower (v, lower, lower_hint) ->
      v.lower <- lower;
      v.lower_hint <- lower_hint
    | Cvlower (v, vlower) -> v.vlower <- vlower
    | Cvupper (v, vupper) -> v.vupper <- vupper
    | Clevel (v, level) -> v.level <- level
    | Cgencopy (v, copy) -> v.gencopy <- copy
    | Cexact (v, exact) -> v.exact <- exact
    | Cvars previous -> vars := previous

  let creation_log : (changes ref -> unit) ref = ref (fun _ -> ())

  let set_creation_log f = creation_log := f

  let empty_changes = []

  let undo_changes l = List.iter undo_change l

  (** [append_changes l0 l1] returns a log that's equivalent to [l0] followed by
      [l1]. *)
  let append_changes l0 l1 = l1 @ l0

  type copy_change =
    | Coptcopy : 'a C.obj * int * 'a var * 'a var option -> copy_change

  type copy_scope =
    { mutable saved_copies : copy_change list;
      mutable copied_exact : (exact_component * exact_component) list
    }

  type ('a, 'd) mode =
    | Amode :
        'a * ('a, 'l * _) Comp_hint.t * ('a, _ * 'r) Comp_hint.t
        -> ('a, 'l * 'r) mode
    | Amodevar : ('a, 'd) morphvar -> ('a, 'd) mode
    | Amodejoin :
        'a
        * ('a, 'l * disallowed) Comp_hint.t
        * ('a, 'l * disallowed) morphvar VarMap.t
        -> ('a, 'l * disallowed) mode
        (** [Amodejoin a c [mv0, mv1, ..]] represents
            [a join mv0 join mv1 join ..] with the hint [c] for [a] (the
            morphvars have their own hints). *)
    | Amodemeet :
        'a
        * ('a, disallowed * 'r) Comp_hint.t
        * ('a, disallowed * 'r) morphvar VarMap.t
        -> ('a, disallowed * 'r) mode
        (** [Amodemeet a c [mv0, mv1, ..]] represents
            [a meet mv0 meet mv1 meet ..] with the hint [c] for [a] (the
            morphvars have their own hints). *)
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  (** Levels *)
  let generic_level = Ident.highest_scope

  let rigid_level = generic_level - 1

  let active_scope = ref None

  let next_scope_order = ref 2

  let quantifier_of_var v =
    match v.quantifier with
    | Some quantifier -> quantifier
    | None ->
      let order = Option.value v.scope_order ~default:0 in
      if v.level = rigid_level
      then Universal order
      else if v.level > rigid_level
      then Existential (order + 1)
      else Outer

  let fatal_if_rigid mutation v =
    if v.level = rigid_level
    then
      Misc.fatal_errorf "Solver: attempted to %s rigid mode variable %x"
        mutation v.id

  (** Prints a mode variable, including the set of variables related to it
      (recursively). To handle cycles, [traversed] is the set of variables that
      we have already printed and will be skipped. An example of cycle:

      Consider a lattice A containing three elements 0, 1, and 2 with the linear
      lattice structure: 0 < 1 < 2. Furthermore, we define a morphism
      {v
      f : A -> A
      f 0 = 0
      f 1 = 2
      f 2 = 2
      v}

      Note that f has a left adjoint, which allows us to write f on the LHS of
      submode. Say we create a unconstrained variable [x], and invoke submode:
      [f x <= x] this would result in adding (f, x) into the [vlower] of [x].
      That is, there will be a self-loop on [x]. *)
  let rec print_var : type a. ?traversed:VarSet.t -> a C.obj -> _ -> a var -> _
      =
   fun ?traversed obj ppf v ->
    Fmt.fprintf ppf "modevar#%x<%x>[%a .. %a]" v.id v.level (C.print obj)
      v.lower (C.print obj) v.upper;
    match traversed with
    | None -> ()
    | Some traversed ->
      if VarSet.mem v.id traversed
      then ()
      else if (not (VarMap.is_empty v.vlower)) || not (VarMap.is_empty v.vupper)
      then begin
        let traversed = VarSet.add v.id traversed in
        Fmt.fprintf ppf "{%a--%a}"
          (Fmt.pp_print_list (print_morphvar ~traversed obj))
          (var_map_to_list v.vlower)
          (Fmt.pp_print_list (print_morphvar ~traversed obj))
          (var_map_to_list v.vupper)
      end

  and print_morphvar : type a l r.
      ?traversed:VarSet.t -> a C.obj -> _ -> (a, l * r) morphvar -> _ =
   fun ?traversed dst ppf (Amorphvar (v, f, _)) ->
    let src = C.src dst f in
    Fmt.fprintf ppf "%a(%a)" (C.print_morph dst) f (print_var ?traversed src) v

  let print_raw : type a l r.
      ?verbose:bool -> a C.obj -> Fmt.formatter -> (a, l * r) mode -> unit =
   fun ?(verbose = false) (obj : a C.obj) ppf m ->
    let traversed = if verbose then Some VarSet.empty else None in
    match m with
    | Amode (a, _, _) -> C.print obj ppf a
    | Amodevar mv -> print_morphvar ?traversed obj ppf mv
    | Amodejoin (a, _, mvs) ->
      Fmt.fprintf ppf "join(%a,%a)" (C.print obj) a
        (Fmt.pp_print_list
           ~pp_sep:(fun ppf () -> Fmt.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        (var_map_to_list mvs)
    | Amodemeet (a, _, mvs) ->
      Fmt.fprintf ppf "meet(%a,%a)" (C.print obj) a
        (Fmt.pp_print_list
           ~pp_sep:(fun ppf () -> Fmt.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        (var_map_to_list mvs)

  module Morphvar = Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) morphvar constraint 'd = 'l * 'r

    let allow_left : type a l r.
        (a, allowed * r) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m, h) ->
        Amorphvar (v, C.allow_left m, Comp_hint.Morph_hint.allow_left h)

    let allow_right : type a l r.
        (a, l * allowed) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m, h) ->
        Amorphvar (v, C.allow_right m, Comp_hint.Morph_hint.allow_right h)

    let disallow_left : type a l r.
        (a, l * r) morphvar -> (a, disallowed * r) morphvar = function
      | Amorphvar (v, m, h) ->
        Amorphvar (v, C.disallow_left m, Comp_hint.Morph_hint.disallow_left h)

    let disallow_right : type a l r.
        (a, l * r) morphvar -> (a, l * disallowed) morphvar = function
      | Amorphvar (v, m, h) ->
        Amorphvar (v, C.disallow_right m, Comp_hint.Morph_hint.disallow_right h)
  end)

  include Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) mode constraint 'd = 'l * 'r

    let allow_left : type a l r. (a, allowed * r) mode -> (a, l * r) mode =
      function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, Comp_hint.allow_left h_lower, h_upper)
      | Amodevar mv -> Amodevar (Morphvar.allow_left mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin (c, Comp_hint.allow_left h, VarMap.map Morphvar.allow_left mvs)

    let allow_right : type a l r. (a, l * allowed) mode -> (a, l * r) mode =
      function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, h_lower, Comp_hint.allow_right h_upper)
      | Amodevar mv -> Amodevar (Morphvar.allow_right mv)
      | Amodemeet (c, h, mvs) ->
        Amodemeet
          (c, Comp_hint.allow_right h, VarMap.map Morphvar.allow_right mvs)

    let disallow_left : type a l r. (a, l * r) mode -> (a, disallowed * r) mode
        = function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, Comp_hint.disallow_left h_lower, h_upper)
      | Amodevar mv -> Amodevar (Morphvar.disallow_left mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin
          (c, Comp_hint.disallow_left h, VarMap.map Morphvar.disallow_left mvs)
      | Amodemeet (c, h, mvs) ->
        Amodemeet (c, h, VarMap.map Morphvar.disallow_left mvs)

    let disallow_right : type a l r. (a, l * r) mode -> (a, l * disallowed) mode
        = function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, h_lower, Comp_hint.disallow_right h_upper)
      | Amodevar mv -> Amodevar (Morphvar.disallow_right mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin (c, h, VarMap.map Morphvar.disallow_right mvs)
      | Amodemeet (c, h, mvs) ->
        Amodemeet
          (c, Comp_hint.disallow_right h, VarMap.map Morphvar.disallow_right mvs)
  end)

  let mlower dst (Amorphvar (var, morph, _hint)) = C.apply dst morph var.lower

  let mlower_hint (Amorphvar (var, _morph, hint)) : _ Comp_hint.t =
    Apply
      ( Comp_hint.Morph_hint.disallow_right hint,
        Comp_hint.disallow_right var.lower_hint )

  let mupper dst (Amorphvar (var, morph, _hint)) = C.apply dst morph var.upper

  let mupper_hint (Amorphvar (var, _morph, hint)) : _ Comp_hint.t =
    Apply
      ( Comp_hint.Morph_hint.disallow_left hint,
        Comp_hint.disallow_left var.upper_hint )

  let min (type a) (obj : a C.obj) =
    let c = C.min obj in
    Amode (c, Min, Unknown c)

  let max (type a) (obj : a C.obj) =
    let c = C.max obj in
    Amode (c, Unknown c, Max)

  let of_const _obj ?hint a =
    let hint : _ Comp_hint.t =
      match hint with None -> Unknown a | Some h -> Const (h, a)
    in
    Amode (a, hint, hint)

  let check_level_morphvar : type a l r. (a, l * r) morphvar -> int -> bool =
   fun (Amorphvar (v, _, _)) i -> v.level = i

  let check_const_or_level_0 : type a l r. (a, l * r) mode -> bool =
   fun m ->
    match m with
    | Amode _ -> true
    | Amodevar mv -> check_level_morphvar mv 0
    | Amodemeet (_, _, mvs) ->
      VarMap.for_all (fun _ mv -> check_level_morphvar mv 0) mvs
    | Amodejoin (_, _, mvs) ->
      VarMap.for_all (fun _ mv -> check_level_morphvar mv 0) mvs

  let check_generic : type a l r. (a, l * r) mode -> bool =
   fun m ->
    match m with
    | Amode _ -> false
    | Amodevar mv -> check_level_morphvar mv generic_level
    | Amodemeet (_, _, mvs) ->
      VarMap.exists (fun _ mv -> check_level_morphvar mv generic_level) mvs
    | Amodejoin (_, _, mvs) ->
      VarMap.exists (fun _ mv -> check_level_morphvar mv generic_level) mvs

  type var_iterator =
    { iter : 'a. 'a C.obj -> ('a, allowed * allowed) mode -> unit }
  [@@unboxed]

  let mode_iter : type a l r. a C.obj -> (a, l * r) mode -> var_iterator -> unit
      =
   fun dst m { iter } ->
    let iter_morphvar : type l' r'. (a, l' * r') morphvar -> unit =
     fun (Amorphvar (v, f, _f_hint)) ->
      let src = C.src dst f in
      iter src (Amodevar (Amorphvar (v, C.id, Id)))
    in
    match m with
    | Amode _ -> ()
    | Amodevar mv -> iter_morphvar mv
    | Amodejoin (_, _, mvs) -> VarMap.iter (fun _ mv -> iter_morphvar mv) mvs
    | Amodemeet (_, _, mvs) -> VarMap.iter (fun _ mv -> iter_morphvar mv) mvs

  type 'b packed_morph =
    | Packed_morph : ('a, 'b, 'd) C.morph -> 'b packed_morph

  let rec iter_covariant_morphvar : type a r.
      visited:(int, unit) Hashtbl.t ->
      a C.obj ->
      (id:int ->
      level:int ->
      morph:a packed_morph ->
      (a, allowed * disallowed) mode ->
      unit) ->
      (a, allowed * r) morphvar ->
      unit =
   fun ~visited dst iter (Amorphvar (v, f, f_hint)) ->
    if Hashtbl.mem visited v.id
    then ()
    else begin
      Hashtbl.add visited v.id ();
      VarMap.iter
        (fun _ (Amorphvar (u, g, g_hint)) ->
          let fg = C.compose dst (C.disallow_right f) g in
          let fg_hint =
            Comp_hint.Morph_hint.Compose
              (Comp_hint.Morph_hint.disallow_right f_hint, g_hint)
          in
          let mu = Amorphvar (u, fg, fg_hint) in
          iter ~id:u.id ~level:u.level ~morph:(Packed_morph fg) (Amodevar mu);
          iter_covariant_morphvar ~visited dst iter mu)
        v.vlower
    end

  let iter_covariant : type a r.
      a C.obj ->
      (a, allowed * r) mode ->
      (id:int ->
      level:int ->
      morph:a packed_morph ->
      (a, allowed * disallowed) mode ->
      unit) ->
      unit =
   fun dst m iter ->
    match m with
    | Amode _ -> ()
    | Amodevar mv ->
      let visited = Hashtbl.create 17 in
      iter_covariant_morphvar ~visited dst iter mv
    | Amodejoin (_, _, mvs) ->
      let visited = Hashtbl.create 17 in
      VarMap.iter (fun _ mv -> iter_covariant_morphvar ~visited dst iter mv) mvs

  let rec iter_contravariant_morphvar : type a l.
      visited:(int, unit) Hashtbl.t ->
      a C.obj ->
      (id:int ->
      level:int ->
      morph:a packed_morph ->
      (a, disallowed * allowed) mode ->
      unit) ->
      (a, l * allowed) morphvar ->
      unit =
   fun ~visited dst iter (Amorphvar (v, f, f_hint)) ->
    if Hashtbl.mem visited v.id
    then ()
    else begin
      Hashtbl.add visited v.id ();
      VarMap.iter
        (fun _ (Amorphvar (u, g, g_hint)) ->
          let fg = C.compose dst (C.disallow_left f) g in
          let fg_hint =
            Comp_hint.Morph_hint.Compose
              (Comp_hint.Morph_hint.disallow_left f_hint, g_hint)
          in
          let mu = Amorphvar (u, fg, fg_hint) in
          iter ~id:u.id ~level:u.level ~morph:(Packed_morph fg) (Amodevar mu);
          iter_contravariant_morphvar ~visited dst iter mu)
        v.vupper
    end

  let iter_contravariant : type a l.
      a C.obj ->
      (a, l * allowed) mode ->
      (id:int ->
      level:int ->
      morph:a packed_morph ->
      (a, disallowed * allowed) mode ->
      unit) ->
      unit =
   fun dst m iter ->
    match m with
    | Amode _ -> ()
    | Amodevar mv ->
      let visited = Hashtbl.create 17 in
      iter_contravariant_morphvar ~visited dst iter mv
    | Amodemeet (_, _, mvs) ->
      let visited = Hashtbl.create 17 in
      VarMap.iter
        (fun _ mv -> iter_contravariant_morphvar ~visited dst iter mv)
        mvs

  let apply_morphvar dst morph morph_hint (Amorphvar (var, morph', morph'_hint))
      =
    Amorphvar
      ( var,
        C.compose dst morph morph',
        Comp_hint.Morph_hint.compose morph_hint morph'_hint )

  let apply : type a b l r.
      b C.obj ->
      ?hint:(l * r) H.Morph.t ->
      (a, b, l * r) C.morph ->
      (a, l * r) mode ->
      (b, l * r) mode =
   fun dst ?(hint = H.Morph.unknown) morph m ->
    let hint = Comp_hint.Morph_hint.Base (hint, morph) in
    match m with
    | Amode (a, a_hint_lower, a_hint_upper) ->
      Amode
        ( C.apply dst morph a,
          Apply
            ( Comp_hint.Morph_hint.disallow_right hint,
              Comp_hint.disallow_right a_hint_lower ),
          Apply
            ( Comp_hint.Morph_hint.disallow_left hint,
              Comp_hint.disallow_left a_hint_upper ) )
    | Amodevar mv -> Amodevar (apply_morphvar dst morph hint mv)
    | Amodejoin (a, a_hint, vs) ->
      let hint = Comp_hint.Morph_hint.disallow_right hint in
      let vs =
        VarMap.fold
          (fun _ mv acc ->
            let mv = apply_morphvar dst morph hint mv in
            VarMap.add (get_key dst mv) mv acc)
          vs VarMap.empty
      in
      Amodejoin (C.apply dst morph a, Apply (hint, a_hint), vs)
    | Amodemeet (a, a_hint, vs) ->
      let hint = Comp_hint.Morph_hint.disallow_left hint in
      let vs =
        VarMap.fold
          (fun _ mv acc ->
            let mv = apply_morphvar dst morph hint mv in
            VarMap.add (get_key dst mv) mv acc)
          vs VarMap.empty
      in
      Amodemeet (C.apply dst morph a, Apply (hint, a_hint), vs)

  module Unhint = struct
    type ('a, 'd) t =
      | Unhint : ('b, 'a, 'd) C.morph * ('b, 'd) mode -> ('a, 'd) t
      constraint 'd = 'l * 'r
    [@@ocaml.warning "-62"]

    let unhint : type a l r. (a, l * r) mode -> (a, l * r) t =
     fun m -> Unhint (C.id, m)

    let hint : type a l r.
        a C.obj -> ?hint:(l * r) H.Morph.t -> (a, l * r) t -> (a, l * r) mode =
     fun dst ?hint (Unhint (morph, m)) -> apply dst ?hint morph m

    let apply : type a b l r.
        b C.obj -> (a, b, l * r) C.morph -> (a, l * r) t -> (b, l * r) t =
     fun dst morph (Unhint (f, m)) -> Unhint (C.compose dst morph f, m)
  end

  let hint_biased_join obj a a_hint b b_hint =
    (* A version of [hint_join] that assumes that a <= b is false,
       so won't perform this check. *)
    if C.le obj b a then a_hint else Comp_hint.Branch (Join, a_hint, b_hint)

  let hint_join obj a a_hint b b_hint =
    (* This just provides a minor optimization to avoid having unnecessarily-nested Branch hints.
       When a join, [join a b], is performed, the hint we use for the result should be a [Branch]
       if both arguments are relevant, but if we have that [a <= b] in the lattice structure then
       we don't really need to consider the hint for [a], and vice versa for [b]'s hint. *)
    if C.le obj a b then b_hint else hint_biased_join obj a a_hint b b_hint

  let hint_biased_meet obj a a_hint b b_hint =
    (* A version of [hint_meet] that assumes that a <= b is false,
       so won't perform this check. *)
    if C.le obj b a then b_hint else Comp_hint.Branch (Meet, a_hint, b_hint)

  let hint_meet obj a a_hint b b_hint =
    (* Comments for [hint_join] apply here similarly but the other way round
       (since this is for meet, not join) *)
    if C.le obj a b then a_hint else hint_biased_meet obj a a_hint b b_hint

  (** Calling [update_lower ~log obj v a a_hint] assumes that
      [not (a <= v.lower)]. Arguments are not checked and used directly. They
      must satisfy the INVARIANT listed above. *)
  let update_lower (type a) ~allow_rigid ~log (obj : a C.obj) v a a_hint =
    if not allow_rigid then fatal_if_rigid "update the lower bound of" v;
    (match log with
    | None -> ()
    | Some log -> log := Clower (v, v.lower, v.lower_hint) :: !log);
    v.lower_hint <- hint_biased_join obj a a_hint v.lower v.lower_hint;
    v.lower <- C.join obj v.lower a

  (** Calling [update_upper ~log obj v a a_hint] assumes that
      [not (v.upper <= a)]. Arguments are not checked and used directly. They
      must satisfy the INVARIANT listed above. *)
  let update_upper (type a) ~allow_rigid ~log (obj : a C.obj) v a a_hint =
    if not allow_rigid then fatal_if_rigid "update the upper bound of" v;
    (match log with
    | None -> ()
    | Some log -> log := Cupper (v, v.upper, v.upper_hint) :: !log);
    v.upper_hint <- hint_biased_meet obj v.upper v.upper_hint a a_hint;
    v.upper <- C.meet obj v.upper a

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_vlower ~allow_rigid ~log v vlower =
    if not allow_rigid
    then fatal_if_rigid "update lower variable constraints of" v;
    (match log with
    | None -> ()
    | Some log -> log := Cvlower (v, v.vlower) :: !log);
    v.vlower <- vlower

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_vupper ~allow_rigid ~log v vupper =
    if not allow_rigid
    then fatal_if_rigid "update upper variable constraints of" v;
    (match log with
    | None -> ()
    | Some log -> log := Cvupper (v, v.vupper) :: !log);
    v.vupper <- vupper

  (** Function used internally by copy, where [changes] maintains the cleanup
      once the copy is done. Unlike other setters, the [changes] is here
      mandatory *)
  let set_optcopy ~changes obj ~target_level v copy =
    changes.saved_copies
      <- Coptcopy (obj, target_level, v, v.subst) :: changes.saved_copies;
    v.subst <- copy

  (** Function used internally by generalize_structure to cache newly created
      copies *)
  let set_gencopy ~log v copy =
    fatal_if_rigid "update generic copy cache of" v;
    (match log with
    | None -> ()
    | Some log -> log := Cgencopy (v, v.gencopy) :: !log);
    v.gencopy <- copy

  (** When called, graph must be fixed so maintain INVARIANT *)
  let set_level ~allow_rigid ~log v level =
    if not allow_rigid then fatal_if_rigid "update the level of" v;
    (match log with
    | None -> ()
    | Some log -> log := Clevel (v, v.level) :: !log);
    v.level <- level

  (** Returns the lower bound of [v] implied by the constraint graph: the join
      of [v.lower] and the lower bounds of all variables reachable through
      [vlower] edges. The traversal stops as soon as the accumulated bound is
      above [stop]. *)
  let floor_reachable_morphvar : type b l r.
      b C.obj -> stop:b -> (b, l * r) morphvar -> b =
   fun dst ~stop (Amorphvar (v, m, _)) ->
    let src = C.src dst m in
    let rec search (Amorphvar (u, f, f_hint) as mv) ((lower, mvs) as acc) =
      if C.le dst stop (C.apply dst m lower)
      then acc
      else
        let key = get_key src mv in
        if VarMap.mem key mvs
        then acc
        else
          let lower = C.join src lower (mlower src mv) in
          let mvs = VarMap.add key mv mvs in
          VarMap.fold
            (fun _ (Amorphvar (w, g, g_hint)) acc ->
              let fg = C.compose src f g in
              let fg_hint = Comp_hint.Morph_hint.Compose (f_hint, g_hint) in
              search (Amorphvar (w, fg, fg_hint)) acc)
            u.vlower (lower, mvs)
    in
    let lower, _ =
      VarMap.fold
        (fun _ mv acc -> search mv acc)
        v.vlower (v.lower, VarMap.empty)
    in
    C.apply dst m lower

  let rigid_submode_cv : type a.
      a C.obj -> a -> a var -> (unit, a * (a, right_only) Comp_hint.t) Result.t
      =
   fun obj a v ->
    let lower =
      floor_reachable_morphvar obj ~stop:a
        (Amorphvar (v, C.id, Comp_hint.Morph_hint.Id))
    in
    if C.le obj a lower then Ok () else Error (lower, Comp_hint.Unknown lower)

  (** Returns the upper bound of [v] implied by the constraint graph: the meet
      of [v.upper] and the upper bounds of all variables reachable through
      [vupper] edges. The traversal stops as soon as the accumulated bound is
      below [stop]. *)
  let ceil_reachable_morphvar : type b l r.
      b C.obj -> stop:b -> (b, l * r) morphvar -> b =
   fun dst ~stop (Amorphvar (v, m, _)) ->
    let src = C.src dst m in
    let rec search (Amorphvar (u, f, f_hint) as mv) ((upper, mvs) as acc) =
      if C.le dst (C.apply dst m upper) stop
      then acc
      else
        let key = get_key src mv in
        if VarMap.mem key mvs
        then acc
        else
          let upper = C.meet src upper (mupper src mv) in
          let mvs = VarMap.add key mv mvs in
          VarMap.fold
            (fun _ (Amorphvar (w, g, g_hint)) acc ->
              let fg = C.compose src f g in
              let fg_hint = Comp_hint.Morph_hint.Compose (f_hint, g_hint) in
              search (Amorphvar (w, fg, fg_hint)) acc)
            u.vupper (upper, mvs)
    in
    let upper, _ =
      VarMap.fold
        (fun _ mv acc -> search mv acc)
        v.vupper (v.upper, VarMap.empty)
    in
    C.apply dst m upper

  let rigid_submode_vc : type a.
      a C.obj -> a var -> a -> (unit, a * (a, left_only) Comp_hint.t) Result.t =
   fun obj v a ->
    let upper =
      ceil_reachable_morphvar obj ~stop:a
        (Amorphvar (v, C.id, Comp_hint.Morph_hint.Id))
    in
    if C.le obj upper a then Ok () else Error (upper, Comp_hint.Unknown upper)

  (** Returns [Ok ()] if success; [Error x] if failed, and [x] is the next best
      (read: strictly lower) guess to replace the constant argument that MIGHT
      succeed. *)
  let rec submode_cv : type a.
      allow_rigid:bool ->
      log:_ ->
      H.Pinpoint.t ->
      a C.obj ->
      a ->
      (a, left_only) Comp_hint.t ->
      a var ->
      (unit, a * (a, right_only) Comp_hint.t) Result.t =
   fun (type a) ~allow_rigid ~log pp (obj : a C.obj) a' a'_hint v ->
    if C.le obj a' v.lower
    then Ok ()
    else if not (C.le obj a' v.upper)
    then Error (v.upper, v.upper_hint)
    else if (not allow_rigid) && v.level = rigid_level
    then rigid_submode_cv obj a' v
    else (
      update_lower ~allow_rigid ~log obj v a' a'_hint;
      let r =
        v.vupper
        |> find_error (fun mu ->
            let r = submode_cmv ~allow_rigid ~log pp obj a' a'_hint mu in
            (if Result.is_ok r
             then
               (* Optimization: update [v.upper] based on [mupper u].*)
               let mu_upper = mupper obj mu in
               if not (C.le obj v.upper mu_upper)
               then
                 update_upper ~allow_rigid ~log obj v mu_upper (mupper_hint mu));
            r)
      in
      r)

  and submode_cmv : type a l.
      allow_rigid:bool ->
      log:_ ->
      H.Pinpoint.t ->
      a C.obj ->
      a ->
      (a, left_only) Comp_hint.t ->
      (a, l * allowed) morphvar ->
      (unit, a * (a, right_only) Comp_hint.t) Result.t =
   fun ~allow_rigid ~log pp obj a a_hint (Amorphvar (v, f, f_hint) as mv) ->
    let mlower = mlower obj mv in
    let mupper = mupper obj mv in
    let mupper_hint = mupper_hint mv in
    if C.le obj a mlower
    then Ok ()
    else if not (C.le obj a mupper)
    then Error (mupper, mupper_hint)
    else
      (* At this point we know [a <= f v], therefore [a] is in the downward
         closure of [f]'s image. Therefore, asking [a <= f v] is equivalent to
         asking [f' a <= v]. *)
      let f' = C.left_adjoint obj f in
      let src_pp, src, f'_hint =
        Comp_hint.Morph_hint.left_adjoint pp obj f_hint
      in
      let a' = C.apply src f' a in
      let a'_hint = Comp_hint.Apply (f'_hint, a_hint) in
      match submode_cv ~allow_rigid ~log src_pp src a' a'_hint v with
      | Ok () -> Ok ()
      | Error (e, e_hint) ->
        Error
          ( C.apply obj f e,
            Comp_hint.Apply (Comp_hint.Morph_hint.disallow_left f_hint, e_hint)
          )

  (** Returns [Ok ()] if success; [Error x] if failed, and [x] is the next best
      (read: strictly higher) guess to replace the constant argument that MIGHT
      succeed. *)
  let rec submode_vc : type a.
      allow_rigid:bool ->
      log:_ ->
      H.Pinpoint.t ->
      a C.obj ->
      a var ->
      a ->
      (a, right_only) Comp_hint.t ->
      (unit, a * (a, left_only) Comp_hint.t) Result.t =
   fun (type a) ~allow_rigid ~log pp (obj : a C.obj) v a' a'_hint ->
    if C.le obj v.upper a'
    then Ok ()
    else if not (C.le obj v.lower a')
    then Error (v.lower, v.lower_hint)
    else if (not allow_rigid) && v.level = rigid_level
    then rigid_submode_vc obj v a'
    else (
      update_upper ~allow_rigid ~log obj v a' a'_hint;
      let r =
        v.vlower
        |> find_error (fun mu ->
            let r = submode_mvc ~allow_rigid ~log pp obj mu a' a'_hint in
            (if Result.is_ok r
             then
               (* Optimization: update [v.lower] based on [mlower u].*)
               let mu_lower = mlower obj mu in
               let mu_lower_hint = mlower_hint mu in
               if not (C.le obj mu_lower v.lower)
               then update_lower ~allow_rigid ~log obj v mu_lower mu_lower_hint);
            r)
      in
      r)

  and submode_mvc :
      'a 'r.
      allow_rigid:bool ->
      log:change list ref option ->
      H.Pinpoint.t ->
      'a C.obj ->
      ('a, allowed * 'r) morphvar ->
      'a ->
      ('a, right_only) Comp_hint.t ->
      (unit, 'a * ('a, left_only) Comp_hint.t) Result.t =
   fun ~allow_rigid ~log pp obj (Amorphvar (v, f, f_hint) as mv) a a_hint ->
    (* See [submode_cmv] for why we need the following seemingly redundant
       lines. *)
    let mupper = mupper obj mv in
    let mlower = mlower obj mv in
    let mlower_hint = mlower_hint mv in
    if C.le obj mupper a
    then Ok ()
    else if not (C.le obj mlower a)
    then Error (mlower, mlower_hint)
    else
      let f' = C.right_adjoint obj f in
      let src_pp, src, f'_hint =
        Comp_hint.Morph_hint.right_adjoint pp obj f_hint
      in
      let a' = C.apply src f' a in
      let a'_hint = Comp_hint.Apply (f'_hint, a_hint) in
      (* If [mlower] was precise, then the check
         [not (C.le obj (mlower obj mv) a)] should guarantee the following call
         to return [Ok ()]. However, [mlower] is not precise *)
      (* not using [Result.map_error] to avoid allocating closure *)
      match submode_vc ~allow_rigid ~log src_pp src v a' a'_hint with
      | Ok () -> Ok ()
      | Error (e, e_hint) ->
        Error
          ( C.apply obj f e,
            Apply (Comp_hint.Morph_hint.disallow_right f_hint, e_hint) )

  (** Zap [mv] to its lower bound. Returns the [log] of the zapping, in case the
      caller are only interested in the lower bound and wants to reverse the
      zapping.

      As mentioned in [var], [mlower mv] is not precise; to get the precise
      lower bound of [mv], we call [submode mv (mlower mv)]. This will propagate
      to all its children, which might fail because some children's lower bound
      [a] is more up-to-date than [mv]. In that case, we call [submode mv a]. We
      repeat this process until no failure, and we will get the precise lower
      bound.

      The loop is guaranteed to terminate, because for each iteration our
      guessed lower bound is strictly higher; and all lattices are finite. *)
  let zap_to_floor_morphvar_aux (type a r) (obj : a C.obj)
      (mv : (a, allowed * r) morphvar) =
    let rec loop lower =
      let log = ref empty_changes in
      (* We want a hint for why [lower] is low, but we only have hint for why [lower] is
         high. There is no good hint to use. *)
      let r =
        submode_mvc ~allow_rigid:true ~log:(Some log) H.Pinpoint.unknown obj mv
          lower (Unknown lower)
      in
      match r with
      | Ok () -> !log, lower
      | Error (a, _) ->
        undo_changes !log;
        loop (C.join obj a lower)
    in
    loop (mlower obj mv)

  (** Zap [mv] to its upper bound. Returns the [log] of the zapping, in case the
      caller are only interested in the lower bound and wants to reverse the
      zapping.

      [mupper mv] is not precise; to get the precise upper bound of [mv], we
      call [submode (mupper mv) mv ]. This will propagate to all its children,
      which might fail because some children's upper bound [a] is more
      up-to-date than [mv]. In that case, we call [submode a mv]. We repeat this
      process until no failure, and we will get the precise lower bound.

      The loop is guaranteed to terminate, because for each iteration our
      guessed lower bound is strictly higher; and all lattices are finite. *)
  let zap_to_ceil_morphvar_aux (type a l) (obj : a C.obj)
      (mv : (a, l * allowed) morphvar) =
    let rec loop upper =
      let log = ref empty_changes in
      let r =
        submode_cmv ~allow_rigid:true ~log:(Some log) H.Pinpoint.unknown obj
          upper (Unknown upper) mv
      in
      match r with
      | Ok () -> !log, upper
      | Error (a, _) ->
        undo_changes !log;
        loop (C.meet obj a upper)
    in
    loop (mupper obj mv)

  let eq_morphvar : type a l0 r0 l1 r1.
      a C.obj -> (a, l0 * r0) morphvar -> (a, l1 * r1) morphvar -> bool =
   fun dst (Amorphvar (v0, f0, _f0_hint) as mv0)
       (Amorphvar (v1, f1, _f1_hint) as mv1) ->
    (* To align l0/l1, r0/r1; The existing disallow_left/right] is for [mode],
       not [morphvar]. *)
    if v0.id <> v1.id
    then false
    else if
      Morphvar.(
        disallow_left (disallow_right mv0) == disallow_left (disallow_right mv1))
    then true
    else
      match C.equal_morph dst f0 f1 with
      | Misc.Is_not_eq -> false
      | Misc.Is_eq -> true

  (** Check [f v <= g u] at each feasible value of the left rigid variable.
      Fixing [v] before finding the floor of [u] preserves their dependency. *)
  let submode_mvmv_rigid_both : type a b c l r.
      log:_ ->
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      (a, b, allowed * r) C.morph ->
      (a, b, allowed * r) Comp_hint.Morph_hint.t ->
      c var ->
      (c, b, l * allowed) C.morph ->
      (c, b, l * allowed) Comp_hint.Morph_hint.t ->
      (_, b * (b, _) Comp_hint.t * b * (b, _) Comp_hint.t) result =
   fun ~log:_ _pp dst v f _f_hint u g _g_hint ->
    let src_f = C.src dst f in
    let src_g = C.src dst g in
    let rec check = function
      | [] -> Ok ()
      | value :: rest ->
        let slice_log = ref empty_changes in
        let lower_result =
          submode_cmv ~allow_rigid:true ~log:(Some slice_log) H.Pinpoint.unknown
            src_f value (Comp_hint.Unknown value)
            (Amorphvar (v, C.id, Comp_hint.Morph_hint.Id))
        in
        let feasible =
          match lower_result with
          | Error _ -> false
          | Ok () ->
            Result.is_ok
              (submode_mvc ~allow_rigid:true ~log:(Some slice_log)
                 H.Pinpoint.unknown src_f
                 (Amorphvar (v, C.id, Comp_hint.Morph_hint.Id))
                 value (Comp_hint.Unknown value))
        in
        if not feasible
        then begin
          undo_changes !slice_log;
          check rest
        end
        else begin
          let floor_log, floor =
            zap_to_floor_morphvar_aux src_g
              (Amorphvar (u, C.id, Comp_hint.Morph_hint.Id))
          in
          undo_changes floor_log;
          undo_changes !slice_log;
          let left = C.apply dst f value in
          let right = C.apply dst g floor in
          if C.le dst left right
          then check rest
          else
            Error (left, Comp_hint.Unknown left, right, Comp_hint.Unknown right)
        end
    in
    check (C.elements src_f)

  (** Handles [f v <= g u] where [u] is rigid but [v] is not. We cannot add the
      arrow [g' (f v)] to [u.vlower]; instead we enforce the constraint for
      every valuation of [u], namely [f v <= g (floor u)], by pushing
      [g (floor u)] onto [v] as an upper bound. *)
  let submode_mvmv_rigid_right : type a b c l r.
      log:_ ->
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      (a, b, allowed * r) C.morph ->
      (a, b, allowed * r) Comp_hint.Morph_hint.t ->
      c var ->
      (c, b, l * allowed) C.morph ->
      (c, b, l * allowed) Comp_hint.Morph_hint.t ->
      (_, b * (b, _) Comp_hint.t * b * (b, _) Comp_hint.t) result =
   fun ~log pp dst v f f_hint u g g_hint ->
    let floor =
      floor_reachable_morphvar dst ~stop:(C.max dst) (Amorphvar (u, g, g_hint))
    in
    match
      submode_mvc ~allow_rigid:false ~log pp dst
        (Amorphvar (v, f, f_hint))
        floor (Comp_hint.Unknown floor)
    with
    | Ok () -> Ok ()
    | Error (a, a_hint) -> Error (a, a_hint, floor, Comp_hint.Unknown floor)

  (** Handles [f v <= g u] where [v] is rigid but [u] is not. We cannot add the
      arrow [f' (g u)] to [v.vupper]; instead we enforce the constraint for
      every valuation of [v], namely [f (ceil v) <= g u], by pushing
      [f (ceil v)] onto [u] as a lower bound. *)
  let submode_mvmv_rigid_left : type a b c l r.
      log:_ ->
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      (a, b, allowed * r) C.morph ->
      (a, b, allowed * r) Comp_hint.Morph_hint.t ->
      c var ->
      (c, b, l * allowed) C.morph ->
      (c, b, l * allowed) Comp_hint.Morph_hint.t ->
      (_, b * (b, _) Comp_hint.t * b * (b, _) Comp_hint.t) result =
   fun ~log pp dst v f f_hint u g g_hint ->
    let ceil =
      ceil_reachable_morphvar dst ~stop:(C.min dst) (Amorphvar (v, f, f_hint))
    in
    match
      submode_cmv ~allow_rigid:false ~log pp dst ceil (Comp_hint.Unknown ceil)
        (Amorphvar (u, g, g_hint))
    with
    | Ok () -> Ok ()
    | Error (a, a_hint) -> Error (ceil, Comp_hint.Unknown ceil, a, a_hint)

  (** Computes the morphism [g'f] whose arrow [g'f v] [add_vlower] would record
      in [u.vlower] for the relationship [f v <= g u], along with its hint and
      whether the arrow is already recorded. *)
  let vlower_recorded : type a b c l r.
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      (a, b, allowed * r) C.morph ->
      (a, b, allowed * r) Comp_hint.Morph_hint.t ->
      c var ->
      (c, b, l * allowed) C.morph ->
      (c, b, l * allowed) Comp_hint.Morph_hint.t ->
      bool
      * (a, c, left_only) C.morph
      * (a, c, left_only) Comp_hint.Morph_hint.t =
   fun pp dst v f f_hint u g g_hint ->
    let g' = C.left_adjoint dst g in
    let _, src, g'_hint = Comp_hint.Morph_hint.left_adjoint pp dst g_hint in
    let g'f = C.compose src g' (C.disallow_right f) in
    let g'f_hint =
      Comp_hint.Morph_hint.compose g'_hint
        (Comp_hint.Morph_hint.disallow_right f_hint)
    in
    let key = get_key src (Amorphvar (v, g'f, g'f_hint)) in
    VarMap.mem key u.vlower, g'f, g'f_hint

  (** Computes the morphism [f'g] whose arrow [f'g u] [add_vupper] would record
      in [v.vupper] for the relationship [f v <= g u], along with its hint and
      whether the arrow is already recorded. *)
  let vupper_recorded : type a b c l r.
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      (a, b, allowed * r) C.morph ->
      (a, b, allowed * r) Comp_hint.Morph_hint.t ->
      c var ->
      (c, b, l * allowed) C.morph ->
      (c, b, l * allowed) Comp_hint.Morph_hint.t ->
      bool
      * (c, a, right_only) C.morph
      * (c, a, right_only) Comp_hint.Morph_hint.t =
   fun pp dst v f f_hint u g g_hint ->
    let f' = C.right_adjoint dst f in
    let _, src, f'_hint = Comp_hint.Morph_hint.right_adjoint pp dst f_hint in
    let f'g = C.compose src f' (C.disallow_left g) in
    let f'g_hint =
      Comp_hint.Morph_hint.compose f'_hint
        (Comp_hint.Morph_hint.disallow_left g_hint)
    in
    let key = get_key src (Amorphvar (u, f'g, f'g_hint)) in
    VarMap.mem key v.vupper, f'g, f'g_hint

  let rec submode_mvmv : type a l r.
      allow_rigid:bool ->
      log:_ ->
      H.Pinpoint.t ->
      a C.obj ->
      (a, allowed * r) morphvar ->
      (a, l * allowed) morphvar ->
      (_, a * (a, _) Comp_hint.t * a * (a, _) Comp_hint.t) result =
   fun ~allow_rigid ~log pp dst (Amorphvar (v, f, f_hint) as mv)
       (Amorphvar (u, g, g_hint) as mu) ->
    if C.le dst (mupper dst mv) (mlower dst mu)
    then Ok ()
    else if eq_morphvar dst mv mu
    then Ok ()
    else
      let muupper = mupper dst mu in
      let muupper_hint = mupper_hint mu in
      (* The call f v <= g u translates to the following step:
         1. f v <= g u.upper
         2. f v.lower <= g u
         3. If v.level <= u.level, for all (h w) in u.vupper, g' (f v) <= h w
         4. If v.level <= u.level adding g' (f v) to u.vlower, where g' is the left adjoint of g
         5. If v.level > u.level, for all (h w) in v.vlower, h w <= f' (g u)
         6. If v.level > u.level adding f' (g u) to v.vupper, where f' is the right adjoint of f
         Steps 3 and 4 are implemented by [add_vlower], steps 5 and 6 by [add_vupper].
      *)
      match submode_mvc ~allow_rigid ~log pp dst mv muupper muupper_hint with
      | Error (a, a_hint) -> Error (a, a_hint, muupper, muupper_hint)
      | Ok () -> (
        let mvlower = mlower dst mv in
        let mvlower_hint = mlower_hint mv in
        match submode_cmv ~allow_rigid ~log pp dst mvlower mvlower_hint mu with
        | Error (a, a_hint) -> Error (mvlower, mvlower_hint, a, a_hint)
        | Ok () ->
          if v.level <= u.level
          then begin
            let recorded, g'f, g'f_hint =
              vlower_recorded pp dst v f f_hint u g g_hint
            in
            if recorded
            then Ok ()
            else if (not allow_rigid) && u.level = rigid_level
            then
              begin if v.level = rigid_level
              then submode_mvmv_rigid_both ~log pp dst v f f_hint u g g_hint
              else submode_mvmv_rigid_right ~log pp dst v f f_hint u g g_hint
              end
            else
              add_vlower ~allow_rigid ~log pp dst v g'f g'f_hint mv u g g_hint
          end
          else begin
            let recorded, f'g, f'g_hint =
              vupper_recorded pp dst v f f_hint u g g_hint
            in
            if recorded
            then Ok ()
            else if v.level = rigid_level
            then submode_mvmv_rigid_left ~log pp dst v f f_hint u g g_hint
            else
              add_vupper ~allow_rigid ~log pp dst v f f_hint u f'g f'g_hint mu
          end)

  (* Record the arrow [g'f v] (i.e. [g' (f v)]) in [u.vlower] for the
     relationship [f v <= g u]. The caller is responsible for checking that
     the arrow is not already recorded. *)
  and add_vlower : type a b c l r.
      allow_rigid:bool ->
      log:_ ->
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      (a, c, left_only) C.morph ->
      (a, c, left_only) Comp_hint.Morph_hint.t ->
      (b, allowed * r) morphvar ->
      c var ->
      (c, b, l * allowed) C.morph ->
      (c, b, l * allowed) Comp_hint.Morph_hint.t ->
      (_, b * (b, _) Comp_hint.t * b * (b, _) Comp_hint.t) result =
   fun ~allow_rigid ~log pp dst v g'f g'f_hint mv u g g_hint ->
    let src = C.src dst g in
    let x = Amorphvar (v, g'f, g'f_hint) in
    let key = get_key src x in
    set_vlower ~allow_rigid ~log u (VarMap.add key x u.vlower);
    find_error
      (fun (Amorphvar (w, h, h_hint)) ->
        let gh = C.compose dst (C.disallow_left g) h in
        let gh_hint =
          Comp_hint.Morph_hint.compose
            (Comp_hint.Morph_hint.disallow_left g_hint)
            h_hint
        in
        let y = Amorphvar (w, gh, gh_hint) in
        submode_mvmv ~allow_rigid ~log pp dst mv y)
      u.vupper

  (* Record the arrow [f'g u] (i.e. [f' (g u)]) in [v.vupper] for the
     relationship [f v <= g u]. The caller is responsible for checking that
     the arrow is not already recorded. *)
  and add_vupper : type a b c l r.
      allow_rigid:bool ->
      log:_ ->
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      (a, b, allowed * r) C.morph ->
      (a, b, allowed * r) Comp_hint.Morph_hint.t ->
      c var ->
      (c, a, right_only) C.morph ->
      (c, a, right_only) Comp_hint.Morph_hint.t ->
      (b, l * allowed) morphvar ->
      (_, b * (b, _) Comp_hint.t * b * (b, _) Comp_hint.t) result =
   fun ~allow_rigid ~log pp dst v f f_hint u f'g f'g_hint mu ->
    let src = C.src dst f in
    let x = Amorphvar (u, f'g, f'g_hint) in
    let key = get_key src x in
    set_vupper ~allow_rigid ~log v (VarMap.add key x v.vupper);
    find_error
      (fun (Amorphvar (w, h, h_hint)) ->
        let fh = C.compose dst (C.disallow_right f) h in
        let fh_hint =
          Comp_hint.Morph_hint.compose
            (Comp_hint.Morph_hint.disallow_right f_hint)
            h_hint
        in
        let y = Amorphvar (w, fh, fh_hint) in
        submode_mvmv ~allow_rigid ~log pp dst y mu)
      v.vlower

  (* Tighten the lower bound of [u] based on the lower bound of [f' v].
  No recursion into [u.vuppers] *)
  let push_lower_bound : type a b r.
      allow_rigid:bool ->
      log:_ ->
      b C.obj ->
      a var ->
      (a, b, allowed * r) C.morph ->
      (a, b, allowed * r) Comp_hint.Morph_hint.t ->
      b var ->
      unit =
   fun ~allow_rigid ~log dst v f' f'_hint u ->
    let mv = Amorphvar (v, f', f'_hint) in
    let mlower = mlower dst mv in
    let mlower_hint = mlower_hint mv in
    if not (C.le dst mlower u.lower)
    then update_lower ~allow_rigid ~log dst u mlower mlower_hint

  (* Tighten the upper bound of [u] based on the upper bound of [f' v].
  No recursion into [u.vlowers] *)
  let push_upper_bound : type a b l.
      allow_rigid:bool ->
      log:_ ->
      b C.obj ->
      a var ->
      (a, b, l * allowed) C.morph ->
      (a, b, l * allowed) Comp_hint.Morph_hint.t ->
      b var ->
      unit =
   fun ~allow_rigid ~log dst v f' f'_hint u ->
    let mv = Amorphvar (v, f', f'_hint) in
    let mupper = mupper dst mv in
    let mupper_hint = mupper_hint mv in
    if not (C.le dst u.upper mupper)
    then update_upper ~allow_rigid ~log dst u mupper mupper_hint

  let add_vlower_nocheck : type a b r.
      allow_rigid:bool ->
      log:_ ->
      a C.obj ->
      a var ->
      b var ->
      (b, a, allowed * r) C.morph ->
      (b, a, allowed * r) Comp_hint.Morph_hint.t ->
      unit =
   fun ~allow_rigid ~log dst v u f f_hint ->
    let x =
      Amorphvar
        (u, C.disallow_right f, Comp_hint.Morph_hint.disallow_right f_hint)
    in
    if C.le dst (mupper dst x) v.lower
    then ()
    else
      let key = get_key dst x in
      if VarMap.mem key v.vlower
      then ()
      else set_vlower ~allow_rigid ~log v (VarMap.add key x v.vlower)

  let add_vupper_nocheck : type a b l.
      allow_rigid:bool ->
      log:_ ->
      a C.obj ->
      a var ->
      b var ->
      (b, a, l * allowed) C.morph ->
      (b, a, l * allowed) Comp_hint.Morph_hint.t ->
      unit =
   fun ~allow_rigid ~log dst v u f f_hint ->
    let x =
      Amorphvar (u, C.disallow_left f, Comp_hint.Morph_hint.disallow_left f_hint)
    in
    if C.le dst v.upper (mlower dst x)
    then ()
    else
      let key = get_key dst x in
      if VarMap.mem key v.vupper
      then ()
      else set_vupper ~allow_rigid ~log v (VarMap.add key x v.vupper)

  (* Proof of soundness for [add_vlower_reversed].

    [add_vlower_reversed] is not safe to call in general: its soundness
    depends on being called from [update_level_finalize].

    Let's recall the solver variable invariant, denoted INV(v):

     1) v.lower < v.upper
     2) ∀ (f, u) ∈ v.vlower: u.level ≤ v.level
     3) ∀ (f, u) ∈ v.vupper: u.level < v.level
     4) ∀ (f, u) ∈ v.vlower: f u.upper ≤ v.upper
     5) ∀ (f, u) ∈ v.vupper: v.lower ≤ f u.lower
     6) ∀ (f, u) ∈ v.vlower, (g, w) ∈ v.vupper:
       (g'f, u) ∈ w.vlower
     ∨ (f'g, w) ∈ u.vupper

    PRECONDITION: must be called from update_level_finalize where the
    following holds:
    1) u.level < v.level
    2) f v.upper < u.upper
    3) ∀ (g,w') ∈ u.vupper:
       w'.level < v.level ⇒ (f'g,w') ∈ v.vupper
       w'.level ≥ v.level ⇒ (g'f,v) ∈ w'.vlower

    ARGUMENT 1:
    We are tightening u.lower to f v.lower.
    PROOF OBLIGATION: ∀ (g,w’) ∈ u.vupper: f v.lower < g w’.lower

    By INV.3 we know that w’.level < u.level.
    By transitivity with P.1 we then know that w’level < v.level
    By P.3 we then know that (f’g,w’) ∈ v.vupper
    By INV.5 we then know that v.lower < f’g w’.lower
    By left adjoint we conclude: f v.lower < g w’.lower

    ARGUMENT 2:
    We are adding (fh,w) to u.vlower, (where `(h,w) ∈  without propagating
    or checking bounds.
    PROOF OBLIGATION:
       fh w.upper < u.upper
       ∀ (g,w’) ∈ u.vupper: (g’fh,w) ∈ w’.vlower ∨
                           (h’f’g,w’) ∈ w.vupper

    By P.2 we know that f v.upper < u.upper
    By INV.4 we know that h w.upper < v.upper
    By right adjoint we know that v.upper < f’ u.upper
    By transitivity we know that h w.upper < f’ u.upper
    By left adjoint we know that fh w.upper < u.upper

    The second proof obligation follows similarly.
    We know that (h,w) ∈ v.vlower.
    Introduce an arbitrary (g,w’) ∈ u.vupper.
    By P.3 and w’.level < v.level we know that (f’g,w’) ∈ v.vupper
    By P.6 we conclude our proof obligation holds.

  *)

  (* Add a vlower entry for the relation [f' u <= v], tighten the upper bound of [u],
  and recursively add relations to maintain invariant.
  The lower and upper bounds of [u] and [v] are not checked, upper bound is not pushed
  down [u.vlower] *)
  let rec add_vlower_reversed : type a b l.
      allow_rigid:bool ->
      log:_ ->
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      b var ->
      (a, b, l * allowed) C.morph ->
      (a, b, l * allowed) Comp_hint.Morph_hint.t ->
      unit =
   fun ~allow_rigid ~log pp dst v u f f_hint ->
    let f' = C.left_adjoint dst f in
    let src = C.src dst f in
    let f'_hint = Comp_hint.Morph_hint.Adjoint_l (f_hint, f') in
    let x = Amorphvar (u, f', f'_hint) in
    let key = get_key src x in
    if VarMap.mem key v.vlower
    then ()
    else begin
      push_upper_bound ~allow_rigid ~log dst v f f_hint u;
      (* We are tightening u.lower to f v.lower.
           This is sound by ARGUMENT 1 above *)
      set_vlower ~allow_rigid ~log v (VarMap.add key x v.vlower);
      VarMap.iter
        (fun _ (Amorphvar (w, h, h_hint)) ->
          let fh = C.compose dst (C.disallow_left f) h in
          let fh_hint =
            Comp_hint.Morph_hint.Compose
              (Comp_hint.Morph_hint.disallow_left f_hint, h_hint)
          in
          if w.level < u.level
          then add_vupper_nocheck ~allow_rigid ~log dst u w fh fh_hint
          else add_vlower_reversed ~allow_rigid ~log pp dst w u fh fh_hint)
          (* We are adding (fh,w) to u.vlower without propagating or
               checking bounds. This is soud by ARGUMENT 2 above *)
        v.vupper
    end

  (* Add a vupper entry for the relation [v <= f' u], tighten the lower bound of [u],
    and recursively add relations to maintain invariant.
    The lower and upper bounds of [u] and [v] are not checked, lower bound is not pushed
    down [u.vupper] *)
  let rec add_vupper_reversed : type a b r.
      allow_rigid:bool ->
      log:_ ->
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      b var ->
      (a, b, allowed * r) C.morph ->
      (a, b, allowed * r) Comp_hint.Morph_hint.t ->
      unit =
   fun ~allow_rigid ~log pp dst v u f f_hint ->
    let f' = C.right_adjoint dst f in
    let src = C.src dst f in
    let f'_hint = Comp_hint.Morph_hint.Adjoint_r (f_hint, f') in
    let x = Amorphvar (u, f', f'_hint) in
    let key = get_key src x in
    if VarMap.mem key v.vupper
    then ()
    else begin
      push_lower_bound ~allow_rigid ~log dst v f f_hint u;
      set_vupper ~allow_rigid ~log v (VarMap.add key x v.vupper);
      VarMap.iter
        (fun _ (Amorphvar (w, h, h_hint)) ->
          let fh = C.compose dst (C.disallow_right f) h in
          let fh_hint =
            Comp_hint.Morph_hint.Compose
              (Comp_hint.Morph_hint.disallow_right f_hint, h_hint)
          in
          if u.level < w.level
          then add_vupper_reversed ~allow_rigid ~log pp dst w u fh fh_hint
          else add_vlower_nocheck ~allow_rigid ~log dst u w fh fh_hint)
        v.vlower
    end

  let update_level_finalize : type a.
      allow_rigid:bool -> log:_ -> a C.obj -> int -> a var -> unit =
   fun ~allow_rigid ~log dst level u ->
    let vupper_lt, vupper_ge =
      VarMap.partition (fun _ (Amorphvar (v, _, _)) -> v.level < level) u.vupper
    in
    let vlower_le, vlower_gt =
      VarMap.partition
        (fun _ (Amorphvar (v, _, _)) -> v.level <= level)
        u.vlower
    in
    set_vlower ~allow_rigid ~log u vlower_le;
    set_vupper ~allow_rigid ~log u vupper_lt;
    VarMap.iter
      (fun _ (Amorphvar (v, f, f_hint)) ->
        add_vupper_reversed ~allow_rigid ~log H.Pinpoint.unknown dst v u f
          f_hint)
      vlower_gt;
    VarMap.iter
      (fun _ (Amorphvar (v, f, f_hint)) ->
        add_vlower_reversed ~allow_rigid ~log H.Pinpoint.unknown dst v u f
          f_hint)
      vupper_ge;
    (* optimization: if lower = upper, we can remove vuppers and vlowers since the
      information is as precise as it can get *)
    if C.le dst u.upper u.lower
    then begin
      set_vlower ~allow_rigid ~log u VarMap.empty;
      set_vupper ~allow_rigid ~log u VarMap.empty
    end

  let update_level_v : type a. log:_ -> a C.obj -> int -> a var -> unit =
   fun ~log dst level u ->
    if u.level > level
    then begin
      set_level ~allow_rigid:true ~log u level;
      update_level_finalize ~allow_rigid:true ~log dst level u
    end

  let live_id = ref 0

  let next_live_id () =
    let id = !live_id in
    incr live_id;
    id

  (* Ids for persistent copies of variables (as stored in cmi files).
     This mirrors [Subst.new_type_id] for types. *)
  let persistent_id = ref (-1)

  (* Resets the counter for persistent copies *)
  let reset_persistent_id () = persistent_id := -1

  let next_persistent_id () =
    let id = !persistent_id in
    decr persistent_id;
    id

  let fresh ?(neg = false) ?upper ?upper_hint ?lower ?lower_hint ?vlower ?vupper
      ?quantifier ?scope_order ~level obj =
    let id = if neg then next_persistent_id () else next_live_id () in
    let upper, upper_hint =
      match upper, upper_hint with
      | None, None -> C.max obj, Comp_hint.Max
      | None, Some _ -> assert false
      | Some upper, None -> upper, Comp_hint.Unknown upper
      | Some upper, Some upper_hint -> upper, upper_hint
    in
    let lower, lower_hint =
      match lower, lower_hint with
      | None, None -> C.min obj, Comp_hint.Min
      | None, Some _ -> assert false
      | Some lower, None -> lower, Comp_hint.Unknown lower
      | Some lower, Some lower_hint -> lower, lower_hint
    in
    let vlower = Option.value vlower ~default:VarMap.empty in
    let vupper = Option.value vupper ~default:VarMap.empty in
    let gencopy = None in
    let subst = None in
    let scope_order =
      match !active_scope with Some _ as scope -> scope | None -> scope_order
    in
    let var =
      { level;
        quantifier;
        scope_order;
        upper;
        upper_hint;
        lower;
        lower_hint;
        vlower;
        vupper;
        id;
        gencopy;
        subst;
        exact = []
      }
    in
    !creation_log (ref [Cvars !vars]);
    vars := Var (obj, var) :: !vars;
    var

  let unhint_morphvar (Amorphvar (v, f, _)) =
    Amorphvar (v, f, Comp_hint.Morph_hint.Base (H.Morph.unknown, f))

  let unhint_var v =
    v.upper_hint <- Comp_hint.Unknown v.upper;
    v.lower_hint <- Comp_hint.Unknown v.lower;
    v.vlower <- VarMap.map unhint_morphvar v.vlower;
    v.vupper <- VarMap.map unhint_morphvar v.vupper

  let erase_hints () = List.iter (fun (Var (_, v)) -> unhint_var v) !vars

  type ('a, 'd) hint_raw = ('a, 'd) Comp_hint.t

  type 'a error_raw =
    { left : 'a;
      left_hint : ('a, left_only) hint_raw;
      right : 'a;
      right_hint : ('a, right_only) hint_raw
    }

  type 'a submode_failure =
    | Inequality of 'a error_raw
    | Inconsistent_relation
    | Resource_limit

  type 'a exact_failure =
    | Exact_inequality of 'a error_raw
    | Exact_inconsistent

  (* Moves every reachable variable from [u] such that
  [current_level] < [u.level] < [generic_level] to
  [generic_level + (u.level - current_level)], preserving the exact topology

  PRECONDITIONS: None
  POSTCONDITIONS:
    current_level < u.level < generic_level ==>
      Forall v in the reachable set of variables from u:
        v.level = generic_level + (v.level - current_level) *)
  let rec generalize_topology_v : type a.
      log:_ -> current_level:int -> a var -> unit =
   fun ~log ~current_level u ->
    if u.level <= current_level || u.level >= generic_level
    then ()
    else begin
      let new_level = generic_level + (u.level - current_level) in
      set_level ~allow_rigid:true ~log u new_level;
      let do_gen _ (Amorphvar (v, _f, _f_hint)) =
        generalize_topology_v ~log ~current_level v
      in
      VarMap.iter do_gen u.vupper;
      VarMap.iter do_gen u.vlower;
      List.iter
        (fun component ->
          List.iter
            (fun (Var (_, v)) -> generalize_topology_v ~log ~current_level v)
            component.members)
        u.exact
    end

  (* creates a copy of the variable [u] such that [u] < [copy] and [copy] < [u] via
  submode, and lowers the [copy] to [current_level].
  The [copy] is cached in the [gencopy] of [u] *)
  let create_gencopy : type a.
      log:_ -> a C.obj -> current_level:int -> a var -> unit =
   fun ~log dst ~current_level u ->
    (* If bounds are already tight, there is no need to create a copy *)
    if (not (C.le dst u.upper u.lower)) && u.level = generic_level
    then begin
      let copy =
        fresh ~upper:u.upper ~lower:u.lower ?quantifier:u.quantifier
          ?scope_order:u.scope_order ~level:current_level dst
      in
      copy.exact <- u.exact;
      let ok1 =
        submode_mvmv ~allow_rigid:true ~log H.Pinpoint.unknown dst
          (Amorphvar (copy, C.id, Comp_hint.Morph_hint.Id))
          (Amorphvar (u, C.id, Comp_hint.Morph_hint.Id))
      in
      let ok2 =
        submode_mvmv ~allow_rigid:true ~log H.Pinpoint.unknown dst
          (Amorphvar (u, C.id, Comp_hint.Morph_hint.Id))
          (Amorphvar (copy, C.id, Comp_hint.Morph_hint.Id))
      in
      assert (Result.is_ok ok1 && Result.is_ok ok2);
      set_gencopy ~log copy u.gencopy;
      set_gencopy ~log u (Some copy)
    end

  let generalize_v : type a.
      log:_ -> a C.obj -> current_level:int -> a var -> unit =
   fun ~log dst ~current_level u ->
    generalize_topology_v ~log ~current_level u;
    update_level_v ~log dst generic_level u

  (* generalize_structure first moves a variable to generic_level (like generalize does).
     It then has three cases:
    (1) if bounds are tight, leave it as is
    (2) if bounds are fully open --- by virtue of having no
        vupper/vlower at or above the current level, and fully open
        conservative bounds --- we move it to current level
    (3) if bounds are non-trivial (neither tight nor fully open) we make a copy, keep the
        original var at generic_level, and mark the two variables as equivalent by adding
        constraint arrows via [add_vlower] and [add_vupper]

    PRECONDITIONS: None
    POSTCONDITIONS:
      u.level <= current_level ==> no changes to u
      current_level < u.level ==>
        ( (u.upper <= u.lower
              ==> u.vlower = Ø && u.vupper = Ø && u.level = generic_level)
          && (u.lower = min && u.upper = max
              && u does not have children at or above current_level
              ==> u.level = current_level)
          && (u.lower < u.upper
              && (u.lower != min || u.upper != max
                  || u has children at or above current_level)
              ==> u.level = generic_level && u.gencopy = v where v is a copy of u
                  where v.level = current_level && v <= u && u <= v)
        ) *)
  let generalize_structure_v : type a.
      log:_ -> a C.obj -> current_level:int -> a var -> unit =
   fun ~log dst ~current_level u ->
    (* if the following does not hold:
        current_level < u.level || u.level = generic_level
      [generalize_topology_v] and [update_level] do nothing. We check it here
      to optimize away the subsequent checks *)
    if u.level <= current_level || u.level = generic_level
    then ()
    else begin
      (* we optimize away vlower and vuppers if bounds are tight *)
      if C.le dst u.upper u.lower
      then begin
        set_vlower ~allow_rigid:true ~log u VarMap.empty;
        set_vupper ~allow_rigid:true ~log u VarMap.empty
      end;
      generalize_topology_v ~log ~current_level u;
      update_level_v ~log dst generic_level u;
      let vlower_above_current =
        VarMap.filter
          (fun _ (Amorphvar (v, _, _)) -> v.level >= current_level)
          u.vlower
      in
      let vupper_above_current =
        VarMap.filter
          (fun _ (Amorphvar (v, _, _)) -> v.level >= current_level)
          u.vupper
      in
      if C.le dst u.upper u.lower
      then ()
      else if
        C.le dst (C.max dst) u.upper
        && C.le dst u.lower (C.min dst)
        && VarMap.is_empty vlower_above_current
        && VarMap.is_empty vupper_above_current
        && u.exact = []
      then
        (* the bounds are fully open *)
        update_level_v ~log dst current_level u
      else begin
        (* we create a copy of the now generalized variable at [current_level] *)
        create_gencopy ~log dst ~current_level u
      end
    end

  let generalize_topology (type a l r) ~current_level (a : (a, l * r) mode) ~log
      =
    match a with
    | Amodevar (Amorphvar (v, _f, _f_hint)) ->
      generalize_topology_v ~log ~current_level v
    | Amode _ -> ()
    | Amodejoin (_, _, mvs) ->
      VarMap.iter
        (fun _ (Amorphvar (v, _f, _f_hint)) ->
          generalize_topology_v ~log ~current_level v)
        mvs
    | Amodemeet (_, _, mvs) ->
      VarMap.iter
        (fun _ (Amorphvar (v, _f, _f_hint)) ->
          generalize_topology_v ~log ~current_level v)
        mvs

  let generalize (type a l r) ~current_level (obj : a C.obj)
      (a : (a, l * r) mode) ~log =
    match a with
    | Amodevar (Amorphvar (v, f, _f_hint)) ->
      let obj = C.src obj f in
      generalize_v ~log obj ~current_level v
    | Amode _ -> ()
    | Amodejoin (_, _, mvs) ->
      VarMap.iter
        (fun _ (Amorphvar (v, f, _f_hint)) ->
          let obj = C.src obj f in
          generalize_v ~log obj ~current_level v)
        mvs
    | Amodemeet (_, _, mvs) ->
      VarMap.iter
        (fun _ (Amorphvar (v, f, _f_hint)) ->
          let obj = C.src obj f in
          generalize_v ~log obj ~current_level v)
        mvs

  let generalize_structure (type a l r) ~current_level (obj : a C.obj)
      (a : (a, l * r) mode) ~log =
    match a with
    | Amodevar (Amorphvar (v, f, _f_hint)) ->
      let obj = C.src obj f in
      generalize_structure_v ~log obj ~current_level v
    | Amode _ -> ()
    | Amodejoin (_, _, mvs) ->
      VarMap.iter
        (fun _ (Amorphvar (v, f, _f_hint)) ->
          let obj = C.src obj f in
          generalize_structure_v ~log obj ~current_level v)
        mvs
    | Amodemeet (_, _, mvs) ->
      VarMap.iter
        (fun _ (Amorphvar (v, f, _f_hint)) ->
          let obj = C.src obj f in
          generalize_structure_v ~log obj ~current_level v)
        mvs

  let fresh_mode_copy_scope () = { saved_copies = []; copied_exact = [] }

  let undo_copy_change = function
    | Coptcopy (dst, update_to_level, v, copy) ->
      Option.iter
        (fun u ->
          update_level_finalize ~allow_rigid:true ~log:None dst update_to_level
            u)
        v.subst;
      v.subst <- copy

  let cleanup_mode_copy_scope l =
    (* Copy finalization establishes the graph representation at the copy's
       level. Copies can survive type backtracking through abbreviation caches;
       undoing finalization would leave them at that level with unfinalized
       edges, including edges stored on shared variables. *)
    List.iter undo_copy_change l.saved_copies

  let with_copy_scope f =
    let scope = fresh_mode_copy_scope () in
    Fun.protect
      ~finally:(fun () -> cleanup_mode_copy_scope scope)
      (fun () -> f scope)

  let rec trace_gencopy ~target_level v =
    match v.gencopy with
    | None -> None
    | Some v' when v'.level = target_level -> Some v'
    | Some v' -> trace_gencopy ~target_level v'

  let rec copy_v : type a.
      ?copy_hidden:bool ->
      copy_scope:_ ->
      copy_from_level:int ->
      copy_below_level:int ->
      copy_to_level:int option ->
      cause:[`Save | `Restore | `Neither] ->
      a C.obj ->
      a var ->
      a var =
   fun ?(copy_hidden = false) ~copy_scope ~copy_from_level ~copy_below_level
       ~copy_to_level ~cause obj v ->
    let hidden = copy_hidden && v.level > generic_level in
    let in_window =
      hidden || (v.level >= copy_from_level && v.level < copy_below_level)
    in
    if cause = `Restore then assert (v.id < 0);
    if not in_window
    then v
    else
      (* generalize_structure might have already created a copy, cached in [gencopy].
         If such a copy exist, we return it *)
      let target_level =
        if hidden then v.level else Option.value copy_to_level ~default:v.level
      in
      let gencopy = trace_gencopy ~target_level v in
      begin match gencopy with
      | Some v' -> v'
      | None -> (
        match v.subst with
        | Some v' -> v'
        | None ->
          let copy =
            fresh ~neg:(cause = `Save) ~upper:v.upper ~lower:v.lower
              ?quantifier:v.quantifier ?scope_order:v.scope_order ~level:v.level
              obj
          in
          set_optcopy ~changes:copy_scope obj ~target_level v (Some copy);
          let vupper =
            VarMap.fold
              (fun _ (Amorphvar (u, f, f_hint)) acc ->
                let src = C.src obj f in
                let ucopy =
                  copy_v ~copy_hidden ~copy_scope ~copy_from_level
                    ~copy_below_level ~copy_to_level ~cause src u
                in
                let x = Amorphvar (ucopy, f, f_hint) in
                VarMap.add (get_key obj x) x acc)
              v.vupper VarMap.empty
          in
          let vlower =
            VarMap.fold
              (fun _ (Amorphvar (u, f, f_hint)) acc ->
                let src = C.src obj f in
                let ucopy =
                  copy_v ~copy_hidden ~copy_scope ~copy_from_level
                    ~copy_below_level ~copy_to_level ~cause src u
                in
                let x = Amorphvar (ucopy, f, f_hint) in
                VarMap.add (get_key obj x) x acc)
              v.vlower VarMap.empty
          in
          copy.vupper <- vupper;
          copy.vlower <- vlower;
          set_level ~allow_rigid:true ~log:None copy target_level;
          copy)
      end

  let prepare_copied_component : (bool * exact_component -> exact_component) ref
      =
    ref snd

  let copy (type a l r) ?(log = None) ~copy_scope ~copy_from_level
      ~copy_below_level ?copy_to_level ?(cause = `Neither) (obj : a C.obj)
      (a : (a, l * r) mode) : (a, l * r) mode =
    (* We never want to copy variables above [generic_level]. *)
    assert (copy_below_level <= generic_level + 1);
    Option.iter
      (fun l ->
        (* If we copy to a specific level, the copy window must be size 1 to
       preserve graph invariants *)
        assert (copy_below_level - copy_from_level = 1);
        (* We never copy to levels above [generic_level] *)
        assert (l <= generic_level))
      copy_to_level;
    let result : (a, l * r) mode =
      match a with
      | Amodevar (Amorphvar (v, f, f_hint)) ->
        let obj = C.src obj f in
        let vcopy =
          copy_v ~copy_scope ~copy_from_level ~copy_below_level ~copy_to_level
            ~cause obj v
        in
        if vcopy == v then a else Amodevar (Amorphvar (vcopy, f, f_hint))
      | Amode _ -> a
      | Amodejoin (a, a_hint, mvs) ->
        let mvscopy =
          VarMap.fold
            (fun _ (Amorphvar (v, f, f_hint)) acc ->
              let src = C.src obj f in
              let vcopy =
                copy_v ~copy_scope ~copy_from_level ~copy_below_level
                  ~copy_to_level ~cause src v
              in
              let x = Amorphvar (vcopy, f, f_hint) in
              VarMap.add (get_key obj x) x acc)
            mvs VarMap.empty
        in
        Amodejoin (a, a_hint, mvscopy)
      | Amodemeet (a, a_hint, mvs) ->
        let mvscopy =
          VarMap.fold
            (fun _ (Amorphvar (v, f, f_hint)) acc ->
              let src = C.src obj f in
              let vcopy =
                copy_v ~copy_scope ~copy_from_level ~copy_below_level
                  ~copy_to_level ~cause src v
              in
              let x = Amorphvar (vcopy, f, f_hint) in
              VarMap.add (get_key obj x) x acc)
            mvs VarMap.empty
        in
        Amodemeet (a, a_hint, mvscopy)
    in
    let rec copy_components () =
      let components =
        List.concat_map
          (function Coptcopy (_, _, v, _) -> v.exact)
          copy_scope.saved_copies
      in
      match
        List.find_opt
          (fun component ->
            not
              (List.exists
                 (fun (old, copied) -> old == component || copied == component)
                 copy_scope.copied_exact))
          components
      with
      | None -> ()
      | Some component ->
        let members =
          List.map
            (fun (Var (member_obj, v)) ->
              let copy =
                copy_v ~copy_hidden:true ~copy_scope ~copy_from_level
                  ~copy_below_level ~copy_to_level ~cause member_obj v
              in
              Var (member_obj, copy))
            component.members
        in
        let copied = { component with members } in
        let rigidified =
          List.exists2
            (fun (Var (_, before)) (Var (_, after)) ->
              match quantifier_of_var before, quantifier_of_var after with
              | (Outer | Existential _), Universal _ -> true
              | _ -> false)
            component.members members
        in
        let copied = !prepare_copied_component (rigidified, copied) in
        copy_scope.copied_exact
          <- (component, copied) :: copy_scope.copied_exact;
        List.iter
          (fun (Var (_, v)) ->
            (match log with
            | None -> ()
            | Some log -> log := Cexact (v, v.exact) :: !log);
            v.exact <- copied :: v.exact)
          copied.members;
        copy_components ()
    in
    copy_components ();
    result

  (* Persistent IDs are reused between saved copies. *)
  let exact_var_key v = Obj.repr v

  let row_value : type a. a C.obj -> a var -> anyvar list -> anyvalue list -> a
      =
   fun obj v members row ->
    let rec find : anyvar list -> anyvalue list -> a =
     fun members row ->
      match members, row with
      | Var (_, w) :: members, Value (value_obj, value) :: row ->
        if exact_var_key w == exact_var_key v
        then
          match C.equal_obj obj value_obj with
          | Misc.Is_eq -> value
          | Misc.Is_not_eq ->
            Misc.fatal_error "Solver: exact component object mismatch"
        else find members row
      | [], [] -> Misc.fatal_error "Solver: missing exact component variable"
      | _ -> Misc.fatal_error "Solver: malformed exact component row"
    in
    find members row

  let equal_anyvalue (Value (obj_a, a)) (Value (obj_b, b)) =
    match C.equal_obj obj_a obj_b with
    | Misc.Is_not_eq -> false
    | Misc.Is_eq -> C.le obj_a a b && C.le obj_a b a

  let rec equal_values left right =
    match left, right with
    | [], [] -> true
    | a :: left, b :: right -> equal_anyvalue a b && equal_values left right
    | _ -> false

  let eval_exact_morphvar : type a l r.
      a C.obj -> anyvar list -> anyvalue list -> (a, l * r) morphvar -> a =
   fun obj members row (Amorphvar (v, f, _)) ->
    let src = C.src obj f in
    C.apply obj f (row_value src v members row)

  let eval_exact_mode : type a l r.
      a C.obj -> anyvar list -> anyvalue list -> (a, l * r) mode -> a =
   fun obj members row -> function
    | Amode (value, _, _) -> value
    | Amodevar mv -> eval_exact_morphvar obj members row mv
    | Amodejoin (constant, _, mvs) ->
      VarMap.fold
        (fun _ mv acc ->
          C.join obj acc (eval_exact_morphvar obj members row mv))
        mvs constant
    | Amodemeet (constant, _, mvs) ->
      VarMap.fold
        (fun _ mv acc ->
          C.meet obj acc (eval_exact_morphvar obj members row mv))
        mvs constant

  let exact_graph_model members row =
    let rec check members' row' =
      match members', row' with
      | [], [] -> true
      | Var (obj, v) :: rest, Value (value_obj, value) :: values -> (
        match C.equal_obj obj value_obj with
        | Misc.Is_not_eq -> false
        | Misc.Is_eq ->
          C.le obj v.lower value && C.le obj value v.upper
          && VarMap.for_all
               (fun _ mv ->
                 C.le obj (eval_exact_morphvar obj members row mv) value)
               v.vlower
          && VarMap.for_all
               (fun _ mv ->
                 C.le obj value (eval_exact_morphvar obj members row mv))
               v.vupper
          && check rest values)
      | _ -> Misc.fatal_error "Solver: malformed exact component row"
    in
    check members row

  let exact_components members =
    List.fold_left
      (fun components (Var (_, v)) ->
        List.fold_left
          (fun components component ->
            if List.exists (fun other -> other == component) components
            then components
            else component :: components)
          components v.exact)
      [] members

  let exact_component_model component_rows component members row =
    let projection =
      List.map
        (fun (Var (obj, v)) -> Value (obj, row_value obj v members row))
        component.members
    in
    List.exists (equal_values projection) component_rows

  let exact_mode_vars : type a l r. a C.obj -> (a, l * r) mode -> anyvar list =
   fun obj -> function
    | Amode _ -> []
    | Amodevar (Amorphvar (v, f, _)) -> [Var (C.src obj f, v)]
    | Amodejoin (_, _, mvs) ->
      VarMap.fold
        (fun _ (Amorphvar (v, f, _)) acc -> Var (C.src obj f, v) :: acc)
        mvs []
    | Amodemeet (_, _, mvs) ->
      VarMap.fold
        (fun _ (Amorphvar (v, f, _)) acc -> Var (C.src obj f, v) :: acc)
        mvs []

  let exact_neighbors ?(include_fixed = false) (Var (obj, v)) =
    let from_map map =
      VarMap.fold
        (fun _ (Amorphvar (u, f, _)) ids ->
          let src = C.src obj f in
          if include_fixed || not (C.le src u.upper u.lower)
          then exact_var_key u :: ids
          else ids)
        map []
    in
    from_map v.vlower @ from_map v.vupper

  let exact_connected_members seeds =
    let id (Var (_, v)) = exact_var_key v in
    let add id ids = if List.memq id ids then ids else id :: ids in
    let rec close ids =
      let next =
        List.fold_left
          (fun ids (Var (_, v) as member) ->
            let neighbors = exact_neighbors member in
            let component_members =
              List.concat_map
                (fun component -> List.map id component.members)
                v.exact
            in
            if
              List.memq (exact_var_key v) ids
              || List.exists (fun id -> List.memq id ids) neighbors
              || List.exists (fun id -> List.memq id ids) component_members
            then
              List.fold_left
                (fun ids id -> add id ids)
                ids
                ((exact_var_key v :: neighbors) @ component_members)
            else ids)
          ids !vars
      in
      if List.length next = List.length ids then ids else close next
    in
    let ids =
      close (List.fold_left (fun ids member -> add (id member) ids) [] seeds)
    in
    let rec complete ids =
      let selected =
        List.filter (fun member -> List.memq (id member) ids) !vars
      in
      let next =
        List.fold_left
          (fun ids member ->
            List.fold_left
              (fun ids key -> add key ids)
              ids
              (exact_neighbors ~include_fixed:true member))
          ids selected
        |> close
      in
      if List.length next = List.length ids then ids else complete next
    in
    let ids = complete ids in
    List.rev !vars |> List.filter (fun member -> List.memq (id member) ids)

  let exact_member_index members v =
    let rec find index = function
      | [] -> Misc.fatal_error "Solver: missing exact component variable"
      | Var (_, w) :: rest ->
        if exact_var_key v == exact_var_key w
        then index
        else find (index + 1) rest
    in
    find 0 members

  let exact_symbolic_predicate manager support predicate =
    let support = List.sort_uniq Int.compare support in
    let assignment = Array.make (Solver_mdd.arity manager) 0 in
    let rec build = function
      | [] ->
        if predicate assignment then Solver_mdd.true_ else Solver_mdd.false_
      | index :: rest ->
        Solver_mdd.mk manager index
          (Array.init (Solver_mdd.cardinality manager index) (fun value ->
               assignment.(index) <- value;
               build rest))
    in
    build support

  let exact_graph_stamp members =
    List.map
      (fun (Var (_, v)) ->
        Obj.repr v.lower, Obj.repr v.upper, Obj.repr v.vlower, Obj.repr v.vupper)
      members

  type symbolic_morph_key =
    | Symbolic_morph :
        'b C.obj * ('a, 'b, 'd) C.morph * bool
        -> symbolic_morph_key

  module Symbolic_morph_map = Map.Make (struct
    type t = symbolic_morph_key

    let compare (Symbolic_morph (left, f, reverse))
        (Symbolic_morph (right, g, reverse')) =
      let c = C.compare_obj left right in
      if c <> 0
      then c
      else
        match C.equal_obj left right with
        | Misc.Is_not_eq -> assert false
        | Misc.Is_eq ->
          let c = C.compare_morph left f g in
          if c <> 0 then c else Bool.compare reverse reverse'
  end)

  let symbolic_morphs = ref Symbolic_morph_map.empty

  let exact_symbolic_edge manager ~source_index ~target_index obj f ~reverse =
    let source = C.src obj f in
    let source_values = Array.of_list (C.elements source) in
    let target_values = Array.of_list (C.elements obj) in
    let holds source_value target_value =
      let mapped = C.apply obj f source_values.(source_value) in
      let target = target_values.(target_value) in
      if reverse then C.le obj target mapped else C.le obj mapped target
    in
    if source_index = target_index
    then
      exact_symbolic_predicate manager [source_index] (fun assignment ->
          holds assignment.(source_index) assignment.(source_index))
    else
      let key = Symbolic_morph (obj, f, reverse) in
      let template, node =
        match Symbolic_morph_map.find_opt key !symbolic_morphs with
        | Some cached -> cached
        | None ->
          let template =
            Solver_mdd.create
              ~bit_order:
                [| C.symbolic_bit_order source; C.symbolic_bit_order obj |]
              [| Array.length source_values; Array.length target_values |]
          in
          let node =
            exact_symbolic_predicate template [0; 1] (fun assignment ->
                holds assignment.(0) assignment.(1))
          in
          let node = Solver_mdd.factor template node in
          symbolic_morphs
            := Symbolic_morph_map.add key (template, node) !symbolic_morphs;
          template, node
      in
      Solver_mdd.import manager ~old_manager:template
        ~old_to_new:[| source_index; target_index |]
        node

  let exact_same_members members component =
    List.length members = List.length component.members
    && List.for_all2
         (fun (Var (_, v)) (Var (_, w)) -> exact_var_key v == exact_var_key w)
         members component.members

  let exact_same_stamp (lo, hi, below, above) (lo', hi', below', above') =
    lo == lo' && hi == hi' && below == below' && above == above'

  let exact_reusable members component =
    (* The component already entails its stamped graph. The bounds and edge
       maps are immutable values, so unchanged fields preserve that invariant. *)
    exact_same_members members component
    && List.for_all2 exact_same_stamp
         (exact_graph_stamp members)
         component.graph_stamp

  let exact_symbolic_snapshot members components =
    let arities =
      Array.of_list
        (List.map (fun (Var (obj, _)) -> List.length (C.elements obj)) members)
    in
    let bit_order =
      Array.of_list
        (List.map (fun (Var (obj, _)) -> C.symbolic_bit_order obj) members)
    in
    let manager = Solver_mdd.create ~bit_order arities in
    let value_index (Var (obj, _)) value =
      let rec find index = function
        | [] -> Misc.fatal_error "Solver: exact value outside lattice"
        | candidate :: rest ->
          if equal_anyvalue (Value (obj, candidate)) value
          then index
          else find (index + 1) rest
      in
      find 0 (C.elements obj)
    in
    let cube component row =
      let choices =
        List.map2
          (fun (Var (_, v) as member) value ->
            exact_member_index members v, value_index member value)
          component.members row
        |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
      in
      List.fold_right
        (fun (index, chosen) rest ->
          Solver_mdd.mk manager index
            (Array.init (Solver_mdd.cardinality manager index) (fun value ->
                 if value = chosen then rest else Solver_mdd.false_)))
        choices Solver_mdd.true_
    in
    let from_rows component rows =
      List.fold_left
        (fun relation row ->
          Solver_mdd.or_ manager relation (cube component row))
        Solver_mdd.false_ rows
    in
    let guards = ref [Solver_mdd.true_] in
    let witnesses = ref [Solver_mdd.true_] in
    List.iter
      (fun component ->
        let component_guard, component_witness =
          match component.symbolic with
          | None ->
            ( from_rows component component.guard_rows,
              from_rows component component.witness_rows )
          | Some symbolic ->
            let old_to_new =
              Array.of_list
                (List.map
                   (fun (Var (_, v)) -> exact_member_index members v)
                   component.members)
            in
            ( Solver_mdd.import manager ~old_manager:symbolic.manager
                ~old_to_new symbolic.guard,
              Solver_mdd.import manager ~old_manager:symbolic.manager
                ~old_to_new symbolic.witness )
        in
        guards := component_guard :: !guards;
        witnesses := component_witness :: !witnesses;
        Solver_mdd.compact ~threshold:100_000 manager (!guards @ !witnesses))
      components;
    let atoms = ref [] in
    let add atom = atoms := atom :: !atoms in
    let edges = Hashtbl.create 31 in
    let add_edge i j atom =
      let key = Int.min i j, Int.max i j in
      Hashtbl.replace edges key
        (atom :: Option.value (Hashtbl.find_opt edges key) ~default:[])
    in
    let old_stamps =
      List.concat_map
        (fun component ->
          List.map2
            (fun (Var (_, v)) stamp -> exact_var_key v, stamp)
            component.members component.graph_stamp)
        components
    in
    let stamps = Array.of_list (exact_graph_stamp members) in
    List.iteri
      (fun index (Var (obj, v)) ->
        let values = Array.of_list (C.elements obj) in
        add
          (Solver_mdd.factor manager
             (exact_symbolic_predicate manager [index] (fun assignment ->
                  let value = values.(assignment.(index)) in
                  C.le obj v.lower value && C.le obj value v.upper))))
      members;
    List.iteri
      (fun index (Var (obj, v)) ->
        let unchanged =
          List.exists
            (fun (key, stamp) ->
              key == exact_var_key v && exact_same_stamp stamp stamps.(index))
            old_stamps
        in
        if not unchanged
        then begin
          VarMap.iter
            (fun _ (Amorphvar (u, f, _)) ->
              let source_index = exact_member_index members u in
              add_edge source_index index
                (exact_symbolic_edge manager ~source_index ~target_index:index
                   obj f ~reverse:false))
            v.vlower;
          VarMap.iter
            (fun _ (Amorphvar (u, f, _)) ->
              let source_index = exact_member_index members u in
              add_edge source_index index
                (exact_symbolic_edge manager ~source_index ~target_index:index
                   obj f ~reverse:true))
            v.vupper
        end)
      members;
    Hashtbl.fold
      (fun key constraints groups -> (key, constraints) :: groups)
      edges []
    |> List.sort (fun (key, left) (other_key, right) ->
        let c = Int.compare (List.length right) (List.length left) in
        if c = 0 then Stdlib.compare key other_key else c)
    |> List.iter (fun (_, constraints) ->
        add (Solver_mdd.and_many manager constraints));
    let atoms = List.rev !atoms in
    let roots = !guards @ !witnesses @ atoms in
    let conjoin relations = Solver_mdd.and_many manager (relations @ atoms) in
    Solver_mdd.compact manager roots;
    let guard = conjoin !guards in
    Solver_mdd.compact manager (guard :: roots);
    let witness =
      if List.for_all2 ( == ) !guards !witnesses
      then guard
      else conjoin !witnesses
    in
    { members;
      guard_rows = [];
      witness_rows = [];
      graph_stamp = exact_graph_stamp members;
      symbolic = Some { manager; guard; witness }
    }

  let exact_materialize_fixed ~log component =
    let install : type a. a C.obj -> a var -> a -> unit =
     fun obj v value ->
      if not (C.le obj value v.lower)
      then update_lower ~allow_rigid:true ~log obj v value (Unknown value);
      if not (C.le obj v.upper value)
      then update_upper ~allow_rigid:true ~log obj v value (Unknown value)
    in
    match component.symbolic with
    | Some symbolic -> (
      match Solver_mdd.find_sat symbolic.manager symbolic.guard with
      | None -> ()
      | Some assignment ->
        let alternative =
          Option.get
            (Solver_mdd.find_sat ~prefer_high:true symbolic.manager
               symbolic.guard)
        in
        let varying =
          Array.mapi (fun i value -> value <> alternative.(i)) assignment
        in
        List.iteri
          (fun index (Var (obj, v)) ->
            if
              (not varying.(index))
              && quantifier_of_var v = Outer
              && not (C.le obj v.upper v.lower)
            then begin
              let chosen = assignment.(index) in
              let equal =
                exact_symbolic_predicate symbolic.manager [index] (fun values ->
                    values.(index) = chosen)
              in
              match
                Solver_mdd.counterexample symbolic.manager symbolic.guard equal
              with
              | None -> install obj v (List.nth (C.elements obj) chosen)
              | Some other ->
                Array.iteri
                  (fun i value ->
                    if value <> assignment.(i) then varying.(i) <- true)
                  other
            end)
          component.members)
    | None -> (
      match component.guard_rows with
      | [] -> ()
      | first :: rest ->
        List.iter
          (fun (Var (obj, v)) ->
            if quantifier_of_var v = Outer && not (C.le obj v.upper v.lower)
            then begin
              let value = row_value obj v component.members first in
              if
                List.for_all
                  (fun row ->
                    let other = row_value obj v component.members row in
                    C.le obj value other && C.le obj other value)
                  rest
              then install obj v value
            end)
          component.members)

  let exact_without_fixed component =
    let fixed, members =
      List.partition
        (fun (Var (obj, v)) -> C.le obj v.upper v.lower)
        component.members
    in
    if fixed = []
    then component
    else
      let retained =
        List.map
          (fun (Var (_, v)) -> exact_member_index component.members v)
          members
      in
      let graph_stamp = List.map (List.nth component.graph_stamp) retained in
      match component.symbolic with
      | None ->
        let project rows =
          List.filter_map
            (fun row ->
              if
                List.for_all
                  (fun (Var (obj, v)) ->
                    let value = row_value obj v component.members row in
                    C.le obj v.lower value && C.le obj value v.upper)
                  fixed
              then Some (List.map (List.nth row) retained)
              else None)
            rows
        in
        { members;
          graph_stamp;
          symbolic = None;
          guard_rows = project component.guard_rows;
          witness_rows = project component.witness_rows
        }
      | Some symbolic ->
        let fixed_values = Array.make (List.length component.members) None in
        List.iter
          (fun (Var (obj, v)) ->
            let index = exact_member_index component.members v in
            let rec find i = function
              | [] -> assert false
              | value :: rest ->
                if C.le obj v.lower value && C.le obj value v.upper
                then i
                else find (i + 1) rest
            in
            fixed_values.(index) <- Some (find 0 (C.elements obj)))
          fixed;
        let manager =
          Solver_mdd.create
            ~bit_order:
              (Array.of_list
                 (List.map
                    (fun (Var (obj, _)) -> C.symbolic_bit_order obj)
                    members))
            (Array.of_list
               (List.map
                  (fun (Var (obj, _)) -> List.length (C.elements obj))
                  members))
        in
        let old_to_new =
          Array.of_list
            (List.map
               (fun (Var (obj, v)) ->
                 if C.le obj v.upper v.lower
                 then -1
                 else exact_member_index members v)
               component.members)
        in
        let import =
          Solver_mdd.import ~fixed:fixed_values manager
            ~old_manager:symbolic.manager ~old_to_new
        in
        { members;
          graph_stamp;
          guard_rows = [];
          witness_rows = [];
          symbolic =
            Some
              { manager;
                guard = import symbolic.guard;
                witness = import symbolic.witness
              }
        }

  let exact_without_fixed component =
    let component = exact_without_fixed component in
    match component.symbolic with
    | None -> component
    | Some symbolic ->
      let support =
        Solver_mdd.support symbolic.manager [symbolic.guard; symbolic.witness]
      in
      if Array.for_all Fun.id support
      then component
      else
        let retained =
          List.init (Array.length support) Fun.id
          |> List.filter (Array.get support)
        in
        let members = List.map (List.nth component.members) retained in
        let manager =
          Solver_mdd.create
            ~bit_order:
              (Array.of_list
                 (List.map
                    (fun (Var (obj, _)) -> C.symbolic_bit_order obj)
                    members))
            (Array.of_list
               (List.map
                  (fun (Var (obj, _)) -> List.length (C.elements obj))
                  members))
        in
        let mapping = Array.make (Array.length support) (-1) in
        List.iteri (fun index old -> mapping.(old) <- index) retained;
        let import =
          Solver_mdd.import manager ~old_manager:symbolic.manager
            ~old_to_new:mapping
        in
        { members;
          graph_stamp = List.map (List.nth component.graph_stamp) retained;
          guard_rows = [];
          witness_rows = [];
          symbolic =
            Some
              { manager;
                guard = import symbolic.guard;
                witness = import symbolic.witness
              }
        }

  let exact_owns component v =
    List.exists
      (fun (Var (_, u)) -> exact_var_key u == exact_var_key v)
      component.members

  let rec exact_split_components component =
    match component.symbolic with
    | None -> [component]
    | Some symbolic ->
      let members = Array.of_list component.members in
      let count = Array.length members in
      let parent = Array.init count Fun.id in
      let rec root i = if parent.(i) = i then i else root parent.(i) in
      let index v =
        let rec find i =
          if i = count
          then None
          else
            let (Var (_, u)) = members.(i) in
            if exact_var_key u == exact_var_key v then Some i else find (i + 1)
        in
        find 0
      in
      Array.iteri
        (fun i (Var (_, v)) ->
          let edge _ (Amorphvar (u, _, _)) =
            match index u with
            | None -> ()
            | Some j -> parent.(root i) <- root j
          in
          VarMap.iter edge v.vlower;
          VarMap.iter edge v.vupper)
        members;
      let groups =
        List.init count Fun.id
        |> List.fold_left
             (fun groups i ->
               let r = root i in
               let old = Option.value (List.assoc_opt r groups) ~default:[] in
               (r, i :: old) :: List.remove_assoc r groups)
             []
        |> List.map snd
      in
      let manager = symbolic.manager in
      let project keep guard witness =
        let projected_members = List.map (Array.get members) keep in
        let new_manager =
          Solver_mdd.create
            ~bit_order:
              (Array.of_list
                 (List.map
                    (fun (Var (obj, _)) -> C.symbolic_bit_order obj)
                    projected_members))
            (Array.of_list
               (List.map
                  (fun (Var (obj, _)) -> List.length (C.elements obj))
                  projected_members))
        in
        let mapping = Array.make count (-1) in
        List.iteri
          (fun new_index old_index -> mapping.(old_index) <- new_index)
          keep;
        let import =
          Solver_mdd.import new_manager ~old_manager:manager ~old_to_new:mapping
        in
        { members = projected_members;
          guard_rows = [];
          witness_rows = [];
          graph_stamp = List.map (List.nth component.graph_stamp) keep;
          symbolic =
            Some
              { manager = new_manager;
                guard = import guard;
                witness = import witness
              }
        }
      in
      let rec try_groups = function
        | [] -> [component]
        | group :: remaining ->
          let group = List.sort Int.compare group in
          let rest =
            List.init count Fun.id
            |> List.filter (fun i -> not (List.mem i group))
          in
          let guard_left = Solver_mdd.exists_many manager rest symbolic.guard in
          let guard_right =
            Solver_mdd.exists_many manager group symbolic.guard
          in
          let witness_left =
            Solver_mdd.exists_many manager rest symbolic.witness
          in
          let witness_right =
            Solver_mdd.exists_many manager group symbolic.witness
          in
          let independent relation left right =
            Solver_mdd.entails manager
              (Solver_mdd.and_ manager left right)
              relation
          in
          if
            independent symbolic.guard guard_left guard_right
            && independent symbolic.witness witness_left witness_right
          then
            project group guard_left witness_left
            :: exact_split_components (project rest guard_right witness_right)
          else try_groups remaining
      in
      if List.length groups <= 1 then [component] else try_groups groups

  let exact_order_members members components =
    match components with
    | [component]
      when List.length members = List.length component.members
           && List.for_all
                (fun (Var (_, v)) ->
                  List.exists
                    (fun (Var (_, u)) -> exact_var_key v == exact_var_key u)
                    members)
                component.members ->
      component.members
    | _ ->
      if List.length members < 64 || components = []
      then members
      else
        let largest =
          List.fold_left
            (fun best component ->
              if List.length component.members > List.length best.members
              then component
              else best)
            (List.hd components) components
        in
        let key (Var (_, v)) = exact_var_key v in
        let contains ordered member =
          List.exists (fun x -> key x == key member) ordered
        in
        let merge ordered component =
          let rec loop ordered = function
            | [] -> ordered
            | member :: rest when contains ordered member -> loop ordered rest
            | member :: rest ->
              let successor = List.find_opt (contains ordered) rest in
              let rec insert = function
                | [] -> [member]
                | x :: xs as remaining ->
                  if
                    match successor with
                    | Some next -> key x == key next
                    | None -> false
                  then member :: remaining
                  else x :: insert xs
              in
              loop (insert ordered) rest
          in
          loop ordered component.members
        in
        let ordered = List.fold_left merge largest.members components in
        let missing =
          List.filter (fun member -> not (contains ordered member)) members
        in
        let ordered =
          List.fold_left
            (fun ordered member ->
              let neighbors = exact_neighbors member in
              let adjacent =
                List.mapi
                  (fun index (Var (_, v)) ->
                    if List.memq (exact_var_key v) neighbors
                    then Some index
                    else None)
                  ordered
                |> List.filter_map Fun.id
              in
              let after =
                match adjacent with
                | [] -> List.length ordered
                | xs -> 1 + List.nth xs (List.length xs / 2)
              in
              let rec insert n = function
                | rest when n = 0 -> member :: rest
                | [] -> [member]
                | x :: xs -> x :: insert (n - 1) xs
              in
              insert after ordered)
            ordered missing
        in
        let shared, private_members =
          List.partition
            (fun (Var (_, v)) ->
              List.length
                (List.filter
                   (fun component -> exact_owns component v)
                   components)
              > 1)
            ordered
        in
        shared @ private_members

  let exact_order_members members components =
    let ordered = exact_order_members members components in
    if List.length ordered < 64
    then ordered
    else
      let seen = ref [] in
      let result = ref [] in
      let rec emit (Var (obj, v) as member) =
        let key = exact_var_key v in
        if not (List.memq key !seen)
        then begin
          seen := key :: !seen;
          result := member :: !result;
          let identities : type a l r.
              a C.obj -> (a, l * r) morphvar VarMap.t -> Obj.t list =
           fun obj map ->
            VarMap.fold
              (fun _ (Amorphvar (u, f, _)) keys ->
                if C.compare_morph obj f C.id = 0
                then exact_var_key u :: keys
                else keys)
              map []
          in
          let lower = identities obj v.vlower
          and upper = identities obj v.vupper in
          List.iter
            (fun (Var (_, u) as neighbor) ->
              let key = exact_var_key u in
              if List.memq key lower && List.memq key upper then emit neighbor)
            ordered
        end
      in
      List.iter emit ordered;
      List.rev !result

  let exact_snapshot_cache = ref None

  let exact_snapshot seeds =
    let members = exact_connected_members seeds in
    let components = exact_components members in
    let members = exact_order_members members components in
    let cached =
      match !exact_snapshot_cache with
      | Some (inputs, component)
        when List.length inputs = List.length components
             && List.for_all2 ( == ) inputs components
             && exact_reusable members component ->
        Some component
      | _ -> None
    in
    match cached, components with
    | Some component, _ -> Some component
    | _, [component] when exact_reusable members component -> Some component
    | _ ->
      let max_rows = 10_000 in
      let within_limit =
        List.fold_left
          (fun count (Var (obj, _)) ->
            let cardinality = List.length (C.elements obj) in
            if cardinality = 0 || count > max_rows / cardinality
            then max_rows + 1
            else count * cardinality)
          1 members
        <= max_rows
      in
      if
        List.exists (fun component -> component.symbolic <> None) components
        || Sys.getenv_opt "OCAML_EXPERIMENTAL_SYMBOLIC_MODES" = Some "force"
        || (not within_limit)
           && Sys.getenv_opt "OCAML_EXPERIMENTAL_SYMBOLIC_MODES" = Some "1"
      then
        try
          let component = exact_symbolic_snapshot members components in
          exact_snapshot_cache := Some (components, component);
          Some component
        with Solver_mdd.Limit -> None
      else if not within_limit
      then None
      else
        let rec enumerate = function
          | [] -> [[]]
          | Var (obj, _) :: rest ->
            let tails = enumerate rest in
            List.concat_map
              (fun value ->
                List.map (fun tail -> Value (obj, value) :: tail) tails)
              (C.elements obj)
        in
        let graph_rows =
          enumerate members |> List.filter (exact_graph_model members)
        in
        let guard_rows =
          List.filter
            (fun row ->
              List.for_all
                (fun component ->
                  exact_component_model component.guard_rows component members
                    row)
                components)
            graph_rows
        in
        let witness_rows =
          List.filter
            (fun row ->
              List.for_all
                (fun component ->
                  exact_component_model component.witness_rows component members
                    row)
                components)
            graph_rows
        in
        Some
          { members;
            guard_rows;
            witness_rows;
            graph_stamp = exact_graph_stamp members;
            symbolic = None
          }

  type var_predicate = { check : 'a. 'a var -> bool }

  let exact_mode_has : type a l r. var_predicate -> (a, l * r) mode -> bool =
   fun { check } -> function
    | Amode _ -> false
    | Amodevar (Amorphvar (v, _, _)) -> check v
    | Amodejoin (_, _, mvs) ->
      VarMap.exists (fun _ (Amorphvar (v, _, _)) -> check v) mvs
    | Amodemeet (_, _, mvs) ->
      VarMap.exists (fun _ (Amorphvar (v, _, _)) -> check v) mvs

  let exact_same_on selected members left right =
    List.for_all
      (fun (Var (obj, v)) ->
        equal_anyvalue
          (Value (obj, row_value obj v members left))
          (Value (obj, row_value obj v members right)))
      selected

  let rec exact_groups selected members = function
    | [] -> []
    | first :: rest ->
      let same, different =
        List.partition (exact_same_on selected members first) rest
      in
      (first :: same) :: exact_groups selected members different

  let exact_quantifier_blocks members =
    let rank = function
      | Outer -> assert false
      | Universal order -> order, 0
      | Existential order -> order, 1
    in
    let quantified =
      List.filter_map
        (fun (Var (_, v) as member) ->
          match quantifier_of_var v with
          | Outer -> None
          | (Universal _ | Existential _) as quantifier ->
            Some (quantifier, member))
        members
      |> List.sort (fun (left, _) (right, _) ->
          compare (rank left) (rank right))
    in
    let rec blocks = function
      | [] -> []
      | (quantifier, member) :: rest ->
        let same, other =
          List.partition (fun (current, _) -> current = quantifier) rest
        in
        (quantifier, member :: List.map snd same) :: blocks other
    in
    blocks quantified

  let exact_quantified_accepts members guard_rows witness_rows =
    let rec check guard_rows witness_rows = function
      | [] -> witness_rows <> []
      | (Outer, _) :: _ -> assert false
      | (Universal _, vars) :: rest ->
        exact_groups vars members guard_rows
        |> List.for_all (fun guard_group ->
            let first = List.hd guard_group in
            let matching_witnesses =
              List.filter (exact_same_on vars members first) witness_rows
            in
            check guard_group matching_witnesses rest)
      | (Existential _, vars) :: rest ->
        exact_groups vars members witness_rows
        |> List.exists (fun witness_group ->
            let first = List.hd witness_group in
            let matching_guards =
              List.filter (exact_same_on vars members first) guard_rows
            in
            check matching_guards witness_group rest)
    in
    check guard_rows witness_rows (exact_quantifier_blocks members)

  let exact_good_outer_groups component witness_rows =
    let members = component.members in
    let outer =
      List.filter (fun (Var (_, v)) -> quantifier_of_var v = Outer) members
    in
    let good_groups =
      exact_groups outer members component.guard_rows
      |> List.filter (fun guard_rows ->
          let first = List.hd guard_rows in
          let matching_witnesses =
            List.filter (exact_same_on outer members first) witness_rows
          in
          exact_quantified_accepts members guard_rows matching_witnesses)
    in
    outer, good_groups

  let exact_symbolic_row component assignment =
    List.mapi
      (fun index (Var (obj, _)) ->
        let value = List.nth (C.elements obj) assignment.(index) in
        Value (obj, value))
      component.members

  type exact_irreducibles =
    | Irreducibles : 'a C.obj * 'a list * 'a list -> exact_irreducibles

  let irreducibles_cache = ref []

  let exact_irreducibles : type a. a C.obj -> a list * a list =
   fun obj ->
    let rec find : exact_irreducibles list -> a list * a list = function
      | Irreducibles (other, joins, meets) :: rest -> (
        match C.equal_obj obj other with
        | Misc.Is_eq -> joins, meets
        | Misc.Is_not_eq -> find rest)
      | [] ->
        let values = C.elements obj in
        let joins =
          List.filter
            (fun value ->
              let lower =
                List.fold_left
                  (fun acc other ->
                    if C.le obj other value && not (C.le obj value other)
                    then C.join obj acc other
                    else acc)
                  (C.min obj) values
              in
              not (C.le obj value lower))
            values
        in
        let meets =
          List.filter
            (fun value ->
              let upper =
                List.fold_left
                  (fun acc other ->
                    if C.le obj value other && not (C.le obj other value)
                    then C.meet obj acc other
                    else acc)
                  (C.max obj) values
              in
              not (C.le obj upper value))
            values
        in
        irreducibles_cache
          := Irreducibles (obj, joins, meets) :: !irreducibles_cache;
        joins, meets
    in
    find !irreducibles_cache

  let exact_symbolic_mode_bound : type a l r.
      exact_component ->
      a C.obj ->
      (a, l * r) mode ->
      lower:bool ->
      a ->
      Solver_mdd.node =
   fun component obj mode ~lower value ->
    let manager = (Option.get component.symbolic).manager in
    let truth b = if b then Solver_mdd.true_ else Solver_mdd.false_ in
    let morph_bound lower value (Amorphvar (v, f, _)) =
      let index = exact_member_index component.members v in
      let values = Array.of_list (C.elements (C.src obj f)) in
      exact_symbolic_predicate manager [index] (fun assignment ->
          let actual = C.apply obj f values.(assignment.(index)) in
          if lower then C.le obj value actual else C.le obj actual value)
    in
    match mode with
    | Amode (actual, _, _) ->
      truth (if lower then C.le obj value actual else C.le obj actual value)
    | Amodevar morph -> morph_bound lower value morph
    | Amodejoin (constant, _, morphs) ->
      let upper value =
        VarMap.fold
          (fun _ morph relation ->
            Solver_mdd.and_ manager relation (morph_bound false value morph))
          morphs
          (truth (C.le obj constant value))
      in
      if not lower
      then upper value
      else
        List.fold_left
          (fun relation bound ->
            if C.le obj value bound
            then relation
            else
              Solver_mdd.and_ manager relation
                (Solver_mdd.not_ manager (upper bound)))
          Solver_mdd.true_
          (snd (exact_irreducibles obj))
    | Amodemeet (constant, _, morphs) ->
      let lower_bound value =
        VarMap.fold
          (fun _ morph relation ->
            Solver_mdd.and_ manager relation (morph_bound true value morph))
          morphs
          (truth (C.le obj value constant))
      in
      if lower
      then lower_bound value
      else
        List.fold_left
          (fun relation bound ->
            if C.le obj bound value
            then relation
            else
              Solver_mdd.and_ manager relation
                (Solver_mdd.not_ manager (lower_bound bound)))
          Solver_mdd.true_
          (fst (exact_irreducibles obj))

  let exact_symbolic_mode_atom : type a l r.
      exact_component ->
      Solver_mdd.manager ->
      a C.obj ->
      (a, allowed * r) mode ->
      (a, l * allowed) mode ->
      Solver_mdd.node =
   fun component manager obj left right ->
    List.fold_left
      (fun relation bound ->
        let left_bound =
          exact_symbolic_mode_bound component obj left ~lower:true bound
        in
        let right_bound =
          exact_symbolic_mode_bound component obj right ~lower:true bound
        in
        let implication =
          Solver_mdd.or_ manager
            (Solver_mdd.not_ manager left_bound)
            right_bound
        in
        Solver_mdd.and_ manager relation implication)
      Solver_mdd.true_
      (fst (exact_irreducibles obj))

  let exact_symbolic_good_outer ?scope component witness =
    let symbolic = Option.get component.symbolic in
    let manager = symbolic.manager in
    Solver_mdd.compact manager [symbolic.guard; witness];
    let blocks =
      List.rev (exact_quantifier_blocks component.members)
      |> List.filter_map (fun (quantifier, members) ->
          let members =
            List.filter
              (fun (Var (_, v)) ->
                match scope with
                | None -> true
                | Some order -> v.scope_order = Some order)
              members
          in
          if members = [] then None else Some (quantifier, members))
    in
    let same_kind left right =
      match left, right with
      | Existential _, Existential _ | Universal _, Universal _ -> true
      | _ -> false
    in
    let rec coalesce = function
      | (q, xs) :: (r, ys) :: rest when same_kind q r ->
        coalesce ((q, xs @ ys) :: rest)
      | block :: rest -> block :: coalesce rest
      | [] -> []
    in
    let blocks = coalesce blocks in
    let rec eliminate domain winning = function
      | [] -> winning
      | (quantifier, vars) :: rest ->
        let indices =
          List.map
            (fun (Var (_, v)) -> exact_member_index component.members v)
            vars
        in
        let universal =
          match quantifier with
          | Outer -> assert false
          | Existential _ -> false
          | Universal _ -> true
        in
        let domain, winning =
          Solver_mdd.eliminate_guarded manager ~universal indices ~domain
            ~witness:winning
        in
        Solver_mdd.compact ~threshold:100_000 manager
          [symbolic.guard; witness; domain; winning];
        eliminate domain winning rest
    in
    eliminate symbolic.guard witness blocks

  let exact_symbolic_restrict component witness =
    let symbolic = Option.get component.symbolic in
    let manager = symbolic.manager in
    let good = exact_symbolic_good_outer component witness in
    Solver_mdd.compact manager [symbolic.guard; witness; good];
    let guard = Solver_mdd.and_ manager symbolic.guard good in
    Solver_mdd.compact manager [guard; witness; good];
    let witness = Solver_mdd.and_ manager witness good in
    { component with symbolic = Some { symbolic with guard; witness } }

  let with_subsumption_scope ~commit f ~log =
    let previous = !active_scope in
    let before = !vars in
    let order = !next_scope_order in
    next_scope_order := order + 2;
    active_scope := Some order;
    Fun.protect
      ~finally:(fun () -> active_scope := previous)
      (fun () ->
        let result = f () in
        if commit result
        then begin
          let local v =
            v.scope_order = Some order && quantifier_of_var v <> Outer
          in
          let rec created current =
            if current == before
            then []
            else
              match current with
              | [] -> assert false
              | (Var (_, v) as member) :: rest ->
                if local v then member :: created rest else created rest
          in
          let created = created !vars in
          let rec close () =
            match exact_components created with
            | [] -> ()
            | component :: _ ->
              let component =
                match exact_snapshot component.members with
                | None -> raise Exact_resource_limit
                | Some component ->
                  if Option.is_some component.symbolic
                  then component
                  else exact_symbolic_snapshot component.members [component]
              in
              let symbolic = Option.get component.symbolic in
              let locals, members =
                List.partition (fun (Var (_, v)) -> local v) component.members
              in
              let indices =
                List.map
                  (fun (Var (_, v)) -> exact_member_index component.members v)
                  locals
              in
              Solver_mdd.compact symbolic.manager
                [symbolic.guard; symbolic.witness];
              let guard =
                Solver_mdd.exists_many symbolic.manager indices symbolic.guard
              in
              let winning =
                exact_symbolic_good_outer ~scope:order component
                  symbolic.witness
              in
              Solver_mdd.compact symbolic.manager [guard; winning];
              let witness = Solver_mdd.and_ symbolic.manager guard winning in
              let manager =
                Solver_mdd.create
                  ~bit_order:
                    (Array.of_list
                       (List.map
                          (fun (Var (obj, _)) -> C.symbolic_bit_order obj)
                          members))
                  (Array.of_list
                     (List.map
                        (fun (Var (obj, _)) -> List.length (C.elements obj))
                        members))
              in
              let old_to_new =
                Array.of_list
                  (List.map
                     (fun (Var (_, v)) ->
                       if local v then -1 else exact_member_index members v)
                     component.members)
              in
              let guard =
                Solver_mdd.import manager ~old_manager:symbolic.manager
                  ~old_to_new guard
              in
              let witness =
                Solver_mdd.import manager ~old_manager:symbolic.manager
                  ~old_to_new witness
              in
              List.iter
                (fun (Var (_, v)) ->
                  let keep _ (Amorphvar (u, _, _)) = not (local u) in
                  let lower =
                    if local v
                    then VarMap.empty
                    else VarMap.filter keep v.vlower
                  in
                  let upper =
                    if local v
                    then VarMap.empty
                    else VarMap.filter keep v.vupper
                  in
                  if lower != v.vlower
                  then set_vlower ~allow_rigid:true ~log v lower;
                  if upper != v.vupper
                  then set_vupper ~allow_rigid:true ~log v upper)
                component.members;
              let replacement =
                { members;
                  guard_rows = [];
                  witness_rows = [];
                  graph_stamp = exact_graph_stamp members;
                  symbolic = Some { manager; guard; witness }
                }
              in
              exact_materialize_fixed ~log replacement;
              let redundant =
                try
                  let graph = exact_symbolic_snapshot members [] in
                  let graph = Option.get graph.symbolic in
                  let retained = Option.get replacement.symbolic in
                  let import =
                    Solver_mdd.import graph.manager
                      ~old_manager:retained.manager
                      ~old_to_new:(Array.init (List.length members) Fun.id)
                  in
                  Solver_mdd.entails graph.manager graph.guard
                    (import retained.guard)
                  && Solver_mdd.entails graph.manager graph.guard
                       (import retained.witness)
                with Solver_mdd.Limit -> false
              in
              let replacement = exact_without_fixed replacement in
              let replacements =
                try if redundant then [] else exact_split_components replacement
                with Solver_mdd.Limit ->
                  let symbolic = Option.get replacement.symbolic in
                  Solver_mdd.compact symbolic.manager
                    [symbolic.guard; symbolic.witness];
                  [replacement]
              in
              List.iter
                (fun (Var (obj, v)) ->
                  (match log with
                  | None -> ()
                  | Some log -> log := Cexact (v, v.exact) :: !log);
                  v.exact
                    <- (if local v || C.le obj v.upper v.lower
                        then []
                        else
                          List.filter
                            (fun component ->
                              List.exists
                                (fun (Var (_, u)) ->
                                  exact_var_key u == exact_var_key v)
                                component.members)
                            replacements))
                component.members;
              close ()
          in
          close ()
        end;
        result)

  let exact_static_bound : type a l r.
      a C.obj -> (a, l * r) mode -> lower:bool -> a =
   fun obj mode ~lower ->
    let morph_bound morph =
      if lower then mlower obj morph else mupper obj morph
    in
    match mode with
    | Amode (value, _, _) -> value
    | Amodevar morph -> morph_bound morph
    | Amodejoin (value, _, morphs) ->
      VarMap.fold
        (fun _ morph value -> C.join obj value (morph_bound morph))
        morphs value
    | Amodemeet (value, _, morphs) ->
      VarMap.fold
        (fun _ morph value -> C.meet obj value (morph_bound morph))
        morphs value

  let exact_submode : type a l r.
      log:change list ref option ->
      a C.obj ->
      (a, allowed * r) mode ->
      (a, l * allowed) mode ->
      (unit, a exact_failure) result option =
   fun ~log obj left right ->
    let rigid =
      exact_mode_has
        { check =
            (fun v ->
              match quantifier_of_var v with
              | Universal _ -> true
              | Outer | Existential _ -> false)
        }
        left
      || exact_mode_has
           { check =
               (fun v ->
                 match quantifier_of_var v with
                 | Universal _ -> true
                 | Outer | Existential _ -> false)
           }
           right
    in
    let owned =
      exact_mode_has { check = (fun v -> v.exact <> []) } left
      || exact_mode_has { check = (fun v -> v.exact <> []) } right
    in
    if not (rigid || owned)
    then None
    else begin
      let lower = exact_static_bound obj left ~lower:true in
      let upper = exact_static_bound obj right ~lower:false in
      if not (C.le obj lower upper)
      then
        Some
          (Error
             (Exact_inequality
                { left = lower;
                  left_hint = Comp_hint.Unknown lower;
                  right = upper;
                  right_hint = Comp_hint.Unknown upper
                }))
      else
        match
          exact_snapshot (exact_mode_vars obj left @ exact_mode_vars obj right)
        with
        | None -> raise Exact_resource_limit
        | Some component -> (
          let members = component.members in
          match component.symbolic with
          | Some symbolic ->
            let manager = symbolic.manager in
            Solver_mdd.compact manager [symbolic.guard; symbolic.witness];
            let atom =
              exact_symbolic_mode_atom component manager obj left right
            in
            Solver_mdd.compact manager [symbolic.guard; symbolic.witness; atom];
            let witness = Solver_mdd.and_ manager symbolic.witness atom in
            let replacement = exact_symbolic_restrict component witness in
            let replacement_symbolic = Option.get replacement.symbolic in
            if Solver_mdd.is_false replacement_symbolic.guard
            then
              let fallback =
                Solver_mdd.counterexample manager symbolic.witness atom
              in
              let failing, unavoidable =
                try
                  let unavoidable =
                    List.filter_map
                      (fun bound ->
                        let premise =
                          exact_symbolic_mode_bound component obj left
                            ~lower:true bound
                        in
                        let conclusion =
                          exact_symbolic_mode_bound component obj right
                            ~lower:true bound
                        in
                        let clause =
                          Solver_mdd.or_ manager
                            (Solver_mdd.not_ manager premise)
                            conclusion
                        in
                        let candidate =
                          Solver_mdd.and_ manager symbolic.witness clause
                        in
                        let good =
                          exact_symbolic_good_outer component candidate
                        in
                        if
                          Solver_mdd.is_false
                            (Solver_mdd.and_ manager symbolic.guard good)
                        then Some (bound, clause)
                        else None)
                      (fst (exact_irreducibles obj))
                  in
                  let unavoidable =
                    match unavoidable with
                    | [] -> None
                    | (bound, clause) :: rest ->
                      Some
                        (List.fold_left
                           (fun (bound, clause) (next_bound, next_clause) ->
                             ( C.join obj bound next_bound,
                               Solver_mdd.and_ manager clause next_clause ))
                           (bound, clause) rest)
                  in
                  let failing_atom =
                    match unavoidable with
                    | None -> atom
                    | Some (_, clause) -> clause
                  in
                  let failing =
                    Solver_mdd.and_ manager symbolic.witness
                      (Solver_mdd.not_ manager failing_atom)
                  in
                  let failing =
                    match unavoidable with
                    | None -> Solver_mdd.find_sat manager failing
                    | Some (bound, _) -> (
                      let full_bound =
                        exact_symbolic_mode_bound component obj left ~lower:true
                          bound
                      in
                      match
                        Solver_mdd.find_sat manager
                          (Solver_mdd.and_ manager failing full_bound)
                      with
                      | Some _ as assignment -> assignment
                      | None -> Solver_mdd.find_sat manager failing)
                  in
                  failing, unavoidable
                with Solver_mdd.Limit -> fallback, None
              in
              Some
                (Error
                   (match failing with
                   | None -> Exact_inconsistent
                   | Some assignment ->
                     let row = exact_symbolic_row component assignment in
                     let a =
                       match unavoidable with
                       | None -> eval_exact_mode obj members row left
                       | Some (bound, _) ->
                         C.meet obj bound (eval_exact_mode obj members row left)
                     in
                     let b = eval_exact_mode obj members row right in
                     Exact_inequality
                       { left = a;
                         left_hint = Comp_hint.Unknown a;
                         right = b;
                         right_hint = Comp_hint.Unknown b
                       }))
            else begin
              exact_materialize_fixed ~log replacement;
              let replacement = exact_without_fixed replacement in
              List.iter
                (fun (Var (obj, v)) ->
                  (match log with
                  | None -> ()
                  | Some log -> log := Cexact (v, v.exact) :: !log);
                  v.exact
                    <- (if
                          C.le obj v.upper v.lower
                          || not (exact_owns replacement v)
                        then []
                        else [replacement]))
                members;
              Some (Ok ())
            end
          | None ->
            let satisfies row =
              C.le obj
                (eval_exact_mode obj members row left)
                (eval_exact_mode obj members row right)
            in
            let witness_rows = List.filter satisfies component.witness_rows in
            let outer, good_outer_groups =
              exact_good_outer_groups component witness_rows
            in
            let good_outer_row row =
              List.exists
                (fun rows -> exact_same_on outer members (List.hd rows) row)
                good_outer_groups
            in
            let guard_rows = List.concat good_outer_groups in
            let witness_rows = List.filter good_outer_row witness_rows in
            if guard_rows = []
            then begin
              let failing =
                List.find_opt
                  (fun row -> not (satisfies row))
                  component.witness_rows
              in
              Some
                (Error
                   (match failing with
                   | None -> Exact_inconsistent
                   | Some row ->
                     let a = eval_exact_mode obj members row left in
                     let b = eval_exact_mode obj members row right in
                     Exact_inequality
                       { left = a;
                         left_hint = Comp_hint.Unknown a;
                         right = b;
                         right_hint = Comp_hint.Unknown b
                       }))
            end
            else begin
              let component = { component with guard_rows; witness_rows } in
              exact_materialize_fixed ~log component;
              let component = exact_without_fixed component in
              List.iter
                (fun (Var (obj, v)) ->
                  (match log with
                  | None -> ()
                  | Some log -> log := Cexact (v, v.exact) :: !log);
                  v.exact
                    <- (if C.le obj v.upper v.lower then [] else [component]))
                members;
              Some (Ok ())
            end)
    end

  let exact_mode_envelope : type a l r.
      a C.obj -> (a, l * r) mode -> (a * a) option =
   fun obj mode ->
    let owned = exact_mode_has { check = (fun v -> v.exact <> []) } mode in
    if not owned
    then None
    else
      match exact_snapshot (exact_mode_vars obj mode) with
      | None -> raise Exact_resource_limit
      | Some component -> (
        let rigid =
          exact_mode_has
            { check =
                (fun v ->
                  match quantifier_of_var v with
                  | Universal _ -> true
                  | Outer | Existential _ -> false)
            }
            mode
        in
        match component.symbolic with
        | Some symbolic ->
          let manager = symbolic.manager in
          Solver_mdd.compact manager [symbolic.guard; symbolic.witness];
          let relation = if rigid then symbolic.guard else symbolic.witness in
          if Solver_mdd.is_false relation
          then Misc.fatal_error "Solver: empty exact component";
          let joins, meets = exact_irreducibles obj in
          let entailed lower value =
            let bound =
              exact_symbolic_mode_bound component obj mode ~lower value
            in
            Solver_mdd.entails manager relation bound
          in
          let lower =
            List.fold_left
              (fun lower value ->
                if entailed true value then C.join obj lower value else lower)
              (C.min obj) joins
          in
          let upper =
            List.fold_left
              (fun upper value ->
                if entailed false value then C.meet obj upper value else upper)
              (C.max obj) meets
          in
          Some (lower, upper)
        | None -> (
          let rows =
            if rigid then component.guard_rows else component.witness_rows
          in
          let envelope =
            List.fold_left
              (fun acc row ->
                let value = eval_exact_mode obj component.members row mode in
                match acc with
                | None -> Some (value, value)
                | Some (lower, upper) ->
                  Some (C.meet obj lower value, C.join obj upper value))
              None rows
          in
          match envelope with
          | Some envelope -> Some envelope
          | None -> Misc.fatal_error "Solver: empty exact component"))

  let reclassify_exact_relation component =
    let members = component.members in
    let replacement =
      match component.symbolic with
      | Some symbolic ->
        let manager = symbolic.manager in
        let projected =
          List.fold_left
            (fun relation (Var (_, v)) ->
              match quantifier_of_var v with
              | Existential _ ->
                Solver_mdd.exists manager
                  (exact_member_index members v)
                  relation
              | Outer | Universal _ -> relation)
            symbolic.witness members
        in
        let guard = Solver_mdd.and_ manager symbolic.guard projected in
        { component with symbolic = Some { symbolic with guard } }
      | None ->
        let selected =
          List.filter
            (fun (Var (_, v)) ->
              match quantifier_of_var v with
              | Outer | Universal _ -> true
              | Existential _ -> false)
            members
        in
        let guard_rows =
          List.filter
            (fun guard_row ->
              List.exists
                (exact_same_on selected members guard_row)
                component.witness_rows)
            component.guard_rows
        in
        { component with guard_rows }
    in
    replacement

  let () =
    prepare_copied_component
      := fun (rigidified, component) ->
           exact_without_fixed
             (if rigidified
              then reclassify_exact_relation component
              else component)

  let reclassify_exact_component ~log component =
    let members = component.members in
    let replacement = reclassify_exact_relation component in
    List.iter
      (fun (Var (_, v)) ->
        (match log with
        | None -> ()
        | Some log -> log := Cexact (v, v.exact) :: !log);
        v.exact
          <- List.map
               (fun existing ->
                 if existing == component then replacement else existing)
               v.exact)
      members

  let update_level_exact_v : type a. log:_ -> a C.obj -> int -> a var -> unit =
   fun ~log obj level v ->
    let old_level = v.level in
    let components = v.exact in
    update_level_v ~log obj level v;
    if old_level > rigid_level && v.level <= rigid_level
    then List.iter (reclassify_exact_component ~log) components

  let update_level (type a l r) (level : int) (obj : a C.obj)
      (a : (a, l * r) mode) ~log =
    match a with
    | Amodevar (Amorphvar (v, f, _)) ->
      let obj = C.src obj f in
      update_level_exact_v ~log obj level v
    | Amode _ -> ()
    | Amodejoin (_, _, mvs) ->
      VarMap.iter
        (fun _ (Amorphvar (v, f, _)) ->
          let obj = C.src obj f in
          update_level_exact_v ~log obj level v)
        mvs
    | Amodemeet (_, _, mvs) ->
      VarMap.iter
        (fun _ (Amorphvar (v, f, _)) ->
          let obj = C.src obj f in
          update_level_exact_v ~log obj level v)
        mvs

  let submode (type a r l) (pp : H.Pinpoint.t) (obj : a C.obj)
      (a : (a, allowed * r) mode) (b : (a, l * allowed) mode) ~log =
    match exact_submode ~log obj a b with
    | Some (Ok ()) -> Ok ()
    | Some (Error (Exact_inequality error)) -> Error error
    | Some (Error Exact_inconsistent) -> raise Exact_inconsistent_state
    | None -> (
      let submode_cc ~log:_ _pp obj left left_hint right right_hint =
        if C.le obj left right
        then Ok ()
        else Error { left; left_hint; right; right_hint }
      in
      let submode_mvc ~log pp obj v right right_hint =
        Result.map_error
          (fun (left, left_hint) -> { left; left_hint; right; right_hint })
          (submode_mvc ~allow_rigid:false ~log pp obj v right right_hint)
      in
      let submode_cmv ~log pp obj left left_hint v =
        Result.map_error
          (fun (right, right_hint) -> { left; left_hint; right; right_hint })
          (submode_cmv ~allow_rigid:false ~log pp obj left left_hint v)
      in
      let submode_mvmv ~log pp obj v u =
        Result.map_error
          (fun (left, left_hint, right, right_hint) ->
            { left; left_hint; right; right_hint })
          (submode_mvmv ~allow_rigid:false ~log pp obj v u)
      in
      match a, b with
      | ( Amode (left, left_hint_lower, _left_hint_upper),
          Amode (right, _right_hint_lower, right_hint_upper) ) ->
        submode_cc ~log pp obj left
          (Comp_hint.disallow_right left_hint_lower)
          right
          (Comp_hint.disallow_left right_hint_upper)
      | Amodevar v, Amode (right, _right_hint_lower, right_hint_upper) ->
        submode_mvc ~log pp obj v right
          (Comp_hint.disallow_left right_hint_upper)
      | Amode (left, left_hint_lower, _left_hint_upper), Amodevar v ->
        submode_cmv ~log pp obj left
          (Comp_hint.disallow_right left_hint_lower)
          v
      | Amodevar v, Amodevar u -> submode_mvmv ~log pp obj v u
      | Amode (a, a_hint_lower, _a_hint_upper), Amodemeet (b, b_hint, mvs) ->
        Result.bind
          (submode_cc ~log pp obj a
             (Comp_hint.disallow_right a_hint_lower)
             b b_hint)
          (fun () ->
            find_error
              (fun mv ->
                submode_cmv ~log pp obj a
                  (Comp_hint.disallow_right a_hint_lower)
                  mv)
              mvs)
      | Amodevar mv, Amodemeet (b, b_hint, mvs) ->
        Result.bind (submode_mvc ~log pp obj mv b b_hint) (fun () ->
            find_error (fun mv' -> submode_mvmv ~log pp obj mv mv') mvs)
      | Amodejoin (a, a_hint, mvs), Amode (b, _b_hint_lower, b_hint_upper) ->
        Result.bind
          (submode_cc ~log pp obj a a_hint b
             (Comp_hint.disallow_left b_hint_upper))
          (fun () ->
            find_error
              (fun mv' ->
                submode_mvc ~log pp obj mv' b
                  (Comp_hint.disallow_left b_hint_upper))
              mvs)
      | Amodejoin (a, a_hint, mvs), Amodevar mv ->
        Result.bind (submode_cmv ~log pp obj a a_hint mv) (fun () ->
            find_error (fun mv' -> submode_mvmv ~log pp obj mv' mv) mvs)
      | Amodejoin (a, a_hint, mvs), Amodemeet (b, b_hint, mus) ->
        (* TODO: mabye create a intermediate variable? *)
        Result.bind (submode_cc ~log pp obj a a_hint b b_hint) (fun () ->
            Result.bind
              (find_error (fun mv -> submode_mvc ~log pp obj mv b b_hint) mvs)
              (fun () ->
                Result.bind
                  (find_error
                     (fun mu -> submode_cmv ~log pp obj a a_hint mu)
                     mus)
                  (fun () ->
                    find_error
                      (fun mu ->
                        find_error
                          (fun mv -> submode_mvmv ~log pp obj mv mu)
                          mvs)
                      mus))))

  let submode_detailed pp obj left right ~log =
    try
      match submode pp obj left right ~log with
      | Ok () -> Ok ()
      | Error error -> Error (Inequality error)
    with
    | Exact_inconsistent_state -> Error Inconsistent_relation
    | Exact_resource_limit | Solver_mdd.Limit -> Error Resource_limit

  let populate_hint obj a hint =
    let ahint = Comp_hint.populate obj hint in
    assert (Misc.Le_result.equal ~le:(C.le obj) (fst ahint) a);
    ahint

  let populate_error obj { left; left_hint; right; right_hint } =
    let left = populate_hint obj left left_hint in
    let right = populate_hint obj right right_hint in
    { left; right }

  let add_morphvar dst x xs = VarMap.add (get_key dst x) x xs

  let union_morphvars t0 t1 = VarMap.union (fun _ a _b -> Some a) t0 t1

  let join (type a r) obj l =
    let rec loop :
        a ->
        (a, allowed * disallowed) Comp_hint.t ->
        (a, allowed * disallowed) morphvar VarMap.t ->
        (a, allowed * r) mode list ->
        (a, allowed * disallowed) mode =
     fun a a_hint_lower mvs rest ->
      if C.le obj (C.max obj) a
      then
        (* In this case, [a] is the maximum element, and we can use
           [a_hint_lower] as the output's lower hint *)
        Amode (a, a_hint_lower, Max)
      else
        match rest with
        | [] -> Amodejoin (a, a_hint_lower, mvs)
        | mv :: xs -> (
          match disallow_right mv with
          | Amode (b, b_hint_lower, _b_hint_upper) ->
            loop (C.join obj a b)
              (hint_join obj a a_hint_lower b
                 (Comp_hint.disallow_right b_hint_lower))
              mvs xs
          (* some minor optimization: if [a] is lower than [mlower mv], we
              should keep the latter instead. This helps to fail early in
             [submode] *)
          | Amodevar mv ->
            let mvlower = mlower obj mv in
            loop (C.join obj a mvlower)
              (hint_join obj a a_hint_lower mvlower (mlower_hint mv))
              (add_morphvar obj mv mvs) xs
          | Amodejoin (b, b_hint, mvs') ->
            loop (C.join obj a b)
              (hint_join obj a a_hint_lower b b_hint)
              (union_morphvars mvs' mvs) xs)
    in
    loop (C.min obj) Min VarMap.empty l

  let meet (type a l) obj l =
    let rec loop :
        a ->
        (a, disallowed * allowed) Comp_hint.t ->
        (a, disallowed * allowed) morphvar VarMap.t ->
        (a, l * allowed) mode list ->
        (a, disallowed * allowed) mode =
     fun a a_hint_upper mvs rest ->
      if C.le obj a (C.min obj)
      then
        (* In this case, [a] is the minimum element, and we can use [a_hint_upper]
           as the output's upper hint *)
        Amode (a, Min, a_hint_upper)
      else
        match rest with
        | [] -> Amodemeet (a, a_hint_upper, mvs)
        | mv :: xs -> (
          match disallow_left mv with
          | Amode (b, _b_hint_lower, b_hint_upper) ->
            loop (C.meet obj a b)
              (hint_meet obj a a_hint_upper b
                 (Comp_hint.disallow_left b_hint_upper))
              mvs xs
            (* some minor optimization: if [a] is higher than [mupper mv], we should keep the latter instead. This helps to fail early in [submode_log] *)
          | Amodevar mv ->
            let mvupper = mupper obj mv in
            loop (C.meet obj a mvupper)
              (hint_meet obj a a_hint_upper mvupper (mupper_hint mv))
              (add_morphvar obj mv mvs) xs
          | Amodemeet (b, b_hint, mvs') ->
            loop (C.meet obj a b)
              (hint_meet obj a a_hint_upper b b_hint)
              (union_morphvars mvs' mvs) xs)
    in
    loop (C.max obj) Max VarMap.empty l

  let get_loose_ceil : type a l r. a C.obj -> (a, l * r) mode -> a =
   fun obj m ->
    match exact_mode_envelope obj m with
    | Some (_, upper) -> upper
    | None -> (
      match m with
      | Amode (a, _a_hint_lower, _a_hint_upper) -> a
      | Amodevar mv -> mupper obj mv
      | Amodemeet (a, _a_hint, mvs) ->
        VarMap.fold (fun _ mv acc -> C.meet obj acc (mupper obj mv)) mvs a
      | Amodejoin (a, _a_hint, mvs) ->
        VarMap.fold (fun _ mv acc -> C.join obj acc (mupper obj mv)) mvs a)

  let get_loose_floor : type a l r. a C.obj -> (a, l * r) mode -> a =
   fun obj m ->
    match exact_mode_envelope obj m with
    | Some (lower, _) -> lower
    | None -> (
      match m with
      | Amode (a, _a_hint_lower, _a_hint_upper) -> a
      | Amodevar mv -> mlower obj mv
      | Amodejoin (a, _a_hint, mvs) ->
        VarMap.fold (fun _ mv acc -> C.join obj acc (mlower obj mv)) mvs a
      | Amodemeet (a, _a_hint, mvs) ->
        VarMap.fold (fun _ mv acc -> C.meet obj acc (mlower obj mv)) mvs a)

  let exact_zap : type a l r.
      a C.obj ->
      (a, l * r) mode ->
      toward:[`Floor | `Ceil] ->
      log:change list ref option ->
      a option =
   fun obj mode ~toward ~log ->
    if not (exact_mode_has { check = (fun v -> v.exact <> []) } mode)
    then None
    else
      match exact_snapshot (exact_mode_vars obj mode) with
      | None -> raise Exact_resource_limit
      | Some component -> (
        let members = component.members in
        match component.symbolic with
        | Some symbolic -> (
          let manager = symbolic.manager in
          Solver_mdd.compact manager [symbolic.guard; symbolic.witness];
          let candidate value =
            let atom =
              Solver_mdd.and_ manager
                (exact_symbolic_mode_bound component obj mode ~lower:true value)
                (exact_symbolic_mode_bound component obj mode ~lower:false value)
            in
            let witness = Solver_mdd.and_ manager symbolic.witness atom in
            let replacement = exact_symbolic_restrict component witness in
            if Solver_mdd.is_false (Option.get replacement.symbolic).guard
            then None
            else Some replacement
          in
          let chosen =
            List.fold_left
              (fun chosen value ->
                let better =
                  match chosen with
                  | None -> true
                  | Some (previous, _) -> (
                    match toward with
                    | `Floor -> C.le obj value previous
                    | `Ceil -> C.le obj previous value)
                in
                if not better
                then chosen
                else
                  match candidate value with
                  | None -> chosen
                  | Some component -> Some (value, component))
              None (C.elements obj)
          in
          match chosen with
          | None -> raise Exact_zap_impossible
          | Some (value, replacement) ->
            (match log with
            | None -> ()
            | Some _ -> exact_materialize_fixed ~log replacement);
            let replacement = exact_without_fixed replacement in
            (match log with
            | None -> ()
            | Some log ->
              List.iter
                (fun (Var (obj, v)) ->
                  log := Cexact (v, v.exact) :: !log;
                  v.exact
                    <- (if
                          C.le obj v.upper v.lower
                          || not (exact_owns replacement v)
                        then []
                        else [replacement]))
                members);
            Some value)
        | None -> (
          let candidate value =
            let witness_rows =
              List.filter
                (fun row ->
                  let actual = eval_exact_mode obj members row mode in
                  C.le obj actual value && C.le obj value actual)
                component.witness_rows
            in
            let outer, good_outer_groups =
              exact_good_outer_groups component witness_rows
            in
            let guard_rows = List.concat good_outer_groups in
            if guard_rows = []
            then None
            else
              let witness_rows =
                List.filter
                  (fun row ->
                    List.exists (exact_same_on outer members row) guard_rows)
                  witness_rows
              in
              Some { component with guard_rows; witness_rows }
          in
          let chosen =
            List.fold_left
              (fun chosen value ->
                match candidate value, chosen with
                | None, _ -> chosen
                | Some component, None -> Some (value, component)
                | Some component, Some (previous, _) ->
                  let better =
                    match toward with
                    | `Floor -> C.le obj value previous
                    | `Ceil -> C.le obj previous value
                  in
                  if better then Some (value, component) else chosen)
              None (C.elements obj)
          in
          match chosen with
          | None -> raise Exact_zap_impossible
          | Some (value, component) ->
            (match log with
            | None -> ()
            | Some _ -> exact_materialize_fixed ~log component);
            let component = exact_without_fixed component in
            (match log with
            | None -> ()
            | Some log ->
              List.iter
                (fun (Var (obj, v)) ->
                  log := Cexact (v, v.exact) :: !log;
                  v.exact
                    <- (if C.le obj v.upper v.lower then [] else [component]))
                members);
            Some value))

  (** Zaps a morphvar to its floor and returns the floor. [commit] could be
      [Some log], in which case the zapping is appended to [log]; it could also
      be [None], in which case the zapping is reverted. The latter is useful
      when the caller only wants to know the floor without zapping. *)
  let zap_to_floor_morphvar obj mv ~commit =
    let log_, lower = zap_to_floor_morphvar_aux obj mv in
    (match commit with
    | None -> undo_changes log_
    | Some log -> log := append_changes !log log_);
    lower

  let zap_to_floor : type a r. a C.obj -> (a, allowed * r) mode -> log:_ -> a =
   fun obj m ~log ->
    match exact_zap obj m ~toward:`Floor ~log with
    | Some value -> value
    | None -> (
      match m with
      | Amode (a, _a_hint_lower, _a_hint_upper) -> a
      | Amodevar mv -> zap_to_floor_morphvar obj mv ~commit:log
      | Amodejoin (a, _a_hint, mvs) ->
        let floor =
          VarMap.fold
            (fun _ mv acc ->
              C.join obj acc (zap_to_floor_morphvar obj mv ~commit:None))
            mvs a
        in
        VarMap.iter
          (fun _ mv ->
            (* We want a hint for why [floor] is low. However, we only have hint
             for why [floor] is high. There is no hint to use. *)
            submode_mvc ~allow_rigid:true H.Pinpoint.unknown obj mv floor
              (Unknown floor) ~log
            |> Result.get_ok)
          mvs;
        floor)

  (** Zaps a morphvar to its ceiling and returns the ceiling. [commit] could be
      [Some log], in which case the zapping is appended to [log]; it could also
      be [None], in which case the zapping is reverted. The latter is useful
      when the caller only wants to know the ceiling without zapping. *)
  let zap_to_ceil_morphvar obj mv ~commit =
    let log_, upper = zap_to_ceil_morphvar_aux obj mv in
    (match commit with
    | None -> undo_changes log_
    | Some log -> log := append_changes !log log_);
    upper

  let zap_to_ceil : type a l. a C.obj -> (a, l * allowed) mode -> log:_ -> a =
   fun obj m ~log ->
    match exact_zap obj m ~toward:`Ceil ~log with
    | Some value -> value
    | None -> (
      match m with
      | Amode (a, _a_hint_lower, _a_hint_upper) -> a
      | Amodevar mv -> zap_to_ceil_morphvar obj mv ~commit:log
      | Amodemeet (a, _a_hint, mvs) ->
        let ceil =
          VarMap.fold
            (fun _ mv acc ->
              C.meet obj acc (zap_to_ceil_morphvar obj mv ~commit:None))
            mvs a
        in
        VarMap.iter
          (fun _ mv ->
            let ok =
              submode_cmv ~allow_rigid:true H.Pinpoint.unknown obj ceil
                (Unknown ceil) mv ~log
            in
            assert (Result.is_ok ok))
          mvs;
        ceil)

  let get_floor : type a r. a C.obj -> (a, allowed * r) mode -> a =
   fun obj m ->
    match exact_mode_envelope obj m with
    | Some (lower, _) -> lower
    | None -> (
      match m with
      | Amode (a, _a_hint_lower, _a_hint_upper) -> a
      | Amodevar mv -> zap_to_floor_morphvar obj mv ~commit:None
      | Amodejoin (a, _a_hint, mvs) ->
        VarMap.fold
          (fun _ mv acc ->
            C.join obj acc (zap_to_floor_morphvar obj mv ~commit:None))
          mvs a)

  let get_ceil : type a l. a C.obj -> (a, l * allowed) mode -> a =
   fun obj m ->
    match exact_mode_envelope obj m with
    | Some (_, upper) -> upper
    | None -> (
      match m with
      | Amode (a, _a_hint_lower, _a_hint_upper) -> a
      | Amodevar mv -> zap_to_ceil_morphvar obj mv ~commit:None
      | Amodemeet (a, _a_hint, mvs) ->
        VarMap.fold
          (fun _ mv acc ->
            C.meet obj acc (zap_to_ceil_morphvar obj mv ~commit:None))
          mvs a)

  let to_const_exn obj m =
    let floor = get_floor obj m in
    let ceil = get_ceil obj m in
    if C.le obj ceil floor
    then ceil
    else
      Misc.fatal_errorf "mode is not tight: floor = %a, ceil = %a"
        (Fmt.compat (C.print obj))
        floor
        (Fmt.compat (C.print obj))
        ceil

  type zap_failure =
    | No_constant
    | Zap_resource_limit

  let zap_to_floor_detailed obj mode ~log =
    try Ok (zap_to_floor obj mode ~log) with
    | Exact_zap_impossible -> Error No_constant
    | Exact_resource_limit | Solver_mdd.Limit -> Error Zap_resource_limit

  let zap_to_ceil_detailed obj mode ~log =
    try Ok (zap_to_ceil obj mode ~log) with
    | Exact_zap_impossible -> Error No_constant
    | Exact_resource_limit | Solver_mdd.Limit -> Error Zap_resource_limit

  let to_of_const_exn : type a.
      a C.obj -> (a, allowed * allowed) mode -> (a, allowed * allowed) mode =
   fun obj m ->
    match m with
    | Amode _ -> m
    | Amodevar mv ->
      let a = to_const_exn obj m in
      Amode (a, mlower_hint mv, mupper_hint mv)

  let print : type a l r.
      ?verbose:bool -> a C.obj -> Fmt.formatter -> (a, l * r) mode -> unit =
   fun ?verbose (obj : a C.obj) ppf m ->
    let ceil = get_loose_ceil obj m in
    let floor = get_loose_floor obj m in
    if C.le obj ceil floor
    then C.print obj ppf ceil
    else print_raw ?verbose obj ppf m

  let newvar obj level =
    let u = fresh ~level obj in
    Amodevar (Amorphvar (u, C.id, Id))

  let newvar_quantified obj ~level quantifier =
    let u = fresh ~level ~quantifier obj in
    Amodevar (Amorphvar (u, C.id, Id))

  let newvar_above (type a r l s) (obj : a C.obj) (level : int)
      (m : (a, allowed * r) mode) ~log : (a, l * s) mode * bool =
    let result : (a, l * s) mode * bool =
      match disallow_right m with
      | Amode (a, a_hint_lower, _a_hint_upper) ->
        if C.le obj (C.max obj) a
        then Amode (a, Comp_hint.allow_left a_hint_lower, Max), false
        else
          ( Amodevar
              (Amorphvar
                 ( fresh ~lower:a
                     ~lower_hint:(Comp_hint.disallow_right a_hint_lower)
                     ~level obj,
                   C.id,
                   Id )),
            true )
      | Amodevar (Amorphvar (v, _, _) as mv) ->
        if v.level <= level
        then
          (* [~lower] is not precise (because [mlower mv] is not precise), but
         it doesn't need to be *)
          ( Amodevar
              (Amorphvar
                 ( fresh ~lower:(mlower obj mv) ~lower_hint:(mlower_hint mv)
                     ~vlower:(VarMap.singleton (get_key obj mv) mv)
                     ~level obj,
                   C.id,
                   Id )),
            true )
        else
          let u = fresh ~level obj in
          let mu = Amorphvar (u, C.id, Id) in
          let ok =
            submode_mvmv ~allow_rigid:false ~log H.Pinpoint.unknown obj mv mu
          in
          assert (Result.is_ok ok);
          allow_right (Amodevar mu), true
      | Amodejoin (a, a_hint, mvs) ->
        if VarMap.for_all (fun _ (Amorphvar (v, _, _)) -> v.level <= level) mvs
        then
          (* [~lower] is not precise here, but it doesn't need to be *)
          ( Amodevar
              (Amorphvar
                 ( fresh ~lower:a ~lower_hint:a_hint ~vlower:mvs ~level obj,
                   C.id,
                   Id )),
            true )
        else
          let u = fresh ~level obj in
          let mu = Amorphvar (u, C.id, Id) in
          submode_cmv ~allow_rigid:false H.Pinpoint.unknown obj ~log a a_hint mu
          |> Result.get_ok;
          VarMap.iter
            (fun _ mv ->
              let ok =
                submode_mvmv ~allow_rigid:false ~log H.Pinpoint.unknown obj mv
                  mu
              in
              assert (Result.is_ok ok))
            mvs;
          allow_right (Amodevar mu), true
    in
    (match fst result with
    | Amodevar (Amorphvar (v, _, _)) ->
      v.exact <- exact_components (exact_mode_vars obj m) @ v.exact
    | Amode _ | Amodejoin _ | Amodemeet _ -> ());
    result

  let newvar_below (type a l s r) (obj : a C.obj) (level : int)
      (m : (a, l * allowed) mode) ~log : (a, s * r) mode * bool =
    let result : (a, s * r) mode * bool =
      match disallow_left m with
      | Amode (a, _a_hint_lower, a_hint_upper) ->
        if C.le obj a (C.min obj)
        then Amode (a, Min, Comp_hint.allow_right a_hint_upper), false
        else
          ( Amodevar
              (Amorphvar
                 ( fresh ~upper:a
                     ~upper_hint:(Comp_hint.disallow_left a_hint_upper)
                     ~level obj,
                   C.id,
                   Id )),
            true )
      | Amodevar (Amorphvar (v, _, _) as mv) ->
        if v.level < level
        then
          (* [~upper] is not precise (because [mupper mv] is not precise), but
         it doesn't need to be *)
          ( Amodevar
              (Amorphvar
                 ( fresh ~upper:(mupper obj mv) ~upper_hint:(mupper_hint mv)
                     ~vupper:(VarMap.singleton (get_key obj mv) mv)
                     ~level obj,
                   C.id,
                   Id )),
            true )
        else
          let u = fresh ~level obj in
          let mu = Amorphvar (u, C.id, Id) in
          submode_mvmv ~allow_rigid:false H.Pinpoint.unknown obj ~log mu mv
          |> Result.get_ok;
          allow_left (Amodevar mu), true
      | Amodemeet (a, a_hint, mvs) ->
        if VarMap.for_all (fun _ (Amorphvar (v, _, _)) -> v.level < level) mvs
        then
          (* [~upper] is not precise here, but it doesn't need to be *)
          ( Amodevar
              (Amorphvar
                 ( fresh ~upper:a ~upper_hint:a_hint ~vupper:mvs ~level obj,
                   C.id,
                   Id )),
            true )
        else
          let u = fresh ~level obj in
          let mu = Amorphvar (u, C.id, Id) in
          submode_mvc ~allow_rigid:false H.Pinpoint.unknown obj ~log mu a a_hint
          |> Result.get_ok;
          VarMap.iter
            (fun _ mv ->
              submode_mvmv ~allow_rigid:false H.Pinpoint.unknown obj ~log mu mv
              |> Result.get_ok)
            mvs;
          allow_left (Amodevar mu), true
    in
    (match fst result with
    | Amodevar (Amorphvar (v, _, _)) ->
      v.exact <- exact_components (exact_mode_vars obj m) @ v.exact
    | Amode _ | Amodejoin _ | Amodemeet _ -> ());
    result

  (** Exposed description of mode variables, used for printing generic mode
      variables *)
  module Desc = struct
    module Var = struct
      type 'a t = 'a var

      type ('b, 'd) t_with_morph =
        | Amorphvar : 'a t * ('a, 'b, 'd) C.morph -> ('b, 'd) t_with_morph

      module Head = struct
        type 'a t =
          { desc_id : int;
            desc_upper : 'a;
            desc_lower : 'a;
            desc_vlower : ('a, left_only) t_with_morph list;
            desc_level : int
          }

        let equal v1 v2 = v1.desc_id = v2.desc_id

        let hash v = Hashtbl.hash v.desc_id
      end

      let force : 'a C.obj -> 'a var -> 'a Head.t =
       fun obj v ->
        let desc_id = v.id in
        let desc_lower =
          get_floor obj
            (Amodevar (Amorphvar (v, C.id, Comp_hint.Morph_hint.Id)))
        in
        let desc_upper =
          get_ceil obj (Amodevar (Amorphvar (v, C.id, Comp_hint.Morph_hint.Id)))
        in
        (* let desc_lower = v.lower in
          let desc_upper = v.upper in *)
        let desc_vlower =
          let f (Amorphvar (a, f, _) : _ morphvar) = Amorphvar (a, f) in
          let vlower = VarMap.map f v.vlower in
          var_map_to_list vlower
        in
        let desc_level = v.level in
        { desc_id; desc_lower; desc_upper; desc_vlower; desc_level }
    end

    type ('b, 'd) morphvar =
      | Amorphvar : 'a Var.Head.t * ('a, 'b, 'd) C.morph -> ('b, 'd) morphvar

    type ('a, 'd) t =
      | Amode : 'a -> ('a, 'l * 'r) t
      | Amodevar : ('a, 'd) morphvar -> ('a, 'd) t
      | Amodejoin :
          'a * ('a, 'l * disallowed) morphvar list
          -> ('a, 'l * disallowed) t
      | Amodemeet :
          'a * ('a, disallowed * 'r) morphvar list
          -> ('a, disallowed * 'r) t

    let equal_morphvar : type a l r.
        a C.obj -> (a, l * r) morphvar -> (a, l * r) morphvar -> bool =
     fun obj (Amorphvar (v1, m1)) (Amorphvar (v2, m2)) ->
      let c = C.compare_morph obj m1 m2 in
      c = 0 && Var.Head.equal v1 v2

    let equal : type a l r. a C.obj -> (a, l * r) t -> (a, l * r) t -> bool =
     fun obj m1 m2 ->
      match m1, m2 with
      | Amode a1, Amode a2 -> C.equal obj a1 a2
      | Amodevar mv1, Amodevar mv2 -> equal_morphvar obj mv1 mv2
      | Amodejoin (a1, mv1), Amodejoin (a2, mv2) ->
        C.equal obj a1 a2 && List.for_all2 (equal_morphvar obj) mv1 mv2
      | Amodemeet (a1, mv1), Amodemeet (a2, mv2) ->
        C.equal obj a1 a2 && List.for_all2 (equal_morphvar obj) mv1 mv2
      | _, _ -> false

    let print_var : type a. a C.obj -> Fmt.formatter -> a Var.Head.t -> unit =
     fun obj ppf { desc_id; desc_upper; desc_lower; desc_level } ->
      Fmt.fprintf ppf "%x<%d>[%a--%a]" desc_id desc_level (C.print obj)
        desc_lower (C.print obj) desc_upper

    let print_morphvar : type a l r.
        a C.obj -> Fmt.formatter -> (a, l * r) morphvar -> unit =
     fun dst ppf mv ->
      let (Amorphvar (v, f)) = mv in
      let src = C.src dst f in
      Fmt.fprintf ppf "%a(%a)" (C.print_morph dst) f (print_var src) v

    let print : type a l r. a C.obj -> Fmt.formatter -> (a, l * r) t -> unit =
     fun obj ppf m ->
      match m with
      | Amode a -> C.print obj ppf a
      | Amodevar mv -> print_morphvar obj ppf mv
      | Amodejoin (a, mvs) ->
        Fmt.fprintf ppf "join(%a,%a)" (C.print obj) a
          (Fmt.pp_print_list
             ~pp_sep:(fun ppf () -> Fmt.fprintf ppf ",")
             (print_morphvar obj))
          mvs
      | Amodemeet (a, mvs) ->
        Fmt.fprintf ppf "meet(%a,%a)" (C.print obj) a
          (Fmt.pp_print_list
             ~pp_sep:(fun ppf () -> Fmt.fprintf ppf ",")
             (print_morphvar obj))
          mvs
  end

  let desc_morphvar : type a l r.
      a C.obj -> (a, l * r) morphvar -> (a, l * r) Desc.morphvar =
   fun dst (Amorphvar (v, f, _)) ->
    let src = C.src dst f in
    let v = Desc.Var.force src v in
    Desc.Amorphvar (v, f)

  let desc : type a l r. a C.obj -> (a, l * r) mode -> (a, l * r) Desc.t =
   fun dst m ->
    match m with
    | Amode (a, _, _) -> Desc.Amode a
    | Amodevar mv -> Desc.Amodevar (desc_morphvar dst mv)
    | Amodejoin (a, _, mvs) ->
      Desc.Amodejoin (a, var_map_to_list (VarMap.map (desc_morphvar dst) mvs))
    | Amodemeet (a, _, mvs) ->
      Desc.Amodemeet (a, var_map_to_list (VarMap.map (desc_morphvar dst) mvs))
end
[@@inline always]
