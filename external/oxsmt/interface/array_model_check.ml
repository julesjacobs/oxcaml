(* The obligatory in-process model self-check for the ARRAYS theory (QF_AX), the arrays
   analogue of {!Dt_model_check}. Given a candidate model (a [Term.t -> Arr.value]
   assignment extracted at the accepting Final->Sat by {!Oxsmt_arr.Arr.array_model}) and
   the ORIGINAL asserted formula, evaluate every assertion under the model — computing
   [select]/[store]/equality with faithful array semantics, INDEPENDENTLY of the arrays
   solving engine (it consults only the extracted model + the [Array_defs] shape) — and
   return [true] iff all hold. A passing model is a genuine witness, so a satisfiable
   array query becomes a CHECKED sat.

   {b Array semantics.} A [select a i] reads index [i]'s entry in [a]'s finite map (first
   match), or [a]'s default. A [store a i v] overlays [(i,v)] on [a]'s map. Array equality
   is EXTENSIONAL: two arrays are equal iff their defaults are equal and they agree on the
   union of the indices either lists (any other index reads both defaults).

   {b Fail-closed} (soundness): sort-inhabitance is validated first — an [Array]-sorted
   position must hold an [Array] whose keys/values recursively inhabit the index/element
   sorts, a [Bool] position a [Model.Bool], etc. — then any missing binding, type
   confusion, or out-of-fragment term (arithmetic, an applied uninterpreted function)
   raises {!Bad} => [false] => the session degrades to [unknown], never a wrong sat. Never
   an [assert] (release is -noassert). *)

open Oxsmt_core
module Arr = Oxsmt_arr.Arr
module Adefs = Array_defs

exception Bad

type v = Arr.value =
  | Scalar of Model.value
  | Array of
      { entries : (v * v) list
      ; default : v
      }

let mv_eq (a : Model.value) (b : Model.value) =
  match a, b with
  | Model.Int x, Model.Int y -> Bigint.equal x y
  | Model.Bool x, Model.Bool y -> Bool.equal x y
  | Model.Uninterp x, Model.Uninterp y -> Int.equal x y
  | _ -> raise Bad
;;

(* Extensional value equality: scalars by kind; arrays iff their defaults are equal and
   they agree on the union of listed indices (an unlisted index reads both defaults, so
   agreement on the listed union is full function equality for the finite-map+default
   representation). *)
let rec v_eq (a : v) (b : v) =
  match a, b with
  | Scalar x, Scalar y -> mv_eq x y
  | Array a, Array b ->
    v_eq a.default b.default
    && List.for_all
         (fun (k, _) ->
            v_eq (lookup a.entries a.default k) (lookup b.entries b.default k))
         (a.entries @ b.entries)
  | _ -> raise Bad

and lookup (entries : (v * v) list) (default : v) (k : v) : v =
  match
    List.find_opt
      (fun (k', _) ->
         try v_eq k k' with
         | Bad -> false)
      entries
  with
  | Some (_, v) -> v
  | None -> default
;;

let as_bool = function
  | Scalar (Model.Bool b) -> b
  | _ -> raise Bad
;;

let as_array = function
  | Array a -> a.entries, a.default
  | _ -> raise Bad
;;

(* Every model value must INHABIT its term's declared sort. Kills a class of ill-formed
   models (a finite value sort admitted as unbounded, an array cell of the wrong sort). *)
let rec inhabits (sort : Sort.t) (value : v) : bool =
  match sort, value with
  | Sort.Bool, Scalar (Model.Bool _) -> true
  | Sort.Int _, Scalar (Model.Int _) -> true
  | Sort.Uninterpreted _, Scalar (Model.Uninterp _) -> true
  | Sort.Array (index, element), Array a ->
    inhabits element a.default
    && List.for_all (fun (k, v) -> inhabits index k && inhabits element v) a.entries
  | ( ( Sort.Bool
      | Sort.Int _
      | Sort.Uninterpreted _
      | Sort.Array _
      | Sort.Datatype _
      | Sort.BitVec _
      | Sort.Real )
    , _ ) -> false
;;

let ev_with (reg : Adefs.t) (env : v Term.Table.t) =
  let rec ev (t : Term.t) : v =
    match t.Term.node with
    | Term.Bool_const b -> Scalar (Model.Bool b)
    | Term.Int_const n -> Scalar (Model.Int n)
    | Term.Eq (a, b) -> Scalar (Model.Bool (v_eq (ev a) (ev b)))
    | Term.Not a -> Scalar (Model.Bool (not (as_bool (ev a))))
    | Term.And xs ->
      Scalar (Model.Bool (Iarr.fold (fun acc x -> acc && as_bool (ev x)) true xs))
    | Term.Or xs ->
      Scalar (Model.Bool (Iarr.fold (fun acc x -> acc || as_bool (ev x)) false xs))
    | Term.Ite (c, a, b) -> if as_bool (ev c) then ev a else ev b
    | Term.Le _ | Term.Arith _ | Term.Real_const _ | Term.Real_arith _ ->
      raise Bad (* no arithmetic in the QF_AX fragment *)
    | Term.App (sym, args) ->
      let args = Array.of_list (Iarr.to_list args) in
      (match Adefs.role_of_sym reg sym with
       | Some { Adefs.role = Adefs.Select; _ } when Array.length args = 2 ->
         let entries, default = as_array (ev args.(0)) in
         lookup entries default (ev args.(1))
       | Some { Adefs.role = Adefs.Store; _ } when Array.length args = 3 ->
         let entries, default = as_array (ev args.(0)) in
         Array { entries = (ev args.(1), ev args.(2)) :: entries; default }
       | Some _ -> raise Bad
       | None ->
         (* not a select/store: a leaf (nullary) array/index/element variable is read from
            the model; an applied uninterpreted function is out of the v1 fragment *)
         if Array.length args = 0
         then (
           match Term.Table.find_opt env t with
           | Some v -> v
           | None -> raise Bad)
         else raise Bad)
  in
  ev
;;

let build_env (model : (Term.t * Arr.value) list) : v Term.Table.t =
  let env = Term.Table.create 256 in
  List.iter (fun (t, v) -> Term.Table.replace env t v) model;
  env
;;

(* [check reg model assertions] is [true] iff every model value inhabits its term's sort
   AND every assertion evaluates to [Bool true] under the array semantics. Fail-closed. *)
let check reg model assertions =
  if not (List.for_all (fun (t, v) -> inhabits t.Term.sort v) model)
  then false
  else (
    let env = build_env model in
    let ev = ev_with reg env in
    try List.for_all (fun a -> as_bool (ev a)) assertions with
    | Bad -> false)
;;
