(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* gap #31: a KNOWN (transparent, spine-visible) via binder at a value
   binding is registered at its SKELETON sort and injects the FULL
   base-sort predicate ([<inv> _ && <image contract over the map> _]
   instantiated at the binder) -- the same facts a [refine_] unpack
   contributes.  A via value can then be bound by a PLAIN [let] and
   still (a) carry a well-sorted construction fact ([v = Cons ..]),
   (b) be used as a constructor field, (c) flow back into a via result
   by ascription, and (d) be passed as a via ARGUMENT -- where the
   binder's own contract mentions it at the image, its bare skeleton
   stamp is rewritten to the composite map ([lrepr v]).  Abstract
   [refines] (sealed .mli) values are UNCHANGED: they bind at the image
   (lean_via_seal.ml).  refine_ still works; it is redundant at a
   transparent binding. *)

type llist [@@vox.sort lean "LList"]
type tree = Nil | Cons of int * tree
type t = tree{ 0 = 0 } [@vox.via (lrepr : llist)]

[%%vox.lean {lean|
inductive LList where
  | LNil : LList
  | LCons : Int -> LList -> LList

@[grind] def lisnil : LList -> Prop
  | .LNil => True
  | .LCons _ _ => False

@[grind] def lcons (x : Int) (l : LList) : LList := .LCons x l

@[grind] def lapp : LList -> LList -> LList
  | .LNil, m => m
  | .LCons x t, m => .LCons x (lapp t m)

@[grind] def lrepr : Vox_Lean_via_letbind_tree -> LList
  | .Nil => .LNil
  | .Cons x t => .LCons x (lrepr t)
|lean}]

(* SIGHTING (top-level via value by inline constructor).  Under the old
   image-binder its self fact [empty = Nil] was ill-sorted
   [LList = tree], forcing such a value to be defined LAST so the fact
   stayed out of the other functions' scope.  Bound at the skeleton it
   is well sorted, so it may be defined FIRST -- its facts are harmless
   ambient hypotheses. *)
let empty : t{ lisnil _ } = (Nil : t{ lisnil _ })

let cons : (x : int) -> (l : t) -> t{ _ = lcons x l } =
  fun x l ->
    let refine_ t0 = l in
    (Cons (x, t0) : t{ _ = lcons x l })

(* SIGHTING (recursive via-returning helper).  [go] returns the via
   type [t] directly -- NOT the old contortion of a refined SKELETON
   [tree{ lrepr _ = <image eq> }].  The recursive result is bound by a
   PLAIN [let rest = go r] (no [refine_]): [rest] is the payload at the
   skeleton with [lrepr rest = lapp (lrepr r) (lrepr tb)] in context, so
   [Cons (x, rest)] type-checks and its image discharges the contract. *)
let append : (a : t) -> (b : t) -> t{ _ = lapp a b } =
  fun a b ->
    let refine_ ta = a in
    let refine_ tb = b in
    let rec go : (u : tree) -> t{ _ = lapp (lrepr u) (lrepr tb) } =
      fun u ->
        match u with
        | Nil -> (tb : t{ _ = lapp (lrepr u) (lrepr tb) })
        | Cons (x, r) ->
          let rest = go r in
          (Cons (x, rest) : t{ _ = lapp (lrepr u) (lrepr tb) })
    in
    let res = go ta in
    (res : t{ _ = lapp a b })

(* SIGHTING (plain-let value fed as a via ARGUMENT).  [once] and the
   intermediate [r] are plain-let via values fed to [cons]; the callee's
   contract [lcons x l] mentions the parameter at the image, so each
   binder's dependent occurrence is rewritten [once] -> [lrepr once]. *)
let twice : (x : int) -> (l : t) -> t{ _ = lcons x (lcons x l) } =
  fun x l ->
    let once = cons x l in
    let r = cons x once in
    (r : t{ _ = lcons x (lcons x l) })
