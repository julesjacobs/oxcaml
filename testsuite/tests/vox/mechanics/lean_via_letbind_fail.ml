(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* gap #31 SOUNDNESS: a plain-let via value's injected facts must not
   OVER-approximate.  [inner] honestly denotes [lcons y z]; the final
   ascription claims [lcons x (lcons x z)] ([x], not [y]).  The
   well-sorted skeleton facts flow (no ill-sorted [LList = tree]), but
   the false image equation is REFUTED at grind -- a genuine proof
   failure, not a silent pass and not an elaboration error. *)

type llist [@@vox.sort lean "LList"]
type tree = Nil | Cons of int * tree
type t = tree{ 0 = 0 } [@vox.via (lrepr : llist)]

[%%vox.lean {lean|
inductive LList where
  | LNil : LList
  | LCons : Int -> LList -> LList

@[grind] def lcons (x : Int) (l : LList) : LList := .LCons x l

@[grind] def lrepr : Vox_Lean_via_letbind_fail_tree -> LList
  | .Nil => .LNil
  | .Cons x t => .LCons x (lrepr t)
|lean}]

let cons : (x : int) -> (l : t) -> t{ _ = lcons x l } =
  fun x l ->
    let refine_ t0 = l in
    (Cons (x, t0) : t{ _ = lcons x l })

let bad : (x : int) -> (y : int) -> (z : t) -> t{ _ = lcons x (lcons x z) } =
  fun x y z ->
    let refine_ tz = z in
    let inner = (Cons (y, tz) : t{ _ = lcons y z }) in
    (Cons (x, inner) : t{ _ = lcons x (lcons x z) })
