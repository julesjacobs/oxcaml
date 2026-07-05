(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/mset_lib.mli ../lib/mset_lib.ml ../lib/mset.mli ../lib/mset.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* BORROWS MEET VIA: a MUTABLE set behind a sealed [refines (iset)]
   interface.  mset_lib is a RustHorn borrow library whose loan ghosts
   ([snow]/[sfin]) live at the SET image sort ([sinsert]'s spec is
   [snow _ = ins x (snow m)] -- a borrow spec in pure set vocabulary).
   mset seals it: [type t : value refines (iset)] over
   [varr{ .. } via (setof : iset)], implementing IN-PLACE ops that
   PROVE their set-vocab contracts with ZERO assume_unchecked_ (the
   trust boundary is mset_lib's six functions).  This client sees ONLY
   the model (ISet/mem/ins/card) and the ops (Mset); the mutable
   carrier, the abstraction [setof], and the whole borrow discipline
   are invisible.  Two in-place inserts are chained and facts about
   the FINAL contents are proved THROUGH the abstraction. *)

open Mset_lib
open Mset

(* Membership of the FIRST-inserted element survives the second
   in-place insert: [mem x (ins y (ins x (create ())))] holds. *)
let mem_after_two : (x : int) -> (y : int) -> bool{ _ = true } =
  fun x y ->
    let s0 = create () in
    let s1 = insert x s0 in
    let s2 = insert y s1 in
    let (b, _s3) = member x s2 in
    b

(* The count tracks both in-place inserts through the abstraction (ins
   always grows the model): card after two inserts on empty is 2. *)
let card_after_two : (x : int) -> (y : int) -> unit =
  fun x y ->
    let s0 = create () in
    let s1 = insert x s0 in
    let s2 = insert y s1 in
    let _ : t{ card _ = 2 } = s2 in
    ()

(* A single in-place insert, then read back: [member x] on the result
   sees [mem x (ins x s)] -- the mutation's effect, proved in pure set
   vocabulary with no view of the representation. *)
let roundtrip : (x : int) -> (s : t) @ unique -> bool{ _ = true } =
  fun x s ->
    let s1 = insert x s in
    let (b, _s2) = member x s1 in
    b
