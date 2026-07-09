(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/rbt.mli ../lib/rbt.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A client of the verified red-black tree.  [Rbt.t] is a tree carrying
   the full RB invariant [rb] (ordering + no-red-red + equal black
   height); the interface exports [add] and [mem] with specs written
   over the whole-tree membership model, plus the theorem
   [mem_add : mem y (add x t) <-> y = x || mem y t].  The client proves
   membership facts THROUGH those specs, never touching colours,
   rotations, or the balance invariant. *)

open Rbt

(* After inserting [x], it is a member -- the [mem x _] conjunct of
   [add]'s postcondition, read back through [mem]. *)
let add_then_present : (x : int) -> (t : set) -> bool{ _ = true } =
  fun x t ->
    let u = add x t in
    mem x u

(* The full membership characterisation travels to the client: [y] is
   in [add x t] iff it equals [x] or was already present. *)
let mem_add_char
  : (x : int) -> (y : int) -> (t : set) -> bool{ _ = (y = x || mem y t) } =
  fun x y t ->
    let u = add x t in
    mem y u

(* Building a two-element set and observing membership, all discharged
   through the model: 1 and 2 are members, 3 is not. *)
let small : (u : unit) -> bool{ _ = true } =
  fun u ->
    let t0 = empty in
    let t1 = add 1 t0 in
    let t2 = add 2 t1 in
    let has1 = mem 1 t2 in
    let has2 = mem 2 t2 in
    let no3 = mem 3 t2 in
    has1 && has2 && not no3
