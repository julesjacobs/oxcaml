(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/xset.mli ../lib/xset.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* EXTENSIONAL [via] across a module boundary.  [xset] is [via_set] with
   an [Int -> Prop] model instead of an inductive list: the same sealed
   story (client binds [Xset.t] at the set sort, imports the model from
   the unit's VoxSig, proves through the abstraction with no view of the
   tree), plus the headline the list model cannot state -- a set-level
   EQUALITY through the abstraction.  See lib/xset.mli for the trade-off
   against lib/via_set.mli (the inductive, decidable-observable end). *)

open Xset

(* Same roundtrip as lean_via_seal.ml: [add x s = ins x s], [member x] on
   it reads [mem x (ins x s)]. *)
let roundtrip : (x : int) -> (s : t) -> bool{ _ = mem x (ins x s) } =
  fun x s ->
    let u = add x s in
    member x u

(* THE EXTENSIONALITY PAYOFF: adding [x] twice denotes the same set as
   adding it once.  [u = add x s] has model [ins x s]; [add x u] has
   model [ins x (ins x s)]; the result spec [_ = ins x s] then demands
   [ins x (ins x s) = ins x s], which the exported [ins_idem] closes.
   Under [via_set]'s inductive model this goal is FALSE
   ([cons x (cons x s) <> cons x s]) -- only the extensional model proves
   it, and the client sees the equality purely through set vocabulary. *)
let add_twice : (x : int) -> (s : t) -> t{ _ = ins x s } =
  fun x s ->
    let u = add x s in
    add x u

(* CROSS-UNIT 0-ARY CONSTANT: the client references [emp] -- a bare
   0-ary spec constant exported by Xset's block through its VoxSig --
   directly in a refinement.  [empty] denotes [emp], so [member 0 empty]
   reads [mem 0 emp], which the exported [mem_emp] refutes. *)
let empty_has_no_members : unit -> bool{ _ = mem 0 emp } =
  fun () -> member 0 empty
