(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "obst.mli obst.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Gap C acceptance: instantiate the cross-unit sealed functor [Obst.Make]
   at [int].  [IntOrd.compare]'s element-mentioning contract is discharged
   HONESTLY (no assumption), and the client proves a membership fact through
   the sealed abstraction.  The [Make (IntOrd)] coercion pairs the client's
   [compare] arrow against the .cmi-imported [ORD.compare] arrow; before the
   binder-freshening fix their [Scoped] stamps collided across units and the
   pairing matched the wrong partner, spuriously rejecting the argument. *)

open Obst

module IntOrd = struct
  type t = int
  let compare : (x : int) -> (y : int) -> int{ (_ = 0 -> x = y) } =
    fun x y -> if x < y then -1 else if x = y then 0 else 1
end

module IntSet = Make (IntOrd)

let member_after_add : (x : int) -> (s : IntSet.t) -> IntSet.t{ mem_s x _ } =
  fun x s -> IntSet.add x s
