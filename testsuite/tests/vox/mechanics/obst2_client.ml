(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "obst2.mli obst2.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Regression (was obmin2): an unspecced [compare] instantiates and the
   client add-spec verifies -- the arrow has no binder, so freshening
   does nothing. *)

open Obst2

module IntOrd = struct
  type t = int
  let compare : (x : int) -> (y : int) -> int = fun x y -> Stdlib.compare (x : int) y
end

module IntSet = Make (IntOrd)

let add_ok : (x : int) -> (s : IntSet.t) -> IntSet.t{ mem_s x _ } =
  fun x s -> IntSet.add x s
