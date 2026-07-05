(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "obst3.mli obst3.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Regression (was obmin3): a result-only [compare] refinement
   instantiates and the client add-spec verifies. *)

open Obst3

module IntOrd = struct
  type t = int
  let compare : (x : int) -> (y : int) -> int{ _ >= (-1) && _ <= 1 } =
    fun x y -> if x < y then -1 else if x = y then 0 else 1
end

module IntSet = Make (IntOrd)

let add_ok : (x : int) -> (s : IntSet.t) -> IntSet.t{ mem_s x _ } =
  fun x s -> IntSet.add x s
