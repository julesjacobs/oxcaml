(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "obst.mli obst.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Gap C soundness: an argument whose [compare] VIOLATES the ORDERED
   contract is refused at its own verification condition.  Freshening
   imported binders must not let a dishonest instance slip through: the
   contract still fires, and [fun _ _ -> 0] cannot prove [_ = 0 -> x = y]
   (it returns 0 for unequal keys). *)

open Obst

module BadOrd = struct
  type t = int
  let compare : (x : int) -> (y : int) -> int{ (_ = 0 -> x = y) } =
    fun _x _y -> 0
end

module IntSet = Make (BadOrd)
