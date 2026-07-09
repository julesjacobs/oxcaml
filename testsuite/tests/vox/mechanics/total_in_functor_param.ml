(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* W8: a [total_] (reflected) spec function in a FUNCTOR PARAMETER signature forms
   but can never be implemented -- [total_] is only definable at the file top
   level, so no argument module (a nested binding) can supply it.  Rejected at the
   signature with a functor-aware message, rather than the late, confusingly-placed
   "total_ is only supported on top-level bindings" error at the would-be argument. *)

module Make (O : sig
    type t
    val total_ sz : t -> int
    val mk : (n : int) -> t{ sz _ = n }
  end) =
struct
  type elt = O.t
  let ok : (u : unit) -> unit = fun u -> u
end
