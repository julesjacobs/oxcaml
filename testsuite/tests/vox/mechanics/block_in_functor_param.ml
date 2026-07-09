(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* W6: a [%%vox.lean] block in a FUNCTOR PARAMETER signature is unit-level
   material in the wrong place -- the collector reads only top-level items,
   so it would be silently dropped and [Z] undefined at any VC that mentions
   it (a confusing "unknown symbol" Lean error at a downstream consumer).
   Previously the guard's [Tmty_functor (_, body, _)] discarded the parameter;
   now it is rejected with the same clear message as a module-body block. *)

module Make (O : sig
    type t
    [%%vox.lean {lean|
    public opaque Z : Type
    |lean}]
    val f : (x : int) -> int
  end) =
struct
  type elt = O.t
  let ok : (u : unit) -> unit = fun u -> u
end
