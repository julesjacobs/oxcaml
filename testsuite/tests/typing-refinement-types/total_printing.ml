(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -i";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A printed interface keeps [@@ total]; without it clients would see the
   values as partial. *)

let (double @ total) x = x + x
let (ghost_id @ total) (x : int) : int @ ghost = ghost_ x
let partial_ref x = ref x
let rec (length @ total) (l : int list) : int =
  match l with [] -> 0 | _ :: rest -> 1 + length rest

(* Module modalities print as before. *)
module Nested = struct let (triple @ total) x = x * 3 end
