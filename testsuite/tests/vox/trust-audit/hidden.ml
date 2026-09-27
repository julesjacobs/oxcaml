(* Assumptions that are easy to miss: a refinement in a first-class module, a
   total field, an unsafe read at a refined type and a type that crosses
   modes unchecked. *)
module type S = sig
  module M : sig val f : unit -> {r : int | false} end
end
external packed : unit -> (module S) = "vox_audit_packed"
type box = { run : unit -> unit @@ total } [@@boxed]
external boxed : unit -> box = "vox_audit_boxed"
let read (a : {x : int | x = 0} array) : {r : int | r = 0} =
  Array.unsafe_get a 5
type cell : value mod contended = { mutable contents : string }
[@@ocaml.unsafe_allow_any_mode_crossing]
