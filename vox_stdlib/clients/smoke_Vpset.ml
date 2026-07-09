(* Smoke client for Vpset's eq-param layer (WP-2): bool mem + remove at a concrete
   element via a call-site decider lambda — the runtime queries the relational-only
   Vpset could not offer. The op is proven at the abstract element (Vpset.ml); the
   client instantiates at int (total-no-forward: see smoke_Vplist). *)
[@@@warning "-6-32-26-27"]
open Vhof
open Vpset

[%%vox.lean {lean|
abbrev intEq : Int -> Int -> Prop := fun a b => a = b
|lean}]

let mem_int (x : int) (s : int Vpset.t) : bool{ _ = ps_memr intEq x s } =
  Vpset.mem (fun a b -> a = b) (fun a b -> a = b) x s

let remove_gone (x : int) (s : int Vpset.t) : int Vpset.t{ not (ps_memr intEq x _) } =
  Vpset.remove (fun a b -> a = b) (fun a b -> a = b) x s
