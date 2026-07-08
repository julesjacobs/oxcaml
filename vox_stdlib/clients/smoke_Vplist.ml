(* Smoke client for Vplist's eq-param layer (WP-2). The CENTREPIECE is dedup:
   the polymorphic op is PROVEN ONCE at the abstract element (Vplist.ml, verified
   at 'a); a client USES it at a concrete element with a call-site decider lambda.
   This is the WP-6-C gate — dedup was UNWRITABLE before the eq-param route (no
   bool mem at an abstract element). Instantiated here at int (a concrete element whose = reflects). Verified against Vplist + Vhof.

   NOTE (total-no-forward): the decider param is [@vox.total], and a total param
   cannot be FORWARDED (only a call-site lambda / [@vox.reflect] value is a valid
   total arg), so a client supplies the lambda at a concrete element type rather
   than threading an abstract decider param. *)
[@@@warning "-6-32-26-27"]
open Vhof
open Vplist

[%%vox.lean {lean|
abbrev intEq : Int -> Int -> Prop := fun a b => a = b
|lean}]

(* dedup at int: result is a subset of the input. *)
let dedup_int (l : int Vplist.t) : int Vplist.t{ pl_dedup_sub intEq l _ } =
  Vplist.dedup (fun a b -> a = b) (fun a b -> a = b) l

(* NB an int decider (native = reflects to the Prop model) works; a STRING
   decider does NOT — OCaml bool `=` on string carries no model fact (no
   reflected string equality; ties to the no-string-theory wall). So the concrete
   comparator must be at a type whose = reflects (int). The op itself is proven
   at the abstract element regardless. *)

(* bool membership at int, up to the decider. *)
let mem_int (x : int) (l : int Vplist.t) : bool{ _ = pl_memr intEq x l } =
  Vplist.mem (fun a b -> a = b) (fun a b -> a = b) x l

(* remove at int: x is not a member of the result. *)
let remove_gone (x : int) (l : int Vplist.t) : int Vplist.t{ not (pl_memr intEq x _) } =
  Vplist.remove (fun a b -> a = b) (fun a b -> a = b) x l
