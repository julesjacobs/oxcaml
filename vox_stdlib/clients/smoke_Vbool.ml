(* Per-module SMOKE (dead-law check, blueprint 6.7) + Variant-V acceptance.
   Part 1 forces every to_int law and each native op spec. Part 2 is the
   case-able-bool-FIELD datatype exercise (wart (a)): a record whose bool field
   the client cases on and gets exact results -- the thing a Prop bool could not
   do. Verified against Vbool.cmi + VoxSig_Vbool.olean. *)
open Vbool

(* ===== Part 1: op specs + to_int laws ===== *)
let s_bnot (b : bool) : bool{ _ = not b } = bnot b
let s_band (a : bool) (b : bool) : bool{ _ = (a && b) } = band a b
let s_bor (a : bool) (b : bool) : bool{ _ = (a || b) } = bor a b
let s_bxor (a : bool) (b : bool) : bool{ _ = (a <> b) } = bxor a b
let s_bequal (a : bool) (b : bool) : bool{ _ = (a = b) } = bequal a b
let s_of_int (n : int) : bool{ _ = (n <> 0) } = of_int n

(* to_int cases: the reflected result is 0 or 1 (forces vb_toint_cases). *)
let s_toint_cases (b : bool) : int{ _ = 0 || _ = 1 } = to_int b
(* Branch discharge (forces vb_toint_true / vb_toint_false via the V bridge):
   the caller's Prop condition on b connects to vb_toint's Bool case. *)
let s_toint_true (b : bool{ _ }) : int{ _ = 1 } = to_int b
let s_toint_false (b : bool{ not _ }) : int{ _ = 0 } = to_int b

(* ===== Part 2: Variant-V wart-(a) -- a case-able bool FIELD ===== *)
(* A record with a bool FIELD. Pre-V a bool field modelled as Prop and could not
   be cased ("Dependent elimination failed"); Variant V sorts it at Bool, so a
   client constructs a cell and BRANCHES on its bool field to get exact results.
   (Finding, notes/vbool.md: the model DEF/law surface over the field -- a
   [%%vox.lean] def projecting c.live, or an ambient @[grind] projection law --
   is NOT available in a VoxSig-importing context; this plain-OCaml client is the
   working shape.) *)
type cell = { live : bool; v : int }

let demo_live (v : int) : int{ _ = v } =
  let c : cell = { live = true; v } in
  if c.live then c.v else 0

let demo_dead (v : int) : int{ _ = 0 } =
  let c : cell = { live = false; v } in
  if c.live then c.v else 0
