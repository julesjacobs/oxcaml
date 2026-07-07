(* Per-module SMOKE client (dead-law check, blueprint 6.7): one goal per
   shipped law, each stated over a reflected result so its VC can ONLY be
   discharged by that law. Because Vint's model defs are public-but-not-exposed,
   grind cannot unfold vi_min/vi_max/vi_abs here -- it must use the shipped
   [@@grind] theorems, so every law is genuinely load-bearing (delete any one
   and its goal fails). Verified against Vint.cmi + VoxSig_Vint.olean. *)
open Vint

let s_min_comm (x : int) (y : int) : int{ _ = vi_min y x } = imin x y
let s_min_idem (x : int) : int{ _ = x } = imin x x
let s_min_le_left (x : int) (y : int) : int{ _ <= x } = imin x y
let s_min_le_right (x : int) (y : int) : int{ _ <= y } = imin x y

let s_max_comm (x : int) (y : int) : int{ _ = vi_max y x } = imax x y
let s_max_idem (x : int) : int{ _ = x } = imax x x
let s_max_ge_left (x : int) (y : int) : int{ x <= _ } = imax x y
let s_max_ge_right (x : int) (y : int) : int{ y <= _ } = imax x y

let s_abs_nonneg (x : int) : int{ 0 <= _ } = iabs x

(* Combined bounds (Phase-C clamp use). The OUTER bounds (ge_left/le_left)
   don't suffice for clamp's far half; the min/max CASES laws do. s_clamp is
   the reviewer's exact failing shape (c_vint_clamp.ml): imax lo (imin hi x)
   in [lo,hi] under lo<=hi. Its upper half forces vi_max_cases (grind splits
   vi_max lo inner into lo -- bounded by lo<=hi -- or inner -- bounded by
   vi_min_le_left); its lower half is vi_max_ge_left. s_clamp_mirror is the
   dual clamp imin hi (imax lo x): its LOWER half forces vi_min_cases (with
   vi_max_ge_left), its upper half is vi_min_le_left. Together they force both
   cases laws. NB the conditional "vi_max a b <= c given a<=c, b<=c" form was
   tried first and cannot fire (free arithmetic c uncoverable by any grind
   pattern; notes/vint.md) -- the cases laws subsume it. *)
let s_clamp (lo : int) (hi : int{ lo <= _ }) (x : int) : int{ lo <= _ && _ <= hi } =
  let inner = imin hi x in
  imax lo inner

let s_clamp_mirror (lo : int) (hi : int{ lo <= _ }) (x : int)
    : int{ lo <= _ && _ <= hi } =
  let inner = imax lo x in
  imin hi inner
