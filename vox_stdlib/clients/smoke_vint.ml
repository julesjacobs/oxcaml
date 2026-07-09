(* Per-module SMOKE client (dead-law check, blueprint 6.7): one goal per shipped
   law, each stated over a reflected result so its VC can ONLY be discharged by
   that law. Vint's model defs are public-but-not-exposed, so grind cannot unfold
   them here -- it must use the shipped [@grind] theorems, so every law is
   genuinely load-bearing (delete any one and its goal fails). Verified against
   Vint.cmi + VoxSig_Vint.olean. *)
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

(* Combined bounds (Phase-C clamp use, via min/max composition). *)
let s_clamp_compose (lo : int) (hi : int{ lo <= _ }) (x : int)
    : int{ lo <= _ && _ <= hi } =
  let inner = imin hi x in
  imax lo inner

let s_clamp_mirror (lo : int) (hi : int{ lo <= _ }) (x : int)
    : int{ lo <= _ && _ <= hi } =
  let inner = imax lo x in
  imin hi inner

(* ===== WP-4 additions ===== *)

(* sign: pos/neg force the characterization laws; cases forces vi_sign_cases. *)
let s_sign_pos (x : int{ 0 < _ }) : int{ _ = 1 } = isign x
let s_sign_neg (x : int{ _ < 0 }) : int{ _ = -1 } = isign x
let s_sign_cases (x : int) : int{ _ = -1 || _ = 0 || _ = 1 } = isign x

(* pow: zero / nonneg / succ each force their law. *)
let s_pow_zero (b : int) : int{ _ = 1 } = ipow b 0
let s_pow_nonneg (b : int{ 0 <= _ }) (n : int) : int{ 0 <= _ } = ipow b n
let s_pow_succ (b : int) (n : int{ 0 <= _ }) : int{ _ = b * vi_pow b n } =
  ipow b (n + 1)

(* clamp (direct op): lb+ub together; id forces vi_clamp_id. *)
let s_clamp (lo : int) (hi : int{ lo <= _ }) (x : int)
    : int{ lo <= _ && _ <= hi } = iclamp lo hi x
let s_clamp_id (lo : int) (hi : int) (x : int{ lo <= _ && _ <= hi })
    : int{ _ = x } = iclamp lo hi x

(* min3 / max3: all three bounds fire (every var bound by the trigger); cases too. *)
let s_min3_le (a : int) (b : int) (c : int) : int{ _ <= a && _ <= b && _ <= c } =
  imin3 a b c
let s_min3_cases (a : int) (b : int) (c : int) : int{ _ = a || _ = b || _ = c } =
  imin3 a b c
let s_max3_ge (a : int) (b : int) (c : int) : int{ a <= _ && b <= _ && c <= _ } =
  imax3 a b c
let s_max3_cases (a : int) (b : int) (c : int) : int{ _ = a || _ = b || _ = c } =
  imax3 a b c

(* succ / pred / even / odd: native inline specs (transparent). *)
let s_succ (x : int) : int{ _ = x + 1 } = succ x
let s_pred (x : int) : int{ _ = x - 1 } = pred x
let s_even (x : int) : bool{ _ = (x mod 2 = 0) } = ieven x
let s_odd (x : int) : bool{ _ = (x mod 2 <> 0) } = iodd x

(* div / mod ambient laws: nonneg + upper bound + the division identity. *)
let s_mod_nonneg (x : int{ 0 <= _ }) (d : int{ 0 < _ }) : int{ 0 <= _ } = imod x d
let s_mod_lt (x : int{ 0 <= _ }) (d : int{ 0 < _ }) : int{ _ < d } = imod x d
let s_divmod (x : int) (d : int{ 0 < _ }) : int{ d * _ + vi_mod x d = x } = idiv x d
