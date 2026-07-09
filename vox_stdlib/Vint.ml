(* Each reflected body must match its vi_* Lean def in Vint.mli. That body<->def
   correspondence is a TRUST SURFACE the solver does NOT check: a divergent body
   would verify against a def it does not implement. Audited by inspection here --
   these bodies match the defs. NB iabs wraps at min_int, and ipow can overflow;
   the model's vi_abs_nonneg / vi_pow_nonneg hold only under vox's unbounded-Int
   model (L6). See notes/vint.md. succ/pred/ieven/iodd are ordinary spec'd
   functions (native +/-/mod), not reflected. *)
let imin (a : int) (b : int) : int = if a <= b then a else b
let imax (a : int) (b : int) : int = if a <= b then b else a
let iabs (a : int) : int = if a < 0 then -a else a
let isign (a : int) : int = if a < 0 then -1 else if a > 0 then 1 else 0
let rec ipow (b : int) (n : int) : int = if n <= 0 then 1 else b * ipow b (n - 1)

let iclamp (lo : int) (hi : int) (x : int) : int =
  if x < lo then lo else if x > hi then hi else x

let imin3 (a : int) (b : int) (c : int) : int = imin a (imin b c)
let imax3 (a : int) (b : int) (c : int) : int = imax a (imax b c)
let idiv (a : int) (b : int) : int = a / b
let imod (a : int) (b : int) : int = a mod b

let succ : (x : int) -> int{ _ = x + 1 } = fun x -> x + 1
let pred : (x : int) -> int{ _ = x - 1 } = fun x -> x - 1
let ieven : (x : int) -> bool{ _ = (x mod 2 = 0) } = fun x -> x mod 2 = 0
let iodd : (x : int) -> bool{ _ = (x mod 2 <> 0) } = fun x -> x mod 2 <> 0
