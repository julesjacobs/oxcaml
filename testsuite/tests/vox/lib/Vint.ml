(* Each body must match its vi_* Lean def in Vint.mli. That body<->def correspondence is a
   TRUST SURFACE the solver does NOT check: a divergent body would verify against a def it
   does not implement. Audited by inspection here -- these bodies match the defs. NB iabs
   wraps at min_int (-min_int overflows back to min_int < 0), so vi_abs_nonneg holds only
   under vox's unbounded-Int model; see notes/vint.md. *)
let imin (a : int) (b : int) : int = if a <= b then a else b
let imax (a : int) (b : int) : int = if a <= b then b else a
let iabs (a : int) : int = if a < 0 then -a else a
