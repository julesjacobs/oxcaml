(* TEST
 flags = "-vox-solver lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: integer square root by binary search, proved CORRECT (not just
   safe): the result [r] satisfies [sq r <= x < sq (r + 1)], with [sq]
   a reflected [total_] function -- the same definition the program
   runs is the one the spec quantifies over.  The invariant rides on
   [go]'s parameters as CONTRACTS, assumed by the body and discharged
   at each (bare) call; the probe condition [sq m <= x] reflects to
   itself, so the path fact IS the nonlinear relation the invariant
   update needs, and the solver only ever substitutes equals for
   equals.  The one nonlinear lemma ("a nonnegative number is at most
   its square", for the initial bracket) is PROVED about [sq] in the
   embedded prelude block and fired by an explicit grind pattern; the
   only assumption left is floor halving (whose [asr] the logic does
   not model), and even that is RUNTIME CHECKED. *)

let total_ sq m = m * m

[%%vox.prelude.lean {lean|
theorem le_sq (m : Int) (h : 0 <= m) : m <= sq m := by
  simp only [sq]
  by_cases h1 : m = 0
  · simp [h1]
  · have h2 : 1 <= m := by omega
    have h3 := Int.mul_le_mul_of_nonneg_left h2 (by omega : (0:Int) <= m)
    simpa using h3
grind_pattern le_sq => sq m
|lean}]

let half : (s : int) -> int{ s = 2 * _ || s = 2 * _ + 1 } =
  fun s -> assume_ (s asr 1)
[%%expect{|
val sq : int -> int = <fun>
val half : (s : int) -> int{ (s = (2 * _)) || (s = ((2 * _) + 1)) } = <fun>
|}]

let isqrt : (x : int{ 0 <= _ }) -> {r:int | 0 <= r && sq r <= x && x < sq (r + 1)} =
  fun x ->
    let rec go
      : (lo : int{ 0 <= _ && sq _ <= x }) ->
        (hi : int{ lo < _ && x < sq _ }) ->
        {r:int | 0 <= r && sq r <= x && x < sq (r + 1)}
      =
      fun lo hi ->
        if lo + 1 < hi
        then begin
          let refine_ s = refine_ (lo + hi) in
          let refine_ m = half s in
          if sq m <= x then go m hi else go lo m
        end
        else refine_ lo
    in
    let refine_ x1 = refine_ (x + 1) in
    go 0 x1
[%%expect{|
val isqrt :
  (x : int{ 0 <= _ }) ->
  int{ (0 <= _) && (((sq _) <= x) && (x < (sq (_ + 1)))) } = <fun>
|}]

(* Client side: the precondition [0 <= 9] is discharged at the literal
   argument; the functional spec arrives as facts. *)
let three : int =
  let refine_ r = isqrt 9 in
  let _w : {u:int | u = r && 0 <= u} = refine_ r in
  r
[%%expect{|
val three : int = 3
|}]

(* Returning the wrong endpoint is CAUGHT -- with a concrete witness:
   on input x = 0 the broken variant returns hi = 1, and sq 1 > 0. *)
let isqrt_broken
  : (x : int{ 0 <= _ }) -> {r:int | 0 <= r && sq r <= x && x < sq (r + 1)} =
  fun x ->
    let rec go
      : (lo : int{ 0 <= _ && sq _ <= x }) ->
        (hi : int{ lo < _ && x < sq _ }) ->
        {r:int | 0 <= r && sq r <= x && x < sq (r + 1)}
      =
      fun lo hi ->
        if lo + 1 < hi
        then begin
          let refine_ s = refine_ (lo + hi) in
          let refine_ m = half s in
          if sq m <= x then go m hi else go lo m
        end
        else refine_ hi
    in
    let refine_ x1 = refine_ (x + 1) in
    go 0 x1
[%%expect{|
Line 16, characters 21-23:
16 |         else refine_ hi
                          ^^
Error: vox: verification failed (lean).
       Goal: (0 <= hi) && (((sq hi) <= x) && (x < (sq (hi + 1))))
Hypotheses:
  not ((lo + 1) < hi)
  (lo < hi) && (x < (sq hi))
  (0 <= lo) && ((sq lo) <= x)
  0 <= x
Possible counterexample:
  hi = 1
  x = 0
  lo = 0
  sq hi = 0
  sq lo = 0
  sq (hi + 1) = 0
  hi ^ 2 = 1
  lo ^ 2 = 0
(lean: error: `grind` failed)
|}]
