(* TEST
 flags = "-vox-solver lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: integer square root by binary search, proved CORRECT (not just
   safe): the result [r] satisfies [0 <= r && r*r <= x < (r+1)*(r+1)].
   The spec is nonlinear but the proof is not: the invariant
   [lo*lo <= x < hi*hi] rides on [go]'s parameters as CONTRACTS,
   assumed by the body and discharged at each (bare) call; it moves by
   congruence -- the probe condition [m * m <= x] is reflected, so the
   path fact IS the nonlinear term relation, and the solver only ever
   substitutes equals for equals.  The two assumptions are RUNTIME
   CHECKED: floor halving (whose [asr] the logic does not model) and
   one instance of "a nonnegative number is at most its square". *)

let half : (s : int) -> int{ s = 2 * _ || s = 2 * _ + 1 } =
  fun s -> assume_ (s asr 1)
[%%expect{|
val half : (s : int) -> int{ (s = (2 * _)) || (s = ((2 * _) + 1)) } = <fun>
|}]

let isqrt
  : (x : int{ 0 <= _ }) ->
    {r:int | 0 <= r && r * r <= x && x < (r + 1) * (r + 1)}
  =
  fun x ->
    let rec go
      : (lo : int{ 0 <= _ && _ * _ <= x }) ->
        (hi : int{ lo < _ && x < _ * _ }) ->
        {r:int | 0 <= r && r * r <= x && x < (r + 1) * (r + 1)}
      =
      fun lo hi ->
        if lo + 1 < hi
        then begin
          let refine_ s = refine_ (lo + hi) in
          let refine_ m = half s in
          if m * m <= x then go m hi else go lo m
        end
        else refine_ lo
    in
    let refine_ x1 = refine_ (x + 1) in
    (* The only nonlinear step, as a runtime-checked lemma instance. *)
    let _sq : unit{ x1 <= x1 * x1 } = assume_ () in
    go 0 x1
[%%expect{|
val isqrt :
  (x : int{ 0 <= _ }) ->
  int{ (0 <= _) && (((_ * _) <= x) && (x < ((_ + 1) * (_ + 1)))) } = <fun>
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

(* Returning the wrong endpoint is CAUGHT: nothing proves
   [hi * hi <= x] (the invariant proves its negation). *)
let isqrt_broken
  : (x : int{ 0 <= _ }) ->
    {r:int | 0 <= r && r * r <= x && x < (r + 1) * (r + 1)}
  =
  fun x ->
    let rec go
      : (lo : int{ 0 <= _ && _ * _ <= x }) ->
        (hi : int{ lo < _ && x < _ * _ }) ->
        {r:int | 0 <= r && r * r <= x && x < (r + 1) * (r + 1)}
      =
      fun lo hi ->
        if lo + 1 < hi
        then begin
          let refine_ s = refine_ (lo + hi) in
          let refine_ m = half s in
          if m * m <= x then go m hi else go lo m
        end
        else refine_ hi
    in
    let refine_ x1 = refine_ (x + 1) in
    let _sq : unit{ x1 <= x1 * x1 } = assume_ () in
    go 0 x1
[%%expect{|
Line 18, characters 21-23:
18 |         else refine_ hi
                          ^^
Error: vox: verification failed (lean).
       Goal: (0 <= hi) && (((hi * hi) <= x) && (x < ((hi + 1) * (hi + 1))))
Hypotheses:
  not ((lo + 1) < hi)
  (lo < hi) && (x < (hi * hi))
  (0 <= lo) && ((lo * lo) <= x)
  0 <= x
Possible counterexample:
  hi = 1
  x = 0
  lo = 0
  hi ^ 2 = 1
  lo ^ 2 = 0
(lean: error: `grind` failed)
|}]
