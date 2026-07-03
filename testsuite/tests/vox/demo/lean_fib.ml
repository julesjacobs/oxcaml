(* TEST
 flags = "-vox-solver lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo 6/7 -- Fibonacci, where the spec IS the program, and the whole
   development is ONE FILE.  The naive recursion below is TOTAL
   (reflected): the compiler translates its definition into the logic
   and emits it as a Lean [@[grind] def], with [termination_by]
   synthesized from [@@vox.decreases n]; an applied [fib] in any
   refinement then denotes this very function.  The tail-recursive
   accumulator loop and the O(log n) fast doubling are verified
   against it.  Every obligation is really proved: a recursive call
   re-instantiates the dependent signature at the actual arguments, so
   its refined result is the induction hypothesis; the fast-doubling
   identities are lemmas in an embedded [%%vox.prelude.lean] block,
   proved by functional induction DIRECTLY on the reflected [fib] --
   there is no second, Lean-side Fibonacci anywhere.  The only assumed
   primitive is floor halving, whose [asr] the logic does not model --
   and even that assumption is RUNTIME CHECKED. *)

let half : (x : int) -> int{ x = 2 * _ || x = 2 * _ + 1 } =
  fun x -> assume_ (x asr 1)

let rec total_ fib n =
  if n <= 0
  then 0
  else if n = 1
  then 1
  else fib (n - 1) + fib (n - 2)
[@@vox.decreases n]

(* The fast-doubling identities, stated and proved about the reflected
   [fib] itself: the addition formula goes by [fun_induction fib] (the
   induction principle of the emitted definition), and the doubling
   identities are corollaries.  [@[grind =]] keys them on the
   [fib (2 * k)] / [fib (2 * k + 1)] goal terms that the annotations
   in [fib_fd] provide. *)
[%%vox.prelude.lean {lean|
theorem fib_rec (n : Int) (h : 2 <= n) : fib n = fib (n - 1) + fib (n - 2) := by
  grind

theorem fib_add (m n : Int) (hm : 0 <= m) (hn : 0 <= n) :
    fib (m + n + 1) = fib (m + 1) * fib (n + 1) + fib m * fib n := by
  fun_induction fib m with
  | case1 x h => grind
  | case2 x => grind
  | case3 x h1 h2 ih1 ih2 =>
    have i1 := ih1 (by omega)
    have i2 := ih2 (by omega)
    have e1 : x - 1 + n + 1 = x + n := by omega
    have e2 : x - 2 + n + 1 = x + n - 1 := by omega
    have e3 : x - 1 + 1 = x := by omega
    have e4 : x - 2 + 1 = x - 1 := by omega
    rw [e1, e3] at i1
    rw [e2, e4] at i2
    have u0 := fib_rec (x + n + 1) (by omega)
    have e5 : x + n + 1 - 1 = x + n := by omega
    have e6 : x + n + 1 - 2 = x + n - 1 := by omega
    rw [e5, e6] at u0
    have u1 := fib_rec (x + 1) (by omega)
    have e7 : x + 1 - 1 = x := by omega
    have e8 : x + 1 - 2 = x - 1 := by omega
    rw [e7, e8] at u1
    have u2 := fib_rec x (by omega)
    grind

@[grind =] theorem fib_double (k : Int) (hk : 0 <= k) :
    fib (2 * k) = fib k * (2 * fib (k + 1) - fib k) := by
  by_cases h0 : k = 0
  · subst h0; grind
  · have h := fib_add (k - 1) k (by omega) hk
    have e1 : k - 1 + k + 1 = 2 * k := by omega
    have e2 : k - 1 + 1 = k := by omega
    rw [e1, e2] at h
    grind

@[grind =] theorem fib_double_succ (k : Int) (hk : 0 <= k) :
    fib (2 * k + 1) = fib k * fib k + fib (k + 1) * fib (k + 1) := by
  have h := fib_add k k hk hk
  have e : k + k + 1 = 2 * k + 1 := by omega
  rw [e] at h
  grind
|lean}]

(* A reflected call names itself: the program's fib meets the spec
   definitionally (the goal is [fib n = fib n]). *)
let fib_slow : (n : int) -> int{ _ = fib n } = fun n -> refine_ (fib n)

(* Tail-recursive accumulator loop, O(n) iterations: the parameters
   carry the invariant (a, b) = (fib i, fib (i+1)) with i >= 0 as
   CONTRACTS -- the body assumes them, and each call site discharges
   them at its own (bare) arguments, the shifted accumulators and the
   literal seeds included.  (For n < i the loop diverges; partial
   correctness is unbothered.) *)
let rec fib_loop
  : (n : int) -> (i : int) -> (a : int{ _ = fib i && i >= 0 })
    -> (b : int{ _ = fib (i + 1) }) -> int{ _ = fib n }
  =
  fun n i a b ->
    if i = n
    then refine_ a
    else begin
      let refine_ j = refine_ (i + 1) in
      let refine_ r = fib_loop n j b (a + b) in
      refine_ r
    end

let fib_iter : (n : int) -> int{ _ = fib n } =
  fun n -> fib_loop n 0 0 1

(* Fast doubling, O(log n) iterations, on pairs (fib n, fib (n+1)) --
   a simple variant, so the pair appears in predicates.  With k = n/2:
   fib (2k) = fib k * (2 fib (k+1) - fib k) and fib (2k+1) = fib k ^ 2
   + fib (k+1) ^ 2 (the embedded lemmas; they need k >= 0, hence the
   precondition on n, discharged at each call).  Annotating x and y
   with the doubling identities puts [fib (2 * k)] syntactically in
   each obligation, which is what fires the lemmas; the final
   obligations are then pure congruence. *)
type pair = P of int * int

let rec fib_fd : (n : int{ _ >= 0 }) -> pair{ _ = P (fib n, fib (n + 1)) } =
  fun n ->
    if n <= 0
    then refine_ (P (0, 1))
    else begin
      let refine_ k = half n in
      let refine_ q = fib_fd k in
      match q with
      | P (a, b) ->
        let refine_ x = (refine_ (a * (2 * b - a)) : int{ _ = fib (2 * k) }) in
        let refine_ y = (refine_ (a * a + b * b) : int{ _ = fib (2 * k + 1) }) in
        if n = 2 * k
        then refine_ (P (x, y))
        else refine_ (P (y, x + y))
    end

(* Client side: fib 10, by fast doubling; the precondition [10 >= 0]
   is discharged at the literal argument. *)
let fib10 : int{ _ = fib 10 } =
  let refine_ r = fib_fd 10 in
  match r with
  | P (u, _) -> refine_ u
