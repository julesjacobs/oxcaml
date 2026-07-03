(* TEST
 flags = "-vox-solver lean -vox-prelude fib_lib.lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 readonly_files = "fib_lib.lean";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo 6/7 -- Fibonacci, where the spec IS the program: the naive
   recursion below
   is TOTAL (reflected) -- the compiler translates its
   definition into the logic and emits it as a Lean [@[grind] def],
   with [termination_by] synthesized from [@@vox.decreases n]; an
   applied [fib] in any refinement then denotes this very function.
   The tail-recursive accumulator loop and the O(log n) fast doubling
   are verified against it.  Every obligation is really proved: a
   recursive call re-instantiates the dependent signature at the
   actual arguments, so its refined result is the induction
   hypothesis; the fast-doubling identities are prelude lemmas
   (fib_lib.lean), proved by induction over a Nat-indexed helper and
   transported to the reflected [fib] along a functional-induction
   bridge.  The only assumed primitive is floor halving, whose [asr]
   the logic does not model -- and even that assumption is RUNTIME
   CHECKED. *)

let half : (x : int) -> int{ x = 2 * _ || x = 2 * _ + 1 } =
  fun x -> assume_ (x asr 1)

let rec total_ fib n =
  if n <= 0
  then 0
  else if n = 1
  then 1
  else fib (n - 1) + fib (n - 2)
[@@vox.decreases n]

(* A reflected call names itself: the program's fib meets the spec
   definitionally (the goal is [fib n = fib n]). *)
let fib_slow : (n : int) -> int{ _ = fib n } = fun n -> refine_ (fib n)

(* Tail-recursive accumulator loop, O(n) iterations: the parameters
   carry the invariant (a, b) = (fib i, fib (i+1)) with i >= 0; each
   iteration shifts it to i+1 by re-refining the shifted values.
   (For n < i the loop diverges; partial correctness is unbothered.) *)
let rec fib_loop
  : (n : int) -> (i : int) -> (a : int{ _ = fib i && i >= 0 })
    -> (b : int{ _ = fib (i + 1) }) -> int{ _ = fib n }
  =
  fun n i a b ->
    let refine_ a0 = a in
    let refine_ b0 = b in
    if i = n
    then refine_ a0
    else begin
      let refine_ j = refine_ (i + 1) in
      let a2 = (refine_ b0 : int{ _ = fib j && j >= 0 }) in
      let b2 = (refine_ (a0 + b0) : int{ _ = fib (j + 1) }) in
      let refine_ r = fib_loop n j a2 b2 in
      refine_ r
    end

let fib_iter : (n : int) -> int{ _ = fib n } =
  fun n ->
    let refine_ z = refine_ 0 in
    let a0 = (refine_ z : int{ _ = fib z && z >= 0 }) in
    let b0 = (refine_ 1 : int{ _ = fib (z + 1) }) in
    let refine_ r = fib_loop n z a0 b0 in
    refine_ r

(* Fast doubling, O(log n) iterations, on pairs (fib n, fib (n+1)) --
   a simple variant, so the pair appears in predicates.  With k = n/2:
   fib (2k) = fib k * (2 fib (k+1) - fib k) and fib (2k+1) = fib k ^ 2
   + fib (k+1) ^ 2 (prelude lemmas; they need k >= 0, hence the ghost
   proposition parameter).  Annotating x and y with the doubling
   identities puts [fib (2 * k)] syntactically in each obligation,
   which is what fires the prelude lemmas; the final obligations are
   then pure congruence. *)
type pair = P of int * int

let rec fib_fd
  : (n : int) -> (p : unit{ n >= 0 }) -> pair{ _ = P (fib n, fib (n + 1)) }
  =
  fun n p ->
    if n <= 0
    then refine_ (P (0, 1))
    else begin
      let refine_ k = half n in
      let pk = (refine_ () : unit{ k >= 0 }) in
      let refine_ q = fib_fd k pk in
      match q with
      | P (a, b) ->
        let refine_ x = (refine_ (a * (2 * b - a)) : int{ _ = fib (2 * k) }) in
        let refine_ y = (refine_ (a * a + b * b) : int{ _ = fib (2 * k + 1) }) in
        if n = 2 * k
        then refine_ (P (x, y))
        else refine_ (P (y, x + y))
    end

(* Client side: fib 10, by fast doubling. *)
let fib10 : int{ _ = fib 10 } =
  let refine_ ten = refine_ 10 in
  let p10 = (refine_ () : unit{ ten >= 0 }) in
  let refine_ r = fib_fd ten p10 in
  match r with
  | P (u, _) -> refine_ u
