(* TEST
 flags = "-vox-solver lean -vox-prelude fib_lib.lean";
 script = "sh ${test_source_directory}/has-lean.sh";
 readonly_files = "fib_lib.lean";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Three implementations of Fibonacci -- naive recursion, a
   tail-recursive accumulator loop, and O(log n) fast doubling -- all
   verified against the same Lean spec function [fib] (fib_lib.lean,
   totalized with fib n = 0 for n <= 0).  Every obligation is really
   proved: a recursive call re-instantiates the dependent signature at
   the actual arguments, so its refined result is the induction
   hypothesis; the fast-doubling identities are prelude lemmas, proved
   there by induction from the fib addition formula. *)

(* The usual assume_d primitives: the compiler attaches no logical
   meaning to program arithmetic (DESIGN.md); these give it one. *)
let zero : int{ _ = 0 } = assume_ 0
let one : int{ _ = 1 } = assume_ 1
let two : int{ _ = 2 } = assume_ 2

let add : (x : int) -> (y : int) -> int{ _ = x + y } = fun x y -> assume_ (x + y)
let sub : (x : int) -> (y : int) -> int{ _ = x - y } = fun x y -> assume_ (x - y)
let mul : (x : int) -> (y : int) -> int{ _ = x * y } = fun x y -> assume_ (x * y)
let le : (x : int) -> (y : int) -> bool{ _ = (x <= y) } = fun x y -> assume_ (x <= y)
let eq : (x : int) -> (y : int) -> bool{ _ = (x = y) } = fun x y -> assume_ (x = y)

(* Floor halving: for every x, x = 2*(x asr 1) or x = 2*(x asr 1) + 1. *)
let half : (x : int) -> int{ x = 2 * _ || x = 2 * _ + 1 } =
  fun x -> assume_ (x asr 1)

(* Naive recursion, O(fib n) calls.  Total: fib n = 0 for n <= 0. *)
let rec fib_naive : (n : int) -> int{ _ = fib n } =
  fun n ->
    let refine_ z = zero in
    let refine_ o = one in
    let refine_ c0 = le n z in
    if c0
    then refine_ z
    else begin
      let refine_ c1 = eq n o in
      if c1
      then refine_ o
      else begin
        let refine_ t = two in
        let refine_ m1 = sub n o in
        let refine_ m2 = sub n t in
        let refine_ r1 = fib_naive m1 in
        let refine_ r2 = fib_naive m2 in
        let refine_ r = add r1 r2 in
        refine_ r
      end
    end

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
    let refine_ c = eq i n in
    if c
    then refine_ a0
    else begin
      let refine_ o = one in
      let refine_ j = add i o in
      let refine_ s = add a0 b0 in
      let a2 = (refine_ b0 : int{ _ = fib j && j >= 0 }) in
      let b2 = (refine_ s : int{ _ = fib (j + 1) }) in
      let refine_ r = fib_loop n j a2 b2 in
      refine_ r
    end

let fib_iter : (n : int) -> int{ _ = fib n } =
  fun n ->
    let refine_ z = zero in
    let refine_ o = one in
    let a0 = (refine_ z : int{ _ = fib z && z >= 0 }) in
    let b0 = (refine_ o : int{ _ = fib (z + 1) }) in
    let refine_ r = fib_loop n z a0 b0 in
    refine_ r

(* Fast doubling, O(log n) iterations, on pairs (fib n, fib (n+1)) --
   a simple variant, so the pair appears in predicates.  With k = n/2:
   fib (2k) = fib k * (2 fib (k+1) - fib k) and fib (2k+1) = fib k ^ 2
   + fib (k+1) ^ 2 (prelude lemmas; they need k >= 0, hence the ghost
   proposition parameter).  In the odd branch the two stepping stones
   [x2 = fib (2k)] and [y2 = fib (2k+1)] keep each obligation within
   grind's instantiation budget. *)
type pair = P of int * int

let rec fib_fd
  : (n : int) -> (p : unit{ n >= 0 }) -> pair{ _ = P (fib n, fib (n + 1)) }
  =
  fun n p ->
    let refine_ z = zero in
    let refine_ o = one in
    let refine_ c0 = le n z in
    if c0
    then refine_ (P (z, o))
    else begin
      let refine_ t = two in
      let refine_ k = half n in
      let pk = (refine_ () : unit{ k >= 0 }) in
      let refine_ q = fib_fd k pk in
      match q with
      | P (a, b) ->
        let refine_ tb = mul t b in
        let refine_ d = sub tb a in
        let refine_ x = mul a d in
        let refine_ aa = mul a a in
        let refine_ bb = mul b b in
        let refine_ y = add aa bb in
        let refine_ tk = mul t k in
        let refine_ c1 = eq n tk in
        if c1
        then refine_ (P (x, y))
        else begin
          let refine_ x2 = (refine_ x : int{ _ = x && _ = fib (2 * k) }) in
          let refine_ y2 = (refine_ y : int{ _ = y && _ = fib (2 * k + 1) }) in
          let refine_ w = add x2 y2 in
          refine_ (P (y2, w))
        end
    end

(* Client side: fib 10, by fast doubling. *)
let fib10 : int{ _ = fib 10 } =
  let refine_ ten = (assume_ 10 : int{ _ = 10 }) in
  let p10 = (refine_ () : unit{ ten >= 0 }) in
  let refine_ r = fib_fd ten p10 in
  match r with
  | P (u, _) -> refine_ u
