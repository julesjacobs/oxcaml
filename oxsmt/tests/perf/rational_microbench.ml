(* Micro-benchmark for the {!Rational} hot ops (add/mul/compare) on the Small tier, which
   dominate simplex pivoting and bound checks. Visibility tool, not a gate. Reports ns/op
   for the integer-valued (den=1) case — the overwhelmingly common LIA operand — and a
   general small-fraction case, so the den=1 fast-path optimization can be measured
   before/after. Deterministic operand stream; no allocation-order dependence in timing. *)

module Rational = Oxsmt_lia.Rational

let now () = Unix.gettimeofday ()

let bench name iters f =
  (* warm up *)
  let _ = f 1000 in
  let t0 = now () in
  let sink = f iters in
  let dt = now () -. t0 in
  Printf.printf
    "%-28s %10d ops  %8.2f ns/op   (sink=%d)\n%!"
    name
    iters
    (dt /. float_of_int iters *. 1e9)
    sink
;;

(* Integer-valued (den=1) operands: add then compare, mul then compare, in a tight loop.
   Values kept small so nothing promotes to Big — this is the pure Small fast path. *)
let bench_int_add iters =
  let acc = ref 0 in
  let a = Rational.of_int 3 in
  for i = 1 to iters do
    let b = Rational.of_int (i land 0xffff) in
    let s = Rational.add a b in
    if Rational.compare s a > 0 then incr acc
  done;
  !acc
;;

let bench_int_mul iters =
  let acc = ref 0 in
  let a = Rational.of_int 7 in
  for i = 1 to iters do
    let b = Rational.of_int ((i land 0x3ff) + 1) in
    let s = Rational.mul a b in
    if Rational.compare s a >= 0 then incr acc
  done;
  !acc
;;

(* General small fractions (den <> 1): exercises the cross-multiply + gcd path. *)
let bench_frac_add iters =
  let acc = ref 0 in
  let a = Rational.of_frac 3 4 in
  for i = 1 to iters do
    let b = Rational.of_frac ((i land 0xff) + 1) 6 in
    let s = Rational.add a b in
    if Rational.compare s a > 0 then incr acc
  done;
  !acc
;;

let () =
  let iters =
    if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else 20_000_000
  in
  bench "int add+compare (den=1)" iters bench_int_add;
  bench "int mul+compare (den=1)" iters bench_int_mul;
  bench "frac add+compare (den<>1)" iters bench_frac_add
;;
