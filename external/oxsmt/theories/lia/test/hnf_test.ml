(* Standalone unit + property tests for the exact-integer Hermite Normal Form kernel
   (smt/theories/lia/hnf.ml). No solver state — the kernel's correctness is provable in
   isolation, which de-risks the Stage B integration (charter logs/lia-cuts-charter.md,
   spec logs/lia-cuts-hnf-spec.md).

   Coverage: hand matrices with a hand-computed HNF; a random property sweep checking the
   always-on self-check [Hnf.verify] AND an INDEPENDENT unimodularity witness (a
   unimodular matrix's own HNF is the identity); the lattice-determinant invariant
   (product of HNF pivots = |det A| for square A, via a hand det formula); and a
   modular/ring-lattice gcd example mirroring the 2^k ring constants. Stdlib-only (I3);
   fixed-seed PRNG. *)

open Oxsmt_lia
module B = Oxsmt_core.Bigint

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* ---- matrix helpers over Bigint ---- *)
let m_of (rows : int list list) : Hnf.matrix =
  Array.of_list (List.map (fun r -> Array.of_list (List.map B.of_int r)) rows)
;;

let identity n : Hnf.matrix =
  Array.init n (fun i -> Array.init n (fun j -> B.of_int (if i = j then 1 else 0)))
;;

(* ---- fixed-seed xorshift64-star PRNG ---- *)
let rng = ref 0x1E3779B97F4A7C15

let rand_bits () =
  let x = !rng in
  let x = x lxor (x lsr 12) in
  let x = x lxor (x lsl 25) in
  let x = x lxor (x lsr 27) in
  rng := x;
  x * 0x2545F4914F6CDD1D land max_int
;;

let rand_int n = rand_bits () mod n
let rand_range lo hi = lo + rand_int (hi - lo + 1)

(* ================================================================== *)
(* Hand cases: a hand-computed HNF pins the module's convention. *)

let test_hand () =
  print_endline "hnf hand cases:";
  let ok name a expected =
    let t = Hnf.compute (m_of a) in
    check (name ^ ": verify") (Hnf.verify t (m_of a));
    check (name ^ ": H matches") (Hnf.matrix_equal t.h (m_of expected))
  in
  (* 1x1: already reduced; a negative diagonal is normalized positive (U = [-1]). *)
  ok "[3]" [ [ 3 ] ] [ [ 3 ] ];
  (let t = Hnf.compute (m_of [ [ -3 ] ]) in
   check "[-3]: verify" (Hnf.verify t (m_of [ [ -3 ] ]));
   check "[-3]: H = [3]" (Hnf.matrix_equal t.h (m_of [ [ 3 ] ]));
   check "[-3]: det_sign = -1" (t.det_sign = -1));
  (* 2x1 column: gcd(6,4) = 2 in the pivot, second row cleared. *)
  ok "[6;4]" [ [ 6 ]; [ 4 ] ] [ [ 2 ]; [ 0 ] ];
  (* upper-triangular already; the above-pivot entry 3 reduces mod 2 into [0,2). *)
  ok "[[2,3],[0,2]]" [ [ 2; 3 ]; [ 0; 2 ] ] [ [ 2; 1 ]; [ 0; 2 ] ];
  (* full 2x2 reduction; |det| = 16 = 2*8 (pivot product). *)
  ok "[[4,0],[6,4]]" [ [ 4; 0 ]; [ 6; 4 ] ] [ [ 2; 4 ]; [ 0; 8 ] ]
;;

(* ================================================================== *)
(* Edge cases. *)

let test_edges () =
  print_endline "hnf edge cases:";
  let vok name a =
    let t = Hnf.compute (m_of a) in
    check name (Hnf.verify t (m_of a))
  in
  vok "zero row" [ [ 0; 0; 0 ] ];
  vok "zero matrix 2x2" [ [ 0; 0 ]; [ 0; 0 ] ];
  vok "single row" [ [ 6; 9; 15 ] ];
  vok "rectangular 2x3" [ [ 2; 4; 6 ]; [ 3; 3; 3 ] ];
  vok "tall 3x2" [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ];
  vok "leading zero column" [ [ 0; 2 ]; [ 0; 3 ] ];
  (* an empty matrix (0 rows) computes trivially *)
  let t = Hnf.compute [||] in
  check "empty: verify" (Hnf.verify t [||])
;;

(* ================================================================== *)
(* Random property sweep: the self-check holds, det_sign is unimodular, AND an INDEPENDENT
   unimodularity witness — the HNF of the transform U is the identity iff U is unimodular. *)

let test_random () =
  print_endline "hnf random property (self-check + independent unimodularity):";
  let n = 4000 in
  let bad_verify = ref 0 in
  let bad_det = ref 0 in
  let bad_unimod = ref 0 in
  for _ = 1 to n do
    let m = rand_range 1 4 in
    let cols = rand_range 1 4 in
    let a =
      Array.init m (fun _ -> Array.init cols (fun _ -> B.of_int (rand_range (-9) 9)))
    in
    let t = Hnf.compute a in
    if not (Hnf.verify t a) then incr bad_verify;
    if not (t.det_sign = 1 || t.det_sign = -1) then incr bad_det;
    (* U unimodular <=> HNF(U) = I_m *)
    let tu = Hnf.compute t.u in
    if not (Hnf.matrix_equal tu.h (identity m)) then incr bad_unimod
  done;
  check (Printf.sprintf "verify holds on all %d" n) (!bad_verify = 0);
  check "det_sign always +/-1" (!bad_det = 0);
  check "U unimodular (HNF(U)=I) on all" (!bad_unimod = 0);
  Printf.printf
    "    (%d matrices; verify-fail=%d det-fail=%d unimod-fail=%d)\n"
    n
    !bad_verify
    !bad_det
    !bad_unimod
;;

(* ================================================================== *)
(* Lattice determinant: for a square nonsingular A, the product of the HNF diagonal pivots
   equals |det A| (U unimodular preserves the lattice index). Cross-checked against a hand
   2x2 / 3x3 determinant formula. *)

let det2 a = B.sub (B.mul a.(0).(0) a.(1).(1)) (B.mul a.(0).(1) a.(1).(0))

let det3 a =
  let e i j = a.(i).(j) in
  let t = B.mul (e 0 0) (B.sub (B.mul (e 1 1) (e 2 2)) (B.mul (e 1 2) (e 2 1))) in
  let t =
    B.sub t (B.mul (e 0 1) (B.sub (B.mul (e 1 0) (e 2 2)) (B.mul (e 1 2) (e 2 0))))
  in
  B.add t (B.mul (e 0 2) (B.sub (B.mul (e 1 0) (e 2 1)) (B.mul (e 1 1) (e 2 0))))
;;

let pivot_product (h : Hnf.matrix) n =
  let p = ref B.one in
  for i = 0 to n - 1 do
    p := B.mul !p h.(i).(i)
  done;
  !p
;;

let test_det_invariant () =
  print_endline "hnf lattice-determinant invariant (pivot product = |det|):";
  let trials = 2000 in
  let bad = ref 0 in
  for _ = 1 to trials do
    let n = rand_range 2 3 in
    let a =
      Array.init n (fun _ -> Array.init n (fun _ -> B.of_int (rand_range (-6) 6)))
    in
    let d = if n = 2 then det2 a else det3 a in
    if not (B.is_zero d)
    then (
      let t = Hnf.compute a in
      (* nonsingular => H is upper-triangular full rank; pivots are on the diagonal *)
      let pp = pivot_product t.h n in
      if not (B.equal pp (B.abs d)) then incr bad)
  done;
  check "pivot product = |det| on all nonsingular" (!bad = 0);
  Printf.printf "    (%d square trials; mismatches=%d)\n" trials !bad
;;

(* ================================================================== *)
(* Modular / ring lattice: gcd extraction is the "lattice reasoning" a ring cut rests on.
   A column of 2^k-scaled constants (mirroring the ring modulus) collapses to its gcd. *)

let test_ring_lattice () =
  print_endline "hnf modular/ring lattice (gcd of 2^k constants):";
  (* gcd(6,9,15) = 3 *)
  let t = Hnf.compute (m_of [ [ 6 ]; [ 9 ]; [ 15 ] ]) in
  check "gcd column: verify" (Hnf.verify t (m_of [ [ 6 ]; [ 9 ]; [ 15 ] ]));
  check "gcd(6,9,15)=3 in pivot" (Hnf.matrix_equal t.h (m_of [ [ 3 ]; [ 0 ]; [ 0 ] ]));
  (* ring-style: 2^10 and 2^10+1 are coprime, so their lattice is all of Z (pivot 1). *)
  let k = 1024 in
  let t2 = Hnf.compute (m_of [ [ k ]; [ k + 1 ] ]) in
  check
    "coprime ring constants collapse to 1"
    (Hnf.matrix_equal t2.h (m_of [ [ 1 ]; [ 0 ] ]));
  (* a 2^k diagonal with an off-diagonal coupling: HNF exposes the lattice index 2^k. *)
  let t3 = Hnf.compute (m_of [ [ k; 0 ]; [ 1; 1 ] ]) in
  check "2^k lattice: verify" (Hnf.verify t3 (m_of [ [ k; 0 ]; [ 1; 1 ] ]));
  check "2^k lattice index = |det| = 1024" (B.equal (pivot_product t3.h 2) (B.of_int k))
;;

let () =
  print_endline "hnf self-test:";
  test_hand ();
  test_edges ();
  test_random ();
  test_det_invariant ();
  test_ring_lattice ();
  Printf.printf "\nhnf self-test: %d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;
