(* Unit + property + independent-oracle tests for {!Bigint} (core-bignum W2).

   Three layers, in increasing independence (core-bignum-review.md R4):
   1. hand vectors at the limb boundaries (radix, radix^2, min_int/max_int, 0, +/-1);
   2. pure-OCaml PROPERTY vectors — algebraic identities that must hold for any correct
      bignum (round-trip, a=q*b+r, gcd divides both, (a*b)/b=a, commutativity, sign laws);
   3. an INDEPENDENT differential oracle in a DIFFERENT arithmetic (Python's arbitrary-
      precision [int], shelled out) — the only check that can catch a bug shared by the
      producer and a same-language verifier. Skipped (not failed) if python3 is absent.

   Stdlib-only (I3); [Sys.command] + temp files are stdlib and introduce no library dep.
   Deterministic: fixed-seed xorshift PRNG, no wall-clock. *)

open Oxsmt_lia

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* ---- fixed-seed PRNG (xorshift64-star), matching lia_test ---- *)
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

(* A random decimal string (canonical: no leading zero, optional '-', "0" allowed). Length
   up to ~[maxdigits] so magnitudes span several limbs. *)
let rand_decimal maxdigits =
  let d = 1 + rand_int maxdigits in
  let b = Buffer.create (d + 1) in
  if rand_int 2 = 0 && not (d = 1) then Buffer.add_char b '-';
  Buffer.add_char b (Char.chr (Char.code '1' + rand_int 9));
  for _ = 2 to d do
    Buffer.add_char b (Char.chr (Char.code '0' + rand_int 10))
  done;
  let s = Buffer.contents b in
  (* occasionally emit exactly "0" *)
  if rand_int 20 = 0 then "0" else s
;;

let rand_big maxdigits = Bigint.of_string (rand_decimal maxdigits)

(* =================================================================== *)
(* 1. Hand vectors at the limb boundaries. *)

let test_boundaries () =
  print_endline "boundaries:";
  let rt s = check ("round-trip " ^ s) (Bigint.to_string (Bigint.of_string s) = s) in
  List.iter
    rt
    [ "0"
    ; "1"
    ; "-1"
    ; "2147483647" (* radix-1 = 2^31-1 *)
    ; "2147483648" (* radix = 2^31 *)
    ; "2147483649"
    ; "4611686018427387903" (* max_int = 2^62-1 *)
    ; "-4611686018427387904" (* min_int = -2^62 *)
    ; "4611686018427387904" (* 2^62 = radix^2 *)
    ; "9223372036854775807" (* 2^63-1 *)
    ; "-9223372036854775808"
    ; "123456789012345678901234567890"
    ; "-999999999999999999999999999999999999"
    ];
  (* of_int / to_int_opt round-trip at the native boundaries. *)
  List.iter
    (fun n ->
       check
         (Printf.sprintf "of_int/to_int_opt %d" n)
         (Bigint.to_int_opt (Bigint.of_int n) = Some n))
    [ 0; 1; -1; max_int; min_int; 2147483648 (* 2^31 *); -2147483648 ]
;;

let test_fits () =
  print_endline "fits_int:";
  check "max_int fits" (Bigint.fits_int (Bigint.of_int max_int));
  check "min_int fits" (Bigint.fits_int (Bigint.of_int min_int));
  (* max_int + 1 does NOT fit *)
  let over = Bigint.add (Bigint.of_int max_int) Bigint.one in
  check "max_int+1 does not fit" (not (Bigint.fits_int over));
  check "max_int+1 to_int_opt None" (Bigint.to_int_opt over = None);
  (* min_int - 1 does NOT fit *)
  let under = Bigint.sub (Bigint.of_int min_int) Bigint.one in
  check "min_int-1 does not fit" (not (Bigint.fits_int under));
  (* exactly min_int reconstructs *)
  check "min_int reconstructs" (Bigint.to_int_opt (Bigint.of_int min_int) = Some min_int)
;;

(* =================================================================== *)
(* 2. Pure-OCaml property vectors. *)

let test_properties () =
  print_endline "properties:";
  let iters = 4000 in
  for _ = 1 to iters do
    let a = rand_big 40
    and b = rand_big 40 in
    (* round-trip *)
    check "round-trip a" (Bigint.equal (Bigint.of_string (Bigint.to_string a)) a);
    (* additive inverse and commutativity *)
    check "a+b = b+a" (Bigint.equal (Bigint.add a b) (Bigint.add b a));
    check "(a+b)-b = a" (Bigint.equal (Bigint.sub (Bigint.add a b) b) a);
    check "a + (-a) = 0" (Bigint.is_zero (Bigint.add a (Bigint.neg a)));
    (* multiplicative commutativity + distributive-ish *)
    check "a*b = b*a" (Bigint.equal (Bigint.mul a b) (Bigint.mul b a));
    (* sign laws *)
    check
      "sign(a*b) = sign a * sign b"
      (Bigint.sign (Bigint.mul a b) = Bigint.sign a * Bigint.sign b);
    check "abs neg" (Bigint.equal (Bigint.abs a) (Bigint.abs (Bigint.neg a)));
    (* compare trichotomy vs equal *)
    let c = Bigint.compare a b in
    check "compare/equal agree" (c = 0 = Bigint.equal a b);
    check "compare antisymmetric" (Bigint.compare b a = -c);
    if not (Bigint.is_zero b)
    then (
      let q, r = Bigint.divmod a b in
      (* a = q*b + r *)
      check "a = q*b + r" (Bigint.equal a (Bigint.add (Bigint.mul q b) r));
      (* |r| < |b| *)
      check "|r| < |b|" (Bigint.compare (Bigint.abs r) (Bigint.abs b) < 0);
      (* remainder carries dividend sign (or is zero) *)
      check "sign r" (Bigint.is_zero r || Bigint.sign r = Bigint.sign a);
      (* exact division: (a*b)/b = a, rem 0 *)
      let q2, r2 = Bigint.divmod (Bigint.mul a b) b in
      check "(a*b)/b = a" (Bigint.equal q2 a);
      check "(a*b) mod b = 0" (Bigint.is_zero r2);
      (* gcd divides both, is nonneg *)
      let g = Bigint.gcd a b in
      check "gcd >= 0" (Bigint.sign g >= 0);
      if not (Bigint.is_zero g)
      then (
        check "gcd | a" (Bigint.is_zero (snd (Bigint.divmod a g)));
        check "gcd | b" (Bigint.is_zero (snd (Bigint.divmod b g)))))
  done
;;

(* =================================================================== *)
(* 2b. Deep-growth tripwire (core-bignum-review.md R8). The property vectors above top out
   near 45 decimal digits (~5 limbs) — the magnitudes W2 actually meets in the simplex.
   This arm drives magnitudes into the hundreds of limbs to catch carry/borrow,
   canonicalization, and long-division bugs that ONLY surface at depth (a limb-index
   off-by-one, a missing final carry, a mis-sized quotient). Every check is a
   self-consistent algebraic identity, so it needs no oracle. Bounded and fast (pure
   integer work, well under a second). *)

let test_deep_growth () =
  print_endline "deep growth (R8):";
  let two = Bigint.of_int 2 in
  let pow2 n =
    let r = ref Bigint.one in
    for _ = 1 to n do
      r := Bigint.mul !r two
    done;
    !r
  in
  let n = 4096 in
  let big_pow = pow2 n in
  (* 2^4096: ~1234 decimal digits, ~133 limbs. *)
  check
    "2^n round-trips to_string/of_string"
    (Bigint.equal (Bigint.of_string (Bigint.to_string big_pow)) big_pow);
  check "2^n does not fit int63" (not (Bigint.fits_int big_pow));
  (* halving n times returns to 1, with a zero remainder at every step (deep divmod by a
     small divisor while the dividend shrinks limb by limb). *)
  (let r = ref big_pow
   and rem_ok = ref true in
   for _ = 1 to n do
     let q, rm = Bigint.divmod !r two in
     if not (Bigint.is_zero rm) then rem_ok := false;
     r := q
   done;
   check "2^n halved n times: zero remainder each step" !rem_ok;
   check "2^n halved n times returns to 1" (Bigint.equal !r Bigint.one));
  (* 2^a * 2^b = 2^(a+b): deep multiplication whose product crosses many limb boundaries. *)
  (let a = 1500 in
   let b = n - a in
   check "2^a * 2^b = 2^(a+b)" (Bigint.equal (Bigint.mul (pow2 a) (pow2 b)) big_pow));
  (* K! built forward vs. backward must agree (commutativity at depth), and dividing back
     out by every factor 2..K must land exactly on 1. *)
  let k = 400 in
  let fact_forward () =
    let r = ref Bigint.one in
    for i = 2 to k do
      r := Bigint.mul !r (Bigint.of_int i)
    done;
    !r
  in
  let fact_backward () =
    let r = ref Bigint.one in
    for i = k downto 2 do
      r := Bigint.mul !r (Bigint.of_int i)
    done;
    !r
  in
  let f = fact_forward () in
  check "K! is order-independent (a*b = b*a at depth)" (Bigint.equal f (fact_backward ()));
  (let r = ref f
   and rem_ok = ref true in
   for i = 2 to k do
     let q, rm = Bigint.divmod !r (Bigint.of_int i) in
     if not (Bigint.is_zero rm) then rem_ok := false;
     r := q
   done;
   check "K! / (2..K) : zero remainder each step" !rem_ok;
   check "K! divided by all its factors returns to 1" (Bigint.equal !r Bigint.one));
  (* gcd across two deep magnitudes must divide both and be positive. *)
  let g = Bigint.gcd f big_pow in
  check "gcd(K!,2^n) > 0" (Bigint.sign g > 0);
  check "gcd(K!,2^n) | K!" (Bigint.is_zero (snd (Bigint.divmod f g)));
  check "gcd(K!,2^n) | 2^n" (Bigint.is_zero (snd (Bigint.divmod big_pow g)))
;;

(* =================================================================== *)
(* 2c. Knuth Algorithm-D "add-back" regression vector (codex LOW). The add-back correction
   step (quotient-digit estimate q̂ one too high -> the trial subtraction goes negative ->
   add the divisor back and decrement q̂) fires only for specific limb patterns that the
   random oracle essentially never generates, so pin it explicitly. u = 2^63, v = 2^62 +
   1: the estimate overshoots and must add back, landing on q = 1, r = 2^62 - 1 (=
   max_int). Verified: 1*(2^62+1) + (2^62-1) = 2^63 = u, and |r| < |v|. *)

let test_knuth_addback () =
  print_endline "Knuth add-back vector (codex LOW):";
  let u =
    Bigint.of_string "9223372036854775808"
    (* 2^63 *)
  in
  let v =
    Bigint.of_string "4611686018427387905"
    (* 2^62 + 1 *)
  in
  let q, r = Bigint.divmod u v in
  check "add-back q = 1" (Bigint.equal q Bigint.one);
  check "add-back r = 2^62 - 1" (Bigint.equal r (Bigint.of_string "4611686018427387903"));
  check "add-back u = q*v + r" (Bigint.equal u (Bigint.add (Bigint.mul q v) r));
  check "add-back |r| < |v|" (Bigint.compare (Bigint.abs r) (Bigint.abs v) < 0)
;;

(* =================================================================== *)
(* 3. Independent differential oracle (Python int). *)

let python_oracle_script =
  {py|import sys
inp, outp = sys.argv[1], sys.argv[2]
def tdiv(a, b):
    q = abs(a)//abs(b)
    if (a < 0) != (b < 0):
        q = -q
    r = a - q*b
    return q, r
import math
with open(inp) as f, open(outp, "w") as o:
    for line in f:
        line = line.strip()
        if not line:
            continue
        a_s, b_s = line.split()
        a, b = int(a_s), int(b_s)
        add, sub, mul = a+b, a-b, a*b
        g = math.gcd(abs(a), abs(b))
        if b == 0:
            q, r = "NA", "NA"
        else:
            q, r = tdiv(a, b)
        o.write("%s %s %s %s %s %s\n" % (add, sub, mul, q, r, g))
|py}
;;

let have_python () = Sys.command "python3 -c '' >/dev/null 2>&1" = 0

let test_oracle () =
  print_endline "python differential oracle:";
  if not (have_python ())
  then print_endline "  SKIP (python3 not available)"
  else (
    let n = 3000 in
    let inputs = Array.init n (fun _ -> rand_big 45, rand_big 45) in
    let inp = Filename.temp_file "bigint_oracle_in" ".txt" in
    let outp = Filename.temp_file "bigint_oracle_out" ".txt" in
    let script = Filename.temp_file "bigint_oracle" ".py" in
    (* write inputs *)
    let oc = open_out inp in
    Array.iter
      (fun (a, b) ->
         Printf.fprintf oc "%s %s\n" (Bigint.to_string a) (Bigint.to_string b))
      inputs;
    close_out oc;
    (* write + run the oracle *)
    let sc = open_out script in
    output_string sc python_oracle_script;
    close_out sc;
    let rc =
      Sys.command
        (Printf.sprintf
           "python3 %s %s %s"
           (Filename.quote script)
           (Filename.quote inp)
           (Filename.quote outp))
    in
    check "python oracle ran" (rc = 0);
    if rc = 0
    then (
      let ic = open_in outp in
      Array.iter
        (fun (a, b) ->
           let line = input_line ic in
           match String.split_on_char ' ' line with
           | [ p_add; p_sub; p_mul; p_q; p_r; p_gcd ] ->
             check "oracle add" (Bigint.to_string (Bigint.add a b) = p_add);
             check "oracle sub" (Bigint.to_string (Bigint.sub a b) = p_sub);
             check "oracle mul" (Bigint.to_string (Bigint.mul a b) = p_mul);
             check "oracle gcd" (Bigint.to_string (Bigint.gcd a b) = p_gcd);
             if not (Bigint.is_zero b)
             then (
               let q, r = Bigint.divmod a b in
               check "oracle q" (Bigint.to_string q = p_q);
               check "oracle r" (Bigint.to_string r = p_r))
           | _ -> check "oracle line parse" false)
        inputs;
      close_in ic);
    (try Sys.remove inp with
     | _ -> ());
    (try Sys.remove outp with
     | _ -> ());
    try Sys.remove script with
    | _ -> ())
;;

let () =
  test_boundaries ();
  test_fits ();
  test_properties ();
  test_deep_growth ();
  test_knuth_addback ();
  test_oracle ();
  Printf.printf "\n%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
