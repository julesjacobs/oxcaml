(* Focused self-test for the one-word (Zarith-style) {!Rational} representation: an int63
   integer is an IMMEDIATE tagged int (den = 1, zero-alloc), everything else a pointer to
   a {!Bigint}-backed block, discriminated by [Obj.is_int] behind the private
   {!Rational_word} cast module. This suite pins that arithmetic crosses the
   immediate/block boundary BOTH ways and stays exact (user hard constraints 2 and 3):
   - immediate ⊗ immediate that stays immediate;
   - immediate ⊗ immediate that OVERFLOWS int63 -> block (promotion UP);
   - immediate ⊗ block and block ⊗ immediate (mixed);
   - block ⊗ block that DEMOTES back to an immediate (promotion DOWN);
   - fraction results (den <> 1) stay blocks.

   It is deliberately SIMPLEX-FREE and fast (pure {!Rational} arithmetic, no solver), so
   it is the mutation-testing suite for the rational-word mutants: the [poly-compare] and
   [wrap-instead-of-promote] mutants turn it RED and it always TERMINATES (a mis-ordering
   compare can send the branch-and-bound loop in lia-test non-terminating, so the rep
   mutants target this suite instead). Stdlib-only (I3); the differential oracle shells
   out to python3 via [Sys.command] (no library dep) and is SKIPPED if python3 is absent,
   matching bigint_test. Deterministic: fixed-seed xorshift PRNG, no wall-clock. *)

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

(* ---- fixed-seed PRNG (xorshift64-star), matching lia_test/bigint_test ---- *)
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
let q = Rational.of_int
let qf = Rational.of_frac
let s = Rational.of_string

(* Observe the FULL three-tier representation through the PUBLIC API only (the module is
   abstract), so the suite can assert which physical arm a value lands on (codex fix-round
   LOW: the earlier 3-way [rep_of] conflated a small [Frac] with a big fraction). The
   canonical invariant maps to observable behaviour:
   - is_int && num-succeeds <=> immediate integer (den = 1, fits int63) -> `Imm
   - is_int && num-raises <=> Big integer (den = 1, |num| > int63) -> `Big_int
   - not is_int && num,den succeed <=> small [Frac] (both components fit int63) -> `Frac
   - not is_int && num|den raises <=> Big fraction (a component > int63) -> `Big_frac
     [`Imm] is the zero-alloc integer path; [`Frac] is the native-int fraction path. *)
let rep_of x =
  if Rational.is_int x
  then (
    match Rational.num x with
    | _ -> `Imm
    | exception Rational.Overflow -> `Big_int)
  else (
    match Rational.num x, Rational.den x with
    | _, _ -> `Frac
    | exception Rational.Overflow -> `Big_frac)
;;

let test_rep () =
  print_endline "one-word rep boundary (Zarith):";
  (* --- immediate <-> block discrimination on the constructors --- *)
  check "of_int 5 is immediate" (rep_of (q 5) = `Imm);
  check "of_int max_int is immediate" (rep_of (q max_int) = `Imm);
  check "of_int min_int is immediate" (rep_of (q min_int) = `Imm);
  check "zero is immediate" (rep_of Rational.zero = `Imm);
  check "of_frac 1 2 is a fraction block" (rep_of (qf 1 2) = `Frac);
  check
    "2^62 (over int63) is a Big-integer block"
    (rep_of (s "4611686018427387904") = `Big_int);
  check
    "of_frac 6 3 demotes to immediate 2"
    (rep_of (qf 6 3) = `Imm && Rational.equal (qf 6 3) (q 2));
  (* --- promotion UP: immediate ⊗ immediate overflow -> block, exact value --- *)
  check
    "max_int + 1 promotes UP to a block = 2^62"
    (rep_of (Rational.add (q max_int) (q 1)) = `Big_int
     && Rational.to_string (Rational.add (q max_int) (q 1)) = "4611686018427387904");
  check
    "min_int * 2 promotes UP to a block = -2^63"
    (rep_of (Rational.mul (q min_int) (q 2)) = `Big_int
     && Rational.to_string (Rational.mul (q min_int) (q 2)) = "-9223372036854775808");
  check
    "max_int - min_int promotes UP to a block = 2^63-1"
    (rep_of (Rational.sub (q max_int) (q min_int)) = `Big_int
     && Rational.to_string (Rational.sub (q max_int) (q min_int)) = "9223372036854775807"
    );
  check
    "neg min_int promotes UP to a block = 2^62"
    (rep_of (Rational.neg (q min_int)) = `Big_int
     && Rational.to_string (Rational.neg (q min_int)) = "4611686018427387904");
  (* --- mixed: immediate ⊗ block and block ⊗ immediate --- *)
  let big =
    s "10000000000000000000"
    (* 10^19 > int63, a block *)
  in
  check
    "mixed imm+block: 3 + 10^19"
    (Rational.to_string (Rational.add (q 3) big) = "10000000000000000003");
  check
    "mixed block+imm: 10^19 + 3"
    (Rational.to_string (Rational.add big (q 3)) = "10000000000000000003");
  check
    "mixed block*imm stays a block"
    (rep_of (Rational.mul big (q 2)) = `Big_int
     && Rational.to_string (Rational.mul big (q 2)) = "20000000000000000000");
  (* --- promotion DOWN: block ⊗ block (and fraction ops) that DEMOTE to an immediate --- *)
  check
    "block - block demotes DOWN to immediate 1"
    (rep_of (Rational.sub (s "4611686018427387904") (s "4611686018427387903")) = `Imm
     && Rational.to_string
          (Rational.sub (s "4611686018427387904") (s "4611686018427387903"))
        = "1");
  check
    "block / block demotes DOWN to immediate 2"
    (rep_of (Rational.div (s "20000000000000000000") (s "10000000000000000000")) = `Imm
     && Rational.equal
          (Rational.div (s "20000000000000000000") (s "10000000000000000000"))
          (q 2));
  check
    "fraction op demotes DOWN to immediate: (1/2) * 2 = 1"
    (rep_of (Rational.mul (qf 1 2) (q 2)) = `Imm
     && Rational.equal (Rational.mul (qf 1 2) (q 2)) (q 1));
  (* --- fraction results stay blocks --- *)
  check
    "1/2 + 1/3 = 5/6 stays a fraction block"
    (rep_of (Rational.add (qf 1 2) (qf 1 3)) = `Frac);
  check
    "small fraction 1/2 is a native `Frac (both components fit int63)"
    (rep_of (qf 1 2) = `Frac);
  check
    "big fraction 1/(10^19) is a `Big_frac (denominator exceeds int63)"
    (rep_of (Rational.div (q 1) big) = `Big_frac
     && Rational.to_string (Rational.div (q 1) big) = "1/10000000000000000000");
  (* --- poly-compare hazard: an immediate vs a large-NEGATIVE block. Structural
     [Stdlib.compare] orders every immediate BEFORE every block, so it returns the WRONG
     SIGN here; the value-based [Rational.compare] must not. These vectors kill the
     rational-word-poly-compare mutant. --- *)
  let neg_block =
    s "-4611686018427387905"
    (* -(2^62 + 1), below int63 min: a block *)
  in
  check "neg_block is a Big-integer block" (rep_of neg_block = `Big_int);
  check
    "compare 0 (-2^62-1) > 0 (immediate vs negative block)"
    (Rational.compare (q 0) neg_block > 0);
  check
    "compare (-2^62-1) 0 < 0 (negative block vs immediate)"
    (Rational.compare neg_block (q 0) < 0);
  check "compare max_int (-2^62-1) > 0" (Rational.compare (q max_int) neg_block > 0);
  (* --- serialization round-trip (sanctioned in place of Marshal, user constraint 2) --- *)
  List.iter
    (fun str ->
       check
         (Printf.sprintf "to_string/of_string round-trip %s" str)
         (Rational.to_string (s str) = str))
    [ "0"
    ; "1"
    ; "-1"
    ; "5"
    ; "1/2"
    ; "-3/4"
    ; "4611686018427387904"
    ; "-4611686018427387905"
    ; "1/10000000000000000000"
    ; "10000000000000000003"
    ]
;;

(* A random canonical decimal string (optional '-', no leading zeros, "0" allowed), up to
   [maxdigits] long so magnitudes span from immediate through many-limb block. *)
let rand_bigdec maxdigits =
  if rand_int 12 = 0
  then "0"
  else (
    let d = 1 + rand_int maxdigits in
    let b = Buffer.create (d + 2) in
    if rand_int 2 = 0 then Buffer.add_char b '-';
    Buffer.add_char b (Char.chr (Char.code '1' + rand_int 9));
    for _ = 2 to d do
      Buffer.add_char b (Char.chr (Char.code '0' + rand_int 10))
    done;
    Buffer.contents b)
;;

(* A numerator string biased to cross the int63 boundary: small integers (stay immediate),
   values pinned AT the int63 boundary (force overflow on the next op), and big decimals
   (force blocks). *)
let rand_num () =
  match rand_int 6 with
  | 0 -> string_of_int (rand_range (-1000) 1000)
  | 1 -> string_of_int (max_int - rand_int 3)
  | 2 -> string_of_int (min_int + rand_int 3)
  | 3 | 4 -> rand_bigdec 24
  | _ -> rand_bigdec 6
;;

(* A positive denominator string: usually 1 (integers dominate LIA), sometimes a small
   fraction, occasionally a big one (big fraction blocks). *)
let rand_den () =
  match rand_int 5 with
  | 0 | 1 | 2 -> "1"
  | 3 -> string_of_int (rand_range 2 24)
  | _ ->
    let d = rand_bigdec 20 in
    if String.length d > 0 && d.[0] = '-' then String.sub d 1 (String.length d - 1) else d
;;

let rand_den_nonzero () =
  let d = rand_den () in
  if d = "0" then "1" else d
;;

let have_python () = Sys.command "python3 -c '' >/dev/null 2>&1" = 0

(* Independent differential oracle in a DIFFERENT arithmetic (Python's exact
   [fractions.Fraction]), crossing the immediate/block boundary in both directions.
   Skipped (not failed) if python3 is absent. Also self-checks that the generated
   population actually EXERCISES both promotion directions (else the oracle would be
   silently one-sided). *)
let python_rat_oracle =
  {py|import sys
from fractions import Fraction
inp, outp = sys.argv[1], sys.argv[2]
def canon(fr):
    return str(fr.numerator) if fr.denominator == 1 else "%d/%d" % (fr.numerator, fr.denominator)
with open(inp) as f, open(outp, "w") as o:
    for line in f:
        line = line.strip()
        if not line:
            continue
        an, ad, bn, bd = line.split()
        a = Fraction(int(an), int(ad)); b = Fraction(int(bn), int(bd))
        div = "NA" if b == 0 else canon(a / b)
        c = (a > b) - (a < b)
        o.write("%s|%s|%s|%s|%d\n" % (canon(a + b), canon(a - b), canon(a * b), div, c))
|py}
;;

(* min_int at the tier boundary (fable + codex fix-round rider). [bnorm_demote] can
   produce BOTH the immediate min_int and a [Frac { min_int; d }] (min_int fits int63),
   and native [-min_int]/[abs min_int] WRAP (a regression the guarded base avoided). neg
   must be a true additive inverse and abs must be nonnegative in every tier around
   min_int; the results promote to [Big] (|value| exceeds int63). These vectors kill an
   unguarded neg/abs. *)
let test_min_int () =
  print_endline "min_int tier boundary (neg/abs must not wrap):";
  let min_s =
    "-4611686018427387904"
    (* min_int = -2^62 *)
  in
  let pos_s =
    "4611686018427387904"
    (* 2^62 = -min_int, does NOT fit int63 *)
  in
  (* immediate min_int (den = 1) *)
  let mi = q min_int in
  check "immediate min_int is `Imm" (rep_of mi = `Imm);
  check "neg(min_int) = 2^62, a Big block" (Rational.to_string (Rational.neg mi) = pos_s);
  check "abs(min_int) = 2^62, a Big block" (Rational.to_string (Rational.abs mi) = pos_s);
  check "neg(min_int) is nonneg" (Rational.sign (Rational.neg mi) > 0);
  check "abs(min_int) is nonneg" (Rational.sign (Rational.abs mi) >= 0);
  check
    "min_int + neg(min_int) = 0"
    (Rational.is_zero (Rational.add mi (Rational.neg mi)));
  (* Frac { min_int; d } — reachable via bnorm_demote (den <> 1, min_int fits) *)
  List.iter
    (fun d ->
       let fr =
         s (min_s ^ "/" ^ string_of_int d)
         (* min_int / d, a Frac *)
       in
       check (Printf.sprintf "min_int/%d is a `Frac" d) (rep_of fr = `Frac);
       let expect = pos_s ^ "/" ^ string_of_int d in
       check
         (Printf.sprintf "neg(min_int/%d) = 2^62/%d (Big), not wrapped-negative" d d)
         (Rational.to_string (Rational.neg fr) = expect);
       check
         (Printf.sprintf "abs(min_int/%d) = 2^62/%d (Big), not wrapped-negative" d d)
         (Rational.to_string (Rational.abs fr) = expect);
       check
         (Printf.sprintf "abs(min_int/%d) is nonneg" d)
         (Rational.sign (Rational.abs fr) > 0);
       check
         (Printf.sprintf "min_int/%d + neg = 0" d)
         (Rational.is_zero (Rational.add fr (Rational.neg fr))))
    [ 3; 5; 7; 4611686018427387903 (* odd/coprime and a large denominator *) ];
  (* codex-LOW close: neg/abs of a Big fraction. neg(2^62/d) = min_int/d, which is
     [Frac]-representable (min_int fits int63 while +2^62 does not — the int63 asymmetry),
     so the Big neg/abs arm now re-canonicalizes via bnorm_demote instead of rebuilding a
     Big. NOTE this re-demote is NOT observable through the abstract API — a physical
     Big{-2^62,d} and Frac{min_int,d} have identical to_string/num/den/compare (that is why
     the LOW was inert). So these pin the VALUE of neg/abs on a Big fraction — net-new
     coverage, since neg/abs are not in the Python differential oracle — which a wrong-value
     regression on that arm would break; the physical re-canonicalization is internal purity
     that keeps canonical-uniqueness literally true. *)
  List.iter
    (fun d ->
       let big_frac =
         s (pos_s ^ "/" ^ string_of_int d)
         (* 2^62/d, a Big fraction *)
       in
       check (Printf.sprintf "2^62/%d is a `Big_frac" d) (rep_of big_frac = `Big_frac);
       check
         (Printf.sprintf "neg(2^62/%d) = min_int/%d (canonical string)" d d)
         (Rational.to_string (Rational.neg big_frac) = min_s ^ "/" ^ string_of_int d);
       check
         (Printf.sprintf "abs(2^62/%d) = 2^62/%d" d d)
         (Rational.to_string (Rational.abs big_frac) = pos_s ^ "/" ^ string_of_int d);
       check
         (Printf.sprintf "neg is an involution on 2^62/%d" d)
         (Rational.equal (Rational.neg (Rational.neg big_frac)) big_frac))
    [ 3; 5; 7 ]
;;

let test_oracle () =
  print_endline "differential oracle (Python Fraction, boundary-crossing):";
  if not (have_python ())
  then print_endline "  SKIP (python3 not available)"
  else (
    let n = 6000 in
    let pairs =
      Array.init n (fun _ ->
        rand_num (), rand_den_nonzero (), rand_num (), rand_den_nonzero ())
    in
    let inp = Filename.temp_file "rat_oracle_in" ".txt" in
    let outp = Filename.temp_file "rat_oracle_out" ".txt" in
    let script = Filename.temp_file "rat_oracle" ".py" in
    let oc = open_out inp in
    Array.iter
      (fun (an, ad, bn, bd) -> Printf.fprintf oc "%s %s %s %s\n" an ad bn bd)
      pairs;
    close_out oc;
    let sc = open_out script in
    output_string sc python_rat_oracle;
    close_out sc;
    let rc =
      Sys.command
        (Printf.sprintf
           "python3 %s %s %s"
           (Filename.quote script)
           (Filename.quote inp)
           (Filename.quote outp))
    in
    check "rat oracle ran" (rc = 0);
    let saw_promote_up = ref false
    and saw_demote_down = ref false
    and saw_frac = ref false
    and saw_mixed = ref false in
    if rc = 0
    then (
      let ic = open_in outp in
      Array.iter
        (fun (an, ad, bn, bd) ->
           let mk n d = s (n ^ "/" ^ d) in
           let a = mk an ad
           and b = mk bn bd in
           let line = input_line ic in
           (match String.split_on_char '|' line with
            | [ p_add; p_sub; p_mul; p_div; p_c ] ->
              check "oracle add" (Rational.to_string (Rational.add a b) = p_add);
              check "oracle sub" (Rational.to_string (Rational.sub a b) = p_sub);
              check "oracle mul" (Rational.to_string (Rational.mul a b) = p_mul);
              check
                "oracle compare sign"
                (Int.compare (Rational.compare a b) 0 = int_of_string p_c);
              if p_div <> "NA"
              then check "oracle div" (Rational.to_string (Rational.div a b) = p_div)
            | _ -> check "oracle line parse" false);
           let ra = rep_of a
           and rb = rep_of b
           and rsum = rep_of (Rational.add a b) in
           if ra = `Imm && rb = `Imm && rsum <> `Imm then saw_promote_up := true;
           if (ra <> `Imm || rb <> `Imm) && rsum = `Imm then saw_demote_down := true;
           if rsum = `Frac || rsum = `Big_frac then saw_frac := true;
           if ra = `Imm <> (rb = `Imm) then saw_mixed := true)
        pairs;
      close_in ic);
    check "oracle exercised promotion UP (imm+imm -> block)" !saw_promote_up;
    check "oracle exercised promotion DOWN (block involved -> imm)" !saw_demote_down;
    check "oracle exercised fraction-block results" !saw_frac;
    check "oracle exercised mixed imm/block operands" !saw_mixed;
    (try Sys.remove inp with
     | _ -> ());
    (try Sys.remove outp with
     | _ -> ());
    try Sys.remove script with
    | _ -> ())
;;

let () =
  print_endline "rational-word self-test:";
  test_rep ();
  test_min_int ();
  test_oracle ();
  Printf.printf "\nrational-word self-test: %d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;
