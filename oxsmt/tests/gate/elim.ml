(* Euclidean div/mod elimination for the gate's UNSAT (grind) direction.

   grind has no Euclidean [Int.ediv]/[Int.emod] reasoning (NOTES.md exp10), so a query
   with [div]/[mod] left intact would only ever go INCONCLUSIVE in the unsat direction.
   [eliminate] rewrites each [(div x d)] / [(mod x d)] whose divisor [d] is a nonzero
   integer literal into a fresh nullary integer symbol plus the euclidean side constraints

   x = d*q + r and 0 <= r < |d|

   returning [q] for [div] and [r] for [mod]. This is EXACTLY the rewrite smt/preprocess's
   [div_mod_elimination] performs (preprocess.ml: [rhs = d*q + r], [0 <= r], [r < |d|]) —
   the gate must encode the theory the solver solves (encoder-ADR pin). Two deliberate,
   sound divergences from preprocess, noted for the reviewer:
   - The gate keeps numerals as arbitrary-precision decimal strings, so it has no
     [min_int] overflow on [|d|] (preprocess raises [Term.Overflow] at [min_int], a
     native-int ceiling per ADR-0003). The euclidean SEMANTICS are identical; the gate's
     representable divisor range is a strict superset, so it is never LESS sound.
   - The gate does NOT share one witness across equal (dividend, divisor) occurrences
     (preprocess memoises). Per-occurrence witnesses are logically equivalent because the
     euclidean q,r are UNIQUE given x and d, so the constraints entail equality of any two
     witnesses for the same operands; satisfiability — hence the certified verdict — is
     unchanged. Skipping the cache keeps this TCB pass obviously correct.

   Equisatisfiability (why the unsat verdict transfers): the original asserts A(x) with
   div/mod are unsat iff [exists x. A(x)] is false. With q,r introduced and constrained by
   C(x,q,r) (euclidean), [exists x,q,r. A'(x,q,r) AND C] is equivalent because C pins q,r
   to the unique euclidean values of x,d, so A'(x,q,r) = A(x). [Encoder.encode_unsat]
   proves [forall x,q,r. A' -> C -> False], i.e. the eliminated query is unsat, iff the
   original is. The fresh q,r become universally-bound Lean hyp binders, exactly as
   needed.

   Divisor restriction (fail closed, matching the solver's theory): the divisor must be a
   nonzero integer literal (or its negation). A ZERO divisor (SMT-LIB leaves
   div/mod-by-zero unconstrained; smt/preprocess rejects it) and a NON-LITERAL / variable
   divisor (v1 solves linear only) both stay [Reader.Unsupported] — quarantined, never
   certified. [check_divisors] is the single authoritative enforcement point and MUST be
   run as a preflight before either encoding direction (see gate.ml); the SAT direction
   emits [Int.ediv]/[Int.emod] directly (decide computes them on the concrete model) and
   equally relies on [check_divisors] so the gate certifies EXACTLY the div/mod theory the
   solver handles — never a variable-divisor model the solver would reject. *)

open Ast

(* Apply [f] to [t] and every subterm (pre-order). *)
let rec iter (f : term -> unit) (t : term) : unit =
  f t;
  match t with
  | True | False | Int_lit _ | Const _ -> ()
  | Not a | Neg a -> iter f a
  | App (_, xs) | And xs | Or xs | Distinct xs | Add xs | Sub xs | Mul xs ->
    List.iter (iter f) xs
  | Implies (a, b)
  | Eq (a, b)
  | Iff (a, b)
  | Le (a, b)
  | Lt (a, b)
  | Ge (a, b)
  | Gt (a, b)
  | Div (a, b)
  | Mod (a, b) ->
    iter f a;
    iter f b
  | Ite (a, b, c) ->
    iter f a;
    iter f b;
    iter f c
;;

let uses_divmod (q : query) : bool =
  let found = ref false in
  let check = function
    | Div _ | Mod _ -> found := true
    | _ -> ()
  in
  List.iter (iter check) q.asserts;
  !found
;;

let is_zero_numeral k = String.length k = 0 || String.for_all (fun c -> c = '0') k

(* Authoritative divisor check for BOTH directions. A div/mod whose divisor is not a
   nonzero integer literal is UNSUPPORTED (fail closed), matching smt/preprocess. *)
let check_divisors (q : query) : unit =
  let check = function
    | Div (_, d) | Mod (_, d) ->
      (match d with
       | Int_lit k | Neg (Int_lit k) ->
         if is_zero_numeral k
         then
           raise
             (Reader.Unsupported
                "div/mod by zero is not certifiable (SMT-LIB leaves it unconstrained; \
                 the solver rejects it)")
       | _ ->
         raise
           (Reader.Unsupported
              "div/mod by a non-constant divisor is not supported (v1 solves linear \
               only); fail closed"))
    | _ -> ()
  in
  List.iter (iter check) q.asserts
;;

(* Strip leading zeros so the emitted magnitude is a clean Lean decimal (keep one digit). *)
let canon_numeral s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n - 1 && s.[!i] = '0' do
    incr i
  done;
  String.sub s !i (n - !i)
;;

(* A validated divisor yields its normalized term (for [d*q]) and magnitude [|d|]. *)
let divisor (d : term) : [ `Ok of term * string | `Bad ] =
  match d with
  | Int_lit k when not (is_zero_numeral k) ->
    let m = canon_numeral k in
    `Ok (Int_lit m, m)
  | Neg (Int_lit k) when not (is_zero_numeral k) ->
    let m = canon_numeral k in
    `Ok (Neg (Int_lit m), m)
  | _ -> `Bad
;;

let eliminate (q : query) : query =
  let counter = ref 0 in
  let decls =
    ref []
    (* fresh (name, [], Int), reversed *)
  in
  let constraints =
    ref []
    (* euclidean constraints, reversed *)
  in
  let fresh kind =
    let name = Printf.sprintf "%s%s.%d" Reader.reserved_prefix kind !counter in
    incr counter;
    decls := (name, [], Int) :: !decls;
    name
  in
  (* Fresh witness (q,r) for [x' `div`/`mod` d], with d the normalized divisor term and
     [mag] its magnitude. Emits x = d*q + r, 0 <= r, r < |d|. *)
  let witness x' dterm mag =
    let qn = fresh "q" in
    let rn = fresh "r" in
    let q = Const qn
    and r = Const rn in
    constraints
    := Lt (r, Int_lit mag)
       :: Le (Int_lit "0", r)
       :: Eq (x', Add [ Mul [ dterm; q ]; r ])
       :: !constraints;
    q, r
  in
  let rec go (t : term) : term =
    match t with
    | True | False | Int_lit _ | Const _ -> t
    | App (f, xs) -> App (f, List.map go xs)
    | Not a -> Not (go a)
    | And xs -> And (List.map go xs)
    | Or xs -> Or (List.map go xs)
    | Implies (a, b) -> Implies (go a, go b)
    | Ite (a, b, c) -> Ite (go a, go b, go c)
    | Eq (a, b) -> Eq (go a, go b)
    | Iff (a, b) -> Iff (go a, go b)
    | Distinct xs -> Distinct (List.map go xs)
    | Le (a, b) -> Le (go a, go b)
    | Lt (a, b) -> Lt (go a, go b)
    | Ge (a, b) -> Ge (go a, go b)
    | Gt (a, b) -> Gt (go a, go b)
    | Add xs -> Add (List.map go xs)
    | Sub xs -> Sub (List.map go xs)
    | Neg a -> Neg (go a)
    | Mul xs -> Mul (List.map go xs)
    | Div (x, d) ->
      (match divisor d with
       | `Ok (dterm, mag) ->
         let q, _ = witness (go x) dterm mag in
         q
       (* Fail-safe: leave a residual div (-> INCONCLUSIVE via the encoder), never raise;
          [check_divisors] is the authoritative reject in the gate's preflight. *)
       | `Bad -> Div (go x, go d))
    | Mod (x, d) ->
      (match divisor d with
       | `Ok (dterm, mag) ->
         let _, r = witness (go x) dterm mag in
         r
       | `Bad -> Mod (go x, go d))
  in
  let asserts' = List.map go q.asserts in
  { q with
    fun_decls = q.fun_decls @ List.rev !decls
  ; asserts = asserts' @ List.rev !constraints
  }
;;
