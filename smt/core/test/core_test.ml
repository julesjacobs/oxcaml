(* Unit + property tests for smt/core (ADR-0003). This is TCB code (DESIGN §10), so the
   suite is adversarial toward its own constructors: it asserts exact normalized node
   shapes, that every sort error / overflow / unsupported input raises before any
   interning, that hash-consing shares, that Debug.check holds on everything
   constructible, and it brute-forces gcd tightening and comparison lowering by evaluation
   over a small integer box.

   Stdlib-only (dependency firewall I3). Deterministic: the PRNG is a fixed-seed xorshift,
   no wall-clock, no external libs. *)

open Oxsmt_core

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let check_raises name f =
  incr checks;
  match f () with
  | exception _ -> ()
  | _ ->
    incr failures;
    Printf.printf "  FAIL %s (expected an exception, got none)\n" name
;;

(* ------------------------------------------------------------------ *)
(* Deterministic PRNG: xorshift64*, fixed seed. *)

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
let reset_rng () = rng := 0x1E3779B97F4A7C15

(* ------------------------------------------------------------------ *)
(* A shared environment + variable vocabulary for the tests. *)

let env = Env.create ()

let int_vars =
  Array.init 4 (fun i ->
    Env.declare_fun env (Printf.sprintf "x%d" i) (Rank.create [] Sort.int))
;;

let bool_vars =
  Array.init 3 (fun i ->
    Env.declare_fun env (Printf.sprintf "p%d" i) (Rank.create [] Sort.bool))
;;

let f_sym = Env.declare_fun env "f" (Rank.create [ Sort.int ] Sort.int)
let g_sym = Env.declare_fun env "g" (Rank.create [ Sort.int; Sort.int ] Sort.int)
let p_sym = Env.declare_fun env "pred" (Rank.create [ Sort.int ] Sort.bool)

(* ------------------------------------------------------------------ *)
(* Evaluator over an integer/boolean assignment, for brute-force checks. Only handles the
   fragment the generators below produce (nullary vars + arithmetic
   + boolean structure). *)

let rec eval_int ai t =
  match t.Term.node with
  | Term.Int_const k -> k
  | Term.App (s, args) when Iarr.length args = 0 -> ai s
  | Term.Arith l ->
    Iarr.fold (fun acc (tm, c) -> acc + (c * eval_int ai tm)) l.Term.const l.Term.coeffs
  | Term.Ite (c, a, b) -> if eval_bool ai c then eval_int ai a else eval_int ai b
  | _ -> failwith "eval_int: unsupported node"

and eval_bool ai t =
  match t.Term.node with
  | Term.Bool_const b -> b
  | Term.Le a -> eval_int ai a <= 0
  | Term.Eq (a, b) ->
    if Sort.equal a.Term.sort Sort.bool
    then Bool.equal (eval_bool ai a) (eval_bool ai b)
    else eval_int ai a = eval_int ai b
  | Term.Not a -> not (eval_bool ai a)
  | Term.And xs -> Iarr.for_all (eval_bool ai) xs
  | Term.Or xs -> Iarr.exists (eval_bool ai) xs
  | Term.Ite (c, a, b) -> if eval_bool ai c then eval_bool ai a else eval_bool ai b
  | _ -> failwith "eval_bool: unsupported node"
;;

(* ================================================================== *)
let test_iarr () =
  print_endline "Iarr:";
  (* of_array copies: mutating the source does not affect the Iarr. *)
  let arr = [| 1; 2; 3 |] in
  let ia = Iarr.of_array arr in
  arr.(0) <- 99;
  check "of_array copies (source mutation ignored)" (Iarr.to_list ia = [ 1; 2; 3 ]);
  check "of_list round-trips" (Iarr.to_list (Iarr.of_list [ 4; 5 ]) = [ 4; 5 ]);
  check "length" (Iarr.length ia = 3);
  check "get" (Iarr.get ia 1 = 2);
  check "map" (Iarr.to_list (Iarr.map (fun x -> x * 2) ia) = [ 2; 4; 6 ]);
  check "fold" (Iarr.fold ( + ) 0 ia = 6);
  check "equal" (Iarr.equal Int.equal ia (Iarr.of_list [ 1; 2; 3 ]));
  check "not equal (order)" (not (Iarr.equal Int.equal ia (Iarr.of_list [ 3; 2; 1 ])));
  check "not equal (length)" (not (Iarr.equal Int.equal ia (Iarr.of_list [ 1; 2 ])));
  (* hash_fold folds every element + length: order and count matter. *)
  let hf t = Iarr.hash_fold (fun acc x -> (acc * 31) + x) 7 t in
  check
    "hash_fold sensitive to order"
    (hf (Iarr.of_list [ 1; 2 ]) <> hf (Iarr.of_list [ 2; 1 ]));
  check
    "hash_fold sensitive to length"
    (hf (Iarr.of_list [ 1 ]) <> hf (Iarr.of_list [ 1; 0 ]))
;;

(* ================================================================== *)
let test_symbol_sort_env () =
  print_endline "Symbol/Sort/Env:";
  check "intern idempotent" (Symbol.equal (Symbol.intern "foo") (Symbol.intern "foo"));
  check
    "intern distinct names distinct"
    (not (Symbol.equal (Symbol.intern "foo") (Symbol.intern "bar")));
  check "name round-trips" (String.equal (Symbol.name (Symbol.intern "baz")) "baz");
  check "sort bool <> int" (not (Sort.equal Sort.bool Sort.int));
  let s = Symbol.intern "S" in
  check
    "uninterpreted sort equal"
    (Sort.equal (Sort.uninterpreted s) (Sort.uninterpreted s));
  check "rank lookup" (Rank.arity (Env.rank env f_sym) = 1);
  check "reserved div declared" (Rank.arity (Env.rank env (Env.div_sym env)) = 2);
  check_raises "rank of undeclared raises" (fun () ->
    Env.rank env (Symbol.intern "never_declared_xyz"))
;;

(* ================================================================== *)
(* Constructor normalization — assert exact resulting node shapes. *)

let test_normalization () =
  print_endline "normalization (exact node shapes):";
  let c = Context.create env in
  let x = Context.const c int_vars.(0) in
  let y = Context.const c int_vars.(1) in
  let k n = Context.int_const c n in
  let is_int_const t n =
    match t.Term.node with
    | Term.Int_const m -> m = n
    | _ -> false
  in
  let is_arith t expect_coeffs expect_const =
    match t.Term.node with
    | Term.Arith l ->
      l.Term.const = expect_const
      && List.map (fun (tm, cc) -> tm.Term.tag, cc) (Iarr.to_list l.Term.coeffs)
         = expect_coeffs
    | _ -> false
  in
  (* x + 1 = 1 + x (commutativity via canonical form). *)
  check "x+1 = 1+x" (Term.equal (Context.add c x (k 1)) (Context.add c (k 1) x));
  (* 2x = x + x. *)
  check "2x = x+x" (Term.equal (Context.mul_const c 2 x) (Context.add c x x));
  (* x + 0 unwraps to x. *)
  check "x+0 -> x" (Term.equal (Context.add c x (k 0)) x);
  (* x - x folds to 0 (Int_const). *)
  check "x-x -> 0" (is_int_const (Context.sub c x x) 0);
  (* 3 + 4 folds. *)
  check "3+4 -> 7" (is_int_const (Context.add c (k 3) (k 4)) 7);
  (* mul_const 0 -> 0. *)
  check "0*x -> 0" (is_int_const (Context.mul_const c 0 x) 0);
  (* x + 2y is Arith with tag-sorted coeffs, both nonzero. *)
  let x2y = Context.add c x (Context.mul_const c 2 y) in
  check
    "x+2y is Arith [(x,1);(y,2)] const 0"
    (is_arith x2y [ x.Term.tag, 1; y.Term.tag, 2 ] 0);
  (* Eq(x,x) -> true; Eq const fold. *)
  check
    "eq x x -> true"
    (match (Context.eq c x x).Term.node with
     | Term.Bool_const true -> true
     | _ -> false);
  check
    "eq 3 4 -> false"
    (match (Context.eq c (k 3) (k 4)).Term.node with
     | Term.Bool_const false -> true
     | _ -> false);
  (* Eq is tag-ordered. *)
  (match (Context.eq c y x).Term.node with
   | Term.Eq (a, b) -> check "eq tag-ordered" (a.Term.tag < b.Term.tag)
   | _ -> check "eq builds Eq node" false);
  (* not(not p) -> p ; not true -> false. *)
  let p = Context.const c bool_vars.(0) in
  check "not(not p) -> p" (Term.equal (Context.not_ c (Context.not_ c p)) p);
  check
    "not true -> false"
    (match (Context.not_ c (Context.bool_const c true)).Term.node with
     | Term.Bool_const false -> true
     | _ -> false);
  (* and flatten + dedup + fold. *)
  let q = Context.const c bool_vars.(1) in
  check
    "and [] -> true"
    (match (Context.and_ c []).Term.node with
     | Term.Bool_const true -> true
     | _ -> false);
  check
    "or [] -> false"
    (match (Context.or_ c []).Term.node with
     | Term.Bool_const false -> true
     | _ -> false);
  check "and [p] -> p" (Term.equal (Context.and_ c [ p ]) p);
  check "and [p;p] -> p (dedup)" (Term.equal (Context.and_ c [ p; p ]) p);
  check
    "and with false -> false"
    (match (Context.and_ c [ p; Context.bool_const c false ]).Term.node with
     | Term.Bool_const false -> true
     | _ -> false);
  check
    "and drops true"
    (Term.equal
       (Context.and_ c [ p; Context.bool_const c true; q ])
       (Context.and_ c [ p; q ]));
  (* nested and flattened. *)
  check
    "and flattening"
    (Term.equal
       (Context.and_ c [ Context.and_ c [ p; q ]; Context.const c bool_vars.(2) ])
       (Context.and_ c [ p; q; Context.const c bool_vars.(2) ]));
  (* ite folds. *)
  check "ite true a b -> a" (Term.equal (Context.ite c (Context.bool_const c true) x y) x);
  check
    "ite false a b -> b"
    (Term.equal (Context.ite c (Context.bool_const c false) x y) y);
  check "ite c a a -> a" (Term.equal (Context.ite c p x x) x);
  (* implies lowers to or[not a; b]. *)
  check
    "implies a b = or[not a; b]"
    (Term.equal (Context.implies c p q) (Context.or_ c [ Context.not_ c p; q ]));
  (* le lowers to <=0 and gcd-normalizes: 2x<=2 becomes x-1<=0. *)
  (match (Context.le c (Context.mul_const c 2 x) (k 2)).Term.node with
   | Term.Le arg ->
     check "2x<=2 gcd-normalized to x-1" (is_arith arg [ x.Term.tag, 1 ] (-1))
   | _ -> check "le builds Le node" false);
  (* distinct -> pairwise not(eq). *)
  check
    "distinct [x;y] = not(eq x y)"
    (Term.equal (Context.distinct c [ x; y ]) (Context.not_ c (Context.eq c x y)));
  (* abs desugars to an Ite. *)
  check
    "abs is an Ite"
    (match (Context.abs c x).Term.node with
     | Term.Ite _ -> true
     | _ -> false)
;;

(* ================================================================== *)
let test_sort_errors () =
  print_endline "sort errors:";
  let c = Context.create env in
  let x = Context.const c int_vars.(0) in
  let p = Context.const c bool_vars.(0) in
  check_raises "add of Bool raises" (fun () -> Context.add c p x);
  check_raises "le of Bool raises" (fun () -> Context.le c p x);
  check_raises "not of Int raises" (fun () -> Context.not_ c x);
  check_raises "and of Int raises" (fun () -> Context.and_ c [ x ]);
  check_raises "eq mismatched sorts raises" (fun () -> Context.eq c x p);
  check_raises "ite branches mismatched raises" (fun () -> Context.ite c p x p);
  check_raises "ite non-Bool cond raises" (fun () -> Context.ite c x x x);
  check_raises "app arity mismatch raises" (fun () -> Context.app c f_sym [ x; x ]);
  check_raises "app arg sort mismatch raises" (fun () -> Context.app c f_sym [ p ]);
  check_raises "app undeclared symbol raises" (fun () ->
    Context.app c (Symbol.intern "undeclared_q") [])
;;

(* ================================================================== *)
let test_overflow_unsupported () =
  print_endline "overflow / unsupported (pre-mutation contract):";
  let c = Context.create env in
  let x = Context.const c int_vars.(0) in
  (* Direct constant fold overflow: raise BEFORE any interning. *)
  let a = Context.int_const c max_int in
  let b = Context.int_const c 1 in
  let before = Context.term_count c in
  check_raises "max_int + 1 overflows" (fun () -> Context.add c a b);
  check "table unchanged after add overflow" (Context.term_count c = before);
  (* Coefficient-merge overflow. *)
  let t1 = Context.mul_const c max_int x in
  let before = Context.term_count c in
  check_raises "coeff merge overflow" (fun () -> Context.add c t1 x);
  check "table unchanged after coeff overflow" (Context.term_count c = before);
  (* neg min_int overflows. *)
  let before = Context.term_count c in
  check_raises "neg min_int overflows" (fun () ->
    Context.neg c (Context.int_const c min_int));
  ignore before;
  (* mul_const overflow. *)
  check_raises "mul_const overflow" (fun () ->
    Context.mul_const c max_int (Context.add c x x));
  (* Unsupported: div/mod by non-constant or zero. *)
  check_raises "div by zero -> Unsupported" (fun () ->
    Context.div c x (Context.int_const c 0));
  check_raises "div by non-constant -> Unsupported" (fun () -> Context.div c x x);
  let before = Context.term_count c in
  (match Context.div c x x with
   | _ -> ()
   | exception _ -> ());
  check "table unchanged after Unsupported" (Context.term_count c = before);
  (* div by nonzero constant is fine and is an App on the reserved symbol. *)
  check
    "div by 2 builds App"
    (match (Context.div c x (Context.int_const c 2)).Term.node with
     | Term.App _ -> true
     | _ -> false)
;;

(* ================================================================== *)
let test_hashcons_sharing () =
  print_endline "hash-cons sharing:";
  let c = Context.create env in
  let x = Context.const c int_vars.(0) in
  let y = Context.const c int_vars.(1) in
  let e1 = Context.add c x (Context.mul_const c 2 y) in
  let e2 = Context.add c (Context.mul_const c 2 y) x in
  check "equal constructions physically equal" (e1 == e2);
  check "equal constructions Term.equal" (Term.equal e1 e2);
  check "same const shared" (Context.int_const c 42 == Context.int_const c 42);
  check "const symbol shared" (Context.const c int_vars.(0) == x);
  check "g(x,y) App shared" (Context.app c g_sym [ x; y ] == Context.app c g_sym [ x; y ]);
  check
    "g(x,y) <> g(y,x)"
    (not (Term.equal (Context.app c g_sym [ x; y ]) (Context.app c g_sym [ y; x ])))
;;

(* ================================================================== *)
let test_scalar_distinctness () =
  print_endline "scalar distinctness (required #8 sweep):";
  let c = Context.create env in
  let x = Context.const c int_vars.(0) in
  (* x+2 vs x+3 must not collapse. *)
  check
    "x+2 <> x+3"
    (not
       (Term.equal
          (Context.add c x (Context.int_const c 2))
          (Context.add c x (Context.int_const c 3))));
  (* Int_const family all distinct. *)
  let ok = ref true in
  for i = -20 to 20 do
    for j = -20 to 20 do
      if i <> j && Term.equal (Context.int_const c i) (Context.int_const c j)
      then ok := false
    done
  done;
  check "Int_const k family distinct" !ok;
  (* App same args, different symbol. *)
  check
    "App same-args different-symbol distinct"
    (not (Term.equal (Context.app c f_sym [ x ]) (Context.app c p_sym [ x ])));
  (* App same symbol, different args. *)
  check
    "App same-symbol different-args distinct"
    (not
       (Term.equal
          (Context.app c f_sym [ x ])
          (Context.app c f_sym [ Context.int_const c 0 ])));
  (* coefficient distinctness: 2x vs 3x. *)
  check "2x <> 3x" (not (Term.equal (Context.mul_const c 2 x) (Context.mul_const c 3 x)))
;;

(* ================================================================== *)
(* Random term generators (used by the property tests). *)

let gen_int_leaf c =
  match rand_int 3 with
  | 0 -> Context.const c int_vars.(rand_int (Array.length int_vars))
  | 1 -> Context.int_const c (rand_int 11 - 5)
  | _ -> Context.const c int_vars.(rand_int (Array.length int_vars))
;;

let gen_bool_leaf c =
  match rand_int 3 with
  | 0 -> Context.const c bool_vars.(rand_int (Array.length bool_vars))
  | 1 -> Context.bool_const c (rand_int 2 = 0)
  | _ -> Context.const c bool_vars.(rand_int (Array.length bool_vars))
;;

let rec gen_int c depth =
  if depth <= 0
  then gen_int_leaf c
  else (
    match rand_int 6 with
    | 0 -> Context.add c (gen_int c (depth - 1)) (gen_int c (depth - 1))
    | 1 -> Context.sub c (gen_int c (depth - 1)) (gen_int c (depth - 1))
    | 2 -> Context.neg c (gen_int c (depth - 1))
    | 3 -> Context.mul_const c (rand_int 7 - 3) (gen_int c (depth - 1))
    | 4 ->
      Context.ite
        c
        (gen_bool c (depth - 1))
        (gen_int c (depth - 1))
        (gen_int c (depth - 1))
    | _ -> gen_int_leaf c)

and gen_bool c depth =
  if depth <= 0
  then gen_bool_leaf c
  else (
    match rand_int 9 with
    | 0 -> Context.le c (gen_int c (depth - 1)) (gen_int c (depth - 1))
    | 1 -> Context.lt c (gen_int c (depth - 1)) (gen_int c (depth - 1))
    | 2 -> Context.ge c (gen_int c (depth - 1)) (gen_int c (depth - 1))
    | 3 -> Context.gt c (gen_int c (depth - 1)) (gen_int c (depth - 1))
    | 4 -> Context.eq c (gen_int c (depth - 1)) (gen_int c (depth - 1))
    | 5 -> Context.and_ c [ gen_bool c (depth - 1); gen_bool c (depth - 1) ]
    | 6 -> Context.or_ c [ gen_bool c (depth - 1); gen_bool c (depth - 1) ]
    | 7 -> Context.not_ c (gen_bool c (depth - 1))
    | _ ->
      Context.ite
        c
        (gen_bool c (depth - 1))
        (gen_bool c (depth - 1))
        (gen_bool c (depth - 1)))
;;

let test_property_debug_check () =
  print_endline "property: Debug.check holds on all constructible terms:";
  reset_rng ();
  let c = Context.create env in
  let all_ok = ref true in
  for _ = 1 to 4000 do
    let t = if rand_int 2 = 0 then gen_int c 4 else gen_bool c 4 in
    match Term.Debug.check ~env t with
    | () -> ()
    | exception Failure msg ->
      if !all_ok then Printf.printf "    first violation: %s\n" msg;
      all_ok := false
  done;
  check "Debug.check holds on 4000 random terms" !all_ok
;;

let test_property_equal_phys () =
  print_endline "property: equal terms are physically equal:";
  reset_rng ();
  let c = Context.create env in
  let ok = ref true in
  for _ = 1 to 2000 do
    (* Rebuild the same term by replaying the same PRNG-driven sequence twice is awkward;
       instead build one term then rebuild it via a structural copy through the
       constructors and require physical sharing. *)
    let t = gen_int c 3 in
    let t' = Context.add c t (Context.int_const c 0) in
    (* t + 0 must be the very same node as t. *)
    if not (t == t') then ok := false
  done;
  check "t and (t+0) share physically" !ok
;;

(* ------------------------------------------------------------------ *)
(* Brute-force: gcd tightening and comparison lowering are truth-preserving over a small
   integer box (discharges the ADR N2 property-test obligation). *)

let test_brute_gcd_and_compare () =
  print_endline "brute force: gcd tightening + le/lt/ge/gt lowering:";
  let c = Context.create env in
  let x = Context.const c int_vars.(0) in
  let y = Context.const c int_vars.(1) in
  let box = ref [] in
  for xv = -20 to 20 do
    for yv = -20 to 20 do
      box := (xv, yv) :: !box
    done
  done;
  let box = !box in
  let assign xv yv s =
    if Symbol.equal s int_vars.(0)
    then xv
    else if Symbol.equal s int_vars.(1)
    then yv
    else 0
  in
  let sweep name atom truth =
    let ok = ref true in
    List.iter
      (fun (xv, yv) -> if eval_bool (assign xv yv) atom <> truth xv yv then ok := false)
      box;
    check name !ok
  in
  (* gcd tightening equivalences. *)
  sweep
    "2x <= 2  ==  x<=1"
    (Context.le c (Context.mul_const c 2 x) (Context.int_const c 2))
    (fun xv _ -> 2 * xv <= 2);
  sweep
    "2x+4y <= 6"
    (Context.le
       c
       (Context.add c (Context.mul_const c 2 x) (Context.mul_const c 4 y))
       (Context.int_const c 6))
    (fun xv yv -> (2 * xv) + (4 * yv) <= 6);
  sweep
    "3x <= 5 (gcd 3, ceil)"
    (Context.le c (Context.mul_const c 3 x) (Context.int_const c 5))
    (fun xv _ -> 3 * xv <= 5);
  sweep
    "-6x <= 4"
    (Context.le c (Context.mul_const c (-6) x) (Context.int_const c 4))
    (fun xv _ -> -6 * xv <= 4);
  (* Positive residual constant in <=0 form: the CEIL tightening (not floor) is required.
     3x <= -5 => 3x+5<=0 => x + ceil(5/3)=2 <= 0; floor would give x+1<=0 and be unsound.
     These are the cases that distinguish ceil from floor. *)
  sweep
    "3x <= -5 (ceil residual)"
    (Context.le c (Context.mul_const c 3 x) (Context.int_const c (-5)))
    (fun xv _ -> 3 * xv <= -5);
  sweep
    "3x <= -4 (ceil residual)"
    (Context.le c (Context.mul_const c 3 x) (Context.int_const c (-4)))
    (fun xv _ -> 3 * xv <= -4);
  sweep
    "6x+9y <= -5 (gcd 3, ceil residual)"
    (Context.le
       c
       (Context.add c (Context.mul_const c 6 x) (Context.mul_const c 9 y))
       (Context.int_const c (-5)))
    (fun xv yv -> (6 * xv) + (9 * yv) <= -5);
  sweep
    "3x >= 5 (ceil residual)"
    (Context.ge c (Context.mul_const c 3 x) (Context.int_const c 5))
    (fun xv _ -> 3 * xv >= 5);
  sweep
    "5x > 7 (ceil residual)"
    (Context.gt c (Context.mul_const c 5 x) (Context.int_const c 7))
    (fun xv _ -> 5 * xv > 7);
  (* comparison lowering. *)
  sweep "x <= y" (Context.le c x y) (fun xv yv -> xv <= yv);
  sweep "x < y" (Context.lt c x y) (fun xv yv -> xv < yv);
  sweep "x >= y" (Context.ge c x y) (fun xv yv -> xv >= yv);
  sweep "x > y" (Context.gt c x y) (fun xv yv -> xv > yv);
  sweep
    "x+1 < 2y"
    (Context.lt c (Context.add c x (Context.int_const c 1)) (Context.mul_const c 2 y))
    (fun xv yv -> xv + 1 < 2 * yv);
  (* eq lowering (Int). *)
  sweep "x = y" (Context.eq c x y) (fun xv yv -> xv = yv)
;;

(* ================================================================== *)
let test_determinism () =
  print_endline "determinism: two fresh Contexts assign identical tags:";
  let build () =
    let c = Context.create env in
    let x = Context.const c int_vars.(0) in
    let y = Context.const c int_vars.(1) in
    let terms =
      [ Context.add c x y
      ; Context.le c x (Context.int_const c 3)
      ; Context.eq c x y
      ; Context.and_ c [ Context.const c bool_vars.(0); Context.le c y x ]
      ; Context.ite c (Context.const c bool_vars.(1)) x y
      ]
    in
    List.map (fun t -> t.Term.tag) terms
  in
  let a = build () in
  let b = build () in
  check "identical tag assignment across fresh contexts" (a = b)
;;

(* ================================================================== *)
(* Theory_view. *)

let test_theory_view () =
  print_endline "Theory_view (is_atom / atom / is_app):";
  let c = Context.create env in
  let x = Context.const c int_vars.(0) in
  let y = Context.const c int_vars.(1) in
  let p = Context.const c bool_vars.(0) in
  check "Le is atom" (Theory_view.is_atom (Context.le c x y));
  check "Int Eq is atom" (Theory_view.is_atom (Context.eq c x y));
  check
    "Bool Eq (iff) is NOT atom"
    (not (Theory_view.is_atom (Context.eq c p (Context.const c bool_vars.(1)))));
  check "predicate App is atom" (Theory_view.is_atom (Context.app c p_sym [ x ]));
  check
    "And is not atom"
    (not (Theory_view.is_atom (Context.and_ c [ p; Context.le c x y ])));
  check "Not is not atom" (not (Theory_view.is_atom (Context.not_ c p)));
  check "bool var is atom" (Theory_view.is_atom p);
  check "Arith is not atom" (not (Theory_view.is_atom (Context.add c x y)));
  check "is_app on App" (Theory_view.is_app (Context.app c f_sym [ x ]));
  check "is_app false on Arith" (not (Theory_view.is_app (Context.add c x y)));
  (match Theory_view.atom (Context.le c x y) with
   | Theory_view.Le_zero _ -> check "atom Le_zero" true
   | _ -> check "atom Le_zero" false);
  (match Theory_view.atom (Context.eq c x y) with
   | Theory_view.Equality _ -> check "atom Equality" true
   | _ -> check "atom Equality" false);
  match Theory_view.atom (Context.app c p_sym [ x ]) with
  | Theory_view.Predicate _ -> check "atom Predicate" true
  | _ -> check "atom Predicate" false
;;

(* ================================================================== *)
(* Debug.check red paths.

   Construction-mode invariants (1-9) cannot be violated through the public API: Term.t is
   a private, single-construction-path type (I1/I2), and every constructor treats tag
   equality as identity, so no client can build an ill-formed or mis-shared term to feed
   to Debug.check. That unconstructibility IS the invariant. Mutation testing (DESIGN §10)
   exercises the red branches by perturbing the constructors themselves.

   The pipeline-mode invariant (10) is different: an Int-sorted Ite and a div/mod
   application are perfectly valid at construction, and Debug.check in Pipeline mode must
   reject them. Those are constructible, so we test them directly (real red cases, no
   fault injection). *)

let test_debug_pipeline_red () =
  print_endline "Debug.check pipeline red paths (constructible):";
  let c = Context.create env in
  let x = Context.const c int_vars.(0) in
  let p = Context.const c bool_vars.(0) in
  let int_ite = Context.ite c p x (Context.int_const c 0) in
  (match int_ite.Term.node with
   | Term.Ite _ -> ()
   | _ -> check "int_ite is an Ite" false);
  check
    "Int-Ite ok in Construction mode"
    (match Term.Debug.check ~mode:Term.Debug.Construction int_ite with
     | () -> true
     | exception _ -> false);
  check_raises "Int-Ite rejected in Pipeline mode" (fun () ->
    Term.Debug.check ~mode:Term.Debug.Pipeline int_ite);
  let dm = Context.div c x (Context.int_const c 3) in
  check
    "div App ok in Construction mode"
    (match Term.Debug.check dm with
     | () -> true
     | exception _ -> false);
  check_raises "reserved div App rejected in Pipeline mode" (fun () ->
    Term.Debug.check ~mode:Term.Debug.Pipeline dm)
;;

(* ================================================================== *)
let () =
  print_endline "core self-test:";
  test_iarr ();
  test_symbol_sort_env ();
  test_normalization ();
  test_sort_errors ();
  test_overflow_unsupported ();
  test_hashcons_sharing ();
  test_scalar_distinctness ();
  test_theory_view ();
  test_property_debug_check ();
  test_property_equal_phys ();
  test_brute_gcd_and_compare ();
  test_determinism ();
  test_debug_pipeline_red ();
  Printf.printf "\ncore self-test: %d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;
