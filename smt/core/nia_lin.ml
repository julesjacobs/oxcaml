(* Sound linear lemmas for the nonlinear-integer abstraction (dark OXSMT_NIA). Each
   product [a*b] is abstracted to an uninterpreted term [p = (.oxsmt.nia.mul a b)]
   ({!Nia_config}); this module emits the axioms that partially constrain [p] toward real
   multiplication.

   {b Every lemma is a valid implication of [p = a*b] over the integers}, so adding them
   can only make the abstraction MORE unsatisfiable — and since the abstraction's [unsat]
   already holds for every interpretation of the uninterpreted symbol (including real
   multiplication), the lemmas preserve soundness of [unsat]. They do NOT make the
   abstraction complete: an abstraction [sat] is still only a CANDIDATE, re-checked under
   real multiplication by {!Model_check}. The lemmas' job is to let the linear solver
   refute the common nonlinear [unsat] cores (sign/zero/unit reasoning) without any
   nonlinear search. *)

(* A product record: [p = a * b]. *)
type product =
  { p : Term.t
  ; a : Term.t
  ; b : Term.t
  }

(* Lemmas for one product [p = a*b]. All are integer-valid:
   - zero: a=0 -> p=0; b=0 -> p=0
   - sign: (a>=0 & b>=0) -> p>=0 ; (a<=0 & b<=0) -> p>=0 (a>=0 & b<=0) -> p<=0 ; (a<=0 &
     b>=0) -> p<=0
   - unit: a=1 -> p=b ; a=-1 -> p=-b ; b=1 -> p=a ; b=-1 -> p=-a *)
let lemmas_for ctx { p; a; b } =
  let zero = Context.int_const ctx 0 in
  let one = Context.int_const ctx 1 in
  let mone = Context.int_const ctx (-1) in
  let eq = Context.eq ctx in
  let ge x = Context.ge ctx x zero in
  let le x = Context.le ctx x zero in
  let imp = Context.implies ctx in
  let and2 x y = Context.and_ ctx [ x; y ] in
  let neg = Context.neg ctx in
  [ (* zero *)
    imp (eq a zero) (eq p zero)
  ; imp (eq b zero) (eq p zero)
  ; (* sign *)
    imp (and2 (ge a) (ge b)) (ge p)
  ; imp (and2 (le a) (le b)) (ge p)
  ; imp (and2 (ge a) (le b)) (le p)
  ; imp (and2 (le a) (ge b)) (le p)
  ; (* unit *)
    imp (eq a one) (eq p b)
  ; imp (eq a mone) (eq p (neg b))
  ; imp (eq b one) (eq p a)
  ; imp (eq b mone) (eq p (neg a))
  ]
;;

(* All lemmas for a set of products, de-duplicated by the emitting caller's registry (each
   distinct product term appears once). *)
let lemmas ctx products = List.concat_map (lemmas_for ctx) products
