(* Implementation of cfold.mli.  [t] is [expr{ denote _ = denote _ }
   via (denote : lint)]: an expression tree whose via image is its
   integer denotation.  The .mli exposes only the Int image; here the
   tree and [denote] are visible, and every val is PROVED against the
   Int-vocabulary contracts with zero [assume_unchecked_].

   Image-binder idiom (as in lib/via_set): a [t] binder denotes the Int;
   the tree is reached by a [refine_] unpack that supplies the base
   [expr] and the link [denote e0 = e].  A via value is produced ONLY
   as an inline skeleton constructor -- coercing an opaque skeleton
   variable into [t] would bind its subject at the image sort and
   mis-apply [denote] (see findings).  The skeleton predicate is the
   reflexive [denote _ = denote _]: a via type over a BARE skeleton
   emits [true && <image Prop>] (Bool conjoined with Prop) which is ill
   typed, so a Prop invariant matching the image kind is required. *)

type expr = Lit of int | Add of expr * expr | Mul of expr * expr
type lint [@@vox.sort lean "Int"]

[%%vox.lean {lean|
@[grind] def denote : Vox_Cfold_expr -> Int
  | .Lit n => n
  | .Add a b => denote a + denote b
  | .Mul a b => denote a * denote b

-- Peephole rule table: each simplification's DENOTATION-preservation as
-- a named lemma (over the Int denotations).  [simp] below applies these
-- arm by arm; @[grind] feeds them to the per-node obligation.
theorem pr_add_zero (a : Int) : a + 0 = a := by omega
theorem pr_zero_add (a : Int) : 0 + a = a := by omega
theorem pr_mul_one (a : Int) : a * 1 = a := by omega
theorem pr_one_mul (a : Int) : 1 * a = a := by omega
theorem pr_mul_zero (a : Int) : a * 0 = 0 := by omega
theorem pr_zero_mul (a : Int) : 0 * a = 0 := by omega
theorem pr_mul_two (a : Int) : a * 2 = a + a := by omega
|lean}]

type t = expr{ denote _ = denote _ } [@vox.via (denote : lint)]

let lit : (n : int) -> t{ _ = n } =
  fun n -> (Lit n : t{ _ = n })

let add : (a : t) -> (b : t) -> t{ _ = a + b } =
  fun a b ->
    let refine_ a0 = a in
    let refine_ b0 = b in
    (Add (a0, b0) : t{ _ = a + b })

let mul : (a : t) -> (b : t) -> t{ _ = a * b } =
  fun a b ->
    let refine_ a0 = a in
    let refine_ b0 = b in
    (Mul (a0, b0) : t{ _ = a * b })

(* Fold constants bottom-up.  [go] recurses on the plain skeleton, its
   refinement carrying [denote]-preservation; [fold] unpacks the image
   binder and re-enters the folded tree through an inline-constructor
   match. *)
let rec go : (u : expr) -> expr{ denote _ = denote u } =
  fun u ->
    match u with
    | Lit n -> Lit n
    | Add (a, b) ->
      let ra = go a in
      let rb = go b in
      (match ra, rb with
       | Lit x, Lit y -> Lit (x + y)
       | _ -> Add (ra, rb))
    | Mul (a, b) ->
      let ra = go a in
      let rb = go b in
      (match ra, rb with
       | Lit x, Lit y -> Lit (x * y)
       | _ -> Mul (ra, rb))

let fold : (e : t) -> t{ _ = e } =
  fun e ->
    let refine_ e0 = e in
    let r = go e0 in
    (match r with
     | Lit n -> (Lit n : t{ _ = e })
     | Add (a, b) -> (Add (a, b) : t{ _ = e })
     | Mul (a, b) -> (Mul (a, b) : t{ _ = e }))

(* A first-order peephole simplifier: ONE bottom-up pass applying the
   rule table (constant folding, additive/multiplicative identities,
   annihilation, and a strength reduction x*2 -> x+x).  Each arm's
   [denote]-preservation is a rule lemma above; the recursion carries
   [denote _ = denote u] just like [go]. *)
let rec simp : (u : expr) -> expr{ denote _ = denote u } =
  fun u ->
    match u with
    | Lit n -> Lit n
    | Add (a, b) ->
      let sa = simp a in
      let sb = simp b in
      (* [Lit k] patterns do not propagate [= k] to the VC, so the
         literal is bound and tested with [if] (which does). *)
      (match sa, sb with
       | Lit x, Lit y -> Lit (x + y)               (* constant fold *)
       | _, Lit y -> if y = 0 then sa else Add (sa, sb)  (* x + 0 -> x *)
       | Lit x, _ -> if x = 0 then sb else Add (sa, sb)  (* 0 + x -> x *)
       | _ -> Add (sa, sb))
    | Mul (a, b) ->
      let sa = simp a in
      let sb = simp b in
      (match sa, sb with
       | Lit x, Lit y -> Lit (x * y)               (* constant fold *)
       | _, Lit y ->
         if y = 0 then Lit 0                        (* x * 0 -> 0 *)
         else if y = 1 then sa                      (* x * 1 -> x *)
         else if y = 2 then Add (sa, sa)            (* x * 2 -> x + x *)
         else Mul (sa, sb)
       | Lit x, _ ->
         if x = 0 then Lit 0                        (* 0 * x -> 0 *)
         else if x = 1 then sb                      (* 1 * x -> x *)
         else Mul (sa, sb)
       | _ -> Mul (sa, sb))

let simplify : (e : t) -> t{ _ = e } =
  fun e ->
    let refine_ e0 = e in
    let r = simp e0 in
    (match r with
     | Lit n -> (Lit n : t{ _ = e })
     | Add (a, b) -> (Add (a, b) : t{ _ = e })
     | Mul (a, b) -> (Mul (a, b) : t{ _ = e }))

let eval : (e : t) -> int{ _ = e } =
  fun e ->
    let refine_ e0 = e in
    let rec ev : (u : expr) -> int{ _ = denote u } =
      fun u ->
        match u with
        | Lit n -> n
        | Add (a, b) -> let va = ev a in let vb = ev b in va + vb
        | Mul (a, b) -> let va = ev a in let vb = ev b in va * vb
    in
    ev e0
