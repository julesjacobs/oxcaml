(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* A WRONG constant folder is rejected.  The transformation spec
   [_ = e] demands equal denotation; a fold that collapses every [Add]
   to the literal [0] does NOT preserve it, so the [Add] arm's VC
   [denote (Lit 0) = e] -- i.e. [0 = denote a + denote b] -- is refuted
   with a concrete Int counterexample.  [lint] names Lean's built-in
   [Int]. *)

type expr = Lit of int | Add of expr * expr | Mul of expr * expr
type lint [@@vox.sort lean "Int"]

[%%vox.lean {lean|
@[grind] def denote : Vox_expr -> Int
  | .Lit n => n
  | .Add a b => denote a + denote b
  | .Mul a b => denote a * denote b
|lean}]

type t = expr{ denote _ = denote _ } [@vox.via (denote : lint)]
[%%expect{|
type expr = Lit of int | Add of expr * expr | Mul of expr * expr
type lint
type t = expr{ (denote _) = (denote _) via (denote : lint) }
|}]

let bad_fold : (e : t) -> t{ _ = e } =
  fun e ->
    let refine_ e0 = e in
    (match e0 with
     | Lit n -> (Lit n : t{ _ = e })
     | Add (_, _) -> (Lit 0 : t{ _ = e })
     | Mul (a, b) -> (Mul (a, b) : t{ _ = e }))
[%%expect{|
Line 6, characters 22-27:
6 |      | Add (_, _) -> (Lit 0 : t{ _ = e })
                          ^^^^^
Error: vox: verification failed (lean).
       Goal: ((denote (Lit 0)) = (denote (Lit 0))) && ((denote (Lit 0)) = e)
Hypotheses:
  e0 = (Add (*vox-wild*#2, *vox-wild*))
  not (e0 is Lit)
  (denote e0) = (denote e0)
  (denote e0) = e
  e = e
Possible counterexample:
  e = 1
  denote e0 = 1
  denote (Vox_expr.Lit 0) = 0
  denote *vox-wild* = 1
  denote *vox-wild*#2 = 0
  denote (*vox-wild*#2.Add *vox-wild*) = 1
  denote w = 0
  denote w_1 = 1
  denote (w.Add w_1) = 1
(lean: error: `grind` failed)
|}]

(* A WRONG peephole rule is likewise refuted.  Strength/annihilation
   [x * 0 -> Lit 1] is unsound: at a subterm [Mul a b] with [b = Lit 0]
   the VC [denote (Lit 1) = denote (Mul a b)] -- i.e. [1 = denote a * 0]
   -- fails with a concrete counterexample. *)
let bad_simp : (e : t) -> t{ _ = e } =
  fun e ->
    let refine_ e0 = e in
    (match e0 with
     | Lit n -> (Lit n : t{ _ = e })
     | Add (a, b) -> (Add (a, b) : t{ _ = e })
     | Mul (a, b) ->
       (match b with
        | Lit y ->
          if y = 0 then (Lit 1 : t{ _ = e }) else (Mul (a, b) : t{ _ = e })
        | _ -> (Mul (a, b) : t{ _ = e })))
[%%expect{|
Line 10, characters 25-30:
10 |           if y = 0 then (Lit 1 : t{ _ = e }) else (Mul (a, b) : t{ _ = e })
                              ^^^^^
Error: vox: verification failed (lean).
       Goal: ((denote (Lit 1)) = (denote (Lit 1))) && ((denote (Lit 1)) = e)
Hypotheses:
  y = 0
  b = (Lit y)
  e0 = (Mul (a, b))
  not (e0 is Lit)
  not (e0 is Add)
  (denote e0) = (denote e0)
  (denote e0) = e
  e = e
Possible counterexample:
  e = 0
  y = 0
  denote e0 = 0
  denote (Vox_expr.Lit 1) = 1
  denote b = 0
  denote a = 0
  denote (a.Mul b) = 0
  denote w = 0
  denote w_1 = 0
  denote (Vox_expr.Lit y) = 0
  denote (w.Mul w_1) = 0
(lean: error: `grind` failed)
|}]
