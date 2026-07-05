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
