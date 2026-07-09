(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: a deep constructor pattern names its matched value as a SINGLE
   nested constructor term ([Node (Red, Node (Black, a, x, b), y, c)]),
   not a chain of fresh unknowns each tied back by an equation.  The
   single term lets the solver reduce a reflected model call and prove
   the reconstruction equals the scrutinee. *)

type color = Red | Black
type tree = Leaf | Node of color * tree * int * tree
[%%expect{|
type color = Red | Black
type tree = Leaf | Node of color * tree * int * tree
|}]

(* Reconstructing the matched deep pattern proves [_ = t]: the arm knows
   [t = Node (Red, Node (Black, a, x, b), y, c)] as one term. *)
let reassoc (t : tree) : tree{ _ = t } =
  match t with
  | Node (Red, Node (Black, a, x, b), y, c) ->
    Node (Red, Node (Black, a, x, b), y, c)
  | other -> other
[%%expect{|
val reassoc : (t : tree) -> tree{ _ = t } = <fun>
|}]

(* SOUNDNESS: rebuilding a DIFFERENT deep tree does not prove [_ = t] --
   the nested term is exact, so a changed colour is caught. *)
let bad (t : tree) : tree{ _ = t } =
  match t with
  | Node (Red, Node (Black, a, x, b), y, c) ->
    Node (Red, Node (Red, a, x, b), y, c)
  | other -> other
[%%expect{|
Line 4, characters 4-41:
4 |     Node (Red, Node (Red, a, x, b), y, c)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: Node (Red, Node (Red, a, x, b), y, c) = t
Hypotheses:
  t = Node (Red, Node (Black, a, x, b), y, c)
(lean: error: `grind` failed)
|}]
