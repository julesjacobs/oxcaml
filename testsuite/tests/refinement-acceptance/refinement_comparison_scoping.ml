(* TEST
 expect;
*)

(* Comparison primitives are admitted as total only while elaborating a
   refinement predicate.  They remain partial in ordinary program code. *)

let expects_total (f @ total) = f
[%%expect {|
val expects_total : 'a @ total -> 'a = <fun>
|}]

(* @acc id=refinement_comparison_scoping final=REJECT today=REJECT stable=yes
   Outside a predicate, [(>)] must still make a closure partial. *)
let () =
  let f = fun (x : int) -> x > 0 in
  ignore (expects_total f)
[%%expect {|
Line 3, characters 24-25:
3 |   ignore (expects_total f)
                            ^
Error: This value is "partial"
         because it closes over the value "(>)" at line 2, characters 29-30
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

(* A structurally recursive function is total and can therefore occur in a
   refinement predicate.  The reflexive predicate keeps this test focused on
   predicate totality rather than recursive equation emission. *)
let rec structural_length @ total = function
  | [] -> 0
  | _ :: tail -> 1 + structural_length tail

type structural_predicate =
  int{
    structural_length ([] : int list)
    = structural_length ([] : int list)
  }

[%%expect {|
val structural_length : 'a list -> int = <fun>
type structural_predicate =
    int{ structural_length [] = structural_length [] }
|}]
