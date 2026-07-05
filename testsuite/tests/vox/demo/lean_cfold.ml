(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/cfold.mli ../lib/cfold.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the sealed, verified constant folder.  [t] is opaque -- a
   bare [Int] to the client (the tree and [denote] never leave the
   unit).  Expressions are built with the smart constructors [lit]/[add]/
   [mul]; each carries its Int image ([lit 1] is [1], [add a b] is
   [a + b]).

   The transformation spec [fold : (e : t) -> t{ _ = e }] says the
   folded value has the SAME image as the original.  So the client gets
   [eval (fold e) = eval e] with NO new proof: [eval] reads the image,
   [fold] preserves it, and both facts are plain Int equalities. *)

open Cfold

(* eval (fold e) = eval e -- carried across the transformation.  The
   result [bool{ _ }] asserts the equality holds. *)
let preserved : bool{ _ } =
  let one = lit 1 in
  let two = lit 2 in
  let three = lit 3 in
  let prod = mul two three in
  let e = add one prod in
  let folded = fold e in
  let ve = eval e in
  let vf = eval folded in
  vf = ve

(* And the image is the concrete arithmetic the client can read off:
   [1 + 2 * 3 = 7], entirely in Int vocabulary. *)
let value : int{ _ = 7 } =
  let one = lit 1 in
  let two = lit 2 in
  let three = lit 3 in
  let prod = mul two three in
  let e = add one prod in
  let folded = fold e in
  eval folded
