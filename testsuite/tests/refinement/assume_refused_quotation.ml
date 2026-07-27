(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

(* A refinement is imposed inside a quotation, and the admission is refused
   all the same.  The obligation belongs to the stage the quoted code will
   run at, and an admission written at this stage is not a claim anyone can
   attach to that one.  The rest of the refusals, which need no extension,
   are in assume_refused.ml. *)

#syntax quotations on

let quoted (y : int) = <[ (assume y : int{ _ > 0 }) ]>
[%%expect {|
Line 1, characters 26-51:
1 | let quoted (y : int) = <[ (assume y : int{ _ > 0 }) ]>
                              ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This obligation cannot be admitted:
       a quoted or spliced admission belongs to another stage
|}]
