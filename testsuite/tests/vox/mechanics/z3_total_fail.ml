(* TEST
 flags = "-vox-solver z3";
 expect;
*)

(* vox: reflected (total_) definitions are emitted only by the Lean
   backend; under z3 they must be rejected with a real error rather
   than failing later as an undeclared solver symbol.  Needs no z3:
   the rejection happens before any solver runs. *)

let rec total_ len (n : int) = if n <= 0 then 0 else 1 + len (n - 1)
[@@vox.decreases n]
[%%expect{|
Lines 1-2, characters 0-19:
1 | let rec total_ len (n : int) = if n <= 0 then 0 else 1 + len (n - 1)
2 | [@@vox.decreases n]
Error: vox: reflected (total_) functions require "-vox-solver lean"; the z3 backend does not emit their definitions
|}]
