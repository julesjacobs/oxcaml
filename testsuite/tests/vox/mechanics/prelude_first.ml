(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Regression: a toplevel session whose FIRST phrase is prelude-only.
   [uses_vox] must count the block as vox content: it used to be
   invisible (no vox expressions, patterns, or bindings), so the
   still-inactive session skipped the phrase and the block was
   silently dropped from every later phrase's solver input -- [f]
   below then elaborated as an unbound identifier and the (trivially
   true) obligation failed for the wrong reason. *)

[%%vox.prelude.lean {lean|
opaque f : Int -> Int
|lean}]
[%%expect{|
|}]

(* Provable only if the block survived to this phrase's input. *)
let ok : {v:int | f v = f v} = refine_ 1
[%%expect{|
val ok : int{ (f _) = (f _) } = 1
|}]
