(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox models OCaml [int] as UNBOUNDED Lean [Int]; machine wraparound is
   OUTSIDE the model.  This is a deliberate, documented TCB assumption --
   the "ideal arithmetic" stance (typing/vox_reflect.ml:35, and the trust
   ledger in the stdlib design doc).  This test PINS that acceptance so it
   is test-visible, not merely doc-visible: [succ_gt] is proved strictly
   increasing, which is TRUE in the ideal model but FALSE at [max_int]
   (where [x + 1] wraps to [min_int]).  If a future bounded / bitvector
   model makes this stop verifying, this test must be REVISITED with intent,
   not blindly promoted. *)

let succ_gt : (x : int) -> int{ _ > x } = fun x -> x + 1
[%%expect{|
val succ_gt : (x : int) -> int{ _ > x } = <fun>
|}]

(* Its concrete stdlib consequence: [abs] proved nonnegative is false at
   [min_int] (abs min_int = min_int < 0) -- same ideal-arithmetic license. *)
let abs_nonneg : (x : int) -> int{ _ >= 0 } = fun x -> if x < 0 then -x else x
[%%expect{|
val abs_nonneg : int -> int{ _ >= 0 } = <fun>
|}]
