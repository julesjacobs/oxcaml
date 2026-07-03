(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: the [@@vox.total] ATTRIBUTE spelling of [total_] (the .cmi
   transport for cross-module recognition, also accepted in source).
   The reflected call names itself in the dumped VC, proving the
   binding was registered. *)

let rec double n = if n <= 0 then 0 else 2 + double (n - 1)
[@@vox.total] [@@vox.decreases n]

let d : (n : int) -> int{ _ = double n } =
  fun n -> double n
[%%expect{|
val double : int -> int = <fun>
Line 5, characters 11-19: vox VC:
  goal: (double n) = (double n)
  hypotheses: <none>
val d : (n : int) -> int{ _ = (double n) } = <fun>
|}]
