(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* A reflected call may take CONSTRUCTOR arguments (the accumulator
   idiom): the definition-body fragment admits its own constructor
   terms as arguments of saturated reflected calls. *)
type ilist =
  | INil
  | ICons of int * ilist

let rec total_ rev_append (vs : ilist) (ws : ilist) : ilist =
  match vs with
  | INil -> ws
  | ICons (v, vs') -> rev_append vs' (ICons (v, ws))
[%%expect{|
type ilist = INil | ICons of int * ilist
val rev_append : ilist -> ilist -> ilist = <fun>
|}]
