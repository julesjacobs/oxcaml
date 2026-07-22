(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

let quotation_key = Sys.opaque_identity 0

external quotation_law : unit -> unit{ quotation_key = 7 } @@ total
  = "%identity"

let quotation_branch_must_join flag =
  let _code =
    if flag then <[ Stdlib.raise Exit ]>
    else (ignore (quotation_law ()); <[ 0 ]>)
  in
  (quotation_key : int{ _ = 7 })

[%%expect {|
val quotation_key : int = 0
external quotation_law : unit -> unit{ quotation_key = 7 } = "%identity"
Line 11, characters 2-32:
11 |   (quotation_key : int{ _ = 7 })
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let genuinely_nonreturning_branch flag =
  let _code =
    if flag then raise Exit
    else (ignore (quotation_law ()); <[ 0 ]>)
  in
  (quotation_key : int{ _ = 7 })

[%%expect {|
val genuinely_nonreturning_branch : bool -> int{ _ = 7 } = <fun>
|}]
