(* TEST
 expect;
*)

let rec (nonterminating_proof @ total) (x : int @ logical)
    : unit{ x = x } =
  if x = 0 then () else nonterminating_proof x
;;

[%%expect {|
Line 3, characters 24-44:
3 |   if x = 0 then () else nonterminating_proof x
                            ^^^^^^^^^^^^^^^^^^^^
Error: The value "nonterminating_proof" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 39-46
         which is expected to be "total".
|}]
