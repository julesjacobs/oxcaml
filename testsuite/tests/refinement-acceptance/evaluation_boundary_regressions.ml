(* TEST
 expect;
*)

(* A generalized open evaluates its module expression before its body.  The
   initializer's local refinement obligation must therefore be checked. *)
let generalized_open_checks_initializer_obligations () =
  let open struct
    let _bad = (() : unit{ false })
  end in
  ()
[%%expect {|
Line 3, characters 15-35:
3 |     let _bad = (() : unit{ false })
                   ^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Both arms of the [if] can return.  The completing recursive let is not
   lowerable as a logical subject, so match-result summarization must fail
   closed rather than erase its branch and make the [true] arm contradictory. *)
let recursive_let_result_does_not_erase_if_branch flag : int{ _ >= 0 } =
  match
    if flag
    then
      let rec identity value = value in
      identity true
    else false
  with
  | false -> 0
  | true -> -1
[%%expect {|
Line 10, characters 12-14:
10 |   | true -> -1
                 ^^
Error: Refinement verification failed (disproved)
|}]
