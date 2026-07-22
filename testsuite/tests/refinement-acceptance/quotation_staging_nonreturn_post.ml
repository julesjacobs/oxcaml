#syntax quotations on

external stop : exn -> <[ int ]> expr{ false } = "%raise"

(* The verifier still checks explicitly written source annotations after a
   nonreturning let RHS.  The splice's impossible result must not leak [false]
   and make this independent annotation pass. *)
let nonreturning_splice_does_not_pollute_following_source () =
  let _code =
    <[
      $(stop Exit)
    ]>
  in
  ignore (0 : int{ false })
