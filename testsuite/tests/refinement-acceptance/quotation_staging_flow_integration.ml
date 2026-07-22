(* Combined quotation/application control: conservative quotation completion
   must not suppress the enclosing application's domain check, even when a
   current-stage splice is known not to return. *)

#syntax quotations on

let needs_false (_ : <[ int ]> expr{ false }) = ()
external stop : exn -> <[ int ]> expr{ false } = "%raise"

let nonreturning_splice_does_not_suppress_outer_application_domain () =
  needs_false
    <[
      $(stop Exit)
    ]>
