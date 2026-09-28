(* A ghost token is a capability that exists only for checking.
   - It is unique: a function that takes it [@ unique] consumes it, so each
     token can be used at most once (it is affine).
   - It is ghost: [ghost_] erases its creation (the call to [authorize]), and
     [fire] receives a placeholder constant in its place.
   [once] uses its permit once. [twice] tries to use it again. *)

module Launch : sig
  type permit

  val authorize : unit -> permit @ unique total ghost @@ total

  (* Firing consumes the permit. *)
  val fire : permit @ unique ghost -> string -> unit
end = struct
  type permit = unit

  let (authorize @ total) () = ()

  let fire _ target = print_endline ("launching at " ^ target)
end

let once () =
  let permit = ghost_ (Launch.authorize ()) in
  Launch.fire permit "the moon"

let twice () =
  let permit = ghost_ (Launch.authorize ()) in
  Launch.fire permit "the moon";
  Launch.fire permit "Mars"
