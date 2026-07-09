(* The tasks are local once-closures (they capture loans), but the
   join happens before this frame returns, so lending them to another
   domain is region-sound; this identity cast is the trusted step
   that says so. *)
external unsafe_globalize_task :
  (unit -> 'a @ unique) @ once local -> (unit -> 'a) = "%identity"

let fork_join2 :
  (unit -> 'a @ unique) @ once local ->
  (unit -> 'b @ unique) @ once local ->
  ('a * 'b) @ unique =
  fun f g ->
    let f = unsafe_globalize_task f in
    let g = unsafe_globalize_task g in
    (* fork for real where the runtime has domains; degrade to
       sequential where it does not (runtime4) *)
    match Domain.spawn f with
    | d ->
      let b = g () in
      let a = Domain.join d in
      Obj.magic_unique (a, b)
    | exception Failure _ ->
      let a = f () in
      let b = g () in
      Obj.magic_unique (a, b)
