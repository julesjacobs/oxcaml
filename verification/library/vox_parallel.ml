(* Trusted: the only unchecked step is [Obj.magic_unique] on the left result.
   [Domain.join] returns its result aliased; here the domain handle never
   escapes, is joined once, and the domain has exited, so the result produced
   unique by [left] has no other reference. *)
let fork_join left right =
  let domain = (Domain.Safe.spawn [@alert "-do_not_spawn_domains"]) left in
  let right_result =
    try right () with exn ->
      let trace = Printexc.get_raw_backtrace () in
      (try ignore (Domain.join domain) with _ -> ());
      Printexc.raise_with_backtrace exn trace in
  let left_result = Obj.magic_unique (Domain.join domain) in
  left_result, right_result
