[@@@alert "-do_not_spawn_domains"]

(* This is a narrow trusted operational boundary for scoped task transfer.
   The current type system cannot express that a local-once task and its
   exclusively transferred captures are joined before their region ends.
   [%obj_dup] moves the closure block to the heap; callers of this library must
   ensure the child's mutable captures are globally allocated and disjoint
   from the sibling's captures.  [fork_join2] joins on every path.  The
   conversion is private and no task or join handle is exported. *)
external trusted_transfer_task :
  local_ (unit -> 'a @ unique) @ once ->
  (unit -> 'a) @ portable
  = "%obj_dup"

let multidomain_capable =
  1 < Domain.recommended_domain_count ()

let child_limit =
  if multidomain_capable
  then Domain.recommended_domain_count () - 1
  else 0

let available_children = Atomic.make child_limit
let active_reservations = Atomic.make 0
let peak_reservations = Atomic.make 0
let spawned_children = Atomic.make 0
let transferred_tasks = Atomic.make 0

let rec update_peak candidate =
  let previous = Atomic.get peak_reservations in
  if previous < candidate
  then
    if not (Atomic.compare_and_set peak_reservations previous candidate)
    then update_peak candidate

(* [run_with_child_slot] installs its cleanup before calling this function.
   The successful CAS and [reserved := true] are adjacent non-allocating
   operations, so ordinary exceptions cannot observe an owned-but-untracked
   token.  OxCaml's [Sys.with_async_exns] deliberately unwinds past ordinary
   exception handlers, including [Fun.protect]; [fork_join2] therefore does
   not promise cleanup when called inside that explicit runtime boundary. *)
let rec reserve_child reserved =
  let available = Atomic.get available_children in
  if available = 0
  then false
  else if Atomic.compare_and_set available_children available (available - 1)
  then begin
    reserved := true;
    let active = Atomic.fetch_and_add active_reservations 1 + 1 in
    update_peak active;
    true
  end
  else reserve_child reserved

let release_child () =
  Atomic.decr active_reservations;
  Atomic.incr available_children

let run_sequential first second =
  match first () with
  | first_result ->
    let second_result = second () in
    Obj.magic_unique (first_result, second_result)
  | exception first_exception ->
    let first_backtrace = Printexc.get_raw_backtrace () in
    (match second () with
     | _ ->
       Printexc.raise_with_backtrace first_exception first_backtrace
     | exception _ ->
       Printexc.raise_with_backtrace first_exception first_backtrace)

let fork_join2_sequential_for_test first second =
  run_sequential first second

let join_with_cleanup :
  'a Domain.t ->
  local_ (unit -> 'b @ unique) @ once ->
  ('a * 'b) @ unique =
  fun domain second ->
  let joined = ref false in
  let join () =
    match Domain.join domain with
    | result ->
      joined := true;
      result
    | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      joined := true;
      Printexc.raise_with_backtrace exn backtrace
  in
  Obj.magic_unique
    (Fun.protect
       ~finally:(fun () ->
         if not !joined
         then
           match Domain.join domain with
           | _ -> ()
           | exception _ -> ())
       (local_ (fun () ->
         match second () with
         | second_result ->
           let first_result = join () in
           first_result, second_result
         | exception second_exception ->
           let second_backtrace = Printexc.get_raw_backtrace () in
           (match join () with
            | _ ->
              Printexc.raise_with_backtrace
                second_exception second_backtrace
            | exception first_exception ->
              let first_backtrace = Printexc.get_raw_backtrace () in
              Printexc.raise_with_backtrace
                first_exception first_backtrace)))
     [@nontail])

let run_with_child_slot :
  local_ (unit -> 'a @ unique) @ once ->
  local_ (unit -> 'b @ unique) @ once ->
  ('a * 'b) @ unique =
  fun first second ->
  let reserved = ref false in
  Obj.magic_unique
    (Fun.protect
       ~finally:(fun () -> if !reserved then release_child ())
       (local_ (fun () ->
         if not (reserve_child reserved)
         then run_sequential first second
         else begin
           Atomic.incr transferred_tasks;
           let first = trusted_transfer_task first in
           match Domain.Safe.spawn first with
           | domain ->
             Atomic.incr spawned_children;
             join_with_cleanup domain second
           | exception Failure _ ->
             run_sequential first second
           | exception spawn_exception ->
             let spawn_backtrace = Printexc.get_raw_backtrace () in
             Printexc.raise_with_backtrace spawn_exception spawn_backtrace
         end))
     [@nontail])

let fork_join2 :
  local_ (unit -> 'a @ unique) @ once ->
  local_ (unit -> 'b @ unique) @ once ->
  ('a * 'b) @ unique =
  fun first second ->
  if not multidomain_capable
  then run_sequential first second
  else run_with_child_slot first second

let multidomain_capable_for_test () = multidomain_capable
let child_limit_for_test () = child_limit
let spawned_children_for_test () = Atomic.get spawned_children
let peak_reserved_children_for_test () = Atomic.get peak_reservations
let transferred_tasks_for_test () = Atomic.get transferred_tasks

let force_available_children_for_test count =
  if count < 0
  then invalid_arg "Fork_join.force_available_children_for_test";
  if Atomic.get active_reservations <> 0
  then invalid_arg "Fork_join.force_available_children_for_test: active call";
  Atomic.set available_children count

let reset_counters_for_test () =
  if Atomic.get active_reservations <> 0
  then invalid_arg "Fork_join.reset_counters_for_test: active call";
  Atomic.set available_children child_limit;
  Atomic.set spawned_children 0;
  Atomic.set peak_reservations 0;
  Atomic.set transferred_tasks 0
