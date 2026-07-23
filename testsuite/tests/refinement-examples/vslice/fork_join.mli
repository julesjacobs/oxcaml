val fork_join2 :
  local_ (unit -> 'a @ unique) @ once ->
  local_ (unit -> 'b @ unique) @ once ->
  ('a * 'b) @ unique

val fork_join2_sequential_for_test :
  local_ (unit -> 'a @ unique) @ once ->
  local_ (unit -> 'b @ unique) @ once ->
  ('a * 'b) @ unique

val multidomain_capable_for_test : unit -> bool
val child_limit_for_test : unit -> int
val spawned_children_for_test : unit -> int
val peak_reserved_children_for_test : unit -> int
val transferred_tasks_for_test : unit -> int
val force_available_children_for_test : int -> unit
val reset_counters_for_test : unit -> unit
