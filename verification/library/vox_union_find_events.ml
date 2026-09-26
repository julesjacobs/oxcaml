type event = Initialize | Allocate | Find of {depth : Bigint.t | depth >= 0Z} | Link | Union
[@@inductive]

let[@def] (weight @ total) event =
  match event with
  | Initialize | Union -> 1Z
  | Allocate -> 3Z
  | Find depth -> Bigint.add (Bigint.mul 4Z depth) 2Z
  | Link -> 7Z

let[@def] rec (total @ total) events =
  match events with
  | [] -> 0Z
  | event :: rest -> Bigint.add (weight event) (total rest)
