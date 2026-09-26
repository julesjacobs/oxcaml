type event = Initialize | Allocate | Find of {depth : Bigint.t | depth >= 0Z} | Link | Union
[@@inductive]
val weight : event -> Bigint.t @@ total
val weight_def : (event : event) ->
  {u : unit | weight event === (match event with
    | Initialize | Union -> 1Z
    | Allocate -> 3Z
    | Find depth -> Bigint.add (Bigint.mul 4Z depth) 2Z
    | Link -> 7Z)} @@ total
val total : event list -> Bigint.t @@ total
val total_def : (events : event list) ->
  {u : unit | total events === (match events with [] -> 0Z
    | event :: rest -> Bigint.add (weight event) (total rest))} @@ total
