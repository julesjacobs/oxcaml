(** Public edit semantics. Every observation has a complete checked equation.
    Counts use mathematical Bigint integers, not wrapping machine arithmetic.
    These structurally decreasing equations uniquely determine their values. *)

type operation : immutable_data mod total =
  | Keep of int | Delete of int | Insert of int
[@@inductive]
type script = operation list

val source : script -> int list @@ total
val source_def : (script : script) ->
  {u : unit | source script === (match script with
    | [] -> []
    | Keep x :: rest | Delete x :: rest -> x :: source rest
    | Insert _ :: rest -> source rest)} @@ total

val target : script -> int list @@ total
val target_def : (script : script) ->
  {u : unit | target script === (match script with
    | [] -> []
    | Keep x :: rest | Insert x :: rest -> x :: target rest
    | Delete _ :: rest -> target rest)} @@ total

val cost : script -> Bigint.t @@ total
val cost_def : (script : script) ->
  {u : unit | cost script === (match script with
    | [] -> 0Z
    | Keep _ :: rest -> cost rest
    | Delete _ :: rest | Insert _ :: rest -> Bigint.add 1Z (cost rest))}
  @@ total

val apply : int list -> script -> int list option @@ total
val apply_def : (old : int list) -> (script : script) ->
  {u : unit | apply old script === (match script with
    | [] -> (match old with [] -> Some [] | _ :: _ -> None)
    | Keep x :: rest ->
      (match old with
       | y :: ys when x = y ->
         (match apply ys rest with
          | None -> None | Some zs -> Some (x :: zs))
       | _ -> None)
    | Delete x :: rest ->
      (match old with
       | y :: ys when x = y -> apply ys rest | _ -> None)
    | Insert x :: rest ->
      (match apply old rest with
       | None -> None | Some zs -> Some (x :: zs)))} @@ total

val invert : script -> script @@ total
val invert_def : (script : script) ->
  {u : unit | invert script === (match script with
    | [] -> []
    | Keep x :: rest -> Keep x :: invert rest
    | Delete x :: rest -> Insert x :: invert rest
    | Insert x :: rest -> Delete x :: invert rest)} @@ total

val size : int list -> Bigint.t @@ total
val size_def : (values : int list) ->
  {u : unit | size values === (match values with
    | [] -> 0Z | _ :: rest -> Bigint.add 1Z (size rest))} @@ total

val script_size : script -> Bigint.t @@ total
val script_size_def : (script : script) ->
  {u : unit | script_size script === (match script with
    | [] -> 0Z | _ :: rest -> Bigint.add 1Z (script_size rest))} @@ total

(** Insertion/deletion distance; substitution costs two.
    The recurrence decreases the sum of the sequence lengths. *)
val minimum_cost : int list -> int list -> Bigint.t @@ total
val minimum_cost_equation : (old : int list) -> (fresh : int list) ->
  {u : unit | minimum_cost old fresh === (match old, fresh with
    | [], _ -> size fresh
    | _, [] -> size old
    | a :: ats, b :: bts ->
      if a = b then minimum_cost ats bts
      else Bigint.add 1Z
        (if minimum_cost ats fresh <= minimum_cost old bts
         then minimum_cost ats fresh else minimum_cost old bts))} @@ total
