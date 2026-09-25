module S = Vox_sequence

type byte : immediate = {value : int | 0 <= value && value <= 255}

type sequence =
  | Last of byte list
  | Match of byte list * int * int * sequence
[@@inductive]

let (source_position @ total) :
    (written : {n : int | 0 <= n && n <= 4194304}) ->
    (distance : {d : int | 1 <= d && d <= written}) ->
    (copied : {j : int | 0 <= j && j <= 4194304 - written}) ->
    {source : int | source = written + copied - distance
      && 0 <= source && source < written + copied} =
  fun written distance copied ->
    let source = written + copied - distance in
    refine_ source

(* Read each source byte from the output produced so far. In particular,
   distance one repeats the most recently written byte. *)
let[@def] rec (copy @ total) (output : byte list) (distance : int)
    (remaining : int) =
  if remaining <= 0 then Some output
  else if distance <= 0 || Bigint.of_int distance > S.length output then None
  else
    match S.at output
            (Bigint.sub (S.length output) (Bigint.of_int distance)) with
    | None -> None
    | Some byte -> copy (S.append output [byte]) distance (remaining - 1)
[@@decreases remaining]

(* A compressor may emit a match only after validating each source byte
   against the progressively extended output. *)
let[@def] rec (consume_match @ total) (output : byte list)
    (input : byte list) (distance : int) (remaining : int) =
  if remaining <= 0 then Some (output, input)
  else if distance <= 0 || Bigint.of_int distance > S.length output then None
  else match input with
    | [] -> None
    | head :: tail ->
      (match S.at output
               (Bigint.sub (S.length output) (Bigint.of_int distance)) with
       | Some byte when byte = head ->
         consume_match (S.append output [head]) tail distance (remaining - 1)
       | _ -> None)
[@@decreases remaining]

let rec (consume_match_sound @ total) :
    (output : byte list) -> (input : byte list) ->
    (distance : int) -> (remaining : int) ->
    {u : unit | match consume_match output input distance remaining with
      | None -> true
      | Some (after, _) -> copy output distance remaining === Some after}
      @ ghost =
  fun output input distance remaining -> ghost_ (
    consume_match_def output input distance remaining;
    copy_def output distance remaining;
    if remaining > 0 && distance > 0
       && Bigint.of_int distance <= S.length output then
      match input with
      | [] -> ()
      | head :: tail ->
        (match S.at output
                 (Bigint.sub (S.length output) (Bigint.of_int distance)) with
         | Some byte when byte = head ->
           consume_match_sound (S.append output [head]) tail distance
             (remaining - 1)
         | _ -> ())
    else ())
[@@decreases remaining]

let[@def] rec (decode @ total) (output : byte list)
    (sequence : sequence) =
  match sequence with
  | Last literals -> Some (S.append output literals)
  | Match (literals, distance, length, rest) ->
    (match copy (S.append output literals) distance length with
     | None -> None
     | Some output -> decode output rest)
