module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control

type kind = Block | Loop | Then | Else [@@inductive]
type scopes = Empty | Scope of kind * scopes [@@inductive]
let[@def] (ordinary @ total) (instruction : I.t @ immutable) =
  match instruction with I.Block | I.Loop | I.If | I.Plain I.Else | I.Plain I.End -> false | _ -> true
let[@def] rec (structured @ total) (code : T.code @ immutable) =
  match code with
  | T.Empty -> true
  | T.Instruction (instruction, rest) -> ordinary instruction && structured rest
  | T.Block (body, rest) | T.Loop (body, rest) -> structured body && structured rest
  | T.If (yes, no, rest) -> structured yes && structured no && structured rest
let[@def] rec (scan @ total) (code : C.t @ immutable) (scopes : scopes @ immutable) : scopes option @ immutable =
  match code with
  | C.Empty -> Some scopes
  | C.Next (I.Block, rest) -> scan rest (Scope (Block, scopes))
  | C.Next (I.Loop, rest) -> scan rest (Scope (Loop, scopes))
  | C.Next (I.If, rest) -> scan rest (Scope (Then, scopes))
  | C.Next (I.Plain I.Else, rest) -> (match scopes with
    | Scope (Then, outer) -> scan rest (Scope (Else, outer)) | _ -> None)
  | C.Next (I.Plain I.End, rest) -> (match scopes with
    | Scope (_, outer) -> scan rest outer | _ -> None)
  | C.Next (_, rest) -> scan rest scopes
let rec (flatten_correct @ total) : (code : T.code) @ immutable -> (tail : C.t) @ immutable ->
    (scopes : scopes) @ immutable -> {u : unit | structured code} ->
    {u : unit | scan (T.flatten code tail) scopes === scan tail scopes} @ ghost =
  fun code tail scopes premise -> ghost_ (
    structured_def code; T.flatten_def code tail;
    match code with
    | T.Empty -> ()
    | T.Instruction (instruction, rest) ->
      ordinary_def instruction; scan_def (T.flatten code tail) scopes;
      flatten_correct rest tail scopes ()
    | T.Block (body, rest) ->
      let continuation = T.flatten rest tail in
      let close = C.Next (I.Plain I.End, continuation) in
      scan_def (T.flatten code tail) scopes;
      flatten_correct body close (Scope (Block, scopes)) ();
      scan_def close (Scope (Block, scopes)); flatten_correct rest tail scopes ()
    | T.Loop (body, rest) ->
      let continuation = T.flatten rest tail in
      let close = C.Next (I.Plain I.End, continuation) in
      scan_def (T.flatten code tail) scopes;
      flatten_correct body close (Scope (Loop, scopes)) ();
      scan_def close (Scope (Loop, scopes)); flatten_correct rest tail scopes ()
    | T.If (yes, no, rest) ->
      let continuation = T.flatten rest tail in
      let close = C.Next (I.Plain I.End, continuation) in
      let otherwise = C.Next (I.Plain I.Else, T.flatten no close) in
      scan_def (T.flatten code tail) scopes;
      flatten_correct yes otherwise (Scope (Then, scopes)) ();
      scan_def otherwise (Scope (Then, scopes));
      flatten_correct no close (Scope (Else, scopes)) ();
      scan_def close (Scope (Else, scopes)); flatten_correct rest tail scopes ())
