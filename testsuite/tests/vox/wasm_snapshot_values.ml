module B = Wasm_u32
module S = Wasm_scalar
module L = Wasm_locals
module M = Wasm_memory
module Read = Wasm_frame_snapshot
let[@def] rec (selected @ total) (reads : Read.reads @ immutable) (local : B.u32) : B.u32 option @ immutable = match reads with
  | Read.End -> None
  | Read.Read (offset, destination, rest) -> (match selected rest local with
    | Some offset -> Some offset | None -> if local = destination then Some offset else None)
let rec (get @ total) : (reads : Read.reads) @ immutable -> (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (before : S.stack) @ immutable -> (after : S.stack) @ immutable -> (local : B.u32) ->
    {u : unit | Read.project reads memory base before === Some after} ->
    {u : unit | L.get after local === (match selected reads local with None -> L.get before local | Some offset -> M.load memory base offset M.W64)} @ ghost =
  fun reads memory base before after local premise -> ghost_ (
    Read.project_def reads memory base before; selected_def reads local;
    match reads with
    | Read.End -> ()
    | Read.Read (offset, destination, rest) -> (match M.load memory base offset M.W64 with
      | None -> ()
      | Some value -> (match L.set before destination value with
        | None -> ()
        | Some next ->
          get rest memory base next after local ();
          if local <> destination then L.other_local before destination value next local () else ())))
let rec (can_set @ total) : (before : S.stack) @ immutable -> (after : S.stack) @ immutable -> (index : B.u32) -> (value : S.value) @ immutable ->
    {u : unit | L.same_types before after && L.can_set before index value} ->
    {u : unit | L.can_set after index value} @ ghost = fun before after index value premise -> ghost_ (
    L.same_types_def before after; L.can_set_def before index value; L.can_set_def after index value;
    L.get_def before index; L.get_def after index;
    match before, after with
    | S.Push (old, rest), S.Push (new_, tail) ->
      if index = 0 then (S.same_type_def old new_; S.same_type_def old value; S.same_type_def new_ value)
      else (L.can_set_def rest (index - 1) value; L.can_set_def tail (index - 1) value; can_set rest tail (index - 1) value ())
    | _ -> ())
let[@def] rec (ready @ total) (reads : Read.reads @ immutable) (memory : B.bytes @ immutable) (base : B.u32) (locals : S.stack @ immutable) =
  match reads with
  | Read.End -> true
  | Read.Read (offset, destination, rest) -> (match M.load memory base offset M.W64 with
    | None -> false | Some value -> L.can_set locals destination value && ready rest memory base locals)
let rec (preserve @ total) : (reads : Read.reads) @ immutable -> (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (before : S.stack) @ immutable -> (after : S.stack) @ immutable ->
    {u : unit | ready reads memory base before && L.same_types before after} ->
    {u : unit | ready reads memory base after} @ ghost = fun reads memory base before after premise -> ghost_ (
    ready_def reads memory base before; ready_def reads memory base after;
    match reads with
    | Read.End -> ()
    | Read.Read (offset, destination, rest) -> (match M.load memory base offset M.W64 with
      | None -> () | Some value -> can_set before after destination value (); preserve rest memory base before after ()))
let rec (project @ total) : (reads : Read.reads) @ immutable -> (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (locals : S.stack) @ immutable -> {u : unit | ready reads memory base locals} ->
    {out : S.stack | Read.project reads memory base locals === Some out} @ immutable =
  fun reads memory base locals premise ->
    ghost_ (ready_def reads memory base locals; Read.project_def reads memory base locals);
    match reads with
    | Read.End -> locals
    | Read.Read (offset, destination, rest) -> (match M.load memory base offset M.W64 with
      | None -> unreachable_ ()
      | Some value -> (match L.set locals destination value with
        | None -> unreachable_ ()
        | Some next -> ghost_ (preserve rest memory base locals next ()); project rest memory base next ()))
let rec (unselected @ total) : (reads : Read.reads) @ immutable -> (local : B.u32) ->
    {u : unit | Read.separate reads local} -> {u : unit | selected reads local === None} @ ghost =
  fun reads local premise -> ghost_ (
    Read.separate_def reads local; selected_def reads local;
    match reads with Read.End -> () | Read.Read (_, _, rest) -> unselected rest local ())
let (unchanged @ total) : (reads : Read.reads) @ immutable -> (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (before : S.stack) @ immutable -> (after : S.stack) @ immutable -> (local : B.u32) ->
    {u : unit | Read.separate reads local && Read.project reads memory base before === Some after} ->
    {u : unit | L.get after local === L.get before local} @ ghost = fun reads memory base before after local premise -> ghost_ (
    unselected reads local (); get reads memory base before after local ())
