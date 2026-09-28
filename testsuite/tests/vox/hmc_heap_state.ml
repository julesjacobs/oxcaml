module D = Hm_declarative
module M = Hmc_heap_objects
module C = Hmc_tagged_cell
module F = Hmc_heap_frame
module S = Hmc_cfg_semantics
module K = Hmc_closure_ir
module P = Hmc_heap_preservation

type frames = Halt | Frame of F.activation * frames [@@inductive]
type state = Running of F.activation * frames | Done of C.value | Stuck [@@inductive]
let[@def] rec (depth @ total) (frames : frames @ immutable) = match frames with
  | Halt -> D.Z | Frame (_, rest) -> D.S (depth rest)
let[@def] rec (decode_frames @ total) (heap : M.heap @ immutable) (frames : frames @ immutable) = match frames with
  | Halt -> Some S.Halt
  | Frame (a, rest) -> (match F.decode heap a, decode_frames heap rest with
    | Some abstract, Some tail -> Some (S.Frame (abstract, tail)) | _ -> None)
let[@def] (decode @ total) (heap : M.heap @ immutable) (state : state @ immutable) = match state with
  | Stuck -> Some S.Stuck
  | Done value -> (match M.decode heap value with None -> None | Some value -> Some (S.Done value))
  | Running (a, frames) -> (match F.decode heap a, decode_frames heap frames with
    | Some a, Some frames -> Some (S.Running (a, frames)) | _ -> None)
let rec (frames_preserve @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable -> (smaller : M.heap) @ immutable ->
    (frames : frames) @ immutable -> (abstract : S.frames) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && P.extends larger smaller
      && decode_frames smaller frames === Some abstract} ->
    {u : unit | decode_frames larger frames === Some abstract && depth frames === S.depth abstract} @ ghost =
  fun table larger smaller frames abstract premise -> ghost_ (
    decode_frames_def smaller frames; decode_frames_def larger frames; depth_def frames;
    match frames with
    | Halt -> S.depth_def abstract
    | Frame (a, rest) -> (match F.decode smaller a, decode_frames smaller rest with
      | Some value, Some tail -> F.preserve table larger smaller a value ();
        frames_preserve table larger smaller rest tail (); S.depth_def abstract
      | _ -> ()))
let (preserve @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable -> (smaller : M.heap) @ immutable ->
    (state : state) @ immutable -> (abstract : S.state) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && P.extends larger smaller && decode smaller state === Some abstract} ->
    {u : unit | decode larger state === Some abstract} @ ghost = fun table larger smaller state abstract premise -> ghost_ (
  decode_def smaller state; decode_def larger state;
  match state with
  | Stuck -> ()
  | Done value -> (match M.decode smaller value with None -> () | Some value' -> P.decode_preserve table larger smaller value value' ())
  | Running (a, frames) -> (match F.decode smaller a, decode_frames smaller frames with
    | Some value, Some tail -> F.preserve table larger smaller a value (); frames_preserve table larger smaller frames tail ()
    | _ -> ()))
