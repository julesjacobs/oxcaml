module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module S = Hmc_cfg_semantics
module I = Hmc_tail_ir
module H = Hmc_frame_shape
module F = Hmc_frame_codec
module B = Hmc_frame_capacity

type frame = {pc : D.index; slots : F.cells}
let[@def] (decode @ total) (blocks : G.table @ immutable) (frame : frame @ immutable) =
  match G.lookup blocks frame.pc with None -> None | Some block ->
    match F.decode block.G.signature frame.pc frame.slots with None -> None | Some (a, _) -> Some a
let (encode @ total) : (p : I.program) @ immutable -> (a : S.activation) @ immutable ->
    {u : unit | H.at_label p.I.origin.C.origin.P.table p.I.origin.C.blocks a} ->
    {out : frame | decode p.I.origin.C.blocks out === Some a && F.length out.slots === B.capacity p.I.origin.C.blocks
      && out.pc === a.S.pc} @ immutable = fun p a premise ->
  let blocks = p.I.origin.C.blocks in
  ghost_ (H.at_label_def p.I.origin.C.origin.P.table blocks a);
  match G.lookup blocks a.S.pc with
  | None -> unreachable_ ()
  | Some block ->
    ghost_ (F.shaped p.I.origin.C.origin.P.table block.G.signature a (); B.lookup blocks a.S.pc block ());
    let padding = B.padding (B.remaining (B.capacity blocks) (F.size block.G.signature) ()) in
    let slots = F.encode block.G.signature a padding () in
    let out = {pc = a.S.pc; slots} in ghost_ (decode_def blocks out); out

type stack = Empty | Saved of frame * stack [@@inductive]
let[@def] rec (depth @ total) (stack : stack @ immutable) = match stack with Empty -> D.Z | Saved (_, rest) -> D.S (depth rest)
let[@def] rec (decode_stack @ total) (blocks : G.table @ immutable) (stack : stack @ immutable) = match stack with
  | Empty -> Some S.Halt
  | Saved (head, rest) -> (match decode blocks head, decode_stack blocks rest with
    | Some a, Some frames -> Some (S.Frame (a, frames)) | _ -> None)
let[@def] rec (uniform @ total) (capacity : D.index @ immutable) (stack : stack @ immutable) = ghost_ (match stack with
  | Empty -> true | Saved (head, rest) -> F.length head.slots === capacity && uniform capacity rest)
let rec (encode_stack @ total) : (p : I.program) @ immutable -> (stack : S.frames) @ immutable ->
    {u : unit | H.frames p.I.origin.C.origin.P.table p.I.origin.C.blocks stack} ->
    {out : stack | decode_stack p.I.origin.C.blocks out === Some stack && depth out === S.depth stack
      && uniform (B.capacity p.I.origin.C.blocks) out} @ immutable = fun p stack premise ->
  ghost_ (H.frames_def p.I.origin.C.origin.P.table p.I.origin.C.blocks stack; S.depth_def stack);
  match stack with
  | S.Halt ->
    ghost_ (decode_stack_def p.I.origin.C.blocks Empty; depth_def Empty; uniform_def (B.capacity p.I.origin.C.blocks) Empty); Empty
  | S.Frame (a, rest) ->
    let head = encode p a () in
    let tail = encode_stack p rest () in
    let out = Saved (head, tail) in
    ghost_ (decode_stack_def p.I.origin.C.blocks out; depth_def out; uniform_def (B.capacity p.I.origin.C.blocks) out); out

type state = Running of frame * stack | Done of Hmc_closure_semantics.V.value | Stuck [@@inductive]
let[@def] (saved_depth @ total) (state : state @ immutable) = match state with Running (_, stack) -> depth stack | _ -> D.Z
let[@def] (runtime_depth @ total) (state : S.state @ immutable) = match state with S.Running (_, stack) -> S.depth stack | _ -> D.Z
let[@def] (decode_state @ total) (blocks : G.table @ immutable) (state : state @ immutable) = match state with
  | Done v -> Some (S.Done v) | Stuck -> Some S.Stuck
  | Running (a, stack) -> (match decode blocks a, decode_stack blocks stack with
    | Some a, Some stack -> Some (S.Running (a, stack)) | _ -> None)
let[@def] (fits @ total) (capacity : D.index @ immutable) (state : state @ immutable) = ghost_ (match state with
  | Done _ | Stuck -> true
  | Running (a, stack) -> F.length a.slots === capacity && uniform capacity stack)
let (encode_state @ total) : (p : I.program) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | H.state p.I.origin.C.origin.P.table p.I.origin.C.blocks state} ->
    {out : state | decode_state p.I.origin.C.blocks out === Some state && fits (B.capacity p.I.origin.C.blocks) out
      && saved_depth out === runtime_depth state} @ immutable =
  fun p state premise ->
    ghost_ (H.state_def p.I.origin.C.origin.P.table p.I.origin.C.blocks state; runtime_depth_def state);
    match state with
    | S.Done v -> ghost_ (decode_state_def p.I.origin.C.blocks (Done v); fits_def (B.capacity p.I.origin.C.blocks) (Done v); saved_depth_def (Done v)); Done v
    | S.Stuck -> ghost_ (decode_state_def p.I.origin.C.blocks Stuck; fits_def (B.capacity p.I.origin.C.blocks) Stuck; saved_depth_def Stuck); Stuck
    | S.Running (a, stack) ->
      let head = encode p a () in let tail = encode_stack p stack () in
      let out = Running (head, tail) in
      ghost_ (decode_state_def p.I.origin.C.blocks out; fits_def (B.capacity p.I.origin.C.blocks) out; saved_depth_def out); out
let (run @ total) : (p : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {out : state | decode_state p.I.origin.C.blocks out === Some (Hmc_tail_semantics.advance p fuel (Hmc_tail_semantics.initial p input))
      && fits (B.capacity p.I.origin.C.blocks) out
      && saved_depth out === runtime_depth (Hmc_tail_semantics.advance p fuel (Hmc_tail_semantics.initial p input))} @ immutable = fun p input fuel ->
  let state = Hmc_frame_reachable.run p input fuel in encode_state p state ()
