module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module R = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
module PC = Hmc_wasm_pc_update
module E = Wasm_execution
module X = Wasm_memory_execution
module S = Wasm_scalar
module L = Wasm_locals

let rec (sum @ total) : (left : D.index) @ immutable -> (right : D.index) @ immutable ->
    (a : R.count) -> (b : R.count) -> (total : R.count) ->
    {u : unit | Index.represents left a && Index.represents right b && a + b = total} ->
    {u : unit | Index.represents (D.add left right) total} @ ghost =
  fun left right a b total premise -> ghost_ (
    D.add_def left right; Index.represents_def left a;
    match left with D.Z -> () | D.S tail ->
      sum tail right (a - 1) b (total - 1) ();
      Index.represents_def (D.add left right) (a + b))
type fragment = {copies : Plan.plan; pc : B.u32}
let[@def] (matches @ total) (signature : G.signature @ immutable) (next : D.index @ immutable)
    (capacity : R.count) (fragment : fragment @ immutable) = ghost_ (
  Index.represents next fragment.pc && match signature.G.temporaries with
  | G.Value (saved, _, rest) ->
    let env = Geometry.size (Codec.locals_size signature.G.locals) in
    let count = Geometry.size (Codec.locals_size saved) in
    let remaining = Geometry.size (Codec.temporaries_size rest) in
    3 + env + count + remaining <= capacity &&
    (match fragment.copies with
    | Plan.Copy (tag_source, tag_destination, Plan.Copy (pc_source, pc_destination, cells)) ->
      tag_source = 0 && tag_destination = 0 && pc_source = 8 && pc_destination = 8 &&
      Geometry.two cells 0 0 (D.S (D.S D.Z))
        (3 + env) 2 (D.add (Codec.locals_size saved) (Codec.temporaries_size rest))
    | _ -> false)
  | _ -> false)
let[@def] (encodable @ total) (signature : G.signature @ immutable) (next : D.index @ immutable)
    (capacity : R.count) (max_pc : B.u32) = ghost_ (
  match signature.G.temporaries with
  | G.Value (saved, _, rest) ->
    Index.fits (Codec.locals_size signature.G.locals) capacity
    && Index.fits (Codec.locals_size saved) capacity
    && Index.fits (Codec.temporaries_size rest) capacity
    && Index.fits next max_pc
    && 3 + Geometry.size (Codec.locals_size signature.G.locals)
      + Geometry.size (Codec.locals_size saved) + Geometry.size (Codec.temporaries_size rest) <= capacity
  | _ -> false)
let (build @ total) : (signature : G.signature) @ immutable -> (next : D.index) @ immutable ->
    (capacity : R.count) -> (max_pc : B.u32) ->
    {out : fragment option | match out with None -> not (encodable signature next capacity max_pc)
      | Some fragment -> encodable signature next capacity max_pc && matches signature next capacity fragment} @ immutable =
  fun signature next capacity max_pc ->
  ghost_ (encodable_def signature next capacity max_pc);
  match signature.G.temporaries with
  | G.Value (saved, _, rest) ->
    (match Index.encode capacity (Codec.locals_size signature.G.locals),
      Index.encode capacity (Codec.locals_size saved), Index.encode capacity (Codec.temporaries_size rest), Index.encode max_pc next with
    | Some env, Some count, Some remaining, Some pc ->
      ghost_ (Geometry.size_represents (Codec.locals_size signature.G.locals) env ();
        Geometry.size_represents (Codec.locals_size saved) count ();
        Geometry.size_represents (Codec.temporaries_size rest) remaining ());
      if 3 + env + count + remaining > capacity then None else (
      let length = D.add (Codec.locals_size saved) (Codec.temporaries_size rest) in
      ghost_ (sum (Codec.locals_size saved) (Codec.temporaries_size rest) count remaining (count + remaining) ();
        Index.represents_def (D.S (D.S D.Z)) 2; Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0);
      let tail = R.range (3 + env) 2 (count + remaining) Plan.End length () in
      let copies = R.range 0 0 2 tail (D.S (D.S D.Z)) () in
      let fragment = {copies = Plan.Copy (0, 0, Plan.Copy (8, 8, copies)); pc} in
      ghost_ (Geometry.two_def copies 0 0 (D.S (D.S D.Z)) (3 + env) 2 length;
        matches_def signature next capacity fragment);
      Some fragment)
    | _ -> None)
  | _ -> None
let[@def] (emit @ total) (fragment : fragment @ immutable) (source_local : B.u32) (destination_local : B.u32) =
  E.append (Copy.emit fragment.copies source_local destination_local) (PC.emit fragment.pc destination_local)
let (correct @ total) : (fragment : fragment) @ immutable -> (source_local : B.u32) -> (destination_local : B.u32) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (destination : B.u32) ->
    (copied : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals destination_local === Some (S.I32 destination)
      && Copy.apply fragment.copies state.X.memory source destination === Some copied
      && R.finish copied destination fragment.pc === Some after} ->
    {u : unit | X.run (emit fragment source_local destination_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun fragment source_local destination_local state source destination copied after premise -> ghost_ (
    Copy.correct fragment.copies source_local destination_local state source destination copied ();
    R.finish_def copied destination fragment.pc;
    PC.correct fragment.pc destination_local {X.memory = copied; machine = state.X.machine} destination after ();
    emit_def fragment source_local destination_local;
    X.append_correct (Copy.emit fragment.copies source_local destination_local) (PC.emit fragment.pc destination_local) state)
