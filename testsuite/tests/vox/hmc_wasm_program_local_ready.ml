module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module V = Hmc_tagged_cell
module S = Hmc_cfg_semantics
module Lookup = Hmc_heap_simple
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Simple = Hmc_wasm_simple_lower
let (value @ total) : (program : I.program) @ immutable -> (heap : H.heap) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (abstract : S.state) @ immutable ->
    (index : D.index) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable -> (next : D.index) @ immutable ->
    {u : unit | Q.decode heap (Q.Running (activation, frames)) === Some abstract
      && I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Load (G.Local index, ty, derivation, next)))
      && Hmc_heap_operand_shapes.not_stuck (U.step program abstract)} ->
    {out : V.value | Lookup.lookup activation.F.env index === Some out} @ immutable =
  fun program heap activation frames abstract index ty derivation next premise ->
    ghost_ (Q.decode_def heap (Q.Running (activation, frames)); F.decode_def heap activation;
      U.step_def program abstract; Hmc_heap_operand_shapes.not_stuck_def (U.step program abstract);
      match F.decode heap activation with
      | Some source ->
        S.load_def program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.globals source.S.env (G.Local index);
        Hmc_heap_simple_proofs.lookup heap activation.F.env source.S.env index ()
      | None -> ());
    match Lookup.lookup activation.F.env index with Some out -> out | None -> unreachable_ ()
let rec (environment_bound @ total) : (context : D.context) @ immutable -> (cells : H.cells) @ immutable ->
    (env : H.cells) @ immutable -> (suffix : H.cells) @ immutable -> (index : D.index) @ immutable ->
    (number : B.u32) -> (count : B.u32) ->
    {u : unit | Codec.decode_environment context cells === Some (env, suffix)
      && Index.represents index number && Index.represents (H.length cells) count
      && (match Lookup.lookup env index with None -> false | Some _ -> true)} ->
    {u : unit | number < count} @ ghost =
  fun context cells env suffix index number count premise -> ghost_ (
    Codec.decode_environment_def context cells; Lookup.lookup_def env index;
    Index.represents_def index number; H.length_def cells; Index.represents_def (H.length cells) count;
    match context, cells with
    | D.Binding (_, rest), H.Cell (_, tail) ->
      (match Codec.decode_environment rest tail with
      | Some (remaining, _) ->
        (match index with D.Z -> () | D.S i -> environment_bound rest tail remaining suffix i (number - 1) (count - 1) ())
      | None -> ())
    | _ -> ())
let (address_bound @ total) : (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable -> (index : D.index) @ immutable ->
    (number : Simple.slot) -> (count : B.u32) -> (base : B.u32) -> (frame_end : B.u32) ->
    {u : unit | Codec.decode signature activation.F.pc cells === Some (activation, padding)
      && Index.represents index number && Index.represents (D.S (H.length cells)) count
      && frame_end = base + 16 * count
      && (match Lookup.lookup activation.F.env index with None -> false | Some _ -> true)} ->
    {u : unit | base + Simple.slot_tag number <= 4294967280} @ ghost =
  fun signature activation cells padding index number count base frame_end premise -> ghost_ (
    Codec.decode_def signature activation.F.pc cells;
    Index.represents_def (D.S (H.length cells)) count; H.length_def cells;
    Index.represents_def (H.length cells) (count - 1); Simple.slot_tag_def number;
    match cells with
    | H.Cell (_, (H.Cell (_, body) as rest)) ->
      H.length_def rest; Index.represents_def (H.length rest) (count - 2);
      (match Codec.decode_environment signature.G.locals body with
      | Some (env, suffix) -> environment_bound signature.G.locals body env suffix index number (count - 3) ()
      | None -> ())
    | _ -> ())
