module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module K = Hmc_closure_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Plans = Hmc_wasm_call_plan_table
module Copy = Hmc_wasm_call_captures
module Geometry = Hmc_wasm_relayout_geometry
module Model = Hmc_frame_call_entry
let rec (capture_capacity @ total) : (table : K.table) @ immutable -> (plans : Plans.table) @ immutable ->
    (capacity : Hmc_wasm_relayout.count) -> (id : D.index) @ immutable -> (entry : K.entry) @ immutable ->
    {u : unit | Plans.related table capacity plans && K.lookup table id === Some entry} ->
    {u : unit | Index.fits (Codec.locals_size entry.K.captured) capacity
      && (if entry.K.recursive then 4 else 3) + Geometry.size (Codec.locals_size entry.K.captured) <= capacity} @ ghost =
  fun table plans capacity id entry premise -> ghost_ (
    Plans.related_def table capacity plans; K.lookup_def table id;
    match table, plans with
    | K.Add (head, rest), Plans.Add (_, fragment, tail) ->
      if Hm_elaboration_check.index_equal id (K.size rest) then (
        Copy.matches_def head capacity fragment; Copy.position_def head.K.recursive)
      else capture_capacity rest tail capacity id entry ()
    | _ -> ())
let (entry_block @ total) : (program : I.program) @ immutable -> (id : D.index) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable ->
    {u : unit | K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_} ->
    {block : G.block | G.lookup program.I.origin.C.blocks function_.C.start === Some block
      && block.G.signature === Model.signature entry} @ immutable = fun program id entry function_ premise ->
    ghost_ (C.valid_def program.I.origin;
      let _ = C.lookup_origin program.I.origin.C.blocks program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions id entry () in
      C.function_valid_def program.I.origin.C.blocks entry function_;
      G.entry_def program.I.origin.C.blocks function_.C.start (K.context entry) G.Empty_temporaries;
      Model.signature_def entry);
    match G.lookup program.I.origin.C.blocks function_.C.start with Some block -> block | None -> unreachable_ ()
let rec (environment_extent @ total) : (context : D.context) @ immutable -> (cells : H.cells) @ immutable ->
    (env : H.cells) @ immutable -> (tail : H.cells) @ immutable -> (count : B.u32) -> (env_count : B.u32) ->
    {u : unit | Codec.decode_environment context cells === Some (env, tail)
      && Index.represents (H.length cells) count && Index.represents (Codec.locals_size context) env_count} ->
    {u : unit | env_count <= count && Index.represents (H.length tail) (Hmc_wasm_program_block.difference count env_count)} @ ghost =
  fun context cells env tail count env_count premise -> ghost_ (
    Hmc_wasm_program_block.difference_def count env_count;
    Codec.decode_environment_def context cells; Codec.locals_size_def context;
    H.length_def cells; Index.represents_def (H.length cells) count;
    Index.represents_def (Codec.locals_size context) env_count;
    match context, cells with
    | D.Binding (_, rest), H.Cell (_, remaining) ->
      (match Codec.decode_environment rest remaining with
      | Some (env, suffix) -> environment_extent rest remaining env suffix (count - 1) (env_count - 1) (); Hmc_wasm_program_block.difference_def (count - 1) (env_count - 1)
      | None -> ())
    | _ -> ())
let (operand_address @ total) : (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable -> (env_count : Hmc_wasm_simple_lower.slot) ->
    (count : B.u32) -> (base : B.u32) -> (frame_end : B.u32) ->
    {u : unit | Codec.decode signature activation.F.pc cells === Some (activation, padding)
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && Index.represents (D.S (H.length cells)) count && frame_end = base + 16 * count
      && (match signature.G.temporaries with G.Value _ -> true | _ -> false)} ->
    {u : unit | base + 48 + 16 * env_count <= 4294967280} @ ghost =
  fun signature activation cells padding env_count count base frame_end premise -> ghost_ (
    Codec.decode_def signature activation.F.pc cells;
    Index.represents_def (D.S (H.length cells)) count; H.length_def cells;
    Index.represents_def (H.length cells) (count - 1);
    match cells with
    | H.Cell (_, (H.Cell (_, body) as rest)) ->
      H.length_def rest; Index.represents_def (H.length rest) (count - 2);
      (match Codec.decode_environment signature.G.locals body with
      | Some (env, remaining) ->
        environment_extent signature.G.locals body env remaining (count - 3) env_count ();
        Hmc_wasm_program_block.difference_def (count - 3) env_count;
        Codec.decode_temporaries_def signature.G.temporaries remaining;
        H.length_def remaining; Index.represents_def (H.length remaining) (count - 3 - env_count)
      | None -> ())
    | _ -> ())
let rec (size_add @ total) : (left : D.index) @ immutable -> (right : D.index) @ immutable ->
    {u : unit | Geometry.size (D.add left right) = Geometry.size left + Geometry.size right} @ ghost =
  fun left right -> ghost_ (
    D.add_def left right; Geometry.size_def left; Geometry.size_def (D.add left right);
    match left with D.Z -> () | D.S rest -> size_add rest right)
