(* The input as a run-time value. The compiler builds the module's initial
   memory for [placeholder ()]: the entry frame then holds the placeholder as
   the payload of its first environment cell. The dispatcher's prologue
   overwrites that payload with the run's input. [install] shows that the
   result is the state the initializer would have built for that input. *)
module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module S = Wasm_scalar
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module CS = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module Heap = Hmc_heap_initialize
module Invariant = Hmc_heap_invariant
module Init = Hmc_wasm_program_initialize
module Lower = Hmc_wasm_program_lower
module State = Hmc_wasm_program_state
module Registers = Hmc_wasm_program_registers
module Resources = Hmc_wasm_program_resources
module Descriptors = Hmc_wasm_program_descriptors
module Frame = Hmc_wasm_program_frame
module Store = Hmc_wasm_program_frame_store
module Runtime = Hmc_wasm_program_runtime
module Codec = Hmc_pointer_frame_codec
module Header = Hmc_wasm_header_update
module Wire = Hmc_heap_wire
module Words = Hmc_wasm_wire_words
module Sequence = Wasm_word_sequence
module Update = Wasm_sequence_update
module Unique = Hmc_frame_decode_unique
module Memory = Wasm_memory
module Preservation = Hmc_wasm_frame_preservation

let[@def] (placeholder @ total) (unit : unit) : W.t @ immutable = {W.lo = 0; hi = 0}

(* The entry activation and the heap-machine start for another input. *)
let[@def] (entered @ total) (activation : F.activation @ immutable) (input : W.t @ immutable) : F.activation @ immutable =
  match activation.F.env with
  | H.Cell (_, rest) -> {activation with F.env = H.Cell (V.Word input, rest)}
  | H.Empty -> activation
let[@def] (restart @ total) (start : Heap.start @ immutable) (input : W.t @ immutable) : Heap.start @ immutable =
  {Heap.globals = start.Heap.globals; configuration = match start.Heap.configuration.X.state with
    | Q.Running (activation, frames) ->
      {X.heap = start.Heap.configuration.X.heap; state = Q.Running (entered activation input, frames)}
    | _ -> start.Heap.configuration}

(* [restart start input] satisfies [Heap.correct] for [input]: words are
   stored in cells rather than on the heap, so the heap is unchanged and
   only the entry's argument cell differs. *)
let (heap @ total) : (program : I.program) @ immutable -> (base : W.limb) -> (limit : W.limb) ->
    (old : W.t) @ immutable -> (input : W.t) @ immutable -> (start : Heap.start) @ immutable ->
    {u : unit | Heap.correct program base limit old (Heap.Initialized start)} ->
    {u : unit | Heap.correct program base limit input (Heap.Initialized (restart start input))
      && (match start.Heap.configuration.X.state with
        | Q.Running (activation, _) -> (match activation.F.env with H.Cell (first, _) -> first === V.Word old | H.Empty -> false)
        | _ -> false)} @ ghost =
  fun program base limit old input start premise -> ghost_ (
    let next = restart start input in
    Heap.correct_def program base limit old (Heap.Initialized start);
    Heap.correct_def program base limit input (Heap.Initialized next);
    restart_def start input;
    let configuration = start.Heap.configuration in
    let heap = configuration.X.heap in
    match configuration.X.state with
    | Q.Running (activation, frames) ->
      let updated = entered activation input in
      entered_def activation input;
      Invariant.valid_def program start.Heap.globals limit configuration (U.initial program old);
      Invariant.valid_def program start.Heap.globals limit next.Heap.configuration (U.initial program input);
      U.initial_def program old; U.initial_def program input;
      CS.initial_def program.I.origin old; CS.initial_def program.I.origin input;
      Q.decode_def heap configuration.X.state; Q.decode_def heap next.Heap.configuration.X.state;
      Q.decode_frames_def heap frames;
      F.decode_def heap activation; F.decode_def heap updated;
      H.decode_value_def (H.view heap) (V.Word input);
      (match activation.F.env with
      | H.Cell (first, _) ->
        H.decode_value_def (H.view heap) first;
        H.decode_environment_def (H.view heap) activation.F.env;
        H.decode_environment_def (H.view heap) updated.F.env
      | H.Empty -> H.decode_environment_def (H.view heap) activation.F.env);
      Hmc_frame_reachable.initial program input
    | _ -> ())

(* From [Hmc_memory_current_shape]: the valid heap machine's activation has the
   shape of its block's signature. *)
let (current_shape @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable -> (limit : W.limb) ->
    (model : X.configuration) @ immutable -> (abstract : CS.state) @ immutable ->
    {u : unit | Invariant.valid program globals limit model abstract} ->
    {u : unit | match model.X.state with Q.Running (a, _) ->
      (match G.lookup program.I.origin.Hmc_cfg_program.blocks a.F.pc with None -> false | Some block -> Codec.shape block.G.signature a)
      | _ -> true} @ ghost = fun program globals limit model abstract premise -> ghost_ (
  Invariant.valid_def program globals limit model abstract; Q.decode_def model.X.heap model.X.state;
  Hmc_frame_shape.state_def program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.I.origin.Hmc_cfg_program.blocks abstract;
  match model.X.state with
  | Q.Running (a, _) ->
    F.decode_def model.X.heap a;
    (match F.decode model.X.heap a with None -> () | Some source_a ->
      Hmc_frame_shape.at_label_def program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.I.origin.Hmc_cfg_program.blocks source_a;
      match G.lookup program.I.origin.Hmc_cfg_program.blocks a.F.pc with None -> () | Some block ->
        Hmc_frame_codec.shaped program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table block.G.signature source_a ();
        Hmc_pointer_frame_shape.shape model.X.heap block.G.signature a source_a ())
  | _ -> ())

(* The words of a frame's header cell and first three cells. *)
let[@def] (head @ total) (a : V.value @ immutable) (b : V.value @ immutable) (c : V.value @ immutable)
    (tag : W.t @ immutable) : Sequence.words @ immutable =
  Sequence.Word (V.tag a, Sequence.Word (V.payload a, Sequence.Word (V.tag b, Sequence.Word (V.payload b,
    Sequence.Word (V.tag c, Sequence.Word (V.payload c, Sequence.Word (tag, Sequence.End)))))))
let (frame_words @ total) : (count : D.index) @ immutable -> (bytes : B.bytes) @ immutable ->
    (a : V.value) @ immutable -> (b : V.value) @ immutable -> (c : V.value) @ immutable -> (d : V.value) @ immutable ->
    (cells : H.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Wire.decode_cells (D.S (D.S (D.S (D.S count)))) bytes
      === Some (H.Cell (a, H.Cell (b, H.Cell (c, H.Cell (d, cells)))), tail)} ->
    {rest : B.bytes | Sequence.decode (Sequence.append (head a b c (V.tag d)) (Sequence.Word (V.payload d, Sequence.End))) bytes
      === Some rest && Wire.decode_cells count rest === Some (cells, tail)} @ immutable =
  fun count bytes a b c d cells tail premise ->
    ghost_ (Wire.decode_cells_def (D.S (D.S (D.S (D.S count)))) bytes);
    match V.decode bytes with
    | None -> unreachable_ ()
    | Some (_, b1) ->
      ghost_ (Wire.decode_cells_def (D.S (D.S (D.S count))) b1);
      match V.decode b1 with
      | None -> unreachable_ ()
      | Some (_, b2) ->
        ghost_ (Wire.decode_cells_def (D.S (D.S count)) b2);
        match V.decode b2 with
        | None -> unreachable_ ()
        | Some (_, b3) ->
          ghost_ (Wire.decode_cells_def (D.S count) b3);
          match V.decode b3 with
          | None -> unreachable_ ()
          | Some (_, rest) ->
            let m0 = ghost_ (Words.parts bytes a b1 ()) in
            let m1 = ghost_ (Words.parts b1 b b2 ()) in
            let m2 = ghost_ (Words.parts b2 c b3 ()) in
            let m3 = ghost_ (Words.parts b3 d rest ()) in
            ghost_ (
              let w7 = Sequence.Word (V.payload d, Sequence.End) in
              let w6 = Sequence.Word (V.tag d, w7) in
              let w5 = Sequence.Word (V.payload c, w6) in
              let w4 = Sequence.Word (V.tag c, w5) in
              let w3 = Sequence.Word (V.payload b, w4) in
              let w2 = Sequence.Word (V.tag b, w3) in
              let w1 = Sequence.Word (V.payload a, w2) in
              let w0 = Sequence.Word (V.tag a, w1) in
              let h6 = Sequence.Word (V.tag d, Sequence.End) in
              let h5 = Sequence.Word (V.payload c, h6) in
              let h4 = Sequence.Word (V.tag c, h5) in
              let h3 = Sequence.Word (V.payload b, h4) in
              let h2 = Sequence.Word (V.tag b, h3) in
              let h1 = Sequence.Word (V.payload a, h2) in
              let h0 = Sequence.Word (V.tag a, h1) in
              head_def a b c (V.tag d);
              Sequence.append_def h0 w7; Sequence.append_def h1 w7; Sequence.append_def h2 w7; Sequence.append_def h3 w7;
              Sequence.append_def h4 w7; Sequence.append_def h5 w7; Sequence.append_def h6 w7; Sequence.append_def Sequence.End w7;
              W.equal_def (V.tag a) (V.tag a); W.equal_def (V.payload a) (V.payload a);
              W.equal_def (V.tag b) (V.tag b); W.equal_def (V.payload b) (V.payload b);
              W.equal_def (V.tag c) (V.tag c); W.equal_def (V.payload c) (V.payload c);
              W.equal_def (V.tag d) (V.tag d); W.equal_def (V.payload d) (V.payload d);
              Sequence.decode_def w0 bytes; Sequence.decode_def w1 m0; Sequence.decode_def w2 b1; Sequence.decode_def w3 m1;
              Sequence.decode_def w4 b2; Sequence.decode_def w5 m2; Sequence.decode_def w6 b3; Sequence.decode_def w7 m3;
              Sequence.decode_def Sequence.End rest);
            rest

let (head_size @ total) : (a : V.value) @ immutable -> (b : V.value) @ immutable -> (c : V.value) @ immutable ->
    (tag : W.t) @ immutable -> {u : unit | Sequence.size (head a b c tag) (Runtime.input_offset ())} @ ghost =
  fun a b c tag -> ghost_ (
    Runtime.input_offset_def (); head_def a b c tag;
    let h6 = Sequence.Word (tag, Sequence.End) in
    let h5 = Sequence.Word (V.payload c, h6) in
    let h4 = Sequence.Word (V.tag c, h5) in
    let h3 = Sequence.Word (V.payload b, h4) in
    let h2 = Sequence.Word (V.tag b, h3) in
    let h1 = Sequence.Word (V.payload a, h2) in
    let h0 = Sequence.Word (V.tag a, h1) in
    Sequence.size_def h0 56; Sequence.size_def h1 48; Sequence.size_def h2 40; Sequence.size_def h3 32;
    Sequence.size_def h4 24; Sequence.size_def h5 16; Sequence.size_def h6 8; Sequence.size_def Sequence.End 0)

(* [target] is [prepared] with the input replaced: the same code, context
   (up to the input) and registers, and the memory that the prologue's store
   produces. *)
let[@def] (retargeted @ total) (prepared : Init.prepared @ immutable) (input : W.t @ immutable) (target : Init.prepared @ immutable) = ghost_ (
  target.Init.lowered === prepared.Init.lowered
  && target.Init.context === {prepared.Init.context with State.input = input}
  && target.Init.state.State.registers === prepared.Init.state.State.registers
  && Memory.store prepared.Init.state.State.memory prepared.Init.state.State.registers.Registers.frame
    (Runtime.input_offset ()) (S.I64 input) === Some target.Init.state.State.memory)

(* The proof that the prologue's store of the input at byte
   [Runtime.input_offset] of the entry frame yields a state that satisfies
   [Init.ready] for [input], so that the proofs about runs, stated for the
   initializer's state, apply to the module built for the placeholder. *)
let (install @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable -> (prepared : Init.prepared) @ immutable ->
    (input : W.t) @ immutable ->
    {u : unit | Init.correct program layout (placeholder ()) memory (Init.Initialized (start, prepared))} ->
    {target : Init.prepared | Init.ready program layout input memory (restart start input) target
      && retargeted prepared input target} @ immutable ghost =
  fun program layout memory start prepared input premise -> ghost_ (
    let old = placeholder () in
    Init.correct_def program layout old memory (Init.Initialized (start, prepared));
    Init.installed_def program layout old memory start prepared;
    let state = prepared.Init.state in
    let context = prepared.Init.context in
    let lowered = prepared.Init.lowered in
    let registers = state.State.registers in
    let globals = start.Heap.globals in
    let next = restart start input in
    heap program layout.Init.heap_base layout.Init.heap_limit old input start ();
    Heap.correct_def program layout.Init.heap_base layout.Init.heap_limit old (Heap.Initialized start);
    Heap.correct_def program layout.Init.heap_base layout.Init.heap_limit input (Heap.Initialized next);
    State.valid_def program globals lowered context state; State.configuration_def state;
    restart_def start input;
    let activation = state.State.activation in
    let updated = entered activation input in
    let abstract = U.initial program input in
    let signature = state.State.block.G.signature in
    let header = V.Word (Header.number state.State.pc) in
    entered_def activation input;
    Resources.valid_def program globals lowered.Lower.width context.State.stack_base state.State.frame_end
      state.State.abstract state.State.heap activation state.State.frames registers state.State.memory;
    Resources.valid_def program globals lowered.Lower.width context.State.stack_base state.State.frame_end
      abstract state.State.heap updated state.State.frames registers state.State.memory;
    current_shape program globals layout.Init.heap_limit next.Heap.configuration abstract ();
    Frame.valid_def signature activation registers state.State.memory state.State.frame_end state.State.pc
      state.State.cells state.State.padding state.State.bytes state.State.suffix state.State.cell_count;
    H.length_def (H.Cell (header, state.State.cells));
    Preservation.suffix state.State.memory registers.Registers.frame state.State.frame_end state.State.bytes
      (H.Cell (header, state.State.cells)) state.State.suffix state.State.cell_count ();
    Codec.decode_def signature activation.F.pc state.State.cells;
    let encoded = Codec.encode signature updated state.State.padding () in
    (match state.State.cells with
    | H.Cell (_, H.Cell (_, rest)) -> Codec.decode_environment_def signature.G.locals rest
    | _ -> ());
    (match state.State.cells with
    | H.Cell (current, H.Cell (accumulator, H.Cell (first, rest))) ->
      let cells = H.Cell (current, H.Cell (accumulator, H.Cell (V.Word input, rest))) in
      Codec.decode_environment_def signature.G.locals (H.Cell (first, rest));
      Codec.decode_environment_def signature.G.locals (H.Cell (V.Word input, rest));
      Codec.decode_def signature updated.F.pc cells;
      Unique.frame signature updated.F.pc encoded cells updated state.State.padding ();
      H.length_def state.State.cells; H.length_def (H.Cell (accumulator, H.Cell (first, rest)));
      H.length_def (H.Cell (first, rest));
      H.length_def cells; H.length_def (H.Cell (accumulator, H.Cell (V.Word input, rest)));
      H.length_def (H.Cell (V.Word input, rest))
    | _ -> unreachable_ ());
    let out = Store.store program globals lowered.Lower.width context.State.stack_base state.State.frame_end abstract
      state.State.heap updated state.State.frames registers state.State.memory signature state.State.padding
      state.State.pc state.State.cell_count () in
    Frame.valid_def signature updated registers out.Store.memory state.State.frame_end state.State.pc
      out.Store.cells state.State.padding out.Store.bytes out.Store.suffix state.State.cell_count;
    (match state.State.cells with
    | H.Cell (current, H.Cell (accumulator, H.Cell (first, rest))) ->
      let cells = H.Cell (current, H.Cell (accumulator, H.Cell (V.Word input, rest))) in
      Codec.decode_environment_def signature.G.locals (H.Cell (first, rest));
      Codec.decode_environment_def signature.G.locals (H.Cell (V.Word input, rest));
      Codec.decode_def signature updated.F.pc cells;
      Unique.frame signature updated.F.pc out.Store.cells cells updated state.State.padding ();
      H.length_def state.State.cells; H.length_def (H.Cell (accumulator, H.Cell (first, rest)));
      H.length_def (H.Cell (first, rest));
      H.length_def cells; H.length_def (H.Cell (accumulator, H.Cell (V.Word input, rest)));
      H.length_def (H.Cell (V.Word input, rest)); H.length_def (H.Cell (header, cells));
      let count = H.length rest in
      let before = frame_words count state.State.bytes header current accumulator first rest state.State.suffix () in
      let after = frame_words count out.Store.bytes header current accumulator (V.Word input) rest out.Store.suffix () in
      Words.cells_unique count before after rest state.State.suffix ();
      V.tag_def first; V.tag_def (V.Word input); V.payload_def first; V.payload_def (V.Word input);
      Runtime.input_offset_def ();
      head_size header current accumulator (V.tag first);
      Update.at (head header current accumulator (V.tag first)) Sequence.End state.State.memory out.Store.memory
        state.State.bytes out.Store.bytes before (V.payload first) input registers.Registers.frame 56
        (registers.Registers.frame + 56) ()
    | _ -> unreachable_ ());
    Descriptors.preserve program registers registers state.State.memory out.Store.memory context.State.runtime
      context.State.table_base context.State.table_count ();
    let target_state = {state with State.abstract; activation = updated; memory = out.Store.memory;
      cells = out.Store.cells; bytes = out.Store.bytes; suffix = out.Store.suffix} in
    let target = {Init.lowered; context = {context with State.input}; state = target_state} in
    U.advance_def program D.Z abstract;
    State.valid_def program globals lowered target.Init.context target_state;
    State.configuration_def target_state;
    Init.installed_def program layout input memory next target;
    Init.ready_def program layout input memory next target;
    retargeted_def prepared input target;
    target)
