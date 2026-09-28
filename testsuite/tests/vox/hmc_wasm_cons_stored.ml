module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module L = Hmc_linear_bytes
module P = Hmc_linear_preservation
module Wire = Hmc_heap_wire
module Q = Wasm_word_sequence
module Four = Wasm_four_words
module Memory = Hmc_memory_object
let rec (take_self @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | L.take (V.length bytes) bytes === Some bytes} @ ghost = fun bytes -> ghost_ (
    V.length_def bytes; L.take_def (V.length bytes) bytes;
    match bytes with B.End -> () | B.Byte (_, rest) -> take_self rest)
let (correct @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    {u : unit | L.drop memory base === Some bytes
      && Wire.decode Wire.Cons_schema bytes === Some (Wire.Cons (head, tail), suffix)} ->
    {u : unit | Memory.load memory base Wire.Cons_schema === Some (Wire.Cons (head, tail))} @ ghost =
  fun memory base bytes suffix head tail premise -> ghost_ (
    let payload = Wire.encode (Wire.Cons (head, tail)) B.End in
    Wire.schema_def (Wire.Cons (head, tail)); Wire.slots_def (Wire.Cons (head, tail)); V.length_def B.End;
    Hmc_wasm_cons_write.words head tail bytes suffix ();
    Hmc_wasm_cons_write.words head tail payload B.End ();
    Four.size (V.tag head) (V.payload head) (V.tag tail) (V.payload tail); Four.width_def ();
    Q.prefix (Four.layout (V.tag head) (V.payload head) (V.tag tail) (V.payload tail)) bytes payload suffix B.End 32 ();
    Hmc_heap_extent.span_def (D.S (D.S D.Z)) 0 32;
    Hmc_heap_extent.span_def (D.S D.Z) 16 32; Hmc_heap_extent.span_def D.Z 32 32;
    Hmc_memory_extent.cells (D.S (D.S D.Z)) 0 32 ();
    Hmc_linear_bounds.range_def (Wire.bytes_size (D.S (D.S D.Z)) D.Z) 0 32;
    P.take (Wire.bytes_size (D.S (D.S D.Z)) D.Z) 32 bytes payload ();
    take_self payload;
    Memory.load_def memory base Wire.Cons_schema; Memory.slots_def Wire.Cons_schema;
    L.load_def memory base (Wire.bytes_size (D.S (D.S D.Z)) D.Z))
