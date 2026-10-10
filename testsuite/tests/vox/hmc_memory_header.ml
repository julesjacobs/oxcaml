module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell
module M = Hmc_heap_objects
module Wire = Hmc_heap_wire
module L = Hmc_linear_bytes
module Memory = Hmc_memory_object
module Prefix = Hmc_memory_prefix
module Cap = Hmc_frame_capacity

let (load @ total) : (memory : B.bytes) @ immutable -> (address : W.limb) -> (count : D.index) @ immutable ->
    (code : W.limb) -> (captures : M.cells) @ immutable ->
    {u : unit | Memory.load memory address (Wire.Closure_schema count) === Some (Wire.Closure (code, captures))} ->
    {u : unit | Memory.load memory address (Wire.Closure_schema D.Z) === Some (Wire.Closure (code, M.Empty))} @ ghost =
  fun memory address count code captures premise -> ghost_ (
    Memory.load_def memory address (Wire.Closure_schema count); Memory.slots_def (Wire.Closure_schema count);
    Wire.bytes_size_def (D.S count) D.Z;
    let full = Wire.bytes_size (D.S count) D.Z in
    L.load_def memory address full;
    Memory.load_def memory address (Wire.Closure_schema D.Z); Memory.slots_def (Wire.Closure_schema D.Z);
    Wire.bytes_size_def (D.S D.Z) D.Z; Wire.bytes_size_def D.Z D.Z;
    L.load_def memory address (C.sixteen D.Z);
    match L.drop memory address with None -> () | Some bytes ->
      match L.take full bytes with None -> () | Some prefix ->
        Wire.decode_def (Wire.Closure_schema count) prefix;
        match C.decode prefix with
        | Some (C.Word word, rest) ->
          L.take_def D.Z rest;
          let header = Prefix.cell prefix D.Z (C.Word word) rest B.End () in
          Cap.le_def D.Z (Wire.bytes_size count D.Z);
          Prefix.sixteen_le D.Z (Wire.bytes_size count D.Z) ();
          Prefix.take (C.sixteen D.Z) full bytes prefix ();
          Wire.decode_def (Wire.Closure_schema D.Z) header; Wire.decode_cells_def D.Z B.End
        | _ -> ())
let[@def] (code @ total) (memory : B.bytes @ immutable) (address : W.limb) : W.limb option @ immutable =
  match Memory.load memory address (Wire.Closure_schema D.Z) with Some (Wire.Closure (code, _)) -> Some code | _ -> None
