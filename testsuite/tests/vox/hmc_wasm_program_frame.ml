module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module V = Hmc_tagged_cell
module Registers = Hmc_wasm_program_registers
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Bytes = Hmc_linear_bytes
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
let[@def] (valid @ total) (signature : G.signature @ immutable) (activation : F.activation @ immutable)
    (registers : Registers.registers @ immutable) (memory : B.bytes @ immutable) (frame_end : B.u32) (pc : B.u32)
    (cells : H.cells @ immutable) (padding : H.cells @ immutable) (bytes : B.bytes @ immutable) (suffix : B.bytes @ immutable) (count : B.u32) = ghost_ (
  Index.represents activation.F.pc pc && Codec.decode signature activation.F.pc cells === Some (activation, padding)
  && registers.Registers.frame <= 4294967248 && Index.represents (D.S (H.length cells)) count
  && frame_end = registers.Registers.frame + 16 * count && Bytes.drop memory registers.Registers.frame === Some bytes
  && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number pc), cells), suffix))
