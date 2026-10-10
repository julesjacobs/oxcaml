module B = Wasm_u32
module W = Hmc_word64
module Header = Hmc_wasm_header_update
module Write = Wasm_mixed_write
let[@def] (writes @ total) (recursive : bool) (pc : W.limb) (closure : B.u32) (argument_tag : B.u32) (argument_payload : B.u32) =
  let self = if recursive then Write.Write (64, Write.Constant (Header.number 4), Write.Write (72, Write.Pointer_local closure, Write.End)) else Write.End in
  Write.Write (0, Write.Constant (Header.number 1), Write.Write (8, Write.Constant (Header.number pc),
    Write.Write (16, Write.Constant (Header.number 4), Write.Write (24, Write.Pointer_local closure,
    Write.Write (32, Write.Constant (Header.number 2), Write.Write (40, Write.Constant (Header.number 0),
    Write.Write (48, Write.Word_local argument_tag, Write.Write (56, Write.Word_local argument_payload, self))))))))
let[@def] (emit @ total) (recursive : bool) (pc : W.limb) (frame : B.u32) (closure : B.u32) (argument_tag : B.u32) (argument_payload : B.u32) =
  Write.emit (writes recursive pc closure argument_tag argument_payload) frame
