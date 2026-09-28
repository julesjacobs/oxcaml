module F = Wasm_functions
module T = Wasm_control
module I = Wasm_instruction
module Checked = Wasm_shallow_module
let fixtures () =
  let block = {F.result = F.Void; locals = F.No_locals; code = T.Empty} in
  let dispatcher = {block with F.code = T.Instruction (I.I32_const 0, T.Instruction (I.Call_indirect 0, T.Empty))} in
  let module_ = {F.functions = F.Function (block, F.Function (dispatcher, F.No_functions));
    signatures = F.Signature (F.Void, F.No_signatures); table = F.Element (Some 0, F.No_elements)} in
  if Checked.check module_ 1 = None then failwith "call-free table rejected";
  if Checked.check module_ 2 <> None then failwith "missing entry accepted";
  let calls = {block with F.code = T.Block (T.Instruction (I.Call 1, T.Empty), T.Empty)} in
  let nested = {module_ with F.functions = F.Function (calls, F.Function (dispatcher, F.No_functions))} in
  if Checked.check nested 1 <> None then failwith "nested direct call accepted";
  let indirect = {block with F.code = T.If (T.Empty, T.Instruction (I.Call_indirect 0, T.Empty), T.Empty)} in
  let nested = {module_ with F.functions = F.Function (indirect, F.Function (dispatcher, F.No_functions))} in
  if Checked.check nested 1 <> None then failwith "nested indirect call accepted";
  if Checked.check {module_ with F.table = F.Element (Some 1, F.No_elements)} 1 <> None then failwith "dispatcher in table accepted";
  if Checked.check {module_ with F.table = F.Element (Some 99, F.No_elements)} 1 <> None then failwith "missing table function accepted";
  let direct = {dispatcher with F.code = T.Instruction (I.Call 0, T.Empty)} in
  if Checked.check {module_ with F.functions = F.Function (block, F.Function (direct, F.No_functions))} 1 <> None then
    failwith "direct call from root accepted"
