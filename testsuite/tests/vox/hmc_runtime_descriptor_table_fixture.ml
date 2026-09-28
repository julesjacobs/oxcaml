module B = Wasm_u32
module R = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module Descriptor = Hmc_runtime_descriptor
module Index = Hmc_u32_index
module Load = Hmc_wasm_descriptor_load
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module F = Hmc_wasm_descriptor_load_fixture
let rec ordered = function
  | R.Empty -> []
  | R.Add (d, rest) -> ordered rest @ [Hmc_tagged_cell.Word {Hmc_word64.lo = d.R.start; hi = d.R.captures}; Hmc_tagged_cell.Boolean d.R.recursive]
let rec prefix n tail = if n = 0 then tail else B.Byte (173, prefix (n - 1) tail)
let check (base : B.u32) table =
  if base > 7 then failwith "table base" else
  match Index.encode 3 (R.size table) with
  | None -> failwith "table size"
  | Some count ->
    let before = F.fill 512 in
    match Hmc_linear_bytes.drop before 512 with
    | None -> failwith "table capacity"
    | Some _ ->
      ghost_ (Hmc_linear_bounds.covers_def before 512);
      let memory = Table.store table before base count 512 () in
      let expected = prefix base (Hmc_heap_wire.encode_cells (Hmc_wasm_call_operands_fixture.cells (ordered table))
        (F.fill (512 - base - 32 * count))) in
      if memory <> expected then failwith "table memory layout";
      List.iter (fun (code : Table.count) ->
        match R.lookup table code with
        | None -> if code < count then failwith "missing descriptor"
        | Some descriptor ->
          ghost_ (Table.lookup table memory base count code descriptor ();
            Table.address_def base code; S.add32_def base (32 * code));
          let address = Table.address base code in
          let locals = F.locals [address; 0; 0; 0] in
          let slots = {Load.start = 1; captures = 2; recursive = 3} in
          if not (Load.distinct slots 0 && Load.writable slots locals) then failwith "table slots" else
          match L.get locals 0 with
          | Some (S.I32 actual) when actual = address ->
            let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
            let out = Load.correct descriptor state address 0 slots () in
            if out <> F.locals [address; descriptor.R.start; descriptor.R.captures; S.boolean descriptor.R.recursive]
            then failwith "table descriptor";
            let initial = {X.memory; machine = {E.locals = F.locals [0; 0; 0; 0; code]; stack = S.Empty}} in
            let expected = {X.memory; machine = {E.locals = F.locals [address; descriptor.R.start; descriptor.R.captures;
              S.boolean descriptor.R.recursive; code]; stack = S.Empty}} in
            if X.run (Hmc_wasm_descriptor_select.emit base 4 0 slots) initial <> X.Done expected
            then failwith "indexed descriptor execution"
          | _ -> failwith "table address") [0; 1; 2; 3]
let fixtures () =
  let first = {R.start = 7; captures = 0; recursive = false} in
  let second = {R.start = 19; captures = 3; recursive = true} in
  let third = {R.start = 4294967295; captures = 4294967295; recursive = false} in
  List.iter (fun base -> List.iter (check base)
    [R.Empty; R.Add (first, R.Empty); R.Add (third, R.Add (second, R.Add (first, R.Empty)))]) [0; 7]
