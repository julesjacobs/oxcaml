module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module Block = Hmc_wasm_program_block
module Table = Hmc_wasm_program_table
module Lower = Hmc_wasm_program_lower
module Index = Hmc_u32_index
module Header = Hmc_wasm_header_update
let[@def] (bound @ total) (unit : unit) : Wasm_u32.u32 = 10000
let rec check_lookup : (source : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (target : Lower.program) @ immutable -> (blocks : G.table) @ immutable ->
    {u : unit | Lower.corresponds source globals (bound ()) target} -> unit =
  fun source globals target blocks premise -> ghost_ (bound_def ()); match blocks with
  | G.Empty -> ()
  | G.Add (_, rest) ->
    (match Index.encode 10000 (G.size rest) with
    | None -> failwith "program label overflow"
    | Some number ->
      ghost_ (I.valid_def source; Lower.corresponds_def source globals 10000 target;
        Table.lookup_correct globals source.I.origin.C.blocks source.I.code source.I.sites target.Lower.blocks target.Lower.capacity 10000 (G.size rest) number ());
      (match I.lookup source.I.code (G.size rest), Table.lookup target.Lower.blocks number with
      | Some I.Tail_call, Some (Block.Tail_call _) -> ()
      | Some (I.Keep (G.Call _)), Some (Block.Call _) -> ()
      | Some (I.Keep G.Return), Some Block.Return -> ()
      | Some (I.Keep _), Some (Block.Structured _) -> ()
      | _ -> failwith "rewritten instruction lost during lowering"));
    check_lookup source globals target rest ()
let rec counts = function
  | Table.Empty -> (0, 0, 0)
  | Table.Add (_, fragment, rest) ->
    let calls, tails, returns = counts rest in
    match fragment with
    | Block.Call _ -> calls + 1, tails, returns
    | Block.Tail_call _ -> calls, tails + 1, returns
    | Block.Return -> calls, tails, returns + 1
    | Block.Structured _ -> calls, tails, returns
let check term =
  let source = Hmc_wasm_global_fixture.build term in
  match Hmc_heap_initialize.initialize source 1024 8192 (Header.number 7) () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "program initialization"
  | Hmc_heap_initialize.Initialized start ->
    let globals = start.Hmc_heap_initialize.globals in
    match Lower.lower source globals 10000 with
    | None -> failwith "complete program lowering rejected"
    | Some target ->
      ghost_ (bound_def ());
      check_lookup source globals target source.I.origin.C.blocks ();
      let _ = ghost_ (Lower.corresponds_def source globals 10000 target) in
      if target.Lower.width <> 16 + 16 * target.Lower.capacity then failwith "saved frame width";
      if Table.lower globals source.I.origin.C.blocks I.Empty (Hmc_frame_capacity.capacity source.I.origin.C.blocks) target.Lower.capacity 10000 () <> None then
        failwith "mismatched rewritten table accepted";
      if Lower.lower source globals 0 <> None then failwith "out-of-range block label accepted";
      counts target.Lower.blocks
let fixtures () =
  let word n = D.Word (Header.number n) in
  let captured = D.Lambda (D.Let (D.Apply (D.Lambda (D.Bound (D.S D.Z)), word 7), D.Bound D.Z)) in
  let recursive = D.Recursive (D.If (D.Primitive (D.Equal_word, D.Bound D.Z, word 0), word 17,
    D.Apply (D.Bound (D.S D.Z), D.Primitive (D.Subtract, D.Bound D.Z, word 1)))) in
  let lists = D.Lambda (D.CaseList (D.Cons (D.Bound D.Z, D.Nil), word 0, D.Bound D.Z)) in
  let calls, _, returns = check captured in
  if calls = 0 || returns = 0 then failwith "ordinary call coverage";
  let _, tails, returns = check recursive in
  if tails = 0 || returns = 0 then failwith "self-tail call coverage";
  ignore (check lists);
  let identity = D.Let (D.Lambda (D.Bound D.Z), D.Lambda (D.If (
    D.Apply (D.Bound (D.S D.Z), D.Truth),
    D.Apply (D.Bound (D.S D.Z), D.Bound D.Z), word 0))) in
  ignore (check identity)
