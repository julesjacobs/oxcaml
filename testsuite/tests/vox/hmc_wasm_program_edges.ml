module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module K = Hmc_closure_program
module M = Hmc_monomorphic
let (keep @ total) : (program : I.program) @ immutable -> (id : D.index) @ immutable ->
    (block : G.block) @ immutable -> (instruction : G.instruction) @ immutable ->
    {u : unit | G.lookup program.I.origin.C.blocks id === Some block
      && I.lookup program.I.code id === Some (I.Keep instruction)} ->
    {u : unit | block.G.instruction === instruction
      && G.block_valid (M.manifest program.I.origin.C.origin.K.origin.M.definitions)
        program.I.origin.C.origin.K.table program.I.origin.C.blocks block} @ ghost =
  fun program id block instruction premise -> ghost_ (
    I.valid_def program; I.lookup_related program.I.origin.C.blocks program.I.code program.I.sites id ();
    I.select_def program.I.sites id block.G.instruction; C.valid_def program.I.origin;
    Hmc_cfg_extension.lookup_valid (M.manifest program.I.origin.C.origin.K.origin.M.definitions)
      program.I.origin.C.origin.K.table program.I.origin.C.blocks id block ())
let (next @ total) : (blocks : G.table) @ immutable -> (id : D.index) @ immutable ->
    (locals : D.context) @ immutable -> (temporaries : G.temporaries) @ immutable -> (accumulator : D.mono option) @ immutable ->
    {u : unit | G.accepts blocks id locals temporaries accumulator} ->
    {block : G.block | G.lookup blocks id === Some block && block.G.signature.G.locals === locals
      && block.G.signature.G.temporaries === temporaries
      && (block.G.signature.G.accumulator === None || block.G.signature.G.accumulator === accumulator)} @ immutable =
  fun blocks id locals temporaries accumulator premise ->
    ghost_ (G.accepts_def blocks id locals temporaries accumulator);
    match G.lookup blocks id with Some block -> block | None -> unreachable_ ()
