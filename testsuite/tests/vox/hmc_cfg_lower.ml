module D = Hm_declarative
module K = Hmc_closure_ir
module M = Hmc_manifest
module G = Hmc_cfg_ir
module E = Hmc_cfg_extension
module O = Hmc_cfg_origin

type result = {table : G.table; trace : O.t}
let (leaf @ total) : (interface : M.table) @ immutable -> (closures : K.table) @ immutable ->
    (initial : G.table) @ immutable -> (locals : D.context) @ immutable -> (temps : G.temporaries) @ immutable ->
    (atom : G.atom) @ immutable -> (ty : D.mono) @ immutable -> (d : D.typing) @ immutable -> (next : D.index) @ immutable ->
    {u : unit | G.valid interface closures initial && K.typed interface closures locals (G.term atom) ty d
      && G.accepts initial next locals temps (Some ty)} ->
    {r : result | G.valid interface closures r.table && G.extends r.table initial
      && G.entry r.table (O.entry r.trace) locals temps && O.generated r.table (G.term atom) next r.trace} @ immutable =
  fun interface closures initial locals temps atom ty d next premise ->
    let block = {G.signature = {G.locals; temporaries = temps; accumulator = None}; instruction = G.Load (atom, ty, d, next)} in
    ghost_ (G.block_valid_def interface closures initial block);
    let emitted = E.emit interface closures initial block () in
    let trace = O.Leaf (emitted.E.label, atom, ty, d) in
    ghost_ (O.entry_def trace; G.entry_def emitted.E.table emitted.E.label locals temps;
      O.generated_def emitted.E.table (G.term atom) next trace; O.instruction_def emitted.E.table emitted.E.label block.G.instruction);
    {table = emitted.E.table; trace}

let rec (lower @ total) : (interface : M.table) @ immutable -> (closures : K.table) @ immutable ->
    (initial : G.table) @ immutable -> (locals : D.context) @ immutable -> (temps : G.temporaries) @ immutable ->
    (source : K.term) @ immutable -> (ty : D.mono) @ immutable -> (d : D.typing) @ immutable -> (next : D.index) @ immutable ->
    {u : unit | G.valid interface closures initial && K.typed interface closures locals source ty d
      && G.accepts initial next locals temps (Some ty)} ->
    {r : result | G.valid interface closures r.table && G.extends r.table initial
      && G.entry r.table (O.entry r.trace) locals temps && O.generated r.table source next r.trace} @ immutable =
  fun interface closures initial locals temps source ty d next premise ->
    ghost_ (K.typed_def interface closures locals source ty d);
    match source, d with
    | K.Local i, _ ->
      ghost_ (G.term_def (G.Local i)); leaf interface closures initial locals temps (G.Local i) ty d next ()
    | K.Global i, _ ->
      ghost_ (G.term_def (G.Global i)); leaf interface closures initial locals temps (G.Global i) ty d next ()
    | K.Closure i, _ ->
      ghost_ (G.term_def (G.Closure i)); leaf interface closures initial locals temps (G.Closure i) ty d next ()
    | K.Truth, _ ->
      ghost_ (G.term_def (G.Truth)); leaf interface closures initial locals temps (G.Truth) ty d next ()
    | K.False, _ ->
      ghost_ (G.term_def (G.False)); leaf interface closures initial locals temps (G.False) ty d next ()
    | K.Word w, _ ->
      ghost_ (G.term_def (G.Word w)); leaf interface closures initial locals temps (G.Word w) ty d next ()
    | K.Nil, _ ->
      ghost_ (G.term_def (G.Nil)); leaf interface closures initial locals temps (G.Nil) ty d next ()
    | (K.Apply (a, b) | K.Cons (a, b) | K.Primitive (_, a, b)), _ ->
      let left_ty, right_ty, da, db = match source, d with
      | K.Apply _, D.Application (arg, da, db) -> D.Function (arg, ty), arg, da, db
      | K.Cons _, D.List_cons (element, da, db) -> element, ty, da, db
      | K.Primitive _, D.Word_primitive (da, db) -> D.Word64, D.Word64, da, db
      | _ -> unreachable_ () in
      let saved = G.Environment (locals, temps) in let values = G.Value (locals, left_ty, temps) in
      let finish_block = {G.signature = {G.locals; temporaries = values; accumulator = Some right_ty};
        instruction = O.binary_operation source next} in
      ghost_ (O.binary_operation_def source next; G.block_valid_def interface closures initial finish_block);
      let finish = E.emit interface closures initial finish_block () in
      ghost_ (G.accepts_def finish.E.table finish.E.label locals values (Some right_ty));
      let right = lower interface closures finish.E.table locals values b right_ty db finish.E.label () in
      let save_block = {G.signature = {G.locals; temporaries = saved; accumulator = Some left_ty};
        instruction = G.Save_value (O.entry right.trace)} in
      ghost_ (E.entry_accepts right.table (O.entry right.trace) locals values None ();
        G.block_valid_def interface closures right.table save_block);
      let save = E.emit interface closures right.table save_block () in
      ghost_ (G.accepts_def save.E.table save.E.label locals saved (Some left_ty));
      let left = lower interface closures save.E.table locals saved a left_ty da save.E.label () in
      let start_block = {G.signature = {G.locals; temporaries = temps; accumulator = None};
        instruction = G.Save_environment (O.entry left.trace)} in
      ghost_ (E.entry_accepts left.table (O.entry left.trace) locals saved None ();
        G.block_valid_def interface closures left.table start_block);
      let start = E.emit interface closures left.table start_block () in
      let trace = O.Binary (start.E.label, save.E.label, finish.E.label, left.trace, right.trace) in
      ghost_ (
        E.transitive start.E.table left.table save.E.table (); E.transitive start.E.table save.E.table right.table ();
        E.transitive start.E.table right.table finish.E.table (); E.transitive start.E.table finish.E.table initial ();
        O.preserve start.E.table left.table a save.E.label left.trace ();
        O.preserve start.E.table right.table b finish.E.label right.trace ();
        O.instruction_def finish.E.table finish.E.label finish_block.G.instruction;
        O.instruction_def save.E.table save.E.label save_block.G.instruction;
        O.preserve_instruction start.E.table finish.E.table finish.E.label finish_block.G.instruction ();
        O.preserve_instruction start.E.table save.E.table save.E.label save_block.G.instruction ();
        O.instruction_def start.E.table start.E.label start_block.G.instruction;
        O.entry_def trace; G.entry_def start.E.table start.E.label locals temps;
        O.generated_def start.E.table source next trace);
      {table = start.E.table; trace}
    | K.Let (a, b), D.Let_binding (D.Forall (D.Z, arg), da, db) ->
      let saved = G.Environment (locals, temps) in
      let bound = D.Binding (D.Forall (D.Z, arg), locals) in
      let restore_block = {G.signature = {G.locals = bound; temporaries = saved; accumulator = Some ty}; instruction = G.Restore next} in
      ghost_ (G.block_valid_def interface closures initial restore_block);
      let restore = E.emit interface closures initial restore_block () in
      ghost_ (G.accepts_def restore.E.table restore.E.label bound saved (Some ty));
      let right = lower interface closures restore.E.table bound saved b ty db restore.E.label () in
      let bind_block = {G.signature = {G.locals; temporaries = saved; accumulator = Some arg}; instruction = G.Bind (O.entry right.trace)} in
      ghost_ (E.entry_accepts right.table (O.entry right.trace) bound saved None ();
        G.block_valid_def interface closures right.table bind_block);
      let bind = E.emit interface closures right.table bind_block () in
      ghost_ (G.accepts_def bind.E.table bind.E.label locals saved (Some arg));
      let left = lower interface closures bind.E.table locals saved a arg da bind.E.label () in
      let start_block = {G.signature = {G.locals; temporaries = temps; accumulator = None}; instruction = G.Save_environment (O.entry left.trace)} in
      ghost_ (E.entry_accepts left.table (O.entry left.trace) locals saved None ();
        G.block_valid_def interface closures left.table start_block);
      let start = E.emit interface closures left.table start_block () in
      let trace = O.Binding (start.E.label, bind.E.label, restore.E.label, left.trace, right.trace) in
      ghost_ (
        E.transitive start.E.table left.table bind.E.table (); E.transitive start.E.table bind.E.table right.table ();
        E.transitive start.E.table right.table restore.E.table (); E.transitive start.E.table restore.E.table initial ();
        O.preserve start.E.table left.table a bind.E.label left.trace (); O.preserve start.E.table right.table b restore.E.label right.trace ();
        O.instruction_def restore.E.table restore.E.label restore_block.G.instruction;
        O.instruction_def bind.E.table bind.E.label bind_block.G.instruction;
        O.preserve_instruction start.E.table restore.E.table restore.E.label restore_block.G.instruction ();
        O.preserve_instruction start.E.table bind.E.table bind.E.label bind_block.G.instruction ();
        O.instruction_def start.E.table start.E.label start_block.G.instruction;
        O.entry_def trace; G.entry_def start.E.table start.E.label locals temps; O.generated_def start.E.table source next trace);
      {table = start.E.table; trace}
    | K.If (a, b, c), D.Conditional (da, db, dc) ->
      let no = lower interface closures initial locals temps c ty dc next () in
      ghost_ (E.accepts no.table initial next locals temps (Some ty) ());
      let yes = lower interface closures no.table locals temps b ty db next () in
      let branch_block = {G.signature = {G.locals; temporaries = temps; accumulator = Some D.Boolean};
        instruction = G.Branch (O.entry yes.trace, O.entry no.trace)} in
      ghost_ (E.entry yes.table no.table (O.entry no.trace) locals temps ();
        E.entry_accepts yes.table (O.entry no.trace) locals temps None ();
        E.entry_accepts yes.table (O.entry yes.trace) locals temps None ();
        G.block_valid_def interface closures yes.table branch_block);
      let branch = E.emit interface closures yes.table branch_block () in
      ghost_ (G.accepts_def branch.E.table branch.E.label locals temps (Some D.Boolean));
      let condition = lower interface closures branch.E.table locals temps a D.Boolean da branch.E.label () in
      let start_block = {G.signature = {G.locals; temporaries = temps; accumulator = None}; instruction = G.Jump (O.entry condition.trace)} in
      ghost_ (E.entry_accepts condition.table (O.entry condition.trace) locals temps None ();
        G.block_valid_def interface closures condition.table start_block);
      let start = E.emit interface closures condition.table start_block () in
      let trace = O.Conditional (start.E.label, branch.E.label, condition.trace, yes.trace, no.trace) in
      ghost_ (E.transitive start.E.table condition.table branch.E.table (); E.transitive start.E.table branch.E.table yes.table ();
        E.transitive start.E.table yes.table no.table (); E.transitive start.E.table no.table initial ();
        O.preserve start.E.table condition.table a branch.E.label condition.trace ();
        O.preserve start.E.table yes.table b next yes.trace (); O.preserve start.E.table no.table c next no.trace ();
        O.instruction_def branch.E.table branch.E.label branch_block.G.instruction;
        O.preserve_instruction start.E.table branch.E.table branch.E.label branch_block.G.instruction ();
        O.instruction_def start.E.table start.E.label start_block.G.instruction;
        O.entry_def trace; G.entry_def start.E.table start.E.label locals temps; O.generated_def start.E.table source next trace);
      {table = start.E.table; trace}
    | K.CaseList (scr, a, b), D.List_case (element, ds, da, db) ->
      let saved = G.Environment (locals, temps) in
      let bound = D.Binding (D.Forall (D.Z, element), D.Binding (D.Forall (D.Z, D.List_type element), locals)) in
      let restore_block = {G.signature = {G.locals = bound; temporaries = saved; accumulator = Some ty}; instruction = G.Restore next} in
      ghost_ (G.block_valid_def interface closures initial restore_block);
      let restore = E.emit interface closures initial restore_block () in
      ghost_ (G.accepts_def restore.E.table restore.E.label bound saved (Some ty));
      let full = lower interface closures restore.E.table bound saved b ty db restore.E.label () in
      ghost_ (E.transitive full.table restore.E.table initial (); E.accepts full.table initial next locals temps (Some ty) ());
      let empty = lower interface closures full.table locals temps a ty da next () in
      let branch_block = {G.signature = {G.locals; temporaries = temps; accumulator = Some (D.List_type element)};
        instruction = G.List_branch (O.entry empty.trace, O.entry full.trace)} in
      ghost_ (E.entry empty.table full.table (O.entry full.trace) bound saved ();
        E.entry_accepts empty.table (O.entry full.trace) bound saved None ();
        E.entry_accepts empty.table (O.entry empty.trace) locals temps None ();
        G.block_valid_def interface closures empty.table branch_block);
      let branch = E.emit interface closures empty.table branch_block () in
      ghost_ (G.accepts_def branch.E.table branch.E.label locals temps (Some (D.List_type element)));
      let scrutinee = lower interface closures branch.E.table locals temps scr (D.List_type element) ds branch.E.label () in
      let start_block = {G.signature = {G.locals; temporaries = temps; accumulator = None}; instruction = G.Jump (O.entry scrutinee.trace)} in
      ghost_ (E.entry_accepts scrutinee.table (O.entry scrutinee.trace) locals temps None ();
        G.block_valid_def interface closures scrutinee.table start_block);
      let start = E.emit interface closures scrutinee.table start_block () in
      let trace = O.Matching (start.E.label, branch.E.label, restore.E.label, scrutinee.trace, empty.trace, full.trace) in
      ghost_ (E.transitive start.E.table scrutinee.table branch.E.table (); E.transitive start.E.table branch.E.table empty.table ();
        E.transitive start.E.table empty.table full.table (); E.transitive start.E.table full.table restore.E.table ();
        E.transitive start.E.table restore.E.table initial ();
        O.preserve start.E.table scrutinee.table scr branch.E.label scrutinee.trace ();
        O.preserve start.E.table empty.table a next empty.trace (); O.preserve start.E.table full.table b restore.E.label full.trace ();
        O.instruction_def branch.E.table branch.E.label branch_block.G.instruction;
        O.instruction_def restore.E.table restore.E.label restore_block.G.instruction;
        O.preserve_instruction start.E.table branch.E.table branch.E.label branch_block.G.instruction ();
        O.preserve_instruction start.E.table restore.E.table restore.E.label restore_block.G.instruction ();
        O.instruction_def start.E.table start.E.label start_block.G.instruction;
        O.entry_def trace; G.entry_def start.E.table start.E.label locals temps; O.generated_def start.E.table source next trace);
      {table = start.E.table; trace}
    | _ -> unreachable_ ()
