module D = Hm_declarative
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module G = Hmc_cfg_ir
module O = Hmc_cfg_origin

type exit = Return of D.index | Restore of D.index * exit [@@inductive]
let[@def] (entry @ total) (exit : exit @ immutable) = match exit with
  | Return label | Restore (label, _) -> label
let[@def] rec (exit_valid @ total) (blocks : G.table @ immutable) (exit : exit @ immutable) = ghost_ (match exit with
  | Return label -> O.instruction blocks label G.Return
  | Restore (label, rest) -> O.instruction blocks label (G.Restore (entry rest)) && exit_valid blocks rest)
type sites = Empty | Site of D.index * exit * sites [@@inductive]
let[@def] rec (valid @ total) (blocks : G.table @ immutable) (sites : sites @ immutable) = ghost_ (match sites with
  | Empty -> true
  | Site (label, exit, rest) -> O.instruction blocks label (G.Call (entry exit)) && exit_valid blocks exit && valid blocks rest)
let rec (append @ total) : (blocks : G.table) @ immutable -> (left : sites) @ immutable -> (right : sites) @ immutable ->
    {u : unit | valid blocks left && valid blocks right} -> {out : sites | valid blocks out} @ immutable =
  fun blocks left right premise ->
    ghost_ (valid_def blocks left);
    match left with Empty -> right | Site (label, exit, rest) ->
      let tail = append blocks rest right () in
      let out = Site (label, exit, tail) in ghost_ (valid_def blocks out); out

let rec (collect @ total) : (blocks : G.table) @ immutable -> (source : K.term) @ immutable ->
    (trace : O.t) @ immutable -> (self : D.index) @ immutable -> (exit : exit) @ immutable ->
    {u : unit | O.generated blocks source (entry exit) trace && exit_valid blocks exit} ->
    {out : sites | valid blocks out} @ immutable = fun blocks source trace self exit premise ->
  ghost_ (O.generated_def blocks source (entry exit) trace; valid_def blocks Empty);
  match source, trace with
  | K.Apply (K.Local id, _), O.Binary (_, _, call, _, _) ->
    if Hm_elaboration_check.index_equal id self then
      let out = Site (call, exit, Empty) in
      ghost_ (O.binary_operation_def source (entry exit); valid_def blocks out); out
    else Empty
  | K.Let (_, body), O.Binding (_, _, restore, _, body_trace) ->
    let next = Restore (restore, exit) in
    ghost_ (entry_def next; exit_valid_def blocks next);
    collect blocks body body_trace (D.S self) next ()
  | K.If (_, yes, no), O.Conditional (_, _, _, yes_trace, no_trace) ->
    let left = collect blocks yes yes_trace self exit () in
    let right = collect blocks no no_trace self exit () in
    append blocks left right ()
  | K.CaseList (_, empty, full), O.Matching (_, _, restore, _, empty_trace, full_trace) ->
    let left = collect blocks empty empty_trace self exit () in
    let next = Restore (restore, exit) in
    ghost_ (entry_def next; exit_valid_def blocks next);
    let right = collect blocks full full_trace (D.S (D.S self)) next () in
    append blocks left right ()
  | _ -> Empty

let rec (functions @ total) : (blocks : G.table) @ immutable -> (source : K.table) @ immutable ->
    (compiled : C.functions) @ immutable -> {u : unit | C.mapped blocks source compiled} ->
    {out : sites | valid blocks out} @ immutable = fun blocks source compiled premise ->
  ghost_ (C.mapped_def blocks source compiled; valid_def blocks Empty);
  match source, compiled with
  | K.Empty, C.No_functions -> Empty
  | K.Add (fn, rest), C.Function (code, tail) ->
    let earlier = functions blocks rest tail () in
    if fn.K.recursive then
      let exit = Return code.C.return_label in
      ghost_ (C.function_valid_def blocks fn code; C.return_block_def fn;
        entry_def exit; exit_valid_def blocks exit; O.instruction_def blocks code.C.return_label G.Return);
      let selected = collect blocks fn.K.body code.C.trace (D.S D.Z) exit () in
      append blocks selected earlier ()
    else earlier
  | _ -> unreachable_ ()
let (program @ total) : (p : C.program) @ immutable -> {out : sites | valid p.C.blocks out} @ immutable = fun p ->
  ghost_ (C.valid_def p); functions p.C.blocks p.C.origin.Hmc_closure_program.table p.C.functions ()
