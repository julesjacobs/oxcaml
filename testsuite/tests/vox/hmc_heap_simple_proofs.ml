module D = Hm_declarative
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module B = Hmc_heap_simple
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module C = Hmc_cfg_program
module P = Hmc_closure_program

let rec (lookup @ total) : (heap : M.heap) @ immutable -> (env : M.cells) @ immutable -> (abstract : R.V.value) @ immutable ->
    (index : D.index) @ immutable -> {u : unit | M.decode_environment (M.view heap) env === Some abstract} ->
    {u : unit | match B.lookup env index with None -> R.lookup abstract index === None
      | Some value -> M.decode heap value === R.lookup abstract index && not (M.decode heap value === None)} @ ghost = fun heap env abstract index premise -> ghost_ (
  M.decode_environment_def (M.view heap) env; B.lookup_def env index; R.lookup_def abstract index;
  match env, index with
  | M.Cell (head, _), D.Z -> M.decode_def heap head
  | M.Cell (_, rest), D.S i -> (match M.decode_environment (M.view heap) rest with None -> () | Some next -> lookup heap rest next i ())
  | _ -> ())
let (load @ total) : (heap : M.heap) @ immutable -> (globals : P.globals) @ immutable ->
    (env : M.cells) @ immutable -> (abstract : R.V.value) @ immutable -> (atom : G.atom) @ immutable ->
    {u : unit | M.decode_environment (M.view heap) env === Some abstract
      && (match atom with G.Global _ | G.Closure _ -> false | _ -> true)} ->
    {u : unit | match B.load env atom with None -> S.load globals abstract atom === None
      | Some value -> M.decode heap value === S.load globals abstract atom && not (M.decode heap value === None)} @ ghost = fun heap globals env abstract atom premise -> ghost_ (
  B.load_def env atom; S.load_def globals abstract atom;
  match atom with
  | G.Local index -> lookup heap env abstract index ()
  | _ -> (match B.load env atom with None -> () | Some value -> M.decode_def heap value; M.decode_value_def (M.view heap) value))
let (primitive @ total) : (heap : M.heap) @ immutable -> (op : D.word_operation) @ immutable ->
    (left : Hmc_word64.t) @ immutable -> (right : Hmc_word64.t) @ immutable ->
    {u : unit | M.decode heap (B.primitive op left right) === Some (R.primitive op left right)} @ ghost = fun heap op left right -> ghost_ (
  B.primitive_def op left right; R.primitive_def op left right;
  M.decode_def heap (B.primitive op left right); M.decode_value_def (M.view heap) (B.primitive op left right))
let (step @ total) : (program : I.program) @ immutable -> (heap : M.heap) @ immutable ->
    (a : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (abstract : S.state) @ immutable ->
    (op : G.instruction) @ immutable ->
    {u : unit | Q.decode heap (Q.Running (a, frames)) === Some abstract && B.supports op
      && I.lookup program.I.code a.F.pc === Some (I.Keep op)} ->
    {u : unit | Q.decode heap (B.step op (Q.Running (a, frames))) === Some (U.step program abstract)} @ ghost =
  fun program heap a frames abstract op premise -> ghost_ (
    Q.decode_def heap (Q.Running (a, frames)); F.decode_def heap a;
    B.supports_def op; B.step_def op (Q.Running (a, frames)); U.step_def program abstract;
    F.decode_temporaries_def heap a.F.temporaries;
    M.decode_def heap a.F.accumulator; M.decode_value_def (M.view heap) a.F.accumulator;
    (match op with
    | G.Load (atom, _, _, _) ->
      (match M.decode_environment (M.view heap) a.F.env with None -> () | Some env ->
        load heap program.I.origin.C.origin.P.globals a.F.env env atom ())
    | G.Bind _ -> (match a.F.temporaries with F.Environment (env, _) ->
      M.decode_environment_def (M.view heap) (M.Cell (a.F.accumulator, env)) | _ -> ())
    | G.Primitive (op, _) -> (match a.F.temporaries with
      | F.Value (head, _, _) ->
        M.decode_def heap head; M.decode_value_def (M.view heap) head;
        (match head, a.F.accumulator with Hmc_tagged_cell.Word left, Hmc_tagged_cell.Word right -> primitive heap op left right | _ -> ())
      | _ -> ())
    | G.Return -> Q.decode_frames_def heap frames;
      (match frames with Q.Halt -> () | Q.Frame (saved, _) -> F.decode_def heap saved)
    | _ -> ());
    let next = B.step op (Q.Running (a, frames)) in
    Q.decode_def heap next;
    match next with
    | Q.Running (b, _) -> F.decode_def heap b; F.decode_temporaries_def heap b.F.temporaries
    | _ -> ())
