module D = Hm_declarative
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module A = Hmc_heap_allocate
module P = Hmc_heap_preservation
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module I = Hmc_tail_ir
module O = Hmc_closure_program

let (allocate @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (limit : Hmc_word64.limb) ->
    (object_ : M.object_) @ immutable ->
    {u : unit | M.valid table heap && M.used heap <= limit && M.object_valid table (M.view heap) object_} ->
    {u : unit | A.correct table heap limit object_ (X.allocate heap limit object_)} @ ghost = fun table heap limit object_ premise -> ghost_ (
  X.allocate_def heap limit object_;
  match Hmc_heap_extent.reserve (M.slots object_) (M.used heap) limit with
  | None -> A.correct_def table heap limit object_ A.Exhausted
  | Some stop ->
    let next = M.Allocate ({M.address = M.used heap; stop; object_}, heap) in
    let reference = M.reference object_ (M.used heap) in
    M.valid_def table next; M.used_def next; P.extends_def heap heap; P.extends_def next heap;
    M.decode_def next reference; M.reference_def object_ (M.used heap); M.view_def next;
    M.decode_value_def (M.view next) reference; M.lookup_def (M.view next) (M.used heap);
    M.object_valid_def table (M.view heap) object_; M.decode_object_def (M.view heap) object_;
    A.correct_def table heap limit object_ (A.Allocated {A.heap = next; reference}))
let[@def] (abstract_invoke @ total) (program : I.program @ immutable) (closure : R.V.value @ immutable)
    (argument : R.V.value @ immutable) = match closure with
  | R.V.Closure (id, captures) ->
    (match K.lookup program.I.origin.C.origin.O.table id, C.lookup program.I.origin.C.functions id with
    | Some entry, Some code ->
      let captures = if entry.K.recursive then R.V.Bind (closure, captures) else captures in
      Some {S.pc = code.C.start; env = R.V.Bind (argument, captures); accumulator = R.V.Nil; temporaries = S.Empty; current = closure}
    | _ -> None)
  | _ -> None
let (invoke @ total) : (program : I.program) @ immutable -> (heap : M.heap) @ immutable ->
    (closure : V.value) @ immutable -> (argument : V.value) @ immutable ->
    (source_closure : R.V.value) @ immutable -> (source_argument : R.V.value) @ immutable ->
    {u : unit | M.valid program.I.origin.C.origin.O.table heap && M.decode heap closure === Some source_closure
      && M.decode heap argument === Some source_argument} ->
    {u : unit | match X.invoke program heap closure argument with
      | None -> abstract_invoke program source_closure source_argument === None
      | Some entered -> F.decode heap entered === abstract_invoke program source_closure source_argument
        && not (F.decode heap entered === None)} @ ghost = fun program heap closure argument source_closure source_argument premise -> ghost_ (
  X.invoke_def program heap closure argument; abstract_invoke_def program source_closure source_argument;
  M.decode_def heap closure; M.decode_value_def (M.view heap) closure;
  match closure with
  | V.Closure_pointer address ->
    let object_ = P.fetch program.I.origin.C.origin.O.table heap address source_closure () in
    M.decode_object_def (M.view heap) object_;
    (match X.invoke program heap closure argument with None -> () | Some entered ->
      F.decode_def heap entered; F.decode_temporaries_def heap F.Empty;
      M.decode_def heap V.Nil; M.decode_value_def (M.view heap) V.Nil;
      M.decode_environment_def (M.view heap) entered.F.env;
      match object_ with M.Closure (_, captures) ->
        M.decode_environment_def (M.view heap) (M.Cell (closure, captures)); M.decode_def heap argument
      | _ -> ())
  | _ -> ())
