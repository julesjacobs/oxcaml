module D = Hm_declarative
module K = Hmc_closure_ir
module G = Hmc_cfg_ir
module E = Hmc_cfg_extension

type t = Leaf of D.index * G.atom * D.mono * D.typing
  | Binary of D.index * D.index * D.index * t * t
  | Binding of D.index * D.index * D.index * t * t
  | Conditional of D.index * D.index * t * t * t
  | Matching of D.index * D.index * D.index * t * t * t [@@inductive]
let[@def] (entry @ total) (trace : t @ immutable) = match trace with
  | Leaf (i, _, _, _) | Binary (i, _, _, _, _) | Binding (i, _, _, _, _)
  | Conditional (i, _, _, _, _) | Matching (i, _, _, _, _, _) -> i
let[@def] (instruction @ total) (table : G.table @ immutable) (id : D.index @ immutable)
    (op : G.instruction @ immutable) = ghost_ (match G.lookup table id with
  | None -> false | Some block -> block.G.instruction === op)
let[@def] (binary_operation @ total) (source : K.term @ immutable) (next : D.index @ immutable) = match source with
  | K.Apply _ -> G.Call next | K.Cons _ -> G.Cons next
  | K.Primitive (op, _, _) -> G.Primitive (op, next) | _ -> G.Jump next
let[@def] rec (generated @ total) (table : G.table @ immutable) (source : K.term @ immutable)
    (next : D.index @ immutable) (trace : t @ immutable) = ghost_ (match source, trace with
  | _, Leaf (id, atom, ty, d) -> source === G.term atom && instruction table id (G.Load (atom, ty, d, next))
  | (K.Apply (a, b) | K.Cons (a, b) | K.Primitive (_, a, b)), Binary (start, save, finish, left, right) ->
    instruction table start (G.Save_environment (entry left))
    && instruction table save (G.Save_value (entry right)) && instruction table finish (binary_operation source next)
    && generated table a save left && generated table b finish right
  | K.Let (a, b), Binding (start, bind, restore, left, right) ->
    instruction table start (G.Save_environment (entry left)) && instruction table bind (G.Bind (entry right))
    && instruction table restore (G.Restore next) && generated table a bind left && generated table b restore right
  | K.If (a, b, c), Conditional (start, branch, condition, yes, no) ->
    instruction table start (G.Jump (entry condition)) && instruction table branch (G.Branch (entry yes, entry no))
    && generated table a branch condition && generated table b next yes && generated table c next no
  | K.CaseList (s, a, b), Matching (start, branch, restore, scrutinee, empty, full) ->
    instruction table start (G.Jump (entry scrutinee)) && instruction table branch (G.List_branch (entry empty, entry full))
    && instruction table restore (G.Restore next) && generated table s branch scrutinee
    && generated table a next empty && generated table b restore full
  | _ -> false)
let (preserve_instruction @ total) : (larger : G.table) @ immutable -> (smaller : G.table) @ immutable ->
    (id : D.index) @ immutable -> (op : G.instruction) @ immutable ->
    {u : unit | G.extends larger smaller && instruction smaller id op} ->
    {u : unit | instruction larger id op} @ ghost = fun larger smaller id op premise -> ghost_ (
  instruction_def smaller id op; instruction_def larger id op;
  match G.lookup smaller id with None -> () | Some block -> E.lookup larger smaller id block ())
let rec (preserve @ total) : (larger : G.table) @ immutable -> (smaller : G.table) @ immutable ->
    (source : K.term) @ immutable -> (next : D.index) @ immutable -> (trace : t) @ immutable ->
    {u : unit | G.extends larger smaller && generated smaller source next trace} ->
    {u : unit | generated larger source next trace} @ ghost = fun larger smaller source next trace premise -> ghost_ (
  generated_def smaller source next trace; generated_def larger source next trace;
  match source, trace with
  | _, Leaf (id, atom, ty, d) -> preserve_instruction larger smaller id (G.Load (atom, ty, d, next)) ()
  | (K.Apply (a, b) | K.Cons (a, b) | K.Primitive (_, a, b)), Binary (start, save, finish, left, right) ->
    preserve_instruction larger smaller start (G.Save_environment (entry left)) ();
    preserve_instruction larger smaller save (G.Save_value (entry right)) ();
    preserve_instruction larger smaller finish (binary_operation source next) ();
    preserve larger smaller a save left (); preserve larger smaller b finish right ()
  | K.Let (a, b), Binding (start, bind, restore, left, right) ->
    preserve_instruction larger smaller start (G.Save_environment (entry left)) ();
    preserve_instruction larger smaller bind (G.Bind (entry right)) ();
    preserve_instruction larger smaller restore (G.Restore next) ();
    preserve larger smaller a bind left (); preserve larger smaller b restore right ()
  | K.If (a, b, c), Conditional (start, branch, condition, yes, no) ->
    preserve_instruction larger smaller start (G.Jump (entry condition)) ();
    preserve_instruction larger smaller branch (G.Branch (entry yes, entry no)) ();
    preserve larger smaller a branch condition (); preserve larger smaller b next yes (); preserve larger smaller c next no ()
  | K.CaseList (s, a, b), Matching (start, branch, restore, scrutinee, empty, full) ->
    preserve_instruction larger smaller start (G.Jump (entry scrutinee)) ();
    preserve_instruction larger smaller branch (G.List_branch (entry empty, entry full)) ();
    preserve_instruction larger smaller restore (G.Restore next) ();
    preserve larger smaller s branch scrutinee (); preserve larger smaller a next empty (); preserve larger smaller b restore full ()
  | _ -> ())
