module D = Hm_declarative
module W = Hmc_word64
module C = Hmc_tagged_cell
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module G = Hmc_cfg_ir

let[@def] rec (lookup @ total) (env : M.cells @ immutable) (index : D.index @ immutable) = match env, index with
  | M.Empty, _ -> None | M.Cell (head, _), D.Z -> Some head | M.Cell (_, rest), D.S i -> lookup rest i
let[@def] (primitive @ total) (op : D.word_operation @ immutable) (left : W.t @ immutable) (right : W.t @ immutable) =
  match op with
  | D.Add -> C.Word (W.add left right) | D.Subtract -> C.Word (W.subtract left right)
  | D.Equal_word -> C.Boolean (W.equal left right) | D.Unsigned_less -> C.Boolean (W.unsigned_less left right)
let[@def] (load @ total) (env : M.cells @ immutable) (atom : G.atom @ immutable) = match atom with
  | G.Local index -> lookup env index
  | G.Truth -> Some (C.Boolean true) | G.False -> Some (C.Boolean false)
  | G.Word word -> Some (C.Word word) | G.Nil -> Some C.Nil
  | G.Global _ | G.Closure _ -> None
let[@def] (supports @ total) (op : G.instruction @ immutable) = match op with
  | G.Load (G.Global _, _, _, _) | G.Load (G.Closure _, _, _, _)
  | G.Cons _ | G.Call _ | G.List_branch _ -> false | _ -> true
let[@def] (step @ total) (op : G.instruction @ immutable) (state : Q.state @ immutable) = match state with
  | Q.Done _ | Q.Stuck -> state
  | Q.Running (a, frames) -> match op with
    | G.Load (atom, _, _, next) -> (match load a.F.env atom with None -> Q.Stuck | Some accumulator -> Q.Running ({a with F.pc = next; accumulator}, frames))
    | G.Jump next -> Q.Running ({a with F.pc = next}, frames)
    | G.Save_environment next -> Q.Running ({a with F.pc = next; temporaries = F.Environment (a.F.env, a.F.temporaries)}, frames)
    | G.Save_value next -> (match a.F.temporaries with
      | F.Environment (env, rest) -> Q.Running ({a with F.pc = next; env; temporaries = F.Value (a.F.accumulator, env, rest)}, frames)
      | _ -> Q.Stuck)
    | G.Bind next -> (match a.F.temporaries with
      | F.Environment (env, _) -> Q.Running ({a with F.pc = next; env = M.Cell (a.F.accumulator, env)}, frames)
      | _ -> Q.Stuck)
    | G.Restore next -> (match a.F.temporaries with
      | F.Environment (env, rest) -> Q.Running ({a with F.pc = next; env; temporaries = rest}, frames)
      | _ -> Q.Stuck)
    | G.Primitive (op, next) -> (match a.F.temporaries, a.F.accumulator with
      | F.Value (C.Word left, env, rest), C.Word right ->
        Q.Running ({a with F.pc = next; env; temporaries = rest; accumulator = primitive op left right}, frames)
      | _ -> Q.Stuck)
    | G.Branch (yes, no) -> (match a.F.accumulator with
      | C.Boolean b -> Q.Running ({a with F.pc = (if b then yes else no)}, frames) | _ -> Q.Stuck)
    | G.Return -> (match a.F.temporaries, frames with
      | F.Empty, Q.Halt -> Q.Done a.F.accumulator
      | F.Empty, Q.Frame (saved, rest) -> Q.Running ({saved with F.accumulator = a.F.accumulator}, rest)
      | _ -> Q.Stuck)
    | _ -> Q.Stuck
