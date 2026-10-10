module D = Hm_declarative
module K = Hmc_closure_ir
module M = Hmc_manifest

type temporaries = Empty_temporaries | Environment of D.context * temporaries
  | Value of D.context * D.mono * temporaries [@@inductive]
type signature = {locals : D.context; temporaries : temporaries; accumulator : D.mono option}
type atom = Local of D.index | Global of D.index | Closure of D.index
  | Truth | False | Word of Hmc_word64.t | Nil [@@inductive]
let[@def] (term @ total) (atom : atom @ immutable) = match atom with
  | Local i -> K.Local i | Global i -> K.Global i | Closure i -> K.Closure i
  | Truth -> K.Truth | False -> K.False | Word w -> K.Word w | Nil -> K.Nil

type instruction = Load of atom * D.mono * D.typing * D.index | Jump of D.index
  | Save_environment of D.index | Save_value of D.index | Bind of D.index | Restore of D.index
  | Primitive of D.word_operation * D.index | Cons of D.index | Call of D.index
  | Branch of D.index * D.index | List_branch of D.index * D.index | Return [@@inductive]
type block = {signature : signature; instruction : instruction}
type table = Empty | Add of block * table [@@inductive]
let[@def] rec (size @ total) (table : table @ immutable) = match table with
  | Empty -> D.Z | Add (_, rest) -> D.S (size rest)
let[@def] rec (lookup @ total) (table : table @ immutable) (id : D.index @ immutable) = match table with
  | Empty -> None | Add (block, rest) ->
    if Hm_elaboration_check.index_equal id (size rest) then Some block else lookup rest id
let[@def] rec (extends @ total) (larger : table @ immutable) (smaller : table @ immutable) =
  ghost_ (larger === smaller || match larger with Empty -> false | Add (_, rest) -> extends rest smaller)
let[@def] (accepts @ total) (table : table @ immutable) (id : D.index @ immutable)
    (locals : D.context @ immutable) (temporaries : temporaries @ immutable) (accumulator : D.mono option @ immutable) = ghost_ (
  match lookup table id with None -> false | Some block ->
    block.signature.locals === locals && block.signature.temporaries === temporaries
    && (block.signature.accumulator === None || block.signature.accumulator === accumulator))
let[@def] (entry @ total) (table : table @ immutable) (id : D.index @ immutable)
    (locals : D.context @ immutable) (temporaries : temporaries @ immutable) = ghost_ (
  match lookup table id with None -> false | Some block ->
    block.signature === {locals; temporaries; accumulator = None})
let[@def] (block_valid @ total) (interface : M.table @ immutable) (closures : K.table @ immutable)
    (table : table @ immutable) (block : block @ immutable) = ghost_ (
  let s = block.signature in match block.instruction with
  | Load (atom, ty, d, next) -> s.accumulator === None
    && K.typed interface closures s.locals (term atom) ty d
    && accepts table next s.locals s.temporaries (Some ty)
  | Jump next -> s.accumulator === None && accepts table next s.locals s.temporaries None
  | Save_environment next -> s.accumulator === None
    && accepts table next s.locals (Environment (s.locals, s.temporaries)) None
  | Save_value next -> (match s.accumulator, s.temporaries with
    | Some ty, Environment (g, rest) -> accepts table next g (Value (g, ty, rest)) None | _ -> false)
  | Bind next -> (match s.accumulator, s.temporaries with
    | Some ty, Environment (g, rest) -> accepts table next (D.Binding (D.Forall (D.Z, ty), g)) s.temporaries None
    | _ -> false)
  | Restore next -> (match s.accumulator, s.temporaries with
    | Some ty, Environment (g, rest) -> accepts table next g rest (Some ty) | _ -> false)
  | Primitive (op, next) -> (match s.accumulator, s.temporaries with
    | Some D.Word64, Value (g, D.Word64, rest) -> accepts table next g rest (Some (D.operation_type op)) | _ -> false)
  | Cons next -> (match s.accumulator, s.temporaries with
    | Some (D.List_type a), Value (g, b, rest) -> a === b && accepts table next g rest s.accumulator | _ -> false)
  | Call next -> (match s.accumulator, s.temporaries with
    | Some arg, Value (g, D.Function (a, b), rest) -> arg === a && accepts table next g rest (Some b) | _ -> false)
  | Branch (yes, no) -> s.accumulator === Some D.Boolean
    && accepts table yes s.locals s.temporaries None && accepts table no s.locals s.temporaries None
  | List_branch (empty, full) -> (match s.accumulator with Some (D.List_type a) ->
    accepts table empty s.locals s.temporaries None
    && accepts table full (D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, D.List_type a), s.locals)))
      (Environment (s.locals, s.temporaries)) None | _ -> false)
  | Return -> s.temporaries === Empty_temporaries && not (s.accumulator === None))
let[@def] rec (valid @ total) (interface : M.table @ immutable) (closures : K.table @ immutable)
    (table : table @ immutable) = ghost_ (match table with
  | Empty -> true | Add (block, rest) -> valid interface closures rest && block_valid interface closures rest block)
