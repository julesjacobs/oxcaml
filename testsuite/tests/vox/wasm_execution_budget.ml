module C = Wasm_code
module D = Hm_declarative
module Add = Wasm_control_compose
let[@def] rec (le @ total) (a : C.count @ immutable) (b : C.count @ immutable) =
  match a, b with C.Zero, _ -> true | C.Succ _, C.Zero -> false | C.Succ a, C.Succ b -> le a b
let[@def] rec (of_index @ total) (n : D.index @ immutable) = match n with D.Z -> C.Zero | D.S n -> C.Succ (of_index n)
let[@def] rec (to_index @ total) (n : C.count @ immutable) = match n with C.Zero -> D.Z | C.Succ n -> D.S (to_index n)
let rec (inverse @ total) : (n : C.count) @ immutable -> {u : unit | of_index (to_index n) === n} @ ghost =
  fun n -> ghost_ (to_index_def n; of_index_def (to_index n); match n with C.Zero -> () | C.Succ n -> inverse n)
let rec (weaken @ total) : (a : C.count) @ immutable -> (b : C.count) @ immutable ->
    {u : unit | le a b} -> {u : unit | le a (C.Succ b)} @ ghost =
  fun a b premise -> ghost_ (le_def a b; le_def a (C.Succ b);
    match a, b with C.Succ a, C.Succ b -> weaken a b () | _ -> ())
let rec (add_left @ total) : (first : C.count) @ immutable -> (second : C.count) @ immutable -> (bound : C.count) @ immutable ->
    {u : unit | le bound second} -> {u : unit | le bound (Add.add first second)} @ ghost =
  fun first second bound premise -> ghost_ (Add.add_def first second;
    match first with C.Zero -> () | C.Succ rest -> add_left rest second bound (); weaken bound (Add.add rest second) ())
let (positive_add @ total) : (first : C.count) @ immutable -> (second : C.count) @ immutable -> (bound : C.count) @ immutable ->
    {u : unit | not (first === C.Zero) && le bound second} -> {u : unit | le (C.Succ bound) (Add.add first second)} @ ghost =
  fun first second bound premise -> ghost_ (Add.add_def first second; le_def (C.Succ bound) (Add.add first second);
    match first with C.Zero -> () | C.Succ rest -> add_left rest second bound ())
