module D = Hm_declarative
module G = Hmc_cfg_ir
module F = Hmc_frame_codec
module R = Hmc_closure_semantics

let[@def] rec (le @ total) (a : D.index @ immutable) (b : D.index @ immutable) = match a, b with
  | D.Z, _ -> true | D.S _, D.Z -> false | D.S a, D.S b -> le a b
let rec (reflexive @ total) : (a : D.index) @ immutable -> {u : unit | le a a} @ ghost = fun a -> ghost_ (
  le_def a a; match a with D.Z -> () | D.S n -> reflexive n)
let rec (transitive @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable -> (c : D.index) @ immutable ->
    {u : unit | le a b && le b c} -> {u : unit | le a c} @ ghost = fun a b c premise -> ghost_ (
  le_def a b; le_def b c; le_def a c;
  match a, b, c with D.S a, D.S b, D.S c -> transitive a b c () | _ -> ())
let[@def] rec (max @ total) (a : D.index @ immutable) (b : D.index @ immutable) = match a, b with
  | D.Z, _ -> b | _, D.Z -> a | D.S a, D.S b -> D.S (max a b)
let rec (max_bounds @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    {u : unit | le a (max a b) && le b (max a b)} @ ghost = fun a b -> ghost_ (
  max_def a b; le_def a (max a b); le_def b (max a b);
  match a, b with
  | D.Z, _ -> reflexive b
  | _, D.Z -> reflexive a
  | D.S a, D.S b -> max_bounds a b)
let[@def] rec (capacity @ total) (blocks : G.table @ immutable) = match blocks with
  | G.Empty -> D.Z | G.Add (block, rest) -> max (F.size block.G.signature) (capacity rest)
let rec (lookup @ total) : (blocks : G.table) @ immutable -> (label : D.index) @ immutable -> (block : G.block) @ immutable ->
    {u : unit | G.lookup blocks label === Some block} ->
    {u : unit | le (F.size block.G.signature) (capacity blocks)} @ ghost = fun blocks label block premise -> ghost_ (
  G.lookup_def blocks label; capacity_def blocks;
  match blocks with G.Empty -> () | G.Add (head, rest) ->
    max_bounds (F.size head.G.signature) (capacity rest);
    if Hm_elaboration_check.index_equal label (G.size rest) then () else (
      lookup rest label block (); transitive (F.size block.G.signature) (capacity rest) (capacity blocks) ()))
let rec (remaining @ total) : (capacity : D.index) @ immutable -> (used : D.index) @ immutable ->
    {u : unit | le used capacity} -> {rest : D.index | D.add used rest === capacity} @ immutable = fun capacity used premise ->
  ghost_ (le_def used capacity);
  match used, capacity with
  | D.Z, _ -> ghost_ (D.add_def D.Z capacity); capacity
  | D.S n, D.S m ->
    let rest = remaining m n () in ghost_ (D.add_def used rest); rest
  | _ -> unreachable_ ()
let rec (padding @ total) : (count : D.index) @ immutable -> {out : F.cells | F.length out === count} @ immutable = fun count ->
  match count with
  | D.Z -> ghost_ (F.length_def F.Empty); F.Empty
  | D.S n -> let rest = padding n in let out = F.Cell (R.V.Nil, rest) in ghost_ (F.length_def out); out
