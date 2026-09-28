module D = Hm_declarative
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Patch = Hmc_cell_patch
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Model = Hmc_frame_relayout_model
let rec (associate @ total) : (first : Heap.cells) @ immutable -> (second : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Seg.append (Seg.append first second) tail === Seg.append first (Seg.append second tail)} @ ghost =
  fun first second tail -> ghost_ (
    Seg.append_def first second; Seg.append_def (Seg.append first second) tail; Seg.append_def first (Seg.append second tail);
    match first with Heap.Empty -> () | Heap.Cell (_, rest) -> associate rest second tail)
let rec (prefix @ total) : (head : Heap.cells) @ immutable -> (body : Heap.cells) @ immutable ->
    (values : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Seg.drop (Heap.length values) body === Some tail} ->
    {u : unit | Patch.write (Heap.length head) values (Seg.append head body) ===
      Some (Seg.append head (Seg.append values tail))} @ ghost = fun head body values tail premise -> ghost_ (
    Heap.length_def head; Seg.append_def head body; Seg.append_def head (Seg.append values tail);
    Patch.write_def (Heap.length head) values (Seg.append head body);
    match values with
    | Heap.Empty ->
      Heap.length_def values; Seg.drop_def D.Z body; Seg.append_def values tail
    | _ -> (match head with
      | Heap.Empty -> Hmc_cell_patch_algebra.overlay values body tail ()
      | Heap.Cell (_, rest) -> prefix rest body values tail ()))
let (save_environment @ total) : (signature : G.signature) @ immutable -> (next : D.index) @ immutable ->
    (current : Hmc_tagged_cell.value) @ immutable -> (accumulator : Hmc_tagged_cell.value) @ immutable ->
    (body : Heap.cells) @ immutable -> (env : Heap.cells) @ immutable -> (temporaries : Heap.cells) @ immutable ->
    (old : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    {u : unit | Seg.take (Codec.locals_size signature.G.locals) body === Some env
      && Seg.drop (Codec.locals_size signature.G.locals) body === Some temporaries
      && Seg.take (Codec.temporaries_size signature.G.temporaries) temporaries === Some old
      && Seg.drop (Heap.length (Seg.append env old)) temporaries === Some padding} ->
    {u : unit | Patch.write (D.S (D.S (Codec.locals_size signature.G.locals))) (Seg.append env old)
      (Heap.Cell (current, Heap.Cell (accumulator, body))) ===
      Model.reshape signature (G.Save_environment next) (Heap.Cell (current, Heap.Cell (accumulator, body))) padding} @ ghost =
  fun signature next current accumulator body env temporaries old padding premise -> ghost_ (
    Hmc_cell_slice.take_length (Codec.locals_size signature.G.locals) body env ();
    Hmc_cell_patch_algebra.join (Codec.locals_size signature.G.locals) body env temporaries ();
    let head = Heap.Cell (current, Heap.Cell (accumulator, env)) in
    Heap.length_def head; Heap.length_def (Heap.Cell (accumulator, env));
    Seg.append_def head temporaries; Seg.append_def (Heap.Cell (accumulator, env)) temporaries;
    prefix head temporaries (Seg.append env old) padding ();
    Seg.append_def head (Seg.append (Seg.append env old) padding);
    Seg.append_def (Heap.Cell (accumulator, env)) (Seg.append (Seg.append env old) padding);
    associate env old padding;
    Model.reshape_def signature (G.Save_environment next) (Heap.Cell (current, Heap.Cell (accumulator, body))) padding)
let[@def] (replacement @ total) (instruction : G.instruction @ immutable) (saved : Heap.cells @ immutable)
    (accumulator : Hmc_tagged_cell.value @ immutable) (old : Heap.cells @ immutable) = match instruction with
  | G.Save_value _ -> Some (Seg.append saved (Heap.Cell (accumulator, Seg.append saved old)))
  | G.Bind _ -> Some (Heap.Cell (accumulator, Seg.append saved (Seg.append saved old)))
  | G.Restore _ -> Some (Seg.append saved old)
  | _ -> None
let (saved_environment @ total) : (signature : G.signature) @ immutable -> (instruction : G.instruction) @ immutable ->
    (context : D.context) @ immutable -> (rest : G.temporaries) @ immutable ->
    (current : Hmc_tagged_cell.value) @ immutable -> (accumulator : Hmc_tagged_cell.value) @ immutable ->
    (body : Heap.cells) @ immutable -> (env : Heap.cells) @ immutable -> (temporaries : Heap.cells) @ immutable ->
    (saved : Heap.cells) @ immutable -> (more : Heap.cells) @ immutable -> (old : Heap.cells) @ immutable ->
    (values : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    {u : unit | signature.G.temporaries === G.Environment (context, rest)
      && Seg.take (Codec.locals_size signature.G.locals) body === Some env
      && Seg.drop (Codec.locals_size signature.G.locals) body === Some temporaries
      && Seg.take (Codec.locals_size context) temporaries === Some saved
      && Seg.drop (Codec.locals_size context) temporaries === Some more
      && Seg.take (Codec.temporaries_size rest) more === Some old
      && replacement instruction saved accumulator old === Some values
      && Seg.drop (Heap.length values) body === Some padding} ->
    {u : unit | Patch.write (D.S (D.S D.Z)) values (Heap.Cell (current, Heap.Cell (accumulator, body))) ===
      Model.reshape signature instruction (Heap.Cell (current, Heap.Cell (accumulator, body))) padding} @ ghost =
  fun signature instruction context rest current accumulator body env temporaries saved more old values padding premise -> ghost_ (
    let head = Heap.Cell (current, Heap.Cell (accumulator, Heap.Empty)) in
    Heap.length_def head; Heap.length_def (Heap.Cell (accumulator, Heap.Empty)); Heap.length_def Heap.Empty;
    Seg.append_def head body; Seg.append_def (Heap.Cell (accumulator, Heap.Empty)) body; Seg.append_def Heap.Empty body;
    prefix head body values padding ();
    Seg.append_def head (Seg.append values padding);
    Seg.append_def (Heap.Cell (accumulator, Heap.Empty)) (Seg.append values padding); Seg.append_def Heap.Empty (Seg.append values padding);
    replacement_def instruction saved accumulator old;
    Model.reshape_def signature instruction (Heap.Cell (current, Heap.Cell (accumulator, body))) padding;
    match instruction with
    | G.Save_value _ ->
      associate saved (Heap.Cell (accumulator, Seg.append saved old)) padding;
      Seg.append_def (Heap.Cell (accumulator, Seg.append saved old)) padding;
      associate saved old padding
    | G.Bind _ ->
      Seg.append_def (Heap.Cell (accumulator, Seg.append saved (Seg.append saved old))) padding;
      associate saved (Seg.append saved old) padding; associate saved old padding
    | G.Restore _ -> associate saved old padding
    | _ -> ())
