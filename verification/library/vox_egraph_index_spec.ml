module I = Vox_iarray
module K = Vox_egraph_key
module Template = Vox_table_map.Make (K)
module Make (M : module type of Template) = struct

let[@def] rec (valid @ total) (slots : int M.slots @ immutable)
    (arena : K.t option iarray @ immutable) (count : int) = ghost_ (
  match slots with
  | [] -> true
  | None :: tail -> valid tail arena count
  | Some (key, id) :: tail ->
    0 <= id && id < count && I.at arena id === Some (Some key) &&
    valid tail arena count)

let (valid_cons_none @ total) :
    (tail : int M.slots) @ immutable ->
    (arena : K.t option iarray) @ immutable -> (count : int) ->
    {u : unit | valid tail arena count} ->
    {u : unit | valid (None :: tail) arena count} @ ghost =
  fun tail arena count premise -> ghost_ (
    valid_def (None :: tail) arena count;
    ())

let (empty16 @ total) :
    (arena : K.t option iarray) @ immutable ->
    {u : unit | valid (Vox_table_model.repeat 16 (None : (K.t * int) option)) arena 0}
    @ ghost = fun arena -> ghost_ (
  Vox_table_model.repeat_def 0 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 0 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 1 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 1 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 2 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 2 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 3 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 3 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 4 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 4 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 5 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 5 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 6 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 6 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 7 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 7 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 8 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 8 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 9 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 9 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 10 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 10 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 11 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 11 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 12 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 12 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 13 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 13 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 14 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 14 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 15 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 15 (None : (K.t * int) option)) arena 0;
  Vox_table_model.repeat_def 16 (None : (K.t * int) option);
  valid_def (Vox_table_model.repeat 16 (None : (K.t * int) option)) arena 0;
  ())

let rec (lookup_valid @ total) :
    (slots : int M.slots) @ immutable ->
    (arena : K.t option iarray) @ immutable ->
    (count : int) -> (key : K.t) @ immutable -> (id : int) ->
    {u : unit | valid slots arena count &&
      M.lookup slots key === Some id} ->
    {u : unit | 0 <= id && id < count &&
      I.at arena id === Some (Some key)} @ ghost =
  fun slots arena count key id premise -> ghost_ (
    valid_def slots arena count;
    M.lookup_def slots key;
    match slots with
    | [] -> ()
    | None :: tail -> lookup_valid tail arena count key id (); ()
    | Some (stored, _) :: tail ->
      K.exact stored key;
      if K.equal stored key then ()
      else lookup_valid tail arena count key id ();
      ())

let rec (valid_agrees @ total) :
    (before : int M.slots) @ immutable ->
    (after : int M.slots) @ immutable ->
    (arena : K.t option iarray) @ immutable ->
    (count : int) ->
    {u : unit | valid before arena count && M.agrees after before} ->
    {u : unit | valid after arena count} @ ghost =
  fun before after arena count premise -> ghost_ (
    M.agrees_def after before;
    valid_def after arena count;
    match after with
    | [] -> ()
    | None :: tail -> valid_agrees before tail arena count (); ()
    | Some (key, id) :: tail ->
      lookup_valid before arena count key id ();
      valid_agrees before tail arena count ();
      ())

let rec (frame @ total) :
    (slots : int M.slots) @ immutable ->
    (arena : K.t option iarray) @ immutable ->
    (count : int) -> (key : K.t) @ immutable ->
    {u : unit | 0 <= count && count < Iarray.length arena &&
      valid slots arena count} ->
    {u : unit | valid slots (I.updated arena count (Some key))
      (count + 1)} @ ghost = fun slots arena count key premise -> ghost_ (
  valid_def slots arena count;
  valid_def slots (I.updated arena count (Some key)) (count + 1);
  match slots with
  | [] -> ()
  | None :: tail -> frame tail arena count key (); ()
  | Some (_, id) :: tail ->
    I.updated_read arena count (Some key) id;
    frame tail arena count key ();
    ())

let rec (erase_valid @ total) :
    (slots : int M.slots) @ immutable ->
    (arena : K.t option iarray) @ immutable ->
    (count : int) -> (key : K.t) @ immutable ->
    {u : unit | valid slots arena count} ->
    {u : unit | valid (M.erase slots key) arena count}
    @ ghost = fun slots arena count key premise -> ghost_ (
  valid_def slots arena count;
  M.erase_def slots key;
  valid_def (M.erase slots key) arena count;
  match slots with
  | [] -> ()
  | None :: tail -> erase_valid tail arena count key (); ()
  | Some (_, _) :: tail -> erase_valid tail arena count key (); ())

let (append_valid @ total) :
    (slots : int M.slots) @ immutable ->
    (arena : K.t option iarray) @ immutable ->
    (count : int) -> (key : K.t) @ immutable ->
    {u : unit | 0 <= count && count < Iarray.length arena &&
      valid slots arena count} ->
    {u : unit | valid (M.put slots key count)
      (I.updated arena count (Some key)) (count + 1)}
    @ ghost = fun slots arena count key premise -> ghost_ (
  let changed = I.updated arena count (Some key) in
  frame slots arena count key ();
  erase_valid slots changed (count + 1) key ();
  I.updated_read arena count (Some key) count;
  M.put_def slots key count;
  valid_def (M.put slots key count) changed (count + 1);
  ())

let[@def] rec (indexed @ total) (slots : int M.slots @ immutable)
    (arena : K.t option iarray @ immutable) (count : int) = ghost_ (
  if count <= 0 then true
  else
    (match I.at arena (count - 1) with
     | Some (Some key) -> M.lookup slots key === Some (count - 1)
     | _ -> false) &&
    indexed slots arena (count - 1))
  [@@decreases if count > 0 then count else 0]

let rec (indexed_frame @ total) :
    (slots : int M.slots) @ immutable ->
    (arena : K.t option iarray) @ immutable ->
    (count : int) -> (index : int) -> (key : K.t) @ immutable ->
    {u : unit | 0 <= count && count <= index &&
      index < Iarray.length arena && indexed slots arena count &&
      M.lookup slots key === None} ->
    {u : unit | indexed (M.put slots key index)
      (I.updated arena index (Some key)) count}
    @ ghost = fun slots arena count index key premise -> ghost_ (
  indexed_def slots arena count;
  indexed_def (M.put slots key index)
    (I.updated arena index (Some key)) count;
  if count > 0 then (
    I.updated_read arena index (Some key) (count - 1);
    match I.at arena (count - 1) with
    | Some (Some stored) ->
      M.lookup_congruent slots key stored;
      K.symmetric key stored;
      M.put_get slots key index stored;
      indexed_frame slots arena (count - 1) index key ();
      ()
    | _ -> ());
  ())
  [@@decreases if count > 0 then count else 0]

let (append_indexed @ total) :
    (slots : int M.slots) @ immutable ->
    (arena : K.t option iarray) @ immutable ->
    (count : int) -> (key : K.t) @ immutable ->
    {u : unit | 0 <= count && count < Iarray.length arena &&
      indexed slots arena count && M.lookup slots key === None} ->
    {u : unit | indexed (M.put slots key count)
      (I.updated arena count (Some key)) (count + 1)}
    @ ghost = fun slots arena count key premise -> ghost_ (
  indexed_frame slots arena count count key ();
  I.updated_read arena count (Some key) count;
  M.put_get slots key count key;
  K.reflexive key;
  indexed_def (M.put slots key count)
    (I.updated arena count (Some key)) (count + 1);
  ())

let rec (indexed_same @ total) :
    (before : int M.slots) @ immutable ->
    (after : int M.slots) @ immutable ->
    (arena : K.t option iarray) @ immutable ->
    (count : int) ->
    {u : unit | indexed before arena count && M.same before after} ->
    {u : unit | indexed after arena count} @ ghost =
  fun before after arena count premise -> ghost_ (
    indexed_def before arena count;
    indexed_def after arena count;
    if count > 0 then (
      match I.at arena (count - 1) with
      | Some (Some key) ->
        M.same_get before after key;
        indexed_same before after arena (count - 1) ();
        ()
      | _ -> ());
    ())
  [@@decreases if count > 0 then count else 0]
end
