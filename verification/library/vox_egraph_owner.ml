module P = Ghost_pref
module H = P.Heap
module T = Vox_table_storage
module A = Borrow_iarray.Owned_array
module Slice = Borrow_iarray.Slice
module I = Vox_iarray
module K = Vox_egraph_key
module S = Vox_egraph_arena_spec
module Memo = Vox_table_implementation.Make (K)
module X = Vox_egraph_index_spec.Make (Memo.Spec.Map)

type t = {
  arena : {a : K.t option A.t | Iarray.length (A.contents a) = 512};
  count : int;
  memo : int Memo.t @@ aliased;
  view : int Memo.Spec.view @@ aliased;
  token : (K.t, int) Vox_table_model.state P.token @@ ghost;
}

let[@def] valid (owner : t @ local immutable total ghost forkable unyielding) =
  ghost_ (0 <= owner.count && owner.count <= 512 &&
    S.allocated (A.contents owner.arena) owner.count &&
    X.valid owner.view.model.slots (A.contents owner.arena) owner.count &&
    X.indexed owner.view.model.slots (A.contents owner.arena) owner.count)

type result = #{value : int option @@ aliased; state : t}

let create : unit -> {owner : t | valid owner && owner.count = 0 &&
    Memo.Spec.valid owner.view &&
    H.at (P.own owner.token) (T.location owner.memo) ===
      Some owner.view.model}
    @ unique = fun () ->
  let arena = A.of_iarray (Iarray.init 512 (fun _ -> (None : K.t option))) in
  let r : int Memo.created = Memo.create (P.empty ()) in
  let owner = {arena; count = 0; memo = r.table; view = r.view;
    token = r.state} in
  let cells = ghost_ (A.contents (borrow_ owner.arena)) in
  ghost_ (
    S.allocated_def cells 0;
    Vox_table_model.initial_def 16 (None : (K.t * int) option);
    X.empty16 cells;
    X.indexed_def r.view.model.slots cells 0;
    let _ : {u : unit | r.view.model.slots ===
      Vox_table_model.repeat 16 (None : (K.t * int) option)} =
      () in
    let _ : {u : unit | X.valid r.view.model.slots
      cells 0} = () in
    valid_def (borrow_ owner));
  owner

let lookup : (owner : {o : t | valid o && Memo.Spec.valid o.view &&
      H.at (P.own o.token) (T.location o.memo) === Some o.view.model})
    @ unique ->
    (key : K.t) @ immutable ->
    {r : result | valid r.#state &&
      Memo.Spec.valid r.#state.view &&
      H.at (P.own r.#state.token) (T.location r.#state.memo) ===
        Some r.#state.view.model &&
      r.#state.count = owner.count &&
      A.contents r.#state.arena === A.contents owner.arena &&
      r.#state.view.model === owner.view.model &&
      r.#value === Memo.Spec.Map.lookup r.#state.view.model.slots key &&
      (match r.#value with
       | None -> true
       | Some id -> 0 <= id && id < r.#state.count &&
         I.at (A.contents r.#state.arena) id === Some (Some key))}
    @ unique = fun owner key ->
  ghost_ (valid_def (borrow_ owner));
  let {arena; count; memo; view; token} = owner in
  let cells = ghost_ (A.contents (borrow_ arena)) in
  let found = Memo.find_opt memo view key (borrow_ token) in
  ghost_ (
    let _ : {u : unit | X.valid view.model.slots
      cells count} = () in
    match found with
    | None -> ()
    | Some id ->
      let _ : {u : unit | Memo.Spec.Map.lookup view.model.slots key ===
        Some id} = () in
      X.lookup_valid view.model.slots cells count
        key id ());
  #{value = found; state = {arena; count; memo; view; token}}

let append : (owner : {o : t | valid o && Memo.Spec.valid o.view &&
      H.at (P.own o.token) (T.location o.memo) === Some o.view.model})
    @ unique ->
    (key : K.t) @ immutable ->
    {r : result | valid r.#state &&
      Memo.Spec.valid r.#state.view &&
      H.at (P.own r.#state.token) (T.location r.#state.memo) ===
        Some r.#state.view.model &&
      (match Memo.Spec.Map.lookup owner.view.model.slots key with
       | Some id -> r.#value === Some id &&
         r.#state.count = owner.count &&
         A.contents r.#state.arena === A.contents owner.arena
       | None ->
         if owner.count < 512 then
           r.#value === Some owner.count &&
           r.#state.count = owner.count + 1 &&
           A.contents r.#state.arena ===
             I.updated (A.contents owner.arena) owner.count (Some key)
         else r.#value === None && r.#state.count = owner.count) &&
      (match r.#value with
       | None -> owner.count = 512 && r.#state.count = owner.count
       | Some id -> 0 <= id && id < r.#state.count &&
         I.at (A.contents r.#state.arena) id === Some (Some key))}
    @ unique = fun owner key ->
  ghost_ (valid_def (borrow_ owner));
  let {arena; count; memo; view; token} = owner in
  let found = Memo.find_opt memo view key (borrow_ token) in
  match found with
  | Some id ->
    ghost_ (X.lookup_valid view.model.slots (A.contents (borrow_ arena))
      count key id ());
    #{value = Some id; state = {arena; count; memo; view; token}}
  | None ->
    if count = 512 then
      #{value = None; state = {arena; count; memo; view; token}}
    else
      let before = ghost_ (A.contents (borrow_ arena)) in
      let post = ghost_ (fun (_ : unit) (after : K.t option iarray @ immutable) ->
        after === I.updated before count (Some key)) in
      let result = A.with_mut arena post (fun slice ->
        let slice = slice in
        let slot :
          {i : int | 0 <= i && i < Iarray.length (Slice.current slice)} =
          count in
        let slice = Slice.set slice slot (Some key) in
        Slice.finish slice;
        ()) in
      let {Borrow_iarray.state = arena; _} = result in
      let changed = Memo.replace memo view key count token in
      ghost_ (
        S.append_preserves before count key ();
        X.append_valid view.model.slots before count key ();
        X.append_indexed view.model.slots before count key ();
        Memo.Spec.Map.same_def changed.#view.model.slots
          (Memo.Spec.Map.put view.model.slots key count);
        Memo.Spec.Map.same_def
          (Memo.Spec.Map.put view.model.slots key count)
          changed.#view.model.slots;
        X.valid_agrees
          (Memo.Spec.Map.put view.model.slots key count)
          changed.#view.model.slots (A.contents (borrow_ arena))
          (count + 1) ();
        X.indexed_same
          (Memo.Spec.Map.put view.model.slots key count)
          changed.#view.model.slots (A.contents (borrow_ arena))
          (count + 1) ();
        I.updated_read before count (Some key) count;
        Memo.Spec.Map.same_get changed.#view.model.slots
          (Memo.Spec.Map.put view.model.slots key count) key;
        Memo.Spec.Map.put_get view.model.slots key count key;
        K.reflexive key);
      let owner = {arena; count = count + 1; memo;
        view = changed.#view; token = changed.#state} in
      ghost_ (valid_def (borrow_ owner));
      #{value = Some count; state = owner}
