module P = Ghost_pref
module H = P.Heap
module Exercise (Key : Vox_verified_flat_hashtbl.Key) = struct
  module V = Vox_verified_flat_hashtbl.Make (Key)

  let empty_lookup (key : Key.t @ immutable) :
      {v : int option | v === None} @ immutable =
    let r : int V.created = V.create (P.empty ()) in
    ghost_ (
      V.Map.lookup_def ([] : int V.Map.t) key);
    V.find_opt r.table r.view key (borrow_ r.state)

  let replace_lookup :
      (table : int V.t) @ immutable ->
      (before : int V.view) @ immutable ->
      (key : Key.t) @ immutable ->
      (value : int) ->
      (query : Key.t) @ immutable ->
      (token : {t : int V.state P.token |
        H.at (P.own t) (V.location table) === Some (V.model before)}) @ unique read_write ghost ->
      {v : int option | v === (if Key.equal key query then Some value
        else V.Map.lookup (V.bindings before) query)} @ immutable =
    fun table before key value query token ->
    let r = V.replace table before key value token in
    ghost_ (
      V.Map.same_get (V.bindings r.#view)
        (V.Map.put (V.bindings before) key value) query;
      V.Map.put_get (V.bindings before) key value query);
    V.find_opt table r.#view query (borrow_ r.#state)

  let remove_lookup :
      (table : int V.t) @ immutable ->
      (before : int V.view) @ immutable ->
      (key : Key.t) @ immutable ->
      (query : Key.t) @ immutable ->
      (token : {t : int V.state P.token |
        H.at (P.own t) (V.location table) === Some (V.model before)}) @ unique read_write ghost ->
      {v : int option | v === (if Key.equal key query then None
        else V.Map.lookup (V.bindings before) query)} @ immutable =
    fun table before key query token ->
    let r = V.remove table before key token in
    ghost_ (
      V.Map.same_get (V.bindings r.#view)
        (V.Map.erase (V.bindings before) key) query;
      V.Map.erase_get (V.bindings before) key query);
    V.find_opt table r.#view query (borrow_ r.#state)

  let clear_lookup :
      (table : int V.t) @ immutable ->
      (before : int V.view) @ immutable ->
      (query : Key.t) @ immutable ->
      (token : {t : int V.state P.token |
        H.at (P.own t) (V.location table) === Some (V.model before)}) @ unique read_write ghost ->
      {v : int option | v === None} @ immutable =
    fun table before query token ->
    let r = V.clear table before token in
    ghost_ (V.Map.lookup_def ([] : int V.Map.t) query);
    V.find_opt table r.#view query (borrow_ r.#state)

  let update_framed :
      (table : int V.t) @ immutable ->
      (before : int V.view) @ immutable ->
      (other : {t : int V.t | not (V.location t === V.location table)}) @ immutable ->
      (other_view : int V.view) @ immutable ->
      (key : Key.t) @ immutable ->
      (value : int) ->
      (query : Key.t) @ immutable ->
      (token : {t : int V.state P.token |
        H.at (P.own t) (V.location table) === Some (V.model before) &&
        H.at (P.own t) (V.location other) === Some (V.model other_view)}) @ unique read_write ghost ->
      {v : int option | v === V.Map.lookup (V.bindings other_view) query} @ immutable =
    fun table before other other_view key value query token ->
    let r = V.replace table before key value token in
    V.find_opt other other_view query (borrow_ r.#state)
end

module Key = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = x = y
  let[@def] (hash @ total) (x : int) = 0
  let (reflexive @ total) (x : int) : {u : unit | equal x x} =
    equal_def x x; ()
  let (symmetric @ total) (x : int) (y : int) :
      {u : unit | equal x y = equal y x} =
    equal_def x y; equal_def y x; ()
  let (transitive @ total) (x : int) (y : int) (z : int) :
      {u : unit | not (equal x y && equal y z) || equal x z} =
    equal_def x y; equal_def y z; equal_def x z; ()
  let (hash_equal @ total) (x : int) (y : int) :
      {u : unit | not (equal x y) || hash x = hash y} =
    hash_def x; hash_def y; ()
end
module E = Exercise (Key)
module V = E.V
let rec fill :
      (table : int V.t) @ immutable ->
      (view : int V.view) @ immutable ->
      (i : int) ->
      (token : {t : int V.state P.token |
      H.at (P.own t) (V.location table) === Some (V.model view)}) @ unique read_write ghost ->
      {r : int V.result | H.at (P.own r.#state) (V.location table) ===
      Some (V.model r.#view)} @ unique =
    fun table view i token ->
  if i = 300 then #{V.view; state = token}
  else let r = V.replace table view i (i + 7) token in
    fill table r.#view (i + 1) r.#state

let () =
  assert (E.empty_lookup 5 = None);
  let a : int V.created = V.create (P.empty ()) in
  let alias = a.table in
  let r = fill alias a.view 0 a.state in
  Gc.full_major (); Gc.compact ();
  assert (V.length a.table r.#view (borrow_ r.#state) = 300);
  for key = 0 to 299 do
    assert (V.find a.table r.#view key (borrow_ r.#state) = key + 7)
  done;
  assert (E.replace_lookup alias r.#view 12 99 12 r.#state = Some 99);
  let a : int V.created = V.create (P.empty ()) in
  let r = V.replace a.table a.view 1 84 a.state in
  assert (E.remove_lookup a.table r.#view 1 1 r.#state = None);
  let a : int V.created = V.create (P.empty ()) in
  let r = V.replace a.table a.view 1 84 a.state in
  assert (E.clear_lookup a.table r.#view 1 r.#state = None)

let () =
  let a : int V.created = V.create (P.empty ()) in
  let r = fill a.table a.view 0 a.state in
  let removed = V.remove a.table r.#view 0 r.#state in
  let replaced = V.replace a.table removed.#view 299 999 removed.#state in
  assert (V.length a.table replaced.#view (borrow_ replaced.#state) = 299);
  assert (V.find a.table replaced.#view 299 (borrow_ replaced.#state) = 999);
  let inserted = V.replace a.table replaced.#view 500 1500 replaced.#state in
  assert (V.length a.table inserted.#view (borrow_ inserted.#state) = 300);
  assert (V.find a.table inserted.#view 500 (borrow_ inserted.#state) = 1500);
  for key = 1 to 298 do
    assert (V.find a.table inserted.#view key (borrow_ inserted.#state) = key + 7)
  done

module Equivalent_key = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = (x land 255) = (y land 255)
  let[@def] (hash @ total) (x : int) = 0
  let (reflexive @ total) (x : int) : {u : unit | equal x x} =
    equal_def x x; ()
  let (symmetric @ total) (x : int) (y : int) :
      {u : unit | equal x y = equal y x} =
    equal_def x y; equal_def y x; ()
  let (transitive @ total) (x : int) (y : int) (z : int) :
      {u : unit | not (equal x y && equal y z) || equal x z} =
    equal_def x y; equal_def y z; equal_def x z; ()
  let (hash_equal @ total) (x : int) (y : int) :
      {u : unit | not (equal x y) || hash x = hash y} =
    hash_def x; hash_def y; ()
end
module Equivalent = Exercise (Equivalent_key)
let () =
  let module V = Equivalent.V in
  let a : int V.created = V.create (P.empty ()) in
  let r = V.replace a.table a.view 1 11 a.state in
  let r = V.replace a.table r.#view 257 99 r.#state in
  assert (V.length a.table r.#view (borrow_ r.#state) = 1);
  assert (V.find a.table r.#view 1 (borrow_ r.#state) = 99);
  assert (V.find a.table r.#view 513 (borrow_ r.#state) = 99);
  let r = V.remove a.table r.#view 513 r.#state in
  assert (V.length a.table r.#view (borrow_ r.#state) = 0);
  assert (not (V.mem a.table r.#view 257 (borrow_ r.#state)))
