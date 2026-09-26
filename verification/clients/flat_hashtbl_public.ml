module P = Ghost_pref
module H = P.Heap
module Exercise (Key : Vox_verified_flat_hashtbl.Key) = struct
  module V = Vox_verified_flat_hashtbl.Make (Key)

  (* Reading a key back after storing it returns the stored value. The
     result type is checked at compile time; the ghost_ block is the proof
     and is erased. *)
  let find_after_replace (key : Key.t) (value : int) :
      {v : int option | v === Some value} =
    let t : int V.created = V.create (P.empty ()) in
    let r = V.replace t.table t.view key value t.token in
    ghost_ (Key.reflexive key; V.Map.put_get (V.bindings t.view) key value key);
    V.find_opt t.table r.#view key (borrow_ r.#token)

  let empty_lookup (key : Key.t) : {v : int option | v === None} =
    let r : int V.created = V.create (P.empty ()) in
    ghost_ (V.Map.lookup_empty (V.bindings r.view) key);
    V.find_opt r.table r.view key (borrow_ r.token)

  let replace_lookup :
      (table : int V.t) ->
      (before : int V.view) @ immutable ->
      (key : Key.t) ->
      (value : int) ->
      (query : Key.t) ->
      (token : {t : int V.state P.token |
        H.at (P.own t) (V.location table) === Some (V.model before)}) @ unique read_write ghost ->
      {v : int option | v === (if Key.equal key query then Some value
        else V.Map.lookup (V.bindings before) query)} =
    fun table before key value query token ->
    let r = V.replace table before key value token in
    ghost_ (V.Map.put_get (V.bindings before) key value query);
    V.find_opt table r.#view query (borrow_ r.#token)

  let remove_lookup :
      (table : int V.t) ->
      (before : int V.view) @ immutable ->
      (key : Key.t) ->
      (query : Key.t) ->
      (token : {t : int V.state P.token |
        H.at (P.own t) (V.location table) === Some (V.model before)}) @ unique read_write ghost ->
      {v : int option | v === (if Key.equal key query then None
        else V.Map.lookup (V.bindings before) query)} =
    fun table before key query token ->
    let r = V.remove table before key token in
    ghost_ (V.Map.erase_get (V.bindings before) key query);
    V.find_opt table r.#view query (borrow_ r.#token)

  let clear_lookup :
      (table : int V.t) ->
      (before : int V.view) @ immutable ->
      (query : Key.t) ->
      (token : {t : int V.state P.token |
        H.at (P.own t) (V.location table) === Some (V.model before)}) @ unique read_write ghost ->
      {v : int option | v === None} =
    fun table before query token ->
    let r = V.clear table before token in
    ghost_ (V.Map.lookup_empty (V.bindings r.#view) query);
    V.find_opt table r.#view query (borrow_ r.#token)

  let replace_length :
      (table : int V.t) ->
      (before : int V.view) @ immutable ->
      (key : Key.t) ->
      (value : int) ->
      (token : {t : int V.state P.token |
        H.at (P.own t) (V.location table) === Some (V.model before)}) @ unique read_write ghost ->
      {n : int | Bigint.of_int n =
        (if V.Map.lookup (V.bindings before) key === None
         then Bigint.add (V.Map.count (V.bindings before)) 1Z
         else V.Map.count (V.bindings before))} =
    fun table before key value token ->
    let r = V.replace table before key value token in
    ghost_ (V.Map.count_put (V.bindings before) key value);
    V.length table r.#view (borrow_ r.#token)

  let remove_length :
      (table : int V.t) ->
      (before : int V.view) @ immutable ->
      (key : Key.t) ->
      (token : {t : int V.state P.token |
        H.at (P.own t) (V.location table) === Some (V.model before)}) @ unique read_write ghost ->
      {n : int | Bigint.of_int n =
        (if V.Map.lookup (V.bindings before) key === None
         then V.Map.count (V.bindings before)
         else Bigint.sub (V.Map.count (V.bindings before)) 1Z)} =
    fun table before key token ->
    let r = V.remove table before key token in
    ghost_ (V.Map.count_erase (V.bindings before) key);
    V.length table r.#view (borrow_ r.#token)

  let update_framed :
      (table : int V.t) ->
      (before : int V.view) @ immutable ->
      (other : {t : int V.t | not (V.location t === V.location table)}) ->
      (other_view : int V.view) @ immutable ->
      (key : Key.t) ->
      (value : int) ->
      (query : Key.t) ->
      (token : {t : int V.state P.token |
        H.at (P.own t) (V.location table) === Some (V.model before) &&
        H.at (P.own t) (V.location other) === Some (V.model other_view)}) @ unique read_write ghost ->
      {v : int option | v === V.Map.lookup (V.bindings other_view) query} =
    fun table before other other_view key value query token ->
    let r = V.replace table before key value token in
    V.find_opt other other_view query (borrow_ r.#token)
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
      (table : int V.t) ->
      (view : int V.view) @ immutable ->
      (i : int) ->
      (token : {t : int V.state P.token |
      H.at (P.own t) (V.location table) === Some (V.model view)}) @ unique read_write ghost ->
      {r : int V.updated | H.at (P.own r.#token) (V.location table) ===
      Some (V.model r.#view)} @ unique =
    fun table view i token ->
  if i = 300 then #{V.view; token}
  else let r = V.replace table view i (i + 7) token in
    fill table r.#view (i + 1) r.#token

let () =
  assert (E.empty_lookup 5 = None);
  assert (E.find_after_replace 5 42 = Some 42);
  let a : int V.created = V.create (P.empty ()) in
  let alias = a.table in
  let r = fill alias a.view 0 a.token in
  Gc.full_major (); Gc.compact ();
  assert (V.length a.table r.#view (borrow_ r.#token) = 300);
  for key = 0 to 299 do
    assert (V.find a.table r.#view key (borrow_ r.#token) = key + 7)
  done;
  assert (E.replace_lookup alias r.#view 12 99 12 r.#token = Some 99);
  let a : int V.created = V.create (P.empty ()) in
  let r = V.replace a.table a.view 1 84 a.token in
  assert (E.remove_lookup a.table r.#view 1 1 r.#token = None);
  let a : int V.created = V.create (P.empty ()) in
  let r = V.replace a.table a.view 1 84 a.token in
  assert (E.clear_lookup a.table r.#view 1 r.#token = None)

let () =
  let a : int V.created = V.create (P.empty ()) in
  let r = fill a.table a.view 0 a.token in
  let removed = V.remove a.table r.#view 0 r.#token in
  let replaced = V.replace a.table removed.#view 299 999 removed.#token in
  assert (V.length a.table replaced.#view (borrow_ replaced.#token) = 299);
  assert (V.find a.table replaced.#view 299 (borrow_ replaced.#token) = 999);
  let inserted = V.replace a.table replaced.#view 500 1500 replaced.#token in
  assert (V.length a.table inserted.#view (borrow_ inserted.#token) = 300);
  assert (V.find a.table inserted.#view 500 (borrow_ inserted.#token) = 1500);
  for key = 1 to 298 do
    assert (V.find a.table inserted.#view key (borrow_ inserted.#token) = key + 7)
  done

let () =
  let a : int V.created = V.create (P.empty ()) in
  let r = V.replace a.table a.view 1 84 a.token in
  assert (E.replace_length a.table r.#view 2 90 r.#token = 2);
  let a : int V.created = V.create (P.empty ()) in
  let r = V.replace a.table a.view 1 84 a.token in
  assert (E.remove_length a.table r.#view 1 r.#token = 0)

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
  let r = V.replace a.table a.view 1 11 a.token in
  let r = V.replace a.table r.#view 257 99 r.#token in
  assert (V.length a.table r.#view (borrow_ r.#token) = 1);
  assert (V.find a.table r.#view 1 (borrow_ r.#token) = 99);
  assert (V.find a.table r.#view 513 (borrow_ r.#token) = 99);
  let r = V.remove a.table r.#view 513 r.#token in
  assert (V.length a.table r.#view (borrow_ r.#token) = 0);
  assert (not (V.mem a.table r.#view 257 (borrow_ r.#token)))
