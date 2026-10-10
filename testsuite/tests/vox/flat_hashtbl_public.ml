module Exercise (Key : Vox_verified_flat_hashtbl.Key) = struct
  module V = Vox_verified_flat_hashtbl.Make (Key)

  let find_after_replace (key : Key.t) (value : int) :
      {v : int option | v === Some value} =
    let c : int V.created = V.create () in
    let p = V.replace c.#table key value c.#permission in
    V.find_opt c.#table key (borrow_ p)

  let empty_lookup (key : Key.t) : {v : int option | v === None} =
    let c : int V.created = V.create () in
    V.find_opt c.#table key (borrow_ c.#permission)

  let find_equivalent :
      (table : int V.t) -> (key : Key.t) ->
      (query : {q : Key.t | Key.equal key q}) ->
      (p : {p : int V.permission | V.owner p === table}) @ local read ->
      {v : int option | v === V.Model.find_opt key (V.bindings p)} =
    fun table key query p -> V.find_opt table query p

  let replace_lookup :
      (table : int V.t) -> (key : Key.t) -> (value : int) -> (query : Key.t) ->
      (p : {p : int V.permission | V.owner p === table}) @ unique read_write ->
      {v : int option | v === (if Key.equal key query then Some value
        else V.Model.find_opt query (V.bindings p))} =
    fun table key value query p ->
    let q = V.replace table key value p in
    V.find_opt table query (borrow_ q)

  let remove_lookup :
      (table : int V.t) -> (key : Key.t) -> (query : Key.t) ->
      (p : {p : int V.permission | V.owner p === table}) @ unique read_write ->
      {v : int option | v === (if Key.equal key query then None
        else V.Model.find_opt query (V.bindings p))} =
    fun table key query p ->
    let q = V.remove table key p in
    V.find_opt table query (borrow_ q)

  let clear_lookup :
      (table : int V.t) -> (query : Key.t) ->
      (p : {p : int V.permission | V.owner p === table}) @ unique read_write ->
      {v : int option | v === None} =
    fun table query p ->
    let q = V.clear table p in
    V.find_opt table query (borrow_ q)

  let replace_length :
      (table : int V.t) -> (key : Key.t) -> (value : int) ->
      (p : {p : int V.permission | V.owner p === table}) @ unique read_write ->
      {n : int | Bigint.of_int n =
        (if V.Model.mem key (V.bindings p) then V.Model.cardinal (V.bindings p)
         else Bigint.add (V.Model.cardinal (V.bindings p)) 1Z)} =
    fun table key value p ->
    let q = V.replace table key value p in
    V.length table (borrow_ q)

  let remove_length :
      (table : int V.t) -> (key : Key.t) ->
      (p : {p : int V.permission | V.owner p === table}) @ unique read_write ->
      {n : int | Bigint.of_int n =
        (if V.Model.mem key (V.bindings p)
         then Bigint.sub (V.Model.cardinal (V.bindings p)) 1Z
         else V.Model.cardinal (V.bindings p))} =
    fun table key p ->
    let q = V.remove table key p in
    V.length table (borrow_ q)

  let update_framed :
      (table : int V.t) -> (other : int V.t) ->
      (key : Key.t) -> (value : int) -> (query : Key.t) ->
      (p : {p : int V.permission | V.owner p === table}) @ unique read_write ->
      (other_p : {p : int V.permission | V.owner p === other}) @ local read ->
      {v : int option | v === V.Model.find_opt query (V.bindings other_p)} =
    fun table other key value query p other_p ->
    let _ = V.replace table key value p in
    V.find_opt other query other_p
end

module Key = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = x = y
  let[@def] (hash @ total) (x : int) =
    (x lxor Int.Refined.shift_right_logical x 32) * 0x9e3779b97f4a7c1
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
    equal_def x y; ()
end
module E = Exercise (Key)
module V = E.V

(* The example again at top level, where Flambda can specialize the table's
   functor. flat_hashtbl_boundary.ml checks its native code at -O3. *)
let find_after_replace_int (key : int) (value : int) :
    {v : int option | v === Some value} =
  let c : int V.created = V.create () in
  let u = V.replace c.#table key value c.#permission in
  V.find_opt c.#table key (borrow_ u)
let rec fill :
    (table : int V.t) -> (i : int) ->
    (p : {p : int V.permission | V.owner p === table}) @ unique read_write ->
    {q : int V.permission | V.owner q === table} @ unique = fun table i p ->
  if i = 300 then p
  else let q = V.replace table i (i + 7) p in
    fill table (i + 1) q

let () =
  assert (E.empty_lookup 5 = None);
  assert (E.find_after_replace 5 42 = Some 42);
  assert (find_after_replace_int 5 42 = Some 42);
  let a : int V.created = V.create () in
  let alias = a.#table in
  let r = fill alias 0 a.#permission in
  Gc.full_major (); Gc.compact ();
  assert (V.length a.#table (borrow_ r) = 300);
  for key = 0 to 299 do
    assert (V.find a.#table key (borrow_ r) = key + 7)
  done;
  assert (E.replace_lookup alias 12 99 12 r = Some 99);
  let a : int V.created = V.create () in
  let r = V.replace a.#table 1 84 a.#permission in
  assert (E.remove_lookup a.#table 1 1 r = None);
  let a : int V.created = V.create () in
  let r = V.replace a.#table 1 84 a.#permission in
  assert (E.clear_lookup a.#table 1 r = None)

let () =
  let a : int V.created = V.create () in
  let r = fill a.#table 0 a.#permission in
  let removed = V.remove a.#table 0 r in
  let replaced = V.replace a.#table 299 999 removed in
  assert (V.length a.#table (borrow_ replaced) = 299);
  assert (V.find a.#table 299 (borrow_ replaced) = 999);
  let inserted = V.replace a.#table 500 1500 replaced in
  assert (V.length a.#table (borrow_ inserted) = 300);
  assert (V.find a.#table 500 (borrow_ inserted) = 1500);
  for key = 1 to 298 do
    assert (V.find a.#table key (borrow_ inserted) = key + 7)
  done

let () =
  let a : int V.created = V.create () in
  let r = V.replace a.#table 1 84 a.#permission in
  assert (E.replace_length a.#table 2 90 r = 2);
  let a : int V.created = V.create () in
  let r = V.replace a.#table 1 84 a.#permission in
  assert (E.remove_length a.#table 1 r = 0)

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
  let a : int V.created = V.create () in
  let r = V.replace a.#table 1 11 a.#permission in
  let r = V.replace a.#table 257 99 r in
  assert (V.length a.#table (borrow_ r) = 1);
  assert (V.find a.#table 1 (borrow_ r) = 99);
  assert (V.find a.#table 513 (borrow_ r) = 99);
  let r = V.remove a.#table 513 r in
  assert (V.length a.#table (borrow_ r) = 0);
  assert (not (V.mem a.#table 257 (borrow_ r)))

let () =
  let a : int V.created = V.create () in
  let b : int V.created = V.create () in
  let bp = V.replace b.#table 5 42 b.#permission in
  assert (E.update_framed a.#table b.#table 5 99 5
    a.#permission (borrow_ bp) = Some 42)
