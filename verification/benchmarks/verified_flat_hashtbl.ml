module Key = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = x = y
  let[@def] (hash @ total) (x : int) =
    let x = (x lxor (x lsr 16)) * 73244475 in
    let x = (x lxor (x lsr 16)) * 73244475 in
    x lxor (x lsr 16)
  let (reflexive @ total) (x : int) : {u : unit | equal x x} = equal_def x x; ()
  let (symmetric @ total) (x : int) (y : int) :
      {u : unit | equal x y = equal y x} = equal_def x y; equal_def y x; ()
  let (transitive @ total) (x : int) (y : int) (z : int) :
      {u : unit | not (equal x y && equal y z) || equal x z} =
    equal_def x y; equal_def y z; equal_def x z; ()
  let (hash_equal @ total) (x : int) (y : int) :
      {u : unit | not (equal x y) || hash x = hash y} =
    equal_def x y; ()
end

module V = Vox_verified_flat_hashtbl.Make (Key)
module I = V.Spec
module T = Vox_table_storage
module P = Ghost_pref
module H = P.Heap
module Standard = Hashtbl.Make (Key)

let key i = i * 104729
let stamp () = Gc.full_major (); (Gc.allocated_bytes (), Sys.time ())
let report name payload n operation count (bytes, time) =
  let elapsed = Sys.time () -. time in
  let allocated = Gc.allocated_bytes () -. bytes in
  Printf.printf "%s,%s,%d,%s,%.3f,%.3f\n%!" name payload n operation
    (elapsed *. 1e9 /. float count) (allocated /. float count)

let rec fill : ('a : immutable_data).
    (table : 'a V.t) @ immutable -> (values : 'a iarray) @ immutable -> (index
      : int) ->
    (view : {v : 'a I.view | I.valid v}) @ immutable ->
    (token : {t : P.token | H.at (P.own t) (T.location table) === Some
      view.model})
      @ unique read_write ghost ->
    {r : 'a V.result | I.valid r.#view &&
      H.at (P.own r.#state) (T.location table) === Some r.#view.model} @
        unique =
  fun table values index view token ->
    if index >= Iarray.length values then #{V.view; state = token} else begin
      let changed = V.replace table view (key index) (Iarray.get values index)
        token in
      fill table values (index + 1) changed.#view changed.#state
    end

let rec churn : ('a : immutable_data).
    (table : 'a V.t) @ immutable -> (values : 'a iarray) @ immutable -> (index
      : int) ->
    (view : {v : 'a I.view | I.valid v}) @ immutable ->
    (token : {t : P.token | H.at (P.own t) (T.location table) === Some
      view.model})
      @ unique read_write ghost ->
    {r : 'a V.result | I.valid r.#view &&
      H.at (P.own r.#state) (T.location table) === Some r.#view.model} @
        unique =
  fun table values index view token ->
    if index >= Iarray.length values then #{V.view; state = token} else begin
      let removed = V.remove table view (key index) token in
      let changed = V.replace table removed.#view (key (index + Iarray.length
        values))
        (Iarray.get values index) removed.#state in
      churn table values (index + 1) changed.#view changed.#state
    end

let verified : ('a : immutable_data).
    string -> string -> ('a iarray) @ immutable -> unit = fun name payload
      values ->
  let n = Iarray.length values in
  let timing = stamp () in
  let r : 'a V.created = V.create (P.empty ()) in
  let built = fill r.table values 0 r.view r.state in
  report name payload n "build" n timing;
  for i = 0 to n - 1 do
    assert (V.find r.table built.#view (key i) (borrow_ built.#state) =
      (Iarray.get values i))
  done;
  let iterations = max n 1000000 in
  let timing = stamp () in
  for i = 0 to iterations - 1 do
    ignore (Sys.opaque_identity
      (V.find r.table built.#view (key (i mod n)) (borrow_ built.#state)))
  done;
  report name payload n "hit" iterations timing;
  let timing = stamp () in
  let found = ref 0 in
  for i = 0 to iterations - 1 do
    if V.mem r.table built.#view (key (n + i mod n)) (borrow_ built.#state) then
      incr found
  done;
  report name payload n "miss" iterations timing;
  assert (!found = 0);
  let timing = stamp () in
  let changed = churn r.table values 0 built.#view built.#state in
  report name payload n "churn" (2 * n) timing;
  for i = 0 to n - 1 do
    assert (not (V.mem r.table changed.#view (key i) (borrow_ changed.#state)));
    assert (V.find r.table changed.#view (key (i + n)) (borrow_
      changed.#state) = (Iarray.get values i))
  done

let standard payload values =
  let n = Iarray.length values in
  let timing = stamp () in
  let table = Standard.create 16 in
  for i = 0 to n - 1 do Standard.replace table (key i) (Iarray.get values i)
    done;
  report "stdlib" payload n "build" n timing;
  for i = 0 to n - 1 do assert (Standard.find table (key i) = (Iarray.get
    values i)) done;
  let iterations = max n 1000000 in
  let timing = stamp () in
  for i = 0 to iterations - 1 do
    ignore (Sys.opaque_identity (Standard.find table (key (i mod n))))
  done;
  report "stdlib" payload n "hit" iterations timing;
  let timing = stamp () in
  let found = ref 0 in
  for i = 0 to iterations - 1 do
    if Standard.mem table (key (n + i mod n)) then incr found
  done;
  report "stdlib" payload n "miss" iterations timing;
  assert (!found = 0);
  let timing = stamp () in
  for i = 0 to n - 1 do
    Standard.remove table (key i);
    Standard.replace table (key (i + n)) (Iarray.get values i)
  done;
  report "stdlib" payload n "churn" (2 * n) timing;
  for i = 0 to n - 1 do
    assert (not (Standard.mem table (key i)));
    assert (Standard.find table (key (i + n)) = (Iarray.get values i))
  done

let () =
  let name = Sys.argv.(1) in
  let n = int_of_string Sys.argv.(2) in
  let ints = Iarray.init n (fun i -> i) in
  let strings = Iarray.init n (fun i -> string_of_int i) in
  if name = "stdlib" then begin standard "int" ints; standard "string" strings
    end
  else begin verified name "int" ints; verified name "string" strings end
