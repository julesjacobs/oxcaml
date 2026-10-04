module Key = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = x = y
  let (hash @ total) (x : int) =
    let x = (x lxor Int.Refined.(x lsr 16)) * 73244475 in
    let x = (x lxor Int.Refined.(x lsr 16)) * 73244475 in
    x lxor Int.Refined.(x lsr 16)
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
module Standard = Hashtbl.Make (Key)

let work_factor = if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3)
  else 1

let key i = i * 104729
let queries n =
  let indices = Array.init n Fun.id in
  let rng = Random.State.make [|1729|] in
  for i = n - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let x = indices.(i) in indices.(i) <- indices.(j); indices.(j) <- x
  done;
  Iarray.init n (fun i -> key indices.(i)),
  Iarray.init n (fun i -> key (n + indices.(i)))

let stamp () = Gc.full_major (); (Gc.allocated_bytes (), Sys.time ())
let report name payload n operation count (bytes, time) =
  let elapsed = Sys.time () -. time in
  let allocated = Gc.allocated_bytes () -. bytes in
  Printf.printf "%s,%s,%d,%s,%.3f,%.3f\n%!" name payload n operation
    (elapsed *. 1e9 /. float count) (allocated /. float count)

let rec fill : ('a : immutable_data).
    (table : 'a V.t) @ immutable -> (values : 'a iarray) @ immutable -> (index
      : int) ->
    (p : {p : 'a V.permission | V.owner p === table}) @ unique read_write ->
    {q : 'a V.permission | V.owner q === table} @ unique =
  fun table values index p ->
    if index >= Iarray.length values then p else begin
      let changed = V.replace table (key index) (Iarray.get values index)
        p in
      fill table values (index + 1) changed
    end

let rec churn : ('a : immutable_data).
    (table : 'a V.t) @ immutable -> (values : 'a iarray) @ immutable -> (index
      : int) -> (offset : int) ->
    (p : {p : 'a V.permission | V.owner p === table}) @ unique read_write ->
    {q : 'a V.permission | V.owner q === table} @ unique =
  fun table values index offset p ->
    if index >= Iarray.length values then p else begin
      let removed = V.remove table (key (index + offset)) p in
      let changed =
        V.replace table (key (index + Iarray.length values - offset))
          (Iarray.get values index) removed in
      churn table values (index + 1) offset changed
    end

let rec churn_rounds : ('a : immutable_data).
    (table : 'a V.t) @ immutable -> (values : 'a iarray) @ immutable -> (index
      : int) -> (offset : int) ->
    (p : {p : 'a V.permission | V.owner p === table}) @ unique read_write ->
    {q : 'a V.permission | V.owner q === table} @ unique =
  fun table values index offset p ->
    if index = 0 then p else
    let changed = churn table values 0 offset p in
    churn_rounds table values (index - 1) (Iarray.length values - offset)
      changed

let rec replace_rounds : ('a : immutable_data).
    (table : 'a V.t) @ immutable -> (values : 'a iarray) @ immutable -> (index
      : int) ->
    (p : {p : 'a V.permission | V.owner p === table}) @ unique read_write ->
    {q : 'a V.permission | V.owner q === table} @ unique =
  fun table values index p ->
    if index = 0 then p else
    let changed = fill table values 0 p in
    replace_rounds table values (index - 1)
      changed

let verified : ('a : immutable_data).
    string -> string -> ('a iarray) @ immutable -> unit = fun name payload
      values ->
  let n = Iarray.length values in
  let batches = max 1 (100000 * work_factor / n) in
  let timing = stamp () in
  for _batch = 1 to batches do
    let r : 'a V.created = V.create () in
    let _ = fill r.#table values 0 r.#permission in
    ignore (Sys.opaque_identity r.#table)
  done;
  report name payload n "build" (batches * n) timing;
  let r : 'a V.created = V.create () in
  let built = fill r.#table values 0 r.#permission in
  for i = 0 to n - 1 do
    assert (V.find r.#table (key i) (borrow_ built) =
      (Iarray.get values i))
  done;
  let hits, misses = queries n in
  let rounds = max 1 (1000000 * work_factor / n) in
  let iterations = rounds * n in
  let timing = stamp () in
  for _round = 1 to rounds do
    for i = 0 to n - 1 do
      ignore (Sys.opaque_identity
        (V.find r.#table (Iarray.get hits i) (borrow_ built)))
    done
  done;
  report name payload n "hit" iterations timing;
  let timing = stamp () in
  let found = ref 0 in
  for _round = 1 to rounds do
    for i = 0 to n - 1 do
      if V.mem r.#table (Iarray.get misses i) (borrow_ built)
        then
        incr found
    done
  done;
  report name payload n "miss" iterations timing;
  assert (!found = 0);
  let timing = stamp () in
  let built = replace_rounds r.#table values batches built in
  report name payload n "replace" (batches * n) timing;
  let timing = stamp () in
  let changed = churn_rounds r.#table values batches 0
    built in
  report name payload n "churn" (2 * n * batches) timing;
  let offset = if batches mod 2 = 0 then 0 else n in
  for i = 0 to n - 1 do
    assert (not (V.mem r.#table (key (i + n - offset))
      (borrow_ changed)));
    assert (V.find r.#table (key (i + offset)) (borrow_
      changed) = (Iarray.get values i))
  done

let standard payload values =
  let n = Iarray.length values in
  let batches = max 1 (100000 * work_factor / n) in
  let timing = stamp () in
  for _batch = 1 to batches do
    let table = Standard.create 16 in
    for i = 0 to n - 1 do Standard.replace table (key i) (Iarray.get values i)
      done;
    ignore (Sys.opaque_identity table)
  done;
  report "stdlib" payload n "build" (batches * n) timing;
  let table = Standard.create 16 in
  for i = 0 to n - 1 do Standard.replace table (key i) (Iarray.get values i)
    done;
  for i = 0 to n - 1 do assert (Standard.find table (key i) = (Iarray.get
    values i)) done;
  let hits, misses = queries n in
  let rounds = max 1 (1000000 * work_factor / n) in
  let iterations = rounds * n in
  let timing = stamp () in
  for _round = 1 to rounds do
    for i = 0 to n - 1 do
      ignore (Sys.opaque_identity (Standard.find table (Iarray.get hits i)))
    done
  done;
  report "stdlib" payload n "hit" iterations timing;
  let timing = stamp () in
  let found = ref 0 in
  for _round = 1 to rounds do
    for i = 0 to n - 1 do
      if Standard.mem table (Iarray.get misses i) then incr found
    done
  done;
  report "stdlib" payload n "miss" iterations timing;
  assert (!found = 0);
  let timing = stamp () in
  for _batch = 1 to batches do
    for i = 0 to n - 1 do
      Standard.replace table (key i) (Iarray.get values i)
    done
  done;
  report "stdlib" payload n "replace" (batches * n) timing;
  let timing = stamp () in
  for batch = 0 to batches - 1 do
    let offset = if batch mod 2 = 0 then 0 else n in
    for i = 0 to n - 1 do
      Standard.remove table (key (i + offset));
      Standard.replace table (key (i + n - offset)) (Iarray.get values i)
    done
  done;
  report "stdlib" payload n "churn" (2 * n * batches) timing;
  let offset = if batches mod 2 = 0 then 0 else n in
  for i = 0 to n - 1 do
    assert (not (Standard.mem table (key (i + n - offset))));
    assert (Standard.find table (key (i + offset)) = (Iarray.get values i))
  done

let () =
  let name = Sys.argv.(1) in
  let n = int_of_string Sys.argv.(2) in
  let ints = Iarray.init n (fun i -> i) in
  let strings = Iarray.init n (fun i -> string_of_int i) in
  if name = "stdlib" then begin standard "int" ints; standard "string" strings
    end
  else begin verified name "int" ints; verified name "string" strings end
