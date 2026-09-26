(* TEST
 has-z3;
 ocamlrunparam += ",l=262144";
 flags = "-g -extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_table_model.ml vox_table_model_proofs.ml vox_table_bits.ml vox_table_probe.ml vox_table_wrap.ml vox_table_mask.ml vox_table_map.ml vox_table_invariant.ml vox_table_initial.ml vox_table_update_proofs.ml vox_table_insert_proofs.ml vox_table_migration_proofs.ml vox_table_read_proofs.ml vox_table_search_spec.ml vox_table_stop_proof.ml pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_table_storage.mli vox_table_storage.ml vox_table_search.ml vox_table_mutation.ml vox_table_coverage.ml vox_table_occupancy.ml vox_table_vacancy_progress.ml vox_table_vacancy.ml vox_table_insert.ml vox_table_migrate.ml vox_table_resize.ml vox_table_implementation.ml vox_table_bindings.ml vox_table_bindings_bridge.ml vox_verified_flat_hashtbl.mli vox_verified_flat_hashtbl.ml table_model.ml";
 { native; }
*)
let () =
  let model = Vox_table_model.initial 16 (None : (int * int) option) in
  assert (model.capacity = 16);
  assert (List.length model.slots = 16);
  assert (List.length model.controls = 31)

module T = Vox_table_storage
module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model
module L = Vox_table_model_proofs

let () =
  let r : ((int, int) T.t, (int, int) M.state) P.step = T.create 16 (P.empty ()) in
  let table = r.value in
  let before = ghost_ (M.initial 16 (None : (int * int) option)) in
  let view = {T.model = before} in
  ghost_ (M.initial_def 16 (None : (int * int) option));
  let capacity : {n : int | n = 16} =
    T.capacity table view (borrow_ r.state) in
  assert (capacity = 16);
  ghost_ (L.initial_slot before 16 None 3);
  let answer = 84 in
  let state = T.write_slot table view 3 42 answer r.state in
  let written = ghost_ (M.set_slot before 3 (Some (42, answer))) in
  let view = {T.model = written} in
  ghost_ (
    M.set_slot_def before 3 (Some (42, answer));
    L.slot_write before 3 (Some (42, answer)) 3);
  let key : {k : int | k = 42} = T.read_key table view 3 (borrow_ state) in
  let value : {v : int | v = answer} =
    T.read_value table view 3 (borrow_ state) in
  assert (key = 42 && value = 84);
  Gc.full_major (); Gc.compact ();
  let value = T.read_value table view 3 (borrow_ state) in
  assert (value = 84);
  let state = T.write_control table view 3 7 state in
  let controls = ghost_ (M.set_control written 3 7) in
  let view = {T.model = controls} in
  ghost_ (
    M.set_control_def written 3 7;
    M.control_def written 3;
    L.initial_control before 16 None 3;
    M.control_def before 3;
    L.control_write written 3 7 3);
  let byte : {n : int | n = 7} =
    T.read_control table view 3 (borrow_ state) in
  assert (byte = 7);
  let mask = T.match16 table view 0 7 (borrow_ state) in
  assert (mask = 8);
  let scanned = T.match16_empty table view 0 7 (borrow_ state) in
  assert (scanned = 65544);
  let state = T.clear table view state in
  let view = {T.model = before} in
  let size : {n : int | n = 0} = T.size table view (borrow_ state) in
  assert (size = 0);
  let mask = T.match16 table view 0 128 (borrow_ state) in
  assert (mask = 65535)

let (ctz_zero @ total) () :
    {n : int | n = 63} = Vox_table_bits.count_trailing_zeros 0

let (ctz_negative @ total) () :
    {n : int | n = 0} = Vox_table_bits.count_trailing_zeros (-1)

let () =
  assert (ctz_zero () = 63);
  assert (ctz_negative () = 0);
  for bit = 0 to 62 do
    let mask = 1 lsl bit in
    assert (Vox_table_bits.count_trailing_zeros mask = bit);
    assert (Vox_table_bits.count_trailing_zeros (-mask) = bit)
  done

let () =
  for mask = 1 to 65535 do
    let first = Vox_table_bits.first mask in
    assert (mask land M.lane_bit first <> 0);
    assert (mask land (M.lane_bit first - 1) = 0)
  done

module Int_key = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = x = y
  let[@def] (hash @ total) (x : int) = x
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
    equal_def x y; hash_def x; hash_def y; ()
end

module Search = Vox_table_search.Make (Int_key)
module I = Search.I
module Initial = Vox_table_initial.Make (Int_key) (I)
module Probe = Vox_table_probe
module Wrap = Vox_table_wrap

let () =
  let r : ((int, int) T.t, (int, int) M.state) P.step = T.create 16 (P.empty ()) in
  let model = ghost_ (M.initial 16 (None : (int * int) option)) in
  let view = {I.model; routes = M.repeat 16 (0, 0); plan = Probe.One} in
  ghost_ (
    Probe.valid_def Probe.One;
    Probe.groups_def Probe.One;
    Wrap.scale16_def 1;
    M.initial_def 16 (None : (int * int) option);
    Initial.initial 16 None view;
    I.Map.empty_lookup 16 (None : (int * int) option) 42);
  let answer : {v : int option | v === None} =
    Search.find_opt r.value view 42 (borrow_ r.state) in
  assert (answer = None)

module Mutation = Vox_table_mutation.Make (Int_key)

let () =
  let first : int Mutation.created = Mutation.create (P.empty ()) in
  let second : bool Mutation.created = Mutation.create (P.empty ()) in
  let alias = first.table in
  let cleared = Mutation.clear alias first.view first.state in
  ghost_ (
    M.initial_def 16 (None : (int * int) option);
    Mutation.I.Map.empty_lookup 16 (None : (int * int) option) 42);
  let answer : {v : int option | v === None} =
    Mutation.Search.find_opt first.table cleared.#view 42 (borrow_ cleared.#state) in
  assert (answer = None);
  ghost_ (
    M.initial_def 16 (None : (int * bool) option);
    Mutation.I.Map.empty_lookup 16 (None : (int * bool) option) 42);
  let answer : {v : bool option | v === None} =
    Mutation.Search.find_opt second.table second.view 42 (borrow_ second.state) in
  assert (answer = None)

module Insert = Vox_table_insert.Make (Int_key)
module Mut = Insert.Mutation
module Inv = Insert.I

let () =
  let r : int Mut.created = Mut.create (P.empty ()) in
  ghost_ (
    M.initial_def 16 (None : (int * int) option);
    L.initial_slot r.view.model 16 (None : (int * int) option) 5;
    L.initial_control r.view.model 16 (None : (int * int) option) 5;
    Inv.Map.empty_lookup 16 (None : (int * int) option) 5;
    Inv.Map.absent_lookup r.view.model.slots 5;
    Inv.reserve_def 16;
    Int_key.hash_def 5;
    Inv.route_def r.view.model 5Z (Some (5, 84)) (0, 5);
    Inv.route_position_def 16 5 5Z (0, 5);
    Inv.group_def 16 5 0; Inv.probe_def 16 5 0;
    Inv.wrap_def 16 0; Inv.wrap_def 16 5;
    Inv.empty_free_def r.view.model 5 0);
  let inserted = Insert.write_new r.table r.view 5 5 84 128 (ghost_ (0, 5)) r.state in
  ghost_ (
    Inv.Map.same_get inserted.#view.model.slots (Inv.Map.put r.view.model.slots 5 84) 5;
    Inv.Map.put_get r.view.model.slots 5 84 5;
    Int_key.reflexive 5);
  let answer : {v : int option | v === Some 84} =
    Mut.Search.find_opt r.table inserted.#view 5 (borrow_ inserted.#state) in
  assert (answer = Some 84);
  let removed = Mut.remove r.table inserted.#view 5 inserted.#state in
  ghost_ (
    Inv.Map.same_get removed.#view.model.slots (Inv.Map.erase inserted.#view.model.slots 5) 5;
    Inv.Map.erase_get inserted.#view.model.slots 5 5;
    Int_key.reflexive 5);
  let answer : {v : int option | v === None} =
    Mut.Search.find_opt r.table removed.#view 5 (borrow_ removed.#state) in
  assert (answer = None)

module Verified = Vox_table_implementation.Make (Int_key)
module VI = Verified.Spec
module VM = Verified

let rec fill :
    (table : (int, int) T.t) @ immutable ->
    (view : {v : int VI.view | VI.valid v}) @ immutable -> (index : int) ->
    (token : {t : (int, int) M.state P.token | H.at (P.own t) (T.location table) === Some view.model})
      @ unique read_write ghost ->
    {r : int VM.result | VI.valid r.#view &&
      H.at (P.own r.#state) (T.location table) === Some r.#view.model} @ unique =
  fun table view index token ->
    if index >= 41 then #{VM.view; state = token} else begin
      let changed = Verified.replace table view index (index + 7) token in
      fill table changed.#view (index + 1) changed.#state
    end

let () =
  let r : int VM.created = Verified.create (P.empty ()) in
  let populated = fill r.table r.view 0 r.state in
  for key = 0 to 40 do
    assert (Verified.find r.table populated.#view key (borrow_ populated.#state) = key + 7)
  done;
  assert (T.capacity r.table {T.model = populated.#view.model}
    (borrow_ populated.#state) = 64);
  let changed = Verified.replace r.table populated.#view 5 900 populated.#state in
  ghost_ (
    VI.Map.same_get changed.#view.model.slots (VI.Map.put populated.#view.model.slots 5 900) 5;
    VI.Map.put_get populated.#view.model.slots 5 900 5;
    Int_key.reflexive 5);
  let value : {v : int | v = 900} =
    Verified.find r.table changed.#view 5 (borrow_ changed.#state) in
  assert (value = 900);
  let removed = Verified.remove r.table changed.#view 5 changed.#state in
  assert (not (Verified.mem r.table removed.#view 5 (borrow_ removed.#state)));
  let removed = Verified.remove r.table removed.#view 500 removed.#state in
  let cleared = Verified.clear r.table removed.#view removed.#state in
  assert (T.size r.table {T.model = cleared.#view.model} (borrow_ cleared.#state) = 0);
  assert (not (Verified.mem r.table cleared.#view 10 (borrow_ cleared.#state)))

module Reference = Hashtbl.Make (struct
  type t = int
  let equal = Int.equal
  let hash x = x
end)

let rec random_operations :
    (table : int Verified.t) @ immutable -> int Reference.t -> int ->
    (view : {v : int VI.view | VI.valid v}) @ immutable ->
    (token : {t : (int, int) M.state P.token | H.at (P.own t) (T.location table) === Some view.model})
      @ unique read_write ghost ->
    {r : int Verified.result | VI.valid r.#view &&
      H.at (P.own r.#state) (T.location table) === Some r.#view.model} @ unique =
  fun table reference remaining view token ->
    if remaining <= 0 then #{Verified.view; state = token} else begin
      let key = Random.int 97 in
      if remaining mod 113 = 0 then begin Gc.full_major (); Gc.compact () end;
      match Random.int 4 with
      | 0 | 1 ->
        let value = Random.int 10000 in
        Reference.replace reference key value;
        let changed = Verified.replace table view key value token in
        assert (Verified.length table changed.#view (borrow_ changed.#state) =
          Reference.length reference);
        random_operations table reference (remaining - 1) changed.#view changed.#state
      | 2 ->
        Reference.remove reference key;
        let changed = Verified.remove table view key token in
        random_operations table reference (remaining - 1) changed.#view changed.#state
      | _ ->
        assert (Verified.find_opt table view key (borrow_ token) =
          Reference.find_opt reference key);
        random_operations table reference (remaining - 1) view token
    end

let () =
  Random.init 20260919;
  let r : int Verified.created = Verified.create (P.empty ()) in
  let reference = Reference.create 16 in
  let changed = random_operations r.table reference 10000 r.view r.state in
  for key = 0 to 96 do
    assert (Verified.find_opt r.table changed.#view key (borrow_ changed.#state) =
      Reference.find_opt reference key)
  done

let rec pointer_fill :
    (table : (int * string) Verified.t) @ immutable -> (index : int) ->
    (view : {v : (int * string) VI.view | VI.valid v}) @ immutable ->
    (token : {t : (int, int * string) M.state P.token | H.at (P.own t) (T.location table) === Some view.model})
      @ unique read_write ghost ->
    {r : (int * string) Verified.result | VI.valid r.#view &&
      P.own r.#state === H.put (P.own token) (T.location table) r.#view.model} @ unique =
  fun table index view token ->
    if index >= 300 then begin
      ghost_ (H.put_law (P.own token) (T.location table) view.model view.model);
      #{Verified.view; state = token}
    end else begin
      let heap = ghost_ (P.own (borrow_ token)) in
      let changed = Verified.replace table view index (index, string_of_int index) token in
      if index mod 17 = 0 then begin Gc.full_major (); Gc.compact () end;
      let result = pointer_fill table (index + 1) changed.#view changed.#state in
      ghost_ (H.put_law heap (T.location table) changed.#view.model result.#view.model);
      #{Verified.view = result.#view; state = result.#state}
    end

let () =
  let first : (int * string) Verified.created = Verified.create (P.empty ()) in
  let other : bool Verified.created = Verified.create (P.empty ()) in
  let alias = first.table in
  let changed = pointer_fill alias 0 first.view first.state in
  for key = 0 to 299 do
    assert (Verified.find first.table changed.#view key (borrow_ changed.#state) =
      (key, string_of_int key))
  done;
  assert (not (Verified.mem other.table other.view 42 (borrow_ other.state)))

let rec stress_fill :
    (table : int Verified.t) @ immutable -> (index : int) ->
    (view : {v : int VI.view | VI.valid v}) @ immutable ->
    (token : {t : (int, int) M.state P.token | H.at (P.own t) (T.location table) === Some view.model})
      @ unique read_write ghost ->
    {r : int Verified.result | VI.valid r.#view &&
      H.at (P.own r.#state) (T.location table) === Some r.#view.model} @ unique =
  fun table index view token ->
    if index >= 65536 then #{Verified.view; state = token} else begin
      let changed = Verified.replace table view (index * 104729) index token in
      stress_fill table (index + 1) changed.#view changed.#state
    end

let () =
  let created : int Verified.created = Verified.create (P.empty ()) in
  let filled = stress_fill created.table 0 created.view created.state in
  assert (Verified.length created.table filled.#view (borrow_ filled.#state) = 65536);
  for index = 0 to 65535 do
    assert (Verified.find created.table filled.#view (index * 104729)
      (borrow_ filled.#state) = index)
  done

let () =
  let r : string Verified.created = Verified.create (P.empty ()) in
  let changed = Verified.replace r.table r.view 7 "hello" r.state in
  ghost_ (
    VI.Map.same_get changed.#view.model.slots
      (VI.Map.put r.view.model.slots 7 "hello") 7;
    VI.Map.put_get r.view.model.slots 7 "hello" 7;
    Int_key.reflexive 7);
  Gc.full_major (); Gc.compact ();
  let value : {s : string | s === "hello"} =
    Verified.find r.table changed.#view 7 (borrow_ changed.#state) in
  assert (value = "hello")

let () =
  let calls = ref 0 in
  let token = P.empty (incr calls) in
  assert (!calls = 1);
  let r : int Verified.created = Verified.create token in
  assert (Verified.length r.table r.view (borrow_ r.state) = 0)

let () =
  let r : (int * string) Verified.created = Verified.create (P.empty ()) in
  let first_pass = pointer_fill r.table 0 r.view r.state in
  Gc.full_major ();
  let replaced = pointer_fill r.table 0 first_pass.#view first_pass.#state in
  Gc.compact ();
  for key = 0 to 299 do
    assert (Verified.find r.table replaced.#view key (borrow_ replaced.#state)
      = (key, string_of_int key))
  done

let () =
  let r : int VM.created = Verified.create (P.empty ()) in
  let populated = fill r.table r.view 0 r.state in
  let removed = Verified.remove r.table populated.#view 5 populated.#state in
  let replaced = Verified.replace r.table removed.#view 35 900 removed.#state in
  assert (Verified.length r.table replaced.#view (borrow_ replaced.#state) = 40);
  assert (Verified.find r.table replaced.#view 35 (borrow_ replaced.#state) = 900);
  assert (not (Verified.mem r.table replaced.#view 5 (borrow_ replaced.#state)));
  let inserted = Verified.replace r.table replaced.#view 97 901 replaced.#state in
  assert (Verified.length r.table inserted.#view (borrow_ inserted.#state) = 41);
  assert (Verified.find r.table inserted.#view 97 (borrow_ inserted.#state) = 901);
  assert (T.deleted r.table {T.model = inserted.#view.model}
    (borrow_ inserted.#state) = 0)
