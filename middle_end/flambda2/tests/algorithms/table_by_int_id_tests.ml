module Tables = Flambda2_algorithms.Table_by_int_id

module Test (Flags : sig
  val flags : int
end) =
struct
  module Element = struct
    type t =
      { key : int;
        payload : string
      }

    let flags = Flags.flags

    let print ppf t = Format.pp_print_int ppf t.key

    let hash t = if t.key land 1 = 0 then -1 else t.key land 3

    let equal t1 t2 = Int.equal t1.key t2.key
  end

  module Table = Tables.Make (Element)

  let create_element key : Element.t = { key; payload = string_of_int key }

  let missing table id =
    match Table.find table id with _ -> false | exception Not_found -> true

  let () =
    let table = Table.create () in
    let ids =
      Array.init 1024 (fun key -> Table.add table (create_element key))
    in
    Array.iteri
      (fun key id ->
        assert (Tables.Id.flags id = Flags.flags);
        assert (Table.find table id = create_element key);
        assert (Table.add table { Element.key; payload = "equal" } = id))
      ids;
    let missing_id = (Flags.flags lsl (Sys.int_size - 3)) lor 10_000 in
    assert (missing table missing_id);
    let exported =
      Table.export table ~iter:(fun f ->
          List.iter (fun index -> f ids.(index)) [0; 3; 127; 1023])
    in
    let imported : Table.serializable =
      Marshal.from_string (Marshal.to_string exported []) 0
    in
    let destination = Table.create () in
    ignore (Table.add destination (create_element 10_000));
    List.iter
      (fun key ->
        let element = Table.import imported ids.(key) in
        let id = Table.add destination element in
        assert (Table.find destination id = create_element key))
      [0; 3; 127; 1023]

  module Legacy_table = Hashtbl.Make (struct
    type t = int

    let hash t =
      let mixed = t lxor (t lsr 31) in
      let h = mixed * 0x27d4eb2f in
      h lxor (h lsr 29)

    let equal = Int.equal
  end)

  let () =
    let legacy = Legacy_table.create 0 in
    let entries = [13, 5; 65_537, 19; 999_999, 27] in
    let old_id index = (Flags.flags lsl (Sys.int_size - 3)) lor index in
    List.iter
      (fun (index, key) ->
        Legacy_table.add legacy (old_id index) (create_element key))
      entries;
    let imported : Table.serializable =
      Marshal.from_string (Marshal.to_string legacy []) 0
    in
    let destination = Table.create () in
    List.iter
      (fun (index, key) ->
        let element = Table.import imported (old_id index) in
        assert (element = create_element key);
        let id = Table.add destination element in
        assert (Table.find destination id = element))
      entries
end

module _ = Test (struct
  let flags = 0
end)

module _ = Test (struct
  let flags = 4
end)

module _ = Test (struct
  let flags = 7
end)

let () =
  let module Ids = Flambda2_identifiers.Int_ids in
  let null = Ids.Const.descr Ids.Const.const_null in
  Ids.reset ();
  ignore (Ids.Const.const_poison Flambda2_kinds.Flambda_kind.value "first");
  assert (Ids.Const.Descr.equal null (Ids.Const.descr Ids.Const.const_null))

module Physical_float = struct
  type t = Obj.t

  let flags = 0

  let print ppf x = Format.pp_print_float ppf (Obj.obj x)

  let hash _ = 0

  let equal t1 t2 = t1 == t2
end

module Physical_float_table = Tables.Make (Physical_float)

let () =
  let table = Physical_float_table.create () in
  let elements =
    List.init 1024 (fun i ->
        ref (Sys.opaque_identity (Obj.repr (float_of_string (string_of_int i)))))
  in
  let ids =
    List.map (fun element -> Physical_float_table.add table !element) elements
  in
  Gc.full_major ();
  List.iter2
    (fun element id ->
      let found = Physical_float_table.find table id in
      assert (Obj.repr found == Obj.repr !element);
      assert (Physical_float_table.add table found = id))
    elements ids;
  let equal_value =
    ref (Sys.opaque_identity (Obj.repr (float_of_string "17")))
  in
  let original = List.nth elements 17 in
  assert (!equal_value = !original);
  assert (Obj.repr !equal_value != Obj.repr !original);
  let id = Physical_float_table.add table !equal_value in
  assert (id <> List.nth ids 17);
  assert (Obj.repr (Physical_float_table.find table id) == Obj.repr !equal_value)
