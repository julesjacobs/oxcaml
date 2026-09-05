let value_jkind =
  Jkind.Builtin.value ~why:(Jkind.History.Unknown "Vox encoding test")

let () =
  let context = Vox_encoding.create_context () in
  let weak = Ctype.newvar2 Btype.lowest_level value_jkind in
  assert (Vox_encoding.sort context Env.empty weak = None);
  let generic = Btype.newgenvar value_jkind in
  match Vox_encoding.sort context Env.empty generic with
  | Some (Vox_smt.Opaque _) -> ()
  | Some _ | None -> failwith "generic type variable was not opaque"

let () =
  let context = Vox_encoding.create_context () in
  let int_iarray = Predef.type_iarray Predef.type_int in
  let nested_iarray = Predef.type_iarray int_iarray in
  let sort = Option.get (Vox_encoding.sort context Env.empty int_iarray) in
  assert (Vox_encoding.is_iarray_sort context sort);
  assert (Vox_encoding.sort context Env.empty nested_iarray = Some sort);
  assert (
    Vox_encoding.iarray context Env.empty nested_iarray = Some (sort, Some sort))

let () =
  let context = Vox_encoding.create_context () in
  let env = Lazy.force Env.initial in
  let int_list = Predef.type_list Predef.type_int in
  let data = Option.get (Vox_encoding.data context env int_list) in
  let datatype = data.declaration.datatype in
  let constructors = data.declaration.constructors in
  assert (List.map Vox_smt.Constructor.label constructors = ["[]"; "::"]);
  let cons = List.nth constructors 1 in
  begin match Vox_smt.Constructor.fields cons with
  | [(_, Vox_smt.Int63); (_, Vox_smt.Datatype tail)] -> assert (tail = datatype)
  | _ -> assert false
  end;
  let nested = Predef.type_list int_list in
  let nested_data = Option.get (Vox_encoding.data context env nested) in
  assert (List.length (Vox_encoding.declarations context nested_data) = 2);
  let bool_list = Predef.type_list Predef.type_bool in
  let bool_data = Option.get (Vox_encoding.data context env bool_list) in
  assert (bool_data.declaration.datatype <> datatype)

let () =
  let context = Vox_encoding.create_context () in
  let env = Lazy.force Env.initial in
  let element = Btype.newgenvar value_jkind in
  let data =
    Option.get (Vox_encoding.data context env (Predef.type_list element))
  in
  let datatype = data.declaration.datatype in
  let cons = List.nth data.declaration.constructors 1 in
  match Vox_smt.Constructor.fields cons with
  | [(_, Vox_smt.Opaque _); (_, Vox_smt.Datatype tail)] ->
    assert (tail = datatype)
  | _ -> assert false
