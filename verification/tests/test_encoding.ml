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
