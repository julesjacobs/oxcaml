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
