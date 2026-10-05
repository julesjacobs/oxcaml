(* TEST
 include ocamlcommon;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/typing";
 native;
*)

let check_name name =
  let initial = Lazy.force Env.initial in
  let original = Env.find_type Predef.path_int initial in
  let changed = { original with Types.type_manifest = Some Predef.type_string } in
  let id = Ident.create_local name in
  let path = Path.Pident id in
  let first = Env.add_type ~check:false id original initial in
  let second = Env.add_type ~check:false id changed first in
  let constrained =
    Env.add_local_constraint ~stage:(Env.stage first) path changed first
  in
  let quoted = Env.enter_quote constrained in
  let shadow = Ident.create_local name in
  let shadowed = Env.add_type ~check:false shadow changed first in
  let check () =
    for _ = 1 to 10 do
      assert (Env.find_type path first == original);
      assert (Env.find_type path second == changed);
      assert (Env.find_type path first == original);
      assert (Env.find_type path constrained == changed);
      assert (Env.find_type path quoted == original);
      assert (Env.find_type path shadowed == original);
      assert (Env.find_type (Path.Pident shadow) shadowed == changed)
    done
  in
  check ();
  Env.reset_cache_toplevel ();
  check ();
  let store1 = Local_store.fresh () in
  let store2 = Local_store.fresh () in
  Local_store.with_store store1 check;
  Local_store.with_store store2 check;
  Local_store.with_store store1 (fun () -> Local_store.reset (); check ());
  Local_store.with_store store2 check

let () =
  check_name "t";
  check_name ""
