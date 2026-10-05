(* TEST
 include ocamlcommon;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/file_formats";
 native;
*)

let missing path env =
  match Env.find_module path env with
  | _ -> failwith "Expected module lookup to fail"
  | exception Not_found -> ()

let check_local name =
  let id = Ident.create_local name in
  let path = Path.Pident id in
  let first = Env.add_module id Mp_present (Mty_signature []) Env.empty in
  let declaration = Env.find_module path first in
  let second = Env.add_module id Mp_absent (Mty_alias path) first in
  let shadow = Ident.create_local name in
  let shadowed = Env.add_module shadow Mp_absent (Mty_alias path) first in
  let check () =
    for _ = 1 to 10 do
      missing path Env.empty;
      assert ((Env.find_module path first).md_type = declaration.md_type);
      assert ((Env.find_module path second).md_type = Mty_alias path);
      assert ((Env.find_module path first).md_type = declaration.md_type);
      assert ((Env.find_module path shadowed).md_type = declaration.md_type);
      assert
        ((Env.find_module (Path.Pident shadow) shadowed).md_type = Mty_alias path);
      assert
        ((Env.find_module path (Env.enter_quote first)).md_type
         = declaration.md_type)
    done
  in
  check ();
  Env.reset_cache_toplevel ();
  check ();
  let first_store = Local_store.fresh () in
  let second_store = Local_store.fresh () in
  Local_store.with_store first_store check;
  Local_store.with_store second_store check;
  Local_store.with_store first_store (fun () -> Local_store.reset (); check ())

let () =
  check_local "M";
  check_local ""

let () =
  Env.reset_cache ~preserve_persistent_env:false;
  let name = Compilation_unit.Name.of_string "Cached_module" in
  let cu = Compilation_unit.create Compilation_unit.Prefix.empty name in
  let unit = Unit_info.make_dummy ~input_name:"cached_module.ml" cu in
  let id =
    Ident.create_global (Global_module.Name.create_no_args "Cached_module")
  in
  let path = Path.Pident id in
  let declaration = Env.find_type Predef.path_int (Lazy.force Env.initial) in
  let signature =
    Subst.Lazy.of_signature
      [Types.Sig_type (Ident.create_local "t", declaration, Trec_not, Exported)]
  in
  let loads = ref 0 in
  let available = ref true in
  Persistent_env.Persistent_signature.load :=
    (fun ~allow_hidden:_ ~unit_name ->
      incr loads;
      assert (Compilation_unit.Name.equal unit_name name);
      if not !available then None
      else
        Some
          { filename = "cached_module.cmi";
            visibility = Load_path.Visible { cmx_guaranteed = false };
            cmi =
              { cmi_name = name;
                cmi_kind = Normal { cmi_impl = cu; cmi_arg_for = None };
                cmi_globals = [||];
                cmi_sign = signature, Dynamic;
                cmi_params = [];
                cmi_crcs = [||];
                cmi_flags = [] } });
  Current_unit.set unit;
  missing path Env.empty;
  assert (!loads = 0);
  Current_unit.unset ();
  Env.without_cmis (missing path) Env.empty;
  assert (!loads = 0);
  (* Link Mtype to initialize Env.scrape_alias. *)
  ignore (Mtype.scrape_alias Env.empty (Mty_signature []));
  let alias = Ident.create_local "Alias" in
  let alias_env = Env.add_module alias Mp_absent (Mty_alias path) Env.empty in
  let member = Path.Pdot (Path.Pident alias, "t") in
  Env.without_cmis
    (fun () ->
      match Env.find_type member alias_env with
      | _ -> failwith "Expected alias member lookup without CMI loading to fail"
      | exception Not_found -> ()) ();
  assert (!loads = 0);
  ignore (Env.find_type member alias_env);
  ignore (Env.find_module path Env.empty);
  assert (!loads = 1);
  let explicit = Env.add_persistent_structure id Env.empty in
  Current_unit.set unit;
  missing path Env.empty;
  ignore (Env.find_module path explicit);
  assert (!loads = 1);
  Current_unit.unset ();
  Env.without_cmis (fun () -> ignore (Env.find_module path Env.empty)) ();
  available := false;
  Env.reset_cache ~preserve_persistent_env:false;
  missing path Env.empty;
  assert (!loads = 2);
  available := true;
  missing path Env.empty;
  assert (!loads = 2);
  Env.reset_cache_toplevel ();
  ignore (Env.find_module path Env.empty);
  assert (!loads = 3)
