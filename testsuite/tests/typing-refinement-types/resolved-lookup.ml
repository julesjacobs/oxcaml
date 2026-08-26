(* TEST
 include ocamlcommon;
 expect;
*)

let parsed =
  Parse.implementation
    (Lexing.from_string "module M = struct let f x = x exception E end")
in
let _, _, _, _, _, env =
  Typemod.type_structure (Lazy.force Env.initial) parsed
in
let loc = Location.none in
let env = Env.add_region_lock env in
let original, md, (_, locks) =
  Env.lookup_module ~loc (Longident.Lident "M") env
in
let alias = Ident.create_local "Alias" in
let md = { md with Types.md_type = Types.Mty_alias original } in
let env =
  Env.add_module_declaration ~check:false alias Types.Mp_absent md
    ~locks env
in
let lid name =
  Longident.Ldot (Location.mknoloc (Longident.Lident "Alias"),
                 Location.mknoloc name)
in
let path name = Path.Pdot (Path.Pident alias, name) in
let _, _, (_, named_locks) = Env.lookup_value ~loc (lid "f") env in
let _, _, (_, resolved_locks) = Env.lookup_value_path ~loc (path "f") env in
assert (not (Env.locks_is_empty named_locks));
assert (not (Env.locks_is_empty resolved_locks));
let _, named_locks =
  Env.lookup_constructor ~loc Env.Positive (lid "E") env
in
let _, resolved_locks =
  Env.lookup_constructor_path ~loc Env.Positive (path "E") env
in
assert (not (Env.locks_is_empty named_locks));
assert (not (Env.locks_is_empty resolved_locks));
let shadow = Ident.create_local "Alias" in
let env = Env.add_module shadow Types.Mp_present (Types.Mty_signature []) env in
let found, _, _ = Env.lookup_value_path ~loc (path "f") env in
assert (Path.same found (path "f"));;
[%%expect{|
- : unit = ()
|}]
