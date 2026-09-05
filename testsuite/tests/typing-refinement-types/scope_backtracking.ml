(* TEST
 include ocamlcommon;
 expect;
*)

open Types

let run () =
  let ref_desc =
    { ref_structural_scope = 0;
      ref_binder = Ident.create_local "x";
      ref_payload = Predef.type_int;
      ref_pred =
        { rexp_desc =
            Rexp_construct
              (Path.Pextra_ty (Predef.path_bool, Path.Pcstr_ty "true"), []);
          rexp_type = Predef.type_bool;
          rexp_type_constraint = false;
          rexp_loc = Location.none } }
  in
  let ty = Btype.newty2 ~level:10 (Trefine ref_desc) in
  let variable = Ctype.newvar2 10 (Jkind.Builtin.any ~why:Dummy_jkind) in
  set_scope variable 5;
  let before = Subst.type_expr Subst.identity ty in
  let print_scope label =
    let structural =
      match get_desc ty with
      | Trefine r -> r.ref_structural_scope
      | _ -> assert false
    in
    Format.printf "%s: scope=%d structural=%d@."
      label (get_scope ty) structural
  in
  let snapshot = Btype.snapshot () in
  Ctype.unify Env.empty variable ty;
  print_scope "after unify";
  Btype.backtrack snapshot;
  print_scope "after backtrack";
  let copy = Subst.type_expr Subst.identity ty in
  Format.printf "after copy: scope=%d@." (get_scope copy);
  let try_use label t =
    let variable = Ctype.newvar2 1 (Jkind.Builtin.any ~why:Dummy_jkind) in
    try
      Ctype.unify Env.empty variable t;
      Format.printf "%s: accepted@." label
    with Ctype.Unify _ -> Format.printf "%s: rejected@." label
  in
  try_use "copy before trial" before;
  try_use "copy after rolled-back trial" copy
;;
[%%expect{|
val run : unit -> unit = <fun>
|}]

let () = run ();;
[%%expect{|
after unify: scope=5 structural=5
after backtrack: scope=0 structural=0
after copy: scope=0
copy before trial: accepted
copy after rolled-back trial: accepted
|}]
