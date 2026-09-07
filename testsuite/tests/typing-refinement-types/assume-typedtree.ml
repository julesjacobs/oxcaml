(* TEST
 flags = "-extension refinement_types";
 include ocamlcommon;
 expect;
*)

let () = Language_extension.enable Refinement_types () in
let source =
  "let check (x : int) : {v : int | true} = assume_ x"
in
let parsed = Parse.implementation (Lexing.from_string source) in
let tree, _, _, _, _, _ =
  Typemod.type_structure (Lazy.force Env.initial) parsed
in
let assumes = ref 0 and conditionals = ref 0 in
let iterator =
  { Tast_iterator.default_iterator with
    expr = (fun self exp ->
      (match exp.Typedtree.exp_desc with
       | Texp_assume (binding, predicate, body) ->
           incr assumes;
           let ids = Typedtree.pat_bound_idents binding.vb_pat in
           (match ids, body.exp_desc, predicate.exp_desc with
            | [id], Texp_ident {path = Path.Pident result; _},
              Texp_construct (_, cstr, _, [], _) ->
                assert (Ident.same id result);
                assert (cstr.Data_types.cstr_name = "true")
            | _ -> assert false)
       | Texp_ifthenelse _ -> incr conditionals
       | _ -> ());
      Tast_iterator.default_iterator.expr self exp) }
in
iterator.structure iterator tree;
Format.printf "assumes=%d conditionals=%d@." !assumes !conditionals;
Format.printf "%a@." Pprintast.structure (Untypeast.untype_structure tree);;
[%%expect{|
assumes=1 conditionals=0
let check (x : int) = (assume_ x : {v : int | true})
- : unit = ()
|}]
