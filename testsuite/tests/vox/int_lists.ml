(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "int_list_laws.mli int_list_laws.ml";
 readonly_files = "int_lists.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   binary_modules = "int_list_laws";
   run-expect;
   check-program-output;
 }
*)

let () =
  let module Int_list = Int_list_laws in
  let open Int_list in
  let module Laws = Int_list_laws.Laws
  in
  let xs = Cons (1, Cons (2, Nil)) in
  let ys = Cons (3, Cons (4, Cons (5, Nil))) in
  let zs = ghost_ (Cons (6, Nil)) in
  ghost_ (Laws.append_nil_left xs);
  ghost_ (Laws.append_nil_right xs);
  ghost_ (Laws.append_associative xs ys zs);
  ghost_ (Laws.length_append xs ys);
  ghost_ (Laws.sum_append xs ys);
  let result = append xs ys in
  Format.printf "length = %d, sum = %d@." (length result) (sum result);;
[%%expect{|
length = 5, sum = 15
|}]
