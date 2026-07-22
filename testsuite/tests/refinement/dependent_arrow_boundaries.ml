(* TEST
 readonly_files = "\
   dependent_arrow_boundary_api.mli dependent_arrow_boundary_api.ml \
   dependent_arrow_boundary_reexport.mli \
   dependent_arrow_boundary_reexport.ml \
 ";
 setup-ocamlc.byte-build-env;
 module = "dependent_arrow_boundary_api.mli";
 ocamlc.byte;
 module = "dependent_arrow_boundary_api.ml";
 ocamlc.byte;
 module = "dependent_arrow_boundary_reexport.mli";
 ocamlc.byte;
 module = "dependent_arrow_boundary_reexport.ml";
 ocamlc.byte;
 flags = "-principal";
 module = "dependent_arrow_boundary_api.mli";
 ocamlc.byte;
 module = "dependent_arrow_boundary_api.ml";
 ocamlc.byte;
 module = "dependent_arrow_boundary_reexport.mli";
 ocamlc.byte;
 module = "dependent_arrow_boundary_reexport.ml";
 ocamlc.byte;
 flags = "";
 expect;
*)

#directory "ocamlc.byte";;
#load "dependent_arrow_boundary_api.cmo";;
#load "dependent_arrow_boundary_reexport.cmo";;

module Positive : sig end = struct
  let direct_step = Dependent_arrow_boundary_api.keep 3
  let direct : int{ _ = 3 } = direct_step 0

  let included_step = Dependent_arrow_boundary_reexport.keep 4
  let included : int{ _ = 4 } = included_step 0

  let alias_step = Dependent_arrow_boundary_reexport.Alias.keep 5
  let alias : int{ _ = 5 } = alias_step 0

  let choose_include flag =
    if flag
    then Dependent_arrow_boundary_api.keep
    else Dependent_arrow_boundary_reexport.keep

  let via_include_join : int{ _ = 6 } = choose_include true 6 0

  let choose_alias flag =
    if flag
    then Dependent_arrow_boundary_api.keep
    else Dependent_arrow_boundary_reexport.Alias.keep

  let via_alias_join : int{ _ = 7 } = choose_alias true 7 0

  module Arg = struct end

  module A1 = Dependent_arrow_boundary_api.Applicative (Arg)
  module A2 = Dependent_arrow_boundary_api.Applicative (Arg)
  let choose_a flag = if flag then A1.keep else A2.keep
  let a_step = choose_a true 11
  let a : int{ _ = 11 } = a_step 0

  module U1 = Dependent_arrow_boundary_api.Anonymous (struct end)
  module U2 = Dependent_arrow_boundary_api.Anonymous (struct end)
  let choose_u flag = if flag then U1.keep else U2.keep
  let u_step = choose_u true 12
  let u : int{ _ = 12 } = u_step 0

  module G1 = Dependent_arrow_boundary_api.Generative ()
  module G2 = Dependent_arrow_boundary_api.Generative ()
  let choose_g flag = if flag then G1.keep else G2.keep
  let g_step = choose_g true 13
  let g : int{ _ = 13 } = g_step 0
end

[%%expect {|
module Positive : sig end
|}]

module Direct_rejected : sig end = struct
  let bad : int{ _ = 9 } = Dependent_arrow_boundary_api.keep 8 0
end

[%%expect {|
Line 2, characters 6-64:
2 |   let bad : int{ _ = 9 } = Dependent_arrow_boundary_api.keep 8 0
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

module Applicative_rejected : sig end = struct
  module Arg = struct end
  module A = Dependent_arrow_boundary_api.Applicative (Arg)
  let bad_a : int{ _ = 15 } = A.keep 14 0
end

[%%expect {|
Line 4, characters 6-41:
4 |   let bad_a : int{ _ = 15 } = A.keep 14 0
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

module Anonymous_rejected : sig end = struct
  module U = Dependent_arrow_boundary_api.Anonymous (struct end)
  let bad_u : int{ _ = 16 } = U.keep 14 0
end

[%%expect {|
Line 3, characters 6-41:
3 |   let bad_u : int{ _ = 16 } = U.keep 14 0
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

module Generative_rejected : sig end = struct
  module G = Dependent_arrow_boundary_api.Generative ()
  let bad_g : int{ _ = 17 } = G.keep 14 0
end

[%%expect {|
Line 3, characters 6-41:
3 |   let bad_g : int{ _ = 17 } = G.keep 14 0
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]
