(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/pvghost.mli ../lib/pvghost.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The inhabitation guard: a prophecy over 'a is minted only against a
   WITNESS of 'a (lib/pvghost's [new_proph : (w : 'a) -> 'a proph]).
   The old [new_proph ()] route to a prophecy over an uninhabited type
   is therefore closed -- [()] witnesses [unit], not [empty], so the
   ascription to [empty proph] is a type error and no empty-sorted
   prophecy can be created. *)

open Pvghost

type empty = |

let no_empty_proph () : empty proph @ unique = new_proph ()
