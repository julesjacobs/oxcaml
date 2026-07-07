(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Task #53 SOUNDNESS: inlining a reflectable dependent argument must
   not let a FALSE relation slip through.  [f_l (ICons (x, t))] has
   result [len (ICons (x, t))] = [1 + len t]; claiming it equals [len t]
   is refuted with a validated counterexample (t = INil, result = 1). *)

type ilist = INil | ICons of int * ilist

let rec total_ len (l : ilist) : int =
  match l with
  | INil -> 0
  | ICons (_, t) -> 1 + len t

let f_l : (l : ilist) -> int{ _ = len l } = fun l -> len l

let bad (x : int) (t : ilist) : int{ _ = len t } = f_l (ICons (x, t))
