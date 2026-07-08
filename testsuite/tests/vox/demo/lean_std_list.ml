(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/voption.mli ../lib/voption.ml ../lib/Vlist.mli ../lib/Vlist.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: a client of the vox_stdlib [Vlist], both its first-order core and
   its WP-0 higher-order surface.  [Vlist] depends on [Voption] (find_opt's
   result type), so both are snapshotted into ../lib and listed in
   dependency order.  Every law arrives through Vlist's .cmi / VoxSig. *)

[@@@warning "-6-26-27"]

open Vlist

(* First-order: the textbook length facts (ll_len_cons / ll_len_app). *)
let len_cons (x : int) (l : Vlist.t) : int{ _ = 1 + ll_len l } =
  length (cons x l)
let len_app (a : Vlist.t) (b : Vlist.t) : int{ _ = ll_len a + ll_len b } =
  length (append a b)

(* nth: the stdlib ships a TOTAL nth (0 outside range); index 0 of a cons
   is its head (ll_nth_cons).  Contrast demo/lean_nth, whose hand-rolled
   nth is BOUND-CARRYING (safe, no default, dead Nil arm) -- a stronger
   contract the stdlib does not yet offer, so lean_nth stays as-is. *)
let nth0 (x : int) (l : Vlist.t) : int{ _ = x } = nth 0 (cons x l)

(* HOF surface (WP-0): the relation / predicate are supplied as call-site
   lambdas (total, reflected + substituted); refinement GOALS name the
   block abbrevs (a lambda may not appear in refinement text). *)
[%%vox.lean {lean|
@[grind, expose] abbrev pPos : Int -> Prop := fun x => x > 0
|lean}]

(* map preserves length (ll_listRel_len on the listRel contract). *)
let map_len (l : Vlist.t) : int{ _ = ll_len l } =
  length (map (fun a b -> a <= b) (fun x -> x + 1) l)

(* filter keeps only elements satisfying p (ll_allP). *)
let filter_pos (l : Vlist.t) : Vlist.t{ ll_allP pPos _ } =
  filter (fun x -> x > 0) (fun x -> x > 0) l

(* for_all: the bool result equals the lifted predicate over the list. *)
let all_pos (l : Vlist.t) : bool{ _ = ll_allP pPos l } =
  for_all (fun x -> x > 0) (fun x -> x > 0) l
