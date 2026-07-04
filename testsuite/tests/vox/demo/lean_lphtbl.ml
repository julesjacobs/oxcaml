(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/pslice.mli ../lib/pslice.ml ../lib/lphtbl.mli ../lib/lphtbl.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the LINEAR-PROBING hash table (lib/lphtbl over the
   polymorphic lib/pslice at int): every model fact arrives through
   the .cmis.  Keys 3 and 11 COLLIDE (both home to slot 3), so the
   second insert probes past an occupied slot and the hits below
   exercise a real probe chain.  The [hasfree] hypothesis of the hit
   theorem is DERIVED, never assumed: the fresh table's free count is
   8 ([freecnt_pconst]), each insert costs at most one
   ([freecnt_ins]), and positivity converts back ([freecnt_hasfree]).
   The miss walks the frame theorem [T3] down to the fresh table and
   [T4].  Ownership of both arrays threads [@ unique] through every
   call -- the writes are in place and unobservable. *)

open Lphtbl

let demo : opt{ _ = Found 7 } * opt{ _ = Found 5 } * opt{ _ = Missing } =
  let (ks, vs) = create () in
  let (ks, vs) = add 3 7 ks vs in
  let (ks, vs) = add 11 5 ks vs in
  let (hit3, ks, vs) = find 3 ks vs in
  let (hit11, ks, vs) = find 11 ks vs in
  let (miss, ks, vs) = find 4 ks vs in
  ignore ks;
  ignore vs;
  (hit3, hit11, miss)
