(* TEST
 flags = "-extension refinement_types -extension overwriting -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An overwrite hole keeps a ghost field's value, which is read like a kept
   field of a record update: ghost and total, whatever the record's
   totality. Overwriting is not yet translated, so this test stops after
   typing; it is accepted. *)
type 'a og = { on : int; og : 'a @@ ghost }

let keep_hole (r : (unit -> unit) og @ unique partial)
    : (unit -> unit) og @ unique =
  overwrite_ r with { on = 1; og = _ }
