(* TEST
 readonly_files = "digest_variant_a.mli digest_variant_b.mli";
 setup-ocamlc.byte-build-env;

 (* Under -no-keep-locs the predicate's source locations are normalized to
    Location.none when the interface is marshaled, so shifting a comment (which
    moves every declaration's line and absolute char offset) must produce a
    byte-identical .cmi -- hence an identical interface CRC.  This pins that the
    predicate location traversal (Refinement.map_locs) reaches every embedded
    Location.t: module M mixes a string, an int, and a nested-string predicate,
    and the string cases exercise the Location.t inside a Const_string constant
    that an earlier fix initially missed.  If a future change adds a new
    location-carrying node the traversal skips, this comparison fails.

    Only the -no-keep-locs direction is asserted here.  Under the default
    (-keep-locs) locations are deliberately kept for predicates as for every
    other declaration, so the same comment shift changes the .cmi; that
    direction is verified out of band (a boot-compiler CRC experiment) and is
    not asserted here because the ocamltest [script] action has no clean
    "files differ" form. *)
 script = "cp digest_variant_a.mli m.mli";
 script;
 flags = "-no-keep-locs";
 module = "m.mli";
 ocamlc.byte;
 script = "mv m.cmi nokeep_a.cmi";
 script;
 script = "cp digest_variant_b.mli m.mli";
 script;
 flags = "-no-keep-locs";
 module = "m.mli";
 ocamlc.byte;
 program = "m.cmi";
 program2 = "nokeep_a.cmi";
 compare-binary-files;
*)
