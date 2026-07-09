(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Gap A: a [%%vox.lean] block nested in a module body is UNIT-LEVEL
   material in the wrong place -- the collector reads only top-level
   items, so silently it would never reach the solver and [foo] would
   be undefined at VC time.  Rejected with a clear message instead. *)

module M = struct
  [%%vox.lean {lean|
  @[grind] def foo (x : Int) : Int := x + 1
  |lean}]

  let g : (x : int) -> int{ _ = foo x } = fun x -> x + 1
end
