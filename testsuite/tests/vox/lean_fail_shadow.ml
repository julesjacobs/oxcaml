(* TEST
 flags = "-vox-solver lean -vox-solver-path /nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Shadowing soundness: logical facts are keyed by stamp, never by
   name, so the rebound [a] must NOT inherit the outer [a]'s fact and
   this obligation must fail. *)

let shadow_unsound (a : {v:int | v > 0}) : {w:int | w > 0} =
  let a = 0 in
  refine_ a
