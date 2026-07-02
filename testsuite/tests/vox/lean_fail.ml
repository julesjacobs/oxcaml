(* TEST
 flags = "-vox-solver lean -vox-solver-path /nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A false obligation must fail verification. *)

let bad : {v:int | v > 0} = refine_ 0
