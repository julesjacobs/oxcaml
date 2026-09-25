(* TEST
 flags += "-extension mode_polymorphism_alpha";
 setup-ocamlopt.byte-build-env;
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

module Producer = struct
  let f x y = y
end

module Good_client : module type of Producer = struct
  let f x y = y
end

module Bad_client : module type of Producer = struct
  let f (x @ local) y = y
end

let keep = Bad_client.f 1

let () = assert (keep 2 = 2)
