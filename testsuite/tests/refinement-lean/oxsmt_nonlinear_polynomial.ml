(* TEST
 flags = "-keywords 5.3 -vox-backend oxsmt";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
*)

let square_sum
    (left : Bigint.t @ logical)
    (right : Bigint.t @ logical)
    : unit{
        Bigint.equal
          (Bigint.mul (Bigint.add left right) (Bigint.add left right))
          (Bigint.add
             (Bigint.add
                (Bigint.mul left left)
                (Bigint.mul (Bigint.mul (Bigint.of_int 2) left) right))
             (Bigint.mul right right))
      } =
  ()
