(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 native;
*)

type descr = Unknown | Bottom | Ok of payload
and payload = No_alias of int | Equals of int

type ty = Value of descr | Other of descr

let[@inline never] opaque x = Sys.opaque_identity x
let[@inline never] side alias x = opaque (alias + x + 100)
let[@inline never] common x = opaque (x + 3)
let[@inline never] finish x = opaque (x + 1)

let[@inline never] f x ty =
  let ( has_alias,
        alias,
        d0,
        d1,
        d2,
        d3,
        d4,
        d5,
        d6,
        d7,
        d8,
        d9,
        d10,
        d11,
        d12,
        d13,
        d14,
        d15,
        d16,
        d17,
        d18,
        d19,
        d20,
        d21,
        d22 ) =
    match
      match ty with
      | Value (Ok (Equals alias)) | Other (Ok (Equals alias)) -> alias
      | Value (Unknown | Bottom | Ok (No_alias _))
      | Other (Unknown | Bottom | Ok (No_alias _)) ->
        raise Not_found
    with
    | s ->
      ( true,
        s,
        s + 1,
        s + 2,
        s + 3,
        s + 4,
        s + 5,
        s + 6,
        s + 7,
        s + 8,
        s + 9,
        s + 10,
        s + 11,
        s + 12,
        s + 13,
        s + 14,
        s + 15,
        s + 16,
        s + 17,
        s + 18,
        s + 19,
        s + 20,
        s + 21,
        s + 22,
        s + 23 )
    | exception Not_found ->
      ( false,
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20,
        21,
        22,
        23 )
  in
  let y =
    common
      (d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11
     + d12 + d13 + d14 + d15 + d16 + d17 + d18 + d19 + d20 + d21
     + d22 + x)
  in
  if has_alias then ignore (side alias y);
  finish y

let mk n =
  if opaque (n land 1) = 0
  then Value (Ok (No_alias n))
  else Value (Ok (Equals n))

let () = Printf.printf "%d %d\n" (f 10 (mk 0)) (f 10 (mk 1))
