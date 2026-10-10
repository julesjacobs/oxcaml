(* TEST
 modules = "producer.ml";
 flags = "-extension refinement_types";
 has-z3;
 { bytecode; }
 { native; }
*)

module Alias = Producer
module B = Bigint
module Bigint = struct let of_int _ = false end
module Stdlib = struct let min_int = false end
module Stdlib__Bigint = struct let of_int _ = false end

let checked (x : Alias.number) : {r : B.t | r > x} =
  let r = Alias.next x in
  let refine_ proof = Alias.next_def x in
  refine_ r

let one () : {r : B.t | r = 1Z} =
  let r = B.one in refine_ r

let () =
  let n = 123456789012345678901234567890Z in
  let refine_ result = checked n in
  assert (B.to_string result = "123456789012345678901234567891");
  let refine_ result = Alias.huge () in
  assert (B.to_string result = B.to_string n);
  ignore (one ())
