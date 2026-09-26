(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_big_credits.mli vox_big_credits.ml big_time_credits.ml";
 { bytecode; }
*)

module C = Vox_big_credits.Make ()

let (twice @ total) (token : {t : C.token | C.credits t >= 2Z} @ unique total ghost) :
    {t : C.token | let token = token in
      C.credits t = Bigint.sub (C.credits token) 2Z} @ unique total ghost =
  let token = token in
  let first = C.tick (token) in
  C.tick (first)

let () =
  let ten = 10Z in
  let four = 4Z in
  let initial : {n : Bigint.t | n >= 0Z} = ten in
  let token = C.Budget.create initial in
  ghost_ (let u = () in
    (u : {u : unit | C.credits token = 10Z}));
  let input : {t : C.token | C.credits t >= four} = token in
  let parts = C.split four input in
  let { C.left; right } = parts in
  let start : {t : C.token | C.credits t >= 2Z} = left in
  let left = twice start in
  ghost_ (let u = () in
    (u : {u : unit | C.credits left = 2Z && C.credits right = 6Z}));
  let result = C.merge left (right) in
  ghost_ (
    let u = () in
    (u : {u : unit | C.credits result = 8Z}));
  ()

let (observe @ total) :
    (token : {t : C.token | C.credits t > 0Z}) @ unique total ghost ->
    {t : C.token | let token = token in
      C.credits t = Bigint.sub (C.credits token) 1Z} @ unique total ghost = fun token ->
  let token = token in
  let first = ghost_ (C.credits (borrow_ token)) in
  let second = ghost_ (C.credits (borrow_ token)) in
  ghost_ (C.nonnegative (borrow_ token));
  let available : {t : C.token | C.credits t > 0Z} = token in
  let result = C.tick available in
  ghost_ (let u = () in
    (u : {u : unit | first = second && C.credits result = Bigint.sub first 1Z}));
  result

let (roundtrip @ total) : (amount : {n : Bigint.t | n >= 0Z}) ->
    {u : unit | true} = fun amount ->
  let token = C.Budget.create amount in
  let zero = C.empty () in
  let admissible : {t : C.token | 0Z <= C.credits token &&
    0Z <= C.credits t && 0Z <= Bigint.add (C.credits token) (C.credits t)} = zero in
  let result = C.merge token admissible in
  ghost_ (let expected = amount in
    let expected : Bigint.t = expected in let u = () in
    (u : {u : unit | C.credits result = expected}));
  ()

let () =
  let limit = Bigint.mul (Bigint.of_int max_int) (Bigint.of_int max_int) in
  let bound : {n : Bigint.t | n >= 0Z} = assume_ limit in
  let _ = roundtrip bound in ()
