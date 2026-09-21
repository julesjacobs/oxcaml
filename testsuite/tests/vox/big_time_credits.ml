(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_big_credits.mli vox_big_credits.ml big_time_credits.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

module C = Vox_big_credits.Make ()

let (twice @ total) (token : {t : C.token | C.credits t >= 2Z} @ unique total ghost) :
    {t : C.token | let refine_ token = token in
      C.credits t = Bigint.sub (C.credits token) 2Z} @ unique total ghost =
  let refine_ token = token in
  let refine_ first = C.tick (refine_ token) in
  C.tick (refine_ first)

let () =
  let ten = 10Z in
  let four = 4Z in
  let initial : {n : Bigint.t | n >= 0Z} = refine_ ten in
  let refine_ token = C.Budget.create initial in
  ghost_ (let u = () in
    (refine_ u : {u : unit | C.credits token = 10Z}));
  let input : {t : C.token | C.credits t >= four} = refine_ token in
  let refine_ parts = C.split four input in
  let { C.left; right } = parts in
  let start : {t : C.token | C.credits t >= 2Z} = refine_ left in
  let refine_ left = twice start in
  ghost_ (let u = () in
    (refine_ u : {u : unit | C.credits left = 2Z && C.credits right = 6Z}));
  let refine_ result = C.merge left (refine_ right) in
  ghost_ (
    let u = () in
    (refine_ u : {u : unit | C.credits result = 8Z}));
  ()

let (observe @ total) :
    (token : {t : C.token | C.credits t > 0Z}) @ unique total ghost ->
    {t : C.token | let refine_ token = token in
      C.credits t = Bigint.sub (C.credits token) 1Z} @ unique total ghost = fun token ->
  let refine_ token = token in
  let first = ghost_ (C.credits (borrow_ token)) in
  let second = ghost_ (C.credits (borrow_ token)) in
  ghost_ (C.nonnegative (borrow_ token));
  let available : {t : C.token | C.credits t > 0Z} = refine_ token in
  let refine_ result = C.tick available in
  ghost_ (let u = () in
    (refine_ u : {u : unit | first = second && C.credits result = Bigint.sub first 1Z}));
  refine_ result

let (roundtrip @ total) : (amount : {n : Bigint.t | n >= 0Z}) ->
    {u : unit | true} = fun amount ->
  let refine_ token = C.Budget.create amount in
  let refine_ zero = C.empty () in
  let admissible : {t : C.token | 0Z <= C.credits token &&
    0Z <= C.credits t && 0Z <= Bigint.add (C.credits token) (C.credits t)} = refine_ zero in
  let refine_ result = C.merge token admissible in
  ghost_ (let refine_ expected = amount in
    let expected : Bigint.t = expected in let u = () in
    (refine_ u : {u : unit | C.credits result = expected}));
  let u = () in refine_ u

let () =
  let limit = Bigint.mul (Bigint.of_int max_int) (Bigint.of_int max_int) in
  let bound : {n : Bigint.t | n >= 0Z} = assume_ limit in
  let refine_ checked = roundtrip bound in ()
