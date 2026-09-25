(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_credits.mli vox_credits.ml time_credits.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

module C = Vox_credits.Make ()

let (twice @ total) (token : {t : C.token | C.credits t >= 2} @ unique total ghost) :
    {t : C.token | let token = token in
      C.credits t = C.credits token - 2} @ unique total ghost =
  let token = token in
  let first = C.tick (token) in
  C.tick (first)

let () =
  let ten = 10 in
  let four = 4 in
  let initial : {n : int | n >= 0} = ten in
  let token = C.Budget.create initial in
  ghost_ (let u = () in
    (u : {u : unit | C.credits token = 10}));
  let input : {t : C.token | C.credits t >= four} = token in
  let parts = C.split four input in
  let { C.left; right } = parts in
  let start : {t : C.token | C.credits t >= 2} = left in
  let left = twice start in
  ghost_ (let u = () in
    (u : {u : unit | C.credits left = 2 && C.credits right = 6}));
  let result = C.merge left (right) in
  ghost_ (
    let u = () in
    (u : {u : unit | C.credits result = 8}));
  ()

let (observe @ total) :
    (token : {t : C.token | C.credits t > 0}) @ unique total ghost ->
    {t : C.token | let token = token in
      C.credits t = C.credits token - 1} @ unique total ghost = fun token ->
  let token = token in
  let first = ghost_ (C.credits (borrow_ token)) in
  let second = ghost_ (C.credits (borrow_ token)) in
  ghost_ (C.nonnegative (borrow_ token));
  let available : {t : C.token | C.credits t > 0} = token in
  let result = C.tick available in
  ghost_ (let u = () in
    (u : {u : unit | first = second && C.credits result = first - 1}));
  result

let (roundtrip @ total) : (amount : {n : int | n >= 0}) ->
    {u : unit | true} = fun amount ->
  let token = C.Budget.create amount in
  let zero = C.empty () in
  let admissible : {t : C.token | 0 <= C.credits token &&
    0 <= C.credits t && 0 <= C.credits token + C.credits t} = zero in
  let result = C.merge token admissible in
  ghost_ (let expected = amount in
    let expected : int = expected in let u = () in
    (u : {u : unit | C.credits result = expected}));
  ()

let () =
  let limit = max_int in
  let bound : {n : int | n >= 0} = assume_ limit in
  let checked = roundtrip bound in ()
