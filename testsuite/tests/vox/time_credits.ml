(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_credits.mli vox_credits.ml time_credits.ml";
 { native; }
*)

module C = Vox_credits.Make ()

let (twice @ total) (token : {t : C.token | C.credits t >= 2} @ unique total ghost) :
    {t : C.token | let refine_ token = token in
      C.credits t = C.credits token - 2} @ unique total ghost =
  let refine_ token = token in
  let refine_ first = C.tick (refine_ token) in
  C.tick (refine_ first)

let () =
  let ten = 10 in
  let four = 4 in
  let initial : {n : int | n >= 0} = refine_ ten in
  let refine_ token = C.Budget.create initial in
  ghost_ (let u = () in
    (refine_ u : {u : unit | C.credits token = 10}));
  let input : {t : C.token | C.credits t >= four} = refine_ token in
  let refine_ parts = C.split four input in
  let { C.left; right } = parts in
  let start : {t : C.token | C.credits t >= 2} = refine_ left in
  let refine_ left = twice start in
  ghost_ (let u = () in
    (refine_ u : {u : unit | C.credits left = 2 && C.credits right = 6}));
  let refine_ result = C.merge left (refine_ right) in
  ghost_ (
    let u = () in
    (refine_ u : {u : unit | C.credits result = 8}));
  ()

let (observe @ total) :
    (token : {t : C.token | C.credits t > 0}) @ unique total ghost ->
    {t : C.token | let refine_ token = token in
      C.credits t = C.credits token - 1} @ unique total ghost = fun token ->
  let refine_ token = token in
  let first = ghost_ (C.credits (borrow_ token)) in
  let second = ghost_ (C.credits (borrow_ token)) in
  ghost_ (C.nonnegative (borrow_ token));
  let available : {t : C.token | C.credits t > 0} = refine_ token in
  let refine_ result = C.tick available in
  ghost_ (let u = () in
    (refine_ u : {u : unit | first = second && C.credits result = first - 1}));
  refine_ result

let (roundtrip @ total) : (amount : {n : int | n >= 0}) ->
    {u : unit | true} = fun amount ->
  let refine_ token = C.Budget.create amount in
  let refine_ zero = C.empty () in
  let admissible : {t : C.token | 0 <= C.credits token &&
    0 <= C.credits t && 0 <= C.credits token + C.credits t} = refine_ zero in
  let refine_ result = C.merge token admissible in
  ghost_ (let refine_ expected = amount in
    let expected : int = expected in let u = () in
    (refine_ u : {u : unit | C.credits result = expected}));
  let u = () in refine_ u

let () =
  let limit = max_int in
  let bound : {n : int | n >= 0} = assume_ limit in
  let refine_ checked = roundtrip bound in ()
