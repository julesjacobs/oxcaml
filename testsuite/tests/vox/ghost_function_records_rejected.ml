(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 { expect; }
 { expect.opt; }
*)
type callbacks = { first : int -> int; second : int -> int };;
[%%expect{|
type callbacks = { first : int -> int; second : int -> int; }
|}]

let conflate (r : callbacks @ total) (x : int) :
    {u : unit | r.first x === r.second x} =
  let u = () in refine_ u;;
[%%expect{|
Line 3, characters 16-25:
3 |   let u = () in refine_ u;;
                    ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let different () =
  let one = ghost_ (fun (_ : int) -> 1) in
  let two = ghost_ (fun (_ : int) -> 2) in
  let r = ghost_ {first = one; second = two} in
  ghost_ (let u = () in let proof : {u : unit | r.first 0 === r.second 0} = refine_ u in
    let refine_ proof = proof in ());;
[%%expect{|
Line 5, characters 76-85:
5 |   ghost_ (let u = () in let proof : {u : unit | r.first 0 === r.second 0} = refine_ u in
                                                                                ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let partial_calls (r : callbacks) (x : int) : {y : int | y === 0} =
  let a = r.first x in
  let b = r.first x in
  let difference = a - b in refine_ difference;;
[%%expect{|
Line 4, characters 28-46:
4 |   let difference = a - b in refine_ difference;;
                                ^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let equal_functions (r : callbacks @ total) : {u : unit | r.first === r.second} =
  let u = () in refine_ u;;
[%%expect{|
Line 1, characters 58-78:
1 | let equal_functions (r : callbacks @ total) : {u : unit | r.first === r.second} =
                                                              ^^^^^^^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 2, characters 16-25:
2 |   let u = () in refine_ u;;
                    ^^^^^^^^^
  Required by this refinement introduction
|}]

let replaced () =
  let one = ghost_ (fun (_ : int) -> 1) in
  let two = ghost_ (fun (_ : int) -> 2) in
  let r = ghost_ {first = one; second = one} in
  let r = ghost_ {r with first = two} in
  ghost_ (let u = () in let proof : {u : unit | r.first 0 === 1} = refine_ u in
    let refine_ proof = proof in ());;
[%%expect{|
Line 6, characters 67-76:
6 |   ghost_ (let u = () in let proof : {u : unit | r.first 0 === 1} = refine_ u in
                                                                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let chosen (pick : bool) =
  let one = ghost_ (fun (_ : int) -> 1) in
  let two = ghost_ (fun (_ : int) -> 2) in
  let left = ghost_ {first = one; second = one} in
  let right = ghost_ {first = two; second = two} in
  let r = ghost_ (if pick then left else right) in
  ghost_ (let u = () in let proof : {u : unit | r.first 0 === 1} = refine_ u in
    let refine_ proof = proof in ());;
[%%expect{|
Line 7, characters 67-76:
7 |   ghost_ (let u = () in let proof : {u : unit | r.first 0 === 1} = refine_ u in
                                                                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

type mutable_callbacks = { mutable call : int -> int };;
[%%expect{|
type mutable_callbacks = { mutable call : int -> int; }
|}]

let mutable_field (r : mutable_callbacks) (x : int) : {y : int | y === 0} =
  let a = r.call x in
  let b = r.call x in
  let difference = a - b in refine_ difference;;
[%%expect{|
Line 4, characters 28-46:
4 |   let difference = a - b in refine_ difference;;
                                ^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
