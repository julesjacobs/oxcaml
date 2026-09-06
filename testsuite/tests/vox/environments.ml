(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }{
   flags = "-extension refinement_types -principal";
   { expect; }
   { expect.opt; }
 }
*)

let () =
  let module Environment = struct
    module Identifier = struct
      type t = int
      external compare : int -> int -> int @@ total = "%compare"
    end
    module M = Map.MakeTotal (Identifier)

    let[@def] distinct query key =
      not (M.mem query (M.Refined.singleton key 0))

    let (shadow @ total) (outer : int M.t @ total) (key : int) (value : int) :
        {r : int | r = value} =
      let inner = M.Refined.add key value outer in
      let result = M.Refined.find inner (refine_ key) in
      refine_ result

    let (preserve @ total) : int M.t @ total ->
        (key : int) -> int -> {q : int | distinct q key} ->
        {r : int option * int option |
          match r with before, after -> before === after} =
      fun outer key value query ->
      let refine_ query = query in
      let refine_ separation = ghost_ (distinct_def query key) in
      let inner = M.Refined.add key value outer in
      let before =
        if M.mem query outer then
          Some (M.Refined.find outer (refine_ query))
        else None
      in
      let after =
        if M.mem query inner then
          Some (M.Refined.find inner (refine_ query))
        else None
      in
      let result = before, after in
      refine_ result

    let (nested @ total) (key : int) (original : int) (replacement : int) :
        {r : int * int |
          match r with inside, outside ->
            inside = replacement && outside = original} =
      let outer = M.Refined.singleton key original in
      let inner = M.Refined.add key replacement outer in
      let inside = M.Refined.find inner (refine_ key) in
      let outside = M.Refined.find outer (refine_ key) in
      let result = inside, outside in
      refine_ result
  end in
  let open Environment in
  let outer = M.Refined.singleton 1 10 in
  let query = 1 in
  let key = 2 in
  let separated : {q : int | distinct q key} = assume_ query in
  let value = 99 in
  let refine_ preservation = ghost_ (preserve outer key value separated) in
  let identifier = 1 in
  let original = 10 in
  let refine_ result = nested identifier original value in
  let inside, outside = result in
  Format.printf "inner=%d outer=%d unrelated=%d@."
    inside outside (M.find 1 (M.Refined.add 2 99 outer))
;;
[%%expect{|
inner=99 outer=10 unrelated=10
|}]

let remove_does_not_restore key =
  let module Identifier = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end in
  let module M = Map.MakeTotal (Identifier) in
  let outer = M.Refined.singleton key 10 in
  let inner = M.Refined.add key 99 outer in
  let removed = M.Refined.remove key inner in
  let present = M.mem key removed in
  let proof : {b : bool | b} = refine_ present in
  let refine_ proof = proof in
  ()
;;
[%%expect{|
Line 11, characters 31-46:
11 |   let proof : {b : bool | b} = refine_ present in
                                    ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
