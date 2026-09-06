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

module Search = struct
  let[@def] at (array : int iarray) (index : int) =
    if 0 <= index && index < Iarray.length array then
      let bounded : {i : int | 0 <= i && i < Iarray.length array} =
        refine_ index
      in
      Iarray.Refined.get array bounded
    else 0

  let rec (find @ total) :
      (array : int iarray) -> (target : int) ->
      (hi : {hi : int | 0 <= hi && hi <= Iarray.length array}) ->
      (lo : {lo : int | 0 <= lo && lo <= (let refine_ h = hi in h)}) ->
      {r : int option |
        match r with
        | None -> true
        | Some index ->
          (let refine_ l = lo in l <= index)
          && (let refine_ h = hi in index < h)
          && at array index = target} =
    fun array target hi lo ->
    let refine_ upper = hi in
    let upper : int = upper in
    let refine_ lower = lo in
    let lower : int = lower in
    if lower = upper then
      let result = None in
      refine_ result
    else
      let index : {i : int | 0 <= i && i < Iarray.length array} =
        refine_ lower
      in
      let value = Iarray.Refined.get array index in
      let refine_ equation = ghost_ (at_def array lower) in
      if value = target then
        let result = Some lower in
        refine_ result
      else
        let next = lower + 1 in
        let next : {i : int | 0 <= i && i <= (let refine_ h = hi in h)} =
          refine_ next
        in
        let refine_ result = find array target hi next in
        refine_ result
  [@@decreases
    let refine_ upper = hi in
    let upper : int = upper in
    let refine_ lower = lo in
    let lower : int = lower in
    upper - lower]
end
;;
[%%expect{|
module Search :
  sig
    val at : int iarray -> int -> int
    val at_def :
      (array : int iarray) ->
      (index : int) ->
      {u : unit
        | (at array index) ===
            (if (0 <= index) && (index < (Iarray.length array))
             then
               let (bounded : int) = (index : int) in
               Iarray.Refined.get array bounded
             else 0)}
    val find :
      (array : int iarray) ->
      ((target : int) ->
       (hi : {hi : int | (0 <= hi) && (hi <= (Iarray.length array))}) ->
       (lo : {lo : int | (0 <= lo) && (lo <= (let refine_ h = hi in h))}) ->
       {r : int option
         | match r with
           | None -> true
           | Some index ->
               (let refine_ l = lo in l <= index) &&
                 ((let refine_ h = hi in index < h) &&
                    ((at array index) = target))}) @ total
      stateful
  end
|}]

let () =
  let array = [: 4; 8; 15; 16 :] in
  let upper = 4 in
  let lower = 0 in
  let upper : {i : int | 0 <= i && i <= Iarray.length array} = refine_ upper in
  let lower : {i : int | 0 <= i && i <= (let refine_ h = upper in h)} =
    refine_ lower
  in
  List.iter (fun target ->
    let refine_ result = Search.find array target upper lower in
    match result with
    | None -> Format.printf "%d: absent@." target
    | Some index -> Format.printf "%d: index %d@." target index)
    [4; 16; 9];
  let target = 4 in
  let refine_ limit = upper in
  let end_index : {i : int | 0 <= i && i <= (let refine_ h = upper in h)} =
    refine_ limit
  in
  let refine_ result = Search.find array target upper end_index in
  match result with
  | None -> Format.printf "empty interval@."
  | Some _ -> Format.printf "unexpected match@."
;;
[%%expect{|
4: index 0
16: index 3
9: absent
empty interval
|}]

let invalid () =
  let array = [: 4 :] in
  let index = 1 in
  Iarray.Refined.get array (refine_ index)
;;
[%%expect{|
Line 4, characters 27-42:
4 |   Iarray.Refined.get array (refine_ index)
                               ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
