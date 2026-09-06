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

  let[@def] rec absent (array : int iarray) (target : int) (lo : int) (hi : int) =
    if 0 <= lo && lo < hi then
      at array lo <> target && absent array target (lo + 1) hi
    else true
  [@@decreases
    let lo : int = lo in
    let hi : int = hi in
    if 0 <= lo && lo < hi then hi - lo else 0]

  let rec (find @ total) :
      (array : int iarray) -> (target : int) ->
      (hi : {hi : int | 0 <= hi && hi <= Iarray.length array}) ->
      (lo : {lo : int | 0 <= lo && lo <= (let refine_ h = hi in h)}) ->
      {r : int option |
        match r with
        | None -> absent array target
            (let refine_ l = lo in l) (let refine_ h = hi in h)
        | Some index ->
          (let refine_ l = lo in l <= index)
          && (let refine_ h = hi in index < h)
          && at array index = target
          && absent array target (let refine_ l = lo in l) index} =
    fun array target hi lo ->
    let refine_ upper = hi in
    let upper : int = upper in
    let refine_ lower = lo in
    let lower : int = lower in
    if lower = upper then
      let result = None in
      let refine_ equation = ghost_ (absent_def array target lower upper) in
      refine_ result
    else
      let index : {i : int | 0 <= i && i < Iarray.length array} =
        refine_ lower
      in
      let value = Iarray.Refined.get array index in
      let refine_ equation = ghost_ (at_def array lower) in
      if value = target then
        let result = Some lower in
        let refine_ equation = ghost_ (absent_def array target lower lower) in
        refine_ result
      else
        let next = lower + 1 in
        let next : {i : int | 0 <= i && i <= (let refine_ h = hi in h)} =
          refine_ next
        in
        let refine_ result = find array target hi next in
        let refine_ proof = ghost_ (
          let stop = match result with None -> upper | Some index -> index in
          let refine_ equation = absent_def array target lower stop in
          let u = () in
          (refine_ u : {u : unit |
            match result with
            | None -> absent array target lower upper
            | Some index -> absent array target lower index}))
        in
        refine_ result
  [@@decreases
    let refine_ upper = hi in
    let upper : int = upper in
    let refine_ lower = lo in
    let lower : int = lower in
    upper - lower]

  let rec (absent_at @ total) :
      (array : int iarray) -> (target : int) ->
      (lo : int) -> (hi : int) -> (index : int) ->
      {u : unit | 0 <= lo && lo <= index && index < hi
        && absent array target lo hi} ->
      {u : unit | at array index <> target} =
    fun array target lo hi index premise ->
    let refine_ premise = premise in
    let refine_ equation = absent_def array target lo hi in
    let u = () in
    if index = lo then refine_ u
    else
      let next = lo + 1 in
      let premise : {u : unit | 0 <= next && next <= index && index < hi
        && absent array target next hi} = refine_ u in
      let refine_ induction = absent_at array target next hi index premise in
      refine_ u
  [@@decreases
    let lo : int = lo in
    let hi : int = hi in
    if 0 <= lo && lo < hi then hi - lo else 0]

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
    val absent : int iarray -> int -> int -> int -> bool
    val absent_def :
      (array : int iarray) ->
      (target : int) ->
      (lo : int) ->
      (hi : int) ->
      {u : unit
        | (absent array target lo hi) ===
            (if (0 <= lo) && (lo < hi)
             then
               ((at array lo) <> target) && (absent array target (lo + 1) hi)
             else true)}
    val find :
      (array : int iarray) ->
      ((target : int) ->
       (hi : {hi : int | (0 <= hi) && (hi <= (Iarray.length array))}) ->
       (lo : {lo : int | (0 <= lo) && (lo <= (let refine_ h = hi in h))}) ->
       {r : int option
         | match r with
           | None ->
               absent array target (let refine_ l' = lo in l')
                 (let refine_ h' = hi in h')
           | Some index ->
               (let refine_ l'' = lo in l'' <= index) &&
                 ((let refine_ h = hi in index < h) &&
                    (((at array index) = target) &&
                       (absent array target (let refine_ l = lo in l) index)))}) @ total
      stateful
    val absent_at :
      (array : int iarray) ->
      ((target : int) ->
       (lo : int) ->
       (hi : int) ->
       (index : int) ->
       {u : unit
         | (0 <= lo) &&
             ((lo <= index) && ((index < hi) && (absent array target lo hi)))} ->
       {u : unit | (at array index) <> target}) @ total
      stateful
  end
|}]

let () =
  let array = [: 4; 8; 4; 16 :] in
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
  let start = 1 in
  let start : {i : int | 0 <= i && i <= (let refine_ h = upper in h)} =
    refine_ start
  in
  let refine_ result = Search.find array target upper start in
  (match result with
   | None -> Format.printf "unexpected absence@."
   | Some index -> Format.printf "subinterval: index %d@." index);
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
subinterval: index 2
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

let invalid_first () =
  let array = [: 4; 4 :] in
  let target = 4 in
  let lo = 0 in
  let later = 1 in
  let refine_ equation = Search.at_def array lo in
  let refine_ equation = Search.at_def array later in
  let refine_ equation = Search.absent_def array target lo later in
  let result : {i : int | Search.at array i = target
    && Search.absent array target lo i} = refine_ later in
  let refine_ result = result in
  result
;;
[%%expect{|
Line 10, characters 42-55:
10 |     && Search.absent array target lo i} = refine_ later in
                                               ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
