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

module Demo : sig end = struct
  module Index = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end

  module Updates = Map.MakeTotal (Index)

  type 'a t =
    { base : 'a iarray;
      updates : 'a Updates.t
    }

  let (empty @ total) base =
    ({ base;
       updates = Updates.Refined.empty ()
     } : _ t @ read_write)

  let (get @ total) :
      ('a : value mod separable).
      (base : 'a iarray) ->
      'a Updates.t @ total ->
      {index : int |
        0 <= index && index < Iarray.length base} ->
      'a @ total =
    fun base updates refined_index ->
    let refine_ index = refined_index in
    if Updates.mem index updates then
      let member : {key : int | Updates.mem key updates} = refine_ index in
      Updates.Refined.find updates member
    else Iarray.Refined.get base refined_index

  let (set @ total) index value overlay =
    let updates = Updates.Refined.add index value overlay.updates in
    { overlay with updates }

  let (clear @ total) index overlay =
    let updates = Updates.Refined.remove index overlay.updates in
    { overlay with updates }

  let (read_after_write @ total) :
      ('a : value mod separable).
      (base : 'a iarray) ->
      'a Updates.t @ total ->
      (index : {index : int |
        0 <= index && index < Iarray.length base}) ->
      (value : 'a) ->
      {result : 'a | result === value} =
    fun base updates index value ->
    let refine_ raw_index = index in
    let updates = Updates.Refined.add raw_index value updates in
    let result =
      if Updates.mem raw_index updates then
        let member : {key : int | Updates.mem key updates} = refine_ raw_index in
        Updates.Refined.find updates member
      else Iarray.Refined.get base index
    in
    refine_ result

  let (last_write_wins @ total) :
      ('a : value mod separable).
      (base : 'a iarray) ->
      'a Updates.t @ total ->
      (index : {index : int |
        0 <= index && index < Iarray.length base}) ->
      'a @ total ->
      (last : 'a) ->
      {result : 'a | result === last} =
    fun base updates index first last ->
    let refine_ raw_index = index in
    let once = Updates.Refined.add raw_index first updates in
    let updates = Updates.Refined.add raw_index last once in
    let result =
      if Updates.mem raw_index updates then
        let member : {key : int | Updates.mem key updates} = refine_ raw_index in
        Updates.Refined.find updates member
      else Iarray.Refined.get base index
    in
    refine_ result

  let (clear_reads_base @ total) :
      (base : int iarray) ->
      int Updates.t @ total ->
      (index : {index : int |
        0 <= index && index < Iarray.length base}) ->
      {results : int * int |
        match results with result, base_result -> result === base_result} =
    fun base updates index ->
    let refine_ raw_index = index in
    let updates = Updates.Refined.remove raw_index updates in
    let base_result = Iarray.Refined.get base index in
    let result =
      if Updates.mem raw_index updates then
        let member : {key : int | Updates.mem key updates} = refine_ raw_index in
        Updates.Refined.find updates member
      else Iarray.Refined.get base index
    in
    let results = result, base_result in
    refine_ results

  let (independent_updates @ total) :
      (base : int iarray) -> int Updates.t @ total ->
      (left : int) ->
      {right : int | not (Updates.mem right (Updates.Refined.singleton left 0))} ->
      int -> int ->
      {index : int | 0 <= index && index < Iarray.length base} ->
      {results : int * int | match results with before, after -> before = after} =
    fun base updates left right left_value right_value index ->
    let refine_ right = right in
    let refine_ probe = index in
    let left_first = Updates.Refined.add left left_value updates in
    let left_first = Updates.Refined.add right right_value left_first in
    let right_first = Updates.Refined.add right right_value updates in
    let right_first = Updates.Refined.add left left_value right_first in
    let before =
      if Updates.mem probe left_first then
        Updates.Refined.find left_first (refine_ probe)
      else Iarray.Refined.get base index
    in
    let after =
      if Updates.mem probe right_first then
        Updates.Refined.find right_first (refine_ probe)
      else Iarray.Refined.get base index
    in
    let results = before, after in
    refine_ results

  let literal () : {result : int | result = 20} =
    let base = [: 10; 20; 30 :] in
    let overlay =
      { base;
        updates = Updates.Refined.empty ()
      }
    in
    let updates = overlay.updates in
    let index = 1 in
    let bounded :
        {index : int |
          0 <= index && index < Iarray.length base} =
      refine_ index
    in
    let result =
      if Updates.mem index updates then
        let member : {key : int | Updates.mem key updates} = refine_ index in
        Updates.Refined.find updates member
      else Iarray.Refined.get base bounded
    in
    refine_ result

  type mutable_value = {mutable payload : int}

  let update_result :
      (base : mutable_value iarray) ->
      mutable_value Updates.t @ total ->
      {index : int |
        0 <= index && index < Iarray.length base} ->
      unit =
    fun base updates index -> (get base updates index).payload <- 1

  let () =
    let base = [: 10; 20; 30 :] in
    let index = 1 in
    let bounded :
        {index : int |
          0 <= index && index < Iarray.length base} =
      refine_ index
    in
    let overlay = set index 99 (empty base) in
    let updated = get base overlay.updates bounded in
    let cleared = clear index overlay in
    let restored = get base cleared.updates bounded in
    Format.printf "sparse reads = %d,%d@." updated restored;
    let left = 0 in
    let right = 2 in
    let right :
        {right : int |
          not (Updates.mem right (Updates.Refined.singleton left 0))} =
      assume_ right
    in
    let updates = Updates.Refined.empty () in
    let refine_ results =
      independent_updates base updates left right 77 88 bounded
    in
    let before, after = results in
    Format.printf "independent updates at index 1 = %d,%d@." before after
end;;
[%%expect{|
sparse reads = 99,20
independent updates at index 1 = 20,20
module Demo : sig end
|}]

module Invalid_index : sig end = struct
  module Index = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end

  module Updates = Map.MakeTotal (Index)

  let rejected () =
    let base = [: 10 :] in
    let index = 1 in
    let bounded :
        {index : int | 0 <= index && index < Iarray.length base} =
      refine_ index
    in
    Iarray.Refined.get base bounded
end;;
[%%expect{|
Line 14, characters 6-19:
14 |       refine_ index
           ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
