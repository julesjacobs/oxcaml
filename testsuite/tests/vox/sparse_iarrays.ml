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
    Format.printf "sparse reads = %d,%d@." updated restored
end;;
[%%expect{|
sparse reads = 99,20
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
