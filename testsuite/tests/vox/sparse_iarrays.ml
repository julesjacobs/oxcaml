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

  let[@def] (get @ total) :
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
    let result = get base updates index in
    let refine_ equation = get_def base updates index in
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
    let result = get base updates index in
    let refine_ equation = get_def base updates index in
    refine_ result

  module Laws (Element : sig type t : immutable_data end) = struct
    let (clear_reads_base @ total) :
        (base : Element.t iarray) -> (updates : Element.t Updates.t) ->
        (index : {index : int |
          0 <= index && index < Iarray.length base}) ->
        {u : unit |
          let refine_ raw_index = index in
          let cleared = Updates.Refined.remove raw_index updates in
          get base cleared index === Iarray.Refined.get base index} =
      fun base updates index ->
      let refine_ raw_index = index in
      let cleared = Updates.Refined.remove raw_index updates in
      let refine_ equation = get_def base cleared index in
      let u = () in
      refine_ u

    let (independent_updates @ total) :
        (base : Element.t iarray) -> (updates : Element.t Updates.t) ->
        (left : int) ->
        (right : {right : int |
          not (Updates.mem right (Updates.Refined.singleton left 0))}) ->
        (left_value : Element.t) ->
        (right_value : Element.t) ->
        (index : {index : int | 0 <= index && index < Iarray.length base}) ->
        {u : unit |
          let refine_ right = right in
          let left_first = Updates.Refined.add left left_value updates in
          let left_first = Updates.Refined.add right right_value left_first in
          let right_first = Updates.Refined.add right right_value updates in
          let right_first = Updates.Refined.add left left_value right_first in
          get base left_first index === get base right_first index} =
      fun base updates left right left_value right_value index ->
      let refine_ right = right in
      let left_first = Updates.Refined.add left left_value updates in
      let left_first = Updates.Refined.add right right_value left_first in
      let right_first = Updates.Refined.add right right_value updates in
      let right_first = Updates.Refined.add left left_value right_first in
      let refine_ left_equation = get_def base left_first index in
      let refine_ right_equation = get_def base right_first index in
      let u = () in
      refine_ u
  end

  module Int_laws = Laws (struct type t = int end)
  type item = {label : int}
  module Item_laws = Laws (struct type t = item end)

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
    let result = get base updates bounded in
    let refine_ equation = ghost_ (get_def base updates bounded) in
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
    let left_value = 77 in
    let right_value = 88 in
    let refine_ proof = ghost_ (
      Int_laws.independent_updates base updates left right
        left_value right_value bounded)
    in
    let refine_ right = right in
    let left_first = Updates.Refined.add left left_value updates in
    let left_first = Updates.Refined.add right right_value left_first in
    let right_first = Updates.Refined.add right right_value updates in
    let right_first = Updates.Refined.add left left_value right_first in
    let before = get base left_first bounded in
    let after = get base right_first bounded in
    Format.printf "independent updates at index 1 = %d,%d@." before after

  let () =
    let base = [: {label = 10}; {label = 20}; {label = 30} :] in
    let updates = Updates.Refined.empty () in
    let left = 0 in
    let right = 2 in
    let left_value = {label = 77} in
    let right_value = {label = 88} in
    let distinct : {right : int |
      not (Updates.mem right (Updates.Refined.singleton left 0))} =
      assume_ right
    in
    let left_first = Updates.Refined.add left left_value updates in
    let left_first = Updates.Refined.add right right_value left_first in
    let right_first = Updates.Refined.add right right_value updates in
    let right_first = Updates.Refined.add left left_value right_first in
    List.iter (fun (index : int) ->
      let bounded : {index : int |
        0 <= index && index < Iarray.length base} = assume_ index in
      let refine_ commutation = ghost_ (
        Item_laws.independent_updates base updates left distinct
          left_value right_value bounded)
      in
      let refine_ removal = ghost_ (
        Item_laws.clear_reads_base base left_first bounded)
      in
      let before = get base left_first bounded in
      let after = get base right_first bounded in
      let cleared = Updates.Refined.remove index left_first in
      let restored = get base cleared bounded in
      let base_value = Iarray.Refined.get base bounded in
      let proof : {u : unit |
        before === after && restored === base_value} =
        let u = () in refine_ u
      in
      let refine_ proof = proof in
      Format.printf "record reads at %d = %d,%d; cleared=%d@."
        index before.label after.label restored.label)
      [0; 1; 2]

end;;
[%%expect{|
sparse reads = 99,20
independent updates at index 1 = 20,20
record reads at 0 = 77,77; cleared=10
record reads at 1 = 20,20; cleared=20
record reads at 2 = 88,88; cleared=30
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
