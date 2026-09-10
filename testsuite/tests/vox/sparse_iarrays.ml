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

  let (empty @ total) : ('a : value mod separable).
      'a iarray @ total -> 'a t @ total =
    fun base ->
    {base; updates = Updates.Refined.empty ()}

  let[@def] (get @ total) :
      ('a : value mod separable).
      (overlay : 'a t) ->
      {index : int |
        0 <= index && index < Iarray.length overlay.base} ->
      'a @ total =
    fun overlay refined_index ->
    let base = overlay.base in
    let updates = overlay.updates in
    let refine_ index = refined_index in
    if Updates.mem index updates then
      let member : {key : int | Updates.mem key updates} = refine_ index in
      Updates.Refined.find updates member
    else Iarray.Refined.get base (refine_ index)

  let[@def] (lookup @ total) : ('a : value mod separable).
      int -> 'a t @ total -> 'a option @ total =
    fun index overlay ->
    if 0 <= index && index < Iarray.length overlay.base then
      let bounded : {i : int | 0 <= i && i < Iarray.length overlay.base} =
        refine_ index in
      Some (get overlay bounded)
    else None

  let (set @ total) : ('a : value mod separable).
      int -> 'a @ total ->
      'a t @ total -> 'a t @ total =
    fun index value overlay ->
    let updates = Updates.Refined.add index value overlay.updates in
    { overlay with updates }

  let (clear @ total) : ('a : value mod separable).
      int -> 'a t @ total -> 'a t @ total =
    fun index overlay ->
    let updates = Updates.Refined.remove index overlay.updates in
    { overlay with updates }

  let (read_after_write @ total) :
      ('a : value mod separable).
      (overlay : 'a t) ->
      (index : {index : int |
        0 <= index && index < Iarray.length overlay.base}) ->
      (value : 'a) ->
      {result : 'a | result === value} =
    fun overlay index value ->
    let updates = overlay.updates in
    let refine_ raw_index = index in
    let updates = Updates.Refined.add raw_index value updates in
    let updated = {overlay with updates} in
    let bounded : {i : int | 0 <= i && i < Iarray.length updated.base} =
      refine_ raw_index in
    let result = get updated bounded in
    get_def updated bounded;
    refine_ result

  let (last_write_wins @ total) :
      ('a : value mod separable).
      (overlay : 'a t) ->
      (index : {index : int |
        0 <= index && index < Iarray.length overlay.base}) ->
      'a @ total ->
      (last : 'a) ->
      {result : 'a | result === last} =
    fun overlay index first last ->
    let updates = overlay.updates in
    let refine_ raw_index = index in
    let once = Updates.Refined.add raw_index first updates in
    let updates = Updates.Refined.add raw_index last once in
    let updated = {overlay with updates} in
    let bounded : {i : int | 0 <= i && i < Iarray.length updated.base} =
      refine_ raw_index in
    let result = get updated bounded in
    get_def updated bounded;
    refine_ result

  module Laws (Element : sig type t : immutable_data end) = struct
    let (clear_reads_base @ total) :
        (overlay : Element.t t) -> (index : int) ->
        {u : unit |
          let cleared = {overlay with
            updates = Updates.Refined.remove index overlay.updates} in
          let original = {overlay with updates = Updates.Refined.empty ()} in
          lookup index cleared === lookup index original} =
      fun overlay index ->
      let cleared = {overlay with
        updates = Updates.Refined.remove index overlay.updates} in
      let original = {overlay with updates = Updates.Refined.empty ()} in
      lookup_def index cleared;
      lookup_def index original;
      let u = () in
      if 0 <= index && index < Iarray.length overlay.base then
        let cleared_index :
            {i : int | 0 <= i && i < Iarray.length cleared.base} =
          refine_ index in
        let base_index :
            {i : int | 0 <= i && i < Iarray.length original.base} =
          refine_ index in
        get_def cleared cleared_index;
        get_def original base_index;
        refine_ u
      else refine_ u

    let (independent_updates @ total) :
        (overlay : Element.t t) -> (left : int) ->
        (right : {right : int |
          not (Updates.mem right (Updates.Refined.singleton left 0))}) ->
        (left_value : Element.t) -> (right_value : Element.t) ->
        (index : int) ->
        {u : unit |
          let refine_ right = right in
          let left_updates =
            Updates.Refined.add left left_value overlay.updates in
          let left_first = {overlay with
            updates = Updates.Refined.add right right_value left_updates} in
          let right_updates =
            Updates.Refined.add right right_value overlay.updates in
          let right_first = {overlay with
            updates = Updates.Refined.add left left_value right_updates} in
          lookup index left_first === lookup index right_first} =
      fun overlay left right left_value right_value index ->
      let refine_ right = right in
      let left_updates =
        Updates.Refined.add left left_value overlay.updates in
      let left_first = {overlay with
        updates = Updates.Refined.add right right_value left_updates} in
      let right_updates =
        Updates.Refined.add right right_value overlay.updates in
      let right_first = {overlay with
        updates = Updates.Refined.add left left_value right_updates} in
      lookup_def index left_first;
      lookup_def index right_first;
      let u = () in
      if 0 <= index && index < Iarray.length overlay.base then
        let left_index :
            {i : int | 0 <= i && i < Iarray.length left_first.base} =
          refine_ index in
        let right_index :
            {i : int | 0 <= i && i < Iarray.length right_first.base} =
          refine_ index in
        get_def left_first left_index;
        get_def right_first right_index;
        refine_ u
      else refine_ u
  end

  module Int_laws = Laws (struct type t = int end)
  type item = {label : int}
  module Item_laws = Laws (struct type t = item end)

  let literal () : {result : int | result = 20} =
    let overlay =
      { base = [: 10; 20; 30 :]; updates = Updates.Refined.empty () } in
    let index = 1 in
    let bounded : {i : int | 0 <= i && i < Iarray.length overlay.base} =
      refine_ index in
    let result = get overlay bounded in
    ghost_ (get_def overlay bounded);
    refine_ result

  type mutable_value = {mutable payload : int}

  let update_result : (overlay : mutable_value t) ->
      {index : int | 0 <= index && index < Iarray.length overlay.base} ->
      unit =
    fun overlay index -> (get overlay index).payload <- 1

  let () =
    let base = [: 10; 20; 30 :] in
    let overlay = set 1 99 (empty base) in
    let updated = lookup 1 overlay in
    let restored = lookup 1 (clear 1 overlay) in
    (match updated, restored with
     | Some updated, Some restored ->
       Format.printf "sparse reads = %d,%d@." updated restored
     | _ -> assert false);
    let overlay = empty base in
    let left = 0 in
    let right = 2 in
    let right : {right : int |
      not (Updates.mem right (Updates.Refined.singleton left 0))} =
      assume_ right in
    let left_value = 77 in
    let right_value = 88 in
    let index = 1 in
    ghost_ (
      Int_laws.independent_updates overlay left right
        left_value right_value index);
    let refine_ right = right in
    let left_updates = Updates.Refined.add left left_value overlay.updates in
    let left_first = {overlay with
      updates = Updates.Refined.add right right_value left_updates} in
    let right_updates =
      Updates.Refined.add right right_value overlay.updates in
    let right_first = {overlay with
      updates = Updates.Refined.add left left_value right_updates} in
    match lookup index left_first, lookup index right_first with
    | Some before, Some after ->
      let u = () in
      let proof : {u : unit | before = after} = refine_ u in
      proof;
      Format.printf "independent updates at index 1 = %d,%d@." before after
    | _ -> assert false

  let () =
    let base = [: {label = 10}; {label = 20}; {label = 30} :] in
    let overlay = empty base in
    let left = 0 in
    let right = 2 in
    let left_value = {label = 77} in
    let right_value = {label = 88} in
    let distinct : {right : int |
      not (Updates.mem right (Updates.Refined.singleton left 0))} =
      assume_ right in
    let left_updates = Updates.Refined.add left left_value overlay.updates in
    let left_first = {overlay with
      updates = Updates.Refined.add right right_value left_updates} in
    let right_updates =
      Updates.Refined.add right right_value overlay.updates in
    let right_first = {overlay with
      updates = Updates.Refined.add left left_value right_updates} in
    List.iter (fun (index : int) ->
      ghost_ (
        Item_laws.independent_updates overlay left distinct
          left_value right_value index);
      ghost_ (
        Item_laws.clear_reads_base left_first index);
      let cleared = {left_first with
        updates = Updates.Refined.remove index left_first.updates} in
      let original = {left_first with updates = Updates.Refined.empty ()} in
      match lookup index left_first, lookup index right_first,
        lookup index cleared, lookup index original with
      | Some before, Some after, Some restored, Some base_value ->
        let u = () in
        let proof : {u : unit |
          before === after && restored === base_value} = refine_ u in
        proof;
        Format.printf "record reads at %d = %d,%d; cleared=%d@."
          index before.label after.label restored.label
      | _ -> assert false)
      [0; 1; 2]

  let () =
    let overlay = set (-1) 99 (empty [: 10 :]) in
    assert (lookup (-1) overlay = None);
    assert (lookup 1 overlay = None)

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
