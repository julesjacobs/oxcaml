module D = Hm_declarative
module W = Hmc_word64
module K = Hmc_closure_ir
module I = Hmc_u32_index

let[@def] rec (lookup @ total) (table : K.table @ immutable) (number : W.limb) = match table with
  | K.Empty -> None
  | K.Add (entry, rest) -> if I.represents (K.size rest) number then Some (K.size rest, entry) else lookup rest number
let rec (correct @ total) : (table : K.table) @ immutable -> (id : D.index) @ immutable -> (number : W.limb) ->
    {u : unit | I.represents id number} ->
    {u : unit | lookup table number === (match K.lookup table id with None -> None | Some entry -> Some (id, entry))} @ ghost =
  fun table id number premise -> ghost_ (
    lookup_def table number; K.lookup_def table id;
    match table with K.Empty -> () | K.Add (_, rest) ->
      let _ = Hm_elaboration_check.index_equal id (K.size rest) in
      if I.represents (K.size rest) number then I.injective id (K.size rest) number () else correct rest id number ())
