exception Limit

module Int_list = struct
  type t = int list

  let equal = List.equal Int.equal

  let hash values =
    List.fold_left (fun hash value -> hash * 65599 lxor value) 0 values
end

module Int_list_table = Hashtbl.Make (Int_list)

module Int_lists_table = Hashtbl.Make (struct
  type t = int list * int list

  let equal (a, b) (c, d) = Int_list.equal a c && Int_list.equal b d

  let hash (a, b) = Int_list.hash a * 65599 lxor Int_list.hash b
end)

let node_limit = ref 500_000

module For_testing = struct
  let with_node_limit limit f =
    let saved = !node_limit in
    node_limit := limit;
    Fun.protect ~finally:(fun () -> node_limit := saved) f
end

type node =
  { id : int;
    branch : (int * node * node) option;
    truth : bool;
    factors : node list option;
    owner : manager option
  }

and manager =
  { arities : int array;
    widths : int array;
    bit_order : int array array;
    bit_positions : int array array;
    unique : (int * int * int, node) Hashtbl.t;
    select_cache : (int * int * int, node) Hashtbl.t;
    apply_cache : (int * int * int, node) Hashtbl.t;
    not_cache : (int, node) Hashtbl.t;
    quant_cache : (int * int * int, node) Hashtbl.t;
    restrict_cache : (int * int * int, node) Hashtbl.t;
    models_cache : (int, int array list) Hashtbl.t;
    mutable compacted_size : int;
    mutable next_id : int;
    node_limit : int;
    mutable remaining_work : int
  }

let false_ =
  { id = 0; branch = None; truth = false; factors = None; owner = None }

let true_ =
  { id = 1; branch = None; truth = true; factors = None; owner = None }

let is_false node = node == false_

let create ?bit_order arities =
  if Array.exists (fun n -> n <= 0) arities
  then invalid_arg "Solver_mdd.create";
  let width cardinality =
    let rec loop bits maximum =
      if maximum >= cardinality - 1
      then bits
      else loop (bits + 1) ((maximum lsl 1) lor 1)
    in
    loop 0 0
  in
  let widths = Array.map width arities in
  let bit_order =
    match bit_order with
    | None -> Array.map (fun width -> Array.init width Fun.id) widths
    | Some order -> Array.map Array.copy order
  in
  if Array.length bit_order <> Array.length arities
  then invalid_arg "Solver_mdd.create: bit order";
  let bit_positions =
    Array.mapi
      (fun var order ->
        if
          Array.length order <> widths.(var)
          || Array.exists (fun rank -> rank < 0 || rank > 1024) order
        then invalid_arg "Solver_mdd.create: bit order";
        let positions = Array.make (1 + Array.fold_left max (-1) order) (-1) in
        Array.iteri
          (fun bit rank ->
            if positions.(rank) <> -1
            then invalid_arg "Solver_mdd.create: duplicate bit rank";
            positions.(rank) <- bit)
          order;
        positions)
      bit_order
  in
  { arities = Array.copy arities;
    widths;
    bit_order;
    bit_positions;
    unique = Hashtbl.create 251;
    select_cache = Hashtbl.create 251;
    apply_cache = Hashtbl.create 251;
    not_cache = Hashtbl.create 251;
    quant_cache = Hashtbl.create 251;
    restrict_cache = Hashtbl.create 251;
    models_cache = Hashtbl.create 31;
    compacted_size = 0;
    next_id = 2;
    node_limit = !node_limit;
    remaining_work = 10_000_000
  }

let arity manager = Array.length manager.arities

let cardinality manager index = manager.arities.(index)

let decode manager var value =
  let cardinality = cardinality manager var in
  let stride = cardinality land -cardinality in
  (min (value / stride) ((cardinality / stride) - 1) * stride)
  + (value mod stride)

let bit_index manager var bit =
  let rank = manager.bit_order.(var).(bit) in
  (((rank / 2 * arity manager) + var) * 2) + (rank mod 2)

let variable_of_bit manager index = index / 2 mod arity manager

let bit_position manager var index =
  let rank = (index / (2 * arity manager) * 2) + (index mod 2) in
  manager.bit_positions.(var).(rank)

let node_var node =
  match node.branch with None -> max_int | Some (var, _, _) -> var

let charge manager =
  manager.remaining_work <- manager.remaining_work - 1;
  if manager.remaining_work < 0
  then begin
    raise Limit
  end

let branch manager var low high =
  if low == high
  then low
  else
    let key = var, low.id, high.id in
    match Hashtbl.find_opt manager.unique key with
    | Some node -> node
    | None ->
      charge manager;
      if
        Hashtbl.length manager.unique >= manager.node_limit - 2
        || manager.next_id = max_int
      then begin
        raise Limit
      end;
      let node =
        { id = manager.next_id;
          branch = Some (var, low, high);
          truth = false;
          factors = None;
          owner = None
        }
      in
      manager.next_id <- manager.next_id + 1;
      Hashtbl.add manager.unique key node;
      node

let child node var high =
  match node.branch with
  | Some (node_var, low, right) when node_var = var ->
    if high then right else low
  | _ -> node

let rec apply manager op left right =
  let left, right = if left.id <= right.id then left, right else right, left in
  if left == right
  then left
  else if op = 0 && (left == false_ || right == false_)
  then false_
  else if op = 0 && left == true_
  then right
  else if op = 0 && right == true_
  then left
  else if op = 1 && (left == true_ || right == true_)
  then true_
  else if op = 1 && left == false_
  then right
  else if op = 1 && right == false_
  then left
  else
    let key = op, left.id, right.id in
    match Hashtbl.find_opt manager.apply_cache key with
    | Some node -> node
    | None ->
      charge manager;
      let var = min (node_var left) (node_var right) in
      let low =
        apply manager op (child left var false) (child right var false)
      in
      let high =
        apply manager op (child left var true) (child right var true)
      in
      let result = branch manager var low high in
      Hashtbl.add manager.apply_cache key result;
      result

let and_ manager left right = apply manager 0 left right

let or_ manager left right = apply manager 1 left right

let and_many manager nodes =
  let cache = Int_list_table.create 251 in
  let rec loop nodes =
    if List.exists is_false nodes
    then false_
    else
      let nodes =
        List.filter (fun node -> node != true_) nodes
        |> List.sort_uniq (fun a b -> Int.compare a.id b.id)
      in
      match nodes with
      | [] -> true_
      | [node] -> node
      | [left; right] -> and_ manager left right
      | _ -> (
        let key = List.map (fun node -> node.id) nodes in
        match Int_list_table.find_opt cache key with
        | Some result -> result
        | None ->
          charge manager;
          let var =
            List.fold_left
              (fun var node -> min var (node_var node))
              max_int nodes
          in
          let result =
            branch manager var
              (loop (List.map (fun node -> child node var false) nodes))
              (loop (List.map (fun node -> child node var true) nodes))
          in
          Int_list_table.add cache key result;
          result)
  in
  loop nodes

let rec not_ manager node =
  if node == false_
  then true_
  else if node == true_
  then false_
  else
    match Hashtbl.find_opt manager.not_cache node.id with
    | Some result -> result
    | None ->
      charge manager;
      let var, low, high = Option.get node.branch in
      let result = branch manager var (not_ manager low) (not_ manager high) in
      Hashtbl.add manager.not_cache node.id result;
      result

let rec select manager var low high =
  if low == high
  then low
  else if var < node_var low && var < node_var high
  then branch manager var low high
  else
    let key = var, low.id, high.id in
    match Hashtbl.find_opt manager.select_cache key with
    | Some result -> result
    | None ->
      charge manager;
      let top = min var (min (node_var low) (node_var high)) in
      let result =
        if top = var
        then branch manager var (child low var false) (child high var true)
        else
          branch manager top
            (select manager var (child low top false) (child high top false))
            (select manager var (child low top true) (child high top true))
      in
      Hashtbl.add manager.select_cache key result;
      result

let mk manager var children =
  if
    var < 0
    || var >= arity manager
    || Array.length children <> cardinality manager var
  then invalid_arg "Solver_mdd.mk";
  (* Extra bit encodings repeat the last odd-factor slice. This makes every bit
     assignment meaningful, so Boolean complementation and quantification
     preserve finite-domain semantics without adding validity constraints. *)
  let rec build bit value =
    if bit = manager.widths.(var)
    then children.(decode manager var value)
    else
      select manager
        (bit_index manager var bit)
        (build (bit + 1) (value lsl 1))
        (build (bit + 1) ((value lsl 1) lor 1))
  in
  build 0 0

let quantify manager op var node =
  if var < 0 || var >= arity manager then invalid_arg "Solver_mdd.quantify";
  let rec loop node =
    match node.branch with
    | None -> node
    | Some (node_var, low, high) -> (
      let key = op, var, node.id in
      match Hashtbl.find_opt manager.quant_cache key with
      | Some result -> result
      | None ->
        charge manager;
        let low = loop low in
        let high = loop high in
        let result =
          if variable_of_bit manager node_var = var
          then (if op = 0 then or_ else and_) manager low high
          else branch manager node_var low high
        in
        Hashtbl.add manager.quant_cache key result;
        result)
  in
  loop node

let exists manager var node = quantify manager 0 var node

let forall manager var node = quantify manager 1 var node

let abstract_binary manager ~implication vars left right =
  let selected = Array.make (arity manager) false in
  List.iter (fun var -> selected.(var) <- true) vars;
  let cache = Hashtbl.create 251 in
  let rec loop left right =
    if implication && (left == false_ || right == true_ || left == right)
    then true_
    else if (not implication) && (left == false_ || right == false_)
    then false_
    else if left.branch = None && right.branch = None
    then if implication then false_ else true_
    else
      match Hashtbl.find_opt cache (left.id, right.id) with
      | Some result -> result
      | None ->
        charge manager;
        let var = min (node_var left) (node_var right) in
        let low = loop (child left var false) (child right var false) in
        let high = loop (child left var true) (child right var true) in
        let result =
          if selected.(variable_of_bit manager var)
          then (if implication then and_ else or_) manager low high
          else branch manager var low high
        in
        Hashtbl.add cache (left.id, right.id) result;
        result
  in
  loop left right

let and_exists manager vars left right =
  abstract_binary manager ~implication:false vars left right

let exists_many manager vars node = and_exists manager vars node true_

let implies_forall manager vars left right =
  abstract_binary manager ~implication:true vars left right

let restrict manager var value node =
  if
    var < 0
    || var >= arity manager
    || value < 0
    || value >= cardinality manager var
  then invalid_arg "Solver_mdd.restrict";
  let rec loop node =
    match node.branch with
    | None -> node
    | Some (node_var, low, high) -> (
      let key = var, value, node.id in
      match Hashtbl.find_opt manager.restrict_cache key with
      | Some result -> result
      | None ->
        charge manager;
        let result =
          if variable_of_bit manager node_var = var
          then
            let bit = bit_position manager var node_var in
            let mask = 1 lsl (manager.widths.(var) - bit - 1) in
            loop (if value land mask = 0 then low else high)
          else branch manager node_var (loop low) (loop high)
        in
        Hashtbl.add manager.restrict_cache key result;
        result)
  in
  loop node

let import ?fixed manager ~old_manager ~old_to_new node =
  if Array.length old_to_new <> arity old_manager
  then invalid_arg "Solver_mdd.import";
  let seen = Array.make (arity manager) false in
  Array.iteri
    (fun old_index new_index ->
      if new_index <> -1
      then begin
        if
          new_index < 0
          || new_index >= arity manager
          || seen.(new_index)
          || cardinality old_manager old_index <> cardinality manager new_index
        then invalid_arg "Solver_mdd.import";
        seen.(new_index) <- true
      end)
    old_to_new;
  let fixed =
    match fixed with
    | None -> Array.make (arity old_manager) None
    | Some values ->
      if Array.length values <> arity old_manager
      then invalid_arg "Solver_mdd.import: fixed arity";
      Array.iteri
        (fun i -> function
          | None -> ()
          | Some value ->
            if
              old_to_new.(i) <> -1
              || value < 0
              || value >= cardinality old_manager i
            then invalid_arg "Solver_mdd.import: fixed value")
        values;
      values
  in
  let cache = Hashtbl.create 251 in
  let rec loop node =
    match node.branch with
    | None -> node
    | Some (old_bit, low, high) -> (
      match Hashtbl.find_opt cache node.id with
      | Some result -> result
      | None ->
        charge manager;
        let old_var = variable_of_bit old_manager old_bit in
        let bit = bit_position old_manager old_var old_bit in
        let result =
          match fixed.(old_var) with
          | Some value ->
            let high_bit =
              value land (1 lsl (old_manager.widths.(old_var) - bit - 1)) <> 0
            in
            loop (if high_bit then high else low)
          | None ->
            if old_to_new.(old_var) = -1
            then invalid_arg "Solver_mdd.import: retained eliminated variable";
            let new_bit = bit_index manager old_to_new.(old_var) bit in
            select manager new_bit (loop low) (loop high)
        in
        Hashtbl.add cache node.id result;
        result)
  in
  loop node

let entails manager left right =
  let cache = Hashtbl.create 251 in
  let rec loop left right =
    if left == false_ || right == true_ || left == right
    then true
    else if left == true_ && right == false_
    then false
    else
      match Hashtbl.find_opt cache (left.id, right.id) with
      | Some result -> result
      | None ->
        charge manager;
        let var = min (node_var left) (node_var right) in
        let result =
          loop (child left var false) (child right var false)
          && loop (child left var true) (child right var true)
        in
        Hashtbl.add cache (left.id, right.id) result;
        result
  in
  loop left right

let counterexample manager left right =
  let dead = Hashtbl.create 251 in
  let rec loop left right =
    if left == false_ || right == true_ || left == right
    then None
    else if left == true_ && right == false_
    then Some []
    else if Hashtbl.mem dead (left.id, right.id)
    then None
    else begin
      charge manager;
      let bit = min (node_var left) (node_var right) in
      match loop (child left bit false) (child right bit false) with
      | Some _ as result -> result
      | None -> (
        match loop (child left bit true) (child right bit true) with
        | Some bits -> Some (bit :: bits)
        | None ->
          Hashtbl.add dead (left.id, right.id) ();
          None)
    end
  in
  Option.map
    (fun bits ->
      let values = Array.make (arity manager) 0 in
      List.iter
        (fun index ->
          let var = variable_of_bit manager index in
          let bit = bit_position manager var index in
          values.(var)
            <- values.(var) lor (1 lsl (manager.widths.(var) - bit - 1)))
        bits;
      Array.mapi (decode manager) values)
    (loop left right)

let support manager roots =
  let selected = Array.make (arity manager) false in
  let seen = Hashtbl.create 251 in
  let rec visit node =
    match node.factors, node.branch with
    | Some factors, _ -> List.iter visit factors
    | None, None -> ()
    | None, Some (bit, low, high) ->
      if not (Hashtbl.mem seen node.id)
      then begin
        Hashtbl.add seen node.id ();
        selected.(variable_of_bit manager bit) <- true;
        visit low;
        visit high
      end
  in
  List.iter visit roots;
  selected

let find_sat manager node =
  let values = Array.make (arity manager) 0 in
  let rec loop node =
    match node.branch with
    | None -> node.truth
    | Some (bit_index, low, high) ->
      if low != false_
      then loop low
      else begin
        let var = variable_of_bit manager bit_index in
        let bit = bit_position manager var bit_index in
        values.(var) <-
          values.(var) lor (1 lsl (manager.widths.(var) - bit - 1));
        loop high
      end
  in
  if loop node then Some (Array.mapi (decode manager) values) else None

let compact ?(threshold = 0) manager roots =
  let threshold =
    min threshold (max 0 ((manager.node_limit - manager.compacted_size) / 2))
  in
  if
    threshold = 0
    || Hashtbl.length manager.unique > manager.compacted_size + threshold
  then begin
    Hashtbl.clear manager.unique;
    Hashtbl.clear manager.apply_cache;
    Hashtbl.clear manager.select_cache;
    Hashtbl.clear manager.not_cache;
    Hashtbl.clear manager.quant_cache;
    Hashtbl.clear manager.restrict_cache;
    let visited = Hashtbl.create 251 in
    let rec visit node =
      match node.factors, node.branch with
      | Some factors, _ -> List.iter visit factors
      | None, None -> ()
      | None, Some (var, low, high) ->
        if not (Hashtbl.mem visited node.id)
        then begin
          Hashtbl.add visited node.id ();
          Hashtbl.replace manager.unique (var, low.id, high.id) node;
          visit low;
          visit high
        end
    in
    List.iter visit roots;
    manager.compacted_size <- Hashtbl.length manager.unique
  end

let bdd_exists = exists

let bdd_exists_many = exists_many

let bdd_implies_forall = implies_forall

let bdd_mk = mk

let bdd_or = or_

let bdd_and = and_

let bdd_not = not_

let bdd_forall = forall

let bdd_restrict = restrict

let bdd_import = import

let bdd_entails = entails

let bdd_counterexample = counterexample

let bdd_find_sat = find_sat

let bdd_and_many = and_many

let with_work_budget manager f =
  manager.remaining_work <- 10_000_000;
  f ()

let factors node =
  match node.factors with None -> [node] | Some nodes -> nodes

let normalize nodes =
  List.filter (fun node -> node != true_) nodes
  |> List.sort_uniq (fun left right -> Int.compare left.id right.id)

let conjunction manager nodes =
  let nodes = List.concat_map factors nodes |> normalize in
  if List.exists (fun node -> node == false_) nodes
  then false_
  else
    match nodes with
    | [] -> true_
    | [node] -> node
    | _ ->
      charge manager;
      if List.length nodes > manager.node_limit || manager.next_id = max_int
      then raise Limit;
      let node =
        { id = manager.next_id;
          branch = None;
          truth = false;
          factors = Some nodes;
          owner = Some manager
        }
      in
      manager.next_id <- manager.next_id + 1;
      node

let assignment manager bits =
  let values = Array.make (arity manager) 0 in
  List.iter
    (fun index ->
      let var = variable_of_bit manager index in
      let bit = bit_position manager var index in
      values.(var) <- values.(var) lor (1 lsl (manager.widths.(var) - bit - 1)))
    bits;
  Array.mapi (decode manager) values

let search ?(prefer_high = false) manager positive negative =
  let dead = Int_lists_table.create 251 in
  let restricted = Hashtbl.create 251 in
  let rec cofactor bit high node =
    if node_var node > bit
    then node
    else if node_var node = bit
    then child node bit high
    else
      match Hashtbl.find_opt restricted (bit, high, node.id) with
      | Some result -> result
      | None ->
        charge manager;
        let var, low, right = Option.get node.branch in
        let result =
          branch manager var (cofactor bit high low) (cofactor bit high right)
        in
        Hashtbl.add restricted (bit, high, node.id) result;
        result
  in
  let forced positive negative =
    match
      List.find_map
        (fun node ->
          match node.branch with
          | Some (bit, low, _) when low == false_ -> Some (bit, true)
          | Some (bit, _, high) when high == false_ -> Some (bit, false)
          | _ -> None)
        positive
    with
    | Some _ as result -> result
    | None ->
      List.find_map
        (fun node ->
          match node.branch with
          | Some (bit, low, _) when low == true_ -> Some (bit, true)
          | Some (bit, _, high) when high == true_ -> Some (bit, false)
          | _ -> None)
        negative
  in
  let rec loop positive negative =
    if
      List.exists (fun node -> node == false_) positive
      || List.exists (fun node -> node == true_) negative
      || List.exists (fun node -> List.memq node negative) positive
    then None
    else
      let positive = normalize positive in
      let negative =
        List.filter (fun node -> node != false_) negative
        |> List.sort_uniq (fun a b -> Int.compare a.id b.id)
      in
      if positive = [] && negative = []
      then Some []
      else
        let key =
          ( List.map (fun node -> node.id) positive,
            List.map (fun node -> node.id) negative )
        in
        if Int_lists_table.mem dead key
        then None
        else begin
          charge manager;
          let forced = forced positive negative in
          let bit =
            match forced with
            | Some (bit, _) -> bit
            | None ->
              List.fold_left
                (fun bit node -> min bit (node_var node))
                max_int (positive @ negative)
          in
          let descend high =
            loop
              (List.map (cofactor bit high) positive)
              (List.map (cofactor bit high) negative)
          in
          let result =
            match forced with
            | Some (_, high) ->
              Option.map
                (fun bits -> if high then bit :: bits else bits)
                (descend high)
            | None -> (
              let choose high =
                Option.map
                  (fun bits -> if high then bit :: bits else bits)
                  (descend high)
              in
              match choose prefer_high with
              | Some _ as result -> result
              | None -> choose (not prefer_high))
          in
          if result = None then Int_lists_table.add dead key ();
          result
        end
  in
  loop positive negative

let selected_variables manager vars =
  let selected = Array.make (arity manager) false in
  List.iter
    (fun var ->
      if var < 0 || var >= arity manager then invalid_arg "Solver_mdd.quantify";
      selected.(var) <- true)
    vars;
  selected

module Bit_set = Set.Make (Int)

let bits_of_variables manager vars =
  ignore (selected_variables manager vars);
  List.concat_map
    (fun var -> List.init manager.widths.(var) (bit_index manager var))
    vars
  |> List.sort_uniq Int.compare

let bit_support manager node =
  let seen = Hashtbl.create 31 in
  let rec visit bits node =
    if Hashtbl.mem seen node.id
    then bits
    else begin
      charge manager;
      Hashtbl.add seen node.id ();
      match node.factors, node.branch with
      | Some nodes, _ -> List.fold_left visit bits nodes
      | None, None -> bits
      | None, Some (bit, low, high) ->
        visit (visit (Bit_set.add bit bits) low) high
    end
  in
  visit Bit_set.empty node

let project_conjunction_bits manager bits nodes =
  let selected = Bit_set.of_list bits in
  let cache = Int_list_table.create 251 in
  let rec loop nodes =
    if List.exists (fun node -> node == false_) nodes
    then false_
    else
      let nodes = normalize nodes in
      match nodes with
      | [] -> true_
      | _ -> (
        let key = List.map (fun node -> node.id) nodes in
        match Int_list_table.find_opt cache key with
        | Some result -> result
        | None ->
          charge manager;
          let bit =
            List.fold_left
              (fun bit node -> min bit (node_var node))
              max_int nodes
          in
          let low = loop (List.map (fun node -> child node bit false) nodes) in
          let high = loop (List.map (fun node -> child node bit true) nodes) in
          let result =
            if Bit_set.mem bit selected
            then bdd_or manager low high
            else branch manager bit low high
          in
          Int_list_table.add cache key result;
          result)
  in
  loop nodes

module Elimination_order = Set.Make (struct
  type t = int * int

  let compare = Stdlib.compare
end)

let project manager vars node =
  let bits = bits_of_variables manager vars in
  let selected = Bit_set.of_list bits in
  let pending = ref selected in
  let order =
    ref (Elimination_order.of_list (List.map (fun bit -> 0, bit) bits))
  in
  let live = Hashtbl.create 251 and buckets = Hashtbl.create 251 in
  let support_cache = Hashtbl.create 251 in
  let support_of node =
    match Hashtbl.find_opt support_cache node.id with
    | Some bits -> bits
    | None ->
      let bits = Bit_set.inter selected (bit_support manager node) in
      Hashtbl.add support_cache node.id bits;
      bits
  in
  let bucket bit =
    match Hashtbl.find_opt buckets bit with
    | Some bucket -> bucket
    | None ->
      let bucket = Hashtbl.create 7 in
      Hashtbl.add buckets bit bucket;
      bucket
  in
  let update bit f =
    let bucket = bucket bit in
    let before = Hashtbl.length bucket in
    f bucket;
    if Bit_set.mem bit !pending
    then
      order
        := Elimination_order.add
             (Hashtbl.length bucket, bit)
             (Elimination_order.remove (before, bit) !order)
  in
  let add node =
    if node != true_ && not (Hashtbl.mem live node.id)
    then begin
      Hashtbl.add live node.id node;
      Bit_set.iter
        (fun bit ->
          update bit (fun bucket -> Hashtbl.replace bucket node.id node))
        (support_of node)
    end
  in
  let remove node =
    Hashtbl.remove live node.id;
    Bit_set.iter
      (fun bit -> update bit (fun bucket -> Hashtbl.remove bucket node.id))
      (support_of node)
  in
  List.iter add (factors node);
  let rec eliminate () =
    match Elimination_order.min_elt_opt !order with
    | None ->
      conjunction manager
        (Hashtbl.fold (fun _ node nodes -> node :: nodes) live [])
    | Some ((_, bit) as first) ->
      charge manager;
      order := Elimination_order.remove first !order;
      pending := Bit_set.remove bit !pending;
      let dependent =
        Hashtbl.fold (fun _ node nodes -> node :: nodes) (bucket bit) []
      in
      List.iter remove dependent;
      let projected = project_conjunction_bits manager [bit] dependent in
      if projected == false_
      then false_
      else begin
        add projected;
        eliminate ()
      end
  in
  if Hashtbl.mem live false_.id then false_ else eliminate ()

let implication_forall_bits manager bits left right =
  let selected = Bit_set.of_list bits in
  let cache = Int_lists_table.create 251 in
  let rec loop left right =
    if List.exists (fun node -> node == false_) left
    then true_
    else
      let left = normalize left in
      let right =
        normalize right |> List.filter (fun node -> not (List.memq node left))
      in
      if right = []
      then true_
      else if left = [] && List.exists (fun node -> node == false_) right
      then false_
      else
        let key =
          ( List.map (fun node -> node.id) left,
            List.map (fun node -> node.id) right )
        in
        match Int_lists_table.find_opt cache key with
        | Some result -> result
        | None ->
          charge manager;
          let bit =
            List.fold_left
              (fun bit node -> min bit (node_var node))
              max_int (left @ right)
          in
          let descend high =
            loop
              (List.map (fun node -> child node bit high) left)
              (List.map (fun node -> child node bit high) right)
          in
          let low = descend false in
          let high = descend true in
          let result =
            if Bit_set.mem bit selected
            then bdd_and manager low high
            else branch manager bit low high
          in
          Int_lists_table.add cache key result;
          result
  in
  loop (factors left) (factors right)

let force manager node =
  match node.factors with
  | None -> node
  | Some nodes -> bdd_and_many manager nodes

let factor manager node =
  with_work_budget manager (fun () ->
      let cache = Hashtbl.create 31 in
      let rec split node =
        match node.factors, node.branch with
        | Some nodes, _ -> List.concat_map split nodes |> normalize
        | None, None -> if node.truth then [] else [false_]
        | None, Some (var, low, high) -> (
          match Hashtbl.find_opt cache node.id with
          | Some result -> result
          | None ->
            charge manager;
            let result =
              if low == false_
              then branch manager var false_ true_ :: split high
              else if high == false_
              then branch manager var true_ false_ :: split low
              else
                let left = split low and right = split high in
                let common, left =
                  List.partition (fun n -> List.memq n right) left
                in
                let right =
                  List.filter (fun n -> not (List.memq n common)) right
                in
                branch manager var
                  (bdd_and_many manager left)
                  (bdd_and_many manager right)
                :: common
            in
            let result = normalize result in
            Hashtbl.add cache node.id result;
            result)
      in
      conjunction manager (split node))

let mk manager var children =
  with_work_budget manager (fun () ->
      bdd_mk manager var (Array.map (force manager) children))

let and_many manager nodes =
  with_work_budget manager (fun () -> conjunction manager nodes)

let and_ manager left right = and_many manager [left; right]

let or_ manager left right =
  with_work_budget manager (fun () ->
      bdd_or manager (force manager left) (force manager right))

let not_ manager node =
  with_work_budget manager (fun () -> bdd_not manager (force manager node))

let exists_many manager vars node =
  with_work_budget manager (fun () ->
      match node.factors with
      | None -> bdd_exists_many manager vars node
      | Some _ -> project manager vars node)

let exists manager var node =
  with_work_budget manager (fun () ->
      match node.factors with
      | None -> bdd_exists manager var node
      | Some _ -> project manager [var] node)

let forall manager var node =
  with_work_budget manager (fun () ->
      conjunction manager (List.map (bdd_forall manager var) (factors node)))

let and_exists manager vars left right =
  with_work_budget manager (fun () ->
      project manager vars (conjunction manager [left; right]))

let implies_forall manager vars left right =
  with_work_budget manager (fun () ->
      match left.factors, right.factors with
      | None, None -> bdd_implies_forall manager vars left right
      | _ ->
        implication_forall_bits manager
          (bits_of_variables manager vars)
          left right)

let restrict manager var value node =
  with_work_budget manager (fun () ->
      conjunction manager
        (List.map (bdd_restrict manager var value) (factors node)))

let import ?fixed manager ~old_manager ~old_to_new node =
  with_work_budget manager (fun () ->
      conjunction manager
        (List.map
           (bdd_import ?fixed manager ~old_manager ~old_to_new)
           (factors node)))

let remember_model manager node values =
  let old =
    Option.value (Hashtbl.find_opt manager.models_cache node.id) ~default:[]
  in
  if not (List.exists (( = ) values) old)
  then begin
    if Hashtbl.length manager.models_cache >= 64
    then Hashtbl.clear manager.models_cache;
    let old =
      if List.length old >= 8 then List.filteri (fun i _ -> i < 7) old else old
    in
    Hashtbl.replace manager.models_cache node.id (Array.copy values :: old)
  end

let rec satisfied manager values node =
  charge manager;
  match node.factors, node.branch with
  | Some factors, _ -> List.for_all (satisfied manager values) factors
  | None, None -> node.truth
  | None, Some (index, low, high) ->
    let var = variable_of_bit manager index in
    let bit = bit_position manager var index in
    let high_bit =
      values.(var) land (1 lsl (manager.widths.(var) - bit - 1)) <> 0
    in
    satisfied manager values (if high_bit then high else low)

let counterexample manager left right =
  with_work_budget manager (fun () ->
      let known =
        Option.value (Hashtbl.find_opt manager.models_cache left.id) ~default:[]
      in
      match
        List.find_opt (fun values -> not (satisfied manager values right)) known
      with
      | Some values -> Some (Array.copy values)
      | None ->
        let result =
          match left.factors, right.factors with
          | None, None -> bdd_counterexample manager left right
          | _ ->
            let rec find = function
              | [] -> None
              | node :: rest -> (
                if List.memq node (factors left)
                then find rest
                else
                  match search manager (factors left) [node] with
                  | Some bits -> Some (assignment manager bits)
                  | None -> find rest)
            in
            find (factors right)
        in
        Option.iter (remember_model manager left) result;
        result)

let entails manager left right =
  match left.factors, right.factors with
  | None, None ->
    with_work_budget manager (fun () -> bdd_entails manager left right)
  | _ -> Option.is_none (counterexample manager left right)

let find_sat ?(prefer_high = false) manager node =
  with_work_budget manager (fun () ->
      let result =
        match node.factors with
        | None when not prefer_high -> bdd_find_sat manager node
        | _ ->
          Option.map (assignment manager)
            (search ~prefer_high manager (factors node) [])
      in
      Option.iter (remember_model manager node) result;
      result)

let is_false node =
  match node.factors, node.owner with
  | None, _ -> node == false_
  | Some _, Some manager ->
    (not (Hashtbl.mem manager.models_cache node.id))
    && Option.is_none (find_sat manager node)
  | Some _, None -> assert false

let eliminate_guarded manager ~universal vars ~domain ~witness =
  with_work_budget manager (fun () ->
      ignore (selected_variables manager vars);
      let combined = conjunction manager [domain; witness] in
      let same =
        let left = factors domain and right = factors combined in
        List.length left = List.length right && List.for_all2 ( == ) left right
      in
      if same
      then
        let projected = project manager vars domain in
        projected, projected
      else if not universal
      then project manager vars domain, project manager vars combined
      else
        let support_cache = Hashtbl.create 251 in
        let support_of node =
          match Hashtbl.find_opt support_cache node.id with
          | Some support -> support
          | None ->
            let variables = bit_support manager node in
            Hashtbl.add support_cache node.id variables;
            variables
        in
        let mentions bit node = Bit_set.mem bit (support_of node) in
        let rec eliminate domain witness = function
          | [] -> domain, conjunction manager [domain; witness]
          | bits ->
            let guards = factors domain and winning = factors witness in
            let counts = Hashtbl.create 251 in
            List.iter
              (fun node ->
                Bit_set.iter
                  (fun bit ->
                    let count =
                      Option.value (Hashtbl.find_opt counts bit) ~default:0
                    in
                    Hashtbl.replace counts bit (count + 1))
                  (support_of node))
              (guards @ winning);
            let degree bit =
              Option.value (Hashtbl.find_opt counts bit) ~default:0
            in
            let bit, _ =
              List.fold_left
                (fun (best, best_degree) bit ->
                  let degree = degree bit in
                  if degree < best_degree
                  then bit, degree
                  else best, best_degree)
                (List.hd bits, max_int)
                bits
            in
            let rest = List.filter (( <> ) bit) bits in
            let guard_bound, guard_free =
              List.partition (mentions bit) guards
            in
            let winning_bound, winning_free =
              List.partition (mentions bit) winning
            in
            let projected =
              project_conjunction_bits manager [bit] guard_bound
            in
            let good =
              implication_forall_bits manager [bit]
                (conjunction manager guard_bound)
                (conjunction manager winning_bound)
            in
            let domain = conjunction manager (projected :: guard_free) in
            let witness =
              conjunction manager (domain :: good :: winning_free)
            in
            eliminate domain witness rest
        in
        eliminate domain witness (bits_of_variables manager vars))
