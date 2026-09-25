type edge =
  { source : int;
    target : int;
    morph : int array
  }

type t =
  { domains : int array;
    orders : bool array array array;
    joins : int array array array;
    unary : bool array array;
    edges : edge list;
    unsat : bool;
    mutable remaining : int
  }

exception Limit

let max_work = 200_000_000

let charge graph amount =
  graph.remaining <- graph.remaining - amount;
  if graph.remaining < 0 then raise Limit

let for_alli predicate array =
  let rec loop index =
    index = Array.length array
    || (predicate index array.(index) && loop (index + 1))
  in
  loop 0

let extremum order ~least values =
  let candidate = ref None in
  Array.iteri
    (fun value present ->
      if present
      then
        match !candidate with
        | None -> candidate := Some value
        | Some previous ->
          if
            if least then order.(value).(previous) else order.(previous).(value)
          then candidate := Some value)
    values;
  match !candidate with
  | None -> None
  | Some value ->
    if
      for_alli
        (fun other present ->
          (not present)
          || if least then order.(value).(other) else order.(other).(value))
        values
    then Some value
    else None

let join_table order =
  let size = Array.length order in
  Array.init size (fun left ->
      Array.init size (fun right ->
          let common =
            Array.init size (fun value ->
                order.(left).(value) && order.(right).(value))
          in
          match extremum order ~least:true common with
          | Some value -> value
          | None -> invalid_arg "Solver_graph_compact: no join"))

let join_cache = Hashtbl.create 7

let joins_for orders =
  Array.map
    (fun order ->
      match Hashtbl.find_opt join_cache order with
      | Some joins -> joins
      | None ->
        let joins = join_table order in
        Hashtbl.add join_cache order joins;
        joins)
    orders

let join_preserving source target morph =
  let result = ref true in
  for left = 0 to Array.length morph - 1 do
    for right = 0 to Array.length morph - 1 do
      if morph.(source.(left).(right)) <> target.(morph.(left)).(morph.(right))
      then result := false
    done
  done;
  !result

let merge_edges graph edges =
  let merged = Hashtbl.create 37 in
  List.iter
    (fun edge ->
      let key = edge.source, edge.target in
      let morph =
        match Hashtbl.find_opt merged key with
        | None -> edge.morph
        | Some prior ->
          let join = graph.joins.(edge.target) in
          Array.mapi
            (fun value result -> join.(result).(prior.(value)))
            edge.morph
      in
      Hashtbl.replace merged key morph)
    edges;
  Hashtbl.fold
    (fun (source, target) morph edges -> { source; target; morph } :: edges)
    merged []

let create ~domains ~orders atoms =
  if Array.length domains <> Array.length orders
  then invalid_arg "Solver_graph_compact.create";
  let joins = joins_for orders in
  let graph =
    { domains;
      orders;
      joins;
      unary = Array.map (fun size -> Array.make size true) domains;
      edges = [];
      unsat = false;
      remaining = max_work
    }
  in
  let edges = ref [] in
  let supported = ref true in
  let add_unary index values =
    Array.iteri
      (fun value allowed ->
        graph.unary.(index).(value) <- graph.unary.(index).(value) && allowed)
      values
  in
  let orient source target relation =
    let source_size = domains.(source) in
    let target_size = domains.(target) in
    let morph = Array.make source_size 0 in
    let guard = Array.make source_size true in
    let possible = ref true in
    for source_value = 0 to source_size - 1 do
      let slice =
        Array.init target_size (fun target_value ->
            relation source_value target_value)
      in
      match extremum orders.(target) ~least:true slice with
      | None -> guard.(source_value) <- false
      | Some least ->
        morph.(source_value) <- least;
        if
          not
            (for_alli
               (fun value present -> present = orders.(target).(least).(value))
               slice)
        then possible := false
    done;
    if !possible && join_preserving joins.(source) joins.(target) morph
    then Some (guard, { source; target; morph })
    else None
  in
  List.iter
    (function
      | Solver_graph.Unary (index, values) -> add_unary index values
      | Solver_graph.Binary (left, right, values) when left = right ->
        add_unary left
          (Array.init domains.(left) (fun value -> values.(value).(value)))
      | Solver_graph.Binary (left, right, values) -> (
        let forward = orient left right (fun x y -> values.(x).(y)) in
        let backward =
          match forward with
          | Some _ -> None
          | None -> orient right left (fun y x -> values.(x).(y))
        in
        match forward, backward with
        | Some (guard, edge), _ | None, Some (guard, edge) ->
          add_unary edge.source guard;
          edges := edge :: !edges
        | None, None -> supported := false))
    atoms;
  if not !supported
  then None
  else
    let unsat =
      Array.exists (fun values -> not (Array.exists Fun.id values)) graph.unary
    in
    Some { graph with edges = merge_edges graph !edges; unsat }

let interval order values =
  match
    extremum order ~least:true values, extremum order ~least:false values
  with
  | Some lower, Some upper ->
    if
      for_alli
        (fun value allowed ->
          allowed = (order.(lower).(value) && order.(value).(upper)))
        values
    then Some (lower, upper)
    else None
  | _ -> None

let project_one ~allow_self_edges graph variable =
  if graph.unsat
  then Some graph
  else
    let unary = Array.map Array.copy graph.unary in
    let incoming, outgoing, retained =
      List.fold_left
        (fun (incoming, outgoing, retained) edge ->
          if edge.source = variable && edge.target = variable
          then begin
            if not allow_self_edges then raise Limit;
            Array.iteri
              (fun value allowed ->
                unary.(variable).(value)
                  <- allowed
                     && graph.orders.(variable).(edge.morph.(value)).(value))
              unary.(variable);
            incoming, outgoing, retained
          end
          else if edge.target = variable
          then edge :: incoming, outgoing, retained
          else if edge.source = variable
          then incoming, edge :: outgoing, retained
          else incoming, outgoing, edge :: retained)
        ([], [], []) graph.edges
    in
    if not (Array.exists Fun.id unary.(variable))
    then Some { graph with unary; edges = []; unsat = true }
    else
      match interval graph.orders.(variable) unary.(variable) with
      | None -> None
      | Some (lower, upper) ->
        unary.(variable) <- Array.make graph.domains.(variable) true;
        List.iter
          (fun edge ->
            let order = graph.orders.(variable) in
            Array.iteri
              (fun value required ->
                unary.(edge.source).(value)
                  <- unary.(edge.source).(value) && order.(required).(upper))
              edge.morph)
          incoming;
        List.iter
          (fun edge ->
            let required = edge.morph.(lower) in
            let order = graph.orders.(edge.target) in
            Array.iteri
              (fun value allowed ->
                unary.(edge.target).(value)
                  <- allowed && order.(required).(value))
              unary.(edge.target))
          outgoing;
        let generated =
          List.concat_map
            (fun incoming ->
              List.map
                (fun outgoing ->
                  charge graph (Array.length incoming.morph);
                  { source = incoming.source;
                    target = outgoing.target;
                    morph =
                      Array.map
                        (fun intermediate -> outgoing.morph.(intermediate))
                        incoming.morph
                  })
                outgoing)
            incoming
        in
        let edges = merge_edges graph (generated @ retained) in
        let unsat =
          Array.exists (fun values -> not (Array.exists Fun.id values)) unary
        in
        Some { graph with unary; edges; unsat }

let project ?(allow_self_edges = false) graph variables =
  let degree graph variable =
    List.fold_left
      (fun count edge ->
        if edge.source = variable || edge.target = variable
        then count + 1
        else count)
      0 graph.edges
  in
  let rec loop graph = function
    | [] -> Some graph
    | variables -> (
      let selected =
        List.fold_left
          (fun best variable ->
            if degree graph variable < degree graph best then variable else best)
          (List.hd variables) (List.tl variables)
      in
      let remaining = List.filter (( <> ) selected) variables in
      match project_one ~allow_self_edges graph selected with
      | None -> None
      | Some graph -> loop graph remaining)
  in
  loop graph variables

let satisfies graph assignment =
  (not graph.unsat)
  && for_alli (fun index values -> values.(assignment.(index))) graph.unary
  && List.for_all
       (fun edge ->
         graph.orders.(edge.target).(edge.morph.(assignment.(edge.source))).(assignment.(
                                                                             edge
                                                                               .target)))
       graph.edges

let path_order graph =
  let count = Array.length graph.domains in
  let outgoing = Array.make count [] in
  let indegree = Array.make count 0 in
  List.iter
    (fun edge ->
      outgoing.(edge.source) <- edge :: outgoing.(edge.source);
      indegree.(edge.target) <- indegree.(edge.target) + 1)
    graph.edges;
  let queue = Queue.create () in
  Array.iteri
    (fun index degree -> if degree = 0 then Queue.add index queue)
    indegree;
  let order = ref [] in
  while not (Queue.is_empty queue) do
    let variable = Queue.take queue in
    order := variable :: !order;
    List.iter
      (fun edge ->
        indegree.(edge.target) <- indegree.(edge.target) - 1;
        if indegree.(edge.target) = 0 then Queue.add edge.target queue)
      outgoing.(variable)
  done;
  if List.length !order = count then Some (List.rev !order, outgoing) else None

let path_bounds graph order outgoing source =
  let paths = Array.make (Array.length graph.domains) None in
  paths.(source) <- Some (Array.init graph.domains.(source) Fun.id);
  List.iter
    (fun variable ->
      match paths.(variable) with
      | None -> ()
      | Some prefix ->
        List.iter
          (fun edge ->
            charge graph (Array.length prefix);
            let composed = Array.map (fun value -> edge.morph.(value)) prefix in
            let merged =
              match paths.(edge.target) with
              | None -> composed
              | Some previous ->
                let joins = graph.joins.(edge.target) in
                Array.mapi
                  (fun value result -> joins.(result).(previous.(value)))
                  composed
            in
            paths.(edge.target) <- Some merged)
          outgoing.(variable))
    order;
  paths

let least_model graph intervals fixed =
  let values = Array.map fst intervals in
  match fixed with
  | Some (index, value) when not graph.unary.(index).(value) -> None
  | _ -> (
    (match fixed with
    | None -> ()
    | Some (index, value) -> values.(index) <- value);
    let exception No_model in
    try
      let changed = ref true in
      while !changed do
        changed := false;
        List.iter
          (fun edge ->
            charge graph 1;
            let required = edge.morph.(values.(edge.source)) in
            let old = values.(edge.target) in
            let next = graph.joins.(edge.target).(old).(required) in
            if next <> old
            then begin
              (match fixed with
              | Some (index, value) when index = edge.target && next <> value ->
                raise No_model
              | _ -> ());
              if
                not
                  graph.orders.(edge.target).(next).(snd intervals.(edge.target))
              then raise No_model;
              values.(edge.target) <- next;
              changed := true
            end)
          graph.edges
      done;
      if satisfies graph values then Some values else None
    with No_model -> None)

let implies graph consequence =
  if graph.unsat
  then Some true
  else
    let tasks =
      Array.to_list
        (Array.mapi
           (fun index values -> `Unary (index, values))
           consequence.unary)
      @ List.map (fun edge -> `Edge edge) consequence.edges
    in
    let paths = Hashtbl.create 17 in
    let order = path_order graph in
    let path_implies edge =
      match order with
      | None -> false
      | Some (order, outgoing) -> (
        let bounds =
          match Hashtbl.find_opt paths edge.source with
          | Some bounds -> bounds
          | None ->
            let bounds = path_bounds graph order outgoing edge.source in
            Hashtbl.add paths edge.source bounds;
            bounds
        in
        match bounds.(edge.target) with
        | None -> false
        | Some bound ->
          for_alli
            (fun value actual ->
              graph.orders.(edge.target).(edge.morph.(value)).(actual))
            bound)
    in
    let intervals =
      Array.mapi
        (fun index values -> interval graph.orders.(index) values)
        graph.unary
    in
    if not (Array.for_all Option.is_some intervals)
    then None
    else
      let intervals = Array.map Option.get intervals in
      let models = Hashtbl.create 37 in
      let model index value =
        match Hashtbl.find_opt models (index, value) with
        | Some result -> result
        | None ->
          let result = least_model graph intervals (Some (index, value)) in
          Hashtbl.add models (index, value) result;
          result
      in
      let rec check = function
        | [] -> Some true
        | task :: rest ->
          let already_implied =
            match task with
            | `Unary (index, values) ->
              for_alli
                (fun value allowed -> (not allowed) || values.(value))
                graph.unary.(index)
            | `Edge edge ->
              path_implies edge
              || List.exists
                   (fun premise ->
                     premise.source = edge.source
                     && premise.target = edge.target
                     && for_alli
                          (fun value result ->
                            graph.orders.(edge.target).(edge.morph.(value)).(result))
                          premise.morph)
                   graph.edges
          in
          if already_implied
          then check rest
          else
            let counterexample =
              match task with
              | `Unary (index, allowed) ->
                let rec loop value =
                  value < graph.domains.(index)
                  && ((not allowed.(value))
                      && Option.is_some (model index value)
                     || loop (value + 1))
                in
                loop 0
              | `Edge edge ->
                let rec loop value =
                  value < graph.domains.(edge.source)
                  && ((match model edge.source value with
                        | None -> false
                        | Some assignment ->
                          not
                            consequence.orders.(edge.target).(edge.morph.(value)).(
                            assignment.(edge.target)))
                     || loop (value + 1))
                in
                loop 0
            in
            if counterexample then Some false else check rest
      in
      check tasks
