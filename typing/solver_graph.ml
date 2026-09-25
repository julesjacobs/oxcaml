type atom =
  | Unary of int * bool array
  | Binary of int * int * bool array array

type t =
  { domains : int array;
    orders : bool array array array;
    atoms : atom list;
    unsat : bool
  }

exception Limit

let max_cells = 10_000_000

type budget = { mutable remaining : int }

let budget () = { remaining = 20_000_000 }

let charge budget cells =
  budget.remaining <- budget.remaining - cells;
  if budget.remaining < 0 then raise Limit

let atom_cells = function
  | Unary (_, values) -> Array.length values
  | Binary (_, _, values) ->
    Array.fold_left (fun cells row -> cells + Array.length row) 0 values

let check_cells atoms =
  let remaining = ref max_cells in
  List.iter
    (fun atom ->
      remaining := !remaining - atom_cells atom;
      if !remaining < 0 then raise Limit)
    atoms

let atoms graph = graph.atoms

let for_alli predicate array =
  let rec loop index =
    index = Array.length array
    || (predicate index array.(index) && loop (index + 1))
  in
  loop 0

let normalize domains atoms =
  check_cells atoms;
  let unary = Hashtbl.create 17 in
  let binary = ref [] in
  let unsat = ref false in
  let add_unary index values =
    let values =
      match Hashtbl.find_opt unary index with
      | None -> Array.copy values
      | Some old -> Array.mapi (fun i value -> value && old.(i)) values
    in
    if not (Array.exists Fun.id values) then unsat := true;
    Hashtbl.replace unary index values
  in
  List.iter
    (function
      | Unary (index, values) -> add_unary index values
      | Binary (left, right, values) when left = right ->
        add_unary left
          (Array.init domains.(left) (fun value -> values.(value).(value)))
      | Binary (left, right, values) ->
        let left, right, values =
          if left < right
          then left, right, values
          else
            ( right,
              left,
              Array.init domains.(right) (fun i ->
                  Array.init domains.(left) (fun j -> values.(j).(i))) )
        in
        if not (Array.exists (Array.exists Fun.id) values) then unsat := true;
        binary := Binary (left, right, values) :: !binary)
    atoms;
  let result = ref [] in
  Hashtbl.iter
    (fun index values ->
      if not (Array.for_all Fun.id values)
      then result := Unary (index, values) :: !result)
    unary;
  result
    := List.filter
         (function
           | Unary _ -> true
           | Binary (_, _, values) ->
             not (Array.for_all (Array.for_all Fun.id) values))
         !binary
       @ !result;
  !result, !unsat

let create ~domains ~orders atoms =
  if Array.length domains <> Array.length orders
  then invalid_arg "Solver_graph.create";
  let atoms, unsat = normalize domains atoms in
  { domains; orders; atoms; unsat }

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

let interval order values =
  match
    extremum order ~least:true values, extremum order ~least:false values
  with
  | Some lower, Some upper ->
    if
      for_alli
        (fun value present ->
          present = (order.(lower).(value) && order.(value).(upper)))
        values
    then Some (lower, upper)
    else None
  | _ -> None

type requirement =
  | Constant of int
  | Dependent of int * int array

let project_one budget graph variable =
  if graph.unsat
  then Some graph
  else
    let order = graph.orders.(variable) in
    let retained = ref [] in
    let incident = ref [] in
    let unary = Array.make graph.domains.(variable) true in
    List.iter
      (function
        | Unary (index, values) when index = variable ->
          Array.iteri
            (fun value allowed -> unary.(value) <- unary.(value) && allowed)
            values
        | Binary (left, right, values) when left = variable ->
          incident
            := (right, fun other self -> values.(self).(other)) :: !incident
        | Binary (left, right, values) when right = variable ->
          incident
            := (left, fun other self -> values.(other).(self)) :: !incident
        | atom -> retained := atom :: !retained)
      graph.atoms;
    if not (Array.exists Fun.id unary)
    then Some { graph with atoms = []; unsat = true }
    else
      match interval order unary with
      | None -> None
      | Some (lower, upper) ->
        let lowers = ref [Constant lower] in
        let uppers = ref [Constant upper] in
        let valid = ref [] in
        let supported = ref true in
        List.iter
          (fun (neighbor, relation) ->
            let domain = graph.domains.(neighbor) in
            charge budget (domain * graph.domains.(variable));
            let slices =
              Array.init domain (fun neighbor_value ->
                  Array.init graph.domains.(variable) (fun value ->
                      relation neighbor_value value))
            in
            let allowed = Array.map (Array.exists Fun.id) slices in
            valid := Unary (neighbor, allowed) :: !valid;
            let principal least =
              Array.mapi
                (fun neighbor_value slice ->
                  if not allowed.(neighbor_value)
                  then Some 0
                  else
                    match extremum order ~least slice with
                    | None -> None
                    | Some bound ->
                      if
                        for_alli
                          (fun value present ->
                            present
                            =
                            if least
                            then order.(bound).(value)
                            else order.(value).(bound))
                          slice
                      then Some bound
                      else None)
                slices
            in
            let lower = principal true in
            let upper = principal false in
            let collect bounds =
              if Array.for_all Option.is_some bounds
              then Some (Array.map Option.get bounds)
              else None
            in
            match collect lower, collect upper with
            | Some bounds, _ ->
              lowers := Dependent (neighbor, bounds) :: !lowers
            | None, Some bounds ->
              uppers := Dependent (neighbor, bounds) :: !uppers
            | None, None -> supported := false)
          !incident;
        if not !supported
        then None
        else
          let generated = ref (!retained @ !valid) in
          let remaining = ref max_cells in
          List.iter
            (fun atom -> remaining := !remaining - atom_cells atom)
            !generated;
          let reserve cells =
            remaining := !remaining - cells;
            if !remaining < 0 then raise Limit
          in
          List.iter
            (fun lower ->
              List.iter
                (fun upper ->
                  let satisfies lower upper = order.(lower).(upper) in
                  match lower, upper with
                  | Constant left, Constant right ->
                    if not (satisfies left right)
                    then begin
                      reserve graph.domains.(variable);
                      generated
                        := Unary
                             ( variable,
                               Array.make graph.domains.(variable) false )
                           :: !generated
                    end
                  | Dependent (index, bounds), Constant right ->
                    reserve (Array.length bounds);
                    generated
                      := Unary
                           ( index,
                             Array.map (fun left -> satisfies left right) bounds
                           )
                         :: !generated
                  | Constant left, Dependent (index, bounds) ->
                    reserve (Array.length bounds);
                    generated
                      := Unary
                           ( index,
                             Array.map
                               (fun right -> satisfies left right)
                               bounds )
                         :: !generated
                  | ( Dependent (left_index, left_bounds),
                      Dependent (right_index, right_bounds) ) ->
                    reserve
                      (Array.length left_bounds * Array.length right_bounds);
                    generated
                      := Binary
                           ( left_index,
                             right_index,
                             Array.map
                               (fun left ->
                                 Array.map
                                   (fun right -> satisfies left right)
                                   right_bounds)
                               left_bounds )
                         :: !generated)
                !uppers)
            !lowers;
          let atoms, unsat = normalize graph.domains !generated in
          charge budget (max_cells - !remaining);
          Some { graph with atoms; unsat }

let project ?(budget = budget ()) graph variables =
  let degree graph variable =
    List.fold_left
      (fun degree -> function
        | Unary _ -> degree
        | Binary (left, right, _) when left = variable || right = variable ->
          degree + 1
        | Binary _ -> degree)
      0 graph.atoms
  in
  let rec loop graph = function
    | [] -> Some graph
    | variables -> (
      let variable =
        List.fold_left
          (fun best variable ->
            if degree graph variable < degree graph best then variable else best)
          (List.hd variables) (List.tl variables)
      in
      let rest = List.filter (( <> ) variable) variables in
      match project_one budget graph variable with
      | None -> None
      | Some graph -> loop graph rest)
  in
  loop graph variables

let support = function
  | Unary (index, _) -> [index]
  | Binary (left, right, _) -> [left; right]

let atom_value atom assignment =
  match atom with
  | Unary (index, values) -> values.(assignment.(index))
  | Binary (left, right, values) ->
    values.(assignment.(left)).(assignment.(right))

let satisfies graph assignment =
  (not graph.unsat)
  && List.for_all (fun atom -> atom_value atom assignment) graph.atoms

let implies ?(budget = budget ()) graph consequence =
  if graph.unsat
  then Some true
  else if consequence.unsat
  then Some false
  else
    let projection_cache = Hashtbl.create 17 in
    let local_implies atom =
      List.exists
        (fun premise ->
          match premise, atom with
          | Unary (premise_index, premise_values), Unary (index, values)
            when premise_index = index ->
            for_alli
              (fun value allowed -> (not allowed) || values.(value))
              premise_values
          | ( Binary (premise_left, premise_right, premise_values),
              Binary (left, right, values) )
            when premise_left = left && premise_right = right ->
            for_alli
              (fun i row ->
                for_alli (fun j allowed -> (not allowed) || values.(i).(j)) row)
              premise_values
          | _ -> false)
        graph.atoms
    in
    let rec check = function
      | [] -> Some true
      | atom :: rest when local_implies atom -> check rest
      | atom :: rest -> (
        let selected = support atom in
        let eliminated =
          Array.to_list (Array.mapi (fun index _ -> index) graph.domains)
          |> List.filter (fun index -> not (List.mem index selected))
        in
        charge budget (List.length graph.atoms * 16);
        let projection =
          match Hashtbl.find_opt projection_cache selected with
          | Some projection -> projection
          | None ->
            let projection = project ~budget graph eliminated in
            Hashtbl.add projection_cache selected projection;
            projection
        in
        match projection with
        | None -> None
        | Some projection ->
          let assignment = Array.make (Array.length graph.domains) 0 in
          let rec search = function
            | [] ->
              satisfies projection assignment
              && not (atom_value atom assignment)
            | index :: rest ->
              let rec values value =
                value < graph.domains.(index)
                &&
                (assignment.(index) <- value;
                 search rest || values (value + 1))
              in
              values 0
          in
          if search selected then Some false else check rest)
    in
    check consequence.atoms
