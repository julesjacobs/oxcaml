(* TEST
 include ocamlcommon;
 native;
*)

let size = 4

let carrier = 3

let domains = Array.make size carrier

let order =
  Array.init size (fun _ ->
      Array.init carrier (fun left ->
          Array.init carrier (fun right -> left <= right)))

let random = Random.State.make [|0x31; 0x59; 0x26|]

let choose bound = Random.State.int random bound

let unary index =
  let lower = choose carrier in
  let upper = lower + choose (carrier - lower) in
  Solver_graph.Unary
    (index, Array.init carrier (fun value -> lower <= value && value <= upper))

let binary () =
  let left = choose size in
  let right = choose size in
  let morph = if left = right then 0 else choose 3 in
  let left_map value =
    match morph with 0 -> value | 1 -> if value = 1 then 0 else value | _ -> 0
  in
  Solver_graph.Binary
    ( left,
      right,
      Array.init carrier (fun x ->
          Array.init carrier (fun y -> left_map x <= y)) )

let holds atoms assignment =
  List.for_all
    (function
      | Solver_graph.Unary (index, values) -> values.(assignment.(index))
      | Solver_graph.Binary (left, right, values) ->
        values.(assignment.(left)).(assignment.(right)))
    atoms

let assignments f =
  let values = Array.make size 0 in
  let rec loop index =
    if index = size
    then f values
    else
      for value = 0 to carrier - 1 do
        values.(index) <- value;
        loop (index + 1)
      done
  in
  loop 0

let projection_mismatches = ref 0

let implication_mismatches = ref 0

let compact_projection_mismatches = ref 0

let compact_implication_mismatches = ref 0

let compact_false_positives = ref 0

let compact_false_negatives = ref 0

let compact_unsupported = ref 0

let dag_binary () =
  let left = choose (size - 1) in
  let right = left + 1 + choose (size - left - 1) in
  let morph = choose 3 in
  let left_map value =
    match morph with 0 -> value | 1 -> if value = 1 then 0 else value | _ -> 0
  in
  Solver_graph.Binary
    ( left,
      right,
      Array.init carrier (fun x ->
          Array.init carrier (fun y -> left_map x <= y)) )

let () =
  for _case = 1 to 300 do
    let atoms =
      List.init (choose 5) (fun _ -> unary (choose size))
      @ List.init (choose 7) (fun _ -> binary ())
    in
    let graph = Solver_graph.create ~domains ~orders:order atoms in
    (match Solver_graph.project graph [3] with
    | None -> incr projection_mismatches
    | Some projected ->
      assignments (fun assignment ->
          if assignment.(3) = 0
          then begin
            let actual = Solver_graph.satisfies projected assignment in
            let expected =
              let rec choose_inner value =
                value < carrier
                && (assignment.(3) <- value;
                    holds atoms assignment || choose_inner (value + 1))
              in
              choose_inner 0
            in
            if actual <> expected then incr projection_mismatches
          end));
    let extension = binary () in
    let witness =
      Solver_graph.create ~domains ~orders:order (extension :: atoms)
    in
    let expected = ref true in
    assignments (fun assignment ->
        if holds atoms assignment && not (holds [extension] assignment)
        then expected := false);
    (match Solver_graph.implies graph witness with
    | Some actual when actual = !expected -> ()
    | Some _ | None -> incr implication_mismatches)
  done;
  for _case = 1 to 300 do
    let atoms =
      List.init (choose 5) (fun _ -> unary (choose size))
      @ List.init (choose 7) (fun _ -> dag_binary ())
    in
    (match Solver_graph_compact.create ~domains ~orders:order atoms with
    | None -> incr compact_projection_mismatches
    | Some graph ->
      (match Solver_graph_compact.project graph [1] with
      | None -> incr compact_projection_mismatches
      | Some projected ->
        assignments (fun assignment ->
            if assignment.(1) = 0
            then begin
              let actual =
                Solver_graph_compact.satisfies projected assignment
              in
              let expected =
                let rec choose_inner value =
                  value < carrier
                  && (assignment.(1) <- value;
                      holds atoms assignment || choose_inner (value + 1))
                in
                choose_inner 0
              in
              if actual <> expected then incr compact_projection_mismatches
            end)));
    let extension = dag_binary () in
    let expected = ref true in
    assignments (fun assignment ->
        if holds atoms assignment && not (holds [extension] assignment)
        then expected := false);
    (match
       Solver_graph_compact.create ~domains ~orders:order atoms,
       Solver_graph_compact.create ~domains ~orders:order (extension :: atoms)
     with
    | Some graph, Some witness -> (
      match Solver_graph_compact.implies graph witness with
      | Some actual when actual = !expected -> ()
      | Some true ->
        incr compact_implication_mismatches;
        incr compact_false_positives
      | Some false ->
        incr compact_implication_mismatches;
        incr compact_false_negatives
      | None ->
        incr compact_implication_mismatches;
        incr compact_unsupported)
    | _ ->
      incr compact_implication_mismatches;
      incr compact_unsupported)
  done;
  let edge source target =
    Solver_graph.Binary
      ( source,
        target,
        Array.init carrier (fun x ->
            Array.init carrier (fun y -> x <= y)) )
  in
  let cyclic =
    Solver_graph_compact.create ~domains ~orders:order
      [edge 1 2; edge 2 1;
       Solver_graph.Unary (1, [|false; true; true|]);
       Solver_graph.Unary (2, [|true; true; false|])]
  in
  (match cyclic with
  | None -> failwith "cyclic graph unsupported"
  | Some graph -> (
    match Solver_graph_compact.project ~allow_self_edges:true graph [1; 2] with
    | None -> failwith "cyclic projection unsupported"
    | Some projected ->
      assignments (fun assignment ->
          if not (Solver_graph_compact.satisfies projected assignment)
          then failwith "cyclic projection mismatch")));
  Printf.printf
    "%d projection mismatches; %d implication mismatches; %d compact projection mismatches; %d compact implication mismatches (%d false positives, %d false negatives, %d unsupported)\n"
    !projection_mismatches !implication_mismatches
    !compact_projection_mismatches !compact_implication_mismatches
    !compact_false_positives !compact_false_negatives !compact_unsupported
