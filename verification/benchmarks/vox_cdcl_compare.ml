open Vox_sat_spec

let random_3cnf n clauses seed =
  let random = Random.State.make [|seed|] in
  List.init clauses (fun _ ->
    List.init 3 (fun _ ->
      let variable = Random.State.int random n in
      if Random.State.bool random then Positive variable
      else Negative variable))

let total_status = function
  | Ok {Vox_cdcl_total.answer = Vox_cdcl_total.Sat _; statistics} ->
    "sat", statistics.learned
  | Ok {Vox_cdcl_total.answer = Vox_cdcl_total.Unsat; statistics} ->
    "unsat", statistics.learned
  | Ok {Vox_cdcl_total.answer = Vox_cdcl_total.Unknown; statistics} ->
    "unknown", statistics.learned
  | Error _ -> "error", 0

let run n clauses =
  let formula = random_3cnf n clauses 42 in
  let started = Sys.time () in
  let bounded_result = Vox_cdcl_total.solve 1_000_000 n formula in
  let bounded_time = Sys.time () -. started in
  let started = Sys.time () in
  let complete_result = Vox_cdcl_total.solve_complete n formula in
  let complete_time = Sys.time () -. started in
  let bounded_status, bounded_learned = total_status bounded_result in
  let complete_status, complete_learned = total_status complete_result in
  assert (bounded_status = complete_status);
  assert (complete_status = "sat" || complete_status = "unsat");
  Printf.printf "%d/%d bounded=%s,%d,%.3fs complete=%s,%d,%.3fs\n%!"
    n clauses bounded_status bounded_learned bounded_time
    complete_status complete_learned complete_time

let () =
  run 50 218;
  run 100 430
