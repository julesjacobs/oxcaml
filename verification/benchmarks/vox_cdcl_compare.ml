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
  let mutable_result = Vox_cdcl.solve 1_000_000 n formula in
  let mutable_time = Sys.time () -. started in
  let started = Sys.time () in
  let total_result = Vox_cdcl_total.solve 1_000_000 n formula in
  let total_time = Sys.time () -. started in
  let started = Sys.time () in
  let combined_result =
    Vox_cdcl_total.solve_with_fallback 1_000_000 (n + 1) n formula in
  let combined_time = Sys.time () -. started in
  let mutable_status, mutable_learned = match mutable_result with
    | Ok {answer = Vox_cdcl.Sat _; statistics} -> "sat", statistics.learned
    | Ok {answer = Vox_cdcl.Unsat; statistics} -> "unsat", statistics.learned
    | Ok {answer = Vox_cdcl.Unknown; statistics} ->
      "unknown", statistics.learned
    | Error _ -> "error", 0
  in
  let combined_status, combined_learned = total_status combined_result in
  let total_status, total_learned = total_status total_result in
  assert (mutable_status = total_status && total_status = combined_status);
  assert (combined_status = "sat" || combined_status = "unsat");
  Printf.printf
    "%d/%d mutable=%s,%d,%.3fs total=%s,%d,%.3fs combined=%s,%d,%.3fs\n%!"
    n clauses mutable_status mutable_learned mutable_time
    total_status total_learned total_time
    combined_status combined_learned combined_time

let () =
  run 50 218;
  run 100 430
