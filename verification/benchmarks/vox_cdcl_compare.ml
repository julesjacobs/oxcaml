open Vox_sat

let random_3cnf n clauses seed =
  let random = Random.State.make [|seed|] in
  List.init clauses (fun _ ->
    List.init 3 (fun _ ->
      let variable = Random.State.int random n in
      if Random.State.bool random then Positive variable
      else Negative variable))

let run n clauses =
  let formula = random_3cnf n clauses 42 in
  let started = Sys.time () in
  let mutable_result = Vox_cdcl.solve 1_000_000 n formula in
  let mutable_time = Sys.time () -. started in
  let started = Sys.time () in
  let total_result = Vox_cdcl_total.solve 1_000_000 n formula in
  let total_time = Sys.time () -. started in
  let mutable_status, mutable_learned = match mutable_result with
    | Ok {answer = Vox_cdcl.Sat _; statistics} -> "sat", statistics.learned
    | Ok {answer = Vox_cdcl.Unsat _; statistics} -> "unsat", statistics.learned
    | Ok {answer = Vox_cdcl.Unknown; statistics} -> "unknown", statistics.learned
    | Error _ -> "error", 0
  in
  let total_status, total_learned = match total_result with
    | Ok {answer = Vox_cdcl_total.Sat _; statistics} ->
      "sat", statistics.learned
    | Ok {answer = Vox_cdcl_total.Unsat _; statistics} ->
      "unsat", statistics.learned
    | Ok {answer = Vox_cdcl_total.Unknown; statistics} ->
      "unknown", statistics.learned
    | Error _ -> "error", 0
  in
  Printf.printf
    "%d/%d mutable=%s,%d,%.3fs total=%s,%d,%.3fs\n%!"
    n clauses mutable_status mutable_learned mutable_time
    total_status total_learned total_time

let () =
  run 50 218;
  run 100 430
