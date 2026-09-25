open Vox_sat

let random_3cnf n clauses seed =
  let state = Random.State.make [|seed|] in
  List.init clauses (fun _ ->
    List.init 3 (fun _ ->
      let variable = Random.State.int state n in
      if Random.State.bool state then Positive variable
      else Negative variable))

let run n clauses =
  let formula = random_3cnf n clauses 42 in
  let started = Sys.time () in
  let result = Vox_cdcl.solve 1_000_000 n formula in
  let elapsed = Sys.time () -. started in
  match result with
  | Error _ -> Printf.printf "%d variables: input error\n%!" n
  | Ok report ->
    let status = match report.answer with
      | Vox_cdcl.Sat _ -> "sat"
      | Vox_cdcl.Unsat _ -> "unsat"
      | Vox_cdcl.Unknown -> "unknown"
    in
    let s = report.statistics in
    Printf.printf
      "%d %s d=%d c=%d l=%d b=%d w=%d cpu=%.3f s\n%!"
      n status s.decisions s.conflicts s.learned s.backjumps s.work elapsed

let () =
  List.iter (fun (n, clauses) -> run n clauses)
    [20, 90; 30, 130; 50, 218; 75, 325; 100, 430]
