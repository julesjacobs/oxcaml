open Vox_sat_spec
open Vox_sat

let implication_chain n =
  [ [Positive 0] ]
  @ List.init (n - 1) (fun i -> [Negative i; Positive (i + 1)])
  @ [ [Negative (n - 1)] ]

let random_3cnf n clauses seed =
  let state = Random.State.make [|seed|] in
  List.init clauses (fun _ ->
    List.init 3 (fun _ ->
      let variable = Random.State.int state n in
      if Random.State.bool state then Positive variable
      else Negative variable))

let run name n formula fuel =
  let started = Sys.time () in
  let result = solve fuel n formula in
  let elapsed = Sys.time () -. started in
  let status, remaining =
    match result with
    | Error _ -> "error", fuel
    | Ok report ->
      (match report.answer with
       | Sat _ -> "sat"
       | Unsat -> "unsat"
       | Unknown -> "unknown"),
      report.fuel_left
  in
  Printf.printf "%s vars=%d clauses=%d result=%s nodes=%d cpu=%.4f s\n%!"
    name n (List.length formula) status (fuel - remaining) elapsed

let () =
  run "chain" 256 (implication_chain 256) 300;
  List.iter (fun (n, clauses) ->
    run "random-3cnf" n (random_3cnf n clauses 42) 1_000_000)
    [20, 90; 30, 130; 50, 218; 75, 325; 100, 430]
