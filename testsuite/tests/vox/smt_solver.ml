open Vox_smt

let config = { Vox_smt_solver.default_config with executable = Sys.argv.(1) }

let n = Symbol.create ~label:"n" Int63

let next = App (Sub, [Var n; Integer 1L])

let countdown facts =
  { symbols = [n];
    facts;
    goal = { label = "decreases"; term = App (Lt, [next; Var n]) }
  }

let check query = (Vox_smt_solver.check ~config ~int_width:63 query).validity

let () =
  let positive = { label = "positive"; term = App (Gt, [Var n; Integer 0L]) } in
  (match check (countdown [positive]) with
   | Valid -> print_endline "n > 0 implies n - 1 < n: proved"
   | Failure message -> failwith message
   | _ -> failwith "guarded decrement was not proved");
  match check (countdown []) with
  | Invalid (Some [(symbol, Int_value value)])
    when symbol = n && value = -4611686018427387904L ->
      Printf.printf "n - 1 < n: refuted at n = %Ld (min_int)\n" value
  | Failure message -> failwith message
  | _ -> failwith "missing signed underflow counterexample"
