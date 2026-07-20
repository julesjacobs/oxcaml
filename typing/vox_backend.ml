type backend =
  | Lean
  | Z3
  | Oxsmt

type selection =
  | Single of backend
  | Cross

type verdict =
  | Proved
  | Not_proved
  | Disproved
  | Unknown
  | Solver_error
  | Unavailable

type fact_usage_capability =
  | Fact_usage
  | No_fact_usage

type capabilities =
  { fact_usage : fact_usage_capability;
  }

type obligation =
  { env : Env.t;
    condition : Vox_vc.t;
    prove_contents : string option;
  }

type backend_result =
  { backend : backend;
    capabilities : capabilities;
    verdict : verdict;
    location : Location.t;
    detail : string option;
    unused_facts : int list option;
  }

type result =
  { verdict : verdict;
    location : Location.t;
    detail : string option;
    unused_facts : int list option;
    backend_results : backend_result list;
  }

module type S = sig
  val backend : backend
  val capabilities : capabilities

  val discharge :
    command:string option -> obligation -> backend_result
end

let string_of_backend = function
  | Lean -> "lean"
  | Z3 -> "z3"
  | Oxsmt -> "oxsmt"

let string_of_selection = function
  | Single backend -> string_of_backend backend
  | Cross -> "cross"

let string_of_verdict = function
  | Proved -> "proved"
  | Not_proved -> "not-proved"
  | Disproved -> "disproved"
  | Unknown -> "unknown"
  | Solver_error -> "solver-error"
  | Unavailable -> "unavailable"

let backend_of_string = function
  | "lean" -> Ok Lean
  | "z3" -> Ok Z3
  | "oxsmt" -> Ok Oxsmt
  | backend ->
    Error
      (Printf.sprintf
         "unknown refinement discharge backend %S (expected lean, z3, or \
          oxsmt)"
         backend)

let selection_of_string = function
  | "cross" -> Ok Cross
  | backend ->
    Result.map (fun backend -> Single backend) (backend_of_string backend)

let capabilities = function
  | Lean | Z3 | Oxsmt -> { fact_usage = Fact_usage }

let resolve_command ~explicit ~environment ~fallback =
  match explicit with
  | Some _ as command -> command
  | None ->
    begin
      match Sys.getenv_opt environment with
      | Some _ as command -> command
      | None -> fallback ()
    end

let z3_command explicit =
  resolve_command ~explicit ~environment:"VOX_SMT_SOLVER"
    ~fallback:(fun () -> Some "z3 -in")

let oxsmt_command explicit =
  resolve_command ~explicit ~environment:"VOX_OXSMT_SOLVER"
    ~fallback:(fun () ->
      let root = Filename.dirname (Filename.dirname Config.bindir) in
      Some
        (Filename.quote
           (Filename.concat root "_build/vox_oxsmt_runner.exe")))

let legacy_oxsmt_external () =
  match Sys.getenv_opt "VOX_OXSMT_LEGACY_EXTERNAL" with
  | Some "1" -> true
  | Some _ | None -> false

let oxsmt_timeout_seconds () =
  match Sys.getenv_opt "VOX_OXSMT_TIMEOUT_SECONDS" with
  | None -> 30
  | Some timeout ->
    let timeout = int_of_string timeout in
    if timeout <= 0 then invalid_arg "VOX_OXSMT_TIMEOUT_SECONDS";
    timeout

let result ~backend ~verdict ~location ?detail ?unused_facts () =
  { backend;
    capabilities = capabilities backend;
    verdict;
    location;
    detail;
    unused_facts;
  }

module Lean_backend = struct
  let backend = Lean
  let capabilities = capabilities backend

  let discharge ~command:_ { env; condition; prove_contents = _ } =
    let lean = Vox_lean.discharge ~env condition in
    let verdict =
      match lean.verdict with
      | Vox_lean.Proved -> Proved
      | Vox_lean.Not_proved -> Not_proved
      | Vox_lean.Disproved -> Disproved
      | Vox_lean.Solver_error -> Solver_error
    in
    { backend;
      capabilities;
      verdict;
      location = lean.location;
      detail = lean.detail;
      (* Lean declares the usage capability even when no proof is produced.
         Its legacy result then carries the conservative empty-unused set,
         which keeps every fact visible and preserves the existing JSON. *)
      unused_facts = Some lean.unused_facts;
    }
end

let verdict_of_smt = function
  | Vox_smt.Proved -> Proved
  | Vox_smt.Not_proved -> Not_proved
  | Vox_smt.Disproved -> Disproved
  | Vox_smt.Solver_error -> Solver_error
  | Vox_smt.Unavailable -> Unavailable

module Z3_backend = struct
  let backend = Z3
  let capabilities = capabilities backend

  let discharge ~command { env; condition; prove_contents } =
    let smt =
      Vox_smt.discharge ~backend:`Z3 ~command:(z3_command command)
        ?prove_contents ~input_mode:Vox_smt.Stdin ~env condition
    in
    { backend;
      capabilities;
      verdict = verdict_of_smt smt.verdict;
      location = smt.location;
      detail = smt.detail;
      unused_facts = Some smt.unused_facts;
    }
end

module Oxsmt_backend = struct
  let backend = Oxsmt
  let capabilities = capabilities backend

  let discharge ~command { env; condition; prove_contents } =
    let smt =
      if legacy_oxsmt_external () then
        Vox_smt.discharge ~backend:`Oxsmt
          ~command:(oxsmt_command command) ?prove_contents
          ~input_mode:Vox_smt.Stdin ~env condition
      else
        Vox_smt.discharge_oxsmt
          ~timeout_seconds:(oxsmt_timeout_seconds ()) ~env condition
    in
    { backend;
      capabilities;
      verdict = verdict_of_smt smt.verdict;
      location = smt.location;
      detail = smt.detail;
      unused_facts = Some smt.unused_facts;
    }
end

let module_for_backend = function
  | Lean -> (module Lean_backend : S)
  | Z3 -> (module Z3_backend : S)
  | Oxsmt -> (module Oxsmt_backend : S)

let command_for_backend ~smt_solver ~oxsmt_solver = function
  | Lean -> None
  | Z3 -> smt_solver
  | Oxsmt -> oxsmt_solver

let protect_discharge (module Backend : S) ~command obligation =
  try Backend.discharge ~command obligation with
  | exception_ ->
    result ~backend:Backend.backend ~verdict:Solver_error
      ~location:obligation.condition.location
      ~detail:(Printexc.to_string exception_) ()

let discharge_backend ~smt_solver ~oxsmt_solver backend obligation =
  let command = command_for_backend ~smt_solver ~oxsmt_solver backend in
  protect_discharge (module_for_backend backend) ~command obligation

let single_result (backend_result : backend_result) : result =
  { verdict = backend_result.verdict;
    location = backend_result.location;
    detail = backend_result.detail;
    unused_facts = backend_result.unused_facts;
    backend_results = [backend_result];
  }

let unavailable_lean (condition : Vox_vc.t) =
  result ~backend:Lean ~verdict:Unavailable ~location:condition.Vox_vc.location
    ~detail:"Lean executable not found" ()

let normalize_cross_result (result : backend_result) =
  match result.verdict with
  | Proved -> result
  | Not_proved ->
    { result with verdict = Unknown; unused_facts = None }
  | Disproved | Unknown | Solver_error | Unavailable ->
    { result with unused_facts = None }

let cross_summary (results : backend_result list) =
  let backend (result : backend_result) =
    string_of_backend result.backend ^ "=" ^ string_of_verdict result.verdict
  in
  "cross-check failed: " ^ String.concat ", " (List.map backend results)

let cross_result (condition : Vox_vc.t) (results : backend_result list) =
  let unused_facts =
    match results with
    | lean :: _ -> lean.unused_facts
    | [] -> None
  in
  if
    List.for_all
      (fun (result : backend_result) -> result.verdict = Proved)
      results
  then
    { verdict = Proved;
      location = condition.Vox_vc.location;
      detail = None;
      unused_facts;
      backend_results = results;
    }
  else
    { verdict = Solver_error;
      location = condition.Vox_vc.location;
      detail = Some (cross_summary results);
      unused_facts;
      backend_results = results;
    }

let discharge ~selection ~smt_solver ~oxsmt_solver ?prove_contents ~env
    condition =
  let obligation = { env; condition; prove_contents } in
  match selection with
  | Single backend ->
    discharge_backend ~smt_solver ~oxsmt_solver backend obligation
    |> single_result
  | Cross ->
    let lean =
      if Vox_lean.lean_available () then
        discharge_backend ~smt_solver ~oxsmt_solver Lean obligation
      else unavailable_lean condition
    in
    let z3 = discharge_backend ~smt_solver ~oxsmt_solver Z3 obligation in
    let oxsmt =
      discharge_backend ~smt_solver ~oxsmt_solver Oxsmt obligation
    in
    List.map normalize_cross_result [lean; z3; oxsmt]
    |> cross_result condition
