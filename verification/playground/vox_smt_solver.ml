(* The browser implementation of [Vox_smt_solver]: the same interface as the
   native one in ../vox_smt_solver.ml, but the solver is Z3 compiled to
   WebAssembly and loaded in the same worker. The page installs
   [globalThis.voxZ3Eval], which runs SMT-LIB text in one persistent Z3
   context with [Z3_eval_smtlib2_string] and returns what Z3 printed. The call
   is synchronous, so the verifier runs unchanged. *)

open Vox_smt

type config =
  { executable : string;
    timeout_ms : int
  }

let default_config = { executable = "z3"; timeout_ms = 5000 }

let monotonic_time () =
  Js_of_ocaml.Js.Unsafe.meth_call
    (Js_of_ocaml.Js.Unsafe.pure_js_expr "globalThis.performance")
    "now" [||]
  /. 1000.

type result =
  { validity : validity;
    stderr : string;
    resources : int option;
    encoding_seconds : float;
    solving_seconds : float
  }

exception Cancelled

exception Solver_error of string

(* Z3 reports errors in its output as [(error "...")] and keeps going, like
   the native process. The JavaScript side raises only when Z3 itself fails
   (for example, when it runs out of memory). *)
let eval text =
  let open Js_of_ocaml in
  let eval = Js.Unsafe.pure_js_expr "globalThis.voxZ3Eval" in
  if Js.Optdef.test eval |> not
  then raise (Solver_error "Z3 is not loaded");
  match
    Js.Unsafe.fun_call eval [| Js.Unsafe.inject (Js.string text) |]
  with
  | output -> Js.to_string output
  | exception Js_error.Exn error -> raise (Solver_error (Js_error.message error))

let first_line text =
  let rec skip text =
    match String.index_opt text '\n' with
    | None -> String.trim text, ""
    | Some i ->
      let line = String.trim (String.sub text 0 i) in
      let rest = String.sub text (i + 1) (String.length text - i - 1) in
      if line = "" then skip rest else line, rest
  in
  skip text

let check_impl ?(config = default_config) ?(dump = fun _ -> ())
    ?(cancelled = fun () -> false) ?resource_limit ~int_width q =
  if config.timeout_ms <= 0 then invalid_arg "Vox_smt_solver: timeout_ms";
  let started = monotonic_time () in
  if cancelled () then raise Cancelled;
  let input =
    to_smtlib
      ~poll:(fun () -> if cancelled () then raise Cancelled)
      ?resource_limit ~int_width ~timeout_ms:config.timeout_ms q
  in
  let encoded = monotonic_time () in
  let resources = ref None in
  let validity =
    try
      let input = Vox_smt_response.session_input input in
      dump input;
      let answer, rest = first_line (eval input) in
      let request =
        Vox_smt_response.followup q.symbols answer
        ^ Vox_smt_response.resource_request
      in
      dump request;
      let response = rest ^ eval request in
      (* Z3's own [:timeout] option, set by [to_smtlib], stands in for the
         native runner's deadline: it answers unknown with reason timeout. *)
      Vox_smt_response.interpret_response ~resources q.symbols answer response
    with
    | Vox_smt_response.Protocol_error message -> Failure message
    | Solver_error message -> Failure message
  in
  let finished = monotonic_time () in
  { validity;
    stderr = "";
    resources = !resources;
    encoding_seconds = encoded -. started;
    solving_seconds = finished -. encoded
  }

let with_session ?config ?dump ?cancelled ~int_width f =
  let closed = ref false and busy = ref false in
  Fun.protect
    ~finally:(fun () -> closed := true)
    (fun () ->
      f (fun ?resource_limit query ->
          if !closed then invalid_arg "Vox_smt_solver: closed session";
          if !busy then invalid_arg "Vox_smt_solver: recursive session query";
          busy := true;
          Fun.protect
            ~finally:(fun () -> busy := false)
            (fun () ->
              check_impl ?config ?dump ?cancelled ?resource_limit ~int_width
                query)))

let check ?config ?dump ?cancelled ?resource_limit ~int_width query =
  with_session ?config ?dump ?cancelled ~int_width (fun check ->
      check ?resource_limit query)
