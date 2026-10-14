open Vox_smt
module Codec = Vox_smt_query_codec
module Solver = Vox_smt_solver

let json_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c when Char.code c < 32 ->
        Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let validity = function
  | Valid -> "unsat"
  | Invalid _ -> "sat"
  | Unknown _ -> "unknown"
  | Timeout -> "timeout"
  | Failure _ -> "error"

let certificate symbols (result : Solver.result) =
  let index symbol =
    let rec find i = function
      | [] -> invalid_arg "Result contains an undeclared symbol"
      | other :: rest -> if symbol == other then i else find (i + 1) rest
    in
    find 0 symbols
  in
  match result.validity, result.core with
  | Invalid (Some model), _ ->
    let binding (symbol, value) =
      let kind, value = match value with
        | Bool_value value -> "bool", string_of_bool value
        | Int_value value -> "int63", Int64.to_string value
        | Bigint_value value -> "int", value
      in
      Printf.sprintf "[%d,%s,%s]" (index symbol)
        (json_string kind) (json_string value)
    in
    "{\"kind\":\"model\",\"bindings\":["
    ^ String.concat "," (List.map binding model) ^ "]}"
  | Valid, Some core ->
    "{\"kind\":\"assumptions\",\"indices\":["
    ^ String.concat "," (List.map (fun s -> string_of_int (index s)) core)
    ^ "]}"
  | _ -> "null"

let run output path executable mode repeat resource_policy =
  In_channel.with_open_bin path (fun input ->
      match Codec.read_call input with
      | None -> ()
      | Some first -> (
        let config = { Solver.executable; timeout_ms = first.timeout_ms } in
        let perform check =
          let index = ref 0 in
          let rec loop (call : Codec.call) =
            if
              call.timeout_ms <> first.timeout_ms
              || call.int_width <> first.int_width
            then invalid_arg "Configuration changed within an API session";
            incr index;
            let resource_limit =
              match resource_policy with
              | "unlimited" -> None
              | "preserve" -> call.resource_limit
              | _ -> invalid_arg "Expected unlimited or preserve"
            in
            let started = Solver.monotonic_time () in
            let result : Solver.result =
              check ?request_model:(Some call.request_model) ?resource_limit
                ?assumptions:call.assumptions call.query
            in
            let elapsed = Solver.monotonic_time () -. started in
            let model_values =
              match result.validity with
              | Invalid (Some model) -> List.length model
              | Valid | Invalid None | Unknown _ | Timeout | Failure _ -> 0
            in
            let core_size =
              Option.fold ~none:(-1) ~some:List.length result.core
            in
            let model_requested =
              match result.validity with
              | Invalid _ -> call.request_model && call.query.symbols <> []
              | Valid | Unknown _ | Timeout | Failure _ -> false
            in
            let model_available =
              match result.validity with
              | Invalid (Some _) -> true
              | Valid | Invalid None | Unknown _ | Timeout | Failure _ -> false
            in
            let core_requested =
              result.validity = Valid && Option.is_some call.assumptions
            in
            let core_available = Option.is_some result.core in
            let status = validity result.validity in
            Printf.fprintf output
              "{\"session\":%s,\"ordinal\":%d,\"repeat\":%d,\"mode\":%s,\
               \"solver\":%s,\"result\":%s,\"seconds\":%.9f,\
               \"encoding_seconds\":%.9f,\"solving_seconds\":%.9f,\
               \"model_values\":%d,\"core_size\":%d,\"resources\":%s,\
               \"model_requested\":%b,\"model_available\":%b,\
               \"core_requested\":%b,\"core_available\":%b,\
               \"certificate\":%s,\
               \"stderr\":%s}\n%!"
              (json_string (Filename.basename path))
              !index repeat (json_string mode) (json_string executable)
              (json_string status) elapsed result.encoding_seconds
              result.solving_seconds model_values core_size
              (Option.fold ~none:"null" ~some:string_of_int result.resources)
              model_requested model_available core_requested core_available
              (certificate call.query.symbols result)
              (json_string result.stderr);
            match Codec.read_call input with
            | None -> ()
            | Some call -> loop call
          in
          loop first
        in
        match mode with
        | "fresh" ->
          perform
            (Solver.check ~config ?dump:None ?cancelled:None
               ~int_width:first.int_width)
        | "reset" ->
          Solver.with_session ~config ~int_width:first.int_width perform
        | "incremental" ->
          Solver.with_incremental_session ~config ~int_width:first.int_width
            perform
        | "automatic" ->
          Solver.with_automatic_session ~config ~int_width:first.int_width
            perform
        | _ -> invalid_arg "Unknown API session mode"))

let export_query path ordinal resource_policy output =
  if ordinal < 1 then invalid_arg "Query ordinal must be positive";
  In_channel.with_open_bin path (fun input ->
      let rec find index =
        match Codec.read_call input with
        | None -> invalid_arg "Query ordinal is outside the capture"
        | Some call ->
          if index <> ordinal then find (index + 1)
          else
            let resource_limit =
              match resource_policy with
              | "unlimited" -> None
              | "preserve" -> call.resource_limit
              | _ -> invalid_arg "Expected unlimited or preserve"
            in
            let script =
              to_smtlib ?resource_limit ?assumptions:call.assumptions
                ~int_width:call.int_width ~timeout_ms:call.timeout_ms call.query
              |> Vox_smt_response.session_input
            in
            Out_channel.with_open_bin output (fun output ->
                output_string output script)
      in
      find 1)

let modes = ["fresh"; "reset"; "incremental"; "automatic"]

let export_queries path resource_policy =
  In_channel.with_open_bin path (fun input ->
    let rec loop ordinal =
      match Codec.read_call input with
      | None -> ()
      | Some call ->
        let resource_limit = match resource_policy with
          | "unlimited" -> None
          | "preserve" -> call.resource_limit
          | _ -> invalid_arg "Expected unlimited or preserve"
        in
        let script =
          to_smtlib ?resource_limit ?assumptions:call.assumptions
            ~int_width:call.int_width ~timeout_ms:call.timeout_ms call.query
          |> Vox_smt_response.session_input
        in
        Printf.printf "{\"ordinal\":%d,\"script\":%s}\n%!"
          ordinal (json_string script);
        loop (ordinal + 1)
    in
    loop 1)

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--list-modes"
  then List.iter print_endline modes
  else if Array.length Sys.argv = 4 && Sys.argv.(1) = "--export-queries"
  then export_queries Sys.argv.(2) Sys.argv.(3)
  else if Array.length Sys.argv = 6 && Sys.argv.(1) = "--export-query"
  then
    export_query Sys.argv.(2) (int_of_string Sys.argv.(3)) Sys.argv.(4)
      Sys.argv.(5)
  else begin
    if Array.length Sys.argv <> 7
    then
      invalid_arg
        "smt_api_bench SOLVER MODE REPEAT unlimited|preserve INPUT OUTPUT";
    Out_channel.with_open_text Sys.argv.(6) (fun output ->
        run output Sys.argv.(5) Sys.argv.(1) Sys.argv.(2)
          (int_of_string Sys.argv.(3))
          Sys.argv.(4))
  end
