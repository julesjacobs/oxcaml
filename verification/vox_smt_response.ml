open Vox_smt

exception Protocol_error of string

let protocol message = raise (Protocol_error message)

type sexp =
  | Atom of string
  | String of string
  | List of sexp list

let parse text =
  let pos = ref 0 and len = String.length text in
  let rec space () =
    if !pos < len
    then
      match text.[!pos] with
      | ' ' | '\t' | '\r' | '\n' ->
        incr pos;
        space ()
      | ';' ->
        while !pos < len && text.[!pos] <> '\n' do
          incr pos
        done;
        space ()
      | _ -> ()
  in
  let rec value depth =
    if depth > 256 then protocol "Solver response is nested too deeply";
    space ();
    if !pos = len then protocol "Incomplete solver response";
    match text.[!pos] with
    | '(' ->
      incr pos;
      List (items (depth + 1) [])
    | ')' -> protocol "Unexpected ')' in solver response"
    | '"' ->
      incr pos;
      let b = Buffer.create 32 in
      let rec quoted () =
        if !pos = len then protocol "Unterminated solver string";
        let c = text.[!pos] in
        incr pos;
        if c = '"'
        then
          if !pos < len && text.[!pos] = '"'
          then (
            incr pos;
            Buffer.add_char b c;
            quoted ())
          else String (Buffer.contents b)
        else (
          Buffer.add_char b c;
          quoted ())
      in
      quoted ()
    | _ ->
      let start = !pos in
      while
        !pos < len
        && not
             (List.mem text.[!pos] [' '; '\t'; '\r'; '\n'; '('; ')'; ';'; '"'])
      do
        incr pos
      done;
      Atom (String.sub text start (!pos - start))
  and items depth acc =
    space ();
    if !pos = len then protocol "Unterminated solver list";
    if text.[!pos] = ')'
    then (
      incr pos;
      List.rev acc)
    else
      let x = value depth in
      items depth (x :: acc)
  in
  let rec all acc =
    space ();
    if !pos = len
    then List.rev acc
    else
      let x = value 0 in
      all (x :: acc)
  in
  all []

let model symbols response =
  let integer = function
    | Atom digits -> Int64.of_string_opt digits
    | List [Atom "-"; Atom digits] ->
      Option.map Int64.neg (Int64.of_string_opt digits)
    | _ -> None
  in
  let machine_integer = function
    | Atom bits when String.starts_with ~prefix:"#b" bits ->
      if String.length bits <> 65
      then None
      else
        Option.map
          (fun n -> Int64.shift_right (Int64.shift_left n 1) 1)
          (Int64.of_string_opt ("0b" ^ String.sub bits 2 63))
    | List [Atom "_"; Atom bits; Atom "63"]
      when String.starts_with ~prefix:"bv" bits ->
      Option.bind
        (Int64.of_string_opt (String.sub bits 2 (String.length bits - 2)))
        (fun n ->
          if n < 0L
          then None
          else Some (Int64.shift_right (Int64.shift_left n 1) 1))
    | value -> integer value
  in
  let value symbol sexp =
    match Symbol.sort symbol, sexp with
    | Bool, Atom "true" -> Some (Bool_value true)
    | Bool, Atom "false" -> Some (Bool_value false)
    | Int, Atom digits when decimal_integer digits && digits.[0] <> '-' ->
      Some (Bigint_value digits)
    | Int, List [Atom "-"; Atom digits]
      when decimal_integer digits && digits.[0] <> '-' ->
      Some (Bigint_value (if digits = "0" then "0" else "-" ^ digits))
    | Int63, sexp ->
      Option.bind (machine_integer sexp) (fun n ->
          if n < -4611686018427387904L || n > 4611686018427387903L
          then None
          else Some (Int_value n))
    | (Opaque _ | Datatype _), _ -> None
    | _ -> None
  in
  let rec bindings i acc symbols entries =
    match symbols, entries with
    | [], [] -> Some (List.rev acc)
    | s :: ss, List [Atom name; v] :: vs when name = "v" ^ string_of_int i ->
      Option.bind (value s v) (fun v -> bindings (i + 1) ((s, v) :: acc) ss vs)
    | _ -> None
  in
  match response with
  | List entries -> bindings 0 [] symbols entries
  | _ -> None

let interpret symbols status response =
  match status, response with
  | "unsat", [] -> Valid
  | "sat", [] -> Invalid (if symbols = [] then Some [] else None)
  | "sat", [List [Atom "error"; String _]] -> Invalid None
  | "sat", [(List entries as values)]
    when List.for_all (function List [Atom _; _] -> true | _ -> false) entries
    ->
    Invalid (model symbols values)
  | "unknown", [] -> Unknown None
  | "unknown", [List [Atom ":reason-unknown"; String reason]] ->
    if reason = "timeout" then Timeout else Unknown (Some reason)
  | "unknown", [List [Atom "error"; String _]] -> Unknown None
  | _ -> protocol "Unexpected solver response"

(* Each query starts from a reset solver, so its result and resource count do
   not depend on the queries before it. The session keeps the logic it has
   always used. *)
let session_input input =
  "(reset)\n"
  ^ String.concat "\n"
      (List.map
         (fun line ->
           if String.starts_with ~prefix:"(set-logic " line
           then "(set-logic ALL)"
           else line)
         (String.split_on_char '\n' input))

let followup ?assumptions symbols answer =
  match answer with
  | "unsat" -> if Option.is_some assumptions then "(get-unsat-core)\n" else ""
  | "sat" ->
    if symbols = []
    then ""
    else
      "(get-value ("
      ^ String.concat " " (List.mapi (fun i _ -> "v" ^ string_of_int i) symbols)
      ^ "))\n"
  | "unknown" -> "(get-info :reason-unknown)\n"
  | _ ->
    protocol
      (Printf.sprintf "Expected sat, unsat or unknown; got %S"
         (String.sub answer 0 (min 200 (String.length answer))))

let resource_request = "(get-info :rlimit)\n"

(* The resource count restarts at each reset. It is recorded before the rest of
   the response is interpreted, which may fail. *)
let interpret_response ~resources ?assumptions ?(core = ref None) symbols answer
    text =
  let response =
    match List.rev (parse text) with
    | List [Atom ":rlimit"; Atom count] :: rest -> (
      match int_of_string_opt count with
      | Some count when count >= 0 ->
        resources := Some count;
        List.rev rest
      | _ -> protocol "Unexpected solver resource count")
    | Atom "unsupported" :: rest -> List.rev rest
    | response -> List.rev response
  in
  (* The core names the assumptions a proof used, as [to_smtlib] names
     symbols. *)
  let response =
    match assumptions, answer, response with
    | Some assumptions, "unsat", [List names] ->
      let assumed = Hashtbl.create 16 in
      List.iteri
        (fun i s ->
          if List.memq s assumptions
          then Hashtbl.replace assumed ("v" ^ string_of_int i) s)
        symbols;
      core
        := Some
             (List.map
                (function
                  | Atom name when Hashtbl.mem assumed name ->
                    Hashtbl.find assumed name
                  | _ -> protocol "Unexpected unsat core")
                names);
      []
    | Some _, "unsat", _ -> protocol "Missing unsat core"
    | _ -> response
  in
  interpret symbols answer response
