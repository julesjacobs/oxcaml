(* Queries on emitted Lambda and Cmm text, for the boundary tests.

   Patterns are literal text with three wildcards: %d matches one or more
   digits, %w one or more identifier characters (letters, digits, _ and '),
   and %s one or more whitespace characters; %% matches %. Each %w is
   captured. *)

let read file = In_channel.with_open_bin file In_channel.input_all

let is_digit c = c >= '0' && c <= '9'

let is_word c =
  is_digit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  || c = '_' || c = '\''

let is_space c = c = ' ' || c = '\n' || c = '\t' || c = '\r'

(* Matches [pattern] at [pos]; returns the end position and the captures. *)
let match_at pattern text pos =
  let plen = String.length pattern and tlen = String.length text in
  let rec go p t captures =
    if p = plen then Some (t, List.rev captures)
    else if pattern.[p] = '%' && p + 1 < plen && pattern.[p + 1] = '%' then
      if t < tlen && text.[t] = '%' then go (p + 2) (t + 1) captures else None
    else if pattern.[p] = '%' && p + 1 < plen then begin
      let cls =
        match pattern.[p + 1] with
        | 'd' -> is_digit
        | 'w' -> is_word
        | 's' -> is_space
        | c -> invalid_arg (Printf.sprintf "Emitted_code: %%%c" c)
      in
      let stop = ref t in
      while !stop < tlen && cls text.[!stop] do incr stop done;
      if !stop = t then None
      else
        let captures =
          if pattern.[p + 1] = 'w' then
            String.sub text t (!stop - t) :: captures
          else captures
        in
        go (p + 2) !stop captures
    end
    else if t < tlen && text.[t] = pattern.[p] then go (p + 1) (t + 1) captures
    else None
  in
  go 0 pos []

(* Every match, as (start, end, captures), scanning left to right. A match
   of %w must not start inside a word. *)
let find_all pattern text =
  let tlen = String.length text in
  let starts_word =
    String.length pattern >= 2 && String.sub pattern 0 2 = "%w"
  in
  let rec scan pos acc =
    if pos >= tlen then List.rev acc
    else if starts_word && pos > 0 && is_word text.[pos - 1] then
      scan (pos + 1) acc
    else
      match match_at pattern text pos with
      | Some (stop, captures) ->
        scan (max stop (pos + 1)) ((pos, stop, captures) :: acc)
      | None -> scan (pos + 1) acc
  in
  scan 0 []

let count pattern text = List.length (find_all pattern text)

let occurs pattern text = find_all pattern text <> []

let captures pattern text =
  List.concat_map (fun (_, _, captures) -> captures) (find_all pattern text)
  |> List.sort_uniq String.compare

(* The balanced parenthesised expression starting at [start], ignoring
   parentheses in string literals. *)
let balanced text start =
  let len = String.length text in
  let rec go i depth quoted escaped =
    if i >= len then failwith "Emitted_code: unterminated expression"
    else
      let c = text.[i] in
      if quoted then
        if escaped then go (i + 1) depth true false
        else if c = '\\' then go (i + 1) depth true true
        else go (i + 1) depth (c <> '"') false
      else if c = '"' then go (i + 1) depth true false
      else if c = '(' then go (i + 1) (depth + 1) false false
      else if c = ')' then
        if depth = 1 then String.sub text start (i - start + 1)
        else go (i + 1) (depth - 1) false false
      else go (i + 1) depth false false
  in
  go start 0 false false

(* The bodies of the functions bound to [name] in a Lambda dump, in order:
   [name/N (function ...)] or [name/N = (function ...)]. *)
let function_bodies text name =
  List.concat_map
    (fun pattern ->
      List.map
        (fun (start, _, _) -> start)
        (find_all (name ^ pattern) text))
    [ "/%d%s(function"; "/%d%s=%s(function"; "/%d =(function" ]
  |> List.sort_uniq compare
  |> List.filter (fun start -> start = 0 || not (is_word text.[start - 1]))
  |> List.map (fun start -> balanced text (String.index_from text start '('))

let function_body text name =
  match function_bodies text name with
  | body :: _ -> body
  | [] -> failwith ("Emitted_code: no function " ^ name)

(* Functions called directly by name in a Lambda expression. *)
let direct_calls body = captures "(apply%s%w/%d" body

let applications body = count "(apply" body

(* The functions reachable from [entry] through direct calls, among the
   functions defined in [text]. *)
let reachable text entry =
  let rec go pending visited =
    match pending with
    | [] -> List.sort String.compare visited
    | name :: pending when List.mem name visited -> go pending visited
    | name :: pending ->
      let callees =
        match function_bodies text name with
        | [] -> []
        | bodies -> List.concat_map direct_calls bodies
      in
      go (callees @ pending) (name :: visited)
  in
  go [ entry ] []

let failures = ref 0

(* Prints [ok: message] or [FAILED: message]; [finish] exits with status 1
   after any failure. *)
let check condition message =
  if condition then Printf.printf "ok: %s\n" message
  else begin
    incr failures;
    Printf.printf "FAILED: %s\n" message
  end

let finish () = if !failures > 0 then exit 1

let words list = String.concat " " list
