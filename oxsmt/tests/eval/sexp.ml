type t =
  | Atom of string
  | Quoted of string
  | List of t list

exception Parse_error of string

let is_ws = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

(* An atom terminates at whitespace, a paren, a comment, or a quote/vertical bar. *)
let is_atom_terminator = function
  | ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '|' -> true
  | _ -> false
;;

let parse_all (s : string) : t list =
  let n = String.length s in
  let pos = ref 0 in
  (* Skip whitespace and ;-comments; returns at the next significant char or eof. *)
  let rec skip_trivia () =
    if !pos >= n
    then ()
    else (
      let c = s.[!pos] in
      if is_ws c
      then (
        incr pos;
        skip_trivia ())
      else if c = ';'
      then (
        while !pos < n && s.[!pos] <> '\n' do
          incr pos
        done;
        skip_trivia ())
      else ())
  in
  let read_quoted () =
    (* precondition: s.[!pos] = '|' *)
    incr pos;
    let buf = Buffer.create 16 in
    let closed = ref false in
    while (not !closed) && !pos < n do
      let c = s.[!pos] in
      if c = '|'
      then (
        closed := true;
        incr pos)
      else if c = '\\'
      then raise (Parse_error "backslash not permitted inside |quoted| symbol")
      else (
        Buffer.add_char buf c;
        incr pos)
    done;
    if not !closed then raise (Parse_error "unterminated |quoted| symbol");
    Quoted (Buffer.contents buf)
  in
  let read_atom () =
    let start = !pos in
    while !pos < n && not (is_atom_terminator s.[!pos]) do
      incr pos
    done;
    Atom (String.sub s start (!pos - start))
  in
  let rec read_sexp () : t =
    skip_trivia ();
    if !pos >= n then raise (Parse_error "unexpected end of input");
    match s.[!pos] with
    | '(' ->
      incr pos;
      let items = read_list () in
      List items
    | ')' -> raise (Parse_error "unexpected ')'")
    | '|' -> read_quoted ()
    | _ -> read_atom ()
  and read_list () : t list =
    skip_trivia ();
    if !pos >= n then raise (Parse_error "unterminated '(' list");
    if s.[!pos] = ')'
    then (
      incr pos;
      [])
    else (
      let hd = read_sexp () in
      let tl = read_list () in
      hd :: tl)
  in
  let rec loop acc =
    skip_trivia ();
    if !pos >= n then List.rev acc else loop (read_sexp () :: acc)
  in
  loop []
;;
