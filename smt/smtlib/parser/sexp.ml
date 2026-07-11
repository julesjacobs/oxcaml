(* S-expression reader for the test-only SMT-LIB2 parser.

   Handles the SMT-LIB 2.6 lexical conventions the corpora and our own dumps use:
   [;] line comments, [|quoted symbols|] (which may span newlines and contain any byte
   except [|]/[\\]), and ordinary atoms (symbols, numerals, keywords). Positions are
   tracked so lexical errors carry a line/col.

   This is deliberately its own reader (not shared with the gate's), so the parser library
   stays self-contained and test-only. *)

type t =
  | Atom of string
  | List of t list

(* Lexically/structurally malformed input: unbalanced parens, EOF inside a quote, a stray
   close paren. Distinct from "well-formed sexp outside our subset", which the parser
   layer above reports. *)
exception Malformed of string

type pos =
  { mutable idx : int
  ; mutable line : int
  ; mutable col : int
  }

let malformedf pos fmt =
  Printf.ksprintf
    (fun s -> raise (Malformed (Printf.sprintf "line %d, col %d: %s" pos.line pos.col s)))
    fmt
;;

let advance pos s =
  let c = s.[pos.idx] in
  pos.idx <- pos.idx + 1;
  if Char.equal c '\n'
  then (
    pos.line <- pos.line + 1;
    pos.col <- 1)
  else pos.col <- pos.col + 1
;;

let peek s pos = if pos.idx < String.length s then Some s.[pos.idx] else None

let is_ws = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

(* SMT-LIB delimiters that terminate an unquoted atom. *)
let is_delim = function
  | '(' | ')' | ';' | '|' -> true
  | c -> is_ws c
;;

let rec skip_trivia s pos =
  match peek s pos with
  | Some c when is_ws c ->
    advance pos s;
    skip_trivia s pos
  | Some ';' ->
    let rec eol () =
      match peek s pos with
      | Some '\n' -> advance pos s
      | Some _ ->
        advance pos s;
        eol ()
      | None -> ()
    in
    eol ();
    skip_trivia s pos
  | _ -> ()
;;

let read_quoted s pos =
  (* [pos] is at the opening '|'. *)
  advance pos s;
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek s pos with
    | None -> malformedf pos "end of input inside |quoted| symbol"
    | Some '\\' -> malformedf pos "backslash is not permitted inside a |quoted| symbol"
    | Some '|' ->
      advance pos s;
      Atom (Buffer.contents buf)
    | Some c ->
      Buffer.add_char buf c;
      advance pos s;
      loop ()
  in
  loop ()
;;

let read_atom s pos =
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek s pos with
    | Some c when not (is_delim c) ->
      Buffer.add_char buf c;
      advance pos s;
      loop ()
    | _ -> Atom (Buffer.contents buf)
  in
  loop ()
;;

let rec read_one s pos =
  skip_trivia s pos;
  match peek s pos with
  | None -> None
  | Some '(' ->
    advance pos s;
    Some (read_list s pos)
  | Some ')' -> malformedf pos "unexpected ')'"
  | Some '|' -> Some (read_quoted s pos)
  | Some _ -> Some (read_atom s pos)

and read_list s pos =
  let rec loop acc =
    skip_trivia s pos;
    match peek s pos with
    | None -> malformedf pos "end of input inside list (unbalanced '(')"
    | Some ')' ->
      advance pos s;
      List (List.rev acc)
    | Some _ ->
      (match read_one s pos with
       | Some e -> loop (e :: acc)
       | None -> malformedf pos "end of input inside list")
  in
  loop []
;;

let parse_many (s : string) : t list =
  let pos = { idx = 0; line = 1; col = 1 } in
  let rec loop acc =
    match read_one s pos with
    | None -> List.rev acc
    | Some e -> loop (e :: acc)
  in
  loop []
;;

let rec to_string = function
  | Atom a -> a
  | List xs -> "(" ^ String.concat " " (List.map to_string xs) ^ ")"
;;
