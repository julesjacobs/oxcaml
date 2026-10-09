(* S-expression layer for the test-only SMT-LIB2 parser. Tokenizing is delegated to the
   shared {!Oxsmt_lexical.Lexer} (ADR-0008); this module only adds paren nesting.
   Crucially an [Atom] carries the full {!Oxsmt_lexical.Lexer.token}, so token kind is
   preserved all the way to the parser — a quoted [|0|] stays a quoted symbol, never a
   numeral. *)

module Lexer = Oxsmt_lexical.Lexer

type t =
  | Atom of Lexer.token (* never [Lparen]/[Rparen] *)
  | List of t list

exception Malformed of string

(* [simple] is the text of an UNQUOTED symbol atom (command keywords, operators, sort
   keywords) — kind-precise: a quoted [|set-logic|] or a numeral is [None]. *)
let simple = function
  | Atom (Lexer.Symbol { text; quoted = false }) -> Some text
  | _ -> None
;;

(* [symbol_name] is the text of any symbol atom, quoted or not (a declared function/sort
   name may be [|quoted|]). A reserved word or numeral is [None] — those cannot name a
   declaration unquoted. *)
let symbol_name = function
  | Atom (Lexer.Symbol { text; _ }) -> Some text
  | _ -> None
;;

let rec to_string = function
  | List xs -> "(" ^ String.concat " " (List.map to_string xs) ^ ")"
  | Atom tok ->
    (match tok with
     | Lexer.Lparen -> "("
     | Lexer.Rparen -> ")"
     | Lexer.Numeral n -> n
     | Lexer.Decimal d -> d
     | Lexer.Hex h -> "#x" ^ h
     | Lexer.Binary b -> "#b" ^ b
     | Lexer.String s -> "\"" ^ s ^ "\""
     | Lexer.Keyword k -> ":" ^ k
     | Lexer.Reserved r -> r
     | Lexer.Symbol { text; quoted = false } -> text
     | Lexer.Symbol { text; quoted = true } -> "|" ^ text ^ "|")
;;

(* Build the s-expression forest from the token stream. A stray [Rparen] or an
   unterminated list is malformed; a lexical violation surfaces via the caller (it catches
   {!Lexer.Error}). *)
let parse_many (src : string) : t list =
  (* A lexical violation is reported as [Malformed] too, so every consumer's existing
     [Malformed] handling covers lexer errors without catching [Lexer.Error] itself. *)
  let toks =
    try Lexer.tokenize src with
    | Lexer.Error m -> raise (Malformed ("lexical: " ^ m))
  in
  let rec one toks =
    match toks with
    | [] -> None
    | Lexer.Rparen :: _ -> raise (Malformed "unexpected ')'")
    | Lexer.Lparen :: rest ->
      let items, rest = list_items rest [] in
      Some (List items, rest)
    | tok :: rest -> Some (Atom tok, rest)
  and list_items toks acc =
    match toks with
    | [] -> raise (Malformed "end of input inside list (unbalanced '(')")
    | Lexer.Rparen :: rest -> List.rev acc, rest
    | _ ->
      (match one toks with
       | Some (node, rest) -> list_items rest (node :: acc)
       | None -> raise (Malformed "end of input inside list"))
  in
  let rec top toks acc =
    match one toks with
    | None -> List.rev acc
    | Some (node, rest) -> top rest (node :: acc)
  in
  top toks []
;;
