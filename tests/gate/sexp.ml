(* S-expression reader for the SMT-LIB2 gate.

   Tokenizing is delegated to the ONE shared lexer, {!Oxsmt_lexical.Lexer} (ADR-0008): the
   token-boundary bug family (the [|0|] confusion this reader fixed as G1, and the
   cache-collision exploit) is closed once, in one spec-cited §3.1 lexer, rather than in
   three divergent hand-rolled ones. This module keeps ONLY the paren nesting and the
   gate's own 3-way surface type, so [reader.ml] is unchanged and its G1/G2 hardening is
   preserved.

   {b Token KIND is preserved} (still load-bearing for soundness, codex G1/G2): the shared
   lexer already distinguishes a quoted symbol from a numeral/keyword, so [|0|] maps to
   [Quoted "0"] (never the numeral [0]) and [|let|] to a [Quoted] symbol (never a
   keyword); a [Str] is inert data, never re-tokenized as a command/term.

   {b Trust-topology note (ADR-0008):} the gate now links a module under [smt/], narrowing
   the DESIGN §10 N-version isolation at the LEXICAL layer only. The uncorrelated
   backstops that actually exist are Lean elaboration and the pre-labeled benchmarks, plus
   the [dump-canonical] tool for reader-preservation diffs over the corpus. A true
   cross-implementation differential (this reader vs the [Oxsmt_smtlib] parser over the
   same input) is NOT yet implemented — it is a deferred obligation (review F4), not a
   live target. The encoder and the Lean kernel — where oracle independence actually lives
   — are untouched.

   {b Behavior delta vs the previous hand-rolled lexer (documented, adjudicated):}
   structural errors ("unexpected ')'", "unbalanced '('") come from the token-nesting
   layer below, which has no source position, so their message drops the [line N, col M]
   prefix the old lexer carried. The OUTCOME is unchanged (still {!Malformed}); only the
   diagnostic string differs. Lexical errors (unterminated quote/string, etc.) still carry
   line/col via the shared lexer. *)

module Lexer = Oxsmt_lexical.Lexer

type t =
  | Atom of string (* unquoted token: symbol, numeral, keyword, or operator *)
  | Quoted of
      string (* |...| symbol: always a plain symbol name, never a numeral/keyword *)
  | Str of string (* "..." string literal: inert data, never a command or term *)
  | List of t list

(* Raised on lexically/structurally malformed input (unbalanced parens, EOF inside a
   quote, stray close-paren, a lexer violation). Distinct from "well-formed sexp but not a
   query we support", which the reader layer above reports. *)
exception Malformed of string

(* Map one shared-lexer token to the gate's 3-way surface node. Numerals/keywords/reserved
   words/decimals/hex/binary all collapse back to their surface [Atom] string exactly as
   the gate's previous hand-rolled lexer produced them (the reader re-inspects the
   string); only a quoted symbol ([Quoted]) and a string literal ([Str]) keep a distinct
   kind. Paren tokens are handled structurally by the nesting below. *)
let node_of_token : Lexer.token -> t = function
  | Lexer.Numeral n -> Atom n
  | Lexer.Decimal d -> Atom d
  | Lexer.Hex h -> Atom ("#x" ^ h)
  | Lexer.Binary b -> Atom ("#b" ^ b)
  | Lexer.Keyword k -> Atom (":" ^ k)
  | Lexer.Reserved r -> Atom r
  | Lexer.Symbol { text; quoted = false } -> Atom text
  | Lexer.Symbol { text; quoted = true } -> Quoted text
  | Lexer.String s -> Str s
  | Lexer.Lparen | Lexer.Rparen -> assert false (* handled structurally below *)
;;

(* Parse a whole document into a list of top-level s-expressions. *)
let parse_many (src : string) : t list =
  let toks =
    try Lexer.tokenize src with
    | Lexer.Error m -> raise (Malformed m)
  in
  let rec one toks =
    match toks with
    | [] -> None
    | Lexer.Rparen :: _ -> raise (Malformed "unexpected ')'")
    | Lexer.Lparen :: rest ->
      let items, rest = list_items rest [] in
      Some (List items, rest)
    | tok :: rest -> Some (node_of_token tok, rest)
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

let rec to_string = function
  | Atom a -> a
  | Quoted a -> "|" ^ a ^ "|"
  | Str s ->
    (* re-double embedded quotes, per SMT-LIB string syntax *)
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '"';
    String.iter
      (fun c -> if c = '"' then Buffer.add_string buf "\"\"" else Buffer.add_char buf c)
      s;
    Buffer.add_char buf '"';
    Buffer.contents buf
  | List xs -> "(" ^ String.concat " " (List.map to_string xs) ^ ")"
;;
