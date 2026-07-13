(* Standing adversarial round-trip fuzzer for the shared lexer (ADR-0008, board #123).
   Deterministic (fixed seeds, own LCG PRNG — no [Random], no wall-clock), so a failure is
   a reproducible finding. Generators target the token-boundary bug family: quoted symbols
   whose text is a numeral / reserved word / operator / has whitespace, parens, newlines;
   numeral-lookalikes (|0|, |007|, |-5|); reserved-word symbols; high (unicode-adjacent)
   bytes.

   Properties checked on every generated case:
   - P1 printer<->lexer: the printer's rendering of a symbol name lexes back to exactly
     one symbol with that same text (or the printer refuses it — predefined/empty/|/\\).
   - P2 print->parse round-trip: a session built with the adversarial name round-trips to
     Term.equal terms (the |0|/|let| class: a const named "0" must come back a symbol, not
     the numeral 0).
   - P3 lexer idempotence: lex -> render tokens back to text -> lex again yields the same
     token kinds and texts (a boundary bug shows up as a kind/text drift here).

   The cross-implementation differential against the gate's independent reader (the truly
   uncorrelated check) lands with the gate migration over task/gate3; see ADR-0008. *)

open Oxsmt_core
module Tok = Oxsmt_lexical.Lexer
module Printer = Oxsmt_smtlib.Printer
module Parser = Oxsmt_smtlib_parser.Parser

let failures = ref 0
let cases = ref 0

let fail fmt =
  Printf.ksprintf
    (fun s ->
       incr failures;
       print_string "  FUZZ-FAIL: ";
       print_endline s)
    fmt
;;

(* Deterministic PRNG (SplitMix64-ish); seeded per run. *)
let state = ref 0

let next () =
  (* 63-bit LCG (Knuth MMIX-style multiplier, trimmed to fit native int); wraps silently. *)
  state := (!state * 2862933555777941757) + 3037000493;
  (!state lsr 16) land 0x3FFFFFFF
;;

let pick arr = arr.(next () mod Array.length arr)

(* Adversarial name fragments: reserved words, operators, numerals, punctuation the lexer
   must treat as boundaries, and high bytes. *)
let frags =
  [| "let"
   ; "!"
   ; "as"
   ; "forall"
   ; "exists"
   ; "par"
   ; "NUMERAL" (* reserved words *)
   ; "+"
   ; "-"
   ; "*"
   ; "="
   ; "<="
   ; ">="
   ; "and"
   ; "or"
   ; "not"
   ; "ite"
   ; "distinct" (* operators *)
   ; "0"
   ; "007"
   ; "-5"
   ; "42"
   ; "00" (* numeral-lookalikes *)
   ; "a"
   ; "x1"
   ; "foo.bar"
   ; "<hi>"
   ; "p"
   ; "S" (* ordinary *)
   ; " "
   ; "  "
   ; "("
   ; ")"
   ; ";"
   ; "\n"
   ; "\t" (* boundary chars *)
   ; "\xc3\xa9"
   ; "\xe2\x88\x80"
   ; "\x80" (* unicode-adjacent / high bytes *)
  |]
;;

(* A random name: a concatenation of 1-4 fragments. May contain spaces/parens/newlines/
   high bytes — all legal inside a |quoted| symbol (which is how the printer will render
   it). We never generate '|' or '\\' (those are unrepresentable and separately refused). *)
let gen_name () =
  let n = 1 + (next () mod 4) in
  let b = Buffer.create 16 in
  for _ = 1 to n do
    Buffer.add_string b (pick frags)
  done;
  Buffer.contents b
;;

(* Render a token back to its surface syntax (for P3 idempotence). *)
let unlex = function
  | Tok.Lparen -> "("
  | Tok.Rparen -> ")"
  | Tok.Numeral n -> n
  | Tok.Decimal d -> d
  | Tok.Hex h -> "#x" ^ h
  | Tok.Binary b -> "#b" ^ b
  | Tok.String s -> "\"" ^ s ^ "\""
  | Tok.Keyword k -> ":" ^ k
  | Tok.Reserved r -> r
  | Tok.Symbol { text; quoted = false } -> text
  | Tok.Symbol { text; quoted = true } -> "|" ^ text ^ "|"
;;

let toks_equal a b =
  List.length a = List.length b
  && List.for_all2
       (fun x y ->
          String.equal (Tok.kind x) (Tok.kind y) && String.equal (unlex x) (unlex y))
       a
       b
;;

(* P1 + P2 on a single generated name. *)
let check_name name =
  incr cases;
  match Printer.quote_symbol name with
  | exception Printer.Unsupported _ -> () (* predefined/empty/|/\\: refusal is correct *)
  | rendered ->
    (* P1: the rendering lexes to exactly one symbol whose text is [name]. *)
    (match Tok.tokenize rendered with
     | [ Tok.Symbol { text; _ } ] when String.equal text name -> ()
     | exception Tok.Error m ->
       fail "P1 %S rendered %S but re-lex errored: %s" name rendered m
     | toks ->
       fail
         "P1 %S rendered %S re-lexed to [%s]"
         name
         rendered
         (String.concat "; " (List.map Tok.kind toks)));
    (* P2: a session using [name] as a const round-trips print -> parse (same Context). *)
    let env = Env.create () in
    let ctx = Context.create env in
    (match
       let c = Context.const ctx (Env.declare_fun env name (Rank.create [] Sort.int)) in
       let a = Context.eq ctx c (Context.int_const ctx 0) in
       let text = Printer.print_session env [ a ] in
       Parser.parse_into env ctx text, a
     with
     | parsed, a ->
       (match parsed.Parser.assertions with
        | [ a' ] when Term.equal a a' -> ()
        | other ->
          fail
            "P2 %S: round-trip changed the assertion (%d back)"
            name
            (List.length other))
     | exception Parser.Malformed m -> fail "P2 %S: reparse Malformed: %s" name m
     | exception Parser.Unsupported m -> fail "P2 %S: reparse Unsupported: %s" name m
     | exception e -> fail "P2 %S: %s" name (Printexc.to_string e))
;;

(* P3: idempotence of lexing on a generated raw input built from surface fragments. *)
let check_idempotent () =
  incr cases;
  let n = 1 + (next () mod 8) in
  let b = Buffer.create 32 in
  for _ = 1 to n do
    (match next () mod 5 with
     | 0 -> Buffer.add_string b (Printf.sprintf "|%s|" (gen_name ()))
     | 1 -> Buffer.add_string b (pick frags)
     | 2 -> Buffer.add_string b "(+ 1 x)"
     | 3 -> Buffer.add_string b " :key "
     | _ -> Buffer.add_string b (pick frags));
    Buffer.add_char b ' '
  done;
  let src = Buffer.contents b in
  match Tok.tokenize src with
  | exception Tok.Error _ -> () (* a lexically invalid stream is fine; just don't crash *)
  | toks1 ->
    let reser = String.concat " " (List.map unlex toks1) in
    (match Tok.tokenize reser with
     | exception Tok.Error m -> fail "P3 re-lex of %S errored: %s" reser m
     | toks2 ->
       if not (toks_equal toks1 toks2) then fail "P3 not idempotent on %S -> %S" src reser)
;;

let () =
  let iters = if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else 20000 in
  (* A few fixed seeds so the stream is broad but fully reproducible. *)
  List.iter
    (fun seed ->
       state := seed;
       for _ = 1 to iters do
         check_name (gen_name ());
         check_idempotent ()
       done)
    [ 1; 0x9E3779B9; 0x1234567; 42 ];
  Printf.printf
    "fuzz-lex: %d cases, %d failures (4 seeds x %d iters)\n"
    !cases
    !failures
    iters;
  if !failures > 0 then exit 1
;;
