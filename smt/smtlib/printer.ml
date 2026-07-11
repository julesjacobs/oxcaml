(* SMT-LIB2 printer. Reads the frozen 9-node [Term.t] and an [Env] and emits a complete
   QF_UFLIA script. See printer.mli for the rendering choices; the invariants of the term
   layer (ADR-0003) are what make this a total, deterministic function. *)

open Oxsmt_core

exception Unsupported of string

(* ------------------------------------------------------------------ *)
(* Symbol quoting (SMT-LIB 2.6 §3.1). A "simple symbol" is nonempty, every char in the
   reserved set, and not starting with a digit. Three refusal classes exist because quoting
   is purely LEXICAL — [|s|] and [s] denote the SAME symbol — so quoting cannot rescue a
   name whose denotation is already fixed:
   - a name containing [|]/[\\] has no [|...|] escape → refuse;
   - a name equal to a predefined function/operator (or, in sort position, a predefined
     sort) → refuse: [|+|] is still the operator [+], so faithful printing is impossible;
   - the empty name → refuse ([||] is the degenerate empty symbol, rejected by tools).
   A RESERVED WORD (a token that only *looks* like a symbol, e.g. [let]) is representable —
   [|let|] is a legal symbol distinct from the keyword — so it is quoted, not refused. *)

let is_simple_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '~'
  | '!'
  | '@'
  | '$'
  | '%'
  | '^'
  | '&'
  | '*'
  | '_'
  | '+'
  | '='
  | '<'
  | '>'
  | '.'
  | '?'
  | '/'
  | '-' -> true
  | _ -> false
;;

let is_simple_symbol name =
  String.length name > 0
  && (match name.[0] with
      | '0' .. '9' -> false
      | _ -> true)
  && String.for_all is_simple_char name
;;

(* SMT-LIB 2.6 §3.1 reserved words: match the symbol syntax but are keywords; quoting
   turns them into legal, distinct symbols. *)
let reserved_words =
  [ "_"
  ; "!"
  ; "as"
  ; "let"
  ; "exists"
  ; "forall"
  ; "match"
  ; "par"
  ; "BINARY"
  ; "DECIMAL"
  ; "HEXADECIMAL"
  ; "NUMERAL"
  ; "STRING"
  ]
;;

(* Predefined Core + Ints (QF_UFLIA) function/operator symbols. A user symbol with one of
   these names is unrepresentable (see header). [div]/[mod] are deliberately absent: they
   are the reserved built-ins (Env forbids user-declaring them, ADR-0003 Decision 5), so
   an [App] head named [div]/[mod] is always the built-in operator and legitimately prints
   bare. *)
let predefined_funs =
  [ "+"
  ; "-"
  ; "*"
  ; "abs"
  ; "<="
  ; "<"
  ; ">="
  ; ">"
  ; "="
  ; "distinct"
  ; "=>"
  ; "and"
  ; "or"
  ; "not"
  ; "xor"
  ; "ite"
  ; "true"
  ; "false"
  ]
;;

(* Predefined sorts: a user sort so named would redeclare a built-in sort. (Distinct from
   the function namespace — a *function* named [Int] is legal and not refused.) *)
let predefined_sorts = [ "Int"; "Bool" ]

let refuse name why =
  raise (Unsupported (Printf.sprintf "symbol %S cannot be printed: %s" name why))
;;

let check_representable name =
  if String.length name = 0 then refuse name "the empty symbol is not representable";
  String.iter
    (fun c ->
       if Char.equal c '|' || Char.equal c '\\'
       then refuse name (Printf.sprintf "contains %c, which |...| cannot escape" c))
    name
;;

let quote_lexical name =
  if is_simple_symbol name && not (List.mem name reserved_words)
  then name
  else "|" ^ name ^ "|"
;;

let quote_symbol name =
  check_representable name;
  if List.mem name predefined_funs
  then
    refuse
      name
      "collides with a predefined SMT-LIB operator (quoting is lexical, so it cannot be \
       disambiguated)";
  quote_lexical name
;;

(* Uninterpreted-sort name: same rules, but the refused set is the predefined SORTS. *)
let quote_sort_symbol name =
  check_representable name;
  if List.mem name predefined_sorts
  then refuse name "collides with a predefined SMT-LIB sort";
  quote_lexical name
;;

(* ------------------------------------------------------------------ *)
(* Term rendering into a Buffer. *)

(* Integer literal: nonnegative as-is; negative as [(- N)]. Strip the leading '-' from the
   string form rather than negating, so [min_int] does not overflow. *)
let add_int_lit buf n =
  if n >= 0
  then Buffer.add_string buf (string_of_int n)
  else (
    let s = string_of_int n in
    Buffer.add_string buf "(- ";
    Buffer.add_substring buf s 1 (String.length s - 1);
    Buffer.add_char buf ')')
;;

let rec render buf (t : Term.t) =
  match t.node with
  | Bool_const b -> Buffer.add_string buf (if b then "true" else "false")
  | Int_const n -> add_int_lit buf n
  | App (sym, args) ->
    if Iarr.length args = 0
    then Buffer.add_string buf (quote_symbol (Symbol.name sym))
    else (
      Buffer.add_char buf '(';
      Buffer.add_string buf (quote_symbol (Symbol.name sym));
      Iarr.iter
        (fun a ->
           Buffer.add_char buf ' ';
           render buf a)
        args;
      Buffer.add_char buf ')')
  | Arith l -> render_arith buf l
  | Le arg ->
    Buffer.add_string buf "(<= ";
    render buf arg;
    Buffer.add_string buf " 0)"
  | Eq (a, b) -> render_bin buf "=" a b
  | Not a ->
    Buffer.add_string buf "(not ";
    render buf a;
    Buffer.add_char buf ')'
  | And xs -> render_nary buf "and" xs
  | Or xs -> render_nary buf "or" xs
  | Ite (c, a, b) ->
    Buffer.add_string buf "(ite ";
    render buf c;
    Buffer.add_char buf ' ';
    render buf a;
    Buffer.add_char buf ' ';
    render buf b;
    Buffer.add_char buf ')'

and render_bin buf op a b =
  Buffer.add_char buf '(';
  Buffer.add_string buf op;
  Buffer.add_char buf ' ';
  render buf a;
  Buffer.add_char buf ' ';
  render buf b;
  Buffer.add_char buf ')'

and render_nary buf op xs =
  Buffer.add_char buf '(';
  Buffer.add_string buf op;
  Iarr.iter
    (fun x ->
       Buffer.add_char buf ' ';
       render buf x)
    xs;
  Buffer.add_char buf ')'

(* [Arith] = sum of (coeff * term) plus a constant. Render each summand (the term bare
   when its coeff is 1, else a "( * coeff term )" product), append the constant when
   nonzero. One summand and no constant prints that summand alone (never a unary [+]);
   otherwise wrap in a "(+ ...)". *)
and render_arith buf (l : Term.linear) =
  let summands =
    Iarr.fold
      (fun acc (t, c) ->
         let b = Buffer.create 32 in
         if c = 1
         then render b t
         else (
           Buffer.add_string b "(* ";
           add_int_lit b c;
           Buffer.add_char b ' ';
           render b t;
           Buffer.add_char b ')');
         Buffer.contents b :: acc)
      []
      l.coeffs
  in
  let summands = List.rev summands in
  let parts =
    if l.const = 0
    then summands
    else (
      let b = Buffer.create 16 in
      add_int_lit b l.const;
      summands @ [ Buffer.contents b ])
  in
  match parts with
  | [ only ] -> Buffer.add_string buf only
  | _ ->
    Buffer.add_string buf "(+ ";
    Buffer.add_string buf (String.concat " " parts);
    Buffer.add_char buf ')'
;;

let print_term t =
  let buf = Buffer.create 64 in
  render buf t;
  Buffer.contents buf
;;

(* ------------------------------------------------------------------ *)
(* Declaration collection: a deterministic DFS over the assertions gathering the
   uninterpreted sorts and non-reserved function symbols actually used, each in
   first-encounter order. All sorts are emitted before all funs, so declarations always
   precede uses (0-arity uninterpreted sorts depend on nothing). *)

module Sym_tbl = Hashtbl.Make (struct
    type t = Symbol.t

    let equal = Symbol.equal
    let hash = Symbol.hash
  end)

type decls =
  { sorts : Symbol.t list (* uninterpreted sort symbols, first-use order *)
  ; funs : Symbol.t list (* function/const symbols, first-use order *)
  }

let collect_decls env assertions =
  let sort_seen = Sym_tbl.create 16 in
  let fun_seen = Sym_tbl.create 64 in
  let sorts = ref [] in
  let funs = ref [] in
  let div_sym = Env.div_sym env in
  let mod_sym = Env.mod_sym env in
  let visit_sort (s : Sort.t) =
    match s with
    | Sort.Bool | Sort.Int _ -> ()
    | Sort.Uninterpreted sym ->
      if not (Sym_tbl.mem sort_seen sym)
      then (
        Sym_tbl.add sort_seen sym ();
        sorts := sym :: !sorts)
  in
  let register_fun sym =
    (* reserved div/mod are built-ins, never declared *)
    if (not (Symbol.equal sym div_sym)) && not (Symbol.equal sym mod_sym)
    then
      if not (Sym_tbl.mem fun_seen sym)
      then (
        Sym_tbl.add fun_seen sym ();
        (match Env.rank env sym with
         | rank ->
           Iarr.iter visit_sort rank.Rank.domain;
           visit_sort rank.Rank.codomain
         | exception Not_found -> ());
        funs := sym :: !funs)
  in
  let rec visit (t : Term.t) =
    match t.node with
    | Bool_const _ | Int_const _ -> ()
    | App (sym, args) ->
      register_fun sym;
      Iarr.iter visit args
    | Arith l -> Iarr.iter (fun (t, _) -> visit t) l.coeffs
    | Le a | Not a -> visit a
    | Eq (a, b) ->
      visit a;
      visit b
    | And xs | Or xs -> Iarr.iter visit xs
    | Ite (c, a, b) ->
      visit c;
      visit a;
      visit b
  in
  List.iter visit assertions;
  { sorts = List.rev !sorts; funs = List.rev !funs }
;;

(* ------------------------------------------------------------------ *)
(* Sort rendering (in declarations). *)

let sort_string (s : Sort.t) =
  match s with
  | Sort.Bool -> "Bool"
  | Sort.Int _ -> "Int"
  | Sort.Uninterpreted sym -> quote_sort_symbol (Symbol.name sym)
;;

let print_session ?status env assertions =
  let buf = Buffer.create 1024 in
  let line s =
    Buffer.add_string buf s;
    Buffer.add_char buf '\n'
  in
  (match status with
   | None -> ()
   | Some st -> line (Printf.sprintf "(set-info :status %s)" (Status.to_string st)));
  line "(set-logic QF_UFLIA)";
  let { sorts; funs } = collect_decls env assertions in
  List.iter
    (fun sym ->
       line (Printf.sprintf "(declare-sort %s 0)" (quote_sort_symbol (Symbol.name sym))))
    sorts;
  List.iter
    (fun sym ->
       let name = quote_symbol (Symbol.name sym) in
       let rank = Env.rank env sym in
       let dom = Iarr.to_list rank.Rank.domain in
       let cod = sort_string rank.Rank.codomain in
       match dom with
       | [] -> line (Printf.sprintf "(declare-const %s %s)" name cod)
       | _ ->
         let dom_s = String.concat " " (List.map sort_string dom) in
         line (Printf.sprintf "(declare-fun %s (%s) %s)" name dom_s cod))
    funs;
  List.iter
    (fun t ->
       Buffer.add_string buf "(assert ";
       render buf t;
       line ")")
    assertions;
  line "(check-sat)";
  Buffer.contents buf
;;
