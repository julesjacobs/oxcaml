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

module Tok = Oxsmt_lexical.Lexer

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

(* [quote_lexical name] emits [name] bare iff the shared lexer reads the bare form back as
   exactly one UNQUOTED symbol with the same text. Numerals ([0]), reserved words ([let]),
   and multi-token or malformed names ([a b(c)], [3x]) all fail that test and are
   [|quoted|]. Grounding the decision in the one shared lexer is what stops the printer
   and any reader from disagreeing on a token boundary (ADR-0008). *)
let quote_lexical name =
  let bare_ok =
    match Tok.tokenize name with
    | [ Tok.Symbol { text; quoted = false } ] -> String.equal text name
    | _ -> false
    | exception Tok.Error _ -> false
  in
  if bare_ok then name else "|" ^ name ^ "|"
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

(* Integer literal (arbitrary precision, core-bignum W2): nonnegative as-is; negative as
   [(- N)]. [Bigint.to_string] of the negated magnitude gives the unsigned digits. *)
let add_int_lit buf n =
  if Bigint.sign n >= 0
  then Buffer.add_string buf (Bigint.to_string n)
  else (
    Buffer.add_string buf "(- ";
    Buffer.add_string buf (Bigint.to_string (Bigint.neg n));
    Buffer.add_char buf ')')
;;

(* The render family closes over the datatype registry [dts] so a tester application
   [App (is-C, [t])] prints as the indexed identifier [((_ is C) t)] rather than as the
   internal ["is-C"] function name. Constructor and selector applications are ordinary
   [App]s and print bare under their SMT-LIB names. [dts] is [Datatype_defs.empty] for the
   registry-free [print_term]. *)
(* The SMT-LIB keyword for a prefix-form bitvector operator ([(bvadd a b)] etc.); [None]
   for the ops rendered with special syntax ([concat], indexed [extract]/extend). *)
let bv_prefix_keyword (op : Bv.op) =
  match op with
  | Bvnot -> Some "bvnot"
  | Bvand -> Some "bvand"
  | Bvor -> Some "bvor"
  | Bvxor -> Some "bvxor"
  | Bvneg -> Some "bvneg"
  | Bvadd -> Some "bvadd"
  | Bvsub -> Some "bvsub"
  | Bvmul -> Some "bvmul"
  | Bvudiv -> Some "bvudiv"
  | Bvurem -> Some "bvurem"
  | Bvshl -> Some "bvshl"
  | Bvlshr -> Some "bvlshr"
  | Bvashr -> Some "bvashr"
  | Bvult -> Some "bvult"
  | Bvule -> Some "bvule"
  | Bvslt -> Some "bvslt"
  | Bvsle -> Some "bvsle"
  | Concat | Extract _ | Zero_extend _ | Sign_extend _ -> None
;;

(* A bitvector literal renders as the binary form [#b<bits>] (MSB first), always exact. *)
let bv_const_string ~value ~width =
  let bits = Bv.bits_lsb value ~width in
  let buf = Buffer.create (width + 2) in
  Buffer.add_string buf "#b";
  for i = width - 1 downto 0 do
    Buffer.add_char buf (if bits.(i) then '1' else '0')
  done;
  Buffer.contents buf
;;

let render_family dts arrs =
  let rec render buf (t : Term.t) =
    match t.node with
    | Bool_const b -> Buffer.add_string buf (if b then "true" else "false")
    | Int_const n -> add_int_lit buf n
    | App (sym, _args) when Bv.is_bv_sym sym ->
      (match Bv.view t with
       | Some v -> render_bv buf v
       | None ->
         (* A [Bv]-namespaced symbol always views; this is unreachable. Fail closed. *)
         raise (Unsupported "bitvector symbol did not decode"))
    | App (sym, args) ->
      (match Array_defs.role_of_sym arrs sym with
       | Some { Array_defs.role; _ } ->
         (* An array operator prints as its SMT-LIB builtin name ([select]/[store]), never
            the internal per-instantiation symbol name. *)
         let op =
           match role with
           | Array_defs.Select -> "select"
           | Array_defs.Store -> "store"
         in
         Buffer.add_char buf '(';
         Buffer.add_string buf op;
         Iarr.iter
           (fun a ->
              Buffer.add_char buf ' ';
              render buf a)
           args;
         Buffer.add_char buf ')'
       | None ->
         (match Datatype_defs.tester_of_sym dts sym with
          | Some (_, ctor) ->
            (* tester: ((_ is C) arg) *)
            Buffer.add_string buf "((_ is ";
            Buffer.add_string buf (quote_symbol (Symbol.name ctor.Datatype_defs.sym));
            Buffer.add_char buf ')';
            Iarr.iter
              (fun a ->
                 Buffer.add_char buf ' ';
                 render buf a)
              args;
            Buffer.add_char buf ')'
          | None ->
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
              Buffer.add_char buf ')')))
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
  and render_bv buf (v : Bv.view) =
    match v with
    | Bv.Const { value; width } -> Buffer.add_string buf (bv_const_string ~value ~width)
    | Bv.Op { op; args; result_width = _ } ->
      let render_args () =
        List.iter
          (fun a ->
             Buffer.add_char buf ' ';
             render buf a)
          args
      in
      let indexed head =
        Buffer.add_string buf "(";
        Buffer.add_string buf head;
        render_args ();
        Buffer.add_char buf ')'
      in
      (match op, bv_prefix_keyword op with
       | _, Some kw -> indexed kw
       | Bv.Concat, None -> indexed "concat"
       | Bv.Extract (i, j), None -> indexed (Printf.sprintf "(_ extract %d %d)" i j)
       | Bv.Zero_extend n, None -> indexed (Printf.sprintf "(_ zero_extend %d)" n)
       | Bv.Sign_extend n, None -> indexed (Printf.sprintf "(_ sign_extend %d)" n)
       | ( ( Bv.Bvnot
           | Bv.Bvand
           | Bv.Bvor
           | Bv.Bvxor
           | Bv.Bvneg
           | Bv.Bvadd
           | Bv.Bvsub
           | Bv.Bvmul
           | Bv.Bvudiv
           | Bv.Bvurem
           | Bv.Bvshl
           | Bv.Bvlshr
           | Bv.Bvashr
           | Bv.Bvult
           | Bv.Bvule
           | Bv.Bvslt
           | Bv.Bvsle )
         , None ) ->
         (* [bv_prefix_keyword] returns [Some] for exactly these, so [None] is impossible. *)
         raise (Unsupported "bitvector prefix operator without keyword"))
  (* [Arith] = sum of (coeff * term) plus a constant. Render each summand (the term bare
     when its coeff is 1, else a "( * coeff term )" product), append the constant when
     nonzero. One summand and no constant prints that summand alone (never a unary [+]);
     otherwise wrap in a "(+ ...)". Coefficients/const are core-bignum [Bigint.t] (W2). *)
  and render_arith buf (l : Term.linear) =
    let summands =
      Iarr.fold
        (fun acc (t, c) ->
           let b = Buffer.create 32 in
           if Bigint.equal c Bigint.one
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
      if Bigint.is_zero l.const
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
  in
  render
;;

let print_term ?(datatypes = Datatype_defs.empty) ?(arrays = Array_defs.empty) t =
  let buf = Buffer.create 64 in
  render_family datatypes arrays buf t;
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
  ; datatypes : Symbol.t list (* datatype sort symbols, first-use order *)
  ; funs : Symbol.t list (* function/const symbols, first-use order *)
  ; uses_bitvec : bool (* any bitvector sort/term appears — selects a BV logic label *)
  }

let collect_decls dts arrs env assertions =
  let sort_seen = Sym_tbl.create 16 in
  let dt_seen = Sym_tbl.create 16 in
  let fun_seen = Sym_tbl.create 64 in
  let sorts = ref [] in
  let datatypes = ref [] in
  let funs = ref [] in
  let uses_bitvec = ref false in
  let div_sym = Env.div_sym env in
  let mod_sym = Env.mod_sym env in
  let rec visit_sort (s : Sort.t) =
    match s with
    | Sort.Bool | Sort.Int _ -> ()
    (* [BitVec] is a built-in indexed sort — no [declare-sort] to collect, but its presence
       selects a bitvector logic label. *)
    | Sort.BitVec _ -> uses_bitvec := true
    (* An [(Array I E)] sort is built-in — no [declare-sort] of its own — but its index
       and element sorts must still be collected so an uninterpreted [I]/[E] is declared. *)
    | Sort.Array (index, element) ->
      visit_sort index;
      visit_sort element
    | Sort.Uninterpreted sym ->
      if not (Sym_tbl.mem sort_seen sym)
      then (
        Sym_tbl.add sort_seen sym ();
        sorts := sym :: !sorts)
    | Sort.Datatype sym ->
      if not (Sym_tbl.mem dt_seen sym)
      then (
        Sym_tbl.add dt_seen sym ();
        datatypes := sym :: !datatypes;
        (* Pull in the sorts a constructor field references — a sibling datatype (mutual
           recursion) or an uninterpreted field sort — so every sort the emitted
           [(declare-datatypes ...)] block mentions is itself declared first. *)
        match Datatype_defs.datatype_of_sort dts sym with
        | Some dt ->
          List.iter
            (fun (c : Datatype_defs.constructor) ->
               List.iter
                 (fun (sel : Datatype_defs.selector) -> visit_sort sel.field_sort)
                 c.selectors)
            dt.constructors
        | None ->
          raise
            (Unsupported
               (Printf.sprintf
                  "datatype sort %s has no registry entry to print"
                  (Symbol.name sym))))
  in
  (* A constructor / selector / tester symbol is declared by the datatype block, never as
     a standalone [declare-fun]; its ranks are still walked (above) so its sorts are
     collected. *)
  let is_datatype_symbol sym =
    Option.is_some (Datatype_defs.constructor_of_sym dts sym)
    || Option.is_some (Datatype_defs.selector_of_sym dts sym)
    || Option.is_some (Datatype_defs.tester_of_sym dts sym)
  in
  (* An array [select]/[store] symbol is a theory builtin printed as [(select ...)] /
     [(store ...)]; it is never emitted as a [declare-fun] (its internal per-instantiation
     name is not even a legal SMT-LIB symbol). Its rank's sorts are still walked so the
     index/element sorts get declared. *)
  let is_array_symbol sym = Option.is_some (Array_defs.role_of_sym arrs sym) in
  let register_fun sym =
    (* reserved div/mod, and the bitvector operator/literal symbols, are built-ins: never
       emitted as [declare-fun] (their sorts are built-in and need no declaration). *)
    if
      (not (Symbol.equal sym div_sym))
      && (not (Symbol.equal sym mod_sym))
      && not (Bv.is_bv_sym sym)
    then
      if not (Sym_tbl.mem fun_seen sym)
      then (
        Sym_tbl.add fun_seen sym ();
        (match Env.rank env sym with
         | rank ->
           Iarr.iter visit_sort rank.Rank.domain;
           visit_sort rank.Rank.codomain
         | exception Not_found -> ());
        if (not (is_datatype_symbol sym)) && not (is_array_symbol sym)
        then funs := sym :: !funs)
  in
  let rec visit (t : Term.t) =
    match t.node with
    | Bool_const _ | Int_const _ -> ()
    | App (sym, args) ->
      if Bv.is_bv_sym sym then uses_bitvec := true;
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
  { sorts = List.rev !sorts
  ; datatypes = List.rev !datatypes
  ; funs = List.rev !funs
  ; uses_bitvec = !uses_bitvec
  }
;;

(* ------------------------------------------------------------------ *)
(* Sort rendering (in declarations). *)

let rec sort_string (s : Sort.t) =
  match s with
  | Sort.Bool -> "Bool"
  | Sort.Int _ -> "Int"
  (* A datatype sort prints by its name, the same as an uninterpreted sort; the datatype's
     shape is emitted separately in the [(declare-datatypes ...)] block. *)
  | Sort.Uninterpreted sym | Sort.Datatype sym -> quote_sort_symbol (Symbol.name sym)
  | Sort.Array (index, element) ->
    Printf.sprintf "(Array %s %s)" (sort_string index) (sort_string element)
  | Sort.BitVec w -> Printf.sprintf "(_ BitVec %d)" w
;;

(* Render one constructor [(C (sel1 S1) ... (seln Sn))] for a declare-datatypes block;
   nullary constructors print as [(C)]. *)
let constructor_string (c : Datatype_defs.constructor) =
  let buf = Buffer.create 32 in
  Buffer.add_char buf '(';
  Buffer.add_string buf (quote_symbol (Symbol.name c.sym));
  List.iter
    (fun (sel : Datatype_defs.selector) ->
       Buffer.add_string buf " (";
       Buffer.add_string buf (quote_symbol (Symbol.name sel.sym));
       Buffer.add_char buf ' ';
       Buffer.add_string buf (sort_string sel.field_sort);
       Buffer.add_char buf ')')
    c.selectors;
  Buffer.add_char buf ')';
  Buffer.contents buf
;;

let print_session
      ?status
      ?(datatypes = Datatype_defs.empty)
      ?(arrays = Array_defs.empty)
      env
      assertions
  =
  let buf = Buffer.create 1024 in
  let line s =
    Buffer.add_string buf s;
    Buffer.add_char buf '\n'
  in
  let { sorts; datatypes = dt_syms; funs; uses_bitvec } =
    collect_decls datatypes arrays env assertions
  in
  let render = render_family datatypes arrays in
  (match status with
   | None -> ()
   | Some st -> line (Printf.sprintf "(set-info :status %s)" (Status.to_string st)));
  (* The base printer targets QF_UFLIA; a session that declares datatypes needs a logic
     that admits them (QF_UFDT is the UF+DT superset our reader accepts). Non-datatype
     sessions are byte-identical to before. *)
  (* Non-datatype sessions keep the base QF_UFLIA label (LIA is the superset the base
     always declares). A datatype session declares QF_UFDTLIA — the UF+DT+LIA superset —
     NOT QF_UFDT: a datatype with integer fields carries arithmetic, and QF_UFDT would
     omit LIA, so a strict consumer (the Lean oracle) would reject the otherwise-faithful
     dump. The superset is always sound (a pure-DT problem is in QF_UFDTLIA), matching the
     base's always-superset convention. *)
  (* Logic label (always a superset of the query's theories, hence sound): a datatype
     session needs the DT superset; a bitvector session [QF_UFBV]; an array session
     [QF_AUFLIA] (the broad UF+arrays+LIA superset our reader accepts); otherwise the base
     [QF_UFLIA]. Datatypes take precedence in the (not-yet-produced) mixed cases. *)
  let logic =
    if dt_syms <> []
    then "QF_UFDTLIA"
    else if uses_bitvec
    then "QF_UFBV"
    else if not (Array_defs.is_empty arrays)
    then "QF_AUFLIA"
    else "QF_UFLIA"
  in
  line (Printf.sprintf "(set-logic %s)" logic);
  List.iter
    (fun sym ->
       line (Printf.sprintf "(declare-sort %s 0)" (quote_sort_symbol (Symbol.name sym))))
    sorts;
  (* All datatypes in one [(declare-datatypes ...)] block: SMT-LIB declares every sort
     name before any constructor list, so mutual recursion needs no ordering among them. *)
  (match dt_syms with
   | [] -> ()
   | _ ->
     let sort_decls =
       String.concat
         " "
         (List.map
            (fun s -> Printf.sprintf "(%s 0)" (quote_sort_symbol (Symbol.name s)))
            dt_syms)
     in
     let ctor_lists =
       List.map
         (fun s ->
            match Datatype_defs.datatype_of_sort datatypes s with
            | Some dt ->
              "(" ^ String.concat " " (List.map constructor_string dt.constructors) ^ ")"
            | None ->
              raise
                (Unsupported
                   (Printf.sprintf
                      "datatype sort %s has no registry entry to print"
                      (Symbol.name s))))
         dt_syms
     in
     line
       (Printf.sprintf
          "(declare-datatypes (%s) (%s))"
          sort_decls
          (String.concat " " ctor_lists)));
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
