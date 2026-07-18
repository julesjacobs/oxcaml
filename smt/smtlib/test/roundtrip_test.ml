(* smtlib round-trip tests (make smtlib-test). Two directions, both deterministic and
   corpus-independent:

   A. print -> parse: ~30 programmatic sessions covering every term node, quoting edge
      cases, negative constants, div/mod, and deep nesting. Each is built in a Context,
      printed, and parsed back INTO THE SAME Context; equality is then [Term.equal] (tag
      identity). Justification: re-parsing a printed term into the same hash-consing
      Context must reproduce the very same interned node, so tag equality is the strongest
      possible check that print;parse is the identity on our subset (a structural
      cross-Context comparison would be weaker and needs bespoke tag-blind code — the
      single-Context contract of ADR-0003 makes the strong check the simple one).

   B. parse -> print -> parse: over committed .smt2 files (tests/cases, harness fixtures,
      gate honeypots) passed as argv dirs. Parse (fresh Context), print, re-parse into the
      SAME Context, assert the assertion lists are [Term.equal] and the :status label
      survives. Files outside our subset are reported as skipped, not failed. *)

open Oxsmt_core
module Printer = Oxsmt_smtlib.Printer
module Status = Oxsmt_smtlib.Status
module Parser = Oxsmt_smtlib_parser.Parser

let failures = ref 0
let checks = ref 0

let fail fmt =
  Printf.ksprintf
    (fun s ->
      incr failures;
      print_string "  FAIL: ";
      print_endline s)
    fmt
;;

let status_equal a b =
  match a, b with
  | None, None -> true
  | Some x, Some y -> Status.equal x y
  | _ -> false
;;

let status_str = function
  | None -> "<none>"
  | Some s -> Status.to_string s
;;

(* Compare two assertion lists by tag identity (same Context). *)
let terms_equal xs ys = List.length xs = List.length ys && List.for_all2 Term.equal xs ys

(* ---- direction A: programmatic sessions ---- *)

let check_a ~name ?status build =
  incr checks;
  let env = Env.create () in
  let ctx = Context.create env in
  match build env ctx with
  | assertions ->
    (match Printer.print_session ?status env assertions with
     | text ->
       (match Parser.parse_into env ctx text with
        | parsed ->
          if not (terms_equal assertions parsed.assertions)
          then
            fail
              "A/%s: assertions differ after round-trip\n--- printed ---\n%s---"
              name
              text
          else if not (status_equal status parsed.status)
          then
            fail
              "A/%s: status %s -> %s"
              name
              (status_str status)
              (status_str parsed.status)
        | exception Parser.Malformed m ->
          fail "A/%s: reparse Malformed: %s\n%s" name m text
        | exception Parser.Unsupported m ->
          fail "A/%s: reparse Unsupported: %s\n%s" name m text)
     | exception Printer.Unsupported m -> fail "A/%s: printer Unsupported: %s" name m)
  | exception e -> fail "A/%s: build raised %s" name (Printexc.to_string e)
;;

(* Direction A for bit-vectors (board #58). Identical to {!check_a} but the env carries
   the reserved-minting cap and the reparse threads its [internal_mint], because
   bit-vector operator/literal symbols live in the reserved [.oxsmt.bv.*] namespace: the
   [build] receives a {!Bv.minter} to construct them, and the printed text reparses
   through the SAME minter (same cap + env) so a round-tripped bit-vector symbol interns
   to the very same node and [Term.equal] still holds. *)
let check_a_bv ~name ?status build =
  incr checks;
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  (* board #58 O-MINTER: the reprint reparses through the SAME opaque minter (bv admit
     grammar); [build] gets the [Internal_minter.mint] closure to construct terms. *)
  let minter = Internal_minter.create ~admit:Bv.is_bv_name cap env in
  let mint = Internal_minter.mint minter in
  match build env ctx mint with
  | assertions ->
    (match Printer.print_session ?status env assertions with
     | text ->
       (match Parser.parse_into ~internal_mint:minter env ctx text with
        | parsed ->
          if not (terms_equal assertions parsed.assertions)
          then
            fail
              "A/%s: assertions differ after round-trip\n--- printed ---\n%s---"
              name
              text
          else if not (status_equal status parsed.status)
          then
            fail
              "A/%s: status %s -> %s"
              name
              (status_str status)
              (status_str parsed.status)
        | exception Parser.Malformed m ->
          fail "A/%s: reparse Malformed: %s\n%s" name m text
        | exception Parser.Unsupported m ->
          fail "A/%s: reparse Unsupported: %s\n%s" name m text)
     | exception Printer.Unsupported m -> fail "A/%s: printer Unsupported: %s" name m)
  | exception e -> fail "A/%s: build raised %s" name (Printexc.to_string e)
;;

(* Print-only check: the printed text must contain [expect]. For rendering that is correct
   SMT-LIB but outside our native-int reingest range (e.g. min_int). *)
let check_print ~name ?status ~expect build =
  incr checks;
  let env = Env.create () in
  let ctx = Context.create env in
  let text = Printer.print_session ?status env (build env ctx) in
  let contains hay needle =
    let nl = String.length needle in
    let rec go i =
      i + nl <= String.length hay && (String.sub hay i nl = needle || go (i + 1))
    in
    nl = 0 || go 0
  in
  if not (contains text expect) then fail "print/%s: expected %S in\n%s" name expect text
;;

(* Refusal check: printing the session MUST raise [Printer.Unsupported] (a name the
   printer cannot faithfully render — a predefined-operator/sort name or the empty name). *)
let check_refused ~name build =
  incr checks;
  let env = Env.create () in
  let ctx = Context.create env in
  match
    let asserts = build env ctx in
    Printer.print_session env asserts
  with
  | _ -> fail "refuse/%s: expected Printer.Unsupported, but printing succeeded" name
  | exception Printer.Unsupported _ -> ()
  | exception e ->
    fail "refuse/%s: expected Printer.Unsupported, got %s" name (Printexc.to_string e)
;;

(* ---- define-fun expansion helpers (parse two texts into ONE Context and compare) ---- *)

(* [a] uses a define-fun macro; [b] is the same query hand-expanded. Parsing both into the
   same Context and comparing by [Term.equal] asserts the macro expands to exactly the
   hand-written term. *)
let check_same ~name a b =
  incr checks;
  (* Self-capped + [internal_mint] threaded (board #58) so a theory that mints a reserved
     symbol mid-parse — e.g. the bit-vector sugar cases — resolves rather than hitting the
     [no_internal_mint] default. Inert for the non-bit-vector define-fun cases. *)
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  let internal_mint = Internal_minter.create ~admit:Bv.is_bv_name cap env in
  match Parser.parse_into ~internal_mint env ctx a with
  | pa ->
    (match Parser.parse_into ~internal_mint env ctx b with
     | pb ->
       if not (terms_equal pa.assertions pb.assertions)
       then fail "expand/%s: macro form differs from hand-expanded form" name
     | exception e ->
       fail "expand/%s: hand-expanded parse raised %s" name (Printexc.to_string e))
  | exception e -> fail "expand/%s: macro parse raised %s" name (Printexc.to_string e)
;;

let check_parse ~name ~kind text =
  incr checks;
  match Parser.parse text with
  | _ -> fail "%s/%s: expected %s, parsed OK" kind name kind
  | exception Parser.Malformed _ when String.equal kind "malformed" -> ()
  | exception Parser.Unsupported _ when String.equal kind "unsupported" -> ()
  | exception e -> fail "%s/%s: wrong exception %s" kind name (Printexc.to_string e)
;;

let check_malformed ~name text = check_parse ~name ~kind:"malformed" text
let check_unsupported ~name text = check_parse ~name ~kind:"unsupported" text

let check_parse_ok ~name text =
  incr checks;
  match Parser.parse text with
  | _ -> ()
  | exception e ->
    fail "parse-ok/%s: expected success, got %s" name (Printexc.to_string e)
;;

(* parse (expands define-fun) -> print (define-fun erased) -> parse : must fixpoint. *)
let check_df_roundtrip ~name text =
  incr checks;
  let env = Env.create () in
  let ctx = Context.create env in
  match Parser.parse_into env ctx text with
  | p ->
    let out = Printer.print_session ?status:p.status env p.assertions in
    (match Parser.parse_into env ctx out with
     | p2 ->
       if not (terms_equal p.assertions p2.assertions)
       then fail "df-roundtrip/%s: differ after reprint\n%s" name out
     | exception e -> fail "df-roundtrip/%s: reparse %s" name (Printexc.to_string e))
  | exception e -> fail "df-roundtrip/%s: parse %s" name (Printexc.to_string e)
;;

(* declaration helpers *)
let const env ctx name sort =
  Context.const ctx (Env.declare_fun env name (Rank.create [] sort))
;;

let fn env name dom cod = Env.declare_fun env name (Rank.create dom cod)
let usort env name = Sort.uninterpreted (Env.declare_sort env name)

(* First occurrence of [a] strictly precedes first occurrence of [b] (both present). *)
let first_before text a b =
  let find sub =
    let n = String.length text
    and m = String.length sub in
    let rec go i =
      if i + m > n then -1 else if String.sub text i m = sub then i else go (i + 1)
    in
    go 0
  in
  let ia = find a
  and ib = find b in
  ia >= 0 && ib >= 0 && ia < ib
;;

(* F3 (LRA review bounce, flag-OFF byte-id): [collect_decls] must emit declare-sort in
   domain-before-codomain order for a multi-sort UF, as trunk did. The regressed printer
   collected an application's RESULT sort first (an unconditional [visit_sort t.sort]),
   flipping the order for existing UF inputs. This is invisible to parse->print->parse
   round-trips (a reordered but complete declaration set re-parses fine), so it needs an
   explicit order assertion. Runs with OXSMT_LRA unset (the fixed line is flag-gated). *)
let check_f3_declare_sort_order () =
  incr checks;
  let env = Env.create () in
  let ctx = Context.create env in
  let a_sort = usort env "F3domain" in
  let b_sort = usort env "F3codomain" in
  let f = fn env "f3f" [ a_sort ] b_sort in
  (* Only DOMAIN-sorted leaves (a1,a2); both Eq operands are codomain-sorted applications
     and there is NO codomain-sorted leaf. So the first sort collected comes solely from
     the first-visited [f]-application's [register_fun] (domain-then-codomain) — trunk
     order is F3domain,F3codomain; the regressed [visit_sort t.sort] collects the
     application's result sort F3codomain first, flipping to F3codomain,F3domain. (A
     codomain-sorted leaf would mask the effect since it would collect the codomain first
     regardless.) *)
  let a1 = const env ctx "f3a1" a_sort in
  let a2 = const env ctx "f3a2" a_sort in
  let assertion = Context.eq ctx (Context.app ctx f [ a1 ]) (Context.app ctx f [ a2 ]) in
  match Printer.print_session env [ assertion ] with
  | text ->
    if not (first_before text "F3domain" "F3codomain")
    then fail "F3: declare-sort order must be domain-before-codomain (flag OFF):\n%s" text
  | exception Printer.Unsupported m -> fail "F3: printer Unsupported: %s" m
;;

let sessions () =
  check_f3_declare_sort_order ();
  let i = Sort.int
  and b = Sort.bool in
  check_a ~name:"bool-true" (fun _ ctx -> [ Context.bool_const ctx true ]);
  check_a ~name:"bool-false" ~status:Status.Unsat (fun _ ctx ->
    [ Context.bool_const ctx false ]);
  check_a ~name:"int-consts" ~status:Status.Sat (fun env ctx ->
    let x = const env ctx "x" i in
    [ Context.eq ctx x (Context.int_const ctx 0)
    ; Context.eq ctx x (Context.int_const ctx 42)
    ; Context.eq ctx x (Context.int_const ctx (-5))
    ; (* -max_int is the most-negative literal our native-int parser can reingest *)
      Context.eq ctx x (Context.int_const ctx (-max_int))
    ]);
  (* [min_int] prints correctly as [(- 2^62)] (valid SMT-LIB), but its absolute value is
     [max_int + 1], which our native-int parser cannot reingest — the documented
     native-int boundary (ADR-0003). So this is a PRINT-ONLY check, not a round-trip. *)
  check_print
    ~name:"int-min-print"
    (fun env ctx ->
      let x = const env ctx "x" i in
      [ Context.eq ctx x (Context.int_const ctx min_int) ])
    ~expect:
      (Printf.sprintf
         "(- %s)"
         (String.sub
            (string_of_int min_int)
            1
            (String.length (string_of_int min_int) - 1)));
  check_a ~name:"uninterpreted-const" (fun env ctx ->
    let s = usort env "S" in
    let a = const env ctx "a" s
    and c = const env ctx "c" s in
    [ Context.eq ctx a c ]);
  check_a ~name:"app-fun" ~status:Status.Unsat (fun env ctx ->
    let f = fn env "f" [ i ] i in
    let g = fn env "g" [ i; i ] i in
    let x = const env ctx "x" i in
    [ Context.eq ctx (Context.app ctx f [ x ]) (Context.app ctx g [ x; x ]) ]);
  check_a ~name:"predicate" (fun env ctx ->
    let p = fn env "p" [ i ] b in
    let x = const env ctx "x" i in
    [ Context.app ctx p [ x ] ]);
  check_a ~name:"arith-linear" (fun env ctx ->
    let x = const env ctx "x" i
    and y = const env ctx "y" i in
    [ Context.eq
        ctx
        (Context.linear_combination ctx [ 2, x; -3, y ] (-4))
        (Context.int_const ctx 0)
    ]);
  check_a ~name:"arith-coeff-one-plus-const" (fun env ctx ->
    let x = const env ctx "x" i in
    [ Context.eq
        ctx
        (Context.add ctx x (Context.int_const ctx 7))
        (Context.int_const ctx 0)
    ]);
  check_a ~name:"arith-single-coeff" (fun env ctx ->
    let x = const env ctx "x" i in
    [ Context.eq ctx (Context.mul_const ctx 5 x) (Context.int_const ctx 0) ]);
  check_a ~name:"le" (fun env ctx ->
    let x = const env ctx "x" i in
    [ Context.le ctx x (Context.int_const ctx 3) ]);
  check_a ~name:"lt-ge-gt" (fun env ctx ->
    let x = const env ctx "x" i
    and y = const env ctx "y" i in
    [ Context.lt ctx x y; Context.ge ctx x (Context.int_const ctx 0); Context.gt ctx y x ]);
  check_a ~name:"eq-bool-iff" (fun env ctx ->
    let p = const env ctx "p" b
    and q = const env ctx "q" b in
    [ Context.iff ctx p q ]);
  check_a ~name:"not" (fun env ctx ->
    let p = const env ctx "p" b in
    [ Context.not_ ctx p ]);
  check_a ~name:"and-or-nary" (fun env ctx ->
    let p = const env ctx "p" b
    and q = const env ctx "q" b
    and r = const env ctx "r" b in
    [ Context.and_ ctx [ p; q; r ]; Context.or_ ctx [ p; q; r ] ]);
  check_a ~name:"implies" (fun env ctx ->
    let p = const env ctx "p" b
    and q = const env ctx "q" b in
    [ Context.implies ctx p q ]);
  check_a ~name:"ite-int" (fun env ctx ->
    let x = const env ctx "x" i
    and p = const env ctx "p" b in
    [ Context.eq
        ctx
        (Context.ite ctx p x (Context.int_const ctx 0))
        (Context.int_const ctx 1)
    ]);
  check_a ~name:"ite-bool" (fun env ctx ->
    let p = const env ctx "p" b
    and q = const env ctx "q" b
    and r = const env ctx "r" b in
    [ Context.ite ctx p q r ]);
  check_a ~name:"distinct" ~status:Status.Unsat (fun env ctx ->
    let x = const env ctx "x" i
    and y = const env ctx "y" i
    and z = const env ctx "z" i in
    [ Context.distinct ctx [ x; y; z ] ]);
  check_a ~name:"distinct-pair" (fun env ctx ->
    let x = const env ctx "x" i
    and y = const env ctx "y" i in
    [ Context.distinct ctx [ x; y ] ]);
  check_a ~name:"abs" (fun env ctx ->
    let x = const env ctx "x" i in
    [ Context.eq ctx (Context.abs ctx x) (Context.int_const ctx 3) ]);
  check_a ~name:"div-mod" (fun env ctx ->
    let x = const env ctx "x" i in
    [ Context.eq
        ctx
        (Context.div ctx x (Context.int_const ctx 4))
        (Context.int_const ctx 2)
    ; Context.eq
        ctx
        (Context.mod_ ctx x (Context.int_const ctx 4))
        (Context.int_const ctx 1)
    ]);
  check_a ~name:"div-neg-divisor" (fun env ctx ->
    let x = const env ctx "x" i in
    [ Context.eq
        ctx
        (Context.div ctx x (Context.int_const ctx (-3)))
        (Context.int_const ctx 0)
    ]);
  (* quoting edge cases *)
  check_a ~name:"quote-space-parens" (fun env ctx ->
    let a = const env ctx "a b(c)" i in
    [ Context.eq ctx a (Context.int_const ctx 0) ]);
  check_a ~name:"fun-named-Int-ok" (fun env ctx ->
    (* [Int] as a FUNCTION name is legal (distinct namespace from the sort Int) — printed
       bare, must round-trip. (A *sort* named Int is refused; see naming_classes.) *)
    let a = const env ctx "Int" i in
    [ Context.eq ctx a (Context.int_const ctx 0) ]);
  check_a ~name:"quote-uninterpreted-sort-name" (fun env ctx ->
    let s = usort env "My Sort" in
    let a = const env ctx "the a" s
    and c = const env ctx "the c" s in
    [ Context.eq ctx a c ]);
  check_a ~name:"quote-digit-leading" (fun env ctx ->
    let c = const env ctx "3x" i in
    [ Context.eq ctx c (Context.int_const ctx 0) ]);
  check_a ~name:"quote-simple-symbols" (fun env ctx ->
    (* these are all valid simple symbols and must NOT be quoted *)
    let a = const env ctx "a.b" i
    and c = const env ctx "x+y" i
    and d = const env ctx "<hi>" i in
    [ Context.eq ctx a c; Context.eq ctx c d ]);
  (* deep nesting *)
  check_a ~name:"deep-nesting" ~status:Status.Unknown (fun env ctx ->
    let x = const env ctx "x" i
    and y = const env ctx "y" i
    and p = const env ctx "p" b
    and f = fn env "f" [ i ] i in
    let inner =
      Context.ite
        ctx
        p
        (Context.add ctx (Context.app ctx f [ x ]) (Context.int_const ctx 1))
        (Context.sub ctx y (Context.mul_const ctx 2 x))
    in
    [ Context.and_
        ctx
        [ Context.le ctx inner (Context.int_const ctx 100)
        ; Context.or_
            ctx
            [ Context.gt ctx x y; Context.not_ ctx (Context.eq ctx inner x) ]
        ]
    ])
;;

(* Class-driven naming coverage (R1). The driving tables ARE the spec: extending them is a
   one-line change, so a newly-guarded/allowed name class cannot silently escape the suite
   (which is exactly how R1 slipped past the original 46 checks).
   - reserved WORDS: representable via |quoting|, must round-trip;
   - predefined OPERATOR names as functions: unrepresentable, printer must REFUSE;
   - the empty name: REFUSED;
   - predefined SORT names as an uninterpreted sort: REFUSED;
   - a reserved word as a sort name: representable, must round-trip. *)
let reserved_word_class =
  [ "let"; "as"; "forall"; "exists"; "_"; "!"; "par"; "match"; "NUMERAL"; "STRING" ]
;;

let operator_class =
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

let naming_classes () =
  let i = Sort.int in
  (* reserved words as function names: must RENDER as |w| (direct string assertion on the
     printed text — a round-trip alone would pass even if rendered bare, since our own
     parser is lenient; that masking is exactly the R1 pattern) AND round-trip. *)
  List.iter
    (fun w ->
      check_print
        ~name:("reserved-word-render:" ^ w)
        ~expect:(Printf.sprintf "(declare-const |%s| Int)" w)
        (fun env ctx ->
           let c = const env ctx w i in
           [ Context.eq ctx c (Context.int_const ctx 0) ]);
      check_a ~name:("reserved-word:" ^ w) (fun env ctx ->
        let c = const env ctx w i in
        [ Context.eq ctx c (Context.int_const ctx 0) ]))
    reserved_word_class;
  (* predefined operators as function names (const and applied): REFUSED *)
  List.iter
    (fun op ->
      check_refused ~name:("operator-const:" ^ op) (fun env ctx ->
        let c = const env ctx op i in
        [ Context.eq ctx c (Context.int_const ctx 0) ]);
      check_refused ~name:("operator-app:" ^ op) (fun env ctx ->
        let f = fn env op [ i ] i in
        let x = const env ctx "x" i in
        [ Context.eq ctx (Context.app ctx f [ x ]) (Context.int_const ctx 0) ]))
    operator_class;
  (* empty name: REFUSED *)
  check_refused ~name:"empty-name" (fun env ctx ->
    let c = const env ctx "" i in
    [ Context.eq ctx c (Context.int_const ctx 0) ]);
  (* predefined SORT names as an uninterpreted sort: REFUSED *)
  List.iter
    (fun s ->
      check_refused ~name:("sort-name:" ^ s) (fun env ctx ->
        let so = usort env s in
        let a = const env ctx "a" so
        and c = const env ctx "c" so in
        [ Context.eq ctx a c ]))
    [ "Int"; "Bool" ];
  (* reserved word as a sort name: |quoted|, round-trip *)
  check_a ~name:"sort-reserved-word" (fun env ctx ->
    let so = usort env "forall" in
    let a = const env ctx "a" so
    and c = const env ctx "c" so in
    [ Context.eq ctx a c ])
;;

(* ---- define-fun macro expansion (task board #76) ---- *)

let define_fun_cases () =
  let hdr = "(set-logic QF_UFLIA)\n" in
  let decls = "(declare-const a Int)\n(declare-const b Int)\n" in
  (* 1. basic unary macro: (inc a) == (+ a 1) *)
  check_same
    ~name:"basic"
    (hdr ^ decls ^ "(define-fun inc ((x Int)) Int (+ x 1))\n(assert (<= (inc a) b))\n")
    (hdr ^ decls ^ "(assert (<= (+ a 1) b))\n");
  (* 2. zero-arg define = named constant *)
  check_same
    ~name:"zero-arg-const"
    (hdr ^ decls ^ "(define-fun k () Int 42)\n(assert (<= a k))\n")
    (hdr ^ decls ^ "(assert (<= a 42))\n");
  (* 3. multi-arg + nested application in body *)
  check_same
    ~name:"multi-arg"
    (hdr
     ^ decls
     ^ "(define-fun g ((x Int) (y Int)) Int (+ x (* 2 y)))\n(assert (<= (g a b) 0))\n")
    (hdr ^ decls ^ "(assert (<= (+ a (* 2 b)) 0))\n");
  (* 4. parameter shadowed by a nested let in the body (let binds tighter): the arg is
     ignored, body = 7 + 1 = 8. If shadowing were wrong we'd get a+1 instead. *)
  check_same
    ~name:"param-shadowed-by-let"
    (hdr
     ^ decls
     ^ "(define-fun h ((x Int)) Int (let ((x 7)) (+ x 1)))\n(assert (<= (h a) b))\n")
    (hdr ^ decls ^ "(assert (<= 8 b))\n");
  (* 5. body references a global const *)
  check_same
    ~name:"body-uses-global"
    (hdr ^ decls ^ "(define-fun m ((x Int)) Int (+ x b))\n(assert (<= (m a) 0))\n")
    (hdr ^ decls ^ "(assert (<= (+ a b) 0))\n");
  (* 6. Bool-valued macro over an uninterpreted sort + predicate *)
  check_same
    ~name:"bool-macro"
    (hdr
     ^ "(declare-sort S 0)\n\
        (declare-fun p (S) Bool)\n\
        (declare-const u S)\n\
        (define-fun q ((z S)) Bool (not (p z)))\n\
        (assert (q u))\n")
    (hdr
     ^ "(declare-sort S 0)\n\
        (declare-fun p (S) Bool)\n\
        (declare-const u S)\n\
        (assert (not (p u)))\n");
  (* 7. a macro used inside a let: the argument is read in the caller scope (sees the
     let), but the body does NOT see the caller's let binding. (m x) = x + b, arg = a. *)
  check_same
    ~name:"arg-read-in-caller-scope"
    (hdr
     ^ decls
     ^ "(define-fun m2 ((x Int)) Int (+ x b))\n(assert (<= (let ((c a)) (m2 c)) 0))\n")
    (hdr ^ decls ^ "(assert (<= (+ a b) 0))\n");
  (* round-trip: expanded terms print (define-fun erased) and re-parse identically *)
  check_df_roundtrip
    ~name:"basic"
    (hdr ^ decls ^ "(define-fun inc ((x Int)) Int (+ x 1))\n(assert (<= (inc a) b))\n");
  check_df_roundtrip
    ~name:"nested-and-bool"
    (hdr
     ^ decls
     ^ "(define-fun clamp ((x Int)) Int (ite (< x 0) 0 x))\n\
        (assert (= (clamp a) (clamp b)))\n");
  (* core-bignum W2: integer literals and coefficients past native int63 (max_int =
     2^62-1) parse and print/re-parse identically. 2^62, 2^63, 2^64 (the convert-family
     value) each exceed max_int; the last case is a negated big literal and a big
     coefficient. Discriminating: pre-bignum [int_lit] returned Unsupported ("exceeds
     native int range") on every one of these, so the parse step here failed outright. *)
  check_df_roundtrip
    ~name:"bignum-literals-boundary"
    (hdr
     ^ decls
     ^ "(assert (<= a 4611686018427387904))\n\
        (assert (<= a 9223372036854775808))\n\
        (assert (>= a 18446744073709551616))\n\
        (assert (= (* a 18446744073709551616) (- 9223372036854775808)))\n");
  (* ---- rejections ---- *)
  (* caller's let binding must NOT leak into the body: body references y, which is neither
     a param nor a global, so expansion fails with an undeclared-symbol Malformed even
     though the caller binds y. *)
  check_malformed
    ~name:"no-caller-capture"
    (hdr
     ^ decls
     ^ "(define-fun leak ((x Int)) Int (+ x y))\n(assert (<= (let ((y a)) (leak 0)) 0))\n"
    );
  (* direct recursion *)
  check_unsupported
    ~name:"direct-recursion"
    (hdr ^ decls ^ "(define-fun r ((x Int)) Int (r x))\n(assert (<= (r a) 0))\n");
  (* mutual recursion (f -> g -> f); both defined before use, cycle caught at expansion *)
  check_unsupported
    ~name:"mutual-recursion"
    (hdr
     ^ decls
     ^ "(define-fun f ((x Int)) Int (g x))\n\
        (define-fun g ((x Int)) Int (f x))\n\
        (assert (<= (f a) 0))\n");
  (* define-fun-rec is rejected outright *)
  check_unsupported
    ~name:"define-fun-rec"
    (hdr
     ^ "(define-fun-rec r ((x Int)) Int (ite (<= x 0) 0 (r (- x 1))))\n(assert true)\n");
  (* arity mismatch at use site *)
  check_malformed
    ~name:"arity-mismatch"
    (hdr ^ decls ^ "(define-fun inc ((x Int)) Int (+ x 1))\n(assert (<= (inc a b) 0))\n");
  (* argument sort mismatch: inc expects Int, given a Bool *)
  check_malformed
    ~name:"arg-sort-mismatch"
    (hdr
     ^ "(declare-const p Bool)\n\
        (define-fun inc ((x Int)) Int (+ x 1))\n\
        (assert (<= (inc p) 0))\n");
  (* body sort disagrees with declared result sort (Int body, Bool result) *)
  check_malformed
    ~name:"body-ret-sort-mismatch"
    (hdr ^ decls ^ "(define-fun bad ((x Int)) Bool (+ x 1))\n(assert (bad a))\n");
  (* redeclaration: a name both declared and defined *)
  check_malformed
    ~name:"redeclare-decl-then-define"
    (hdr ^ "(declare-fun f (Int) Int)\n(define-fun f ((x Int)) Int x)\n(assert true)\n")
;;

(* Ground [xor] desugaring (n-ary, left-assoc): each [xor] step is [not (= acc a)]. The
   quantifier IR already builds [Fol.Xor]; these guard the quantifier-free [read_op] path
   that a bare [(xor ...)] takes (previously rejected as an unknown operator — the
   QF_LRA/sc benchmark family, 31 xor/file, could not parse). Checked against the
   hand-expanded form by tag identity, so a wrong desugar (arity/associativity) fails. *)
let xor_cases () =
  let hdr = "(set-logic QF_UFLIA)\n" in
  let decls =
    "(declare-const a Bool)\n(declare-const b Bool)\n(declare-const c Bool)\n"
  in
  (* binary: (xor a b) == (not (= a b)) *)
  check_same
    ~name:"xor-binary"
    (hdr ^ decls ^ "(assert (xor a b))\n")
    (hdr ^ decls ^ "(assert (not (= a b)))\n");
  (* ternary, left-assoc: (xor a b c) == (not (= (not (= a b)) c)) *)
  check_same
    ~name:"xor-ternary-left-assoc"
    (hdr ^ decls ^ "(assert (xor a b c))\n")
    (hdr ^ decls ^ "(assert (not (= (not (= a b)) c)))\n");
  (* arity: unary xor is malformed (>= 2 operands required) *)
  check_malformed ~name:"xor-unary" (hdr ^ decls ^ "(assert (xor a))\n")
;;

(* Parser fail-closed guards (codex rider): the parser must ERROR/degrade on ill-typed or
   malformed input, never mis-parse to a definite verdict. Each was a wrong-verdict bug. *)
let parser_fail_closed_cases () =
  let hdr = "(set-logic QF_UFLIA)\n" in
  (* F1 (let form): a let-bound scalar shadows a global function; using the bound name in
     head position [(f 1)] is ill-sorted. Pre-fix [read_app] resolved the head to the
     global [f], mis-parsing the body and producing a (wrong) [unsat]. Now [read_app]
     consults the scope for the head first -> Malformed -> unknown. *)
  check_malformed
    ~name:"F1-let-shadow-head"
    (hdr
     ^ "(declare-fun f (Int) Int)\n\
        (assert (= (f 1) 1))\n\
        (assert (let ((f 0)) (= (f 1) 5)))\n\
        (check-sat)\n");
  (* F2: a malformed [!] annotation tail — a bare term where a [:keyword] attribute is
     expected — was silently dropped, deleting the [forall] and flipping [unsat] to [sat].
     The tail is now validated; the bad tail is Malformed -> unknown. *)
  check_malformed
    ~name:"F2-bang-tail-drops-quantifier"
    (hdr ^ "(assert (! true (forall ((x Int)) false)))\n(check-sat)\n");
  (* F2 control: a WELL-FORMED [:named] tail still parses (it is validated then dropped). *)
  check_parse_ok
    ~name:"F2-bang-named-ok"
    (hdr ^ "(declare-const p Bool)\n(assert (! p :named foo))\n(check-sat)\n");
  (* H3 (lemmas-climb review hedge): a DEGENERATE quantifier in term position — bare
     [(exists)], binder-less / wrong-arity — must be [Malformed] (hard fail), not the
     salvageable [Unsupported] a well-formed-but-nested quantifier gets. Guards the
     "Malformed whole-fails, Unsupported drops" contract on the nested-quantifier path
     ([read_term] validates the quantifier's structural shape before rejecting it). *)
  check_malformed
    ~name:"H3-degenerate-nested-exists"
    (hdr ^ "(declare-fun p (Int) Bool)\n(assert (or (p 0) (exists)))\n(check-sat)\n");
  (* H3 control: a WELL-FORMED nested quantifier is out of the fragment but NOT malformed
     — it is [Unsupported], which partial assertion (lemmas-climb) SALVAGE-DROPS rather
     than raising, so the whole document parses OK (the assertion is dropped; the query
     degrades to a sound [unknown] at solve time). Contrast the degenerate case above,
     whose [Malformed] propagates and fails the parse. Together they pin the "Malformed
     hard-fails / Unsupported drops" distinction on the nested-quantifier path. *)
  check_parse_ok
    ~name:"H3-wellformed-nested-exists-parses-ok-dropped"
    (hdr
     ^ "(declare-fun p (Int) Bool)\n\
        (assert (or (p 0) (exists ((x Int)) (p x))))\n\
        (check-sat)\n");
  (* Command-state boundary: the parser returns one assertion batch. Before this guard it
     accepted all three scripts and the real CLI reported [unsat]: junk arguments still
     counted as a check, a post-check assertion was moved before the check, and a
     post-exit assertion was executed even though SMT-LIB terminates at [exit]. Each must
     instead reach the CLI's parse-failure -> [unknown] path. *)
  check_malformed
    ~name:"check-sat-arguments"
    (hdr ^ "(assert false)\n(check-sat unexpected)\n");
  check_unsupported ~name:"assert-after-check-sat" (hdr ^ "(check-sat)\n(assert false)\n");
  check_malformed
    ~name:"command-after-exit"
    (hdr ^ "(exit)\n(assert false)\n(check-sat)\n");
  (* Tester-name collision (parser path). A datatype constructor [zero] mints the tester
     function [is-zero]. On the PARSER path a user symbol of that same name can never
     coexist with the tester — [declare_fun]'s redeclaration guard rejects whichever comes
     second — so the printer can never conflate them here (the programmatic client path,
     which bypasses this guard, is closed separately by minting testers in the reserved
     namespace). Both declaration orders must be Malformed. *)
  let dt = "(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))\n" in
  check_malformed
    ~name:"tester-collision-dt-then-user"
    (hdr ^ dt ^ "(declare-fun is-zero () Bool)\n(assert true)\n");
  check_malformed
    ~name:"tester-collision-user-then-dt"
    (hdr ^ "(declare-fun is-zero () Bool)\n" ^ dt ^ "(assert true)\n")
;;

let str_contains hay needle =
  let nl = String.length needle in
  let rec go i =
    (i + nl <= String.length hay && String.equal (String.sub hay i nl) needle)
    || (i + nl <= String.length hay && go (i + 1))
  in
  nl = 0 || go 0
;;

(* Tester-name collision, PROGRAMMATIC-client path (codex, TCB gate-integrity). A client
   that bypasses the parser (the compiler oracle path) can declare a user function AND
   register a datatype in one [Env]. Were the datatype's tester minted under the SAME NAME
   as the user function, they would be the SAME interned symbol, and the printer — which
   suppresses ctor/selector/tester [declare-fun]s by registry membership — would silently
   DROP the user function's declaration, so the Lean oracle would certify a DIFFERENT
   problem. The closing invariant: tester symbols are minted in the RESERVED [.oxsmt.*]
   namespace (which [Env] forbids user declarations from), so a reserved tester can never
   be the same symbol as any user fn. This test constructs that configuration directly — a
   reserved tester for [succ] alongside a USER fn literally named [is-succ] — and asserts
   the printed session STILL declares and uses the user [is-succ] (neither suppressed nor
   mis-rendered as a tester). A regression that minted testers under the bare [is-succ]
   name would make [user_is_succ] the tester symbol and drop it, flipping this red. *)
let printer_programmatic_tester_collision () =
  incr checks;
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  let nat_sym = Env.declare_sort env "nat" in
  let nat = Sort.datatype_ nat_sym in
  let succ = Env.declare_fun env "succ" (Rank.create [ nat ] nat) in
  let pred = Env.declare_fun env "pred" (Rank.create [ nat ] nat) in
  let zero = Env.declare_fun env "zero" (Rank.create [] nat) in
  let mk_tester c =
    Env.declare_reserved
      cap
      env
      (Env.reserved_prefix ^ "dt.is." ^ c)
      (Rank.create [ nat ] Sort.bool)
  in
  let dts =
    Datatype_defs.add
      Datatype_defs.empty
      { Datatype_defs.sort_sym = nat_sym
      ; constructors =
          [ { Datatype_defs.sym = succ
            ; selectors = [ { Datatype_defs.sym = pred; index = 0; field_sort = nat } ]
            ; tester = mk_tester "succ"
            }
          ; { Datatype_defs.sym = zero; selectors = []; tester = mk_tester "zero" }
          ]
      }
  in
  (* a USER function whose NAME collides with the tester's conventional "is-succ" *)
  let user_is_succ = Env.declare_fun env "is-succ" (Rank.create [ nat ] Sort.bool) in
  let x = Context.const ctx (Env.declare_fun env "x" (Rank.create [] nat)) in
  let text =
    Printer.print_session ~datatypes:dts env [ Context.app ctx user_is_succ [ x ] ]
  in
  if not (str_contains text "(declare-fun is-succ")
  then
    fail
      "programmatic-tester-collision: user fn is-succ was dropped from the printed session\n\
       %s"
      text;
  if str_contains text "((_ is succ)"
  then
    fail
      "programmatic-tester-collision: user fn is-succ was mis-rendered as a tester\n%s"
      text
;;

(* BYTE-level printer oracle (codex): round-trip B compares assertions by [Term.equal],
   and the parser accepts BOTH [(is-C t)] and [((_ is C) t)], so a printer mutant that
   emits the wrong tester form still round-trips equal — the byte-identity claim is
   untested there. This pins print_session's EXACT output for a datatype session with a
   tester: the [(set-logic QF_UFDTLIA)] label (not QF_UFDT), the [(declare-datatypes ...)]
   block with ctor/selector suppression, and the [((_ is succ) n)] tester rendering. Any
   of those mutating changes the bytes and fails here. *)
let printer_byte_oracle () =
  incr checks;
  let src =
    "(set-logic QF_DT)\n\
     (declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))\n\
     (declare-const n nat)\n\
     (assert ((_ is succ) n))\n\
     (check-sat)\n"
  in
  let expected =
    "(set-logic QF_UFDTLIA)\n\
     (declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))\n\
     (declare-const n nat)\n\
     (assert ((_ is succ) n))\n\
     (check-sat)\n"
  in
  match Parser.parse src with
  | exception e -> fail "printer-byte-oracle: parse raised %s" (Printexc.to_string e)
  | parsed ->
    let text =
      Printer.print_session ~datatypes:parsed.datatypes parsed.env parsed.assertions
    in
    if not (String.equal text expected)
    then
      fail
        "printer-byte-oracle: bytes differ\n--- expected ---\n%s--- actual ---\n%s"
        expected
        text
;;

(* BYTE oracle for the Bigint x datatype rendering INTERSECTION (team-lead): a datatype
   session whose Int-field selector is equated to a literal far beyond int63. Pins that
   the arbitrary-precision numeral renders via [Bigint.to_string] EXACTLY, inside the
   datatype-session framing (QF_UFDTLIA + declare-datatypes with the Int-field selector,
   which is suppressed as a datatype member and used bare in [(val c)]). The literal is
   2^128, unrepresentable in int63. *)
let printer_byte_oracle_bignum () =
  incr checks;
  let big = "340282366920938463463374607431768211456" in
  let src =
    "(set-logic QF_DT)\n\
     (declare-datatypes ((cell 0)) (((mk (val Int)))))\n\
     (declare-const c cell)\n\
     (assert (= (val c) "
    ^ big
    ^ "))\n(check-sat)\n"
  in
  let expected =
    "(set-logic QF_UFDTLIA)\n\
     (declare-datatypes ((cell 0)) (((mk (val Int)))))\n\
     (declare-const c cell)\n\
     (assert (= (val c) "
    ^ big
    ^ "))\n(check-sat)\n"
  in
  match Parser.parse src with
  | exception e ->
    fail "printer-byte-oracle-bignum: parse raised %s" (Printexc.to_string e)
  | parsed ->
    let text =
      Printer.print_session ~datatypes:parsed.datatypes parsed.env parsed.assertions
    in
    if not (String.equal text expected)
    then
      fail
        "printer-byte-oracle-bignum: bytes differ\n--- expected ---\n%s--- actual ---\n%s"
        expected
        text
;;

(* Exponential-re-read guard (reviewer-identified test-only DoS): a doubling chain
   [f_{i+1}(x) = f_i(x) + f_i(x)] makes an unmemoized expander read [f_0]'s body 2^depth
   times. Memoization on (define, arg-tags) collapses that to one expansion per (define,
   arg), i.e. linear. Every [f_i] here is called with the same argument [a], so a
   correctly-memoized parse does [depth+1] expansions and finishes instantly; an
   unmemoized one does 2^depth reads and cannot finish at depth 40. We assert BOTH the
   result is right (the chain equals [2^depth * a]) and a coarse wall-bound, so a memo
   regression fails loudly rather than merely hanging. *)
let define_fun_perf () =
  incr checks;
  let depth = 40 in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "(set-logic QF_UFLIA)\n(declare-const a Int)\n";
  Buffer.add_string buf "(define-fun f0 ((x Int)) Int x)\n";
  for i = 1 to depth do
    Buffer.add_string
      buf
      (Printf.sprintf
         "(define-fun f%d ((x Int)) Int (+ (f%d x) (f%d x)))\n"
         i
         (i - 1)
         (i - 1))
  done;
  Buffer.add_string buf (Printf.sprintf "(assert (<= (f%d a) 0))\n" depth);
  let env = Env.create () in
  let ctx = Context.create env in
  let t0 = Sys.time () in
  match Parser.parse_into env ctx (Buffer.contents buf) with
  | p ->
    let dt = Sys.time () -. t0 in
    (* f_depth(a) = 2^depth * a; 2^40 fits native int (< 2^62). *)
    let coeff = 1 lsl depth in
    let expanded =
      Parser.parse_into
        env
        ctx
        (Printf.sprintf
           "(set-logic QF_UFLIA)\n(declare-const a Int)\n(assert (<= (* %d a) 0))\n"
           coeff)
    in
    if not (terms_equal p.assertions expanded.assertions)
    then fail "df-perf: depth-%d doubling chain expanded to the wrong term" depth
    else if dt > 5.0
    then fail "df-perf: depth-%d parse took %.2fs cpu — memoization regressed?" depth dt
  | exception e -> fail "df-perf: parse raised %s" (Printexc.to_string e)
;;

(* ---- direction B: parse -> print -> parse over committed files ---- *)

let smt2_files dir =
  let rec walk acc dir =
    let entries =
      try Sys.readdir dir with
      | _ -> [||]
    in
    Array.sort String.compare entries;
    Array.fold_left
      (fun acc e ->
        let p = Filename.concat dir e in
        if Sys.is_directory p
        then walk acc p
        else if Filename.check_suffix p ".smt2"
        then p :: acc
        else acc)
      acc
      entries
  in
  List.rev (walk [] dir)
;;

let read_file p = In_channel.with_open_bin p In_channel.input_all
let b_pass = ref 0
let b_skip = ref 0
let b_skip_unprintable = ref 0

(* The EXACT set of committed files allowed to enter the print-time unprintable-skip
   class: the two degenerate goldens that exist to DOCUMENT a legitimate printer refusal
   (the empty symbol [||]; a predefined-operator collision like [|+|]). The skip class
   must stay bounded to these — any OTHER file that becomes unprintable is a printer
   regression on legal input (e.g. [quote_sort_symbol] wrongly raising on an ordinary sort
   would silently skip [euf_unsat.smt2]) and MUST fail the suite rather than hide behind
   the counter. Matched by basename so it is independent of which argv dir supplied the
   file. *)
let b_unprintable_allowlist =
  [ "bool_empty_symbol_unknown.smt2"; "bool_operator_collision_unknown.smt2" ]
;;

let b_unprintable_seen = ref []

let check_b path =
  let text = read_file path in
  (* board #58: a committed file may mint arrays [.oxsmt.arr.*] or bit-vector
     [.oxsmt.bv|*] op/marker symbols mid-parse through the cap-backed [?internal_mint]
     door, so this direction-B check owns a throwaway env with its own capability (the
     analogue of what a [Session] hands the product drivers). The env is private to this
     parse, so there is no cross-context aliasing risk; the SAME minter/env backs the
     reprint reparse below, so a round-tripped reserved symbol interns to one identity. *)
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  (* Admit BOTH theory grammars (the same narrowing [Session.parse_minter] applies in the
     combined tree), so this local parse can intern arrays op symbols and bit-vector
     markers and nothing else. *)
  let internal_mint =
    Internal_minter.create
      ~admit:(fun name -> Array_defs.is_op_name name || Bv.is_bv_name name)
      cap
      env
  in
  match Parser.parse_into ~internal_mint env ctx text with
  | exception Parser.Malformed m ->
    incr b_skip;
    Printf.printf "  skip (malformed): %s (%s)\n" path m
  | exception Parser.Unsupported m ->
    incr b_skip;
    Printf.printf "  skip (unsupported): %s (%s)\n" path m
  | parsed ->
    (* A file can parse yet contain a symbol the shipped printer legitimately refuses (the
       empty symbol [||], an operator collision like [|+|]) — such goldens exist to
       document exactly that refusal (e.g. tests/cases/bool_*_unknown.smt2). Printing is
       not the identity on them, so they are a COUNTED skip, not a failure: reported and
       tallied as [skipped-unprintable], never silent. This is print-time and distinct
       from the parse-time (malformed/unsupported) skips above; direction A must never
       gain this catch, since A only prints printer-legal sessions and a raise there is a
       real bug. *)
    (match
       Printer.print_session
         ?status:parsed.status
         ~datatypes:parsed.datatypes
         ~arrays:parsed.arrays
         parsed.env
         parsed.assertions
     with
     | exception Printer.Unsupported m ->
       incr b_skip_unprintable;
       b_unprintable_seen := Filename.basename path :: !b_unprintable_seen;
       Printf.printf "  skip (unprintable): %s (%s)\n" path m
     | out ->
       incr checks;
       (match Parser.parse_into ~internal_mint parsed.env parsed.ctx out with
        | parsed2 ->
          if not (terms_equal parsed.assertions parsed2.assertions)
          then fail "B/%s: assertions differ after reprint\n%s" path out
          else if not (status_equal parsed.status parsed2.status)
          then fail "B/%s: status not preserved" path
          else incr b_pass
        | exception Parser.Malformed m ->
          fail "B/%s: reparse Malformed: %s\n%s" path m out
        | exception Parser.Unsupported m ->
          fail "B/%s: reparse Unsupported: %s\n%s" path m out))
;;

(* Stateful-command gate (board #152 item 5 / issue5144): [reset] / [reset-assertions]
   clear the assertion set mid-script, which the batch single-check reader cannot honour,
   so they must FAIL CLOSED (raise [Unsupported] -> the CLI degrades to [unknown]) rather
   than be silently ignored. Discrimination: the exact issue5144 shape below is SAT (the
   contradiction is reset away before the check), but with the pre-fix silent no-op the
   parser returned [(= 0 1)] as a live assertion -> the CLI answered [unsat]. The pre-fix
   code PARSED this OK, so [check_unsupported] was RED before the fix. Non-stateful
   directives ([set-option], [get-*]) stay no-ops — ignoring them cannot flip a verdict. *)
let command_gate_cases () =
  let hdr = "(set-logic QF_LIA)\n" in
  (* the verdict-flipping repro: assert-false, reset-assertions, check-sat *)
  check_unsupported
    ~name:"reset-assertions-fail-closed"
    (hdr ^ "(assert (= 0 1))\n(reset-assertions)\n(check-sat)\n");
  check_unsupported
    ~name:"reset-fail-closed"
    (hdr ^ "(assert (= 0 1))\n(reset)\n(check-sat)\n");
  (* positive controls: non-stateful directives must still parse (kept as no-ops), so a
     file using them is NOT spuriously degraded to unknown. *)
  let check_parses_ok ~name text =
    incr checks;
    match Parser.parse text with
    | _ -> ()
    | exception e ->
      fail "parses-ok/%s: expected parse OK, got %s" name (Printexc.to_string e)
  in
  check_parses_ok
    ~name:"set-option-and-get-still-parse"
    (hdr
     ^ "(set-option :produce-models true)\n\
        (declare-const a Int)\n\
        (assert (<= a 0))\n\
        (get-value (a))\n\
        (check-sat)\n\
        (get-model)\n")
;;

(* Bitvectors (GOALS: bitvectors). Direction-A round-trips build terms via the [Bv] smart
   constructors, print (set-logic QF_UFBV, [(_ BitVec w)] sorts, [#b...] literals,
   prefix/indexed operators), reparse into the SAME context, and compare by [Term.equal] —
   so parser + printer + sort-checking are all exercised at once. Plus parse-only cases
   for the sugar duals, and malformed cases for the width/index errors (fail-closed). *)
let bv_cases () =
  let bvc env ctx name w = const env ctx name (Sort.bitvec w) in
  let lit ctx mint v w = Bv.const ctx mint ~value:(Bigint.of_int v) ~width:w in
  check_a_bv ~name:"bv-literal-eq" ~status:Status.Sat (fun env ctx mint ->
    let x = bvc env ctx "x" 4 in
    [ Context.eq ctx x (lit ctx mint 5 4) ]);
  check_a_bv ~name:"bv-hex-literal" (fun env ctx mint ->
    let x = bvc env ctx "x" 8 in
    [ Context.eq ctx x (lit ctx mint 0xa3 8) ]);
  check_a_bv ~name:"bv-bitwise" (fun env ctx mint ->
    let x = bvc env ctx "x" 8
    and y = bvc env ctx "y" 8 in
    [ Context.eq
        ctx
        (Bv.binop ctx mint Bv.Bvand x y)
        (Bv.binop ctx mint Bv.Bvor x (Bv.unop ctx mint Bv.Bvnot y))
    ]);
  check_a_bv ~name:"bv-arith" (fun env ctx mint ->
    let x = bvc env ctx "x" 16
    and y = bvc env ctx "y" 16 in
    [ Context.eq
        ctx
        (Bv.binop
           ctx
           mint
           Bv.Bvadd
           (Bv.binop ctx mint Bv.Bvmul x y)
           (Bv.unop ctx mint Bv.Bvneg x))
        (Bv.binop ctx mint Bv.Bvsub x y)
    ]);
  check_a_bv ~name:"bv-shifts" (fun env ctx mint ->
    let x = bvc env ctx "x" 8
    and y = bvc env ctx "y" 8 in
    [ Context.eq
        ctx
        (Bv.binop ctx mint Bv.Bvshl x y)
        (Bv.binop ctx mint Bv.Bvashr (Bv.binop ctx mint Bv.Bvlshr x y) y)
    ]);
  check_a_bv ~name:"bv-compares" (fun env ctx mint ->
    let x = bvc env ctx "x" 8
    and y = bvc env ctx "y" 8 in
    [ Context.and_
        ctx
        [ Bv.binop ctx mint Bv.Bvult x y
        ; Bv.binop ctx mint Bv.Bvule x y
        ; Bv.binop ctx mint Bv.Bvslt x y
        ; Bv.binop ctx mint Bv.Bvsle x y
        ]
    ]);
  check_a_bv ~name:"bv-concat-extract" (fun env ctx mint ->
    let x = bvc env ctx "x" 4
    and y = bvc env ctx "y" 4 in
    let c = Bv.concat ctx mint x y in
    (* extract the high nibble back out; it must equal x *)
    [ Context.eq ctx (Bv.extract ctx mint ~i:7 ~j:4 c) x ]);
  check_a_bv ~name:"bv-extends" (fun env ctx mint ->
    let x = bvc env ctx "x" 8 in
    [ Context.eq ctx (Bv.zero_extend ctx mint ~n:8 x) (Bv.sign_extend ctx mint ~n:8 x) ]);
  check_a_bv ~name:"bv-udiv-urem" (fun env ctx mint ->
    let x = bvc env ctx "x" 8
    and y = bvc env ctx "y" 8 in
    [ Context.eq ctx (Bv.binop ctx mint Bv.Bvudiv x y) (Bv.binop ctx mint Bv.Bvurem x y) ]);
  (* sugar: (bvugt x y) parses to (bvult y x); prove the rewrite by comparing to the
     hand-written swapped form in one context. *)
  let bv_hdr =
    "(set-logic QF_UFBV)\n\
     (declare-const x (_ BitVec 8))\n\
     (declare-const y (_ BitVec 8))\n"
  in
  check_same
    ~name:"bv-ugt-sugar"
    (bv_hdr ^ "(assert (bvugt x y))\n")
    (bv_hdr ^ "(assert (bvult y x))\n");
  check_same
    ~name:"bv-sge-sugar"
    (bv_hdr ^ "(assert (bvsge x y))\n")
    (bv_hdr ^ "(assert (bvsle y x))\n");
  check_parse_ok ~name:"bv-all-ops" (bv_hdr ^ "(assert (bvuge (bvadd x #x01) y))\n");
  (* width errors are parse-time failures (Malformed), fail-closed *)
  check_malformed
    ~name:"bv-width-mismatch"
    "(set-logic QF_UFBV)\n\
     (declare-const x (_ BitVec 8))\n\
     (declare-const y (_ BitVec 4))\n\
     (assert (= (bvadd x y) x))\n";
  check_malformed
    ~name:"bv-extract-oob"
    "(set-logic QF_UFBV)\n\
     (declare-const x (_ BitVec 8))\n\
     (assert (= ((_ extract 8 0) x) x))\n";
  check_malformed
    ~name:"bv-compare-non-bv"
    "(set-logic QF_UFBV)\n\
     (declare-const x (_ BitVec 8))\n\
     (declare-const n Int)\n\
     (assert (bvult x n))\n";
  check_malformed
    ~name:"bv-bad-width-sort"
    "(set-logic QF_UFBV)\n(declare-const x (_ BitVec 0))\n(assert (= x x))\n"
;;

let () =
  print_endline "== round-trip A (print -> parse), programmatic sessions ==";
  sessions ();
  print_endline "== bitvectors (round-trip + sugar + width errors) ==";
  bv_cases ();
  print_endline
    "== naming classes (reserved words quoted; operators/sorts/empty refused) ==";
  naming_classes ();
  print_endline "== stateful-command gate (reset/reset-assertions fail closed) ==";
  command_gate_cases ();
  print_endline "== define-fun macro expansion ==";
  define_fun_cases ();
  print_endline "== ground xor desugaring ==";
  xor_cases ();
  parser_fail_closed_cases ();
  printer_programmatic_tester_collision ();
  printer_byte_oracle ();
  printer_byte_oracle_bignum ();
  define_fun_perf ();
  let dirs = List.tl (Array.to_list Sys.argv) in
  if dirs <> []
  then (
    print_endline "== round-trip B (parse -> print -> parse), committed files ==";
    List.iter
      (fun dir -> if Sys.file_exists dir then List.iter check_b (smt2_files dir))
      dirs;
    (* Bound the unprintable-skip class to the exact allowlist (the two degenerate
       goldens). Any OTHER file skipped here is a printer regression on legal input hiding
       behind the counter — hard FAILURE, named. Symmetrically, an allowlist golden that
       STOPPED being unprintable (printer/golden drift) is also flagged: the standard
       invocation scans tests/cases, so both goldens are exercised every run. *)
    let seen = List.sort_uniq String.compare !b_unprintable_seen in
    let unexpected =
      List.filter (fun n -> not (List.mem n b_unprintable_allowlist)) seen
    in
    let missing = List.filter (fun n -> not (List.mem n seen)) b_unprintable_allowlist in
    List.iter
      (fun n ->
        fail
          "B: %s entered the unprintable-skip class but is NOT allowlisted (printer \
           regression on legal input?)"
          n)
      unexpected;
    List.iter
      (fun n ->
        fail
          "B: allowlisted golden %s was NOT unprintable-skipped this run (printer \
           stopped refusing it, or it was not scanned — expected under tests/cases)"
          n)
      missing;
    let expected = unexpected = [] && missing = [] in
    Printf.printf
      "  B: %d round-tripped, %d skipped, skipped-unprintable: %d (%s)\n"
      !b_pass
      !b_skip
      !b_skip_unprintable
      (if expected then "expected" else "UNEXPECTED — see failures above"));
  Printf.printf "\n%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
