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

(* declaration helpers *)
let const env ctx name sort =
  Context.const ctx (Env.declare_fun env name (Rank.create [] sort))
;;

let fn env name dom cod = Env.declare_fun env name (Rank.create dom cod)
let usort env name = Sort.uninterpreted (Env.declare_sort env name)

let sessions () =
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
  check_a ~name:"quote-reserved-Int" (fun env ctx ->
    let a = const env ctx "Int" i in
    [ Context.eq ctx a (Context.int_const ctx 0) ]);
  check_a ~name:"quote-uninterpreted-sort-name" (fun env ctx ->
    let s = usort env "My Sort" in
    let a = const env ctx "the a" s
    and c = const env ctx "the c" s in
    [ Context.eq ctx a c ]);
  check_a ~name:"quote-empty-and-digits" (fun env ctx ->
    let a = const env ctx "" i
    and c = const env ctx "3x" i in
    [ Context.eq ctx a c ]);
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

let check_b path =
  let text = read_file path in
  match Parser.parse text with
  | exception Parser.Malformed m ->
    incr b_skip;
    Printf.printf "  skip (malformed): %s (%s)\n" path m
  | exception Parser.Unsupported m ->
    incr b_skip;
    Printf.printf "  skip (unsupported): %s (%s)\n" path m
  | parsed ->
    incr checks;
    let out = Printer.print_session ?status:parsed.status parsed.env parsed.assertions in
    (match Parser.parse_into parsed.env parsed.ctx out with
     | parsed2 ->
       if not (terms_equal parsed.assertions parsed2.assertions)
       then fail "B/%s: assertions differ after reprint\n%s" path out
       else if not (status_equal parsed.status parsed2.status)
       then fail "B/%s: status not preserved" path
       else incr b_pass
     | exception Parser.Malformed m -> fail "B/%s: reparse Malformed: %s\n%s" path m out
     | exception Parser.Unsupported m ->
       fail "B/%s: reparse Unsupported: %s\n%s" path m out)
;;

let () =
  print_endline "== round-trip A (print -> parse), programmatic sessions ==";
  sessions ();
  let dirs = List.tl (Array.to_list Sys.argv) in
  if dirs <> []
  then (
    print_endline "== round-trip B (parse -> print -> parse), committed files ==";
    List.iter
      (fun dir -> if Sys.file_exists dir then List.iter check_b (smt2_files dir))
      dirs;
    Printf.printf "  B: %d round-tripped, %d skipped\n" !b_pass !b_skip);
  Printf.printf "\n%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
