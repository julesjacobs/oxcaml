open Oxsmt_core
module Parser = Oxsmt_smtlib_parser.Parser
module Printer = Oxsmt_smtlib.Printer

let failures = ref 0
let checks = ref 0

let fail fmt =
  Printf.ksprintf
    (fun message ->
      incr failures;
      Printf.eprintf "FAIL: %s\n" message)
    fmt
;;

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec loop offset =
    offset + fragment_length <= text_length
    && (String.equal (String.sub text offset fragment_length) fragment || loop (offset + 1)
       )
  in
  fragment_length = 0 || loop 0
;;

let terms_equal left right =
  List.length left = List.length right && List.for_all2 Term.equal left right
;;

let check_roundtrip ~name ?logic ?(fragments = []) source =
  incr checks;
  let env = Env.create () in
  let context = Context.create env in
  match Parser.parse_into env context source with
  | parsed ->
    (match Printer.print_session env parsed.assertions with
     | printed ->
       Option.iter
         (fun expected ->
           if not (contains printed ("(set-logic " ^ expected ^ ")"))
           then fail "%s: expected logic %s in\n%s" name expected printed)
         logic;
       List.iter
         (fun fragment ->
           if not (contains printed fragment)
           then fail "%s: expected %S in\n%s" name fragment printed)
         fragments;
       (match Parser.parse_into env context printed with
        | reparsed ->
          if not (terms_equal parsed.assertions reparsed.assertions)
          then fail "%s: assertions changed across print/reparse\n%s" name printed
        | exception exn ->
          fail
            "%s: printed output did not reparse: %s\n%s"
            name
            (Printexc.to_string exn)
            printed)
     | exception exn -> fail "%s: printing failed: %s" name (Printexc.to_string exn))
  | exception exn -> fail "%s: parsing failed: %s" name (Printexc.to_string exn)
;;

let check_rejected ~name source =
  incr checks;
  match Parser.parse source with
  | _ -> fail "%s: expected malformed/unsupported input to be rejected" name
  | exception (Parser.Malformed _ | Parser.Unsupported _) -> ()
  | exception exn -> fail "%s: unexpected exception %s" name (Printexc.to_string exn)
;;

let check_quantifier_degrades ~name source =
  incr checks;
  match Parser.parse source with
  | parsed when parsed.dropped > 0 -> ()
  | _ -> fail "%s: Real quantifier was not dropped for unknown degradation" name
  | exception Parser.Unsupported _ -> ()
  | exception exn -> fail "%s: unexpected exception %s" name (Printexc.to_string exn)
;;

let check_quantified_body_degrades ~name source =
  incr checks;
  match Parser.parse source with
  | { lemmas = [ lemma ]; ctx; _ } ->
    let qvar = Context.int_const ctx 0 in
    (match lemma.build ~skolem:(fun ~cod:_ ~args:_ -> assert false) [| qvar |] with
     | _ -> fail "%s: quantified Real body was built for E-matching" name
     | exception Parser.Unsupported _ -> ()
     | exception exn -> fail "%s: unexpected exception %s" name (Printexc.to_string exn))
  | _ -> fail "%s: expected one deferred universal lemma" name
  | exception exn ->
    fail "%s: parse failed unexpectedly: %s" name (Printexc.to_string exn)
;;

let () =
  if not (Lra_config.enabled ()) then fail "lra_roundtrip_test must run with OXSMT_LRA=1";
  check_roundtrip
    ~name:"exact-literals-and-strict"
    ~logic:"QF_LRA"
    ~fragments:[ "(/ 7 3)"; "(- (/ 5 7))"; "(/ 3 4)"; "2.0"; "(<=" ]
    "(set-logic QF_LRA)\n\
     (declare-const x Real)\n\
     (assert (< (- (/ 5 7)) x))\n\
     (assert (< x (+ (/ 1 3) 2.0)))\n\
     (assert (= x (- (/ 5 7))))\n\
     (assert (= x (/ (- 3) (- 4))))\n\
     (assert (= x 2.0))\n\
     (assert (= (* (/ 2 3) x 3) (* 2.0 x)))\n";
  check_roundtrip
    ~name:"all-numeral-coercion-contexts"
    ~logic:"QF_UFLRA"
    "(set-logic QF_UFLRA)\n\
     (declare-const x Real)\n\
     (declare-const p Bool)\n\
     (declare-fun f (Real) Real)\n\
     (define-fun k () Real 2)\n\
     (define-fun choose ((b Bool)) Real (ite b 1 2))\n\
     (assert (= (+ x 2) (+ 1 2)))\n\
     (assert (= (ite true 2 x) x))\n\
     (assert (= x (ite p 1 2)))\n\
     (assert (= (f (ite p 1 2)) x))\n\
     (assert (= (choose p) x))\n\
     (assert (= (f 2) k))\n\
     (assert (distinct x 2))\n\
     (assert (= x (as 2 Real)))\n";
  check_roundtrip
    ~name:"uninterpreted-sort-selects-uflra"
    ~logic:"QF_UFLRA"
    "(set-logic QF_UFLRA)\n\
     (declare-sort S 0)\n\
     (declare-const a S)\n\
     (declare-const b S)\n\
     (declare-const x Real)\n\
     (assert (and (= a b) (< x 1)))\n";
  check_roundtrip
    ~name:"beyond-int63"
    ~logic:"QF_LRA"
    ~fragments:[ "123456789012345678901234567890123456789.0" ]
    "(set-logic QF_LRA)\n\
     (declare-const x Real)\n\
     (assert (= x 123456789012345678901234567890123456789.0))\n";
  (* A let-bound Int-sorted [ite] is parsed once; a later use in a Real context sees the
     already-built Int term and cannot re-read its branches, so [coerce_to_sort] must
     widen the [ite] structurally (bottoming out at the Int-const leaves). Nested ites too
     (the QF_LRA/spider_benchmarks shape). *)
  check_roundtrip
    ~name:"let-bound-int-ite-coerced-to-real"
    ~logic:"QF_LRA"
    ~fragments:[ "(ite p 1.0 2.0)"; "(ite p (ite q 3.0 2.0) 1.0)" ]
    "(set-logic QF_UFLRA)\n\
     (declare-const x Real)\n\
     (declare-const p Bool)\n\
     (declare-const q Bool)\n\
     (assert (let ((v (ite p 1 2))) (<= v x)))\n\
     (assert (let ((v (ite p (ite q 3 2) 1))) (= v x)))\n";
  (* Boundary: a genuinely non-constant Int leaf (an Int variable) inside the ite is NOT
     silently widened — still rejected (a real mixed-Int/Real problem). *)
  check_rejected
    ~name:"let-bound-int-var-in-ite-not-coerced"
    "(set-logic QF_UFLRA)\n\
     (declare-const x Real)\n\
     (declare-const n Int)\n\
     (declare-const p Bool)\n\
     (assert (let ((v (ite p n 2))) (<= v x)))\n";
  check_rejected
    ~name:"nonconstant-int-not-coerced"
    "(set-logic QF_UFLRA)\n\
     (declare-const x Real)\n\
     (declare-const n Int)\n\
     (assert (= x n))\n";
  check_rejected
    ~name:"nonconstant-real-division"
    "(set-logic QF_LRA)\n(declare-const x Real)\n(assert (= (/ x 2) 1.0))\n";
  check_rejected
    ~name:"zero-real-denominator"
    "(set-logic QF_LRA)\n(assert (= (/ 1 0) 0.0))\n";
  check_quantifier_degrades
    ~name:"real-binder"
    "(set-logic LRA)\n(assert (forall ((x Real)) (= x x)))\n";
  check_quantified_body_degrades
    ~name:"real-in-quantified-body"
    "(set-logic UFLRA)\n(declare-const x Real)\n(assert (forall ((n Int)) (= x 0)))\n";
  Printf.printf "lra smtlib: %d checks, %d failures\n" !checks !failures;
  if !failures <> 0 then exit 1
;;
