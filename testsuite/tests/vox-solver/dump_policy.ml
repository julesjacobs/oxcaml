(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

(* Dump-mode failure policy (design-docs/vc-generation.md, "Where the pass
   sits"): Dump suppresses exactly the printing backend's expected
   non-verdict — [Ok (Unknown _)] — while a [discharge] error (the shared
   renderer refusing an ill-formed obligation) reports and counts as a
   failure in Dump mode exactly as in Discharge mode.  Synthetic, because
   once the symbol allocator exists, source programs cannot produce an
   ill-formed obligation — which is exactly why the path needs this test to
   stay covered.  The output is the renderer's result plus the four policy
   booleans (each preceded by the failure report the counted cases print). *)

open Vox_logic
open Vox_backend

let () =
  (* Ill-formed: the goal mentions a variable the signature does not
     declare, which the shared renderer refuses. *)
  let ill_formed : Obligation.t =
    { signature = Signature.empty
    ; hypotheses = []
    ; goal = App (Ge, [Var "undeclared"; Const (Int "0")])
    ; location = Location.none
    }
  in
  let outcome = Printing.discharge ~config:Config.default ill_formed in
  (match outcome with
   | Error (Error { cause; _ }) -> Format.printf "renderer: %s@." cause
   | _ -> Format.printf "renderer: unexpected outcome@.");
  let describe failed = if failed then "counted" else "suppressed" in
  let policy ~what ~dump_only outcome =
    Format.printf "%s under %s: %s@." what
      (if dump_only then "Dump" else "Discharge")
      (describe
         (Vox_verify.discharge_outcome ~dump_only ~loc:Location.none
            outcome))
  in
  policy ~what:"renderer error" ~dump_only:true outcome;
  policy ~what:"renderer error" ~dump_only:false outcome;
  let expected_non_verdict : outcome =
    Ok (Unknown (Incomplete "printing backend discharges nothing"))
  in
  policy ~what:"expected non-verdict" ~dump_only:true expected_non_verdict;
  policy ~what:"expected non-verdict" ~dump_only:false expected_non_verdict

[%%expect{|
renderer: ill-formed obligation: undeclared variable undeclared
File "_none_", line 1:
Error: The solver backend failed: ill-formed obligation: undeclared variable undeclared.
renderer error under Dump: counted
File "_none_", line 1:
Error: The solver backend failed: ill-formed obligation: undeclared variable undeclared.
renderer error under Discharge: counted
expected non-verdict under Dump: suppressed
File "_none_", line 1:
Error: This refinement obligation could not be verified (printing backend discharges nothing).
expected non-verdict under Discharge: counted
|}]
