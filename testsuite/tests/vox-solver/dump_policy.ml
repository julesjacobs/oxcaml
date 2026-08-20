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
   stay covered. *)

open Vox_logic
open Vox_backend

(* Ill-formed: the goal mentions a variable the signature does not
   declare, which the shared renderer refuses. *)
let ill_formed : Obligation.t =
  { signature = Signature.empty
  ; hypotheses = []
  ; goal = App (Ge, [Var "undeclared"; Const (Int "0")])
  ; location = Location.none
  }

let outcome = Printing.discharge ~config:Config.default ill_formed

let describe failed = if failed then "counted as failure" else "suppressed"

[%%expect{|
val ill_formed : Vox_logic.Obligation.t =
  {Obligation.signature =
    {Signature.sorts = []; datatypes = []; variables = []; functions = []};
   hypotheses = [];
   goal =
    Term.App (Op.Ge, [Term.Var "undeclared"; Term.Const (Literal.Int "0")]);
   location =
    {Location.loc_start =
      {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1};
     loc_end =
      {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1};
     loc_ghost = true}}
val outcome : Vox_backend.outcome =
  Error
   (Error
     {Vox_backend.cause = "ill-formed obligation";
      raw = "undeclared variable undeclared"})
val describe : bool -> string = <fun>
|}]

(* A renderer error reports (the located report prints as found) and
   counts, in both modes. *)

let () =
  Format.printf "%s@." (describe
       (Vox_verify.discharge_outcome ~dump_only:true ~loc:Location.none
          outcome))

[%%expect{|
File "_none_", line 1:
Error: The solver backend failed: ill-formed obligation.
counted as failure
|}]

let () =
  Format.printf "%s@." (describe
       (Vox_verify.discharge_outcome ~dump_only:false ~loc:Location.none
          outcome))

[%%expect{|
File "_none_", line 1:
Error: The solver backend failed: ill-formed obligation.
counted as failure
|}]

(* The printing backend's expected non-verdict is suppressed under Dump and
   only under Dump. *)

let expected_non_verdict : outcome =
  Ok (Unknown (Incomplete "printing backend discharges nothing"))

let () =
  Format.printf "%s@." (describe
       (Vox_verify.discharge_outcome ~dump_only:true ~loc:Location.none
          expected_non_verdict))

[%%expect{|
val expected_non_verdict : Vox_backend.outcome =
  Ok (Unknown (Incomplete "printing backend discharges nothing"))
suppressed
|}]

let () =
  Format.printf "%s@." (describe
       (Vox_verify.discharge_outcome ~dump_only:false ~loc:Location.none
          expected_non_verdict))

[%%expect{|
File "_none_", line 1:
Error: This refinement obligation could not be verified (printing backend discharges nothing).
counted as failure
|}]
