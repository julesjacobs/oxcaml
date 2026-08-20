(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

(* Dump-mode driver policy, stated where the [plan] type lives: the
   printing backend's contract is to emit the query and discharge nothing,
   so [plan] gives it the [Dump] arm, under which a driver suppresses
   exactly its expected non-verdict — [Ok (Unknown _)] — while a
   [discharge] error (the shared renderer refusing an ill-formed
   obligation) still refuses the unit.  This test pins the two outcomes
   that policy discriminates: a well-formed obligation yields the expected
   non-verdict, and an ill-formed one yields an [Error] whose cause
   carries the renderer's own reason.  The ill-formed case is synthetic,
   because source programs cannot produce an ill-formed obligation — which
   is exactly why the path needs a test to stay covered. *)

open Vox_logic
open Vox_backend

let config = { Config.default with timeout_seconds = None }

let describe = function
  | Ok (Proved _) -> "proved"
  | Ok (Refuted _) -> "refuted"
  | Ok (Unknown Timeout) -> "unknown (timeout)"
  | Ok (Unknown (Incomplete reason)) -> "expected non-verdict: " ^ reason
  | Result.Error (Unavailable message) -> "unavailable: " ^ message
  | Result.Error (Error { cause; raw = _ }) -> "error: " ^ cause

[%%expect{|
val config : Vox_backend.Config.t =
  {Config.timeout_seconds = None; z3_command = None}
val describe : (Vox_backend.verdict, Vox_backend.failure) Result.t -> string =
  <fun>
|}]

(* [plan] sends the printing backend down the [Dump] arm. *)

let () =
  match plan ~backend_name:"printing" ~config with
  | Ok (Dump (module Backend)) ->
    Format.printf "plan: dump with %s@." Backend.name
  | Ok (Discharge (module Backend)) ->
    Format.printf "plan: discharge with %s@." Backend.name
  | Ok No_discharge -> Format.printf "plan: not discharged@."
  | Result.Error message -> Format.printf "plan: selection failed: %s@." message

[%%expect{|
plan: dump with printing
|}]

(* Ill-formed: the goal mentions a variable the signature does not declare,
   which the shared renderer refuses — an [Error], not a non-verdict, so a
   [Dump] driver still refuses the unit. *)

let () =
  let ill_formed : Obligation.t =
    { signature = Signature.empty
    ; hypotheses = []
    ; goal = App (Ge, [Var "undeclared"; Const (Int "0")])
    ; location = Location.none
    }
  in
  Format.printf "%s@." (describe (Printing.discharge ~config ill_formed))

[%%expect{|
error: ill-formed obligation: undeclared variable undeclared
|}]

(* Well-formed: the query is emitted and the outcome is the expected
   non-verdict that a [Dump] driver suppresses. *)

let () =
  let trivial : Obligation.t =
    { signature = Signature.empty
    ; hypotheses = []
    ; goal = Const (Bool true)
    ; location = Location.none
    }
  in
  Format.printf "%s@." (describe (Printing.discharge ~config trivial))

[%%expect{|
(set-option :produce-unsat-cores true)
(assert (not true))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
expected non-verdict: printing backend discharges nothing
|}]
