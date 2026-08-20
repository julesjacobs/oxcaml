(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Solver backends: how an obligation gets discharged. *)

module Config : sig
  type t =
    { timeout_seconds : float option
          (** budget per solver query — an obligation may take two queries,
              so up to twice this long; [None] means no limit *)
    ; z3_command : string option
          (** how to run z3, e.g. ["z3"] or an absolute path; [None] means
              no solver is configured *)
    }

  (** Ten seconds, no solver configured. *)
  val default : t
end

type reason =
  | Timeout
  | Incomplete of string

(** Counterexample values for the obligation's variables, term-valued so a
    datatype counterexample can carry its constructor tree.  Best-effort:
    variables whose value the backend cannot read back are absent. *)
type model = (string * Vox_logic.Term.t) list

type verdict =
  | Proved of { unused_hypotheses : int list option }
      (** The goal holds in every state satisfying the hypotheses.
          [unused_hypotheses = None] means the backend makes no claim; it
          never means "all hypotheses were used".  The conservative
          direction is forced: an unsat core need not be minimal, so an
          unused hypothesis may be missed but never invented. *)
  | Refuted of model option
      (** The goal is {e false} in every state satisfying the hypotheses:
          the {!Vox_smtlib.Disprove} query returned [unsat].  A mere model
          of the [Prove] query does not refute (see {!Vox_smtlib.query});
          such a model is at most payload for [Unknown]. *)
  | Unknown of reason

(** Failure to produce a verdict is not a verdict.  Putting these beside
    [Proved] in one type is how vox2 reported a solver defect to a user as
    "the solver could not be run". *)
type failure =
  | Unavailable of string
  | Error of
      { cause : string  (** one line *)
      ; raw : string  (** everything the solver said *)
      }

type outcome = (verdict, failure) result

module type BACKEND = sig
  val name : string

  (** Whether [discharge] can be expected to work; checked once at
      selection, not per obligation. *)
  val configured : config:Config.t -> (unit, string) result

  val discharge : config:Config.t -> Vox_logic.Obligation.t -> outcome
end

(** Prints the {!Vox_smtlib.Prove} query to standard output — byte-for-byte
    what the z3 backend would send under the same config — and discharges
    nothing: every obligation is [Unknown]. *)
module Printing : BACKEND

module Z3 : BACKEND

val backends : (module BACKEND) list

(** Look up a backend by name.  The error message lists the valid names. *)
val select : string -> ((module BACKEND), string) result

(** How the driver finds z3 when [-vox-z3] is absent, mirroring the test
    gate (testsuite/tests/vox-solver/has_z3.sh): [$VOX_TEST_Z3] if set,
    else [z3] on [PATH], else the pinned install the gate names — same
    checks, same order, so a gate skip decision and a driver run decision
    can never disagree.  [None] when none of the three is available. *)
val resolve_z3 : unit -> string option

(** Driver policy for [-vox-backend].  ["none"] is not a backend: a backend
    answering every obligation with [Unknown] would report every obligation
    as unproved, and one answering [Unavailable] would report every unit as
    failing.  Typecheck-only is the caller's decision to not discharge, made
    here, before any backend is consulted.

    For any other name, selects the backend and checks [configured], so an
    unusable configuration fails once, with one message, rather than once
    per obligation.

    Whether a non-verdict refuses the unit is also driver policy, decided
    here: the printing backend's contract is to emit the query and
    discharge nothing, so it gets the [Dump] arm, under which only its
    expected non-verdict — [Ok (Unknown _)] — is suppressed.  A
    [discharge] error still refuses the unit in [Dump] exactly as in
    [Discharge]. *)
type plan =
  | No_discharge
  | Dump of (module BACKEND)  (** printing: emit, expect no verdicts *)
  | Discharge of (module BACKEND)  (** z3 and future backends: verify *)

val plan : backend_name:string -> config:Config.t -> (plan, string) result
