(** The solver side of a Vox query session, independent of how the text reaches
    the solver: a Z3 process (see {!Vox_smt_solver}) or Z3 compiled to
    WebAssembly (see [playground/]). A query is sent as [session_input text];
    after the first response line, [answer], the session sends
    [followup symbols answer ^ resource_request] and passes everything the
    solver printed after [answer] to [interpret_response]. *)

(** A malformed or unexpected solver response. *)
exception Protocol_error of string

(** [protocol message] raises [Protocol_error message]. *)
val protocol : string -> 'a

(** [session_input text] resets the solver and then runs the query [text] (the
    output of {!Vox_smt.to_smtlib}) under the logic [ALL]. *)
val session_input : string -> string

(** The request for a countermodel after ["sat"] or a reason after ["unknown"];
    after ["unsat"], the unsat core for a query checked with [assumptions] (see
    {!Vox_smt.to_smtlib}), else nothing. Raises [Protocol_error] for any other
    answer. *)
val followup :
  ?assumptions:Vox_smt.Symbol.t list ->
  Vox_smt.Symbol.t list ->
  string ->
  string

(** Asks for the resource count of the query since the reset. *)
val resource_request : string

(** The validity from the answer and the responses to [followup] and
    [resource_request]. The resource count, when the solver reports one, is
    stored in [resources] first, even if interpreting the rest raises
    [Protocol_error]. With [assumptions] (as given to [followup]), the unsat
    core of a proof, the assumptions it used, is stored in [core]. *)
val interpret_response :
  resources:int option ref ->
  ?assumptions:Vox_smt.Symbol.t list ->
  ?core:Vox_smt.Symbol.t list option ref ->
  Vox_smt.Symbol.t list ->
  string ->
  string ->
  Vox_smt.validity
