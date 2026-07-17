(** Certified weighted MaxSMT over a {!Session}.

    Hard constraints are the session's active assertions. Each [soft] is a Boolean term
    whose positive [weight] is paid when the term is false. Repeated equal terms are
    distinct soft occurrences, so their weights add.

    The implementation is core-guided: it gives every soft term a fresh Boolean selector,
    asks {!Session.check_sat_assuming} for subset-minimal selector cores, and computes an
    exact minimum-weight hitting set of the cores seen so far. A satisfiable candidate is
    optimal because every model's violated-soft set must hit every sound core, while the
    candidate violates only members of that minimum hitting set. *)

type soft =
  { term : Oxsmt_core.Term.t
  ; weight : Oxsmt_core.Bigint.t
  }

type optimum =
  { cost : Oxsmt_core.Bigint.t
  ; model : Session.model
  ; violated : soft list
  }

type result =
  | Optimal of optimum
  | Hard_unsat
  | Unknown

(** [max_smt ?max_checks session softs] minimizes the total weight of false soft terms
    subject to the active assertions in [session]. Every soft term must be Bool-sorted,
    built in [Session.context session], and have strictly positive weight; invalid inputs
    raise [Invalid_argument] before the session is changed.

    [Optimal o] contains a model that achieves exactly [o.cost]; [o.violated] is the
    input-order list of false soft occurrences in that model. Helper selector bindings are
    removed from the returned model. [Hard_unsat] is returned only after an empty
    assumption core certifies that the active assertions alone are unsatisfiable.

    Any underlying [Unknown], absent model, or malformed core/model evidence returns
    [Unknown], never an uncertified optimum. If [max_checks] is supplied, it is a
    deterministic cap on calls to {!Session.check_sat_assuming}; reaching it also returns
    [Unknown]. A negative cap raises [Invalid_argument]. Without a cap, termination follows
    from finite progress: every nonempty core rules out the current hitting set, of which
    there are at most [2^length softs].

    The optimizer scopes its selector definitions with {!Session.push}/[Session.pop]. The
    fresh nullary selector declarations necessarily remain in the session environment,
    but are not active constraints after this function returns. *)
val max_smt : ?max_checks:int -> Session.t -> soft list -> result

(** Optimization of one linear integer objective over a {!Session}'s active assertions.

    The implementation adds a scoped fresh integer constant equal to the objective, then
    searches its values with exponentially widening bounds followed by bisection. A
    reported optimum has two certificates: the returned model attains [value], and a
    solver query proved the adjacent strict improvement unsatisfiable. *)
module Omt : sig
  type optimum =
    { value : Oxsmt_core.Bigint.t
    ; model : Session.model
    }

  type result =
    | Optimal of optimum
    | Hard_unsat
    | Unbounded
    | Unknown

  (** [minimize ?max_checks session objective] minimizes [objective] subject to the
      active assertions in [session]. [maximize] is the analogous maximization query.
      [objective] must be an affine combination of integer literals and nullary integer
      symbols, built in [Session.context session]. An Int-sorted [Ite] or non-nullary
      application (including [div] and [mod]) is rejected with [Invalid_argument] before
      the session is changed.

      [Optimal { value; model }] is returned only after a model witnesses [objective =
      value] and an [Unsat] query certifies [objective < value] for [minimize], or
      [objective > value] for [maximize]. The fresh objective binding is removed from
      [model]. [Hard_unsat] means the hard assertions were proved unsatisfiable.

      [max_checks] caps calls to {!Session.check_sat}; it defaults to 256, and zero
      returns [Unknown]. A negative cap raises [Invalid_argument]. The finite default
      makes every query terminate, including an objective unbounded in the requested
      direction. The public session API cannot certify an unbounded ray, so this version
      never guesses [Unbounded]: failure to find an infeasible bound within the budget
      returns [Unknown]. Any underlying [Unknown], missing model, or malformed model
      evidence also returns [Unknown].

      The objective definition and every search bound are scoped with
      {!Session.push}/[Session.pop]. The fresh nullary declaration remains in the session
      environment, but no optimizer constraint is active after the call returns. *)
  val minimize : ?max_checks:int -> Session.t -> Oxsmt_core.Term.t -> result

  val maximize : ?max_checks:int -> Session.t -> Oxsmt_core.Term.t -> result
end
