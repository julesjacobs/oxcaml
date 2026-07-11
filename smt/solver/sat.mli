(** Propositional CDCL SAT core — MiniSat design (Eén–Sörensson, "An Extensible
    SAT-solver", SAT 2003), deliberately novelty-free (DESIGN.md §5).

    Two-watched-literal propagation, 1UIP conflict analysis with clause learning and local
    (self-subsumption) minimization, VSIDS activity branching with exponential decay,
    phase saving, Luby restarts, and activity-based learned- clause deletion.
    Propositional only: it sees no theories and no terms. The clausifier (smt/preprocess)
    is its sole intended client and talks to it only through this interface.

    {b Determinism (I6).} No wall-clock and no randomness: restart and activity schedules
    are purely count-based, so two identical runs produce identical verdicts, models, and
    stats.

    {b Incrementality.} [add_clause] is permanent and may be called between [solve]s.
    [solve] takes optional assumptions; a fresh assumption set each call. Learned clauses
    persist across [solve]s.

    {b Encoding.} Variables are consecutive nonnegative ints from 0. A literal packs a
    variable and a polarity as [2*v] (positive) / [2*v+1] (negative); treat [lit] as
    opaque and build it with {!pos}/{!neg}. *)

type t
type var = int

(** A signed variable. Opaque; build with {!pos}/{!neg}, negate with {!neg_lit}. *)
type lit = private int

val pos : var -> lit
val neg : var -> lit
val neg_lit : lit -> lit
val var_of_lit : lit -> var

(** [true] when the literal is positive (i.e. built by {!pos}). *)
val sign_of_lit : lit -> bool

type result =
  | Sat
  | Unsat

(** A fresh solver with no variables and no clauses. *)
val create : unit -> t

(** Allocate and return the next variable. Variables are also auto-allocated on demand by
    {!add_clause} and {!solve} when a literal names one not yet created, so explicit calls
    are optional. *)
val new_var : t -> var

(** Number of variables allocated so far. *)
val num_vars : t -> int

(** Add a permanent clause (disjunction of literals). Legal between [solve]s. Level-0
    simplification (tautology/duplicate/falsified-literal removal, unit propagation)
    happens here; an empty clause makes the instance permanently unsat. *)
val add_clause : t -> lit list -> unit

(** [solve ?assumptions t] decides satisfiability under the given unit assumptions
    (default none). After [Sat], query the model with {!value}. After [Unsat] with
    assumptions, {!failed_assumptions} gives an unsat subset of them. *)
val solve : ?assumptions:lit list -> t -> result

(** Truth value of a variable in the model of the most recent [Sat]. Unspecified before
    any [Sat]. *)
val value : t -> var -> bool

(** The model of the most recent [Sat] as a [var]-indexed array (element [v] is
    {!value}[ t v]). Empty before any [Sat]. *)
val model : t -> bool array

(** A subset of the assumptions that is jointly unsatisfiable, valid after [solve]
    returned [Unsat] with a nonempty assumption set (the failed- assumption core, §7).
    Empty otherwise. *)
val failed_assumptions : t -> lit list

module Stats : sig
  (** The harness counter trio (DESIGN.md §8). Monotonic across [solve]s. *)
  type t =
    { conflicts : int
    ; decisions : int
    ; propagations : int
    }
end

val stats : t -> Stats.t

(** {2 Proof-readiness hook (§7)}

    A compile-out-able trace of the resolution derivation of each learned clause:
    [on_learned] fires once per learned clause with a fresh clause [id], the learned
    [clause] (asserting literal at index 0), the ids of the [antecedents] resolved to
    derive it (the initial conflict clause followed by the reason clauses visited during
    1UIP analysis), and the [btlevel] the solver then backjumps to. Zero cost when no
    trace is set — antecedents are not even accumulated. This is the seam for DRAT-style
    logging later (serialization, not redesign). *)
type trace =
  { on_learned : id:int -> clause:lit array -> antecedents:int list -> btlevel:int -> unit
  }

val set_trace : t -> trace option -> unit
