(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Generic fixpoint solver.

    Example system of equations:
      f(0) = min(f(1) + 1, 20)    [initial: 10]
      f(1) = f(0) + 2              [initial: 5]

    The solver iterates all equations until fixpoint f(0) = 20, f(1) = 22 is reached.

    Priorities control the order of evaluation. For example:
      f(0) := f(1)                [initial: 10, HIGH]
      f(1) := f(0)                 [initial: 5, LOW]

    There are two possible fixpoints from these initial values: f(0) = f(1) = 10 and
    f(0) = f(1) = 5. In this example, we first update the HIGH value f(0) with f(1) = 5,
    so we reach fixpoint f(0) = f(1) = 5 deterministically.

    Evaluation strategy (HIGH reset algorithm):
    The solver uses nested loops with a HIGH reset strategy:
    
    Outer loop (continues while LOW priority values change):
      1. HIGH reset and fixpoint:
         - Reset all HIGH priority values to their initial values
         - Iterate only HIGH priority equations until they reach fixpoint
         - During this phase, only HIGH priority values can update
         - LOW priority values remain unchanged from previous iteration
      
      2. Single LOW pass:
         - Perform one iteration where LOW priority values can update
         - HIGH priority values remain at their fixpoint from step 1
         - If any LOW value changes, return to step 1
    
    This HIGH reset strategy ensures that HIGH priority values (concrete types in
    jkind inference) are re-evaluated fresh each outer iteration, preventing them
    from accumulating information across iterations. This is crucial for soundness
    when inferring kinds for mutually recursive abstract and concrete types.

    Examples:

    1. Simple convergence:
       f() := min(f() + 1, 10)     [initial: 0]
       → Converges to fixpoint f() = 10

    2. Mutual recursion with different priorities:
       f(true) := f(false)          [initial: 0, HIGH]
       f(false) := f(true)          [initial: 1, LOW]
       → Both converge to 1 (low priority's initial value wins)

    3. Same system with equal priorities:
       f(true) := f(false)          [initial: 0, HIGH]
       f(false) := f(true)          [initial: 1, HIGH]
       → Both converge to 0 (due to HIGH reset and evaluation order)

    4. Incrementing cycle:
       f(true) := min(f(false) + 1, 10)    [initial: 0]
       f(false) := f(true)                 [initial: 0]
       → Eventually stabilizes at fixpoint f(true) = f(false) = 10

    5. Three-way cycle with mixed priorities:
       f(0) := min(f(1) + 1, 20)   [initial: 0, LOW]
       f(1) := f(2)                [initial: 0, HIGH]
       f(2) := f(0)                [initial: 10, LOW]
       → All converge to 20 (HIGH reset causes gradual increase)

    Key insight: The function f can read any f(k) during computation, but only
    appropriate priorities can update their stored values based on the current phase.

    Usage:

    1. Define your key and value types with equality:
       ```ocaml
       module IntKey = struct
         type t = int
         let equal = (=)
         let hash x = x
       end

       module IntValue = struct
         type t = int
         let equal = (=)
       end
       ```

    2. Create the solver module:
       ```ocaml
       module Solver = Fixpoint_solver.Make(IntKey)(IntValue)
       ```

    3. Define priority and initial values:
       ```ocaml
       let priority_and_init key =
         match key with
         | 0 -> (true, 10)   (* high priority, initial value 10 *)
         | 1 -> (false, 5)   (* low priority, initial value 5 *)
         | _ -> (false, 0)   (* default *)
       ```

    4. Define your recursive function:
       ```ocaml
       let f_def f_rec key =
         match key with
         | 0 -> min (f_rec 1 + 1) 20   (* f(0) = min(f(1) + 1, 20) *)
         | 1 -> f_rec 0                 (* f(1) = f(0) *)
         | _ -> 0
       ```

    5. Create the fixpoint function and use it:
       ```ocaml
       let f = Solver.fix priority_and_init f_def in
       let result = f 0  (* Triggers solving for key 0 and its dependencies *)
       ```

    The solver handles cycles, memoization, and priority-based evaluation automatically.
    Once a key is solved, subsequent calls return the cached fixpoint value immediately.

    Why priorities?
    ~~~~~~~~~~~~~~~
    The solver will be used for computing kinds of mutually recursive abstract and
    concrete types. Consider:

      type t : value mod contended with t = Zero | Succ of t

    This should type check and we should have `t : value mod contended`.
    The `with t` has no effect.
    Now consider:

      type t : value mod contended with t

    This should *not* have `t : value mod contended`. The `with t` *cannot* be ignored
    for abstract types. The reason is that we should be allowed to instantiate `t`
    with any type that satisfies the kind constraint, including `t = int ref`.
    Note that `int ref : value mod contended with int ref`.
    [This was previously a bug in the compiler; found by Benjamin Peters.]

    The same holds in mutual blocks:

      type t : value mod contended with q = Zero | Succ of q
      type q : value mod contended with t

    We should *not* have `q : value mod contended` nor `t : value mod contended`,
    because `q = int ref` does satisfy the kind prescription.

    What does this all got to do with fixpoint solving?
    The answer is that we can compute the kinds of such mutually recursive definitions
    as a fixpoint:

      Start out all concrete types with the best (least restrictive) kind.
      Start out all abstract types with the worst (most restrictive) kind.
      Iterate the "kind equations" until fixpoint.
      i.e. iteratively update the kind of each type, assuming that the other types have
      the kind they currently have in the iteration, until this stabilizes.

    However, with mutually recursive abstract and concrete types, this requires more care
    because the order of iterating the equations matters. In short, we must make the
    concrete types HIGH priority equations, and the abstract types LOW priority equations.

    The solver will now first iterate the HIGH priority equations. What this accomplishes
    is that it pulls down the kinds of concrete types to *below* any fixpoint (because
    the abstract types still have worst kind during the HIGH phase).
    The iteration pulls the concrete types' kinds down from *above* the fixpoint (because
    they were initialized to the best kind), and when the HIGH equations reach fixpoint,
    all concrete types have kinds *below* the fixpoint.

    Now the general iteration can start, updating all equations, which pulls the kinds of
    all types up as much as possible.

    Let's consider another example:

      type t : value mod contended with q = Zero | Succ of q
      type q : value mod contended with (t @@ contended)

    [using as-of-yet non-existent modality syntax]

    In this case, the kinds of `t` and `q` *should* be `value mod contended`.
    Here is how that happens with the algorithm above:

      Initially in the HIGH phase, we have t : value mod all and q : value.
      We iterate t's equation, keeping q : value. Because t contains q,
      we get t :value also.

      Then in the LOW phase, we iterate all equations. Initially, we have:
        t : value
        q : value
      When we iterate the q equation, we get
        t : value
        q : value mod contended with (t @@ contended) == value mod contended
      We now iterate t's equation to get the fixpoint:
        t : value mod contended
        q : value mod contended

    The key is that after the HIGH phase, all concrete types kinds are pessimistic,
    and the abstract types are pessimistic as well, so it is safe to iterate everything.

    Summary
    ~~~~~~~
    The priority-based fixpoint solver with HIGH reset ensures sound kind inference for 
    mutually recursive abstract and concrete types by:

    1. Starting concrete types (HIGH) with optimistic initial values (best kinds)
    2. Starting abstract types (LOW) with pessimistic initial values (worst kinds)
    3. Using a HIGH reset strategy in nested loops:
       - Outer loop continues while LOW values change
       - Each outer iteration resets HIGH values to their initial (optimistic) values
       - HIGH values iterate to fixpoint with current LOW values
       - Single LOW pass updates abstract types based on current HIGH fixpoint
    4. This reset mechanism prevents HIGH priority values from accumulating information
       across iterations, ensuring they are re-evaluated fresh each time

    The HIGH reset approach guarantees that abstract types never become more permissive
    than what could be safely instantiated, while concrete types get the most permissive
    kinds allowed by their definitions, without unsoundly accumulating constraints across
    iterations.

    Solver efficiency
    ~~~~~~~~~~~~~~~~~
    The solver tries to iterate the equations semi-intelligently. In particular, the
    solver uses RPO graph traversal. This can be further improved in future versions,
    by tracking what changed in a fine-grained manner, and only recomputing equations
    whose dependencies changed.
*)

module type S = sig
  type key
  type value

  (** [fix priority_and_init f] computes the fixpoint of function [f].

      - [priority_and_init] is a function that returns a pair (priority, initial_value):
        * priority: true for high priority (evaluated to fixpoint first), false for low priority
        * initial_value: the initial value for this key before iteration begins

      - [f] is the recursive function, which receives a memoized version of itself
        as its first argument. The memoized version should be used for all recursive
        calls to ensure proper fixpoint computation and avoid recomputation.

      When the resulting function is called, the solver iterates values of f until
      fixpoint is reached (when values stop changing according to Value.equal),
      and memoizes the results.
  *)
  val fix : (key -> bool * value) -> ((key -> value) -> (key -> value)) -> (key -> value)
end

(** Functor to create a fixpoint solver for given key and value types *)
module Make (Key : Hashtbl.HashedType) (Value : sig
  type t
  val equal : t -> t -> bool
end) : S with type key = Key.t and type value = Value.t
