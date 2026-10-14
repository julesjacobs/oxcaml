module Z = Vox_smt_integer.Z
module Q = Vox_smt_integer.Q

type t

type 'a solution =
  { model : (int * Q.t) list;
    split : (int * Z.t) option;
    split_form : ((int * Z.t) list * Z.t) option;
    cut : ((int * Z.t) list * Z.t * int array) option;
    propagations : (int * 'a) list
  }

type 'a check_result =
  | Conflict of int array | Consistent of 'a solution | Unknown

type consistent = int array solution
type lazy_consistent = int array Lazy.t solution
type result = int array check_result
type lazy_result = int array Lazy.t check_result

type phase = Propagation | Final
type cut_basis = Tableau | Reduced | Combined

val create : unit -> t

(** Register [literal <=> sum (coefficient * variable) <= bound]. Variables
    are stable integer IDs. Registration survives backtracking. Coefficients
    and bounds are integers; a negative literal asserts the integer complement.
    Reusing a literal for another atom is rejected. *)
val register :
  ?poll:(unit -> unit) -> t -> literal:int -> (int * Z.t) list -> Z.t -> unit

val assert_literal : t -> int -> unit
val push : t -> unit
val pop : t -> unit
val level : t -> int
val backtrack : t -> int -> unit
val reset : t -> unit
(** Remove every assertion, including root assertions, retaining registered
    atoms and tableau definitions. *)

(** Explanations contain asserted antecedent literals. A split [(x, n)] asks
    the Boolean solver to add [x <= n or x >= n + 1]. A missing split means
    that all original variables have integral values. Propagations concern
    registered atoms. A cut [(terms, bound, reasons)] states that [reasons]
    imply [sum (coefficient * variable) <= bound]. Its variable IDs are the
    original IDs passed to [register], and its coefficients and bound are
    integers. [split_form] optionally requests a split along an integer
    linear form using the same coefficient representation. Its bound is the
    floor of that form's value, so the two branches are [form <= bound] and
    [form >= bound + 1]. It is returned only with [split_forms = true].
    The cut excludes the current rational model and remains valid
    after backtracking when guarded by its reasons. Cuts are returned only
    for [Final] checks with [cuts = true], which defaults to [false].
    Unknown leaves the solver usable after interruption.
    [budget] counts arithmetic work and tableau steps, rather than time.
    [Propagation] checks rational feasibility and derives bound implications;
    its model is empty and its split is absent. [Final], the default, also
    checks integer consistency and produces the model or an integer split. *)
val check :
  ?cut_basis:cut_basis -> ?split_forms:bool -> ?optimize:bool ->
  ?model_search:bool -> ?phase:phase -> ?cuts:bool ->
  ?budget:int -> ?cancelled:(unit -> bool) ->
  ?charge:(unit -> bool) -> t -> result

val value : t -> int -> Q.t option

(** Like [check], retaining propagation explanations without materializing
    their arrays. Each explanation captures immutable antecedents and can be
    forced after later checks, backtracking or reset. Its antecedents were
    asserted when the propagation was derived. *)
val check_lazy :
  ?cut_basis:cut_basis -> ?split_forms:bool -> ?optimize:bool ->
  ?model_search:bool -> ?phase:phase -> ?cuts:bool ->
  ?budget:int -> ?cancelled:(unit -> bool) ->
  ?charge:(unit -> bool) -> t -> lazy_result

(** Visit original variables in the proposed split's current tableau row.
    For a nonbasic variable, visit the rows that contain it. This is a demand
    hint only; each consequence still requires an independent explanation. *)
val iter_split_support : t -> int -> f:(int -> unit) -> unit

val is_fixed : t -> int -> bool

(** Number of original variable IDs registered in arithmetic atoms. *)
val variable_count : t -> int
