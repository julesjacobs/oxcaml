(** Operations have a work limit; managers limit retained unique-table entries.
*)
exception Limit

module For_testing : sig
  val with_node_limit : int -> (unit -> 'a) -> 'a
end

type manager

type node

(** [bit_order] assigns distinct ranks to each variable's index bits, most
    significant first. Ranks [2*k] and [2*k+1] form one coordinate; both bits of
    a variable precede the next variable in that coordinate. It affects diagram
    size only. *)
val create : ?bit_order:int array array -> int array -> manager

val arity : manager -> int

val cardinality : manager -> int -> int

val true_ : node

val false_ : node

val is_false : node -> bool

val mk : manager -> int -> node array -> node

val and_ : manager -> node -> node -> node

val or_ : manager -> node -> node -> node

val not_ : manager -> node -> node

val exists : manager -> int -> node -> node

val forall : manager -> int -> node -> node

val exists_many : manager -> int list -> node -> node

(** Existentially quantify the conjunction without materializing it first. *)
val and_exists : manager -> int list -> node -> node -> node

(** Universally quantify the implication without materializing it first. *)
val implies_forall : manager -> int list -> node -> node -> node

val restrict : manager -> int -> int -> node -> node

(** A mapping entry of [-1] removes a variable absent from the imported diagram
    or substituted by its corresponding [fixed] value. Fixed variables must map
    to [-1]; other mapped variables preserve their cardinalities. *)
val import :
  ?fixed:int option array ->
  manager ->
  old_manager:manager ->
  old_to_new:int array ->
  node ->
  node

val find_sat : ?prefer_high:bool -> manager -> node -> int array option

(** Clear operation caches and retain the canonical nodes reachable from
    [roots]. With a positive [threshold], compact only after that many
    additional entries have accumulated since the last compaction. Older nodes
    remain valid; node identities are never reused. *)
val compact : ?threshold:int -> manager -> node list -> unit

(** Test implication without constructing a result diagram. *)
val entails : manager -> node -> node -> bool

val support : manager -> node list -> bool array

(** Find an assignment satisfying the left relation and falsifying the right. *)
val counterexample : manager -> node -> node -> int array option

(** Conjoin all relations without materializing prefix conjunctions. *)
val and_many : manager -> node list -> node

(** Extract shared conjuncts from a diagram without changing its models. *)
val factor : manager -> node -> node

(** Project the domain and retain winning assignments within that projection.
    Existential winning is [exists vars. domain && witness]. Universal winning
    is [(exists vars. domain) && forall vars. (domain -> witness)]. *)
val eliminate_guarded :
  manager ->
  universal:bool ->
  int list ->
  domain:node ->
  witness:node ->
  node * node
