type trace =
  | Failed
  | Variable_use of Copy_spec.node Pref.t
  | Boolean_literal of Copy_spec.node Pref.t
  | False_literal of Copy_spec.node Pref.t
  | Empty_list_literal of Copy_spec.node Pref.t
  | Word_literal of Hmc_word64.t * Copy_spec.node Pref.t
  | Abstraction of Copy_spec.node Pref.t * Copy_spec.node Pref.t * trace
  | List_constructor of Copy_spec.node Pref.t option * trace * trace
  | Application of Copy_spec.node Pref.t option * trace * trace
  | Conditional of trace
  | List_case of trace
  | Primitive of Hm_declarative.word_operation * Copy_spec.node Pref.t option * trace
  | Recursion of Copy_spec.node Pref.t option * Copy_spec.node Pref.t * Copy_spec.node Pref.t * trace
  | Let_binding of trace * trace
[@@inductive]

let[@def] rec (root @ total) (trace : trace @ immutable) =
  match trace with
  | Failed -> None
  | Variable_use p | Boolean_literal p | False_literal p | Empty_list_literal p | Word_literal (_, p) | Abstraction (p, _, _) -> Some p
  | Application (p, _, _) | List_constructor (p, _, _) | Recursion (p, _, _, _) -> p
  | Primitive (_, p, _) -> p
  | Conditional body | List_case body -> root body
  | Let_binding (_, body) -> root body

let[@def] (option_contains @ total) (p : Copy_spec.node Pref.t @ immutable)
    (root : Copy_spec.node Pref.t option @ immutable) = ghost_ (
  match root with None -> false | Some q -> p === q)

let[@def] rec (contains @ total) (p : Copy_spec.node Pref.t @ immutable) (trace : trace @ immutable) = ghost_ (
  match trace with
  | Failed -> false
  | Variable_use q | Boolean_literal q | False_literal q | Empty_list_literal q | Word_literal (_, q) -> p === q
  | Abstraction (q, a, body) -> p === q || p === a || contains p body
  | Application (q, left, right) | List_constructor (q, left, right) -> option_contains p q || contains p left || contains p right
  | Recursion (q, a, b, body) -> option_contains p q || p === a || p === b || contains p body
  | Conditional body | List_case body -> contains p body
  | Primitive (_, q, body) -> option_contains p q || contains p body
  | Let_binding (rhs, body) -> contains p rhs || contains p body)

let rec (root_contains @ total) : (trace : trace) @ immutable -> (p : Copy_spec.node Pref.t) @ immutable ->
    {u : unit | root trace === Some p} -> {u : unit | contains p trace} @ ghost =
  fun trace p premise -> ghost_ (
    root_def trace; contains_def p trace;
    match trace with
    | Application (q, _, _) | List_constructor (q, _, _) | Recursion (q, _, _, _)
    | Primitive (_, q, _) -> option_contains_def p q
    | Conditional body | List_case body | Let_binding (_, body) -> root_contains body p ()
    | _ -> ())

let[@def] rec (subtrace @ total) (child : trace @ immutable) (parent : trace @ immutable) = ghost_ (
  child === parent || match parent with
  | Abstraction (_, _, body) | Recursion (_, _, _, body) | Conditional body | List_case body | Primitive (_, _, body) -> subtrace child body
  | Application (_, left, right) | List_constructor (_, left, right) | Let_binding (left, right) -> subtrace child left || subtrace child right
  | _ -> false)

let (subtrace_refl @ total) : (trace : trace) @ immutable -> {u : unit | subtrace trace trace} @ ghost =
  fun trace -> ghost_ (subtrace_def trace trace)

let rec (subtrace_trans @ total) : (child : trace) @ immutable -> (middle : trace) @ immutable -> (parent : trace) @ immutable ->
    {u : unit | subtrace child middle && subtrace middle parent} -> {u : unit | subtrace child parent} @ ghost =
  fun child middle parent premise -> ghost_ (
    subtrace_def child parent; subtrace_def middle parent;
    if middle === parent then () else match parent with
    | Abstraction (_, _, body) | Recursion (_, _, _, body) | Conditional body | List_case body | Primitive (_, _, body) ->
      subtrace_trans child middle body ()
    | Application (_, left, right) | List_constructor (_, left, right) | Let_binding (left, right) ->
      if subtrace middle left then subtrace_trans child middle left () else subtrace_trans child middle right ()
    | _ -> ())

let (children @ total) : (parent : trace) @ immutable -> (all : trace) @ immutable ->
    {u : unit | subtrace parent all} ->
    {u : unit | match parent with
      | Abstraction (_, _, body) | Recursion (_, _, _, body) | Conditional body | List_case body | Primitive (_, _, body) -> subtrace body all
      | Application (_, left, right) | List_constructor (_, left, right) | Let_binding (left, right) -> subtrace left all && subtrace right all
      | _ -> true} @ ghost =
  fun parent all premise -> ghost_ (
    match parent with
    | Abstraction (_, _, body) | Recursion (_, _, _, body) | Conditional body | List_case body | Primitive (_, _, body) ->
      subtrace_refl body; subtrace_def body parent; subtrace_trans body parent all ()
    | Application (_, left, right) | List_constructor (_, left, right) | Let_binding (left, right) ->
      subtrace_refl left; subtrace_refl right; subtrace_def left parent; subtrace_def right parent;
      subtrace_trans left parent all (); subtrace_trans right parent all ()
    | _ -> ())

let rec (subtrace_contains @ total) : (p : Copy_spec.node Pref.t) @ immutable -> (child : trace) @ immutable -> (parent : trace) @ immutable ->
    {u : unit | contains p child && subtrace child parent} -> {u : unit | contains p parent} @ ghost =
  fun p child parent premise -> ghost_ (
    subtrace_def child parent; contains_def p parent;
    if child === parent then () else match parent with
    | Abstraction (_, _, body) | Recursion (_, _, _, body) | Conditional body | List_case body | Primitive (_, _, body) ->
      subtrace_contains p child body ()
    | Application (_, left, right) | List_constructor (_, left, right) | Let_binding (left, right) ->
      if subtrace child left then subtrace_contains p child left () else subtrace_contains p child right ()
    | _ -> ())
