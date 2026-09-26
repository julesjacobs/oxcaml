module D = Hm_declarative
module W = Hmc_cfg_states

let[@def] rec (continuation @ total) (k : W.continuation @ immutable) = ghost_ (
  match k with
  | W.Halt _ -> D.Z
  | W.Left (_, _, _, _, _, rest) | W.Right (_, _, _, rest)
  | W.Let_body (_, _, _, _, rest) | W.Scope (_, _, rest)
  | W.Conditional (_, _, _, _, _, rest) | W.List_cases (_, _, _, _, _, _, rest)
  | W.Call_return (_, _, _, _, rest) -> D.S (continuation rest))

let[@def] (state @ total) (state : W.state @ immutable) = ghost_ (
  match state with W.Running (_, k, _) -> continuation k | W.Done _ | W.Stuck -> D.Z)

let[@def] rec (le @ total) (left : D.index @ immutable) (right : D.index @ immutable) = ghost_ (
  match left, right with
  | D.Z, _ -> true | D.S _, D.Z -> false
  | D.S left, D.S right -> le left right)

let rec (reflexive @ total) : (height : D.index) @ immutable ->
    {u : unit | le height height} @ ghost = fun height -> ghost_ (
  le_def height height; match height with D.Z -> () | D.S rest -> reflexive rest)

let rec (grow @ total) : (height : D.index) @ immutable ->
    {u : unit | le height (D.S height)} @ ghost = fun height -> ghost_ (
  le_def height (D.S height); match height with D.Z -> () | D.S rest -> grow rest)

let rec (weaken @ total) : (left : D.index) @ immutable -> (right : D.index) @ immutable ->
    {u : unit | le left right} -> {u : unit | le left (D.S right)} @ ghost =
  fun left right premise -> ghost_ (
    le_def left right; le_def left (D.S right);
    match left, right with D.S a, D.S b -> weaken a b () | _ -> ())

let rec (transitive @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    (c : D.index) @ immutable -> {u : unit | le a b && le b c} -> {u : unit | le a c} @ ghost =
  fun a b c premise -> ghost_ (
    le_def a b; le_def b c; le_def a c;
    match a, b, c with D.S a, D.S b, D.S c -> transitive a b c () | _ -> ())

let rec (add_zero @ total) : (a : D.index) @ immutable -> {u : unit | D.add a D.Z === a} @ ghost =
  fun a -> ghost_ (D.add_def a D.Z; match a with D.Z -> () | D.S rest -> add_zero rest)

let rec (add_successor @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    {u : unit | D.add a (D.S b) === D.S (D.add a b)} @ ghost = fun a b -> ghost_ (
  D.add_def a (D.S b); D.add_def a b; match a with D.Z -> () | D.S rest -> add_successor rest b)

let rec (add_commute @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    {u : unit | D.add a b === D.add b a} @ ghost = fun a b -> ghost_ (
  D.add_def a b;
  match a with D.Z -> add_zero b | D.S rest -> add_commute rest b; add_successor b rest)

let rec (add_associate @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    (c : D.index) @ immutable -> {u : unit | D.add (D.add a b) c === D.add a (D.add b c)} @ ghost =
  fun a b c -> ghost_ (
    D.add_def a b; D.add_def a (D.add b c); D.add_def (D.add a b) c;
    match a with D.Z -> () | D.S rest -> add_associate rest b c)

let rec (add_monotone @ total) : (prefix : D.index) @ immutable -> (a : D.index) @ immutable ->
    (b : D.index) @ immutable -> {u : unit | le a b} ->
    {u : unit | le (D.add prefix a) (D.add prefix b)} @ ghost = fun prefix a b premise -> ghost_ (
  D.add_def prefix a; D.add_def prefix b; le_def (D.add prefix a) (D.add prefix b);
  match prefix with D.Z -> () | D.S rest -> add_monotone rest a b ())

let (normalized_work @ total) : (cost : D.index) @ immutable -> (before : D.index) @ immutable ->
    (middle : D.index) @ immutable -> (after : D.index) @ immutable ->
    {u : unit | D.add cost middle === before && le after (D.S middle)} ->
    {u : unit | le (D.add (D.add cost (D.S D.Z)) after) (D.S (D.S before))} @ ghost =
  fun cost before middle after premise -> ghost_ (
    add_monotone cost after (D.S middle) ();
    add_successor cost middle; add_successor cost after;
    add_associate cost (D.S D.Z) after;
    D.add_def (D.S D.Z) after; D.add_def D.Z after;
    le_def (D.add (D.add cost (D.S D.Z)) after) (D.S (D.S before)))

let[@def] rec (twice @ total) (fuel : D.index @ immutable) = ghost_ (
  match fuel with D.Z -> D.Z | D.S rest -> D.S (D.S (twice rest)))

let (compose_work @ total) : (first : D.index) @ immutable -> (second : D.index) @ immutable ->
    (before : D.index) @ immutable -> (middle : D.index) @ immutable -> (after : D.index) @ immutable ->
    (allowance : D.index) @ immutable ->
    {u : unit | le (D.add first middle) (D.S (D.S before))
      && le (D.add second after) (D.add allowance middle)} ->
    {u : unit | le (D.add (D.add first second) after) (D.S (D.S (D.add allowance before)))} @ ghost =
  fun first second before middle after allowance premise -> ghost_ (
    add_monotone first (D.add second after) (D.add allowance middle) ();
    add_associate first second after;
    add_associate first allowance middle; add_commute first allowance;
    add_associate allowance first middle;
    add_monotone allowance (D.add first middle) (D.S (D.S before)) ();
    add_successor allowance before; add_successor allowance (D.S before);
    transitive (D.add (D.add first second) after) (D.add allowance (D.add first middle))
      (D.S (D.S (D.add allowance before))) ())

let rec (prefix_le @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    {u : unit | le a (D.add a b)} @ ghost = fun a b -> ghost_ (
  D.add_def a b; le_def a (D.add a b);
  match a with D.Z -> () | D.S rest -> prefix_le rest b)
