module K = Vox_egraph_key
module L = Vox_egraph_language_spec
module LP = Vox_egraph_language_proof
module S = Vox_egraph_rule_semantics
module I = Vox_iarray
module U = Vox_egraph_union_spec

type t = Vox_egraph_match_spec.node =
  | Int_lit of int
  | Bool_lit of bool
  | Int_input
  | Bool_input
  | Add of int * int
  | Eq_int of int * int
  | Int_if of int * int * int
  | Bool_if of int * int * int
[@@inductive]

let[@def] (key @ total) (node : t @ immutable) : K.t =
  match node with
  | Int_lit value ->
    {tag = 0; payload = value; first = 0; second = 0; third = 0}
  | Bool_lit value ->
    {tag = 1; payload = (if value then 1 else 0);
      first = 0; second = 0; third = 0}
  | Int_input ->
    {tag = 2; payload = 0; first = 0; second = 0; third = 0}
  | Bool_input ->
    {tag = 3; payload = 0; first = 0; second = 0; third = 0}
  | Add (first, second) ->
    {tag = 4; payload = 0; first; second; third = 0}
  | Eq_int (first, second) ->
    {tag = 5; payload = 0; first; second; third = 0}
  | Int_if (first, second, third) ->
    {tag = 6; payload = 0; first; second; third}
  | Bool_if (first, second, third) ->
    {tag = 7; payload = 0; first; second; third}

let[@def] (sort @ total) (node : t @ immutable) =
  match node with
  | Int_lit _ | Int_input | Add _ | Int_if _ -> L.Integer
  | Bool_lit _ | Bool_input | Eq_int _ | Bool_if _ -> L.Boolean

let[@def] (canonical @ total) (parents : int iarray @ immutable)
    (node : t @ immutable) =
  match node with
  | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> node
  | Add (a, b) -> Add (U.root parents a, U.root parents b)
  | Eq_int (a, b) -> Eq_int (U.root parents a, U.root parents b)
  | Int_if (c, a, b) ->
    Int_if (U.root parents c, U.root parents a, U.root parents b)
  | Bool_if (c, a, b) ->
    Bool_if (U.root parents c, U.root parents a, U.root parents b)

let[@def] (signature @ total) (parents : int iarray @ immutable)
    (node : t @ immutable) = key (canonical parents node)

let[@def] (origin @ total) (origins : L.expr iarray @ immutable)
    (node : t @ immutable) =
  match node with
  | Int_lit value -> L.Int_lit value
  | Bool_lit value -> L.Bool_lit value
  | Int_input -> L.Int_input
  | Bool_input -> L.Bool_input
  | Add (a, b) -> L.Add (S.origin origins a, S.origin origins b)
  | Eq_int (a, b) -> L.Eq_int (S.origin origins a, S.origin origins b)
  | Int_if (c, a, b) ->
    L.Int_if (S.origin origins c, S.origin origins a,
      S.origin origins b)
  | Bool_if (c, a, b) ->
    L.Bool_if (S.origin origins c, S.origin origins a,
      S.origin origins b)

let[@def] (child_has_sort @ total)
    (sorts : L.sort iarray @ immutable) (count : int)
    (id : int) (expected : L.sort) =
  0 <= id && id < count &&
  (match I.at sorts id, expected with
   | Some L.Integer, L.Integer | Some L.Boolean, L.Boolean -> true
   | _ -> false)

let[@def] (well_typed @ total)
    (sorts : L.sort iarray @ immutable) (count : int)
    (node : t @ immutable) =
  match node with
  | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> true
  | Add (a, b) | Eq_int (a, b) ->
    child_has_sort sorts count a L.Integer &&
    child_has_sort sorts count b L.Integer
  | Int_if (c, a, b) ->
    child_has_sort sorts count c L.Boolean &&
    child_has_sort sorts count a L.Integer &&
    child_has_sort sorts count b L.Integer
  | Bool_if (c, a, b) ->
    child_has_sort sorts count c L.Boolean &&
    child_has_sort sorts count a L.Boolean &&
    child_has_sort sorts count b L.Boolean

let[@def] (child_origins_typed @ total)
    (origins : L.expr iarray @ immutable)
    (node : t @ immutable) = ghost_ (
  match node with
  | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> true
  | Add (a, b) | Eq_int (a, b) ->
    L.sort (S.origin origins a) === Some L.Integer &&
    L.sort (S.origin origins b) === Some L.Integer
  | Int_if (c, a, b) ->
    L.sort (S.origin origins c) === Some L.Boolean &&
    L.sort (S.origin origins a) === Some L.Integer &&
    L.sort (S.origin origins b) === Some L.Integer
  | Bool_if (c, a, b) ->
    L.sort (S.origin origins c) === Some L.Boolean &&
    L.sort (S.origin origins a) === Some L.Boolean &&
    L.sort (S.origin origins b) === Some L.Boolean)

let[@def] (children_below @ total) (limit : int)
    (node : t @ immutable) =
  match node with
  | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> true
  | Add (a, b) | Eq_int (a, b) ->
    0 <= a && a < limit && 0 <= b && b < limit
  | Int_if (c, a, b) | Bool_if (c, a, b) ->
    0 <= c && c < limit && 0 <= a && a < limit &&
    0 <= b && b < limit

let (well_typed_children @ total) :
    (sorts : L.sort iarray) @ immutable ->
    (count : int) -> (node : t) @ immutable ->
    {u : unit | well_typed sorts count node} ->
    {u : unit | children_below count node} @ ghost =
  fun sorts count node premise -> ghost_ (
    well_typed_def sorts count node;
    children_below_def count node;
    match node with
    | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> ()
    | Add (a, b) | Eq_int (a, b) ->
      child_has_sort_def sorts count a L.Integer;
      child_has_sort_def sorts count b L.Integer
    | Int_if (c, a, b) ->
      child_has_sort_def sorts count c L.Boolean;
      child_has_sort_def sorts count a L.Integer;
      child_has_sort_def sorts count b L.Integer
    | Bool_if (c, a, b) ->
      child_has_sort_def sorts count c L.Boolean;
      child_has_sort_def sorts count a L.Boolean;
      child_has_sort_def sorts count b L.Boolean;
    ())

let (origin_frame @ total) :
    (origins : L.expr iarray) @ immutable ->
    (index : int) -> (expr : L.expr) @ immutable ->
    (node : t) @ immutable ->
    {u : unit | children_below index node} ->
    {u : unit | origin (I.updated origins index expr) node ===
      origin origins node} @ ghost =
  fun origins index expr node premise -> ghost_ (
    let changed = I.updated origins index expr in
    children_below_def index node;
    origin_def origins node;
    origin_def changed node;
    (match node with
     | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> ()
     | Add (a, b) | Eq_int (a, b) ->
       I.updated_read origins index expr a;
       I.updated_read origins index expr b;
       S.origin_def origins a;
       S.origin_def origins b;
       S.origin_def changed a;
       S.origin_def changed b
     | Int_if (c, a, b) | Bool_if (c, a, b) ->
       I.updated_read origins index expr c;
       I.updated_read origins index expr a;
       I.updated_read origins index expr b;
       S.origin_def origins c;
       S.origin_def origins a;
       S.origin_def origins b;
       S.origin_def changed c;
       S.origin_def changed a;
       S.origin_def changed b);
    ())

let (child_sort_frame @ total) :
    (sorts : L.sort iarray) @ immutable ->
    (index : int) -> (value : L.sort) ->
    (count : int) -> (id : int) -> (expected : L.sort) ->
    {u : unit | id < index} ->
    {u : unit | child_has_sort (I.updated sorts index value)
      count id expected = child_has_sort sorts count id expected}
    @ ghost = fun sorts index value count id expected premise -> ghost_ (
  I.updated_read sorts index value id;
  child_has_sort_def sorts count id expected;
  child_has_sort_def (I.updated sorts index value) count id expected;
  ())

let (well_typed_frame @ total) :
    (sorts : L.sort iarray) @ immutable ->
    (index : int) -> (value : L.sort) ->
    (count : int) ->
    (node : t) @ immutable ->
    {u : unit | children_below index node} ->
    {u : unit | well_typed (I.updated sorts index value) count node =
      well_typed sorts count node} @ ghost =
  fun sorts index value count node premise -> ghost_ (
    let changed = I.updated sorts index value in
    children_below_def index node;
    well_typed_def sorts count node;
    well_typed_def changed count node;
    (match node with
     | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> ()
     | Add (a, b) | Eq_int (a, b) ->
       child_sort_frame sorts index value count a L.Integer ();
       child_sort_frame sorts index value count b L.Integer ()
     | Int_if (c, a, b) ->
       child_sort_frame sorts index value count c L.Boolean ();
       child_sort_frame sorts index value count a L.Integer ();
       child_sort_frame sorts index value count b L.Integer ()
     | Bool_if (c, a, b) ->
       child_sort_frame sorts index value count c L.Boolean ();
       child_sort_frame sorts index value count a L.Boolean ();
       child_sort_frame sorts index value count b L.Boolean ());
    ())

let (origin_typed @ total) :
    (origins : L.expr iarray) @ immutable ->
    (node : t) @ immutable ->
    {u : unit | child_origins_typed origins node} ->
    {u : unit | L.sort (origin origins node) === Some (sort node)}
    @ ghost = fun origins node premise -> ghost_ (
  origin_def origins node;
  sort_def node;
  child_origins_typed_def origins node;
  L.sort_def (origin origins node);
  ())

let (key_exact @ total) (a : t @ immutable) (b : t @ immutable) :
    {u : unit | (key a === key b) = (a === b)} =
  key_def a;
  key_def b;
  match a, b with
  | Int_lit _, Int_lit _
  | Bool_lit _, Bool_lit _
  | Int_input, Int_input
  | Bool_input, Bool_input
  | Add _, Add _
  | Eq_int _, Eq_int _
  | Int_if _, Int_if _
  | Bool_if _, Bool_if _ -> ()
  | _ -> ()
