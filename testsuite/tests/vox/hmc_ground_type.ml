module D = Hm_declarative
module C = Copy_spec

type t = Bool | Word64 | List of t | Arrow of t * t [@@inductive]

let[@def] rec (mono @ total) (ty : t @ immutable) = match ty with
  | Bool -> D.Boolean | Word64 -> D.Word64
  | List a -> D.List_type (mono a)
  | Arrow (a, b) -> D.Function (mono a, mono b)

let[@def] rec (copy @ total) (ty : t @ immutable) = match ty with
  | Bool -> C.Boolean | Word64 -> C.Word64
  | List a -> C.List_type (copy a)
  | Arrow (a, b) -> C.Function (copy a, copy b)

let rec (representations @ total) : (ty : t) @ immutable ->
    {u : unit | D.embed (copy ty) === mono ty} @ ghost = fun ty -> ghost_ (
    mono_def ty; copy_def ty; D.embed_def (copy ty);
    match ty with
    | Bool | Word64 -> ()
    | List a -> representations a
    | Arrow (a, b) -> representations a; representations b)

let rec (equal @ total) : (a : t) @ immutable -> (b : t) @ immutable ->
    {r : bool | r = (a === b)} = fun a b -> match a, b with
  | Bool, Bool | Word64, Word64 -> true
  | List a, List b -> equal a b
  | Arrow (a, b), Arrow (x, y) -> equal a x && equal b y
  | _ -> false

let rec (well_formed @ total) : (n : D.index) @ immutable -> (ty : t) @ immutable ->
    {u : unit | D.mono_wf n (mono ty)} @ ghost = fun n ty -> ghost_ (
    mono_def ty; D.mono_wf_def n (mono ty);
    match ty with Bool | Word64 -> () | List a -> well_formed n a
    | Arrow (a, b) -> well_formed n a; well_formed n b)

let[@def] rec (ground @ total) (ty : D.mono @ immutable) = match ty with
  | D.Boolean | D.Word64 -> true
  | D.List_type a -> ground a
  | D.Function (a, b) -> ground a && ground b
  | D.Parameter _ | D.Free _ -> false

let rec (is_ground @ total) : (ty : t) @ immutable ->
    {u : unit | ground (mono ty)} @ ghost = fun ty -> ghost_ (
    mono_def ty; ground_def (mono ty);
    match ty with Bool | Word64 -> () | List a -> is_ground a
    | Arrow (a, b) -> is_ground a; is_ground b)
