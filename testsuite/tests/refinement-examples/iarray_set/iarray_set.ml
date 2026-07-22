type t = int iarray

external int_equal : int -> int -> bool @@ total = "%equal"

(* AXIOMATIZED_IARRAY_WRAPPER: these four runtime operations and four
   first-order laws are the complete trusted boundary.  The public .mli hides
   the iarray representation and all view/model details. *)
external make_empty : unit -> t @@ total = "vox_sorted_iarray_empty"
external insert : int -> t @ logical -> t @@ total
  = "vox_sorted_iarray_insert"
external member : int -> t @ local logical -> bool @@ total
  = "vox_sorted_iarray_member"
external view : t @ local logical -> int list @@ total
  = "vox_sorted_iarray_view"

let empty = make_empty ()

external wrapper_view_empty :
  seed:int -> unit{ view empty = [] } @@ total
  = "%ignore"

external wrapper_view_insert :
  inserted:int ->
  array:t @ logical ->
  unit{
    view (insert inserted array)
    = Iarray_model.insert inserted (view array)
  } @@ total
  = "vox_sorted_iarray_view_insert_law"

external wrapper_member_view :
  query:int ->
  array:t @ logical ->
  unit{
    member query array = Iarray_model.member query (view array)
  } @@ total
  = "vox_sorted_iarray_member_view_law"

external wrapper_view_sorted :
  array:t @ logical ->
  unit{ Iarray_model.sorted_unique (view array) = true } @@ total
  = "%ignore"

type membership_side =
  | First
  | Second
  | Neither

let[@vox.def] membership_side first_member second_member =
  if first_member
  then First
  else if second_member then Second else Neither

let[@vox.def] rec agrees (t1 : t @ logical) (t2 : t @ logical)
    (nodes : int list @ logical) =
  match nodes with
  | [] -> true
  | key :: rest ->
    let first_member = member key t1 in
    let second_member = member key t2 in
    if first_member
    then
      if second_member then agrees t1 t2 rest else false
    else if second_member
    then false
    else agrees t1 t2 rest

let[@vox.def] equal (t1 : t @ local logical)
    (t2 : t @ local logical) =
  let first_view = view t1 in
  let second_view = view t2 in
  if agrees t1 t2 first_view
  then agrees t1 t2 second_view
  else false

let empty_law ~(query : int)
    : unit{ member query empty = false } =
  wrapper_view_empty ~seed:query;
  wrapper_member_view ~query ~array:empty;
  Iarray_model.empty_member_law ~query;
  ()

let insert_law ~(inserted : int) ~(tree : t @ logical) ~(query : int)
    : unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    } =
  wrapper_view_insert ~inserted ~array:tree;
  wrapper_member_view ~query ~array:(insert inserted tree);
  wrapper_member_view ~query ~array:tree;
  Iarray_model.insert_member_law
    ~inserted ~values:(view tree) ~query;
  ()

let agrees_cons ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(key : int) ~(rest : int list @ logical)
    ~proof:(_proof : unit{ agrees t1 t2 (key :: rest) = true })
    : unit{
      member key t1 = member key t2
      && agrees t1 t2 rest = true
    } =
  let _definition = agrees_def t1 t2 (key :: rest) in
  ()

let rec agrees_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(nodes : int list @ logical) ~(query : int)
    ~(agreement : unit{ agrees t1 t2 nodes = true })
    ~(present : unit{ Iarray_model.member query nodes = true })
    : unit{ member query t1 = member query t2 } =
  match nodes with
  | [] ->
    Iarray_model.empty_member_law ~query;
    ()
  | key :: rest ->
    let facts = agrees_cons ~t1 ~t2 ~key ~rest ~proof:agreement in
    Iarray_model.member_cons_law ~query ~value:key ~rest;
    if int_equal query key
    then facts
    else
      agrees_member ~t1 ~t2 ~nodes:rest ~query
        ~agreement:facts ~present:()

let prove_equal_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(equal_trees : unit{ equal t1 t2 = true }) ~(query : int)
    : unit{ member query t1 = member query t2 } =
  let _equality = equal_trees in
  let _definition = equal_def t1 t2 in
  let first_member = member query t1 in
  let second_member = member query t2 in
  let side = membership_side first_member second_member in
  let _side = membership_side_def first_member second_member in
  match side with
  | First ->
    let present = wrapper_member_view ~query ~array:t1 in
    agrees_member ~t1 ~t2 ~nodes:(view t1) ~query
      ~agreement:() ~present
  | Second ->
    let present = wrapper_member_view ~query ~array:t2 in
    agrees_member ~t1 ~t2 ~nodes:(view t2) ~query
      ~agreement:() ~present
  | Neither -> ()

let equal_forward_law ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(equal_trees : unit{ equal t1 t2 = true }) ~(query : int)
    : unit{ member query t1 = member query t2 } =
  let _equality = equal_trees in
  prove_equal_member ~t1 ~t2 ~equal_trees ~query

let equal_backward_law ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(pointwise : query:int ->
                   unit{ member query t1 = member query t2 })
    : unit{ equal t1 t2 = true } =
  let rec prove nodes : unit{ agrees t1 t2 nodes = true } =
    match nodes with
    | [] ->
      let _definition = agrees_def t1 t2 [] in
      ()
    | key :: rest ->
      pointwise ~query:key;
      prove rest;
      let _definition = agrees_def t1 t2 (key :: rest) in
      ()
  in
  let first_view = view t1 in
  let second_view = view t2 in
  prove first_view;
  prove second_view;
  let _definition = equal_def t1 t2 in
  ()

(* This theorem makes the representation invariant explicit above the trusted
   wrapper boundary.  It is internal because Iarray_set.mli is exactly SET. *)
let representation_law ~(set : t @ logical)
    : unit{
      Iarray_model.sorted_unique (view set) = true
    } =
  wrapper_view_sorted ~array:set
