module T = Fast_term

let[@def] rec (fits @ total) (term : T.term @ immutable)
    (depth : int) (limit : int) = ghost_ (
  0 <= depth && depth <= limit && match term with
  | T.Bound _ | T.Truth -> true
  | T.Lambda body | T.Recursive body -> fits body depth limit
  | T.Apply (a, b) -> fits a depth limit && fits b depth limit
  | T.Let (a, b) -> depth + 1 > depth
    && fits a (depth + 1) limit && fits b depth limit)

let rec (weaken @ total) : (term : T.term) @ immutable ->
    (depth : int) -> (before : int) -> (after : int) ->
    {u : unit | fits term depth before && before <= after} ->
    {u : unit | fits term depth after} @ ghost =
  fun term depth before after premise -> ghost_ (
    fits_def term depth before; fits_def term depth after;
    match term with
    | T.Bound _ | T.Truth -> ()
    | T.Lambda body | T.Recursive body ->
      weaken body depth before after (); ()
    | T.Apply (a, b) ->
      weaken a depth before after ();
      weaken b depth before after (); ()
    | T.Let (a, b) -> let next = depth + 1 in
      weaken a next before after ();
      weaken b depth before after (); ())

type goal = {term : T.term @@ ghost; depth : int @@ ghost}

let rec work : (goal : goal) @ immutable -> (term : T.term) @ immutable ->
    (depth : int) ->
    (proof : ({u : unit | 0 <= depth}) Ghost.t) @ immutable ->
    (use : ((n : {n : int | fits term depth n}) ->
      {n : int | fits goal.term goal.depth n})) ->
    {n : int | fits goal.term goal.depth n} = fun goal term depth proof use ->
  ghost_ (let () = proof.Ghost.ghost in ());
  match term with
  | T.Bound _ | T.Truth ->
    ghost_ (fits_def term depth depth); use (depth)
  | T.Lambda body | T.Recursive body ->
    let resume : (n : {n : int | fits body depth n}) ->
        {n : int | fits goal.term goal.depth n} = fun n ->
      ghost_ (fits_def body depth n; fits_def term depth n);
      use (n) in
    work goal body depth {Ghost.ghost = ghost_ ()} resume
  | T.Apply (a, b) ->
    let resume_left : (left : {n : int | fits a depth n}) ->
        {n : int | fits goal.term goal.depth n} = fun left ->
      let resume_right : (right : {n : int | fits b depth n}) ->
          {n : int | fits goal.term goal.depth n} = fun right ->
        let limit = if left > right then left else right in
        ghost_ (weaken a depth left limit ();
          weaken b depth right limit ();
          fits_def a depth limit; fits_def term depth limit);
        use (limit) in
      work goal b depth {Ghost.ghost = ghost_ ()} resume_right in
    work goal a depth {Ghost.ghost = ghost_ ()} resume_left
  | T.Let (a, b) ->
    let child_depth = depth + 1 in
    if child_depth <= depth then failwith "type inference level capacity" else
    let resume_left : (left : {n : int | fits a child_depth n}) ->
        {n : int | fits goal.term goal.depth n} = fun left ->
      let resume_right : (right : {n : int | fits b depth n}) ->
          {n : int | fits goal.term goal.depth n} = fun right ->
        let limit = if left > right then left else right in
        ghost_ (weaken a child_depth left limit ();
          weaken b depth right limit ();
          fits_def b depth limit; fits_def term depth limit);
        use (limit) in
      work goal b depth {Ghost.ghost = ghost_ ()} resume_right in
    work goal a child_depth {Ghost.ghost = ghost_ ()} resume_left

let measure : (term : T.term) @ immutable ->
    {n : int | fits term 0 n} = fun term ->
  let goal = {term = ghost_ term; depth = ghost_ 0} in
  let use : (n : {n : int | fits term 0 n}) ->
      {n : int | fits goal.term goal.depth n} = fun n ->
    n in
  let n = work goal term 0 {Ghost.ghost = ghost_ ()} use in n

let create : (term : T.term) @ immutable ->
    {a : Generalize_spec.pool Borrow_iarray.Owned_array.t |
      fits term 0 (Iarray.length (Borrow_iarray.Owned_array.contents a))}
      @ unique = fun term ->
  let limit = measure term in
  let values = Iarray.init limit (fun _ -> Generalize_spec.Empty) in
  Borrow_iarray.Owned_array.of_iarray values
