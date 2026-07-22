module type ORDERED_KEY = sig
  type t : immutable_data

  val witness : t

  val compare :
    t @ local logical -> t @ local logical -> int @@ total

  val compare_zero_iff_equal :
    left:t @ logical -> right:t @ logical ->
    unit{ (compare left right = 0) = (left = right) } @@ total

  val compare_sign_reversal :
    left:t @ logical -> right:t @ logical ->
    unit{
      (compare left right < 0) = (compare right left > 0)
    } @@ total

  val compare_negative_transitive :
    first:t @ logical -> second:t @ logical -> third:t @ logical ->
    unit{
      not (compare first second < 0)
      || not (compare second third < 0)
      || compare first third < 0
    } @@ total
end

module type SET = sig
  type key : immutable_data
  type t

  val key_witness : key

  val empty : t @@ total
  val insert : key @ logical -> t @ logical -> t @@ total
  val member :
    key @ local logical -> t @ local logical -> bool @@ total
  val equal :
    t @ local logical -> t @ local logical -> bool @@ total

  val empty_law :
    query:key @ logical ->
    unit{ member query empty = false } @@ total

  val insert_law :
    inserted:key @ logical -> set:t @ logical -> query:key @ logical ->
    unit{
      member query (insert inserted set)
      = ((query = inserted) || member query set)
    } @@ total

  val equal_left_to_right :
    left:t @ logical -> right:t @ logical -> query:key @ logical ->
    unit{
      equal left right = false
      || member query left = false
      || member query right = true
    } @@ total

  val equal_right_to_left :
    left:t @ logical -> right:t @ logical -> query:key @ logical ->
    unit{
      equal left right = false
      || member query right = false
      || member query left = true
    } @@ total

  val equal_forward_law :
    left:t @ logical -> right:t @ logical ->
    query:key @ logical ->
    equal_sets:unit{ equal left right = true } ->
    unit{ member query left = member query right } @@ total

  val equal_backward_law :
    left:t @ logical -> right:t @ logical ->
    pointwise:
      (query:key @ logical ->
       unit{ member query left = member query right }) @ total ->
    unit{ equal left right = true } @@ total
end

external int_equal : int -> int -> bool @@ total = "%equal"

module Make (K : ORDERED_KEY) = struct
  type key = K.t
  type t = key list

  (* Keep a direct ordinary value of the abstract key type in the functor
     body.  Seal VCs can then establish that [key] is inhabited even when
     relevance pruning removes a particular query binder. *)
  let key_witness : key = K.witness

  type direction =
    | Same
    | Different

  type membership_side =
    | Left
    | Right
    | Neither

  let[@vox.def] direction (left : key @ logical)
      (right : key @ logical) =
    if int_equal (K.compare left right) 0 then Same else Different

  let[@vox.def] membership_side left_member right_member =
    if left_member
    then Left
    else if right_member then Right else Neither

  let empty =
    K.compare_zero_iff_equal ~left:K.witness ~right:K.witness;
    ([] : t{ _ = [] })

  let[@vox.def] insert (inserted : key @ logical)
      (set : t @ logical) =
    inserted :: set

  let[@vox.def] rec member (query : key @ logical)
      (set : t @ logical) =
    match set with
    | [] -> false
    | key :: rest ->
      if int_equal (K.compare query key) 0
      then true
      else member query rest

  let[@vox.def] rec agrees (left : t @ logical)
      (right : t @ logical) (keys : t @ logical) =
    match keys with
    | [] -> true
    | key :: rest ->
      let left_member = member key left in
      let right_member = member key right in
      if left_member
      then
        if right_member then agrees left right rest else false
      else if right_member then false else agrees left right rest

  let[@vox.def] equal (left : t @ logical) (right : t @ logical) =
    if agrees left right left then agrees left right right else false

  let empty_law ~(query : key @ logical)
      : unit{ member query empty = false } =
    let _definition = member_def query empty in
    ()

  let insert_law ~(inserted : key @ logical)
      ~(set : t @ logical) ~(query : key @ logical)
      : unit{
        member query (insert inserted set)
        = ((query = inserted) || member query set)
      } =
    let _insert = insert_def inserted set in
    let _member = member_def query (inserted :: set) in
    K.compare_zero_iff_equal ~left:query ~right:inserted;
    if int_equal (K.compare query inserted) 0 then () else ()

  let agrees_cons ~(left : t @ logical) ~(right : t @ logical)
      ~(key : key @ logical) ~(rest : t @ logical)
      ~proof:(_proof : unit{
        agrees left right (key :: rest) = true
      })
      : unit{
        member key left = member key right
        && agrees left right rest = true
      } =
    let _definition = agrees_def left right (key :: rest) in
    let left_member = member key left in
    let right_member = member key right in
    if left_member
    then if right_member then () else ()
    else if right_member then () else ()

  let rec agrees_member ~(left : t @ logical)
      ~(right : t @ logical) ~(keys : t @ logical)
      ~(query : key @ logical)
      ~(agreement : unit{ agrees left right keys = true })
      ~(present : unit{ member query keys = true })
      : unit{ member query left = member query right } =
    match keys with
    | [] ->
      let _member = member_def query [] in
      ()
    | key :: rest ->
      let facts = agrees_cons ~left ~right ~key ~rest ~proof:agreement in
      let _member = member_def query (key :: rest) in
      let choice = direction query key in
      let _choice = direction_def query key in
      K.compare_zero_iff_equal ~left:query ~right:key;
      match choice with
      | Same -> facts
      | Different ->
        agrees_member ~left ~right ~keys:rest ~query
          ~agreement:facts ~present

  let prove_equal_member ~(left : t @ logical)
      ~(right : t{ equal left _ = true } @ logical)
      ~(query : key @ logical)
      : unit{ member query left = member query right } =
    let _definition = equal_def left right in
    let left_member = member query left in
    let right_member = member query right in
    let side = membership_side left_member right_member in
    let _side = membership_side_def left_member right_member in
    match side with
    | Left ->
      agrees_member ~left ~right ~keys:left ~query
        ~agreement:() ~present:()
    | Right ->
      agrees_member ~left ~right ~keys:right ~query
        ~agreement:() ~present:()
    | Neither -> ()

  let equal_left_to_right ~(left : t @ logical)
      ~(right : t @ logical) ~(query : key @ logical)
      : unit{
        equal left right = false
        || member query left = false
        || member query right = true
      } =
    let equality = equal left right in
    let equality_side = membership_side equality false in
    let _equality_side = membership_side_def equality false in
    match equality_side with
    | Left ->
      K.compare_zero_iff_equal ~left:K.witness ~right:K.witness;
      prove_equal_member ~left ~right ~query;
      ()
    | Right -> ()
    | Neither -> ()

  let equal_right_to_left ~(left : t @ logical)
      ~(right : t @ logical) ~(query : key @ logical)
      : unit{
        equal left right = false
        || member query right = false
        || member query left = true
      } =
    let equality = equal left right in
    let equality_side = membership_side equality false in
    let _equality_side = membership_side_def equality false in
    match equality_side with
    | Left ->
      K.compare_zero_iff_equal ~left:K.witness ~right:K.witness;
      prove_equal_member ~left ~right ~query;
      ()
    | Right -> ()
    | Neither -> ()

  let equal_forward_law ~(left : t @ logical) ~(right : t @ logical)
      ~(query : key @ logical)
      ~(equal_sets : unit{ equal left right = true })
      : unit{ member query left = member query right } =
    K.compare_zero_iff_equal ~left:K.witness ~right:K.witness;
    prove_equal_member ~left ~right ~query

  let equal_backward_law ~(left : t @ logical) ~(right : t @ logical)
      ~(pointwise :
          query:key @ logical ->
          unit{ member query left = member query right })
      : unit{ equal left right = true } =
    K.compare_zero_iff_equal ~left:K.witness ~right:K.witness;
    let rec prove keys : unit{ agrees left right keys = true } =
      match keys with
      | [] ->
        let _definition = agrees_def left right [] in
        ()
      | key :: rest ->
        pointwise ~query:key;
        prove rest;
        let _definition = agrees_def left right (key :: rest) in
        ()
    in
    prove left;
    prove right;
    let _definition = equal_def left right in
    ()
end
