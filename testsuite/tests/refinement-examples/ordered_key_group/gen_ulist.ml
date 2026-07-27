(* A list with no repeated key.

   Why it is in this directory: it is the control.  It is the only one of the
   four that uses the comparison as an equality test and never as an order,
   and that is what makes "twelve law instantiations per tree" a measurement
   rather than an assertion -- this one needs four, all of
   [compare_zero_iff_equal], and it is the only one that still verifies when
   sign reversal and transitivity are weakened to [unit{ true }]. *)

external int_equal : int -> int -> bool @@ total = "%equal"

module Make (K : Key_intf.ORDERED_KEY) = struct
  type key = K.t
  type t = key list

  let key_witness : key = K.witness

  type direction =
    | Same
    | Different

  type membership_side =
    | First
    | Second
    | Neither

  let[@vox.def] direction (left : key @ logical)
      (right : key @ logical) =
    if int_equal (K.compare left right) 0 then Same else Different

  let[@vox.def] membership_side first_member second_member =
    if first_member
    then First
    else if second_member then Second else Neither

  let empty = ([] : t{ _ = [] })

  let[@vox.def] rec member (query : key @ logical) (set : t @ logical) =
    match set with
    | [] -> false
    | key :: rest ->
      if int_equal (K.compare query key) 0
      then true
      else member query rest

  let[@vox.def] insert (inserted : key @ logical) (set : t @ logical) =
    if member inserted set then set else inserted :: set

  (* No key is repeated: unlike the plain list of [polyset], this really is
     a set, and every key occupies exactly one cell. *)
  let[@vox.def] rec unique (set : t @ logical) =
    match set with
    | [] -> true
    | key :: rest -> if member key rest then false else unique rest

  let[@vox.def] invariant (set : t @ logical) = unique set

  let[@vox.def] rec agrees (left : t @ logical) (right : t @ logical)
      (keys : t @ logical) =
    match keys with
    | [] -> true
    | key :: rest ->
      let first_member = member key left in
      let second_member = member key right in
      if first_member
      then
        if second_member then agrees left right rest else false
      else if second_member then false else agrees left right rest

  let[@vox.def] equal (left : t @ logical) (right : t @ logical) =
    if agrees left right left then agrees left right right else false

  let empty_law ~(query : key @ logical)
      : unit{ member query empty = false } =
    let _definition = member_def query empty in
    ()

  let empty_invariant : unit{ invariant empty = true } =
    let _invariant = invariant_def empty in
    let _definition = unique_def [] in
    ()

  let insert_invariant ~(inserted : key @ logical) ~(set : t @ logical)
      ~(well_formed : unit{ invariant set = true })
      : unit{ invariant (insert inserted set) = true } =
    let _set = invariant_def set in
    let _result = invariant_def (insert inserted set) in
    let _insert = insert_def inserted set in
    let present = member inserted set in
    match present with
    | true -> ()
    | false ->
      let _definition = unique_def (inserted :: set) in
      ()

  let insert_law ~(inserted : key @ logical) ~(set : t @ logical)
      ~(query : key @ logical)
      ~(well_formed : unit{ invariant set = true })
      : unit{
        member query (insert inserted set)
        = ((query = inserted) || member query set)
      } =
    let _insert = insert_def inserted set in
    let _new_member = member_def query (inserted :: set) in
    (* [compare query inserted = 0] is not [query = inserted] until the law
       says so, and it says so only at this pair. *)
    K.compare_zero_iff_equal ~left:query ~right:inserted;
    let choice = direction query inserted in
    let _choice = direction_def query inserted in
    match choice with
    | Same -> ()
    | Different -> ()

  let agrees_cons ~(left : t @ logical) ~(right : t @ logical)
      ~(key : key @ logical) ~(rest : t @ logical)
      ~proof:(_proof : unit{ agrees left right (key :: rest) = true })
      : unit{
        member key left = member key right
        && agrees left right rest = true
      } =
    let _definition = agrees_def left right (key :: rest) in
    ()

  let rec agrees_member ~(left : t @ logical) ~(right : t @ logical)
      ~(keys : t @ logical) ~(query : key @ logical)
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
          ~agreement:facts ~present:()

  let prove_equal_member ~(left : t @ logical)
      ~(right : t{ equal left _ = true } @ logical)
      ~(query : key @ logical)
      : unit{ member query left = member query right } =
    let _definition = equal_def left right in
    let first_member = member query left in
    let second_member = member query right in
    let side = membership_side first_member second_member in
    let _side = membership_side_def first_member second_member in
    match side with
    | First ->
      agrees_member ~left ~right ~keys:left ~query
        ~agreement:() ~present:()
    | Second ->
      agrees_member ~left ~right ~keys:right ~query
        ~agreement:() ~present:()
    | Neither -> ()

  let equal_forward_law ~(left : t @ logical) ~(right : t @ logical)
      ~(equal_sets : unit{ equal left right = true })
      ~(query : key @ logical)
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

  (* Not part of [SET].  See the note in [key_intf.ml]: the runtime gate
     needs one observation that differs between implementations, and the
     keys in representation order is it.  Here the representation is the
     list itself, most recently inserted first. *)
  let shape (set : t) = set
end
