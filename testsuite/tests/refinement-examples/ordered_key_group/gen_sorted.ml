external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"

(* A strictly increasing list.  Like the trees, membership uses the order:
   it stops as soon as it passes the key it is looking for, which is wrong
   unless the list really is sorted. *)
module Make (K : Key_intf.ORDERED_KEY) = struct
  type key = K.t
  type t = key list

  let key_witness : key = K.witness

  type direction =
    | Same
    | Before
    | After

  type membership_side =
    | First
    | Second
    | Neither

  let[@vox.def] direction (probe : key @ logical) (pivot : key @ logical) =
    if int_equal (K.compare probe pivot) 0
    then Same
    else if int_less (K.compare probe pivot) 0
    then Before
    else After

  let[@vox.def] membership_side first_member second_member =
    if first_member
    then First
    else if second_member then Second else Neither

  let empty = ([] : t{ _ = [] })

  let[@vox.def] rec member (query : key @ logical) (values : t @ logical) =
    match values with
    | [] -> false
    | value :: rest ->
      if int_equal (K.compare query value) 0
      then true
      else if int_less (K.compare query value) 0
      then false
      else member query rest

  let[@vox.def] rec occurs (query : key @ logical) (values : t @ logical) =
    match values with
    | [] -> false
    | value :: rest ->
      int_equal (K.compare query value) 0 || occurs query rest

  let[@vox.def] rec above_all (lower : key @ logical)
      (values : t @ logical) =
    match values with
    | [] -> true
    | value :: rest ->
      int_less (K.compare lower value) 0 && above_all lower rest

  let[@vox.def] rec sorted (values : t @ logical) =
    match values with
    | [] -> true
    | value :: rest -> above_all value rest && sorted rest

  let[@vox.def] invariant (values : t @ logical) = sorted values

  let rec above_all_weaken (outer : key @ logical)
      (inner : key{ K.compare outer _ < 0 } @ logical)
      (values : t @ logical)
      (_bounded : unit{ above_all inner values = true })
      : unit{ above_all outer values = true } =
    match values with
    | [] ->
      above_all_def outer [];
      ()
    | value :: rest ->
      above_all_def inner (value :: rest);
      above_all_def outer (value :: rest);
      K.compare_negative_transitive ~first:outer ~second:inner ~third:value;
      above_all_weaken outer inner rest ();
      ()

  let rec above_all_absent (lower : key @ logical)
      (query : key{ K.compare _ lower < 0 } @ logical)
      (values : t @ logical)
      (_bounded : unit{ above_all lower values = true })
      : unit{ occurs query values = false } =
    match values with
    | [] ->
      occurs_def query [];
      ()
    | value :: rest ->
      above_all_def lower (value :: rest);
      occurs_def query (value :: rest);
      K.compare_negative_transitive ~first:query ~second:lower ~third:value;
      above_all_absent lower query rest ();
      ()

  (* On a sorted list the early exit finds exactly the keys that occur. *)
  let rec member_occurs (query : key @ logical) (values : t @ logical)
      (_sorted : unit{ sorted values = true })
      : unit{ member query values = occurs query values } =
    match values with
    | [] ->
      member_def query [];
      occurs_def query [];
      ()
    | value :: rest ->
      sorted_def (value :: rest);
      member_def query (value :: rest);
      occurs_def query (value :: rest);
      member_occurs query rest ();
      let choice = direction query value in
      direction_def query value;
      match choice with
      | Same -> ()
      | Before -> above_all_absent value query rest ()
      | After -> ()

  let[@vox.def] rec insert (fresh : key @ logical) (values : t @ logical) =
    match values with
    | [] -> [ fresh ]
    | value :: rest ->
      if int_equal (K.compare fresh value) 0
      then values
      else if int_less (K.compare fresh value) 0
      then fresh :: values
      else value :: insert fresh rest

  let rec insert_above_all (lower : key @ logical)
      (fresh : key{ K.compare lower _ < 0 } @ logical)
      (values : t @ logical)
      (_bounded : unit{ above_all lower values = true })
      : unit{ above_all lower (insert fresh values) = true } =
    match values with
    | [] ->
      insert_def fresh [];
      above_all_def lower [ fresh ];
      above_all_def lower [];
      ()
    | value :: rest ->
      insert_def fresh (value :: rest);
      above_all_def lower (value :: rest);
      let choice = direction fresh value in
      direction_def fresh value;
      match choice with
      | Same -> ()
      | Before ->
        above_all_def lower (fresh :: value :: rest);
        ()
      | After ->
        insert_above_all lower fresh rest ();
        above_all_def lower (value :: insert fresh rest);
        ()

  let rec insert_sorted (fresh : key @ logical) (values : t @ logical)
      (_sorted : unit{ sorted values = true })
      : unit{ sorted (insert fresh values) = true } =
    match values with
    | [] ->
      insert_def fresh [];
      sorted_def [ fresh ];
      sorted_def [];
      above_all_def fresh [];
      ()
    | value :: rest ->
      insert_def fresh (value :: rest);
      sorted_def (value :: rest);
      let choice = direction fresh value in
      direction_def fresh value;
      match choice with
      | Same -> ()
      | Before ->
        sorted_def (fresh :: value :: rest);
        above_all_def fresh (value :: rest);
        above_all_weaken fresh value rest ();
        ()
      | After ->
        (* [direction] gives the comparison the other way round. *)
        K.compare_sign_reversal ~left:value ~right:fresh;
        insert_sorted fresh rest ();
        insert_above_all value fresh rest ();
        sorted_def (value :: insert fresh rest);
        ()

  let rec occurs_insert (fresh : key @ logical) (values : t @ logical)
      (query : key @ logical)
      : unit{
        occurs query (insert fresh values)
        = (K.compare query fresh = 0 || occurs query values)
      } =
    match values with
    | [] ->
      insert_def fresh [];
      occurs_def query [ fresh ];
      occurs_def query [];
      ()
    | value :: rest ->
      insert_def fresh (value :: rest);
      occurs_def query (value :: rest);
      let choice = direction fresh value in
      direction_def fresh value;
      K.compare_zero_iff_equal ~left:fresh ~right:value;
      match choice with
      | Same -> ()
      | Before ->
        occurs_def query (fresh :: value :: rest);
        ()
      | After ->
        occurs_insert fresh rest query;
        occurs_def query (value :: insert fresh rest);
        ()

  let empty_law ~(query : key @ logical)
      : unit{ member query empty = false } =
    member_def query empty;
    ()

  let empty_invariant : unit{ invariant empty = true } =
    invariant_def empty;
    sorted_def [];
    ()

  let insert_invariant ~(inserted : key @ logical) ~(set : t @ logical)
      ~(well_formed : unit{ invariant set = true })
      : unit{ invariant (insert inserted set) = true } =
    invariant_def set;
    invariant_def (insert inserted set);
    insert_sorted inserted set ();
    ()

  let insert_law ~(inserted : key @ logical) ~(set : t @ logical)
      ~(query : key @ logical)
      ~(well_formed : unit{ invariant set = true })
      : unit{
        member query (insert inserted set)
        = ((query = inserted) || member query set)
      } =
    invariant_def set;
    insert_sorted inserted set ();
    member_occurs query set ();
    member_occurs query (insert inserted set) ();
    occurs_insert inserted set query;
    K.compare_zero_iff_equal ~left:query ~right:inserted;
    ()

  let[@vox.def] rec agrees (left : t @ logical) (right : t @ logical)
      (keys : t @ logical) =
    match keys with
    | [] -> true
    | value :: rest ->
      let first_member = member value left in
      let second_member = member value right in
      if first_member
      then
        if second_member then agrees left right rest else false
      else if second_member then false else agrees left right rest

  let[@vox.def] equal (left : t @ logical) (right : t @ logical) =
    if agrees left right left then agrees left right right else false

  let agrees_cons ~(left : t @ logical) ~(right : t @ logical)
      ~(value : key @ logical) ~(rest : t @ logical)
      ~proof:(_proof : unit{ agrees left right (value :: rest) = true })
      : unit{
        member value left = member value right
        && agrees left right rest = true
      } =
    agrees_def left right (value :: rest);
    ()

  let rec agrees_member ~(left : t @ logical) ~(right : t @ logical)
      ~(keys : t @ logical) ~(query : key @ logical)
      ~(agreement : unit{ agrees left right keys = true })
      ~(present : unit{ member query keys = true })
      : unit{ member query left = member query right } =
    match keys with
    | [] ->
      member_def query [];
      ()
    | value :: rest ->
      let facts = agrees_cons ~left ~right ~value ~rest ~proof:agreement in
      member_def query (value :: rest);
      let choice = direction query value in
      direction_def query value;
      K.compare_zero_iff_equal ~left:query ~right:value;
      match choice with
      | Same -> facts
      | Before -> facts
      | After ->
        agrees_member ~left ~right ~keys:rest ~query
          ~agreement:facts ~present:()

  let prove_equal_member ~(left : t @ logical)
      ~(right : t{ equal left _ = true } @ logical)
      ~(query : key @ logical)
      : unit{ member query left = member query right } =
    equal_def left right;
    let first_member = member query left in
    let second_member = member query right in
    let side = membership_side first_member second_member in
    membership_side_def first_member second_member;
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
        agrees_def left right [];
        ()
      | value :: rest ->
        pointwise ~query:value;
        prove rest;
        agrees_def left right (value :: rest);
        ()
    in
    prove left;
    prove right;
    equal_def left right;
    ()
end
