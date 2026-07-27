external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"

module Make (K : Key_intf.ORDERED_KEY) = struct
  type key = K.t

  type t =
    | Empty
    | Node of t * key * t

  let key_witness : key = K.witness

  type direction =
    | Same
    | Left
    | Right

  type membership_side =
    | First
    | Second
    | Neither

  let[@vox.def] direction (probe : key @ logical) (pivot : key @ logical) =
    if int_equal (K.compare probe pivot) 0
    then Same
    else if int_less (K.compare probe pivot) 0
    then Left
    else Right

  let[@vox.def] membership_side first_member second_member =
    if first_member
    then First
    else if second_member then Second else Neither

  let empty = Empty

  (* One comparison per level, descending a single spine. *)
  let[@vox.def] rec member (query : key @ logical) (tree : t @ logical) =
    match tree with
    | Empty -> false
    | Node (left, pivot, right) ->
      if int_equal (K.compare query pivot) 0
      then true
      else if int_less (K.compare query pivot) 0
      then member query left
      else member query right

  (* Occurrence anywhere in the tree, used to say what the spine is worth. *)
  let[@vox.def] rec occurs (query : key @ logical) (tree : t @ logical) =
    match tree with
    | Empty -> false
    | Node (left, pivot, right) ->
      int_equal (K.compare query pivot) 0
      || occurs query left
      || occurs query right

  let[@vox.def] rec below (tree : t @ logical) (bound : key @ logical) =
    match tree with
    | Empty -> true
    | Node (left, pivot, right) ->
      int_less (K.compare pivot bound) 0
      && below left bound && below right bound

  let[@vox.def] rec above (tree : t @ logical) (bound : key @ logical) =
    match tree with
    | Empty -> true
    | Node (left, pivot, right) ->
      int_less (K.compare bound pivot) 0
      && above left bound && above right bound

  let[@vox.def] rec ordered (tree : t @ logical) =
    match tree with
    | Empty -> true
    | Node (left, pivot, right) ->
      ordered left && ordered right
      && below left pivot && above right pivot

  let[@vox.def] invariant (tree : t @ logical) = ordered tree

  (* Every key of [tree] is below [bound], and [bound] is below [query], so
     [query] is not one of them.  For an [int] key the solver would take both
     steps by itself; here each node needs transitivity and the sign law at
     that node's key. *)
  let rec below_absent (bound : key @ logical)
      (query : key{ K.compare bound _ < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ below tree bound = true })
      : unit{ occurs query tree = false } =
    match tree with
    | Empty ->
      occurs_def query Empty;
      ()
    | Node (left, pivot, right) ->
      below_def (Node (left, pivot, right)) bound;
      occurs_def query (Node (left, pivot, right));
      K.compare_negative_transitive ~first:pivot ~second:bound ~third:query;
      K.compare_sign_reversal ~left:pivot ~right:query;
      below_absent bound query left ();
      below_absent bound query right ();
      ()

  let rec above_absent (bound : key @ logical)
      (query : key{ K.compare _ bound < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ above tree bound = true })
      : unit{ occurs query tree = false } =
    match tree with
    | Empty ->
      occurs_def query Empty;
      ()
    | Node (left, pivot, right) ->
      above_def (Node (left, pivot, right)) bound;
      occurs_def query (Node (left, pivot, right));
      K.compare_negative_transitive ~first:query ~second:bound ~third:pivot;
      above_absent bound query left ();
      above_absent bound query right ();
      ()

  (* On an ordered tree the single spine finds exactly the keys that occur. *)
  let rec member_occurs (query : key @ logical) (tree : t @ logical)
      (_ordered : unit{ ordered tree = true })
      : unit{ member query tree = occurs query tree } =
    match tree with
    | Empty ->
      member_def query Empty;
      occurs_def query Empty;
      ()
    | Node (left, pivot, right) ->
      ordered_def (Node (left, pivot, right));
      member_def query (Node (left, pivot, right));
      occurs_def query (Node (left, pivot, right));
      member_occurs query left ();
      member_occurs query right ();
      let choice = direction query pivot in
      direction_def query pivot;
      match choice with
      | Same -> ()
      | Left -> above_absent pivot query right ()
      | Right ->
        K.compare_sign_reversal ~left:pivot ~right:query;
        below_absent pivot query left ()

  let[@vox.def] rec insert (fresh : key @ logical) (tree : t @ logical) : t =
    match tree with
    | Empty -> Node (Empty, fresh, Empty)
    | Node (left, pivot, right) ->
      if int_equal (K.compare fresh pivot) 0
      then tree
      else if int_less (K.compare fresh pivot) 0
      then Node (insert fresh left, pivot, right)
      else Node (left, pivot, insert fresh right)

  let rec insert_below (bound : key @ logical)
      (fresh : key{ K.compare _ bound < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ below tree bound = true })
      : unit{ below (insert fresh tree) bound = true } =
    match tree with
    | Empty ->
      insert_def fresh Empty;
      below_def (Node (Empty, fresh, Empty)) bound;
      below_def Empty bound;
      ()
    | Node (left, pivot, right) ->
      insert_def fresh (Node (left, pivot, right));
      below_def (Node (left, pivot, right)) bound;
      let choice = direction fresh pivot in
      direction_def fresh pivot;
      match choice with
      | Same -> ()
      | Left ->
        insert_below bound fresh left ();
        below_def (Node (insert fresh left, pivot, right)) bound;
        ()
      | Right ->
        insert_below bound fresh right ();
        below_def (Node (left, pivot, insert fresh right)) bound;
        ()

  let rec insert_above (bound : key @ logical)
      (fresh : key{ K.compare bound _ < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ above tree bound = true })
      : unit{ above (insert fresh tree) bound = true } =
    match tree with
    | Empty ->
      insert_def fresh Empty;
      above_def (Node (Empty, fresh, Empty)) bound;
      above_def Empty bound;
      ()
    | Node (left, pivot, right) ->
      insert_def fresh (Node (left, pivot, right));
      above_def (Node (left, pivot, right)) bound;
      let choice = direction fresh pivot in
      direction_def fresh pivot;
      match choice with
      | Same -> ()
      | Left ->
        insert_above bound fresh left ();
        above_def (Node (insert fresh left, pivot, right)) bound;
        ()
      | Right ->
        insert_above bound fresh right ();
        above_def (Node (left, pivot, insert fresh right)) bound;
        ()

  let rec insert_ordered (fresh : key @ logical) (tree : t @ logical)
      (_ordered : unit{ ordered tree = true })
      : unit{ ordered (insert fresh tree) = true } =
    match tree with
    | Empty ->
      insert_def fresh Empty;
      ordered_def (Node (Empty, fresh, Empty));
      ordered_def Empty;
      below_def Empty fresh;
      above_def Empty fresh;
      ()
    | Node (left, pivot, right) ->
      insert_def fresh (Node (left, pivot, right));
      ordered_def (Node (left, pivot, right));
      let choice = direction fresh pivot in
      direction_def fresh pivot;
      match choice with
      | Same -> ()
      | Left ->
        insert_ordered fresh left ();
        insert_below pivot fresh left ();
        ordered_def (Node (insert fresh left, pivot, right));
        ()
      | Right ->
        (* [direction] gives [compare fresh pivot > 0]; the bound lemma wants
           it the other way round. *)
        K.compare_sign_reversal ~left:pivot ~right:fresh;
        insert_ordered fresh right ();
        insert_above pivot fresh right ();
        ordered_def (Node (left, pivot, insert fresh right));
        ()

  let member_node (query : key @ logical) (left : t @ logical)
      (pivot : key @ logical) (right : t @ logical)
      : unit{
        member query (Node (left, pivot, right))
        = if K.compare query pivot = 0
          then true
          else if K.compare query pivot < 0
          then member query left
          else member query right
      } =
    member_def query (Node (left, pivot, right));
    ()

  let member_insert_empty (fresh : key @ logical) (query : key @ logical)
      : unit{
        member query (insert fresh Empty)
        = (K.compare query fresh = 0 || member query Empty)
      } =
    insert_def fresh Empty;
    member_def query Empty;
    member_node query Empty fresh Empty;
    ()

  let member_insert_same (pivot : key @ logical)
      (fresh : key{ K.compare _ pivot = 0 } @ logical)
      (left : t @ logical) (right : t @ logical) (query : key @ logical)
      : unit{
        member query (insert fresh (Node (left, pivot, right)))
        = (K.compare query fresh = 0
           || member query (Node (left, pivot, right)))
      } =
    insert_def fresh (Node (left, pivot, right));
    member_node query left pivot right;
    (* [compare fresh pivot = 0] only becomes [fresh = pivot], and so lets
       [compare query fresh] be rewritten to [compare query pivot], through
       the law. *)
    K.compare_zero_iff_equal ~left:fresh ~right:pivot;
    ()

  let member_insert_left (pivot : key @ logical)
      (fresh : key{ K.compare _ pivot < 0 } @ logical)
      (left : t @ logical) (right : t @ logical) (query : key @ logical)
      (_induction : unit{
         member query (insert fresh left)
         = (K.compare query fresh = 0 || member query left)
       })
      : unit{
        member query (insert fresh (Node (left, pivot, right)))
        = (K.compare query fresh = 0
           || member query (Node (left, pivot, right)))
      } =
    insert_def fresh (Node (left, pivot, right));
    member_node query left pivot right;
    member_node query (insert fresh left) pivot right;
    (* If [query = fresh] then [query] is on the left spine, because
       [fresh < pivot]. *)
    K.compare_zero_iff_equal ~left:query ~right:fresh;
    ()

  let member_insert_right (pivot : key @ logical)
      (fresh : key{ K.compare _ pivot <> 0 && not (K.compare _ pivot < 0) }
                 @ logical)
      (left : t @ logical) (right : t @ logical) (query : key @ logical)
      (_induction : unit{
         member query (insert fresh right)
         = (K.compare query fresh = 0 || member query right)
       })
      : unit{
        member query (insert fresh (Node (left, pivot, right)))
        = (K.compare query fresh = 0
           || member query (Node (left, pivot, right)))
      } =
    insert_def fresh (Node (left, pivot, right));
    member_node query left pivot right;
    member_node query left pivot (insert fresh right);
    K.compare_zero_iff_equal ~left:query ~right:fresh;
    ()

  let rec member_insert (fresh : key @ logical) (tree : t @ logical)
      (query : key @ logical)
      : unit{
        member query (insert fresh tree)
        = (K.compare query fresh = 0 || member query tree)
      } =
    match tree with
    | Empty -> member_insert_empty fresh query
    | Node (left, pivot, right) ->
      let choice = direction fresh pivot in
      direction_def fresh pivot;
      match choice with
      | Same -> member_insert_same pivot fresh left right query
      | Left ->
        let induction = member_insert fresh left query in
        member_insert_left pivot fresh left right query induction
      | Right ->
        let induction = member_insert fresh right query in
        member_insert_right pivot fresh left right query induction

  let empty_law ~(query : key @ logical)
      : unit{ member query empty = false } =
    member_def query empty;
    ()

  let empty_invariant : unit{ invariant empty = true } =
    invariant_def empty;
    ordered_def empty;
    ()

  let insert_invariant ~(inserted : key @ logical) ~(set : t @ logical)
      ~(well_formed : unit{ invariant set = true })
      : unit{ invariant (insert inserted set) = true } =
    invariant_def set;
    invariant_def (insert inserted set);
    insert_ordered inserted set ();
    ()

  let insert_law ~(inserted : key @ logical) ~(set : t @ logical)
      ~(query : key @ logical)
      ~(well_formed : unit{ invariant set = true })
      : unit{
        member query (insert inserted set)
        = ((query = inserted) || member query set)
      } =
    member_insert inserted set query;
    (* The one place the comparison has to be turned into equality of keys. *)
    K.compare_zero_iff_equal ~left:query ~right:inserted;
    ()

  let[@vox.def] rec agrees (left : t @ logical) (right : t @ logical)
      (nodes : t @ logical) =
    match nodes with
    | Empty -> true
    | Node (subleft, pivot, subright) ->
      let first_member = member pivot left in
      let second_member = member pivot right in
      if first_member
      then
        if second_member
        then
          if agrees left right subleft
          then agrees left right subright
          else false
        else false
      else if second_member
      then false
      else if agrees left right subleft
      then agrees left right subright
      else false

  let[@vox.def] equal (left : t @ logical) (right : t @ logical) =
    if agrees left right left then agrees left right right else false

  let agrees_node ~(left : t @ logical) ~(right : t @ logical)
      ~(subleft : t @ logical) ~(pivot : key @ logical)
      ~(subright : t @ logical)
      ~proof:(_proof : unit{
         agrees left right (Node (subleft, pivot, subright)) = true
       })
      : unit{
        member pivot left = member pivot right
        && agrees left right subleft = true
        && agrees left right subright = true
      } =
    agrees_def left right (Node (subleft, pivot, subright));
    ()

  let rec agrees_member ~(left : t @ logical) ~(right : t @ logical)
      ~(nodes : t @ logical) ~(query : key @ logical)
      ~(agreement : unit{ agrees left right nodes = true })
      ~(present : unit{ member query nodes = true })
      : unit{ member query left = member query right } =
    match nodes with
    | Empty ->
      member_def query Empty;
      ()
    | Node (subleft, pivot, subright) ->
      let facts =
        agrees_node ~left ~right ~subleft ~pivot ~subright ~proof:agreement
      in
      member_def query (Node (subleft, pivot, subright));
      let choice = direction query pivot in
      direction_def query pivot;
      K.compare_zero_iff_equal ~left:query ~right:pivot;
      match choice with
      | Same -> facts
      | Left ->
        agrees_member ~left ~right ~nodes:subleft ~query
          ~agreement:facts ~present:()
      | Right ->
        agrees_member ~left ~right ~nodes:subright ~query
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
      agrees_member ~left ~right ~nodes:left ~query
        ~agreement:() ~present:()
    | Second ->
      agrees_member ~left ~right ~nodes:right ~query
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
    let rec prove nodes : unit{ agrees left right nodes = true } =
      match nodes with
      | Empty ->
        agrees_def left right Empty;
        ()
      | Node (subleft, pivot, subright) ->
        pointwise ~query:pivot;
        prove subleft;
        prove subright;
        agrees_def left right (Node (subleft, pivot, subright));
        ()
    in
    prove left;
    prove right;
    equal_def left right;
    ()
end
