module M : Bal_intf.REMOVING_SET = struct
type t =
  | Nil
  | Cons of int * t

external int_equal : int -> int -> bool @@ total = "%equal"

let empty = Nil

let[@vox.def] rec member (query : int) (set : t @ logical) =
  match set with
  | Nil -> false
  | Cons (key, rest) ->
    if int_equal query key then true else member query rest

let[@vox.def] insert (inserted : int) (set : t @ logical) =
  if member inserted set then set else Cons (inserted, set)

(* No key is repeated: the list really is a set, and every key occupies
   exactly one cell.

   [member] here is a full scan, so it is occurrence-exact and
   [insert_law] holds whether or not the list is unique.  No law over
   [Set_intf.SET]'s four operations forces this predicate, and that was
   once recorded as a proof that nothing could.  [remove_law] at the
   foot of this file forces it: [remove] deletes the first occurrence,
   so on a list holding two copies of a key the second survives the
   removal and the law is false rather than merely unproved. *)
let[@vox.def] rec unique (set : t @ logical) =
  match set with
  | Nil -> true
  | Cons (key, rest) -> if member key rest then false else unique rest

let[@vox.def] invariant (_set : t @ logical) = true

let empty_invariant : unit{ invariant empty = true } =
  let _invariant = invariant_def empty in
  let _definition = unique_def Nil in
  ()

let insert_invariant ~(inserted : int) ~(tree : t @ logical)
    ~(well_formed : unit{ invariant tree = true })
    : unit{ invariant (insert inserted tree) = true } =
  let _tree = invariant_def tree in
  let _result = invariant_def (insert inserted tree) in
  let _insert = insert_def inserted tree in
  let present = member inserted tree in
  match present with
  | true -> ()
  | false ->
    let _definition = unique_def (Cons (inserted, tree)) in
    ()

type membership_side =
  | First
  | Second
  | Neither

let[@vox.def] membership_side first_member second_member =
  if first_member
  then First
  else if second_member then Second else Neither

let[@vox.def] rec agrees (t1 : t @ logical) (t2 : t @ logical)
    (nodes : t @ logical) =
  match nodes with
  | Nil -> true
  | Cons (key, rest) ->
    let first_member = member key t1 in
    let second_member = member key t2 in
    if first_member
    then
      if second_member then agrees t1 t2 rest else false
    else if second_member
    then false
    else agrees t1 t2 rest

let[@vox.def] equal (t1 : t @ logical) (t2 : t @ logical) =
  if agrees t1 t2 t1 then agrees t1 t2 t2 else false

let empty_law ~(query : int)
    : unit{ member query empty = false } =
  let _definition = member_def query Nil in
  ()

let insert_law ~(inserted : int) ~(tree : t @ logical) ~(query : int)
    ~(well_formed : unit{ invariant tree = true })
    : unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    } =
  let _insert = insert_def inserted tree in
  let _new_member = member_def query (Cons (inserted, tree)) in
  if int_equal query inserted then () else ()

let agrees_cons ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(key : int) ~(rest : t @ logical)
    ~proof:(_proof : unit{ agrees t1 t2 (Cons (key, rest)) = true })
    : unit{
      member key t1 = member key t2
      && agrees t1 t2 rest = true
  } =
  let _definition = agrees_def t1 t2 (Cons (key, rest)) in
  ()

let rec agrees_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(nodes : t @ logical) ~(query : int)
    ~(agreement : unit{ agrees t1 t2 nodes = true })
    ~(present : unit{ member query nodes = true })
    : unit{ member query t1 = member query t2 } =
  match nodes with
  | Nil ->
    let _member = member_def query Nil in
    ()
  | Cons (key, rest) ->
    let facts = agrees_cons ~t1 ~t2 ~key ~rest ~proof:agreement in
    let _member = member_def query (Cons (key, rest)) in
    if int_equal query key
    then facts
    else
      agrees_member ~t1 ~t2 ~nodes:rest ~query
        ~agreement:facts ~present:()

let prove_equal_member ~(t1 : t @ logical)
    ~(t2 : t{ equal t1 _ = true } @ logical)
    ~(query : int)
    : unit{ member query t1 = member query t2 } =
  let _definition = equal_def t1 t2 in
  let first_member = member query t1 in
  let second_member = member query t2 in
  let side = membership_side first_member second_member in
  let _side = membership_side_def first_member second_member in
  match side with
  | First ->
    agrees_member ~t1 ~t2 ~nodes:t1 ~query
      ~agreement:() ~present:()
  | Second ->
    agrees_member ~t1 ~t2 ~nodes:t2 ~query
      ~agreement:() ~present:()
  | Neither -> ()

let equal_forward_law ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(equal_trees : unit{ equal t1 t2 = true }) ~(query : int)
    : unit{ member query t1 = member query t2 } =
  prove_equal_member ~t1 ~t2 ~query

let equal_backward_law ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(pointwise : query:int ->
                   unit{ member query t1 = member query t2 })
    : unit{ equal t1 t2 = true } =
  let rec prove nodes : unit{ agrees t1 t2 nodes = true } =
    match nodes with
    | Nil ->
      let _definition = agrees_def t1 t2 Nil in
      ()
    | Cons (key, rest) ->
      let _same_membership = pointwise ~query:key in
      let _rest = prove rest in
      let _definition = agrees_def t1 t2 (Cons (key, rest)) in
      ()
  in
  let _first = prove t1 in
  let _second = prove t2 in
  let _definition = equal_def t1 t2 in
  ()

(* ------------------------------------------------------------------ *)
(* The [COUNTED_SET] laws.                                             *)
(* ------------------------------------------------------------------ *)

let[@vox.def] rec size (set : t @ logical) : Bigint.t =
  match set with
  | Nil -> Bigint.zero
  | Cons (_, rest) -> Bigint.add Bigint.one (size rest)

let size_empty : unit{ size empty = Bigint.zero } =
  size_def Nil;
  ()

let size_insert ~(inserted : int) ~(tree : t @ logical)
    ~(well_formed : unit{ invariant tree = true })
    : unit{
      size (insert inserted tree)
      = (if member inserted tree
         then size tree
         else Bigint.add (size tree) Bigint.one)
    } =
  insert_def inserted tree;
  let present = member inserted tree in
  match present with
  | true -> ()
  | false -> size_def (Cons (inserted, tree)); ()

(* ------------------------------------------------------------------ *)
(* The [REMOVING_SET] law.  This is what makes [unique] load-bearing.  *)
(* ------------------------------------------------------------------ *)

let[@vox.def] rec remove (removed : int) (set : t @ logical) : t =
  match set with
  | Nil -> Nil
  | Cons (key, rest) ->
    if int_equal key removed then rest else Cons (key, remove removed rest)

(* [remove] drops one cell.  The equal-key arm is where [unique] does
   the work: it is what says no later cell holds the removed key. *)
let rec remove_step (removed : int) (set : t @ logical) (query : int)
    (_well_formed : unit{ unique set = true })
    : unit{
      member query (remove removed set)
      = ((query <> removed) && member query set)
    } =
  match set with
  | Nil ->
    remove_def removed Nil;
    member_def query Nil;
    ()
  | Cons (key, rest) ->
    remove_def removed (Cons (key, rest));
    unique_def (Cons (key, rest));
    member_def query (Cons (key, rest));
    if int_equal key removed
    then ()
    else begin
      remove_step removed rest query ();
      member_def query (Cons (key, remove removed rest));
      ()
    end

let remove_law ~(removed : int) ~(tree : t @ logical) ~(query : int)
    ~(well_formed : unit{ invariant tree = true })
    : unit{
      member query (remove removed tree)
      = ((query <> removed) && member query tree)
    } =
  invariant_def tree;
  remove_step removed tree query ()

(* ------------------------------------------------------------------ *)
(* The [CARDINAL_SET] law: extensionally equal well-formed values     *)
(* have the same size.  [subset] is internal, and [remove] and        *)
(* [remove_step] are the ones already above.                          *)
(* ------------------------------------------------------------------ *)

let rec size_nonneg (set : t @ logical)
    : unit{ Bigint.le Bigint.zero (size set) = true } =
  match set with
  | Nil -> size_def Nil; ()
  | Cons (_key, rest) -> size_def (Cons (_key, rest)); size_nonneg rest; ()

let[@vox.def] rec subset (a : t @ logical) (b : t @ logical) =
  match a with
  | Nil -> true
  | Cons (key, rest) -> member key b && subset rest b

let rec remove_unique (removed : int) (set : t @ logical)
    (_well_formed : unit{ unique set = true })
    : unit{ unique (remove removed set) = true } =
  match set with
  | Nil ->
    remove_def removed Nil;
    ()
  | Cons (key, rest) ->
    remove_def removed (Cons (key, rest));
    unique_def (Cons (key, rest));
    if int_equal key removed
    then ()
    else begin
      remove_unique removed rest ();
      remove_step removed rest key ();
      unique_def (Cons (key, remove removed rest));
      ()
    end

let rec remove_size (removed : int) (set : t @ logical)
    (_well_formed : unit{ unique set = true })
    (_present : unit{ member removed set = true })
    : unit{ size set = Bigint.add (size (remove removed set)) Bigint.one } =
  match set with
  | Nil ->
    member_def removed Nil;
    ()
  | Cons (key, rest) ->
    remove_def removed (Cons (key, rest));
    member_def removed (Cons (key, rest));
    size_def (Cons (key, rest));
    unique_def (Cons (key, rest));
    if int_equal key removed
    then ()
    else begin
      remove_size removed rest () ();
      size_def (Cons (key, remove removed rest));
      ()
    end

let rec subset_cons (s : t @ logical) (x : int) (b : t @ logical)
    (_sub : unit{ subset s b = true })
    : unit{ subset s (Cons (x, b)) = true } =
  match s with
  | Nil ->
    subset_def Nil (Cons (x, b));
    ()
  | Cons (key, rest) ->
    subset_def (Cons (key, rest)) b;
    subset_def (Cons (key, rest)) (Cons (x, b));
    member_def key (Cons (x, b));
    subset_cons rest x b ();
    ()

let rec subset_self (a : t @ logical) : unit{ subset a a = true } =
  match a with
  | Nil ->
    subset_def Nil Nil;
    ()
  | Cons (key, rest) ->
    subset_def (Cons (key, rest)) (Cons (key, rest));
    member_def key (Cons (key, rest));
    subset_self rest;
    subset_cons rest key rest ();
    ()

let rec subset_remove (s : t @ logical) (b : t @ logical) (x : int)
    (_sub : unit{ subset s b = true })
    (_fresh : unit{ member x s = false })
    (_well_formed : unit{ unique b = true })
    : unit{ subset s (remove x b) = true } =
  match s with
  | Nil ->
    subset_def Nil (remove x b);
    ()
  | Cons (key, rest) ->
    subset_def (Cons (key, rest)) b;
    subset_def (Cons (key, rest)) (remove x b);
    member_def x (Cons (key, rest));
    remove_step x b key ();
    subset_remove rest b x () () ();
    ()

let rec subset_size (a : t @ logical) (b : t @ logical)
    (_unique_a : unit{ unique a = true })
    (_unique_b : unit{ unique b = true })
    (_sub : unit{ subset a b = true })
    : unit{ Bigint.le (size a) (size b) = true } =
  match a with
  | Nil ->
    size_def Nil;
    size_nonneg b;
    ()
  | Cons (key, rest) ->
    subset_def (Cons (key, rest)) b;
    unique_def (Cons (key, rest));
    size_def (Cons (key, rest));
    remove_size key b () ();
    remove_unique key b ();
    subset_remove rest b key () () ();
    subset_size rest (remove key b) () () ();
    ()

let rec subset_of_agreement (t1 : t @ logical) (t2 : t @ logical)
    (nodes : t @ logical)
    (_agreement : unit{ agrees t1 t2 nodes = true })
    (_sub : unit{ subset nodes t1 = true })
    : unit{ subset nodes t2 = true } =
  match nodes with
  | Nil ->
    subset_def Nil t2;
    ()
  | Cons (key, rest) ->
    let facts = agrees_cons ~t1 ~t2 ~key ~rest ~proof:_agreement in
    subset_def (Cons (key, rest)) t1;
    subset_def (Cons (key, rest)) t2;
    subset_of_agreement t1 t2 rest facts ();
    ()

let rec subset_of_agreement_flip (t1 : t @ logical) (t2 : t @ logical)
    (nodes : t @ logical)
    (_agreement : unit{ agrees t1 t2 nodes = true })
    (_sub : unit{ subset nodes t2 = true })
    : unit{ subset nodes t1 = true } =
  match nodes with
  | Nil ->
    subset_def Nil t1;
    ()
  | Cons (key, rest) ->
    let facts = agrees_cons ~t1 ~t2 ~key ~rest ~proof:_agreement in
    subset_def (Cons (key, rest)) t2;
    subset_def (Cons (key, rest)) t1;
    subset_of_agreement_flip t1 t2 rest facts ();
    ()

let equal_size ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(well_formed_1 : unit{ invariant t1 = true })
    ~(well_formed_2 : unit{ invariant t2 = true })
    ~(equal_trees : unit{ equal t1 t2 = true })
    : unit{ size t1 = size t2 } =
  invariant_def t1;
  invariant_def t2;
  equal_def t1 t2;
  subset_self t1;
  subset_self t2;
  subset_of_agreement t1 t2 t1 () ();
  subset_of_agreement_flip t1 t2 t2 () ();
  subset_size t1 t2 () () ();
  subset_size t2 t1 () () ();
  ()
end
