type ('a : logical_data) bindings = ('a * 'a) list

let[@def] rec lookup (p : ('a : logical_data) bindings @ immutable)
    (x : 'a @ immutable) = ghost_ (
  match p with
  | [] -> None
  | (member, rep) :: rest ->
      if x === member then Some rep else lookup rest x)

let[@def] contains (p : ('a : logical_data) bindings @ immutable)
    (x : 'a @ immutable) = ghost_ (
  match lookup p x with None -> false | Some _ -> true)

let[@def] representative (p : ('a : logical_data) bindings @ immutable)
    (x : 'a @ immutable) = ghost_ (
  match lookup p x with None -> x | Some rep -> rep)

let[@def] rec size (p : ('a : logical_data) bindings @ immutable) = ghost_ (
  match p with [] -> 0Z | _ :: rest -> Bigint.add 1Z (size rest))

let[@def] connected (p : ('a : logical_data) bindings @ immutable)
    (x : 'a @ immutable) (y : 'a @ immutable) = ghost_ (
  contains p x && contains p y && representative p x === representative p y)

let[@def] rec unique (p : ('a : logical_data) bindings @ immutable) = ghost_ (
  match p with
  | [] -> true
  | (member, _) :: rest -> not (contains rest member) && unique rest)

let[@def] rec closed (whole : ('a : logical_data) bindings @ immutable)
    (entries : 'a bindings @ immutable) = ghost_ (
  match entries with
  | [] -> true
  | (_, rep) :: rest ->
      contains whole rep &&
      representative whole rep === rep && closed whole rest)

let[@def] valid (p : ('a : logical_data) bindings @ immutable) = ghost_ (
  unique p && closed p p)

let[@def] add_singleton (p : ('a : logical_data) bindings @ immutable)
    (x : 'a @ immutable) = ghost_ ((x, x) :: p)

let[@def] rec redirect (p : ('a : logical_data) bindings @ immutable)
    (rx : 'a @ immutable) (ry : 'a @ immutable) (r : 'a @ immutable) = ghost_ (
  match p with
  | [] -> []
  | (member, rep) :: rest ->
      (member, if rep === rx || rep === ry then r
        else rep) :: redirect rest rx ry r)

let[@def] merge_classes (p : ('a : logical_data) bindings @ immutable)
    (x : 'a @ immutable) (y : 'a @ immutable) (r : 'a @ immutable) = ghost_ (
  redirect p (representative p x) (representative p y) r)

let[@def] empty (p : ('a : logical_data) bindings @ immutable) =
  ghost_ (p === [])

let[@def] rec agree (p : ('a : logical_data) bindings @ immutable)
    (q : 'a bindings @ immutable) (entries : 'a bindings @ immutable) = ghost_ (
  match entries with
  | [] -> true
  | (x, _) :: rest -> lookup p x === lookup q x && agree p q rest)

let[@def] same (p : ('a : logical_data) bindings @ immutable)
    (q : 'a bindings @ immutable) = ghost_ (
  size p = size q && agree p q p && agree p q q)

let[@def] added (before : ('a : logical_data) bindings @ immutable)
    (after : 'a bindings @ immutable) (x : 'a @ immutable) = ghost_ (
  valid before && valid after && not (contains before x) &&
  same after (add_singleton before x))

let[@def] joined (before : ('a : logical_data) bindings @ immutable)
    (after : 'a bindings @ immutable) (x : 'a @ immutable) (y : 'a @ immutable)
    (r : 'a @ immutable) = ghost_ (
  valid before && valid after && contains before x && contains before y &&
  (connected before x r || connected before y r) &&
  (if connected before x y then same before after else true) &&
  same after (merge_classes before x y r))


let (contains_cons @ total) : (p : ('a : logical_data) bindings) @ immutable ->
    (member : 'a) @ immutable -> (rep : 'a) @ immutable ->
    (x : 'a) @ immutable ->
    {u : unit | contains ((member, rep) :: p) x ===
      (x === member || contains p x)} @ ghost =
    fun p member rep x -> ghost_ (
  contains_def ((member, rep) :: p) x;
  lookup_def ((member, rep) :: p) x;
  contains_def p x;
  ())

let (root_cons @ total) : (p : ('a : logical_data) bindings) @ immutable ->
    (member : 'a) @ immutable -> (rep : 'a) @ immutable ->
    (x : 'a) @ immutable ->
    {u : unit | representative ((member, rep) :: p) x ===
      (if x === member then rep else representative p x)} @ ghost =
    fun p member rep x -> ghost_ (
  representative_def ((member, rep) :: p) x;
  lookup_def ((member, rep) :: p) x;
  representative_def p x;
  ())

let (absent @ total) : (p : ('a : logical_data) bindings) @ immutable ->
    (x : 'a) @ immutable ->
    {u : unit | if not (contains p x) then representative p x === x else true}
    @ ghost = fun p x -> ghost_ (
  contains_def p x; representative_def p x;
  ())

let rec (closed_lookup @ total) :
    (whole : ('a : logical_data) bindings) @ immutable ->
    (entries : 'a bindings) @ immutable -> (x : 'a) @ immutable ->
    (r : 'a) @ immutable ->
    {u : unit | if closed whole entries && lookup entries x === Some r then
      contains whole r && representative whole r === r else true} @ ghost =
    fun whole entries x r -> ghost_ (
  closed_def whole entries; lookup_def entries x;
  match entries with
  | [] -> ()
  | (_, _) :: rest -> closed_lookup whole rest x r)

let (representative_law @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if valid p && contains p x then
      contains p (representative p x) && representative p (representative p x)
        === representative p x else true}
    @ ghost = fun p x -> ghost_ (
  valid_def p; contains_def p x; representative_def p x;
  (match lookup p x with
  | None -> ()
  | Some r -> closed_lookup p p x r);
  ())

let rec (size_nonnegative @ total) :
    (p : ('a : logical_data) bindings) @ immutable ->
    {u : unit | size p >= 0Z} @ ghost = fun p -> ghost_ (
  size_def p;
  match p with [] -> () | _ :: rest -> size_nonnegative rest)

let (empty_model_law @ total) : (x : ('a : logical_data)) @ immutable ->
    {u : unit | valid ([] : 'a bindings) && size ([] : 'a bindings) = 0Z &&
      not (contains ([]) x) && representative ([]) x === x} @ ghost =
    fun x -> ghost_ (
  let p : 'a bindings = [] in
  valid_def p; unique_def p; closed_def p p;
  size_def p; contains_def p x; lookup_def p x; representative_def p x;
  ())

let (add_singleton_law @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (x : 'a) @ immutable ->
    (q : 'a) @ immutable ->
    {u : unit | contains (add_singleton p x) q ===
      (q === x || contains p q) &&
      representative (add_singleton p x) q ===
        (if q === x then x else representative p q) &&
      size (add_singleton p x) = Bigint.add 1Z (size p)} @ ghost =
    fun p x q -> ghost_ (
  add_singleton_def p x;
  contains_cons p x x q; root_cons p x x q;
  size_def ((x, x) :: p);
  ())

let rec (redirect_lookup @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (rx : 'a) @ immutable ->
    (ry : 'a) @ immutable -> (r : 'a) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | lookup (redirect p rx ry r) q ===
      (match lookup p q with
       | None -> None
       | Some old -> Some (if old === rx || old === ry then r else old))}
    @ ghost = fun p rx ry r q -> ghost_ (
  redirect_def p rx ry r; lookup_def p q;
  lookup_def (redirect p rx ry r) q;
  match p with
  | [] -> ()
  | _ :: rest -> redirect_lookup rest rx ry r q)

let (redirect_contains @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (rx : 'a) @ immutable ->
    (ry : 'a) @ immutable -> (r : 'a) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | contains (redirect p rx ry r) q === contains p q}
    @ ghost = fun p rx ry r q -> ghost_ (
  redirect_lookup p rx ry r q;
  contains_def p q; contains_def (redirect p rx ry r) q;
  ())

let (redirect_root @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (rx : 'a) @ immutable ->
    (ry : 'a) @ immutable -> (r : 'a) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | representative (redirect p rx ry r) q ===
      (if contains p q && (representative p q === rx || representative p q ===
        ry)
       then r else representative p q)} @ ghost = fun p rx ry r q -> ghost_ (
  redirect_lookup p rx ry r q;
  contains_def p q; representative_def p q;
  representative_def (redirect p rx ry r) q;
  ())

let rec (redirect_size @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (rx : 'a) @ immutable ->
    (ry : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | size (redirect p rx ry r) = size p} @ ghost =
    fun p rx ry r -> ghost_ (
  redirect_def p rx ry r; size_def p;
  size_def (redirect p rx ry r);
  match p with [] -> () | _ :: rest -> redirect_size rest rx ry r)

let (merge_classes_law @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | contains (merge_classes p x y r) q === contains p q &&
      size (merge_classes p x y r) = size p &&
      representative (merge_classes p x y r) q ===
        (if contains p q &&
          (representative p q === representative p x || representative p q ===
            representative p y)
         then r else representative p q)} @ ghost = fun p x y r q -> ghost_ (
  merge_classes_def p x y r;
  redirect_contains p (representative p x) (representative p y) r q;
  redirect_root p (representative p x) (representative p y) r q;
  redirect_size p (representative p x) (representative p y) r;
  ())

let rec (add_singleton_closed @ total) :
    (p : ('a : logical_data) bindings) @ immutable ->
    (entries : 'a bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if closed p entries && not (contains p x) then
      closed (add_singleton p x) entries else true} @ ghost =
    fun p entries x -> ghost_ (
  add_singleton_def p x;
  closed_def p entries; closed_def ((x, x) :: p) entries;
  match entries with
  | [] -> ()
  | (_, rep) :: rest ->
      contains_cons p x x rep;
      root_cons p x x rep;
      add_singleton_closed p rest x)

let (add_singleton_valid @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if valid p && not (contains p x) then
      valid (add_singleton p x) else true} @ ghost = fun p x -> ghost_ (
  valid_def p; add_singleton_def p x;
  let after = (x, x) :: p in
  add_singleton_closed p p x;
  contains_cons p x x x; root_cons p x x x;
  unique_def after; closed_def after after; valid_def after;
  ())

let rec (redirect_unique @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (rx : 'a) @ immutable ->
    (ry : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | unique (redirect p rx ry r) === unique p} @ ghost =
    fun p rx ry r -> ghost_ (
  redirect_def p rx ry r; unique_def p;
  unique_def (redirect p rx ry r);
  match p with
  | [] -> ()
  | (member, _) :: rest ->
      redirect_contains rest rx ry r member;
      redirect_unique rest rx ry r)

let rec (redirect_closed @ total) :
    (p : ('a : logical_data) bindings) @ immutable ->
    (entries : 'a bindings) @ immutable -> (rx : 'a) @ immutable ->
    (ry : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if closed p entries && contains p r && representative p r === r
      &&
      (r === rx || r === ry) then
      closed (redirect p rx ry r) (redirect entries rx ry r) else true}
    @ ghost = fun p entries rx ry r -> ghost_ (
  closed_def p entries; redirect_def entries rx ry r;
  closed_def (redirect p rx ry r) (redirect entries rx ry r);
  match entries with
  | [] -> ()
  | (_, rep) :: rest ->
      redirect_contains p rx ry r r;
      redirect_root p rx ry r r;
      redirect_contains p rx ry r rep;
      redirect_root p rx ry r rep;
      redirect_closed p rest rx ry r)

let (merge_classes_valid @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if valid p && contains p x && contains p y &&
      (r === representative p x || r === representative p y) then
      valid (merge_classes p x y r) else true} @ ghost =
    fun p x y r -> ghost_ (
  representative_law p x; representative_law p y;
  valid_def p; merge_classes_def p x y r;
  redirect_unique p (representative p x) (representative p y) r;
  redirect_closed p p (representative p x) (representative p y) r;
  valid_def (redirect p (representative p x) (representative p y) r);
  ())

let (empty_size @ total) : (p : ('a : logical_data) bindings) @ immutable ->
    {u : unit | if empty p then valid p && size p = 0Z else true}
    @ ghost = fun p -> ghost_ (
  empty_def p; valid_def p; unique_def p; closed_def p p; size_def p; ())

let (empty_law @ total) : (p : ('a : logical_data) bindings) @ immutable ->
    (x : 'a) @ immutable ->
    {u : unit | if empty p then valid p && size p = 0Z &&
      not (contains p x) && representative p x === x else true} @ ghost =
    fun p x -> ghost_ (empty_def p; empty_model_law x; ())

let rec (agree_refl @ total) : (p : ('a : logical_data) bindings) @ immutable ->
    (entries : 'a bindings) @ immutable ->
    {u : unit | agree p p entries} @ ghost = fun p entries -> ghost_ (
  agree_def p p entries;
  match entries with [] -> () | _ :: rest -> agree_refl p rest)

let (same_refl @ total) : (p : ('a : logical_data) bindings) @ immutable ->
    {u : unit | same p p} @ ghost = fun p -> ghost_ (
  agree_refl p p; same_def p p; ())

let rec (agree_lookup @ total) : (p : ('a : logical_data) bindings) @ immutable
  ->
    (q : 'a bindings) @ immutable -> (entries : 'a bindings) @ immutable ->
    (x : 'a) @ immutable ->
    {u : unit | if agree p q entries && contains entries x then
      lookup p x === lookup q x else true} @ ghost =
    fun p q entries x -> ghost_ (
  agree_def p q entries; contains_def entries x; lookup_def entries x;
  match entries with
  | [] -> ()
  | (_, _) :: rest -> contains_def rest x; agree_lookup p q rest x)

let (same_law @ total) : (before : ('a : logical_data) bindings) @ immutable ->
    (after : 'a bindings) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | if same before after then
      contains after q = contains before q &&
      representative after q === representative before q && size after = size
        before else true}
    @ ghost = fun before after q -> ghost_ (
  same_def before after;
  agree_lookup before after before q; agree_lookup before after after q;
  contains_def before q; contains_def after q;
  representative_def before q; representative_def after q;
  ())

let (added_intro @ total) : (before : ('a : logical_data) bindings) @ immutable
  ->
    (x : 'a) @ immutable ->
    {u : unit | if valid before && not (contains before x) then
      added before (add_singleton before x) x else true} @ ghost =
    fun before x -> ghost_ (
  add_singleton_valid before x; same_refl (add_singleton before x);
  added_def before (add_singleton before x) x; ())

let (added_law @ total) : (before : ('a : logical_data) bindings) @ immutable ->
    (after : 'a bindings) @ immutable -> (x : 'a) @ immutable ->
    (q : 'a) @ immutable ->
    {u : unit | if added before after x then valid before && valid after &&
      not (contains before x) &&
      contains after q = (q === x || contains before q) &&
      representative after q === (if q === x then x else representative before
        q) &&
      size after = Bigint.add 1Z (size before) else true} @ ghost =
    fun before after x q -> ghost_ (
  added_def before after x;
  same_law after (add_singleton before x) q;
  add_singleton_law before x q; ())

let rec (redirect_identity @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | redirect p r r r === p} @ ghost = fun p r -> ghost_ (
  redirect_def p r r r;
  match p with [] -> () | _ :: rest -> redirect_identity rest r)

let (joined_intro @ total) : (before : ('a : logical_data) bindings) @ immutable
  ->
    (x : 'a) @ immutable -> (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if valid before && contains before x && contains before y &&
      (r === representative before x || r === representative before y) then
      joined before (merge_classes before x y r) x y r else true} @ ghost =
    fun before x y r -> ghost_ (
  representative_law before x; representative_law before y;
  connected_def before x r; connected_def before y r;
  connected_def before x y;
  merge_classes_valid before x y r;
  merge_classes_def before x y r;
  redirect_identity before r;
  same_refl before; same_refl (merge_classes before x y r);
  joined_def before (merge_classes before x y r) x y r; ())

let (joined_law @ total) : (before : ('a : logical_data) bindings) @ immutable
  ->
    (after : 'a bindings) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | if joined before after x y r then
      valid before && valid after && contains before x && contains before y &&
      (connected before x r || connected before y r) &&
      (if connected before x y then same before after else true) &&
      contains after q = contains before q && size after = size before &&
      representative after q ===
        (if connected before q x || connected before q y then r
         else representative before q) else true} @ ghost =
    fun before after x y r q -> ghost_ (
  joined_def before after x y r;
  same_law after (merge_classes before x y r) q;
  merge_classes_law before x y r q;
  connected_def before q x; connected_def before q y; ())

let (joined_connected @ total) : (before : ('a : logical_data) bindings) @
  immutable ->
    (after : 'a bindings) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    (a : 'a) @ immutable -> (b : 'a) @ immutable ->
    {u : unit | if joined before after x y r then
      connected after a b = (connected before a b ||
        (connected before a x && connected before b y) ||
        (connected before a y && connected before b x)) else true}
    @ ghost = fun before after x y r a b -> ghost_ (
  joined_law before after x y r a;
  joined_law before after x y r b;
  representative_law before a; representative_law before b;
  connected_def before x r; connected_def before y r;
  connected_def before a b; connected_def after a b;
  connected_def before a x; connected_def before a y;
  connected_def before b x; connected_def before b y;
  ())

let rec (redirect_closed_member @ total) :
    (p : ('a : logical_data) bindings) @ immutable ->
    (entries : 'a bindings) @ immutable -> (rx : 'a) @ immutable ->
    (ry : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if closed p entries && contains p r &&
      (representative p r === rx || representative p r === ry) then
      closed (redirect p rx ry r) (redirect entries rx ry r) else true}
    @ ghost = fun p entries rx ry r -> ghost_ (
  closed_def p entries; redirect_def entries rx ry r;
  closed_def (redirect p rx ry r) (redirect entries rx ry r);
  match entries with
  | [] -> ()
  | (_, rep) :: rest ->
      redirect_contains p rx ry r r; redirect_root p rx ry r r;
      redirect_contains p rx ry r rep; redirect_root p rx ry r rep;
      redirect_closed_member p rest rx ry r)

let (merge_classes_valid_member @ total) :
    (p : ('a : logical_data) bindings) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if valid p && contains p x && contains p y &&
      (connected p x r || connected p y r) then
      valid (merge_classes p x y r) else true} @ ghost =
    fun p x y r -> ghost_ (
  connected_def p x r; connected_def p y r;
  valid_def p; merge_classes_def p x y r;
  redirect_unique p (representative p x) (representative p y) r;
  redirect_closed_member p p (representative p x) (representative p y) r;
  valid_def (redirect p (representative p x) (representative p y) r); ())

let (joined_member_intro @ total) :
    (before : ('a : logical_data) bindings) @ immutable ->
    (x : 'a) @ immutable -> (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if valid before && contains before x && contains before y &&
      (connected before x r || connected before y r) &&
      (if connected before x y then r === representative before x else true)
      then joined before (merge_classes before x y r) x y r else true}
    @ ghost = fun before x y r -> ghost_ (
  merge_classes_valid_member before x y r;
  connected_def before x y; merge_classes_def before x y r;
  redirect_identity before r;
  same_refl before; same_refl (merge_classes before x y r);
  joined_def before (merge_classes before x y r) x y r; ())

type ('a : logical_data) t = {p : 'a bindings | valid p}
