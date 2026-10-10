module C = Vox_partition_classes
module B = Vox_partition
module L = Vox_partition_classes_proof

let[@def] rec emit (xs : ('a : logical_data) list @ immutable)
    (r : 'a @ immutable) (tail : 'a B.bindings @ immutable) = ghost_ (
  match xs with [] -> tail | x :: rest -> (x, r) :: emit rest r tail)

let[@def] rec flatten (p : ('a : logical_data) C.classes @ immutable) = ghost_ (
  match p with
  | [] -> []
  | (r, others) :: rest -> (r, r) :: emit others r (flatten rest))

let rec (emit_lookup @ total) :
    (xs : ('a : logical_data) list) @ immutable -> (r : 'a) @ immutable ->
    (tail : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | B.lookup (emit xs r tail) x ===
      (if C.mem xs x then Some r else B.lookup tail x)} @ ghost =
    fun xs r tail x -> ghost_ (
  emit_def xs r tail; C.mem_def xs x;
  B.lookup_def (emit xs r tail) x;
  match xs with [] -> () | _ :: rest -> emit_lookup rest r tail x)

let rec (flatten_lookup @ total) :
    (p : ('a : logical_data) C.classes) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | B.lookup (flatten p) x === C.lookup p x} @ ghost =
    fun p x -> ghost_ (
  flatten_def p; C.lookup_def p x; B.lookup_def (flatten p) x;
  match p with
  | [] -> ()
  | (r, others) :: rest ->
      emit_lookup others r (flatten rest) x; flatten_lookup rest x)

let (flatten_observation @ total) :
    (p : ('a : logical_data) C.classes) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | B.contains (flatten p) x === C.contains p x &&
      B.representative (flatten p) x === C.representative p x} @ ghost =
    fun p x -> ghost_ (
  flatten_lookup p x;
  B.contains_def (flatten p) x; C.contains_def p x;
  B.representative_def (flatten p) x; C.representative_def p x; ())

let (flatten_connected @ total) :
    (p : ('a : logical_data) C.classes) @ immutable ->
    (x : 'a) @ immutable -> (y : 'a) @ immutable ->
    {u : unit | B.connected (flatten p) x y === C.connected p x y} @ ghost =
    fun p x y -> ghost_ (
  flatten_observation p x; flatten_observation p y;
  B.connected_def (flatten p) x y; C.connected_def p x y; ())

let rec (length_append @ total) :
    (xs : ('a : logical_data) list) @ immutable -> (ys : 'a list) @ immutable ->
    {u : unit | C.length (C.append xs ys) = Bigint.add (C.length xs) (C.length
      ys)}
    @ ghost = fun xs ys -> ghost_ (
  C.append_def xs ys; C.length_def xs;
  C.length_def (C.append xs ys);
  match xs with [] -> () | _ :: rest -> length_append rest ys)

let rec (emit_size @ total) :
    (xs : ('a : logical_data) list) @ immutable -> (r : 'a) @ immutable ->
    (tail : 'a B.bindings) @ immutable ->
    {u : unit | B.size (emit xs r tail) = Bigint.add (C.length xs) (B.size
      tail)}
    @ ghost = fun xs r tail -> ghost_ (
  emit_def xs r tail; C.length_def xs; B.size_def (emit xs r tail);
  match xs with [] -> () | _ :: rest -> emit_size rest r tail)

let rec (flatten_size @ total) :
    (p : ('a : logical_data) C.classes) @ immutable ->
    {u : unit | B.size (flatten p) = C.size p} @ ghost = fun p -> ghost_ (
  flatten_def p; C.size_def p; C.members_def p;
  B.size_def (flatten p); C.length_def (C.members p);
  match p with
  | [] -> ()
  | (r, others) :: rest ->
      emit_size others r (flatten rest);
      length_append others (C.members rest); C.size_def rest; flatten_size rest)

let[@def] rec keys (p : ('a : logical_data) B.bindings @ immutable) = ghost_ (
  match p with [] -> [] | (x, _) :: rest -> x :: keys rest)

let rec (keys_emit @ total) :
    (xs : ('a : logical_data) list) @ immutable -> (r : 'a) @ immutable ->
    (tail : 'a B.bindings) @ immutable ->
    {u : unit | keys (emit xs r tail) === C.append xs (keys tail)} @ ghost =
    fun xs r tail -> ghost_ (
  emit_def xs r tail; keys_def (emit xs r tail); C.append_def xs (keys tail);
  match xs with [] -> () | _ :: rest -> keys_emit rest r tail)

let rec (keys_flatten @ total) :
    (p : ('a : logical_data) C.classes) @ immutable ->
    {u : unit | keys (flatten p) === C.members p} @ ghost = fun p -> ghost_ (
  flatten_def p; keys_def (flatten p); C.members_def p;
  match p with
  | [] -> ()
  | (r, others) :: rest -> keys_emit others r (flatten rest); keys_flatten rest)

let rec (contains_keys @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | B.contains p x === C.mem (keys p) x} @ ghost = fun p x -> ghost_
      (
  keys_def p; C.mem_def (keys p) x; B.contains_def p x; B.lookup_def p x;
  match p with
  | [] -> ()
  | (member, rep) :: rest -> B.contains_cons rest member rep x; contains_keys
    rest x)

let rec (unique_keys @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    {u : unit | B.unique p === C.distinct (keys p)} @ ghost = fun p -> ghost_ (
  B.unique_def p; keys_def p; C.distinct_def (keys p);
  match p with
  | [] -> ()
  | (x, _) :: rest -> contains_keys rest x; unique_keys rest)

let (flatten_unique @ total) :
    (p : ('a : logical_data) C.classes) @ immutable ->
    {u : unit | B.unique (flatten p) === C.distinct (C.members p)} @ ghost =
    fun p -> ghost_ (keys_flatten p; unique_keys (flatten p); ())

let rec (agree_emit @ total) :
    (before : ('a : logical_data) C.classes) @ immutable ->
    (after : 'a C.classes) @ immutable -> (xs : 'a list) @ immutable ->
    (r : 'a) @ immutable -> (tail : 'a B.bindings) @ immutable ->
    {u : unit | B.agree (flatten before) (flatten after) (emit xs r tail) ===
      (C.agree before after xs && B.agree (flatten before) (flatten after)
        tail)}
    @ ghost = fun before after xs r tail -> ghost_ (
  emit_def xs r tail; C.agree_def before after xs;
  B.agree_def (flatten before) (flatten after) (emit xs r tail);
  match xs with
  | [] -> ()
  | x :: rest ->
      flatten_lookup before x; flatten_lookup after x;
      agree_emit before after rest r tail)

let rec (agree_append @ total) :
    (before : ('a : logical_data) C.classes) @ immutable ->
    (after : 'a C.classes) @ immutable -> (xs : 'a list) @ immutable ->
    (ys : 'a list) @ immutable ->
    {u : unit | C.agree before after (C.append xs ys) ===
      (C.agree before after xs && C.agree before after ys)} @ ghost =
    fun before after xs ys -> ghost_ (
  C.append_def xs ys; C.agree_def before after xs;
  C.agree_def before after (C.append xs ys);
  match xs with [] -> () | _ :: rest -> agree_append before after rest ys)

let rec (agree_flatten @ total) :
    (before : ('a : logical_data) C.classes) @ immutable ->
    (after : 'a C.classes) @ immutable -> (entries : 'a C.classes) @ immutable
      ->
    {u : unit | B.agree (flatten before) (flatten after) (flatten entries) ===
      C.agree before after (C.members entries)} @ ghost =
    fun before after entries -> ghost_ (
  flatten_def entries; C.members_def entries;
  B.agree_def (flatten before) (flatten after) (flatten entries);
  C.agree_def before after (C.members entries);
  match entries with
  | [] -> ()
  | (r, others) :: rest ->
      flatten_lookup before r; flatten_lookup after r;
      agree_emit before after others r (flatten rest);
      agree_append before after others (C.members rest);
      agree_flatten before after rest)

let (flatten_same @ total) :
    (before : ('a : logical_data) C.classes) @ immutable ->
    (after : 'a C.classes) @ immutable ->
    {u : unit | B.same (flatten before) (flatten after) === C.same before after}
    @ ghost = fun before after -> ghost_ (
  B.same_def (flatten before) (flatten after); C.same_def before after;
  flatten_size before; flatten_size after;
  agree_flatten before after before; agree_flatten before after after; ())

let rec (agree_symmetric @ total) :
    (before : ('a : logical_data) C.classes) @ immutable ->
    (after : 'a C.classes) @ immutable -> (xs : 'a list) @ immutable ->
    {u : unit | C.agree before after xs === C.agree after before xs} @ ghost =
    fun before after xs -> ghost_ (
  C.agree_def before after xs; C.agree_def after before xs;
  match xs with [] -> () | _ :: rest -> agree_symmetric before after rest)

let (same_symmetric @ total) :
    (before : ('a : logical_data) C.classes) @ immutable ->
    (after : 'a C.classes) @ immutable ->
    {u : unit | C.same before after === C.same after before} @ ghost =
    fun before after -> ghost_ (
  C.same_def before after; C.same_def after before;
  agree_symmetric before after (C.members before);
  agree_symmetric before after (C.members after); ())

let[@def] rec observed (whole : ('a : logical_data) B.bindings @ immutable)
    (entries : 'a B.bindings @ immutable) = ghost_ (
  match entries with
  | [] -> true
  | (x, r) :: rest -> B.lookup whole x === Some r && observed whole rest)

let rec (observed_cons @ total) :
    (whole : ('a : logical_data) B.bindings) @ immutable ->
    (entries : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    (r : 'a) @ immutable ->
    {u : unit | if observed whole entries && not (B.contains entries x) then
      observed ((x, r) :: whole) entries else true} @ ghost =
    fun whole entries x r -> ghost_ (
  observed_def whole entries; observed_def ((x, r) :: whole) entries;
  B.contains_def entries x; B.lookup_def entries x;
  match entries with
  | [] -> ()
  | (q, _) :: rest ->
      B.lookup_def ((x, r) :: whole) q;
      B.contains_def rest x; observed_cons whole rest x r)

let rec (observed_self @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    {u : unit | if B.unique p then observed p p else true} @ ghost =
    fun p -> ghost_ (
  B.unique_def p; observed_def p p;
  match p with
  | [] -> ()
  | (x, r) :: rest ->
      B.lookup_def p x; observed_self rest; observed_cons rest rest x r)

let rec (closed_observed @ total) :
    (p : ('a : logical_data) C.classes) @ immutable ->
    (entries : 'a B.bindings) @ immutable ->
    {u : unit | if C.distinct (C.members p) && observed (flatten p) entries then
      B.closed (flatten p) entries else true} @ ghost = fun p entries -> ghost_
        (
  observed_def (flatten p) entries; B.closed_def (flatten p) entries;
  match entries with
  | [] -> ()
  | (x, r) :: rest ->
      flatten_lookup p x; C.contains_def p x; C.representative_def p x;
      L.lookup_representative p x r;
      C.contains_def p r; C.representative_def p r;
      flatten_observation p r; closed_observed p rest)

let (flatten_valid @ total) :
    (p : ('a : logical_data) C.classes) @ immutable ->
    {u : unit | B.valid (flatten p) === C.distinct (C.members p)} @ ghost =
    fun p -> ghost_ (
  flatten_unique p; observed_self (flatten p); closed_observed p (flatten p);
  B.valid_def (flatten p); ())

let (flatten_added @ total) :
    (before : ('a : logical_data) C.t) @ immutable ->
    (after : 'a C.t) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | B.added (flatten before) (flatten after) x === C.added before
      after x}
    @ ghost = fun before after x -> ghost_ (
  flatten_valid before; flatten_valid after; flatten_observation before x;
  B.added_def (flatten before) (flatten after) x; C.added_def before after x;
  flatten_def ((x, []) :: before); emit_def [] x (flatten before);
  B.add_singleton_def (flatten before) x;
  flatten_same after ((x, []) :: before); same_symmetric after ((x, []) ::
    before); ())

let rec (keys_redirect @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (rx : 'a) @ immutable ->
    (ry : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | keys (B.redirect p rx ry r) === keys p} @ ghost =
    fun p rx ry r -> ghost_ (
  B.redirect_def p rx ry r; keys_def p; keys_def (B.redirect p rx ry r);
  match p with [] -> () | _ :: rest -> keys_redirect rest rx ry r)

let (merge_lookup @ total) :
    (before : ('a : logical_data) C.classes) @ immutable ->
    (x : 'a) @ immutable -> (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    (q : 'a) @ immutable ->
    {u : unit | if C.contains before x && C.contains before y then
      B.lookup (B.merge_classes (flatten before) x y r) q ===
        (if C.connected before q x || C.connected before q y then Some r
         else C.lookup before q) else true} @ ghost = fun before x y r q ->
           ghost_ (
  B.merge_classes_def (flatten before) x y r;
  B.redirect_lookup (flatten before) (B.representative (flatten before) x)
    (B.representative (flatten before) y) r q;
  flatten_lookup before q; flatten_observation before q;
  flatten_observation before x; flatten_observation before y;
  C.connected_def before q x; C.connected_def before q y;
  C.contains_def before q; C.representative_def before q; ())

let rec (agree_merged @ total) :
    (before : ('a : logical_data) C.classes) @ immutable ->
    (after : 'a C.classes) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    (entries : 'a B.bindings) @ immutable ->
    {u : unit | if C.contains before x && C.contains before y then
      B.agree (flatten after) (B.merge_classes (flatten before) x y r) entries
        ===
      C.merged before after x y r (keys entries) else true} @ ghost =
    fun before after x y r entries -> ghost_ (
  keys_def entries; C.merged_def before after x y r (keys entries);
  B.agree_def (flatten after) (B.merge_classes (flatten before) x y r) entries;
  match entries with
  | [] -> ()
  | (q, _) :: rest ->
      flatten_lookup after q; merge_lookup before x y r q;
      agree_merged before after x y r rest)

let (same_merged @ total) :
    (before : ('a : logical_data) C.classes) @ immutable ->
    (after : 'a C.classes) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if C.contains before x && C.contains before y then
      B.same (flatten after) (B.merge_classes (flatten before) x y r) ===
      (C.size after = C.size before &&
       C.merged before after x y r (C.members before) &&
       C.merged before after x y r (C.members after)) else true} @ ghost =
    fun before after x y r -> ghost_ (
  B.same_def (flatten after) (B.merge_classes (flatten before) x y r);
  B.merge_classes_def (flatten before) x y r;
  B.redirect_size (flatten before) (B.representative (flatten before) x)
    (B.representative (flatten before) y) r;
  keys_redirect (flatten before) (B.representative (flatten before) x)
    (B.representative (flatten before) y) r;
  keys_flatten before; keys_flatten after;
  flatten_size before; flatten_size after;
  agree_merged before after x y r (flatten after);
  agree_merged before after x y r (B.merge_classes (flatten before) x y r); ())

let (flatten_joined @ total) :
    (before : ('a : logical_data) C.t) @ immutable ->
    (after : 'a C.t) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | B.joined (flatten before) (flatten after) x y r ===
      C.joined before after x y r} @ ghost = fun before after x y r -> ghost_ (
  flatten_valid before; flatten_valid after;
  flatten_observation before x; flatten_observation before y;
  flatten_connected before x r; flatten_connected before y r;
  flatten_connected before x y; flatten_same before after;
  same_merged before after x y r;
  B.joined_def (flatten before) (flatten after) x y r;
  C.joined_def before after x y r;
  L.joined_law before after x y r x;
  B.joined_law (flatten before) (flatten after) x y r x;
  B.same_law (flatten before) (flatten after) x;
  B.connected_def (flatten before) x x;
  B.connected_def (flatten before) x y; ())

let (flatten_empty @ total) :
    (p : ('a : logical_data) C.classes) @ immutable ->
    {u : unit | B.empty (flatten p) = C.empty p} @ ghost = fun p -> ghost_ (
  flatten_def p; B.empty_def (flatten p); C.empty_def p;
  match p with [] -> () | _ :: _ -> ())
