open Vox_partition_classes

let rec (mem_append @ total) : (xs : ('a : logical_data) list) @ immutable ->
    (ys : 'a list) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | mem (append xs ys) x = (mem xs x || mem ys x)} @ ghost =
    fun xs ys x -> ghost_ (
  append_def xs ys; mem_def xs x; mem_def (append xs ys) x;
  match xs with [] -> () | _ :: rest -> mem_append rest ys x)

let rec (contains_members @ total) : (p : ('a : logical_data) classes) @
  immutable ->
    (x : 'a) @ immutable ->
    {u : unit | contains p x = mem (members p) x} @ ghost = fun p x -> ghost_ (
  contains_def p x; lookup_def p x; members_def p; mem_def (members p) x;
  match p with
  | [] -> ()
  | (_, others) :: rest ->
      mem_append others (members rest) x; contains_members rest x;
      contains_def rest x)

let rec (length_nonnegative @ total) : (xs : ('a : logical_data) list) @
  immutable ->
    {u : unit | length xs >= 0Z} @ ghost = fun xs -> ghost_ (
  length_def xs; match xs with [] -> () | _ :: rest -> length_nonnegative rest)

let (size_nonnegative @ total) : (p : ('a : logical_data) classes) @ immutable
  ->
    {u : unit | size p >= 0Z} @ ghost = fun p -> ghost_ (
  size_def p; length_nonnegative (members p))

let rec (distinct_append @ total) : (xs : ('a : logical_data) list) @ immutable
  ->
    (ys : 'a list) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if distinct (append xs ys) then distinct xs && distinct ys &&
      not (mem xs x && mem ys x) else true} @ ghost = fun xs ys x -> ghost_ (
  append_def xs ys; distinct_def (append xs ys); distinct_def xs;
  mem_def xs x;
  match xs with
  | [] -> ()
  | a :: rest -> mem_append rest ys a; distinct_append rest ys x)

let rec (lookup_representative @ total) :
    (p : ('a : logical_data) classes) @ immutable -> (x : 'a) @ immutable ->
    (r : 'a) @ immutable ->
    {u : unit | if distinct (members p) && lookup p x === Some r then
      lookup p r === Some r else true} @ ghost = fun p x r -> ghost_ (
  lookup_def p x; lookup_def p r; members_def p;
  distinct_def (members p);
  match p with
  | [] -> ()
  | (a, others) :: rest ->
      mem_append others (members rest) a;
      distinct_append others (members rest) r;
      lookup_representative rest x r;
      contains_members rest r; contains_def rest r;
      mem_def (members p) r)

let (representative_valid_law @ total) :
    (p : ('a : logical_data) classes) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if distinct (members p) && contains p x then
      contains p (representative p x) &&
      representative p (representative p x) === representative p x else true}
    @ ghost = fun p x -> ghost_ (
  contains_def p x; representative_def p x;
  match lookup p x with
  | None -> ()
  | Some r -> lookup_representative p x r;
      contains_def p r; representative_def p r)

let (representative_law @ total) : (p : ('a : logical_data) t) @ immutable ->
    (x : 'a) @ immutable ->
    {u : unit | if contains p x then contains p (representative p x) &&
      representative p (representative p x) === representative p x else true}
    @ ghost = fun p x -> ghost_ (representative_valid_law p x)

let (empty_size @ total) : (p : ('a : logical_data) classes) @ immutable ->
    {u : unit | if empty p then size p = 0Z else true} @ ghost =
    fun p -> ghost_ (empty_def p; size_def p; members_def p; length_def (members
      p); ())

let (empty_law @ total) : (p : ('a : logical_data) classes) @ immutable ->
    (x : 'a) @ immutable ->
    {u : unit | if empty p then size p = 0Z && not (contains p x) &&
      representative p x === x else true} @ ghost = fun p x -> ghost_ (
  empty_size p; empty_def p; contains_def p x; representative_def p x;
  lookup_def p x; ())

let rec (agree_refl @ total) : (p : ('a : logical_data) classes) @ immutable ->
    (qs : 'a list) @ immutable -> {u : unit | agree p p qs} @ ghost =
    fun p qs -> ghost_ (agree_def p p qs;
  match qs with [] -> () | _ :: rest -> agree_refl p rest)

let (same_refl @ total) : (p : ('a : logical_data) classes) @ immutable ->
    {u : unit | same p p} @ ghost = fun p -> ghost_ (
  same_def p p; agree_refl p (members p))

let rec (agree_lookup @ total) : (p : ('a : logical_data) classes) @ immutable
  ->
    (q : 'a classes) @ immutable -> (qs : 'a list) @ immutable ->
    (x : 'a) @ immutable ->
    {u : unit | if agree p q qs && mem qs x then lookup p x === lookup q x
      else true} @ ghost = fun p q qs x -> ghost_ (
  agree_def p q qs; mem_def qs x;
  match qs with [] -> () | _ :: rest -> agree_lookup p q rest x)

let (same_law @ total) : (before : ('a : logical_data) classes) @ immutable ->
    (after : 'a classes) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | if same before after then contains after q = contains before q
      &&
      representative after q === representative before q && size after = size
        before
      else true} @ ghost = fun before after q -> ghost_ (
  same_def before after; contains_members before q; contains_members after q;
  agree_lookup before after (members before) q;
  agree_lookup before after (members after) q;
  contains_def before q; contains_def after q;
  representative_def before q; representative_def after q; ())

let (added_law @ total) : (before : ('a : logical_data) classes) @ immutable ->
    (after : 'a classes) @ immutable -> (x : 'a) @ immutable ->
    (q : 'a) @ immutable ->
    {u : unit | if added before after x then not (contains before x) &&
      contains after q = (q === x || contains before q) &&
      representative after q === (if q === x then x else representative before
        q) &&
      size after = Bigint.add 1Z (size before) else true} @ ghost =
    fun before after x q -> ghost_ (
  added_def before after x; same_law ((x, []) :: before) after q;
  contains_def ((x, []) :: before) q; representative_def ((x, []) :: before) q;
  lookup_def ((x, []) :: before) q; mem_def [] q;
  contains_def before q; representative_def before q;
  size_def ((x, []) :: before); members_def ((x, []) :: before);
  append_def [] (members before); length_def (x :: members before);
  size_def before; ())

let rec (merged_lookup @ total) :
    (before : ('a : logical_data) classes) @ immutable ->
    (after : 'a classes) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    (qs : 'a list) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | if merged before after x y r qs && mem qs q then
      lookup after q === (if connected before q x || connected before q y
        then Some r else lookup before q) else true} @ ghost =
    fun before after x y r qs q -> ghost_ (
  merged_def before after x y r qs; mem_def qs q;
  match qs with [] -> () | _ :: rest -> merged_lookup before after x y r rest q)

let rec (merged_unchanged @ total) :
    (before : ('a : logical_data) classes) @ immutable ->
    (after : 'a classes) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    (queries : 'a list) @ immutable ->
    {u : unit | if connected before x y && r === representative before x &&
      merged before after x y r queries then agree before after queries else
        true}
    @ ghost = fun before after x y r queries -> ghost_ (
  merged_def before after x y r queries; agree_def before after queries;
  connected_def before x y;
  match queries with
  | [] -> ()
  | q :: rest ->
      connected_def before q x; connected_def before q y;
      contains_def before q; representative_def before q;
      merged_unchanged before after x y r rest)

let (joined_law @ total) : (before : ('a : logical_data) classes) @ immutable ->
    (after : 'a classes) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | if joined before after x y r then
      contains before x && contains before y &&
      (connected before x r || connected before y r) &&
      (if connected before x y then same before after else true) &&
      contains after q = contains before q && size after = size before &&
      representative after q ===
        (if connected before q x || connected before q y then r
         else representative before q) else true} @ ghost =
    fun before after x y r q -> ghost_ (
  joined_def before after x y r;
  merged_unchanged before after x y r (members before);
  merged_unchanged before after x y r (members after);
  same_def before after;
  contains_members before q; contains_members after q;
  merged_lookup before after x y r (members before) q;
  merged_lookup before after x y r (members after) q;
  contains_def before q; contains_def after q;
  representative_def before q; representative_def after q;
  connected_def before q x; connected_def before q y; ())

let (joined_connected @ total) : (before : ('a : logical_data) t) @ immutable ->
    (after : 'a t) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    (a : 'a) @ immutable -> (b : 'a) @ immutable ->
    {u : unit | if joined before after x y r then
      connected after a b = (connected before a b ||
        (connected before a x && connected before b y) ||
        (connected before a y && connected before b x)) else true} @ ghost =
    fun before after x y r a b -> ghost_ (
  joined_law before after x y r a; joined_law before after x y r b;
  representative_law before a; representative_law before b;
  connected_def before x r; connected_def before y r;
  connected_def before a b; connected_def after a b;
  connected_def before a x; connected_def before a y;
  connected_def before b x; connected_def before b y; ())
