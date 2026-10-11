(** Each pair is a representative and the other members of its class.
    Relations ignore the order of classes and of the other members. *)
type ('a : logical_data) classes = ('a * 'a list) list

let[@def] rec mem (xs : ('a : logical_data) list @ immutable)
    (x : 'a @ immutable) = ghost_ (
  match xs with [] -> false | y :: rest -> x === y || mem rest x)

let[@def] rec append (xs : ('a : logical_data) list @ immutable)
    (ys : 'a list @ immutable) = ghost_ (
  match xs with [] -> ys | x :: rest -> x :: append rest ys)

let[@def] rec members (p : ('a : logical_data) classes @ immutable) = ghost_ (
  match p with
  | [] -> []
  | (r, others) :: rest -> r :: append others (members rest))

let[@def] rec distinct (xs : ('a : logical_data) list @ immutable) = ghost_ (
  match xs with [] -> true | x :: rest -> not (mem rest x) && distinct rest)

type ('a : logical_data) t = {p : 'a classes | distinct (members p)}

let[@def] rec lookup (p : ('a : logical_data) classes @ immutable)
    (x : 'a @ immutable) = ghost_ (
  match p with
  | [] -> None
  | (r, others) :: rest ->
      if x === r || mem others x then Some r else lookup rest x)

let[@def] contains (p : ('a : logical_data) classes @ immutable)
    (x : 'a @ immutable) = ghost_ (
  match lookup p x with None -> false | Some _ -> true)

let[@def] representative (p : ('a : logical_data) classes @ immutable)
    (x : 'a @ immutable) : 'a @ total immutable ghost = ghost_ (
  match lookup p x with None -> x | Some r -> r)

let[@def] connected (p : ('a : logical_data) classes @ immutable)
    (x : 'a @ immutable) (y : 'a @ immutable) = ghost_ (
  contains p x && contains p y && representative p x === representative p y)

let[@def] rec length (xs : ('a : logical_data) list @ immutable) = ghost_ (
  match xs with [] -> 0Z | _ :: rest -> Bigint.add 1Z (length rest))

let[@def] size (p : ('a : logical_data) classes @ immutable) = ghost_ (
  length (members p))

let[@def] empty (p : ('a : logical_data) classes @ immutable) =
  ghost_ (p === [])

let[@def] rec agree (before : ('a : logical_data) classes @ immutable)
    (after : 'a classes @ immutable) (queries : 'a list @ immutable) = ghost_ (
  match queries with
  | [] -> true
  | q :: rest -> lookup after q === lookup before q && agree before after rest)

let[@def] same (before : ('a : logical_data) classes @ immutable)
    (after : 'a classes @ immutable) = ghost_ (
  size after = size before &&
  agree before after (members before) && agree before after (members after))

(** Add a fresh singleton; preserve the existing classes and representatives. *)
let[@def] added (before : ('a : logical_data) classes @ immutable)
    (after : 'a classes @ immutable) (x : 'a @ immutable) = ghost_ (
  not (contains before x) && same ((x, []) :: before) after)

let[@def] rec merged (before : ('a : logical_data) classes @ immutable)
    (after : 'a classes @ immutable) (x : 'a @ immutable) (y : 'a @ immutable)
    (r : 'a @ immutable) (queries : 'a list @ immutable) = ghost_ (
  match queries with
  | [] -> true
  | q :: rest ->
      lookup after q ===
        (if connected before q x || connected before q y then Some r
         else lookup before q) && merged before after x y r rest)

(** Merge exactly the two classes, choosing any merged member as representative.
    Other representatives are preserved. An already-connected union preserves
    membership and representatives. *)
let[@def] joined (before : ('a : logical_data) classes @ immutable)
    (after : 'a classes @ immutable) (x : 'a @ immutable) (y : 'a @ immutable)
    (r : 'a @ immutable) = ghost_ (
  contains before x && contains before y &&
  (connected before x r || connected before y r) &&
  (if connected before x y then r === representative before x else true) &&
  size after = size before &&
  merged before after x y r (members before) &&
  merged before after x y r (members after))
