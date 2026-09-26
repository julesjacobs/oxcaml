open Vox_sequence

let[@def] rec reverse (xs : ('a : immutable_data) list @ immutable)
    : 'a list @ total =
  match xs with
  | [] -> []
  | head :: tail -> append (reverse tail) [head]

let[@def] rec reverse_append (xs : ('a : immutable_data) list @ immutable)
    (acc : ('a : immutable_data) list @ immutable) : 'a list @ total =
  match xs with
  | [] -> acc
  | head :: tail -> reverse_append tail (head :: acc)

let rec (reverse_append_correct @ total) :
    (xs : ('a : immutable_data) list) @ immutable ->
    (acc : 'a list) @ immutable ->
    {u : unit | reverse_append xs acc === append (reverse xs) acc}
      @ immutable contended =
  fun xs acc ->
  reverse_append_def xs acc;
  reverse_def xs;
  let reversed = reverse xs in
  append_def reversed acc;
  let u = () in
  match xs with
  | [] -> u
  | head :: tail ->
    let next = head :: acc in
    let singleton = [head] in
    let rest = reverse tail in
    let nil : 'a list = [] in
    reverse_append_correct tail next;
    append_associative rest singleton acc;
    append_def singleton acc;
    append_def nil acc;
    u

type ('a : immutable_data) t = {front : 'a list; rear : 'a list}

let[@def] contents (q : 'a t @ immutable) : 'a list @ total =
  let nil : 'a list = [] in
  let reversed = reverse_append q.rear nil in
  ghost_ (reverse_append_correct q.rear nil; append_nil (reverse q.rear));
  append q.front reversed

let (contents_model @ total) (q : 'a t @ immutable) :
    {u : unit | contents q === append q.front (reverse q.rear)} =
  let nil : 'a list = [] in
  contents_def q;
  reverse_append_correct q.rear nil;
  append_nil (reverse q.rear);
  ()

let (empty @ total) : {q : 'a t | contents q === []} @ immutable =
  let result : 'a t = {front = []; rear = []} in
  ghost_ (
    let nil : 'a list = [] in
    contents_model result;
    reverse_def nil;
    append_def nil nil);
  result

let (enqueue @ total) : (q : 'a t) @ immutable -> (value : 'a) @ immutable ->
    {r : 'a t | contents r === append (contents q) [value]} @ immutable total =
  fun q value ->
  let next_rear = value :: q.rear in
  let result : 'a t = {front = q.front; rear = next_rear} in
  ghost_ (
    contents_model q;
    contents_model result;
    reverse_def next_rear;
    append_associative q.front (reverse q.rear) [value]);
  result

let (dequeue @ total) :
    (q : {q : 'a t | (contents q === []) === false}) @ immutable ->
    {r : 'a * 'a t |
      match r with head, tail -> contents q === head :: contents tail}
      @ immutable total =
  fun q ->
  let original : 'a t = q in
  ghost_ (contents_model original);
  let reversed = ghost_ (reverse original.rear) in
  ghost_ (append_def original.front reversed);
  match original.front with
  | head :: tail ->
    let rest : 'a t = {front = tail; rear = original.rear} in
    ghost_ (contents_model rest);
    let result : 'a * 'a t = head, rest in
    result
  | [] ->
    let nil : 'a list = [] in
    let reversed = reverse_append original.rear nil in
    ghost_ (
      reverse_append_correct original.rear nil;
      append_nil (reverse original.rear));
    match reversed with
    | [] -> unreachable_ ()
    | head :: tail ->
      let rest : 'a t = {front = tail; rear = []} in
      ghost_ (
        contents_model rest;
        reverse_def nil;
        append_nil tail);
      let result : 'a * 'a t = head, rest in
      result
