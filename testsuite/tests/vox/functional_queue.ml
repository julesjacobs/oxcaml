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
  | [] -> refine_ u
  | head :: tail ->
    let next = head :: acc in
    let singleton = [head] in
    let rest = reverse tail in
    let nil : 'a list = [] in
    reverse_append_correct tail next;
    append_associative rest singleton acc;
    append_def singleton acc;
    append_def nil acc;
    refine_ u

type ('a : immutable_data) representation = {front : 'a list; rear : 'a list}
type ('a : immutable_data) t = {q : 'a representation |
  match q.front with [] -> q.rear === [] | _ :: _ -> true}

let[@def] contents (q : 'a t @ immutable) : 'a list @ total =
  let refine_ q = q in
  append q.front (reverse q.rear)

let (normalize @ total) :
    (front : 'a list) @ immutable -> (rear : 'a list) @ immutable ->
    {r : 'a t | contents r === append front (reverse rear)} @ immutable total =
  fun front rear ->
  let nil : 'a list = [] in
  let q : 'a t =
    match front with
    | [] ->
      let reversed = reverse_append rear nil in
      let raw = {front = reversed; rear = []} in
      refine_ raw
    | _ :: _ ->
      let raw = {front; rear} in
      refine_ raw
  in
  ghost_ (
    reverse_def nil;
    contents_def q;
    let proof =
      match front with
      | [] ->
        let reversed = reverse_append rear nil in
        let model = reverse rear in
        reverse_append_correct rear nil;
        append_nil model;
        append_def nil model;
        append_nil reversed;
        ()
      | _ :: _ -> ()
    in
    (refine_ proof : {u : unit |
      contents q === append front (reverse rear)}));
  refine_ q

let (empty @ total) : {q : 'a t | contents q === []} @ immutable =
  let nil : 'a list = [] in
  let refine_ result = normalize nil nil in
  ghost_ (reverse_def nil);
  ghost_ (append_def nil nil);
  refine_ result

let (enqueue @ total) : (q : 'a t) @ immutable -> (value : 'a) @ immutable ->
    {r : 'a t | contents r === append (contents q) [value]} @ immutable total =
  fun q value ->
  let refine_ raw = q in
  let front = raw.front in
  let rear = raw.rear in
  let next_rear = value :: rear in
  let refine_ result = normalize front next_rear in
  ghost_ (
    let singleton = [value] in
    let reversed = reverse rear in
    contents_def q;
    reverse_def next_rear;
    append_associative front reversed singleton;
    let u = () in
    (refine_ u : {u : unit | contents result === append (contents q) [value]}));
  refine_ result

let (dequeue @ total) :
    (q : {q : 'a t | (contents q === []) === false}) @ immutable ->
    {r : 'a * 'a t |
      let refine_ original = q in
      match r with head, tail -> contents original === head :: contents tail}
      @ immutable total =
  fun q ->
  let refine_ original = q in
  ghost_ (contents_def original);
  let refine_ raw = original in
  let front = raw.front in
  let rear = raw.rear in
  let reversed = ghost_ (reverse rear) in
  ghost_ (append_def front reversed);
  match front with
  | [] ->
    ghost_ (reverse_def rear);
    let nonempty : {xs : 'a list | (xs === []) === false} = refine_ front in
    let head = List.Refined.hd nonempty in
    let refine_ rest = empty in
    let result = head, rest in
    refine_ result
  | head :: tail ->
    let refine_ rest = normalize tail rear in
    let result = head, rest in
    refine_ result
