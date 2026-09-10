let[@def] rec append (xs : int list) ys =
  match xs with [] -> ys | head :: tail -> head :: append tail ys

let rec (append_nil @ total) : (xs : int list) ->
    {u : unit | append xs [] === xs} @ immutable contended =
  fun xs ->
  let nil = [] in
  append_def xs nil;
  let u = () in
  match xs with
  | [] -> refine_ u
  | _ :: tail ->
    append_nil tail;
    refine_ u

let rec (append_associative @ total) :
    (xs : int list) -> (ys : int list) -> (zs : int list) ->
    {u : unit | append (append xs ys) zs === append xs (append ys zs)}
      @ immutable contended =
  fun xs ys zs ->
  let xy = append xs ys in
  let yz = append ys zs in
  append_def xs ys;
  append_def xy zs;
  append_def xs yz;
  let u = () in
  match xs with
  | [] -> refine_ u
  | _ :: tail ->
    append_associative tail ys zs;
    refine_ u

let[@def] rec reverse (xs : int list) =
  match xs with
  | [] -> []
  | head :: tail -> append (reverse tail) [head]

let[@def] rec reverse_append (xs : int list) acc =
  match xs with
  | [] -> acc
  | head :: tail -> reverse_append tail (head :: acc)

let rec (reverse_append_correct @ total) :
    (xs : int list) -> (acc : int list) ->
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
    let nil = [] in
    reverse_append_correct tail next;
    append_associative rest singleton acc;
    append_def singleton acc;
    append_def nil acc;
    refine_ u

type representation = {front : int list; rear : int list}
type t = {q : representation |
  match q.front with [] -> q.rear === [] | _ :: _ -> true}

let[@def] contents (q : t) =
  let refine_ q = q in
  append q.front (reverse q.rear)

let (normalize @ total) : (front : int list) -> (rear : int list) ->
    {r : t | contents r === append front (reverse rear)} =
  fun front rear ->
  let nil = [] in
  let q : t =
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

let (empty @ total) : {q : t | contents q === []} =
  let nil = [] in
  let refine_ result = normalize nil nil in
  ghost_ (reverse_def nil);
  ghost_ (append_def nil nil);
  refine_ result

let (enqueue @ total) : (q : t) -> (value : int) ->
    {r : t | contents r === append (contents q) [value]} =
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
    (q : {q : t | (contents q === []) === false}) ->
    {r : int * t |
      let refine_ original = q in
      match r with head, tail -> contents original === head :: contents tail} =
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
    let nonempty : {xs : int list | (xs === []) === false} = refine_ front in
    let head = List.Refined.hd nonempty in
    let refine_ rest = empty in
    let result = head, rest in
    refine_ result
  | head :: tail ->
    let refine_ rest = normalize tail rear in
    let result = head, rest in
    refine_ result
