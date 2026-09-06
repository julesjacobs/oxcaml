let[@def] rec append (xs : ('a : immutable_data) list @ immutable)
    (ys : ('a : immutable_data) list @ immutable) : 'a list @ total =
  match xs with [] -> ys | head :: tail -> head :: append tail ys

let rec (append_nil @ total) : (xs : ('a : immutable_data) list) @ immutable ->
    {u : unit | append xs [] === xs} @ immutable contended =
  fun xs ->
  let nil : 'a list = [] in
  let refine_ equation = append_def xs nil in
  let u = () in
  match xs with
  | [] -> refine_ u
  | _ :: tail ->
    let refine_ induction = append_nil tail in
    refine_ u

let rec (append_associative @ total) :
    (xs : ('a : immutable_data) list) @ immutable ->
    (ys : 'a list) @ immutable -> (zs : 'a list) @ immutable ->
    {u : unit | append (append xs ys) zs === append xs (append ys zs)}
      @ immutable contended =
  fun xs ys zs ->
  let xy = append xs ys in
  let yz = append ys zs in
  let refine_ equation = append_def xs ys in
  let refine_ equation = append_def xy zs in
  let refine_ equation = append_def xs yz in
  let u = () in
  match xs with
  | [] -> refine_ u
  | _ :: tail ->
    let refine_ induction = append_associative tail ys zs in
    refine_ u

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
  let refine_ equation = reverse_append_def xs acc in
  let refine_ equation = reverse_def xs in
  let reversed = reverse xs in
  let refine_ equation = append_def reversed acc in
  let u = () in
  match xs with
  | [] -> refine_ u
  | head :: tail ->
    let next = head :: acc in
    let singleton = [head] in
    let rest = reverse tail in
    let nil : 'a list = [] in
    let refine_ induction = reverse_append_correct tail next in
    let refine_ association = append_associative rest singleton acc in
    let refine_ equation = append_def singleton acc in
    let refine_ equation = append_def nil acc in
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
  let refine_ proof = ghost_ (
    let refine_ reverse_nil = reverse_def nil in
    let refine_ equation = contents_def q in
    let u = () in
    match front with
    | [] ->
      let reversed = reverse_append rear nil in
      let model = reverse rear in
      let refine_ reverse_law = reverse_append_correct rear nil in
      let refine_ append_law = append_nil model in
      let refine_ left_nil = append_def nil model in
      let refine_ right_nil = append_nil reversed in
      (refine_ u : {u : unit | contents q === append front (reverse rear)})
    | _ :: _ ->
      (refine_ u : {u : unit | contents q === append front (reverse rear)}))
  in
  refine_ q

let (empty @ total) : {q : 'a t | contents q === []} @ immutable =
  let nil : 'a list = [] in
  let refine_ result = normalize nil nil in
  let refine_ equation = ghost_ (reverse_def nil) in
  let refine_ equation = ghost_ (append_def nil nil) in
  refine_ result

let (enqueue @ total) : (q : 'a t) @ immutable -> (value : 'a) @ immutable ->
    {r : 'a t | contents r === append (contents q) [value]} @ immutable total =
  fun q value ->
  let refine_ raw = q in
  let front = raw.front in
  let rear = raw.rear in
  let next_rear = value :: rear in
  let refine_ result = normalize front next_rear in
  let refine_ proof = ghost_ (
    let singleton = [value] in
    let reversed = reverse rear in
    let refine_ model = contents_def q in
    let refine_ reverse_law = reverse_def next_rear in
    let refine_ association = append_associative front reversed singleton in
    let u = () in
    (refine_ u : {u : unit | contents result === append (contents q) [value]}))
  in
  refine_ result

let (dequeue @ total) :
    (q : {q : 'a t | (contents q === []) === false}) @ immutable ->
    {r : 'a * 'a t |
      let refine_ original = q in
      match r with head, tail -> contents original === head :: contents tail}
      @ immutable total =
  fun q ->
  let refine_ original = q in
  let refine_ model = ghost_ (contents_def original) in
  let refine_ raw = original in
  let front = raw.front in
  let rear = raw.rear in
  let reversed = ghost_ (reverse rear) in
  let refine_ equation = ghost_ (append_def front reversed) in
  match front with
  | [] ->
    let refine_ equation = ghost_ (reverse_def rear) in
    let nonempty : {xs : 'a list | (xs === []) === false} = refine_ front in
    let head = List.Refined.hd nonempty in
    let refine_ rest = empty in
    let result = head, rest in
    refine_ result
  | head :: tail ->
    let refine_ rest = normalize tail rear in
    let result = head, rest in
    refine_ result
