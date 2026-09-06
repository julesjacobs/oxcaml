(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }{
   flags = "-extension refinement_types -principal";
   { expect; }
   { expect.opt; }
 }
*)

module Regex : sig
  type t = Empty | Epsilon | Symbol of int | Alt of t * t | Seq of t * t | Star of t
  [@@inductive]

  module Membership : sig
    type evidence =
      | Epsilon_match
      | Symbol_match of int
      | Alt_left of evidence
      | Alt_right of evidence
      | Seq_match of evidence * evidence
      | Star_empty
      | Star_step of evidence * evidence
    [@@inductive]

    val word : evidence -> int list @@ total
    val valid : t -> evidence -> bool @@ total
  end

  open Membership
  val alt : t -> t -> t @@ total
  val nullable : t -> bool @@ total
  val derive : int -> t -> t @@ total
  val matches : t -> int list -> bool @@ total
  val sound : (r : t) -> (s : int list) ->
    {p : evidence | if matches r s then valid r p && word p === s else true}
    @@ total
  val complete : (r : t) -> (s : int list) -> (p : evidence) ->
    {u : unit | if valid r p && word p === s then matches r s else true}
    @@ total
  val recognize : (r : t) -> (s : int list) ->
    {result : evidence option |
      match result with
      | None -> matches r s === false
      | Some p -> matches r s && valid r p && word p === s}
    @@ total
  module Dfa : sig
    type automaton
    val compile : t -> automaton @@ total
    val run : automaton -> int list -> bool @@ total
    val correct : (root : t) -> (s : int list) ->
      {u : unit | run (compile root) s === matches root s} @@ total
    val sound : (root : t) -> (s : int list) ->
      {p : evidence | if run (compile root) s then valid root p && word p === s
        else true} @@ total
    val complete : (root : t) -> (s : int list) -> (p : evidence) ->
      {u : unit | if valid root p && word p === s then run (compile root) s
        else true} @@ total
  end
end = struct
  type t = Empty | Epsilon | Symbol of int | Alt of t * t | Seq of t * t | Star of t
  [@@inductive]

  module Membership = struct
    type evidence =
      | Epsilon_match
      | Symbol_match of int
      | Alt_left of evidence
      | Alt_right of evidence
      | Seq_match of evidence * evidence
      | Star_empty
      | Star_step of evidence * evidence
    [@@inductive]

    let[@def] rec append (xs : int list) ys =
      match xs with [] -> ys | x :: rest -> x :: append rest ys

    let[@def] rec word p =
      match p with
      | Epsilon_match | Star_empty -> []
      | Symbol_match c -> [c]
      | Alt_left p | Alt_right p -> word p
      | Seq_match (p, q) | Star_step (p, q) -> append (word p) (word q)

    let[@def] rec valid r p =
      match p with
      | Epsilon_match -> (match r with Epsilon -> true | _ -> false)
      | Symbol_match c -> (match r with Symbol d -> c = d | _ -> false)
      | Alt_left p -> (match r with Alt (a, _) -> valid a p | _ -> false)
      | Alt_right p -> (match r with Alt (_, b) -> valid b p | _ -> false)
      | Seq_match (p, q) ->
        (match r with Seq (a, b) -> valid a p && valid b q | _ -> false)
      | Star_empty -> (match r with Star _ -> true | _ -> false)
      | Star_step (p, q) ->
        (match r with Star a -> valid a p && valid r q | _ -> false)

  end

  open Membership

  let rec (append_nil @ total) : (xs : int list) ->
      {u : unit | append xs [] === xs} @ immutable contended =
    fun xs ->
    let nil = [] in
    let refine_ equation = append_def xs nil in
    let u = () in
    match xs with
    | [] -> refine_ u
    | _ :: rest ->
      let refine_ induction = append_nil rest in
      refine_ u

  let[@def] rec equal a b =
    match a with
    | Empty -> (match b with Empty -> true | _ -> false)
    | Epsilon -> (match b with Epsilon -> true | _ -> false)
    | Symbol c -> (match b with Symbol d -> c = d | _ -> false)
    | Alt (a1, a2) ->
      (match b with Alt (b1, b2) -> equal a1 b1 && equal a2 b2 | _ -> false)
    | Seq (a1, a2) ->
      (match b with Seq (b1, b2) -> equal a1 b1 && equal a2 b2 | _ -> false)
    | Star a -> (match b with Star b -> equal a b | _ -> false)

  let rec (equal_correct @ total) : (a : t) -> (b : t) ->
      {u : unit | equal a b === (a === b)} @ immutable contended =
    fun a b ->
    let refine_ equation = equal_def a b in
    let u = () in
    match a with
    | Empty | Epsilon | Symbol _ -> refine_ u
    | Alt (a1, a2) ->
      (match b with
       | Alt (b1, b2) ->
         let refine_ induction = equal_correct a1 b1 in
         let refine_ induction = equal_correct a2 b2 in
         refine_ u
       | _ -> refine_ u)
    | Seq (a1, a2) ->
      (match b with
       | Seq (b1, b2) ->
         let refine_ induction = equal_correct a1 b1 in
         let refine_ induction = equal_correct a2 b2 in
         refine_ u
       | _ -> refine_ u)
    | Star a ->
      (match b with
       | Star b ->
         let refine_ induction = equal_correct a b in
         refine_ u
       | _ -> refine_ u)

  let (rank @ total) r =
    match r with
    | Empty -> 0 | Epsilon -> 1 | Symbol _ -> 2
    | Alt _ -> 3 | Seq _ -> 4 | Star _ -> 5

  let rec (compare @ total) a b =
    match a with
    | Empty | Epsilon -> rank a - rank b
    | Symbol c ->
      (match b with
       | Symbol d -> if c < d then -1 else if c = d then 0 else 1
       | _ -> rank a - rank b)
    | Alt (a1, a2) ->
      (match b with
       | Alt (b1, b2) ->
         let first = compare a1 b1 in
         if first = 0 then compare a2 b2 else first
       | _ -> rank a - rank b)
    | Seq (a1, a2) ->
      (match b with
       | Seq (b1, b2) ->
         let first = compare a1 b1 in
         if first = 0 then compare a2 b2 else first
       | _ -> rank a - rank b)
    | Star inner ->
      (match b with Star other -> compare inner other | _ -> rank a - rank b)

  let[@def] rec insert (a @ total) (r @ total) : t @ total =
    match r with
    | Empty -> a
    | Alt (head, tail) ->
      if equal a head then r
      else if compare a head < 0 then Alt (a, r)
      else Alt (head, insert a tail)
    | _ ->
      if equal a r then r
      else if compare a r < 0 then Alt (a, r)
      else Alt (r, a)

  let[@def] rec add_alternatives (a @ total) (acc @ total) : t @ total =
    match a with
    | Empty -> acc
    | Alt (left, right) -> add_alternatives left (add_alternatives right acc)
    | _ -> insert a acc

  let[@def] alt (a @ total) (b @ total) : t @ total =
    add_alternatives a (add_alternatives b Empty)

  let[@def] rec contains a r =
    match r with
    | Empty -> false
    | Alt (left, right) -> contains a left || contains a right
    | _ -> equal a r

  let rec (insert_contains @ total) : (a : t) -> (r : t) -> (query : t) ->
      {u : unit | contains query (insert a r) ===
        (contains query a || contains query r)} @ immutable contended =
    fun a r query ->
    let result = insert a r in
    let refine_ equation = insert_def a r in
    let refine_ equation = contains_def query r in
    let refine_ equation = contains_def query result in
    let refine_ equality = equal_correct a r in
    let u = () in
    match r with
    | Empty -> refine_ u
    | Alt (head, tail) ->
      let refine_ equality = equal_correct a head in
      let next = insert a tail in
      let refine_ induction = insert_contains a tail query in
      let refine_ equation = contains_def query next in
      refine_ u
    | _ -> refine_ u

  let rec (add_contains @ total) : (a : t) -> (acc : t) -> (query : t) ->
      {u : unit | contains query (add_alternatives a acc) ===
        (contains query a || contains query acc)} @ immutable contended =
    fun a acc query ->
    let refine_ equation = add_alternatives_def a acc in
    let refine_ equation = contains_def query a in
    let u = () in
    match a with
    | Empty -> refine_ u
    | Alt (left, right) ->
      let next = add_alternatives right acc in
      let refine_ induction = add_contains right acc query in
      let refine_ induction = add_contains left next query in
      refine_ u
    | _ ->
      let refine_ induction = insert_contains a acc query in
      refine_ u

  let (alt_contains @ total) (a @ total) (b @ total) query :
      {u : unit | contains query (alt a b) ===
        (contains query a || contains query b)} =
    let empty = Empty in
    let next = add_alternatives b empty in
    let refine_ equation = alt_def a b in
    let refine_ equation = contains_def query empty in
    let refine_ proof = add_contains b empty query in
    let refine_ proof = add_contains a next query in
    let u = () in refine_ u

  let rec (select_alternative @ total) : (r : t) -> (p : evidence) ->
      {choice : t * evidence |
        if valid r p then
          match choice with a, q ->
            contains a r && valid a q && word q === word p
        else true} @ immutable contended =
    fun r p ->
    let refine_ equation = ghost_ (valid_def r p) in
    let refine_ equation = ghost_ (word_def p) in
    let result =
      match r with
      | Alt (left, right) ->
        (match p with
         | Alt_left inner ->
           let refine_ choice = select_alternative left inner in
           let (a : t), _ = choice in
           let refine_ equation = ghost_ (contains_def a r) in
           choice
         | Alt_right inner ->
           let refine_ choice = select_alternative right inner in
           let (a : t), _ = choice in
           let refine_ equation = ghost_ (contains_def a r) in
           choice
         | _ -> r, p)
      | _ ->
        let refine_ equation = ghost_ (contains_def r r) in
        let refine_ equation = ghost_ (equal_correct r r) in
        r, p
    in
    refine_ result

  let rec (inject_alternative @ total) :
      (r : t) -> (a : t) -> (p : evidence) ->
      {q : evidence |
        if contains a r && valid a p then valid r q && word q === word p
        else true} @ immutable contended =
    fun r a p ->
    let refine_ equation = ghost_ (contains_def a r) in
    let result =
      match r with
      | Alt (left, right) ->
        if contains a left then
          let refine_ q = inject_alternative left a p in
          let result = Alt_left q in
          let refine_ equation = ghost_ (valid_def r result) in
          let refine_ equation = ghost_ (word_def result) in
          result
        else
          let refine_ q = inject_alternative right a p in
          let result = Alt_right q in
          let refine_ equation = ghost_ (valid_def r result) in
          let refine_ equation = ghost_ (word_def result) in
          result
      | _ ->
        let refine_ equation = ghost_ (equal_correct a r) in
        p
    in
    refine_ result

  let (alt_expand @ total) (a @ total) (b @ total) p :
      {q : evidence |
        if valid (alt a b) p then valid (Alt (a, b)) q && word q === word p
        else true} =
    let simplified = alt a b in
    let original = Alt (a, b) in
    let refine_ choice = select_alternative simplified p in
    let (leaf : t), (inner : evidence) = choice in
    let refine_ proof = ghost_ (alt_contains a b leaf) in
    let refine_ equation = ghost_ (contains_def leaf original) in
    let refine_ q = inject_alternative original leaf inner in
    refine_ q

  let (alt_contract @ total) (a @ total) (b @ total) p :
      {q : evidence |
        if valid (Alt (a, b)) p then valid (alt a b) q && word q === word p
        else true} =
    let simplified = alt a b in
    let original = Alt (a, b) in
    let refine_ choice = select_alternative original p in
    let (leaf : t), (inner : evidence) = choice in
    let refine_ equation = ghost_ (contains_def leaf original) in
    let refine_ proof = ghost_ (alt_contains a b leaf) in
    let refine_ q = inject_alternative simplified leaf inner in
    refine_ q

  let[@def] seq (a @ total) (b @ total) : t @ total =
    match a, b with
    | Empty, _ | _, Empty -> Empty
    | Epsilon, _ -> b
    | _, Epsilon -> a
    | _ -> Seq (a, b)

  let (seq_expand @ total) (a @ total) (b @ total) p :
      {q : evidence |
        if valid (seq a b) p then valid (Seq (a, b)) q && word q === word p
        else true} =
    let simplified = ghost_ (seq a b) in
    let original = ghost_ (Seq (a, b)) in
    let refine_ equation = ghost_ (seq_def a b) in
    let refine_ equation = ghost_ (valid_def simplified p) in
    let empty = Epsilon_match in
    let refine_ equation = ghost_ (word_def empty) in
    let epsilon = Epsilon in
    let refine_ equation = ghost_ (valid_def epsilon empty) in
    let q = match a, b with
      | Empty, _ | _, Empty -> Epsilon_match
      | Epsilon, _ -> Seq_match (empty, p)
      | _, Epsilon -> Seq_match (p, empty)
      | _ -> p
    in
    let refine_ equation = ghost_ (valid_def original q) in
    let refine_ equation = ghost_ (word_def q) in
    let nil = [] in
    let pw = ghost_ (word p) in
    let refine_ equation = ghost_ (append_def nil pw) in
    let refine_ equation = ghost_ (append_nil pw) in
    refine_ q

  let (seq_contract @ total) (a @ total) (b @ total) p :
      {q : evidence |
        if valid (Seq (a, b)) p then valid (seq a b) q && word q === word p
        else true} =
    let original = ghost_ (Seq (a, b)) in
    let refine_ equation = ghost_ (seq_def a b) in
    let refine_ equation = ghost_ (valid_def original p) in
    let refine_ equation = ghost_ (word_def p) in
    let q = match p with
      | Seq_match (left, right) ->
        let refine_ equation = ghost_ (valid_def a left) in
        let refine_ equation = ghost_ (valid_def b right) in
        let refine_ equation = ghost_ (word_def left) in
        let refine_ equation = ghost_ (word_def right) in
        let lw = ghost_ (word left) in
        let rw = ghost_ (word right) in
        let refine_ equation = ghost_ (append_def lw rw) in
        let refine_ equation = ghost_ (append_nil lw) in
        (match a, b with
         | Empty, _ | _, Empty -> Epsilon_match
         | Epsilon, _ -> right
         | _, Epsilon -> left
         | _ -> p)
      | _ -> Epsilon_match
    in
    refine_ q

  let[@def] rec nullable r =
    match r with
    | Empty | Symbol _ -> false
    | Epsilon | Star _ -> true
    | Alt (a, b) -> nullable a || nullable b
    | Seq (a, b) -> nullable a && nullable b

  let[@def] rec derive c r =
    match r with
    | Empty | Epsilon -> Empty
    | Symbol d -> if c = d then Epsilon else Empty
    | Alt (a, b) -> alt (derive c a) (derive c b)
    | Seq (a, b) ->
      if nullable a then alt (seq (derive c a) b) (derive c b)
      else seq (derive c a) b
    | Star a -> seq (derive c a) (Star a)

  let[@def] rec matches r (s : int list) =
    match s with [] -> nullable r | c :: rest -> matches (derive c r) rest

  let rec (epsilon @ total) : (r : t) ->
      {p : evidence | if nullable r then valid r p && word p === [] else true}
        @ immutable contended =
    fun r ->
    let refine_ equation = ghost_ (nullable_def r) in
    let result =
      match r with
      | Empty | Epsilon | Symbol _ -> Epsilon_match
      | Star _ -> Star_empty
      | Alt (a, b) ->
        if nullable a then
          let refine_ p = epsilon a in
          let result = Alt_left p in
          let refine_ equation = ghost_ (valid_def r result) in
          let refine_ equation = ghost_ (word_def result) in
          result
        else
          let refine_ p = epsilon b in
          let result = Alt_right p in
          let refine_ equation = ghost_ (valid_def r result) in
          let refine_ equation = ghost_ (word_def result) in
          result
      | Seq (a, b) ->
        let refine_ p = epsilon a in
        let refine_ q = epsilon b in
        let result = Seq_match (p, q) in
        let refine_ equation = ghost_ (valid_def r result) in
        let refine_ equation = ghost_ (word_def result) in
        let left = ghost_ (word p) in
        let right = ghost_ (word q) in
        let refine_ equation = ghost_ (append_def left right) in
        result
    in
    let refine_ equation = ghost_ (valid_def r result) in
    let refine_ equation = ghost_ (word_def result) in
    refine_ result
  let rec (expand @ total) : (r : t) -> (c : int) -> (p : evidence) ->
      {q : evidence |
        if valid (derive c r) p then valid r q && word q === c :: word p
        else true} @ immutable contended =
    fun r c p ->
    let derivative = ghost_ (derive c r) in
    let refine_ equation = ghost_ (derive_def c r) in
    let refine_ equation = ghost_ (valid_def derivative p) in
    let refine_ equation = ghost_ (word_def p) in
    let result =
      match r with
      | Empty | Epsilon -> Epsilon_match
      | Symbol _ -> Symbol_match c
      | Alt (a, b) ->
        let da = derive c a in
        let db = derive c b in
        let refine_ p = alt_expand da db p in
        let original = ghost_ (Alt (da, db)) in
        let refine_ equation = ghost_ (valid_def original p) in
        let refine_ equation = ghost_ (word_def p) in
        (match p with
         | Alt_left inner ->
           let refine_ q = expand a c inner in
           let result = Alt_left q in
           let refine_ equation = ghost_ (valid_def r result) in
           let refine_ equation = ghost_ (word_def result) in
           result
         | Alt_right inner ->
           let refine_ q = expand b c inner in
           let result = Alt_right q in
           let refine_ equation = ghost_ (valid_def r result) in
           let refine_ equation = ghost_ (word_def result) in
           result
         | _ -> Epsilon_match)
      | Seq (a, b) ->
        let da = derive c a in
        let db = derive c b in
        let product = seq da b in
        if nullable a then
          let refine_ p = alt_expand product db p in
          let original = ghost_ (Alt (product, db)) in
          let refine_ equation = ghost_ (valid_def original p) in
          let refine_ equation = ghost_ (word_def p) in
          (match p with
           | Alt_left inner ->
             let refine_ inner = seq_expand da b inner in
             let left_derivative = ghost_ (Seq (derive c a, b)) in
             let refine_ equation = ghost_ (valid_def left_derivative inner) in
             let refine_ equation = ghost_ (word_def inner) in
             (match inner with
              | Seq_match (left, right) ->
                let refine_ q = expand a c left in
                let result = Seq_match (q, right) in
                let refine_ equation = ghost_ (valid_def r result) in
                let refine_ equation = ghost_ (word_def result) in
                let qw = ghost_ (word q) in
                let rw = ghost_ (word right) in
                let refine_ equation = ghost_ (append_def qw rw) in
                result
              | _ -> Epsilon_match)
           | Alt_right right ->
             let refine_ left = epsilon a in
             let refine_ q = expand b c right in
             let result = Seq_match (left, q) in
             let refine_ equation = ghost_ (valid_def r result) in
             let refine_ equation = ghost_ (word_def result) in
             let lw = ghost_ (word left) in
             let qw = ghost_ (word q) in
             let refine_ equation = ghost_ (append_def lw qw) in
             result
           | _ -> Epsilon_match)
        else
          let refine_ p = seq_expand da b p in
          let original = ghost_ (Seq (da, b)) in
          let refine_ equation = ghost_ (valid_def original p) in
          let refine_ equation = ghost_ (word_def p) in
          (match p with
           | Seq_match (left, right) ->
             let refine_ q = expand a c left in
             let result = Seq_match (q, right) in
             let refine_ equation = ghost_ (valid_def r result) in
             let refine_ equation = ghost_ (word_def result) in
             let qw = ghost_ (word q) in
             let rw = ghost_ (word right) in
             let refine_ equation = ghost_ (append_def qw rw) in
             result
           | _ -> Epsilon_match)
      | Star a ->
        let da = derive c a in
        let refine_ p = seq_expand da r p in
        let original = ghost_ (Seq (da, r)) in
        let refine_ equation = ghost_ (valid_def original p) in
        let refine_ equation = ghost_ (word_def p) in
        (match p with
         | Seq_match (left, right) ->
           let refine_ q = expand a c left in
           let result = Star_step (q, right) in
           let refine_ equation = ghost_ (valid_def r result) in
           let refine_ equation = ghost_ (word_def result) in
           let qw = ghost_ (word q) in
           let rw = ghost_ (word right) in
           let refine_ equation = ghost_ (append_def qw rw) in
           result
         | _ -> Epsilon_match)
    in
    let refine_ equation = ghost_ (valid_def r result) in
    let refine_ equation = ghost_ (word_def result) in
    refine_ result
  let (append_empty @ total) (xs : int list) (ys : int list) :
      {u : unit | (append xs ys === []) === (xs === [] && ys === [])} =
    let refine_ equation = append_def xs ys in
    let u = () in
    match xs with [] -> refine_ u | _ :: _ -> refine_ u

  let rec (empty_complete @ total) : (r : t) -> (p : evidence) ->
      {u : unit | if valid r p && word p === [] then nullable r else true}
        @ immutable contended =
    fun r p ->
    let refine_ equation = valid_def r p in
    let refine_ equation = word_def p in
    let refine_ equation = nullable_def r in
    let u = () in
    match p with
    | Epsilon_match | Symbol_match _ | Star_empty | Star_step _ -> refine_ u
    | Alt_left inner ->
      (match r with
       | Alt (a, _) ->
         let refine_ induction = empty_complete a inner in
         refine_ u
       | _ -> refine_ u)
    | Alt_right inner ->
      (match r with
       | Alt (_, b) ->
         let refine_ induction = empty_complete b inner in
         refine_ u
       | _ -> refine_ u)
    | Seq_match (left, right) ->
      (match r with
       | Seq (a, b) ->
         let lw = word left in
         let rw = word right in
         let refine_ equation = append_empty lw rw in
         let refine_ induction = empty_complete a left in
         let refine_ induction = empty_complete b right in
         refine_ u
       | _ -> refine_ u)

  let rec (contract @ total) :
      (r : t) -> (c : int) -> (s : int list) -> (p : evidence) ->
      {q : evidence |
        if valid r p && word p === c :: s
        then valid (derive c r) q && word q === s else true}
        @ immutable contended =
    fun r c s p ->
    let derivative = ghost_ (derive c r) in
    let refine_ equation = ghost_ (derive_def c r) in
    let refine_ equation = ghost_ (valid_def r p) in
    let refine_ equation = ghost_ (word_def p) in
    let result =
      match p with
      | Epsilon_match | Star_empty | Symbol_match _ -> Epsilon_match
      | Alt_left inner ->
        (match r with
         | Alt (a, b) ->
           let refine_ q = contract a c s inner in
           let da = derive c a in
           let db = derive c b in
           let original = ghost_ (Alt (da, db)) in
           let result = Alt_left q in
           let refine_ equation = ghost_ (valid_def original result) in
           let refine_ equation = ghost_ (word_def result) in
           let refine_ result = alt_contract da db result in
           result
         | _ -> Epsilon_match)
      | Alt_right inner ->
        (match r with
         | Alt (a, b) ->
           let refine_ q = contract b c s inner in
           let da = derive c a in
           let db = derive c b in
           let original = ghost_ (Alt (da, db)) in
           let result = Alt_right q in
           let refine_ equation = ghost_ (valid_def original result) in
           let refine_ equation = ghost_ (word_def result) in
           let refine_ result = alt_contract da db result in
           result
         | _ -> Epsilon_match)
      | Seq_match (left, right) ->
        (match r with
         | Seq (a, b) ->
           let da = derive c a in
           let db = derive c b in
           let product = seq da b in
           let sum = ghost_ (Alt (product, db)) in
           let lw = word left in
           let rw = ghost_ (word right) in
           let refine_ equation = ghost_ (append_def lw rw) in
           (match lw with
            | [] ->
              let refine_ empty_law = ghost_ (empty_complete a left) in
              let refine_ q = contract b c s right in
              let result = Alt_right q in
              let refine_ equation = ghost_ (valid_def sum result) in
              let refine_ equation = ghost_ (word_def result) in
              let refine_ result = alt_contract product db result in
              result
            | h :: rest ->
              let refine_ q = contract a h rest left in
              let pair = Seq_match (q, right) in
              let pair_regex = ghost_ (Seq (derive c a, b)) in
              let refine_ equation = ghost_ (valid_def pair_regex pair) in
              let refine_ equation = ghost_ (word_def pair) in
              let refine_ pair = seq_contract da b pair in
              if nullable a then
                let result = Alt_left pair in
                let refine_ equation = ghost_ (valid_def sum result) in
                let refine_ equation = ghost_ (word_def result) in
                let refine_ result = alt_contract product db result in
                result
              else pair)
         | _ -> Epsilon_match)
      | Star_step (left, right) ->
        (match r with
         | Star a ->
           let lw = word left in
           let rw = ghost_ (word right) in
           let refine_ equation = ghost_ (append_def lw rw) in
           (match lw with
            | [] ->
              let refine_ q = contract r c s right in
              q
            | h :: rest ->
              let refine_ q = contract a h rest left in
              let da = derive c a in
              let original = ghost_ (Seq (da, r)) in
              let result = Seq_match (q, right) in
              let refine_ equation = ghost_ (valid_def original result) in
              let refine_ equation = ghost_ (word_def result) in
              let refine_ result = seq_contract da r result in
              result)
         | _ -> Epsilon_match)
    in
    let refine_ equation = ghost_ (valid_def derivative result) in
    let refine_ equation = ghost_ (word_def result) in
    refine_ result
  let rec (sound @ total) : (r : t) -> (s : int list) ->
      {p : evidence | if matches r s then valid r p && word p === s else true}
        @ immutable contended =
    fun r s ->
    let refine_ equation = ghost_ (matches_def r s) in
    match s with
    | [] ->
      let refine_ p = epsilon r in
      refine_ p
    | c :: rest ->
      let derivative = derive c r in
      let refine_ p = sound derivative rest in
      let refine_ q = expand r c p in
      refine_ q

  let rec (complete @ total) : (r : t) -> (s : int list) -> (p : evidence) ->
      {u : unit | if valid r p && word p === s then matches r s else true}
        @ immutable contended =
    fun r s p ->
    let refine_ equation = matches_def r s in
    let u = () in
    match s with
    | [] ->
      let refine_ proof = empty_complete r p in
      refine_ u
    | c :: rest ->
      let derivative = derive c r in
      let refine_ q = contract r c rest p in
      let refine_ proof = complete derivative rest q in
      refine_ u

  let (recognize @ total) (r @ total) (s : int list) :
      {result : evidence option |
        match result with
        | None -> matches r s === false
        | Some p -> matches r s && valid r p && word p === s} =
    if matches r s then
      let refine_ p = sound r s in
      let result = Some p in refine_ result
    else
      let result = None in refine_ result

  module Dfa = struct
    let[@def] rec member r (xs : t list) =
      match xs with [] -> false | x :: rest -> equal r x || member r rest

    let[@def] rec join (xs : t list) ys =
      match xs with [] -> ys | x :: rest -> x :: join rest ys

    let rec (join_member @ total) :
        (xs : t list) -> (ys : t list) -> (r : t) ->
        {u : unit | member r (join xs ys) === (member r xs || member r ys)}
          @ immutable contended =
      fun xs ys r ->
      let joined = join xs ys in
      let refine_ equation = join_def xs ys in
      let refine_ equation = member_def r xs in
      let refine_ equation = member_def r joined in
      let u = () in
      match xs with
      | [] -> refine_ u
      | _ :: rest ->
        let refine_ induction = join_member rest ys r in
        refine_ u

    let[@def] rec suffix (xs : t list) b =
      match xs with [] -> [] | a :: rest -> Seq (a, b) :: suffix rest b

    let rec (suffix_member @ total) :
        (xs : t list) -> (b : t) -> (r : t) ->
        {u : unit | member r (suffix xs b) ===
          (match r with Seq (a, tail) -> tail === b && member a xs | _ -> false)}
          @ immutable contended =
      fun xs b r ->
      let result = suffix xs b in
      let refine_ equation = suffix_def xs b in
      let refine_ equation = member_def r result in
      let u = () in
      match xs with
      | [] ->
        (match r with
         | Seq (a, _) ->
           let refine_ equation = member_def a xs in
           refine_ u
         | _ -> refine_ u)
      | a :: rest ->
        let head = Seq (a, b) in
        let refine_ equality = equal_correct r head in
        let refine_ induction = suffix_member rest b r in
        (match r with
         | Seq (left, _) ->
           let refine_ equation = member_def left xs in
           let refine_ equality = equal_correct left a in
           refine_ u
         | _ -> refine_ u)

    let[@def] rec support r =
      match r with
      | Empty | Epsilon -> []
      | Symbol _ -> [Epsilon]
      | Alt (a, b) -> join (support a) (support b)
      | Seq (a, b) -> join (suffix (support a) b) (support b)
      | Star a -> suffix (support a) r

    let rec (support_closed @ total) : (r : t) -> (p : t) -> (q : t) ->
        {u : unit | if member p (support r) && member q (support p)
          then member q (support r) else true} @ immutable contended =
      fun r p q ->
      let sr = support r in
      let sp = support p in
      let refine_ equation = support_def r in
      let refine_ equation = support_def p in
      let u = () in
      match r with
      | Empty | Epsilon ->
        let refine_ equation = member_def p sr in
        refine_ u
      | Symbol _ ->
        let refine_ equation = member_def p sr in
        let epsilon = Epsilon in
        let refine_ equality = equal_correct p epsilon in
        let nil = [] in
        let refine_ equation = member_def p nil in
        let refine_ equation = member_def q sp in
        refine_ u
      | Alt (a, b) ->
        let sa = support a in
        let sb = support b in
        let refine_ equation = join_member sa sb p in
        let refine_ equation = join_member sa sb q in
        let refine_ induction = support_closed a p q in
        let refine_ induction = support_closed b p q in
        refine_ u
      | Seq (a, b) ->
        let sa = support a in
        let sb = support b in
        let left = suffix sa b in
        let refine_ equation = join_member left sb p in
        let refine_ equation = join_member left sb q in
        let refine_ equation = suffix_member sa b p in
        let refine_ equation = suffix_member sa b q in
        let refine_ induction = support_closed b p q in
        (match p with
         | Seq (inner, tail) ->
           let si = support inner in
           let st = support tail in
           let mapped = suffix si tail in
           let refine_ equation = join_member mapped st q in
           let refine_ equation = suffix_member si tail q in
           (match q with
            | Seq (next, _) ->
              let refine_ induction = support_closed a inner next in
              refine_ u
            | _ -> refine_ u)
         | _ -> refine_ u)
      | Star a ->
        let sa = support a in
        let refine_ equation = suffix_member sa r p in
        let refine_ equation = suffix_member sa r q in
        (match p with
         | Seq (inner, tail) ->
           let si = support inner in
           let st = support tail in
           let mapped = suffix si tail in
           let refine_ equation = join_member mapped st q in
           let refine_ equation = suffix_member si tail q in
           (match q with
            | Seq (next, _) ->
              let refine_ induction = support_closed a inner next in
              refine_ u
            | _ -> refine_ u)
         | _ -> refine_ u)

    let[@def] rec partial c r =
      match r with
      | Empty | Epsilon -> []
      | Symbol d -> if c = d then [Epsilon] else []
      | Alt (a, b) -> join (partial c a) (partial c b)
      | Seq (a, b) ->
        let left = suffix (partial c a) b in
        if nullable a then join left (partial c b) else left
      | Star a -> suffix (partial c a) r

    let rec (partial_supported @ total) : (r : t) -> (c : int) -> (p : t) ->
        {u : unit | if member p (partial c r) then member p (support r) else true}
          @ immutable contended =
      fun r c p ->
      let refine_ equation = partial_def c r in
      let refine_ equation = support_def r in
      let u = () in
      match r with
      | Empty | Epsilon | Symbol _ ->
        let nil = [] in
        let refine_ equation = member_def p nil in
        refine_ u
      | Alt (a, b) ->
        let da = partial c a in
        let db = partial c b in
        let sa = support a in
        let sb = support b in
        let refine_ equation = join_member da db p in
        let refine_ equation = join_member sa sb p in
        let refine_ induction = partial_supported a c p in
        let refine_ induction = partial_supported b c p in
        refine_ u
      | Seq (a, b) ->
        let da = partial c a in
        let db = partial c b in
        let sa = support a in
        let sb = support b in
        let dl = suffix da b in
        let sl = suffix sa b in
        let refine_ equation = join_member dl db p in
        let refine_ equation = join_member sl sb p in
        let refine_ equation = suffix_member da b p in
        let refine_ equation = suffix_member sa b p in
        let refine_ induction = partial_supported b c p in
        (match p with
         | Seq (inner, _) ->
           let refine_ induction = partial_supported a c inner in
           refine_ u
         | _ -> refine_ u)
      | Star a ->
        let da = partial c a in
        let sa = support a in
        let refine_ equation = suffix_member da r p in
        let refine_ equation = suffix_member sa r p in
        (match p with
         | Seq (inner, _) ->
           let refine_ induction = partial_supported a c inner in
           refine_ u
         | _ -> refine_ u)
    let rec (partial_contract @ total) :
        (r : t) -> (c : int) -> (s : int list) -> (p : evidence) ->
        {choice : t * evidence |
          if valid r p && word p === c :: s then
            match choice with k, q ->
              member k (partial c r) && valid k q && word q === s
          else true} @ immutable contended =
      fun r c s p ->
      let derivative = partial c r in
      let refine_ equation = partial_def c r in
      let refine_ equation = valid_def r p in
      let refine_ equation = word_def p in
      let result =
        match p with
        | Epsilon_match | Star_empty | Symbol_match _ -> Epsilon, Epsilon_match
        | Alt_left inner ->
          (match r with
           | Alt (a, b) ->
             let refine_ choice = partial_contract a c s inner in
             let k, _ = choice in
             let da = partial c a in
             let db = partial c b in
             let refine_ equation = join_member da db k in
             choice
           | _ -> Empty, Epsilon_match)
        | Alt_right inner ->
          (match r with
           | Alt (a, b) ->
             let refine_ choice = partial_contract b c s inner in
             let k, _ = choice in
             let da = partial c a in
             let db = partial c b in
             let refine_ equation = join_member da db k in
             choice
           | _ -> Empty, Epsilon_match)
        | Seq_match (left, right) ->
          (match r with
           | Seq (a, b) ->
             let da = partial c a in
             let db = partial c b in
             let mapped = suffix da b in
             let lw = word left in
             let rw = word right in
             let refine_ equation = append_def lw rw in
             (match lw with
              | [] ->
                let refine_ proof = empty_complete a left in
                let refine_ choice = partial_contract b c s right in
                let k, _ = choice in
                let refine_ equation = join_member mapped db k in
                choice
              | h :: rest ->
                let refine_ choice = partial_contract a h rest left in
                let k, inner = choice in
                let target = Seq (k, b) in
                let q = Seq_match (inner, right) in
                let refine_ equation = suffix_member da b target in
                let refine_ equation = join_member mapped db target in
                let refine_ equation = valid_def target q in
                let refine_ equation = word_def q in
                target, q)
           | _ -> Empty, Epsilon_match)
        | Star_step (left, right) ->
          (match r with
           | Star a ->
             let da = partial c a in
             let lw = word left in
             let rw = word right in
             let refine_ equation = append_def lw rw in
             (match lw with
              | [] ->
                let refine_ choice = partial_contract r c s right in
                choice
              | h :: rest ->
                let refine_ choice = partial_contract a h rest left in
                let k, inner = choice in
                let target = Seq (k, r) in
                let q = Seq_match (inner, right) in
                let refine_ equation = suffix_member da r target in
                let refine_ equation = valid_def target q in
                let refine_ equation = word_def q in
                target, q)
           | _ -> Empty, Epsilon_match)
      in
      let k, q = result in
      let refine_ equation = member_def k derivative in
      let nil = [] in
      let refine_ equation = member_def k nil in
      let epsilon = Epsilon in
      let refine_ equation = equal_correct k epsilon in
      let refine_ equation = valid_def k q in
      let refine_ equation = word_def q in
      refine_ result

    let rec (partial_expand @ total) :
        (r : t) -> (c : int) -> (k : t) -> (p : evidence) ->
        {q : evidence | if member k (partial c r) && valid k p
          then valid r q && word q === c :: word p else true}
          @ immutable contended =
      fun r c k p ->
      let derivative = partial c r in
      let refine_ equation = partial_def c r in
      let refine_ equation = member_def k derivative in
      let refine_ equation = valid_def k p in
      let refine_ equation = word_def p in
      let nil = [] in
      let refine_ equation = member_def k nil in
      let epsilon_regex = Epsilon in
      let refine_ equality = equal_correct k epsilon_regex in
      let result =
        match r with
        | Empty | Epsilon -> Epsilon_match
        | Symbol _ -> Symbol_match c
        | Alt (a, b) ->
          let da = partial c a in
          let db = partial c b in
          let refine_ equation = join_member da db k in
          let q =
            if member k da then
              let refine_ q = partial_expand a c k p in Alt_left q
            else
              let refine_ q = partial_expand b c k p in Alt_right q
          in
          let refine_ equation = valid_def r q in
          let refine_ equation = word_def q in
          q
        | Seq (a, b) ->
          let da = partial c a in
          let db = partial c b in
          let mapped = suffix da b in
          let refine_ equation = join_member mapped db k in
          let refine_ equation = suffix_member da b k in
          if member k mapped then
            (match k, p with
             | Seq (inner, _), Seq_match (left, right) ->
               let refine_ q = partial_expand a c inner left in
               let result = Seq_match (q, right) in
               let refine_ equation = valid_def r result in
               let refine_ equation = word_def result in
               let qw = word q in
               let rw = word right in
               let refine_ equation = append_def qw rw in
               result
             | _ -> Epsilon_match)
          else
            let refine_ left = epsilon a in
            let refine_ q = partial_expand b c k p in
            let result = Seq_match (left, q) in
            let refine_ equation = valid_def r result in
            let refine_ equation = word_def result in
            let lw = word left in
            let qw = word q in
            let refine_ equation = append_def lw qw in
            result
        | Star a ->
          let da = partial c a in
          let refine_ equation = suffix_member da r k in
          (match k, p with
           | Seq (inner, _), Seq_match (left, right) ->
             let refine_ q = partial_expand a c inner left in
             let result = Star_step (q, right) in
             let refine_ equation = valid_def r result in
             let refine_ equation = word_def result in
             let qw = word q in
             let rw = word right in
             let refine_ equation = append_def qw rw in
             result
           | _ -> Epsilon_match)
      in
      let refine_ equation = valid_def r result in
      let refine_ equation = word_def result in
      refine_ result

    let[@def] rec accepts (xs : t list) (s : int list) =
      match xs with [] -> false | r :: rest -> matches r s || accepts rest s

    let rec (accepts_member @ total) :
        (xs : t list) -> (r : t) -> (s : int list) ->
        {u : unit | if member r xs && matches r s then accepts xs s else true}
          @ immutable contended =
      fun xs r s ->
      let refine_ equation = member_def r xs in
      let refine_ equation = accepts_def xs s in
      let u = () in
      match xs with
      | [] -> refine_ u
      | head :: rest ->
        let refine_ equality = equal_correct r head in
        let refine_ induction = accepts_member rest r s in
        refine_ u

    let rec (accepts_pick @ total) : (xs : t list) -> (s : int list) ->
        {r : t | if accepts xs s then member r xs && matches r s else true}
          @ immutable contended =
      fun xs s ->
      let refine_ equation = accepts_def xs s in
      let result =
        match xs with
        | [] -> Empty
        | head :: rest ->
          if matches head s then
            let refine_ equality = equal_correct head head in
            let refine_ equation = member_def head xs in
            head
          else
            let refine_ r = accepts_pick rest s in
            let refine_ equation = member_def r xs in
            r
      in
      refine_ result

    let (partial_correct @ total) (r @ total) c s :
        {u : unit | accepts (partial c r) s === matches r (c :: s)} =
      let derivative = partial c r in
      let whole = c :: s in
      let u = () in
      if matches r whole then
        let refine_ p = sound r whole in
        let refine_ choice = partial_contract r c s p in
        let k, q = choice in
        let refine_ proof = complete k s q in
        let refine_ proof = accepts_member derivative k s in
        refine_ u
      else if accepts derivative s then
        let refine_ k = accepts_pick derivative s in
        let refine_ p = sound k s in
        let refine_ q = partial_expand r c k p in
        let refine_ proof = complete r whole q in
        refine_ u
      else refine_ u

    let[@def] rec same_state (xs : t list) (ys : t list) =
      match xs with
      | [] -> (match ys with [] -> true | _ :: _ -> false)
      | x :: rest ->
        (match ys with [] -> false | y :: tail -> equal x y && same_state rest tail)

    let rec (same_state_correct @ total) : (xs : t list) -> (ys : t list) ->
        {u : unit | same_state xs ys === (xs === ys)} @ immutable contended =
      fun xs ys ->
      let refine_ equation = same_state_def xs ys in
      let u = () in
      match xs with
      | [] -> refine_ u
      | x :: rest ->
        (match ys with
         | [] -> refine_ u
         | y :: tail ->
           let refine_ equality = equal_correct x y in
           let refine_ induction = same_state_correct rest tail in
           refine_ u)

    let[@def] rec has_state (xs : t list) (states : t list list) =
      match states with [] -> false | state :: rest -> same_state xs state || has_state xs rest

    let[@def] rec combine (xs : t list list) ys =
      match xs with [] -> ys | x :: rest -> x :: combine rest ys

    let rec (combine_has @ total) :
        (xs : t list list) -> (ys : t list list) -> (state : t list) ->
        {u : unit | has_state state (combine xs ys) ===
          (has_state state xs || has_state state ys)} @ immutable contended =
      fun xs ys state ->
      let joined = combine xs ys in
      let refine_ equation = combine_def xs ys in
      let refine_ equation = has_state_def state xs in
      let refine_ equation = has_state_def state joined in
      let u = () in
      match xs with
      | [] -> refine_ u
      | _ :: rest ->
        let refine_ induction = combine_has rest ys state in
        refine_ u

    let[@def] rec prepend r (states : t list list) =
      match states with [] -> [] | state :: rest -> (r :: state) :: prepend r rest

    let rec (prepend_has @ total) :
        (r : t) -> (states : t list list) -> (state : t list) ->
        {u : unit | has_state state (prepend r states) ===
          (match state with [] -> false | head :: rest -> head === r && has_state rest states)}
          @ immutable contended =
      fun r states state ->
      let result = prepend r states in
      let refine_ equation = prepend_def r states in
      let refine_ equation = has_state_def state result in
      let u = () in
      match states with
      | [] ->
        (match state with
         | [] -> refine_ u
         | _ :: rest ->
           let refine_ equation = has_state_def rest states in
           refine_ u)
      | first :: tail ->
        let head = r :: first in
        let refine_ equality = same_state_correct state head in
        let refine_ induction = prepend_has r tail state in
        (match state with
         | [] -> refine_ u
         | _ :: rest ->
           let refine_ equation = has_state_def rest states in
           let refine_ equality = same_state_correct rest first in
           refine_ u)

    let[@def] rec powerset (universe : t list) =
      match universe with
      | [] -> [[]]
      | head :: rest ->
        let smaller = powerset rest in
        combine smaller (prepend head smaller)

    let[@def] rec restrict (universe : t list) (candidates : t list) =
      match universe with
      | [] -> []
      | head :: rest ->
        if member head candidates then head :: restrict rest candidates
        else restrict rest candidates

    let rec (restrict_member @ total) :
        (universe : t list) -> (candidates : t list) -> (query : t) ->
        {u : unit | member query (restrict universe candidates) ===
          (member query universe && member query candidates)} @ immutable contended =
      fun universe candidates query ->
      let result = restrict universe candidates in
      let refine_ equation = restrict_def universe candidates in
      let refine_ equation = member_def query result in
      let refine_ equation = member_def query universe in
      let u = () in
      match universe with
      | [] -> refine_ u
      | head :: rest ->
        let refine_ equality = equal_correct query head in
        let refine_ induction = restrict_member rest candidates query in
        refine_ u

    let rec (powerset_cover @ total) : (universe : t list) -> (candidates : t list) ->
        {u : unit | has_state (restrict universe candidates) (powerset universe)}
          @ immutable contended =
      fun universe candidates ->
      let result = restrict universe candidates in
      let states = powerset universe in
      let refine_ equation = restrict_def universe candidates in
      let refine_ equation = powerset_def universe in
      let u = () in
      match universe with
      | [] ->
        let nil = [] in
        let refine_ equality = same_state_correct result nil in
        let refine_ equation = has_state_def result states in
        refine_ u
      | head :: rest ->
        let smaller = powerset rest in
        let extended = prepend head smaller in
        let refine_ induction = powerset_cover rest candidates in
        let refine_ equation = combine_has smaller extended result in
        let refine_ equation = prepend_has head smaller result in
        refine_ u

    let rec (powerset_member @ total) :
        (universe : t list) -> (state : t list) -> (query : t) ->
        {u : unit | if has_state state (powerset universe) && member query state
          then member query universe else true} @ immutable contended =
      fun universe state query ->
      let states = powerset universe in
      let refine_ equation = powerset_def universe in
      let refine_ equation = member_def query universe in
      let u = () in
      match universe with
      | [] ->
        let nil = [] in
        let refine_ equation = has_state_def state states in
        let refine_ equation = has_state_def state nil in
        let refine_ equality = same_state_correct state nil in
        let refine_ equation = member_def query state in
        refine_ u
      | head :: rest ->
        let smaller = powerset rest in
        let extended = prepend head smaller in
        let refine_ equation = combine_has smaller extended state in
        let refine_ equation = prepend_has head smaller state in
        let refine_ induction = powerset_member rest state query in
        (match state with
         | [] ->
           let refine_ equation = member_def query state in
           refine_ u
         | first :: tail ->
           let refine_ equation = member_def query state in
           let refine_ equality = equal_correct query first in
           let refine_ equality = equal_correct query head in
           let refine_ induction = powerset_member rest tail query in
           refine_ u)

    let[@def] rec step c (state : t list) =
      match state with [] -> [] | r :: rest -> join (partial c r) (step c rest)

    let rec (step_pick @ total) : (state : t list) -> (c : int) -> (query : t) ->
        {r : t | if member query (step c state)
          then member r state && member query (partial c r) else true}
          @ immutable contended =
      fun state c query ->
      let next = step c state in
      let refine_ equation = step_def c state in
      let result =
        match state with
        | [] ->
          let refine_ equation = member_def query next in
          Empty
        | head :: rest ->
          let dh = partial c head in
          let dr = step c rest in
          let refine_ equation = join_member dh dr query in
          if member query dh then
            let refine_ equation = member_def head state in
            let refine_ equality = equal_correct head head in
            head
          else
            let refine_ r = step_pick rest c query in
            let refine_ equation = member_def r state in
            r
      in
      refine_ result

    let (step_supported @ total) (root @ total) (state : t list) c query :
        {u : unit | if has_state state (powerset (root :: support root))
          && member query (step c state)
          then member query (root :: support root) else true} =
      let universe = root :: support root in
      let refine_ r = step_pick state c query in
      let refine_ proof = powerset_member universe state r in
      let refine_ proof = partial_supported r c query in
      let refine_ proof = support_closed root r query in
      let refine_ equation = member_def r universe in
      let refine_ equality = equal_correct r root in
      let refine_ equation = member_def query universe in
      let u = () in refine_ u

    let rec (accepts_join @ total) :
        (xs : t list) -> (ys : t list) -> (s : int list) ->
        {u : unit | accepts (join xs ys) s === (accepts xs s || accepts ys s)}
          @ immutable contended =
      fun xs ys s ->
      let joined = join xs ys in
      let refine_ equation = join_def xs ys in
      let refine_ equation = accepts_def xs s in
      let refine_ equation = accepts_def joined s in
      let u = () in
      match xs with
      | [] -> refine_ u
      | _ :: rest ->
        let refine_ induction = accepts_join rest ys s in
        refine_ u

    let rec (step_correct @ total) : (state : t list) -> (c : int) -> (s : int list) ->
        {u : unit | accepts (step c state) s === accepts state (c :: s)}
          @ immutable contended =
      fun state c s ->
      let next = step c state in
      let whole = c :: s in
      let refine_ equation = step_def c state in
      let refine_ equation = accepts_def state whole in
      let u = () in
      match state with
      | [] ->
        let refine_ equation = accepts_def next s in
        refine_ u
      | head :: rest ->
        let dh = partial c head in
        let dr = step c rest in
        let refine_ proof = accepts_join dh dr s in
        let refine_ proof = partial_correct head c s in
        let refine_ induction = step_correct rest c s in
        refine_ u

    let (next_correct @ total) (root @ total) (state : t list) c s :
        {u : unit | if has_state state (powerset (root :: support root)) then
          accepts (restrict (root :: support root) (step c state)) s
          === accepts state (c :: s) else true} =
      let universe = root :: support root in
      let next = step c state in
      let target = restrict universe next in
      let refine_ proof = step_correct state c s in
      let u = () in
      if accepts next s then
        let refine_ query = accepts_pick next s in
        let refine_ proof = step_supported root state c query in
        let refine_ proof = restrict_member universe next query in
        let refine_ proof = accepts_member target query s in
        refine_ u
      else if accepts target s then
        let refine_ query = accepts_pick target s in
        let refine_ proof = restrict_member universe next query in
        let refine_ proof = accepts_member next query s in
        refine_ u
      else refine_ u

    let[@def] rec has_letter c (letters : int list) =
      match letters with [] -> false | d :: rest -> c = d || has_letter c rest

    let rec (append_letter @ total) :
        (xs : int list) -> (ys : int list) -> (c : int) ->
        {u : unit | has_letter c (append xs ys) ===
          (has_letter c xs || has_letter c ys)} @ immutable contended =
      fun xs ys c ->
      let joined = append xs ys in
      let refine_ equation = append_def xs ys in
      let refine_ equation = has_letter_def c xs in
      let refine_ equation = has_letter_def c joined in
      let u = () in
      match xs with
      | [] -> refine_ u
      | _ :: rest ->
        let refine_ induction = append_letter rest ys c in
        refine_ u

    let[@def] rec letters r =
      match r with
      | Empty | Epsilon -> []
      | Symbol c -> [c]
      | Alt (a, b) | Seq (a, b) -> append (letters a) (letters b)
      | Star a -> letters a

    let rec (partial_letter @ total) : (r : t) -> (c : int) -> (q : t) ->
        {u : unit | if member q (partial c r) then has_letter c (letters r)
          else true} @ immutable contended =
      fun r c q ->
      let ds = partial c r in
      let alphabet = letters r in
      let refine_ equation = partial_def c r in
      let refine_ equation = letters_def r in
      let u = () in
      match r with
      | Empty | Epsilon ->
        let refine_ equation = member_def q ds in
        refine_ u
      | Symbol _ ->
        let nil = [] in
        let refine_ equation = member_def q nil in
        let refine_ equation = has_letter_def c alphabet in
        refine_ u
      | Alt (a, b) ->
        let da = partial c a in
        let db = partial c b in
        let la = letters a in
        let lb = letters b in
        let refine_ proof = append_letter la lb c in
        let refine_ proof = join_member da db q in
        let refine_ induction = partial_letter a c q in
        let refine_ induction = partial_letter b c q in
        refine_ u
      | Seq (a, b) ->
        let da = partial c a in
        let db = partial c b in
        let left = suffix da b in
        let la = letters a in
        let lb = letters b in
        let refine_ proof = append_letter la lb c in
        let refine_ proof = join_member left db q in
        let refine_ proof = suffix_member da b q in
        let refine_ induction = partial_letter b c q in
        (match q with
         | Seq (inner, _) ->
           let refine_ induction = partial_letter a c inner in refine_ u
         | _ -> refine_ u)
      | Star a ->
        let da = partial c a in
        let refine_ proof = suffix_member da r q in
        (match q with
         | Seq (inner, _) ->
           let refine_ induction = partial_letter a c inner in refine_ u
         | _ -> refine_ u)

    let[@def] rec alphabet (universe : t list) =
      match universe with
      | [] -> []
      | r :: rest -> append (letters r) (alphabet rest)

    let rec (alphabet_member @ total) :
        (universe : t list) -> (r : t) -> (c : int) ->
        {u : unit | if member r universe && has_letter c (letters r)
          then has_letter c (alphabet universe) else true}
          @ immutable contended =
      fun universe r c ->
      let refine_ equation = member_def r universe in
      let refine_ equation = alphabet_def universe in
      let u = () in
      match universe with
      | [] -> refine_ u
      | head :: rest ->
        let lh = letters head in
        let lr = alphabet rest in
        let refine_ proof = append_letter lh lr c in
        let refine_ proof = equal_correct r head in
        let refine_ induction = alphabet_member rest r c in
        refine_ u

    let (step_letter @ total) (universe : t list) state c query :
        {u : unit | if has_state state (powerset universe)
          && member query (step c state)
          then has_letter c (alphabet universe) else true} =
      let refine_ r = step_pick state c query in
      let refine_ proof = powerset_member universe state r in
      let refine_ proof = partial_letter r c query in
      let refine_ proof = alphabet_member universe r c in
      let u = () in refine_ u

    type row = (int * t list) list

    let[@def] rec build_row universe state (labels : int list) : row =
      match labels with
      | [] -> []
      | c :: rest ->
        (c, restrict universe (step c state)) :: build_row universe state rest

    let[@def] rec transition (row : row) c =
      match row with
      | [] -> []
      | (d, target) :: rest -> if c = d then target else transition rest c

    let rec (build_row_correct @ total) :
        (universe : t list) -> (state : t list) -> (labels : int list) -> (c : int) ->
        {u : unit | transition (build_row universe state labels) c ===
          (if has_letter c labels then restrict universe (step c state) else [])}
          @ immutable contended =
      fun universe state labels c ->
      let row = build_row universe state labels in
      let refine_ equation = build_row_def universe state labels in
      let refine_ equation = transition_def row c in
      let refine_ equation = has_letter_def c labels in
      let u = () in
      match labels with
      | [] -> refine_ u
      | _ :: rest ->
        let refine_ induction = build_row_correct universe state rest c in
        refine_ u

    let rec (powerset_empty @ total) : (universe : t list) ->
        {u : unit | has_state [] (powerset universe)} @ immutable contended =
      fun universe ->
      let states = powerset universe in
      let nil = [] in
      let refine_ equation = powerset_def universe in
      let u = () in
      match universe with
      | [] ->
        let refine_ equation = has_state_def nil states in
        let refine_ proof = same_state_correct nil nil in
        refine_ u
      | head :: rest ->
        let smaller = powerset rest in
        let extended = prepend head smaller in
        let refine_ proof = combine_has smaller extended nil in
        let refine_ induction = powerset_empty rest in
        refine_ u

    let (row_closed @ total) (root @ total) (state : t list) c :
        {u : unit | has_state
          (transition (build_row (root :: support root) state
            (alphabet (root :: support root))) c)
          (powerset (root :: support root))} =
      let universe = root :: support root in
      let labels = alphabet universe in
      let next = step c state in
      let refine_ proof = build_row_correct universe state labels c in
      let refine_ proof = powerset_cover universe next in
      let u = () in
      if has_letter c labels then refine_ u
      else
        let refine_ proof = powerset_empty universe in
        refine_ u

    let (row_correct @ total) (root @ total) (state : t list) c s :
        {u : unit | if has_state state (powerset (root :: support root)) then
          accepts (transition (build_row (root :: support root) state
            (alphabet (root :: support root))) c) s === accepts state (c :: s)
          else true} =
      let universe = root :: support root in
      let labels = alphabet universe in
      let next = step c state in
      let nil = [] in
      let refine_ proof = build_row_correct universe state labels c in
      let refine_ proof = next_correct root state c s in
      let u = () in
      if has_letter c labels then refine_ u
      else
        let refine_ proof = step_correct state c s in
        let refine_ query = accepts_pick next s in
        let refine_ proof = step_letter universe state c query in
        let refine_ equation = accepts_def nil s in
        refine_ u

    type table = (t list * bool * row) list
    type automaton = t list * table

    let[@def] rec build_table universe (states : t list list) : table =
      match states with
      | [] -> []
      | state :: rest ->
        (state, accepts state [], build_row universe state (alphabet universe))
          :: build_table universe rest

    let[@def] rec final (table : table) state =
      match table with
      | [] -> false
      | (key, accepting, _) :: rest ->
        if same_state state key then accepting else final rest state

    let[@def] rec advance (table : table) state c =
      match table with
      | [] -> []
      | (key, _, row) :: rest ->
        if same_state state key then transition row c else advance rest state c

    let rec (table_final @ total) :
        (universe : t list) -> (states : t list list) -> (state : t list) ->
        {u : unit | if has_state state states then
          final (build_table universe states) state === accepts state [] else true}
          @ immutable contended =
      fun universe states state ->
      let table = build_table universe states in
      let refine_ equation = build_table_def universe states in
      let refine_ equation = final_def table state in
      let refine_ equation = has_state_def state states in
      let u = () in
      match states with
      | [] -> refine_ u
      | head :: rest ->
        let refine_ proof = same_state_correct state head in
        let refine_ induction = table_final universe rest state in
        refine_ u

    let rec (table_advance @ total) :
        (universe : t list) -> (states : t list list) -> (state : t list) -> (c : int) ->
        {u : unit | if has_state state states then
          advance (build_table universe states) state c ===
          transition (build_row universe state (alphabet universe)) c else true}
          @ immutable contended =
      fun universe states state c ->
      let table = build_table universe states in
      let refine_ equation = build_table_def universe states in
      let refine_ equation = advance_def table state c in
      let refine_ equation = has_state_def state states in
      let u = () in
      match states with
      | [] -> refine_ u
      | head :: rest ->
        let refine_ proof = same_state_correct state head in
        let refine_ induction = table_advance universe rest state c in
        refine_ u

    let[@def] rec execute (table : table) state (s : int list) =
      match s with
      | [] -> final table state
      | c :: rest -> execute table (advance table state c) rest

    let rec (execute_correct @ total) :
        (root : t) -> (s : int list) -> (state : t list) ->
        {u : unit | if has_state state (powerset (root :: support root)) then
          execute (build_table (root :: support root)
            (powerset (root :: support root))) state s === accepts state s
          else true} @ immutable contended =
      fun root s state ->
      let universe = root :: support root in
      let states = powerset universe in
      let table = build_table universe states in
      let refine_ equation = execute_def table state s in
      let u = () in
      match s with
      | [] ->
        let refine_ proof = table_final universe states state in
        refine_ u
      | c :: rest ->
        let target = advance table state c in
        let refine_ proof = table_advance universe states state c in
        let refine_ proof = row_closed root state c in
        let refine_ proof = row_correct root state c rest in
        let refine_ induction = execute_correct root rest target in
        refine_ u

    let[@def] compile root : automaton =
      let universe = root :: support root in
      ([root], build_table universe (powerset universe))

    let[@def] run (dfa : automaton) s =
      match dfa with (initial, table) -> execute table initial s

    let (correct @ total) (root @ total) (s : int list) :
        {u : unit | run (compile root) s === matches root s} =
      let universe = root :: support root in
      let residuals = support root in
      let smaller = powerset residuals in
      let extended = prepend root smaller in
      let initial = [root] in
      let nil = [] in
      let dfa = compile root in
      let refine_ equation = compile_def root in
      let refine_ equation = run_def dfa s in
      let refine_ equation = powerset_def universe in
      let refine_ proof = combine_has smaller extended initial in
      let refine_ proof = prepend_has root smaller initial in
      let refine_ proof = powerset_empty residuals in
      let refine_ proof = execute_correct root s initial in
      let refine_ equation = accepts_def initial s in
      let refine_ equation = accepts_def nil s in
      let u = () in refine_ u

    let (sound @ total) (root @ total) (s : int list) :
        {p : evidence | if run (compile root) s then valid root p && word p === s
          else true} =
      let refine_ proof = ghost_ (correct root s) in
      let refine_ p = sound root s in
      refine_ p

    let (complete @ total) (root @ total) (s : int list) (p : evidence) :
        {u : unit | if valid root p && word p === s then run (compile root) s
          else true} =
      let refine_ proof = correct root s in
      let refine_ proof = complete root s p in
      let u = () in refine_ u

  end

end;;
[%%expect{|
module Regex :
  sig
    type t =
        Empty
      | Epsilon
      | Symbol of int
      | Alt of t * t
      | Seq of t * t
      | Star of t
    [@@inductive]
    module Membership :
      sig
        type evidence =
            Epsilon_match
          | Symbol_match of int
          | Alt_left of evidence
          | Alt_right of evidence
          | Seq_match of evidence * evidence
          | Star_empty
          | Star_step of evidence * evidence
        [@@inductive]
        val word : evidence -> int list @@ total
        val valid : t -> evidence -> bool @@ total
      end
    val alt : t -> t -> t @@ total
    val nullable : t -> bool @@ total
    val derive : int -> t -> t @@ total
    val matches : t -> int list -> bool @@ total
    val sound :
      (r : t) ->
      (s : int list) ->
      {p : Membership.evidence
        | if matches r s
          then (Membership.valid r p) && ((Membership.word p) === s)
          else true}
      @@ total
    val complete :
      (r : t) ->
      (s : int list) ->
      (p : Membership.evidence) ->
      {u : unit
        | if (Membership.valid r p) && ((Membership.word p) === s)
          then matches r s
          else true}
      @@ total
    val recognize :
      (r : t) ->
      (s : int list) ->
      {result : Membership.evidence option
        | match result with
          | None -> (matches r s) === false
          | Some p ->
              (matches r s) &&
                ((Membership.valid r p) && ((Membership.word p) === s))}
      @@ total
    module Dfa :
      sig
        type automaton
        val compile : t -> automaton @@ total
        val run : automaton -> int list -> bool @@ total
        val correct :
          (root : t) ->
          (s : int list) ->
          {u : unit | (run (compile root) s) === (matches root s)} @@ total
        val sound :
          (root : t) ->
          (s : int list) ->
          {p : Membership.evidence
            | if run (compile root) s
              then (Membership.valid root p) && ((Membership.word p) === s)
              else true}
          @@ total
        val complete :
          (root : t) ->
          (s : int list) ->
          (p : Membership.evidence) ->
          {u : unit
            | if (Membership.valid root p) && ((Membership.word p) === s)
              then run (compile root) s
              else true}
          @@ total
      end
  end
|}]

module Dfa_client = struct
  let (verified @ total) (r @ total) (s : int list) :
      {result : bool | result === Regex.matches r s} =
    let dfa = Regex.Dfa.compile r in
    let result = Regex.Dfa.run dfa s in
    let refine_ proof = ghost_ (Regex.Dfa.correct r s) in
    refine_ result
end;;
[%%expect{|
module Dfa_client :
  sig
    val verified :
      (r : Regex.t) ->
      (s : int list) -> {result : bool | result === (Regex.matches r s)}
  end
|}]

let () =
  let r = Regex.Star (Regex.Symbol 0) in
  let yes = [0; 0] in
  let no = [0; 1] in
  let refine_ accepted = Dfa_client.verified r yes in
  let refine_ rejected = Dfa_client.verified r no in
  assert (accepted && not rejected)
;;
[%%expect{|
|}]

let () =
  let open Regex in
  let open Membership in
  let rec splits = function
    | [] -> [([], [])]
    | c :: rest as s ->
      ([], s) :: List.map (fun (prefix, suffix) -> c :: prefix, suffix) (splits rest)
  in
  let rec member r s =
    match r with
    | Empty -> false
    | Epsilon -> s = []
    | Symbol c -> s = [c]
    | Alt (a, b) -> member a s || member b s
    | Seq (a, b) ->
      List.exists (fun (prefix, suffix) -> member a prefix && member b suffix)
        (splits s)
    | Star a ->
      s = [] ||
      List.exists (fun (prefix, suffix) ->
        prefix <> [] && member a prefix && member r suffix) (splits s)
  in
  let atoms = [Empty; Epsilon; Symbol 0; Symbol 1] in
  let layer children =
    atoms @ List.map (fun r -> Star r) children @
    List.concat_map (fun a ->
      List.concat_map (fun b -> [Alt (a, b); Seq (a, b)]) children) children
  in
  let regexes = layer (layer atoms) in
  let rec words n =
    if n = 0 then [[]]
    else [] :: List.concat_map (fun rest -> [0 :: rest; 1 :: rest]) (words (n - 1))
  in
  let inputs = words 3 in
  let dfa_inputs = inputs @ [[2]; [0; 2]; [2; 0]; [min_int]; [max_int]] in
  List.iter (fun (r : t) ->
    let dfa = Dfa.compile r in
    List.iter (fun s -> assert (Dfa.run dfa s = member r s)) dfa_inputs;
    List.iter (fun (s : int list) ->
      let expected = member r s in
      assert (matches r s = expected);
      let refine_ result = recognize r s in
      match result with
      | None -> assert (not expected)
      | Some (p : evidence) ->
        assert (expected && valid r p && word p = s);
        let refine_ proof = ghost_ (complete r s p) in
        ()) inputs) regexes;
  Format.printf "split-spec agreement: %d regexes x %d words@."
    (List.length regexes) (List.length inputs);
  Format.printf "DFA split-spec agreement: %d regexes x %d words@."
    (List.length regexes) (List.length dfa_inputs);
  let a = Alt (Epsilon, Symbol 0) in
  let r = Star a in
  let p = Star_step (Alt_left Epsilon_match,
    Star_step (Alt_right (Symbol_match 0),
      Star_step (Alt_left Epsilon_match, Star_empty))) in
  let s = [0] in
  assert (valid r p && word p = s);
  let refine_ proof = ghost_ (complete r s p) in
  let refine_ proof = ghost_ (Dfa.complete r s p) in
  let refine_ witness = Dfa.sound r s in
  assert (valid r witness && word witness = s);
  assert (Dfa.run (Dfa.compile r) s);
  assert (matches r s);
  let r = Star (Star (Symbol 0)) in
  let p = Star_step (Star_empty,
    Star_step (Star_step (Symbol_match 0, Star_empty), Star_empty)) in
  assert (valid r p && word p = s);
  let refine_ proof = ghost_ (complete r s p) in
  let refine_ proof = ghost_ (Dfa.complete r s p) in
  let refine_ witness = Dfa.sound r s in
  assert (valid r witness && word witness = s);
  assert (Dfa.run (Dfa.compile r) s);
  assert (matches r s);
  Format.printf "completeness: empty repetitions and nested stars@.";
  let rewrites =
    [Alt (Empty, Symbol 0), Epsilon;
     Alt (Symbol 0, Empty), Epsilon;
     Alt (Symbol 0, Symbol 0), Epsilon;
     Seq (Symbol 1, Symbol 0), Empty;
     Seq (Symbol 0, Empty), Empty;
     Seq (Epsilon, Symbol 0), Epsilon;
     Seq (Symbol 0, Epsilon), Epsilon;
     Star (Symbol 0), Star (Symbol 0);
     Alt (Seq (Symbol 0, Symbol 1), Seq (Symbol 0, Symbol 1)), Symbol 1;
     Alt (Seq (Symbol 0, Symbol 1), Seq (Symbol 0, Symbol 2)),
       Alt (Symbol 1, Symbol 2)]
  in
  List.iter (fun (r, expected) -> assert (derive 0 r = expected)) rewrites;
  let residual = Star (Alt (Epsilon, Symbol 0)) in
  let state = List.fold_left (fun state _ ->
    let next = derive 0 state in
    assert (next = residual);
    next) residual (List.init 128 (fun i -> i)) in
  assert (nullable state);
  Format.printf "simplification: %d derivative shapes; stable nullable star@."
    (List.length rewrites);
  let samples = layer atoms in
  List.iter (fun a ->
    let canonical = alt a Empty in
    assert (alt canonical canonical = canonical);
    assert (alt canonical Empty = canonical);
    List.iter (fun b ->
      assert (alt a b = alt b a);
      List.iter (fun c ->
        assert (alt (alt a b) c = alt a (alt b c))) samples) samples) samples;
  let extreme = alt (Symbol max_int) (alt (Symbol 0) (Symbol min_int)) in
  let dfa = Dfa.compile extreme in
  List.iter (fun s -> assert (Dfa.run dfa s = member extreme s))
    [[]; [min_int]; [max_int]; [0]; [-1]; [1]; [min_int; max_int]];
  assert (extreme = Alt (Symbol min_int, Alt (Symbol 0, Symbol max_int)));
  let star = Star (Symbol 0) in
  let repeated = Seq (star, star) in
  let first = derive 0 repeated in
  assert (first = Alt (repeated, star));
  let last = List.fold_left (fun state _ ->
    let next = derive 0 state in
    assert (next = first);
    next) first (List.init 128 (fun i -> i)) in
  assert (derive 1 last = Empty);
  Format.printf "ACI: canonical alternatives; stable a*a* derivatives@.";
  let closure r =
    let seen = Hashtbl.create 16 in
    let pending = Queue.create () in
    let add r =
      if not (Hashtbl.mem seen r) then begin
        assert (Hashtbl.length seen < 128);
        Hashtbl.add seen r ();
        Queue.add r pending
      end
    in
    add r;
    while not (Queue.is_empty pending) do
      let state = Queue.take pending in
      List.iter (fun c -> add (derive c state)) [0; 1; 2]
    done;
    Hashtbl.length seen
  in
  let maximum = List.fold_left (fun largest r -> max largest (closure r)) 0 regexes in
  Format.printf "derivative closures: %d regexes; maximum %d states@."
    (List.length regexes) maximum;
  let alphabet = Alt (Symbol 0, Symbol 1) in
  let suffix = Seq (alphabet, Seq (alphabet, Seq (alphabet, alphabet))) in
  let lookback = Seq (Star alphabet, Seq (Symbol 0, suffix)) in
  let dfa = Dfa.compile lookback in
  List.iter (fun s ->
    assert (matches lookback s = member lookback s);
    assert (Dfa.run dfa s = member lookback s)) (words 6);
  Format.printf "fifth-from-last symbol: %d derivative states@." (closure lookback);
  List.iter (fun (label, r, s, expected) ->
    assert (matches r s = expected);
    Format.printf "%s: %b@." label expected)
    ["empty language", Empty, [], false;
     "epsilon", Epsilon, [], true;
     "nullable concatenation", Seq (Star (Symbol 0), Symbol 1), [1], true;
     "star continuation", Star (Symbol 0), [0; 0], true;
     "full-input rejection", Symbol 0, [0; 1], false;
     "nested nullable star", Star (Star (Alt (Epsilon, Symbol 0))), [0; 0], true;
     "symbol mismatch", Star (Symbol 0), [1], false]
;;
[%%expect{|
split-spec agreement: 3244 regexes x 15 words
DFA split-spec agreement: 3244 regexes x 20 words
completeness: empty repetitions and nested stars
simplification: 10 derivative shapes; stable nullable star
ACI: canonical alternatives; stable a*a* derivatives
derivative closures: 3244 regexes; maximum 6 states
fifth-from-last symbol: 33 derivative states
empty language: false
epsilon: true
nullable concatenation: true
star continuation: true
full-input rejection: false
nested nullable star: true
symbol mismatch: false
|}]

let fabricated_evidence r s :
    {p : Regex.Membership.evidence |
      if Regex.matches r s then
        Regex.Membership.valid r p && Regex.Membership.word p === s
      else true} =
  let p = Regex.Membership.Epsilon_match in
  refine_ p
;;
[%%expect{|
Line 7, characters 2-11:
7 |   refine_ p
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let reversed_completeness r s p :
    {u : unit |
      if Regex.Membership.valid r p && Regex.Membership.word p === s
      then Regex.matches r s === false else true} =
  let refine_ proof = Regex.complete r s p in
  let u = () in
  refine_ u
;;
[%%expect{|
Line 7, characters 2-11:
7 |   refine_ u
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let reversed_dfa_completeness r s p :
    {u : unit |
      if Regex.Membership.valid r p && Regex.Membership.word p === s
      then Regex.Dfa.run (Regex.Dfa.compile r) s === false else true} =
  let refine_ proof = Regex.Dfa.complete r s p in
  let u = () in
  refine_ u
;;
[%%expect{|
Line 7, characters 2-11:
7 |   refine_ u
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
