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

  let[@def] alt (a @ total) (b @ total) : t @ total =
    match a, b with
    | Empty, _ -> b
    | _, Empty -> a
    | _ -> if equal a b then a else Alt (a, b)

  let[@def] seq (a @ total) (b @ total) : t @ total =
    match a, b with
    | Empty, _ | _, Empty -> Empty
    | Epsilon, _ -> b
    | _, Epsilon -> a
    | _ -> Seq (a, b)

  let (alt_expand @ total) (a @ total) (b @ total) p :
      {q : evidence |
        if valid (alt a b) p then valid (Alt (a, b)) q && word q === word p
        else true} =
    let simplified = alt a b in
    let original = Alt (a, b) in
    let refine_ equation = alt_def a b in
    let refine_ equation = equal_correct a b in
    let refine_ equation = valid_def simplified p in
    let q = match a, b with
      | Empty, _ -> Alt_right p
      | _, Empty -> Alt_left p
      | _ -> if equal a b then Alt_left p else p
    in
    let refine_ equation = valid_def original q in
    let refine_ equation = word_def q in
    refine_ q

  let (alt_contract @ total) (a @ total) (b @ total) p :
      {q : evidence |
        if valid (Alt (a, b)) p then valid (alt a b) q && word q === word p
        else true} =
    let original = Alt (a, b) in
    let refine_ equation = alt_def a b in
    let refine_ equation = equal_correct a b in
    let refine_ equation = valid_def original p in
    let refine_ equation = word_def p in
    let q = match p with
      | Alt_left inner ->
        let refine_ equation = valid_def a inner in
        (match a, b with
         | Empty, _ -> Epsilon_match
         | _, Empty -> inner
         | _ -> if equal a b then inner else p)
      | Alt_right inner ->
        let refine_ equation = valid_def b inner in
        (match a, b with
         | Empty, _ -> inner
         | _, Empty -> Epsilon_match
         | _ -> if equal a b then inner else p)
      | _ -> Epsilon_match
    in
    refine_ q

  let (seq_expand @ total) (a @ total) (b @ total) p :
      {q : evidence |
        if valid (seq a b) p then valid (Seq (a, b)) q && word q === word p
        else true} =
    let simplified = seq a b in
    let original = Seq (a, b) in
    let refine_ equation = seq_def a b in
    let refine_ equation = valid_def simplified p in
    let empty = Epsilon_match in
    let refine_ equation = word_def empty in
    let epsilon = Epsilon in
    let refine_ equation = valid_def epsilon empty in
    let q = match a, b with
      | Empty, _ | _, Empty -> Epsilon_match
      | Epsilon, _ -> Seq_match (empty, p)
      | _, Epsilon -> Seq_match (p, empty)
      | _ -> p
    in
    let refine_ equation = valid_def original q in
    let refine_ equation = word_def q in
    let nil = [] in
    let pw = word p in
    let refine_ equation = append_def nil pw in
    let refine_ equation = append_nil pw in
    refine_ q

  let (seq_contract @ total) (a @ total) (b @ total) p :
      {q : evidence |
        if valid (Seq (a, b)) p then valid (seq a b) q && word q === word p
        else true} =
    let original = Seq (a, b) in
    let refine_ equation = seq_def a b in
    let refine_ equation = valid_def original p in
    let refine_ equation = word_def p in
    let q = match p with
      | Seq_match (left, right) ->
        let refine_ equation = valid_def a left in
        let refine_ equation = valid_def b right in
        let refine_ equation = word_def left in
        let refine_ equation = word_def right in
        let lw = word left in
        let rw = word right in
        let refine_ equation = append_def lw rw in
        let refine_ equation = append_nil lw in
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
    let refine_ equation = nullable_def r in
    let result =
      match r with
      | Empty | Epsilon | Symbol _ -> Epsilon_match
      | Star _ -> Star_empty
      | Alt (a, b) ->
        if nullable a then
          let refine_ p = epsilon a in
          let result = Alt_left p in
          let refine_ equation = valid_def r result in
          let refine_ equation = word_def result in
          result
        else
          let refine_ p = epsilon b in
          let result = Alt_right p in
          let refine_ equation = valid_def r result in
          let refine_ equation = word_def result in
          result
      | Seq (a, b) ->
        let refine_ p = epsilon a in
        let refine_ q = epsilon b in
        let result = Seq_match (p, q) in
        let refine_ equation = valid_def r result in
        let refine_ equation = word_def result in
        let left = word p in
        let right = word q in
        let refine_ equation = append_def left right in
        result
    in
    let refine_ equation = valid_def r result in
    let refine_ equation = word_def result in
    refine_ result
  let rec (expand @ total) : (r : t) -> (c : int) -> (p : evidence) ->
      {q : evidence |
        if valid (derive c r) p then valid r q && word q === c :: word p
        else true} @ immutable contended =
    fun r c p ->
    let derivative = derive c r in
    let refine_ equation = derive_def c r in
    let refine_ equation = valid_def derivative p in
    let refine_ equation = word_def p in
    let result =
      match r with
      | Empty | Epsilon -> Epsilon_match
      | Symbol _ -> Symbol_match c
      | Alt (a, b) ->
        let da = derive c a in
        let db = derive c b in
        let refine_ p = alt_expand da db p in
        let original = Alt (da, db) in
        let refine_ equation = valid_def original p in
        let refine_ equation = word_def p in
        (match p with
         | Alt_left inner ->
           let refine_ q = expand a c inner in
           let result = Alt_left q in
           let refine_ equation = valid_def r result in
           let refine_ equation = word_def result in
           result
         | Alt_right inner ->
           let refine_ q = expand b c inner in
           let result = Alt_right q in
           let refine_ equation = valid_def r result in
           let refine_ equation = word_def result in
           result
         | _ -> Epsilon_match)
      | Seq (a, b) ->
        let da = derive c a in
        let db = derive c b in
        let product = seq da b in
        if nullable a then
          let refine_ p = alt_expand product db p in
          let original = Alt (product, db) in
          let refine_ equation = valid_def original p in
          let refine_ equation = word_def p in
          (match p with
           | Alt_left inner ->
             let refine_ inner = seq_expand da b inner in
             let left_derivative = Seq (derive c a, b) in
             let refine_ equation = valid_def left_derivative inner in
             let refine_ equation = word_def inner in
             (match inner with
              | Seq_match (left, right) ->
                let refine_ q = expand a c left in
                let result = Seq_match (q, right) in
                let refine_ equation = valid_def r result in
                let refine_ equation = word_def result in
                let qw = word q in
                let rw = word right in
                let refine_ equation = append_def qw rw in
                result
              | _ -> Epsilon_match)
           | Alt_right right ->
             let refine_ left = epsilon a in
             let refine_ q = expand b c right in
             let result = Seq_match (left, q) in
             let refine_ equation = valid_def r result in
             let refine_ equation = word_def result in
             let lw = word left in
             let qw = word q in
             let refine_ equation = append_def lw qw in
             result
           | _ -> Epsilon_match)
        else
          let refine_ p = seq_expand da b p in
          let original = Seq (da, b) in
          let refine_ equation = valid_def original p in
          let refine_ equation = word_def p in
          (match p with
           | Seq_match (left, right) ->
             let refine_ q = expand a c left in
             let result = Seq_match (q, right) in
             let refine_ equation = valid_def r result in
             let refine_ equation = word_def result in
             let qw = word q in
             let rw = word right in
             let refine_ equation = append_def qw rw in
             result
           | _ -> Epsilon_match)
      | Star a ->
        let da = derive c a in
        let refine_ p = seq_expand da r p in
        let original = Seq (da, r) in
        let refine_ equation = valid_def original p in
        let refine_ equation = word_def p in
        (match p with
         | Seq_match (left, right) ->
           let refine_ q = expand a c left in
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
    let derivative = derive c r in
    let refine_ equation = derive_def c r in
    let refine_ equation = valid_def r p in
    let refine_ equation = word_def p in
    let result =
      match p with
      | Epsilon_match | Star_empty | Symbol_match _ -> Epsilon_match
      | Alt_left inner ->
        (match r with
         | Alt (a, b) ->
           let refine_ q = contract a c s inner in
           let da = derive c a in
           let db = derive c b in
           let original = Alt (da, db) in
           let result = Alt_left q in
           let refine_ equation = valid_def original result in
           let refine_ equation = word_def result in
           let refine_ result = alt_contract da db result in
           result
         | _ -> Epsilon_match)
      | Alt_right inner ->
        (match r with
         | Alt (a, b) ->
           let refine_ q = contract b c s inner in
           let da = derive c a in
           let db = derive c b in
           let original = Alt (da, db) in
           let result = Alt_right q in
           let refine_ equation = valid_def original result in
           let refine_ equation = word_def result in
           let refine_ result = alt_contract da db result in
           result
         | _ -> Epsilon_match)
      | Seq_match (left, right) ->
        (match r with
         | Seq (a, b) ->
           let da = derive c a in
           let db = derive c b in
           let product = seq da b in
           let sum = Alt (product, db) in
           let lw = word left in
           let rw = word right in
           let refine_ equation = append_def lw rw in
           (match lw with
            | [] ->
              let refine_ empty_law = empty_complete a left in
              let refine_ q = contract b c s right in
              let result = Alt_right q in
              let refine_ equation = valid_def sum result in
              let refine_ equation = word_def result in
              let refine_ result = alt_contract product db result in
              result
            | h :: rest ->
              let refine_ q = contract a h rest left in
              let pair = Seq_match (q, right) in
              let pair_regex = Seq (derive c a, b) in
              let refine_ equation = valid_def pair_regex pair in
              let refine_ equation = word_def pair in
              let refine_ pair = seq_contract da b pair in
              if nullable a then
                let result = Alt_left pair in
                let refine_ equation = valid_def sum result in
                let refine_ equation = word_def result in
                let refine_ result = alt_contract product db result in
                result
              else pair)
         | _ -> Epsilon_match)
      | Star_step (left, right) ->
        (match r with
         | Star a ->
           let lw = word left in
           let rw = word right in
           let refine_ equation = append_def lw rw in
           (match lw with
            | [] ->
              let refine_ q = contract r c s right in
              q
            | h :: rest ->
              let refine_ q = contract a h rest left in
              let da = derive c a in
              let original = Seq (da, r) in
              let result = Seq_match (q, right) in
              let refine_ equation = valid_def original result in
              let refine_ equation = word_def result in
              let refine_ result = seq_contract da r result in
              result)
         | _ -> Epsilon_match)
    in
    let refine_ equation = valid_def derivative result in
    let refine_ equation = word_def result in
    refine_ result
  let rec (sound @ total) : (r : t) -> (s : int list) ->
      {p : evidence | if matches r s then valid r p && word p === s else true}
        @ immutable contended =
    fun r s ->
    let refine_ equation = matches_def r s in
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
  end
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
  List.iter (fun r ->
    List.iter (fun s ->
      let expected = member r s in
      assert (matches r s = expected);
      let refine_ result = recognize r s in
      match result with
      | None -> assert (not expected)
      | Some p ->
        assert (expected && valid r p && word p = s);
        let refine_ proof = complete r s p in
        ()) inputs) regexes;
  Format.printf "split-spec agreement: %d regexes x %d words@."
    (List.length regexes) (List.length inputs);
  let a = Alt (Epsilon, Symbol 0) in
  let r = Star a in
  let p = Star_step (Alt_left Epsilon_match,
    Star_step (Alt_right (Symbol_match 0),
      Star_step (Alt_left Epsilon_match, Star_empty))) in
  let s = [0] in
  assert (valid r p && word p = s);
  let refine_ proof = complete r s p in
  assert (matches r s);
  let r = Star (Star (Symbol 0)) in
  let p = Star_step (Star_empty,
    Star_step (Star_step (Symbol_match 0, Star_empty), Star_empty)) in
  assert (valid r p && word p = s);
  let refine_ proof = complete r s p in
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
completeness: empty repetitions and nested stars
simplification: 10 derivative shapes; stable nullable star
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
