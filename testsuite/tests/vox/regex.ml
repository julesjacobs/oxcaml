(* TEST
 has-z3;
 readonly_files = "regex_semantics.ml regex_core.ml";
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

#use "regex_semantics.ml";;
#use "regex_core.ml";;
[%%expect{|
module Regex_semantics :
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
        val append : int list -> int list -> int list
        val append_def :
          (xs : int list) ->
          (ys : int list) ->
          {u : unit
            | (append xs ys) ===
                (match xs with | [] -> ys | x::rest -> x :: (append rest ys))}
        val word : evidence -> int list
        val word_def :
          (p' : evidence) ->
          {u : unit
            | (word p') ===
                (match p' with
                 | Epsilon_match | Star_empty -> []
                 | Symbol_match c -> [c]
                 | Alt_left p'' | Alt_right p'' -> word p''
                 | Seq_match (p, q) | Star_step (p, q) ->
                     append (word p) (word q))}
        val valid : t -> evidence -> bool
        val valid_def :
          (r : t) ->
          (p' : evidence) ->
          {u : unit
            | (valid r p') ===
                (match p' with
                 | Epsilon_match ->
                     (match r with | Epsilon -> true | _ -> false)
                 | Symbol_match c ->
                     (match r with | Symbol d -> c = d | _ -> false)
                 | Alt_left p'' ->
                     (match r with | Alt (a', _) -> valid a' p'' | _ -> false)
                 | Alt_right p''' ->
                     (match r with
                      | Alt (_, b') -> valid b' p'''
                      | _ -> false)
                 | Seq_match (p'''', q') ->
                     (match r with
                      | Seq (a'', b) -> (valid a'' p'''') && (valid b q')
                      | _ -> false)
                 | Star_empty -> (match r with | Star _ -> true | _ -> false)
                 | Star_step (p, q) ->
                     (match r with
                      | Star a -> (valid a p) && (valid r q)
                      | _ -> false))}
      end
  end
module Regex :
  sig
    type t =
      Regex_semantics.t =
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
          Regex_semantics.Membership.evidence =
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
        type automaton : value mod immutable
        type state = t list
        val compile : t @ total -> automaton @ total @@ total
        val run : automaton -> int list -> bool @@ total
        val initial : automaton -> state @@ total
        val states : automaton -> state list @@ total
        val contains_in : state -> state list -> bool @@ total
        val contains_in_empty :
          (source : state) -> {u : unit | (contains_in source []) === false}
          @@ total
        val contains_state : automaton -> state -> bool @@ total
        val contains_state_equation :
          (dfa : automaton) ->
          (source : state) ->
          {u : unit
            | (contains_state dfa source) ===
                (contains_in source (states dfa))}
          @@ total
        val output : automaton -> state -> bool @@ total
        val next : automaton -> state -> int -> state @@ total
        val compiled_next_member :
          (root : t) ->
          (source : state) ->
          (letter : int) ->
          {u : unit
            | if contains_state (compile root) source
              then
                contains_state (compile root)
                  (next (compile root) source letter)
              else true}
          @@ total
        val compiled_initial_member :
          (root : t) ->
          {u : unit | contains_state (compile root) (initial (compile root))}
          @@ total
        val compiled_empty_member :
          (root : t) -> {u : unit | contains_state (compile root) []} @@
          total
        val default : automaton -> state -> state @@ total
        val default_empty :
          (dfa : automaton) ->
          (source : state) -> {u : unit | (default dfa source) === []} @@
          total
        val labels : automaton -> state -> int list @@ total
        val label_member : int -> int list -> bool @@ total
        val label_member_empty :
          (letter : int) -> {u : unit | (label_member letter []) === false}
          @@ total
        val label_member_cons :
          (letter : int) ->
          (head : int) ->
          (rest : int list) ->
          {u : unit
            | (label_member letter (head :: rest)) ===
                ((letter = head) || (label_member letter rest))}
          @@ total
        val next_outside_labels :
          (dfa : automaton) ->
          (source : state) ->
          (letter : int) ->
          {u : unit
            | if not (label_member letter (labels dfa source))
              then (next dfa source letter) === []
              else true}
          @@ total
        val run_from : automaton -> state -> int list -> bool @@ total
        val same_state : state -> state -> bool @@ total
        val contains_in_cons :
          (source : state) ->
          (head : state) ->
          (rest : state list) ->
          {u : unit
            | (contains_in source (head :: rest)) ===
                ((same_state source head) || (contains_in source rest))}
          @@ total
        val same_state_correct :
          (left : state) ->
          (right : state) ->
          {u : unit | (same_state left right) === (left === right)} @@ total
        val run_from_empty :
          (dfa : automaton) ->
          (state : state) ->
          {u : unit | (run_from dfa state []) === (output dfa state)} @@
          total
        val run_from_letter :
          (dfa : automaton) ->
          (state : state) ->
          (letter : int) ->
          (suffix : int list) ->
          {u : unit
            | (run_from dfa state (letter :: suffix)) ===
                (run_from dfa (next dfa state letter) suffix)}
          @@ total
        val run_initial :
          (dfa : automaton) ->
          (word : int list) ->
          {u : unit | (run dfa word) === (run_from dfa (initial dfa) word)}
          @@ total
        val universe_size : t -> int @@ total
        val state_count : automaton -> int @@ total
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
    val membership_word :
      (p : Membership.evidence) ->
      {u : unit
        | (Membership.word p) === (Regex_semantics.Membership.word p)}
      @@ total
    val membership_valid :
      (r : t) ->
      (p : Membership.evidence) ->
      {u : unit
        | (Membership.valid r p) === (Regex_semantics.Membership.valid r p)}
      @@ total
  end
|}]

module Dfa_client = struct
  let (verified @ total) (r @ total) (s : int list) :
      {result : bool | result === Regex.matches r s} =
    let dfa = Regex.Dfa.compile r in
    let result = Regex.Dfa.run dfa s in
    ghost_ (Regex.Dfa.correct r s);
    result
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
  let accepted = Dfa_client.verified r yes in
  let rejected = Dfa_client.verified r no in
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
      let result = recognize r s in
      match result with
      | None -> assert (not expected)
      | Some (p : evidence) ->
        assert (expected && valid r p && word p = s);
        ghost_ (complete r s p);
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
  ghost_ (complete r s p);
  ghost_ (Dfa.complete r s p);
  let witness = Dfa.sound r s in
  assert (valid r witness && word witness = s);
  assert (Dfa.run (Dfa.compile r) s);
  assert (matches r s);
  let r = Star (Star (Symbol 0)) in
  let p = Star_step (Star_empty,
    Star_step (Star_step (Symbol_match 0, Star_empty), Star_empty)) in
  assert (valid r p && word p = s);
  ghost_ (complete r s p);
  ghost_ (Dfa.complete r s p);
  let witness = Dfa.sound r s in
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
  assert (Dfa.universe_size lookback = 12);
  assert (Dfa.state_count dfa = 4096);
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
  p
;;
[%%expect{|
Line 7, characters 2-3:
7 |   p
      ^
Error: Refinement could not be proved (counterexample)
|}]

let reversed_completeness r s p :
    {u : unit |
      if Regex.Membership.valid r p && Regex.Membership.word p === s
      then Regex.matches r s === false else true} =
  Regex.complete r s p;
  let u = () in
  u
;;
[%%expect{|
Line 7, characters 2-3:
7 |   u
      ^
Error: Refinement could not be proved (counterexample)
|}]

let reversed_dfa_completeness r s p :
    {u : unit |
      if Regex.Membership.valid r p && Regex.Membership.word p === s
      then Regex.Dfa.run (Regex.Dfa.compile r) s === false else true} =
  Regex.Dfa.complete r s p;
  let u = () in
  u
;;
[%%expect{|
Line 7, characters 2-3:
7 |   u
      ^
Error: Refinement could not be proved (counterexample)
|}]
