  $ cat >main.ml <<'EOF'
  > module M = struct module N = struct let x = () let y = () end end
  > module B = M.N
  > EOF

With index 0 only the first type is shown:
  $ $MERLIN single type-enclosing -position 2:7 -verbosity 0 -index 0 \
  > -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 7
        },
        "end": {
          "line": 2,
          "col": 8
        },
        "type": "(module M.N)",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 1,
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 2:7 -verbosity 1 -index 0 \
  > -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 7
        },
        "end": {
          "line": 2,
          "col": 8
        },
        "type": "sig val x : unit val y : unit end",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 1,
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ cat >main.ml <<'EOF'
  > module M = struct module N = List end
  > module B = M.N
  > EOF

With index 0 only the first type is shown:
  $ $MERLIN single type-enclosing -position 2:13 -verbosity 0 -index 0 \
  > -filename ./main.ml < ./main.ml  
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": "(module List)",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 1,
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 2,
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 2:13 -verbosity 1 -index 0 \
  > -filename ./main.ml < ./main.ml  
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": "sig
    type ('a : value_or_null) t = 'a list = [] | (::) of 'a * 'a list
    [@@inductive]
    val length : 'a list -> int @@ total
    val compare_lengths : 'a list -> 'b list -> int @@ total
    val compare_length_with : 'a list -> int -> int @@ total
    val is_empty : 'a list -> bool @@ total
    val cons : 'a -> 'a list -> 'a list @@ total
    val singleton : 'a -> 'a list @@ total
    val hd : 'a list -> 'a @@ portable
    val tl : 'a list -> 'a list @@ portable
    val nth : 'a list -> int -> 'a @@ portable
    val nth_opt : 'a list -> int -> 'a option @@ portable
    val rev : 'a list -> 'a list @@ total
    val init : int -> (int -> 'a) -> 'a list @@ portable
    val append : 'a list -> 'a list -> 'a list @@ total
    val rev_append : 'a list -> 'a list -> 'a list @@ total
    val concat : 'a list list -> 'a list @@ total
    val flatten : 'a list list -> 'a list @@ total
    val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool @@ total
    val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int @@ total
    val iter : ('a -> unit) -> 'a list -> unit @@ total
    val iteri : (int -> 'a -> unit) -> 'a list -> unit @@ total
    val map : ('a -> 'b) -> 'a list -> 'b list @@ total
    val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list @@ total
    val rev_map : ('a -> 'b) -> 'a list -> 'b list @@ total
    val filter_map : ('a -> 'b option) -> 'a list -> 'b list @@ total
    val concat_map : ('a -> 'b list) -> 'a list -> 'b list @@ total
    val fold_left_map :
      ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list @@ total
    val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc @@ total
    val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc @@ total
    val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit @@ portable
    val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list @@ portable
    val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list @@
      portable
    val fold_left2 :
      ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a list -> 'b list -> 'acc @@
      portable
    val fold_right2 :
      ('a -> 'b -> 'acc -> 'acc) -> 'a list -> 'b list -> 'acc -> 'acc @@
      portable
    val for_all : ('a -> bool) -> 'a list -> bool @@ total
    val exists : ('a -> bool) -> 'a list -> bool @@ total
    val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool @@ portable
    val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool @@ portable
    val mem : 'a @ local -> 'a list @ local -> bool @@ portable
    val memq : 'a @ local -> 'a list @ local -> bool @@ portable
    val find : ('a -> bool) -> 'a list -> 'a @@ portable
    val find_opt : ('a -> bool) -> 'a list -> 'a option @@ total
    val find_index : ('a -> bool) -> 'a list -> int option @@ total
    val find_map : ('a -> 'b option) -> 'a list -> 'b option @@ total
    val find_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b option @@ total
    val filter : ('a -> bool) -> 'a list -> 'a list @@ total
    val find_all : ('a -> bool) -> 'a list -> 'a list @@ total
    val filteri : (int -> 'a -> bool) -> 'a list -> 'a list @@ total
    val take : int -> 'a list -> 'a list @@ total
    val drop : int -> 'a list -> 'a list @@ total
    val take_while : ('a -> bool) -> 'a list -> 'a list @@ total
    val drop_while : ('a -> bool) -> 'a list -> 'a list @@ total
    val partition : ('a -> bool) -> 'a list -> 'a list * 'a list @@ total
    val partition_map :
      ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list @@ total
    val assoc : 'a -> ('a * 'b) list -> 'b @@ portable
    val assoc_opt : 'a -> ('a * 'b) list -> 'b option @@ portable
    val assq : 'a -> ('a * 'b) list -> 'b @@ portable
    val assq_opt : 'a -> ('a * 'b) list -> 'b option @@ portable
    val mem_assoc : 'a -> ('a * 'b) list -> bool @@ portable
    val mem_assq : 'a -> ('a * 'b) list -> bool @@ portable
    val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list @@ portable
    val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list @@ portable
    val split : ('a * 'b) list -> 'a list * 'b list @@ total
    val combine : 'a list -> 'b list -> ('a * 'b) list @@ portable
    val sort : ('a -> 'a -> int) -> 'a list -> 'a list @@ total
    val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list @@ total
    val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list @@ total
    val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list @@ total
    val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list @@ total
    val to_seq : 'a list -> 'a Seq.t @@ total
    val of_seq : 'a Seq.t -> 'a list @@ portable
    module Refined = List.Refined
  end",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 1,
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 2,
        "tail": "no"
      }
    ],
    "notifications": []
  }
