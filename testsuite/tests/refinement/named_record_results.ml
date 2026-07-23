(* TEST
 expect;
*)

type point =
  { x : int;
    y : int
  }

let make (n : int) : point{ _.x = n && _.y = n + 1 } =
  { x = n + 0;
    y = n + 1
  }

[%%expect{|
type point = { x : int; y : int; }
val make : (n : int) -> point{ _.x = n && _.y = n + 1 } = <fun>
|}]

let false_field (n : int) : point{ _.x = n + 1 } =
  { x = n;
    y = 0
  }

[%%expect{|
Lines 2-4, characters 2-3:
2 | ..{ x = n;
3 |     y = 0
4 |   }
Error: Refinement verification failed (disproved)
|}]

type cell = { mutable value : int }

let mutable_record (n : int) : cell{ _.value = n } =
  { value = n }

[%%expect{|
type cell = { mutable value : int; }
Line 3, characters 37-44:
3 | let mutable_record (n : int) : cell{ _.value = n } =
                                         ^^^^^^^
Error: Mutable record fields is not yet supported in refinements.
|}]

type functional =
  { run : int -> int;
    value : int
  }

let functional_record (n : int) : functional{ _.value = n } =
  { run = (fun x -> x);
    value = n
  }

[%%expect{|
type functional = { run : int -> int; value : int; }
Line 6, characters 34-59:
6 | let functional_record (n : int) : functional{ _.value = n } =
                                      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value refined by this predicate has type "functional",
       which is not modelable: it contains a function type,
       and a function value cannot be read in its own refinement predicate.
|}]
