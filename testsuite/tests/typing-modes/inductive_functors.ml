(* TEST
   expect;
*)

module type Ordered = sig
  type t : immutable_data mod total
  val equal : t -> t -> bool @@ total
end

module List_set (Element : Ordered) = struct
  type t = Nil | Cons of Element.t * t [@@inductive]

  let (head @ total) xs =
    match xs with
    | Nil -> None
    | Cons (head, _) -> Some head
end

module Int = struct
  type t = int
  external equal : int -> int -> bool @@ total = "%equal"
end

module Int_set = List_set (Int)

let found = Int_set.head (Int_set.Cons (1, Int_set.Nil))
[%%expect{|
module type Ordered =
  sig type t : immutable_data val equal : t -> t -> bool @@ total end
module List_set :
  functor (Element : Ordered) ->
    sig
      type t = Nil | Cons of Element.t * t
      [@@inductive]
      val head : t -> Element.t option
    end
module Int :
  sig type t = int external equal : int -> int -> bool = "%equal" end
module Int_set :
  sig
    type t = List_set(Int).t = Nil | Cons of Int.t * t
    [@@inductive]
    val head : t -> Int.t option
  end
val found : Int.t option = Some 1
|}]

module Roller (Payload : sig type t end) = struct
  type t = Roll of (Payload.t -> int) [@@inductive]
  let (unroll @ total) = function Roll function_ -> function_
end
[%%expect{|
module Roller :
  functor (Payload : sig type t end) ->
    sig
      type t = Roll of (Payload.t -> int)
      [@@inductive]
      val unroll : t -> Payload.t -> int
    end
|}]

module Direct_attack = struct
  module rec Closed : sig
    type t = Stop | Step of t [@@inductive]
    val cycle : t
    val depth : t -> int @@ total
  end = struct
    type t = Stop | Step of t [@@inductive]
    let rec cycle = Step cycle
    let rec (depth @ total) = function
      | Stop -> 0
      | Step child -> 1 + depth child
  end
end
[%%expect{|
Lines 2-6, characters 22-5:
2 | ......................sig
3 |     type t = Stop | Step of t [@@inductive]
4 |     val cycle : t
5 |     val depth : t -> int @@ total
6 |   end.........
Error: Type "t" has an [@@inductive] guarantee, which is not allowed in a recursive module signature.
|}]

module Alias_attack = struct
  module rec Closed : sig
    type t = Roller(Closed).t
  end = struct
    type t = Roller(Closed).t
  end

  module R = Roller (Closed)
  let (delta @ total) (x : Closed.t) = R.unroll x x
  let (omega @ total) () = delta (R.Roll delta)
end
[%%expect{|
Lines 2-4, characters 22-5:
2 | ......................sig
3 |     type t = Roller(Closed).t
4 |   end.........
Error: Type "Roller(Closed).t" has an [@@inductive] guarantee, which is not allowed in a recursive module signature.
|}]

module Conversion_attack = struct
  module rec Closed : sig
    type t
    val into : t -> Roller(Closed).t @@ total
    val out : Roller(Closed).t -> t @@ total
  end = struct
    type t = Roller(Closed).t
    let (into @ total) value = value
    let (out @ total) value = value
  end

  module R = Roller (Closed)
  let (delta @ total) (x : Closed.t) = R.unroll (Closed.into x) x
  let (omega @ total) () = delta (Closed.out (R.Roll delta))
end
[%%expect{|
Line 4, characters 4-45:
4 |     val into : t -> Roller(Closed).t @@ total
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "into" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Closed : sig
  type t
  val pack : (t -> int) -> t @@ total
  val apply : t -> t -> int @@ total
end = struct
  module Local = struct
    type t = T of (Closed.t -> int) [@@inductive]
    let (pack @ total) x = T x
    let (apply @ total) (T f) x = f x
  end
  type t = Local.t
  let pack = Local.pack
  let apply = Local.apply
end
let (delta @ total) x = Closed.apply x x
let (omega @ total) () = delta (Closed.pack delta)
[%%expect{|
Line 3, characters 2-37:
3 |   val pack : (t -> int) -> t @@ total
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "pack" exposes a total value whose type depends on the current recursive module group.
|}]

module type Eliminable = sig
  type payload
  type t = Roll of (payload -> int) [@@inductive]
  val unroll : t -> (payload -> int) @@ total
end

module First_class_attack = struct
  module rec Closed :
    Eliminable with type payload = Closed.t = struct
    type payload = Closed.t
    type t = Roll of (payload -> int) [@@inductive]
    let (unroll @ total) = function Roll function_ -> function_
  end

  let (delta @ total) x = Closed.unroll x x
  let (omega @ total) () = delta (Closed.Roll delta)
end
[%%expect{|
module type Eliminable =
  sig
    type payload
    type t = Roll of (payload -> int)
    [@@inductive]
    val unroll : t -> payload -> int @@ total
  end
Line 9, characters 4-43:
9 |     Eliminable with type payload = Closed.t = struct
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "Closed.t" has an [@@inductive] guarantee, which is not allowed in a recursive module signature.
|}]

type 'a roll = Roll of ('a -> int) [@@inductive]

module rec Closed : sig
  type t
  val into : t -> t roll @@ total
  val out : t roll -> t @@ total
end = struct
  module Local = struct
    type t = T of Closed.t roll [@@inductive]
    let (into @ total) (T x) = x
    let (out @ total) x = T x
  end
  type t = Local.t
  let into = Local.into
  let out = Local.out
end

let (delta @ total) (x : Closed.t) =
  match Closed.into x with Roll f -> f x
let (omega @ total) () = delta (Closed.out (Roll delta))
[%%expect{|
type 'a roll = Roll of ('a -> int) [@@inductive]
Line 5, characters 2-33:
5 |   val into : t -> t roll @@ total
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "into" exposes a total value whose type depends on the current recursive module group.
|}]
