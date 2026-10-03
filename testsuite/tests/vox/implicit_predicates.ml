(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 expect;
*)

module type Identity = sig
  val id : (x : int) -> {y : int | y = x} @@ total
end;;
[%%expect{|
module type Identity =
  sig val id : (x : int) -> {y : int | y = x} @@ total end
|}]

module Client (I : Identity) = struct
  let explicit () :
      {u : unit | let refine_ y = I.id 0 in y = 0} = ()

  let implicit () : {u : unit | I.id (I.id 0) = 0} = ()

  let branch (b : bool) :
      {u : unit | (if b then I.id (I.id 0) else I.id 0) = 0} = ()

  let local () :
      {u : unit | let x = 3 in let y = I.id (x + 1) in y = 4} = ()

  let matched (b : bool) :
      {u : unit | (match b with true -> I.id 0 | false -> I.id 0) = 0} = ()

  let constrained () : {u : unit | (I.id 0 : int) = 0} = ()

  let short_circuit (b : bool) :
      {u : unit | b || I.id 0 = 0} = ()

  let ghosted () :
      {u : unit | ghost_ (I.id (I.id 0)) === ghost_ 0} = ()

  let runtime () : {u : unit | I.id (I.id 0) = 0} =
    let u = () in assume_ u
end;;
[%%expect{|
module Client :
  functor (I : Identity) ->
    sig
      val explicit : unit -> {u : unit | let refine_ y = I.id 0 in y = 0}
      val implicit :
        unit -> {u : unit | let argument = I.id 0 in (I.id argument) = 0}
      val branch :
        (b : bool) ->
        {u : unit
          | (if b then let argument = I.id 0 in I.id argument else I.id 0) =
              0}
      val local :
        unit ->
        {u : unit
          | let x = 3 in
            let argument = x + 1 in let y = I.id argument in y = 4}
      val matched :
        (b : bool) ->
        {u : unit | (match b with | true -> I.id 0 | false -> I.id 0) = 0}
      val constrained : unit -> {u : unit | (I.id 0 : int) = 0}
      val short_circuit : (b : bool) -> {u : unit | b || ((I.id 0) = 0)}
      val ghosted :
        unit ->
        {u : unit
          | let argument = ghost_ (I.id 0) in
            (ghost_ (I.id argument)) === (ghost_ 0)}
      val runtime :
        unit -> {u : unit | let argument = I.id 0 in (I.id argument) = 0}
    end
|}]

module Wrong (I : Identity) = struct
  let wrong () : {u : unit | I.id (I.id 0) = 1} = ()
end;;
[%%expect{|
Line 2, characters 50-52:
2 |   let wrong () : {u : unit | I.id (I.id 0) = 1} = ()
                                                      ^^
Error: Refinement could not be proved (counterexample)
|}]

module Wrong_branch (I : Identity) = struct
  let wrong (b : bool) :
      {u : unit | (if b then I.id 0 else I.id 1) = 0} = ()
end;;
[%%expect{|
Line 3, characters 56-58:
3 |       {u : unit | (if b then I.id 0 else I.id 1) = 0} = ()
                                                            ^^
Error: Refinement could not be proved (counterexample)
|}]

module Checked : sig end = struct
module Implementation : Identity = struct
  let (id @ total) (x : int) : {y : int | y = x} = x
end
module Instance = Client (Implementation)
module Copy : module type of Instance = Instance
let () =
  Instance.explicit ();
  Instance.implicit ();
  Instance.branch true;
  Instance.branch false;
  Instance.local ();
  Instance.matched true;
  Instance.matched false;
  Instance.constrained ();
  Instance.short_circuit true;
  Instance.short_circuit false;
  Copy.ghosted ();
  Instance.runtime ()
end;;
[%%expect{|
module Checked : sig end
|}]

let destructured_pair (xs : {p : int * int | let x, y = p in x >= y} list) :
    {u : unit | match xs with [] -> true | (x, y) :: _ -> x >= y} = ();;
[%%expect{|
val destructured_pair :
  (xs : {p : int * int | match p with | (x, y) -> x >= y} list) ->
  {u : unit | match xs with | [] -> true | (x, y)::_ -> x >= y} = <fun>
|}]

let wrong_destructured_pair (xs : {p : int * int | let x, y = p in x >= y} list) :
    {u : unit | match xs with [] -> true | (x, y) :: _ -> y >= x} = ();;
[%%expect{|
Line 2, characters 68-70:
2 |     {u : unit | match xs with [] -> true | (x, y) :: _ -> y >= x} = ();;
                                                                        ^^
Error: Refinement could not be proved (counterexample)
|}]

let constrained_pair (xs : {p : int * int | let x, y = p in x >= y} list) :
    {u : unit | match xs with [] -> true | ((x : int), y) :: _ -> x >= y} = ();;
[%%expect{|
val constrained_pair :
  (xs : {p : int * int | match p with | (x, y) -> x >= y} list) ->
  {u : unit | match xs with | [] -> true | ((x : int), y)::_ -> x >= y} =
  <fun>
|}]

type coordinate = { lo : int; hi : int }
let destructured_record (xs : {p : coordinate | p.hi >= p.lo} list) :
    {u : unit | match xs with [] -> true | {lo; hi} :: _ -> hi >= lo} = ();;
[%%expect{|
type coordinate = { lo : int; hi : int; }
val destructured_record :
  (xs : {p : coordinate | p.hi >= p.lo} list) ->
  {u : unit | match xs with | [] -> true | { lo; hi }::_ -> hi >= lo} = <fun>
|}]

let destructured_variant (xs : {o : int option | match o with None -> true | Some n -> n >= 0} list) :
    {u : unit | match xs with [] | None :: _ -> true | Some n :: _ -> n >= 0} = ();;
[%%expect{|
val destructured_variant :
  (xs : {o : int option | match o with | None -> true | Some n -> n >= 0}
        list) ->
  {u : unit | match xs with | [] | (None)::_ -> true | (Some n)::_ -> n >= 0} =
  <fun>
|}]

let destructured_scalar (xs : {n : int | n >= 0} list) :
    {u : unit | match xs with [] -> true | n :: _ -> n >= 0} = ();;
[%%expect{|
val destructured_scalar :
  (xs : {n : int | n >= 0} list) ->
  {u : unit | match xs with | [] -> true | n::_ -> n >= 0} = <fun>
|}]

let dependent_pattern (lower : int)
    (xs : {p : int * int | let x, _ = p in x >= lower} list) :
    {u : unit | match xs with [] -> true | (x, _) :: _ -> x >= lower} = ();;
[%%expect{|
val dependent_pattern :
  (lower : int) ->
  (xs : {p : int * int | match p with | (x, _) -> x >= lower} list) ->
  {u : unit | match xs with | [] -> true | (x, _)::_ -> x >= lower} = <fun>
|}]

let wrong_dependent_pattern (lower : int) (other : int)
    (xs : {p : int * int | let x, _ = p in x >= lower} list) :
    {u : unit | match xs with [] -> true | (x, _) :: _ -> x >= other} = ();;
[%%expect{|
Line 3, characters 72-74:
3 |     {u : unit | match xs with [] -> true | (x, _) :: _ -> x >= other} = ();;
                                                                            ^^
Error: Refinement could not be proved (counterexample)
|}]

module Refinement_layers = struct
  type nonnegative = {n : int | n >= 0}
  let[@def] value (n : nonnegative) = n
  let positive (n : {n : nonnegative | value n > 0}) :
      {u : unit | value n > 0} = ()

  type ('a : immutable_data) box = {value : 'a}
  type ('a : immutable_data) t = {b : 'a box | true}
  let[@def] get (b : 'a t) = b.value
  let nested (b : {b : 'a t | true}) : {u : unit | get b === get b} = ()
end;;
[%%expect{|
module Refinement_layers :
  sig
    type nonnegative = {n : int | n >= 0}
    val value : nonnegative -> int
    val value_def : (n : nonnegative) -> {u : unit | (value n) === n}
    val positive :
      (n : {n : nonnegative | (value n) > 0}) -> {u : unit | (value n) > 0}
    type ('a : immutable_data) box = { value : 'a; }
    type ('a : immutable_data) t = {b : 'a box | true}
    val get : ('a : immutable_data). 'a t -> 'a
    val get_def :
      ('a : immutable_data). (b : 'a t) -> {u : unit | (get b) === b.value}
    val nested :
      ('a : immutable_data).
        (b : {b : 'a t | true}) -> {u : unit | (get b) === (get b)}
  end
|}]
