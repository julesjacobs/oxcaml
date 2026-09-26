(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

module Fibonacci : sig
  open Bigint
  val fib : Bigint.t -> Bigint.t @@ total
  val tail : (n : Bigint.t) -> {r : Bigint.t | r = fib n} @@ total
  val doubling : (n : Bigint.t) -> {r : Bigint.t | r = fib n} @@ total
end = struct
  open Bigint

  let[@def] rec fib n =
    if n <= 0Z then 0Z
    else if n = 1Z then 1Z
    else fib (n - 1Z) + fib (n - 2Z)
  [@@decreases n]

  let rec (tail_loop @ total) : (n : t) ->
      (index : {i : t | 0Z <= i && i <= n}) ->
      {a : t | let i = index in a = fib i} ->
      {b : t | let i = index in b = fib (i + 1Z)} ->
      {r : t | r = fib n} = fun n index a b ->
    let i = index in
    let a = a in
    let b = b in
    if i = n then a
    else
      let j = i + 1Z in
      let k = j + 1Z in
      let c = a + b in
      ghost_ (fib_def k);
      let next : {i : t | 0Z <= i && i <= n} = j in
      (tail_loop[@tailcall]) n next (b) (c)
  [@@decreases let i = index in n - i]

  let (tail @ total) (n : t) : {r : t | r = fib n} =
    let zero = 0Z in
    if n <= zero then
      (ghost_ (fib_def n);
      zero)
    else
      let one = 1Z in
      ghost_ (fib_def zero);
      ghost_ (fib_def one);
      let index : {i : t | 0Z <= i && i <= n} = zero in
      tail_loop n index (zero) (one)

  let rec (doubling_identity @ total) : (n : t) ->
      {u : unit |
        if 0Z <= n then
          fib (2Z * n) = fib n * (2Z * fib (n + 1Z) - fib n)
          && fib (2Z * n + 1Z) = fib n * fib n + fib (n + 1Z) * fib (n + 1Z)
        else true} = fun n ->
    let u = () in
    if n < 0Z then u
    else if n = 0Z then
      let zero = 0Z in
      let one = 1Z in
      fib_def zero;
      fib_def one;
      u
    else
      let prev = n - 1Z in
      doubling_identity prev;
      let next = n + 1Z in
      let twice = 2Z * n in
      let twice_next = twice + 1Z in
      fib_def next;
      fib_def twice;
      fib_def twice_next;
      u
  [@@decreases n]

  let rec (doubling_pair @ total) : (index : {n : t | 0Z <= n}) ->
      {a : t | let n = index in a = fib n} *
      {b : t | let n = index in b = fib (n + 1Z)} = fun index ->
    let n = index in
    if n = 0Z then
      let zero = 0Z in
      let one = 1Z in
      ghost_ (fib_def zero);
      ghost_ (fib_def one);
      (zero, one)
    else
      let k = n / 2Z in
      let smaller : {n : t | 0Z <= n} = k in
      let a, b = doubling_pair smaller in
      let a = a in
      let b = b in
      ghost_ (doubling_identity k);
      let c = a * (2Z * b - a) in
      let d = a * a + b * b in
      if n mod 2Z = 0Z then (c, d)
      else
        let next = n + 1Z in
        ghost_ (fib_def next);
        let e = c + d in
        (d, e)
  [@@decreases let n = index in n]

  let (doubling @ total) (n : t) : {r : t | r = fib n} =
    if n <= 0Z then
      (ghost_ (fib_def n);
      let zero = 0Z in zero)
    else
      let index : {n : t | 0Z <= n} = n in
      let a, _ = doubling_pair index in
      let a = a in
      a
end;;
[%%expect{|
module Fibonacci :
  sig
    val fib : Bigint.t -> Bigint.t @@ total
    val tail : (n : Bigint.t) -> {r : Bigint.t | Bigint.(=) r (fib n)} @@
      total
    val doubling : (n : Bigint.t) -> {r : Bigint.t | Bigint.(=) r (fib n)} @@
      total
  end
|}]

let () =
  List.iter (fun n ->
    let tail = Fibonacci.tail n in
    let doubling = Fibonacci.doubling n in
    assert (tail = Fibonacci.fib n && doubling = tail))
    [-100Z; -1Z; 0Z; 1Z; 2Z; 10Z];
  let hundred = 100Z in
  let tail = Fibonacci.tail hundred in
  let doubling = Fibonacci.doubling hundred in
  Format.printf "%s@.%s@." (Bigint.to_string tail) (Bigint.to_string doubling);;
[%%expect{|
354224848179261915075
354224848179261915075
|}]
