(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
 { expect.opt; }
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
      {a : t | let refine_ i = index in a = fib i} ->
      {b : t | let refine_ i = index in b = fib (i + 1Z)} ->
      {r : t | r = fib n} = fun n index a b ->
    let refine_ i = index in
    let refine_ a = a in
    let refine_ b = b in
    if i = n then refine_ a
    else
      let j = i + 1Z in
      let k = j + 1Z in
      let c = a + b in
      let refine_ proof = ghost_ (fib_def k) in
      let next : {i : t | 0Z <= i && i <= n} = refine_ j in
      (tail_loop[@tailcall]) n next (refine_ b) (refine_ c)
  [@@decreases let refine_ i = index in n - i]

  let (tail @ total) (n : t) : {r : t | r = fib n} =
    let zero = 0Z in
    if n <= zero then
      let refine_ proof = ghost_ (fib_def n) in
      refine_ zero
    else
      let one = 1Z in
      let refine_ proof = ghost_ (fib_def zero) in
      let refine_ proof = ghost_ (fib_def one) in
      let index : {i : t | 0Z <= i && i <= n} = refine_ zero in
      tail_loop n index (refine_ zero) (refine_ one)

  let rec (doubling_identity @ total) : (n : t) ->
      {u : unit |
        if 0Z <= n then
          fib (2Z * n) = fib n * (2Z * fib (n + 1Z) - fib n)
          && fib (2Z * n + 1Z) = fib n * fib n + fib (n + 1Z) * fib (n + 1Z)
        else true} = fun n ->
    let u = () in
    if n < 0Z then refine_ u
    else if n = 0Z then
      let zero = 0Z in
      let one = 1Z in
      let refine_ proof = fib_def zero in
      let refine_ proof = fib_def one in
      refine_ u
    else
      let prev = n - 1Z in
      let refine_ proof = doubling_identity prev in
      let next = n + 1Z in
      let twice = 2Z * n in
      let twice_next = twice + 1Z in
      let refine_ proof = fib_def next in
      let refine_ proof = fib_def twice in
      let refine_ proof = fib_def twice_next in
      refine_ u
  [@@decreases n]

  let rec (doubling_pair @ total) : (index : {n : t | 0Z <= n}) ->
      {a : t | let refine_ n = index in a = fib n} *
      {b : t | let refine_ n = index in b = fib (n + 1Z)} = fun index ->
    let refine_ n = index in
    if n = 0Z then
      let zero = 0Z in
      let one = 1Z in
      let refine_ proof = ghost_ (fib_def zero) in
      let refine_ proof = ghost_ (fib_def one) in
      (refine_ zero, refine_ one)
    else
      let k = n / 2Z in
      let smaller : {n : t | 0Z <= n} = refine_ k in
      let a, b = doubling_pair smaller in
      let refine_ a = a in
      let refine_ b = b in
      let refine_ proof = ghost_ (doubling_identity k) in
      let c = a * (2Z * b - a) in
      let d = a * a + b * b in
      if n mod 2Z = 0Z then (refine_ c, refine_ d)
      else
        let next = n + 1Z in
        let refine_ proof = ghost_ (fib_def next) in
        let e = c + d in
        (refine_ d, refine_ e)
  [@@decreases let refine_ n = index in n]

  let (doubling @ total) (n : t) : {r : t | r = fib n} =
    if n <= 0Z then
      let refine_ proof = ghost_ (fib_def n) in
      let zero = 0Z in refine_ zero
    else
      let index : {n : t | 0Z <= n} = refine_ n in
      let a, _ = doubling_pair index in
      let refine_ a = a in
      refine_ a
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
    let refine_ tail = Fibonacci.tail n in
    let refine_ doubling = Fibonacci.doubling n in
    assert (tail = Fibonacci.fib n && doubling = tail))
    [-100Z; -1Z; 0Z; 1Z; 2Z; 10Z];
  let hundred = 100Z in
  let refine_ tail = Fibonacci.tail hundred in
  let refine_ doubling = Fibonacci.doubling hundred in
  Format.printf "%s@.%s@." (Bigint.to_string tail) (Bigint.to_string doubling);;
[%%expect{|
354224848179261915075
354224848179261915075
|}]
