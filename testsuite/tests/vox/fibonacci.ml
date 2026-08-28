(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
 { expect.opt; }
*)

module Fibonacci = struct
  let[@def] rec fib n =
    if n <= 0 then 0
    else if n = 1 then 1
    else fib (n - 1) + fib (n - 2)
  [@@decreases n]

  exception Overflow

  let successor (n : int)
      (a : {a : int | a = fib (n - 1)}) (b : {b : int | b = fib n}) :
      {r : int | r = fib (n + 1)} =
    if n < 1 || n > 90 then raise Overflow;
    let refine_ a = a in
    let refine_ b = b in
    let next = n + 1 in
    let refine_ proof = fib_def next in
    let r = a + b in
    refine_ r

  let rec tail_loop : (n : int) -> (i : int) ->
      {a : int | a = fib i} -> {b : int | b = fib (i + 1)} ->
      {r : int | r = fib n} = fun n i a b ->
    let refine_ a = a in
    let refine_ b = b in
    if i = n then refine_ a
    else if 0 <= i && i < n && n <= 90 then
      let j = i + 1 in
      if j = n then refine_ b
      else
        let c = a + b in
        let k = j + 1 in
        let refine_ proof = fib_def k in
        let a : {v : int | v = fib j} = refine_ b in
        let b : {v : int | v = fib (j + 1)} = refine_ c in
        (tail_loop[@tailcall]) n j a b
    else raise Overflow
  [@@decreases n - i]

  let tail (n : int) : {r : int | r = fib n} =
    if n < 0 || n > 90 then raise Overflow;
    let zero = 0 in
    let one = 1 in
    let refine_ proof = fib_def zero in
    let refine_ proof = fib_def one in
    let a : {a : int | a = fib zero} = refine_ zero in
    let b : {b : int | b = fib (zero + 1)} = refine_ one in
    tail_loop n zero a b

  let rec (doubling_identity @ total) : (n : int) ->
      {u : unit |
        if 0 <= n && n <= 45 then
          fib (2 * n) = fib n * (2 * fib (n + 1) - fib n)
          && fib (2 * n + 1) = fib n * fib n + fib (n + 1) * fib (n + 1)
        else true} = fun n ->
    let u = () in
    if n < 0 || n > 45 then refine_ u
    else if n = 0 then
      let zero = 0 in
      let one = 1 in
      let refine_ proof = fib_def zero in
      let refine_ proof = fib_def one in
      refine_ u
    else
      let prev = n - 1 in
      let refine_ proof = doubling_identity prev in
      let next = n + 1 in
      let twice = 2 * n in
      let twice_next = twice + 1 in
      let refine_ proof = fib_def next in
      let refine_ proof = fib_def twice in
      let refine_ proof = fib_def twice_next in
      refine_ u
  [@@decreases n]

  let rec doubling_pair : (n : int) ->
      {a : int | a = fib n} * {b : int | b = fib (n + 1)} = fun n ->
    if n < 0 || n > 90 then raise Overflow;
    if n = 0 then
      let zero = 0 in
      let one = 1 in
      let refine_ proof = fib_def zero in
      let refine_ proof = fib_def one in
      (refine_ zero, refine_ one)
    else
      let k = n / 2 in
      let a, b = doubling_pair k in
      let refine_ a = a in
      let refine_ b = b in
      let refine_ proof = doubling_identity k in
      let c = a * (2 * b - a) in
      let d = a * a + b * b in
      let c : {r : int | r = fib (2 * k)} = refine_ c in
      let d : {r : int | r = fib (2 * k + 1)} = refine_ d in
      let refine_ c = c in
      let refine_ d = d in
      if n mod 2 = 0 then (refine_ c, refine_ d)
      else
        let lower : {r : int | r = fib (n - 1)} = refine_ c in
        let upper : {r : int | r = fib n} = refine_ d in
        (upper, successor n lower upper)
  [@@decreases n]

  let doubling (n : int) : {r : int | r = fib n} =
    let result, _ = doubling_pair n in result
end;;
[%%expect{|
module Fibonacci :
  sig
    val fib : int -> int
    val fib_def :
      (n : int) ->
      {u : unit
        | (fib n) ===
            (if n <= 0
             then 0
             else if n = 1 then 1 else (fib (n - 1)) + (fib (n - 2)))}
    exception Overflow
    val successor :
      (n : int) ->
      {a : int | a = (fib (n - 1))} ->
      {b : int | b = (fib n)} -> {r : int | r = (fib (n + 1))}
    val tail_loop :
      (n : int) ->
      (i : int) ->
      {a : int | a = (fib i)} ->
      {b : int | b = (fib (i + 1))} -> {r : int | r = (fib n)}
    val tail : (n : int) -> {r : int | r = (fib n)}
    val doubling_identity :
      (n : int) ->
      {u : unit
        | if (0 <= n) && (n <= 45)
          then
            ((fib (2 * n)) = ((fib n) * ((2 * (fib (n + 1))) - (fib n)))) &&
              ((fib ((2 * n) + 1)) =
                 (((fib n) * (fib n)) + ((fib (n + 1)) * (fib (n + 1)))))
          else true}
    val doubling_pair :
      (n : int) -> {a : int | a = (fib n)} * {b : int | b = (fib (n + 1))}
    val doubling : (n : int) -> {r : int | r = (fib n)}
  end
|}]

let tail n = let refine_ r = Fibonacci.tail n in r
let doubling n = let refine_ r = Fibonacci.doubling n in r;;
[%%expect{|
val tail : int -> int = <fun>
val doubling : int -> int = <fun>
|}]

let examples = List.init 12 (fun n -> n, Fibonacci.fib n, tail n, doubling n);;
[%%expect{|
val examples : (int * int * int * int) list =
  [(0, 0, 0, 0); (1, 1, 1, 1); (2, 1, 1, 1); (3, 2, 2, 2); (4, 3, 3, 3);
   (5, 5, 5, 5); (6, 8, 8, 8); (7, 13, 13, 13); (8, 21, 21, 21);
   (9, 34, 34, 34); (10, 55, 55, 55); (11, 89, 89, 89)]
|}]

let boundaries = tail 90, doubling 90;;
[%%expect{|
val boundaries : int * int = (2880067194370816120, 2880067194370816120)
|}]

let overflow = (try ignore (tail 91); false with Fibonacci.Overflow -> true),
(try ignore (doubling 91); false with Fibonacci.Overflow -> true);;
[%%expect{|
val overflow : bool * bool = (true, true)
|}]
