(* See viarray.mli. length/get/unsafe_get are proved against the built-in iarray
   theory (no assumption). for_all/any/mem are read-only index loops; each
   inner [go] carries the window predicate [_from i n] as its result spec and is
   discharged by the step/done laws (i in bounds throughout, so a.(i) is safe). *)

open Vhof

let length : (a : int iarray) -> int{ _ = Iarray.length a } =
  fun a -> Iarray.length a

let get : (a : int iarray) -> (i : int{ 0 <= _ && _ < Iarray.length a })
          -> int{ _ = a.(i) } =
  fun a i -> Iarray.get a i

let unsafe_get : (a : int iarray)
                 -> (i : int{ 0 <= _ && _ < Iarray.length a }) -> int =
  fun a i -> Iarray.unsafe_get a i

let for_all :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (a : int iarray) -> bool{ _ = ia_all_from p a 0 (Iarray.length a) } =
  fun p test a ->
    let n : int{ _ = Iarray.length a } = Iarray.length a in
    let rec go (i : int{ 0 <= _ }) : bool{ _ = ia_all_from p a i n } =
      if i >= n then true else test (Iarray.get a i) && go (i + 1)
    in
    go 0

let any :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (a : int iarray) -> bool{ _ = ia_ex_from p a 0 (Iarray.length a) } =
  fun p test a ->
    let n : int{ _ = Iarray.length a } = Iarray.length a in
    let rec go (i : int{ 0 <= _ }) : bool{ _ = ia_ex_from p a i n } =
      if i >= n then false else test (Iarray.get a i) || go (i + 1)
    in
    go 0

let mem :
  (x : int) -> (a : int iarray) -> bool{ _ = ia_mem_from x a 0 (Iarray.length a) } =
  fun x a ->
    let n : int{ _ = Iarray.length a } = Iarray.length a in
    let rec go (i : int{ 0 <= _ }) : bool{ _ = ia_mem_from x a i n } =
      if i >= n then false else Iarray.get a i = x || go (i + 1)
    in
    go 0
