(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml";
 readonly_files = "pref_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
*)

module Missing = struct
  let bad p =
    let t = Pref.empty () in
    let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p} = t in
    let _ = Pref.read p t in ()
end;;
[%%expect{|
Line 4, characters 62-63:
4 |     let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p} = t in
                                                                  ^
Error: Refinement could not be proved (counterexample)
|}]

module Ghost_write = struct
  let bad : (p : int Pref.t) -> (v : int) ->
      (t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p}) @ unique ->
      unit = fun p v t ->
    let _ = ghost_ (let u = Pref.write p v t in u) in ()
end;;
[%%expect{|
Line 5, characters 28-38:
5 |     let _ = ghost_ (let u = Pref.write p v t in u) in ()
                                ^^^^^^^^^^
Error: The value "Pref.write" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 5, characters 12-50).
|}]

module Borrowed_write = struct
  let bad : (p : int Pref.t) -> (v : int) ->
      (t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p}) @ unique ->
      unit = fun p v t ->
    let b = borrow_ t in
    let ignored = Pref.write p v b in ()
end;;
[%%expect{|
Line 6, characters 33-34:
6 |     let ignored = Pref.write p v b in ()
                                     ^
Error: This value is "local" because it is borrowed.
       However, the highlighted expression is expected to be "global".
|}]

module Stale = struct
  let bad : (p : int Pref.t) -> (v : int) ->
      (t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p}) @ unique ->
      unit = fun p v t ->
    let _next = Pref.write p v t in
    let ignored = Pref.read p t in ()
end;;
[%%expect{|
Line 6, characters 30-31:
6 |     let ignored = Pref.read p t in ()
                                  ^
Error: This value is used here, but it has already been used as unique at:
Line 5, characters 31-32:
5 |     let _next = Pref.write p v t in
                                   ^

|}]

module Ghost_authority = struct
  type box = { n : int; state : Pref.token @@ ghost }
  let bad : Pref.token @ unique -> Pref.token @ unique real = fun t ->
    let box = { n = 0; state = t } in
    box.state
end;;
[%%expect{|
Line 5, characters 4-13:
5 |     box.state
        ^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]

module Total_read = struct
  let (bad @ total) : (p : int Pref.t) ->
      (t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p}) @ local read ->
      int = fun p t ->
    let x = Pref.read p t in x
end;;
[%%expect{|
Line 5, characters 12-21:
5 |     let x = Pref.read p t in x
                ^^^^^^^^^
Error: The value "Pref.read" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 4-5, characters 12-30
         which is expected to be "total".
|}]

module Duplicated_join = struct
  let bad (t : Pref.token @ unique) =
    let result = Pref.join t t in ()
end;;
[%%expect{|
Line 3, characters 29-30:
3 |     let result = Pref.join t t in ()
                                 ^
Error: This value is used here, but it is also being used as unique at:
Line 3, characters 27-28:
3 |     let result = Pref.join t t in ()
                               ^

|}]

module Ghost_split = struct
  let bad (h : Pref.heap @ immutable ghost) (t : Pref.token @ unique) =
    let _ = ghost_ (Pref.split h t) in ()
end;;
[%%expect{|
Line 3, characters 33-34:
3 |     let _ = ghost_ (Pref.split h t) in ()
                                     ^
Error: This value is "aliased"
         because it is used in an expression (at line 3, characters 12-35).
       However, the highlighted expression is expected to be "unique".
|}]

module Split_stale = struct
  let bad (h : Pref.heap @ immutable ghost) (t : Pref.token @ unique) =
    let parts = Pref.split h t in
    let left = parts.#Pref.left in
    let result = Pref.join t left in ()
end;;
[%%expect{|
Line 5, characters 27-28:
5 |     let result = Pref.join t left in ()
                               ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 29-30:
3 |     let parts = Pref.split h t in
                                 ^

|}]

module Forged = struct
  let bad (h : Pref.heap @ ghost) : Pref.token = h
end;;
[%%expect{|
Line 2, characters 49-50:
2 |   let bad (h : Pref.heap @ ghost) : Pref.token = h
                                                     ^
Error: The value "h" has type "Pref.heap" but an expression was expected of type
         "Pref.token"
|}]

module False_frame = struct
  let bad (h : Pref.heap @ immutable) (p : int Pref.t @ immutable) =
    let z = 0 in
    let after = ghost_ (Pref.Heap.put h p z) in
    let u = () in
    let claim : {u : unit | Pref.Heap.at after p === Pref.Heap.at h p} =
      u in
    ignore claim
end;;
[%%expect{|
Line 7, characters 6-7:
7 |       u in
          ^
Error: Refinement could not be proved (counterexample)
|}]

module False_value = struct
  let bad (h : Pref.heap @ immutable) (p : int Pref.t @ immutable) =
    let z = 0 in
    let after = ghost_ (Pref.Heap.put h p z) in
    let u = () in
    let claim : {u : unit | Pref.Heap.at after p === Some 1} = u in
    ignore claim
end;;
[%%expect{|
Line 6, characters 63-64:
6 |     let claim : {u : unit | Pref.Heap.at after p === Some 1} = u in
                                                                   ^
Error: Refinement could not be proved (counterexample)
|}]

module Captured_read = struct
  let bad : (p : int Pref.t) -> (v : int) ->
      (t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p}) @ unique ->
      unit = fun p v t ->
    let read_later () = let x = Pref.read p t in x in
    let _next = Pref.write p v t in
    ignore (read_later ())
end;;
[%%expect{|
Line 6, characters 31-32:
6 |     let _next = Pref.write p v t in
                                   ^
Error: This value is used here as unique, but it has already been used at:
Line 5, characters 44-45:
5 |     let read_later () = let x = Pref.read p t in x in
                                                ^

|}]

module Immutable_write = struct
  let bad : (p : int Pref.t) -> (v : int) ->
      (t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p}) @ immutable unique ->
      unit = fun p v t ->
    let ignored = Pref.write p v t in ()
end;;
[%%expect{|
Line 5, characters 33-34:
5 |     let ignored = Pref.write p v t in ()
                                     ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

module Total_write = struct
  let (bad @ total) : (p : int Pref.t) -> (v : int) ->
      (t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p}) @ unique ->
      unit = fun p v t ->
    let ignored = Pref.write p v t in ()
end;;
[%%expect{|
Line 5, characters 18-28:
5 |     let ignored = Pref.write p v t in ()
                      ^^^^^^^^^^
Error: The value "Pref.write" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 4-5, characters 13-40
         which is expected to be "total".
|}]

module Wrong_fragment = struct
  let bad () =
    let value = 7 in
    let t = Pref.empty () in
    let a = Pref.alloc value t in
    let p = a.Pref.value in
    let t = a.Pref.state in
    let b = Pref.alloc value t in
    let q = b.Pref.value in
    let t = b.Pref.state in
    let selection = ghost_ (Pref.Heap.put (Pref.Heap.empty ()) p value) in
    let parts = Pref.split selection t in
    let left = parts.#Pref.left in
    let left : {t : Pref.token | Pref.Heap.mem (Pref.own t) q} = left in
    let _ = Pref.read q left in ()
end;;
[%%expect{|
Line 14, characters 65-69:
14 |     let left : {t : Pref.token | Pref.Heap.mem (Pref.own t) q} = left in
                                                                      ^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Overlapping_maps = struct
  let bad (p : int Pref.t @ immutable) =
    let h = ghost_ (Pref.Heap.put (Pref.Heap.empty ()) p 7) in
    let u = () in
    let claim : {u : unit | Pref.Heap.disjoint h h} = u in
    ignore claim
end;;
[%%expect{|
Line 5, characters 54-55:
5 |     let claim : {u : unit | Pref.Heap.disjoint h h} = u in
                                                          ^
Error: Refinement could not be proved (counterexample)
|}]

module Unboxed_ghost_authority = struct
  type box = #{ n : int; state : Pref.token @@ ghost }
  let bad : Pref.token @ unique -> Pref.token @ unique real = fun t ->
    let box = #{ n = 0; state = t } in
    box.#state
end;;
[%%expect{|
Line 5, characters 4-14:
5 |     box.#state
        ^^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]

module Unboxed_ghost_join = struct
  type box = #{ n : int; state : Pref.token @@ ghost }
  let bad (box : box) = ghost_ (
    let left = box.#state in
    let right = box.#state in
    let joined = Pref.join left right in ())
end;;
[%%expect{|
Line 6, characters 32-37:
6 |     let joined = Pref.join left right in ())
                                    ^^^^^
Error: This value is used here, but it is also being used as unique at:
Line 6, characters 27-31:
6 |     let joined = Pref.join left right in ())
                               ^^^^

|}]
