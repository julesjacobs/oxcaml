(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml raw_memory.mli raw_memory.ml";
 readonly_files = "raw_memory_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   run-expectnat;
   check-program-output;
 }
*)
module P = Ghost_pref;;
module H = P.Heap;;
module M = Raw_memory;;
[%%expect{|
module P = Ghost_pref
module H = P.Heap
module M = Raw_memory
|}]

let uninitialized () =
  let r = M.malloc 1 (P.empty ()) in
  match r.P.value with
  | None -> 0
  | Some p ->
    ghost_ (M.footprint_at p 0);
    M.read p 0 (borrow_ r.P.state);;
[%%expect{|
Line 7, characters 24-33:
7 |     M.read p 0 (borrow_ r.P.state);;
                            ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let premature_free () =
  let r = M.malloc 2 (P.empty ()) in
  match r.P.value with
  | None -> r.P.state
  | Some p ->
    ghost_ (M.footprint_at p (-1));
    ghost_ (M.range_at p 0 1 (-1));
    let halves = P.split (ghost_ (M.range p 0 1)) r.P.state in
    M.free p halves.right;;
[%%expect{|
Line 9, characters 13-25:
9 |     M.free p halves.right;;
                 ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let no_deallocation_permission () =
  let r = M.malloc 1 (P.empty ()) in
  match r.P.value with
  | None -> r.P.state
  | Some p ->
    let parts = P.split (ghost_ (M.range p 0 1)) r.P.state in
    M.free p parts.left;;
[%%expect{|
Line 7, characters 13-23:
7 |     M.free p parts.left;;
                 ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let double_free () =
  let r = M.malloc 1 (P.empty ()) in
  match r.P.value with
  | None -> r.P.state
  | Some p ->
    ghost_ (M.allocated_covers p (H.empty ()));
    ghost_ (M.footprint_at p (-1));
    let token = M.free p r.P.state in
    M.free p token;;
[%%expect{|
Line 9, characters 13-18:
9 |     M.free p token;;
                 ^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let use_after_free () =
  let r = M.malloc 1 (P.empty ()) in
  match r.P.value with
  | None -> r.P.state
  | Some p ->
    ghost_ (M.allocated_covers p (H.empty ()));
    ghost_ (M.footprint_at p (-1));
    let token = M.free p r.P.state in
    M.write p 0 42 token;;
[%%expect{|
Line 9, characters 19-24:
9 |     M.write p 0 42 token;;
                       ^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let out_of_bounds (p : M.t) (token : M.contents P.token @ unique ghost) =
  M.write p (M.length p) 0 token;;
[%%expect{|
Line 2, characters 12-24:
2 |   M.write p (M.length p) 0 token;;
                ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let negative_size () = M.malloc (-1) (P.empty ());;
[%%expect{|
Line 1, characters 32-36:
1 | let negative_size () = M.malloc (-1) (P.empty ());;
                                    ^^^^
Error: Refinement could not be proved (counterexample)
|}]

let bad_byte () =
  let r = M.malloc 1 (P.empty ()) in
  match r.P.value with
  | None -> r.P.state
  | Some p ->
    ghost_ (M.footprint_at p 0);
    M.write p 0 256 r.P.state;;
[%%expect{|
Line 7, characters 16-19:
7 |     M.write p 0 256 r.P.state;;
                    ^^^
Error: Refinement could not be proved (counterexample)
|}]

let zero_size_access () =
  let r = M.malloc 0 (P.empty ()) in
  match r.P.value with
  | None -> r.P.state
  | Some p -> M.write p 0 0 r.P.state;;
[%%expect{|
Line 5, characters 24-25:
5 |   | Some p -> M.write p 0 0 r.P.state;;
                            ^
Error: Refinement could not be proved (counterexample)
|}]

let duplicate_token () =
  let r = M.malloc 1 (P.empty ()) in
  match r.P.value with
  | None -> r.P.state
  | Some p ->
    ghost_ (M.footprint_at p 0);
    let first = M.write p 0 1 r.P.state in
    M.write p 0 2 r.P.state;;
[%%expect{|
Line 8, characters 18-27:
8 |     M.write p 0 2 r.P.state;;
                      ^^^^^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 7, characters 30-39:
7 |     let first = M.write p 0 1 r.P.state in
                                  ^^^^^^^^^

|}]

let use_ghost_location (p : M.t) (token : M.contents P.token @ unique ghost) =
  P.write (M.location p (-1)) None token;;
[%%expect{|
Line 2, characters 10-29:
2 |   P.write (M.location p (-1)) None token;;
              ^^^^^^^^^^^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]

let snapshot_is_not_ownership () =
  let r = M.malloc 1 (P.empty ()) in
  match r.P.value with
  | None -> r.P.state
  | Some p ->
    let _snapshot = ghost_ (P.own (borrow_ r.P.state)) in
    ghost_ (M.allocated_covers p (H.empty ()));
    ghost_ (M.footprint_at p (-1));
    M.free p (P.empty ());;
[%%expect{|
Line 9, characters 13-25:
9 |     M.free p (P.empty ());;
                 ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let erased_allocation () = ghost_ (M.malloc 0 (P.empty ()));;
[%%expect{|
Line 1, characters 35-43:
1 | let erased_allocation () = ghost_ (M.malloc 0 (P.empty ()));;
                                       ^^^^^^^^
Error: The value "M.malloc" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 27-59).
|}]

let erased_free : (p : M.t) ->
    (token : {s : M.contents P.token | H.mem (P.own s) (M.location p (-1)) &&
      M.covers (P.own s) p 0 (M.length p)}) @ unique ghost ->
    M.contents P.token @ unique ghost = fun p token ->
  ghost_ (M.free p token);;
[%%expect{|
Line 5, characters 10-16:
5 |   ghost_ (M.free p token);;
              ^^^^^^
Error: The value "M.free" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 5, characters 2-25).
|}]
