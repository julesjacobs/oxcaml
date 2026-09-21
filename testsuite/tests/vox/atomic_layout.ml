(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml atomic_layout.ml";
 { bytecode; }
 { native; }
*)

module P = Ghost_pref
module H = P.Heap
module Invariant = struct
  type key = { witness : unit @@ ghost }
  let[@def] (holds @ total) (_key : key @ immutable)
      (_value : int @ immutable) (heap : P.heap @ immutable) =
    ghost_ (heap === H.empty ())
end
module A = Verified_atomic.Make (Invariant)

let[@def] (post @ total) (_value : int @ immutable)
    (heap : P.heap @ immutable) = ghost_ (heap === H.empty ())

let load a =
  let caller = P.empty () in
  let result = A.load a (ghost_ post) caller
    (ghost_ (fun before inside outside ->
      post_def before (P.own (borrow_ outside));
      { A.restored = inside; outgoing = outside })) in
  result.#value

let[@def] (cas_post @ total) (_success : bool @ immutable)
    (heap : P.heap @ immutable) = ghost_ (heap === H.empty ())

let cas a expected desired =
  let caller = P.empty () in
  let result = A.compare_and_set a expected desired (ghost_ cas_post) caller
    (ghost_ (fun before inside outside ->
      let heap = P.own (borrow_ inside) in
      Invariant.holds_def (A.key a) before heap;
      Invariant.holds_def (A.key a)
        (if before = expected then desired else before) heap;
      cas_post_def (before = expected) (P.own (borrow_ outside));
      { A.restored = inside; outgoing = outside })) in
  result.#value

let () =
  let key = ghost_ { Invariant.witness = () } in
  let token = P.empty () in
  ghost_ (Invariant.holds_def key 0 (P.own (borrow_ token)));
  let cell = A.create key 0 token in
  let pending = A.compare_and_set cell 0 1 (ghost_ cas_post) in
  assert (load cell = 0);
  let result = pending (P.empty ())
    (ghost_ (fun before inside outside ->
      let heap = P.own (borrow_ inside) in
      Invariant.holds_def key before heap;
      Invariant.holds_def key (if before = 0 then 1 else before) heap;
      cas_post_def (before = 0) (P.own (borrow_ outside));
      { A.restored = inside; outgoing = outside })) in
  assert result.#value;
  let before = Gc.minor_words () in
  for i = 1 to 10000 do
    assert (load cell = i);
    assert (cas cell i (i + 1));
    assert (not (cas cell i 0))
  done;
  let allocated = Gc.minor_words () -. before in
  match Sys.backend_type with
  | Native -> assert (allocated < 20.)
  | Bytecode | Other _ -> ()
