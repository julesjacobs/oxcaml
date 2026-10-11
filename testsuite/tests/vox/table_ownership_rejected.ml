(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_table_model.ml vox_table_model_proofs.ml vox_table_bits.ml vox_table_probe.ml vox_table_wrap.ml vox_table_mask.ml vox_table_map.ml vox_table_invariant.ml vox_table_initial.ml vox_table_update_proofs.ml vox_table_insert_proofs.ml vox_table_migration_proofs.ml vox_table_read_proofs.ml vox_table_search_spec.ml vox_table_stop_proof.ml pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_table_storage.mli vox_table_storage.ml vox_table_search.ml vox_table_mutation.ml vox_table_coverage.ml vox_table_occupancy.ml vox_table_vacancy_progress.ml vox_table_vacancy.ml vox_table_insert.ml vox_table_migrate.ml vox_table_resize.ml vox_table_implementation.ml vox_table_bindings_bridge.ml vox_verified_flat_hashtbl.mli vox_verified_flat_hashtbl.ml";
 readonly_files = "table_ownership_rejected.ml";
 { setup-ocamlc.opt-build-env; run-expect; check-program-output; }
*)

module Key : Vox_table_map.Key with type t = int = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = x = y
  let[@def] (hash @ total) (x : int) = x
  let (reflexive @ total) (x : int) : {u : unit | equal x x} =
    equal_def x x; ()
  let (symmetric @ total) (x : int) (y : int) :
      {u : unit | equal x y = equal y x} =
    equal_def x y; equal_def y x; ()
  let (transitive @ total) (x : int) (y : int) (z : int) :
      {u : unit | not (equal x y && equal y z) || equal x z} =
    equal_def x y; equal_def y z; equal_def x z; ()
  let (hash_equal @ total) (x : int) (y : int) :
      {u : unit | not (equal x y) || hash x = hash y} =
    equal_def x y; hash_def x; hash_def y; ()
end
;;
[%%expect{|
module Key :
  sig
    type t = int
    val equal : t -> t -> bool @@ total
    val reflexive : (x : t) -> {u : unit | equal x x} @@ total
    val symmetric :
      (x : t) -> (y : t) -> {u : unit | (equal x y) = (equal y x)} @@ total
    val transitive :
      (x : t) ->
      (y : t) ->
      (z : t) ->
      {u : unit | (not ((equal x y) && (equal y z))) || (equal x z)} @@ total
    val hash : t -> int @@ total
    val hash_equal :
      (x : t) ->
      (y : t) -> {u : unit | (not (equal x y)) || ((hash x) = (hash y))} @@
      total
  end
|}]

let stale_permission () =
  let module V = Vox_verified_flat_hashtbl.Make (Key) in
  let r : int V.created = V.create () in
  let changed = V.replace r.#table 1 84 r.#permission in
  V.find_opt r.#table 1 (borrow_ r.#permission);;
[%%expect{|
Line 5, characters 24-47:
5 |   V.find_opt r.#table 1 (borrow_ r.#permission);;
                            ^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is borrowed here,
       but it has already been used as unique at:
Line 4, characters 40-53:
4 |   let changed = V.replace r.#table 1 84 r.#permission in
                                            ^^^^^^^^^^^^^

|}]

let wrong_table () =
  let module V = Vox_verified_flat_hashtbl.Make (Key) in
  let r : int V.created = V.create () in
  let other : int V.created = V.create () in
  V.find_opt r.#table 1 (borrow_ other.#permission);;
[%%expect{|
Line 5, characters 33-50:
5 |   V.find_opt r.#table 1 (borrow_ other.#permission);;
                                     ^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
File "vox_verified_flat_hashtbl.mli", line 40, characters 30-47:
  The refinement is stated here.
|}]

let reused_permission () =
  let module V = Vox_verified_flat_hashtbl.Make (Key) in
  let r : int V.created = V.create () in
  let changed = V.replace r.#table 1 84 r.#permission in
  let again = V.replace r.#table 2 90 r.#permission in
  V.length r.#table (borrow_ again);;
[%%expect{|
Line 5, characters 38-51:
5 |   let again = V.replace r.#table 2 90 r.#permission in
                                          ^^^^^^^^^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 4, characters 40-53:
4 |   let changed = V.replace r.#table 1 84 r.#permission in
                                            ^^^^^^^^^^^^^

|}]

let wrong_value () =
  let module V = Vox_verified_flat_hashtbl.Make (Key) in
  let r : int V.created = V.create () in
  let changed = V.replace r.#table 1 84 r.#permission in
  let value : {v : int | v = 85} =
    V.find r.#table 1 (borrow_ changed) in value;;
[%%expect{|
Line 6, characters 4-39:
6 |     V.find r.#table 1 (borrow_ changed) in value;;
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 5, characters 25-31:
5 |   let value : {v : int | v = 85} =
                             ^^^^^^
  The refinement is stated here.
|}]
