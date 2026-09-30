(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_table_model.ml";
 prebuilt_modules += " vox_table_model_proofs.ml vox_table_bits.ml";
 prebuilt_modules += " vox_table_probe.ml vox_table_wrap.ml vox_table_mask.ml";
 prebuilt_modules += " vox_table_map.ml vox_table_invariant.ml";
 prebuilt_modules += " vox_table_initial.ml vox_table_update_proofs.ml";
 prebuilt_modules += " vox_table_insert_proofs.ml vox_table_migration_proofs.ml";
 prebuilt_modules += " vox_table_read_proofs.ml vox_table_search_spec.ml";
 prebuilt_modules += " vox_table_stop_proof.ml pref.mli pref.ml ghost_pref.mli";
 prebuilt_modules += " ghost_pref.ml vox_table_storage.mli vox_table_storage.ml";
 prebuilt_modules += " vox_table_search.ml vox_table_mutation.ml";
 prebuilt_modules += " vox_table_coverage.ml vox_table_occupancy.ml";
 prebuilt_modules += " vox_table_vacancy_progress.ml vox_table_vacancy.ml";
 prebuilt_modules += " vox_table_insert.ml vox_table_migrate.ml";
 prebuilt_modules += " vox_table_resize.ml vox_table_implementation.ml";
 prebuilt_modules += " vox_table_bindings.ml vox_table_bindings_bridge.ml";
 prebuilt_modules += " vox_verified_flat_hashtbl.mli";
 prebuilt_modules += " vox_verified_flat_hashtbl.ml";
 readonly_files = "talk_flat_hashtbl.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

(* Talk, section 3 ("the hash table: functor, ownership, tombstones").

   1. Tombstones. Deletion writes the tombstone 254. Write EMPTY (128)
      instead, reusing the library's own proof of route preservation
      ([Vox_table_update_proofs.routes_remove]), and the proof fails exactly
      at its premise "no new EMPTY byte" (vox_table_update_proofs.ml, the
      [not (M.control after index === Some 128) || ...] conjunct): a later
      key whose probe passed over this group would become unreachable. The
      identical proof with 254 is accepted. From the investigation's
      empty_instead_of_tombstone.ml and tombstone_ok.ml.
   2. Functor laws are checked by implication: applying the functor proves
      each law of [Key] from the one its argument states. [Int_key] states
      the laws as [Key] does and is accepted. A commuted [symmetric] law
      ([Commuted_table]) is also accepted, and a [symmetric] law that states
      one direction only ([One_way_table]) is rejected with a counterexample.
   3. "Store 84, read 85", told honestly: flat_hashtbl_boundary.ml accepts
      the true claim 84 only because its ghost block calls [Key.reflexive]
      and [Map.put_get]; it rejects the false claim 85. Without those law
      calls even the true claim 84 is rejected. *)

#load "vox_sequence.cmo";;
#load "vox_table_model.cmo";;
#load "vox_table_model_proofs.cmo";;
#load "vox_table_bits.cmo";;
#load "vox_table_probe.cmo";;
#load "vox_table_wrap.cmo";;
#load "vox_table_mask.cmo";;
#load "vox_table_map.cmo";;
#load "vox_table_invariant.cmo";;
#load "vox_table_initial.cmo";;
#load "vox_table_update_proofs.cmo";;
#load "vox_table_insert_proofs.cmo";;
#load "vox_table_migration_proofs.cmo";;
#load "vox_table_read_proofs.cmo";;
#load "vox_table_search_spec.cmo";;
#load "vox_table_stop_proof.cmo";;
#load "pref.cmo";;
#load "ghost_pref.cmo";;
#load "vox_table_storage.cmo";;
#load "vox_table_search.cmo";;
#load "vox_table_mutation.cmo";;
#load "vox_table_coverage.cmo";;
#load "vox_table_occupancy.cmo";;
#load "vox_table_vacancy_progress.cmo";;
#load "vox_table_vacancy.cmo";;
#load "vox_table_insert.cmo";;
#load "vox_table_migrate.cmo";;
#load "vox_table_resize.cmo";;
#load "vox_table_implementation.cmo";;
#load "vox_table_bindings.cmo";;
#load "vox_table_bindings_bridge.cmo";;
#load "vox_verified_flat_hashtbl.cmo";;

module M = Vox_table_model
module S = Vox_sequence;;
[%%expect{|
module M = Vox_table_model
module S = Vox_sequence
|}]

(* 1a. Remove a binding by writing EMPTY (128) instead of the tombstone. The
   wrapper's empty signature keeps the accepted outputs short. *)
module Empty_check : sig end = struct
  module Empty_instead_of_tombstone (Key : Vox_table_map.Key)
      (Invariant : module type of Vox_table_invariant.Make (Key)) = struct
    module U = Vox_table_update_proofs.Make (Key) (Invariant)
    let (routes_after_empty @ total) : ('a : immutable_data).
        (before : 'a Invariant.view) @ immutable -> (index : int) ->
        {u : unit | not (Invariant.valid before &&
          0 <= index && index < before.model.capacity) ||
          Invariant.routes_valid (M.set_byte before.model index 128)
            (S.set before.model.slots (Bigint.of_int index) None)
            before.routes 0Z} @ ghost =
      fun before index -> ghost_ (
        if Invariant.valid before && 0 <= index && index < before.model.capacity
        then begin
          Invariant.valid_def before; Invariant.shape_def before.model;
          U.routes_remove before.model (M.set_byte before.model index 128)
            before.model.slots before.routes 0Z (Bigint.of_int index)
            (fun query -> (
              Vox_table_model_proofs.byte_write before.model index 128 query;
              ()));
          M.set_byte_def before.model index 128;
          M.set_control_def before.model index 128;
          if index < 15 then
            M.set_control_def (M.set_control before.model index 128)
              (before.model.capacity + index) 128;
          ()
        end else ())
  end
end;;
[%%expect{|
Line 20, characters 14-16:
20 |               ()));
                   ^^
Error: Refinement could not be proved (counterexample)
File "vox_table_update_proofs.ml", lines 442-443, characters 8-43:
  The refinement is stated here.
|}, Principal{|
Line 10, characters 19-37:
10 |             (S.set before.model.slots (Bigint.of_int index) None)
                        ^^^^^^^^^^^^^^^^^^
Error: The field access "before.model.slots" has type "(Key.t * 'a) option list"
       but an expression was expected of type "'b S.t" = "'b list"
       The kind of (Key.t * 'a) option is logical_data with Key.t * 'a
         because it's a boxed variant type.
       But the kind of (Key.t * 'a) option must be a subkind of
           immutable_data.
|}]

(* 1b. The same proof with the tombstone 254 is accepted. *)
module Tombstone_check : sig end = struct
  module Tombstone (Key : Vox_table_map.Key)
      (Invariant : module type of Vox_table_invariant.Make (Key)) = struct
    module U = Vox_table_update_proofs.Make (Key) (Invariant)
    let (routes_after_empty @ total) : ('a : immutable_data).
        (before : 'a Invariant.view) @ immutable -> (index : int) ->
        {u : unit | not (Invariant.valid before &&
          0 <= index && index < before.model.capacity) ||
          Invariant.routes_valid (M.set_byte before.model index 254)
            (S.set before.model.slots (Bigint.of_int index) None)
            before.routes 0Z} @ ghost =
      fun before index -> ghost_ (
        if Invariant.valid before && 0 <= index && index < before.model.capacity
        then begin
          Invariant.valid_def before; Invariant.shape_def before.model;
          U.routes_remove before.model (M.set_byte before.model index 254)
            before.model.slots before.routes 0Z (Bigint.of_int index)
            (fun query -> (
              Vox_table_model_proofs.byte_write before.model index 254 query;
              ()));
          M.set_byte_def before.model index 254;
          M.set_control_def before.model index 254;
          if index < 15 then
            M.set_control_def (M.set_control before.model index 254)
              (before.model.capacity + index) 254;
          ()
        end else ())
  end
end;;
[%%expect{|
module Tombstone_check : sig end
|}, Principal{|
Line 10, characters 19-37:
10 |             (S.set before.model.slots (Bigint.of_int index) None)
                        ^^^^^^^^^^^^^^^^^^
Error: The field access "before.model.slots" has type "(Key.t * 'a) option list"
       but an expression was expected of type "'b S.t" = "'b list"
       The kind of (Key.t * 'a) option is logical_data with Key.t * 'a
         because it's a boxed variant type.
       But the kind of (Key.t * 'a) option must be a subkind of
           immutable_data.
|}]

(* 2a. A key type with its laws stated as in [Key] is accepted. *)
module Int_key = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = x = y
  let[@def] (hash @ total) (x : int) = x land 1023
  let (reflexive @ total) (x : int) : {u : unit | equal x x} = equal_def x x
  let (symmetric @ total) (x : int) (y : int) :
      {u : unit | equal x y = equal y x} = equal_def x y; equal_def y x
  let (transitive @ total) (x : int) (y : int) (z : int) :
      {u : unit | not (equal x y && equal y z) || equal x z} =
    equal_def x y; equal_def y z; equal_def x z
  let (hash_equal @ total) (x : int) (y : int) :
      {u : unit | not (equal x y) || hash x = hash y} =
    equal_def x y; hash_def x; hash_def y
end;;
[%%expect{|
module Int_key :
  sig
    type t = int
    val equal : int -> int -> bool
    val equal_def :
      (x : int) -> (y : int) -> {u : unit | (equal x y) === (x = y)}
    val hash : int -> int
    val hash_def : (x : int) -> {u : unit | (hash x) === (x land 1023)}
    val reflexive : (x : int) -> {u : unit | equal x x}
    val symmetric :
      (x : int) -> (y : int) -> {u : unit | (equal x y) = (equal y x)}
    val transitive :
      (x : int) ->
      (y : int) ->
      (z : int) ->
      {u : unit | (not ((equal x y) && (equal y z))) || (equal x z)}
    val hash_equal :
      (x : int) ->
      (y : int) -> {u : unit | (not (equal x y)) || ((hash x) = (hash y))}
  end
|}]

module Int_table : sig end = Vox_verified_flat_hashtbl.Make (Int_key);;
[%%expect{|
module Int_table : sig end
|}]

(* 2b. The same law with its equation commuted is accepted: the functor
   application proves the declared law from the stated one. *)
module Commuted_key = struct
  include Int_key
  let (symmetric @ total) (x : int) (y : int) :
      {u : unit | equal y x = equal x y} = equal_def x y; equal_def y x
end;;
[%%expect{|
module Commuted_key :
  sig
    type t = int
    val equal : int -> int -> bool
    val equal_def :
      (x : int) -> (y : int) -> {u : unit | (equal x y) === (x = y)}
    val hash : int -> int
    val hash_def : (x : int) -> {u : unit | (hash x) === (x land 1023)}
    val reflexive : (x : int) -> {u : unit | equal x x}
    val transitive :
      (x : int) ->
      (y : int) ->
      (z : int) ->
      {u : unit | (not ((equal x y) && (equal y z))) || (equal x z)}
    val hash_equal :
      (x : int) ->
      (y : int) -> {u : unit | (not (equal x y)) || ((hash x) = (hash y))}
    val symmetric :
      (x : int) -> (y : int) -> {u : unit | (equal y x) = (equal x y)}
  end
|}]

module Commuted_table : sig end =
  Vox_verified_flat_hashtbl.Make (Commuted_key);;
[%%expect{|
module Commuted_table : sig end
|}]

(* 2c. A law that states only one direction of symmetry is rejected, with a
   counterexample. *)
module One_way_key = struct
  include Int_key
  let (symmetric @ total) (x : int) (y : int) :
      {u : unit | not (equal x y) || equal y x} = equal_def x y; equal_def y x
end;;
[%%expect{|
module One_way_key :
  sig
    type t = int
    val equal : int -> int -> bool
    val equal_def :
      (x : int) -> (y : int) -> {u : unit | (equal x y) === (x = y)}
    val hash : int -> int
    val hash_def : (x : int) -> {u : unit | (hash x) === (x land 1023)}
    val reflexive : (x : int) -> {u : unit | equal x x}
    val transitive :
      (x : int) ->
      (y : int) ->
      (z : int) ->
      {u : unit | (not ((equal x y) && (equal y z))) || (equal x z)}
    val hash_equal :
      (x : int) ->
      (y : int) -> {u : unit | (not (equal x y)) || ((hash x) = (hash y))}
    val symmetric :
      (x : int) -> (y : int) -> {u : unit | (not (equal x y)) || (equal y x)}
  end
|}]

module One_way_table : sig end =
  Vox_verified_flat_hashtbl.Make (One_way_key);;
[%%expect{|
Line 2, characters 2-46:
2 |   Vox_verified_flat_hashtbl.Make (One_way_key);;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "One_way_key.symmetric" does not satisfy the functor's parameter.
       Refinement could not be proved (counterexample: x = 0, y = 1)
File "vox_verified_flat_hashtbl.mli", line 59, characters 52-73:
  The refinement is stated here.
|}]

(* 3. Store 84 and claim 84, without the two law calls: rejected. *)
module Store_84_without_laws = struct
  module T = Vox_verified_flat_hashtbl.Make (Int_key)
  let f () =
    let r : int T.created = T.create (Ghost_pref.empty ()) in
    let changed = T.replace r.#table r.#view 1 84 r.#token in
    let value : {v : int | v = 84} =
      T.find r.#table changed.#view 1 (borrow_ changed.#token) in value
end;;
[%%expect{|
Line 7, characters 6-62:
7 |       T.find r.#table changed.#view 1 (borrow_ changed.#token) in value
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 6, characters 27-33:
6 |     let value : {v : int | v = 84} =
                               ^^^^^^
  The refinement is stated here.
|}]

(* With the law calls, as in flat_hashtbl_boundary.ml, 84 is accepted. *)
module Store_84_with_laws : sig val f : unit -> int end = struct
  module T = Vox_verified_flat_hashtbl.Make (Int_key)
  let f () =
    let r : int T.created = T.create (Ghost_pref.empty ()) in
    let changed = T.replace r.#table r.#view 1 84 r.#token in
    ghost_ (Int_key.reflexive 1; T.Map.put_get (T.bindings r.#view) 1 84 1);
    let value : {v : int | v = 84} =
      T.find r.#table changed.#view 1 (borrow_ changed.#token) in value
end;;
[%%expect{|
module Store_84_with_laws : sig val f : unit -> int end
|}]
