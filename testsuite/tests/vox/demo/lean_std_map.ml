(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/voption.mli ../lib/voption.ml ../lib/Vlist.mli ../lib/Vlist.ml ../lib/vmap.mli ../lib/vmap.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: a client of the vox_stdlib [Vmap] SEMANTIC map face -- the stdlib
   counterpart of demo/lean_htbl.  lean_htbl models a bucketed hash table
   with real collisions (its lesson is the hashing repr); [Vmap] models an
   abstract key->value map, so this demo is additive, not a replacement.
   [Vmap] depends on [Vlist] (keys enumerates into it) and (transitively)
   [Voption], snapshotted in dependency order.

   The [mopt] constructors ([MFound] / [MMiss]) may not appear in a
   refinement (grammar; see notes/vmap.md), so find goals are stated as
   model EQUATIONS between two [m_find] applications -- the law rewrites
   both sides to the same normal form. *)

open Vmap
open Vlist

(* find of the just-added key -> its value (m_find_add_eq). *)
let find_added (k : int) (v : int) (m : Vmap.t) :
    mopt{ _ = m_find k (m_add k v m) } =
  Vmap.find k (Vmap.add k v m)

(* find on empty misses, key-independent (m_find_empty). *)
let find_empty (k : int) : mopt{ _ = m_find k m_empty } =
  Vmap.find k (Vmap.empty ())

(* a shadowing add of a DIFFERENT key is seen through (m_find_add_ne):
   ground distinct keys 1, 2. *)
let find_other (v : int) (m : Vmap.t) : mopt{ _ = m_find 1 m } =
  Vmap.find 1 (Vmap.add 2 v m)

(* keys eliminator: enumerate the map's keys into a [Vlist]; membership in
   the key-list agrees with key-presence (m_keys_spec + Vlist.ll_mem),
   composing two stdlib modules' vocabularies. *)
let has_key (k : int) (m : Vmap.t) : bool{ _ = m_haskey k m } =
  Vlist.mem k (Vmap.keys m)
