(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_ring.mli pref_ring.ml pref_ring_proofs.ml pref_ring_general.mli pref_ring_general.ml pref_ring_general_client.ml";
 { bytecode; }
*)
open Pref_ring
module G = Pref_ring_general

let reverse_owned :
    (b : {b : G.built | ring (Pref.own b.state) b.sentinel b.nodes &&
      separated (b.sentinel :: b.nodes)}) @ unique ->
    {r : G.built | r.sentinel === b.sentinel && r.nodes === G.reversed b.nodes &&
      Pref.own r.state === flipped_all (Pref.own b.state) (b.sentinel :: b.nodes) &&
      ring (Pref.own r.state) r.sentinel r.nodes && separated (r.sentinel :: r.nodes)}
      @ unique = fun b ->
  let owned = G.Owned.adopt b in
  let owned = G.Owned.reverse owned in
  let seen = G.Owned.observe (borrow_ owned) in
  ghost_ (let _ : {u : unit | seen === G.Owned.model owned} = () in ());
  let raw = G.Owned.release owned in
  let owned = G.Owned.adopt raw in
  G.Owned.release owned

let () =
  let ring = G.Owned.empty () in
  let before = G.Owned.observe (borrow_ ring) in
  let ring = G.Owned.reverse ring in
  ghost_ (G.reversed_def []);
  let after = G.Owned.observe (borrow_ ring) in
  assert (before = [] && after = []);
  let _raw = G.Owned.release ring in ()
