(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_ring.mli pref_ring.ml pref_ring_proofs.ml pref_ring_general.mli pref_ring_general.ml pref_ring_splice_general.mli pref_ring_splice_general.ml pref_ring_splice_general_client.ml";
 { native; }
*)
open Pref_ring
open Pref_ring_general
module S = Pref_ring_splice_general

let splice_owned : (prefix : node list) @ immutable ghost -> (first : node) @ immutable ->
    (rest : node list) @ immutable ghost ->
    (final : {n : node | n === S.last first rest}) @ immutable ->
    (suffix : node list) @ immutable ghost ->
    (destination_prefix : node list) @ immutable ghost -> (destination_left : node) @ immutable ->
    (destination_suffix : node list) @ immutable ghost ->
    (b : {b : S.paired |
      ring (Pref.own b.#state) b.#source b.#source_nodes &&
      ring (Pref.own b.#state) b.#destination b.#destination_nodes &&
      separated (append (b.#source :: b.#source_nodes) (b.#destination :: b.#destination_nodes)) &&
      b.#source_nodes === append prefix (append (first :: rest) suffix) &&
      b.#destination_nodes === append destination_prefix destination_suffix &&
      destination_left === S.last b.#destination destination_prefix}) @ unique ->
    {r : S.paired | r.#source === b.#source && r.#destination === b.#destination &&
      r.#source_nodes === append prefix suffix &&
      r.#destination_nodes === append destination_prefix (append (first :: rest) destination_suffix) &&
      Pref.own r.#state === S.spliced (Pref.own b.#state) (S.last b.#source prefix) first final
        (head suffix b.#source) destination_left (head destination_suffix b.#destination) &&
      ring (Pref.own r.#state) r.#source r.#source_nodes &&
      ring (Pref.own r.#state) r.#destination r.#destination_nodes &&
      separated (append (r.#source :: r.#source_nodes) (r.#destination :: r.#destination_nodes))} @ unique =
  fun prefix first rest final suffix destination_prefix destination_left destination_suffix b ->
  let state = S.Owned.adopt b in
  let state = S.Owned.splice prefix first rest final suffix destination_prefix
    destination_left destination_suffix state in
  let source = S.Owned.observe_source (borrow_ state) in
  let destination = S.Owned.observe_destination (borrow_ state) in
  ghost_ (let _ : {u : unit | source === append prefix suffix &&
    destination === append destination_prefix (append (first :: rest) destination_suffix)} = () in ());
  let raw = S.Owned.release state in
  let state = S.Owned.adopt raw in
  S.Owned.release state
