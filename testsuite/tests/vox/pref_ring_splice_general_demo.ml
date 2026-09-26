(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_ring.mli pref_ring.ml pref_ring_proofs.ml pref_ring_general.mli pref_ring_general.ml pref_ring_splice_general.mli pref_ring_splice_general.ml pref_ring_splice_fixture.ml pref_ring_splice_general_demo.ml";
 { bytecode; }
 { native; }
*)
open Pref_ring
open Pref_ring_general
module S = Pref_ring_splice_general
module F = Pref_ring_splice_fixture

let () =
  let frame = Pref.alloc 42 (Pref.empty ()) in
  let unrelated = frame.value in
  let frame_token = frame.state in
  let state = Pref.empty () in
  ghost_ (isolated_def (Pref.own (borrow_ state)) []);
  let made = F.allocate false 5 (ghost_ []) state in
  let e = made.node in
  let state = made.state in
  let made = F.allocate false 4 (ghost_ [e]) state in
  let d = made.node in
  let state = made.state in
  let made = F.allocate true 0 (ghost_ [d; e]) state in
  let t = made.node in
  let state = made.state in
  let made = F.allocate false 3 (ghost_ [t; d; e]) state in
  let c = made.node in
  let state = made.state in
  let made = F.allocate false 2 (ghost_ [c; t; d; e]) state in
  let b = made.node in
  let state = made.state in
  let made = F.allocate false 1 (ghost_ [b; c; t; d; e]) state in
  let a = made.node in
  let state = made.state in
  let made = F.allocate true 0 (ghost_ [a; b; c; t; d; e]) state in
  let s = made.node in
  let state = made.state in
  let state = F.wire s a b c t d e state in
  ghost_ (
    append_def [b] [c];
    append_def [] [c];
    append_def [a] [b; c];
    append_def [] [b; c];
    append_def [d] [e];
    append_def [] [e];
    S.last_def b [];
    S.last_def t [d];
    S.last_def d [];
    ());
  let state = S.splice (ghost_ s) (ghost_ t) (ghost_ [a]) b (ghost_ []) b
    (ghost_ [c]) (ghost_ [d]) d (ghost_ [e]) state in
  ghost_ (
    append_def [a] [c];
    append_def [] [c];
    append_def [b] [e];
    append_def [] [e];
    append_def [d] [b; e];
    append_def [] [b; e];
    ());
  let rings = S.Owned.adopt #{S.source = s; destination = t;
    source_nodes = ghost_ [a; c]; destination_nodes = ghost_ [d; b; e]; state} in
  let source = S.Owned.observe_source (borrow_ rings) in
  let destination = S.Owned.observe_destination (borrow_ rings) in
  assert (List.map (fun n -> n.value) source = [1; 3]);
  assert (List.map (fun n -> n.value) destination = [4; 2; 5]);
  ghost_ (
    append_def [a] [c];
    append_def [] [c];
    append_def [] [a; c];
    append_def [] [d; b; e];
    S.last_def a [];
    S.last_def t [];
    ());
  let rings = S.Owned.splice (ghost_ []) a (ghost_ []) a (ghost_ [c])
    (ghost_ []) t (ghost_ [d; b; e]) rings in
  ghost_ (
    append_def [] [c];
    append_def [a] [d; b; e];
    append_def [] [d; b; e];
    append_def [] [a; d; b; e];
    append_def [c] [];
    append_def [] [];
    append_def [] [c];
    append_def [a; d; b; e] [];
    append_def [d; b; e] [];
    append_def [b; e] [];
    append_def [e] [];
    append_def [] [];
    S.last_def c [];
    S.last_def t [a; d; b; e];
    S.last_def a [d; b; e];
    S.last_def d [b; e];
    S.last_def b [e];
    S.last_def e [];
    ());
  let rings = S.Owned.splice (ghost_ []) c (ghost_ []) c (ghost_ [])
    (ghost_ [a; d; b; e]) e (ghost_ []) rings in
  ghost_ (
    append_def [] [];
    append_def [c] [];
    append_def [] [];
    append_def [a; d; b; e] [c];
    append_def [d; b; e] [c];
    append_def [b; e] [c];
    append_def [e] [c];
    append_def [] [c];
    ());
  let rings = S.Owned.swap rings in
  ghost_ (
    append_def [a; d; b; e; c] [];
    append_def [d; b; e; c] [];
    append_def [b; e; c] [];
    append_def [e; c] [];
    append_def [c] [];
    append_def [] [];
    append_def [] [a; d; b; e; c];
    S.last_def a [d; b; e; c];
    S.last_def d [b; e; c];
    S.last_def b [e; c];
    S.last_def e [c];
    S.last_def c [];
    S.last_def s [];
    ());
  let rings = S.Owned.splice (ghost_ []) a (ghost_ [d; b; e; c]) c (ghost_ [])
    (ghost_ []) s (ghost_ []) rings in
  ghost_ (
    append_def [] [];
    append_def [a; d; b; e; c] [];
    append_def [d; b; e; c] [];
    append_def [b; e; c] [];
    append_def [e; c] [];
    append_def [c] [];
    append_def [] [];
    append_def [] [a; d; b; e; c];
    ());
  let source = S.Owned.observe_source (borrow_ rings) in
  let destination = S.Owned.observe_destination (borrow_ rings) in
  ghost_ (let _ : {u : unit | source === [] && destination === [a; d; b; e; c]} = () in ());
  assert (source = []);
  assert (List.map (fun n -> n.value) destination = [1; 4; 2; 5; 3]);
  let raw = S.Owned.release rings in
  ghost_ (let _ : {u : unit | raw.#source === t && raw.#destination === s &&
    raw.#source_nodes === [] && raw.#destination_nodes === [a; d; b; e; c]} = () in ());
  let rings = S.Owned.adopt raw in
  let _raw = S.Owned.release rings in
  let actual = Pref.read unrelated (borrow_ frame_token) in
  assert (actual = 42)
