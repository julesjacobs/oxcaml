(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_ring.mli pref_ring.ml pref_ring_proofs.ml pref_ring_checks.ml pref_ring_alloc.ml pref_ring_reverse_model.ml pref_ring_reverse_setup.ml pref_ring_reverse.mli pref_ring_reverse.ml pref_ring_general.mli pref_ring_general.ml pref_ring_general_demo.ml";
 { bytecode; }
 { native; }
*)

open Pref_ring
open Pref_ring_reverse
module G = Pref_ring_general

let run_general_demo () =
  let frame = Pref_ring_alloc.make_frame () in
  let unrelated = frame.value in
  let frame_token = frame.state in
  let u = () in
  let _frame_contents : {u : unit | H.mem (Pref.own frame_token) unrelated
    && H.at (Pref.own frame_token) unrelated === Some 42} = u in
  let t = Pref.empty () in
  let flag = true in
  let data = 0 in
  let r = make_node flag data t in
  let s = r.node in
  let t = r.state in
  let flag = false in
  let data = 1 in
  let t : {t : node option Pref.token | H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next
    && H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next
    && H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next} = t in
  let r = Pref_ring_alloc.extend s s s flag data t in
  let a = r.node in
  let t = r.state in
  let flag = false in
  let data = 2 in
  let t : {t : node option Pref.token | H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next
    && H.mem (Pref.own t) a.prev
    && H.mem (Pref.own t) a.next
    && H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next} = t in
  let r = Pref_ring_alloc.extend s a s flag data t in
  let b = r.node in
  let t = r.state in
  let flag = false in
  let data = 3 in
  let t : {t : node option Pref.token | H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next
    && H.mem (Pref.own t) a.prev
    && H.mem (Pref.own t) a.next
    && H.mem (Pref.own t) b.prev
    && H.mem (Pref.own t) b.next} = t in
  let r = Pref_ring_alloc.extend s a b flag data t in
  let c = r.node in
  let t = r.state in
  ghost_ (
    apart_def s a; apart_def s b; apart_def s c;
    apart_def a b; apart_def a c; apart_def b c;
    apart_all_def s [a; b; c]; apart_all_def s [b; c];
    apart_all_def s [c]; apart_all_def s [];
    apart_all_def a [b; c]; apart_all_def a [c]; apart_all_def a [];
    apart_all_def b [c]; apart_all_def b []; apart_all_def c [];
    separated_def [s; a; b; c]; separated_def [a; b; c];
    separated_def [b; c]; separated_def [c]; separated_def [];
    Pref_ring_proofs.isolated_four (Pref.own (borrow_ t)) s a b c);
  let t = reverse_demo s a b c t in
  ghost_ (
    apart_all_def s [c; b; a]; apart_all_def s [b; a]; apart_all_def s [a];
    apart_all_def c [b; a]; apart_all_def c [a];
    apart_all_def b [a]; apart_all_def a [];
    apart_def c b; apart_def c a; apart_def b a;
    separated_def [s; c; b; a]; separated_def [c; b; a];
    separated_def [b; a]; separated_def [a]);
  let ring = G.Owned.adopt {G.sentinel = s; nodes = ghost_ [c; b; a]; state = t} in
  let ring = G.Owned.reverse ring in
  ghost_ (
    G.reversed_def [c; b; a]; G.reversed_def [b; a];
    G.reversed_def [a]; G.reversed_def [];
    G.append_def [] [a]; G.append_def [a] [b]; G.append_def [] [b];
    G.append_def [a; b] [c]; G.append_def [b] [c]; G.append_def [] [c]);
  let nodes = G.Owned.observe (borrow_ ring) in
  ghost_ (let _ : {u : unit | nodes === [a; b; c]} = () in ());
  assert (List.map (fun n -> n.value) nodes = [1; 2; 3]);
  let raw = G.Owned.release ring in
  ghost_ (let _ : {u : unit | raw.sentinel === s && raw.nodes === [a; b; c]} = () in ());
  let actual = Pref.read unrelated (borrow_ frame_token) in
  assert (actual = 42)

let () = run_general_demo ()
