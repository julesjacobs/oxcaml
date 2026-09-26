(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_ring.mli pref_ring.ml pref_ring_proofs.ml pref_ring_checks.ml pref_ring_alloc.ml pref_ring_heap.ml pref_ring_insert_remove.ml pref_ring_insert_remove_demo.ml";
 { bytecode; }
*)

open Pref_ring
open Pref_ring_insert_remove

let run_insert_remove_demo () =
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
  let t : {t : node option Pref.token | true
      && s.sentinel
      && not (s.prev === s.next)
      && H.mem (Pref.own t) s.prev
      && H.mem (Pref.own t) s.next
      && H.at (Pref.own t) s.prev === Some (Some s)
      && H.at (Pref.own t) s.next === Some (Some s)
      && not a.sentinel
      && not (a.prev === a.next)
      && H.mem (Pref.own t) a.prev
      && H.mem (Pref.own t) a.next
      && H.at (Pref.own t) a.prev === Some (Some a)
      && H.at (Pref.own t) a.next === Some (Some a)
      && not b.sentinel
      && not (b.prev === b.next)
      && H.mem (Pref.own t) b.prev
      && H.mem (Pref.own t) b.next
      && H.at (Pref.own t) b.prev === Some (Some b)
      && H.at (Pref.own t) b.next === Some (Some b)
      && not (s === a)
      && not (s.prev === a.prev)
      && not (s.prev === a.next)
      && not (s.next === a.prev)
      && not (s.next === a.next)
      && not (s === b)
      && not (s.prev === b.prev)
      && not (s.prev === b.next)
      && not (s.next === b.prev)
      && not (s.next === b.next)
      && not (a === b)
      && not (a.prev === b.prev)
      && not (a.prev === b.next)
      && not (a.next === b.prev)
      && not (a.next === b.next)} = t in
  insert_remove_demo s a b t;

  let actual : {v : int | v = 42} =
    let borrowed = borrow_ frame_token in
    let borrowed : {t : int Pref.token | H.mem (Pref.own t) unrelated} = borrowed in
    let actual = Pref.read unrelated borrowed in actual in
  assert (actual = 42)

let () = run_insert_remove_demo ()
