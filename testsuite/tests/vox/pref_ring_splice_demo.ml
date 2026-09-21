(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_ring.ml pref_ring_checks.ml pref_ring_alloc.ml pref_ring_splice_model.ml pref_ring_splice_setup.ml pref_ring_splice.ml pref_ring_splice_demo.ml";
 { bytecode; }
 { native; }
*)

open Pref_ring
open Pref_ring_splice

let run_splice_demo () =
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
  let flag = true in
  let data = 1 in
  let t : {t : Pref.token | H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next
    && H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next
    && H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next} = t in
  let r = Pref_ring_alloc.extend s s s flag data t in
  let d = r.node in
  let t = r.state in
  let flag = false in
  let data = 2 in
  let t : {t : Pref.token | H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next
    && H.mem (Pref.own t) d.prev
    && H.mem (Pref.own t) d.next
    && H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next} = t in
  let r = Pref_ring_alloc.extend s d s flag data t in
  let a = r.node in
  let t = r.state in
  let flag = false in
  let data = 3 in
  let t : {t : Pref.token | H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) s.next
    && H.mem (Pref.own t) d.prev
    && H.mem (Pref.own t) d.next
    && H.mem (Pref.own t) a.prev
    && H.mem (Pref.own t) a.next} = t in
  let r = Pref_ring_alloc.extend s d a flag data t in
  let b = r.node in
  let t = r.state in
  let t : {t : Pref.token | true
      && s.sentinel
      && not (s.prev === s.next)
      && H.mem (Pref.own t) s.prev
      && H.mem (Pref.own t) s.next
      && H.at (Pref.own t) s.prev === Some (Some s)
      && H.at (Pref.own t) s.next === Some (Some s)
      && d.sentinel
      && not (d.prev === d.next)
      && H.mem (Pref.own t) d.prev
      && H.mem (Pref.own t) d.next
      && H.at (Pref.own t) d.prev === Some (Some d)
      && H.at (Pref.own t) d.next === Some (Some d)
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
      && not (s === d)
      && not (s.prev === d.prev)
      && not (s.prev === d.next)
      && not (s.next === d.prev)
      && not (s.next === d.next)
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
      && not (d === a)
      && not (d.prev === a.prev)
      && not (d.prev === a.next)
      && not (d.next === a.prev)
      && not (d.next === a.next)
      && not (d === b)
      && not (d.prev === b.prev)
      && not (d.prev === b.next)
      && not (d.next === b.prev)
      && not (d.next === b.next)
      && not (a === b)
      && not (a.prev === b.prev)
      && not (a.prev === b.next)
      && not (a.next === b.prev)
      && not (a.next === b.next)} = t in
  splice_demo s d a b t;

  let actual : {v : int | v = 42} =
    let borrowed = borrow_ frame_token in
    let borrowed : {t : Pref.token | H.mem (Pref.own t) unrelated} =
        borrowed in
    let actual = Pref.read unrelated borrowed in actual in
  assert (actual = 42)

let () = run_splice_demo ()
