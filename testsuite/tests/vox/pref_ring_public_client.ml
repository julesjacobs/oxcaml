(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_pref_semantics.mli vox_pref_semantics.ml pref_ring.mli pref_ring.ml pref_ring_public_client.ml";
 { bytecode; }
 { native; }
*)

open Pref_ring

let (at_put @ total) (h : node option Pref.heap @ immutable)
    (p : node option Pref.t @ immutable) (v : node option @ immutable)
    (q : node option Pref.t @ immutable) :
    {u : unit | H.at (H.put h p v) q ===
      (if p === q then Some v else H.at h q)} @ ghost =
  ghost_ (())

let splice_preserves_frame_cell (left : node @ immutable) (first : node @ immutable)
    (last : node @ immutable) (right : node @ immutable)
    (destination_left : node @ immutable) (destination_right : node @ immutable)
    (q : node option Pref.t @ immutable)
    (t : {t : node option Pref.token |
      not (q === left.next) && not (q === right.prev)
      && not (q === destination_left.next) && not (q === first.prev)
      && not (q === last.next) && not (q === destination_right.prev) &&
      H.mem (Pref.own t) left.next && H.mem (Pref.own t) right.prev
      && H.mem (Pref.own t) destination_left.next
      && H.mem (Pref.own t) first.prev && H.mem (Pref.own t) last.next
      && H.mem (Pref.own t) destination_right.prev
      && H.at (Pref.own t) left.next === Some (Some first)
      && H.at (Pref.own t) first.prev === Some (Some left)
      && H.at (Pref.own t) last.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some last)
      && H.at (Pref.own t) destination_left.next === Some (Some
          destination_right)
      && H.at (Pref.own t) destination_right.prev === Some (Some
          destination_left)}
      @ unique)
    : {r : node option Pref.token | H.at (Pref.own r) q === H.at (Pref.own t) q} @ unique =
  let before = ghost_ (Pref.own (borrow_ t)) in
  ghost_ (
    at_put before left.next (Some right) q;
    let h = H.put before left.next (Some right) in
    at_put h right.prev (Some left) q;
    let h = H.put h right.prev (Some left) in
    at_put h destination_left.next (Some first) q;
    let h = H.put h destination_left.next (Some first) in
    at_put h first.prev (Some destination_left) q;
    let h = H.put h first.prev (Some destination_left) in
    at_put h last.next (Some destination_right) q;
    let h = H.put h last.next (Some destination_right) in
    at_put h destination_right.prev (Some last) q);
  let result = splice_range left first last right
    destination_left destination_right t in
  result

let run () =
  let token = Pref.empty () in
  let sentinel = true in
  let payload = 42 in
  let made = make_node sentinel payload token in
  let n = made.node in
  let token = made.state in
  let before = ghost_ (Pref.own (borrow_ token)) in
  let token = connect n n token in
  ghost_ (connected_def before n n;
    Vox_pref_semantics.put before n.next (Some n) n.next;
    Vox_pref_semantics.put (H.put before n.next (Some n))
      n.prev (Some n) n.next);
  let next = n.next in
  let token : {t : node option Pref.token | H.mem (Pref.own t) next} = token in
  let value = Pref.read next (borrow_ token) in
  assert (value = Some n)

let () = run ()
