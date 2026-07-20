(* Differential + directed self-test for the incremental difference-logic graph {!Dl}.
   Stdlib-only (I3), deterministic (fixed-seed xorshift), SIMPLEX-FREE.

   The oracle is an INDEPENDENT Bellman–Ford over an explicit edge list (written from the
   difference-constraint definition, not from {!Dl}'s internals), used two ways:
   - consistency: {!Dl.assert_edge}'s verdict must match "is the resulting live set free
     of negative cycles"; a returned conflict's premises must themselves be a negative
     cycle;
   - entailment: {!Dl.implied} must return [Some] iff [G ∧ ¬C] is infeasible (adding the
     integer negation [hd - tl >= weight+1] closes a negative cycle) — soundness AND
     completeness. Returned reason paths are checked to actually entail the query. *)

open Oxsmt_lia

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* ---- fixed-seed PRNG (xorshift64-star) ---- *)
let rng = ref 0x1E3779B97F4A7C15

let rand_bits () =
  let x = !rng in
  let x = x lxor (x lsr 12) in
  let x = x lxor (x lsl 25) in
  let x = x lxor (x lsr 27) in
  rng := x;
  x * 0x2545F4914F6CDD1D
;;

let rand_int n = abs (rand_bits ()) mod n

(* ---- independent oracle: negative cycle over an edge list [(tl,hd,w)] ---- *)
(* Constraint hd - tl <= w is edge tl->hd weight w. Consistent iff no negative cycle
   (virtual 0-source to all nodes). *)
let has_negative_cycle nnodes edges =
  let d = Array.make (max nnodes 1) 0 in
  let n = Array.length d in
  for _ = 0 to n do
    List.iter (fun (tl, hd, w) -> if d.(tl) + w < d.(hd) then d.(hd) <- d.(tl) + w) edges
  done;
  List.exists (fun (tl, hd, w) -> d.(tl) + w < d.(hd)) edges
;;

let consistent nnodes edges = not (has_negative_cycle nnodes edges)

(* Sum a premise path's weights and verify it is a walk tl ⇝ hd. Premise ids index [tbl]. *)
let path_entails tbl ~tl ~hd ~weight prems =
  (* Reorder-free check: the premise edges must chain from tl to hd with total <= weight.
     We greedily thread them as a path. *)
  let get id = Hashtbl.find tbl id in
  let rec thread cur total = function
    | [] -> cur = hd && total <= weight
    | _ :: _ as rem ->
      (* find an edge in [rem] starting at [cur] *)
      let rec pick acc = function
        | [] -> None
        | id :: tl' ->
          let a, b, w = get id in
          if a = cur
          then Some (id, b, w, List.rev_append acc tl')
          else pick (id :: acc) tl'
      in
      (match pick [] rem with
       | Some (_, b, w, rest) -> thread b (total + w) rest
       | None -> false)
  in
  (* empty path entails only tl = hd, weight >= 0 *)
  match prems with
  | [] -> tl = hd && weight >= 0
  | _ -> thread tl 0 prems
;;

let () = Printf.printf "dl_test: directed + differential\n"

(* ---- directed cases ---- *)
let directed () =
  (* self negative loop x - x <= -1 : immediate conflict *)
  let t = Dl.create () in
  (match Dl.assert_edge t ~tl:0 ~hd:0 ~weight:(-1) 0 with
   | Some { premises } -> check "self-loop conflict prems" (premises = [ 0 ])
   | None -> check "self-loop should conflict" false);
  (* triangle: y-x<=1 (0->1,1), z-y<=1 (1->2,1), x-z<=-3 (2->0,-3): sum -1 < 0 => cycle *)
  let t = Dl.create () in
  check "e1" (Dl.assert_edge t ~tl:0 ~hd:1 ~weight:1 1 = None);
  check "e2" (Dl.assert_edge t ~tl:1 ~hd:2 ~weight:1 2 = None);
  (match Dl.assert_edge t ~tl:2 ~hd:0 ~weight:(-3) 3 with
   | Some { premises } -> check "triangle cycle 3 prems" (List.length premises = 3)
   | None -> check "triangle should conflict" false);
  (* after rejected conflict edge, graph still consistent; and x-z<=-2 (sum 0) is fine *)
  check "e3 ok" (Dl.assert_edge t ~tl:2 ~hd:0 ~weight:(-1) 4 = None);
  (* implied: with 0->1<=1 and 1->2<=1, is 2-0 <= 2 implied? yes (path 0->1->2 = 2) *)
  let t = Dl.create () in
  check "i1" (Dl.assert_edge t ~tl:0 ~hd:1 ~weight:1 1 = None);
  check "i2" (Dl.assert_edge t ~tl:1 ~hd:2 ~weight:1 2 = None);
  (match Dl.implied t ~tl:0 ~hd:2 ~weight:2 with
   | Some prems -> check "implied 2-0<=2" (List.length prems = 2)
   | None -> check "should be implied 2-0<=2" false);
  check "not implied 2-0<=1" (Dl.implied t ~tl:0 ~hd:2 ~weight:1 = None)
;;

(* ---- differential fuzz ---- *)
let fuzz () =
  let tbl = Hashtbl.create 256 in
  (* one Dl.t; a parallel stack of live-edge lists mirroring push/pop frames *)
  let t = Dl.create () in
  let nnodes = 6 in
  let frames = ref [ [] ] in
  (* head = current frame's live (tl,hd,w) *)
  let all_live () = List.concat !frames in
  let next_id = ref 0 in
  for _step = 1 to 20000 do
    let op = rand_int 10 in
    if op < 6
    then (
      (* assert a random edge *)
      let tl = rand_int nnodes
      and hd = rand_int nnodes
      and w = rand_int 7 - 3 in
      let id = !next_id in
      incr next_id;
      Hashtbl.replace tbl id (tl, hd, w);
      let before = all_live () in
      let res = Dl.assert_edge t ~tl ~hd ~weight:w id in
      match res with
      | None ->
        (* edge accepted: naive says the augmented set is consistent *)
        check "accept => consistent" (consistent nnodes ((tl, hd, w) :: before));
        (* record it in the current frame *)
        (match !frames with
         | fr :: rest -> frames := ((tl, hd, w) :: fr) :: rest
         | [] -> frames := [ [ tl, hd, w ] ])
      | Some { premises } ->
        (* edge rejected: adding it WOULD be inconsistent; graph stays consistent *)
        check
          "reject => would be inconsistent"
          (not (consistent nnodes ((tl, hd, w) :: before)));
        check "reject => graph still consistent" (consistent nnodes before);
        (* premises form a real negative cycle: build their edge list and test *)
        let pe = List.map (fun id -> Hashtbl.find tbl id) premises in
        check "conflict prems are a neg cycle" (has_negative_cycle nnodes pe))
    else if op < 8
    then (
      Dl.push t;
      frames := [] :: !frames)
    else if op < 9
    then (
      match !frames with
      | _ :: (_ :: _ as rest) ->
        Dl.pop t 1;
        frames := rest
      | _ -> ())
    else (
      (* random implied query, differentially checked via negation *)
      let tl = rand_int nnodes
      and hd = rand_int nnodes
      and w = rand_int 7 - 3 in
      let live = all_live () in
      (* entailed iff G ∧ (hd - tl >= w+1) infeasible; negation edge hd->tl weight -(w+1) *)
      let entailed = not (consistent nnodes ((hd, tl, -(w + 1)) :: live)) in
      match Dl.implied t ~tl ~hd ~weight:w with
      | Some prems ->
        check "implied Some => entailed" entailed;
        check "implied path entails query" (path_entails tbl ~tl ~hd ~weight:w prems)
      | None -> check "implied None => not entailed" (not entailed))
  done
;;

let () =
  directed ();
  fuzz ();
  Printf.printf "dl_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
