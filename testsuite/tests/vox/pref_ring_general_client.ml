(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml pref_ring.mli pref_ring.ml pref_ring_proofs.ml pref_ring_general.mli pref_ring_general.ml";
 { bytecode; }
 { native; }
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

let push_front : (value : int) -> (state : G.Owned.t) @ unique ->
    {r : G.Owned.insertion | r.#node.value = value &&
      G.Owned.model r.#ring === r.#node :: G.Owned.model state} @ unique =
  fun value state ->
  let sentinel = G.Owned.sentinel_node (borrow_ state) in
  let nodes = ghost_ (G.Owned.model (borrow_ state)) in
  ghost_ (G.append_def [] nodes; G.last_def sentinel []);
  let #{node; ring} : G.Owned.insertion =
    G.Owned.insert [] sentinel nodes value state in
  ghost_ (G.append_def [] (node :: nodes));
  #{node; ring}

let pop_front : (state : G.Owned.t) @ unique ->
    {next : G.Owned.t | G.Owned.model next ===
      (match G.Owned.model state with [] -> [] | _ :: rest -> rest)} @ unique =
  fun state ->
  match G.Owned.observe (borrow_ state) with
  | [] -> state
  | n :: rest ->
    ghost_ (G.append_def [] (n :: rest); G.append_def [] rest);
    G.Owned.remove [] n rest state

let values nodes = List.map (fun n -> n.value) nodes

let () =
  let ring = G.Owned.empty () in
  let before = G.Owned.observe (borrow_ ring) in
  let ring = G.Owned.reverse ring in
  ghost_ (G.reversed_def []);
  let after = G.Owned.observe (borrow_ ring) in
  assert (before = [] && after = []);
  let third = push_front 3 ring in
  let second = push_front 2 third.#ring in
  let first = push_front 1 second.#ring in
  let ring = first.#ring in
  let a = first.#node in
  let b = second.#node in
  let c = third.#node in
  let nodes = G.Owned.observe (borrow_ ring) in
  ghost_ ((() : {u : unit | nodes === [a; b; c]}));
  assert (values nodes = [1; 2; 3]);
  (* Remove the middle node, then append a node after the last one. *)
  ghost_ (G.append_def [a] (b :: [c]); G.append_def [] (b :: [c]));
  let ring = G.Owned.remove [a] b [c] ring in
  let sentinel = ghost_ (G.Owned.sentinel (borrow_ ring)) in
  ghost_ (G.append_def [a] [c]; G.append_def [] [c];
    G.append_def [a; c] []; G.append_def [c] []; G.append_def [] [];
    G.last_def sentinel [a; c]; G.last_def a [c]; G.last_def c []);
  let last = G.Owned.insert [a; c] c [] 4 ring in
  let d = last.#node in
  let ring = last.#ring in
  ghost_ (G.append_def [a; c] [d]; G.append_def [c] [d]; G.append_def [] [d]);
  let nodes = G.Owned.observe (borrow_ ring) in
  ghost_ ((() : {u : unit | nodes === [a; c; d] && a.value = 1 && c.value = 3
    && d.value = 4}));
  assert (values nodes = [1; 3; 4]);
  let ring = G.Owned.reverse ring in
  ghost_ (G.reversed_def [a; c; d]; G.reversed_def [c; d]; G.reversed_def [d];
    G.reversed_def []; G.append_def [] [d]; G.append_def [d] [c];
    G.append_def [] [c]; G.append_def [d; c] [a]; G.append_def [c] [a];
    G.append_def [] [a]);
  let nodes = G.Owned.observe (borrow_ ring) in
  ghost_ ((() : {u : unit | nodes === [d; c; a]}));
  assert (values nodes = [4; 3; 1]);
  let ring = pop_front ring in
  let ring = pop_front ring in
  let ring = pop_front ring in
  let ring = pop_front ring in
  let nodes = G.Owned.observe (borrow_ ring) in
  ghost_ ((() : {u : unit | nodes === []}));
  assert (nodes = []);
  let _raw = G.Owned.release ring in ()
