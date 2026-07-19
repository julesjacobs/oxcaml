(* Incremental difference-logic constraint graph — see dl.mli. Cotton–Maler 2006 for the
   incremental consistency check + potential maintenance; the negative-cycle premise
   extraction and the on-demand implied-path query are the theory-solver additions.

   All scratch (heap, gamma/pred, distance arrays) lives per-instance in [t]: no
   module-global mutable state, so distinct [Lia] instances never alias, and every run
   fully resets what it touched. *)

type 'p conflict = { premises : 'p list }

(* An asserted constraint [hd - tl <= w], edge [tl -(w)-> hd], attributed to [prem]. *)
type 'p edge =
  { tl : int
  ; hd : int
  ; w : int
  ; prem : 'p
  }

(* Binary min-heap over (key, node) with lazy deletion (decrease-key = re-push, stale
   entries skipped by the caller comparing the popped key to the current one). *)
type heap =
  { mutable ks : int array
  ; mutable ns : int array
  ; mutable len : int
  }

let heap_create () = { ks = Array.make 16 0; ns = Array.make 16 0; len = 0 }
let heap_clear h = h.len <- 0

let heap_swap h i j =
  let k = h.ks.(i)
  and n = h.ns.(i) in
  h.ks.(i) <- h.ks.(j);
  h.ns.(i) <- h.ns.(j);
  h.ks.(j) <- k;
  h.ns.(j) <- n
;;

let heap_push h key node =
  if h.len >= Array.length h.ks
  then (
    let cap = 2 * Array.length h.ks in
    let ks = Array.make cap 0
    and ns = Array.make cap 0 in
    Array.blit h.ks 0 ks 0 h.len;
    Array.blit h.ns 0 ns 0 h.len;
    h.ks <- ks;
    h.ns <- ns);
  let i = ref h.len in
  h.ks.(!i) <- key;
  h.ns.(!i) <- node;
  h.len <- h.len + 1;
  while !i > 0 && h.ks.((!i - 1) / 2) > h.ks.(!i) do
    let p = (!i - 1) / 2 in
    heap_swap h !i p;
    i := p
  done
;;

let heap_pop_min h =
  if h.len = 0
  then None
  else (
    let key = h.ks.(0)
    and node = h.ns.(0) in
    h.len <- h.len - 1;
    if h.len > 0
    then (
      h.ks.(0) <- h.ks.(h.len);
      h.ns.(0) <- h.ns.(h.len);
      let i = ref 0 in
      let continue = ref true in
      while !continue do
        let l = (2 * !i) + 1
        and r = (2 * !i) + 2 in
        let sm = ref !i in
        if l < h.len && h.ks.(l) < h.ks.(!sm) then sm := l;
        if r < h.len && h.ks.(r) < h.ks.(!sm) then sm := r;
        if !sm <> !i
        then (
          heap_swap h !i !sm;
          i := !sm)
        else continue := false
      done);
    Some (key, node))
;;

type 'p t =
  { mutable edges : 'p edge array (* trail log; live prefix is [0, n_edges) *)
  ; mutable n_edges : int
  ; mutable out : int array array (* node -> edge indices, live prefix per node *)
  ; mutable out_len : int array (* live length of each [out.(node)] *)
  ; mutable pi : int array (* feasible potential; length = n_nodes capacity *)
  ; mutable n_nodes : int
  ; mutable frames : int list (* push watermarks: n_edges at each open frame *)
  ; heap : heap
  ; mutable gamma : int array (* consistency scratch: needed pi decrease; 0 = none *)
  ; mutable pred_edge :
      int array (* edge that propagated the decrease, for cycle extract *)
  ; mutable dist : int array (* implied-query reduced distances *)
  ; mutable dist_pred : int array
  ; mutable dist_seen : int array (* epoch-stamped visited marker *)
  ; mutable dist_epoch : int
  }

let create () =
  { edges = [||]
  ; n_edges = 0
  ; out = [||]
  ; out_len = [||]
  ; pi = [||]
  ; n_nodes = 0
  ; frames = []
  ; heap = heap_create ()
  ; gamma = [||]
  ; pred_edge = [||]
  ; dist = [||]
  ; dist_pred = [||]
  ; dist_seen = [||]
  ; dist_epoch = 0
  }
;;

let grow_nodes t need =
  if need > Array.length t.pi
  then (
    let cap = max need (max 16 (2 * Array.length t.pi)) in
    let pi = Array.make cap 0 in
    Array.blit t.pi 0 pi 0 t.n_nodes;
    let out = Array.make cap [||] in
    Array.blit t.out 0 out 0 t.n_nodes;
    let out_len = Array.make cap 0 in
    Array.blit t.out_len 0 out_len 0 t.n_nodes;
    t.pi <- pi;
    t.out <- out;
    t.out_len <- out_len)
;;

let ensure_node t i =
  if i >= t.n_nodes
  then (
    grow_nodes t (i + 1);
    t.n_nodes <- i + 1)
;;

let add_out t node ei =
  let a = t.out.(node) in
  let len = t.out_len.(node) in
  if len >= Array.length a
  then (
    let cap = max 4 (2 * Array.length a) in
    let a' = Array.make cap (-1) in
    Array.blit a 0 a' 0 len;
    t.out.(node) <- a';
    t.out.(node).(len) <- ei)
  else a.(len) <- ei;
  t.out_len.(node) <- len + 1
;;

let push_edge t e =
  if t.n_edges >= Array.length t.edges
  then (
    let cap = max 16 (2 * Array.length t.edges) in
    let a = Array.make cap e in
    Array.blit t.edges 0 a 0 t.n_edges;
    t.edges <- a);
  t.edges.(t.n_edges) <- e;
  let ei = t.n_edges in
  t.n_edges <- ei + 1;
  add_out t e.tl ei;
  ei
;;

let ensure_scratch t =
  if Array.length t.gamma < t.n_nodes
  then (
    let cap = max t.n_nodes 16 in
    t.gamma <- Array.make cap 0;
    t.pred_edge <- Array.make cap (-1))
;;

(* Cotton–Maler: add [hd - tl <= weight] (edge tl->hd). Keep [pi] feasible; a negative
   cycle is detected when the decrease propagates back to [tl]. On conflict the edge is
   NOT added and [pi] is restored (the edge set is unchanged, so [pi] must remain its
   feasible potential).

   [gamma.(v)] is the (negative) amount by which [pi.(v)] must drop; 0 = "not improved".
   [pred_edge.(v)] is the edge that propagated the decrease to [v] (-1 = the added edge). *)
let assert_edge t ~tl ~hd ~weight prem =
  ensure_node t (max tl hd);
  ensure_scratch t;
  let pi = t.pi in
  if pi.(hd) - pi.(tl) <= weight
  then (
    (* Already feasible: add the edge, no potential change. *)
    let (_ : int) = push_edge t { tl; hd; w = weight; prem } in
    None)
  else (
    let g = t.gamma in
    let pe = t.pred_edge in
    heap_clear t.heap;
    let touched = ref [] in
    (* (node, old_pi) for every node whose potential we lowered — restored verbatim on a
       conflict. *)
    let pi_saved = ref [] in
    let set_gamma v value edge =
      if g.(v) = 0 then touched := v :: !touched;
      g.(v) <- value;
      pe.(v) <- edge;
      heap_push t.heap value v
    in
    (* The added edge tl->hd forces pi.(hd) down by [pi.(tl)+weight - pi.(hd)] (<0). *)
    set_gamma hd (pi.(tl) + weight - pi.(hd)) (-1);
    let conflict = ref None in
    let continue = ref true in
    while !continue do
      match heap_pop_min t.heap with
      | None -> continue := false
      | Some (gs, s) ->
        (* Skip stale heap entries (lazy decrease-key). *)
        if gs = g.(s) && g.(s) < 0
        then
          if s = tl
          then (
            (* Decrease propagated back to the source: negative cycle. Follow pred_edge
               from tl back to hd, then close with the added edge (seeded below). *)
            let prems = ref [ prem ] in
            let v = ref tl in
            let guard = ref 0 in
            let stop = ref false in
            while (not !stop) && !guard <= t.n_edges do
              let ei = pe.(!v) in
              if ei < 0
              then stop := true (* reached the added-edge sentinel: cycle closed *)
              else (
                let e = t.edges.(ei) in
                prems := e.prem :: !prems;
                v := e.tl;
                incr guard;
                if !v = hd then stop := true)
            done;
            conflict := Some { premises = !prems };
            continue := false)
          else (
            (* Finalize s: apply the decrease, relax its out-edges. *)
            pi_saved := (s, pi.(s)) :: !pi_saved;
            pi.(s) <- pi.(s) + g.(s);
            g.(s) <- 0 (* finalized; still in touched for the reset below *);
            let a = t.out.(s) in
            for k = 0 to t.out_len.(s) - 1 do
              let ei = a.(k) in
              let e = t.edges.(ei) in
              let y = e.hd in
              (* After lowering pi.(s), edge s->y ([y - s <= c]) may be violated. *)
              let need = pi.(s) + e.w - pi.(y) in
              if need < 0 && need < g.(y) then set_gamma y need ei
            done)
    done;
    (* Reset scratch for every touched node. *)
    List.iter
      (fun v ->
        g.(v) <- 0;
        pe.(v) <- -1)
      !touched;
    (match !conflict with
     | Some _ -> List.iter (fun (v, old) -> pi.(v) <- old) !pi_saved
     | None ->
       let (_ : int) = push_edge t { tl; hd; w = weight; prem } in
       ());
    !conflict)
;;

let ensure_dist t =
  if Array.length t.dist < t.n_nodes
  then (
    let cap = max t.n_nodes 16 in
    t.dist <- Array.make cap 0;
    t.dist_pred <- Array.make cap (-1);
    t.dist_seen <- Array.make cap 0)
;;

(* On-demand entailment: is [hd - tl <= weight] implied? True iff shortest path tl⇝hd has
   length <= weight. Dijkstra on reduced costs (all >= 0 by feasibility of [pi]); the real
   distance is [pi.(tl) - pi.(hd) + reduced_dist]. Returns the path's edge premises.
   Read-only. *)
let implied t ~tl ~hd ~weight =
  if tl >= t.n_nodes || hd >= t.n_nodes
  then None
  else if tl = hd
  then if weight >= 0 then Some [] else None
  else (
    ensure_dist t;
    t.dist_epoch <- t.dist_epoch + 1;
    let ep = t.dist_epoch in
    let d = t.dist
    and pr = t.dist_pred
    and seen = t.dist_seen in
    let pi = t.pi in
    heap_clear t.heap;
    d.(tl) <- 0;
    seen.(tl) <- ep;
    pr.(tl) <- -1;
    heap_push t.heap 0 tl;
    let result = ref None in
    let continue = ref true in
    while !continue do
      match heap_pop_min t.heap with
      | None -> continue := false
      | Some (rd, u) ->
        if seen.(u) = ep && rd = d.(u)
        then
          if u = hd
          then (
            (* Johnson: real dist tl⇝hd = reduced_dist − pi(tl) + pi(hd). *)
            let real = rd - pi.(tl) + pi.(hd) in
            if real <= weight
            then (
              let prems = ref [] in
              let v = ref hd in
              let guard = ref 0 in
              while pr.(!v) >= 0 && !guard <= t.n_edges do
                let e = t.edges.(pr.(!v)) in
                prems := e.prem :: !prems;
                v := e.tl;
                incr guard
              done;
              result := Some !prems);
            continue := false)
          else (
            let a = t.out.(u) in
            for k = 0 to t.out_len.(u) - 1 do
              let ei = a.(k) in
              let e = t.edges.(ei) in
              let y = e.hd in
              let rc = pi.(u) + e.w - pi.(y) in
              (* reduced cost >= 0 *)
              let nd = rd + rc in
              if seen.(y) <> ep || nd < d.(y)
              then (
                d.(y) <- nd;
                pr.(y) <- ei;
                seen.(y) <- ep;
                heap_push t.heap nd y)
            done)
    done;
    !result)
;;

(* ---- trail ---- *)

let truncate_to t target =
  (* Remove edges [target, n_edges) newest-first, popping each from the tail of its
     tl-node's out list (edge [i] was appended last there, removed in reverse i order). *)
  for i = t.n_edges - 1 downto target do
    let e = t.edges.(i) in
    t.out_len.(e.tl) <- t.out_len.(e.tl) - 1
  done;
  t.n_edges <- target
;;

let push t = t.frames <- t.n_edges :: t.frames

let pop t n =
  let rec drop k frames =
    if k = 0
    then frames
    else (
      match frames with
      | wm :: rest ->
        truncate_to t wm;
        drop (k - 1) rest
      | [] -> [])
  in
  t.frames <- drop n t.frames
;;

type checkpoint = int

let checkpoint t = t.n_edges
let rewind_to_checkpoint t c = truncate_to t c
let edge_count t = t.n_edges

(* Reference oracle (test-only): Bellman–Ford with a virtual 0-weight super-source to
   every node. Consistent iff no negative cycle. O(V*E). *)
let consistent_naive t =
  let d = Array.make (max t.n_nodes 1) 0 in
  let changed = ref true in
  let iter = ref 0 in
  let ok = ref true in
  while !changed && !iter <= t.n_nodes do
    changed := false;
    for i = 0 to t.n_edges - 1 do
      let e = t.edges.(i) in
      if d.(e.tl) + e.w < d.(e.hd)
      then (
        d.(e.hd) <- d.(e.tl) + e.w;
        changed := true)
    done;
    incr iter
  done;
  if !changed
  then
    for i = 0 to t.n_edges - 1 do
      let e = t.edges.(i) in
      if d.(e.tl) + e.w < d.(e.hd) then ok := false
    done;
  !ok
;;
