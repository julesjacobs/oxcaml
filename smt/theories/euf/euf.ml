(* Proof-producing congruence closure (Nieuwenhuis-Oliveras). See euf.mli for the
   adapter-facing contract. Engine-independent: built against frozen [core] only.

   Structure (N-O):
   - union-find over e-node ids (union by size, NO path compression), for class membership
     and O(1)-ish equality;
   - an explanation forest, a SEPARATE structure whose edges are the original union edges
     (never rewritten, never compressed), each labelled with its reason — this is what
     makes explanations native (I4);
   - a congruence table (signature -> representative App e-node), keyed on (symbol,
     current argument-class ids), for detecting congruences;
   - a pending merge queue processed to a fixpoint inside each merge.

   All mutation is recorded on a trail so [push]/[pop] undo it level-granularly. E-nodes
   (registration) are monotone within a frame and truncated on [pop] of the frame that
   introduced them; everything derived from asserted (dis)equalities is trailed. *)

open Oxsmt_core

(* --- reasons on explanation-forest edges --------------------------------- *)

type 'p reason =
  | R_none (* forest-tree root: no edge *)
  | R_given of
      'p
      * int
      * int (* an asserted equality: premise token + the two e-nodes it equated *)
  | R_cong of
      int * int (* a congruence: the two App e-nodes made equal (args pairwise equal) *)

(* --- e-nodes ------------------------------------------------------------- *)

type kind =
  | Leaf (* opaque to congruence (Arith/Le/Ite/consts/connectives) *)
  | Fun of Symbol.t * int array (* App: symbol + argument e-node ids *)

type 'p enode =
  { term : Term.t
  ; kind : kind
  ; mutable parent : int (* union-find parent; = self at a root *)
  ; mutable size : int (* class size; valid at a root *)
  ; mutable uses :
      int list (* App e-nodes with this root as a direct arg; valid at a root *)
  ; mutable fparent : int (* explanation-forest parent; = self at a tree root *)
  ; mutable freason : 'p reason (* reason for the edge to [fparent] *)
  ; mutable stamp : int (* scratch marker for NCA; not trailed *)
  }

type watched =
  { w_atom : Term.t (* a watched non-Bool Eq atom *)
  ; w_a : int
  ; w_b : int
  ; mutable w_reported : int
    (* last value propagate reported: -1 unknown, 0 distinct, 1 equal *)
  }

type 'p diseq =
  { d_a : int
  ; d_b : int
  ; d_prem : 'p
  }

(* Signature-table key: (symbol id, argument-class ids). Never iterated -> no
   Hashtbl-order in any observable path (C8); lookup/insert only. *)
module Sig = Hashtbl.Make (struct
    type t = int * int array

    let equal (s1, a1) (s2, a2) =
      s1 = s2
      && Array.length a1 = Array.length a2
      &&
      let ok = ref true in
      Array.iteri (fun i x -> if a2.(i) <> x then ok := false) a1;
      !ok
    ;;

    let hash (s, a) = Array.fold_left (fun h x -> (h * 31) + x) (s * 65599) a
  end)

type 'p undo =
  | U_parent of int * int
  | U_size of int * int
  | U_uses of int * int list
  | U_fedge of int * int * 'p reason
  | U_sig_add of (int * int array)
  | U_sig_del of (int * int array) * int
  | U_reported of int * int

type level =
  { l_trail : int
  ; l_enodes : int
  ; l_watched : int
  ; l_diseqs : int
  ; l_touched : int (* {!t.touched} length at push (restored on pop) *)
  ; l_prop_mark : int (* {!t.prop_mark} at push (restored on pop) *)
  }

type 'p t =
  { ctx : Context.t
  ; enodes : 'p enode Dynarray.t
  ; index : int Term.Table.t (* Term -> e-node id *)
  ; sigtbl : int Sig.t
  ; watched : watched Dynarray.t
  ; diseqs : 'p diseq Dynarray.t
  ; trail : 'p undo Dynarray.t
  ; levels : level Dynarray.t
  ; (* {!propagate} delta log (C1). Every union's surviving root, every asserted
       disequality's endpoints, and every freshly-watched atom's endpoints are appended
       here; {!propagate} re-evaluates only watched atoms touching a class in
       [touched.(prop_mark ..)], then advances [prop_mark]. Truncated to [l_touched] on
       [pop] and [prop_mark] restored to [l_prop_mark], so a union propagated at a deeper
       level and then popped is re-evaluated at the shallower level (the watermark trap).
       A stale (no-longer-root) id in [touched] is harmless: [find] never returns it, so
       it matches no watched endpoint. *)
    touched : int Dynarray.t
  ; mutable prop_mark : int
  ; mutable stamp : int
  }

(* Note a class root as dirty for the next {!propagate} (see [touched]). *)
let mark_touched t root = Dynarray.add_last t.touched root

let create ctx =
  { ctx
  ; enodes = Dynarray.create ()
  ; index = Term.Table.create 256
  ; sigtbl = Sig.create 256
  ; watched = Dynarray.create ()
  ; diseqs = Dynarray.create ()
  ; trail = Dynarray.create ()
  ; levels = Dynarray.create ()
  ; touched = Dynarray.create ()
  ; prop_mark = 0
  ; stamp = 0
  }
;;

let num_terms t = Dynarray.length t.enodes
let context t = t.ctx

(* --- independent naive union-find (self-check oracle, DESIGN §7) ---------- *)

module Naive = struct
  let create () : (int, int) Hashtbl.t = Hashtbl.create 64

  let rec find h i =
    match Hashtbl.find_opt h i with
    | None -> i
    | Some p -> if p = i then i else find h p
  ;;

  let union h a b =
    let ra = find h a
    and rb = find h b in
    if ra <> rb then Hashtbl.replace h ra rb
  ;;

  let equal h a b = find h a = find h b
end

(* --- basic accessors ----------------------------------------------------- *)

let get t i = Dynarray.get t.enodes i

let rec find t i =
  let n = get t i in
  if n.parent = i then i else find t n.parent
;;

let dedup_int lst =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun x ->
       if Hashtbl.mem seen x
       then false
       else (
         Hashtbl.add seen x ();
         true))
    lst
;;

(* --- trailed mutation ---------------------------------------------------- *)

let push_undo t u = Dynarray.add_last t.trail u

let set_parent t i v =
  let n = get t i in
  push_undo t (U_parent (i, n.parent));
  n.parent <- v
;;

let set_size t i v =
  let n = get t i in
  push_undo t (U_size (i, n.size));
  n.size <- v
;;

let set_uses t i v =
  let n = get t i in
  push_undo t (U_uses (i, n.uses));
  n.uses <- v
;;

let set_fedge t i p r =
  let n = get t i in
  push_undo t (U_fedge (i, n.fparent, n.freason));
  n.fparent <- p;
  n.freason <- r
;;

let set_reported t idx v =
  let w = Dynarray.get t.watched idx in
  push_undo t (U_reported (idx, w.w_reported));
  w.w_reported <- v
;;

let sig_add t key v =
  Sig.replace t.sigtbl key v;
  push_undo t (U_sig_add key)
;;

let sig_del t key =
  match Sig.find_opt t.sigtbl key with
  | Some v ->
    Sig.remove t.sigtbl key;
    push_undo t (U_sig_del (key, v))
  | None -> ()
;;

let apply_undo t = function
  | U_parent (i, old) -> (get t i).parent <- old
  | U_size (i, old) -> (get t i).size <- old
  | U_uses (i, old) -> (get t i).uses <- old
  | U_fedge (i, op, orr) ->
    let n = get t i in
    n.fparent <- op;
    n.freason <- orr
  | U_sig_add key -> Sig.remove t.sigtbl key
  | U_sig_del (key, v) -> Sig.replace t.sigtbl key v
  | U_reported (idx, old) -> (Dynarray.get t.watched idx).w_reported <- old
;;

(* --- congruence signatures ----------------------------------------------- *)

let sig_key t id =
  match (get t id).kind with
  | Fun (sym, args) -> (sym :> int), Array.map (fun a -> find t a) args
  | Leaf -> invalid_arg "Euf: sig_key on a non-App e-node"
;;

let add_use t root id =
  let n = get t root in
  set_uses t root (id :: n.uses)
;;

(* Remove [p]'s current-signature table entry, but only if it is the representative
   (identity by e-node id) — a congruence-merged non-representative has no entry. *)
let sig_remove_if t p =
  let key = sig_key t p in
  match Sig.find_opt t.sigtbl key with
  | Some v when v = p -> sig_del t key
  | _ -> ()
;;

(* --- explanation forest -------------------------------------------------- *)

(* Reverse the path from [a] to its forest root so [a] becomes the root, preserving each
   edge's reason (an edge is undirected; only its orientation flips). Every fparent/
   freason change is trailed. *)
let reroot t a =
  let rec go cur newp newr =
    let n = get t cur in
    let op = n.fparent
    and orr = n.freason in
    set_fedge t cur newp newr;
    if op <> cur then go op cur orr
  in
  go a a R_none
;;

let add_forest_edge t a b reason =
  reroot t a;
  set_fedge t a b reason
;;

(* --- merge + congruence closure (pending queue to fixpoint) -------------- *)

let merge t a0 b0 reason0 =
  let q = Queue.create () in
  Queue.add (a0, b0, reason0) q;
  while not (Queue.is_empty q) do
    let a, b, reason = Queue.pop q in
    let ra = find t a
    and rb = find t b in
    if ra <> rb
    then (
      let sa = (get t ra).size
      and sb = (get t rb).size in
      let child, root = if sa <= sb then ra, rb else rb, ra in
      (* the surviving root is the only class whose membership changed — dirty it for the
         incremental {!propagate} (a watched atom's status can only flip because one of
         its endpoints now finds to [root]; see [touched]). *)
      mark_touched t root;
      (* forest edge between the ORIGINAL endpoints, carrying [reason] *)
      add_forest_edge t a b reason;
      let parents = dedup_int (get t child).uses in
      (* remove parents from the table under their pre-union signatures *)
      List.iter (fun p -> sig_remove_if t p) parents;
      (* union child under root *)
      set_size t root ((get t root).size + (get t child).size);
      set_uses t root ((get t child).uses @ (get t root).uses);
      set_parent t child root;
      (* recompute parent signatures; schedule congruences *)
      List.iter
        (fun p ->
           let key = sig_key t p in
           match Sig.find_opt t.sigtbl key with
           | Some qq when find t qq <> find t p -> Queue.add (p, qq, R_cong (p, qq)) q
           | Some _ -> ()
           | None -> sig_add t key p)
        parents)
  done
;;

let insert_congruence t id =
  let key = sig_key t id in
  match Sig.find_opt t.sigtbl key with
  | Some qq when find t qq <> find t id -> merge t id qq (R_cong (id, qq))
  | Some _ -> ()
  | None -> sig_add t key id
;;

(* --- registration (CONTRACT-REG) ----------------------------------------- *)

let children (term : Term.t) : Term.t list =
  match term.node with
  | Bool_const _ | Int_const _ -> []
  | App (_, args) -> Iarr.to_list args
  | Arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
  | Le a -> [ a ]
  | Eq (a, b) -> [ a; b ]
  | Not a -> [ a ]
  | And a | Or a -> Iarr.to_list a
  | Ite (c, a, b) -> [ c; a; b ]
;;

let rec register t (term : Term.t) : int =
  match Term.Table.find_opt t.index term with
  | Some i -> i
  | None ->
    (* post-order: children get smaller ids *)
    let kind =
      match term.node with
      | App (sym, args) ->
        let ids = List.map (fun a -> register t a) (Iarr.to_list args) in
        Fun (sym, Array.of_list ids)
      | _ ->
        List.iter (fun c -> ignore (register t c : int)) (children term);
        Leaf
    in
    let id = Dynarray.length t.enodes in
    Dynarray.add_last
      t.enodes
      { term
      ; kind
      ; parent = id
      ; size = 1
      ; uses = []
      ; fparent = id
      ; freason = R_none
      ; stamp = 0
      };
    Term.Table.replace t.index term id;
    (match kind with
     | Fun (_, args) ->
       let roots = dedup_int (Array.to_list (Array.map (fun a -> find t a) args)) in
       List.iter (fun r -> add_use t r id) roots;
       insert_congruence t id
     | Leaf -> ());
    (match term.node with
     | Eq (a, b) when not (Sort.equal a.sort Sort.bool) ->
       let ia = register t a
       and ib = register t b in
       Dynarray.add_last t.watched { w_atom = term; w_a = ia; w_b = ib; w_reported = -1 };
       (* a freshly-watched atom must be evaluated by the next {!propagate} even if no
          merge follows (its sides may already be (dis)equal) — dirty its endpoints. *)
       mark_touched t (find t ia);
       mark_touched t (find t ib)
     | _ -> ());
    id
;;

let register_term t term = ignore (register t term : int)

let assert_eq t ~premise a b =
  let ia = register t a
  and ib = register t b in
  merge t ia ib (R_given (premise, ia, ib))
;;

let assert_neq t ~premise a b =
  let ia = register t a
  and ib = register t b in
  Dynarray.add_last t.diseqs { d_a = ia; d_b = ib; d_prem = premise };
  (* a new disequality can newly-separate a watched pair whose classes match its
     endpoints' — dirty both endpoint classes so {!propagate} re-checks them. *)
  mark_touched t (find t ia);
  mark_touched t (find t ib)
;;

(* --- explanation --------------------------------------------------------- *)

(* Nearest common ancestor of [x] and [y] in the forest (same tree: same class). *)
let nca t x y =
  t.stamp <- t.stamp + 1;
  let s = t.stamp in
  let cur = ref x
  and go = ref true in
  while !go do
    let n = get t !cur in
    n.stamp <- s;
    if n.fparent = !cur then go := false else cur := n.fparent
  done;
  let cur = ref y in
  while (get t !cur).stamp <> s do
    cur := (get t !cur).fparent
  done;
  !cur
;;

(* Explanation as a list of used given-edges: (forest-child id, premise, u, v). Each given
   edge is identified by its forest child, so dedup is by that id. Congruence edges expand
   to their arguments' equalities (pushed back onto [pending]). *)
let explain_core t a b =
  let out = ref [] in
  let out_seen = Hashtbl.create 32 in
  let explained = Hashtbl.create 32 in
  let pending = Queue.create () in
  Queue.add (a, b) pending;
  while not (Queue.is_empty pending) do
    let x, y = Queue.pop pending in
    if x <> y
    then (
      let key = if x < y then x, y else y, x in
      if not (Hashtbl.mem explained key)
      then (
        Hashtbl.add explained key ();
        let c = nca t x y in
        let walk start =
          let cur = ref start in
          while !cur <> c do
            let n = get t !cur in
            let child = !cur in
            (match n.freason with
             | R_given (prem, u, v) ->
               if not (Hashtbl.mem out_seen child)
               then (
                 Hashtbl.add out_seen child ();
                 out := (child, prem, u, v) :: !out)
             | R_cong (f, g) ->
               (match (get t f).kind, (get t g).kind with
                | Fun (_, af), Fun (_, ag) ->
                  Array.iteri (fun i fa -> Queue.add (fa, ag.(i)) pending) af
                | _ -> ())
             | R_none -> ());
            cur := n.fparent
          done
        in
        walk x;
        walk y))
  done;
  List.sort (fun (c1, _, _, _) (c2, _, _, _) -> compare (c1 : int) c2) !out
;;

(* Independent-replay self-check (below): a from-scratch O(n²)–O(n³) [naive_closure] run
   on EVERY conflict/explain. Pure debug machinery — production trusts the main
   union-find/forest/congruence path — so it defaults OFF and is opt-in via
   [OXSMT_EUF_SELF_CHECK] (set in the test Makefile targets; unset for the corpus/CLI
   binary). Left on, it is a latent O(n²) cliff on any conflict-heavy large-n instance
   (perf analysis, quick-win aside). Still a public [bool ref] so a caller can force it. *)
let self_check =
  ref
    (match Sys.getenv_opt "OXSMT_EUF_SELF_CHECK" with
     | Some ("0" | "false" | "no" | "") | None -> false
     | Some _ -> true)
;;

(* Independent replay oracle (DESIGN §7): a from-scratch naive congruence closure over ALL
   e-nodes, seeded with ONLY the given-equality endpoints from an explanation, then
   saturated by the brute-force O(n^2) congruence rule. If the premise subset alone
   entails the queried (dis)equality under EUF, [a] and [b] end up connected here. This
   shares no code with the main union-find / forest / congruence table. *)
let naive_closure t edges =
  let h = Naive.create () in
  List.iter (fun (_, _, u, v) -> Naive.union h u v) edges;
  let n = Dynarray.length t.enodes in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        match (get t i).kind, (get t j).kind with
        | Fun (si, ai), Fun (sj, aj)
          when (si :> int) = (sj :> int)
               && Array.length ai = Array.length aj
               && not (Naive.equal h i j) ->
          let all = ref true in
          Array.iteri (fun k a -> if not (Naive.equal h a aj.(k)) then all := false) ai;
          if !all
          then (
            Naive.union h i j;
            changed := true)
        | _ -> ()
      done
    done
  done;
  h
;;

let check_explains_eq t edges a b msg =
  if !self_check && not (Naive.equal (naive_closure t edges) a b) then failwith msg
;;

let premises edges = List.map (fun (_, p, _, _) -> p) edges

let explain t a b =
  let ia = register t a
  and ib = register t b in
  if find t ia <> find t ib then invalid_arg "Euf.explain: terms are not equal";
  let edges = explain_core t ia ib in
  check_explains_eq t edges ia ib "Euf self-check: explanation does not entail a=b";
  premises edges
;;

(* --- conflict detection -------------------------------------------------- *)

type 'p check_result =
  | Consistent
  | Conflict of 'p list

(* Scan disequalities in assertion order; return the first violated one (C3). *)
let check t =
  let result = ref Consistent in
  (try
     Dynarray.iteri
       (fun _ d ->
          if find t d.d_a = find t d.d_b
          then (
            let edges = explain_core t d.d_a d.d_b in
            if !self_check && not (Naive.equal (naive_closure t edges) d.d_a d.d_b)
            then
              failwith
                "Euf self-check: conflict explanation does not connect the disequal terms";
            result := Conflict (premises edges @ [ d.d_prem ]);
            raise Exit))
       t.diseqs
   with
   | Exit -> ());
  !result
;;

(* --- disequality propagation + Nelson-Oppen sharing ---------------------- *)

(* An asserted disequality separating the classes of [a] and [b], if any (C: fixed
   assertion-order scan). Returned so [explain_implied] can cite it and its endpoints. *)
let distinct_witness t a b =
  let ra = find t a
  and rb = find t b in
  let w = ref None in
  (try
     Dynarray.iter
       (fun d ->
          let du = find t d.d_a
          and dv = find t d.d_b in
          if (du = ra && dv = rb) || (du = rb && dv = ra)
          then (
            w := Some d;
            raise Exit))
       t.diseqs
   with
   | Exit -> ());
  !w
;;

type implied =
  { atom : Term.t
  ; value : bool
  }

(* Incremental delta-driven propagation (C1). A watched atom's entailed truth can flip
   only if one of its endpoints now belongs to a class touched since the last [propagate]:
   a merge (the surviving root), a new disequality (its endpoints), or the atom's own
   fresh registration (its endpoints) — all recorded in [touched]. So we build the dirty
   root set from [touched.(prop_mark ..)] and re-evaluate ONLY watched atoms with a dirty
   endpoint, skipping the O(#diseqs) [distinct_witness] rescan for the unchanged majority.
   The output is IDENTICAL to a full rescan: a skipped atom's status is provably unchanged
   since it was last reported (its [w_reported] already matches), so a full rescan would
   report nothing for it either. Iteration stays in watched-index (registration) order, so
   the reported list is byte-identical run to run. Empty dirty set ⇒ no work. *)
let propagate t =
  let dirty = Hashtbl.create 64 in
  for i = t.prop_mark to Dynarray.length t.touched - 1 do
    Hashtbl.replace dirty (Dynarray.get t.touched i) ()
  done;
  t.prop_mark <- Dynarray.length t.touched;
  let acc = ref [] in
  if Hashtbl.length dirty > 0
  then (
    (* Separated-class index, built ONCE per [propagate] call. No merge happens inside a
       [propagate], so every representative is stable here: the unordered root-pair
       [{find d_a, find d_b}] of each asserted disequality is fixed for the whole call. So
       instead of re-running the O(#diseqs) [distinct_witness] scan for every dirty
       watched atom (the quadratic that dominated QG: ~252 dirty atoms × up to ~600 diseqs
       × 2 finds each, per call), we scan the diseqs once into a hash set of normalized
       (lo,hi) root pairs and test membership in O(1) per watched atom. Equivalent
       predicate: [distinct_witness a b <> None] iff the unordered pair [{find a, find b}]
       equals some diseq's separated pair — exactly membership of its normalized key.
       [propagate] only needs this boolean; the citable witness is still produced by
       [distinct_witness] via [explain_implied], which the adapter calls at propagation
       time to snapshot each propagation's reason (#102 CONTRACT-EX), so the witness path
       is unchanged. The set is membership-tested only (never iterated), so it introduces
       no Hashtbl-order into any observable path (C8); watched iteration stays in
       registration/index order, so the reported list is byte-identical to the old
       full-scan output.

       Error asymmetry (soundness): a FALSE POSITIVE in this set (a spurious separated
       pair, or a stale/pre-merge rep that coincides with a watched pair's roots) makes
       [propagate] report a watched Eq FALSE that is not actually entailed distinct — a
       wrong theory propagation, the wrong-verdict direction. A FALSE NEGATIVE (a real
       separated pair missing) only drops a distinct-propagation — a completeness loss,
       not a soundness one. We do not lean on the downstream lazy [explain_implied] guard
       to catch a spurious propagation; the [test_propagate_pushpop_vs_full] oracle checks
       byte-identical output against an independent full scan, forbidding BOTH directions
       (mutants [euf_propagate_sep_stale_reps] / [euf_propagate_sep_skip_rebuild]). *)
    let sep = Hashtbl.create (Dynarray.length t.diseqs) in
    Dynarray.iter
      (fun d ->
         let du = find t d.d_a
         and dv = find t d.d_b in
         let key = if du <= dv then du, dv else dv, du in
         Hashtbl.replace sep key ())
      t.diseqs;
    Dynarray.iteri
      (fun idx w ->
         let ra = find t w.w_a
         and rb = find t w.w_b in
         if Hashtbl.mem dirty ra || Hashtbl.mem dirty rb
         then (
           let cur =
             if ra = rb
             then 1
             else (
               let key = if ra <= rb then ra, rb else rb, ra in
               if Hashtbl.mem sep key then 0 else -1)
           in
           if cur <> -1 && cur <> w.w_reported
           then (
             set_reported t idx cur;
             acc := { atom = w.w_atom; value = cur = 1 } :: !acc)))
      t.watched);
  List.rev !acc
;;

let eq_sides (atom : Term.t) =
  match atom.node with
  | Eq (a, b) -> a, b
  | _ -> invalid_arg "Euf.explain_implied: atom is not an equality"
;;

let explain_implied t imp =
  let a, b = eq_sides imp.atom in
  let ia = register t a
  and ib = register t b in
  if imp.value
  then (
    if find t ia <> find t ib then invalid_arg "Euf.explain_implied: not equal";
    let edges = explain_core t ia ib in
    check_explains_eq t edges ia ib "Euf self-check: implied-equal explanation invalid";
    premises edges)
  else (
    match distinct_witness t ia ib with
    | None -> invalid_arg "Euf.explain_implied: not provably distinct"
    | Some d ->
      let ra = find t ia in
      (* orient the witness so [ia ~ e1] and [ib ~ e2] *)
      let e1, e2 = if find t d.d_a = ra then d.d_a, d.d_b else d.d_b, d.d_a in
      let edges1 = explain_core t ia e1 in
      let edges2 = explain_core t ib e2 in
      if !self_check
      then (
        let h = naive_closure t (edges1 @ edges2) in
        if not (Naive.equal h ia e1 && Naive.equal h ib e2)
        then failwith "Euf self-check: implied-distinct explanation invalid");
      premises edges1 @ premises edges2 @ [ d.d_prem ])
;;

(* --- queries ------------------------------------------------------------- *)

let are_equal t a b = find t (register t a) = find t (register t b)
let class_of t term = find t (register t term)

(* --- read-only query API (ADR-0012 L2 / R6, tranche 2) ------------------- *)
(* These four accessors are GENUINELY NON-REGISTERING: they never call [register] and
   never mutate the union-find / forest / congruence table ([find] has no path
   compression, so it is a pure traversal). The E-matcher reads the e-graph through them;
   the failure-direction table (ADR-0012 §3) requires the matcher cannot perturb the
   e-graph. Iteration order is e-node id (= registration order), never
   [Hashtbl]/[Term.Table] traversal (C8, I6). *)

(* Registered ground [App] e-nodes whose head is [sym], in registration (id) order.
   Trigger root candidates for the matcher. A non-representative (congruence-merged) node
   is still returned — its own [term] is a legitimate ground term with head [sym]. *)
let app_terms_by_symbol t sym =
  let out = ref [] in
  for i = Dynarray.length t.enodes - 1 downto 0 do
    match (get t i).kind with
    | Fun (s, _) when Symbol.equal s sym -> out := (get t i).term :: !out
    | Fun _ | Leaf -> ()
  done;
  !out
;;

(* The class root of [term] iff it is already registered, else [None]. No registration. *)
let find_class_opt t term =
  match Term.Table.find_opt t.index term with
  | Some id -> Some (find t id)
  | None -> None
;;

(* Congruence-equality check that treats an UNREGISTERED term as its own singleton class
   (tag-equality only): both registered => same root; else fall back to [Term.equal] (O(1)
   hash-cons tag equality). Never registers, never mutates. *)
let equal_if_registered t a b =
  match Term.Table.find_opt t.index a, Term.Table.find_opt t.index b with
  | Some ia, Some ib -> find t ia = find t ib
  | _ -> Term.equal a b
;;

(* Members of [term]'s congruence class (id order), for matching modulo EUF-congruence
   equalities. An unregistered term is a singleton class [ [term] ]. No registration. *)
let class_members t term =
  match Term.Table.find_opt t.index term with
  | None -> [ term ]
  | Some id ->
    let root = find t id in
    let out = ref [] in
    for i = Dynarray.length t.enodes - 1 downto 0 do
      if find t i = root then out := (get t i).term :: !out
    done;
    !out
;;

(* --- backtracking -------------------------------------------------------- *)

let push t =
  Dynarray.add_last
    t.levels
    { l_trail = Dynarray.length t.trail
    ; l_enodes = Dynarray.length t.enodes
    ; l_watched = Dynarray.length t.watched
    ; l_diseqs = Dynarray.length t.diseqs
    ; l_touched = Dynarray.length t.touched
    ; l_prop_mark = t.prop_mark
    }
;;

let pop t n =
  if n < 0 then invalid_arg "Euf.pop: negative";
  if n > Dynarray.length t.levels then invalid_arg "Euf.pop: too many frames";
  if n > 0
  then (
    let target = Dynarray.length t.levels - n in
    let lv = Dynarray.get t.levels target in
    while Dynarray.length t.trail > lv.l_trail do
      apply_undo t (Dynarray.pop_last t.trail)
    done;
    Dynarray.truncate t.diseqs lv.l_diseqs;
    Dynarray.truncate t.watched lv.l_watched;
    (* restore the propagate delta log to its push-time snapshot: drop touched-roots
       logged in the popped frames and rewind [prop_mark], so a union that was propagated
       at a deeper level (its [set_reported] now undone by the trail) is re-evaluated
       here. *)
    Dynarray.truncate t.touched lv.l_touched;
    t.prop_mark <- lv.l_prop_mark;
    for i = Dynarray.length t.enodes - 1 downto lv.l_enodes do
      Term.Table.remove t.index (get t i).term
    done;
    Dynarray.truncate t.enodes lv.l_enodes;
    Dynarray.truncate t.levels target)
;;

module Debug = struct
  let self_check = self_check
end
