(* The weak-equivalence graph (Christ & Hoenicke 2015), the dark W0 substrate for the
   QF_AX weak-equivalence decision procedure (ADR-weakeq / DESIGN.md A12). See
   weq_graph.mli for the contract and the soundness argument.

   W0 emits NOTHING: this module only MAINTAINS the graph and answers term-level path
   queries; the L1/L2 lemma generators that consume the paths arrive at W1/W2. It is
   written against an abstract {!egraph_view} (O6/OQ4) so nothing here calls
   {!Oxsmt_euf.Euf} directly and the W3 fabric migration is a re-binding of the view, not
   a rewrite. *)

open Oxsmt_core

type egraph_view =
  { class_of : Term.t -> int
  ; are_equal : Term.t -> Term.t -> bool
  ; explain_equal : Term.t -> Term.t -> Lit.t list
  }

type edge =
  | Store of
      { u : Term.t
      ; v : Term.t
      ; store_term : Term.t
      ; base : Term.t
      ; index : Term.t
      }
  | Equality of
      { u : Term.t
      ; v : Term.t
      }

(* One neighbour record in the undirected adjacency: the [edge] carries its own oriented
   [u]/[v]; [neighbour] is the term reached from the keyed term. Records are compared by
   physical identity so a trailed equality-edge undo removes exactly the two records it
   inserted, regardless of any store-edge record prepended to the same bucket afterwards
   (store edges are untrailed and monotonic, so head-popping would be fragile). *)
type entry =
  { neighbour : Term.t
  ; edge : edge
  }

type t =
  { view : egraph_view
  ; adj : (int, entry list) Hashtbl.t
    (* term tag -> adjacency. Store-edge entries are permanent (never removed — a store
         term is weakly equivalent to its base in every state, and [store_terms] is
         monotonic). Equality-edge entries are folded from the merge stream and removed by
         {!pop} via [trail]. *)
  ; store_seen : (int, unit) Hashtbl.t (* store term tag -> unit; dedup add_store_edge *)
  ; eq_seen : (int * int, unit) Hashtbl.t
    (* orientation-normalized (min tag, max tag) equality-edge key -> unit; dedup
         on_merge. A structural pair key, not a packed int: a Cantor-style pack overflows
         for large tags and a collision would silently drop a distinct equality edge (W1
         obligation 3). Trailed alongside the adjacency entries so a popped edge can be
         re-added after further merges. *)
  ; trail : (unit -> unit, unit) Trail.t
  }

let create view =
  { view
  ; adj = Hashtbl.create 256
  ; store_seen = Hashtbl.create 128
  ; eq_seen = Hashtbl.create 128
  ; trail = Trail.create ()
  }
;;

let index_of_array_sort (s : Sort.t) : Sort.t option =
  match s with
  | Sort.Array (index, _element) -> Some index
  | Sort.Bool | Sort.Int _ | Sort.Real | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.BitVec _ ->
    None
;;

let index_sort_stably_infinite (s : Sort.t) : bool =
  match s with
  | Sort.Int _ -> true (* mathematical integers: the whole of ℤ, infinite *)
  | Sort.Real -> true (* mathematical reals are infinite *)
  | Sort.Uninterpreted _ ->
    true (* uninterpreted sorts are stably infinite by convention *)
  | Sort.Bool -> false (* a finite two-element domain *)
  | Sort.BitVec _ -> false (* finite: 2^width elements *)
  | Sort.Datatype _ -> false (* may be a finite enumeration; conservatively excluded *)
  | Sort.Array _ -> false (* an array-sorted index is exotic; conservatively excluded *)
;;

(* Whether the rules may fire over an array of this sort (O9). Gated in the substrate: a
   finite index sort gets NO graph tracking, so no path is ever found over it and the
   W1/W2 rules never fire — the model-value validator's finite-default assumption is never
   relied on for a sort whose index domain can be exhausted (a wrong-SAT vector). *)
let array_sort_admissible (s : Sort.t) : bool =
  match index_of_array_sort s with
  | Some index -> index_sort_stably_infinite index
  | None -> false
;;

let add_entry t (key : Term.t) (e : entry) =
  let prev = Option.value (Hashtbl.find_opt t.adj key.Term.tag) ~default:[] in
  Hashtbl.replace t.adj key.Term.tag (e :: prev)
;;

let remove_entry t (key : Term.t) (e : entry) =
  match Hashtbl.find_opt t.adj key.Term.tag with
  | None -> ()
  | Some es -> Hashtbl.replace t.adj key.Term.tag (List.filter (fun x -> x != e) es)
;;

let add_store_edge t ~store_term ~base ~index =
  if
    array_sort_admissible store_term.Term.sort
    && not (Hashtbl.mem t.store_seen store_term.Term.tag)
  then (
    Hashtbl.replace t.store_seen store_term.Term.tag ();
    let e_fwd =
      { neighbour = base
      ; edge = Store { u = store_term; v = base; store_term; base; index }
      }
    in
    let e_bwd =
      { neighbour = store_term
      ; edge = Store { u = base; v = store_term; store_term; base; index }
      }
    in
    (* store edges are PERMANENT (untrailed): valid in every state, [store_terms]
       monotonic *)
    add_entry t store_term e_fwd;
    add_entry t base e_bwd)
;;

let eq_key (a : Term.t) (b : Term.t) : int * int =
  (* orientation-normalized (min tag, max tag); a structural pair, exact for all tags *)
  let x = a.Term.tag
  and y = b.Term.tag in
  if x <= y then x, y else y, x
;;

let on_merge t (a : Term.t) (b : Term.t) =
  if array_sort_admissible a.Term.sort && array_sort_admissible b.Term.sort
  then (
    let key = eq_key a b in
    if not (Hashtbl.mem t.eq_seen key)
    then (
      Hashtbl.replace t.eq_seen key ();
      let e_fwd = { neighbour = b; edge = Equality { u = a; v = b } } in
      let e_bwd = { neighbour = a; edge = Equality { u = b; v = a } } in
      add_entry t a e_fwd;
      add_entry t b e_bwd;
      (* trailed: undo removes exactly these three insertions on pop of this frame *)
      Trail.record t.trail (fun () ->
        remove_entry t a e_fwd;
        remove_entry t b e_bwd;
        Hashtbl.remove t.eq_seen key)))
;;

let push t = Trail.push t.trail ()
let pop t n = Trail.pop t.trail ~apply:(fun f -> f ()) n

(* Deterministic BFS over the maintained adjacency (store edges + folded equality edges).
   Neighbours are visited in ascending term-tag order so the discovered path is a
   deterministic function of the assertion sequence (I6). The maintained equality
   adjacency is a spanning forest of the equality relation by construction —
   {!Oxsmt_euf.Euf} reports only class-CONNECTING merges (redundant merges are skipped,
   CONTRACT-EX), so there are no redundant equality edges — and store edges add one edge
   per store term; BFS therefore yields a canonical path per query with lemma count
   bounded by the read/diseq population (OQ1). *)
let neighbours t (x : Term.t) : entry list =
  match Hashtbl.find_opt t.adj x.Term.tag with
  | None -> []
  | Some es -> List.sort (fun p q -> compare p.neighbour.Term.tag q.neighbour.Term.tag) es
;;

let find_path t (a : Term.t) (b : Term.t) : edge list option =
  (* QUERY-SIDE O9 guard (W1 obligation 2): reject an inadmissible (finite/non-stably-
     infinite index) array up front, BEFORE the reflexive [a = a -> Some []] shortcut —
     otherwise a rule could fire over a finite-index array via a zero-length path, since
     the substrate's other O9 gates (add_store_edge/on_merge) never see a bare query. A
     rule that asks for a path is thereby prevented from firing over any array the rules
     must not touch, reflexive case included. *)
  if not (array_sort_admissible a.Term.sort && array_sort_admissible b.Term.sort)
  then None
  else if Term.equal a b
  then Some []
  else (
    let pred : (int, Term.t * edge) Hashtbl.t = Hashtbl.create 64 in
    let visited : (int, unit) Hashtbl.t = Hashtbl.create 64 in
    Hashtbl.replace visited a.Term.tag ();
    let queue = Queue.create () in
    Queue.add a queue;
    let found = ref false in
    while (not !found) && not (Queue.is_empty queue) do
      let x = Queue.pop queue in
      List.iter
        (fun e ->
           if (not !found) && not (Hashtbl.mem visited e.neighbour.Term.tag)
           then (
             Hashtbl.replace visited e.neighbour.Term.tag ();
             Hashtbl.replace pred e.neighbour.Term.tag (x, e.edge);
             if Term.equal e.neighbour b
             then found := true
             else Queue.add e.neighbour queue))
        (neighbours t x)
    done;
    if not !found
    then None
    else (
      (* reconstruct b -> a, then reverse to a -> b *)
      let rec walk (node : Term.t) acc =
        if Term.equal node a
        then acc
        else (
          match Hashtbl.find_opt pred node.Term.tag with
          | None -> acc (* unreachable; defensive *)
          | Some (prev, edge) -> walk prev (edge :: acc))
      in
      Some (walk b [])))
;;

let weakly_equivalent t (a : Term.t) (b : Term.t) : bool =
  match find_path t a b with
  | Some _ -> true
  | None -> false
;;

let self_check t (array_terms : Term.t list) : unit =
  (* Invariant: any two array terms in the same e-class (the view's ground truth) are
     weakly equivalent in the maintained graph — i.e. the folded equality edges span each
     class. A gap here is INCOMPLETENESS (a missed lemma), never unsoundness, but it means
     the merge stream was mis-folded, so the self-check is fail-loud in test/CI. Only
     admissible (stably-infinite-index) array terms participate; a finite-index array is
     untracked by design (O9) and its class membership is not a graph obligation.
     Quadratic in the sample; the caller passes a bounded sample. *)
  let admissible = List.filter (fun x -> array_sort_admissible x.Term.sort) array_terms in
  let rec pairs = function
    | [] | [ _ ] -> ()
    | x :: rest ->
      List.iter
        (fun y ->
           if t.view.are_equal x y && not (weakly_equivalent t x y)
           then
             failwith
               "Weq_graph.self_check: two same-e-class array terms are not weakly \
                equivalent in the graph (merge stream mis-folded) [tripwire]")
        rest;
      pairs rest
  in
  pairs admissible
;;
