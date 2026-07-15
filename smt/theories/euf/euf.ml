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

(* ADR-0014 Stage 2/3 merge notification (defined in {!Fabric} so the combinator can read
   it without depending on this engine). Re-exported here as the engine's own
   [merge_event]. *)
type merge_event = Fabric.merge_event =
  { kept : Term.t
  ; merged : Term.t
  ; kept_tag : Term.t option
  ; merged_tag : Term.t option
  }

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
    (* The union-find PARENT lives in the flat [t.parents] int array, not here — a
         per-op hot read that a boxed record field made a pointer chase (see [find]).
         Every other per-node field stays inline. *)
  ; mutable size : int (* class size; valid at a root *)
  ; mutable uses :
      int list (* App e-nodes with this root as a direct arg; valid at a root *)
  ; mutable fparent : int (* explanation-forest parent; = self at a tree root *)
  ; mutable freason : 'p reason (* reason for the edge to [fparent] *)
  ; mutable stamp : int (* scratch marker for NCA; not trailed *)
  ; mutable tag : Term.t option
    (* ADR-0014 Stage 3 (datatypes-scoped): per-class theory data — a witness [Term.t] a
     client attaches to the class (datatypes: the representative constructor application
     [C(a..)] of the class). Valid at a ROOT only; trailed; the surviving root inherits a
     tag on merge if it had none. Two tagged classes merging is surfaced via the merge log
     (both tags) for the client to resolve. *)
  }

type watched =
  { w_atom : Term.t
    (* a watched atom: either a non-Bool [Eq(a,b)] (truth = [a ~ b]) or a Bool-codomain
         predicate application [p(x…)] (truth = [p(x…) ~ true_const]). *)
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

(* Int-keyed set for the per-call separated-root-pair table (#125). The two class-rep ids
   of a disequality are packed into one [int] key (see [propagate]), so the hot membership
   test uses an identity [int] hash — matching the [Symbol]/[Term] table idiom — instead
   of the polymorphic [caml_hash] over a boxed [(int * int)] tuple (~11% of QG wall).
   Never iterated -> no Hashtbl-order in any observable path (C8); lookup/insert only. *)
module Int_set = Hashtbl.Make (struct
    type t = int

    let equal (a : int) b = a = b
    let hash (x : int) = x
  end)

type 'p undo =
  | U_parent of int * int
  | U_size of int * int
  | U_uses of int * int list
  | U_fedge of int * int * 'p reason
  | U_sig_add of (int * int array)
  | U_sig_del of (int * int array) * int
  | U_psig_add of int (* packed-signature key added to [packtbl] (task #47) *)
  | U_psig_del of int * int (* packed key + prior value restored to [packtbl] *)
  | U_reported of int * int
  | U_tag of int * Term.t option (* ADR-0014 Stage 3: restore a root's per-class tag *)

(* Per-frame watermarks for EUF's auxiliary arrays — the [pop] target lengths for state
   NOT reversed by the typed undo trail (which the shared substrate drains). The trail
   watermark itself lives in the substrate's frame stack (ADR-0014 Stage 0: EUF shares
   only the frame stack + drain loop, keeping its int-packed entry representation). This
   whole record is one frame's payload, so all five watermarks push/pop atomically with
   the trail watermark through a single stack — no separate level array to keep in
   lockstep. *)
type level =
  { l_enodes : int
  ; l_watched : int
  ; l_diseqs : int
  ; l_touched : int (* {!t.touched} length at push (restored on pop) *)
  ; l_prop_mark : int (* {!t.prop_mark} at push (restored on pop) *)
  }

type 'p t =
  { ctx : Context.t
  ; enodes : 'p enode Dynarray.t
  ; (* Union-find parent, one slot per e-node id, kept OUT of the [enode] record: [find] is
       the solver's hottest read (~22% of QG wall) and a flat unboxed [int array] read with
       [Array.unsafe_get] is far cheaper than a bounds-checked [Dynarray.get] into a boxed
       record. Grown (doubling) in lockstep with [enodes] in [register]; slots [0,
       Dynarray.length enodes) are the live parents, each seeded to itself at registration.
       Capacity ([Array.length]) may exceed the live count — the surplus slots are stale
       and never read (ids are always < [num_terms]). *)
    mutable parents : int array
  ; index : int Term.Table.t (* Term -> e-node id *)
  ; sigtbl : int Sig.t
  ; (* Packed small-arity congruence signatures (task #47): a signature whose (sym, <=2
       arg-rep ids) all fit their bitfields is stored here under a single injective [int]
       key (identity hash, no array alloc, no generic array hash/compare) instead of in
       [sigtbl]. Large arity / out-of-range ids fall back to [sigtbl] — the packing
       decision is a deterministic function of the (sym, arg-rep) VALUES, so a given
       signature always resolves to exactly one table and the two never collide. Never
       iterated -> no Hashtbl-order in any observable path (C8). *)
    packtbl : int Int_set.t
  ; watched : watched Dynarray.t
  ; (* w_atom -> its two watched side TERMS, for [explain_implied]. Write-once and stable
       (an atom's sides never change): a non-Bool [Eq(a,b)] maps to [(a, b)]; a predicate
       [p(x…)] maps to [(p(x…), true_const)]. Terms (not e-node ids) so a re-registration
       after [pop] re-derives fresh ids via [register]. Never cleaned: only queried for a
       currently-live watch, so a stale entry for a popped-and-not-rewatched atom is
       inert. *)
    watch_sides : (Term.t * Term.t) Term.Table.t
  ; diseqs : 'p diseq Dynarray.t
  ; (* The int-packed typed undo trail (kept as-is — the hottest path in the solver) rides
       the shared substrate, which owns the frame stack, the newest-first
       drain-to-watermark pop loop (where the prop-mark watermark-trap bug lived), and the
       truncation discipline. Each frame's payload is the auxiliary-array watermarks
       {!level}. *)
    trail : ('p undo, level) Trail.t
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
  ; (* ADR-0014 Stage 2 merge-notification log. When [record_merges] is set (by the
       combinator when fabric callbacks are live), every actual class union appends its
       two ORIGINAL endpoint terms here (newest first); {!take_merges} drains it. Default
       OFF ⇒ the append is skipped, so direct-drive / fabric-off callers are
       byte-identical and pay zero hot-path cost. Reset on {!pop} so an undrained merge
       from a popped frame is never delivered (a completeness-safe drop; the normal
       CDCL(T) loop drains at every [check] between push/pop, so this only guards an
       assert-without-check driver). Order is the deterministic merge-queue order (I6).
       MULTI-CONSUMER: an append-only log with a per-client read cursor
       ({!add_merge_consumer} / {!drain_merges}), so the datatypes client and the
       LIA-notify path each see every merge independently. Cleared (and cursors reset) on
       {!pop}. *)
    mutable record_merges : bool
  ; merges : Fabric.merge_event Dynarray.t
  ; mutable merge_cursors : int ref list
  ; (* Per-call scratch dirty-root set for {!propagate}, owned by this engine instance and
       cleared (not reallocated) at the head of every call. An int-identity-hashed
       {!Int_set} replaces the former per-call generic [Hashtbl.create 64]: the membership
       test runs once per watched atom per propagate call (the hot [caml_hash] on int
       keys, ~7% of QF_UF/QF_AX wall — EUF-internal), and a fresh [Hashtbl] every call
       also churned the minor heap. Reusing one cleared table drops both. Not trailed: it
       is rebuilt from [touched] on every call, so [push]/[pop] never touch it. *)
    dirty : unit Int_set.t
  ; (* Separated-root-pair -> a witness disequality, rebuilt by {!propagate} (it already
       scans every diseq to build its membership set, so recording the witness diseq
       instead of [()] is free). {!distinct_witness} — called per distinct-propagation by
       [explain_implied] to cite a separating diseq (fun_2309, ~17% of QF_AX instructions:
       an O(#diseqs) scan PER call) — consults this for an O(1) hit instead of rescanning.
       [sep_wit_m] is the e-node count at the last build ([0] = invalid): the packing key
       [lo*m+hi] uses it, and equality with the current e-node count proves no [register]
       (count grows) or [pop] (count shrinks, and would leave popped ids unsafe to [find])
       has happened since — so every stored diseq id is still in bounds. INVALIDATION is
       the load-bearing correctness mechanism: [merge] (roots move) and [pop] set
       [sep_wit_m <- 0], and the count-equality gate catches [register]/[pop], so the
       cache is only ever consulted in the propagate→explain window where it holds the
       current, earliest-asserted witness. The per-hit re-verify ([find] of the cached
       diseq's endpoints must still equal the queried roots, else full-scan fallback) is
       DEFENSE-IN-DEPTH over that invalidation, not load-bearing: with the invalidation
       the cache is never consulted stale, so no functional/public-API test can
       distinguish dropping the re-verify (fable's review executed that mutant — euf-test,
       counted-identity, and a randomized push/pop oracle all stayed green), which is
       itself the proof it is non-load-bearing. It is kept as a cheap fail-safe on the
       wrong-verdict surface, guarding against a future root-mutating path added without
       invalidation. Not trailed. *)
    sep_wit : 'p diseq Int_set.t
  ; mutable sep_wit_m : int
  ; (* Per-call scratch for {!explain_core}, owned by this engine instance and cleared
       (not reallocated) at the head of every call — the same reuse discipline as
       {!dirty}. [explain_core] runs on every conflict/implied-explanation (112K–307K
       calls per solve on the QG/AX/UFLIA exemplars) and freshly allocated these three
       structures each call (~13% of EUF-core minor-heap churn, measured). It is never
       re-entrant: {!explain_implied} calls it twice in sequence and consumes each result
       (a [premises] list) before the next, and the [self_check] replay uses the
       independent [Naive] module — so a single shared, cleared instance is safe.
       [ex_out_seen] keys on the forest-child e-node id; [ex_explained] on the packed
       unordered pair; [ex_pending] is the congruence-expansion work queue. Not trailed:
       rebuilt from the queried pair on every call. *)
    ex_out_seen : unit Int_set.t
  ; ex_explained : unit Int_set.t
  ; ex_pending : (int * int) Queue.t
  ; (* Per-call scratch for {!merge} and {!dedup_int}, cleared (not reallocated) at entry
       — same reuse discipline as {!dirty}/{!ex_pending}. [merge_q] is the
       congruence-closure pending-merge fixpoint queue; [merge] is not re-entrant (its
       loop only enqueues onto the same queue — it never calls [merge], [register], or
       {!insert_congruence}). [dedup_seen] backs {!dedup_int}, called once per union in
       [merge] and once per {!register}; the two never overlap ([register] binds its
       [dedup_int] result before calling [insert_congruence], and nothing in [merge]'s
       loop calls [register]), so one shared cleared set is safe. Not trailed: pure
       per-call scratch. *)
    merge_q : (int * int * 'p reason) Queue.t
  ; dedup_seen : unit Int_set.t
  }

(* Note a class root as dirty for the next {!propagate} (see [touched]). *)
let mark_touched t root = Dynarray.add_last t.touched root

let create ctx =
  { ctx
  ; enodes = Dynarray.create ()
  ; parents = [||]
  ; index = Term.Table.create 256
  ; sigtbl = Sig.create 256
  ; packtbl = Int_set.create 256
  ; watched = Dynarray.create ()
  ; watch_sides = Term.Table.create 256
  ; diseqs = Dynarray.create ()
  ; trail = Trail.create ()
  ; touched = Dynarray.create ()
  ; prop_mark = 0
  ; stamp = 0
  ; record_merges = false
  ; merges = Dynarray.create ()
  ; merge_cursors = []
  ; dirty = Int_set.create 64
  ; sep_wit = Int_set.create 64
  ; sep_wit_m = 0
  ; ex_out_seen = Int_set.create 32
  ; ex_explained = Int_set.create 32
  ; ex_pending = Queue.create ()
  ; merge_q = Queue.create ()
  ; dedup_seen = Int_set.create 16
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

(* Union-find [find] over the flat [t.parents] int array (NOT the boxed [enode] record).
   Profiling QG timeouts (avg chain depth ~0.5, ~7M calls/solve, merge only ~2k) showed
   [find] at ~22% of wall spent almost entirely on per-CALL cost — a [Dynarray.get] bounds
   check plus a pointer chase into a boxed [enode] record — not on chain depth. A flat
   [int array] read with [Array.unsafe_get] removes both: contiguous, unboxed, no bounds
   check. Ids passed here are always valid e-node ids (< [num_terms]); every id is seeded
   to itself at registration, so no slot is ever read uninitialised. No path compression
   (the read-only query accessors — ADR-0012 R6 — rely on [find] being a pure traversal). *)
let rec find_go parents i =
  let p = Array.unsafe_get parents i in
  if p = i then i else find_go parents p
;;

let find t i = find_go t.parents i

let dedup_int seen lst =
  (* [Int_set] (int-identity hash) rather than a generic [Hashtbl]: the keys are e-node
     ids, so this drops the polymorphic [caml_hash]/[compare_val] on the merge/register
     hot path (called on a merged class's [uses] every union). Same semantics — a set of
     seen ints. [seen] is the caller's reusable scratch set, cleared here. *)
  Int_set.clear seen;
  List.filter
    (fun x ->
       if Int_set.mem seen x
       then false
       else (
         Int_set.replace seen x ();
         true))
    lst
;;

(* --- trailed mutation ---------------------------------------------------- *)

let push_undo t u = Trail.record t.trail u

let set_parent t i v =
  push_undo t (U_parent (i, Array.unsafe_get t.parents i));
  Array.unsafe_set t.parents i v
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

let apply_undo t = function
  | U_parent (i, old) -> Array.unsafe_set t.parents i old
  | U_size (i, old) -> (get t i).size <- old
  | U_uses (i, old) -> (get t i).uses <- old
  | U_fedge (i, op, orr) ->
    let n = get t i in
    n.fparent <- op;
    n.freason <- orr
  | U_sig_add key -> Sig.remove t.sigtbl key
  | U_sig_del (key, v) -> Sig.replace t.sigtbl key v
  | U_psig_add key -> Int_set.remove t.packtbl key
  | U_psig_del (key, v) -> Int_set.replace t.packtbl key v
  | U_reported (idx, old) -> (Dynarray.get t.watched idx).w_reported <- old
  | U_tag (i, old) -> (get t i).tag <- old
;;

(* --- congruence signatures ----------------------------------------------- *)

(* --- packed small-arity signature keys (task #47) ------------------------- Bitfield
   layout in a 63-bit OCaml [int] (max positive [2^62 - 1], so 62 usable bits): [61..60]
   arity tag (0|1|2) [59..40] sym (20b) [39..20] a0 (20b) [19..0] a1 (20b) Each field is
   range-CHECKED before it is shifted in; if [arity > 2], [sym >= 2^20], or any arg-rep
   [>= 2^20] the signature does NOT pack and [pack_sig] returns the sentinel [-1] (all
   packed keys are [>= 0]), routing the caller to the unpacked [sigtbl]. The disjoint bit
   ranges plus the arity tag make the map INJECTIVE by construction — distinct
   [(sym, arg-reps)] tuples (of packable shape) map to distinct ints, and a field that
   would overflow is never truncated into a shared key (it falls back instead). This
   injectivity is the wrong-congruence firewall: a lossy pack would merge two distinct App
   terms => wrong-UNSAT (the collision RED). *)
let sig_pack_sym_bits = 20
let sig_pack_arg_bits = 20
let sig_pack_sym_max = 1 lsl sig_pack_sym_bits
let sig_pack_arg_max = 1 lsl sig_pack_arg_bits

(* PURE packing core (testable in isolation — the collision RED targets exactly this).
   Given the arity [n] and the (already-resolved) symbol id [s] and arg-rep ids [a0]/[a1]
   (unused fields pass [0]), return the injective packed key, or [-1] if any field is out
   of its bitfield range. The range checks are the firewall: a field that would overflow
   forces [-1] (unpacked fallback), NEVER a truncated/aliased key. NB [a1] is only
   consulted for [n = 2] and [a0] only for [n >= 1], so the arity tag keeps the arities
   disjoint even when the ignored args happen to collide. *)
let pack_signature_fields ~n ~s ~a0 ~a1 =
  if n < 0 || n > 2 || s >= sig_pack_sym_max
  then -1
  else if n = 0
  then (* tag 0 *) s lsl 40
  else if n = 1
  then if a0 >= sig_pack_arg_max then -1 else (1 lsl 60) lor (s lsl 40) lor (a0 lsl 20)
  else if a0 >= sig_pack_arg_max || a1 >= sig_pack_arg_max
  then -1
  else (2 lsl 60) lor (s lsl 40) lor (a0 lsl 20) lor a1
;;

let pack_sig t id =
  match (get t id).kind with
  | Leaf -> invalid_arg "Euf: pack_sig on a non-App e-node"
  | Fun (sym, args) ->
    let s = (sym :> int) in
    (match Array.length args with
     | 0 -> pack_signature_fields ~n:0 ~s ~a0:0 ~a1:0
     | 1 -> pack_signature_fields ~n:1 ~s ~a0:(find t args.(0)) ~a1:0
     | 2 -> pack_signature_fields ~n:2 ~s ~a0:(find t args.(0)) ~a1:(find t args.(1))
     | _ -> -1)
;;

(* The unpacked fallback key (identical to the pre-#47 [sig_key]): materialize the arg-rep
   array. Only reached when [pack_sig] returned [-1] (large arity / out-of-range ids). *)
let unpacked_key t id =
  match (get t id).kind with
  | Fun (sym, args) -> (sym :> int), Array.map (fun a -> find t a) args
  | Leaf -> invalid_arg "Euf: unpacked_key on a non-App e-node"
;;

(* Value stored under [id]'s signature, or [-1] if absent. [pk = pack_sig t id]
   (caller-computed once, so the packed hot path recomputes no [find]s). *)
let sig_lookup t id pk =
  if pk >= 0
  then (
    match Int_set.find_opt t.packtbl pk with
    | Some v -> v
    | None -> -1)
  else (
    match Sig.find_opt t.sigtbl (unpacked_key t id) with
    | Some v -> v
    | None -> -1)
;;

(* Store [v] under [id]'s signature (trailed). [pk = pack_sig t id]. *)
let sig_store t id pk v =
  if pk >= 0
  then (
    Int_set.replace t.packtbl pk v;
    push_undo t (U_psig_add pk))
  else (
    let key = unpacked_key t id in
    Sig.replace t.sigtbl key v;
    push_undo t (U_sig_add key))
;;

let add_use t root id =
  let n = get t root in
  set_uses t root (id :: n.uses)
;;

(* Remove [p]'s current-signature table entry, but only if it is the representative
   (identity by e-node id) — a congruence-merged non-representative has no entry. *)
let sig_remove_if t p =
  let pk = pack_sig t p in
  if pk >= 0
  then (
    match Int_set.find_opt t.packtbl pk with
    | Some v when v = p ->
      Int_set.remove t.packtbl pk;
      push_undo t (U_psig_del (pk, v))
    | _ -> ())
  else (
    let key = unpacked_key t p in
    match Sig.find_opt t.sigtbl key with
    | Some v when v = p ->
      Sig.remove t.sigtbl key;
      push_undo t (U_sig_del (key, v))
    | _ -> ())
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
  (* Invalidate the {!distinct_witness} witness cache: a merge moves class roots, so its
     packed keys and stored witnesses are stale. {!propagate} rebuilds it (and no merge
     happens inside a [propagate]), so the cache is only ever consulted in the
     propagate→explain window; this keeps that invariant true for any caller (not just the
     CDCL(T) drive order), so the served witness is always the earliest-asserted one —
     same as the full scan (counted-metric identity). One int write per [merge] call. *)
  t.sep_wit_m <- 0;
  let q = t.merge_q in
  Queue.clear q;
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
      (* ADR-0014 Stage 2/3: capture the two classes' per-class tags NOW (before the
         surviving root inherits) and log the union for the merge-notification callback
         (the two ORIGINAL endpoint terms — for an asserted equality the asserted pair,
         for a congruence the two congruent [App] terms). Gated: off ⇒ skipped. *)
      let tag_a = (get t ra).tag
      and tag_b = (get t rb).tag in
      if t.record_merges
      then
        Dynarray.add_last
          t.merges
          { Fabric.kept = (get t a).term
          ; merged = (get t b).term
          ; kept_tag = tag_a (* tag of [a]'s class (ra) *)
          ; merged_tag = tag_b (* tag of [b]'s class (rb) *)
          };
      (* forest edge between the ORIGINAL endpoints, carrying [reason] *)
      add_forest_edge t a b reason;
      let parents = dedup_int t.dedup_seen (get t child).uses in
      (* remove parents from the table under their pre-union signatures *)
      List.iter (fun p -> sig_remove_if t p) parents;
      (* union child under root *)
      set_size t root ((get t root).size + (get t child).size);
      set_uses t root ((get t child).uses @ (get t root).uses);
      set_parent t child root;
      (* ADR-0014 Stage 3: the surviving root inherits the child's per-class tag if it had
         none (trailed). If BOTH carried a tag the collision is surfaced via the merge log
         above for the client to resolve; the root keeps its own tag meanwhile. *)
      (match (get t root).tag, (get t child).tag with
       | None, (Some _ as ct) ->
         push_undo t (U_tag (root, None));
         (get t root).tag <- ct
       | _ -> ());
      (* recompute parent signatures; schedule congruences *)
      List.iter
        (fun p ->
           let pk = pack_sig t p in
           let qq = sig_lookup t p pk in
           if qq >= 0
           then (if find t qq <> find t p then Queue.add (p, qq, R_cong (p, qq)) q)
           else sig_store t p pk p)
        parents)
  done
;;

let insert_congruence t id =
  let pk = pack_sig t id in
  let qq = sig_lookup t id pk in
  if qq >= 0
  then (if find t qq <> find t id then merge t id qq (R_cong (id, qq)))
  else sig_store t id pk id
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
      ; size = 1
      ; uses = []
      ; fparent = id
      ; freason = R_none
      ; stamp = 0
      ; tag = None
      };
    (* Grow the flat parent array in lockstep (doubling) and seed this node as its own
       root. Not trailed: a [pop] truncates [enodes] back below [id] (so slot [id] becomes
       stale and unread) and a later [register] reusing [id] re-seeds it here. *)
    if id >= Array.length t.parents
    then (
      let ncap = Int.max 16 (2 * Array.length t.parents) in
      let grown = Array.make ncap 0 in
      Array.blit t.parents 0 grown 0 (Array.length t.parents);
      t.parents <- grown);
    Array.unsafe_set t.parents id id;
    Term.Table.replace t.index term id;
    (match kind with
     | Fun (_, args) ->
       let roots =
         dedup_int t.dedup_seen (Array.to_list (Array.map (fun a -> find t a) args))
       in
       List.iter (fun r -> add_use t r id) roots;
       insert_congruence t id
     | Leaf -> ());
    (* Set up a watch on this term if its entailed truth is a congruence fact the theory
       must report (theory propagation / Nelson-Oppen sharing). Two shapes:
       - a non-Bool [Eq(a,b)]: truth = [a ~ b], sides [(a, b)];
       - a Bool-codomain predicate application [p(x…)] (arity >= 1): truth =
         [p(x…) ~ true_const], sides [(term, true_const)]. This is the ⊤/⊥ bridge that
         lets [p(a), a = b |- p(b)] flow back as a literal implication rather than being
         discovered only reactively via the [true <> false] axiom on a wrong guess. A
         nullary Bool [App] (a bare Bool variable) is NOT watched: its class can only be
         merged with true/false by directly asserting it, so a watch would only ever
         self-report the value SAT just set — no congruence can derive it. Follows the
         same over-watch/filter pattern as [Eq] (the adapter maps a reported [w_atom] back
         to its [Atom] and ignores watches with no atom, e.g. a buried predicate). *)
    let add_watch sa sb =
      let ia = register t sa
      and ib = register t sb in
      Dynarray.add_last t.watched { w_atom = term; w_a = ia; w_b = ib; w_reported = -1 };
      Term.Table.replace t.watch_sides term (sa, sb);
      (* a freshly-watched atom must be evaluated by the next {!propagate} even if no
         merge follows (its sides may already be (dis)equal) — dirty its endpoints. *)
      mark_touched t (find t ia);
      mark_touched t (find t ib)
    in
    (match term.node with
     | Eq (a, b) when not (Sort.equal a.sort Sort.bool) -> add_watch a b
     | App (_, args) when Sort.equal term.sort Sort.bool && Iarr.length args >= 1 ->
       add_watch term (Context.bool_const t.ctx true)
     | _ -> ());
    id
;;

let register_term t term = ignore (register t term : int)

(* --- ADR-0014 Stage 2 merge-notification log ----------------------------- *)

let clear_merges t =
  Dynarray.clear t.merges;
  List.iter (fun c -> c := 0) t.merge_cursors
;;

let set_record_merges t b =
  t.record_merges <- b;
  if not b then clear_merges t
;;

type merge_cursor = int ref

(* A merge consumer starts reading at the current end of the log (it sees only merges from
   here on). Multiple consumers (datatypes, LIA-notify) each register their own cursor. *)
let add_merge_consumer t =
  let c = ref (Dynarray.length t.merges) in
  t.merge_cursors <- c :: t.merge_cursors;
  c
;;

let drain_merges t c =
  let n = Dynarray.length t.merges in
  let out = ref [] in
  for i = n - 1 downto !c do
    out := Dynarray.get t.merges i :: !out
  done;
  c := n;
  !out
;;

(* ADR-0014 Stage 3 per-class tag (datatypes-scoped). Attach a witness [Term.t] to
   [term]'s class (registering [term]/[tag] if new); trailed, so a [pop] restores it.
   FIRST-set wins is NOT enforced here — a re-[set] on the same root overwrites (the
   client sets a class's constructor witness once at the constructor assertion, so
   overwrite does not arise in practice; the tag is a witness, not a justification). *)
let set_class_tag t term tag =
  let r = find t (register t term) in
  ignore (register t tag : int);
  push_undo t (U_tag (r, (get t r).tag));
  (get t r).tag <- Some tag
;;

(* The per-class tag of [term]'s class, or [None]. Registers [term] if new. *)
let class_tag t term = (get t (find t (register t term))).tag

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
  (* [Int_set] (int-identity hash) for both seen-sets, replacing generic [Hashtbl]s: the
     [caml_hash]/[compare_val] here is EUF-internal (fired per conflict/implied
     explanation). [out_seen] keys on the forest-child e-node id directly. [explained]
     normalizes each visited unordered pair [{x,y}] and packs it into one [int] key
     [lo*m+hi] — the same injective packing the separated-class index uses ([m] = e-node
     count, stable because [explain_core] never merges or registers). Distinct pairs give
     distinct keys while [m <= 2^31] (guarded below by an explicit fail-closed raise; the
     bound is physically unreachable — ~155 GB of e-nodes). Same semantics. Reuse the
     engine-owned scratch sets (cleared, not reallocated); see the [ex_*] field docs. *)
  let out_seen = t.ex_out_seen in
  let explained = t.ex_explained in
  Int_set.clear out_seen;
  Int_set.clear explained;
  let m = Dynarray.length t.enodes in
  (* Fail CLOSED if the packing precondition is ever violated: [assert] is elided under
     the release [-noassert] build, so an explicit raise is used instead — an overflowed
     [lo*m+hi] could alias two pairs and mis-deduplicate the explanation walk. The raise
     degrades to a sound [Unknown] via the solve firewall's catch-all
     ({!Session.raw_solve}), never a silently wrong explanation. Unreachable in practice. *)
  if m > 1 lsl 31 then invalid_arg "Euf.explain_core: e-node count exceeds packing bound";
  let pack lo hi = (lo * m) + hi in
  let pending = t.ex_pending in
  Queue.clear pending;
  Queue.add (a, b) pending;
  while not (Queue.is_empty pending) do
    let x, y = Queue.pop pending in
    if x <> y
    then (
      let key = if x < y then pack x y else pack y x in
      if not (Int_set.mem explained key)
      then (
        Int_set.replace explained key ();
        let c = nca t x y in
        let walk start =
          let cur = ref start in
          while !cur <> c do
            let n = get t !cur in
            let child = !cur in
            (match n.freason with
             | R_given (prem, u, v) ->
               if not (Int_set.mem out_seen child)
               then (
                 Int_set.replace out_seen child ();
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

(* An asserted disequality separating the classes of [a] and [b], if any. Returned so
   [explain_implied] can cite it and its endpoints.

   Fast path (fun_2309 was ~17% of QF_AX instructions as a raw O(#diseqs) scan run per
   distinct-propagation): consult [t.sep_wit], the witness index {!propagate} builds while
   scanning every diseq. It is usable only when its build-time e-node count still matches
   (no [register]/[pop] since, so every stored id is in bounds and the [lo*m+hi] key is
   the one it was stored under) and both queried roots are [< m]. Correctness rests on the
   INVALIDATION ([merge]/[pop] zero [sep_wit_m]; the count gate catches [register]/[pop]):
   the cache is only ever consulted fresh, holding the earliest-asserted witness, so it
   returns exactly what the scan would. The per-hit re-verify (cached diseq's endpoints
   must still [find] to the queried roots, else fall back) is DEFENSE-IN-DEPTH over that
   invalidation, not load-bearing — see the [sep_wit] field doc. Any miss/verify-fail/
   gate-fail falls back to the authoritative full scan (which also covers a diseq asserted
   since the build, which the index would lack). Same result as the scan in every case (a
   fast path over a sound fallback), so output is unchanged. *)
let distinct_witness t a b =
  let ra = find t a
  and rb = find t b in
  let full_scan () =
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
  in
  let m = t.sep_wit_m in
  if m > 0 && Dynarray.length t.enodes = m && ra < m && rb < m
  then (
    let key = if ra <= rb then (ra * m) + rb else (rb * m) + ra in
    match Int_set.find_opt t.sep_wit key with
    | Some d ->
      let du = find t d.d_a
      and dv = find t d.d_b in
      if (du = ra && dv = rb) || (du = rb && dv = ra) then Some d else full_scan ()
    | None -> full_scan ())
  else full_scan ()
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
  (* Reuse the engine-owned scratch set (int-identity hash, cleared not reallocated): the
     dirty-membership test below runs once per watched atom per call, so a generic
     [Hashtbl] over int keys paid [caml_hash]/[compare_val] there (EUF-internal, ~7% of
     QF_UF/QF_AX wall) and a fresh table every call churned the minor heap. Semantics
     unchanged — a set of dirty class roots for this call. *)
  let dirty = t.dirty in
  Int_set.clear dirty;
  for i = t.prop_mark to Dynarray.length t.touched - 1 do
    Int_set.replace dirty (Dynarray.get t.touched i) ()
  done;
  t.prop_mark <- Dynarray.length t.touched;
  let acc = ref [] in
  if Int_set.length dirty > 0
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
    (* Reuse the engine-owned witness index (cleared, not reallocated). It maps each
       separated root pair to a WITNESS diseq (not just [()]); {!propagate} needs only the
       membership test, but recording the witness is free here (we already scan every
       diseq) and lets {!distinct_witness} skip its own O(#diseqs) rescan. *)
    let sep = t.sep_wit in
    Int_set.clear sep;
    (* Pack an unordered rep pair [(lo, hi)] into one [int] key: [lo * m + hi] with
       [m = #e-nodes]. Every rep is an e-node id in [0, m), so distinct pairs give distinct
       keys UNLESS the packing wraps: OCaml [int] arithmetic is mod 2^63, so injectivity
       holds only for [m < floor (sqrt (2^63)) ~ 3.037e9]; the fail-closed check below
       enforces the stricter [m <= 2^31], well inside that bound, so a wrap can never alias
       two pairs. No merge happens inside [propagate], so [m] and
       every [find] are stable for the whole call; the build and lookup loops therefore use
       the same [m]. *)
    let m = Dynarray.length t.enodes in
    (* [m] is fixed for the whole call (no merge inside [propagate]). Guard the packing's
       injectivity precondition once here: keys alias only once [lo*m+hi] wraps mod 2^63,
       i.e. at [m >= floor (sqrt (2^63)) ~ 3.037e9]. We enforce the STRICTER [m <= 2^31]
       with an EXPLICIT fail-closed raise (NOT [assert], which [main/dune]'s release
       [-noassert] would compile out of the promotable binary): an overflowed key could
       alias two pairs and yield a wrong distinct-propagation (the wrong-verdict
       direction), so we raise instead — degrading to a sound [Unknown] via the solve
       firewall's catch-all ({!Session.raw_solve}) — rather than relying on the assert
       firing. The bound is also physically unreachable ([m > 2^31] e-nodes ~ 155 GB). *)
    if m > 1 lsl 31 then invalid_arg "Euf.propagate: e-node count exceeds packing bound";
    t.sep_wit_m <- m;
    let pack lo hi = (lo * m) + hi in
    Dynarray.iter
      (fun d ->
         let du = find t d.d_a
         and dv = find t d.d_b in
         let key = if du <= dv then pack du dv else pack dv du in
         (* FIRST-writer-wins: keep the earliest-asserted separating diseq for each pair,
           so the witness {!distinct_witness} serves is byte-identical to what its full
           assertion-order scan would return (same premise token ⇒ identical learned
           clauses ⇒ counted-metric identity). [Dynarray.iter] visits diseqs in assertion
           order. *)
         if not (Int_set.mem sep key) then Int_set.replace sep key d)
      t.diseqs;
    Dynarray.iteri
      (fun idx w ->
         let ra = find t w.w_a
         and rb = find t w.w_b in
         if Int_set.mem dirty ra || Int_set.mem dirty rb
         then (
           let cur =
             if ra = rb
             then 1
             else (
               let key = if ra <= rb then pack ra rb else pack rb ra in
               if Int_set.mem sep key then 0 else -1)
           in
           if cur <> -1 && cur <> w.w_reported
           then (
             set_reported t idx cur;
             acc := { atom = w.w_atom; value = cur = 1 } :: !acc)))
      t.watched);
  List.rev !acc
;;

(* The two watched side terms for a reported atom. Recorded at watch-creation time
   ([watch_sides]); for a non-Bool [Eq(a,b)] this is [(a, b)], for a predicate [p(x…)] it
   is [(p(x…), true_const)]. Fall back to the [Eq] node's own sides for robustness. *)
let watched_sides t (atom : Term.t) =
  match Term.Table.find_opt t.watch_sides atom with
  | Some sides -> sides
  | None ->
    (match atom.node with
     | Eq (a, b) -> a, b
     | _ -> invalid_arg "Euf.explain_implied: atom is not a watched atom")
;;

let explain_implied t imp =
  let a, b = watched_sides t imp.atom in
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

(* Re-arm the watch on [term]: reset its last-reported value to unknown (trailed, so a
   [pop] restores it like any other [w_reported] change) and re-dirty its endpoints, so
   the next {!propagate} re-reports [term]'s currently-entailed truth. No-op if [term] is
   not watched. Used when an [Atom] is bound to a predicate whose watch was created
   earlier by a boundary-only registration and whose one-shot flip was already consumed
   and dropped for lack of an atom (CONTRACT-REG late binding): without this, [register]'s
   idempotent early return leaves [w_reported] stale and the propagation is lost forever. *)
let rearm_watch t term =
  Dynarray.iteri
    (fun idx w ->
       if Term.equal w.w_atom term
       then (
         if w.w_reported <> -1 then set_reported t idx (-1);
         mark_touched t (find t w.w_a);
         mark_touched t (find t w.w_b)))
    t.watched
;;

(* Batched {!rearm_watch}: re-arm (same per-watch effect) every watch whose [w_atom]
   satisfies [pred], in ONE O(#watches) pass. The adapter's pop-recovery for the predicate
   late-binding recurrence (#161) re-arms a whole set of bound predicate watches at once;
   a per-term {!rearm_watch} loop would be O(#predicates x #watches). Iteration is
   watched-index (registration) order, so the set of endpoints dirtied is
   order-independent and the next {!propagate}'s reported list stays byte-identical (I6). *)
let rearm_watches_if t pred =
  Dynarray.iteri
    (fun idx w ->
       if pred w.w_atom
       then (
         if w.w_reported <> -1 then set_reported t idx (-1);
         mark_touched t (find t w.w_a);
         mark_touched t (find t w.w_b)))
    t.watched
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
  Trail.push
    t.trail
    { l_enodes = Dynarray.length t.enodes
    ; l_watched = Dynarray.length t.watched
    ; l_diseqs = Dynarray.length t.diseqs
    ; l_touched = Dynarray.length t.touched
    ; l_prop_mark = t.prop_mark
    }
;;

(* Restore EUF's auxiliary arrays to a frame's push-time watermarks. The substrate has
   already drained the typed undo trail newest-first (reversing every union/sig/reported
   mutation via [apply_undo]); this only truncates the append-only arrays and rewinds
   [prop_mark]. *)
let restore_aux t lv =
  Dynarray.truncate t.diseqs lv.l_diseqs;
  Dynarray.truncate t.watched lv.l_watched;
  (* restore the propagate delta log to its push-time snapshot: drop touched-roots logged
     in the popped frames and rewind [prop_mark], so a union that was propagated at a
     deeper level (its [set_reported] now undone by the trail) is re-evaluated here. *)
  Dynarray.truncate t.touched lv.l_touched;
  t.prop_mark <- lv.l_prop_mark;
  for i = Dynarray.length t.enodes - 1 downto lv.l_enodes do
    Term.Table.remove t.index (get t i).term
  done;
  Dynarray.truncate t.enodes lv.l_enodes;
  (* Invalidate the {!distinct_witness} cache: a pop truncates [enodes]/[diseqs], so
     cached diseq endpoint ids may now be out of bounds. Belt-and-suspenders — the
     count-equality gate in {!distinct_witness} already fails after a pop changes the
     e-node count, but a pop+re-register back to the same count between a [propagate] and
     a [distinct_witness] (not the normal drive order) would slip past it; zeroing
     [sep_wit_m] closes that. *)
  t.sep_wit_m <- 0
;;

let pop t n =
  Trail.pop t.trail ~apply:(apply_undo t) ~restore:(restore_aux t) n;
  (* Drop any merges accumulated since the last drain and reset cursors: a pop retracts
     the unions that produced them, so an undrained entry would name a merge that no
     longer holds (a completeness-safe drop — see {!merges}). A consumer's action on an
     already-drained merge unwinds via that consumer's own trailed state, not via
     re-notification. *)
  clear_merges t
;;

module Debug = struct
  let self_check = self_check

  (* Task #47: the PURE packed-signature core + its field widths, exposed for the
     collision RED unit test (euf_test.ml). Not on any solve path. *)
  let pack_signature_fields = pack_signature_fields
  let sig_pack_sym_bits = sig_pack_sym_bits
  let sig_pack_arg_bits = sig_pack_arg_bits
end
