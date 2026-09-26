open Pref_ring
open Pref_ring_general

let[@def] (allocated @ total) (h : node option Pref.heap @ immutable) (n : node @ immutable) =
  ghost_ (H.put (H.put (H.put (H.put h n.prev None) n.next None)
    n.prev (Some n)) n.next (Some n))

let rec (allocation_nodes @ total) : (h : node option Pref.heap) @ immutable ->
    (n : node) @ immutable -> (ns : node list) @ immutable ->
    {u : unit | if isolated h ns && not (H.mem h n.prev) && not (H.mem h n.next) then
      isolated (allocated h n) ns && apart_all n ns else true} @ ghost =
  fun h n ns -> ghost_ (
    if isolated h ns && not (H.mem h n.prev) && not (H.mem h n.next) then (
    allocated_def h n;
    isolated_def h ns; isolated_def (allocated h n) ns; apart_all_def n ns;
    match ns with [] -> () | x :: rest ->
      present_def h x; present_def (allocated h n) x;
      Pref_ring_proofs.allocation_frame n x h;
      apart_def n x; allocation_nodes h n rest; ()) else ())

let allocate : (sentinel : bool) -> (value : int) ->
    (ns : node list) @ immutable ghost ->
    (state : {state : node option Pref.token | isolated (Pref.own state) ns}) @ unique ->
    {r : created | r.node.sentinel = sentinel && r.node.value = value &&
      isolated (Pref.own r.state) (r.node :: ns)} @ unique =
  fun sentinel value ns state ->
  let before = ghost_ (Pref.own (borrow_ state)) in
  let made = make_node sentinel value state in
  let node = made.node in
  let state = made.state in
  ghost_ (allocation_nodes before node ns; allocated_def before node;
    isolated_def (Pref.own (borrow_ state)) (node :: ns));
  {node; state}

let (connect_view @ total) (h : node option Pref.heap @ immutable) (left : node @ immutable)
    (right : node @ immutable) (n : node @ immutable) :
    {u : unit | H.mem (connected h left right) n.prev =
        (right.prev === n.prev || left.next === n.prev || H.mem h n.prev) &&
      H.mem (connected h left right) n.next =
        (right.prev === n.next || left.next === n.next || H.mem h n.next) &&
      H.at (connected h left right) n.prev ===
        (if right.prev === n.prev then Some (Some left)
         else if left.next === n.prev then Some (Some right) else H.at h n.prev) &&
      H.at (connected h left right) n.next ===
        (if right.prev === n.next then Some (Some left)
         else if left.next === n.next then Some (Some right) else H.at h n.next)} @ ghost = ghost_ (
  connected_def h left right;
  Pref_ring_proofs.put_observations h left.next (Some right) n;
  Pref_ring_proofs.put_observations (H.put h left.next (Some right)) right.prev (Some left) n; ())

let wire (s : node @ immutable) (a : node @ immutable) (b : node @ immutable) (c : node @ immutable) (t : node @ immutable) (d : node @ immutable) (e : node @ immutable)
    (state : {state : node option Pref.token | s.sentinel && t.sentinel &&
      not a.sentinel && not b.sentinel && not c.sentinel && not d.sentinel && not e.sentinel &&
      isolated (Pref.own state) [s; a; b; c; t; d; e]} @ unique) :
    {result : node option Pref.token | ring (Pref.own result) s [a; b; c] &&
      ring (Pref.own result) t [d; e] && separated (append [s; a; b; c] [t; d; e])} @ unique =
  let before = ghost_ (Pref.own (borrow_ state)) in
  ghost_ (
    isolated_def before [s; a; b; c; t; d; e]; separated_def [s; a; b; c; t; d; e];
    isolated_def before [a; b; c; t; d; e]; separated_def [a; b; c; t; d; e];
    isolated_def before [b; c; t; d; e]; separated_def [b; c; t; d; e];
    isolated_def before [c; t; d; e]; separated_def [c; t; d; e];
    isolated_def before [t; d; e]; separated_def [t; d; e];
    isolated_def before [d; e]; separated_def [d; e];
    isolated_def before [e]; separated_def [e];
    isolated_def before []; separated_def [];
    present_def before s;
    apart_all_def s [a; b; c; t; d; e];
    apart_all_def s [b; c; t; d; e];
    apart_all_def s [c; t; d; e];
    apart_all_def s [t; d; e];
    apart_all_def s [d; e];
    apart_all_def s [e];
    apart_all_def s [];
    apart_def s a;
    apart_def s b;
    apart_def s c;
    apart_def s t;
    apart_def s d;
    apart_def s e;
    present_def before a;
    apart_all_def a [b; c; t; d; e];
    apart_all_def a [c; t; d; e];
    apart_all_def a [t; d; e];
    apart_all_def a [d; e];
    apart_all_def a [e];
    apart_all_def a [];
    apart_def a b;
    apart_def a c;
    apart_def a t;
    apart_def a d;
    apart_def a e;
    present_def before b;
    apart_all_def b [c; t; d; e];
    apart_all_def b [t; d; e];
    apart_all_def b [d; e];
    apart_all_def b [e];
    apart_all_def b [];
    apart_def b c;
    apart_def b t;
    apart_def b d;
    apart_def b e;
    present_def before c;
    apart_all_def c [t; d; e];
    apart_all_def c [d; e];
    apart_all_def c [e];
    apart_all_def c [];
    apart_def c t;
    apart_def c d;
    apart_def c e;
    present_def before t;
    apart_all_def t [d; e];
    apart_all_def t [e];
    apart_all_def t [];
    apart_def t d;
    apart_def t e;
    present_def before d;
    apart_all_def d [e];
    apart_all_def d [];
    apart_def d e;
    present_def before e;
    apart_all_def e [];
    ());
  ghost_ (
    connect_view (Pref.own (borrow_ state)) s a s;
    connect_view (Pref.own (borrow_ state)) s a a;
    connect_view (Pref.own (borrow_ state)) s a b;
    connect_view (Pref.own (borrow_ state)) s a c;
    connect_view (Pref.own (borrow_ state)) s a t;
    connect_view (Pref.own (borrow_ state)) s a d;
    connect_view (Pref.own (borrow_ state)) s a e;
    ());
  let state = connect s a state in
  ghost_ (
    connect_view (Pref.own (borrow_ state)) a b s;
    connect_view (Pref.own (borrow_ state)) a b a;
    connect_view (Pref.own (borrow_ state)) a b b;
    connect_view (Pref.own (borrow_ state)) a b c;
    connect_view (Pref.own (borrow_ state)) a b t;
    connect_view (Pref.own (borrow_ state)) a b d;
    connect_view (Pref.own (borrow_ state)) a b e;
    ());
  let state = connect a b state in
  ghost_ (
    connect_view (Pref.own (borrow_ state)) b c s;
    connect_view (Pref.own (borrow_ state)) b c a;
    connect_view (Pref.own (borrow_ state)) b c b;
    connect_view (Pref.own (borrow_ state)) b c c;
    connect_view (Pref.own (borrow_ state)) b c t;
    connect_view (Pref.own (borrow_ state)) b c d;
    connect_view (Pref.own (borrow_ state)) b c e;
    ());
  let state = connect b c state in
  ghost_ (
    connect_view (Pref.own (borrow_ state)) c s s;
    connect_view (Pref.own (borrow_ state)) c s a;
    connect_view (Pref.own (borrow_ state)) c s b;
    connect_view (Pref.own (borrow_ state)) c s c;
    connect_view (Pref.own (borrow_ state)) c s t;
    connect_view (Pref.own (borrow_ state)) c s d;
    connect_view (Pref.own (borrow_ state)) c s e;
    ());
  let state = connect c s state in
  ghost_ (
    connect_view (Pref.own (borrow_ state)) t d s;
    connect_view (Pref.own (borrow_ state)) t d a;
    connect_view (Pref.own (borrow_ state)) t d b;
    connect_view (Pref.own (borrow_ state)) t d c;
    connect_view (Pref.own (borrow_ state)) t d t;
    connect_view (Pref.own (borrow_ state)) t d d;
    connect_view (Pref.own (borrow_ state)) t d e;
    ());
  let state = connect t d state in
  ghost_ (
    connect_view (Pref.own (borrow_ state)) d e s;
    connect_view (Pref.own (borrow_ state)) d e a;
    connect_view (Pref.own (borrow_ state)) d e b;
    connect_view (Pref.own (borrow_ state)) d e c;
    connect_view (Pref.own (borrow_ state)) d e t;
    connect_view (Pref.own (borrow_ state)) d e d;
    connect_view (Pref.own (borrow_ state)) d e e;
    ());
  let state = connect d e state in
  ghost_ (
    connect_view (Pref.own (borrow_ state)) e t s;
    connect_view (Pref.own (borrow_ state)) e t a;
    connect_view (Pref.own (borrow_ state)) e t b;
    connect_view (Pref.own (borrow_ state)) e t c;
    connect_view (Pref.own (borrow_ state)) e t t;
    connect_view (Pref.own (borrow_ state)) e t d;
    connect_view (Pref.own (borrow_ state)) e t e;
    ());
  let state = connect e t state in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ (
    present_def after s;
    present_def after a;
    present_def after b;
    present_def after c;
    present_def after t;
    present_def after d;
    present_def after e;
    ring_def after s [a; b; c]; head_def [a; b; c] s;
    linked_def after s [a; b; c] s; head_def [a; b; c] s;
    linked_def after a [b; c] s; head_def [b; c] s;
    linked_def after b [c] s; head_def [c] s;
    linked_def after c [] s; head_def [] s;
    ring_def after t [d; e]; head_def [d; e] t;
    linked_def after t [d; e] t; head_def [d; e] t;
    linked_def after d [e] t; head_def [e] t;
    linked_def after e [] t; head_def [] t;
    append_def [s; a; b; c] [t; d; e];
    append_def [a; b; c] [t; d; e];
    append_def [b; c] [t; d; e];
    append_def [c] [t; d; e];
    append_def [] [t; d; e];
    ());
  state
