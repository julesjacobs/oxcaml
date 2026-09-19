open Copy_spec
open Copy_certificate_spec
module C = Representative_certificate
module E = Effective_level

let (desc_preserves @ total) : (heads : E.heads) @ total ->
    (desc : desc) @ immutable -> (c : C.certificate) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | C.listed c x} -> {u : unit | C.listed (capture_desc heads desc c) x} @ ghost =
  fun heads desc c x premise -> ghost_ (
    capture_desc_def heads desc c; match desc with Var | Bool -> ()
    | Link p -> let out = C.Entry (p, heads p, c) in C.listed_def out x; ()
    | Arrow (a, b) -> let tail = C.Entry (b, heads b, c) in
      let out = C.Entry (a, heads a, tail) in C.listed_def tail x; C.listed_def out x; ())

let rec (capture_preserves @ total) : (heads : E.heads) @ total ->
    (d : history) @ immutable -> (c : C.certificate) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | C.listed c x} -> {u : unit | C.listed (capture heads d c) x} @ ghost =
  fun heads d c x premise -> ghost_ (
    capture_def heads d c; match d with Start | Clean -> ()
    | Fresh (rest, p, _, old, _) | Alias (rest, p, _, old) ->
      desc_preserves heads old.desc c x ();
      let next = C.Entry (p, heads p, capture_desc heads old.desc c) in
      C.listed_def next x; capture_preserves heads rest next x (); ())

let (desc_valid @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p})) @ total ->
    (desc : desc) @ immutable -> (c : C.certificate) @ immutable ->
    {u : unit | C.certificate_valid h c} ->
    {u : unit | C.certificate_valid h (capture_desc heads desc c) && covered_desc (capture_desc heads desc c) desc} @ ghost =
  fun h heads witness desc c premise -> ghost_ (
    capture_desc_def heads desc c;
    let out = capture_desc heads desc c in covered_desc_def out desc; match desc with Var | Bool -> ()
    | Link p -> witness p; E.valid_head_def h heads p;
      C.certificate_valid_def h out; C.listed_def out p; ()
    | Arrow (a, b) -> witness a; witness b; E.valid_head_def h heads a; E.valid_head_def h heads b;
      let tail = C.Entry (b, heads b, c) in
      C.certificate_valid_def h out; C.certificate_valid_def h tail;
      C.listed_def out a; C.listed_def out b; C.listed_def tail b; ())

let (desc_transport @ total) : (heads : E.heads) @ total ->
    (d : history) @ immutable -> (c : C.certificate) @ immutable -> (desc : desc) @ immutable ->
    {u : unit | covered_desc c desc} ->
    {u : unit | covered_desc (capture heads d c) desc} @ ghost =
  fun heads d c desc premise -> ghost_ (
    covered_desc_def c desc;
    let out = capture heads d c in covered_desc_def out desc; match desc with Var | Bool -> ()
    | Link p -> capture_preserves heads d c p (); ()
    | Arrow (a, b) -> capture_preserves heads d c a ();
      capture_preserves heads d c b (); ())

let rec (capture_valid @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p})) @ total ->
    (d : history) @ immutable -> (c : C.certificate) @ immutable ->
    {u : unit | C.certificate_valid h c} ->
    {u : unit | C.certificate_valid h (capture heads d c) && covered (capture heads d c) d} @ ghost =
  fun h heads witness d c premise -> ghost_ (
    capture_def heads d c;
    let out = capture heads d c in covered_def out d; match d with Start | Clean -> ()
    | Fresh (rest, p, _, old, _) | Alias (rest, p, _, old) ->
      desc_valid h heads witness old.desc c ();
      let tail = capture_desc heads old.desc c in
      let next = C.Entry (p, heads p, tail) in
      witness p; E.valid_head_def h heads p; C.certificate_valid_def h next;
      capture_valid h heads witness rest next ();
      C.listed_def next p; capture_preserves heads rest next p ();
      covered_desc_def tail old.desc; covered_desc_def next old.desc;
      (match old.desc with Var | Bool -> ()
      | Link x -> C.listed_def next x; ()
      | Arrow (a, b) -> C.listed_def next a; C.listed_def next b; ());
      desc_transport heads rest next old.desc (); ())
