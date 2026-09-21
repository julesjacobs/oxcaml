open Copy_spec
open Copy_certificate_spec
module C = Representative_certificate
module E = Effective_level

let (desc_preserves @ total) : (heads : E.heads) @ total ->
    (desc : desc) @ immutable -> (c : C.certificate) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | C.listed c x} -> {u : unit | C.listed (capture_desc heads desc c) x} @ ghost =
  fun heads desc c x premise -> ghost_ (
    let refine_ premise = premise in capture_desc_def heads desc c; let u = () in
    match desc with Var | Bool -> refine_ u
    | Link p -> let out = C.Entry (p, heads p, c) in C.listed_def out x; refine_ u
    | Arrow (a, b) -> let tail = C.Entry (b, heads b, c) in
      let out = C.Entry (a, heads a, tail) in C.listed_def tail x; C.listed_def out x; refine_ u)

let rec (capture_preserves @ total) : (heads : E.heads) @ total ->
    (d : history) @ immutable -> (c : C.certificate) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | C.listed c x} -> {u : unit | C.listed (capture heads d c) x} @ ghost =
  fun heads d c x premise -> ghost_ (
    let refine_ premise = premise in capture_def heads d c; let u = () in
    match d with Start | Clean -> refine_ u
    | Fresh (rest, p, _, old, _) | Alias (rest, p, _, old) ->
      desc_preserves heads old.desc c x (refine_ u);
      let next = C.Entry (p, heads p, capture_desc heads old.desc c) in
      C.listed_def next x; capture_preserves heads rest next x (refine_ u); refine_ u)

let (desc_valid @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p})) @ total ->
    (desc : desc) @ immutable -> (c : C.certificate) @ immutable ->
    {u : unit | C.certificate_valid h c} ->
    {u : unit | C.certificate_valid h (capture_desc heads desc c) && covered_desc (capture_desc heads desc c) desc} @ ghost =
  fun h heads witness desc c premise -> ghost_ (
    let refine_ premise = premise in capture_desc_def heads desc c;
    let out = capture_desc heads desc c in covered_desc_def out desc; let u = () in
    match desc with Var | Bool -> refine_ u
    | Link p -> witness p; E.valid_head_def h heads p;
      C.certificate_valid_def h out; C.listed_def out p; refine_ u
    | Arrow (a, b) -> witness a; witness b; E.valid_head_def h heads a; E.valid_head_def h heads b;
      let tail = C.Entry (b, heads b, c) in
      C.certificate_valid_def h out; C.certificate_valid_def h tail;
      C.listed_def out a; C.listed_def out b; C.listed_def tail b; refine_ u)

let (desc_transport @ total) : (heads : E.heads) @ total ->
    (d : history) @ immutable -> (c : C.certificate) @ immutable -> (desc : desc) @ immutable ->
    {u : unit | covered_desc c desc} ->
    {u : unit | covered_desc (capture heads d c) desc} @ ghost =
  fun heads d c desc premise -> ghost_ (
    let refine_ premise = premise in covered_desc_def c desc;
    let out = capture heads d c in covered_desc_def out desc; let u = () in
    match desc with Var | Bool -> refine_ u
    | Link p -> capture_preserves heads d c p (refine_ u); refine_ u
    | Arrow (a, b) -> capture_preserves heads d c a (refine_ u);
      capture_preserves heads d c b (refine_ u); refine_ u)

let rec (capture_valid @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p})) @ total ->
    (d : history) @ immutable -> (c : C.certificate) @ immutable ->
    {u : unit | C.certificate_valid h c} ->
    {u : unit | C.certificate_valid h (capture heads d c) && covered (capture heads d c) d} @ ghost =
  fun h heads witness d c premise -> ghost_ (
    let refine_ premise = premise in capture_def heads d c;
    let out = capture heads d c in covered_def out d; let u = () in
    match d with Start | Clean -> refine_ u
    | Fresh (rest, p, _, old, _) | Alias (rest, p, _, old) ->
      desc_valid h heads witness old.desc c (refine_ u);
      let tail = capture_desc heads old.desc c in
      let next = C.Entry (p, heads p, tail) in
      witness p; E.valid_head_def h heads p; C.certificate_valid_def h next;
      capture_valid h heads witness rest next (refine_ u);
      C.listed_def next p; capture_preserves heads rest next p (refine_ u);
      covered_desc_def tail old.desc; covered_desc_def next old.desc;
      (match old.desc with Var | Bool -> ()
      | Link x -> C.listed_def next x; ()
      | Arrow (a, b) -> C.listed_def next a; C.listed_def next b; ());
      desc_transport heads rest next old.desc (refine_ u); refine_ u)
