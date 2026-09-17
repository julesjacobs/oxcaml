open Copy_spec
open Copy_certificate_spec
module C = Representative_certificate
module E = Effective_level
module K = Effective_copy_spec

let (target_agrees @ total) : (h : node Pref.heap) @ immutable ->
    (c : C.certificate) @ immutable -> (heads : E.heads) @ total ->
    (d : history) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable ->
    {u : unit | C.certificate_valid h c && C.listed c p && E.valid_head h heads p} ->
    {u : unit | certified_target h c d p q === K.effective_target_for h heads d p q} @ ghost =
  fun h c heads d p q premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    certified_target_def h c d p q; K.effective_target_for_def h heads d p q;
    certificate_level_def h c p;
    if H.mem h p then (C.level_agrees h c heads p (refine_ u); refine_ u) else refine_ u)

let (ready_agrees @ total) : (h : node Pref.heap) @ immutable ->
    (c : C.certificate) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p})) @ total ->
    (d : history) @ immutable -> (source : desc) @ immutable -> (dest : desc) @ immutable ->
    {u : unit | C.certificate_valid h c && covered_desc c source} ->
    {u : unit | certified_ready h c d source dest === K.effective_ready h heads d source dest} @ ghost =
  fun h c heads witness d source dest premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    certified_ready_def h c d source dest; K.effective_ready_def h heads d source dest;
    covered_desc_def c source;
    match source, dest with
    | Arrow (a, b), Arrow (x, y) -> witness a; witness b;
      target_agrees h c heads d a x (refine_ u);
      target_agrees h c heads d b y (refine_ u); refine_ u
    | _ -> refine_ u)

let rec (valid_agrees @ total) : (h : node Pref.heap) @ immutable ->
    (c : C.certificate) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    {u : unit | C.certificate_valid h c && covered c d} ->
    {u : unit | certified_valid h c epoch depth d === K.effective_valid h heads epoch depth d} @ ghost =
  fun h c heads witness epoch depth d premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    covered_def c d; certified_valid_def h c epoch depth d;
    K.effective_valid_def h heads epoch depth d;
    match d with Start | Clean -> refine_ u
    | Fresh (rest, p, _, old, desc) ->
      valid_agrees h c heads witness epoch depth rest (refine_ u);
      ready_agrees h c heads witness rest old.desc desc (refine_ u);
      certificate_level_def h c p; witness p;
      if H.mem h p then (C.level_agrees h c heads p (refine_ u); refine_ u) else refine_ u
    | Alias (rest, p, q, old) ->
      valid_agrees h c heads witness epoch depth rest (refine_ u);
      certificate_level_def h c p; witness p;
      if H.mem h p then (
        C.level_agrees h c heads p (refine_ u);
        covered_desc_def c old.desc;
        match old.desc with Link x -> witness x; target_agrees h c heads rest x q (refine_ u); refine_ u
        | _ -> refine_ u) else refine_ u)

let (certify @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | K.effective_valid h heads epoch depth d && K.effective_target_for h heads d p q} ->
    {c : C.certificate | certifies h c epoch depth d p q} @ immutable ghost =
  fun h heads witness epoch depth d p q premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    let empty = C.Empty in C.certificate_valid_def h empty;
    witness p; E.valid_head_def h heads p;
    let seed = C.Entry (p, heads p, empty) in
    C.certificate_valid_def h seed; C.listed_def seed p;
    let out = capture heads d seed in
    Copy_certificate_capture.capture_valid h heads witness d seed (refine_ u);
    Copy_certificate_capture.capture_preserves heads d seed p (refine_ u);
    valid_agrees h out heads witness epoch depth d (refine_ u);
    target_agrees h out heads d p q (refine_ u);
    certifies_def h out epoch depth d p q; refine_ out)

let (replay @ total) : (h : node Pref.heap) @ immutable -> (c : C.certificate) @ immutable ->
    (heads : E.heads) @ total ->
    (witness : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | certifies h c epoch depth d p q} ->
    {u : unit | K.effective_valid h heads epoch depth d && K.effective_target_for h heads d p q} @ ghost =
  fun h c heads witness epoch depth d p q premise -> ghost_ (
    let refine_ premise = premise in certifies_def h c epoch depth d p q;
    let u = () in
    valid_agrees h c heads witness epoch depth d (refine_ u);
    witness p; target_agrees h c heads d p q (refine_ u); refine_ u)
