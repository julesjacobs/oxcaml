module B = Vox_partition

let (same_lookup @ total) : (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if B.same p q then B.lookup p x === B.lookup q x else true}
    @ ghost = fun p q x -> ghost_ (
  B.same_law p q x; B.contains_def p x; B.contains_def q x;
  B.representative_def p x; B.representative_def q x; ())

let rec (same_agree_reverse @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (entries : 'a B.bindings) @ immutable ->
    {u : unit | if B.same p q then B.agree q p entries else true} @ ghost =
    fun p q entries -> ghost_ (
  B.agree_def q p entries;
  match entries with [] -> () | (x, _) :: rest ->
    same_lookup p q x; same_agree_reverse p q rest)

let (same_symmetric @ total) : (p : ('a : logical_data) B.bindings) @ immutable
  ->
    (q : 'a B.bindings) @ immutable ->
    {u : unit | if B.same p q then B.same q p else true} @ ghost =
    fun p q -> ghost_ (
  B.same_def p q; B.same_def q p;
  same_agree_reverse p q p; same_agree_reverse p q q;
  ())

let rec (same_transitive_agree @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (r : 'a B.bindings) @ immutable ->
    (entries : 'a B.bindings) @ immutable ->
    {u : unit | if B.same p q && B.same q r then B.agree p r entries else true}
    @ ghost = fun p q r entries -> ghost_ (
  B.agree_def p r entries;
  match entries with [] -> () | (x, _) :: rest ->
    same_lookup p q x; same_lookup q r x; same_transitive_agree p q r rest)

let (same_transitive @ total) : (p : ('a : logical_data) B.bindings) @ immutable
  ->
    (q : 'a B.bindings) @ immutable -> (r : 'a B.bindings) @ immutable ->
    {u : unit | if B.same p q && B.same q r then B.same p r else true}
    @ ghost = fun p q r -> ghost_ (
  B.same_def p q; B.same_def q r; B.same_def p r;
  same_transitive_agree p q r p; same_transitive_agree p q r r)

let rec (singleton_agree @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    (entries : 'a B.bindings) @ immutable ->
    {u : unit | if B.same p q then
      B.agree (B.add_singleton p x) (B.add_singleton q x) entries else true}
    @ ghost = fun p q x entries -> ghost_ (
  B.agree_def (B.add_singleton p x) (B.add_singleton q x) entries;
  B.add_singleton_def p x; B.add_singleton_def q x;
  match entries with [] -> () | (a, _) :: rest ->
    same_lookup p q a; B.lookup_def ((x, x) :: p) a;
    B.lookup_def ((x, x) :: q) a; singleton_agree p q x rest)

let (singleton_congruent @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if B.same p q then
      B.same (B.add_singleton p x) (B.add_singleton q x) else true}
    @ ghost = fun p q x -> ghost_ (
  B.same_def p q;
  B.same_def (B.add_singleton p x) (B.add_singleton q x);
  singleton_agree p q x (B.add_singleton p x);
  singleton_agree p q x (B.add_singleton q x);
  B.add_singleton_law p x x; B.add_singleton_law q x x; ())

let (added_congruent @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (p' : 'a B.bindings) @ immutable -> (q : 'a B.bindings) @ immutable ->
    (q' : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if B.same p p' && B.same q q' && B.added p q x &&
      B.valid p' && B.valid q' then B.added p' q' x else true} @ ghost =
    fun p p' q q' x -> ghost_ (
  B.added_def p q x; B.same_law p p' x;
  same_symmetric q q'; same_transitive q' q (B.add_singleton p x);
  singleton_congruent p p' x;
  same_transitive q' (B.add_singleton p x) (B.add_singleton p' x);
  B.added_def p' q' x; ())

let rec (redirect_agree @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (rx : 'a) @ immutable ->
    (ry : 'a) @ immutable -> (r : 'a) @ immutable ->
    (entries : 'a B.bindings) @ immutable ->
    {u : unit | if B.same p q then
      B.agree (B.redirect p rx ry r) (B.redirect q rx ry r) entries else true}
    @ ghost = fun p q rx ry r entries -> ghost_ (
  B.agree_def (B.redirect p rx ry r) (B.redirect q rx ry r) entries;
  match entries with [] -> () | (a, _) :: rest ->
    same_lookup p q a; B.redirect_lookup p rx ry r a;
    B.redirect_lookup q rx ry r a; redirect_agree p q rx ry r rest)

let (redirect_congruent @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (rx : 'a) @ immutable ->
    (ry : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if B.same p q then
      B.same (B.redirect p rx ry r) (B.redirect q rx ry r) else true} @ ghost =
    fun p q rx ry r -> ghost_ (
  B.same_def p q;
  B.same_def (B.redirect p rx ry r) (B.redirect q rx ry r);
  redirect_agree p q rx ry r (B.redirect p rx ry r);
  redirect_agree p q rx ry r (B.redirect q rx ry r);
  B.redirect_size p rx ry r; B.redirect_size q rx ry r)

let (merge_congruent @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if B.same p q then
      B.same (B.merge_classes p x y r) (B.merge_classes q x y r) else true}
    @ ghost = fun p q x y r -> ghost_ (
  B.same_law p q x; B.same_law p q y;
  B.merge_classes_def p x y r; B.merge_classes_def q x y r;
  redirect_congruent p q (B.representative p x) (B.representative p y) r)

let (joined_congruent @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (p' : 'a B.bindings) @ immutable -> (q : 'a B.bindings) @ immutable ->
    (q' : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if B.same p p' && B.same q q' && B.joined p q x y r &&
      B.valid p' && B.valid q' then B.joined p' q' x y r else true} @ ghost =
    fun p p' q q' x y r -> ghost_ (
  B.joined_def p q x y r;
  B.same_law p p' x; B.same_law p p' y; B.same_law p p' r;
  B.connected_def p x r; B.connected_def p y r; B.connected_def p x y;
  B.connected_def p' x r; B.connected_def p' y r; B.connected_def p' x y;
  same_symmetric p p'; same_transitive p' p q;
  same_transitive p' q q';
  same_symmetric q q'; same_transitive q' q (B.merge_classes p x y r);
  merge_congruent p p' x y r;
  same_transitive q' (B.merge_classes p x y r) (B.merge_classes p' x y r);
  B.joined_def p' q' x y r; ())
