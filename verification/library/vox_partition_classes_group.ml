module C = Vox_partition_classes
module B = Vox_partition
module Bridge = Vox_partition_classes_bridge

let[@def] rec bucket (p : ('a : logical_data) B.bindings @ immutable)
    (r : 'a @ immutable) = ghost_ (
  match p with
  | [] -> []
  | (x, rep) :: rest ->
      if rep === r && not (x === r) then x :: bucket rest r else bucket rest r)

let[@def] rec roots (p : ('a : logical_data) B.bindings @ immutable)
    (r : 'a @ immutable) = ghost_ (
  match p with
  | [] -> false
  | (x, rep) :: rest -> (x === rep && x === r) || roots rest r)

let[@def] rec scan (whole : ('a : logical_data) B.bindings @ immutable)
    (entries : 'a B.bindings @ immutable) = ghost_ (
  match entries with
  | [] -> []
  | (x, rep) :: rest ->
      if x === rep then (x, bucket whole x) :: scan whole rest else scan whole
        rest)

let[@def] group (p : ('a : logical_data) B.bindings @ immutable) :
    'a C.classes @ total immutable ghost = ghost_ (scan p p)

let rec (bucket_mem @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (r : 'a) @ immutable ->
    (q : 'a) @ immutable ->
    {u : unit | if B.unique p then C.mem (bucket p r) q ===
      (B.lookup p q === Some r && not (q === r)) else true} @ ghost =
    fun p r q -> ghost_ (
  bucket_def p r; B.unique_def p; B.lookup_def p q;
  C.mem_def (bucket p r) q;
  match p with
  | [] -> ()
  | (x, rep) :: rest ->
      bucket_mem rest r q; B.contains_def rest x;
      ())

let rec (roots_lookup @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if B.unique p then roots p r === (B.lookup p r === Some r)
      else true} @ ghost = fun p r -> ghost_ (
  roots_def p r; B.unique_def p; B.lookup_def p r;
  match p with
  | [] -> ()
  | (x, _) :: rest -> roots_lookup rest r; B.contains_def rest x)

let rec (scan_lookup @ total) :
    (whole : ('a : logical_data) B.bindings) @ immutable ->
    (entries : 'a B.bindings) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | if B.valid whole && Bridge.observed whole entries then
      C.lookup (scan whole entries) q ===
        (match B.lookup whole q with
         | None -> None
         | Some r -> if roots entries r then Some r else None) else true}
    @ ghost = fun whole entries q -> ghost_ (
  scan_def whole entries; roots_def entries q;
  Bridge.observed_def whole entries; B.valid_def whole;
  match entries with
  | [] -> C.lookup_def (scan whole entries) q;
      (match B.lookup whole q with None -> () | Some r -> roots_def entries r)
  | (x, rep) :: rest ->
      scan_lookup whole rest q;
      (match B.lookup whole q with None -> () | Some r -> roots_def entries r);
      if x === rep then (
        C.lookup_def (scan whole entries) q;
        bucket_mem whole x q;
        B.contains_def whole q; B.representative_def whole q;
        B.representative_law whole q;
        roots_def entries (B.representative whole q))
      else roots_def entries (B.representative whole q))

let (group_lookup @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | if B.valid p then C.lookup (group p) q === B.lookup p q else
      true}
    @ ghost = fun p q -> ghost_ (
  group_def p; B.valid_def p; Bridge.observed_self p; scan_lookup p p q;
  B.contains_def p q; B.representative_def p q;
  B.representative_law p q;
  roots_lookup p (B.representative p q);
  B.contains_def p (B.representative p q);
  B.representative_def p (B.representative p q); ())

let (emit_contains @ total) :
    (xs : ('a : logical_data) list) @ immutable -> (r : 'a) @ immutable ->
    (tail : 'a B.bindings) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | B.contains (Bridge.emit xs r tail) q ===
      (C.mem xs q || B.contains tail q)} @ ghost = fun xs r tail q -> ghost_ (
  Bridge.emit_lookup xs r tail q; B.contains_def (Bridge.emit xs r tail) q;
  B.contains_def tail q; ())

let rec (bucket_unique @ total) :
    (whole : ('a : logical_data) B.bindings) @ immutable ->
    (source : 'a B.bindings) @ immutable -> (entries : 'a B.bindings) @
      immutable ->
    (r : 'a) @ immutable ->
    {u : unit | if B.valid whole && B.unique source && Bridge.observed whole
      source &&
      Bridge.observed whole entries && not (roots entries r) &&
      B.unique (Bridge.flatten (scan whole entries)) then
      B.unique (Bridge.emit (bucket source r) r (Bridge.flatten (scan whole
        entries)))
      else true} @ ghost = fun whole source entries r -> ghost_ (
  bucket_def source r; B.unique_def source; Bridge.observed_def whole source;
  match source with
  | [] -> Bridge.emit_def (bucket source r) r (Bridge.flatten (scan whole
    entries))
  | (x, rep) :: rest ->
      bucket_unique whole rest entries r;
      if rep === r && not (x === r) then (
        Bridge.emit_def (bucket source r) r (Bridge.flatten (scan whole
          entries));
        B.unique_def (Bridge.emit (bucket source r) r (Bridge.flatten (scan
          whole entries)));
        emit_contains (bucket rest r) r (Bridge.flatten (scan whole entries)) x;
        bucket_mem rest r x; B.contains_def rest x;
        scan_lookup whole entries x; Bridge.flatten_lookup (scan whole entries)
          x;
        B.contains_def (Bridge.flatten (scan whole entries)) x)
      else ())

let rec (scan_unique @ total) :
    (whole : ('a : logical_data) B.bindings) @ immutable ->
    (entries : 'a B.bindings) @ immutable ->
    {u : unit | if B.valid whole && B.unique entries && Bridge.observed whole
      entries
      then B.unique (Bridge.flatten (scan whole entries)) else true} @ ghost =
    fun whole entries -> ghost_ (
  scan_def whole entries; B.unique_def entries; Bridge.observed_def whole
    entries;
  match entries with
  | [] -> Bridge.flatten_def (scan whole entries);
      B.unique_def (Bridge.flatten (scan whole entries))
  | (x, rep) :: rest ->
      scan_unique whole rest;
      if x === rep then (
        roots_lookup rest x; B.contains_def rest x;
        B.valid_def whole; Bridge.observed_self whole;
        bucket_unique whole whole rest x;
        Bridge.flatten_def (scan whole entries);
        B.unique_def (Bridge.flatten (scan whole entries));
        emit_contains (bucket whole x) x (Bridge.flatten (scan whole rest)) x;
        bucket_mem whole x x;
        scan_lookup whole rest x; Bridge.flatten_lookup (scan whole rest) x;
        B.contains_def (Bridge.flatten (scan whole rest)) x)
      else ())

let (group_valid @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    {u : unit | if B.valid p then C.distinct (C.members (group p)) else true} @
      ghost =
    fun p -> ghost_ (
  group_def p; B.valid_def p; Bridge.observed_self p; scan_unique p p;
  Bridge.flatten_unique (group p); ())

let (group_observation @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | if B.valid p then C.contains (group p) q === B.contains p q &&
      C.representative (group p) q === B.representative p q else true} @ ghost =
    fun p q -> ghost_ (
  group_lookup p q; C.contains_def (group p) q; B.contains_def p q;
  C.representative_def (group p) q; B.representative_def p q; ())

let[@def] rec remove (p : ('a : logical_data) B.bindings @ immutable)
    (x : 'a @ immutable) = ghost_ (
  match p with
  | [] -> []
  | (q, r) :: rest -> if q === x then remove rest x else (q, r) :: remove rest
    x)

let[@def] rec included (p : ('a : logical_data) B.bindings @ immutable)
    (q : 'a B.bindings @ immutable) = ghost_ (
  match p with
  | [] -> true
  | (x, _) :: rest -> B.contains q x && included rest q)

let rec (remove_lookup @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (x : 'a) @ immutable ->
    (q : 'a) @ immutable ->
    {u : unit | B.lookup (remove p x) q ===
      (if q === x then None else B.lookup p q)} @ ghost = fun p x q -> ghost_ (
  remove_def p x; B.lookup_def p q; B.lookup_def (remove p x) q;
  match p with [] -> () | _ :: rest -> remove_lookup rest x q)

let (remove_contains @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (x : 'a) @ immutable ->
    (q : 'a) @ immutable ->
    {u : unit | B.contains (remove p x) q === (not (q === x) && B.contains p q)}
    @ ghost = fun p x q -> ghost_ (
  remove_lookup p x q; B.contains_def p q; B.contains_def (remove p x) q; ())

let rec (remove_absent @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if not (B.contains p x) then remove p x === p else true} @ ghost
      =
    fun p x -> ghost_ (
  remove_def p x; B.contains_def p x; B.lookup_def p x;
  match p with
  | [] -> ()
  | _ :: rest -> B.contains_def rest x; remove_absent rest x)

let rec (remove_unique @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if B.unique p then B.unique (remove p x) else true} @ ghost =
    fun p x -> ghost_ (
  remove_def p x; B.unique_def p; B.unique_def (remove p x);
  match p with
  | [] -> ()
  | (q, _) :: rest -> remove_unique rest x; remove_contains rest x q)

let rec (remove_size @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if B.unique p && B.contains p x then
      B.size p = Bigint.add 1Z (B.size (remove p x)) else true} @ ghost =
    fun p x -> ghost_ (
  remove_def p x; B.unique_def p; B.size_def p;
  B.size_def (remove p x); B.contains_def p x; B.lookup_def p x;
  match p with
  | [] -> ()
  | (q, _) :: rest ->
      B.contains_def rest x; remove_size rest x; remove_absent rest x)

let rec (included_contains @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if included p q && B.contains p x then B.contains q x else true}
    @ ghost = fun p q x -> ghost_ (
  included_def p q; B.contains_def p x; B.lookup_def p x;
  match p with
  | [] -> ()
  | _ :: rest -> B.contains_def rest x; included_contains rest q x)

let rec (included_remove @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if included p q then included (remove p x) (remove q x) else
      true}
    @ ghost = fun p q x -> ghost_ (
  included_def p q; remove_def p x;
  included_def (remove p x) (remove q x);
  match p with
  | [] -> ()
  | (y, _) :: rest -> included_remove rest q x; remove_contains q x y)

let (included_remove_absent @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if included p q && not (B.contains p x) then
      included p (remove q x) else true} @ ghost = fun p q x -> ghost_ (
  included_remove p q x; remove_absent p x; ())

let (included_empty @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    {u : unit | if included p [] then p === [] else true} @ ghost = fun p ->
      ghost_ (
  included_def p []; match p with
  | [] -> ()
  | (x, _) :: _ -> B.contains_def [] x; B.lookup_def [] x)

let rec (domain_size @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable ->
    {u : unit | if B.unique p && B.unique q && included p q && included q p then
      B.size p = B.size q else true} @ ghost = fun p q -> ghost_ (
  B.unique_def p; included_def p q; B.size_def p;
  match p with
  | [] -> included_empty q; B.size_def q
  | (x, _) :: rest ->
      included_remove_absent rest q x;
      included_remove q p x; remove_def p x; remove_absent rest x;
      remove_unique q x; remove_size q x;
      domain_size rest (remove q x))

let rec (included_cons @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (entries : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    (r : 'a) @ immutable ->
    {u : unit | if included entries p then included entries ((x, r) :: p) else
      true}
    @ ghost = fun p entries x r -> ghost_ (
  included_def entries p; included_def entries ((x, r) :: p);
  match entries with
  | [] -> ()
  | (q, _) :: rest -> B.contains_cons p x r q; included_cons p rest x r)

let rec (included_self @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    {u : unit | included p p} @ ghost = fun p -> ghost_ (
  included_def p p;
  match p with
  | [] -> ()
  | (x, r) :: rest -> included_self rest; included_cons rest rest x r;
      B.contains_cons rest x r x)

let rec (group_included @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (entries : 'a B.bindings) @ immutable ->
    {u : unit | if B.valid p then included entries p ===
      included entries (Bridge.flatten (group p)) else true} @ ghost =
    fun p entries -> ghost_ (
  included_def entries p; included_def entries (Bridge.flatten (group p));
  match entries with
  | [] -> ()
  | (x, _) :: rest -> group_observation p x; Bridge.flatten_observation (group
    p) x;
      group_included p rest)

let (group_size @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    {u : unit | if B.valid p then C.size (group p) = B.size p else true} @ ghost
      =
    fun p -> ghost_ (
  group_valid p; Bridge.flatten_valid (group p); B.valid_def p;
  B.valid_def (Bridge.flatten (group p));
  included_self p; included_self (Bridge.flatten (group p));
  group_included p p; group_included p (Bridge.flatten (group p));
  domain_size p (Bridge.flatten (group p)); Bridge.flatten_size (group p); ())

let rec (group_agree @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (entries : 'a B.bindings) @ immutable ->
    {u : unit | if B.valid p then B.agree p (Bridge.flatten (group p)) entries
      else true}
    @ ghost = fun p entries -> ghost_ (
  B.agree_def p (Bridge.flatten (group p)) entries;
  match entries with
  | [] -> ()
  | (x, _) :: rest -> group_lookup p x; Bridge.flatten_lookup (group p) x;
      group_agree p rest)

let (group_same @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    {u : unit | if B.valid p then B.same p (Bridge.flatten (group p)) else true}
    @ ghost = fun p -> ghost_ (
  group_size p; Bridge.flatten_size (group p);
  group_agree p p; group_agree p (Bridge.flatten (group p));
  B.same_def p (Bridge.flatten (group p)); ())

let (group_contains @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | if B.valid p then C.contains (group p) q === B.contains p q else
      true}
    @ ghost = fun p q -> ghost_ (group_observation p q)

let (group_root @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable -> (q : 'a) @ immutable ->
    {u : unit | if B.valid p then C.representative (group p) q ===
      B.representative p q
      else true} @ ghost = fun p q -> ghost_ (group_observation p q)

module T = Vox_partition_transport_proof

let (group_same_relation @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable ->
    {u : unit | if B.valid p && B.valid q && B.same p q then
      C.same (group p) (group q) else true} @ ghost = fun p q -> ghost_ (
  group_same p; group_same q;
  T.same_symmetric p (Bridge.flatten (group p));
  T.same_transitive (Bridge.flatten (group p)) p q;
  T.same_transitive (Bridge.flatten (group p)) q (Bridge.flatten (group q));
  Bridge.flatten_same (group p) (group q); ())

let (group_added @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    {u : unit | if B.added p q x then C.added (group p) (group q) x else true}
    @ ghost = fun p q x -> ghost_ (
  B.added_def p q x;
  if B.added p q x then (
    group_valid p; group_valid q; group_same p; group_same q;
    Bridge.flatten_valid (group p); Bridge.flatten_valid (group q);
    T.added_congruent p (Bridge.flatten (group p)) q (Bridge.flatten (group q))
      x;
    Bridge.flatten_added (group p) (group q) x)
  else ())

let (group_joined @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    (q : 'a B.bindings) @ immutable -> (x : 'a) @ immutable ->
    (y : 'a) @ immutable -> (r : 'a) @ immutable ->
    {u : unit | if B.joined p q x y r then C.joined (group p) (group q) x y r
      else true}
    @ ghost = fun p q x y r -> ghost_ (
  B.joined_def p q x y r;
  if B.joined p q x y r then (
    group_valid p; group_valid q; group_same p; group_same q;
    Bridge.flatten_valid (group p); Bridge.flatten_valid (group q);
    T.joined_congruent p (Bridge.flatten (group p)) q (Bridge.flatten (group q))
      x y r;
    Bridge.flatten_joined (group p) (group q) x y r)
  else ())

let (group_empty @ total) :
    (p : ('a : logical_data) B.bindings) @ immutable ->
    {u : unit | if B.empty p then C.empty (group p) else true} @ ghost =
    fun p -> ghost_ (
  B.empty_def p; group_def p; scan_def p p; C.empty_def (group p); ())
