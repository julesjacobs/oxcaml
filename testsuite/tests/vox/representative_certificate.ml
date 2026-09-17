open Copy_spec
open Level_spec
open Level_unifier_spec
module R = Representative_level
module E = Effective_level

type certificate = Empty | Entry of node Pref.t * R.representative * certificate [@@inductive]

let[@def] rec (listed @ total) (d : certificate @ immutable) (p : node Pref.t @ immutable) = ghost_ (
  match d with Empty -> false | Entry (x, _, rest) -> p === x || listed rest p)

let[@def] rec (head @ total) (d : certificate @ immutable) (p : node Pref.t @ immutable total) : R.representative @ immutable total = ghost_ (
  match d with Empty -> {R.root = p; path = Here}
  | Entry (x, r, rest) -> if p === x then r else head rest p)

let[@def] rec (certificate_valid @ total) (h : node Pref.heap @ immutable) (d : certificate @ immutable) = ghost_ (
  match d with Empty -> true | Entry (p, r, rest) -> (not (H.mem h p) || resolves h p r.root r.path) && certificate_valid h rest)

let rec (head_valid @ total) : (h : node Pref.heap) @ immutable -> (d : certificate) @ immutable ->
    (p : node Pref.t) @ immutable -> {u : unit | certificate_valid h d && listed d p && H.mem h p} ->
    {u : unit | resolves h p (head d p).root (head d p).path} @ ghost = fun h d p premise -> ghost_ (
      let refine_ premise = premise in certificate_valid_def h d; listed_def d p; head_def d p;
      let u = () in match d with Empty -> refine_ u | Entry (x, _, rest) ->
        if p === x then refine_ u else (head_valid h rest p (refine_ u); refine_ u))

let (level_agrees @ total) : (h : node Pref.heap) @ immutable -> (d : certificate) @ immutable ->
    (heads : E.heads) @ total -> (p : node Pref.t) @ immutable ->
    {u : unit | certificate_valid h d && listed d p && H.mem h p && E.valid_head h heads p} ->
    {u : unit | E.level h heads p === at_level h (head d p).root} @ ghost = fun h d heads p premise -> ghost_ (
      let refine_ premise = premise in let u = () in head_valid h d p (refine_ u);
      let r = head d p in Compression_path_proofs.resolution_terminal h p r.root r.path (refine_ u);
      resolves_def h p r.root r.path;
      E.valid_head_def h heads p; E.level_def h heads p; let other = heads p in
      R.unique h p r.root r.path other.root other.path (refine_ u); refine_ u)
