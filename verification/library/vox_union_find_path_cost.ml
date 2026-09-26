module A = Vox_ackermann
module P = Vox_union_find_potential
module L = Vox_union_find_levels

type edge = { rank : Bigint.t; parent : Bigint.t }

let[@def] rec chain (lower : Bigint.t) (root : Bigint.t) (edges : edge list) =
  match edges with
  | [] -> lower <= root
  | e :: tail -> lower <= e.rank && 1Z <= e.rank && e.rank < e.parent &&
      e.parent <= root && chain e.parent root tail
let[@def] rec length (edges : edge list) =
  match edges with [] -> 0Z | _ :: tail -> Bigint.add 1Z (length tail)
let[@def] rec loss (cap : Bigint.t) (alpha : Bigint.t)
    (root : Bigint.t) (edges : edge list) =
  match edges with
  | [] -> 0Z
  | e :: tail -> Bigint.add
      (Bigint.sub (P.node_phi cap alpha e.rank e.parent)
        (P.node_phi cap alpha e.rank root)) (loss cap alpha root tail)
let[@def] rec levels (cap : Bigint.t) (alpha : Bigint.t) (edges : edge list) =
  match edges with [] -> [] | e :: tail ->
    L.insert (P.node_level cap alpha e.rank e.parent) (levels cap alpha tail)

let rec (release @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (rank : Bigint.t) -> (parent : Bigint.t) -> (lower : Bigint.t) ->
    (root : Bigint.t) -> (edges : edge list) ->
    {u : unit | 1Z <= rank && rank < parent && parent <= lower &&
      root < cap && alpha >= 1Z && A.iter cap alpha 1Z 1Z >= cap &&
      chain lower root edges &&
      L.mem (P.node_level cap alpha rank parent) (levels cap alpha edges)} ->
    {u : unit | Bigint.add (P.node_phi cap alpha rank root) 1Z <=
      P.node_phi cap alpha rank parent} =
    fun cap alpha rank parent lower root edges premise ->
  let _ = premise in
  chain_def lower root edges; levels_def cap alpha edges;
  let lev = P.node_level cap alpha rank parent in
  match edges with
  | [] -> L.mem_def lev []; ()
  | e :: tail ->
    let current = P.node_level cap alpha e.rank e.parent in
    L.insert_mem current (levels cap alpha tail) lev;
    if Bigint.equal current lev then (
      let u = () in
      P.repeated_level cap alpha rank parent e.rank e.parent root (u);
      ())
    else (
      let u = () in
      release cap alpha rank parent e.parent root tail (u);
      ())

let rec (counting @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (lower : Bigint.t) -> (root : Bigint.t) -> (edges : edge list) ->
    {u : unit | root < cap && alpha >= 1Z && A.iter cap alpha 1Z 1Z >= cap &&
      chain lower root edges} ->
    {u : unit | L.range 0Z alpha (levels cap alpha edges) &&
      0Z <= loss cap alpha root edges &&
      length edges <= Bigint.add (loss cap alpha root edges)
        (L.size (levels cap alpha edges))} =
    fun cap alpha lower root edges premise ->
  let _ = premise in
  chain_def lower root edges; levels_def cap alpha edges;
  loss_def cap alpha root edges; length_def edges;
  match edges with
  | [] -> L.range_def 0Z alpha []; L.size_def []; ()
  | e :: tail ->
    let u = () in counting cap alpha e.parent root tail (u);
    let u = () in P.analyze cap alpha e.rank e.parent (u);
    let u = () in P.compression cap alpha e.rank e.parent root (u);
    let lev = P.node_level cap alpha e.rank e.parent in
    let suffix = levels cap alpha tail in
    L.insert_range 0Z alpha suffix lev;
    L.insert_size 0Z alpha lev suffix;
    if L.mem lev suffix then (
      let u = () in
      release cap alpha e.rank e.parent e.parent root tail (u);
      ())
    else ()

let (bound @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (lower : Bigint.t) -> (root : Bigint.t) -> (edges : edge list) ->
    {u : unit | root < cap && alpha >= 1Z && A.iter cap alpha 1Z 1Z >= cap &&
      chain lower root edges} ->
    {u : unit | length edges <= Bigint.add (loss cap alpha root edges) alpha}
    = fun cap alpha lower root edges premise ->
  counting cap alpha lower root edges premise;
  let _ = premise in
  L.range_size 0Z alpha (levels cap alpha edges);
  ()
