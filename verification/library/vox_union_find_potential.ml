module A = Vox_ackermann

let[@def] rec level (cap : Bigint.t) (rank : Bigint.t)
    (parent : Bigint.t) (bound : Bigint.t) =
  if bound <= 0Z then 0Z
  else if A.iter cap bound 1Z rank <= parent then bound
  else level cap rank parent (Bigint.sub bound 1Z)
[@@decreases let bound : Bigint.t = bound in
  if bound > 0Z then bound else 0Z]

let[@def] rec index (cap : Bigint.t) (lev : Bigint.t) (rank : Bigint.t)
    (parent : Bigint.t) (bound : Bigint.t) =
  if bound <= 1Z then 1Z
  else if A.iter cap lev bound rank <= parent then bound
  else index cap lev rank parent (Bigint.sub bound 1Z)
[@@decreases let bound : Bigint.t = bound in
  if bound > 0Z then bound else 0Z]

let (next_level @ total) : (cap : Bigint.t) -> (lev : Bigint.t) ->
    (rank : Bigint.t) ->
    {u : unit | cap >= 1Z && lev >= 0Z && 0Z <= rank && rank < cap} ->
    {u : unit | A.iter cap (Bigint.add lev 1Z) 1Z rank =
      A.iter cap lev (Bigint.add rank 1Z) rank} = fun cap lev rank premise ->
  let next = Bigint.add lev 1Z in
  A.iter_def cap next 1Z rank;
  let repetitions = Bigint.add rank 1Z in
  A.bounds cap lev repetitions rank;
  let value = A.iter cap lev repetitions rank in
  A.iter_def cap next 0Z value;
  let u = () in u

let rec (level_bounds @ total) : (cap : Bigint.t) -> (rank : Bigint.t) ->
    (parent : Bigint.t) -> (bound : Bigint.t) ->
    {u : unit | 1Z <= rank && rank < parent && parent < cap &&
      0Z <= bound &&
      A.iter cap (Bigint.add bound 1Z) 1Z rank > parent} ->
    {u : unit | 0Z <= level cap rank parent bound &&
      level cap rank parent bound <= bound &&
      A.iter cap (level cap rank parent bound) 1Z rank <= parent &&
      A.iter cap (Bigint.add (level cap rank parent bound) 1Z) 1Z rank > parent}
      = fun cap rank parent bound premise ->
  level_def cap rank parent bound;
  if bound <= 0Z then (
    A.iter_def cap 0Z 1Z rank;
    A.minimum_def cap (Bigint.add rank 1Z);
    let u = () in u)
  else if A.iter cap bound 1Z rank <= parent then
    (let u = () in u)
  else (
    let next = Bigint.sub bound 1Z in
    let u = () in level_bounds cap rank parent next (u);
    let u = () in u)
[@@decreases let bound : Bigint.t = bound in bound]

let rec (index_bounds @ total) : (cap : Bigint.t) -> (lev : Bigint.t) ->
    (rank : Bigint.t) -> (parent : Bigint.t) -> (bound : Bigint.t) ->
    {u : unit | 1Z <= rank && rank < parent && parent < cap && lev >= 0Z &&
      1Z <= bound && bound <= rank && A.iter cap lev 1Z rank <= parent &&
      A.iter cap lev (Bigint.add bound 1Z) rank > parent} ->
    {u : unit | 1Z <= index cap lev rank parent bound &&
      index cap lev rank parent bound <= bound &&
      A.iter cap lev (index cap lev rank parent bound) rank <= parent &&
      A.iter cap lev (Bigint.add (index cap lev rank parent bound) 1Z) rank
        > parent} = fun cap lev rank parent bound premise ->
  index_def cap lev rank parent bound;
  if bound <= 1Z || A.iter cap lev bound rank <= parent then
    (let u = () in u)
  else (
    let next = Bigint.sub bound 1Z in
    let u = () in index_bounds cap lev rank parent next (u);
    let u = () in u)
[@@decreases let bound : Bigint.t = bound in bound]

let[@def] phi (alpha : Bigint.t) (rank : Bigint.t)
    (lev : Bigint.t) (idx : Bigint.t) =
  Bigint.sub (Bigint.mul (Bigint.sub alpha lev) rank) idx

let (nonnegative @ total) : (alpha : Bigint.t) -> (rank : Bigint.t) ->
    (lev : Bigint.t) -> (idx : Bigint.t) ->
    {u : unit | rank >= 1Z && 0Z <= lev && lev < alpha &&
      1Z <= idx && idx <= rank} ->
    {u : unit | 0Z <= phi alpha rank lev idx &&
      phi alpha rank lev idx <= Bigint.mul alpha rank} =
    fun alpha rank lev idx premise ->
  phi_def alpha rank lev idx;
  let u = () in u

let (decrease @ total) : (alpha : Bigint.t) -> (rank : Bigint.t) ->
    (old_level : Bigint.t) -> (old_index : Bigint.t) ->
    (new_level : Bigint.t) -> (new_index : Bigint.t) ->
    {u : unit | rank >= 1Z && 1Z <= old_index && old_index <= rank &&
      1Z <= new_index && new_index <= rank &&
      (old_level < new_level ||
        (old_level = new_level && old_index <= new_index))} ->
    {u : unit | phi alpha rank new_level new_index <=
      phi alpha rank old_level old_index &&
      (if old_level < new_level || old_index < new_index then
        Bigint.add (phi alpha rank new_level new_index) 1Z <=
          phi alpha rank old_level old_index else true)} =
    fun alpha rank old_level old_index new_level new_index premise ->
  phi_def alpha rank old_level old_index;
  phi_def alpha rank new_level new_index;
  let u = () in u

let rec (level_at_least @ total) : (cap : Bigint.t) -> (rank : Bigint.t) ->
    (parent : Bigint.t) -> (bound : Bigint.t) -> (witness : Bigint.t) ->
    {u : unit | 0Z <= witness && witness <= bound &&
      A.iter cap witness 1Z rank <= parent} ->
    {u : unit | witness <= level cap rank parent bound} =
    fun cap rank parent bound witness premise ->
  level_def cap rank parent bound;
  if bound <= 0Z || A.iter cap bound 1Z rank <= parent then
    (let u = () in u)
  else (
    let next = Bigint.sub bound 1Z in
    let u = () in level_at_least cap rank parent next witness (u);
    let u = () in u)
[@@decreases let bound : Bigint.t = bound in bound]

let rec (index_at_least @ total) : (cap : Bigint.t) -> (lev : Bigint.t) ->
    (rank : Bigint.t) -> (parent : Bigint.t) -> (bound : Bigint.t) ->
    (witness : Bigint.t) ->
    {u : unit | 1Z <= witness && witness <= bound &&
      A.iter cap lev witness rank <= parent} ->
    {u : unit | witness <= index cap lev rank parent bound} =
    fun cap lev rank parent bound witness premise ->
  index_def cap lev rank parent bound;
  if bound <= 1Z || A.iter cap lev bound rank <= parent then
    (let u = () in u)
  else (
    let next = Bigint.sub bound 1Z in
    let u = () in index_at_least cap lev rank parent next witness (u);
    let u = () in u)
[@@decreases let bound : Bigint.t = bound in bound]

let[@def] node_level (cap : Bigint.t) (alpha : Bigint.t)
    (rank : Bigint.t) (parent : Bigint.t) =
  level cap rank parent (Bigint.sub alpha 1Z)
let[@def] node_index (cap : Bigint.t) (alpha : Bigint.t)
    (rank : Bigint.t) (parent : Bigint.t) =
  index cap (node_level cap alpha rank parent) rank parent rank
let[@def] node_phi (cap : Bigint.t) (alpha : Bigint.t)
    (rank : Bigint.t) (parent : Bigint.t) =
  if rank <= 0Z then 0Z else
    phi alpha rank (node_level cap alpha rank parent)
      (node_index cap alpha rank parent)

let (analyze @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (rank : Bigint.t) -> (parent : Bigint.t) ->
    {u : unit | 1Z <= rank && rank < parent && parent < cap &&
      alpha >= 1Z && A.iter cap alpha 1Z 1Z >= cap} ->
    {u : unit | let l = node_level cap alpha rank parent in
      let i = node_index cap alpha rank parent in
      0Z <= l && l < alpha && 1Z <= i && i <= rank &&
      A.iter cap l 1Z rank <= parent &&
      A.iter cap (Bigint.add l 1Z) 1Z rank > parent &&
      A.iter cap l i rank <= parent &&
      A.iter cap l (Bigint.add i 1Z) rank > parent &&
      0Z <= node_phi cap alpha rank parent &&
      node_phi cap alpha rank parent <= Bigint.mul alpha rank} =
    fun cap alpha rank parent premise ->
  let one = 1Z in
  let u = () in
  let monotone : {u : unit | cap >= 1Z && alpha >= 0Z &&
    0Z <= one && one <= cap && 0Z <= one && one <= rank && rank <= cap} =
    u in
  A.monotone cap alpha one one one rank monotone;
  let bound = Bigint.sub alpha 1Z in
  let u = () in level_bounds cap rank parent bound (u);
  node_level_def cap alpha rank parent;
  let lev = node_level cap alpha rank parent in
  let u = () in next_level cap lev rank (u);
  let u = () in index_bounds cap lev rank parent rank (u);
  node_index_def cap alpha rank parent;
  let idx = node_index cap alpha rank parent in
  let u = () in nonnegative alpha rank lev idx (u);
  node_phi_def cap alpha rank parent;
  let u = () in u

let (compression @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (rank : Bigint.t) -> (old_parent : Bigint.t) -> (new_parent : Bigint.t) ->
    {u : unit | 1Z <= rank && rank < old_parent && old_parent <= new_parent &&
      new_parent < cap && alpha >= 1Z && A.iter cap alpha 1Z 1Z >= cap} ->
    {u : unit | node_level cap alpha rank old_parent <=
      node_level cap alpha rank new_parent &&
      node_phi cap alpha rank new_parent <= node_phi cap alpha rank old_parent &&
      (if A.iter cap (node_level cap alpha rank old_parent)
        (Bigint.add (node_index cap alpha rank old_parent) 1Z) rank <= new_parent
       then Bigint.add (node_phi cap alpha rank new_parent) 1Z <=
         node_phi cap alpha rank old_parent else true)} =
    fun cap alpha rank old_parent new_parent premise ->
  let u = () in analyze cap alpha rank old_parent (u);
  let u = () in analyze cap alpha rank new_parent (u);
  let old_level = node_level cap alpha rank old_parent in
  let old_index = node_index cap alpha rank old_parent in
  let new_level = node_level cap alpha rank new_parent in
  let new_index = node_index cap alpha rank new_parent in
  node_level_def cap alpha rank old_parent;
  node_level_def cap alpha rank new_parent;
  node_index_def cap alpha rank old_parent;
  node_index_def cap alpha rank new_parent;
  node_phi_def cap alpha rank old_parent;
  node_phi_def cap alpha rank new_parent;
  let bound = Bigint.sub alpha 1Z in
  let u = () in level_at_least cap rank new_parent bound old_level (u);
  if old_level < new_level then (
    let u = () in
    decrease alpha rank old_level old_index new_level new_index (u);
    let u = () in u)
  else (
    let u = () in
    index_at_least cap old_level rank new_parent rank old_index (u);
    if A.iter cap old_level (Bigint.add old_index 1Z) rank <= new_parent then (
      let u = () in next_level cap old_level rank (u);
      let next = Bigint.add old_index 1Z in
      let u = () in
      index_at_least cap old_level rank new_parent rank next (u);
      let u = () in
      decrease alpha rank old_level old_index new_level new_index (u);
      let u = () in u)
    else (
      let u = () in
      decrease alpha rank old_level old_index new_level new_index (u);
      let u = () in u))

let (repeated_level @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (rank : Bigint.t) -> (parent : Bigint.t) ->
    (later_rank : Bigint.t) -> (later_parent : Bigint.t) ->
    (root : Bigint.t) ->
    {u : unit | 1Z <= rank && rank < parent && parent <= later_rank &&
      later_rank < later_parent && later_parent <= root && root < cap &&
      alpha >= 1Z && A.iter cap alpha 1Z 1Z >= cap &&
      node_level cap alpha rank parent =
        node_level cap alpha later_rank later_parent} ->
    {u : unit | Bigint.add (node_phi cap alpha rank root) 1Z <=
      node_phi cap alpha rank parent} =
    fun cap alpha rank parent later_rank later_parent root premise ->
  let u = () in analyze cap alpha rank parent (u);
  let u = () in analyze cap alpha later_rank later_parent (u);
  let lev = node_level cap alpha rank parent in
  let idx = node_index cap alpha rank parent in
  let one = 1Z in
  let middle = A.iter cap lev idx rank in
  A.bounds cap lev idx rank;
  let u = () in
  let admissible : {u : unit | cap >= 1Z && lev >= 0Z &&
    0Z <= one && one <= cap && 0Z <= middle && middle <= later_rank &&
    later_rank <= cap} = u in
  A.monotone cap lev one one middle later_rank admissible;
  let u = () in
  let composition : {u : unit | cap >= 1Z && lev >= 0Z &&
    0Z <= idx && 0Z <= one && Bigint.add idx one <= cap &&
    0Z <= rank && rank <= cap} = u in
  A.compose cap lev idx one rank composition;
  let u = () in compression cap alpha rank parent root (u);
  let u = () in u

let rec (level_cap @ total) : (small : Bigint.t) -> (large : Bigint.t) ->
    (rank : Bigint.t) -> (parent : Bigint.t) -> (bound : Bigint.t) ->
    {u : unit | 1Z <= rank && rank < parent && parent < small &&
      small <= large && 0Z <= bound} ->
    {u : unit | level small rank parent bound = level large rank parent bound} =
    fun small large rank parent bound premise ->
  level_def small rank parent bound; level_def large rank parent bound;
  if bound > 0Z then (
    let one = 1Z in
    let u = () in A.coherent small large bound one rank (u);
    A.minimum_def small (A.iter large bound 1Z rank);
    if A.iter small bound 1Z rank > parent then (
      let next = Bigint.sub bound 1Z in
      let u = () in level_cap small large rank parent next (u));
    let u = () in u)
  else let u = () in u
[@@decreases let bound : Bigint.t = bound in bound]

let rec (index_cap @ total) : (small : Bigint.t) -> (large : Bigint.t) ->
    (lev : Bigint.t) -> (rank : Bigint.t) -> (parent : Bigint.t) ->
    (bound : Bigint.t) ->
    {u : unit | 1Z <= rank && rank < parent && parent < small &&
      small <= large && lev >= 0Z && 1Z <= bound && bound <= rank} ->
    {u : unit | index small lev rank parent bound =
      index large lev rank parent bound} = fun small large lev rank parent bound premise ->
  index_def small lev rank parent bound; index_def large lev rank parent bound;
  if bound > 1Z then (
    let u = () in A.coherent small large lev bound rank (u);
    A.minimum_def small (A.iter large lev bound rank);
    if A.iter small lev bound rank > parent then (
      let next = Bigint.sub bound 1Z in
      let u = () in index_cap small large lev rank parent next (u));
    let u = () in u)
  else let u = () in u
[@@decreases let bound : Bigint.t = bound in bound]

let (reparameterize @ total) : (small : Bigint.t) -> (large : Bigint.t) ->
    (a : Bigint.t) -> (b : Bigint.t) -> (rank : Bigint.t) -> (parent : Bigint.t) ->
    {u : unit | 0Z <= rank && rank < parent && parent < small &&
      small <= large && 1Z <= a && a <= b && b <= Bigint.add a 1Z &&
      A.iter small a 1Z 1Z >= small} ->
    {u : unit | node_phi large b rank parent = Bigint.add
      (node_phi small a rank parent) (Bigint.mul (Bigint.sub b a) rank)} =
    fun small large a b rank parent premise ->
  node_phi_def small a rank parent; node_phi_def large b rank parent;
  if rank > 0Z then (
    let one = 1Z in
    let u = () in analyze small a rank parent (u);
    let u = () in A.monotone small a one one one rank (u);
    let u = () in A.coherent small large a one rank (u);
    A.minimum_def small (A.iter large a 1Z rank);
    let old_bound = Bigint.sub a 1Z in
    let u = () in level_cap small large rank parent old_bound (u);
    node_level_def small a rank parent; node_level_def large b rank parent;
    if b > a then level_def large rank parent a;
    let lev = node_level small a rank parent in
    let u = () in index_cap small large lev rank parent rank (u);
    node_index_def small a rank parent; node_index_def large b rank parent;
    phi_def a rank lev (node_index small a rank parent);
    phi_def b rank lev (node_index large b rank parent);
    let u = () in u)
  else let u = () in u
