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
  let refine_ premise = premise in
  let next = Bigint.add lev 1Z in
  A.iter_def cap next 1Z rank;
  let repetitions = Bigint.add rank 1Z in
  A.bounds cap lev repetitions rank;
  let value = A.iter cap lev repetitions rank in
  A.iter_def cap next 0Z value;
  let u = () in refine_ u

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
  let refine_ premise = premise in
  level_def cap rank parent bound;
  if bound <= 0Z then (
    A.iter_def cap 0Z 1Z rank;
    A.minimum_def cap (Bigint.add rank 1Z);
    let u = () in refine_ u)
  else if A.iter cap bound 1Z rank <= parent then
    (let u = () in refine_ u)
  else (
    let next = Bigint.sub bound 1Z in
    let u = () in level_bounds cap rank parent next (refine_ u);
    let u = () in refine_ u)
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
  let refine_ premise = premise in
  index_def cap lev rank parent bound;
  if bound <= 1Z || A.iter cap lev bound rank <= parent then
    (let u = () in refine_ u)
  else (
    let next = Bigint.sub bound 1Z in
    let u = () in index_bounds cap lev rank parent next (refine_ u);
    let u = () in refine_ u)
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
  let refine_ premise = premise in phi_def alpha rank lev idx;
  let u = () in refine_ u

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
  let refine_ premise = premise in
  phi_def alpha rank old_level old_index;
  phi_def alpha rank new_level new_index;
  let u = () in refine_ u

let rec (level_at_least @ total) : (cap : Bigint.t) -> (rank : Bigint.t) ->
    (parent : Bigint.t) -> (bound : Bigint.t) -> (witness : Bigint.t) ->
    {u : unit | 0Z <= witness && witness <= bound &&
      A.iter cap witness 1Z rank <= parent} ->
    {u : unit | witness <= level cap rank parent bound} =
    fun cap rank parent bound witness premise ->
  let refine_ premise = premise in
  level_def cap rank parent bound;
  if bound <= 0Z || A.iter cap bound 1Z rank <= parent then
    (let u = () in refine_ u)
  else (
    let next = Bigint.sub bound 1Z in
    let u = () in level_at_least cap rank parent next witness (refine_ u);
    let u = () in refine_ u)
[@@decreases let bound : Bigint.t = bound in bound]

let rec (index_at_least @ total) : (cap : Bigint.t) -> (lev : Bigint.t) ->
    (rank : Bigint.t) -> (parent : Bigint.t) -> (bound : Bigint.t) ->
    (witness : Bigint.t) ->
    {u : unit | 1Z <= witness && witness <= bound &&
      A.iter cap lev witness rank <= parent} ->
    {u : unit | witness <= index cap lev rank parent bound} =
    fun cap lev rank parent bound witness premise ->
  let refine_ premise = premise in
  index_def cap lev rank parent bound;
  if bound <= 1Z || A.iter cap lev bound rank <= parent then
    (let u = () in refine_ u)
  else (
    let next = Bigint.sub bound 1Z in
    let u = () in index_at_least cap lev rank parent next witness (refine_ u);
    let u = () in refine_ u)
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
  let refine_ premise = premise in
  let one = 1Z in
  let u = () in
  let monotone : {u : unit | cap >= 1Z && alpha >= 0Z &&
    0Z <= one && one <= cap && 0Z <= one && one <= rank && rank <= cap} =
    refine_ u in
  A.monotone cap alpha one one one rank monotone;
  let bound = Bigint.sub alpha 1Z in
  let u = () in level_bounds cap rank parent bound (refine_ u);
  node_level_def cap alpha rank parent;
  let lev = node_level cap alpha rank parent in
  let u = () in next_level cap lev rank (refine_ u);
  let u = () in index_bounds cap lev rank parent rank (refine_ u);
  node_index_def cap alpha rank parent;
  let idx = node_index cap alpha rank parent in
  let u = () in nonnegative alpha rank lev idx (refine_ u);
  node_phi_def cap alpha rank parent;
  let u = () in refine_ u

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
  let refine_ premise = premise in
  let u = () in analyze cap alpha rank old_parent (refine_ u);
  let u = () in analyze cap alpha rank new_parent (refine_ u);
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
  let u = () in level_at_least cap rank new_parent bound old_level (refine_ u);
  if old_level < new_level then (
    let u = () in
    decrease alpha rank old_level old_index new_level new_index (refine_ u);
    let u = () in refine_ u)
  else (
    let u = () in
    index_at_least cap old_level rank new_parent rank old_index (refine_ u);
    if A.iter cap old_level (Bigint.add old_index 1Z) rank <= new_parent then (
      let u = () in next_level cap old_level rank (refine_ u);
      let next = Bigint.add old_index 1Z in
      let u = () in
      index_at_least cap old_level rank new_parent rank next (refine_ u);
      let u = () in
      decrease alpha rank old_level old_index new_level new_index (refine_ u);
      let u = () in refine_ u)
    else (
      let u = () in
      decrease alpha rank old_level old_index new_level new_index (refine_ u);
      let u = () in refine_ u))

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
  let refine_ premise = premise in
  let u = () in analyze cap alpha rank parent (refine_ u);
  let u = () in analyze cap alpha later_rank later_parent (refine_ u);
  let lev = node_level cap alpha rank parent in
  let idx = node_index cap alpha rank parent in
  let one = 1Z in
  let middle = A.iter cap lev idx rank in
  A.bounds cap lev idx rank;
  let u = () in
  let admissible : {u : unit | cap >= 1Z && lev >= 0Z &&
    0Z <= one && one <= cap && 0Z <= middle && middle <= later_rank &&
    later_rank <= cap} = refine_ u in
  A.monotone cap lev one one middle later_rank admissible;
  let u = () in
  let composition : {u : unit | cap >= 1Z && lev >= 0Z &&
    0Z <= idx && 0Z <= one && Bigint.add idx one <= cap &&
    0Z <= rank && rank <= cap} = refine_ u in
  A.compose cap lev idx one rank composition;
  let u = () in compression cap alpha rank parent root (refine_ u);
  let u = () in refine_ u
