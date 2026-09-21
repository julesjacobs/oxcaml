let[@def] minimum (x : Bigint.t) (y : Bigint.t) =
  if x < y then x else y

let[@def] rec iter (cap : Bigint.t) (level : Bigint.t)
    (count : Bigint.t) (start : Bigint.t) =
  if cap < 1Z || level < 0Z || count < 0Z || count > cap ||
    start < 0Z || start > cap then 0Z
  else if count <= 0Z || start >= cap then start
  else if level <= 0Z then minimum cap (Bigint.add start count)
  else
    let next = iter cap (Bigint.sub level 1Z) (Bigint.add start 1Z) start in
    iter cap level (Bigint.sub count 1Z) next
[@@decreases let cap : Bigint.t = cap in
  let level : Bigint.t = level in let count : Bigint.t = count in
  let start : Bigint.t = start in if cap >= 1Z && level >= 0Z && count >= 0Z &&
  count <= cap && start >= 0Z && start <= cap
  then Bigint.add (Bigint.mul level (Bigint.add cap 1Z)) count else 0Z]

let rec (bounds @ total) : (cap : Bigint.t) -> (level : Bigint.t) ->
    (count : Bigint.t) -> (start : Bigint.t) ->
    {u : unit | if cap >= 1Z && level >= 0Z &&
      0Z <= count && count <= cap && 0Z <= start && start <= cap
      then start <= iter cap level count start &&
        iter cap level count start <= cap else true} =
    fun cap level count start ->
  iter_def cap level count start;
  if cap < 1Z || level < 0Z || count < 0Z || count > cap ||
    start < 0Z || start > cap then (let u = () in refine_ u)
  else if count <= 0Z || start >= cap then (let u = () in refine_ u)
  else if level <= 0Z then (
    minimum_def cap (Bigint.add start count);
    let u = () in refine_ u)
  else (
    let lower = Bigint.sub level 1Z in
    let repetitions = Bigint.add start 1Z in
    bounds cap lower repetitions start;
    let next = iter cap lower repetitions start in
    bounds cap level (Bigint.sub count 1Z) next;
    let u = () in refine_ u)
[@@decreases let cap : Bigint.t = cap in
  let level : Bigint.t = level in let count : Bigint.t = count in
  let start : Bigint.t = start in if cap >= 1Z && level >= 0Z && count >= 0Z &&
  count <= cap && start >= 0Z && start <= cap
  then Bigint.add (Bigint.mul level (Bigint.add cap 1Z)) count else 0Z]

let rec (monotone @ total) : (cap : Bigint.t) -> (level : Bigint.t) ->
    (small : Bigint.t) -> (large : Bigint.t) ->
    (x : Bigint.t) -> (y : Bigint.t) ->
    {u : unit | cap >= 1Z && level >= 0Z &&
      0Z <= small && small <= large && large <= cap &&
      0Z <= x && x <= y && y <= cap} ->
    {u : unit | iter cap level small x <= iter cap level large y} =
    fun cap level small large x y premise ->
  let refine_ premise = premise in
  bounds cap level small x;
  bounds cap level large y;
  iter_def cap level small x;
  iter_def cap level large y;
  if small <= 0Z || x >= cap || y >= cap then (
    let u = () in refine_ u)
  else if level <= 0Z then (
    minimum_def cap (Bigint.add x small);
    minimum_def cap (Bigint.add y large);
    let u = () in refine_ u)
  else (
    let lower = Bigint.sub level 1Z in
    let xt = Bigint.add x 1Z in let yt = Bigint.add y 1Z in
    let u = () in
    monotone cap lower xt yt x y (refine_ u);
    let next_x = iter cap lower xt x in
    let next_y = iter cap lower yt y in
    bounds cap lower xt x; bounds cap lower yt y;
    let s = Bigint.sub small 1Z in let l = Bigint.sub large 1Z in
    let u = () in
    monotone cap level s l next_x next_y (refine_ u);
    let u = () in refine_ u)
[@@decreases let cap : Bigint.t = cap in
  let level : Bigint.t = level in let small : Bigint.t = small in
  Bigint.add (Bigint.mul level (Bigint.add cap 1Z)) small]

let rec (growth @ total) : (cap : Bigint.t) -> (level : Bigint.t) ->
    (count : Bigint.t) -> (start : Bigint.t) ->
    {u : unit | if cap >= 1Z && level >= 0Z &&
      0Z <= count && count <= cap && 0Z <= start && start <= cap
      then minimum cap (Bigint.add start count) <=
        iter cap level count start else true} = fun cap level count start ->
  iter_def cap level count start;
  minimum_def cap (Bigint.add start count);
  if cap < 1Z || level < 0Z || count < 0Z || count > cap ||
    start < 0Z || start > cap then (let u = () in refine_ u)
  else if count <= 0Z || start >= cap || level <= 0Z then
    (let u = () in refine_ u)
  else (
    let lower = Bigint.sub level 1Z in
    let repetitions = Bigint.add start 1Z in
    growth cap lower repetitions start;
    bounds cap lower repetitions start;
    minimum_def cap (Bigint.add start repetitions);
    let next = iter cap lower repetitions start in
    let rest = Bigint.sub count 1Z in
    growth cap level rest next;
    minimum_def cap (Bigint.add next rest);
    let u = () in refine_ u)
[@@decreases let cap : Bigint.t = cap in
  let level : Bigint.t = level in let count : Bigint.t = count in
  let start : Bigint.t = start in
  if cap >= 1Z && level >= 0Z && count >= 0Z && count <= cap &&
    start >= 0Z && start <= cap
  then Bigint.add (Bigint.mul level (Bigint.add cap 1Z)) count else 0Z]

let rec (compose @ total) : (cap : Bigint.t) -> (level : Bigint.t) ->
    (first : Bigint.t) -> (second : Bigint.t) -> (start : Bigint.t) ->
    {u : unit | cap >= 1Z && level >= 0Z &&
      0Z <= first && 0Z <= second && Bigint.add first second <= cap &&
      0Z <= start && start <= cap} ->
    {u : unit | iter cap level (Bigint.add first second) start =
      iter cap level second (iter cap level first start)} =
    fun cap level first second start premise ->
  let refine_ premise = premise in
  let sum = Bigint.add first second in
  iter_def cap level sum start;
  iter_def cap level first start;
  let middle = iter cap level first start in
  bounds cap level first start;
  iter_def cap level second middle;
  if first <= 0Z || start >= cap then (let u = () in refine_ u)
  else if level <= 0Z then (
    minimum_def cap (Bigint.add start sum);
    minimum_def cap (Bigint.add start first);
    minimum_def cap (Bigint.add middle second);
    let u = () in refine_ u)
  else (
    let lower = Bigint.sub level 1Z in
    let repetitions = Bigint.add start 1Z in
    let next = iter cap lower repetitions start in
    bounds cap lower repetitions start;
    let rest = Bigint.sub first 1Z in
    let u = () in
    compose cap level rest second next (refine_ u);
    let u = () in refine_ u)
[@@decreases let first : Bigint.t = first in first]

let rec (level_growth @ total) : (cap : Bigint.t) -> (level : Bigint.t) ->
    {u : unit | cap >= 1Z && level >= 0Z} ->
    {u : unit | minimum cap (Bigint.add level 2Z) <=
      iter cap level 1Z 1Z} = fun cap level premise ->
  let refine_ premise = premise in
  iter_def cap level 1Z 1Z;
  minimum_def cap (Bigint.add level 2Z);
  if cap <= 1Z then (let u = () in refine_ u)
  else if level <= 0Z then (
    minimum_def cap 2Z; let u = () in refine_ u)
  else (
    let lower = Bigint.sub level 1Z in
    let u = () in level_growth cap lower (refine_ u);
    minimum_def cap (Bigint.add lower 2Z);
    let one = 1Z in
    let u = () in
    let admissible : {u : unit | cap >= 1Z && lower >= 0Z &&
      0Z <= one && Bigint.add one one <= cap && one <= cap} = refine_ u in
    compose cap lower one one one admissible;
    bounds cap lower 1Z 1Z;
    let middle = iter cap lower 1Z 1Z in
    growth cap lower 1Z middle;
    minimum_def cap (Bigint.add middle 1Z);
    bounds cap lower 2Z 1Z;
    let next = iter cap lower 2Z 1Z in
    iter_def cap level 0Z next;
    let u = () in refine_ u)
[@@decreases let level : Bigint.t = level in level]

let[@def] rec below (cap : Bigint.t) (level : Bigint.t) =
  if level <= 1Z then true else
    below cap (Bigint.sub level 1Z) &&
    iter cap (Bigint.sub level 1Z) 1Z 1Z < cap
[@@decreases let level : Bigint.t = level in
  if level > 0Z then level else 0Z]

let rec (search @ total) : (cap : Bigint.t) -> (level : Bigint.t) ->
    {u : unit | cap >= 1Z && 1Z <= level && level <= cap && below cap level} ->
    {k : Bigint.t | 1Z <= k && k <= cap &&
      iter cap k 1Z 1Z >= cap && below cap k} = fun cap level premise ->
  let refine_ premise = premise in
  let value = iter cap level 1Z 1Z in
  if value >= cap then refine_ level
  else (
    let u = () in level_growth cap level (refine_ u);
    minimum_def cap (Bigint.add level 2Z);
    let next = Bigint.add level 1Z in
    below_def cap next;
    let u = () in
    let refine_ result = search cap next (refine_ u) in
    refine_ result)
[@@decreases let cap : Bigint.t = cap in
  let level : Bigint.t = level in Bigint.sub cap level]

let (inverse @ total) : (capacity : {n : Bigint.t | n >= 1Z}) ->
    {k : Bigint.t | let refine_ capacity = capacity in
      1Z <= k && k <= capacity && iter capacity k 1Z 1Z >= capacity &&
      below capacity k} = fun capacity ->
  let refine_ capacity = capacity in
  let capacity : Bigint.t = capacity in
  let one = 1Z in
  below_def capacity one;
  let u = () in
  let premise : {u : unit | capacity >= 1Z && 1Z <= one &&
    one <= capacity && below capacity one} = refine_ u in
  let refine_ result = search capacity one premise in
  refine_ result

let rec (coherent @ total) : (small : Bigint.t) -> (large : Bigint.t) ->
    (level : Bigint.t) -> (count : Bigint.t) -> (start : Bigint.t) ->
    {u : unit | 1Z <= small && small <= large && level >= 0Z &&
      0Z <= count && count <= small && 0Z <= start && start <= small} ->
    {u : unit | iter small level count start =
      minimum small (iter large level count start)} =
    fun small large level count start premise ->
  let refine_ premise = premise in
  iter_def small level count start; iter_def large level count start;
  bounds small level count start; bounds large level count start;
  minimum_def small (iter large level count start);
  if count <= 0Z || start >= small then let u = () in refine_ u
  else if level <= 0Z then (
    minimum_def small (Bigint.add start count);
    minimum_def large (Bigint.add start count);
    let u = () in refine_ u)
  else (
    let lower = Bigint.sub level 1Z in
    let repetitions = Bigint.add start 1Z in
    let u = () in coherent small large lower repetitions start (refine_ u);
    let a = iter small lower repetitions start in
    let b = iter large lower repetitions start in
    bounds small lower repetitions start; bounds large lower repetitions start;
    minimum_def small b;
    let rest = Bigint.sub count 1Z in
    bounds small level rest a; bounds large level rest b;
    if b >= small then (
      iter_def small level rest a;
      let u = () in refine_ u)
    else (
      let u = () in coherent small large level rest b (refine_ u);
      minimum_def small (iter large level rest b);
      let u = () in refine_ u))
[@@decreases let small : Bigint.t = small in
  let level : Bigint.t = level in let count : Bigint.t = count in
  Bigint.add (Bigint.mul level (Bigint.add small 1Z)) count]
