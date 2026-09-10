open Vox_sequence
open Vox_traversal
open Hof_candidates

let[@def] decode_relation : int @ immutable total ->
    (bool @ immutable -> bool) @ total = fun tag value ->
  (tag = 0 && not value) || (tag = 1 && value)

let decode : (tag : int) @ immutable ->
    {value : bool | decode_relation tag value} @ immutable total = fun tag ->
  if tag <> 0 && tag <> 1 then failwith "invalid tag"
  else
    let value = tag = 1 in
    ghost_ (decode_relation_def tag value);
    refine_ value

let decode_all (tags : int list) :
    {values : bool list | map_rel decode_relation tags values &&
      length tags === length values} =
  let f = decode in
  let refine_ values = map decode_relation f tags in
  ghost_ (map_length decode_relation tags values);
  refine_ values

let[@def] affine : Bigint.t @ immutable total ->
    (Bigint.t @ immutable total ->
      (Bigint.t @ immutable -> Bigint.t @ immutable total) @ total) @ total =
    fun a b x -> Bigint.add (Bigint.mul a x) b

let affine_map (a : Bigint.t @ total) (b : Bigint.t @ total) (xs : Bigint.t list) :
    {ys : Bigint.t list | ys === model_map (affine a b) xs} =
  let model = affine a b in
  let (f @ total) : (x : Bigint.t) @ immutable ->
      {y : Bigint.t | y === model x} @ immutable total = fun x ->
    let y = Bigint.add (Bigint.mul a x) b in
    ghost_ (affine_def a b x);
    refine_ y in
  let refine_ result = map_model model f xs in
  refine_ result

let[@def] rec all_le (bound : int) (xs : int list @ immutable total) =
  match xs with [] -> true | x :: tail -> x <= bound && all_le bound tail

let rec (all_le_weaken @ total) : (lo : int) -> (hi : int) ->
    (xs : int list) @ immutable ->
    {u : unit | if lo <= hi && all_le lo xs then all_le hi xs else true} =
    fun lo hi xs ->
  all_le_def lo xs;
  all_le_def hi xs;
  match xs with
  | [] -> let u = () in refine_ u
  | _x :: tail ->
    all_le_weaken lo hi tail;
    let u = () in refine_ u

let[@def] upper_relation : int @ immutable total ->
    (int @ immutable total -> (int @ immutable -> bool) @ total) @ total =
    fun x acc result -> x <= result && acc <= result

let[@def] upper_invariant : int @ immutable total ->
    (int list @ immutable total -> (int @ immutable -> bool) @ total) @ total =
    fun initial xs result -> initial <= result && all_le result xs

let (upper_preserved @ total) : (initial : int) ->
    (x : int) @ immutable ->
    (tail : int list) @ immutable -> (acc : int) @ immutable ->
    (result : int) @ immutable ->
    {u : unit | upper_invariant initial tail acc &&
      upper_relation x acc result} ->
    {u : unit | upper_invariant initial (x :: tail) result} =
    fun initial x tail acc result premise ->
  premise;
  let inputs = x :: tail in
  upper_invariant_def initial tail acc;
  upper_relation_def x acc result;
  all_le_weaken acc result tail;
  all_le_def result inputs;
  upper_invariant_def initial inputs result;
  let u = () in refine_ u

let (upper_base @ total) : (initial : int) ->
    {u : unit | upper_invariant initial [] initial} = fun initial ->
  let empty : int list = [] in
  all_le_def initial empty;
  upper_invariant_def initial empty initial;
  let u = () in refine_ u

let upper_separate :
    ((x : int) @ immutable -> (acc : int) @ immutable ->
      {result : int | upper_relation x acc result} @ immutable total) ->
    (xs : int list) -> (initial : int) ->
    {result : int | upper_invariant initial xs result} = fun f xs initial ->
  let inv = upper_invariant initial in
  let preserve : ((x : int) @ immutable -> (tail : int list) @ immutable ->
      (acc : int) @ immutable -> (result : int) @ immutable ->
      {u : unit | inv tail acc && upper_relation x acc result} ->
      {u : unit | inv (x :: tail) result}) @ total ghost = ghost_ (fun x tail acc result premise ->
    premise;
    let u = () in
    upper_preserved initial x tail acc result
      (refine_ u);
    refine_ u) in
  ghost_ (upper_base initial);
  let u = () in
  let refine_ result = fold_right upper_relation inv f preserve xs initial
    (refine_ u) in
  refine_ result

let upper_trace :
    ((x : int) @ immutable -> (acc : int) @ immutable ->
      {result : int | upper_relation x acc result} @ immutable total) ->
    (xs : int list) -> (initial : int) ->
    {result : int | upper_invariant initial xs result} = fun f xs initial ->
  let refine_ evidence = fold_trace upper_relation f xs initial in
  let result = evidence.value in
  ghost_ (
    let inv = upper_invariant initial in
    let (preserve @ total) : ((x : int) @ immutable -> (tail : int list) @ immutable ->
      (acc : int) @ immutable -> (result : int) @ immutable ->
      {u : unit | inv tail acc && upper_relation x acc result} ->
      {u : unit | inv (x :: tail) result}) = fun x tail acc result premise ->
    premise;
    let u = () in
    upper_preserved initial x tail acc result
      (refine_ u);
    refine_ u in
    let trace = evidence.trace in
    upper_base initial;
    let u = () in
    trace_invariant upper_relation inv preserve xs
      initial result trace (refine_ u);
    (refine_ u : {u : unit | upper_invariant initial xs result}));
  refine_ result

let upper_ih :
    ((x : int) @ immutable -> (acc : int) @ immutable ->
      {result : int | upper_relation x acc result} @ immutable total) ->
    (xs : int list) -> (initial : int) ->
    {result : int | upper_invariant initial xs result} = fun f xs initial ->
  let inv = upper_invariant initial in
  let step : (x : int) @ immutable -> (tail : int list) @ immutable ghost ->
      (acc : int) @ immutable -> {u : unit | inv tail acc} @ ghost ->
      {result : int | inv (x :: tail) result} @ immutable total =
      fun x tail acc premise ->
    premise;
    let refine_ result = f x acc in
    let u = () in
    ghost_
      (upper_preserved initial x tail acc result (refine_ u));
    refine_ result in
  ghost_ (upper_base initial);
  let u = () in
  let refine_ result = fold_right_ih inv step xs initial (refine_ u) in
  refine_ result

let choose_upper : (x : int) @ immutable -> (acc : int) @ immutable ->
    {result : int | upper_relation x acc result} @ immutable total =
    fun x acc ->
  if x = 13 then failwith "upper"
  else
    let result = if x > acc then x else if acc < 100 then acc + 1 else acc in
    ghost_ (upper_relation_def x acc result);
    refine_ result

let () =
  let tags = [0; 1; 0] in
  let refine_ values = decode_all tags in
  assert (values = [false; true; false]);
  let tags = [0; 2] in
  (match decode_all tags with
   | exception Failure message -> assert (message = "invalid tag")
   | _ -> failwith "expected invalid tag");
  let a = 3Z in
  let b = 2Z in
  let xs = [0Z; 1Z; 2Z] in
  let refine_ values = affine_map a b xs in
  assert (List.for_all2 Bigint.equal values [2Z; 5Z; 8Z]);
  let xs = [2; 5; 3] in
  let initial = 0 in
  let f = choose_upper in
  let refine_ a = upper_separate f xs initial in
  let refine_ b = upper_trace f xs initial in
  let refine_ c = upper_ih f xs initial in
  assert (a = b && b = c && c >= 5)

let rec (mapped_lower_bound @ total) :
    (r : (int @ immutable total -> int @ immutable total -> bool)) @ total ->
    ((x : int) -> (y : int) -> (fx : int) -> (fy : int) ->
      {u : unit | x <= y && r x fx && r y fy} ->
      {u : unit | fx <= fy}) @ total ->
    (xs : int list) -> (ys : int list) -> (bound : int) -> (mapped : int) ->
    {u : unit | Vox_int_sequence.all xs bound false &&
      r bound mapped && map_rel r xs ys} ->
    {u : unit | Vox_int_sequence.all ys mapped false} =
    fun r monotone xs ys bound mapped premise ->
  premise;
  map_rel_def r xs ys;
  let lower = false in
  Vox_int_sequence.all_def xs bound lower;
  Vox_int_sequence.all_def ys mapped lower;
  match xs with
  | [] -> let u = () in refine_ u
  | x :: tail ->
    match ys with
    | [] -> let u = () in refine_ u
    | y :: outputs ->
      Vox_int_sequence.accepts_def x bound lower;
      Vox_int_sequence.accepts_def y mapped lower;
      let u = () in
      let refine_ step = monotone bound x mapped y (refine_ u) in
      mapped_lower_bound r monotone tail outputs bound mapped (refine_ u);
      refine_ u

let rec (map_sorted @ total) :
    (r : (int @ immutable total -> int @ immutable total -> bool)) @ total ->
    ((x : int) -> (y : int) -> (fx : int) -> (fy : int) ->
      {u : unit | x <= y && r x fx && r y fy} ->
      {u : unit | fx <= fy}) @ total ->
    (xs : int list) -> (ys : int list) ->
    {u : unit | Vox_int_sequence.sorted xs && map_rel r xs ys} ->
    {u : unit | Vox_int_sequence.sorted ys} = fun r monotone xs ys premise ->
  premise;
  map_rel_def r xs ys;
  Vox_int_sequence.sorted_def xs;
  Vox_int_sequence.sorted_def ys;
  match xs with
  | [] -> let u = () in refine_ u
  | x :: tail ->
    match ys with
    | [] -> let u = () in refine_ u
    | y :: outputs ->
      let u = () in
      mapped_lower_bound r monotone tail outputs x y (refine_ u);
      map_sorted r monotone tail outputs (refine_ u);
      refine_ u

let[@def] clamp_relation : int @ immutable total ->
    (int @ immutable -> bool) @ total = fun x y ->
    y = (if x < 0 then 0 else x)

let clamp_partial : (x : int) @ immutable ->
    {y : int | clamp_relation x y} @ immutable total = fun x ->
  if x = 13 then failwith "clamp"
  else
    let y = if x < 0 then 0 else x in
    ghost_ (clamp_relation_def x y);
    refine_ y

let (clamp_monotone @ total) :
    (x : int) -> (y : int) -> (fx : int) -> (fy : int) ->
    {u : unit | x <= y && clamp_relation x fx && clamp_relation y fy} ->
    {u : unit | fx <= fy} = fun x y fx fy premise ->
  premise;
  clamp_relation_def x fx;
  clamp_relation_def y fy;
  let u = () in refine_ u

let sorted_clamp (xs : int list)
    (premise : {u : unit | Vox_int_sequence.sorted xs} @ ghost) :
    {ys : int list | Vox_int_sequence.sorted ys &&
      map_rel clamp_relation xs ys} =
  premise;
  let f = clamp_partial in
  let refine_ ys = map clamp_relation f xs in
  let u = () in
  ghost_ (
    let monotone = clamp_monotone in
    map_sorted clamp_relation monotone xs ys (refine_ u));
  refine_ ys

let[@def] count_until_zero : int @ immutable total ->
    (int @ immutable -> int control @ immutable total) @ total =
    fun x acc -> if x = 0 then Stop acc else Continue (acc + 1)

let count_until_step : (x : int) @ immutable -> (acc : int) @ immutable ->
    {result : int control | result === count_until_zero x acc}
      @ immutable total = fun x acc ->
  if x = 13 then failwith "visited forbidden element"
  else
    let result = if x = 0 then Stop acc else Continue (acc + 1) in
    ghost_ (count_until_zero_def x acc);
    refine_ result

let[@def] number_element : int @ immutable total ->
    (int @ immutable -> (int * int) @ immutable total) @ total =
    fun x index -> index + 1, index + x

let number_step : (x : int) @ immutable -> (index : int) @ immutable ->
    {result : int * int | result === number_element x index}
      @ immutable total = fun x index ->
  if x = 13 then failwith "numbering"
  else
    let result = index + 1, index + x in
    ghost_ (number_element_def x index);
    refine_ result

let () =
  let xs = [7; 0; 13] in
  let initial = 0 in
  let step = count_until_step in
  let refine_ n = fold_until_model count_until_zero step xs initial in
  let (n : int) = n in
  assert (n = 1);
  let xs = [7; 13; 0] in
  (match fold_until_model count_until_zero step xs initial with
   | exception Failure message -> assert (message = "visited forbidden element")
   | _ -> failwith "expected visited element");
  let xs = [4; 8; 2] in
  let initial = 10 in
  let step = number_step in
  let refine_ result = map_accum_model number_element step xs initial in
  let (result : int * int list) = result in
  assert (result = (13, [14; 19; 14]))

let[@def] rec sum_spec (xs : int list @ immutable total) =
  match xs with
  | [] -> 0Z
  | x :: tail -> Bigint.add (Bigint.of_int x) (sum_spec tail)

let[@def] sum_relation : int @ immutable total ->
    (int @ immutable total -> (int @ immutable -> bool) @ total) @ total =
    fun x acc result ->
    Bigint.equal (Bigint.of_int result)
      (Bigint.add (Bigint.of_int x) (Bigint.of_int acc))

let[@def] sum_invariant : int list @ immutable total ->
    (int @ immutable -> bool) @ total =
    fun xs result -> Bigint.equal (Bigint.of_int result) (sum_spec xs)

let checked_add : (x : int) @ immutable -> (acc : int) @ immutable ->
    {result : int | sum_relation x acc result} @ immutable total =
    fun x acc ->
  let wide = Bigint.add (Bigint.of_int x) (Bigint.of_int acc) in
  if wide < Bigint.of_int min_int || wide > Bigint.of_int max_int
  then failwith "overflow"
  else
    let result = x + acc in
    ghost_ (sum_relation_def x acc result);
    refine_ result

let (sum_preserved @ total) : (x : int) @ immutable ->
    (tail : int list) @ immutable -> (acc : int) @ immutable ->
    (result : int) @ immutable ->
    {u : unit | sum_invariant tail acc && sum_relation x acc result} ->
    {u : unit | sum_invariant (x :: tail) result} =
    fun x tail acc result premise ->
  premise;
  let inputs = x :: tail in
  sum_invariant_def tail acc;
  sum_relation_def x acc result;
  sum_invariant_def inputs result;
  sum_spec_def inputs;
  let u = () in refine_ u

let checked_sum (xs : int list) :
    {result : int | Bigint.of_int result === sum_spec xs} =
  let initial = 0 in
  let u = () in
  ghost_ (
    let empty : int list = [] in
    sum_spec_def empty;
    sum_invariant_def empty initial;
    (refine_ u : {u : unit | sum_invariant [] initial}));
  let f = checked_add in
  let preserve = ghost_ sum_preserved in
  let refine_ result = fold_right sum_relation sum_invariant f preserve xs
    initial (refine_ u) in
  ghost_ (sum_invariant_def xs result);
  refine_ result

let () =
  let xs = [7; -2; 10] in
  let refine_ result = checked_sum xs in
  assert (result = 15);
  let xs = [max_int; 1] in
  match checked_sum xs with
  | exception Failure message -> assert (message = "overflow")
  | _ -> failwith "expected overflow"

let[@def] clamp_model (x : int @ immutable) : int @ immutable total = if x < 0 then 0 else x

let clamp_exact : (x : int) @ immutable ->
    {y : int | y === clamp_model x} @ immutable total = fun x ->
  let refine_ y = clamp_partial x in
  ghost_ (clamp_relation_def x y);
  ghost_ (clamp_model_def x);
  refine_ y

let (decrement @ total) : (x : {n : int | n > 0}) ->
    {next : int | let refine_ n = x in 0 <= next && next < n} = fun x ->
  let refine_ n = x in
  let next = n - 1 in refine_ next

let () =
  let input = Node (-2, Leaf, Node (3, Leaf, Leaf)) in
  let f = clamp_exact in
  let refine_ output = tree_map clamp_model f input in
  let (output : int tree) = output in
  assert (output = Node (0, Leaf, Node (3, Leaf, Leaf)));
  let n = 10 in
  let initial : {n : int | n >= 0} = refine_ n in
  let step = decrement in
  let refine_ result = iterate_down step initial in
  assert (result = 0)

let[@def] nonzero (x : int @ immutable) = x <> 0
external div_nonzero : int -> {d : int | d <> 0} -> int
  @@ total = "%divint"
let[@def] quotient_model (x : int) =
  if x = 0 then 0 else div_nonzero 100 (refine_ x)
let[@def] division_relation : int @ immutable total ->
    (int @ immutable -> bool) @ total = fun x y ->
    x <> 0 && y = quotient_model x

let divide : (x : int) @ immutable -> {u : unit | nonzero x} @ ghost ->
    {y : int | division_relation x y} @ immutable total = fun x premise ->
  premise;
  ghost_ (nonzero_def x);
  if x = 13 then failwith "division callback"
  else
    let y = 100 / x in
    ghost_ (division_relation_def x y);
    ghost_ (quotient_model_def x);
    refine_ y

let divide_all (xs : int list)
    (premise : {u : unit | all_inputs nonzero xs} @ ghost) :
    {ys : int list | map_rel division_relation xs ys} =
  let f = divide in
  let refine_ ys = map_pre nonzero division_relation f xs premise in
  refine_ ys
