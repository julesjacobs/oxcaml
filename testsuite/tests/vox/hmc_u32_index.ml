module D = Hm_declarative
module W = Hmc_word64

let[@def] rec (fits @ total) (index : D.index @ immutable) (capacity : W.limb) = match index with
  | D.Z -> true | D.S rest -> if capacity = 0 then false else fits rest (capacity - 1)
let[@def] rec (represents @ total) (index : D.index @ immutable) (number : W.limb) = match index with
  | D.Z -> number = 0 | D.S rest -> if number = 0 then false else represents rest (number - 1)
let rec (encode @ total) : (capacity : W.limb) -> (index : D.index) @ immutable ->
    {out : W.limb option | match out with
      | None -> not (fits index capacity)
      | Some number -> number <= capacity && represents index number && fits index capacity} @ immutable = fun capacity index ->
  ghost_ (fits_def index capacity);
  match index with
  | D.Z -> ghost_ (represents_def index 0); Some 0
  | D.S rest -> if capacity = 0 then None else
    match encode (capacity - 1) rest with None -> None | Some n ->
      ghost_ (represents_def index (n + 1)); Some (n + 1)
let rec (unique @ total) : (index : D.index) @ immutable -> (a : W.limb) -> (b : W.limb) ->
    {u : unit | represents index a && represents index b} -> {u : unit | a = b} @ ghost = fun index a b premise -> ghost_ (
  represents_def index a; represents_def index b;
  match index with D.Z -> () | D.S rest -> unique rest (a - 1) (b - 1) ())
let rec (injective @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable -> (number : W.limb) ->
    {u : unit | represents a number && represents b number} -> {u : unit | a === b} @ ghost = fun a b number premise -> ghost_ (
  represents_def a number; represents_def b number;
  match a, b with D.S a, D.S b -> injective a b (number - 1) () | _ -> ())
