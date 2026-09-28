let[@def] find_fee (alpha : Bigint.t) = Bigint.add (Bigint.mul 4Z alpha) 12Z
let[@def] union_fee (alpha : Bigint.t) = Bigint.add (Bigint.mul 12Z alpha) 36Z

let[@def] budget (alpha : Bigint.t) (allocations : Bigint.t)
    (finds : Bigint.t) (unions : Bigint.t) =
  Bigint.add 1Z (Bigint.add (Bigint.mul 11Z allocations)
    (Bigint.add (Bigint.mul (find_fee alpha) finds)
      (Bigint.mul (union_fee alpha) unions)))

let (bound @ total) : (alpha : Bigint.t) -> (allocations : Bigint.t) ->
    (finds : Bigint.t) -> (unions : Bigint.t) ->
    {u : unit | if alpha >= 1Z && allocations >= 0Z && finds >= 0Z && unions >= 0Z
      then budget alpha allocations finds unions <=
        Bigint.add 1Z (Bigint.add (Bigint.mul 11Z allocations)
          (Bigint.mul (Bigint.mul 48Z alpha) (Bigint.add finds unions))) &&
        budget alpha allocations finds unions >= 1Z else true} =
    fun alpha allocations finds unions ->
  budget_def alpha allocations finds unions;
  find_fee_def alpha; union_fee_def alpha;
  ()

type operation = Allocate | Find | Union
let[@def] same (left : operation) (right : operation) =
  match left, right with Allocate, Allocate | Find, Find | Union, Union -> true | _ -> false
type step = { operation : operation; account : Bigint.t }
let[@def] fee (alpha : Bigint.t) (operation : operation) =
  match operation with Allocate -> 11Z | Find -> find_fee alpha | Union -> union_fee alpha
let[@def] rec trace (alpha : Bigint.t) (initial : Bigint.t) (steps : step list) =
  match steps with
  | [] -> true
  | s :: rest -> s.account <= Bigint.add initial (fee alpha s.operation) &&
      trace alpha s.account rest
let[@def] rec final_account (initial : Bigint.t) (steps : step list) =
  match steps with [] -> initial | s :: rest -> final_account s.account rest
let[@def] rec count (operation : operation) (steps : step list) =
  match steps with [] -> 0Z | s :: rest ->
    Bigint.add (if same s.operation operation then 1Z else 0Z) (count operation rest)
let[@def] rec total_fee (alpha : Bigint.t) (steps : step list) =
  match steps with [] -> 0Z | s :: rest ->
    Bigint.add (fee alpha s.operation) (total_fee alpha rest)

let rec (telescope @ total) : (alpha : Bigint.t) -> (initial : Bigint.t) ->
    (steps : step list) ->
    {u : unit | if trace alpha initial steps then final_account initial steps <=
      Bigint.add initial (total_fee alpha steps) else true} =
    fun alpha initial steps ->
  trace_def alpha initial steps; final_account_def initial steps;
  total_fee_def alpha steps;
  (match steps with [] -> () | s :: rest -> telescope alpha s.account rest);
  ()

let rec (fees @ total) : (alpha : Bigint.t) -> (steps : step list) ->
    {u : unit | Bigint.add 1Z (total_fee alpha steps) =
      budget alpha (count Allocate steps) (count Find steps) (count Union steps)} =
    fun alpha steps ->
  total_fee_def alpha steps; count_def Allocate steps; count_def Find steps;
  count_def Union steps;
  budget_def alpha (count Allocate steps) (count Find steps) (count Union steps);
  (match steps with
  | [] -> ()
  | s :: rest ->
      fees alpha rest; fee_def alpha s.operation;
      same_def s.operation Allocate; same_def s.operation Find; same_def s.operation Union;
      budget_def alpha (count Allocate rest) (count Find rest) (count Union rest));
  ()

let (sequence @ total) : (alpha : Bigint.t) -> (steps : step list) ->
    (ticks : Bigint.t) ->
    {u : unit | if trace alpha 1Z steps && ticks <= final_account 1Z steps then
      ticks <= budget alpha (count Allocate steps) (count Find steps)
        (count Union steps) else true} = fun alpha steps ticks ->
  telescope alpha 1Z steps; fees alpha steps;
  ()
