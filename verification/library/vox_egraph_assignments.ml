module C = Vox_egraph_assignment_spec
module L = Vox_egraph_language_spec

type result = Done of C.cases * int | Exhausted

let rec (append @ total) : (first : C.cases) @ immutable -> (second : C.cases) @ immutable ->
    (fuel : int) ->
    {r : result | match r with Exhausted -> true | Done (cases, left) ->
      0 <= left && left < fuel && cases === C.append first second} @ immutable =
  fun first second fuel ->
    if fuel <= 0 then Exhausted
    else (
      ghost_ (C.append_def first second);
      match first with
      | C.End -> (Done (second, fuel - 1))
      | C.Case (ids, rest) ->
        match append rest second (fuel - 1) with
        | Exhausted -> Exhausted
        | Done (cases, left) -> (Done (C.Case (ids, cases), left)))

let rec (prepend @ total) : (id : int) -> (suffixes : C.cases) @ immutable ->
    (fuel : int) ->
    {r : result | match r with Exhausted -> true | Done (cases, left) ->
      0 <= left && left < fuel && cases === C.prepend id suffixes} @ immutable =
  fun id suffixes fuel ->
    if fuel <= 0 then Exhausted
    else (
      ghost_ (C.prepend_def id suffixes);
      match suffixes with
      | C.End -> (Done (C.End, fuel - 1))
      | C.Case (ids, rest) ->
        match prepend id rest (fuel - 1) with
        | Exhausted -> Exhausted
        | Done (cases, left) -> (Done (C.Case (id :: ids, cases), left)))

let rec (choices @ total) : (count : int) -> (suffixes : C.cases) @ immutable ->
    (fuel : int) ->
    {r : result | match r with Exhausted -> true | Done (cases, left) ->
      0 <= left && left < fuel && cases === C.choices count suffixes} @ immutable =
  fun count suffixes fuel ->
    if fuel <= 0 then Exhausted
    else (
      ghost_ (C.choices_def count suffixes);
      if count <= 0 then prepend (-1) suffixes (fuel - 1)
      else
        match prepend (count - 1) suffixes (fuel - 1) with
        | Exhausted -> Exhausted
        | Done (first, fuel1) ->
          match choices (count - 1) suffixes fuel1 with
          | Exhausted -> Exhausted
          | Done (rest, fuel2) -> append first rest fuel2)
  [@@decreases if count > 0 then count else 0]

let rec (enumerate @ total) : (count : int) -> (vars : L.sort list) @ immutable ->
    (fuel : int) ->
    {r : result | match r with Exhausted -> true | Done (cases, left) ->
      0 <= left && left < fuel && cases === C.assignments count vars} @ immutable =
  fun count vars fuel ->
    if fuel <= 0 then Exhausted
    else (
      ghost_ (C.assignments_def count vars);
      match vars with
      | [] -> (Done (C.Case ([], C.End), fuel - 1))
      | _ :: rest ->
        match enumerate count rest (fuel - 1) with
        | Exhausted -> Exhausted
        | Done (suffixes, left) -> choices count suffixes left)
