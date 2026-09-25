(* An exact, immutable reference solver for one finite lattice.  Variables are
   de Bruijn indices: index zero is the newest variable. *)

module type Lattice = sig
  type elt

  val elements : elt list

  val equal : elt -> elt -> bool

  val le : elt -> elt -> bool

  val join : elt -> elt -> elt

  val meet : elt -> elt -> elt
end

module Make (L : Lattice) = struct
  let max_variables = 64

  let max_nodes = 1_000_000

  type elt = L.elt

  type morph = elt array

  type term =
    | Const of elt
    | Var of int
    | Join of term * term
    | Meet of term * term
    | Apply of morph * term

  type qf =
    | True
    | Le of term * term
    | And of qf * qf
    | Or of qf * qf
    | Not of qf

  type formula =
    | Plain of qf
    | Conj of formula * formula
    | Disj of formula * formula
    | Neg of formula
    | Exists of formula
    | Forall of formula

  type state =
    { arity : int;
      residual : qf
    }

  type error =
    | Invalid_scope
    | Limit

  let empty arity =
    if arity < 0
    then Error Invalid_scope
    else if arity > max_variables
    then Error Limit
    else Ok { arity; residual = True }

  let morph f =
    let values = List.map f L.elements in
    if List.for_all (fun value -> List.exists (L.equal value) L.elements) values
    then Array.of_list values
    else invalid_arg "Solver_exact: morphism leaves the carrier"

  let rec nth xs i =
    match xs with
    | [] -> invalid_arg "Solver_exact: unbound variable"
    | x :: rest -> if i = 0 then x else nth rest (i - 1)

  let apply morph x =
    let rec find i = function
      | [] -> invalid_arg "Solver_exact: value outside the carrier"
      | value :: rest ->
        if L.equal value x then morph.(i) else find (i + 1) rest
    in
    find 0 L.elements

  let rec eval_term env = function
    | Const x -> x
    | Var i -> nth env i
    | Join (a, b) -> L.join (eval_term env a) (eval_term env b)
    | Meet (a, b) -> L.meet (eval_term env a) (eval_term env b)
    | Apply (f, a) -> apply f (eval_term env a)

  let rec eval_qf env = function
    | True -> true
    | Le (a, b) -> L.le (eval_term env a) (eval_term env b)
    | And (a, b) -> eval_qf env a && eval_qf env b
    | Or (a, b) -> eval_qf env a || eval_qf env b
    | Not a -> not (eval_qf env a)

  let rec scoped_term depth = function
    | Const _ -> true
    | Var i -> 0 <= i && i < depth
    | Join (a, b) | Meet (a, b) -> scoped_term depth a && scoped_term depth b
    | Apply (f, a) ->
      Array.length f = List.length L.elements && scoped_term depth a

  let rec scoped_qf depth = function
    | True -> true
    | Le (a, b) -> scoped_term depth a && scoped_term depth b
    | And (a, b) | Or (a, b) -> scoped_qf depth a && scoped_qf depth b
    | Not a -> scoped_qf depth a

  let rec scoped depth = function
    | Plain q -> scoped_qf depth q
    | Conj (a, b) | Disj (a, b) -> scoped depth a && scoped depth b
    | Neg a -> scoped depth a
    | Exists a | Forall a -> depth < max_int && scoped (depth + 1) a

  let rec substitute_term value = function
    | Const x -> Const x
    | Var i -> if i = 0 then Const value else Var (i - 1)
    | Join (a, b) -> Join (substitute_term value a, substitute_term value b)
    | Meet (a, b) -> Meet (substitute_term value a, substitute_term value b)
    | Apply (f, a) -> Apply (f, substitute_term value a)

  let rec substitute_qf value = function
    | True -> True
    | Le (a, b) -> Le (substitute_term value a, substitute_term value b)
    | And (a, b) -> And (substitute_qf value a, substitute_qf value b)
    | Or (a, b) -> Or (substitute_qf value a, substitute_qf value b)
    | Not a -> Not (substitute_qf value a)

  let rec shift_term = function
    | Const x -> Const x
    | Var i -> Var (i + 1)
    | Join (a, b) -> Join (shift_term a, shift_term b)
    | Meet (a, b) -> Meet (shift_term a, shift_term b)
    | Apply (f, a) -> Apply (f, shift_term a)

  let rec shift_qf = function
    | True -> True
    | Le (a, b) -> Le (shift_term a, shift_term b)
    | And (a, b) -> And (shift_qf a, shift_qf b)
    | Or (a, b) -> Or (shift_qf a, shift_qf b)
    | Not a -> Not (shift_qf a)

  let rec offset_term offset = function
    | Const x -> Const x
    | Var i -> Var (i + offset)
    | Join (a, b) -> Join (offset_term offset a, offset_term offset b)
    | Meet (a, b) -> Meet (offset_term offset a, offset_term offset b)
    | Apply (f, a) -> Apply (f, offset_term offset a)

  let rec offset_qf offset = function
    | True -> True
    | Le (a, b) -> Le (offset_term offset a, offset_term offset b)
    | And (a, b) -> And (offset_qf offset a, offset_qf offset b)
    | Or (a, b) -> Or (offset_qf offset a, offset_qf offset b)
    | Not a -> Not (offset_qf offset a)

  let rec move_to_front_term index = function
    | Const x -> Const x
    | Var i -> Var (if i = index then 0 else if i < index then i + 1 else i)
    | Join (a, b) ->
      Join (move_to_front_term index a, move_to_front_term index b)
    | Meet (a, b) ->
      Meet (move_to_front_term index a, move_to_front_term index b)
    | Apply (f, a) -> Apply (f, move_to_front_term index a)

  let rec move_to_front_qf index = function
    | True -> True
    | Le (a, b) -> Le (move_to_front_term index a, move_to_front_term index b)
    | And (a, b) -> And (move_to_front_qf index a, move_to_front_qf index b)
    | Or (a, b) -> Or (move_to_front_qf index a, move_to_front_qf index b)
    | Not a -> Not (move_to_front_qf index a)

  let rec eliminate = function
    | Plain q -> q
    | Conj (a, b) -> And (eliminate a, eliminate b)
    | Disj (a, b) -> Or (eliminate a, eliminate b)
    | Neg a -> Not (eliminate a)
    | Exists a ->
      let q = eliminate a in
      List.fold_left
        (fun acc x -> Or (acc, substitute_qf x q))
        (Not True) L.elements
    | Forall a ->
      let q = eliminate a in
      List.fold_left (fun acc x -> And (acc, substitute_qf x q)) True L.elements

  let capped_add a b = if a > max_nodes - b then max_nodes + 1 else a + b

  let capped_multiply a b =
    if a = 0 || b = 0
    then 0
    else if a > max_nodes / b
    then max_nodes + 1
    else a * b

  let rec term_size = function
    | Const _ | Var _ -> 1
    | Join (a, b) | Meet (a, b) ->
      capped_add 1 (capped_add (term_size a) (term_size b))
    | Apply (_, a) -> capped_add 1 (term_size a)

  let rec qf_size = function
    | True -> 1
    | Le (a, b) -> capped_add 1 (capped_add (term_size a) (term_size b))
    | And (a, b) | Or (a, b) ->
      capped_add 1 (capped_add (qf_size a) (qf_size b))
    | Not a -> capped_add 1 (qf_size a)

  let rec eliminated_size = function
    | Plain q -> qf_size q
    | Conj (a, b) | Disj (a, b) ->
      capped_add 1 (capped_add (eliminated_size a) (eliminated_size b))
    | Neg a -> capped_add 1 (eliminated_size a)
    | Exists a | Forall a ->
      let count = List.length L.elements in
      capped_add 1 (capped_multiply count (capped_add 1 (eliminated_size a)))

  let checked_eliminate formula =
    if eliminated_size formula > max_nodes
    then Error Limit
    else Ok (eliminate formula)

  let assert_clause state clause =
    if not (scoped_qf state.arity clause)
    then Error Invalid_scope
    else if
      capped_add 1 (capped_add (qf_size state.residual) (qf_size clause))
      > max_nodes
    then Error Limit
    else Ok { state with residual = And (state.residual, clause) }

  let fresh state =
    if state.arity >= max_variables
    then Error Limit
    else Ok { arity = state.arity + 1; residual = shift_qf state.residual }

  let merge left right =
    if left.arity > max_variables - right.arity
    then Error Limit
    else if
      capped_add 1 (capped_add (qf_size left.residual) (qf_size right.residual))
      > max_nodes
    then Error Limit
    else
      Ok
        { arity = left.arity + right.arity;
          residual = And (left.residual, offset_qf left.arity right.residual)
        }

  let project_first state =
    if state.arity = 0
    then Error Invalid_scope
    else
      match checked_eliminate (Exists (Plain state.residual)) with
      | Error _ as error -> error
      | Ok residual -> Ok { arity = state.arity - 1; residual }

  let project state index =
    if index < 0 || index >= state.arity
    then Error Invalid_scope
    else
      project_first
        { state with residual = move_to_front_qf index state.residual }

  let copy_first state =
    if state.arity = 0
    then Error Invalid_scope
    else if state.arity >= max_variables
    then Error Limit
    else if qf_size state.residual > max_nodes - 8
    then Error Limit
    else
      Ok
        { arity = state.arity + 1;
          residual =
            And
              ( shift_qf state.residual,
                And (Le (Var 0, Var 1), Le (Var 1, Var 0)) )
        }

  let copy_index state index =
    if index < 0 || index >= state.arity
    then Error Invalid_scope
    else if state.arity >= max_variables
    then Error Limit
    else if qf_size state.residual > max_nodes - 8
    then Error Limit
    else
      Ok
        { arity = state.arity + 1;
          residual =
            And
              ( shift_qf state.residual,
                And (Le (Var 0, Var (index + 1)), Le (Var (index + 1), Var 0))
              )
        }

  let assert_quantified state formula =
    if not (scoped state.arity formula)
    then Error Invalid_scope
    else
      match checked_eliminate formula with
      | Error _ as error -> error
      | Ok residual -> assert_clause state residual

  let holds state values =
    if List.length values = state.arity
    then Ok (eval_qf values state.residual)
    else Error Invalid_scope

  let bounds ?(limit = 1_000_000) state index =
    if index < 0 || index >= state.arity
    then Error Invalid_scope
    else begin
      let remaining = ref limit in
      let envelope = ref None in
      let rec all depth env =
        if !remaining <= 0
        then Error Limit
        else if depth = 0
        then begin
          decr remaining;
          if eval_qf env state.residual
          then begin
            let value = nth env index in
            envelope
              := Some
                   (match !envelope with
                   | None -> value, value
                   | Some (lower, upper) ->
                     L.meet lower value, L.join upper value)
          end;
          Ok ()
        end
        else
          let rec each = function
            | [] -> Ok ()
            | x :: rest -> (
              match all (depth - 1) (x :: env) with
              | Error _ as error -> error
              | Ok () -> each rest)
          in
          each L.elements
      in
      match all state.arity [] with
      | Error _ as error -> error
      | Ok () -> Ok !envelope
    end

  let query ?(limit = 1_000_000) state clause =
    if not (scoped_qf state.arity clause)
    then Error Invalid_scope
    else begin
      let remaining = ref limit in
      let rec all depth env =
        if !remaining <= 0
        then Error Limit
        else if depth = 0
        then begin
          decr remaining;
          Ok ((not (eval_qf env state.residual)) || eval_qf env clause)
        end
        else
          let rec each = function
            | [] -> Ok true
            | x :: rest -> (
              match all (depth - 1) (x :: env) with
              | Error _ as error -> error
              | Ok false -> Ok false
              | Ok true -> each rest)
          in
          each L.elements
      in
      all state.arity []
    end

  let query_quantified ?(limit = 1_000_000) state formula =
    if not (scoped state.arity formula)
    then Error Invalid_scope
    else
      match checked_eliminate formula with
      | Error _ as error -> error
      | Ok clause -> query ~limit state clause
end
