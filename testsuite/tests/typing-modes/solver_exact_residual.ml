(* TEST
 include ocamlcommon;
 native;
*)

module L = struct
  type elt = Global | Regional | Local

  let elements = [Global; Regional; Local]

  let equal = ( = )

  let rank = function Global -> 0 | Regional -> 1 | Local -> 2

  let le a b = rank a <= rank b

  let join a b = if le a b then b else a

  let meet a b = if le a b then a else b
end

open L

module S = Solver_exact.Make (L)

let get = function Ok x -> x | Error _ -> failwith "unexpected solver error"

let right () =
  let open S in
  let initial = get (empty 2) in
  let guard = Le (Var 2, Var 0) in
  let obligation = Le (Var 1, Var 0) in
  let formula = Forall (Disj (Neg (Plain guard), Plain obligation)) in
  let state = get (assert_quantified initial formula) in
  let expected = Le (Var 0, Var 1) in
  get (query state expected)
  && get (holds state [Regional; Regional])
  && not (get (holds state [Regional; Global]))
  && let copied = get (copy_first state) in
     let restored = get (project_first copied) in
     get (holds restored [Regional; Regional])

let left () =
  let open S in
  let initial = get (empty 2) in
  let formula =
    Forall
      (Disj
         (Neg (Plain (Le (Var 0, Var 2))),
          Plain (Le (Var 0, Var 1))))
  in
  let state = get (assert_quantified initial formula) in
  get (query state (Le (Var 1, Var 0)))
  && get (holds state [Regional; Regional])
  && not (get (holds state [Global; Regional]))

let constant_right () =
  let open S in
  let initial = get (empty 1) in
  let formula =
    Forall
      (Disj
         (Neg (Plain (Le (Var 1, Var 0))),
          Plain (Le (Const Regional, Var 0))))
  in
  let state = get (assert_quantified initial formula) in
  get (query state (Le (Const Regional, Var 0)))
  && get (holds state [Regional])
  && not (get (holds state [Global]))

let constant_left () =
  let open S in
  let initial = get (empty 1) in
  let formula =
    Forall
      (Disj
         (Neg (Plain (Le (Var 0, Var 1))),
          Plain (Le (Var 0, Const Regional))))
  in
  let state = get (assert_quantified initial formula) in
  get (query state (Le (Var 0, Const Regional)))
  && get (holds state [Regional])
  && not (get (holds state [Local]))

let limits_and_rollback () =
  let open S in
  let before = get (empty 1) in
  let after = get (assert_clause before (Le (Const Regional, Var 0))) in
  let too_large =
    List.init 13 Fun.id
    |> List.fold_left (fun formula _ -> Exists formula) (Plain True)
  in
  (match empty 65 with Error Limit -> true | _ -> false)
  && (match query ~limit:0 after True with Error Limit -> true | _ -> false)
  && (match assert_quantified before too_large with
     | Error Limit -> true
     | _ -> false)
  && get (holds before [Global])
  && not (get (holds after [Global]))

let quantifiers_and_morphism () =
  let open S in
  let equality =
    And (Le (Var 1, Var 0), Le (Var 0, Var 1))
  in
  let initial = get (empty 0) in
  let forall_exists =
    get (assert_quantified initial (Forall (Exists (Plain equality))))
  in
  let exists_forall =
    get (assert_quantified initial (Exists (Forall (Plain equality))))
  in
  let regional_to_global =
    morph (function Global | Regional -> Global | Local -> Local)
  in
  let rigid_morph =
    get
      (assert_quantified initial
         (Forall (Plain (Le (Var 0, Apply (regional_to_global, Var 0))))))
  in
  get (holds forall_exists [])
  && not (get (holds exists_forall []))
  && not (get (holds rigid_morph []))
  && get (query_quantified initial (Forall (Exists (Plain equality))))
  && not (get (query_quantified initial (Exists (Forall (Plain equality)))))
  && get (holds initial [])

let arbitrary_copy_projection_and_bounds () =
  let open S in
  let before = get (empty 2) in
  let constrained =
    get (assert_clause before (Le (Var 0, Var 1)))
    |> fun state ->
    get (assert_clause state (Le (Var 1, Const Regional)))
  in
  let copy = get (copy_index constrained 1) in
  let restored = get (project_first copy) in
  let projected = get (project constrained 1) in
  get (holds restored [Regional; Regional])
  && not (get (holds restored [Local; Regional]))
  && get (holds projected [Regional])
  && not (get (holds projected [Local]))
  && (match get (bounds projected 0) with
      | Some (Global, Regional) -> true
      | _ -> false)
  && (match bounds ~limit:0 projected 0 with
      | Error Limit -> true
      | _ -> false)

let exhaustive_copy_projection () =
  let open S in
  let values = [Global; Regional; Local] in
  let terms =
    [Var 0; Var 1; Const Global; Const Regional; Const Local]
  in
  List.for_all
    (fun left ->
      List.for_all
        (fun right ->
          let state = get (assert_clause (get (empty 2)) (Le (left, right))) in
          List.for_all
            (fun index ->
              let projected = get (project state index) in
              let copied = get (copy_index state index) in
              List.for_all
                (fun other ->
                  let expected =
                    List.exists
                      (fun removed ->
                        get
                          (holds state
                             (if index = 0
                              then [removed; other]
                              else [other; removed])))
                      values
                  in
                  get (holds projected [other]) = expected)
                values
              && List.for_all
                   (fun a ->
                     List.for_all
                       (fun b ->
                         List.for_all
                           (fun fresh ->
                             get (holds copied [fresh; a; b])
                             =
                             (get (holds state [a; b])
                              && fresh = (if index = 0 then a else b)))
                           values)
                       values)
                   values)
            [0; 1])
        terms)
    terms

let fresh_and_merge () =
  let open S in
  let lower =
    get (assert_clause (get (empty 1)) (Le (Const Regional, Var 0)))
  in
  let upper =
    get (assert_clause (get (empty 1)) (Le (Var 0, Const Regional)))
  in
  let fresh_state = get (fresh upper) in
  let merged = get (merge lower upper) in
  get (holds fresh_state [Local; Regional])
  && not (get (holds fresh_state [Global; Local]))
  && get (holds merged [Regional; Regional])
  && not (get (holds merged [Global; Regional]))
  && not (get (holds merged [Regional; Local]))

let () =
  let r = right () in
  let l = left () in
  let cr = constant_right () in
  let cl = constant_left () in
  let bounds = limits_and_rollback () in
  let quantifiers = quantifiers_and_morphism () in
  let projection = arbitrary_copy_projection_and_bounds () in
  let exhaustive = exhaustive_copy_projection () in
  let components = fresh_and_merge () in
  if
    r && l && cr && cl && bounds && quantifiers && projection && exhaustive
    && components
  then print_endline "exact residuals"
  else
    Printf.printf "incorrect residuals: %b %b %b %b %b %b %b %b %b\n" r
      l cr cl bounds quantifiers projection exhaustive components
