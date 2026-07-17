open Oxsmt_core
module Rational = Oxsmt_lia.Rational

type side =
  | A
  | B

type 'a t =
  { coefficients : ('a * Bigint.t) list
  ; constant : Bigint.t
  }

type 'a replay =
  { assertions : Term.t list
  ; resolve : 'a -> Term.t
  }

exception Invalid_evidence

let map_add map term value =
  let previous =
    match Term.Map.find_opt term map with
    | Some value -> value
    | None -> Rational.zero
  in
  Term.Map.add term (Rational.add previous value) map
;;

let add_scaled_term map constant scale (term : Term.t) =
  match term.node with
  | Term.Arith { coeffs; const } ->
    let map =
      List.fold_left
        (fun map (variable, coefficient) ->
           map_add map variable (Rational.mul scale (Rational.of_bigint coefficient)))
        map
        (Iarr.to_list coeffs)
    in
    map, Rational.add constant (Rational.mul scale (Rational.of_bigint const))
  | Term.Int_const value ->
    map, Rational.add constant (Rational.mul scale (Rational.of_bigint value))
  | _ when Sort.equal term.sort Sort.int -> map_add map term scale, constant
  | _ -> raise Invalid_evidence
;;

let add_premise (map, constant) (coefficient, ((atom, polarity) : Term.t * bool)) =
  match atom.node with
  | Term.Le inner ->
    if Rational.sign coefficient < 0 then raise Invalid_evidence;
    if polarity
    then add_scaled_term map constant coefficient inner
    else (
      let map, constant = add_scaled_term map constant (Rational.neg coefficient) inner in
      map, Rational.add constant coefficient)
  | Term.Eq (left, right)
    when polarity && Sort.equal left.sort Sort.int && Sort.equal right.sort Sort.int ->
    let map, constant = add_scaled_term map constant coefficient left in
    add_scaled_term map constant (Rational.neg coefficient) right
  | _ -> raise Invalid_evidence
;;

let accumulate certificate =
  List.fold_left add_premise (Term.Map.empty, Rational.zero) certificate
;;

let complete_certificate_is_valid certificate =
  let map, constant = accumulate certificate in
  Term.Map.for_all (fun _ coefficient -> Rational.is_zero coefficient) map
  && Rational.sign constant > 0
;;

let denominator value =
  let rendered = Rational.to_string value in
  match String.index_opt rendered '/' with
  | None -> Bigint.one
  | Some slash ->
    Bigint.of_string
      (String.sub rendered (slash + 1) (String.length rendered - slash - 1))
;;

let exact_div numerator denominator =
  let quotient, remainder = Bigint.divmod numerator denominator in
  if Bigint.is_zero remainder then quotient else raise Invalid_evidence
;;

let lcm left right =
  if Bigint.is_zero left || Bigint.is_zero right
  then Bigint.zero
  else Bigint.abs (Bigint.mul (exact_div left (Bigint.gcd left right)) right)
;;

let primitive_integer_form ~project_shared map constant =
  let entries =
    Term.Map.bindings map
    |> List.filter (fun (_, coefficient) -> not (Rational.is_zero coefficient))
  in
  if entries = [] then raise Invalid_evidence;
  let common_denominator =
    List.fold_left
      (fun acc (_, coefficient) -> lcm acc (denominator coefficient))
      (denominator constant)
      entries
  in
  if Bigint.is_zero common_denominator then raise Invalid_evidence;
  let scale value =
    Bigint.mul
      (Rational.num_bigint value)
      (exact_div common_denominator (denominator value))
  in
  let coefficients =
    List.map
      (fun (term, coefficient) ->
         match project_shared term with
         | Some variable -> variable, scale coefficient
         | None -> raise Invalid_evidence)
      entries
  in
  let constant = scale constant in
  let divisor =
    List.fold_left
      (fun acc (_, coefficient) -> Bigint.gcd acc (Bigint.abs coefficient))
      (Bigint.abs constant)
      coefficients
  in
  if Bigint.is_zero divisor then raise Invalid_evidence;
  { coefficients =
      List.map
        (fun (variable, coefficient) -> variable, exact_div coefficient divisor)
        coefficients
  ; constant = exact_div constant divisor
  }
;;

let term session resolve interpolant ~complement =
  let context = Session.context session in
  let expression =
    Context.linear_combination_big
      context
      (List.map
         (fun (variable, coefficient) -> coefficient, resolve variable)
         interpolant.coefficients)
      interpolant.constant
  in
  if complement
  then Context.ge context expression (Context.int_const context 1)
  else Context.le context expression (Context.int_const context 0)
;;

let run_check thunk =
  match thunk () with
  | true -> true
  | false -> false
  | exception Out_of_memory -> raise Out_of_memory
  | exception Stack_overflow -> raise Stack_overflow
  | exception Sys.Break -> raise Sys.Break
  | exception _ -> false
;;

let verify ~create ~build ~is_shared interpolant =
  let structurally_valid =
    run_check (fun () ->
      interpolant.coefficients <> []
      && List.for_all
           (fun (variable, coefficient) ->
              (not (Bigint.is_zero coefficient)) && is_shared variable)
           interpolant.coefficients)
  in
  if not structurally_valid
  then false
  else (
    let session = create () in
    let a_valid =
      run_check (fun () ->
        let replay = build A session in
        let not_candidate = term session replay.resolve interpolant ~complement:true in
        List.iter (Session.assert_term session) replay.assertions;
        Session.assert_term session not_candidate;
        Session.check_sat session = Session.Unsat)
    in
    let session = create () in
    let b_valid =
      run_check (fun () ->
        let replay = build B session in
        let candidate = term session replay.resolve interpolant ~complement:false in
        Session.assert_term session candidate;
        List.iter (Session.assert_term session) replay.assertions;
        Session.check_sat session = Session.Unsat)
    in
    a_valid && b_valid)
;;

let interpolate evidence ~side_of ~project_shared ~create ~build ~is_shared =
  let derive () =
    match Session.last_farkas evidence with
    | None -> None
    | Some certificate ->
      (match
       if certificate = [] || not (complete_certificate_is_valid certificate)
       then raise Invalid_evidence;
       let a_part = ref [] in
       let a_contributes = ref false in
       let b_contributes = ref false in
       List.iter
         (fun ((coefficient, premise) as entry) ->
            match side_of premise with
            | Some A ->
              a_part := entry :: !a_part;
              if not (Rational.is_zero coefficient) then a_contributes := true
            | Some B -> if not (Rational.is_zero coefficient) then b_contributes := true
            | None -> raise Invalid_evidence)
         certificate;
       if not (!a_contributes && !b_contributes) then raise Invalid_evidence;
       let map, constant = accumulate (List.rev !a_part) in
       Some (primitive_integer_form ~project_shared map constant)
     with
     | result -> result
     | exception Out_of_memory -> raise Out_of_memory
     | exception Stack_overflow -> raise Stack_overflow
     | exception Sys.Break -> raise Sys.Break
     | exception _ -> None)
  in
  match derive () with
  | None -> None
  | Some candidate ->
    if verify ~create ~build ~is_shared candidate then Some candidate else None
;;
