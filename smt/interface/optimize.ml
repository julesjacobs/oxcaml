open Oxsmt_core

type soft =
  { term : Term.t
  ; weight : Bigint.t
  }

type optimum =
  { cost : Bigint.t
  ; model : Session.model
  ; violated : soft list
  }

type result =
  | Optimal of optimum
  | Hard_unsat
  | Unknown

type selector =
  { name : string
  ; atom : Term.t
  ; soft : soft
  }

let selector_prefix = "@oxsmt.optimize.selector."

(* [Env.rank] is the public declaration-membership test. A sort-only use of the same name
   has no rank and cannot occur as a term, so declaring the nullary Bool symbol is still a
   fresh logical constant (the core deliberately permits a shared sort/function name). *)
let fresh_selector session start soft =
  let env = Session.env session in
  let rec choose n =
    let name = selector_prefix ^ string_of_int n in
    match Env.rank env (Symbol.intern name) with
    | (_ : Rank.t) -> choose (n + 1)
    | exception Not_found ->
      let symbol = Session.declare_const session name Sort.bool in
      { name; atom = Context.const (Session.context session) symbol; soft }, n + 1
  in
  choose start
;;

let validate_soft soft =
  if not (Sort.equal soft.term.Term.sort Sort.bool)
  then invalid_arg "Optimize.max_smt: soft term must be Bool-sorted";
  if Bigint.sign soft.weight <= 0
  then invalid_arg "Optimize.max_smt: soft weight must be positive"
;;

let core_is_hit selected core = Array.exists (fun i -> selected.(i)) core

let minimum_hitting_set selectors cores =
  let selected = Array.make (Array.length selectors) false in
  let best = ref None in
  let choose_unhit () =
    List.fold_left
      (fun choice core ->
        if core_is_hit selected core
        then choice
        else (
          match choice with
          | None -> Some core
          | Some prior when Array.length core < Array.length prior -> Some core
          | Some _ -> choice))
      None
      cores
  in
  let rec search cost =
    match !best with
    | Some (upper, _) when Bigint.compare cost upper >= 0 -> ()
    | _ ->
      (match choose_unhit () with
       | None -> best := Some (cost, Array.copy selected)
       | Some core ->
         Array.iter
           (fun i ->
             selected.(i) <- true;
             search (Bigint.add cost selectors.(i).soft.weight);
             selected.(i) <- false)
           core)
  in
  search Bigint.zero;
  !best
;;

let selector_index selectors =
  Array.mapi (fun index selector -> selector.atom, index) selectors
  |> Array.fold_left (fun map (atom, index) -> Term.Map.add atom index map) Term.Map.empty
;;

let indices_of_core ~index_by_atom ~disabled core =
  let seen = Array.make (Array.length disabled) false in
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | (atom, polarity) :: rest ->
      (match Term.Map.find_opt atom index_by_atom with
       | Some index when polarity && (not disabled.(index)) && not seen.(index) ->
         seen.(index) <- true;
         collect (index :: acc) rest
       | Some _ | None -> None)
  in
  collect [] core
;;

let order_core selectors indices =
  let compare_index i j =
    let by_weight = Bigint.compare selectors.(i).soft.weight selectors.(j).soft.weight in
    if by_weight <> 0 then by_weight else Int.compare i j
  in
  Array.of_list (List.sort compare_index indices)
;;

let binding_name = function
  | Session.Const (name, _) | Session.Fun (name, _) -> name
;;

(* The selector definitions are biconditionals, so their model values are exactly the
   soft-term values. Requiring those values to agree with the minimum hitting set is an
   independent witness check at the API boundary, not merely an algorithm invariant. *)
let check_and_strip_model selectors disabled cost ((sorts, bindings) as _model) =
  let by_name = Hashtbl.create ((2 * Array.length selectors) + 1) in
  Array.iteri (fun index selector -> Hashtbl.add by_name selector.name index) selectors;
  let values = Array.make (Array.length selectors) None in
  let valid = ref true in
  List.iter
    (fun binding ->
      match Hashtbl.find_opt by_name (binding_name binding) with
      | None -> ()
      | Some index ->
        (match binding, values.(index) with
         | Session.Const (_, Session.VBool value), None -> values.(index) <- Some value
         | _ -> valid := false))
    bindings;
  let witnessed_cost = ref Bigint.zero in
  Array.iteri
    (fun index selector ->
      match values.(index) with
      | Some value when Bool.equal disabled.(index) (not value) ->
        if not value
        then witnessed_cost := Bigint.add !witnessed_cost selector.soft.weight
      | Some _ | None -> valid := false)
    selectors;
  if (not !valid) || not (Bigint.equal !witnessed_cost cost)
  then None
  else (
    let bindings =
      List.filter
        (fun binding -> not (Hashtbl.mem by_name (binding_name binding)))
        bindings
    in
    Some (sorts, bindings))
;;

let violated_softs selectors disabled =
  let rec collect index acc =
    if index < 0
    then acc
    else
      collect
        (index - 1)
        (if disabled.(index) then selectors.(index).soft :: acc else acc)
  in
  collect (Array.length selectors - 1) []
;;

let max_smt ?max_checks session softs =
  (match max_checks with
   | Some n when n < 0 -> invalid_arg "Optimize.max_smt: max_checks must be nonnegative"
   | Some _ | None -> ());
  List.iter validate_soft softs;
  match max_checks with
  | Some 0 -> Unknown
  | Some _ | None ->
    let selectors, _ =
      List.fold_left
        (fun (rev, next) soft ->
          let selector, next = fresh_selector session next soft in
          selector :: rev, next)
        ([], 0)
        softs
    in
    let selectors = Array.of_list (List.rev selectors) in
    let index_by_atom = selector_index selectors in
    let definitions =
      Array.to_list
        (Array.map
           (fun selector ->
             Context.iff (Session.context session) selector.atom selector.soft.term)
           selectors)
    in
    Session.push session;
    Fun.protect
      ~finally:(fun () -> Session.pop session)
      (fun () ->
        List.iter (Session.assert_term session) definitions;
        let checks = ref 0 in
        let can_check () =
          match max_checks with
          | None -> true
          | Some limit -> !checks < limit
        in
        let rec loop cores =
          match minimum_hitting_set selectors cores with
          | None -> Unknown
          | Some (cost, disabled) ->
            if not (can_check ())
            then Unknown
            else (
              incr checks;
              let assumptions =
                Array.fold_right
                  (fun selector acc ->
                    let index = Term.Map.find selector.atom index_by_atom in
                    if disabled.(index) then acc else (selector.atom, true) :: acc)
                  selectors
                  []
              in
              let checked = Session.check_sat_assuming session assumptions in
              match checked.Session.verdict, checked.Session.unsat_core with
              | Session.Unknown, _ -> Unknown
              | Session.Sat, None ->
                (match Session.get_model session with
                 | None -> Unknown
                 | Some model ->
                   (match check_and_strip_model selectors disabled cost model with
                    | None -> Unknown
                    | Some model ->
                      Optimal
                        { cost; model; violated = violated_softs selectors disabled }))
              | Session.Sat, Some _ -> Unknown
              | Session.Unsat, None -> Unknown
              | Session.Unsat, Some [] -> Hard_unsat
              | Session.Unsat, Some core ->
                (match indices_of_core ~index_by_atom ~disabled core with
                 | None | Some [] -> Unknown
                 | Some indices -> loop (order_core selectors indices :: cores)))
        in
        loop [])
;;

module Omt = struct
  type optimum =
    { value : Bigint.t
    ; model : Session.model
    }

  type result =
    | Optimal of optimum
    | Hard_unsat
    | Unknown

  type direction =
    | Minimize
    | Maximize

  type candidate =
    { score : Bigint.t
    ; optimum : optimum
    }

  type probe =
    | Feasible of candidate
    | Infeasible
    | Probe_unknown

  let default_max_checks = 256
  let objective_prefix = "@oxsmt.optimize.objective."

  (* Core arithmetic is already normalized into [Arith]. Its leaves must be nullary
     integer symbols: an [Ite] or any non-nullary application is not a linear objective. *)
  let rec affine_int_term (term : Term.t) =
    match term.Term.node with
    | Term.Int_const _ -> true
    | Term.App (_, arguments) -> Iarr.length arguments = 0
    | Term.Arith linear ->
      Iarr.fold
        (fun affine (child, _) -> affine && affine_int_term child)
        true
        linear.coeffs
    | Term.Ite _
    | Term.Bool_const _
    | Term.Le _
    | Term.Eq _
    | Term.Not _
    | Term.And _
    | Term.Or _ -> false
  ;;

  let validate_objective objective =
    if not (Sort.equal objective.Term.sort Sort.int)
    then invalid_arg "Optimize.Omt: objective must be Int-sorted";
    if not (affine_int_term objective)
    then invalid_arg "Optimize.Omt: objective must be affine over nullary Int symbols"
  ;;

  let fresh_objective session =
    let env = Session.env session in
    let rec choose n =
      let name = objective_prefix ^ string_of_int n in
      match Env.rank env (Symbol.intern name) with
      | (_ : Rank.t) -> choose (n + 1)
      | exception Not_found ->
        let symbol = Session.declare_const session name Sort.int in
        name, Context.const (Session.context session) symbol
    in
    choose 0
  ;;

  let score direction value =
    match direction with
    | Minimize -> value
    | Maximize -> Bigint.neg value
  ;;

  (* The scoped equality [anchor = objective] is included in Session's obligatory model
     self-check. Consequently, after this shape check, [value] is independently tied to
     the objective. Removing only the fresh binding preserves a user model witnessing it. *)
  let candidate_of_model ~direction ~anchor_name ((sorts, bindings) : Session.model) =
    let value = ref None in
    let valid = ref true in
    List.iter
      (function
        | Session.Const (name, Session.VInt n) when String.equal name anchor_name ->
          (match !value with
           | None -> value := Some n
           | Some _ -> valid := false)
        | (Session.Const (name, _) | Session.Fun (name, _))
          when String.equal name anchor_name -> valid := false
        | Session.Const _ | Session.Fun _ -> ())
      bindings;
    match !valid, !value with
    | true, Some value ->
      let bindings =
        List.filter
          (fun binding -> not (String.equal (binding_name binding) anchor_name))
          bindings
      in
      Some { score = score direction value; optimum = { value; model = sorts, bindings } }
    | false, _ | true, None -> None
  ;;

  let optimize ?max_checks direction session objective =
    let max_checks = Option.value max_checks ~default:default_max_checks in
    if max_checks < 0 then invalid_arg "Optimize.Omt: max_checks must be nonnegative";
    validate_objective objective;
    if max_checks = 0
    then Unknown
    else (
      let anchor_name, anchor = fresh_objective session in
      let ctx = Session.context session in
      let normalized_anchor =
        match direction with
        | Minimize -> anchor
        | Maximize -> Context.neg ctx anchor
      in
      let checks = ref 0 in
      let can_check () = !checks < max_checks in
      let checked () =
        incr checks;
        Session.check_sat session
      in
      let read_candidate () =
        match Session.get_model session with
        | None -> None
        | Some model -> candidate_of_model ~direction ~anchor_name model
      in
      let probe_bound bound =
        if not (can_check ())
        then Probe_unknown
        else (
          let bound_term = Context.int_const_big ctx bound in
          let constraint_ = Context.le ctx normalized_anchor bound_term in
          Session.push session;
          Fun.protect
            ~finally:(fun () -> Session.pop session)
            (fun () ->
              Session.assert_term session constraint_;
              match checked () with
              | Session.Unknown -> Probe_unknown
              | Session.Unsat -> Infeasible
              | Session.Sat ->
                (match read_candidate () with
                 | Some candidate when Bigint.compare candidate.score bound <= 0 ->
                   Feasible candidate
                 | Some _ | None -> Probe_unknown)))
      in
      let midpoint low high =
        let distance = Bigint.sub high low in
        let half, _ = Bigint.divmod distance (Bigint.of_int 2) in
        Bigint.add low half
      in
      let rec refine infeasible candidate =
        let distance = Bigint.sub candidate.score infeasible in
        if Bigint.sign distance <= 0
        then Unknown
        else if Bigint.equal distance Bigint.one
        then Optimal candidate.optimum
        else (
          let bound = midpoint infeasible candidate.score in
          match probe_bound bound with
          | Probe_unknown -> Unknown
          | Infeasible -> refine bound candidate
          | Feasible better when Bigint.compare better.score infeasible > 0 ->
            refine infeasible better
          | Feasible _ -> Unknown)
      in
      let rec bracket step candidate =
        let bound = Bigint.sub candidate.score step in
        match probe_bound bound with
        | Probe_unknown -> Unknown
        | Infeasible -> refine bound candidate
        | Feasible better when Bigint.compare better.score candidate.score < 0 ->
          bracket (Bigint.add step step) better
        | Feasible _ -> Unknown
      in
      Session.push session;
      Fun.protect
        ~finally:(fun () -> Session.pop session)
        (fun () ->
          Session.assert_term session (Context.eq ctx anchor objective);
          match checked () with
          | Session.Unknown -> Unknown
          | Session.Unsat -> Hard_unsat
          | Session.Sat ->
            (match read_candidate () with
             | None -> Unknown
             | Some candidate -> bracket Bigint.one candidate)))
  ;;

  let minimize ?max_checks session objective =
    optimize ?max_checks Minimize session objective
  ;;

  let maximize ?max_checks session objective =
    optimize ?max_checks Maximize session objective
  ;;
end
