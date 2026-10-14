module Core = struct
module Term = Vox_smt_term
module Z = Vox_smt_integer.Z
module Q = Vox_smt_integer.Q
module Sat = Vox_sat_solver
module Arith = Vox_smt_arithmetic
module BV = Vox_smt_bitvector
module DT = Vox_smt_datatype
module Eq = Vox_smt_equality
module NL = Vox_smt_nonlinear
module Resource = Vox_smt_resource

exception Theory_stopped
exception Encoding_stopped of string
module IM = Map.Make (Int)
module Ids = Hashtbl.Make (struct
  type t = int
  let equal (x : int) y = x = y
  let hash x = x land max_int
end)

type value = Boolean of bool | Integer of Z.t | Bits of int * Z.t | Abstract of int
type result = Sat | Unsat of Term.term list | Unknown of string
type linear = { coefficients : Z.t IM.t; constant : Z.t }

let constant z = {coefficients = IM.empty; constant = z}
let variable x = {coefficients = IM.singleton x Z.one; constant = Z.zero}
let scale ?(poll = fun () -> ()) z a =
  if Z.is_zero z then constant Z.zero
  else {coefficients = IM.map (fun c -> poll (); Z.mul ~poll z c)
          a.coefficients;
        constant = Z.mul ~poll z a.constant}
let add ?(poll = fun () -> ()) a b =
  { coefficients = IM.union (fun _ x y ->
      poll ();
      let z = Z.add ~poll x y in if Z.is_zero z then None else Some z)
      a.coefficients b.coefficients;
    constant = Z.add ~poll a.constant b.constant }
let sub ?poll a b = add ?poll a (scale ?poll Z.minus_one b)

type bitvector_bounds =
  { mutable nonnegative : int option;
    mutable uppers : (Z.t * int) list }

type bitvector_quotient =
  { quotient : Term.term;
    remainder : Term.term;
    divisor : Z.t;
    limit : Z.t;
    result : Term.term;
    mutable emitted : bool }

type prepared_frame =
  { frame_assertions : Term.term list list;
    frame_substitutions : Term.term IM.t;
    frame_replay : Term.term list;
    parent_replay : Term.term list;
    frame_finite_products : bool }

type t =
  { terms : Term.t;
    mutable prepared_frames : prepared_frame option list;
    mutable finite_products : bool;
    mutable sat : Sat.t option;
    mutable bitvectors : BV.t option;
    mutable datatypes : DT.t option;
    mutable equality : Eq.t option;
    arithmetic : Arith.t;
    mutable arithmetic_result : Arith.lazy_consistent option;
    mutable arithmetic_dirty : bool;
    booleans : int Ids.t;
    mutable substitutions : Term.term IM.t;
    mutable rewrite_results : Term.term array;
    mutable occurrence_maxima : int array;
    mutable occurrence_stamps : int array;
    mutable occurrence_epoch : int;
    mutable rewrite_stamps : int array;
    mutable rewrite_epoch : int;
    mutable rewrite_parents : int array;
    mutable rewrite_edges : int array;
    mutable rewrite_edge_count : int;
    mutable rewrite_parent_epochs : int array;
    mutable rewrite_registered : int array;
    mutable substitution_scopes : Term.term IM.t list;
    mutable allocation_scopes : (int * int * int) list;
    mutable popped_allocations : int;
    mutable pending_assertions : Term.term list;
    mutable pending_scopes : Term.term list list;
    mutable replay_assertions : Term.term list;
    mutable replay_scopes : Term.term list list;
    mutable symmetry_cache :
      (Term.term list * Term.term list * Term.term list) option;
    mutable alternative : t option;
    mutable result_solver : t option;
    mutable counted_work : int option;
    mutable retired_statistics : Sat.statistics;
    mutable goal_first : bool;
    mutable comparison_congruence : bool;
    mutable theory_required : bool;
    mutable interrupted_theory : bool;
    mutable theory_extension : Sat.extension option;
    mutable pending_bitvector_ites : Term.term list;
    mutable arithmetic_work : int;
    used_terms : unit Ids.t;
    declared_terms : unit Ids.t;
    linear : linear Ids.t;
    arithmetic_atoms : unit Ids.t;
    applications : unit Ids.t;
    mutable equality_request_revision : int;
    mutable equality_predicates : Term.term list;
    mutable comparison_predicates : Term.term list;
    mutable arithmetic_revision : int;
    mutable comparison_request_revision : int * int;
    bitvector_terms : unit Ids.t;
    bitvector_bounds : bitvector_bounds Ids.t;
    bitvector_quotients : bitvector_quotient Ids.t;
    bitvector_quotient_uses : bitvector_quotient list Ids.t;
    mutable nonlinear : NL.t option;
    unsupported : string Ids.t;
    divmods : (Term.term * Term.term, Term.term * Term.term) Hashtbl.t;
    quotient_terms : unit Ids.t;
    explicit_divisions : (Term.term * Term.term, unit) Hashtbl.t;
    constant_divisions : (Term.term * Z.t * Term.term) Ids.t;
    strengthened_divisions : unit Ids.t;
    quotient_candidates : (Term.term * Term.term, unit) Hashtbl.t;
    mutable context : Sat.context option;
    mutable encoding_depth : int;
    mutable operation_active : bool;
    mutable resource : Resource.t option;
    mutable resource_sat_work : int;
    mutable search_remaining : int;
    mutable model : bool array option;
    mutable integer_model : Z.t Ids.t;
    mutable classes : int array;
    mutable cancelled : unit -> bool;
    mutable remaining : int;
    mutable budget : int;
    mutable base_sat_work : int;
    mutable incomplete : bool;
    mutable unknown_reason : string;
    mutable model_attempts : int;
    mutable projection_cache :
      (Term.term list * (Term.term * Term.term) list option) option;
    mutable cuts : int;
    mutable eager_cuts : bool;
    mutable optimize_cuts : bool;
    mutable cut_basis : Arith.cut_basis;
    mutable split_forms : bool;
    mutable splits_since_cut : int;
    mutable next_private : int;
    required_booleans : int Ids.t;
    mutable activity_terms : int;
    mutable occurrence_activity : bool
  }

let add_statistics (a : Sat.statistics) (b : Sat.statistics) =
  { Sat.solves = a.solves + b.solves;
    decisions = a.decisions + b.decisions;
    propagations = a.propagations + b.propagations;
    conflicts = a.conflicts + b.conflicts;
    restarts = a.restarts + b.restarts;
    learnt_clauses = a.learnt_clauses + b.learnt_clauses;
    deleted_clauses = a.deleted_clauses + b.deleted_clauses }

let sat t = match t.sat with Some s -> s | None -> failwith "SMT initialization"
let bitvectors t = match t.bitvectors with Some b -> b
  | None -> failwith "SMT bitvector initialization"
let nonlinear t = match t.nonlinear with Some n -> n
  | None -> failwith "SMT nonlinear initialization"
let equality t = match t.equality with Some e -> e
  | None -> failwith "SMT equality initialization"
let poll_resource ?(work = 1) t =
  match t.resource with
  | None -> ()
  | Some resource ->
    let stats = Sat.statistics (sat t) in
    let sat_work = stats.propagations + stats.conflicts in
    let delta = sat_work - t.resource_sat_work in
    t.resource_sat_work <- sat_work;
    if delta <> 0 then Resource.charge resource delta;
    if work = 1 then Resource.poll resource else Resource.charge resource work

let rec install_resource t resource f =
  let previous = t.resource and previous_work = t.resource_sat_work in
  let stats = Sat.statistics (sat t) in
  t.resource <- resource;
  t.resource_sat_work <- stats.propagations + stats.conflicts;
  Fun.protect ~finally:(fun () ->
    t.resource <- previous; t.resource_sat_work <- previous_work)
    (fun () ->
      let apply () = match t.result_solver with
        | Some solver when (match t.alternative with
            | None -> true | Some alternate -> solver != alternate) ->
          install_resource solver resource f
        | _ -> f () in
      match t.alternative with
      | None -> apply ()
      | Some alternate -> install_resource alternate resource apply)

let shared_poll t =
  match poll_resource t with
  | () -> ()
  | exception Resource.Exhausted reason ->
    t.unknown_reason <- reason;
    if not t.operation_active then raise (Resource.Exhausted reason)
    else if t.encoding_depth > 0 || t.context = None
    then raise (Encoding_stopped reason) else raise Theory_stopped

let poll t =
  shared_poll t;
  if t.operation_active then begin
    if t.remaining land 255 = 0 && t.cancelled () then begin
      t.unknown_reason <- "cancelled";
      if t.encoding_depth > 0 || t.context = None
      then raise (Encoding_stopped "cancelled") else raise Theory_stopped
    end;
    let stats = Sat.statistics (sat t) in
    let sat_work = stats.propagations + stats.conflicts - t.base_sat_work in
    let phase_stopped = t.context <> None && t.encoding_depth = 0
      && t.search_remaining = 0 in
    if t.remaining <= sat_work || phase_stopped then begin
      t.unknown_reason <- "resource limit";
      if t.encoding_depth > 0 || t.context = None
      then raise (Encoding_stopped "resource limit") else raise Theory_stopped
    end;
    t.remaining <- t.remaining - 1;
    if t.context <> None && t.encoding_depth = 0 then
      t.search_remaining <- t.search_remaining - 1
  end

let with_resource t resource f =
  match resource with
  | None -> f ()
  | Some _ -> install_resource t resource (fun () ->
      Term.with_poll t.terms (fun () -> shared_poll t) f)

let model_poll t =
  if Option.is_some t.resource then shared_poll t

let arithmetic_poll t =
  Option.map (fun _ () -> shared_poll t) t.resource
let scale t z a = scale ?poll:(arithmetic_poll t) z a
let add t a b = add ?poll:(arithmetic_poll t) a b
let sub t a b = sub ?poll:(arithmetic_poll t) a b

let reverse t list =
  let rec loop result = function
    | [] -> result
    | x :: rest -> poll t; loop (x :: result) rest in
  loop [] list

let encoding_poll t =
  t.encoding_depth <- t.encoding_depth + 1;
  match poll t with
  | () -> t.encoding_depth <- t.encoding_depth - 1
  | exception exn -> t.encoding_depth <- t.encoding_depth - 1; raise exn

let encoding_resource_poll t =
  t.encoding_depth <- t.encoding_depth + 1;
  match shared_poll t with
  | () -> t.encoding_depth <- t.encoding_depth - 1
  | exception exn -> t.encoding_depth <- t.encoding_depth - 1; raise exn

let register_boolean t term literal =
  match Term.Unsafe.node t.terms term with
  | Term.Boolean _ -> Eq.register_boolean (equality t) ~literal term
  | Term.Apply (symbol, arguments) ->
    (match (Term.Unsafe.signature t.terms symbol).kind with
    | Term.Uninterpreted when Array.length arguments = 0 -> ()
    | Term.Uninterpreted | Term.Selector _ | Term.Tester _ ->
      Eq.register_boolean (equality t) ~literal term
    | kind when t.comparison_congruence ->
      (match kind with
      | Term.Builtin ("<" | "<=" | ">" | ">=") ->
        Eq.register_boolean (equality t) ~literal term
      | _ -> ())
    | _ -> ())
  | _ -> ()

let register_datatype t term =
  Option.iter (fun d -> DT.register d term) t.datatypes
let fresh ?completion t =
  encoding_poll t;
  match t.context with
  | Some c -> Sat.new_theory_var ?completion c
  | None -> Sat.new_var ?completion (sat t)
let clause t xs =
  encoding_poll t;
  match t.context with
  | Some c -> Sat.add_lemma c xs
  | None -> Sat.add_clause_global (sat t) xs
let assigned t l = match t.context with
  | Some c -> Sat.assigned_value c l
  | None -> match t.model with
    | Some model when abs l < Array.length model ->
      Some (if l > 0 then model.(l) else not model.(-l))
    | _ -> None

let model_assigned t l = match t.context with
  | Some c -> Sat.completion_value c l
  | None -> assigned t l

let private_integer t =
  let name = Printf.sprintf "@aux%d" t.next_private in
  t.next_private <- t.next_private + 1;
  let symbol = Term.symbol t.terms
    {name; arguments = [||]; result = Term.Int; kind = Term.Uninterpreted} in
  Term.apply t.terms symbol [||]

let register_bound t lit form =
  t.arithmetic_dirty <- true;
  let poll = Option.map (fun _ () -> encoding_resource_poll t) t.resource in
  Arith.register ?poll t.arithmetic ~literal:lit (IM.bindings form.coefficients)
    (Z.neg form.constant);
  Ids.add t.arithmetic_atoms (abs lit) ()

let encoding t f =
  t.encoding_depth <- t.encoding_depth + 1;
  match (poll t; f ()) with
  | result -> t.encoding_depth <- t.encoding_depth - 1; result
  | exception exn -> t.encoding_depth <- t.encoding_depth - 1; raise exn

let disjunctive_equalities t branches =
  let branch term =
    let parents = Ids.create 16 and remaining = ref 64 in
    let rec root term =
      match Ids.find_opt parents term with
      | None -> Ids.add parents term term; term
      | Some parent ->
        if parent = term then term else
          let representative = root parent in
          Ids.replace parents term representative;
          representative in
    let unite left right =
      let left = root left and right = root right in
      if left <> right then Ids.replace parents (max left right) (min left right)
    in
    let rec visit term =
      poll t;
      if !remaining > 0 then begin
        decr remaining;
        match Term.Unsafe.node t.terms term with
        | Term.Apply (symbol, arguments) ->
          (match (Term.Unsafe.signature t.terms symbol).kind, arguments with
          | Term.Builtin "=", [|left; right|] ->
            (match Term.sort t.terms left with
            | Term.Named _ -> unite left right | _ -> ())
          | Term.Builtin "and", _ ->
            let index = ref 0 in
            while !index < Array.length arguments && !remaining > 0 do
              visit arguments.(!index); incr index
            done
          | _ -> ())
        | _ -> ()
      end in
    visit term;
    parents, root in
  if Array.length branches < 2 || Array.length branches > 8 then [] else
  let first, representative = branch branches.(0) in
  if Ids.length first < 2 then [] else
  let remaining = Array.init (Array.length branches - 1)
      (fun index -> snd (branch branches.(index + 1))) in
  let terms = Ids.fold (fun term _ terms -> term :: terms) first [] in
  let terms = List.sort Int.compare terms in
  let groups = Hashtbl.create 16 in
  List.filter_map (fun term ->
    poll t;
    let key = representative term ::
      Array.to_list (Array.map (fun root -> root term) remaining) in
    match Hashtbl.find_opt groups key with
    | None -> Hashtbl.add groups key term; None
    | Some prior -> Some (prior, term)) terms

let rec boolean t term = encoding t (fun () -> boolean_inner t term)

and boolean_inner t term =
  register_datatype t term;
  match Ids.find_opt t.booleans term with
  | Some l -> l
  | None ->
    (match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, [|argument|])
      when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "not" ->
      let l = -(boolean t argument) in
      Ids.add t.booleans term l;
      register_boolean t term l;
      l
    | _ -> boolean_fresh t term)

and boolean_fresh t term =
    if Term.sort t.terms term <> Term.Bool
    then invalid_arg "SMT Boolean expression required";
    let l = fresh t in
    Ids.add t.booleans term l;
    register_boolean t term l;
    (match Term.Unsafe.node t.terms term with
    | Term.Apply (s, args) when (Term.Unsafe.signature t.terms s).kind = Term.Builtin "=" ->
      Eq.register_equality (equality t) ~literal:l args.(0) args.(1);
      Option.iter (fun d ->
        DT.register_equality d ~literal:l args.(0) args.(1)) t.datatypes
    | _ -> ());
    (match Term.Unsafe.node t.terms term with
    | Term.Boolean b -> clause t [|if b then l else -l|]
    | Term.Apply (s, args) ->
      (match (Term.Unsafe.signature t.terms s).kind with
      | Term.Builtin ("and" | "or" as op) ->
        let xs = Array.map (boolean t) args in
        let l, xs = if op = "and" then l, xs else -l, Array.map (~-) xs in
        Array.iter (fun x -> clause t [|-l; x|]) xs;
        clause t (Array.append [|l|] (Array.map (~-) xs));
        if op = "or" then
          List.iter (fun (left, right) ->
            let equality = boolean t (Term.equal_atom t.terms left right) in
            clause t [|l; equality|]) (disjunctive_equalities t args)
      | Term.Builtin "distinct" ->
        let equalities = ref [] in
        for i = 0 to Array.length args - 1 do
          for j = i + 1 to Array.length args - 1 do
            poll t;
            let equal = boolean t
                (Term.equal_atom t.terms args.(i) args.(j)) in
            clause t [|-l; -equal|];
            equalities := equal :: !equalities
          done
        done;
        clause t (Array.of_list (l :: !equalities))
      | Term.Builtin ("=" | "xor" as op)
        when Term.sort t.terms args.(0) = Term.Bool ->
        let x = boolean t args.(0) and y = boolean t args.(1) in
        let l = if op = "=" then l else -l in
        clause t [|-l; -x; y|]; clause t [|-l; x; -y|];
        clause t [|l; x; y|]; clause t [|l; -x; -y|]
      | Term.Builtin "ite" ->
        let c = boolean t args.(0) in
        let x = boolean t args.(1) and y = boolean t args.(2) in
        clause t [|-c; -l; x|]; clause t [|-c; l; -x|];
        clause t [|c; -l; y|]; clause t [|c; l; -y|]
      | Term.Builtin "=" when Term.sort t.terms args.(0) = Term.Int ->
        NL.register_modular_term (nonlinear t) term;
        let a = arithmetic t args.(0) and b = arithmetic t args.(1) in
        let x = fresh t and y = fresh t in
        register_bound t x (sub t a b); register_bound t y (sub t b a);
        clause t [|-l; x|]; clause t [|-l; y|]; clause t [|l; -x; -y|]
      | Term.Builtin ("<=" | "<" | ">=" | ">" as op) ->
        if Array.for_all (fun term -> match Term.Unsafe.node t.terms term with
            | Term.Integer _ -> false | _ -> true) args then begin
          t.comparison_predicates <- term :: t.comparison_predicates;
          t.arithmetic_revision <- t.arithmetic_revision + 1
        end;
        NL.register_modular_term (nonlinear t) term;
        let a = arithmetic t args.(0) and b = arithmetic t args.(1) in
        let form = if op = ">=" || op = ">" then sub t b a else sub t a b in
        let form = if op = "<" || op = ">"
          then add t form (constant Z.one) else form in
        register_bound t l form
      | Term.Builtin op when Array.length args > 0
          && (match Term.sort t.terms args.(0) with
            | Term.Bitvec _ -> true | _ -> false) ->
        Array.iter (internalize t) args;
        (match BV.predicate (bitvectors t) op args with
        | Some value -> clause t [|-l; value|]; clause t [|l; -value|]
        | None -> Ids.replace t.unsupported term "bitvector predicate");
        bitvector_bound t op args l
      | Term.Builtin "=" ->
        internalize t args.(0); internalize t args.(1)
      | Term.Uninterpreted | Term.Selector _ | Term.Tester _ ->
        if (Term.Unsafe.signature t.terms s).kind = Term.Uninterpreted then
          t.equality_predicates <- term :: t.equality_predicates;
        Array.iter (shared t) args;
        Ids.replace t.applications term ()
      | _ -> Ids.replace t.unsupported term "unsupported Boolean operator")
    | _ -> invalid_arg "SMT Boolean sort invariant");
    l

and bitvector_bound t op args literal =
  (* Outside search, emitted clauses are permanent and cannot be deleted. *)
  if t.context <> None || Array.length args <> 2 then () else
  let op, left, right = match op with
    | "bvslt" -> "<", args.(0), args.(1)
    | "bvsle" -> "<=", args.(0), args.(1)
    | "bvsgt" -> "<", args.(1), args.(0)
    | "bvsge" -> "<=", args.(1), args.(0)
    | _ -> "", args.(0), args.(1) in
  if op = "" then () else
  let register term lower upper guard =
    let bounds = match Ids.find_opt t.bitvector_bounds term with
      | Some bounds -> Some bounds
      | None when Ids.length t.bitvector_bounds < 4096 ->
        let bounds = {nonnegative = None; uppers = []} in
        Ids.add t.bitvector_bounds term bounds;
        Some bounds
      | None -> None in
    match bounds with
    | None -> ()
    | Some bounds ->
      if lower && bounds.nonnegative = None then
        bounds.nonnegative <- Some guard;
      Option.iter (fun value ->
        if List.length bounds.uppers < 8
          && not (List.exists (fun (other, _) -> Z.equal value other)
                    bounds.uppers) then
          bounds.uppers <- (value, guard) :: bounds.uppers) upper;
      bitvector_quotient t term;
      Option.iter (List.iter (bitvector_quotient_clause t))
        (Ids.find_opt t.bitvector_quotient_uses term) in
  let constant term = match Term.Unsafe.node t.terms term with
    | Term.Bits (_, value) -> Some value
    | _ -> None in
  (match constant left with
  | Some value ->
    if Z.is_zero value then register right true None literal;
    if op = "<=" then register right false (Some value) (-literal)
  | None -> ());
  (match constant right with
  | Some value ->
    if Z.is_zero value then register left true None (-literal);
    if op = "<" then register left false (Some value) literal
  | None -> ())

and bitvector_quotient t remainder =
  if Ids.mem t.bitvector_quotients remainder
    || Ids.length t.bitvector_quotients >= 256 then () else
  let product = match Term.Unsafe.node t.terms remainder with
    | Term.Apply (symbol, [|source; product|])
      when (Term.Unsafe.signature t.terms symbol).kind
        = Term.Builtin "bvsub" ->
      (match Term.Unsafe.node t.terms source with
      | Term.Bits (width, value) -> Some (width, value, product)
      | _ -> None)
    | _ -> None in
  Option.iter (fun (width, value, product) ->
    let multiplication = match Term.Unsafe.node t.terms product with
      | Term.Apply (symbol, [|left; right|])
        when (Term.Unsafe.signature t.terms symbol).kind
          = Term.Builtin "bvmul" ->
        (match Term.Unsafe.node t.terms left,
               Term.Unsafe.node t.terms right with
        | Term.Bits (_, divisor), _ -> Some (divisor, right)
        | _, Term.Bits (_, divisor) -> Some (divisor, left)
        | _ -> None)
      | _ -> None in
    Option.iter (fun (divisor, quotient) ->
      let poll () = encoding_poll t in
      let half = Z.pow ~poll (Z.of_int 2) (width - 1) in
      let rec power_of_two value =
        poll ();
        if Z.equal value Z.one then true
        else if Z.sign value <= 0 then false else
        let quotient, remainder = Z.quot_rem ~poll value (Z.of_int 2) in
        Z.is_zero remainder && power_of_two quotient in
      if Z.compare divisor Z.one > 0 && Z.compare divisor half < 0
        && Z.sign value >= 0 && Z.compare value half < 0
        && power_of_two divisor then begin
        let uses = Option.value
            (Ids.find_opt t.bitvector_quotient_uses quotient) ~default:[] in
        let remainder_uses = Option.value
            (Ids.find_opt t.bitvector_quotient_uses remainder) ~default:[] in
        if List.length uses < 8 && List.length remainder_uses < 8 then begin
          let descriptor =
            { quotient; remainder; divisor;
              limit = Z.floor_div ~poll half divisor;
              result = Term.bits t.terms width
                (Z.floor_div ~poll value divisor);
              emitted = false } in
          Ids.add t.bitvector_quotients remainder descriptor;
          Ids.replace t.bitvector_quotient_uses quotient (descriptor :: uses);
          Ids.replace t.bitvector_quotient_uses remainder
            (descriptor :: remainder_uses);
          bitvector_quotient_clause t descriptor
        end
      end) multiplication) product

and bitvector_quotient_clause t descriptor =
  if not descriptor.emitted then begin
    let guards term bound =
      Option.bind (Ids.find_opt t.bitvector_bounds term) (fun bounds ->
        Option.bind bounds.nonnegative (fun lower ->
          Option.map (fun (_, upper) -> lower, upper)
            (List.find_opt (fun (value, _) -> Z.equal bound value)
              bounds.uppers))) in
    match guards descriptor.quotient descriptor.limit,
          guards descriptor.remainder descriptor.divisor with
    | Some (qlower, qupper), Some (rlower, rupper) ->
      let result = boolean t (Term.equal_atom t.terms
          descriptor.quotient descriptor.result) in
      clause t [|-qlower; -qupper; -rlower; -rupper; result|];
      descriptor.emitted <- true
    | _ -> ()
  end

and arithmetic t term = encoding t (fun () -> arithmetic_inner t term)

and arithmetic_inner t term =
  t.theory_required <- true;
  register_datatype t term;
  match Ids.find_opt t.linear term with
  | Some form -> form
  | None ->
    if Term.sort t.terms term <> Term.Int
    then invalid_arg "SMT integer expression required";
    NL.register_modular_term (nonlinear t) term;
    let form =
      match Term.Unsafe.node t.terms term with
      | Term.Integer z -> constant z
      | Term.Apply (s, args) ->
        (match (Term.Unsafe.signature t.terms s).kind with
        | Term.Builtin "+" ->
          Array.fold_left (fun a x -> add t a (arithmetic t x))
            (constant Z.zero) args
        | Term.Builtin "-" ->
          if Array.length args = 1
          then scale t Z.minus_one (arithmetic t args.(0))
          else Array.fold_left (fun a x -> sub t a (arithmetic t x))
            (arithmetic t args.(0)) (Array.sub args 1 (Array.length args - 1))
        | Term.Builtin "*" ->
          (match NL.normalize (nonlinear t) term with
          | Some normalized when normalized <> term ->
            if Array.fold_left (fun count factor ->
              match Term.Unsafe.node t.terms factor with
              | Term.Integer _ -> count | _ -> count + 1) 0 args >= 2
            then NL.register_product (nonlinear t) term;
            arithmetic t normalized
          | _ ->
          let coefficient, rest = Array.fold_left (fun (c, rest) x ->
            let a = arithmetic t x in
            if IM.is_empty a.coefficients
            then Z.mul ?poll:(arithmetic_poll t) c a.constant, rest
            else c, (x, a) :: rest) (Z.one, []) args in
          if Z.is_zero coefficient then constant Z.zero else
          (match rest with
          | [] -> constant coefficient
          | [_, a] -> scale t coefficient a
          | _ ->
            let factors = Array.of_list (List.rev_map fst rest) in
            let product = Term.builtin t.terms "*" Term.Int factors in
            if product = term then begin
              NL.register_product (nonlinear t) term;
              variable term
            end else scale t coefficient (arithmetic t product)))
        | Term.Builtin "ite" ->
          Ids.add t.linear term (variable term);
          let c = boolean t args.(0) in
          let a = boolean t (Term.equal_atom t.terms term args.(1)) in
          let b = boolean t (Term.equal_atom t.terms term args.(2)) in
          clause t [|-c; a|]; clause t [|c; b|]; variable term
        | Term.Builtin ("div" | "mod" as op) ->
          let result = match Term.Unsafe.node t.terms args.(0),
              Term.Unsafe.node t.terms args.(1) with
            | Term.Apply (symbol, [|condition; left; right|]), Term.Integer _
              when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "ite"
                && (Hashtbl.mem t.explicit_divisions (left, args.(1))
                    || Hashtbl.mem t.explicit_divisions (right, args.(1))) ->
              Term.builtin t.terms "ite" Term.Int
                [|condition; Term.builtin t.terms op Term.Int [|left; args.(1)|];
                  Term.builtin t.terms op Term.Int [|right; args.(1)|]|]
            | _ ->
              let q, r = divmod t args.(0) args.(1) in
              if op = "div" then begin
                match Term.Unsafe.node t.terms args.(1) with
                | Term.Integer divisor when not (Z.is_zero divisor) ->
                  division_signs t args.(0) divisor q None
                | _ -> ()
              end;
              if op = "div" then q else r in
          Eq.register_equality (equality t)
            ~literal:(boolean t (Term.boolean t.terms true)) term result;
          arithmetic t result
        | Term.Builtin ("bv2int" | "ubv_to_int" | "sbv_to_int" as op) ->
          internalize t args.(0);
          let bits = BV.bits (bitvectors t) args.(0) in
          bit_sum t ~signed:(op = "sbv_to_int") bits
        | Term.Builtin ("int.pow2" | "int.log2" | "piand") ->
          Array.iter (shared t) args;
          NL.register_extension (nonlinear t) term;
          Ids.replace t.applications term ();
          variable term
        | Term.Uninterpreted | Term.Selector _ ->
          Array.iter (shared t) args;
          Ids.replace t.applications term (); variable term
        | _ -> Ids.replace t.unsupported term "unsupported integer operator";
          variable term)
      | _ -> invalid_arg "SMT integer sort invariant"
    in
    Ids.replace t.linear term form;
    form

and divmod t x y =
  match Hashtbl.find_opt t.divmods (x, y) with
  | Some pair -> pair
  | None ->
    let q = private_integer t and r = private_integer t in
    Hashtbl.add t.divmods (x, y) (q, r);
    ignore (arithmetic t q); ignore (arithmetic t r);
    (match Term.Unsafe.node t.terms y with
    | Term.Integer divisor when not (Z.is_zero divisor) ->
      Ids.add t.constant_divisions q (x, divisor, q);
      Ids.add t.constant_divisions r (x, divisor, q);
      constrain_division t x y q r None;
      if Ids.mem t.quotient_terms x then division_signs t x divisor q None
    | Term.Integer _ ->
      ignore (arithmetic t x); ignore (arithmetic t y)
    | _ ->
      NL.register_division (nonlinear t) ~dividend:x ~divisor:y
        ~quotient:q ~remainder:r;
      ignore (arithmetic t x); ignore (arithmetic t y));
    q, r

and constrain_division t x y q r guard =
  let int = Term.integer t.terms in
  let divisor = match Term.Unsafe.node t.terms y with
    | Term.Integer z -> z | _ -> invalid_arg "SMT division refinement" in
  let product = Term.builtin t.terms "*" Term.Int [|y; q|] in
  let sum = Term.builtin t.terms "+" Term.Int [|product; r|] in
  let zero = int Z.zero in
  let equations = [Term.equal t.terms x sum;
    Term.builtin t.terms "<=" Term.Bool [|zero; r|];
    Term.builtin t.terms "<" Term.Bool [|r; int (Z.abs divisor)|]] in
  List.iter (fun p ->
    let l = boolean t p in
    clause t (match guard with None -> [|l|] | Some g -> [|-g; l|])) equations;
  Option.iter (fun _ -> division_signs t x divisor q guard) guard

and division_signs t x divisor q guard =
  if guard = None && Ids.mem t.strengthened_divisions q then () else begin
    let int = Term.integer t.terms in
    let zero = int Z.zero in
    let nonnegative = boolean t
        (Term.builtin t.terms ">=" Term.Bool [|x; zero|]) in
    let small = boolean t
        (Term.builtin t.terms "<" Term.Bool [|x; int (Z.abs divisor)|]) in
    let sign = if Z.sign divisor > 0 then ">=" else "<=" in
    let other = if Z.sign divisor > 0 then "<=" else ">=" in
    List.iter (fun (condition, operator) ->
      let conclusion = boolean t
          (Term.builtin t.terms operator Term.Bool [|q; zero|]) in
      let add literals = clause t (match guard with None -> literals
        | Some g -> Array.append [|-g|] literals) in
      add [|-condition; conclusion|];
      add [|condition; -conclusion|]) [nonnegative, sign; small, other];
    if guard = None then Ids.replace t.strengthened_divisions q ()
  end

and bit_sum t ~signed bits =
  let sum = ref (constant Z.zero) and coefficient = ref Z.one in
  Array.iteri (fun i lit ->
    let symbol = Term.symbol t.terms
      {name = Printf.sprintf "@bit%d" lit; arguments = [||];
       result = Term.Bool; kind = Term.Uninterpreted} in
    let bit = Term.apply t.terms symbol [||] in
    Ids.add t.booleans bit lit;
    let one = Term.integer t.terms Z.one and zero = Term.integer t.terms Z.zero in
    let value = arithmetic t (Term.ite t.terms bit one zero) in
    let c = if signed && i = Array.length bits - 1
      then Z.neg !coefficient else !coefficient in
    sum := add t !sum (scale t c value);
    coefficient := Z.mul ?poll:(arithmetic_poll t)
        !coefficient (Z.of_int 2)) bits;
  !sum

and internalize t term = encoding t (fun () -> internalize_inner t term)

and internalize_inner t term =
  Eq.register (equality t) term;
  register_datatype t term;
  match Term.sort t.terms term with
  | Term.Bool -> ignore (boolean t term)
  | Term.Int -> ignore (arithmetic t term)
  | Term.Bitvec _ ->
    if not (Ids.mem t.bitvector_terms term) then begin
    Ids.add t.bitvector_terms term ();
    (try
      ignore (BV.bits (bitvectors t) term);
      (match Term.Unsafe.node t.terms term with
      | Term.Apply (s, args) ->
        (match (Term.Unsafe.signature t.terms s).kind with
        | Term.Uninterpreted | Term.Selector _ ->
          Array.iter (shared t) args;
          Ids.replace t.applications term ()
        | Term.Builtin "ite" ->
          Array.iter (internalize t) args;
          if t.theory_required then bitvector_ite t term args
          else t.pending_bitvector_ites <- term :: t.pending_bitvector_ites
        | Term.Builtin _ -> Array.iter (internalize t) args
        | _ -> Ids.replace t.unsupported term "datatype bitvector")
      | _ -> ())
    with BV.Unsupported why -> Ids.replace t.unsupported term why)
    end
  | Term.Named _ ->
    t.theory_required <- true;
    if not (Ids.mem t.applications term) then begin
      Ids.add t.applications term ();
      match Term.Unsafe.node t.terms term with
      | Term.Apply (s, args) ->
        (match (Term.Unsafe.signature t.terms s).kind with
        | Term.Builtin "ite" ->
          let c = boolean t args.(0) in
          let a = boolean t (Term.equal_atom t.terms term args.(1)) in
          let b = boolean t (Term.equal_atom t.terms term args.(2)) in
          clause t [|-c; a|]; clause t [|c; b|]
        | Term.Uninterpreted | Term.Constructor _ | Term.Selector _ ->
          Array.iter (shared t) args
        | _ -> Ids.replace t.unsupported term "datatype support pending")
      | _ -> invalid_arg "SMT abstract sort invariant"
    end

and bitvector_ite t term args =
  let c = boolean t args.(0) in
  let a = boolean t (Term.equal_atom t.terms term args.(1)) in
  let b = boolean t (Term.equal_atom t.terms term args.(2)) in
  clause t [|-c; a|]; clause t [|c; b|]

and shared t term =
  t.theory_required <- true;
  internalize t term;
  if Term.sort t.terms term = Term.Bool then
    Eq.register_boolean (equality t) ~literal:(boolean t term) term

let rec representative t x =
  model_poll t;
  if t.classes.(x) = x then x
  else let p = representative t t.classes.(x) in t.classes.(x) <- p; p

let value t term =
  model_poll t;
  let poll () = model_poll t in
  match Term.sort t.terms term with
  | Term.Bool -> Option.map (fun b -> Boolean b)
      (Option.bind (Ids.find_opt t.booleans term) (model_assigned t))
  | Term.Int ->
    Option.bind (Ids.find_opt t.linear term) (fun a ->
      let total = IM.fold (fun x c total ->
        poll (); Option.bind total (fun sum ->
        Option.map (fun v -> Z.add ~poll sum (Z.mul ~poll c v))
          (Ids.find_opt t.integer_model x))) a.coefficients (Some a.constant) in
      Option.map (fun z -> Integer z) total)
  | Term.Named _ when term < Array.length t.classes
      && Ids.mem t.applications term ->
    Some (Abstract (representative t term))
  | Term.Bitvec width ->
    if not (Ids.mem t.bitvector_terms term) then None else
    let bits = BV.bits (bitvectors t) term in
    let result = ref Z.zero and known = ref true in
    for i = Array.length bits - 1 downto 0 do
      poll ();
      result := Z.mul ~poll !result (Z.of_int 2);
      match model_assigned t bits.(i) with
      | Some true -> result := Z.add ~poll !result Z.one
      | Some false -> ()
      | None -> known := false
    done;
    if !known then Some (Bits (width, !result)) else None
  | Term.Named _ -> None

let equality_check t context =
  match Eq.check_lazy (equality t) with
  | Eq.Lazy_conflict reason ->
    Sat.conflict context ~reason:(fun () -> reason); false
  | Eq.Lazy_consistent implications ->
    List.iter (fun (l, reason) ->
      match Sat.assigned_value context l with
      | None -> Sat.imply context l ~reason
      | Some true -> ()
      | Some false ->
        let reason = Array.append [|-l|] (reason ()) in
        Sat.conflict context ~reason:(fun () -> reason)) implications;
    true

let arithmetic_check ?(phase = Arith.Propagation) t context =
  let before = t.remaining in
  let result = if t.arithmetic_dirty || phase = Arith.Final then
      let stats = Sat.statistics (sat t) in
      let sat_work = stats.propagations + stats.conflicts - t.base_sat_work in
      let allowance = max 0 (min (t.remaining - sat_work)
          t.search_remaining) in
      let remaining = ref allowance in
      poll_resource ~work:0 t;
      let resource = t.resource in
      let charge () =
        if !remaining = 0 then false
        else begin
          Option.iter Resource.poll resource;
          decr remaining; true
        end in
      let model_search = phase = Arith.Final && t.model_attempts > 0 in
      if model_search then t.model_attempts <- t.model_attempts - 1;
      let cuts = not t.split_forms && t.cuts < 16
        && ((t.eager_cuts && Arith.variable_count t.arithmetic <= 32)
            || t.splits_since_cut >= 8) in
      Fun.protect ~finally:(fun () ->
        let used = allowance - !remaining in
        t.remaining <- t.remaining - used;
        t.search_remaining <- t.search_remaining - used)
        (fun () -> Arith.check_lazy ~cut_basis:t.cut_basis
          ~split_forms:t.split_forms
          ~optimize:(t.optimize_cuts || t.cuts >= 8)
          ~model_search ~phase ~cuts ~charge
          ~cancelled:t.cancelled t.arithmetic)
    else match t.arithmetic_result with Some r -> Arith.Consistent r
      | None -> Arith.Unknown in
  t.arithmetic_work <- t.arithmetic_work + (before - t.remaining);
  match result with
  | Arith.Conflict reason ->
    Sat.conflict context ~reason:(fun () -> reason); None
  | Arith.Unknown ->
    t.incomplete <- true; t.unknown_reason <- "arithmetic resource limit"; None
  | Arith.Consistent model ->
    t.arithmetic_dirty <- false;
    t.arithmetic_result <- Some model;
    List.iter (fun (l, reason) ->
      if Sat.assigned_value context l = None
      then Sat.imply context l ~reason:(fun () -> Lazy.force reason))
      model.propagations;
    Some model

let demand_split_equality t context split =
  let exception Found in
  let exception Limit in
  let remaining = ref 256 and visited = Ids.create 32 in
  let spend () =
    poll t;
    if !remaining = 0 then raise Limit;
    decr remaining in
  let fixed term =
    match Term.Unsafe.node t.terms term with
    | Term.Integer _ -> true
    | _ ->
      match Ids.find_opt t.linear term with
      | None -> false
      | Some form -> IM.for_all (fun id _ ->
          spend (); Arith.is_fixed t.arithmetic id) form.coefficients in
  let rational term =
    match Term.Unsafe.node t.terms term with
    | Term.Integer z -> Some (Q.of_z z)
    | _ -> Option.bind (Ids.find_opt t.linear term) (fun form ->
      IM.fold (fun id coefficient sum ->
        spend ();
        Option.bind sum (fun sum ->
          Option.map (fun value ->
            Q.add ~poll:(fun () -> poll t) sum
              (Q.mul ~poll:(fun () -> poll t) (Q.of_z coefficient) value))
            (Arith.value t.arithmetic id))) form.coefficients
        (Some (Q.of_z form.constant))) in
  try
    Arith.iter_split_support t.arithmetic split ~f:(fun term ->
      spend ();
      if Eq.is_registered (equality t) term
        && not (Ids.mem visited term) then begin
        Ids.add visited term ();
        match rational term with
        | None -> ()
        | Some value ->
          let members = Eq.members (equality t) term in
          List.iter (fun _ -> spend ()) members;
          let members = List.sort (fun a b -> spend (); Int.compare a b)
              members in
          List.iter (fun other ->
            spend ();
            if term <> other then match rational other with
            | Some other_value when not (Q.equal value other_value)
                && (term = split || fixed term || fixed other) ->
              let atom = Term.equal t.terms term other in
              if not (Ids.mem t.booleans atom) then begin
                let reason = Eq.explain_later (equality t) term other in
                let literal = boolean t atom in
                Sat.imply context literal ~reason;
                raise Found
              end
            | _ -> ()) members
      end);
    false
  with Found -> true | Limit -> false

let request_integer_equalities t =
  let eq = equality t in
  let revision = Eq.revision eq in
  if t.equality_predicates = [] || not (NL.has_moduli (nonlinear t))
    || t.equality_request_revision = revision then false else begin
    t.equality_request_revision <- revision;
    let exception Limit in
    let remaining = ref 4096 in
    let spend () =
      poll t;
      if !remaining = 0 then raise Limit;
      decr remaining in
    let module Pairs = Set.Make (struct
      type t = Term.term * Term.term
      let compare left right = spend (); Stdlib.compare left right
    end) in
    let pairs = ref Pairs.empty in
    let constructor term =
      let rec find = function
        | [] -> None
        | term :: rest ->
          spend ();
          match Term.Unsafe.node t.terms term with
          | Term.Apply (symbol, arguments) ->
            (match (Term.Unsafe.signature t.terms symbol).kind with
            | Term.Constructor _ -> Some (symbol, arguments)
            | _ -> find rest)
          | _ -> find rest in
      find (Eq.members eq term) in
    let visited = Hashtbl.create 32 in
    let rec descend left right =
      spend ();
      if Eq.representative eq left <> Eq.representative eq right then begin
        let pair = if left < right then left, right else right, left in
        if not (Hashtbl.mem visited pair) then begin
          Hashtbl.add visited pair ();
          match Term.sort t.terms left with
          | Term.Int -> pairs := Pairs.add pair !pairs
          | Term.Named _ ->
            (match constructor left, constructor right with
            | Some (a, xs), Some (b, ys) when a = b ->
              Array.iter2 descend xs ys
            | _ -> ())
          | Term.Bool | Term.Bitvec _ -> ()
        end
      end in
    let module Index = Hashtbl.Make (struct
      type t = Term.symbol * int * Term.term array * int
      let hash (symbol, position, _, hash) =
        Hashtbl.hash (symbol, position, hash)
      let equal (symbol, position, roots, _) (other, index, values, _) =
        symbol = other && position = index
        && Array.length roots = Array.length values
        && let rec equal i =
          if i = Array.length roots then true else begin
            spend ();
            (i = position || roots.(i) = values.(i)) && equal (i + 1)
          end in equal 0
    end) in
    let index = Index.create 128 in
    (try
      let applications = List.fold_left (fun terms term ->
        spend (); term :: terms) [] t.equality_predicates
        |> List.sort (fun x y -> spend (); Int.compare x y) in
      List.iter (fun term ->
        spend ();
        match Term.Unsafe.node t.terms term,
            Option.bind (Ids.find_opt t.booleans term) (assigned t) with
        | Term.Apply (symbol, arguments), Some truth
          when (Term.Unsafe.signature t.terms symbol).kind
            = Term.Uninterpreted ->
          let roots = Array.map (fun term ->
            spend (); Eq.representative eq term) arguments in
          let full_hash = ref 0 in
          let hashes = Array.mapi (fun position root ->
            spend ();
            let hash = Hashtbl.hash (position, root) in
            full_hash := !full_hash lxor hash; hash) roots in
          Array.iteri (fun position argument ->
            spend ();
            let hash = !full_hash lxor hashes.(position) in
            let key = symbol, position, roots, hash in
            let bucket = Option.value (Index.find_opt index key) ~default:[] in
            List.iter (fun (other_truth, other_argument) ->
              spend ();
              if truth <> other_truth then descend argument other_argument)
              bucket;
            let same_truth = List.fold_left (fun count (other, _) ->
              spend (); if other = truth then count + 1 else count) 0 bucket in
            if same_truth < 2 then
              Index.replace index key ((truth, argument) :: bucket)) arguments
        | _ -> ()) applications
    with Limit -> ());
    let pairs = Pairs.elements !pairs in
    NL.request_modular_equalities (nonlinear t)
      ~representative:(Eq.representative eq) ~explain:(Eq.explain eq) pairs
  end

let request_comparison_relations t =
  let eq = equality t in
  let revision = Eq.revision eq, t.arithmetic_revision in
  if t.comparison_predicates = [] || not (NL.has_moduli (nonlinear t))
    || t.comparison_request_revision = revision then false else begin
    t.comparison_request_revision <- revision;
    let exception Limit in
    let remaining = ref 2048 in
    let spend () =
      poll t;
      if !remaining = 0 then raise Limit;
      decr remaining in
    let index = Hashtbl.create 32 and pairs = Hashtbl.create 32 in
    (try
      List.iter (fun term ->
        spend ();
        match Term.Unsafe.node t.terms term,
            Option.bind (Ids.find_opt t.booleans term) (assigned t) with
        | Term.Apply (symbol, [|left; right|]), Some truth ->
          Array.iter (fun (position, shared, argument) ->
            spend ();
            let key = symbol, position, Eq.representative eq shared in
            let bucket = Option.value (Hashtbl.find_opt index key)
                ~default:[] in
            List.iter (fun (other_truth, other) ->
              spend ();
              if truth <> other_truth && argument <> other then
                let pair = if argument < other then argument, other
                  else other, argument in
                Hashtbl.replace pairs pair ()) bucket;
            let same = List.fold_left (fun count (other, _) ->
              spend (); if truth = other then count + 1 else count) 0 bucket in
            if same < 2 then
              Hashtbl.replace index key ((truth, argument) :: bucket))
            [|0, left, right; 1, right, left|]
        | _ -> ()) t.comparison_predicates
    with Limit -> ());
    let pairs = Hashtbl.fold (fun pair () pairs -> poll t; pair :: pairs)
        pairs [] |> List.sort (fun a b -> poll t; Stdlib.compare a b) in
    NL.request_modular_equalities ~affine:true (nonlinear t)
      ~representative:(Eq.representative eq) ~explain:(Eq.explain eq) pairs
  end

let quotient_uniqueness t split =
  match Ids.find_opt t.constant_divisions split with
  | None -> false
  | Some (source, divisor, quotient) ->
    begin
      let candidates = ref [] in
      Ids.iter (fun _ form ->
        poll t;
        if Z.is_zero form.constant then
          match IM.to_seq form.coefficients () with
          | Seq.Cons ((candidate, coefficient), rest)
            when rest () = Seq.Nil && candidate <> quotient
              && not (Hashtbl.mem t.quotient_candidates
                (quotient, candidate))
              && Z.equal coefficient divisor
              && not (Ids.mem t.constant_divisions candidate) ->
            candidates := candidate :: !candidates
          | _ -> ()) t.linear;
      let candidates = List.sort_uniq (fun x y ->
        poll t; Int.compare x y) !candidates in
      let rational term =
        Option.bind (Ids.find_opt t.linear term) (fun form ->
          IM.fold (fun id coefficient sum ->
            poll t;
            Option.bind sum (fun sum ->
              Option.map (fun value ->
                Q.add ~poll:(fun () -> poll t) sum
                  (Q.mul ~poll:(fun () -> poll t)
                    (Q.of_z coefficient) value))
                (Arith.value t.arithmetic id))) form.coefficients
            (Some (Q.of_z form.constant))) in
      let candidates = List.filter (fun candidate ->
        match rational source, rational candidate with
        | Some source, Some value ->
          let delta = Q.sub ~poll:(fun () -> poll t) source
              (Q.mul ~poll:(fun () -> poll t) (Q.of_z divisor) value) in
          Q.compare ~poll:(fun () -> poll t) delta Q.zero >= 0
          && Q.compare ~poll:(fun () -> poll t)
               delta (Q.of_z (Z.abs divisor)) < 0
        | _ -> false) candidates in
      let int = Term.integer t.terms in
      List.iter (fun candidate ->
        poll t;
        let product = Term.builtin t.terms "*" Term.Int
            [|int divisor; candidate|] in
        let delta = Term.builtin t.terms "-" Term.Int [|source; product|] in
        let lower = boolean t (Term.builtin t.terms ">=" Term.Bool
            [|delta; int Z.zero|]) in
        let upper = boolean t (Term.builtin t.terms "<" Term.Bool
            [|delta; int (Z.abs divisor)|]) in
        let result = boolean t (Term.equal_atom t.terms quotient candidate) in
        clause t [|-lower; -upper; result|];
        Hashtbl.replace t.quotient_candidates (quotient, candidate) ())
        candidates;
      candidates <> []
    end

let final_check t context =
  if Eq.is_pure_uf (equality t) && not (Term.has_datatypes t.terms)
      && Ids.length t.linear = 0 && Ids.length t.bitvector_terms = 0
      && Ids.length t.unsupported = 0 && not t.incomplete then begin
    t.classes <- Eq.snapshot (equality t);
    Sat.Done
  end else
  let ready = match t.datatypes with
    | None -> true
    | Some d ->
      (match DT.check d ~representative:(Eq.representative (equality t))
          ~assigned:(assigned t) with
      | DT.Done -> true
      | DT.Continue -> false
      | DT.Incomplete why ->
        t.incomplete <- true; t.unknown_reason <- why; false)
  in
  if not ready then (if t.incomplete then Sat.Incomplete else Sat.Continue)
  else if request_integer_equalities t || request_comparison_relations t
    then Sat.Continue
  else match arithmetic_check ~phase:Arith.Final t context with
  | None -> if t.incomplete then Sat.Incomplete else Sat.Continue
  | Some {Arith.split = Some (x, _); _}
    when demand_split_equality t context x || quotient_uniqueness t x ->
      Sat.Continue
  | Some {Arith.split = Some (x, bound); model; cut; split_form; _} ->
    Option.iter (fun (source, divisor, q) ->
      division_signs t source divisor q None)
      (Ids.find_opt t.constant_divisions x);
    let candidate = lazy (
      t.integer_model <- Ids.create (List.length model);
      List.iter (fun (x, q) -> Ids.replace t.integer_model x (Q.floor q))
        model) in
    let integer term =
      Lazy.force candidate;
      match value t term with Some (Integer z) -> Some z | _ -> None in
    let changed = NL.propagate_products ~affine_bounds:(not t.finite_products)
        (nonlinear t) ~integer
        ~representative:(Eq.representative (equality t))
        ~explain:(Eq.explain (equality t)) in
    if not changed then begin
      match cut with
      | Some (terms, boundary, reasons)
        when t.cuts < 16
        && ((t.eager_cuts && Arith.variable_count t.arithmetic <= 32)
            || t.splits_since_cut >= 8)
          && ((t.eager_cuts && Arith.variable_count t.arithmetic <= 32)
              || t.cuts > 0 || Array.length reasons <= 16) ->
        t.cuts <- t.cuts + 1; t.splits_since_cut <- 0;
        let sum = Term.builtin t.terms "+" Term.Int
            (Array.of_list (List.map (fun (term, coefficient) ->
              Term.builtin t.terms "*" Term.Int
                [|Term.integer t.terms coefficient; term|]) terms)) in
        let atom = Term.builtin t.terms "<=" Term.Bool
            [|sum; Term.integer t.terms boundary|] in
        clause t (Array.append (Array.map Int.neg reasons)
          [|boolean t atom|])
      | _ ->
        t.splits_since_cut <- t.splits_since_cut + 1;
        let term, bound = match split_form with
          | None -> x, bound
          | Some (terms, bound) ->
            let sum = Term.builtin t.terms "+" Term.Int
                (Array.of_list (List.map (fun (term, coefficient) ->
                  Term.builtin t.terms "*" Term.Int
                    [|Term.integer t.terms coefficient; term|]) terms)) in
            sum, bound in
        let atom = Term.builtin t.terms "<=" Term.Bool
          [|term; Term.integer t.terms bound|] in
        ignore (boolean t atom)
    end;
    Sat.Continue
  | Some model ->
    t.integer_model <- Ids.create (List.length model.model);
    List.iter (fun (x, q) -> Ids.replace t.integer_model x (Q.numerator q))
      model.model;
    (* Unconstrained integer terms may be absent from the tableau. *)
    Ids.iter (fun _ form -> IM.iter (fun x _ ->
      if not (Ids.mem t.integer_model x)
      then Ids.add t.integer_model x Z.zero) form.coefficients) t.linear;
    t.classes <- Eq.snapshot (equality t);
    let progress = ref false in
    let applications = Ids.fold (fun term () acc -> term :: acc)
      t.applications [] in
    let groups = Hashtbl.create 32 in
    List.iter (fun term -> match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, args) ->
        let key = symbol, Array.map (value t) args in
        if Array.for_all Option.is_some (snd key) then begin
          match Hashtbl.find_opt groups key with
          | Some other when value t term <> value t other ->
            let other_args = match Term.Unsafe.node t.terms other with
              | Term.Apply (_, args) -> args | _ -> [||] in
            let premises = Array.map2 (fun x y ->
              -(boolean t (Term.equal t.terms x y))) args other_args in
            let conclusion =
              boolean t (Term.equal_atom t.terms term other) in
            clause t (Array.append premises [|conclusion|]); progress := true
          | _ -> Hashtbl.replace groups key term
        end
      | _ -> ()) applications;
    let integer term = match value t term with
      | Some (Integer z) -> Some z | _ -> None in
    let divmods = Hashtbl.fold (fun key pair acc -> (key, pair) :: acc) t.divmods [] in
    let zero_group = Hashtbl.create 4 in
    List.iter (fun ((x, y), (q, r)) ->
      match integer x, integer y, integer q, integer r with
      | Some xv, Some yv, Some qv, Some rv when not (Z.is_zero yv) ->
        let eq, er = Z.div_rem ?poll:(arithmetic_poll t) xv yv in
        if not (Z.equal eq qv && Z.equal er rv) then begin
          let denominator = Term.integer t.terms yv in
          let guard = boolean t (Term.equal t.terms y denominator) in
          constrain_division t x denominator q r (Some guard); progress := true
        end
      | Some xv, Some yv, Some qv, Some rv when Z.is_zero yv ->
        (* Division by zero is an uninterpreted, total function in SMT-LIB. *)
        (match Hashtbl.find_opt zero_group xv with
        | Some (a, b, aq, ar, aqv, arv) ->
          List.iter (fun (u, v, uv, vv) ->
            if not (Z.equal uv vv) then begin
              let ax = boolean t (Term.equal t.terms x a) in
              let by = boolean t (Term.equal t.terms y b) in
              let result = boolean t (Term.equal_atom t.terms u v) in
              clause t [|-ax; -by; result|]; progress := true
            end) [q, aq, qv, aqv; r, ar, rv, arv]
        | None -> Hashtbl.add zero_group xv (x, y, q, r, qv, rv))
      | _ -> t.incomplete <- true; t.unknown_reason <- "division model") divmods;
    (match NL.check ~affine_bounds:(not t.finite_products) (nonlinear t) ~integer
      ~representative:(Eq.representative (equality t))
      ~explain:(Eq.explain (equality t)) with
    | NL.Done -> ()
    | NL.Continue -> progress := true
    | NL.Incomplete why ->
      if not !progress then begin
        t.incomplete <- true; t.unknown_reason <- why
      end);
    if !progress then Sat.Continue
    else if Ids.length t.unsupported > 0 then begin
      t.unknown_reason <- snd (List.hd
        (Ids.fold (fun x why xs -> (x, why) :: xs) t.unsupported []));
      Sat.Incomplete
    end else if t.incomplete then Sat.Incomplete else Sat.Done

let create terms =
  let t =
    { terms; prepared_frames = [None]; finite_products = false; sat = None;
      bitvectors = None; datatypes = None;
      equality = None;
      arithmetic = Arith.create ();
      arithmetic_result = None; arithmetic_dirty = true;
      booleans = Ids.create 1024; linear = Ids.create 1024;
      substitutions = IM.empty; substitution_scopes = [];
      allocation_scopes = []; popped_allocations = 0;
      rewrite_results = [||]; occurrence_maxima = [||];
      occurrence_stamps = [||]; occurrence_epoch = 0;
      rewrite_stamps = [||]; rewrite_epoch = 1;
      rewrite_parents = [||]; rewrite_parent_epochs = [||];
      rewrite_edges = [||]; rewrite_edge_count = 0;
      rewrite_registered = [||];
      pending_assertions = []; pending_scopes = [];
      replay_assertions = []; replay_scopes = []; symmetry_cache = None;
      alternative = None; result_solver = None; counted_work = None;
      goal_first = false; comparison_congruence = false;
      theory_required = false; theory_extension = None;
      interrupted_theory = false;
      pending_bitvector_ites = []; arithmetic_work = 0;
      retired_statistics =
        { Sat.solves = 0; decisions = 0; propagations = 0; conflicts = 0;
          restarts = 0; learnt_clauses = 0; deleted_clauses = 0 };
      used_terms = Ids.create 1024;
      declared_terms = Ids.create 128;
      arithmetic_atoms = Ids.create 1024;
      applications = Ids.create 128; nonlinear = None;
      equality_request_revision = -1; equality_predicates = [];
      comparison_predicates = []; arithmetic_revision = 0;
      comparison_request_revision = -1, -1;
      bitvector_terms = Ids.create 128;
      bitvector_bounds = Ids.create 128;
      bitvector_quotients = Ids.create 32;
      bitvector_quotient_uses = Ids.create 64;
      unsupported = Ids.create 8; divmods = Hashtbl.create 32;
      quotient_terms = Ids.create 32;
      explicit_divisions = Hashtbl.create 32;
      constant_divisions = Ids.create 64;
      strengthened_divisions = Ids.create 32;
      quotient_candidates = Hashtbl.create 32;
      context = None; encoding_depth = 0; operation_active = false;
      resource = None; resource_sat_work = 0;
      search_remaining = max_int; model = None; integer_model = Ids.create 128;
      classes = [||]; cancelled = (fun () -> false);
      remaining = max_int; budget = max_int; base_sat_work = 0;
      incomplete = false; unknown_reason = "incomplete";
      model_attempts = 0; projection_cache = None; eager_cuts = true;
      optimize_cuts = false; cut_basis = Arith.Tableau;
      split_forms = false; cuts = 0;
      splits_since_cut = 0; next_private = 0;
      required_booleans = Ids.create 16;
      activity_terms = 0; occurrence_activity = true }
  in
  let poll () = poll t in
  let congruence symbol =
    let signature = Term.Unsafe.signature terms symbol in
    match signature.kind, signature.result with
    | Term.Builtin ("and" | "or" | "not" | "xor" | "="), _ -> false
    | Term.Builtin "ite", Term.Named _ -> false
    | _ -> true in
  let commutative symbol =
    match (Term.Unsafe.signature terms symbol).kind with
    | Term.Builtin ("+" | "*" | "bvadd" | "bvmul" | "bvand" | "bvor"
        | "bvxor") -> true
    | _ -> false in
  t.equality <- Some (Eq.create ~terms ~poll ~congruence ~commutative ());
  t.nonlinear <- Some (NL.create ~terms ~boolean:(boolean t)
    ~clause:(clause t) ~poll ());
  let with_context context f =
    t.context <- Some context;
    Fun.protect ~finally:(fun () -> t.context <- None) (fun () ->
      match f () with
      | result -> result
      | exception ((Theory_stopped | Encoding_stopped _
          | Resource.Exhausted _) as exn) ->
        t.interrupted_theory <- true;
        raise exn)
  in
  let reset () =
    Eq.reset (equality t);
    Arith.reset t.arithmetic;
    t.arithmetic_dirty <- true;
    t.arithmetic_result <- None
  in
  let eq = equality t in
  let members = Eq.members eq in
  let constructor_members = Eq.constructor_members eq in
  let uses = Eq.uses eq in
  let assigned = assigned t in
  let explain = Eq.explain_later eq in
  let equate = Eq.equate eq in
  let extension : Sat.extension =
    { start = (fun () -> reset (); t.incomplete <- false);
      push_level = (fun () ->
        Eq.push (equality t); Arith.push t.arithmetic);
      pop_levels = (fun n ->
        t.arithmetic_dirty <- true;
        for _ = 1 to n do
          Eq.pop (equality t); Arith.pop t.arithmetic
        done);
      assigned = (fun _ l ->
        Eq.assert_literal (equality t) l;
        if Ids.mem t.arithmetic_atoms (abs l)
        then begin
          t.arithmetic_dirty <- true;
          t.arithmetic_revision <- t.arithmetic_revision + 1;
          Arith.assert_literal t.arithmetic l
        end);
      propagate = (fun context -> with_context context (fun () ->
        let rec propagate () =
          if equality_check t context then begin
            if Term.has_datatypes t.terms then begin
              let classes = Eq.take_changed (equality t) in
              Option.iter (fun d -> DT.propagate d ~classes
                ~members ~constructor_members ~uses ~assigned ~explain
                ~imply:(Sat.imply context)
                ~conflict:(Sat.conflict context)
                ~equate) t.datatypes
            end else Eq.discard_changed (equality t);
            if Eq.has_merges (equality t) then propagate ()
            else ignore (arithmetic_check t context)
          end
        in propagate ()));
      final_check = (fun context -> with_context context (fun () ->
        final_check t context));
      finish = reset }
  in
  t.theory_extension <- Some extension;
  t.sat <- Some (Sat.create ());
  let int_bridge source bits =
    let sum = bit_sum t ~signed:false bits in
    let quotient = private_integer t in
    let modulus = Z.pow ?poll:(arithmetic_poll t)
        (Z.of_int 2) (Array.length bits) in
    let form = sub t (arithmetic t source)
      (add t sum (scale t modulus (arithmetic t quotient))) in
    let upper = fresh t and lower = fresh t in
    register_bound t upper form;
    register_bound t lower (scale t Z.minus_one form);
    clause t [|upper|]; clause t [|lower|]
  in
  t.bitvectors <- Some (BV.create ~terms
    ~fresh:(fun () -> fresh ~completion:true t) ~clause:(clause t)
    ~boolean:(boolean t) ~int_bridge ~poll ());
  t.datatypes <- Some (DT.create ~terms ~boolean:(boolean t)
    ~clause:(clause t) ~internalize:(internalize t) ~poll ());
  t

let resize_rewrite t =
  let count = Term.count t.terms in
  let old_size = Array.length t.rewrite_results in
  if count > old_size then begin
    let size = max count (2 * old_size) in
    let grow array initial =
      let result = Array.make size initial in
      Array.blit array 0 result 0 old_size;
      result
    in
    t.rewrite_results <- grow t.rewrite_results 0;
    t.occurrence_maxima <- grow t.occurrence_maxima (-2);
    t.rewrite_stamps <- grow t.rewrite_stamps 0;
    t.rewrite_parents <- grow t.rewrite_parents (-1);
    t.rewrite_parent_epochs <- grow t.rewrite_parent_epochs 0;
    t.rewrite_registered <- grow t.rewrite_registered 0
  end

let rewrite_parent t child parent =
  if t.rewrite_parent_epochs.(child) <> t.rewrite_epoch then begin
    t.rewrite_parents.(child) <- -1;
    t.rewrite_parent_epochs.(child) <- t.rewrite_epoch
  end;
  let offset = t.rewrite_edge_count in
  if offset + 2 > Array.length t.rewrite_edges then begin
    let edges = Array.make (max 32 (2 * Array.length t.rewrite_edges)) 0 in
    Array.blit t.rewrite_edges 0 edges 0 offset;
    t.rewrite_edges <- edges
  end;
  t.rewrite_edges.(offset) <- parent;
  t.rewrite_edges.(offset + 1) <- t.rewrite_parents.(child);
  t.rewrite_parents.(child) <- offset;
  t.rewrite_edge_count <- offset + 2

let rewrite t term =
  poll t;
  if IM.is_empty t.substitutions then term else
  let () = resize_rewrite t in
  let rec visit term =
    poll t;
    if t.rewrite_stamps.(term) = t.rewrite_epoch
    then t.rewrite_results.(term)
    else begin
      let first = t.rewrite_registered.(term) <> t.rewrite_epoch in
      t.rewrite_registered.(term) <- t.rewrite_epoch;
      let result = match IM.find_opt term t.substitutions with
        | Some value ->
          if first then rewrite_parent t value term;
          visit value
        | None -> match Term.Unsafe.node t.terms term with
          | Term.Apply (symbol, arguments) ->
            if first then Array.iter (fun x -> rewrite_parent t x term) arguments;
            let count = Array.length arguments in
            let rec unchanged index =
              if index = count then arguments else
              let result = visit arguments.(index) in
              if result = arguments.(index) then unchanged (index + 1)
              else begin
                let rewritten = Array.copy arguments in
                rewritten.(index) <- result;
                for next = index + 1 to count - 1 do
                  rewritten.(next) <- visit arguments.(next)
                done;
                rewritten
              end in
            let rewritten = unchanged 0 in
            if arguments == rewritten then term else
            (match (Term.Unsafe.signature t.terms symbol).kind with
            | Term.Builtin name ->
              Term.builtin t.terms name (Term.sort t.terms term) rewritten
            | _ -> Term.apply t.terms symbol rewritten)
          | _ -> term in
      t.rewrite_results.(term) <- result;
      t.rewrite_stamps.(term) <- t.rewrite_epoch;
      result
    end
  in
  visit term

let invalidate_rewrite t =
  t.rewrite_edge_count <- 0;
  if t.rewrite_epoch = max_int then begin
    Array.fill t.rewrite_stamps 0 (Array.length t.rewrite_stamps) 0;
    Array.fill t.rewrite_registered 0 (Array.length t.rewrite_registered) 0;
    Array.fill t.rewrite_parent_epochs 0
      (Array.length t.rewrite_parent_epochs) 0;
    t.rewrite_epoch <- 1
  end else t.rewrite_epoch <- t.rewrite_epoch + 1

let invalidate_substitution t left right =
  resize_rewrite t;
  if t.rewrite_registered.(left) = t.rewrite_epoch
  then rewrite_parent t right left;
  (* A cached parent has cached dependencies in the same epoch. *)
  let rec visit term =
    poll t;
    if t.rewrite_stamps.(term) = t.rewrite_epoch then begin
      t.rewrite_stamps.(term) <- 0;
      if t.rewrite_parent_epochs.(term) = t.rewrite_epoch
      then parents t.rewrite_parents.(term)
    end
  and parents offset =
    if offset >= 0 then begin
      visit t.rewrite_edges.(offset);
      parents t.rewrite_edges.(offset + 1)
    end
  in
  visit left

let mark_used t term =
  let rec visit term =
    poll t;
    if not (Ids.mem t.used_terms term) then begin
      Ids.add t.used_terms term ();
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, arguments) ->
        (match (Term.Unsafe.signature t.terms symbol).kind, arguments with
        | Term.Builtin "div", [|x; y|] ->
          Hashtbl.replace t.explicit_divisions (x, y) ()
        | _ -> ());
        Array.iter visit arguments;
        if (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "div"
          || Array.exists (Ids.mem t.quotient_terms) arguments
        then Ids.replace t.quotient_terms term ()
      | _ -> ()
    end
  in
  visit term

let largest_variable t term =
  resize_rewrite t;
  let rec visit term =
    poll t;
    let cached = t.occurrence_maxima.(term) in
    if cached <> -2 then cached else
    let largest = match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, [||]) ->
        if (Term.Unsafe.signature t.terms symbol).kind = Term.Uninterpreted
        then term else -1
      | Term.Apply (_, arguments) ->
        Array.fold_left (fun largest x -> max largest (visit x)) (-1) arguments
      | _ -> -1 in
    t.occurrence_maxima.(term) <- largest;
    largest
  in
  visit term

let eliminate t left right =
  let candidate = match Term.Unsafe.node t.terms left with
    | Term.Apply (symbol, [||]) ->
      (Term.Unsafe.signature t.terms symbol).kind = Term.Uninterpreted
      && not (Ids.mem t.used_terms left)
    | _ -> false in
  if not candidate then false else
  let expands_connective = match Term.Unsafe.node t.terms right with
    | Term.Apply (symbol, arguments) when Array.length arguments > 64 ->
      (match (Term.Unsafe.signature t.terms symbol).kind with
      | Term.Builtin ("and" | "or") -> true
      | _ -> false)
    | _ -> false in
  if expands_connective then false else
  let contains_left () =
    let count = Term.count t.terms in
    let capacity = Array.length t.occurrence_stamps in
    if count > capacity then
      t.occurrence_stamps <- Array.make (max count (2 * capacity)) 0;
    if t.occurrence_epoch = max_int then begin
      Array.fill t.occurrence_stamps 0 (Array.length t.occurrence_stamps) 0;
      t.occurrence_epoch <- 1
    end else t.occurrence_epoch <- t.occurrence_epoch + 1;
    let epoch = t.occurrence_epoch in
    let rec occurs term =
      poll t;
      if term = left then true
      (* A term is allocated after every argument in its immutable DAG. *)
      else if term < left || t.occurrence_maxima.(term) < left
      then false
      else if t.occurrence_stamps.(term) = epoch then false
      else begin
        t.occurrence_stamps.(term) <- epoch;
        match Term.Unsafe.node t.terms term with
        | Term.Apply (_, arguments) -> Array.exists occurs arguments
        | _ -> false
      end
    in
    occurs right
  in
  if left <= largest_variable t right && contains_left ()
  then false else begin
    t.substitutions <- IM.add left right t.substitutions;
    invalidate_substitution t left right;
    true
  end

let rec assert_inner t term =
  poll t;
  t.model <- None;
  if Term.sort t.terms term <> Term.Bool
  then invalid_arg "SMT Boolean assertion required";
  match Term.Unsafe.node t.terms term with
  | Term.Apply (symbol, arguments)
    when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "and" ->
    Array.iter (assert_inner t) arguments
  | Term.Apply (symbol, _)
    when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "=" ->
    let term = rewrite t term in
    (match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, arguments)
      when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "and" ->
      Array.iter (assert_inner t) arguments
    | Term.Apply (symbol, [|left; right|])
      when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "="
           && (eliminate t left right || eliminate t right left) -> ()
    | _ -> t.pending_assertions <- term :: t.pending_assertions)
  | _ -> t.pending_assertions <- term :: t.pending_assertions

let forget_alternative t =
  Option.iter (fun solver ->
    t.retired_statistics <- add_statistics t.retired_statistics
        (Sat.statistics (sat solver))) t.alternative;
  t.alternative <- None; t.result_solver <- None

let assert_ t term =
  forget_alternative t;
  assert_inner t term;
  t.replay_assertions <- term :: t.replay_assertions

module Required = Set.Make (Int)
exception Required_limit

let required_literals t terms =
  let positive = Ids.create 128 and negative = Ids.create 128 in
  let remaining = ref 10000 in
  let charge () =
    decr remaining;
    if !remaining < 0 then raise Required_limit;
    poll t in
  let rec required expected term =
    charge ();
    let cache = if expected then positive else negative in
    match Ids.find_opt cache term with
    | Some result -> result
    | None ->
      let result = match Term.Unsafe.node t.terms term with
        | Term.Apply (symbol, args) ->
          (match (Term.Unsafe.signature t.terms symbol).kind with
          | Term.Builtin "not" -> required (not expected) args.(0)
          | Term.Builtin ("and" | "or" as name) ->
            if expected = (name = "and") then
              Array.fold_left (fun result term ->
                Required.fold (fun literal result ->
                  charge (); Required.add literal result)
                  (required expected term) result) Required.empty args
            else if Array.length args = 0 then Required.empty
            else
              let result = ref (required expected args.(0)) in
              let index = ref 1 in
              while !index < Array.length args
                    && not (Required.is_empty !result) do
                let branch = required expected args.(!index) in
                result := Required.filter (fun literal ->
                  charge (); Required.mem literal branch) !result;
                incr index
              done;
              !result
          | _ -> atom expected term)
        | _ -> atom expected term in
      Ids.add cache term result;
      result
  and atom expected term =
    match Ids.find_opt t.booleans term with
    | Some literal -> Required.singleton
        (if expected then literal else -literal)
    | None -> Required.empty
  in
  try List.iter (fun term ->
    Required.iter (fun literal ->
      charge (); Sat.add_clause (sat t) [|literal|]) (required true term)) terms
  with Required_limit -> ()

exception Not_difference
exception Difference_inconsistent
exception Difference_limit

let difference_inconsistent_bounded t pending =
  let remaining = ref 600_000 in
  let poll () =
    decr remaining;
    if !remaining < 0 then raise Difference_limit;
    poll t in
  let rec linear term =
    poll ();
    match Term.Unsafe.node t.terms term with
    | Term.Integer value -> constant value
    | Term.Apply (symbol, arguments) ->
      (match (Term.Unsafe.signature t.terms symbol).kind, arguments with
      | Term.Uninterpreted, [||] when Term.sort t.terms term = Term.Int ->
        variable term
      | Term.Builtin "+", _ ->
        Array.fold_left (fun sum term -> add t sum (linear term))
          (constant Z.zero) arguments
      | Term.Builtin "-", [|term|] -> scale t Z.minus_one (linear term)
      | Term.Builtin "-", [|left; right|] -> sub t (linear left) (linear right)
      | _ -> raise Not_difference)
    | _ -> raise Not_difference
  in
  let rec edge positive term =
    poll ();
    match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, arguments) ->
      (match (Term.Unsafe.signature t.terms symbol).kind, arguments with
      | Term.Builtin "not", [|term|] -> edge (not positive) term
      | Term.Builtin ("<=" | "<" | ">=" | ">" as op), [|left; right|] ->
        let left = linear left and right = linear right in
        let form = if op = ">=" || op = ">" then sub t right left
          else sub t left right in
        let form = if op = "<" || op = ">"
          then add t form (constant Z.one) else form in
        let form = if positive then form
          else add t (scale t Z.minus_one form) (constant Z.one) in
        let x, y = match IM.bindings form.coefficients with
          | [] -> -1, -1
          | [x, a] when Z.equal a Z.one -> x, -1
          | [y, a] when Z.equal a Z.minus_one -> -1, y
          | [x, a; y, b] when Z.equal a Z.one && Z.equal b Z.minus_one -> x, y
          | [y, a; x, b] when Z.equal a Z.minus_one && Z.equal b Z.one -> x, y
          | _ -> raise Not_difference in
        y, x, Z.neg form.constant
      | _ -> raise Not_difference)
    | _ -> raise Not_difference
  in
  let edges = ref [] and alternatives = ref [] and binary = ref [] in
  List.iter (fun term ->
    poll ();
    try
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, arguments)
        when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "or"
             && Array.length arguments >= 2 && Array.length arguments <= 8 ->
        let branches = Array.map (edge true) arguments in
        if Array.length arguments = 2 then
          binary := (arguments.(0), arguments.(1), branches.(0), branches.(1))
            :: !binary
        else alternatives := Array.map (fun edge -> [edge]) branches
            :: !alternatives
      | _ -> edges := edge true term :: !edges
    with Not_difference -> ()) pending;
  if List.length !alternatives + List.length !binary < 2 then false else
  let adjacency = Ids.create 32 and constraints = Ids.create 32 in
  let vertices = ref [] in
  let connect left right edge =
    let old = match Ids.find_opt adjacency left with
      | Some old -> old
      | None -> vertices := left :: !vertices; Required.empty in
    Ids.replace adjacency left (Required.add right old);
    Ids.replace constraints left edge in
  List.iter (fun (left, right, a, b) ->
    poll (); connect left right a; connect right left b) !binary;
  let colors = Ids.create 32 and factored = Ids.create 32 in
  List.iter (fun first ->
    poll ();
    if not (Ids.mem colors first) then begin
      let pending = Queue.create () and left = ref [] and right = ref [] in
      let bipartite = ref true in
      Ids.add colors first true;
      Queue.add first pending;
      while not (Queue.is_empty pending) do
        poll ();
        let vertex = Queue.take pending in
        let color = Ids.find colors vertex in
        if color then left := vertex :: !left else right := vertex :: !right;
        Required.iter (fun other ->
          poll ();
          match Ids.find_opt colors other with
          | Some other_color ->
            if color = other_color then bipartite := false
          | None ->
            Ids.add colors other (not color);
            Queue.add other pending) (Ids.find adjacency vertex)
      done;
      let complete part opposite =
        let expected = List.length opposite in
        List.for_all (fun vertex ->
          poll ();
          Required.cardinal (Ids.find adjacency vertex) = expected) part in
      if !bipartite && complete !left !right && complete !right !left then begin
        let branch vertices = List.map (fun vertex ->
          poll ();
          Ids.add factored vertex ();
          Ids.find constraints vertex) vertices in
        alternatives := [|branch !left; branch !right|] :: !alternatives
      end
    end) !vertices;
  List.iter (fun (left, _, a, b) ->
    poll ();
    if not (Ids.mem factored left) then
      alternatives := [|[a]; [b]|] :: !alternatives) !binary;
  let nodes = Ids.create 32 in
  let node term = match Ids.find_opt nodes term with
    | Some index -> index
    | None -> let index = Ids.length nodes in Ids.add nodes term index; index in
  let convert (source, target, bound) = node source, node target, bound in
  let edges = List.map convert !edges in
  let alternatives =
    List.rev_map (Array.map (List.map convert)) !alternatives in
  let size = Ids.length nodes in
  if size > 256 then false else
  let insert matrix (source, target, bound) =
    let improves = match matrix.(source).(target) with
      | Some old -> Z.compare bound old < 0 | None -> true in
    if not improves then matrix else begin
      (match matrix.(target).(source) with
      | Some back when Z.sign (Z.add ~poll back bound) < 0 ->
        raise Difference_inconsistent
      | _ -> ());
      let updated = Array.copy matrix in
      for left = 0 to size - 1 do
        poll ();
        match matrix.(left).(source) with
        | None -> ()
        | Some before ->
          let before = Z.add ~poll before bound and copied = ref false in
          for right = 0 to size - 1 do
            poll ();
            match matrix.(target).(right) with
            | None -> ()
            | Some after ->
              let value = Z.add ~poll before after in
              let improves = match matrix.(left).(right) with
                | None -> true | Some old -> Z.compare value old < 0 in
              if improves then begin
                if not !copied then begin
                  updated.(left) <- Array.copy matrix.(left);
                  copied := true
                end;
                updated.(left).(right) <- Some value
              end
          done
      done;
      updated
    end
  in
  try
    let matrix = ref (Array.init size (fun source -> Array.init size
      (fun target -> if source = target then Some Z.zero else None))) in
    List.iter (fun edge -> matrix := insert !matrix edge) edges;
    let changed = ref true and remaining = ref 4 in
    while !changed && !remaining > 0 do
      changed := false;
      decr remaining;
      List.iter (fun branches ->
        poll ();
        let possibilities =
          Array.to_list branches |> List.filter_map (fun edges ->
          try Some (List.fold_left (fun matrix edge ->
            poll (); insert matrix edge) !matrix edges)
          with Difference_inconsistent -> None) in
        match possibilities with
        | [] -> raise Difference_inconsistent
        | [branch] ->
          if branch != !matrix then changed := true;
          matrix := branch
        | first :: rest ->
          let joined = Array.copy !matrix in
          let rec maximum source target old value = function
            | [] -> value
            | _ when value == old -> old
            | branch :: rest ->
              poll ();
              let other = branch.(source).(target) in
              match value, other with
              | Some left, Some right ->
                maximum source target old
                  (if Z.compare left right >= 0 then value else other) rest
              | _ -> None in
          for source = 0 to size - 1 do
            poll ();
            let old = (!matrix).(source) in
            if first.(source) != old
               && List.for_all (fun branch ->
                    poll (); branch.(source) != old) rest then begin
            let copied = ref false in
            for target = 0 to size - 1 do
              poll ();
              let old = (!matrix).(source).(target) in
              let value = maximum source target old
                first.(source).(target) rest in
              let improves = match value, old with
                | Some value, Some old -> Z.compare value old < 0
                | Some _, None -> true
                | _ -> false in
              if improves then begin
                changed := true;
                if not !copied then begin
                  joined.(source) <- Array.copy (!matrix).(source);
                  copied := true
                end;
                joined.(source).(target) <- value
              end
            done
            end
          done;
          matrix := joined) alternatives
    done;
    false
  with Difference_inconsistent -> true


let difference_inconsistent t pending =
  let rec eligible count = function
    | [] -> false
    | term :: rest ->
      poll t;
      let disjunction = match Term.Unsafe.node t.terms term with
        | Term.Apply (symbol, arguments) ->
          (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "or"
          && Array.length arguments >= 2 && Array.length arguments <= 8
        | _ -> false in
      let count = if disjunction then count + 1 else count in
      count >= 2 || eligible count rest
  in
  eligible 0 pending
  && try difference_inconsistent_bounded t pending
     with Difference_limit -> false

exception Finite_domain_limit
exception Finite_domain_conflict

let finite_domain_inconsistent t pending =
  let remaining = ref 250000 in
  let charge () =
    poll t;
    decr remaining;
    if !remaining < 0 then raise Finite_domain_limit in
  let constant term = match Term.Unsafe.node t.terms term with
    | Term.Integer _ | Term.Bits _ | Term.Boolean _ -> true
    | Term.Apply (symbol, [||]) ->
      (match (Term.Unsafe.signature t.terms symbol).kind with
      | Term.Constructor _ -> true | _ -> false)
    | _ -> false in
  let equality term = match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, [|left; right|])
        when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "=" ->
      if constant right then Some (left, right)
      else if constant left then Some (right, left) else None
    | _ -> None in
  let domains = Ids.create 64 and edges = ref [] in
  let membership arguments =
    if Array.length arguments <= 256 then begin
      let common = ref None and values = ref Required.empty in
      let valid = ref true in
      Array.iter (fun term ->
        charge ();
        match equality term with
        | None -> valid := false
        | Some (subject, value) ->
          (match !common with
          | None -> common := Some subject
          | Some previous -> if previous <> subject then valid := false);
          values := Required.add value !values) arguments;
      match !common with
      | Some subject when !valid ->
        let values = match Ids.find_opt domains subject with
          | None -> !values
          | Some previous -> Required.inter previous !values in
        if Required.is_empty values then raise Finite_domain_conflict;
        Ids.replace domains subject values
      | _ -> ()
    end in
  let rec visit term =
    charge ();
    match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, arguments) ->
      (match (Term.Unsafe.signature t.terms symbol).kind, arguments with
      | Term.Builtin "and", _ -> Array.iter visit arguments
      | Term.Builtin "or", _ -> membership arguments
      | Term.Builtin "not", [|argument|] ->
        (match Term.Unsafe.node t.terms argument with
        | Term.Apply (symbol, [|left; right|])
            when (Term.Unsafe.signature t.terms symbol).kind
              = Term.Builtin "=" ->
          edges := (left, right) :: !edges
        | _ -> ())
      | _ -> ())
    | _ -> () in
  try
    List.iter visit pending;
    if !edges = [] || Ids.length domains > 512 then false else begin
      let adjacent = Hashtbl.create (List.length !edges) in
      let degrees = Ids.create (Ids.length domains) in
      let sort_domains = Hashtbl.create 8 in
      let finite_sort sort = match Hashtbl.find_opt sort_domains sort with
        | Some result -> result
        | None ->
          let result = match sort with
            | Term.Named datatype ->
              let constructors = Term.constructors t.terms datatype in
              if constructors = [] || List.length constructors > 256
                || not (List.for_all (fun symbol ->
                  charge ();
                  Array.length (Term.Unsafe.signature t.terms symbol).arguments
                  = 0) constructors) then None else
              Some (List.fold_left (fun values symbol ->
                charge ();
                Required.add (Term.apply t.terms symbol [||]) values)
                Required.empty constructors)
            | _ -> None in
          Hashtbl.add sort_domains sort result;
          result in
      let domain term = match Ids.find_opt domains term with
        | Some values -> Some values
        | None when constant term ->
          let values = Required.singleton term in
          Ids.add domains term values;
          Some values
        | None ->
          let values = finite_sort (Term.sort t.terms term) in
          Option.iter (fun values -> Ids.replace domains term values) values;
          values in
      let key a b = if a < b then a, b else b, a in
      List.iter (fun (left, right) ->
        charge ();
        match domain left, domain right with
        | Some _, Some _ ->
          let pair = key left right in
          if not (Hashtbl.mem adjacent pair) then begin
            Hashtbl.add adjacent pair ();
            List.iter (fun term ->
              let degree = Option.value (Ids.find_opt degrees term) ~default:0 in
              Ids.replace degrees term (degree + 1)) [left; right]
          end
        | _ -> ()) !edges;
      let vertices = Ids.fold (fun term degree vertices ->
        charge ();
        let values = Ids.find domains term in
        (term, degree, values) :: vertices) degrees [] in
      let vertices = List.sort (fun (a, da, va) (b, db, vb) ->
        charge ();
        let order = Int.compare (Required.cardinal va) (Required.cardinal vb) in
        if order <> 0 then order else
        let order = Int.compare db da in
        if order <> 0 then order else Int.compare a b) vertices in
      let linked left right =
        charge (); Hashtbl.mem adjacent (key left right) in
      let rec choose clique size values = function
        | [] -> ()
        | (term, _, domain) :: rest ->
          if List.for_all (linked term) clique then begin
            let values = Required.union values domain in
            let size = size + 1 in
            if size > Required.cardinal values then raise Finite_domain_conflict;
            choose (term :: clique) size values rest
          end else choose clique size values rest in
      choose [] 0 Required.empty vertices;
      let by_degree = List.sort (fun (a, da, va) (b, db, vb) ->
        charge ();
        let order = Int.compare db da in
        if order <> 0 then order else
        let order = Int.compare (Required.cardinal va) (Required.cardinal vb) in
        if order <> 0 then order else Int.compare a b) vertices in
      choose [] 0 Required.empty by_degree;
      false
    end
  with
  | Finite_domain_conflict -> true
  | Finite_domain_limit -> false

let boolean_cardinality_inconsistent t assertions =
  let exception Limit in
  let remaining = ref 100000 in
  let charge () =
    poll t;
    decr remaining;
    if !remaining < 0 then raise Limit in
  let atom term = match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, [||]) ->
      (Term.Unsafe.signature t.terms symbol).kind = Term.Uninterpreted
      && Term.sort t.terms term = Term.Bool
    | _ -> false in
  let negative term = match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, [|argument|])
      when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "not"
        && atom argument -> Some argument
    | _ -> None in
  let groups = ref [] and edges = ref [] in
  let rec collect term =
    charge ();
    match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, arguments) ->
      (match (Term.Unsafe.signature t.terms symbol).kind with
      | Term.Builtin "and" -> Array.iter collect arguments
      | Term.Builtin "or" ->
        if Array.length arguments = 2 then begin
          match negative arguments.(0), negative arguments.(1) with
          | Some left, Some right -> edges := (left, right) :: !edges
          | _ -> ()
        end;
        if Array.length arguments >= 2 && Array.length arguments <= 64
           && Array.for_all (fun term -> charge (); atom term) arguments
        then groups := Array.fold_left (fun group term ->
          charge (); Required.add term group) Required.empty arguments
          :: !groups
      | _ -> ())
    | _ -> () in
  try
    List.iter collect assertions;
    if List.length !groups < 3 || !edges = [] then false else begin
      let groups = List.sort (fun left right ->
        charge ();
        Int.compare (Required.cardinal left) (Required.cardinal right))
        !groups in
      let required = ref 0 and vertices = ref Required.empty in
      List.iter (fun group ->
        charge ();
        if Required.for_all (fun term ->
            charge (); not (Required.mem term !vertices)) group then begin
          incr required;
          Required.iter (fun term ->
            charge (); vertices := Required.add term !vertices) group
        end) groups;
      if !required < 3 then false else begin
        let adjacency = Ids.create (Required.cardinal !vertices) in
        let connect left right =
          let neighbors = Option.value (Ids.find_opt adjacency left)
              ~default:Required.empty in
          Ids.replace adjacency left (Required.add right neighbors) in
        List.iter (fun (left, right) ->
          charge ();
          if left <> right && Required.mem left !vertices
             && Required.mem right !vertices then begin
            connect left right;
            connect right left
          end) !edges;
        let adjacent left right =
          charge ();
          match Ids.find_opt adjacency left with
          | None -> false
          | Some neighbors -> Required.mem right neighbors in
        let remaining = ref !vertices and capacity = ref 0 in
        while not (Required.is_empty !remaining) && !capacity < !required do
          charge ();
          let clique = ref [] in
          Required.iter (fun term ->
            charge ();
            if List.for_all (adjacent term) !clique then
              clique := term :: !clique) !remaining;
          List.iter (fun term ->
            charge (); remaining := Required.remove term !remaining) !clique;
          incr capacity
        done;
        !capacity < !required && Required.is_empty !remaining
      end
    end
  with Limit -> false

module Finite_evaluation = struct
  type value = Integer of Z.t | Boolean of bool

  let equal a b = match a, b with
    | Integer a, Integer b -> Z.equal a b
    | Boolean a, Boolean b -> a = b
    | _ -> false

  module Applications (Budget : sig val charge : unit -> unit end) =
    Hashtbl.Make (struct
    type t = Term.symbol * value array
    let equal (f, xs) (g, ys) =
      Budget.charge ();
      f = g && Array.length xs = Array.length ys
      && Array.for_all2 (fun x y -> Budget.charge (); equal x y) xs ys
    let hash (f, xs) = Array.fold_left (fun h x ->
      Budget.charge ();
      let v = match x with
        | Boolean false -> 0 | Boolean true -> 1
        | Integer n -> match Z.to_int_opt n with
          | Some n -> n
          | None -> Hashtbl.hash (Z.to_string ~poll:Budget.charge n) in
      h * 65599 lxor v) f xs land max_int
  end)

  exception Limit
  exception Unknown
  exception Inconsistent

  let inconsistent t pending =
    let membership = List.exists (fun term ->
      poll t;
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, _) ->
        (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "or"
      | _ -> false) pending in
    if not membership then false else
    let remaining = ref 2000000 in
    let charge () =
      poll t;
      decr remaining;
      if !remaining < 0 then raise Limit in
    let scalar term = match Term.Unsafe.node t.terms term with
      | Term.Integer value -> Some (Integer value)
      | Term.Boolean value -> Some (Boolean value)
      | _ -> None in
    let variable term = match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, [||]) ->
        (Term.Unsafe.signature t.terms symbol).kind = Term.Uninterpreted
        && (Term.sort t.terms term = Term.Int
            || Term.sort t.terms term = Term.Bool)
      | _ -> false in
    let module Applications = Applications (struct let charge = charge end) in
    let ground = Applications.create 32 in
    let domains = Ids.create 4 in
    let rec alternatives subject values term =
      charge ();
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, arguments) ->
        (match (Term.Unsafe.signature t.terms symbol).kind, arguments with
        | Term.Builtin "or", _ ->
          Array.fold_left (fun (subject, values) term ->
            alternatives subject values term) (subject, values) arguments
        | Term.Builtin "=", [|a; b|] ->
          let candidate, value = match scalar a, scalar b with
            | None, Some value when variable a -> a, value
            | Some value, None when variable b -> b, value
            | _ -> raise Unknown in
          (match subject with
          | Some old when old <> candidate -> raise Unknown
          | _ -> ());
          Some candidate, value :: values
        | _ -> raise Unknown)
      | _ -> raise Unknown in
    let table application result =
      match scalar result, Term.Unsafe.node t.terms application with
      | Some value, Term.Apply (symbol, arguments)
        when Array.length arguments > 0 && Array.length arguments <= 16
          && (Term.Unsafe.signature t.terms symbol).kind
             = Term.Uninterpreted ->
        (try
          let arguments = Array.map (fun term ->
            charge (); match scalar term with
            | Some value -> value | None -> raise Unknown) arguments in
          let key = symbol, arguments in
          (match Applications.find_opt ground key with
          | Some previous when not (equal previous value) ->
            raise Inconsistent
          | _ -> Applications.replace ground key value)
        with Unknown -> ())
      | _ -> () in
    let rec inspect term =
      charge ();
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, arguments) ->
        (match (Term.Unsafe.signature t.terms symbol).kind, arguments with
        | Term.Builtin "and", _ -> Array.iter inspect arguments
        | Term.Builtin "or", _ ->
          (try
            match alternatives None [] term with
            | Some subject, values ->
              let values = List.filter (fun value ->
                charge ();
                match Ids.find_opt domains subject with
                | None -> true
                | Some previous -> List.exists (fun previous ->
                    charge (); equal value previous) previous) values
              in
              if values = [] then raise Inconsistent;
              Ids.replace domains subject values
            | _ -> ()
          with Unknown -> ())
        | Term.Builtin "=", [|a; b|] -> table a b; table b a
        | _ -> ())
      | _ -> () in
    try
      List.iter inspect pending;
      if Applications.length ground = 0 || Ids.length domains = 0
        || Ids.length domains > 4 then false else begin
      let domains = Ids.fold (fun term values result ->
        charge ();
        (term, Array.of_list values) :: result) domains []
        |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
        |> Array.of_list in
      let combinations = Array.fold_left (fun product (_, values) ->
        charge ();
        let count = Array.length values in
        if count = 0 then raise Inconsistent;
        if count > 4096 / product then raise Limit;
        product * count) 1 domains in
      if combinations = 0 then false else
      let assigned = Ids.create 4 in
      let cache = Array.make (Term.count t.terms) None in
      let stamps = Array.make (Array.length cache) 0 in
      let epoch = ref 0 in
      let integer = function Integer n -> n | _ -> raise Unknown in
      let boolean = function Boolean b -> b | _ -> raise Unknown in
      let rec evaluate term =
        charge ();
        if stamps.(term) = !epoch then match cache.(term) with
          | Some value -> value | None -> raise Unknown
        else begin
          stamps.(term) <- !epoch;
          cache.(term) <- None;
          let value = match scalar term with
            | Some value -> value
            | None ->
              match Ids.find_opt assigned term with
              | Some value -> value
              | None -> match Term.Unsafe.node t.terms term with
                | Term.Apply (symbol, arguments) ->
                  let int term = integer (evaluate term) in
                  let truth term = boolean (evaluate term) in
                  (match (Term.Unsafe.signature t.terms symbol).kind,
                      arguments with
                  | Term.Builtin "ite", [|condition; left; right|] ->
                    evaluate (if truth condition then left else right)
                  | Term.Builtin "not", [|term|] -> Boolean (not (truth term))
                  | Term.Builtin "and", _ -> Boolean (all arguments true)
                  | Term.Builtin "or", _ -> Boolean (all arguments false)
                  | Term.Builtin "=", [|a; b|] ->
                    Boolean (equal (evaluate a) (evaluate b))
                  | Term.Builtin ("<" | "<=" | ">" | ">=" as op),
                    [|a; b|] ->
                    let c = Z.compare (int a) (int b) in
                    Boolean (match op with "<" -> c < 0 | "<=" -> c <= 0
                      | ">" -> c > 0 | _ -> c >= 0)
                  | Term.Builtin "+", _ -> Integer
                    (Array.fold_left (fun n x -> Z.add ~poll:charge n (int x))
                      Z.zero arguments)
                  | Term.Builtin "*", _ -> Integer
                    (Array.fold_left (fun n x -> Z.mul ~poll:charge n (int x))
                      Z.one arguments)
                  | Term.Builtin "-", [|x|] -> Integer (Z.neg (int x))
                  | Term.Builtin "-", [|x; y|] ->
                    Integer (Z.sub ~poll:charge (int x) (int y))
                  | Term.Builtin ("div" | "mod" as op), [|x; y|] ->
                    let a = int x and b = int y in
                    if Z.is_zero b then raise Unknown;
                    let q, r = Z.div_rem ~poll:charge a b in
                    Integer (if op = "div" then q else r)
                  | Term.Uninterpreted, _ ->
                    let values = Array.map evaluate arguments in
                    (match Applications.find_opt ground (symbol, values) with
                    | Some value -> value | None -> raise Unknown)
                  | _ -> raise Unknown)
                | _ -> raise Unknown in
          cache.(term) <- Some value;
          value
        end
      and all arguments conjunction =
        let unknown = ref false in
        let rec loop index =
          if index = Array.length arguments then
            if !unknown then raise Unknown else conjunction
          else
            let value = try Some (boolean (evaluate arguments.(index)))
              with Unknown -> None in
            match value with
            | Some value when value <> conjunction -> value
            | Some _ -> loop (index + 1)
            | None -> unknown := true; loop (index + 1) in
        loop 0 in
      incr epoch;
      let pending = List.filter (fun term ->
        charge ();
        try
          if boolean (evaluate term) then false else raise Inconsistent
        with Unknown -> true) pending in
      let rec enumerate index =
        charge ();
        if index = Array.length domains then begin
          incr epoch;
          List.exists (fun term ->
            try not (boolean (evaluate term)) with Unknown -> false) pending
        end else
          let term, values = domains.(index) in
          Array.for_all (fun value ->
            Ids.replace assigned term value;
            enumerate (index + 1)) values in
      enumerate 0
      end
    with Inconsistent -> true | Limit -> false
end

module Linear_bounds = Map.Make (struct
  type t = Z.t IM.t
  let compare = IM.compare Z.compare
end)

let eliminate_fixed_linear_forms t pending =
  let exception Limit in
  let exception Nonlinear in
  let remaining = ref 10000 in
  let charge () =
    poll t;
    decr remaining;
    if !remaining < 0 then raise Limit in
  let rec form term =
    charge ();
    match Term.Unsafe.node t.terms term with
    | Term.Integer z -> constant z
    | Term.Apply (symbol, args) ->
      (match (Term.Unsafe.signature t.terms symbol).kind, args with
      | Term.Uninterpreted, [||] when Term.sort t.terms term = Term.Int ->
        variable term
      | Term.Builtin "+", _ ->
        Array.fold_left (fun a x -> add t a (form x))
          (constant Z.zero) args
      | Term.Builtin "-", [|x|] -> scale t Z.minus_one (form x)
      | Term.Builtin "-", [|x; y|] -> sub t (form x) (form y)
      | Term.Builtin "*", _ ->
        let coefficient = ref Z.one and variable = ref None in
        Array.iter (fun x ->
          let value = form x in
          if IM.is_empty value.coefficients then
            coefficient := Z.mul ~poll:charge !coefficient value.constant
          else match !variable with
            | None -> variable := Some value
            | Some _ -> raise Nonlinear) args;
        scale t !coefficient
          (Option.value !variable ~default:(constant Z.one))
      | _ -> raise Nonlinear)
    | _ -> raise Nonlinear in
  let rec bound positive term =
    charge ();
    match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, args) ->
      (match (Term.Unsafe.signature t.terms symbol).kind, args with
      | Term.Builtin "not", [|x|] -> bound (not positive) x
      | Term.Builtin ("<=" | "<" | ">=" | ">" as op), [|x; y|] ->
        let value = if op = ">=" || op = ">" then sub t (form y) (form x)
          else sub t (form x) (form y) in
        let value = if op = "<" || op = ">" then
            add t value (constant Z.one) else value in
        if positive then value else
          add t (scale t Z.minus_one value) (constant Z.one)
      | _ -> raise Nonlinear)
    | _ -> raise Nonlinear in
  let bounds = ref Linear_bounds.empty in
  (try List.iter (fun term ->
    charge ();
    try
      let value = bound true term in
      if not (IM.is_empty value.coefficients) then begin
        let boundary = Z.neg value.constant in
        let previous = Linear_bounds.find_opt value.coefficients !bounds in
        if Option.fold ~none:true
            ~some:(fun old -> Z.compare boundary old < 0) previous then
          bounds := Linear_bounds.add value.coefficients boundary !bounds
      end
    with Nonlinear -> ()) pending;
    Linear_bounds.iter (fun coefficients boundary ->
      charge ();
      let opposite = IM.map Z.neg coefficients in
      match Linear_bounds.find_opt opposite !bounds with
      | Some other when Z.equal boundary (Z.neg other) ->
        let choices = IM.bindings coefficients |> List.rev in
        let rec choose = function
          | [] -> ()
          | (left, coefficient) :: rest ->
            charge ();
            if not (Z.equal coefficient Z.one
                    || Z.equal coefficient Z.minus_one) then choose rest
            else begin
              let terms = IM.fold (fun term value terms ->
                charge ();
                if term = left then terms else
                let value = Z.neg (Z.mul ~poll:charge value coefficient) in
                Term.builtin t.terms "*" Term.Int
                  [|Term.integer t.terms value; term|] :: terms)
                  coefficients
                  [Term.integer t.terms
                    (Z.mul ~poll:charge boundary coefficient)] in
              let right = Term.builtin t.terms "+" Term.Int
                  (Array.of_list terms) in
              if IM.mem left t.substitutions
                 || not (eliminate t left (rewrite t right))
              then choose rest
            end in
        choose choices
      | _ -> ()) !bounds
  with Limit -> ());
  List.map (rewrite t) pending

let linearize_finite_products t pending =
  let exception Limit in
  let remaining = ref 100000 in
  let charge () =
    poll t;
    if !remaining = 0 then raise Limit;
    decr remaining in
  let bounds = Ids.create 16 in
  let rec bound term high value =
    charge ();
    match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, [||])
      when (Term.Unsafe.signature t.terms symbol).kind = Term.Uninterpreted
        && Term.sort t.terms term = Term.Int ->
      let low, upper = Option.value (Ids.find_opt bounds term)
          ~default:(None, None) in
      let target = if high then upper else low in
      let better = Option.fold ~none:true ~some:(fun previous ->
        if high then Z.compare value previous < 0
        else Z.compare value previous > 0) target in
      if better then Ids.replace bounds term
          (if high then low, Some value else Some value, upper)
    | Term.Apply (symbol, arguments) ->
      (match (Term.Unsafe.signature t.terms symbol).kind, arguments with
      | Term.Builtin "-", [|argument|] ->
        bound argument (not high) (Z.neg value)
      | Term.Builtin ("+" | "*" as op), _ ->
        let constant = ref (if op = "+" then Z.zero else Z.one) in
        let other = ref None and valid = ref true in
        Array.iter (fun argument ->
          charge ();
          match Term.Unsafe.node t.terms argument with
          | Term.Integer number ->
            constant := (if op = "+" then Z.add else Z.mul)
                ~poll:charge !constant number
          | _ -> match !other with
            | None -> other := Some argument | Some _ -> valid := false)
          arguments;
        if !valid then Option.iter (fun argument ->
          if op = "+" then bound argument high
              (Z.sub ~poll:charge value !constant)
          else if not (Z.is_zero !constant) then begin
            let positive = Z.sign !constant > 0 in
            let q, r = Z.div_rem ~poll:charge value !constant in
            let q = if high || Z.is_zero r then q
              else Z.add ~poll:charge q
                  (if positive then Z.one else Z.minus_one) in
            bound argument (high = positive) q
          end) !other
      | Term.Builtin "-", [|left; right|] ->
        (match Term.Unsafe.node t.terms left,
            Term.Unsafe.node t.terms right with
        | _, Term.Integer number ->
          bound left high (Z.add ~poll:charge value number)
        | Term.Integer number, _ ->
          bound right (not high) (Z.sub ~poll:charge number value)
        | _ -> ())
      | _ -> ())
    | _ -> () in
  let rec inspect term =
    charge ();
    match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, arguments) ->
      (match (Term.Unsafe.signature t.terms symbol).kind, arguments with
      | Term.Builtin "and", _ -> Array.iter inspect arguments
      | Term.Builtin ("<=" | "<" | ">=" | ">" as op), [|left; right|] ->
        let value term = match Term.Unsafe.node t.terms term with
          | Term.Integer value -> Some value | _ -> None in
        let upper term value strict =
          bound term true (if strict then Z.sub ~poll:charge value Z.one
            else value) in
        let lower term value strict =
          bound term false (if strict then Z.add ~poll:charge value Z.one
            else value) in
        (match value left, value right, op with
        | _, Some value, "<=" -> upper left value false
        | _, Some value, "<" -> upper left value true
        | _, Some value, ">=" -> lower left value false
        | _, Some value, ">" -> lower left value true
        | Some value, _, "<=" -> lower right value false
        | Some value, _, "<" -> lower right value true
        | Some value, _, ">=" -> upper right value false
        | Some value, _, ">" -> upper right value true
        | _ -> ())
      | _ -> ())
    | _ -> () in
  let domains = Ids.create 8 in
  try
    let pending = List.map (rewrite t) pending in
    List.iter inspect pending;
    Ids.iter (fun term (lower, upper) ->
      charge ();
      match lower, upper with
      | Some lower, Some upper ->
        (match Z.to_int_opt (Z.sub ~poll:charge upper lower) with
        | Some width when width >= 0 && width <= 4 ->
          Ids.add domains term (lower, width)
        | _ -> ())
      | _ -> ()) bounds;
    if Ids.length domains = 0 then pending, false else
    let cache = Ids.create 64 in
    let expanded = ref false in
    let rec rewrite term =
      charge ();
      match Ids.find_opt cache term with
      | Some term -> term
      | None ->
        let result = match Term.Unsafe.node t.terms term with
          | Term.Apply (symbol, arguments) ->
            let arguments = Array.map rewrite arguments in
            (match (Term.Unsafe.signature t.terms symbol).kind with
            | Term.Builtin "*" when Array.fold_left (fun count term ->
                match Term.Unsafe.node t.terms term with
                | Term.Integer _ -> count | _ -> count + 1) 0 arguments >= 2 ->
              let choice = ref None in
              Array.iteri (fun index factor ->
                match Ids.find_opt domains factor with
                | None -> ()
                | Some (_, width) as domain ->
                  (match !choice with
                  | Some (_, _, _, previous) when previous <= width -> ()
                  | _ -> Option.iter (fun (low, width) ->
                      choice := Some (index, factor, low, width)) domain))
                arguments;
              (match !choice with
              | None -> Term.apply t.terms symbol arguments
              | Some (index, factor, low, width) ->
                expanded := true;
                let rest = Array.init (Array.length arguments - 1) (fun i ->
                  arguments.(if i < index then i else i + 1)) in
                let rest = rewrite (Term.builtin t.terms "*" Term.Int rest) in
                let value offset = Z.add ~poll:charge low (Z.of_int offset) in
                let product offset = Term.builtin t.terms "*" Term.Int
                    [|Term.integer t.terms (value offset); rest|] in
                let result = ref (product width) in
                for offset = width - 1 downto 0 do
                  charge ();
                  result := Term.ite t.terms
                    (Term.equal t.terms factor
                       (Term.integer t.terms (value offset)))
                    (product offset) !result
                done;
                !result)
            | _ -> Term.apply t.terms symbol arguments)
          | _ -> term in
        Ids.add cache term result;
        result in
    let pending = List.map rewrite pending in
    pending, !expanded
  with Limit -> pending, false

let cached_required t term encode =
  match Ids.find_opt t.required_booleans term with
  | Some literal -> literal
  | None ->
    let literal = encode () in
    Ids.add t.required_booleans term literal;
    literal

let rec required_positive t term =
  poll t;
  match Term.Unsafe.node t.terms term with
  | Term.Apply (symbol, arguments)
    when (Term.Unsafe.signature t.terms symbol).kind
        = Term.Builtin "distinct"
      && Array.length arguments >= 16
      && (match Term.sort t.terms arguments.(0) with
          | Term.Named id -> Term.constructors t.terms id = []
          | _ -> false) ->
    cached_required t term (fun () ->
      Array.iter (internalize t) arguments;
      let literal = fresh t in
      Eq.register_distinct (equality t) ~literal arguments;
      literal)
  | Term.Apply (symbol, arguments)
    when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "and" ->
    cached_required t term (fun () ->
      let literal = fresh t in
      Array.iter (fun term ->
        let child = required_positive t term in
        clause t [|-literal; child|]) arguments;
      literal)
  | _ -> boolean t term

let update_occurrence_activity t =
  let count = Term.count t.terms in
  if t.activity_terms > count then t.activity_terms <- 0;
  while t.occurrence_activity && t.activity_terms < count do
    poll t;
    t.occurrence_activity <- (match Term.sort t.terms t.activity_terms with
      | Term.Bool -> true
      | Term.Named sort -> Term.constructors t.terms sort = []
      | Term.Int | Term.Bitvec _ -> false);
    t.activity_terms <- t.activity_terms + 1
  done;
  Sat.Internal.set_occurrence_activity (sat t) t.occurrence_activity

let add_interval_conditions t pending =
  let exception Limit in
  let remaining = ref 100000 in
  let charge () =
    poll t;
    decr remaining;
    if !remaining < 0 then raise Limit in
  let facts = Ids.create 128 and watchers = Ids.create 64 in
  let queue = Queue.create () and constraints = ref [] in
  let assign term truth =
    charge ();
    if not (Ids.mem facts term) then begin
      Ids.add facts term truth;
      Queue.add (term, truth) queue
    end in
  let connect left right same =
    let add source target =
      let old = Option.value (Ids.find_opt watchers source) ~default:[] in
      Ids.replace watchers source ((target, same) :: old);
      Option.iter (fun value -> assign target (value = same))
        (Ids.find_opt facts source) in
    add left right; add right left in
  let bounds = Ids.create 64 in
  let better lower next old = match next, old with
    | None, old -> old
    | Some _, None -> next
    | Some next, Some old -> Some
        (if (Z.compare next old > 0) = lower then next else old) in
  let atomic term = match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, _) ->
      (match (Term.Unsafe.signature t.terms symbol).kind with
      | Term.Builtin _ -> false | _ -> true)
    | _ -> false in
  let update term (lower, upper) =
    if not (atomic term) then false else
    let old_lower, old_upper = Option.value (Ids.find_opt bounds term)
        ~default:(None, None) in
    let value = better true lower old_lower, better false upper old_upper in
    if value = (old_lower, old_upper) then false
    else (Ids.replace bounds term value; true) in
  let combine f a b = match a, b with
    | Some a, Some b -> Some (f ?poll:(Some charge) a b)
    | _ -> None in
  let hull lower a b = match a, b with
    | Some a, Some b -> Some
        (if (Z.compare a b < 0) = lower then a else b)
    | _ -> None in
  let add (al, au) (bl, bu) =
    combine Z.add al bl, combine Z.add au bu in
  let neg (lower, upper) = Option.map Z.neg upper, Option.map Z.neg lower in
  let known value = Some value, Some value in
  let ranges = Ids.create 128 in
  let rec range term =
    charge ();
    match Ids.find_opt ranges term with
    | Some result -> result
    | None ->
      let result = match Term.Unsafe.node t.terms term with
        | Term.Integer value -> known value
        | Term.Apply (symbol, args) ->
          (match (Term.Unsafe.signature t.terms symbol).kind, args with
          | Term.Builtin "+", _ ->
            Array.fold_left (fun sum x -> add sum (range x))
              (known Z.zero) args
          | Term.Builtin "-", [|x|] -> neg (range x)
          | Term.Builtin "-", [|x; y|] -> add (range x) (neg (range y))
          | Term.Builtin "ite", [|condition; left; right|] ->
            (match decide condition with
            | Some true -> range left
            | Some false -> range right
            | None ->
              let al, au = range left and bl, bu = range right in
              hull true al bl, hull false au bu)
          | _ -> Option.value (Ids.find_opt bounds term)
              ~default:(None, None))
        | _ -> None, None in
      Ids.add ranges term result;
      result
  and decide term =
    charge ();
    match Term.Unsafe.node t.terms term with
    | Term.Boolean value -> Some value
    | Term.Apply (symbol, [|left; right|]) ->
      let op = (Term.Unsafe.signature t.terms symbol).kind in
      (match op with
      | Term.Builtin ("<" | "<=" | ">" | ">=") ->
        let left, right = match op with
          | Term.Builtin (">" | ">=") -> right, left
          | _ -> left, right in
        let strict = op = Term.Builtin "<" || op = Term.Builtin ">" in
        let lower, upper = range left and lo, hi = range right in
        (match upper, lo with
        | Some x, Some y when (if strict then Z.compare x y < 0
                              else Z.compare x y <= 0) -> Some true
        | _ -> match lower, hi with
          | Some x, Some y when (if strict then Z.compare x y >= 0
                                else Z.compare x y > 0) -> Some false
          | _ -> None)
      | _ -> None)
    | _ -> None in
  let rec constrain term ((lower, upper) as interval) =
    charge ();
    if atomic term then begin
      let changed = update term interval in
      if changed then Ids.clear ranges;
      changed
    end else match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, args) ->
        (match (Term.Unsafe.signature t.terms symbol).kind, args with
        | Term.Builtin "-", [|x|] -> constrain x (neg interval)
        | Term.Builtin "+", [|x; y|] ->
          let xl, xu = range x and yl, yu = range y in
          let a = constrain x
              (combine Z.sub lower yu, combine Z.sub upper yl) in
          let b = constrain y
              (combine Z.sub lower xu, combine Z.sub upper xl) in
          a || b
        | Term.Builtin "-", [|x; y|] ->
          let xl, xu = range x and yl, yu = range y in
          let a = constrain x
              (combine Z.add lower yl, combine Z.add upper yu) in
          let b = constrain y
              (combine Z.sub xl upper, combine Z.sub xu lower) in
          a || b
        | _ -> false)
      | _ -> false in
  try
    List.iter (fun term -> assign term true) pending;
    while not (Queue.is_empty queue) do
      charge ();
      let term, truth = Queue.take queue in
      List.iter (fun (other, same) -> assign other (truth = same))
        (Option.value (Ids.find_opt watchers term) ~default:[]);
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, args) ->
        (match (Term.Unsafe.signature t.terms symbol).kind, args with
        | Term.Builtin "not", [|x|] -> assign x (not truth)
        | Term.Builtin ("and" | "or" as op), _
          when truth = (op = "and") ->
          Array.iter (fun x -> assign x truth) args
        | Term.Builtin "=>", [|x; y|] when not truth ->
          assign x true; assign y false
        | Term.Builtin "=", [|x; y|] when Term.sort t.terms x = Term.Bool ->
          connect x y truth;
          Array.iter (fun x -> match Term.Unsafe.node t.terms x with
            | Term.Boolean value -> assign x value | _ -> ()) args
        | Term.Builtin ("<" | "<=" | ">" | ">=" as op), [|x; y|] ->
          let x, y = if (op = "<" || op = "<=") = truth then x, y
            else y, x in
          let strict = (op = "<" || op = ">") = truth in
          constraints := (x, y, strict) :: !constraints
        | Term.Builtin "=", [|x; y|]
          when truth && Term.sort t.terms x = Term.Int ->
          constraints := (x, y, false) :: (y, x, false) :: !constraints
        | _ -> ())
      | _ -> ()
    done;
    let rounds = ref 8 and changed = ref true in
    while !rounds > 0 && !changed do
      decr rounds;
      changed := false;
      Ids.clear ranges;
      List.iter (fun (left, right, strict) ->
        charge ();
        let lower, _ = range left and _, upper = range right in
        let lower, upper = if strict then
            Option.map (fun x -> Z.add ~poll:charge x Z.one) lower,
            Option.map (fun x -> Z.sub ~poll:charge x Z.one) upper
          else lower, upper in
        let a = constrain left (None, upper) in
        let b = constrain right (lower, None) in
        if a || b then begin
          changed := true;
          Ids.clear ranges
        end) !constraints
    done;
    let seen = Ids.create 128 and units = Ids.create 32 in
    let rec visit term =
      charge ();
      if not (Ids.mem seen term) then begin
        Ids.add seen term ();
        match Term.Unsafe.node t.terms term with
        | Term.Apply (symbol, args) ->
          (match (Term.Unsafe.signature t.terms symbol).kind, args with
          | Term.Builtin "ite", [|condition; left; right|] ->
            (match decide condition with
            | Some value ->
              Ids.replace units condition value;
              visit (if value then left else right)
            | None -> Array.iter visit args)
          | _ -> Array.iter visit args)
        | _ -> ()
      end in
    List.iter visit pending;
    Ids.fold (fun condition value result ->
      charge ();
      (if value then condition else Term.not_ t.terms condition) :: result)
      units pending
  with Limit -> pending


let flush_assertions ?(preprocessed = false) t =
  update_occurrence_activity t;
  let pending = match t.pending_assertions with
    | last :: rest when t.goal_first -> last :: reverse t rest
    | pending -> reverse t pending in
  t.pending_assertions <- [];
  let pending, expanded = if preprocessed then pending, false
    else linearize_finite_products t pending in
  let rec simplify remaining pending =
    let before = t.substitutions in
    List.iter (fun term -> assert_inner t (rewrite t term)) pending;
    let pending = reverse t t.pending_assertions in
    t.pending_assertions <- [];
    if remaining > 0 && before != t.substitutions
    then simplify (remaining - 1) pending
    else List.map (rewrite t) pending in
  let pending = if preprocessed then List.map (rewrite t) pending
    else eliminate_fixed_linear_forms t (simplify 3 pending) in
  let pending = add_interval_conditions t pending in
  let pending = if finite_domain_inconsistent t pending
    || (pending <> [] && t.occurrence_activity
        && boolean_cardinality_inconsistent t t.replay_assertions)
    || Finite_evaluation.inconsistent t pending
    || difference_inconsistent t pending
    then [Term.boolean t.terms false] else pending in
  List.iter (mark_used t) pending;
  List.iter (fun term ->
    poll t;
    let literal = required_positive t term in
    Sat.add_clause (sat t) [|literal|]) pending;
  if pending <> [] then required_literals t pending;
  match t.prepared_frames with
  | previous :: rest ->
    let previous = match previous with
      | None -> [] | Some frame -> frame.frame_assertions in
    let frame_assertions = if pending = [] then previous
      else pending :: previous in
    let parent_replay = match t.replay_scopes with
      | [] -> [] | parent :: _ -> parent in
    let prepared =
      { frame_assertions; frame_substitutions = t.substitutions;
        frame_replay = t.replay_assertions; parent_replay;
        frame_finite_products = t.finite_products || expanded } in
    t.prepared_frames <- Some prepared :: rest;
    t.finite_products <- prepared.frame_finite_products
  | [] -> invalid_arg "SMT prepared frame invariant"

let restore_frame t frame assertions =
  let rec same left right =
    poll t;
    match left, right with
    | [], _ -> right == frame.parent_replay
    | left :: rest, right :: tail -> left = right && same rest tail
    | _ -> false in
  if t.replay_assertions != frame.parent_replay
     || not (same assertions frame.frame_replay)
  then false else begin
    if t.substitutions != frame.frame_substitutions then begin
      t.substitutions <- frame.frame_substitutions;
      invalidate_rewrite t
    end;
    t.pending_assertions <- List.fold_left (fun assertions block ->
      List.fold_left (fun assertions term -> poll t; term :: assertions)
        assertions block) [] (reverse t frame.frame_assertions);
    t.replay_assertions <- frame.frame_replay;
    t.finite_products <- frame.frame_finite_products;
    (* Child frames restore their saved substitutions. Replaying a parent
       must preserve the substitution decisions recorded with its assertions. *)
    flush_assertions ~preprocessed:true t;
    true
  end

let rec register t term =
  if Option.is_some t.resource then shared_poll t;
  Option.iter (fun solver -> register solver term) t.alternative;
  (match t.result_solver with
  | Some solver when (match t.alternative with
      | None -> true | Some alternate -> solver != alternate) ->
    register solver term
  | _ -> ());
  (match Term.sort t.terms term with
  | Term.Int | Term.Named _ -> t.theory_required <- true
  | Term.Bool | Term.Bitvec _ -> ());
  Ids.replace t.declared_terms term ();
  Option.iter (fun d -> DT.register_sort d (Term.sort t.terms term))
    t.datatypes
let allocation_count t =
  Sat.variable_count (sat t) + Sat.clause_count (sat t)

let rec interrupted_theory t =
  t.interrupted_theory
  || Option.fold ~none:false ~some:interrupted_theory t.alternative
  || Option.fold ~none:false ~some:interrupted_theory t.result_solver

let removes_nonlinear t ~poll count =
  let registered = NL.registration_count (nonlinear t) in
  let rec removed count = function
    | _ when count = 0 -> false
    | [] -> false
    | (_, _, before) :: scopes ->
      poll ();
      before < registered || removed (count - 1) scopes in
  removed count t.allocation_scopes

let needs_reclamation t =
  NL.needs_reclamation (nonlinear t)
  || (t.popped_allocations >= 4096
      && t.popped_allocations >= allocation_count t - t.popped_allocations)

let push t =
  flush_assertions t;
  t.prepared_frames <- None :: t.prepared_frames;
  forget_alternative t;
  t.replay_scopes <- t.replay_assertions :: t.replay_scopes;
  t.model <- None;
  t.pending_scopes <- t.pending_assertions :: t.pending_scopes;
  t.substitution_scopes <- t.substitutions :: t.substitution_scopes;
  t.allocation_scopes <-
    (allocation_count t, t.popped_allocations,
      NL.registration_count (nonlinear t)) :: t.allocation_scopes;
  Sat.push (sat t)
let pop t =
  match t.substitution_scopes with
  | [] -> invalid_arg "SMT pop without push"
  | substitutions :: rest ->
    (match t.prepared_frames with
    | _ :: (_ :: _ as frames) ->
      t.prepared_frames <- frames;
      t.finite_products <- (match frames with
        | Some frame :: _ -> frame.frame_finite_products
        | _ -> false)
    | _ -> invalid_arg "SMT prepared frame invariant");
    forget_alternative t;
    (match t.replay_scopes with
    | assertions :: scopes ->
      t.replay_assertions <- assertions; t.replay_scopes <- scopes
    | [] -> invalid_arg "SMT replay scope invariant");
    t.model <- None;
    (match t.pending_scopes with
    | pending :: scopes ->
      t.pending_assertions <- pending; t.pending_scopes <- scopes
    | [] -> invalid_arg "SMT pending scope invariant");
    Sat.pop (sat t);
    (match t.allocation_scopes with
    | (allocated, popped, _) :: scopes ->
      t.popped_allocations <- allocation_count t - allocated + popped;
      t.allocation_scopes <- scopes
    | [] -> invalid_arg "SMT allocation scope invariant");
    t.substitutions <- substitutions;
    invalidate_rewrite t;
    t.substitution_scopes <- rest
let statistics t = Sat.statistics (sat t)
let sat_work t =
  let s = statistics t in s.propagations + s.conflicts - t.base_sat_work
let resource_count t = t.budget - t.remaining + sat_work t
module Small_model = struct
  type number =
    | Constant of Z.t
    | Variable of int
    | Add of number array
    | Subtract of number array
    | Multiply of number array
    | Divide of number * number
    | Modulo of number * number
    | Choose of formula * number * number
  and formula =
    | Boolean of bool
    | Proposition of int
    | Compare of string * number * number
    | Not of formula
    | All of formula array
    | Any of formula array
    | Equivalent of formula * formula
    | Select of formula * formula * formula

  exception Skip
  type assignments =
    { integers : (Term.term * Z.t) list;
      booleans : (Term.term * bool) list;
      projections : (Term.term * Term.term) list }

  exception Found of assignments

  module Numbers = Hashtbl.Make (struct
    type t = number
    let equal (x : t) y = x == y
    let hash = Hashtbl.hash
  end)

  module Formulas = Hashtbl.Make (struct
    type t = formula
    let equal (x : t) y = x == y
    let hash = Hashtbl.hash
  end)

  let bitvector_width ~charge domain_width constraints =
    let maximum = ref 0 in
    let bounds = Numbers.create 32 and formulas = Formulas.create 32 in
    let limit = 0x3fffffff in
    let bounded value = if value > limit then raise Skip else value in
    let rec magnitude number =
      charge ();
      match Numbers.find_opt bounds number with
      | Some bound -> bound
      | None ->
        let bound = match number with
          | Constant value ->
            (match Z.to_int_opt (Z.abs value) with
            | Some value -> bounded value | None -> raise Skip)
          | Variable _ -> 1 lsl (domain_width - 1)
          | Add arguments | Subtract arguments ->
            Array.fold_left (fun sum number ->
              let bound = magnitude number in
              if bound > limit - sum then raise Skip;
              sum + bound) 0 arguments
          | Multiply arguments ->
            Array.fold_left (fun product number ->
              let bound = magnitude number in
              if bound <> 0 && product > limit / bound then raise Skip;
              product * bound) 1 arguments
          | Divide _ | Modulo _ -> raise Skip
          | Choose (condition, left, right) ->
            formula_bounds condition;
            max (magnitude left) (magnitude right) in
        maximum := max !maximum bound;
        Numbers.add bounds number bound;
        bound
    and formula_bounds formula =
      charge ();
      if not (Formulas.mem formulas formula) then begin
        Formulas.add formulas formula ();
        match formula with
        | Boolean _ | Proposition _ -> ()
        | Compare (_, left, right) ->
          ignore (magnitude left); ignore (magnitude right)
        | Not arg -> formula_bounds arg
        | All args | Any args -> Array.iter formula_bounds args
        | Equivalent (left, right) ->
          formula_bounds left; formula_bounds right
        | Select (condition, left, right) ->
          formula_bounds condition;
          formula_bounds left;
          formula_bounds right
      end in
    List.iter formula_bounds constraints;
    let width = ref 1 and bound = ref !maximum in
    while !bound > 0 do
      charge ();
      incr width;
      bound := !bound lsr 1
    done;
    max !width domain_width

  let bitvector_model ~poll variables propositions constraints =
    let remaining = ref 50000 in
    let charge () =
      if !remaining = 0 then raise Skip;
      decr remaining;
      poll () in
    let attempt domain_width =
      let width = bitvector_width ~charge domain_width constraints in
      let terms = Term.create () in
      Term.with_poll terms charge (fun () ->
      let sort = Term.Bitvec width in
      let raw_variables = Array.init variables (fun index ->
        charge ();
        Term.apply terms (Term.symbol terms
          { name = string_of_int index; arguments = [||];
            result = Term.Bitvec domain_width; kind = Term.Uninterpreted })
          [||]) in
      let variables = Array.map (fun term ->
        if width = domain_width then term else
        Term.builtin terms
          ("sign_extend:" ^ string_of_int (width - domain_width)) sort
          [|term|]) raw_variables in
      let propositions = Array.init propositions (fun index ->
        charge ();
        Term.apply terms (Term.symbol terms
          { name = "p" ^ string_of_int index; arguments = [||];
            result = Term.Bool; kind = Term.Uninterpreted }) [||]) in
      let numbers = Numbers.create 32 and formulas = Formulas.create 32 in
      let rec number term =
        charge ();
        match Numbers.find_opt numbers term with
        | Some result -> result
        | None ->
          let fold operator neutral arguments =
            Array.fold_left (fun result term ->
              Term.builtin terms operator sort [|result; number term|])
              (Term.bits terms width neutral) arguments in
          let result = match term with
            | Constant value -> Term.bits terms width value
            | Variable index -> variables.(index)
            | Add arguments -> fold "bvadd" Z.zero arguments
            | Multiply arguments -> fold "bvmul" Z.one arguments
            | Subtract arguments ->
              if Array.length arguments = 0 then raise Skip
              else if Array.length arguments = 1 then
                Term.builtin terms "bvneg" sort [|number arguments.(0)|]
              else begin
                let result = ref (number arguments.(0)) in
                for index = 1 to Array.length arguments - 1 do
                  result := Term.builtin terms "bvsub" sort
                    [|!result; number arguments.(index)|]
                done;
                !result
              end
            | Divide _ | Modulo _ -> raise Skip
            | Choose (condition, left, right) ->
              Term.ite terms (formula condition) (number left) (number right) in
          Numbers.add numbers term result;
          result
      and formula term =
        charge ();
        match Formulas.find_opt formulas term with
        | Some result -> result
        | None ->
          let result = match term with
            | Boolean value -> Term.boolean terms value
            | Proposition index -> propositions.(index)
            | Compare (operator, left, right) ->
              let name = match operator with
                | "=" -> "=" | "<" -> "bvslt" | "<=" -> "bvsle"
                | ">" -> "bvsgt" | _ -> "bvsge" in
              Term.builtin terms name Term.Bool
                [|number left; number right|]
            | Not arg -> Term.not_ terms (formula arg)
            | All args -> Term.and_ terms (Array.map formula args)
            | Any args -> Term.or_ terms (Array.map formula args)
            | Equivalent (left, right) ->
              Term.equal terms (formula left) (formula right)
            | Select (condition, left, right) ->
              Term.ite terms (formula condition) (formula left)
                (formula right) in
          Formulas.add formulas term result;
          result in
      let assertions = List.map formula constraints in
      let solver = Sat.create () in
      let fresh () = charge (); Sat.new_var solver in
      let clause literals = charge (); Sat.add_clause solver literals in
      let truth = fresh () in
      clause [|truth|];
      let conjunction literals =
        if Array.exists ((=) (-truth)) literals then -truth else
        let literals = Array.of_list (List.filter ((<>) truth)
          (Array.to_list literals)) in
        if Array.length literals = 0 then truth
        else if Array.length literals = 1 then literals.(0)
        else begin
          let result = fresh () in
          Array.iter (fun literal -> clause [|-result; literal|]) literals;
          clause (Array.append [|result|] (Array.map (~-) literals));
          result
        end in
      let disjunction literals =
        -conjunction (Array.map (~-) literals) in
      let cache = Ids.create 32 in
      let rec boolean term =
        charge ();
        match Ids.find_opt cache term with
        | Some result -> result
        | None ->
          let result = match Term.Unsafe.node terms term with
            | Term.Boolean value -> if value then truth else -truth
            | Term.Apply (symbol, args) ->
              (match (Term.Unsafe.signature terms symbol).kind, args with
              | Term.Uninterpreted, [||]
                  when Term.sort terms term = Term.Bool -> fresh ()
              | Term.Builtin "not", [|arg|] -> -boolean arg
              | Term.Builtin "and", _ ->
                conjunction (Array.map boolean args)
              | Term.Builtin "or", _ ->
                disjunction (Array.map boolean args)
              | Term.Builtin "ite", [|condition; left; right|] ->
                let condition = boolean condition in
                disjunction [|
                  conjunction [|condition; boolean left|];
                  conjunction [|-condition; boolean right|]|]
              | Term.Builtin "=", [|left; right|]
                  when Term.sort terms left = Term.Bool ->
                let left = boolean left and right = boolean right in
                let result = fresh () in
                clause [|-result; -left; right|];
                clause [|-result; left; -right|];
                clause [|result; left; right|];
                clause [|result; -left; -right|];
                result
              | Term.Builtin name, _ ->
                (match BV.predicate (Lazy.force bitvectors) name args with
                | Some literal -> literal | None -> raise Skip)
              | _ -> raise Skip)
            | _ -> raise Skip in
          Ids.add cache term result;
          result
      and bitvectors = lazy
        (BV.create ~terms ~fresh ~clause ~boolean ~poll:charge ()) in
      List.iter (fun term -> clause [|boolean term|]) assertions;
      let bits = Array.map (BV.bits (Lazy.force bitvectors)) raw_variables in
      let propositions = Array.map boolean propositions in
      let accounted = ref 0 in
      let account () =
        let statistics = Sat.statistics solver in
        let work = statistics.propagations + statistics.conflicts in
        while !accounted < work do
          charge ();
          incr accounted
        done in
      let should_stop () = account (); false in
      let result = Sat.solve ~max_conflicts:2000 ~should_stop solver in
      account ();
      match result with
      | Sat.Sat model ->
        let integers = Array.map (fun bits ->
          let value = ref 0 in
          Array.iteri (fun index literal ->
            charge ();
            if model.(abs literal) = (literal > 0)
            then value := !value lor (1 lsl index)) bits;
          if !value land (1 lsl (domain_width - 1)) <> 0
          then value := !value - (1 lsl domain_width);
          Z.of_int !value) bits in
        let booleans = Array.map (fun literal ->
          charge (); model.(abs literal) = (literal > 0)) propositions in
        Some (integers, booleans)
      | Sat.Unsat _ | Sat.Unknown -> None)
    in
    try
      let rec search = function
        | [] -> None
        | width :: rest ->
          (match attempt width with
          | Some _ as result -> result | None -> search rest) in
      search [3; 5; 8]
    with Skip -> None

  module Integers = Hashtbl.Make (struct
    type t = Z.t
    let equal = Z.equal
    let hash = Hashtbl.hash
  end)

  let separable_model ~charge values evaluate satisfies satisfied constraints =
    let dependencies = Numbers.create 32 in
    let rec number = function
      | Constant _ -> charge (); 0
      | Variable index -> charge (); 1 lsl index
      | expression ->
        charge ();
        match Numbers.find_opt dependencies expression with
        | Some variables -> variables
        | None ->
          let variables = match expression with
            | Add args | Subtract args | Multiply args ->
              Array.fold_left (fun mask arg -> mask lor number arg) 0 args
            | Divide (left, right) | Modulo (left, right) ->
              number left lor number right
            | Choose (condition, left, right) ->
              formula condition lor number left lor number right
            | Constant _ | Variable _ -> raise Skip in
          Numbers.add dependencies expression variables;
          variables
    and formula expression =
      charge ();
      match expression with
      | Boolean _ -> 0
      | Proposition _ -> 0
      | Compare (_, left, right) -> number left lor number right
      | Not arg -> formula arg
      | All args | Any args ->
        Array.fold_left (fun mask arg -> mask lor formula arg) 0 args
      | Equivalent (left, right) -> formula left lor formula right
      | Select (condition, left, right) ->
        formula condition lor formula left lor formula right in
    let solve left right =
      let groups = Array.make (Array.length values) [] in
      let constant = ref Z.zero in
      let rec collect positive expression =
        charge ();
        match expression with
        | Add args -> Array.iter (collect positive) args
        | Choose (condition, left, right) when formula condition = 0 ->
          collect positive (if satisfies condition then left else right)
        | Subtract args when Array.length args = 1 ->
          collect (not positive) args.(0)
        | Subtract args when Array.length args > 1 ->
          collect positive args.(0);
          for index = 1 to Array.length args - 1 do
            collect (not positive) args.(index)
          done
        | _ ->
          let mask = number expression in
          if mask = 0 then begin
            let value = evaluate expression in
            constant := Z.add ~poll:charge !constant
              (if positive then value else Z.neg value)
          end else if mask land (mask - 1) <> 0 then raise Skip
          else begin
            let index = ref 0 in
            while 1 lsl !index <> mask do incr index done;
            groups.(!index) <- (positive, expression) :: groups.(!index)
          end in
      collect true left;
      collect false right;
      let groups = Array.of_list (List.filter_map (fun (index, terms) ->
          if terms = [] then None else Some (index, terms))
          (Array.to_list (Array.mapi (fun index terms -> index, terms) groups)))
      in
      let count = Array.length groups in
      if count = 0 || count > 3 then raise Skip;
      let attempt radius =
        let domain = Array.init (2 * radius) (fun index ->
          Z.of_int (if index land 1 = 0 then index / 2
                    else -(index + 1) / 2)) in
        Array.fill values 0 (Array.length values) Z.zero;
        let tables = Array.map (fun (variable, terms) ->
          Array.map (fun value ->
            charge ();
            values.(variable) <- value;
            List.fold_left (fun sum (positive, expression) ->
              let value = evaluate expression in
              Z.add ~poll:charge sum (if positive then value else Z.neg value))
              Z.zero terms) domain) groups in
        Array.fill values 0 (Array.length values) Z.zero;
        let last = count - 1 in
        let lookup = Integers.create (Array.length domain) in
        Array.iteri (fun index value ->
          charge ();
          let indices = match Integers.find_opt lookup value with
            | None -> [] | Some indices -> indices in
          if List.length indices < 8 then
            Integers.replace lookup value (index :: indices)) tables.(last);
        let rec search index sum =
          charge ();
          let variable, _ = groups.(index) in
          if index = last then
            match Integers.find_opt lookup (Z.neg sum) with
            | None -> false
            | Some indices -> List.exists (fun choice ->
                values.(variable) <- domain.(choice);
                satisfied ()) indices
          else
            let choice = ref 0 and found = ref false in
            while not !found && !choice < Array.length domain do
              values.(variable) <- domain.(!choice);
              found := search (index + 1)
                (Z.add ~poll:charge sum tables.(index).(!choice));
              incr choice
            done;
            !found in
        search 0 !constant in
      List.exists attempt [16; 128] in
    let rec search = function
      | All args | Any args -> Array.exists search args
      | Not arg -> search arg
      | Equivalent (left, right) -> search left || search right
      | Select (condition, left, right) ->
        search condition || search left || search right
      | Compare ("=", left, right) ->
        (try solve left right with Skip -> false)
      | _ -> false in
    List.exists search constraints

  let cubic_model ~charge values satisfied constraints =
    if Sys.word_size < 64 then raise Skip;
    (* Native search intermediates stay below 2^43. *)
    let solve left right =
      let coefficients = Array.make (Array.length values) Z.zero in
      let constant = ref Z.zero in
      let rec monomial = function
        | Constant value -> charge (); value, None, 0
        | Variable index -> charge (); Z.one, Some index, 1
        | Multiply arguments ->
          charge ();
          Array.fold_left (fun (coefficient, variable, degree) argument ->
            let factor, other, power = monomial argument in
            let variable = match variable, other with
              | None, variable | variable, None -> variable
              | Some x, Some y when x = y -> variable
              | _ -> raise Skip in
            if degree + power > 3 then raise Skip;
            Z.mul ~poll:charge coefficient factor, variable, degree + power)
            (Z.one, None, 0) arguments
        | Subtract [|argument|] ->
          charge ();
          let coefficient, variable, degree = monomial argument in
          Z.neg coefficient, variable, degree
        | _ -> raise Skip in
      let rec collect positive = function
        | Add arguments -> charge (); Array.iter (collect positive) arguments
        | Subtract [|argument|] -> charge (); collect (not positive) argument
        | Subtract arguments when Array.length arguments > 1 ->
          charge ();
          collect positive arguments.(0);
          for index = 1 to Array.length arguments - 1 do
            collect (not positive) arguments.(index)
          done
        | expression ->
          let coefficient, variable, degree = monomial expression in
          let coefficient = if positive then coefficient else Z.neg coefficient
          in
          match variable, degree with
          | None, 0 -> constant := Z.add ~poll:charge !constant coefficient
          | Some index, 3 ->
            coefficients.(index) <-
              Z.add ~poll:charge coefficients.(index) coefficient
          | _ -> raise Skip in
      collect true left;
      collect false right;
      let groups = Array.to_list (Array.mapi (fun index coefficient ->
        charge (); index, coefficient) coefficients)
        |> List.filter (fun (_, coefficient) -> not (Z.is_zero coefficient))
        |> Array.of_list in
      if Array.length groups <> 3 then raise Skip;
      let scale = Z.abs (snd groups.(0)) in
      if not (Array.for_all (fun (_, coefficient) ->
        charge (); Z.equal (Z.abs coefficient) scale) groups)
      then raise Skip;
      let target, remainder = Z.quot_rem ~poll:charge (Z.neg !constant) scale in
      if not (Z.is_zero remainder) then raise Skip;
      let target = match Z.to_int_opt target with
        | Some target when target >= -(1 lsl 40) && target <= 1 lsl 40 -> target
        | _ -> raise Skip in
      let permutations = [|[|0; 1; 2|]; [|0; 2; 1|]; [|1; 0; 2|];
                            [|1; 2; 0|]; [|2; 0; 1|]; [|2; 1; 0|]|] in
      let exception Found_cubic in
      let propose x y z =
        let proposed = [|x; y; z|] in
        if Array.exists (fun permutation ->
          Array.iteri (fun index (variable, coefficient) ->
            charge ();
            let value = Z.of_int proposed.(permutation.(index)) in
            values.(variable) <- if Z.sign coefficient < 0
              then Z.neg value else value) groups;
          satisfied ()) permutations then raise Found_cubic in
      let test sum quotient z =
        charge ();
        let discriminant = 4 * quotient - sum * sum in
        if discriminant >= 0 && discriminant mod 3 = 0 then begin
          let square = discriminant / 3 in
          let root = int_of_float (sqrt (float_of_int square)) in
          if root * root = square && (sum + root) land 1 = 0 then
            propose ((sum + root) / 2) ((sum - root) / 2) z
        end in
      Array.fill values 0 (Array.length values) Z.zero;
      try
        for sum = 1 to 256 do
          charge ();
          for residue = 0 to sum - 1 do
            charge ();
            if (residue * residue * residue - target) mod sum = 0 then begin
              let first = -4096 + (residue + 4096) mod sum in
              let rec search z =
                if z <= 4096 then begin
                  charge ();
                  let quotient = (target - z * z * z) / sum in
                  test sum quotient z;
                  test (-sum) (-quotient) z;
                  search (z + sum)
                end in
              search first
            end
          done
        done;
        false
      with Found_cubic -> true in
    let rec search expression =
      charge ();
      match expression with
      | All arguments | Any arguments -> Array.exists search arguments
      | Not argument -> search argument
      | Equivalent (left, right) -> search left || search right
      | Select (condition, left, right) ->
        search condition || search left || search right
      | Compare ("=", left, right) ->
        (try solve left right with Skip -> false)
      | _ -> false in
    List.exists search constraints

  let finite_coloring ~poll terms assertions =
    let remaining = ref 4000000 in
    let charge () =
      poll ();
      decr remaining;
      if !remaining < 0 then raise Skip in
    let sort = ref None and palette = ref [||] in
    let indices = Ids.create 64 and domains = Ids.create 64 in
    let edges = ref [] in
    let constructor term = match Term.Unsafe.node terms term with
      | Term.Apply (symbol, [||]) ->
        (match (Term.Unsafe.signature terms symbol).kind with
        | Term.Constructor _ -> true | _ -> false)
      | _ -> false in
    let same_sort term =
      let current = Term.sort terms term in
      match !sort with
      | Some previous when previous = current -> ()
      | Some _ -> raise Skip
      | None ->
        let constructors = match current with
          | Term.Named name -> Term.constructors terms name
          | _ -> raise Skip in
        let rec count total = function
          | [] -> total
          | _ :: rest ->
            charge ();
            if total >= 512 then raise Skip;
            count (total + 1) rest in
        if count 0 constructors < 2 then raise Skip;
        let constructors = List.rev constructors in
        palette := Array.of_list (List.map (fun symbol ->
          charge ();
          if Array.length (Term.Unsafe.signature terms symbol).arguments <> 0
          then raise Skip;
          Term.apply terms symbol [||]) constructors);
        Array.iteri (fun index term ->
          charge (); Ids.add indices term index) !palette;
        sort := Some current in
    let width = Sys.int_size - 2 in
    let words () = (Array.length !palette + width - 1) / width in
    let singleton color =
      let bits = Array.make (words ()) 0 in
      bits.(color / width) <- 1 lsl (color mod width);
      bits in
    let full () = Array.init (words ()) (fun index ->
      charge ();
      let length = min width (Array.length !palette - index * width) in
      (1 lsl length) - 1) in
    let vertex term =
      charge ();
      same_sort term;
      if not (Ids.mem domains term) then begin
        if Ids.length domains >= 512 then raise Skip;
        let mask = match Ids.find_opt indices term with
          | Some color -> singleton color
          | None ->
            (match Term.Unsafe.node terms term with
            | Term.Apply (symbol, arguments)
              when (Term.Unsafe.signature terms symbol).kind
                  = Term.Uninterpreted
                && Array.for_all (fun term -> charge (); constructor term)
                    arguments -> ()
            | _ -> raise Skip);
            full () in
        Ids.add domains term mask
      end in
    let equality term =
      charge ();
      match Term.Unsafe.node terms term with
      | Term.Apply (symbol, [|left; right|])
        when (Term.Unsafe.signature terms symbol).kind = Term.Builtin "=" ->
        if constructor right then left, right
        else if constructor left then right, left
        else raise Skip
      | _ -> raise Skip in
    let restrict subject values =
      vertex subject;
      let available = Array.mapi (fun index word ->
        charge (); word land values.(index)) (Ids.find domains subject) in
      if Array.for_all (fun word -> charge (); word = 0) available
      then raise Skip;
      Ids.replace domains subject available in
    let rec collect term =
      charge ();
      match Term.Unsafe.node terms term with
      | Term.Boolean true -> ()
      | Term.Apply (symbol, arguments) ->
        (match (Term.Unsafe.signature terms symbol).kind, arguments with
        | Term.Builtin "and", _ -> Array.iter collect arguments
        | Term.Builtin "or", _ ->
          let subject = ref None and values = ref None in
          Array.iter (fun atom ->
            let variable, value = equality atom in
            vertex variable;
            same_sort value;
            (match !subject with
            | None -> subject := Some variable
            | Some previous when previous = variable -> ()
            | _ -> raise Skip);
            let bits = match !values with
              | Some bits -> bits
              | None ->
                let bits = Array.make (words ()) 0 in
                values := Some bits; bits in
            let color = Ids.find indices value in
            bits.(color / width) <-
              bits.(color / width) lor (1 lsl (color mod width))) arguments;
          (match !subject, !values with
          | Some term, Some bits -> restrict term bits
          | _ -> raise Skip)
        | Term.Builtin "=", _ ->
          let subject, value = equality term in
          vertex subject;
          same_sort value;
          restrict subject (singleton (Ids.find indices value))
        | Term.Builtin "distinct", _ ->
          Array.iter vertex arguments;
          for i = 0 to Array.length arguments - 1 do
            for j = i + 1 to Array.length arguments - 1 do
              charge ();
              edges := (arguments.(i), arguments.(j)) :: !edges
            done
          done
        | Term.Builtin "not", [|argument|] ->
          (match Term.Unsafe.node terms argument with
          | Term.Apply (symbol, [|left; right|])
              when (Term.Unsafe.signature terms symbol).kind
                = Term.Builtin "=" ->
            vertex left; vertex right;
            edges := (left, right) :: !edges
          | _ -> raise Skip)
        | _ -> raise Skip)
      | _ -> raise Skip in
    try
      List.iter collect assertions;
      if !edges = [] then None else begin
        let vertices = Ids.fold (fun term _ values ->
          charge (); term :: values) domains []
          |> List.sort (fun a b -> charge (); Int.compare a b)
          |> Array.of_list in
        let count = Array.length vertices in
        let numbers = Ids.create count in
        Array.iteri (fun index term ->
          charge (); Ids.add numbers term index) vertices;
        let adjacent = Array.make count Required.empty in
        List.iter (fun (left, right) ->
          charge ();
          let a = Ids.find numbers left and b = Ids.find numbers right in
          if a = b then raise Skip;
          adjacent.(a) <- Required.add b adjacent.(a);
          adjacent.(b) <- Required.add a adjacent.(b)) !edges;
        let degree = Array.make count 0 in
        let adjacent = Array.mapi (fun index neighbors ->
          Required.fold (fun neighbor values ->
            charge ();
            degree.(index) <- degree.(index) + 1;
            neighbor :: values) neighbors []) adjacent in
        let available = Array.map (Ids.find domains) vertices in
        let assigned = Array.make count (-1) in
        let cardinal mask =
          let rec loop mask count =
            charge ();
            if mask = 0 then count else loop (mask land (mask - 1)) (count + 1)
          in loop mask 0 in
        let available_count = Array.map (fun bits ->
          Array.fold_left (fun count word ->
            charge (); count + cardinal word) 0 bits) available in
        let attempts = ref (4 * count) in
        let rec search left =
          charge ();
          if left = 0 then true else begin
            let chosen = ref (-1) and choices = ref max_int in
            for i = 0 to count - 1 do
              charge ();
              if assigned.(i) < 0 then begin
                let size = available_count.(i) in
                if size < !choices
                    || size = !choices && !chosen >= 0
                       && degree.(i) > degree.(!chosen) then begin
                  chosen := i; choices := size
                end
              end
            done;
            if !chosen < 0 || !choices = 0 then false else
            let vertex = !chosen in
            let rec attempt color =
              charge ();
              if color = Array.length !palette then false else
              let word = color / width and value = 1 lsl (color mod width) in
              if available.(vertex).(word) land value = 0 then
                attempt (color + 1)
              else begin
                decr attempts;
                if !attempts < 0 then raise Skip;
                assigned.(vertex) <- color;
                let previous = ref [] in
                List.iter (fun neighbor ->
                  charge ();
                  if assigned.(neighbor) < 0
                    && available.(neighbor).(word) land value <> 0 then begin
                    previous := (neighbor, available.(neighbor).(word))
                      :: !previous;
                    available.(neighbor).(word) <-
                      available.(neighbor).(word) land lnot value;
                    available_count.(neighbor) <- available_count.(neighbor) - 1
                  end) adjacent.(vertex);
                if search (left - 1) then true else begin
                  List.iter (fun (neighbor, old) ->
                    charge (); available.(neighbor).(word) <- old;
                    available_count.(neighbor) <-
                      available_count.(neighbor) + 1)
                    !previous;
                  assigned.(vertex) <- -1;
                  attempt (color + 1)
                end
              end in
            attempt 0
          end in
        if not (search count) then None else
        Some (Array.to_list (Array.mapi (fun index term ->
          charge ();
          term, !palette.(assigned.(index))) vertices)
          |> List.filter (fun (term, value) ->
            charge (); term <> value))
      end
    with Skip -> None

  let projection ~poll terms assertions =
    let seen = Ids.create 128 and projections = ref [] in
    let remaining = ref 4096 in
    let rec visit term =
      if not (Ids.mem seen term) then begin
        poll ();
        if !remaining = 0 then raise Skip;
        decr remaining;
        Ids.add seen term ();
        match Term.Unsafe.node terms term with
        | Term.Apply (symbol, arguments) ->
          let signature = Term.Unsafe.signature terms symbol in
          (match signature.kind, signature.arguments, arguments with
          | Term.Uninterpreted, [|Term.Int|], [|argument|]
              when signature.result = Term.Int ->
            projections := (term, argument) :: !projections
          | _ -> ());
          Array.iter visit arguments
        | _ -> ()
      end in
    try
      List.iter visit assertions;
      if List.length !projections < 32 then None
      else Some !projections
    with Skip -> None

  let bitvector_boundary ~poll ~resolve terms assertions =
    let remaining = ref 30000 in
    let charge () =
      if !remaining = 0 then raise Skip;
      decr remaining;
      poll () in
    let seen = Ids.create 64 and variables = Ids.create 8 in
    let hard = ref false in
    let rec collect term =
      charge ();
      if not (Ids.mem seen term) then begin
        if Ids.length seen = 128 then raise Skip;
        Ids.add seen term ();
        match Term.Unsafe.node terms term with
        | Term.Boolean _ | Term.Bits _ -> ()
        | Term.Integer _ -> raise Skip
        | Term.Apply (symbol, arguments) ->
          let signature = Term.Unsafe.signature terms symbol in
          (match signature.kind, arguments, signature.result with
          | Term.Uninterpreted, [||], Term.Bitvec width
            when width > 0 && width <= 64 ->
            if Ids.length variables = 4 then raise Skip;
            Ids.add variables term width
          | Term.Builtin name, _, (Term.Bitvec _ | Term.Bool) ->
            if name = "bvmul" || name = "bvudiv" || name = "bvsdiv"
               || name = "bvurem" || name = "bvsrem"
               || name = "bvshl" || name = "bvlshr" || name = "bvashr"
            then hard := true;
            Array.iter collect arguments
          | _ -> raise Skip)
      end in
    try
      let assertions = List.map (fun term -> charge (); resolve term)
          assertions in
      List.iter collect assertions;
      if not !hard || Ids.length variables = 0 then None else
      let variables = Ids.fold (fun term width values ->
        charge (); (term, width) :: values) variables []
        |> List.sort (fun (a, _) (b, _) -> charge (); Int.compare a b)
        |> Array.of_list in
      let domains = Array.map (fun (_, width) ->
        charge ();
        let power = Z.pow ~poll:charge (Z.of_int 2) (width - 1) in
        let mask = Z.sub ~poll:charge
            (Z.mul ~poll:charge power (Z.of_int 2)) Z.one in
        let candidates = [Z.zero; Z.one; Z.of_int 2; Z.of_int 3;
          mask; power; Z.add ~poll:charge power Z.one;
          Z.sub ~poll:charge power Z.one] in
        List.filter (fun value -> charge (); Z.compare value mask <= 0)
          candidates
        |> List.sort_uniq (fun a b -> charge (); Z.compare a b)
        |> List.map (Term.bits terms width)
        |> Array.of_list) variables in
      let selected = Array.map (fun domain -> domain.(0)) domains in
      let assignments = Ids.create 8 and cache = Ids.create 128 in
      let rec evaluate term =
        charge ();
        match Ids.find_opt cache term with
        | Some result -> result
        | None ->
          let result = match Ids.find_opt assignments term with
            | Some value -> value
            | None ->
              match Term.Unsafe.node terms term with
              | Term.Boolean _ | Term.Bits _ -> term
              | Term.Apply (symbol, arguments) ->
                let signature = Term.Unsafe.signature terms symbol in
                (match signature.kind with
                | Term.Builtin name ->
                  Term.builtin terms name signature.result
                    (Array.map evaluate arguments)
                | _ -> raise Skip)
              | _ -> raise Skip in
          Ids.add cache term result;
          result in
      let attempts = ref 0 in
      let satisfies () =
        incr attempts;
        if !attempts > 512 then raise Skip;
        Ids.clear assignments;
        Ids.clear cache;
        Array.iteri (fun index (term, _) ->
          charge (); Ids.add assignments term selected.(index)) variables;
        List.for_all (fun term ->
          match Term.Unsafe.node terms (evaluate term) with
          | Term.Boolean true -> true
          | Term.Boolean false -> false
          | _ -> raise Skip) assertions in
      let rec search index =
        charge ();
        if index = Array.length variables then satisfies () else
        let rec choose offset =
          if offset = Array.length domains.(index) then false else begin
            selected.(index) <- domains.(index).(offset);
            search (index + 1) || choose (offset + 1)
          end in
        choose 0 in
      if not (Term.with_poll terms charge (fun () -> search 0)) then None
      else Some
        { integers = []; booleans = [];
          projections = Array.to_list (Array.mapi (fun index (term, _) ->
            charge (); term, selected.(index)) variables) }
    with Skip -> None

  let factor_boundary ~poll ~resolve terms assertions =
    let remaining = ref 4096 in
    let charge () =
      poll ();
      if !remaining = 0 then raise Skip;
      decr remaining in
    let bounds = Ids.create 4 in
    let seen = Ids.create 32 and variables = Ids.create 16 in
    let edges = ref [] in
    let largest_positive = ref Z.one and has_affine = ref false in
    let rec variable term =
      charge ();
      match Term.Unsafe.node terms term with
      | Term.Apply (symbol, [||]) when
          (Term.Unsafe.signature terms symbol).kind = Term.Uninterpreted
          && Term.sort terms term = Term.Int ->
        let resolved = resolve term in
        if resolved <> term then variable resolved else Some term
      | _ -> None in
    let rec signed_variable term =
      charge ();
      match Term.Unsafe.node terms term with
      | Term.Apply (symbol, [||]) when
          (Term.Unsafe.signature terms symbol).kind = Term.Uninterpreted
          && Term.sort terms term = Term.Int ->
        let other = resolve term in
        if other <> term then signed_variable other else Some (term, 1)
      | Term.Apply (symbol, arguments) ->
        (match (Term.Unsafe.signature terms symbol).kind, arguments with
        | Term.Builtin "-", [|argument|] ->
          Option.map (fun (term, sign) -> term, -sign)
            (signed_variable argument)
        | Term.Builtin "*", _ ->
          let result = Array.fold_left (fun result argument ->
            Option.bind result (fun (subject, sign) ->
              match Term.Unsafe.node terms argument with
              | Term.Integer value when Z.equal value Z.one ->
                Some (subject, sign)
              | Term.Integer value when Z.equal value Z.minus_one ->
                Some (subject, -sign)
              | _ ->
                match subject, signed_variable argument with
                | None, Some (subject, factor) ->
                  Some (Some subject, sign * factor)
                | _ -> None)) (Some (None, 1)) arguments in
          Option.bind result (fun (subject, sign) ->
            Option.map (fun subject -> subject, sign) subject)
        | _ -> None)
      | _ -> None in
    let rec factor_variables term =
      charge ();
      match Term.Unsafe.node terms term with
      | Term.Integer _ -> []
      | Term.Apply (symbol, arguments) ->
        (match (Term.Unsafe.signature terms symbol).kind with
        | Term.Builtin ("+" | "-" | "*") ->
          Array.fold_left (fun variables argument ->
            List.sort_uniq Int.compare
              (factor_variables argument @ variables)) [] arguments
        | _ ->
          match variable term with
          | Some term ->
            (match Ids.find_opt bounds term with
            | Some (Some lo, Some hi) when Z.equal lo hi -> []
            | _ -> [term])
          | None ->
            let other = resolve term in
            if other <> term then factor_variables other else raise Skip)
      | _ -> raise Skip in
    let rec visit term =
      charge ();
      if not (Ids.mem seen term) then begin
        if Ids.length seen >= 512 then raise Skip;
        Ids.add seen term ();
        match Term.Unsafe.node terms term with
        | Term.Integer value ->
          if Z.compare value !largest_positive > 0
          then largest_positive := value
        | Term.Boolean _ -> ()
        | Term.Apply (symbol, arguments) ->
          (match (Term.Unsafe.signature terms symbol).kind with
          | Term.Uninterpreted when Array.length arguments = 0
              && Term.sort terms term = Term.Int ->
            (match variable term with
            | Some term -> Ids.replace variables term ()
            | None -> visit (resolve term))
          | Term.Uninterpreted when Array.length arguments = 0
              && Term.sort terms term = Term.Bool -> ()
          | Term.Builtin ("+" | "-" | "*" | "=" | "<" | "<="
              | ">" | ">=" | "and" | "or" | "not" | "ite" as op) ->
            Array.iter visit arguments;
            if op = "*" then begin
              Array.iter (fun argument ->
                match Term.Unsafe.node terms argument with
                | Term.Apply (symbol, _) when
                    (Term.Unsafe.signature terms symbol).kind
                      = Term.Builtin "+" -> has_affine := true
                | _ -> ()) arguments;
              let groups = Array.to_list (Array.map factor_variables arguments)
              in
              let rec pairs = function
                | [] -> ()
                | first :: rest ->
                  List.iter (fun second -> List.iter (fun a ->
                    List.iter (fun b ->
                      charge (); edges := (a, b) :: !edges) second) first) rest;
                  pairs rest in
              pairs groups
            end
          | _ -> raise Skip)
        | _ -> raise Skip
      end in
    let rec inspect term =
      charge ();
      match Term.Unsafe.node terms term with
      | Term.Apply (symbol, arguments) ->
        (match (Term.Unsafe.signature terms symbol).kind, arguments with
        | Term.Builtin "and", _ -> Array.iter inspect arguments
        | Term.Builtin ("<" | "<=" | ">" | ">=" as op), [|a; b|] ->
          let lower term value =
            let lo, hi = Option.value (Ids.find_opt bounds term)
                ~default:(None, None) in
            let lo = match lo with None -> value
              | Some lo -> if Z.compare value lo > 0 then value else lo in
            Ids.replace bounds term (Some lo, hi) in
          let upper term value =
            let lo, hi = Option.value (Ids.find_opt bounds term)
                ~default:(None, None) in
            let hi = match hi with None -> value
              | Some hi -> if Z.compare value hi < 0 then value else hi in
            Ids.replace bounds term (lo, Some hi) in
          let bound term value high strict =
            match signed_variable term with
            | None -> ()
            | Some (term, sign) ->
              let value = if not strict then value else
                Z.add ~poll:charge value (if high then Z.minus_one else Z.one) in
              let high, value = if sign < 0 then not high, Z.neg value
                else high, value in
              (if high then upper else lower) term value in
          (match Term.Unsafe.node terms a, Term.Unsafe.node terms b with
          | _, Term.Integer value ->
            bound a value (op = "<" || op = "<=") (op = "<" || op = ">")
          | Term.Integer value, _ ->
            bound b value (op = ">" || op = ">=") (op = "<" || op = ">")
          | _ -> ())
        | _ -> ())
      | _ -> () in
    try
      List.iter inspect assertions;
      List.iter visit assertions;
      if Ids.length variables <= 8 || !edges = [] then None else begin
        let rec choose count edges chosen =
          charge ();
          if edges = [] then List.rev chosen else
          if count = 0 then raise Skip else
          let degrees = Ids.create 16 in
          List.iter (fun (a, b) ->
            List.iter (fun term ->
              charge ();
              Ids.replace degrees term
                (1 + Option.value (Ids.find_opt degrees term) ~default:0))
              [a; b]) edges;
          let best = Ids.fold (fun term degree best ->
            charge ();
            match best with
            | Some (old, n) when n > degree || n = degree && old < term -> best
            | _ -> Some (term, degree)) degrees None in
          match best with
          | None -> raise Skip
          | Some (term, _) ->
            let edges = List.filter (fun (a, b) ->
              charge (); a <> term && b <> term) edges in
            choose (count - 1) edges (term :: chosen) in
        let cover = choose 2 !edges [] in
        let bounded = ref false and free = ref false in
        let value term =
          match Ids.find_opt bounds term with
          | Some (Some lo, Some hi) when Z.sign lo < 0 && Z.sign hi > 0
              && Z.compare (Z.sub ~poll:charge hi lo) (Z.of_int 2) <= 0 ->
            bounded := true; lo
          | None ->
            free := true;
            if !has_affine then
              Z.neg (Z.add ~poll:charge !largest_positive Z.one)
            else Z.add ~poll:charge !largest_positive Z.one
          | _ -> raise Skip in
        let integers = List.map (fun term -> term, value term) cover in
        let integers = Ids.fold (fun term bound integers ->
          charge ();
          match bound with
          | Some lo, Some hi when Z.equal lo hi -> (term, lo) :: integers
          | _ -> integers) bounds integers in
        if !bounded && !free then
          Some { integers; booleans = []; projections = [] }
        else None
      end
    with Skip -> None

  let sparse_relations ~poll terms assertions =
    let remaining = ref 1000000 in
    let charge () =
      poll ();
      decr remaining;
      if !remaining < 0 then raise Skip in
    let compare_ids a b = charge (); Int.compare a b in
    let union left right =
      List.sort_uniq compare_ids (List.rev_append left right) in
    let exception Nonlinear in
    let limit = max_int / 4096 in
    let safe value =
      if value <= -limit || value >= limit then raise Skip;
      value in
    let add a b = safe (a + b) in
    let multiply a b =
      if a <> 0 && b <> 0 && abs a > (limit - 1) / abs b
      then raise Skip;
      safe (a * b) in
    let variables = Ids.create 32 in
    let numbers = Ids.create 128 in
    let rec number term =
      charge ();
      match Ids.find_opt numbers term with
      | Some expression -> expression
      | None ->
        if Ids.length numbers >= 4096 then raise Skip;
        let expression = match Term.Unsafe.node terms term with
          | Term.Integer value -> Constant value
          | Term.Apply (symbol, arguments) ->
            (match (Term.Unsafe.signature terms symbol).kind, arguments with
            | Term.Uninterpreted, [||] when Term.sort terms term = Term.Int ->
              Ids.replace variables term ();
              Variable term
            | Term.Builtin "+", _ -> Add (Array.map number arguments)
            | Term.Builtin "-", _ -> Subtract (Array.map number arguments)
            | Term.Builtin "*", _ -> Multiply (Array.map number arguments)
            | _ -> raise Skip)
          | _ -> raise Skip in
        Ids.add numbers term expression;
        expression in
    let values = Ids.create 32 and domains = Ids.create 32 in
    let bounds = Ids.create 32 and relations = ref [] in
    let relation_count = ref 0 in
    let add_relation branches =
      charge ();
      incr relation_count;
      if !relation_count > 32 then raise Skip;
      relations := branches :: !relations in
    let constant expression = match Term.Unsafe.node terms expression with
      | Term.Integer value ->
        Option.map safe (Z.to_int_opt value)
      | _ -> None in
    let variable term = match Term.Unsafe.node terms term with
      | Term.Apply (symbol, [||]) when
          (Term.Unsafe.signature terms symbol).kind = Term.Uninterpreted
          && Term.sort terms term = Term.Int -> Some term
      | _ -> None in
    let equality term = match Term.Unsafe.node terms term with
      | Term.Apply (symbol, [|left; right|]) when
          (Term.Unsafe.signature terms symbol).kind = Term.Builtin "="
          && Term.sort terms left = Term.Int -> Some (left, right)
      | _ -> None in
    let binding term =
      charge ();
      match equality term with
      | None -> None
      | Some (left, right) ->
        (match variable left, constant right with
        | Some term, Some value -> Some (term, value)
        | _ -> Option.bind (variable right) (fun term ->
            Option.map (fun value -> term, value) (constant left))) in
    let rec collect term =
      charge ();
      match Term.Unsafe.node terms term with
      | Term.Boolean true -> ()
      | Term.Apply (symbol, arguments) ->
        (match (Term.Unsafe.signature terms symbol).kind, arguments with
        | Term.Builtin "and", _ -> Array.iter collect arguments
        | Term.Builtin "or", _ ->
          let bindings = Array.map binding arguments in
          let domain = match bindings with
            | [||] -> None
            | _ ->
              (match bindings.(0) with
              | None -> None
              | Some (subject, _) ->
                if Array.length bindings <= 16
                    && Array.for_all (fun binding ->
                      charge ();
                      match binding with
                      | Some (term, _) -> term = subject | None -> false)
                        bindings then Some subject else None) in
          (match domain with
          | Some subject ->
            let alternatives = Array.map (fun binding ->
              charge ();
              match binding with
              | Some (_, value) -> value | None -> raise Skip) bindings in
            let alternatives = Array.of_list
              (List.sort_uniq compare_ids (Array.to_list alternatives)) in
            let alternatives = match Ids.find_opt domains subject with
              | None -> alternatives
              | Some previous -> Array.of_list (List.filter (fun value ->
                  charge (); Array.mem value previous)
                    (Array.to_list alternatives)) in
            Ids.replace domains subject alternatives
          | None ->
            let alternatives = Array.map (fun term ->
              charge ();
              match equality term with
              | Some (left, right) -> number left, number right
              | None -> raise Skip) arguments in
            add_relation alternatives)
        | Term.Builtin "=", _ ->
          (match binding term with
          | Some (subject, value) -> Ids.replace values subject value
          | None ->
            (match equality term with
            | Some (left, right) ->
              add_relation [|number left, number right|]
            | None -> raise Skip))
        | Term.Builtin ("<" | "<=" | ">" | ">=" as name),
            [|left; right|] ->
          let update subject value upper strict =
            let value = if strict then value + (if upper then -1 else 1)
              else value in
            let lo, hi = Option.value (Ids.find_opt bounds subject)
                ~default:(-limit + 1, limit - 1) in
            Ids.replace bounds subject
              (if upper then lo, min hi value else max lo value, hi) in
          (match variable left, constant right with
          | Some subject, Some value ->
            update subject value (name = "<" || name = "<=")
              (name = "<" || name = ">")
          | _ ->
            (match constant left, variable right with
            | Some value, Some subject ->
              update subject value (name = ">" || name = ">=")
                (name = "<" || name = ">")
            | _ -> raise Skip))
        | _ -> raise Skip)
      | _ -> raise Skip in
    let rec dependencies = function
      | Constant _ -> charge (); []
      | Variable term -> charge (); [term]
      | Add arguments | Subtract arguments | Multiply arguments ->
        charge ();
        Array.fold_left (fun result argument ->
          charge (); union (dependencies argument) result)
          [] arguments
      | Divide _ | Modulo _ | Choose _ -> raise Skip in
    let valid term value =
      charge ();
      (match Ids.find_opt domains term with
      | None -> true | Some alternatives -> Array.mem value alternatives)
      && (match Ids.find_opt bounds term with
          | None -> true | Some (lo, hi) -> lo <= value && value <= hi) in
    let rec affine unknown = function
      | Constant value ->
        charge ();
        0, (match Z.to_int_opt value with Some value -> safe value
            | None -> raise Skip)
      | Variable term ->
        charge ();
        if Some term = unknown then 1, 0 else
        (match Ids.find_opt values term with
        | Some value -> 0, value | None -> raise Nonlinear)
      | Add arguments ->
        Array.fold_left (fun (a, b) argument ->
          charge ();
          let c, d = affine unknown argument in add a c, add b d)
          (0, 0) arguments
      | Subtract arguments ->
        if Array.length arguments = 0 then raise Skip;
        if Array.length arguments = 1 then
          let a, b = affine unknown arguments.(0) in -a, -b
        else begin
          let a, b = affine unknown arguments.(0) in
          let result = ref (a, b) in
          for index = 1 to Array.length arguments - 1 do
            charge ();
            let a, b = !result and c, d = affine unknown arguments.(index) in
            result := add a (-c), add b (-d)
          done;
          !result
        end
      | Multiply arguments ->
        Array.fold_left (fun (a, b) argument ->
          charge ();
          let c, d = affine unknown argument in
          if a <> 0 && c <> 0 then raise Nonlinear;
          add (multiply a d) (multiply b c), multiply b d)
          (0, 1) arguments
      | Divide _ | Modulo _ | Choose _ -> raise Skip in
    let equation unknown (left, right) =
      let a, b = affine unknown left and c, d = affine unknown right in
      add a (-c), add b (-d) in
    let assign term value accept =
      if not (valid term value) then false else
      match Ids.find_opt values term with
      | Some previous -> previous = value && accept ()
      | None ->
        Ids.add values term value;
        let result = accept () in
        if not result then Ids.remove values term;
        result in
    let rec enumerate variables accept = match variables with
      | [] -> charge (); accept ()
      | term :: rest ->
        (match Ids.find_opt values term with
        | Some _ -> enumerate rest accept
        | None ->
          let domain = Ids.find domains term in
          Array.exists (fun value ->
            assign term value (fun () -> enumerate rest accept)) domain) in
    try
      List.iter collect assertions;
      if Ids.length domains < 2 || !relations = [] then None else
      let relations = Array.of_list (List.rev !relations) in
      let dependencies = Array.map (fun branches ->
        charge ();
        Array.map (fun (a, b) ->
          charge (); union (dependencies a) (dependencies b)) branches)
          relations in
      let occurrence = Ids.create 32 in
      Array.iter (fun branches ->
        charge ();
        let members = Array.fold_left (fun result branch ->
          charge (); union branch result) [] branches in
        List.iter (fun term -> charge (); Ids.replace occurrence term
          (1 + Option.value (Ids.find_opt occurrence term) ~default:0))
          members) dependencies;
      let direct = Array.map (fun branches ->
        charge ();
        Array.for_all (fun equation ->
          charge ();
          match equation with
          | Variable _, Constant _ | Constant _, Variable _ -> true
          | _ -> false) branches) relations in
      let tables = Array.make (Array.length relations) None in
      let tables_ready = ref false in
      let prepare_tables () =
      Array.fill tables 0 (Array.length tables) None;
      Array.iteri (fun index branches ->
        charge ();
        let members = Array.fold_left (fun result branch ->
          charge (); union branch result) [] dependencies.(index)
        in
        let regular = List.filter (fun term -> charge ();
            not (Ids.mem domains term)
            && not (Ids.mem values term)) members in
        match regular with
        | [subject] when List.for_all (fun term -> charge (); term = subject
            || not (Ids.mem domains term)
            || Ids.find occurrence term = 1) members ->
          let table = Hashtbl.create 1024 and wildcard = ref None in
          Array.iteri (fun branch equation_ ->
            charge ();
            let finite = List.filter (fun term -> charge (); term <> subject
                && Ids.mem domains term) dependencies.(index).(branch) in
            if List.length finite > 2 then raise Skip;
            ignore (enumerate finite (fun () ->
              try
                let a, b = equation (Some subject) equation_ in
                if a = 0 && b = 0 && !wildcard = None then
                  wildcard := Some (List.map (fun term ->
                    charge (); term, Ids.find values term) finite);
                if a <> 0 && b mod a = 0 then begin
                  let value = -b / a in
                  if valid subject value && not (Hashtbl.mem table value)
                  then Hashtbl.add table value (List.map (fun term ->
                    charge (); term, Ids.find values term) finite)
                end;
                false
              with Nonlinear -> false))) branches;
          tables.(index) <- Some (subject, table, !wildcard)
        | _ -> ()) relations;
      tables_ready := true in
      let available term value =
        valid term value && Array.for_all (fun table ->
          charge ();
          match table with
          | Some (subject, table, wildcard) when term = subject ->
            wildcard <> None || Hashtbl.mem table value
          | _ -> true) tables in
      let completed = Array.make (Array.length relations) false in
      let rec search count =
        charge ();
        if count = 0 then true else begin
        if not !tables_ready && Array.for_all (fun completed ->
            charge (); completed)
            (Array.mapi (fun index is_direct ->
              charge (); not is_direct || completed.(index)) direct)
        then prepare_tables ();
        let choice = ref None in
        Array.iteri (fun index branches ->
          charge ();
          if not completed.(index) then begin
            let score = ref max_int in
            Array.iter (fun members ->
              charge ();
              let regular = ref 0 and finite = ref 0 in
              List.iter (fun term ->
                charge ();
                if not (Ids.mem values term) then
                  if Ids.mem domains term then incr finite else incr regular)
                  members;
              if !regular <= 1 && !finite <= 2 then
                score := min !score
                  (if direct.(index) then -100 + !regular
                   else 10 * !regular + !finite)) branches;
            match !choice with
            | Some (_, previous) when previous <= !score -> ()
            | _ when !score < max_int -> choice := Some (index, !score)
            | _ -> ()
          end) dependencies;
        match !choice with
        | None -> false
        | Some (index, _) ->
          let accept () =
            completed.(index) <- true;
            let result = search (count - 1) in
            if not result then begin
              completed.(index) <- false;
              if direct.(index) then tables_ready := false
            end;
            result in
          match tables.(index) with
          | Some (subject, table, wildcard) ->
            let witness value bindings =
              assign subject value (fun () ->
                let rec bind = function
                  | [] -> accept ()
                  | (term, value) :: rest ->
                    assign term value (fun () -> bind rest) in
                bind bindings) in
            (match Ids.find_opt values subject with
            | Some value ->
              (match Hashtbl.find_opt table value with
              | Some bindings -> witness value bindings
              | None -> Option.fold ~none:false
                  ~some:(witness value) wildcard)
            | None ->
              let entries = Hashtbl.fold (fun value bindings entries ->
                charge (); (value, bindings) :: entries) table []
                  |> List.sort (fun (a, _) (b, _) -> compare_ids a b) in
              List.exists (fun (value, bindings) ->
                charge (); witness value bindings)
                entries)
          | None ->
            let rec branch position =
              charge ();
              if position = Array.length relations.(index) then false else
              let members = List.filter (fun term -> charge ();
                  not (Ids.mem values term))
                  dependencies.(index).(position) in
              let regular = List.filter (fun term -> charge ();
                  not (Ids.mem domains term))
                  members in
              let finite = List.filter (fun term ->
                  charge (); Ids.mem domains term) members in
              let found = if List.length regular > 1 || List.length finite > 2
                then false else
                enumerate finite (fun () ->
                  let unknown = match regular with
                    | [] -> None | [term] -> Some term | _ -> raise Skip in
                  try
                    let a, b = equation unknown relations.(index).(position) in
                    match unknown with
                    | None -> b = 0 && accept ()
                    | Some term when a <> 0 && b mod a = 0 ->
                      let value = -b / a in
                      available term value
                      && assign term value accept
                    | Some _ -> false
                  with Nonlinear -> false) in
              found || branch (position + 1) in
            branch 0 end in
      if not (search (Array.length relations)) then None else
      let integers = Ids.fold (fun term () bindings ->
        charge ();
        match Ids.find_opt values term with
        | Some value -> (term, Z.of_int value) :: bindings
        | None -> raise Skip) variables [] in
      Some { integers; booleans = []; projections = [] }
    with Skip -> None

  let find ~prefer_factor ~defer ~poll ~resolve terms assertions =
    let remaining = ref 30000 in
    let charge () =
      if !remaining = 0 then raise Skip;
      decr remaining;
      poll () in
    let variables = Ids.create 8 and propositions = Ids.create 8 in
    let number_cache = Ids.create 32 and formula_cache = Ids.create 32 in
    let nonlinear = ref false in
    let nodes = ref 0 in
    let new_node () =
      if !nodes = 256 then raise Skip;
      incr nodes in
    let rec number term =
      charge ();
      match Ids.find_opt number_cache term with
      | Some expression -> expression
      | None ->
        new_node ();
        let expression = match Term.Unsafe.node terms term with
          | Term.Integer value -> Constant value
          | Term.Apply (symbol, args) ->
            let signature = Term.Unsafe.signature terms symbol in
            (match signature.kind, args with
            | Term.Uninterpreted, [||] when signature.result = Term.Int ->
              let resolved = resolve term in
              if resolved <> term then number resolved else
              let index = match Ids.find_opt variables term with
                | Some index -> index
                | None ->
                  let index = Ids.length variables in
                  if index = 16 then raise Skip;
                  Ids.add variables term index;
                  index in
              Variable index
            | Term.Builtin "+", _ -> Add (Array.map number args)
            | Term.Builtin "-", _ -> Subtract (Array.map number args)
            | Term.Builtin "*", _ ->
              let arguments = Array.map number args in
              let dependent = Array.fold_left (fun n -> function
                | Constant _ -> n | _ -> n + 1) 0 arguments in
              if dependent >= 2 then nonlinear := true;
              Multiply arguments
            | Term.Builtin ("div" | "mod" as operator), [|left; right|] ->
              (match number right with
              | Constant divisor when Z.is_zero divisor -> raise Skip
              | divisor ->
                (match divisor with Constant _ -> ()
                | _ -> nonlinear := true);
                let argument = number left in
                if operator = "div" then Divide (argument, divisor)
                else Modulo (argument, divisor))
            | Term.Builtin "ite", [|condition; left; right|] ->
              Choose (formula condition, number left, number right)
            | _ -> raise Skip)
          | _ -> raise Skip in
        Ids.add number_cache term expression;
        expression
    and formula term =
      charge ();
      match Ids.find_opt formula_cache term with
      | Some expression -> expression
      | None ->
        new_node ();
        let expression = match Term.Unsafe.node terms term with
          | Term.Boolean value -> Boolean value
          | Term.Apply (symbol, args) ->
            (match (Term.Unsafe.signature terms symbol).kind, args with
            | Term.Uninterpreted, [||]
                when Term.sort terms term = Term.Bool ->
              let index = match Ids.find_opt propositions term with
                | Some index -> index
                | None ->
                  let index = Ids.length propositions in
                  if index = 8 then raise Skip;
                  Ids.add propositions term index;
                  index in
              Proposition index
            | Term.Builtin "not", [|arg|] -> Not (formula arg)
            | Term.Builtin "and", _ -> All (Array.map formula args)
            | Term.Builtin "or", _ -> Any (Array.map formula args)
            | Term.Builtin "ite", [|condition; left; right|] ->
              Select (formula condition, formula left, formula right)
            | Term.Builtin "=", [|left; right|]
              when Term.sort terms left = Term.Bool ->
              Equivalent (formula left, formula right)
            | Term.Builtin ("=" | "<" | "<=" | ">" | ">=" as op),
                [|left; right|] ->
              Compare (op, number left, number right)
            | _ -> raise Skip)
          | _ -> raise Skip in
        Ids.add formula_cache term expression;
        expression in
    let exception Zero_divisor in
    try
      let constraints = List.map formula assertions in
      if not !nonlinear then None else
      let () = if Ids.length variables > 8 then prefer_factor () in
      let values = Array.make (Ids.length variables) Z.zero in
      let boolean_values = Array.make (Ids.length propositions) false in
      let rec evaluate = function
        | Constant value -> charge (); value
        | Variable index -> charge (); values.(index)
        | Add arguments ->
          charge ();
          Array.fold_left (fun sum term ->
            Z.add ~poll:charge sum (evaluate term)) Z.zero arguments
        | Multiply arguments ->
          charge ();
          Array.fold_left (fun product term ->
            Z.mul ~poll:charge product (evaluate term)) Z.one arguments
        | Subtract arguments ->
          charge ();
          if Array.length arguments = 0 then raise Skip
          else if Array.length arguments = 1
          then Z.neg (evaluate arguments.(0))
          else
            let result = ref (evaluate arguments.(0)) in
            for index = 1 to Array.length arguments - 1 do
              result := Z.sub ~poll:charge !result (evaluate arguments.(index))
            done;
            !result
        | Divide (argument, divisor) ->
          charge ();
          let divisor = evaluate divisor in
          if Z.is_zero divisor then raise Zero_divisor;
          fst (Z.div_rem ~poll:charge (evaluate argument) divisor)
        | Modulo (argument, divisor) ->
          charge ();
          let divisor = evaluate divisor in
          if Z.is_zero divisor then raise Zero_divisor;
          snd (Z.div_rem ~poll:charge (evaluate argument) divisor)
        | Choose (condition, left, right) ->
          charge (); evaluate (if satisfies condition then left else right)
      and satisfies = function
        | Boolean value -> charge (); value
        | Proposition index -> charge (); boolean_values.(index)
        | Compare (operator, left, right) ->
          charge ();
          let order = Z.compare (evaluate left) (evaluate right) in
          (match operator with
          | "=" -> order = 0 | "<" -> order < 0 | "<=" -> order <= 0
          | ">" -> order > 0 | _ -> order >= 0)
        | Not condition -> charge (); not (satisfies condition)
        | All arguments -> charge (); Array.for_all satisfies arguments
        | Any arguments -> charge (); Array.exists satisfies arguments
        | Equivalent (left, right) ->
          charge (); satisfies left = satisfies right
        | Select (condition, left, right) ->
          charge (); satisfies (if satisfies condition then left else right) in
      let rec enumerate_booleans accept index =
        if index = Array.length boolean_values then accept ()
        else begin
          charge ();
          boolean_values.(index) <- false;
          enumerate_booleans accept (index + 1)
          || (boolean_values.(index) <- true;
              enumerate_booleans accept (index + 1))
        end in
      let satisfied () =
        enumerate_booleans (fun () ->
          try List.for_all satisfies constraints with Zero_divisor -> false)
          0 in
      let rec enumerate domain index =
        charge ();
        if index = Array.length values then satisfied ()
        else Array.exists (fun value ->
          values.(index) <- value;
          enumerate domain (index + 1)) domain in
      let domains = [|[|0; 1|]; [|0; 1; -1|]; [|0; 1; -1; 2; -2|];
                      [|0; 1; -1; 2; -2; 3; -3; 4; -4|]|] in
      let rec near_ones changes start =
        if changes = 0 then satisfied () else
        let rec choose index =
          if index = Array.length values then false else begin
            charge ();
            values.(index) <- Z.zero;
            if near_ones (changes - 1) (index + 1) then true else begin
              values.(index) <- Z.one;
              choose (index + 1)
            end
          end in
        choose start in
      let rec near_zero index =
        charge ();
        if index = Array.length values then false else begin
          values.(index) <- Z.one;
          if satisfied () then true else begin
            values.(index) <- Z.zero;
            near_zero (index + 1)
          end
        end in
      let found = try
          satisfied ()
          || (Array.length values > 8 && near_zero 0)
          || (Array.fill values 0 (Array.length values) Z.one;
              satisfied () || near_ones 1 0 || near_ones 2 0)
          || (Array.length values = 1 &&
              Ids.fold (fun _ expression found ->
                charge ();
                found || match expression with
                  | Constant value -> values.(0) <- value; satisfied ()
                  | _ -> false) number_cache false)
          || Array.exists (fun domain ->
               enumerate (Array.map Z.of_int domain) 0) domains
        with Skip -> false in
      let assignments () =
        let integers = Ids.fold (fun term index bindings ->
          charge (); (term, values.(index)) :: bindings) variables [] in
        let booleans = Ids.fold (fun term index bindings ->
          charge (); (term, boolean_values.(index)) :: bindings)
          propositions [] in
        { integers; booleans; projections = [] } in
      if found then Some (assignments ()) else
      if Array.length values > 8 then None else begin
        ignore (bitvector_width ~charge:poll 3 constraints);
        defer (fun () ->
          remaining := 50000;
          let found =
            try enumerate_booleans (fun () ->
              separable_model ~charge values evaluate satisfies
                (fun () -> List.for_all satisfies constraints) constraints) 0
            with Skip -> false in
          let found = found || begin
              remaining := 500000;
              try enumerate_booleans (fun () ->
                cubic_model ~charge values
                  (fun () -> try List.for_all satisfies constraints
                    with Zero_divisor -> false) constraints) 0
              with Skip -> false
            end in
          remaining := 30000;
          try if found then Some (assignments ()) else
            match bitvector_model ~poll (Array.length values)
                (Array.length boolean_values) constraints with
            | None -> None
            | Some (proposed, proposed_booleans) ->
              Array.blit proposed 0 values 0 (Array.length values);
              Array.blit proposed_booleans 0 boolean_values 0
                (Array.length boolean_values);
              if List.for_all satisfies constraints
              then Some (assignments ()) else None
          with Skip -> None);
        None
      end
    with Skip -> None
end

let finite_domain_symmetry t assertions =
  let exception Limit in
  let remaining = ref 1000000 in
  let charge () =
    poll t;
    decr remaining;
    if !remaining < 0 then raise Limit in
  let constant term = match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, [||]) ->
      (Term.Unsafe.signature t.terms symbol).kind = Term.Uninterpreted
      && (match Term.sort t.terms term with
          | Term.Named sort -> Term.constructors t.terms sort = []
          | _ -> false)
    | _ -> false in
  let memberships = ref [] and flat = ref [] in
  let rec collect term =
    charge ();
    match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, arguments)
        when (Term.Unsafe.signature t.terms symbol).kind
          = Term.Builtin "and" -> Array.iter collect arguments
    | _ ->
      flat := term :: !flat;
      (match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, arguments)
        when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "or"
          && Array.length arguments >= 2 && Array.length arguments <= 16 ->
        let common = ref None and pairs = ref [] in
        Array.iter (fun atom ->
          charge ();
          let values = match Term.Unsafe.node t.terms atom with
            | Term.Apply (head, [|left; right|])
                when (Term.Unsafe.signature t.terms head).kind
                    = Term.Builtin "=" ->
              pairs := (left, right) :: !pairs;
              Required.add left (Required.singleton right)
            | _ -> Required.empty in
          common := Some (match !common with
            | None -> values | Some prior -> Required.inter prior values))
          arguments;
        (match !common with
        | Some common when Required.cardinal common = 1 ->
          let subject = Required.choose common in
          let values = List.fold_left (fun values (left, right) ->
            Required.add (if left = subject then right else left) values)
            Required.empty !pairs in
          if not (Required.mem subject values)
             && Required.for_all constant values then
            memberships := (subject, values) :: !memberships
        | _ -> ())
      | _ -> ()) in
  try
    Ids.iter (fun term () ->
      charge ();
      match Term.sort t.terms term with
      | Term.Bool -> ()
      | Term.Named sort when Term.constructors t.terms sort = [] -> ()
      | _ -> raise Limit) t.declared_terms;
    List.iter collect assertions;
    if !memberships = [] then [] else begin
      let normalize first second =
        let memo = Ids.create 128 in
        let rec term input =
          charge ();
          if input = first then second else if input = second then first else
          match Ids.find_opt memo input with
          | Some output -> output
          | None ->
            (match Term.sort t.terms input with
            | Term.Bool -> ()
            | Term.Named sort when Term.constructors t.terms sort = [] -> ()
            | _ -> raise Limit);
            let output = match Term.Unsafe.node t.terms input with
            | Term.Apply (symbol, arguments) ->
              let arguments = Array.map term arguments in
              (match (Term.Unsafe.signature t.terms symbol).kind with
              | Term.Builtin ("and" | "or" | "=" | "distinct") ->
                Array.sort (fun x y -> charge (); Int.compare x y) arguments
              | _ -> ());
              Term.apply t.terms symbol arguments
            | _ -> input in
            Ids.add memo input output;
            output in
        List.fold_left (fun set input ->
          Required.add (term input) set) Required.empty !flat in
      let reference = normalize (-1) (-1) in
      let checked = Hashtbl.create 8 in
      let rec choose = function
        | [] -> []
        | (_, values) :: rest ->
          let values_list = Required.elements values in
          if Hashtbl.mem checked values_list then choose rest else begin
            Hashtbl.add checked values_list ();
            let first = Required.choose values in
            if not (Required.for_all (fun other ->
                charge ();
                other = first || Required.equal reference
                  (normalize first other)) values) then choose rest else
            let subjects = List.fold_left (fun subjects (subject, other) ->
              if Required.equal values other then Required.add subject subjects
              else subjects) Required.empty !memberships in
            let allowed = ref Required.empty and rest = ref values_list in
            let remaining_subjects = ref subjects in
            let rec uses_fixed_representatives term =
              charge ();
              if Required.mem term values then Required.mem term !allowed
              else match Term.Unsafe.node t.terms term with
                | Term.Apply (_, arguments) ->
                  Array.for_all uses_fixed_representatives arguments
                | _ -> true in
            let rec constrain constraints =
              charge ();
              match !rest with
              | [] | [_] -> constraints
              | value :: tail ->
                let subject = Required.fold (fun subject found ->
                  charge ();
                  if not (uses_fixed_representatives subject) then found
                  else
                    let priority term = match Term.Unsafe.node t.terms term with
                      | Term.Apply (_, args) ->
                        let n = Array.length args in
                        if n = 0 then 0 else 4 - n
                      | _ -> 0 in
                    match found with
                    | Some previous when priority previous >= priority subject -> found
                    | _ -> Some subject)
                  !remaining_subjects None in
                allowed := Required.add value !allowed;
                rest := tail;
                (match subject with
                | None -> constrain constraints
                | Some subject ->
                  remaining_subjects := Required.remove subject
                      !remaining_subjects;
                  let choices = Required.fold (fun value choices ->
                    charge ();
                    Term.equal t.terms subject value :: choices)
                    !allowed [] in
                  constrain (Term.or_ t.terms (Array.of_list choices)
                    :: constraints)) in
            constrain []
          end in
      choose !memberships
    end
  with Limit -> []

let check_once ?(assumptions = []) ?(budget = max_int)
    ?(search_budget = max_int) ?(comparison_after = 200000)
    ?(search_limit = fun () -> max_int) ?(prepare = fun () -> ())
    ?(cancelled = fun () -> false) t =
  t.model <- None; t.cancelled <- cancelled; t.cuts <- 0; t.splits_since_cut <- 0;
  t.model_attempts <- 4;
  t.unknown_reason <- "incomplete";
  let stats = statistics t in
  t.base_sat_work <- stats.propagations + stats.conflicts;
  t.remaining <- budget; t.budget <- budget;
  t.search_remaining <- search_budget;
  t.arithmetic_work <- 0;
  t.operation_active <- true;
  Fun.protect ~finally:(fun () -> t.operation_active <- false) (fun () ->
    let symmetry = ref [] in
    let assumption_literals = encoding t (fun () ->
      prepare ();
      flush_assertions t;
      symmetry := (match t.symmetry_cache with
        | Some (assertions, previous, constraints)
          when assertions == t.replay_assertions && previous == assumptions ->
          constraints
        | _ ->
          let constraints =
            if Ids.length t.linear <> 0 || Ids.length t.bitvector_terms <> 0
            then [] else finite_domain_symmetry t
              (List.rev_append assumptions t.replay_assertions) in
          t.symmetry_cache <- Some (t.replay_assertions, assumptions, constraints);
          constraints);
      List.map (fun original ->
        poll t;
        if Term.sort t.terms original <> Term.Bool
        then invalid_arg "SMT Boolean assumption required";
        let term = rewrite t original in
        mark_used t term;
        original, required_positive t term)
        (List.rev_append !symmetry assumptions)) in
    let search_budget = min search_budget (search_limit ()) in
    t.search_remaining <- search_budget;
    let literals = Array.of_list (List.map snd assumption_literals) in
    let attempted = ref t.comparison_congruence in
    let should_stop () =
      poll_resource t;
      let work = sat_work t in
      let search_work = search_budget - t.search_remaining + work in
      if not !attempted && search_work >= comparison_after then begin
        attempted := true;
        if comparison_after = 0
           || t.arithmetic_work >= search_work / 2 then begin
          try
            let comparisons = Ids.fold (fun term literal bindings ->
              poll t;
              match Term.Unsafe.node t.terms term with
              | Term.Apply (symbol, _) ->
                (match (Term.Unsafe.signature t.terms symbol).kind with
                | Term.Builtin ("<" | "<=" | ">" | ">=") ->
                  (term, literal) :: bindings
                | _ -> bindings)
              | _ -> bindings) t.booleans [] in
            List.iter (fun (term, literal) ->
              poll t;
              Eq.register_boolean (equality t) ~literal term)
              (List.sort (fun (a, _) (b, _) ->
                poll t;
                Int.compare a b) comparisons);
            t.comparison_congruence <- true
          with Encoding_stopped _ ->
            (* Partial registrations survive reset and can finish on retry. *)
            raise Theory_stopped
        end
      end;
      if t.budget - t.remaining + work >= budget
         || t.search_remaining <= work then begin
        t.unknown_reason <- "resource limit"; true
      end else cancelled () || t.incomplete
    in
    let unknown () =
      Unknown (if cancelled () then "cancelled" else t.unknown_reason) in
    if t.theory_required || Ids.length t.unsupported > 0 then begin
      t.theory_required <- true;
      List.iter (fun term ->
        match Term.Unsafe.node t.terms term with
        | Term.Apply (_, args) -> bitvector_ite t term args
        | _ -> invalid_arg "SMT bitvector ITE invariant")
        (reverse t t.pending_bitvector_ites);
      t.pending_bitvector_ites <- [];
      Option.iter (fun extension ->
        Sat.attach_extension (sat t) extension;
        t.theory_extension <- None) t.theory_extension
    end;
    match (let answer = Sat.solve ~assumptions:literals
        ~max_conflicts:search_budget ~should_stop (sat t) in
      poll_resource ~work:0 t;
      Option.iter Resource.check t.resource;
      answer) with
    | Sat.Sat model -> t.model <- Some model; Sat
    | Sat.Unsat core ->
      if !symmetry <> [] then Unsat assumptions else
      (try Unsat (List.filter_map (fun (term, literal) ->
        poll_resource t;
        if Array.exists (fun candidate ->
          poll_resource t; literal = candidate) core
        then Some term else None) assumption_literals)
      with Resource.Exhausted reason -> Unknown reason)
    | Sat.Unknown -> unknown ()
    | exception Theory_stopped -> unknown ()
    | exception Resource.Exhausted reason -> Unknown reason)

let check ?(assumptions = []) ?(budget = max_int)
    ?(reorder_after = 8000000) ?(comparison_after = 200000)
    ?(cancelled = fun () -> false)
    ?(prepare = fun () -> ()) t =
  if budget < 0 then invalid_arg "Vox_smt_engine.check: budget";
  if reorder_after < 0 then invalid_arg "Vox_smt_engine.check: reorder_after";
  if comparison_after < 0
  then invalid_arg "Vox_smt_engine.check: comparison_after";
  t.model <- None; t.result_solver <- None; t.counted_work <- Some 0;
  let stopped = ref false in
  let cancelled () =
    if not !stopped then stopped := cancelled ();
    !stopped in
  if cancelled () then Unknown "cancelled"
  else if budget = 0 then Unknown "resource limit"
  else
  let spent = ref 0 in
  let run ?(prepare = fun () -> ()) ?(search_limit = fun () -> max_int)
      ?(operation_budget = max_int)
      solver allowance =
    Fun.protect ~finally:(fun () ->
      let count = resource_count solver in
      spent := if count > max_int - !spent then max_int else !spent + count;
      t.counted_work <- Some !spent)
      (fun () -> with_resource solver t.resource (fun () ->
        let result = check_once ~assumptions
          ~budget:(min operation_budget (budget - !spent))
          ~search_budget:allowance ~comparison_after ~cancelled ~prepare
          ~search_limit solver in
        Option.iter Resource.check solver.resource;
        result)) in
  let exception Ordering_probe in
  let probe_attempted = ref false in
  let deferred = ref None in
  let proposal_budget = ref max_int in
  let prepare () =
    prepare ();
    if comparison_after <> 0 && t.remaining >= 1024 then
      let assertions = List.rev_append assumptions t.replay_assertions in
      let projections = match t.projection_cache with
        | Some (previous, projections)
            when assumptions = [] && previous == t.replay_assertions ->
          projections
        | _ ->
          let projections = match Small_model.finite_coloring
              ~poll:(fun () -> poll t) t.terms assertions with
            | Some _ as proposal -> proposal
            | None -> Small_model.projection ~poll:(fun () -> poll t)
                t.terms assertions in
          if assumptions = [] then
            t.projection_cache <- Some (t.replay_assertions, projections);
          projections in
      let proposal = match projections with
        | Some projections -> Some { Small_model.integers = [];
            booleans = []; projections }
        | None ->
          match Small_model.bitvector_boundary ~poll:(fun () -> poll t)
              ~resolve:(rewrite t)
              t.terms assertions with
          | Some _ as proposal -> proposal
          | None ->
            let factor = lazy (Small_model.factor_boundary
              ~poll:(fun () -> poll t) ~resolve:(rewrite t)
              t.terms assertions) in
            let prefer_factor () =
              match Lazy.force factor with
              | None -> ()
              | Some assignments ->
                proposal_budget := 10000;
                raise (Small_model.Found assignments) in
            (match Small_model.find ~prefer_factor
              ~defer:(fun proposal -> deferred := Some proposal)
              ~poll:(fun () -> poll t) ~resolve:(rewrite t)
              t.terms assertions with
            | Some _ as proposal -> proposal
            | None ->
              (match Small_model.sparse_relations ~poll:(fun () -> poll t)
                  t.terms assertions with
              | Some _ as proposal -> proposal
              | None ->
                proposal_budget := 10000;
                Lazy.force factor)) in
      match proposal with
      | None ->
        if not !probe_attempted && reorder_after <> 0 then begin
          probe_attempted := true;
          if Term.count t.terms <= 4096 then begin
          let exception Skip_probe in
          let remaining = ref 1000 and products = ref false in
          let rec inspect term =
            poll t;
            decr remaining;
            if !remaining < 0 then raise Skip_probe;
            match Term.Unsafe.node t.terms term with
            | Term.Apply (symbol, arguments) ->
              (match (Term.Unsafe.signature t.terms symbol).kind with
              | Term.Builtin "*" ->
                if Array.fold_left (fun count term ->
                    poll t;
                    match Term.Unsafe.node t.terms term with
                    | Term.Integer _ -> count | _ -> count + 1) 0 arguments > 1
                then products := true
              | Term.Builtin ("+" | "-" | "=" | "<" | "<=" | ">" | ">="
                  | "and" | "or" | "not" | "=>" | "xor" | "distinct"
                  | "ite") | Term.Uninterpreted -> ()
              | _ -> raise Skip_probe);
              Array.iter inspect arguments
            | Term.Bits _ -> raise Skip_probe
            | Term.Boolean _ | Term.Integer _ -> () in
          if (try List.iter inspect assertions; !products
              with Skip_probe -> false) then raise Ordering_probe
          end
        end
      | Some assignments -> raise (Small_model.Found assignments) in
  let first = min budget reorder_after in
  let ordering_probe () =
    let attempt keep =
      let solver = create t.terms in
      NL.enable_ordering (nonlinear solver);
      let prepare () =
        Ids.iter (fun term () -> poll solver; register solver term)
          t.declared_terms;
        List.iter (fun term -> poll solver; assert_ solver term)
          (reverse solver t.replay_assertions) in
      let answer = Fun.protect ~finally:(fun () ->
        t.retired_statistics <- add_statistics t.retired_statistics
          (statistics solver)) (fun () ->
        try run ~prepare ~operation_budget:50000 solver 50000
        with Encoding_stopped reason -> Unknown reason) in
      if keep then (match answer with
        | Sat | Unsat _ -> t.result_solver <- Some solver
        | Unknown _ -> ());
      answer in
    match Term.with_temporary t.terms (fun () -> attempt false) with
    | Sat -> attempt true
    | Unsat _ as answer -> answer
    | Unknown _ -> Unknown "ordering probe" in
  let propose ?search_limit prepare allowance =
    proposal_budget := max_int;
    match run ~prepare ?search_limit t allowance with
    | answer -> answer
    | exception Ordering_probe ->
      (match ordering_probe () with
      | (Sat | Unsat _) as answer -> answer
      | Unknown _ when not !stopped && !spent < budget ->
        run ~prepare ?search_limit t allowance
      | Unknown _ as answer -> answer)
    | exception Small_model.Found assignments ->
      let attempt keep =
      let solver = create t.terms in
      let prepare () =
        List.iter (fun (term, argument) ->
          poll solver;
          solver.substitutions <- IM.add term argument solver.substitutions;
          invalidate_substitution solver term argument)
          assignments.Small_model.projections;
        List.iter (fun (variable, value) ->
          poll solver;
          assert_ solver
            (Term.equal t.terms variable (Term.integer t.terms value)))
          assignments.Small_model.integers;
        List.iter (fun (variable, value) ->
          poll solver;
          assert_ solver
            (Term.equal t.terms variable (Term.boolean t.terms value)))
          assignments.Small_model.booleans;
        Ids.iter (fun term () -> poll solver; register solver term)
          t.declared_terms;
        List.iter (fun term -> poll solver; assert_ solver term)
          (reverse solver t.replay_assertions);
        List.iter (fun (_, argument) ->
          poll solver;
          let argument = rewrite solver argument in
          mark_used solver argument;
          internalize solver argument)
          assignments.Small_model.projections in
      let result = Fun.protect ~finally:(fun () ->
        t.retired_statistics <- add_statistics t.retired_statistics
          (statistics solver)) (fun () ->
        try run ~prepare ~operation_budget:!proposal_budget
          solver (min first 100000)
        with Encoding_stopped reason when !proposal_budget <> max_int ->
          Unknown reason) in
      if keep && result = Sat then t.result_solver <- Some solver;
      result in
      let result =
        if !proposal_budget = max_int then attempt true else
        match Term.with_temporary t.terms (fun () -> attempt false) with
        | Sat -> attempt true
        | answer -> answer in
      (match result with
      | Sat -> Sat
      | _ when not !stopped && !spent < budget ->
        run t (min first (budget - !spent))
      | Unsat _ -> Unknown "resource limit"
      | answer -> answer) in
  let search_limit () = if Option.is_some !deferred then 100000 else max_int in
  let result = propose ~search_limit prepare first in
  let result = match result, !deferred with
    | Unknown _, Some proposal when not !stopped && !spent < budget ->
      let prepare () =
        match proposal () with
        | None -> ()
        | Some assignments -> raise (Small_model.Found assignments) in
      (match propose prepare 0 with
      | Unknown _ when not !stopped && !spent < budget ->
        run t (min first (budget - !spent))
      | answer -> answer)
    | answer, _ -> answer in
  match result with
  | Unknown _ when not !stopped && t.search_remaining <= sat_work t
      && !spent < budget ->
    let alternate, prepare = match t.alternative with
      | Some solver -> solver, (fun () -> ())
      | None ->
        let solver = create t.terms in
        solver.goal_first <- true;
        t.alternative <- Some solver;
        solver, (fun () ->
          Ids.iter (fun term () -> poll solver; register solver term)
            t.declared_terms;
          List.iter (fun term -> poll solver; assert_ solver term)
            (reverse solver t.replay_assertions)) in
    let slice = min 2000000 (budget - !spent) in
    (match run ~prepare alternate slice with
    | (Sat | Unsat _) as answer ->
      t.result_solver <- Some alternate; answer
    | Unknown _ when !stopped || !spent >= budget ->
      Unknown (if !stopped then "cancelled" else "resource limit")
    | Unknown _ ->
      let optimized =
        let rec try_bases = function
          | [] -> Unknown "resource limit"
          | _ when !stopped || !spent >= budget ->
            Unknown (if !stopped then "cancelled" else "resource limit")
          | (basis, split_forms, allowance) :: rest ->
            let solver = create t.terms in
            solver.optimize_cuts <- true;
            solver.cut_basis <- basis;
            solver.split_forms <- split_forms;
            let prepare () =
              Ids.iter (fun term () -> poll solver; register solver term)
                t.declared_terms;
              List.iter (fun term -> poll solver; assert_ solver term)
                (reverse solver t.replay_assertions) in
            let answer = Fun.protect ~finally:(fun () ->
              t.retired_statistics <- add_statistics t.retired_statistics
                (statistics solver)) (fun () ->
              let answer = run ~prepare solver
                  (min allowance (budget - !spent)) in
              match answer with
              | Unknown _ when basis = Arith.Combined && not split_forms
                  && solver.cuts >= 8 && not !stopped && !spent < budget ->
                run solver (min 60000000 (budget - !spent))
              | answer -> answer) in
            (match answer with
            | Sat | Unsat _ -> t.result_solver <- Some solver; answer
            | Unknown _ -> try_bases rest) in
        if Arith.variable_count t.arithmetic <= 32
           && (t.cuts > 0 || alternate.cuts > 0
               || t.splits_since_cut > 0
               || alternate.splits_since_cut > 0) then
          try_bases [Arith.Tableau, false, 500000;
                     Arith.Reduced, false, 5000000;
                     Arith.Combined, false, 20000000;
                     Arith.Combined, true, 500000000]
        else Unknown "resource limit" in
      (match optimized with
      | (Sat | Unsat _) as answer -> answer
      | Unknown _ when !stopped || !spent >= budget ->
        Unknown (if !stopped then "cancelled" else "resource limit")
      | Unknown _ ->
      let delayed_eligible =
        Arith.variable_count t.arithmetic <= 32
        && (t.cuts > 0 || alternate.cuts > 0) in
      let scoped =
        if t.replay_scopes = [] then Unknown "resource limit"
        else match run t (min first (budget - !spent)) with
        | (Sat | Unsat _) as answer -> answer
        | Unknown _ when !stopped || !spent >= budget ->
          Unknown (if !stopped then "cancelled" else "resource limit")
        | Unknown _ ->
          let solver = create t.terms in
          let prepare () =
            Ids.iter (fun term () -> poll solver; register solver term)
              t.declared_terms;
            List.iter (fun term -> poll solver; assert_ solver term)
              (reverse solver t.replay_assertions) in
          let answer = Fun.protect ~finally:(fun () ->
            t.retired_statistics <- add_statistics t.retired_statistics
              (statistics solver)) (fun () ->
            run ~prepare solver (min 32000000 (budget - !spent))) in
          (match answer with
          | Sat | Unsat _ -> t.result_solver <- Some solver
          | Unknown _ -> ());
          answer in
      match scoped with
      | (Sat | Unsat _) as answer -> answer
      | Unknown _ when !stopped || !spent >= budget ->
        Unknown (if !stopped then "cancelled" else "resource limit")
      | Unknown _ ->
      let delayed =
        if delayed_eligible
           || (Arith.variable_count t.arithmetic <= 32
               && (t.cuts > 0 || alternate.cuts > 0)) then begin
          let solver = create t.terms in
          solver.eager_cuts <- false;
          let prepare () =
            Ids.iter (fun term () -> poll solver; register solver term)
              t.declared_terms;
            List.iter (fun term -> poll solver; assert_ solver term)
              (reverse solver t.replay_assertions) in
          let answer = Fun.protect ~finally:(fun () ->
            t.retired_statistics <- add_statistics t.retired_statistics
              (statistics solver)) (fun () ->
            run ~prepare solver (min 128000000 (budget - !spent))) in
          (match answer with
          | Sat | Unsat _ -> t.result_solver <- Some solver
          | Unknown _ -> ());
          answer
        end else Unknown "resource limit" in
      match delayed with
      | (Sat | Unsat _) as answer -> answer
      | Unknown _ when !stopped || !spent >= budget ->
        Unknown (if !stopped then "cancelled" else "resource limit")
      | Unknown _ -> run t (budget - !spent)))
  | answer -> answer

let stored_value = value

let rec value t term =
  model_poll t;
  let poll () = model_poll t in
  match stored_value t term with
  | Some _ as value -> value
  | None when Ids.mem t.declared_terms term ->
    (match Term.sort t.terms term with
    | Term.Bool -> Some (Boolean false)
    | Term.Int -> Some (Integer Z.zero)
    | Term.Bitvec width -> Some (Bits (width, Z.zero))
    | Term.Named _ -> None)
  | None ->
    match Term.Unsafe.node t.terms term with
    | Term.Boolean b -> Some (Boolean b)
    | Term.Integer z -> Some (Integer z)
    | Term.Bits (width, z) -> Some (Bits (width, z))
    | Term.Apply (symbol, args) ->
      let values = Array.map (value t) args in
      if not (Array.for_all Option.is_some values) then None
      else
        let values = Array.map Option.get values in
        let int = function Integer z -> z
          | _ -> invalid_arg "Integer model value" in
        let bool = function Boolean b -> b
          | _ -> invalid_arg "Boolean model value" in
        let ints f = Some (Integer (f (Array.map int values))) in
        let sign = Term.Unsafe.signature t.terms symbol in
        match sign.kind with
        | Term.Builtin "not" -> Some (Boolean (not (bool values.(0))))
        | Term.Builtin "and" -> Some (Boolean (Array.for_all bool values))
        | Term.Builtin "or" -> Some (Boolean (Array.exists bool values))
        | Term.Builtin "xor" ->
          Some (Boolean (bool values.(0) <> bool values.(1)))
        | Term.Builtin "=" -> Some (Boolean (values.(0) = values.(1)))
        | Term.Builtin "distinct" ->
          let seen = Hashtbl.create (Array.length values) in
          Some (Boolean (Array.for_all (fun value ->
            poll ();
            if Hashtbl.mem seen value then false
            else begin Hashtbl.add seen value (); true end) values))
        | Term.Builtin "ite" ->
          Some (if bool values.(0) then values.(1) else values.(2))
        | Term.Builtin "+" -> ints (Array.fold_left (Z.add ~poll) Z.zero)
        | Term.Builtin "*" -> ints (Array.fold_left (Z.mul ~poll) Z.one)
        | Term.Builtin "-" -> ints (fun xs ->
          if Array.length xs = 1 then Z.neg xs.(0)
          else Array.fold_left (Z.sub ~poll) xs.(0)
            (Array.sub xs 1 (Array.length xs - 1)))
        | Term.Builtin ("div" | "mod" as op)
          when not (Z.is_zero (int values.(1))) ->
          let q, r = Z.div_rem ~poll (int values.(0)) (int values.(1)) in
          Some (Integer (if op = "div" then q else r))
        | Term.Builtin ("<" | "<=" | ">" | ">=" as op) ->
          let c = Z.compare (int values.(0)) (int values.(1)) in
          Some (Boolean (match op with
            | "<" -> c < 0 | "<=" -> c <= 0 | ">" -> c > 0 | _ -> c >= 0))
        | Term.Uninterpreted | Term.Selector _ | Term.Tester _ ->
          Ids.fold (fun candidate () result ->
            poll ();
            match result, Term.Unsafe.node t.terms candidate with
            | None, Term.Apply (other, arguments) when symbol = other
                && Array.map (value t) arguments = Array.map Option.some values ->
              stored_value t candidate
            | _ -> result) t.applications None
        | _ -> None

let evaluate = value
let result_solver t = Option.value t.result_solver ~default:t

let value t term =
  let t = result_solver t in
  if t.model = None then None else evaluate t (rewrite t term)

let model_term t term = rewrite (result_solver t) term
let raw_value t term =
  let t = result_solver t in
  if t.model = None then None else stored_value t (rewrite t term)
let model_terms t =
  let t = result_solver t in
  let terms = Ids.create 128 in
  List.iter (fun table -> Ids.iter (fun term () ->
    model_poll t; Ids.replace terms term ()) table)
    [t.applications; t.declared_terms; t.bitvector_terms];
  Ids.iter (fun term _ -> model_poll t; Ids.replace terms term ()) t.booleans;
  Ids.iter (fun term _ -> model_poll t; Ids.replace terms term ()) t.linear;
  IM.iter (fun term _ -> model_poll t; Ids.replace terms term ())
    t.substitutions;
  Ids.fold (fun term () result -> model_poll t; term :: result) terms []

let resource_count t =
  Option.value t.counted_work ~default:(resource_count t)

let statistics t =
  let result = add_statistics (statistics t) t.retired_statistics in
  match t.alternative with
  | None -> result
  | Some solver -> add_statistics result (statistics solver)

end

module Term = Vox_smt_term
module Z = Vox_smt_integer.Z
module Sat = Vox_sat_solver
module Resource = Vox_smt_resource

type value = Core.value =
  | Boolean of bool | Integer of Z.t | Bits of int * Z.t | Abstract of int
type result = Core.result = Sat | Unsat of Term.term list | Unknown of string

type mutation = Register of Term.term | Assert of Term.term | Push | Pop

type model_generation = unit ref

type t =
  { terms : Term.t;
    mutable core : Core.t option;
    mutable frames : Term.term list list;
    registered : unit Core.Ids.t;
    mutable declarations : Term.term list;
    mutable pending : mutation list;
    mutable retired : Sat.statistics;
    mutable work : int;
    mutable model_generation : model_generation option;
    mutable busy : bool
  }

let zero_statistics =
  { Sat.solves = 0; decisions = 0; propagations = 0; conflicts = 0;
    restarts = 0; learnt_clauses = 0; deleted_clauses = 0 }

let create terms =
  { terms; core = Some (Core.create terms); frames = [[]];
    registered = Core.Ids.create 128;
    declarations = []; pending = []; retired = zero_statistics; work = 0;
    model_generation = None; busy = false }

let idle t =
  if t.busy then invalid_arg "Reentrant SMT operation"

let enqueue t mutation =
  if Option.is_some t.core then t.pending <- mutation :: t.pending

let retire t core =
  t.retired <- Core.add_statistics t.retired (Core.statistics core);
  t.core <- None;
  t.pending <- []

let operation t resource f =
  idle t;
  t.busy <- true;
  Fun.protect ~finally:(fun () -> t.busy <- false) (fun () ->
    Option.iter Resource.check resource;
    Option.iter Resource.poll resource;
    f ())

let preprocess t resource mutation =
  match t.core, t.pending with
  | Some core, [] ->
    (match Core.with_resource core resource (fun () ->
      (match mutation with
      | Register term -> Core.register core term
      | Assert term -> Core.assert_ core term
      | Push | Pop -> invalid_arg "SMT preprocessing invariant");
      Option.iter Resource.check resource) with
    | () -> ()
    | exception exn ->
      t.model_generation <- None;
      retire t core;
      raise exn)
  | _ -> enqueue t mutation

let register ?resource t term =
  idle t;
  ignore (Term.sort t.terms term);
  operation t resource (fun () ->
  if not (Core.Ids.mem t.registered term) then begin
    let declarations = term :: t.declarations in
    preprocess t resource (Register term);
    Core.Ids.add t.registered term ();
    t.declarations <- declarations;
    if Option.is_some t.model_generation then
      t.model_generation <- Some (ref ())
  end)

let assert_ ?resource t term =
  idle t;
  if Term.sort t.terms term <> Term.Bool
  then invalid_arg "SMT Boolean assertion required";
  operation t resource (fun () ->
  match t.frames with
  | frame :: rest ->
    let frames = (term :: frame) :: rest in
    preprocess t resource (Assert term);
    t.frames <- frames;
    t.model_generation <- None
  | [] -> invalid_arg "SMT assertion frame invariant")

let push ?resource ?(count = 1) t =
  idle t;
  if count < 0 then invalid_arg "SMT negative push count";
  operation t resource (fun () ->
    let rec prepare count frames pending =
      if count = 0 then frames, pending else begin
        Option.iter Resource.poll resource;
        let pending = if Option.is_some t.core then Push :: pending
          else pending in
        prepare (count - 1) ([] :: frames) pending
      end in
    let frames, pending = prepare count t.frames t.pending in
    Option.iter Resource.check resource;
    if count > 0 then begin
      t.frames <- frames;
      t.pending <- pending;
      t.model_generation <- None
    end)

let pop ?resource ?(count = 1) t =
  idle t;
  if count < 0 then invalid_arg "SMT negative pop count";
  operation t resource (fun () ->
    let poll () = Option.iter Resource.poll resource in
    let removed_assertions = ref false in
    let can_discard = ref (t.pending <> []) in
    let discard pending =
      let rec find depth registered = function
        | [] -> can_discard := false; None
        | mutation :: rest ->
          poll ();
          match mutation with
          | Push when depth = 0 ->
            Some (List.fold_left (fun pending mutation ->
              poll (); mutation :: pending) rest registered)
          | Push -> find (depth - 1) registered rest
          | Pop -> find (depth + 1) registered rest
          | Register _ -> find depth (mutation :: registered) rest
          | Assert _ -> find depth registered rest in
      if !can_discard then find 0 [] pending else None in
    let rec prepare count frames pending =
      if count = 0 then frames, pending else begin
        poll ();
        match frames with
        | frame :: (_ :: _ as rest) ->
          let pending = match t.core with
            | None -> pending
            | Some _ ->
              match discard pending with
              | Some pending -> pending
              | None ->
                if frame <> [] then removed_assertions := true;
                Pop :: pending in
          prepare (count - 1) rest pending
        | _ -> invalid_arg "SMT pop without push"
      end in
    let frames, pending = prepare count t.frames t.pending in
    let rec pending_pops count = function
      | [] -> Some count
      | Pop :: rest -> poll (); pending_pops (count + 1) rest
      | Register _ :: rest -> poll (); pending_pops count rest
      | _ -> None in
    let removed = match t.core with
      | None -> None
      | Some core ->
        match pending_pops 0 pending with
        | Some count when Core.removes_nonlinear core ~poll count -> Some core
        | None when !removed_assertions -> Some core
        | _ -> None in
    Option.iter Resource.check resource;
    if count > 0 then begin
      t.frames <- frames;
      t.pending <- pending;
      t.model_generation <- None;
      Option.iter (retire t) removed
    end)

let check ?resource ?(assumptions = []) ?(budget = max_int)
    ?(reorder_after = 8000000) ?(comparison_after = 200000)
    ?(cancelled = fun () -> false) t =
  idle t;
  if budget < 0 then invalid_arg "Vox_smt_engine.check: budget";
  if reorder_after < 0 then invalid_arg "Vox_smt_engine.check: reorder_after";
  if comparison_after < 0
  then invalid_arg "Vox_smt_engine.check: comparison_after";
  t.model_generation <- None;
  t.work <- 0;
  let prepared_frames = match t.core with
    | Some core when Core.needs_reclamation core ->
      let prepared = core.Core.prepared_frames in
      retire t core;
      prepared
    | Some _ | None -> [] in
  let core, prepare = match t.core with
    | Some core ->
      core, (fun () ->
        List.iter (fun mutation ->
          Core.poll core;
          match mutation with
          | Register term -> Core.register core term
          | Assert term -> Core.assert_ core term
          | Push -> Core.push core
          | Pop -> Core.pop core) (Core.reverse core t.pending);
        t.pending <- [])
    | None ->
      let core = Core.create t.terms in
      core, (fun () ->
        let prepared = ref (Core.reverse core prepared_frames) in
        List.iter (fun term -> Core.poll core; Core.register core term)
          (Core.reverse core t.declarations);
        List.iteri (fun depth frame ->
          Core.poll core;
          if depth > 0 then Core.push core;
          let restored = match !prepared with
            | Some previous :: rest
                when Core.restore_frame core previous frame ->
              prepared := rest;
              true
            | _ -> prepared := []; false in
          if not restored then
            List.iter (fun term -> Core.poll core; Core.assert_ core term)
              (Core.reverse core frame)) (Core.reverse core t.frames);
        t.core <- Some core) in
  t.busy <- true;
  Fun.protect ~finally:(fun () -> t.busy <- false) (fun () ->
  let started = ref false in
  match (Option.iter Resource.check resource;
    Option.iter Resource.poll resource;
    started := true;
    Core.with_resource core resource (fun () ->
      Core.check ~assumptions ~budget ~reorder_after ~comparison_after
        ~cancelled ~prepare core)) with
  | result ->
    t.work <- Core.resource_count core;
    t.model_generation <- (if result = Sat then Some (ref ()) else None);
    (match result with
    | Unknown _ when Core.interrupted_theory core -> retire t core
    | _ -> ());
    result
  | exception Resource.Exhausted reason ->
    if !started then t.work <- Core.resource_count core;
    if !started && Core.interrupted_theory core then retire t core;
    Unknown reason
  | exception Core.Encoding_stopped reason ->
    t.work <- Core.resource_count core;
    retire t core;
    Unknown reason
  | exception exn ->
    t.work <- Core.resource_count core;
    retire t core;
    raise exn)

module For_testing = struct
  let check = check
end

let check ?resource ?assumptions ?budget ?cancelled t =
  check ?resource ?assumptions ?budget ?cancelled t

let access t resource f =
  operation t resource (fun () ->
    match t.core with
    | None -> f ()
    | Some core ->
      let result = Core.with_resource core resource f in
      Option.iter Resource.check resource;
      result)

let value ?resource t term =
  access t resource (fun () ->
  if Option.is_none t.model_generation then None else
  match Option.bind t.core (fun core -> Core.value core term) with
  | Some _ as value -> value
  | None when Core.Ids.mem t.registered term ->
    (match Term.sort t.terms term with
    | Term.Bool -> Some (Boolean false)
    | Term.Int -> Some (Integer Z.zero)
    | Term.Bitvec width -> Some (Bits (width, Z.zero))
    | Term.Named _ -> None)
  | None -> None)

let raw_value ?resource t term =
  access t resource (fun () ->
  if Option.is_none t.model_generation then None
  else Option.bind t.core (fun core -> Core.raw_value core term))

let model_term ?resource t term =
  access t resource (fun () ->
  if Option.is_none t.model_generation then term
  else match t.core with
  | Some core -> Core.model_term core term
  | None -> term)

let model_terms ?resource t =
  access t resource (fun () ->
    let poll () = Option.iter Resource.poll resource in
    let terms = match t.core with
      | None -> t.declarations
      | Some core -> List.rev_append
          (List.fold_left (fun terms term -> poll (); term :: terms)
             [] t.declarations)
          (Core.model_terms core) in
    List.sort_uniq (fun x y -> poll (); Int.compare x y) terms)

let statistics t =
  match t.core with
  | None -> t.retired
  | Some core -> Core.add_statistics t.retired (Core.statistics core)

let resource_count t = t.work

let model_generation t = t.model_generation

let model_is_current t generation =
  match t.model_generation with
  | Some current -> current == generation
  | None -> false
