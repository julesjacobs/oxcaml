module Term = Vox_smt_term
module Z = Vox_smt_integer.Z
module Int_table = Hashtbl.Make (struct
  type t = int
  let equal = Int.equal
  let hash x = x land max_int
end)

let rec compare_monomials a b =
  match a, b with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | x :: xs, y :: ys ->
    let c = Int.compare x y in
    if c = 0 then compare_monomials xs ys else c

module P = Map.Make (struct
  type t = Term.term list
  let compare = compare_monomials
end)

type modular_atom = Value of Term.term | Indicator of Term.term

module Modular_polynomial = Map.Make (struct
  type t = modular_atom list
  let compare = Stdlib.compare
end)

type extension =
  { value : Term.term; operator : string; operands : Term.term array }
type product = { term : Term.term; factors : Term.term array }
type division =
  { dividend : Term.term;
    divisor : Term.term;
    quotient : Term.term;
    remainder : Term.term
  }

type t =
  { terms : Term.t;
    boolean : Term.term -> int;
    clause : int array -> unit;
    poll : unit -> unit;
    polynomials : Z.t P.t option Int_table.t;
    normalized : Term.term option Int_table.t;
    products : (Term.term, product) Hashtbl.t;
    divisions : (Term.term * Term.term, division) Hashtbl.t;
    extensions : extension Int_table.t;
    initialized_extensions : unit Int_table.t;
    initialized_products : unit Int_table.t;
    initialized_divisions : (Term.term * Term.term, unit) Hashtbl.t;
    division_pairs : (Term.term * Term.term, unit) Hashtbl.t;
    emitted : (int list, unit) Hashtbl.t;
    modular_registered : unit Int_table.t;
    residue_checked : unit Int_table.t;
    mutable modular_integers : Term.term list;
    mutable moduli : Z.t list;
    mutable inferred_moduli : Z.t list;
    mutable modular_targets : (Term.term * Term.term * Term.term) list;
    interval_bounds : (bool * Z.t) list Int_table.t;
    mutable modular_bounds : (Term.term * Z.t) list;
    mutable inspected : int;
    comparisons : Term.term Queue.t;
    mutable sums : Term.term list;
    mutable linear_comparisons : Term.term list;
    mutable linear_comparison_count : int;
    comparison_inputs : unit Int_table.t;
    mutable emitting : bool;
    mutable ordering_enabled : bool;
    envelope_ratios :
      (Term.term * Term.term, int * int * (Z.t * Z.t) list) Hashtbl.t;
    mutable revision : int;
    mutable reclaim_extensions : bool
  }

type result = Done | Continue | Incomplete of string
exception Expansion_limit

let create ~terms ~boolean ~clause ?(poll = fun () -> ()) () =
  { terms; boolean; clause; poll;
    polynomials = Int_table.create 64;
    normalized = Int_table.create 64;
    products = Hashtbl.create 32;
    divisions = Hashtbl.create 16;
    extensions = Int_table.create 16;
    initialized_extensions = Int_table.create 16;
    initialized_products = Int_table.create 32;
    initialized_divisions = Hashtbl.create 16;
    division_pairs = Hashtbl.create 16;
    emitted = Hashtbl.create 128;
    modular_registered = Int_table.create 64;
    residue_checked = Int_table.create 64;
    modular_integers = [];
    moduli = [];
    inferred_moduli = [];
    modular_targets = [];
    modular_bounds = [];
    interval_bounds = Int_table.create 32;
    inspected = 0;
    comparisons = Queue.create ();
    sums = [];
    linear_comparisons = [];
    linear_comparison_count = 0;
    comparison_inputs = Int_table.create 32;
    emitting = false; ordering_enabled = false; envelope_ratios = Hashtbl.create 8;
    revision = 0;
    reclaim_extensions = false
  }

let enable_ordering t = t.ordering_enabled <- true

let constant number =
  if Z.is_zero number then P.empty else P.singleton [] number

let add_term t monomial coefficient polynomial =
  t.poll ();
  if List.length monomial > 16 then raise Expansion_limit;
  let coefficient = match P.find_opt monomial polynomial with
    | None -> coefficient
    | Some old -> Z.add ~poll:t.poll old coefficient in
  let polynomial =
    if Z.is_zero coefficient then P.remove monomial polynomial
    else P.add monomial coefficient polynomial in
  if P.cardinal polynomial > 256 then raise Expansion_limit;
  polynomial

let add t left right = P.fold (add_term t) left right

let negate polynomial = P.map Z.neg polynomial

let multiply t left right =
  if P.cardinal left * P.cardinal right > 4096 then raise Expansion_limit;
  P.fold (fun xs a product ->
      P.fold (fun ys b product ->
          let monomial = List.sort Int.compare (xs @ ys) in
          add_term t monomial (Z.mul ~poll:t.poll a b) product)
        right product) left P.empty

let rec polynomial t term =
  t.poll ();
  match Int_table.find_opt t.polynomials term with
  | Some (Some polynomial) -> polynomial
  | Some None -> raise Expansion_limit
  | None ->
    let polynomial =
      try Some (match Term.Unsafe.node t.terms term with
        | Term.Integer number -> constant number
        | Term.Apply (symbol, arguments) -> (
          match (Term.Unsafe.signature t.terms symbol).kind with
          | Term.Builtin "+" ->
            Array.fold_left (fun result term ->
                add t result (polynomial t term))
              P.empty arguments
          | Term.Builtin "-" when Array.length arguments = 1 ->
            negate (polynomial t arguments.(0))
          | Term.Builtin "-" when Array.length arguments > 1 ->
            let result = ref (polynomial t arguments.(0)) in
            for index = 1 to Array.length arguments - 1 do
              result := add t !result (negate (polynomial t arguments.(index)))
            done;
            !result
          | Term.Builtin "*" ->
            Array.fold_left (fun result term ->
                multiply t result (polynomial t term))
              (constant Z.one) arguments
          | _ -> P.singleton [term] Z.one)
        | Term.Boolean _ | Term.Bits _ ->
          invalid_arg "Vox_smt_nonlinear: integer expression required")
      with Expansion_limit -> None
    in
    Int_table.add t.polynomials term polynomial;
    match polynomial with Some polynomial -> polynomial
    | None -> raise Expansion_limit

let integer t number = Term.integer t.terms number
let int t number = integer t (Z.of_int number)
let builtin t name sort arguments = Term.builtin t.terms name sort arguments
let times t arguments = builtin t "*" Term.Int arguments
let plus t arguments = builtin t "+" Term.Int arguments
let minus t left right = builtin t "-" Term.Int [|left; right|]
let compare t name left right = builtin t name Term.Bool [|left; right|]
let equal t left right = Term.equal t.terms left right
let not_ t term = Term.not_ t.terms term

let term_of_polynomial t polynomial =
  let terms = P.fold (fun monomial coefficient terms ->
      t.poll ();
      let factors =
        if Z.equal coefficient Z.one && monomial <> [] then monomial
        else integer t coefficient :: monomial in
      times t (Array.of_list factors) :: terms) polynomial [] in
  plus t (Array.of_list terms)

let normalize t term =
  match Int_table.find_opt t.normalized term with
  | Some result -> result
  | None ->
    let result =
      try Some (term_of_polynomial t (polynomial t term))
      with Expansion_limit -> None in
    Int_table.add t.normalized term result;
    result

let register_product t term =
  match Term.Unsafe.node t.terms term with
  | Term.Apply (symbol, factors)
    when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "*" ->
    Hashtbl.replace t.products term { term; factors }
  | _ -> invalid_arg "Vox_smt_nonlinear.register_product"

let register_extension t value =
  match Term.Unsafe.node t.terms value with
  | Term.Apply (symbol, operands) ->
    (match (Term.Unsafe.signature t.terms symbol).kind with
    | Term.Builtin (("int.pow2" | "int.log2" | "piand") as operator) ->
      Int_table.replace t.extensions value { value; operator; operands }
    | _ -> invalid_arg "Vox_smt_nonlinear.register_extension")
  | _ -> invalid_arg "Vox_smt_nonlinear.register_extension"

let register_division t ~dividend ~divisor ~quotient ~remainder =
  Hashtbl.replace t.divisions (dividend, divisor)
    { dividend; divisor; quotient; remainder }

let emit t reasons premises conclusion =
  t.poll ();
  let literals =
    let previous = t.emitting in
    t.emitting <- true;
    Fun.protect ~finally:(fun () -> t.emitting <- previous) (fun () ->
      t.boolean conclusion
      :: List.rev_append (List.map (fun literal -> -literal) reasons)
           (List.map (fun term -> -(t.boolean term)) premises)
      |> List.sort_uniq Int.compare) in
  if not (Hashtbl.mem t.emitted literals)
     && not (List.exists (fun literal ->
         List.mem (-literal) literals) literals)
  then begin
    t.clause (Array.of_list literals);
    Hashtbl.add t.emitted literals ();
    t.revision <- t.revision + 1
  end

let residue_nonzero t p =
  let exception Skip in
  let remaining = ref 20000 in
  let poll () =
    t.poll ();
    if !remaining = 0 then raise Skip;
    decr remaining in
  try
    let groups = Int_table.create 8 and offset = ref Z.zero in
    let nonlinear = ref false in
    P.iter (fun monomial coefficient ->
      poll ();
      match monomial with
      | [] -> offset := Z.add ~poll !offset coefficient
      | variable :: rest ->
        if not (List.for_all ((=) variable) rest) then raise Skip;
        let degree = List.length monomial in
        if degree > 1 then nonlinear := true;
        let old = Option.value (Int_table.find_opt groups variable)
            ~default:[] in
        Int_table.replace groups variable ((degree, coefficient) :: old)) p;
    if not !nonlinear || Int_table.length groups > 16 then false else
    let groups = Int_table.fold (fun _ group groups -> group :: groups)
        groups [] in
    List.exists (fun modulus ->
      let residue coefficient =
        match Z.to_int_opt (snd (Z.div_rem ~poll coefficient
          (Z.of_int modulus))) with
        | Some value -> value | None -> raise Skip in
      let full = (1 lsl modulus) - 1 in
      let possible = List.fold_left (fun possible group ->
        if possible = full then possible else begin
          let group = List.map (fun (power, coefficient) ->
            power, residue coefficient) group in
          let values = ref 0 in
          for value = 0 to modulus - 1 do
            let result = List.fold_left (fun result (power, coefficient) ->
              let product = ref coefficient in
              for _ = 1 to power do
                poll (); product := (!product * value) mod modulus
              done;
              (result + !product) mod modulus) 0 group in
            values := !values lor (1 lsl result)
          done;
          let result = ref 0 in
          for left = 0 to modulus - 1 do
            if possible land (1 lsl left) <> 0 then
              for right = 0 to modulus - 1 do
                poll ();
                if !values land (1 lsl right) <> 0 then
                  result := !result lor (1 lsl ((left + right) mod modulus))
              done
          done;
          !result
        end) (1 lsl residue !offset) groups in
      possible land 1 = 0) [2; 3; 4; 5; 7; 8; 9; 11; 13; 16]
  with Skip -> false

let residue_equalities t =
  List.iter (fun (term, left, right) ->
    t.poll ();
    if not (Int_table.mem t.residue_checked term) then begin
      (try
        let difference = add t (polynomial t left)
            (negate (polynomial t right)) in
        if residue_nonzero t difference
        then emit t [] [] (not_ t term)
      with Expansion_limit -> ());
      Int_table.add t.residue_checked term ()
    end) t.modular_targets

let same ~representative ~explain left right =
  if left = right then Some []
  else if representative left = representative right
  then Option.map Array.to_list (explain left right)
  else None

let substitute t ~representative ~explain product =
  try
    let reasons = ref [] in
    let result = P.fold (fun monomial coefficient result ->
        let expanded = List.fold_left (fun product factor ->
            let other = representative factor in
            let replacement =
              if other = factor then polynomial t factor
              else match explain factor other with
                | None -> polynomial t factor
                | Some explanation ->
                  reasons := Array.to_list explanation @ !reasons;
                  polynomial t other in
            multiply t product replacement)
            (constant coefficient) monomial in
        add t result expanded) (polynomial t product.term) P.empty in
    let normalized = term_of_polynomial t result in
    if normalized <> product.term
    then emit t !reasons [] (equal t product.term normalized)
  with Expansion_limit -> ()

let basic_product t product =
  let zero = int t 0 and one = int t 1 in
  Array.iteri (fun index factor ->
      t.poll ();
      let other = Array.to_list product.factors
          |> List.mapi (fun i term -> i, term)
          |> List.filter_map (fun (i, term) -> if i = index then None
              else Some term)
          |> Array.of_list |> times t in
      emit t [] [equal t factor zero] (equal t product.term zero);
      emit t [] [equal t factor one] (equal t product.term other);
      emit t [] [equal t factor (int t (-1))]
        (equal t product.term (minus t zero other))) product.factors;
  if Array.length product.factors = 2 then begin
    let x = product.factors.(0) and y = product.factors.(1) in
    let ge term = compare t ">=" term zero in
    let le term = compare t "<=" term zero in
    emit t [] [ge x; ge y] (ge product.term);
    emit t [] [le x; le y] (ge product.term);
    emit t [] [ge x; le y] (le product.term);
    emit t [] [le x; ge y] (le product.term);
    if x = y then emit t [] [] (ge product.term)
  end

let division_axioms t division =
  let x = division.dividend and y = division.divisor in
  let q = division.quotient and r = division.remainder in
  let zero = int t 0 in
  let nonzero = not_ t (equal t y zero) in
  emit t [] [nonzero; equal t x y] (equal t q (int t 1));
  emit t [] [nonzero; equal t x y] (equal t r zero);
  emit t [] [nonzero] (equal t x (plus t [|times t [|y; q|]; r|]));
  emit t [] [nonzero] (compare t ">=" r zero);
  List.iter (fun (sign, magnitude, order, opposite) ->
      List.iter (fun (condition, relation) ->
          let conclusion = compare t relation q zero in
          emit t [] [sign; condition] conclusion;
          emit t [] [sign; not_ t condition] (not_ t conclusion))
        [compare t ">=" x zero, order;
         compare t "<" x magnitude, opposite])
    [compare t ">" y zero, y, ">=", "<=";
     compare t "<" y zero, minus t zero y, "<=", ">="];
  emit t [] [compare t ">" y zero] (compare t "<" r y);
  emit t [] [compare t "<" y zero]
    (compare t "<" r (minus t zero y));
  List.iter (fun (sign, magnitude) ->
      let guards = [sign; compare t ">=" x zero;
                    compare t "<" x magnitude] in
      emit t [] guards (equal t q zero);
      emit t [] guards (equal t r x))
    [compare t ">" y zero, y;
     compare t "<" y zero, minus t zero y]

let division_congruence t left right =
  let key = left.quotient, right.quotient in
  if not (Hashtbl.mem t.division_pairs key) then begin
    let guards = [equal t left.dividend right.dividend;
                  equal t left.divisor right.divisor] in
    emit t [] guards (equal t left.quotient right.quotient);
    emit t [] guards (equal t left.remainder right.remainder);
    Hashtbl.add t.division_pairs key ()
  end

let nested_remainders t divisions =
  if List.compare_length_with divisions 16 <= 0 then begin
    let exception Limit in
    let remaining = ref 5000 in
    let charge () =
      t.poll ();
      if !remaining = 0 then raise Limit;
      decr remaining in
    try
      let groups = Hashtbl.create 8 in
      List.iter (fun division ->
        charge ();
        let previous = Option.value
            (Hashtbl.find_opt groups division.divisor) ~default:[] in
        Hashtbl.replace groups division.divisor (division :: previous))
        divisions;
      Hashtbl.iter (fun divisor group -> match group with
        | [] | [_] -> ()
        | _ ->
          let cache = Int_table.create 32 in
          let rec normalize term =
            charge ();
            match Int_table.find_opt cache term with
            | Some result -> result
            | None ->
              let result = match Term.Unsafe.node t.terms term with
                | Term.Integer value -> constant value, false
                | Term.Apply (symbol, arguments) ->
                  (match (Term.Unsafe.signature t.terms symbol).kind,
                      arguments with
                  | Term.Builtin "mod", [|dividend; modulus|]
                      when modulus = divisor ->
                    let value, _ = normalize dividend in value, true
                  | Term.Builtin ("+" | "-" | "*" as op), _ ->
                    let values = Array.map normalize arguments in
                    let nested = Array.exists snd values in
                    let value = if op = "+" then
                        Array.fold_left (fun p (q, _) ->
                          charge (); add t p q) P.empty values
                      else if op = "*" then
                        Array.fold_left (fun p (q, _) ->
                          charge (); multiply t p q) (constant Z.one) values
                      else if Array.length values = 1 then
                        negate (fst values.(0))
                      else if Array.length values >= 2 then begin
                        let p = ref (fst values.(0)) in
                        for i = 1 to Array.length values - 1 do
                          charge (); p := add t !p (negate (fst values.(i)))
                        done;
                        !p
                      end else raise Expansion_limit in
                    value, nested
                  | _ -> P.singleton [term] Z.one, false)
                | _ -> P.singleton [term] Z.one, false in
              Int_table.add cache term result;
              result in
          let normalized = List.map (fun division ->
            charge ();
            let p, nested = normalize division.dividend in
            division, p, nested) group in
          let rec pairs = function
            | [] -> ()
            | (left, lp, ln) :: rest ->
              List.iter (fun (right, rp, rn) ->
                charge ();
                if (ln || rn) && P.equal (fun a b ->
                    charge (); Z.equal a b) lp rp then
                  emit t [] [not_ t (equal t divisor (int t 0))]
                    (equal t left.remainder right.remainder)) rest;
              pairs rest in
          pairs normalized) groups
    with Limit | Expansion_limit -> ()
  end

let division_uniqueness t ~representative ~explain division product =
  Array.iteri (fun index factor ->
      match same ~representative ~explain division.divisor factor with
      | None -> ()
      | Some reasons ->
        let other = Array.to_list product.factors
            |> List.mapi (fun i term -> i, term)
            |> List.filter_map (fun (i, term) -> if i = index then None
                else Some term)
            |> Array.of_list |> times t in
        if other <> division.quotient then begin
          let zero = int t 0 in
          let delta = minus t division.dividend product.term in
          List.iter (fun (sign, magnitude) ->
              let guards = [sign; compare t ">=" delta zero;
                            compare t "<" delta magnitude] in
              emit t reasons guards (equal t division.quotient other);
              emit t reasons guards (equal t division.remainder delta))
            [compare t ">" division.divisor zero, division.divisor;
             compare t "<" division.divisor zero,
             minus t zero division.divisor]
        end) product.factors

let other_factors t product index =
  let length = Array.length product.factors in
  if length = 2 then Some product.factors.(1 - index)
  else if length > 2 then
    let factors = Array.init (length - 1) (fun i ->
        product.factors.(if i < index then i else i + 1)) in
    let other = times t factors in
    if Hashtbl.mem t.products other then Some other else None
  else None

let cancellation t ~representative ~explain left right =
  if Array.length left.factors = 2 && Array.length right.factors = 2
  then
    for i = 0 to 1 do
      for j = 0 to 1 do
        let factor = left.factors.(i) in
        match same ~representative ~explain factor right.factors.(j) with
        | None -> ()
        | Some reasons ->
          let x = left.factors.(1 - i) and y = right.factors.(1 - j) in
          let zero = int t 0 in
          emit t reasons [not_ t (equal t factor zero);
                          equal t left.term right.term] (equal t x y);
          List.iter (fun (sign, a, b) ->
              emit t reasons [sign]
                (equal t (compare t "<" left.term right.term)
                   (compare t "<" a b));
              emit t reasons [sign]
                (equal t (compare t "<=" left.term right.term)
                   (compare t "<=" a b)))
            [compare t ">" factor zero, x, y;
             compare t "<" factor zero, y, x]
      done
    done

let integer_bounds t model product =
  t.poll ();
  Array.iteri (fun index factor ->
      match other_factors t product index with
      | None -> ()
      | Some other ->
        match model factor, model other with
        | Some factor_value, Some other_value
          when Z.compare (Z.abs factor_value) Z.one >= 0 ->
          let positive = Z.sign factor_value > 0 in
          let nonnegative = Z.sign other_value >= 0 in
          let relation = if nonnegative = positive then ">=" else "<=" in
          let bound = if positive then other else minus t (int t 0) other in
          emit t []
            [compare t (if positive then ">=" else "<=") factor
               (int t (if positive then 1 else -1));
             compare t (if nonnegative then ">=" else "<=") other (int t 0)]
            (compare t relation product.term bound)
        | _ -> ()) product.factors

let product_monotonicity t model product =
  Array.iteri (fun index factor ->
    t.poll ();
    match other_factors t product index with
    | None -> ()
    | Some other ->
      match model factor, model other, model product.term with
      | Some factor_value, Some other_value, Some actual ->
        let expected = Z.mul ~poll:t.poll factor_value other_value in
        let order = Z.compare actual expected in
        if order <> 0 then begin
          let greater = order < 0 in
          let nonnegative = Z.sign other_value >= 0 in
          let boundary = integer t factor_value in
          let scaled = times t [|boundary; other|] in
          emit t []
            [compare t (if greater = nonnegative then ">=" else "<=")
               factor boundary;
             compare t (if nonnegative then ">=" else "<=") other (int t 0)]
            (compare t (if greater then ">=" else "<=") product.term scaled)
        end
      | _ -> ()) product.factors

let integer_gap t model ~representative ~explain left right =
  t.poll ();
  if Array.length left.factors = 2 && Array.length right.factors = 2 then
    for i = 0 to 1 do
      for j = 0 to 1 do
        let factor = left.factors.(i) in
        match same ~representative ~explain factor right.factors.(j) with
        | None -> ()
        | Some reasons ->
          let x = left.factors.(1 - i) and y = right.factors.(1 - j) in
          List.iter (fun (a, b, lhs, rhs) ->
              match model factor, model a, model b, model lhs, model rhs with
              | Some f, Some av, Some bv, Some lv, Some rv
                when Z.compare av bv < 0 ->
                let nonnegative = Z.compare f Z.zero >= 0 in
                let comparison = Z.compare (Z.add ~poll:t.poll lv f) rv in
                if nonnegative && comparison > 0
                   || not nonnegative && comparison < 0
                then
                  let sign = if nonnegative then ">=" else "<=" in
                  let order = if nonnegative then "<=" else ">=" in
                  emit t reasons [compare t sign factor (int t 0);
                                  compare t "<" a b]
                    (compare t order (plus t [|lhs; factor|]) rhs)
              | _ -> ())
            [x, y, left.term, right.term; y, x, right.term, left.term]
      done
    done

let power_order t model left right =
  let degree = Array.length left.factors in
  if degree > 1 && degree = Array.length right.factors then begin
    let x = left.factors.(0) and y = right.factors.(0) in
    let repeated factor arguments = Array.for_all (fun term ->
      t.poll (); term = factor) arguments in
    if x <> y && repeated x left.factors && repeated y right.factors then
      match model x, model y, model left.term, model right.term with
      | Some xv, Some yv, Some lv, Some rv ->
        let even = degree land 1 = 0 in
        let order = Z.compare
            (if even then Z.abs xv else xv)
            (if even then Z.abs yv else yv) in
        let actual = Z.compare lv rv in
        if order <> 0 && actual = 0 then
          emit t [] [equal t left.term right.term]
            (if even then Term.or_ t.terms
               [|equal t x y; equal t x (minus t (int t 0) y)|]
             else equal t x y);
        if order = 0 && actual <> 0 then
          emit t []
            [if even then Term.or_ t.terms
               [|equal t x y; equal t x (minus t (int t 0) y)|]
             else equal t x y]
            (equal t left.term right.term);
        if order < 0 && actual >= 0 || order > 0 && actual <= 0 then begin
          let magnitude factor value =
            if Z.sign value >= 0 then
              factor, compare t ">=" factor (int t 0)
            else minus t (int t 0) factor,
                 compare t "<=" factor (int t 0) in
          let a, b, guards =
            if even then
              let a, sign_a = magnitude x xv in
              let b, sign_b = magnitude y yv in
              a, b, [sign_a; sign_b]
            else x, y, [] in
          let a, b, lhs, rhs =
            if order < 0 then a, b, left.term, right.term
            else b, a, right.term, left.term in
          emit t [] (compare t "<" a b :: guards)
            (compare t "<" lhs rhs)
        end
      | _ -> ()
  end

let product_difference t model ~representative ~explain left right =
  t.poll ();
  if Array.length left.factors = 2 && Array.length right.factors = 2 then
    for i = 0 to 1 do
      for j = 0 to 1 do
        let factor = left.factors.(i) in
        match same ~representative ~explain factor right.factors.(j) with
        | None -> ()
        | Some reasons ->
          let x = left.factors.(1 - i) and y = right.factors.(1 - j) in
          match model factor, model x, model y,
                model left.term, model right.term with
          | Some f, Some xv, Some yv, Some lv, Some rv ->
            let gap = Z.sub ~poll:t.poll xv yv in
            (* Unit offsets bound the number of cuts for each product pair. *)
            if Z.compare (Z.abs gap) Z.one <= 0 then begin
              let expected = Z.mul ~poll:t.poll gap f in
              let difference = Z.sub ~poll:t.poll lv rv in
              let comparison = Z.compare difference expected in
              if comparison <> 0 then begin
                let nonnegative = Z.compare f Z.zero >= 0 in
                let sign, order =
                  if nonnegative then ">=", "<=" else "<=", ">=" in
                let a, b, lhs, rhs, gap =
                  if (comparison > 0) = nonnegative then
                    x, y, left.term, right.term, gap
                  else y, x, right.term, left.term, Z.neg gap in
                emit t reasons
                  [compare t sign factor (int t 0);
                   compare t "<=" a (plus t [|b; integer t gap|])]
                  (compare t order lhs
                     (plus t [|rhs; times t [|integer t gap; factor|]|]))
              end
            end
          | _ -> ()
      done
    done

let refine_product t model product =
  let values = Array.map model product.factors in
  if not (Array.for_all Option.is_some values) then false
  else
    let values = Array.map Option.get values in
    let expected = Array.fold_left (Z.mul ~poll:t.poll) Z.one values in
    if model product.term = Some expected then true
    else begin
      let premises = Array.to_list (Array.map2 (fun term value ->
          equal t term (integer t value)) product.factors values) in
      emit t [] premises (equal t product.term (integer t expected));
      if Array.length product.factors = 2 then begin
        let x = product.factors.(0) and y = product.factors.(1) in
        let a = values.(0) and b = values.(1) in
        let ac = integer t a and bc = integer t b in
        emit t [] [equal t x ac]
          (equal t product.term (times t [|ac; y|]));
        emit t [] [equal t y bc]
          (equal t product.term (times t [|bc; x|]));
        let plane = minus t
            (plus t [|times t [|ac; y|]; times t [|bc; x|]|])
            (integer t (Z.mul ~poll:t.poll a b)) in
        List.iter (fun (sx, sy, order) ->
            emit t [] [compare t sx x ac; compare t sy y bc]
              (compare t order product.term plane))
          [">=", ">=", ">="; "<=", "<=", ">=";
           ">=", "<=", "<="; "<=", ">=", "<="];
        if x = y then begin
          let slope = Z.add ~poll:t.poll (Z.mul ~poll:t.poll (Z.of_int 2) a)
              Z.one in
          let intercept = Z.mul ~poll:t.poll a (Z.add ~poll:t.poll a Z.one) in
          let line = minus t (times t [|integer t slope; x|])
              (integer t intercept) in
          emit t [] [] (compare t ">=" product.term line)
        end
      end;
      false
    end

let associativity t ~representative ~explain factors left right =
  if Array.length left.factors = 2 && Array.length right.factors = 2
  then
    let aliases product =
      match Hashtbl.find_opt factors (representative product.term) with
      | Some aliases -> aliases | None -> [] in
    for i = 0 to 1 do
      for j = 0 to 1 do
        match same ~representative ~explain
                left.factors.(i) right.factors.(j) with
        | None -> ()
        | Some common ->
          List.iter (fun left_alias ->
            List.iter (fun right_alias ->
              t.poll ();
              let lhs = times t [|left.factors.(1 - i); right_alias|] in
              let rhs = times t [|right.factors.(1 - j); left_alias|] in
              match normalize t lhs, normalize t rhs with
              | Some lhs, Some rhs when lhs <> rhs
                  && Hashtbl.mem t.products lhs
                  && Hashtbl.mem t.products rhs ->
                (match same ~representative ~explain left.term left_alias,
                       same ~representative ~explain
                         right.term right_alias with
                | Some first, Some second ->
                  emit t (common @ first @ second) [] (equal t lhs rhs)
                | _ -> ())
              | _ -> ()) (aliases right)) (aliases left)
      done
    done

let modular_products t ~representative ~explain products divisions left right =
  let same = same ~representative ~explain in
  match same left.divisor right.divisor with
  | None -> ()
  | Some divisors ->
    let residues factor =
      (factor, []) :: List.filter_map (fun division ->
          match same left.divisor division.divisor,
                same factor division.dividend with
          | Some modulus, Some dividend ->
            Some (builtin t "mod" Term.Int
              [|division.dividend; division.divisor|], modulus @ dividend)
          | _ -> None) divisions in
    let congruent left right =
      List.find_map (fun (lhs, first) ->
          List.find_map (fun (rhs, second) ->
              Option.map (fun why -> first @ second @ why) (same lhs rhs))
            (residues right)) (residues left) in
    let products division = List.filter_map (fun product ->
        Option.map (fun why -> product, why)
          (same division.dividend product.term)) products in
    List.iter (fun (lhs, first) ->
        List.iter (fun (rhs, second) ->
            t.poll ();
            if Array.length lhs.factors = 2 && Array.length rhs.factors = 2
            then List.iter (fun index ->
              match congruent lhs.factors.(0) rhs.factors.(index),
                    congruent lhs.factors.(1) rhs.factors.(1 - index) with
              | Some a, Some b ->
                emit t (divisors @ first @ second @ a @ b)
                  [not_ t (equal t left.divisor (int t 0))]
                  (equal t left.remainder right.remainder)
              | _ -> ()) [0; 1]) (products right)) (products left)

let factor_comparisons t =
  let limit = Queue.length t.comparisons in
  let rec intersection left right =
    t.poll ();
    match left, right with
    | [], _ | _, [] -> []
    | a :: left, b :: right when a = b -> a :: intersection left right
    | a :: left, b :: _ when a < b -> intersection left right
    | _, _ :: right -> intersection left right in
  let rec quotient whole common =
    t.poll ();
    match whole, common with
    | rest, [] -> Some rest
    | a :: whole, b :: common when a = b -> quotient whole common
    | a :: whole, b :: _ when a < b ->
      Option.map (fun rest -> a :: rest) (quotient whole common)
    | _ -> None in
  for _ = 1 to limit do
    t.poll ();
    let term = Queue.peek t.comparisons in
    (try
      (match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, [|left; right|])
          when Term.sort t.terms left = Term.Int ->
        (match (Term.Unsafe.signature t.terms symbol).kind with
        | Term.Builtin ("=" | "<" | "<=" | ">" | ">=" as op) ->
          let p = add t (polynomial t left) (negate (polynomial t right)) in
          let scalar term =
            t.poll ();
            match Term.Unsafe.node t.terms term with
            | Term.Apply (symbol, [||]) ->
              (Term.Unsafe.signature t.terms symbol).kind = Term.Uninterpreted
            | _ -> false in
          if P.cardinal p >= 2
             && P.for_all (fun monomial _ -> List.for_all scalar monomial) p
          then begin
            let common = P.fold (fun monomial _ common ->
              t.poll ();
              match common with None -> Some monomial
              | Some common -> Some (intersection common monomial)) p None in
            match common with
            | None | Some [] -> ()
            | Some common ->
              let reduced = P.fold (fun monomial coefficient reduced ->
                t.poll ();
                match quotient monomial common with
                | None -> raise Expansion_limit
                | Some monomial -> add_term t monomial coefficient reduced)
                  p P.empty in
              let factor = times t (Array.of_list common) in
              let reduced = term_of_polynomial t reduced and zero = int t 0 in
              let same = equal t factor zero in
              let reduced_zero = equal t reduced zero in
              if op = "=" then begin
                emit t [] [term; not_ t same] reduced_zero;
                emit t [] [same] term;
                emit t [] [reduced_zero] term
              end else begin
                let reversed = match op with
                  | "<" -> ">" | "<=" -> ">=" | ">" -> "<" | _ -> "<=" in
                let equivalence guard consequence =
                  emit t [] [guard; term] consequence;
                  emit t [] [guard; consequence] term in
                equivalence (compare t ">" factor zero)
                  (compare t op reduced zero);
                equivalence (compare t "<" factor zero)
                  (compare t reversed reduced zero);
                let at_zero = if op = "<" || op = ">" then not_ t term
                  else term in
                emit t [] [same] at_zero
              end
          end
        | _ -> ())
      | _ -> ())
    with Expansion_limit -> ());
    ignore (Queue.take t.comparisons)
  done

let inspect_sums t =
  let limit = Term.count t.terms in
  while t.inspected < limit do
    t.poll ();
    let term = t.inspected in
    (match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, _) ->
      let signature = Term.Unsafe.signature t.terms symbol in
      if signature.result = Term.Int
         && (signature.kind = Term.Builtin "+"
             || signature.kind = Term.Builtin "-")
      then t.sums <- term :: t.sums
    | _ -> ());
    t.inspected <- term + 1
  done

let multiply_equalities t ~representative ~explain products =
  if products <> [] then begin
    inspect_sums t;
    let factors = List.concat_map (fun p -> Array.to_list p.factors) products
        |> List.sort_uniq Int.compare in
    let candidates = int t 0 :: int t 1 :: int t (-1) :: factors in
    let aliases = Hashtbl.create 32 in
    List.iter (fun term ->
        let root = representative term in
        if not (Hashtbl.mem aliases root) then Hashtbl.add aliases root term)
      candidates;
    let existing polynomial = P.for_all (fun monomial _ ->
        List.length monomial <= 1
        || Hashtbl.mem t.products (times t (Array.of_list monomial)))
        polynomial in
    List.iter (fun term ->
        (match Hashtbl.find_opt aliases (representative term) with
        | None -> ()
        | Some alias when alias = term -> ()
        | Some alias ->
          match explain term alias with
          | None -> ()
          | Some why ->
            List.iter (fun factor ->
                t.poll ();
                try
                  let multiplier = polynomial t factor in
                  let left = multiply t multiplier (polynomial t term) in
                  let right = multiply t multiplier (polynomial t alias) in
                  if existing left && existing right then
                    emit t (Array.to_list why) []
                      (equal t (term_of_polynomial t left)
                         (term_of_polynomial t right))
                with Expansion_limit -> ()) factors)) t.sums
  end

let multiplied_bounds t model ~representative ~explain products =
  if products <> [] && Hashtbl.length t.divisions = 0
      && Term.count t.terms <= 4096 then begin
    let exception Limit in
    let remaining = ref 10000 in
    let charge () =
      t.poll ();
      if !remaining = 0 then raise Limit;
      decr remaining in
    try
      let factors = List.fold_left (fun factors p ->
        charge ();
        Array.fold_left (fun factors factor ->
          charge (); factor :: factors) factors p.factors) [] products
        |> List.sort_uniq (fun a b -> charge (); Int.compare a b) in
      let aliases = Hashtbl.create 16 in
      List.iter (fun factor ->
        charge ();
        let root = representative factor in
        if not (Hashtbl.mem aliases root) then
          Hashtbl.add aliases root factor) factors;
      let factor_set = Int_table.create 16 in
      List.iter (fun factor ->
        charge (); Int_table.replace factor_set factor ()) factors;
      let conditional_aliases = Int_table.create 16 in
      List.iter (fun (equality, left, right) ->
        charge ();
        try
          let p = add t (polynomial t left) (negate (polynomial t right)) in
          match P.bindings p with
          | [[a], ca; [b], cb] when Z.equal ca (Z.neg cb)
              && (match model a, model b with
                  | Some va, Some vb -> Z.equal va vb | _ -> false) ->
            if Int_table.mem factor_set a && not (Int_table.mem factor_set b)
            then Int_table.replace conditional_aliases b (a, equality)
            else if Int_table.mem factor_set b
                    && not (Int_table.mem factor_set a)
            then Int_table.replace conditional_aliases a (b, equality)
          | _ -> ()
        with Expansion_limit -> ()) t.modular_targets;
      let evaluate p =
        P.fold (fun monomial coefficient value ->
          charge ();
          match value with None -> None | Some value ->
          let term = match monomial with
            | [] -> Some Z.one
            | [term] -> model term
            | _ ->
              let term = times t (Array.of_list monomial) in
              if Hashtbl.mem t.products term then model term else None in
          Option.map (fun term ->
            Z.add ~poll:charge value (Z.mul ~poll:charge coefficient term))
            term) p (Some Z.zero) in
      List.iter (fun term ->
        charge ();
        try match Term.Unsafe.node t.terms term with
        | Term.Apply (symbol, [|left;right|]) ->
          let kind = (Term.Unsafe.signature t.terms symbol).kind in
          let left, right = match kind with
            | Term.Builtin (">=" | ">") -> right, left
            | _ -> left, right in
          let p = add t (polynomial t left) (negate (polynomial t right)) in
          let p = match kind with
            | Term.Builtin ("<" | ">") -> add t p (constant Z.one)
            | _ -> p in
          if P.cardinal p <= 8
             && P.for_all (fun monomial _ -> List.length monomial <= 1) p
             && P.fold (fun monomial _ n ->
                  if monomial = [] then n else n + 1) p 0 >= 2 then begin
            let reasons = ref [] and guards = ref [] in
            let p = P.fold (fun monomial coefficient result ->
              charge ();
              let monomial = match monomial with
                | [atom] ->
                  (match Hashtbl.find_opt aliases (representative atom) with
                  | Some alias when alias <> atom ->
                    (match explain atom alias with
                    | Some why ->
                      reasons := Array.fold_right (fun reason reasons ->
                        charge (); reason :: reasons) why !reasons;
                      [alias]
                    | None -> monomial)
                  | _ ->
                    (match Int_table.find_opt conditional_aliases atom with
                    | Some (alias, equality) ->
                      guards := equality :: !guards;
                      [alias]
                    | None -> monomial))
                | _ -> monomial in
              match monomial with
              | [atom] when not (Int_table.mem factor_set atom) ->
                raise Expansion_limit
              | _ -> add_term t monomial coefficient result) p P.empty in
            match evaluate p with
            | Some value when Z.sign value <= 0 ->
              List.iter (fun factor ->
                charge ();
                match model factor with
                | None -> ()
                | Some value ->
                  let nonnegative = Z.sign value >= 0 in
                  let product = multiply t p (polynomial t factor) in
                  (match evaluate product with
                  | Some scaled when (if nonnegative then Z.sign scaled > 0
                      else Z.sign scaled < 0) ->
                    emit t !reasons
                      (term :: compare t
                         (if nonnegative then ">=" else "<=")
                         factor (int t 0) :: !guards)
                      (compare t (if nonnegative then "<=" else ">=")
                         (term_of_polynomial t product) (int t 0))
                  | _ -> ())) factors
            | _ -> ()
          end
        | _ -> ()
        with Expansion_limit -> ()) t.linear_comparisons
    with Limit -> ()
  end

let symbolic_modular_equalities t ~integer:model ~representative ~explain
    divisions =
  let divisions = List.filter (fun division ->
    t.poll ();
    match Term.Unsafe.node t.terms division.divisor with
    | Term.Integer _ -> false
    | _ -> true) divisions in
  if List.compare_length_with divisions 8 <= 0 then
  List.iter (fun division -> try
    let divisor = polynomial t division.divisor in
    if P.exists (fun monomial _ ->
        t.poll (); List.compare_length_with monomial 1 > 0) divisor then begin
      let append_reasons left right =
        List.fold_left (fun reasons reason -> t.poll (); reason :: reasons)
          right left in
      let ordering a b =
        let order = Int.compare (List.length a) (List.length b) in
        if order = 0 then compare_monomials b a else order in
      let leading p = P.fold (fun monomial coefficient result ->
        t.poll ();
        match result with
        | Some (old, _) when ordering old monomial >= 0 -> result
        | _ -> Some (monomial, coefficient)) p None in
      let rec quotient left right =
        t.poll ();
        match left, right with
        | rest, [] -> Some rest
        | x :: xs, y :: ys when x = y -> quotient xs ys
        | x :: xs, y :: _ when x < y ->
          Option.map (fun rest -> x :: rest) (quotient xs right)
        | _ -> None in
      let times monomial coefficient p =
        multiply t (P.singleton monomial coefficient) p in
      let basis = ref [] and pending = Queue.create () in
      let reduce p reasons =
        let fuel = ref 300 in
        let rec loop p reasons =
          t.poll ();
          decr fuel;
          if !fuel < 0 then raise Expansion_limit;
          let step = P.fold (fun monomial coefficient result ->
            t.poll ();
            match result with Some _ -> result | None ->
            List.find_map (fun (q, qm, qr) ->
              t.poll ();
              Option.map (fun multiplier ->
                add t p (negate (times multiplier coefficient q)),
                append_reasons qr reasons) (quotient monomial qm))
              !basis) p None in
          match step with
          | None -> p, List.sort_uniq
              (fun a b -> t.poll (); Int.compare a b) reasons
          | Some (p, reasons) -> loop p reasons in
        loop p reasons in
      let add_equation p reasons =
        let p, reasons = reduce p reasons in
        (* Dividing a derived relation by a nonunit can change its congruence
           modulo the divisor; reduction therefore uses only monic pivots. *)
        match leading p with
        | Some (monomial, coefficient) when
            Z.equal (Z.abs coefficient) Z.one ->
          if List.length !basis >= 48 then raise Expansion_limit;
          let p = if Z.sign coefficient < 0 then negate p else p in
          let entry = p, monomial, reasons in
          List.iter (fun other -> t.poll (); Queue.add (entry, other) pending)
            !basis;
          basis := entry :: !basis
        | _ -> () in
      add_equation divisor [];
      let aliases = Int_table.create 32 in
      let limit = Term.count t.terms in
      for term = 0 to limit - 1 do
        t.poll ();
        if Term.sort t.terms term = Term.Int then begin
          let root = representative term in
          match Int_table.find_opt aliases root with
          | None -> Int_table.add aliases root term
          | Some other ->
            (match explain term other with
            | None -> ()
            | Some reasons ->
              let p = add t (polynomial t term)
                  (negate (polynomial t other)) in
              if not (P.is_empty p) then
                add_equation p
                  (Array.fold_right (fun reason reasons ->
                    t.poll (); reason :: reasons) reasons []))
        end
      done;
      let target = polynomial t division.dividend in
      let check () =
        let p, reasons = reduce target [] in
        let value = if P.is_empty p then Some Z.zero
          else match P.bindings p with [[], value] -> Some value | _ -> None
        in
        match value with
        | Some value when Z.sign value >= 0 &&
            (match model division.remainder with
            | Some actual -> not (Z.equal actual value) | None -> true) ->
          let value = integer t value in
          let negative = minus t (int t 0) value in
          let guard = builtin t "or" Term.Bool
              [|compare t ">" division.divisor value;
                compare t "<" division.divisor negative|] in
          emit t reasons [guard] (equal t division.remainder value);
          true
        | _ -> false in
      let rec lcm left right =
        t.poll ();
        match left, right with
        | [], rest | rest, [] -> rest
        | x :: xs, y :: ys when x = y -> x :: lcm xs ys
        | x :: xs, y :: _ when x < y -> x :: lcm xs right
        | _, y :: ys -> y :: lcm left ys in
      let fuel = ref 128 and found = ref (check ()) in
      while not !found && !fuel > 0 && not (Queue.is_empty pending) do
        t.poll ();
        decr fuel;
        let (p, pm, pr), (q, qm, qr) = Queue.take pending in
        let common = lcm pm qm in
        if List.length common < List.length pm + List.length qm then
          match quotient common pm, quotient common qm with
          | Some left, Some right ->
            add_equation
              (add t (times left Z.one p) (negate (times right Z.one q)))
              (append_reasons pr qr);
            found := check ()
          | _ -> ()
      done
    end
  with Expansion_limit -> ()) divisions

let partitioned_modular_equalities t ~integer:model ~representative ~explain =
  if t.moduli <> [] then
  let fixed divisor = match Term.Unsafe.node t.terms divisor with
    | Term.Integer value when not (Z.is_zero value) -> Some value
    | _ -> None in
  let small, targets = List.fold_left (fun (small, targets) term ->
    t.poll ();
    match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, [|dividend; divisor|]) ->
      (match (Term.Unsafe.signature t.terms symbol).kind, fixed divisor with
      | Term.Builtin ("div" | "mod" as op), Some divisor ->
        let divisor = Z.abs divisor in
        let small = match Z.to_int_opt divisor with
          | Some value when value >= 2 && value <= 16 ->
            if List.mem value small then small else value :: small
          | _ -> small in
        let targets = if op = "mod" && Z.compare divisor (Z.of_int 4) > 0
            && List.compare_length_with targets 8 <= 0
          then (term, dividend, divisor) :: targets else targets in
        small, targets
      | _ -> small, targets)
    | _ -> small, targets) ([], []) t.modular_integers in
  let small = List.sort (fun a b -> t.poll (); Int.compare a b) small in
  if small <> [] && targets <> []
      && List.compare_length_with targets 8 <= 0 then
  let exception Skip in
  let remaining = ref 2000000 in
  let poll () =
    t.poll ();
    if !remaining = 0 then raise Skip;
    decr remaining in
  try
    let smallest = List.fold_left (fun current (_, _, modulus) ->
      poll (); if Z.compare modulus current < 0 then modulus else current)
      (Z.of_int 17) targets in
    let small = List.filter (fun value ->
      poll (); Z.compare (Z.of_int value) smallest < 0) small in
    if small = [] then raise Skip;
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    let partition = List.fold_left (fun a b ->
      poll ();
      let value = a / gcd a b * b in
      if value > 16 then raise Skip;
      value) 1 small in
    let partition = Z.of_int partition in
    let aliases = Int_table.create 32 in
    let priority term = match Term.Unsafe.node t.terms term with
      | Term.Integer _ -> 0
      | Term.Apply (symbol, _) ->
        (match (Term.Unsafe.signature t.terms symbol).kind with
        | Term.Builtin ("+" | "-" | "*" | "div" | "mod") -> 1
        | _ -> 2)
      | _ -> 2 in
    List.iter (fun term ->
      poll ();
      let root = representative term in
      match Int_table.find_opt aliases root with
      | Some other when priority other < priority term
          || (priority other = priority term && other < term) -> ()
      | _ -> Int_table.replace aliases root term) t.modular_integers;
    let seen = Int_table.create 32 and variables = ref [] in
    let rec visit term =
      poll ();
      if not (Int_table.mem seen term) then begin
        Int_table.add seen term ();
        match Term.Unsafe.node t.terms term with
        | Term.Apply (symbol, [||]) when
            (Term.Unsafe.signature t.terms symbol).kind = Term.Uninterpreted
            && Term.sort t.terms term = Term.Int ->
          variables := term :: !variables
        | Term.Apply (_, arguments) -> Array.iter visit arguments
        | _ -> ()
      end in
    List.iter (fun (_, dividend, _) -> visit dividend) targets;
    let variables = !variables in
    if List.compare_length_with variables 4 > 0 then raise Skip;
    List.iter (fun variable ->
      let size = match Z.to_int_opt partition with
        | Some size -> size | None -> raise Skip in
      let proofs = Int_table.create 8 in
      for selected = 0 to size - 1 do
        poll ();
        let selection = selected in
        let selected = Z.of_int selected in
        let cache = Hashtbl.create 64 and active = Int_table.create 32 in
        let append left right = List.fold_left (fun result reason ->
            poll (); reason :: result) right left in
        let rec expand sample term modulus =
          poll ();
          let residue value = snd (Z.div_rem ~poll value modulus) in
          let add_term monomial coefficient p =
            poll ();
            let coefficient = match P.find_opt monomial p with
              | Some old -> residue (Z.add ~poll old coefficient)
              | None -> residue coefficient in
            if Z.is_zero coefficient then P.remove monomial p else begin
              if List.length monomial > 128 then raise Skip;
              let p = P.add monomial coefficient p in
              if P.cardinal p > 256 then raise Skip;
              p
            end in
          let constant value = add_term [] value P.empty in
          let add left right = P.fold add_term left right in
          let negate p = P.map (fun value -> poll (); residue (Z.neg value)) p
          in
          let multiply left right =
            if P.cardinal left * P.cardinal right > 8192 then raise Skip;
            P.fold (fun xs a p -> P.fold (fun ys b p ->
              let monomial = List.merge (fun a b -> poll (); Int.compare a b)
                  xs ys in
              add_term monomial (Z.mul ~poll a b) p) right p) left P.empty in
          (* The selected variable atom denotes its private quotient. *)
          let atom () =
            if term = variable then
              (if sample = 0 then
                add_term [term] partition (constant selected)
              else constant (Z.add ~poll selected
                  (Z.mul ~poll partition (Z.of_int (sample - 1))))), []
            else add_term [term] Z.one P.empty, [] in
          let key = sample, term, modulus in
          if term = variable || Int_table.mem active term then atom () else
          match Hashtbl.find_opt cache key with
          | Some result -> result
          | None ->
            Int_table.add active term ();
            let aliased () =
              match Int_table.find_opt aliases (representative term) with
              | Some other when other <> term ->
                (match explain term other with
                | Some reasons ->
                  let p, why = expand sample other modulus in
                  p, Array.fold_right (fun reason why ->
                    poll (); reason :: why) reasons why
                | None -> atom ())
              | _ -> atom () in
            let combine op identity arguments =
              Array.fold_left (fun (p, why) argument ->
                let q, reasons = expand sample argument modulus in
                op p q, append reasons why) (identity, []) arguments in
            let result = match Term.Unsafe.node t.terms term with
              | Term.Integer value -> constant value, []
              | Term.Apply (symbol, arguments) ->
                (match (Term.Unsafe.signature t.terms symbol).kind,
                       arguments with
                | Term.Builtin "+", _ -> combine add P.empty arguments
                | Term.Builtin "*", _ ->
                  combine multiply (constant Z.one) arguments
                | Term.Builtin "-", [|argument|] ->
                  let p, why = expand sample argument modulus in negate p, why
                | Term.Builtin "-", _ when Array.length arguments > 1 ->
                  let p, why = expand sample arguments.(0) modulus in
                  let p = ref p and why = ref why in
                  for index = 1 to Array.length arguments - 1 do
                    let q, reasons = expand sample arguments.(index) modulus in
                    p := add !p (negate q); why := append reasons !why
                  done;
                  !p, !why
                | Term.Builtin "div", [|dividend; divisor|] ->
                  (match fixed divisor with
                  | None -> aliased ()
                  | Some divisor ->
                    (* Preserve quotient residues by lifting the modulus. *)
                    let p, why = expand sample dividend
                        (Z.mul ~poll modulus (Z.abs divisor)) in
                    let p = P.fold (fun monomial coefficient p ->
                      let quotient, remainder = Z.div_rem ~poll coefficient
                          divisor in
                      if monomial <> [] && not (Z.is_zero remainder)
                      then raise Skip;
                      add_term monomial quotient p) p P.empty in
                    p, why)
                | Term.Builtin "mod", [|dividend; divisor|] ->
                  (match fixed divisor with
                  | None -> aliased ()
                  | Some divisor ->
                    let divisor = Z.abs divisor in
                    if Z.is_zero (snd (Z.div_rem ~poll divisor modulus))
                    then expand sample dividend modulus else
                    let p, why = expand sample dividend divisor in
                    if P.exists (fun monomial _ -> poll (); monomial <> []) p
                    then raise Skip;
                    constant (Option.value (P.find_opt [] p) ~default:Z.zero),
                    why)
                | _ -> aliased ())
              | _ -> raise Skip in
            Int_table.remove active term;
            Hashtbl.add cache key result;
            result in
        List.iter (fun (remainder, dividend, modulus) ->
          try
            let constant_value p =
              if P.exists (fun monomial _ -> poll (); monomial <> []) p
              then None
              else Some (Option.value (P.find_opt [] p) ~default:Z.zero) in
            let first, _ = expand 1 dividend modulus in
            let second, _ = expand 2 dividend modulus in
            (match constant_value first, constant_value second with
            | Some first, Some second when not (Z.equal first second) ->
              raise Skip
            | _ -> ());
            let p, reasons = expand 0 dividend modulus in
            if not (P.exists (fun monomial _ -> poll (); monomial <> []) p)
            then begin
              let value = Option.value (P.find_opt [] p) ~default:Z.zero in
              if (match model remainder with
                  | Some old -> not (Z.equal old value) | None -> true)
              then begin
                let cases = match Int_table.find_opt proofs remainder with
                  | Some cases -> cases
                  | None ->
                    let cases = Array.make size None in
                    Int_table.add proofs remainder cases;
                    cases in
                cases.(selection) <- Some (value, reasons)
              end
            end
          with Skip -> Int_table.clear active) targets
      done;
      Int_table.iter (fun remainder cases ->
        let covered = Array.make size false in
        for modulus = 1 to size do
          poll ();
          if size mod modulus = 0 then
            for selected = 0 to modulus - 1 do
              poll ();
              if not covered.(selected) then begin
                let proof = ref None and valid = ref true in
                let index = ref selected in
                while !index < size do
                  poll ();
                  (match cases.(!index) with
                  | None -> valid := false
                  | Some (value, reasons) ->
                    (match !proof with
                    | None -> proof := Some (value, reasons)
                    | Some (other, why) when Z.equal other value ->
                      proof := Some (value,
                        List.fold_left (fun why reason ->
                          poll (); reason :: why) why reasons)
                    | _ -> valid := false));
                  index := !index + modulus
                done;
                if !valid then match !proof with
                | Some (value, reasons) ->
                  let condition = equal t
                      (builtin t "mod" Term.Int
                         [|variable; int t modulus|]) (int t selected) in
                  emit t reasons [condition]
                    (equal t remainder (integer t value));
                  let index = ref selected in
                  while !index < size do
                    poll (); covered.(!index) <- true;
                    index := !index + modulus
                  done
                | None -> ()
              end
            done
        done) proofs) variables
  with Skip -> ()

let ring_equalities t ~integer:model ~representative ~explain
    products divisions =
  if products = [] then (fun () -> ()) else try
    let limit = Term.count t.terms in
    let aliases = Hashtbl.create 64 in
    let priority term = match Term.Unsafe.node t.terms term with
      | Term.Integer _ -> 0
      | Term.Apply (symbol, _) ->
        (match (Term.Unsafe.signature t.terms symbol).kind with
        | Term.Builtin ("+" | "-" | "*") -> 2
        | _ -> 1)
      | _ -> 2 in
    for term = 0 to limit - 1 do
      t.poll ();
      if Term.sort t.terms term = Term.Int then begin
        let root = representative term in
        match Hashtbl.find_opt aliases root with
        | Some other when priority other <= priority term -> ()
        | _ -> Hashtbl.replace aliases root term
      end
    done;
    let why = ref [] in
    let alias term =
      let other = match Hashtbl.find_opt aliases (representative term) with
        | Some other -> other | None -> term in
      if other = term then term else match explain term other with
      | None -> term
      | Some reasons -> why := Array.to_list reasons @ !why; other in
    let expand term =
      P.fold (fun monomial coefficient acc ->
          let value = List.fold_left (fun acc term ->
              let term = alias term in
              let value = match Term.Unsafe.node t.terms term with
                | Term.Integer number -> constant number
                | _ -> P.singleton [term] Z.one in
              multiply t acc value) (constant coefficient) monomial in
          add t acc value) (polynomial t term) P.empty in
    let difference left right =
      add t (expand left) (negate (expand right)) in
    let ordering a b =
      let c = Int.compare (List.length a) (List.length b) in
      if c = 0 then compare_monomials a b else c in
    let leading p = P.fold (fun m c result -> match result with
        | Some (old, _) when ordering old m >= 0 -> result
        | _ -> Some (m, c)) p None in
    let primitive p =
      let divisor = P.fold (fun _ c acc -> Z.gcd ~poll:t.poll c acc)
          p Z.zero in
      if Z.is_zero divisor || Z.equal divisor Z.one then p
      else P.map (fun c -> fst (Z.quot_rem ~poll:t.poll c divisor)) p in
    let scale c p = P.map (Z.mul ~poll:t.poll c) p in
    let rec quotient a b = match a, b with
      | rest, [] -> Some rest
      | x :: xs, y :: ys when x = y -> quotient xs ys
      | x :: xs, y :: _ when x < y ->
        Option.map (fun rest -> x :: rest) (quotient xs b)
      | _ -> None in
    let times monomial p = multiply t (P.singleton monomial Z.one) p in
    (* Each polynomial is zero under its explanation literals. *)
    let basis = ref [] and pending = Queue.create () in
    let reduce polynomial reasons =
      let fuel = ref 300 in
      let rec loop p reasons =
        decr fuel;
        if !fuel <= 0 then raise Expansion_limit;
        t.poll ();
        let reduction = P.fold (fun monomial coefficient result ->
          match result with Some _ -> result | None ->
          List.find_map (fun (q, qm, qc, qr) ->
            Option.map (fun multiplier ->
              primitive (add t (scale qc p)
                  (negate (scale coefficient (times multiplier q)))),
              qr @ reasons) (quotient monomial qm)) !basis) p None in
        match reduction with
        | None -> p, List.sort_uniq Int.compare reasons
        | Some (p, reasons) -> loop p reasons in
      loop polynomial reasons in
    let order_basis = ref [] in
    let reduce_order polynomial reasons =
      let fuel = ref 300 in
      let rec loop p reasons =
        decr fuel;
        if !fuel <= 0 then raise Expansion_limit;
        t.poll ();
        let reduction = P.fold (fun monomial coefficient result ->
          match result with Some _ -> result | None ->
          List.find_map (fun (q, qm, qc, qr) ->
            Option.map (fun multiplier ->
              let magnitude = Z.abs qc in
              let coefficient = if Z.sign qc < 0 then Z.neg coefficient
                else coefficient in
              primitive (add t (scale magnitude p)
                (negate (scale coefficient (times multiplier q)))),
              qr @ reasons) (quotient monomial qm)) !order_basis) p None in
        match reduction with
        | None -> p, List.sort_uniq Int.compare reasons
        | Some (p, reasons) -> loop p reasons in
      loop polynomial reasons in
    let check_orderings () =
      if t.ordering_enabled then begin
      order_basis := List.map (fun (q, qm, qc, qr) ->
        let isolated = P.fold (fun monomial coefficient result ->
          match monomial with
          | [subject] when P.for_all (fun other _ ->
              other = monomial || not (List.mem subject other)) q ->
            (match result with
            | Some (previous, _) when previous < subject -> result
            | _ -> Some (subject, coefficient))
          | _ -> result) q None in
        match isolated with
        | Some (subject, coefficient) -> q, [subject], coefficient, qr
        | None -> q, qm, qc, qr) !basis;
      let rec number term =
        t.poll ();
        match model term with
        | Some value -> Some value
        | None ->
          (match Term.Unsafe.node t.terms term with
          | Term.Integer value -> Some value
          | Term.Apply (symbol, arguments) ->
            let values = Array.map number arguments in
            if Array.exists Option.is_none values then None else
            let values = Array.map Option.get values in
            (match (Term.Unsafe.signature t.terms symbol).kind with
            | Term.Builtin "+" -> Some (Array.fold_left
                (Z.add ~poll:t.poll) Z.zero values)
            | Term.Builtin "*" -> Some (Array.fold_left
                (Z.mul ~poll:t.poll) Z.one values)
            | Term.Builtin "-" when Array.length values = 1 ->
              Some (Z.neg values.(0))
            | Term.Builtin "-" when Array.length values > 1 ->
              let result = ref values.(0) in
              for index = 1 to Array.length values - 1 do
                result := Z.sub ~poll:t.poll !result values.(index)
              done;
              Some !result
            | _ -> None)
          | _ -> None) in
      let rec truth term =
        t.poll ();
        match Term.Unsafe.node t.terms term with
        | Term.Boolean value -> Some value
        | Term.Apply (symbol, [|argument|]) when
            (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "not" ->
          Option.map not (truth argument)
        | Term.Apply (symbol, [|left; right|])
            when Term.sort t.terms left = Term.Int ->
          (match number left, number right with
          | Some left, Some right ->
            let order = Z.compare left right in
            (match (Term.Unsafe.signature t.terms symbol).kind with
            | Term.Builtin "=" -> Some (order = 0)
            | Term.Builtin "<" -> Some (order < 0)
            | Term.Builtin "<=" -> Some (order <= 0)
            | Term.Builtin ">" -> Some (order > 0)
            | Term.Builtin ">=" -> Some (order >= 0)
            | _ -> None)
          | _ -> None)
        | _ -> None in
      let violated reasons premises conclusion =
        if truth conclusion = Some false
            && List.for_all (fun premise -> truth premise = Some true) premises
        then emit t reasons premises conclusion in
      let shifted_sign source op p reasons =
        let factors = P.fold (fun monomial _ factors ->
          List.sort_uniq Int.compare (monomial @ factors)) p [] in
        List.iter (fun factor ->
          let group, rest = P.fold (fun monomial coefficient (group, rest) ->
            t.poll ();
            match quotient monomial [factor] with
            | Some monomial ->
              add_term t monomial coefficient group, rest
            | None -> group, add_term t monomial coefficient rest)
              p (P.empty, P.empty) in
          if P.cardinal group >= 2 && P.for_all (fun monomial _ ->
              List.length monomial <= 1) group
              && P.for_all (fun monomial _ ->
                List.length monomial <= 1) rest then begin
            let offset = Option.value (P.find_opt [] rest) ~default:Z.zero in
            let positive = (op = ">" && Z.sign offset <= 0)
              || (op = ">=" && Z.sign offset < 0)
              || (op = "=" && Z.sign offset < 0) in
            let negative = (op = "<" && Z.sign offset >= 0)
              || (op = "<=" && Z.sign offset > 0)
              || (op = "=" && Z.sign offset > 0) in
            if positive || negative then begin
              let zero = int t 0 in
              let guards = P.fold (fun monomial coefficient guards ->
                t.poll ();
                match monomial with
                | [] -> guards
                | [term] ->
                  let op = if (Z.sign coefficient > 0) = positive
                    then "<=" else ">=" in
                  compare t op term zero :: guards
                | _ -> raise Expansion_limit) rest [] in
              let reduced = term_of_polynomial t group in
              let direct, reverse = if positive then ">", "<" else "<", ">" in
              violated reasons
                (source :: compare t ">=" factor zero :: guards)
                (compare t direct reduced zero);
              violated reasons
                (source :: compare t "<=" factor zero :: guards)
                (compare t reverse reduced zero)
            end
          end) factors in
      let comparisons = Int_table.fold (fun term () comparisons ->
          t.poll (); term :: comparisons) t.comparison_inputs []
          |> List.sort Int.compare in
      if List.compare_length_with comparisons 64 <= 0 then
      List.iter (fun term ->
        t.poll ();
        match Term.Unsafe.node t.terms term with
        | Term.Apply (symbol, [|left; right|]) when
            Term.sort t.terms left = Term.Int ->
          (match (Term.Unsafe.signature t.terms symbol).kind with
          | Term.Builtin ("=" | "<" | "<=" | ">" | ">=" as op) ->
            (try
              why := [];
              let p = difference left right in
              if P.cardinal p <= 16 && P.exists (fun monomial _ ->
                    List.length monomial > 1) p then begin
                let reduced, reasons = reduce_order p !why in
                shifted_sign term op reduced reasons;
                let reverse = match op with
                  | "<" -> Some ">=" | "<=" -> Some ">"
                  | ">" -> Some "<=" | ">=" -> Some "<"
                  | _ -> None in
                Option.iter (fun op ->
                  shifted_sign (not_ t term) op reduced reasons) reverse
              end
            with Expansion_limit -> ())
          | _ -> ())
        | _ -> ()) comparisons
      end in
    let add_equation polynomial reasons =
      let p, reasons = reduce polynomial reasons in
      match leading p with
      | None -> ()
      | Some (m, c) ->
        if List.length !basis >= 48 then raise Expansion_limit;
        let entry = p, m, c, reasons in
        List.iter (fun other -> Queue.add (entry, other) pending) !basis;
        basis := entry :: !basis in
    for term = 0 to limit - 1 do
      t.poll ();
      if Term.sort t.terms term = Term.Int then begin
        why := [];
        let other = alias term in
        if other <> term then
          let p = difference term other in
          if not (P.is_empty p) then add_equation p !why
      end
    done;
    List.iter (fun d ->
        if Option.fold ~none:false ~some:(fun z -> not (Z.is_zero z))
             (model d.divisor) then begin
          why := [t.boolean (not_ t (equal t d.divisor (int t 0)))];
          let x = expand d.dividend and y = expand d.divisor in
          let q = expand d.quotient and r = expand d.remainder in
          add_equation (add t x (negate (add t (multiply t y q) r))) !why
        end)
      divisions;
    let halves = ref [] in
    for term = 0 to limit - 1 do
      t.poll ();
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, [|dividend; divisor|])
        when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "div" ->
        (match same ~representative ~explain divisor (int t 2) with
        | Some reasons when List.length !halves < 32 ->
          halves := (dividend, term, reasons) :: !halves
        | _ -> ())
      | _ -> ()
    done;
    List.iter (fun (left, lq, left_reasons) ->
        List.iter (fun (right, rq, right_reasons) ->
            if left <> right then begin
              why := left_reasons @ right_reasons;
              let offset = add t (difference right left)
                  (constant (Z.of_int (-1))) in
              let reduced, reasons = reduce offset !why in
              if P.is_empty reduced then begin
                let sum = plus t [|lq; rq|] in
                emit t reasons [] (equal t sum left);
                why := reasons;
                let equation = difference sum left in
                add_equation equation !why
              end
            end) !halves) !halves;
    let targets = ref [] in
    for term = 0 to limit - 1 do
      t.poll ();
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, [|left; right|])
        when (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "="
          && Term.sort t.terms left = Term.Int
          && representative left <> representative right
          && (match model left, model right with
              | Some x, Some y -> not (Z.equal x y) | _ -> false) ->
        why := [];
        let p = difference left right in
        targets := ((fun () -> term), p, !why) :: !targets
      | _ -> ()
    done;
    let check_targets () =
      List.iter (fun (target, p, reasons) ->
          let reduced, reasons = reduce p reasons in
          if P.is_empty reduced then emit t reasons [] (target ())) !targets in
    check_targets ();
    let fuel = ref 128 in
    while !fuel > 0 && not (Queue.is_empty pending) do
      decr fuel;
      let (p, pm, pc, pr), (q, qm, qc, qr) = Queue.take pending in
      let rec lcm a b = match a, b with
        | [], rest | rest, [] -> rest
        | x :: xs, y :: ys when x = y -> x :: lcm xs ys
        | x :: xs, y :: _ when x < y -> x :: lcm xs b
        | _, y :: ys -> y :: lcm a ys in
      let common = lcm pm qm in
      if List.length common < List.length pm + List.length qm then begin
        match quotient common pm, quotient common qm with
        | Some a, Some b ->
          let p = add t (scale qc (times a p))
              (negate (scale pc (times b q))) in
          let old = List.length !basis in
          add_equation p (pr @ qr);
          if List.length !basis <> old then check_targets ()
        | _ -> ()
      end
    done;
    check_orderings ();
    fun () ->
      try
        targets := [];
        if List.compare_length_with products 16 <= 0 then begin
          let rec pairs = function
            | [] -> ()
            | left :: rest ->
              List.iter (fun right ->
                List.iter (fun negative ->
                  t.poll ();
                  match model left.term, model right.term with
                  | Some a, Some b when not
                      (Z.equal a (if negative then Z.neg b else b)) ->
                    why := [];
                    let lhs = expand left.term
                    and rhs = expand right.term in
                    let p = add t lhs (if negative then rhs else negate rhs) in
                    let target () =
                      let rhs = if negative then minus t (int t 0) right.term
                        else right.term in
                      equal t left.term rhs in
                    targets := (target, p, !why) :: !targets
                  | _ -> ()) [false; true]) rest;
              pairs rest in
          pairs products
        end;
        check_targets ()
      with Expansion_limit -> ()
  with Expansion_limit -> (fun () -> ())

let queue_comparison t term left right =
  let compound term = match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, _) ->
      (match (Term.Unsafe.signature t.terms symbol).kind with
      | Term.Builtin ("+" | "-" | "*") -> true
      | _ -> false)
    | _ -> false in
  if compound left || compound right then Queue.add term t.comparisons

let ite_modulus t left right =
  let shifted term = match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, [|base; offset|]) when
        (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "-" ->
      (match Term.Unsafe.node t.terms offset with
      | Term.Integer offset -> base, Z.neg offset
      | _ -> term, Z.zero)
    | _ -> term, Z.zero in
  let left, a = shifted left and right, b = shifted right in
  if left <> right then None else
  let modulus = Z.abs (Z.sub ~poll:t.poll a b) in
  if Z.compare modulus Z.one > 0 then Some modulus else None

let register_modular_term t term =
  if not (Int_table.mem t.modular_registered term) then begin
    let modulus = if t.moduli <> [] then None else
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, [|_; left; right|]) when
          (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "ite"
          && Term.sort t.terms term = Term.Int ->
        ite_modulus t left right
      | _ -> None in
    Int_table.add t.modular_registered term ();
    (match modulus with
    | None -> ()
    | Some modulus ->
      if not (List.exists (Z.equal modulus) t.inferred_moduli)
         && List.length t.inferred_moduli < 8 then
        t.inferred_moduli <- modulus :: t.inferred_moduli);
    if Term.sort t.terms term = Term.Int then
      t.modular_integers <- term :: t.modular_integers;
    (match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, [|left; right|]) ->
      (match (Term.Unsafe.signature t.terms symbol).kind with
      | Term.Builtin "mod" ->
        (match Term.Unsafe.node t.terms right with
        | Term.Integer divisor when not (Z.is_zero divisor) ->
          let modulus = Z.abs divisor in
          if not (List.exists (Z.equal modulus) t.moduli)
             && List.length t.moduli < 8
          then t.moduli <- modulus :: t.moduli
        | _ -> ())
      | Term.Builtin "=" when Term.sort t.terms left = Term.Int ->
        if t.ordering_enabled && not t.emitting
        then Int_table.replace t.comparison_inputs term ();
        queue_comparison t term left right;
        t.modular_targets <- (term, left, right) :: t.modular_targets
      | Term.Builtin ("<=" | "<" | ">=" | ">" as op)
        when Term.sort t.terms left = Term.Int ->
        if t.ordering_enabled && not t.emitting
        then Int_table.replace t.comparison_inputs term ();
        queue_comparison t term left right;
        if Term.count t.terms <= 4096 then
          t.linear_comparisons <- term :: t.linear_comparisons;
        t.linear_comparison_count <- t.linear_comparison_count + 1;
        let bound term upper value =
          let old = Option.value (Int_table.find_opt t.interval_bounds term)
              ~default:[] in
          Int_table.replace t.interval_bounds term ((upper, value) :: old) in
        (match Term.Unsafe.node t.terms left, Term.Unsafe.node t.terms right, op with
        | _, Term.Integer value, "<=" | Term.Integer value, _, ">=" ->
          bound (if op = "<=" then left else right) true value
        | _, Term.Integer value, "<" | Term.Integer value, _, ">" ->
          bound (if op = "<" then left else right) true (Z.sub value Z.one)
        | _, Term.Integer value, ">=" | Term.Integer value, _, "<=" ->
          bound (if op = ">=" then left else right) false value
        | _, Term.Integer value, ">" | Term.Integer value, _, "<" ->
          bound (if op = ">" then left else right) false (Z.add value Z.one)
        | _ -> ());
        (match Term.Unsafe.node t.terms left, Term.Unsafe.node t.terms right, op with
        | Term.Integer lower, _, "<=" | _, Term.Integer lower, ">=" ->
          let bounded = if op = "<=" then right else left in
          t.modular_bounds <- (bounded, lower) :: t.modular_bounds
        | Term.Integer lower, _, "<" | _, Term.Integer lower, ">" ->
          let bounded = if op = "<" then right else left in
          t.modular_bounds <- (bounded, Z.add lower Z.one) :: t.modular_bounds
        | _ -> ())
      | _ -> ())
    | _ -> ())
  end

let has_moduli t = t.moduli <> [] || t.inferred_moduli <> []

let modular_equalities ?(requests = []) ?(requests_only = false)
    ?(affine_requests = false) t
    ~integer:model ~representative ~explain =
  let inferred = t.moduli = [] in
  let moduli = if inferred then t.inferred_moduli else t.moduli in
  let targets =
    if moduli = [] || requests_only then [] else
      List.filter (fun (_, left, right) ->
          representative left <> representative right
          && (inferred
              || match model left, model right with
                 | Some x, Some y -> not (Z.equal x y) | _ -> false))
        t.modular_targets in
  let targets = List.map (fun (term, left, right) ->
      Some term, left, right, inferred) targets in
  let requests = if moduli = [] then [] else
    List.filter_map (fun (left, right) ->
      t.poll ();
      if representative left = representative right then None
      else Some (None, left, right, true)) requests in
  let targets = requests @ targets in
  if targets <> [] then begin
    let aliases =
      Int_table.create (max 64 (List.length t.modular_integers / 2)) in
    let priority term = match Term.Unsafe.node t.terms term with
      | Term.Integer _ -> 0
      | Term.Apply (symbol, _) ->
        (match (Term.Unsafe.signature t.terms symbol).kind with
        | Term.Builtin ("+" | "-" | "*" | "mod" | "ite") -> 1
        | _ -> 2)
      | _ -> 2 in
    List.iter (fun term ->
        t.poll ();
        let root = representative term in
        match Int_table.find_opt aliases root with
        | Some other when priority other < priority term
            || (priority other = priority term && other < term) -> ()
        | _ -> Int_table.replace aliases root term) t.modular_integers;
    let lower_bounds = Int_table.create 32 in
    List.iter (fun (term, lower) ->
        let root = representative term in
        let old = Option.value (Int_table.find_opt lower_bounds root)
            ~default:[] in
        if not (List.exists (Z.equal lower) old) then
          Int_table.replace lower_bounds root (lower :: old)) t.modular_bounds;
    let module P = Modular_polynomial in
    List.iter (fun modulus ->
      let residue z = snd (Z.div_rem ~poll:t.poll z modulus) in
      let add_term monomial coefficient polynomial =
        t.poll ();
        if List.length monomial > 16 then raise Expansion_limit;
        let coefficient = match P.find_opt monomial polynomial with
          | None -> residue coefficient
          | Some old -> residue (Z.add ~poll:t.poll old coefficient) in
        let polynomial =
          if Z.is_zero coefficient then P.remove monomial polynomial
          else P.add monomial coefficient polynomial in
        if P.cardinal polynomial > 256 then raise Expansion_limit;
        polynomial in
      let add a b = P.fold add_term a b in
      let negate p = P.map (fun c -> residue (Z.neg c)) p in
      let multiply a b =
        if P.cardinal a * P.cardinal b > 4096 then raise Expansion_limit;
        P.fold (fun xs a result ->
          P.fold (fun ys b result ->
            add_term (List.sort Stdlib.compare (xs @ ys))
              (Z.mul ~poll:t.poll a b) result) b result) a P.empty in
      let constant z =
        let z = residue z in
        if Z.is_zero z then P.empty else P.singleton [] z in
      let rec indicator condition =
        match Term.Unsafe.node t.terms condition with
        | Term.Boolean value -> constant (if value then Z.one else Z.zero)
        | Term.Apply (symbol, [|argument|]) when
            (Term.Unsafe.signature t.terms symbol).kind = Term.Builtin "not" ->
          add (constant Z.one) (negate (indicator argument))
        | _ -> P.singleton [Indicator condition] Z.one in
      let cache = Int_table.create 64 and active = Int_table.create 32 in
      let rec expand term =
        t.poll ();
        if Int_table.mem active term then
          P.singleton [Value term] Z.one, []
        else match Int_table.find_opt cache term with
        | Some value -> value
        | None ->
          Int_table.add active term ();
          let atom () = P.singleton [Value term] Z.one, [] in
          let aliased () =
            match Int_table.find_opt aliases (representative term) with
            | Some other when other <> term ->
              (match explain term other with
              | Some why ->
                let p, reasons = expand other in
                p, Array.to_list why @ reasons
              | None -> atom ())
            | _ -> atom () in
          let combine op identity arguments =
            Array.fold_left (fun (p, why) argument ->
                let q, reasons = expand argument in
                op p q, reasons @ why) (identity, []) arguments in
          let value = match Term.Unsafe.node t.terms term with
            | Term.Integer number -> constant number, []
            | Term.Apply (symbol, arguments) ->
              (match (Term.Unsafe.signature t.terms symbol).kind, arguments with
              | Term.Builtin "+", _ -> combine add P.empty arguments
              | Term.Builtin "*", _ ->
                combine multiply (constant Z.one) arguments
              | Term.Builtin "-", [|argument|] ->
                let p, why = expand argument in negate p, why
              | Term.Builtin "-", _ when Array.length arguments > 1 ->
                let p, why = expand arguments.(0) in
                Array.fold_left (fun (p, why) argument ->
                    let q, reasons = expand argument in
                    add p (negate q), reasons @ why) (p, why)
                  (Array.sub arguments 1 (Array.length arguments - 1))
              | Term.Builtin "mod", [|dividend; divisor|] ->
                (match Term.Unsafe.node t.terms divisor with
                | Term.Integer d when not (Z.is_zero d)
                    && Z.is_zero (residue d) -> expand dividend
                | _ -> aliased ())
              | Term.Builtin "ite", [|condition; left; right|] ->
                let p, pr = expand left and q, qr = expand right in
                let delta = add p (negate q) in
                if P.is_empty delta then p, pr @ qr
                else if P.cardinal delta = 1 && P.mem [] delta then
                  add q (multiply delta (indicator condition)), pr @ qr
                else atom ()
              | _ -> aliased ())
            | Term.Boolean _ | Term.Bits _ ->
              invalid_arg "Vox_smt_nonlinear: modular integer expression" in
          Int_table.remove active term;
          Int_table.add cache term value;
          value
      in
      List.iter (fun (term, left, right, requested) ->
        try
            let p, pr = expand left and q, qr = expand right in
            let offset =
              if P.equal Z.equal p q then Some (int t 0)
              else if requested && affine_requests && Option.is_none term then
                let delta = add p (negate q) in
                if P.cardinal delta <= 2 && P.for_all (fun monomial _ ->
                    t.poll ();
                    match monomial with
                    | [] -> true
                    | [Value atom] when atom <> left && atom <> right ->
                      (match Term.Unsafe.node t.terms atom with
                      | Term.Apply (symbol, [||]) ->
                        (Term.Unsafe.signature t.terms symbol).kind
                          = Term.Uninterpreted
                      | _ -> false)
                    | _ -> false) delta then
                  Some (plus t (Array.of_list
                    (P.fold (fun monomial value terms ->
                    t.poll ();
                    let twice = Z.mul ~poll:t.poll value (Z.of_int 2) in
                    let value = if Z.compare twice modulus > 0
                      then Z.sub ~poll:t.poll value modulus else value in
                    let value = integer t value in
                    let term = match monomial with
                      | [] -> value
                      | [Value atom] -> times t [|value; atom|]
                      | _ -> raise Expansion_limit in
                    term :: terms) delta [])))
                else None
              else None in
            match offset with
            | None -> ()
            | Some offset ->
              let original_right = right in
              let right = plus t [|right; offset|] in
              let term = match term with
                | Some term -> term | None -> equal t left right in
              let lower term = Option.value
                  (Int_table.find_opt lower_bounds (representative term))
                  ~default:[] in
              let candidates = lower left @ lower original_right in
              let candidates =
                if candidates = [] then [Z.zero] else candidates in
              let candidates = List.sort_uniq Z.compare candidates in
              List.iter (fun low ->
                let high = Z.sub (Z.add low modulus) Z.one in
                if requested || (match model left, model right with
                    | Some x, Some y ->
                      Z.compare low x <= 0 && Z.compare x high <= 0
                      && Z.compare low y <= 0 && Z.compare y high <= 0
                    | _ -> false) then
                emit t (pr @ qr)
                  [compare t ">=" left (integer t low);
                   compare t "<=" left (integer t high);
                   compare t ">=" right (integer t low);
                   compare t "<=" right (integer t high)] term)
                candidates
          with Expansion_limit -> Int_table.clear active)
        targets) moduli
  end

let request_modular_equalities ?(affine = false) t
    ~representative ~explain pairs =
  let started = t.revision in
  List.iter (fun (left, right) ->
    if Term.sort t.terms left <> Term.Int
      || Term.sort t.terms right <> Term.Int then
      invalid_arg "Vox_smt_nonlinear.request_modular_equalities") pairs;
  if pairs <> [] && has_moduli t then
    modular_equalities ~requests:pairs ~requests_only:true
      ~affine_requests:affine t
      ~integer:(fun _ -> None)
    ~representative ~explain;
  t.revision <> started

let product_intervals t model products =
  let factor_bounds factor =
    match Term.Unsafe.node t.terms factor with
    | Term.Integer value -> Some (value, value, [])
    | _ ->
      match model factor with
      | None -> None
      | Some value ->
        let bounds = Option.value (Int_table.find_opt t.interval_bounds factor)
            ~default:[] in
        let lower = ref None and upper = ref None in
        List.iter (fun (high, boundary) ->
          t.poll ();
          let valid = if high then Z.compare value boundary <= 0
            else Z.compare value boundary >= 0 in
          if valid then begin
            let target = if high then upper else lower in
            let better = match !target with
              | None -> true
              | Some old -> if high then Z.compare boundary old < 0
                  else Z.compare boundary old > 0 in
            if better then target := Some boundary
          end) bounds;
        match !lower, !upper with
        | Some lower, Some upper -> Some (lower, upper, [factor, lower, upper])
        | _ -> None in
  let minimum a b = if Z.compare a b <= 0 then a else b in
  let maximum a b = if Z.compare a b >= 0 then a else b in
  List.iter (fun product ->
    t.poll ();
    if Array.length product.factors <= 16 then begin
    let rec bounds index lower upper guards =
      if index = Array.length product.factors then Some (lower, upper, guards)
      else begin
        t.poll ();
        match factor_bounds product.factors.(index) with
        | None -> None
        | Some (low, high, premises) ->
          let a = Z.mul ~poll:t.poll lower low in
          let b = Z.mul ~poll:t.poll lower high in
          let c = Z.mul ~poll:t.poll upper low in
          let d = Z.mul ~poll:t.poll upper high in
          bounds (index + 1) (minimum (minimum a b) (minimum c d))
            (maximum (maximum a b) (maximum c d)) (premises @ guards)
      end in
    match model product.term with
    | None -> ()
    | Some value ->
      match bounds 0 Z.one Z.one [] with
      | None -> ()
      | Some (lower, upper, guards) ->
        let below = Z.compare value lower < 0 in
        let above = Z.compare value upper > 0 in
        if below || above then begin
          let guards = List.concat_map (fun (factor, low, high) ->
            t.poll ();
            [compare t ">=" factor (integer t low);
             compare t "<=" factor (integer t high)]) guards in
          if below then emit t [] guards
              (compare t ">=" product.term (integer t lower));
          if above then emit t [] guards
              (compare t "<=" product.term (integer t upper))
        end
    end) products

let square_secants t model products =
  List.iter (fun product ->
    if Array.length product.factors = 2
       && product.factors.(0) = product.factors.(1) then begin
      let x = product.factors.(0) in
      match model x, model product.term with
      | Some value, Some square ->
        let bounds = Option.value (Int_table.find_opt t.interval_bounds x)
            ~default:[] in
        let lower = ref None and upper = ref None in
        List.iter (fun (high, boundary) ->
          t.poll ();
          let valid = if high then Z.compare value boundary <= 0
            else Z.compare value boundary >= 0 in
          if valid then begin
            let target = if high then upper else lower in
            let better = match !target with
              | None -> true
              | Some old -> if high then Z.compare boundary old < 0
                  else Z.compare boundary old > 0 in
            if better then target := Some boundary
          end) bounds;
        (match !lower, !upper with
        | Some lower, Some upper when Z.compare lower upper <= 0 ->
          let slope = Z.add ~poll:t.poll lower upper in
          let intercept = Z.mul ~poll:t.poll lower upper in
          let expected = Z.sub ~poll:t.poll
              (Z.mul ~poll:t.poll slope value) intercept in
          if Z.compare square expected > 0 then begin
            let line = minus t (times t [|integer t slope; x|])
                (integer t intercept) in
            emit t [] [compare t ">=" x (integer t lower);
                       compare t "<=" x (integer t upper)]
              (compare t "<=" product.term line)
          end
        | _ -> ())
      | _ -> ()
    end) products

let propagate_products ?(affine_bounds = true) t ~integer:model ~representative ~explain =
  let started = t.revision in
  partitioned_modular_equalities t ~integer:model ~representative ~explain;
  residue_equalities t;
  modular_equalities t ~integer:model ~representative ~explain;
  let products = Hashtbl.fold (fun _ p xs -> p :: xs) t.products [] in
  List.iter (substitute t ~representative ~explain) products;
  multiply_equalities t ~representative ~explain products;
  product_intervals t model products;
  square_secants t model products;
  List.iter (integer_bounds t model) products;

  if affine_bounds then
    multiplied_bounds t model ~representative ~explain products;
  let rec pairs = function
    | [] -> ()
    | product :: rest ->
      List.iter (fun other ->
        power_order t model product other;
        product_difference t model ~representative ~explain product other)
        rest;
      pairs rest in
  pairs products;
  t.revision <> started

let extension_value t extension values =
  match extension.operator with
  | "int.pow2" -> Z.pow2 ~poll:t.poll values.(0)
  | "int.log2" -> Some (Z.log2 ~poll:t.poll values.(0))
  | "piand" -> Z.piand ~poll:t.poll values.(0) values.(1) values.(2)
  | _ -> invalid_arg "Vox_smt_nonlinear.extension_value"

let extension_axioms t extension =
  let zero = int t 0 and one = int t 1 and two = int t 2 in
  let value = extension.value and args = extension.operands in
  let bound op left right = compare t op left right in
  let imply premises conclusion = emit t [] premises conclusion in
  let modulo value = builtin t "mod" Term.Int [|value; two|] in
  imply [] (bound ">=" value zero);
  match extension.operator with
  | "int.pow2" ->
    let exponent = args.(0) in
    imply [bound "<" exponent zero] (equal t value zero);
    imply [equal t exponent zero] (equal t value one);
    imply [bound ">=" exponent zero] (bound ">" value exponent);
    imply [bound ">" exponent zero] (equal t (modulo value) zero)
  | "int.log2" ->
    let operand = args.(0) in
    imply [bound "<" operand two] (equal t value zero);
    imply [bound ">=" operand two] (bound ">" value zero);
    imply [bound ">" operand zero] (bound "<" value operand)
  | "piand" ->
    let width = args.(0) and left = args.(1) and right = args.(2) in
    let power = builtin t "int.pow2" Term.Int [|width|] in
    imply [bound "<=" width zero] (equal t value zero);
    imply [bound ">" width zero] (bound "<" value power);
    List.iter (fun (operand, other) ->
      imply [equal t operand zero] (equal t value zero);
      imply [bound ">=" operand zero] (bound "<=" value operand);
      imply [equal t operand one; bound ">" width zero]
        (equal t value (modulo other));
      imply [equal t (modulo operand) zero]
        (equal t (modulo value) zero);
      let range = [bound ">=" other zero; bound "<" other power;
                   bound ">" width zero] in
      imply (equal t operand (minus t power one) :: range)
        (equal t value other);
      imply (equal t operand other :: range) (equal t value other))
      [left, right; right, left]
  | _ -> invalid_arg "Vox_smt_nonlinear.extension_axioms"

let extension_pairs t extensions =
  let powers, logarithms = List.fold_left (fun (powers, logarithms) extension ->
    t.poll ();
    match extension.operator with
    | "int.pow2" -> extension :: powers, logarithms
    | "int.log2" -> powers, extension :: logarithms
    | _ -> powers, logarithms) ([], []) extensions in
  let rec pairs = function
    | [] -> ()
    | left :: rest ->
      t.poll ();
      List.iter (fun right ->
        t.poll ();
        let x = left.operands.(0) and y = right.operands.(0) in
        List.iter (fun (x, y, a, b) ->
          emit t [] [compare t "<=" x y] (compare t "<=" a b);
          if left.operator = "int.pow2" then
            emit t [] [compare t "<" x y; compare t ">=" y (int t 0)]
              (compare t "<" a b))
          [x, y, left.value, right.value; y, x, right.value, left.value]) rest;
      pairs rest in
  pairs powers;
  pairs logarithms

let quadratic_order t model products =
  if List.compare_length_with products 8 <= 0 then begin
    let scalar term =
      t.poll ();
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, [||]) ->
        (Term.Unsafe.signature t.terms symbol).kind = Term.Uninterpreted
      | _ -> false in
    let products = List.filter_map (fun product ->
      t.poll ();
      match product.factors with
      | [|a; b|] when scalar a && scalar b ->
        (match model product.term, model a, model b with
        | Some value, Some av, Some bv ->
          if Z.compare av bv <= 0 then Some (product.term, value, a, av, b, bv)
          else Some (product.term, value, b, bv, a, av)
        | _ -> None)
      | _ -> None) products in
    let zero = int t 0 and one = int t 1 in
    List.iter (fun (left, lv, a, av, b, bv) ->
      List.iter (fun (right, rv, x, xv, y, yv) ->
        t.poll ();
        if left <> right && Z.sign av >= 0 && Z.sign bv >= 0
            && Z.compare av xv <= 0 && Z.compare bv yv <= 0
        then begin
          let guards = [compare t ">=" a zero; compare t ">=" b zero] in
          if Z.compare lv rv > 0 then
            emit t [] (compare t "<=" a x :: compare t "<=" b y :: guards)
              (compare t "<=" left right);
          if Z.compare lv rv >= 0 && Z.sign xv > 0 && Z.sign yv > 0
              && (Z.compare av xv < 0 || Z.compare bv yv < 0)
          then begin
            let a_op, b_op = if Z.compare av xv < 0 then "<", "<="
              else "<=", "<" in
            emit t [] (compare t a_op a x :: compare t b_op b y ::
              compare t ">=" x one :: compare t ">=" y one :: guards)
              (compare t "<" left right)
          end
        end) products) products
  end

let quadratic_sum_bounds t model products =
  if List.compare_length_with products 8 <= 0 then begin
    let zero = int t 0 and one = int t 1 and two = int t 2 in
    let shifts = [0, 0, 0; 0, 1, 1; 1, 0, 1; 1, 1, 1] in
    let shifted term offset =
      if offset = 0 then term else plus t [|term; one|] in
    let shifted_value value offset =
      if offset = 0 then value else Z.add ~poll:t.poll value Z.one in
    List.iter (fun square ->
      match square.factors with
      | [|c; d|] when c = d ->
        (match model c, model square.term with
        | Some cv, Some square_value ->
          List.iter (fun product ->
            t.poll ();
            match product.factors with
            | [|a; b|] when product.term <> square.term ->
              (match model a, model b, model product.term with
              | Some av, Some bv, Some product_value ->
                List.iter (fun (sa, sb, sc) ->
                  t.poll ();
                  let cv = shifted_value cv sc in
                  let sum = Z.add ~poll:t.poll
                      (shifted_value av sa) (shifted_value bv sb) in
                  let twice = Z.add ~poll:t.poll cv cv in
                  if Z.sign cv >= 0 && Z.sign sum >= 0
                     && Z.compare sum twice <= 0 then begin
                    let lv = Z.add ~poll:t.poll product_value
                        (Z.add ~poll:t.poll
                          (if sa = 0 then Z.zero else bv)
                          (if sb = 0 then Z.zero else av)) in
                    let lv = if sa = 1 && sb = 1
                      then Z.add ~poll:t.poll lv Z.one else lv in
                    let rv = if sc = 0 then square_value
                      else Z.add ~poll:t.poll square_value
                        (Z.sub ~poll:t.poll twice Z.one) in
                    let strict = Z.sign cv > 0 && Z.compare sum twice < 0 in
                    if Z.compare lv rv > 0
                       || strict && Z.compare lv rv = 0 then begin
                      let lhs = ref [product.term] in
                      if sa = 1 then lhs := b :: !lhs;
                      if sb = 1 then lhs := a :: !lhs;
                      if sa = 1 && sb = 1 then lhs := one :: !lhs;
                      let rhs = if sc = 0 then square.term
                        else plus t [|square.term; times t [|two; c|]; one|] in
                      let sum = plus t [|shifted a sa; shifted b sb|] in
                      let point = shifted c sc in
                      emit t []
                        [compare t ">=" point
                           (if strict then one else zero);
                         compare t ">=" sum zero;
                         compare t (if strict then "<" else "<=") sum
                           (times t [|two; point|])]
                        (compare t (if strict then "<" else "<=")
                           (plus t (Array.of_list !lhs)) rhs)
                    end
                  end) shifts
              | _ -> ())
            | _ -> ()) products
        | _ -> ())
      | _ -> ()) products
  end

let quadratic_ratio_bounds t model products =
  if List.compare_length_with products 8 <= 0 then begin
    let zero = int t 0 and one = int t 1 and two = int t 2 in
    let scales = [1; 2; 3; 4; 8; 16] in
    List.iter (fun square ->
      match square.factors with
      | [|c; d|] when c = d ->
        (match model c, model square.term with
        | Some cv, Some square_value ->
          List.iter (fun product ->
            t.poll ();
            match product.factors with
            | [|a; b|] when product.term <> square.term ->
              (match model a, model b, model product.term with
              | Some av, Some bv, Some product_value ->
                List.iter (fun scale ->
                  t.poll ();
                  let k = Z.of_int scale and factor = int t scale in
                  let scaled value = Z.mul ~poll:t.poll k value in
                  let sum = Z.add ~poll:t.poll av bv in
                  if Z.sign av >= 0 && Z.sign bv >= 0 && Z.sign cv >= 0
                     && Z.compare (Z.add ~poll:t.poll cv cv) sum <= 0
                     && Z.compare av (scaled bv) <= 0
                     && Z.compare bv (scaled av) <= 0 then begin
                    let lhs = Z.mul ~poll:t.poll (Z.of_int (4 * scale))
                        square_value in
                    let rhs = Z.mul ~poll:t.poll
                        (Z.of_int ((scale + 1) * (scale + 1))) product_value in
                    if Z.compare lhs rhs > 0 then
                      emit t []
                        [compare t ">=" a zero; compare t ">=" b zero;
                         compare t ">=" c zero;
                         compare t "<=" (times t [|two; c|])
                           (plus t [|a; b|]);
                         compare t "<=" a (times t [|factor; b|]);
                         compare t "<=" b (times t [|factor; a|])]
                        (compare t "<="
                           (times t [|int t (4 * scale); square.term|])
                           (times t [|int t ((scale + 1) * (scale + 1));
                              product.term|]))
                  end;
                  if a = c || b = c then begin
                    let other, other_value = if a = c then b, bv else a, av in
                    let expected = Z.compare other_value (scaled cv) in
                    let actual = Z.compare product_value (scaled square_value) in
                    if Z.sign cv > 0 && actual <> expected then begin
                      let op = if expected < 0 then "<"
                        else if expected = 0 then "=" else ">" in
                      emit t []
                        [compare t ">=" c one;
                         compare t op other (times t [|factor; c|])]
                        (compare t op product.term
                           (times t [|factor; square.term|]))
                    end
                  end) scales
              | _ -> ())
            | _ -> ()) products
        | _ -> ())
      | _ -> ()) products
  end

let quadratic_envelopes t model products =
  if List.compare_length_with products 16 <= 0 then begin
    let zero = int t 0 and one = int t 1 and two = int t 2 in
    let scaled k term = times t [|integer t k; term|] in
    let ratios a x =
      let count = t.linear_comparison_count in
      let divisions = Hashtbl.length t.divisions in
      let previous = Hashtbl.find_opt t.envelope_ratios (a, x) in
      match previous with
      | Some (previous, old_divisions, ratios)
          when previous = count && old_divisions = divisions -> ratios
      | _ ->
        let found = ref (match previous with
          | Some (_, _, ratios) -> ratios | None -> []) in
        let retain p =
          let ps = Option.value (P.find_opt [a; a] p) ~default:Z.zero in
          let qs = Z.neg (Option.value (P.find_opt [x] p) ~default:Z.zero) in
          if List.compare_length_with !found 8 < 0
             && Z.sign ps > 0 && Z.compare qs ps >= 0
             && P.cardinal p = 2 then begin
            let gcd = Z.gcd ~poll:t.poll ps qs in
            let ps = fst (Z.div_rem ~poll:t.poll ps gcd) in
            let qs = fst (Z.div_rem ~poll:t.poll qs gcd) in
            if not (List.exists (fun (p, q) ->
                t.poll (); Z.equal p ps && Z.equal q qs) !found) then
              found := (ps, qs) :: !found
          end in
        if count <= 512 && divisions <= 16 then
          List.iter (fun comparison ->
            t.poll ();
            match Term.Unsafe.node t.terms comparison with
            | Term.Apply (symbol, [|left; right|]) ->
              (match (Term.Unsafe.signature t.terms symbol).kind with
              | Term.Builtin ("<" | "<=" | ">" | ">=" as op) ->
                (try
                  let p = add t (polynomial t left)
                      (negate (polynomial t right)) in
                  let p = if op = ">" || op = ">=" then negate p else p in
                  if P.mem [a; a] p then begin
                    retain p;
                    P.iter (fun monomial coefficient ->
                      t.poll ();
                      if Z.sign coefficient < 0 then
                        match monomial with
                        | [quotient] ->
                          let eliminate dividend divisor =
                            let rest = P.remove [quotient] p in
                            let rest = P.map (Z.mul ~poll:t.poll divisor) rest in
                            let upper = P.map (Z.mul ~poll:t.poll coefficient)
                                (polynomial t dividend) in
                            retain (add t rest upper) in
                          (match Term.Unsafe.node t.terms quotient with
                          | Term.Apply (symbol, [|dividend; divisor|]) ->
                            (match (Term.Unsafe.signature t.terms symbol).kind,
                              Term.Unsafe.node t.terms divisor with
                            | Term.Builtin "div", Term.Integer divisor
                                when Z.sign divisor > 0 ->
                              eliminate dividend divisor
                            | _ -> ())
                          | _ -> ());
                          Hashtbl.iter (fun _ division ->
                            t.poll ();
                            if division.quotient = quotient then
                              match Term.Unsafe.node t.terms division.divisor with
                              | Term.Integer divisor when Z.sign divisor > 0 ->
                                eliminate division.dividend divisor
                              | _ -> ()) t.divisions
                        | _ -> ()) p
                  end
                with Expansion_limit -> ())
              | _ -> ())
            | _ -> ()) t.linear_comparisons;
        Hashtbl.replace t.envelope_ratios (a, x) (count, divisions, !found);
        !found in
    List.iter (fun first ->
      t.poll ();
      match first.factors with
      | [|a; a'|] when a = a' ->
        (match model a, model first.term with
        | Some av, Some uv when Z.sign av > 0 ->
          List.iter (fun product ->
            t.poll ();
            match product.factors with
            | [|left; right|] when left = a || right = a ->
              let b = if left = a then right else left in
              (match model b, model product.term with
              | Some bv, Some pv ->
                Hashtbl.iter (fun _ division ->
                  t.poll ();
                  if division.divisor = a && division.quotient = b then
                    match model division.dividend with
                    | Some xv when Z.compare pv xv <= 0 ->
                      let x = division.dividend in
                      let sum = Z.add ~poll:t.poll av bv in
                      List.iter (fun square ->
                        t.poll ();
                        match square.factors with
                        | [|c; c'|] when c = c' ->
                          (match model c, model square.term with
                          | Some cv, Some wv when Z.sign cv >= 0
                              && Z.compare (Z.add ~poll:t.poll cv cv) sum <= 0 ->
                            let guards () =
                              [compare t ">=" a one;
                               compare t ">=" c zero;
                               compare t "<=" product.term x;
                               compare t "<=" (times t [|two; c|])
                                 (plus t [|a; b|])] in
                            if Z.compare (Z.sub ~poll:t.poll uv av) xv <= 0
                               && Z.compare xv (Z.add ~poll:t.poll uv
                                 (Z.add ~poll:t.poll av av)) <= 0
                               && Z.compare wv (Z.add ~poll:t.poll xv cv) > 0
                            then emit t []
                              (compare t "<="
                                 (minus t first.term a) x ::
                               compare t "<=" x
                                 (plus t [|first.term; times t [|two; a|]|])
                               :: guards ())
                              (compare t "<=" square.term (plus t [|x; c|]));
                            if Z.compare xv uv <= 0 then
                              List.iter (fun (p, q) ->
                                t.poll ();
                                let lhs = Z.mul ~poll:t.poll p uv in
                                let rhs = Z.mul ~poll:t.poll q xv in
                                if Z.compare lhs rhs <= 0 then begin
                                  let sum = Z.add ~poll:t.poll p q in
                                  let lower = Z.mul ~poll:t.poll (Z.of_int 4)
                                      (Z.mul ~poll:t.poll p q) in
                                  let upper = Z.mul ~poll:t.poll sum sum in
                                  if Z.compare (Z.mul ~poll:t.poll lower wv)
                                      (Z.mul ~poll:t.poll upper xv) > 0 then
                                    emit t []
                                      (compare t "<=" x first.term ::
                                       compare t "<=" (scaled p first.term)
                                         (scaled q x) :: guards ())
                                      (compare t "<=" (scaled lower square.term)
                                         (scaled upper x))
                                end) (ratios a x)
                          | _ -> ())
                        | _ -> ()) products
                    | _ -> ()) t.divisions
              | _ -> ())
            | _ -> ()) products
        | _ -> ())
      | _ -> ()) products
  end

let monomial_order t model products =
  let exception Skip in
  let scalar term =
    t.poll ();
    match Term.Unsafe.node t.terms term with
    | Term.Apply (symbol, [||]) ->
      (Term.Unsafe.signature t.terms symbol).kind = Term.Uninterpreted
    | _ -> false in
  if List.compare_length_with products 128 <= 0 then begin
    let products = List.filter_map (fun product ->
      t.poll ();
      try match P.bindings (polynomial t product.term) with
      | [monomial, coefficient]
          when Z.equal coefficient Z.one
               && List.for_all scalar monomial ->
        Some (product.term, monomial)
      | _ -> None
      with Expansion_limit -> None) products in
    if List.exists (fun (_, factors) -> List.length factors >= 3) products
    then begin
      let atoms = List.sort_uniq Int.compare
          (List.concat_map snd products) in
      if List.compare_length_with atoms 8 <= 0 then begin
        let number term = match model term with
          | Some value -> value | None -> raise Skip in
        let monomials = List.fold_left (fun map (term, factors) ->
          t.poll (); P.add factors term map) P.empty products in
        let residue = function
          | [term] -> Some term
          | factors -> P.find_opt factors monomials in
        let relations = Hashtbl.create 16 in
        Int_table.iter (fun term () ->
          t.poll ();
          match Term.Unsafe.node t.terms term with
          | Term.Apply (symbol, [|left; right|])
              when Term.sort t.terms left = Term.Int ->
            (match (Term.Unsafe.signature t.terms symbol).kind with
            | Term.Builtin ("<" | "<=" | ">" | ">=" as op) ->
              (try
                let p = add t (polynomial t left)
                    (negate (polynomial t right)) in
                match P.bindings p with
                | [a, ac; b, bc] when Z.equal ac (Z.neg bc) ->
                  let a, b = if Z.sign ac > 0 then a, b else b, a in
                  let a, b = if op = ">" || op = ">=" then b, a else a, b in
                  (match residue a, residue b with
                  | Some a, Some b ->
                    let old = Option.value
                        (Hashtbl.find_opt relations (a, b)) ~default:[] in
                    Hashtbl.replace relations (a, b) (term :: old)
                  | _ -> ())
                | _ -> ()
              with Expansion_limit -> ())
            | _ -> ())
          | _ -> ()) t.modular_registered;

        let zero = int t 0 in
        let nonnegative term = compare t ">=" term zero in
        let rec difference left right common xs ys =
          t.poll ();
          match left, right with
          | [], [] -> common, List.rev xs, List.rev ys
          | x :: left, [] -> difference left [] common (x :: xs) ys
          | [], y :: right -> difference [] right common xs (y :: ys)
          | x :: left, y :: right when x = y ->
            difference left right (x :: common) xs ys
          | x :: left, y :: _ when x < y ->
            difference left right common (x :: xs) ys
          | _, y :: right -> difference left right common xs (y :: ys) in
        let all_nonnegative factors = List.for_all (fun factor ->
            t.poll (); Z.sign (number factor) >= 0) factors in
        let ordered (left, xs) (right, ys) =
          t.poll ();
          if left <> right && (List.length xs >= 3 || List.length ys >= 3)
          then try
            let lv = number left and rv = number right in
            let actual = Z.compare lv rv in
            if actual > 0 && all_nonnegative xs && all_nonnegative ys
            then begin
              let common, a, b = difference xs ys [] [] [] in
              if common <> [] then begin
                match residue a, residue b with
                | Some x, Some y when Z.compare (number x) (number y) <= 0 ->
                  let guards = if List.length a = 1 && List.length b = 1
                    then [compare t "<=" x y]
                    else Option.value (Hashtbl.find_opt relations (x, y))
                        ~default:[] in
                  List.iter (fun guard ->
                    emit t [] (guard :: List.map nonnegative common)
                      (compare t "<=" left right)) guards
                | _ -> ()
              end
            end
          with Skip -> () in
        List.iter (fun (term, factors) ->
          if List.length factors >= 3 then try
            let rec parity factors result =
              t.poll ();
              match factors with
              | x :: y :: rest when x = y -> parity rest result
              | x :: rest -> parity rest (x :: result)
              | [] -> result in
            let odd = parity factors [] in
            let negative = ref false in
            let guards = List.map (fun factor ->
              if Z.sign (number factor) >= 0 then nonnegative factor
              else begin negative := not !negative;
                compare t "<=" factor zero end) odd in
            let value = number term in
            if (if !negative then Z.sign value > 0 else Z.sign value < 0)
            then emit t [] guards
                (compare t (if !negative then "<=" else ">=") term zero)
          with Skip -> ()) products;
        let products = products @ List.map (fun atom -> atom, [atom]) atoms in
        List.iter (fun left -> List.iter (ordered left) products) products
      end
    end
  end


let needs_reclamation t = t.reclaim_extensions

let refine_extension t model extension =
  let value = extension.value and args = extension.operands in
  let values = Array.map (fun term -> t.poll (); model term) args in
  if Array.exists Option.is_none values then false else
  let values = Array.map Option.get values in
  let expected = extension_value t extension values in
  let consistent = match expected, model value with
    | Some expected, Some actual -> Z.equal expected actual
    | _ -> false in
  if consistent then true else begin
    (if extension.operator = "piand" then
      match Z.to_int_opt values.(0) with
      | Some width when width > 0 && width <= 4096 ->
        let sort = Term.Bitvec width in
        let convert operand = builtin t
            ("int_to_bv:" ^ string_of_int width) sort [|operand|] in
        let bits = builtin t "bvand" sort
            [|convert args.(1); convert args.(2)|] in
        let result = builtin t "ubv_to_int" Term.Int [|bits|] in
        emit t [] [equal t args.(0) (integer t values.(0))]
          (equal t value result)
      | _ -> ());
    (match expected with
    | None -> t.reclaim_extensions <- true
    | Some expected ->
      let result = integer t expected in
      let premises = Array.to_list (Array.mapi (fun i argument ->
        equal t argument (integer t values.(i))) args) in
      emit t [] premises (equal t value result);
      if extension.operator <> "piand" then begin
        let point = integer t values.(0) in
        emit t [] [compare t "<=" args.(0) point] (compare t "<=" value result);
        emit t [] [compare t ">=" args.(0) point] (compare t ">=" value result)
      end);
    (if extension.operator = "int.log2" then
      match model value with
      | Some logarithm when Z.sign logarithm >= 0 ->
        (match Z.pow2 ~poll:t.poll logarithm with
        | None -> ()
        | Some lower ->
          let guard = [equal t value (integer t logarithm)] in
          if Z.sign logarithm > 0 then
            emit t [] guard (compare t ">=" args.(0) (integer t lower));
          let upper = Z.add ~poll:t.poll lower lower in
          emit t [] guard (compare t "<" args.(0) (integer t upper)))
      | _ -> ());
    false
  end

let candidate_model_matches t model =
  let exception Mismatch in
  let number term =
    t.poll ();
    match model term with Some value -> value | None -> raise Mismatch in
  let same actual expected =
    if not (Z.equal actual expected) then raise Mismatch in
  let zero_divisor = ref false in
  try
    Hashtbl.iter (fun _ product ->
      let expected = Array.fold_left (fun value factor ->
        Z.mul ~poll:t.poll value (number factor)) Z.one product.factors in
      same (number product.term) expected) t.products;
    Hashtbl.iter (fun _ division ->
      let x = number division.dividend and y = number division.divisor in
      let q = number division.quotient and r = number division.remainder in
      if Z.is_zero y then zero_divisor := true else begin
        let quotient, remainder = Z.div_rem ~poll:t.poll x y in
        same q quotient; same r remainder
      end) t.divisions;
    Int_table.iter (fun _ extension ->
      let values = Array.map number extension.operands in
      match extension_value t extension values with
      | None -> raise Mismatch
      | Some expected -> same (number extension.value) expected) t.extensions;
    !zero_divisor
  with Mismatch -> false

let check ?(affine_bounds = true) t ~integer:model ~representative ~explain =
  if Hashtbl.length t.divisions > 0 && candidate_model_matches t model
  then Done else
  let started = t.revision in
  residue_equalities t;
  let extensions = Int_table.fold (fun _ e xs ->
    t.poll (); e :: xs) t.extensions [] in
  List.iter (fun extension ->
    t.poll ();
    let values = Array.map (fun term -> t.poll (); model term)
        extension.operands in
    let consistent =
      if Array.exists Option.is_none values then false else
      match extension_value t extension (Array.map Option.get values),
            model extension.value with
      | Some expected, Some actual -> Z.equal expected actual
      | _ -> false in
    if not consistent
        && not (Int_table.mem t.initialized_extensions extension.value)
    then begin
      extension_axioms t extension;
      Int_table.add t.initialized_extensions extension.value ()
    end) extensions;
  extension_pairs t extensions;
  modular_equalities t ~integer:model ~representative ~explain;
  let products = Hashtbl.fold (fun _ p xs -> p :: xs) t.products [] in
  let divisions = Hashtbl.fold (fun _ d xs -> d :: xs) t.divisions [] in
  if products <> [] then factor_comparisons t;
  partitioned_modular_equalities t ~integer:model ~representative ~explain;
  nested_remainders t divisions;
  symbolic_modular_equalities t ~integer:model ~representative ~explain
    divisions;
  let ring_products = ring_equalities t ~integer:model
      ~representative ~explain products divisions in
  if t.revision <> started then Continue else begin
    List.iter (fun product ->
        t.poll ();
        if not (Int_table.mem t.initialized_products product.term) then begin
          basic_product t product;
          Int_table.add t.initialized_products product.term ()
        end;
        substitute t ~representative ~explain product) products;
    List.iter (fun division ->
        t.poll ();
        let key = division.dividend, division.divisor in
        if not (Hashtbl.mem t.initialized_divisions key) then begin
          division_axioms t division;
          Hashtbl.add t.initialized_divisions key ()
        end;
        List.iter (division_uniqueness t ~representative ~explain division)
          products) divisions;
    let factors = Hashtbl.create 32 in
    List.iter (fun product -> Array.iter (fun factor ->
      let root = representative factor in
      let old = match Hashtbl.find_opt factors root with
        | None -> [] | Some factors -> factors in
      if not (List.mem factor old)
      then Hashtbl.replace factors root (factor :: old))
      product.factors) products;
    let rec pairs = function
      | [] -> ()
      | product :: rest ->
        List.iter
          (associativity t ~representative ~explain factors product) rest;
        List.iter (cancellation t ~representative ~explain product) rest;
        pairs rest
    in
    pairs products;
    multiply_equalities t ~representative ~explain products;
    let rec pairs = function
      | [] -> ()
      | division :: rest ->
        List.iter (division_congruence t division) rest;
        List.iter
          (modular_products t ~representative ~explain
             products divisions division)
          rest;
        pairs rest in
    pairs divisions;
    quadratic_order t model products;
    quadratic_sum_bounds t model products;
    quadratic_ratio_bounds t model products;
    quadratic_envelopes t model products;
    monomial_order t model products;
    product_intervals t model products;
    square_secants t model products;
    List.iter (integer_bounds t model) products;
  if affine_bounds then
    multiplied_bounds t model ~representative ~explain products;
    let interpreted_factor_count product = Array.fold_left (fun count factor ->
      t.poll ();
      match Term.Unsafe.node t.terms factor with
      | Term.Apply (symbol, _) ->
        (match (Term.Unsafe.signature t.terms symbol).kind with
        | Term.Builtin _ -> count + 1 | _ -> count)
      | _ -> count) 0 product.factors in
    if (Term.count t.terms <= 4096 || t.revision = started)
       && not (List.exists
         (fun p -> interpreted_factor_count p >= 2) products) then
      List.iter (product_monotonicity t model) products;
    let rec gaps = function
      | [] -> ()
      | product :: rest ->
        List.iter (integer_gap t model ~representative ~explain product) rest;
        List.iter (fun other ->
          power_order t model product other;
          product_difference t model ~representative ~explain product other)
          rest;
        gaps rest in
    gaps products;
    if t.revision = started
       || List.compare_length_with products 2 <= 0
    then ring_products ();
    if t.revision <> started then Continue else begin
      let consistent = ref true in
      List.iter (fun extension ->
        t.poll ();
        if not (refine_extension t model extension) then consistent := false)
        extensions;
      List.iter (fun product ->
          if not (refine_product t model product) then consistent := false)
        products;
      List.iter (fun division ->
          match model division.dividend, model division.divisor,
                model division.quotient, model division.remainder with
          | Some x, Some y, Some q, Some r ->
            if not (Z.is_zero y) then begin
              let expected_q, expected_r = Z.div_rem ~poll:t.poll x y in
              if not (Z.equal q expected_q && Z.equal r expected_r)
              then consistent := false
            end
          | _ -> consistent := false) divisions;
      if t.revision <> started then Continue
      else if !consistent then Done
      else Incomplete "nonlinear candidate model is not established"
    end

  end

let registration_count t =
  Hashtbl.length t.products + Hashtbl.length t.divisions
  + Int_table.length t.extensions
