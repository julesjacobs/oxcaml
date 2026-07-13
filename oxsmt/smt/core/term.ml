(* Public view over {!Node}: the private-type re-export plus tag-based identity,
   collections, and the [Debug.check] validator. Construction stays in Node (whose
   builders are library-private), so this module cannot forge terms either — only match on
   them. *)

type t = Node.t =
  { node : node
  ; sort : Sort.t
  ; tag : int
  }

and node = Node.node =
  | Bool_const of bool
  | Int_const of Bigint.t
  | App of Symbol.t * t Iarr.t
  | Arith of linear
  | Le of t
  | Eq of t * t
  | Not of t
  | And of t Iarr.t
  | Or of t Iarr.t
  | Ite of t * t * t

and linear = Node.linear =
  { coeffs : (t * Bigint.t) Iarr.t
  ; const : Bigint.t
  }

exception Overflow = Node.Overflow
exception Sort_error = Node.Sort_error
exception Unsupported = Node.Unsupported

let equal (a : t) (b : t) = a.tag = b.tag
let compare (a : t) (b : t) = Int.compare a.tag b.tag
let hash (a : t) = a.tag

module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)

module Table = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal
    let hash = hash
  end)

module Debug = struct
  type mode =
    | Construction
    | Pipeline

  let fail fmt = Printf.ksprintf (fun s -> failwith ("Term.Debug.check: " ^ s)) fmt

  (* Reserved div/mod names, matched by name so the pipeline residual check needs no Env
     (mirrors Env's reserved names). *)
  let is_reserved_divmod sym =
    match Symbol.name sym with
    | "div" | "mod" -> true
    | _ -> false
  ;;

  let check_arith (l : linear) =
    let cs = Iarr.to_list l.coeffs in
    if cs = [] then fail "Arith with empty coeffs should be Int_const";
    (match cs with
     | [ (_, c) ] when Bigint.equal c Bigint.one && Bigint.is_zero l.const ->
       fail "Arith {[(a,1)];0} should have unwrapped to a"
     | _ -> ());
    let prev = ref min_int in
    List.iter
      (fun (t, c) ->
         if Bigint.is_zero c then fail "Arith has a zero coefficient";
         if not (Sort.equal t.sort Sort.int) then fail "Arith coefficient term is not Int";
         (match t.node with
          | Arith _ -> fail "Arith coefficient term is itself an Arith"
          | _ -> ());
         if t.tag <= !prev then fail "Arith coeffs not strictly tag-increasing";
         prev := t.tag)
      cs
  ;;

  let gcd_of_coeffs (l : linear) =
    Iarr.fold (fun g (_, c) -> Bigint.gcd g c) Bigint.zero l.coeffs
  ;;

  let check ?(mode = Construction) ?env (root : t) =
    let seen : (int, node) Hashtbl.t = Hashtbl.create 256 in
    let same_sort a b = Sort.equal a.sort b.sort in
    let check_node (t : t) =
      match t.node with
      | Bool_const _ ->
        if not (Sort.equal t.sort Sort.bool) then fail "Bool_const not Bool"
      | Int_const _ -> if not (Sort.equal t.sort Sort.int) then fail "Int_const not Int"
      | App (sym, args) ->
        if mode = Pipeline && is_reserved_divmod sym
        then fail "reserved div/mod App survived to pipeline";
        (match env with
         | None -> ()
         | Some env ->
           (match Env.rank env sym with
            | exception Not_found ->
              fail "App symbol %s has no rank in Env" (Symbol.name sym)
            | rank ->
              if Iarr.length args <> Iarr.length rank.Rank.domain
              then fail "App arity mismatch for %s" (Symbol.name sym);
              Iarr.iteri
                (fun i a ->
                   if not (Sort.equal a.sort (Iarr.get rank.Rank.domain i))
                   then fail "App argument %d sort mismatch for %s" i (Symbol.name sym))
                args;
              if not (Sort.equal t.sort rank.Rank.codomain)
              then fail "App result sort mismatch for %s" (Symbol.name sym)))
      | Arith l ->
        if not (Sort.equal t.sort Sort.int) then fail "Arith not Int";
        check_arith l
      | Le arg ->
        if not (Sort.equal t.sort Sort.bool) then fail "Le not Bool";
        if not (Sort.equal arg.sort Sort.int) then fail "Le arg not Int";
        (match arg.node with
         | Int_const _ -> fail "Le arg is a constant (should have folded)"
         | Arith l ->
           if not (Bigint.equal (gcd_of_coeffs l) Bigint.one)
           then fail "Le not gcd-normalized (gcd <> 1)"
         | _ -> () (* bare term: single coeff 1, gcd = 1 *))
      | Eq (a, b) ->
        if not (Sort.equal t.sort Sort.bool) then fail "Eq not Bool";
        if not (same_sort a b) then fail "Eq operands differ in sort";
        if not (a.tag < b.tag) then fail "Eq not strictly tag-ordered"
      | Not a ->
        if not (Sort.equal t.sort Sort.bool) then fail "Not not Bool";
        if not (Sort.equal a.sort Sort.bool) then fail "Not arg not Bool";
        (match a.node with
         | Not _ -> fail "Not(Not _) not eliminated"
         | Bool_const _ -> fail "Not(Bool_const _) not folded"
         | _ -> ())
      | And xs | Or xs ->
        let is_and =
          match t.node with
          | And _ -> true
          | _ -> false
        in
        if not (Sort.equal t.sort Sort.bool) then fail "And/Or not Bool";
        if Iarr.length xs < 2 then fail "And/Or arity < 2";
        let prev = ref min_int in
        Iarr.iter
          (fun c ->
             if not (Sort.equal c.sort Sort.bool) then fail "And/Or child not Bool";
             (match c.node with
              | Bool_const _ -> fail "And/Or has a Bool_const child"
              | And _ when is_and -> fail "And has an And child (not flattened)"
              | Or _ when not is_and -> fail "Or has an Or child (not flattened)"
              | _ -> ());
             if c.tag <= !prev then fail "And/Or children not strictly tag-increasing";
             prev := c.tag)
          xs
      | Ite (c, a, b) ->
        if not (Sort.equal c.sort Sort.bool) then fail "Ite condition not Bool";
        if not (same_sort a b) then fail "Ite branches differ in sort";
        if not (same_sort t a) then fail "Ite result sort <> branch sort";
        (match c.node with
         | Bool_const _ -> fail "Ite condition is constant (should have folded)"
         | _ -> ());
        if a.tag = b.tag then fail "Ite(_,a,a) not folded";
        if mode = Pipeline && Sort.equal t.sort Sort.int
        then fail "Int-sorted Ite survived to pipeline"
    in
    let rec visit (t : t) =
      match Hashtbl.find_opt seen t.tag with
      | Some n ->
        if not (Node.equal_node n t.node)
        then fail "tag %d bound to two distinct structures (hash-cons broken)" t.tag
      | None ->
        Hashtbl.add seen t.tag t.node;
        check_node t;
        List.iter visit (children t)
    and children (t : t) =
      match t.node with
      | Bool_const _ | Int_const _ -> []
      | App (_, xs) | And xs | Or xs -> Iarr.to_list xs
      | Arith l -> List.map fst (Iarr.to_list l.coeffs)
      | Le a | Not a -> [ a ]
      | Eq (a, b) -> [ a; b ]
      | Ite (a, b, c) -> [ a; b; c ]
    in
    visit root
  ;;
end
