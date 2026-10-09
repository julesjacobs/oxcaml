(* Symbol.t is [private int], so it coerces to [int] and we can key immutable maps by the
   underlying id (O(log n), no mutation, [add] is functional). The registry is tiny (a
   handful of datatypes per query), so map overhead is irrelevant. *)
module Id_map = Map.Make (Int)

type selector =
  { sym : Symbol.t
  ; index : int
  ; field_sort : Sort.t
  }

type constructor =
  { sym : Symbol.t
  ; selectors : selector list
  ; tester : Symbol.t
  }

type datatype =
  { sort_sym : Symbol.t
  ; constructors : constructor list
  }

(* Field names deliberately avoid [selectors]/[constructors] so they don't shadow the
   {!constructor} / {!datatype} record labels during field-access disambiguation. *)
type t =
  { by_sort : datatype Id_map.t
  ; by_ctor : (datatype * constructor) Id_map.t
  ; by_selector : (datatype * constructor * selector) Id_map.t
  ; by_tester : (datatype * constructor) Id_map.t
  }

let empty =
  { by_sort = Id_map.empty
  ; by_ctor = Id_map.empty
  ; by_selector = Id_map.empty
  ; by_tester = Id_map.empty
  }
;;

let id (s : Symbol.t) = (s :> int)

(* Every constructor/selector/tester/sort symbol belongs to exactly one datatype: a repeat
   is a front-end construction bug, not a runtime input error, so we fail loudly rather
   than silently last-wins (the L1-class masking trap Model.of_alist also guards against). *)
let add_unique what key value map =
  if Id_map.mem key map
  then invalid_arg (Printf.sprintf "Datatype_defs: %s already registered" what);
  Id_map.add key value map
;;

let add t dt =
  let by_sort = add_unique "datatype sort" (id dt.sort_sym) dt t.by_sort in
  let by_ctor, by_selector, by_tester =
    List.fold_left
      (fun (by_ctor, by_selector, by_tester) (c : constructor) ->
         let by_ctor = add_unique "constructor" (id c.sym) (dt, c) by_ctor in
         let by_tester = add_unique "tester" (id c.tester) (dt, c) by_tester in
         let by_selector =
           List.fold_left
             (fun by_selector (sel : selector) ->
                add_unique "selector" (id sel.sym) (dt, c, sel) by_selector)
             by_selector
             c.selectors
         in
         by_ctor, by_selector, by_tester)
      (t.by_ctor, t.by_selector, t.by_tester)
      dt.constructors
  in
  { by_sort; by_ctor; by_selector; by_tester }
;;

(* Full-signature rank equality (mirrors [Array_defs.rank_matches]). *)
let rank_matches (a : Rank.t) (b : Rank.t) =
  Sort.equal a.Rank.codomain b.Rank.codomain
  && Iarr.length a.Rank.domain = Iarr.length b.Rank.domain
  &&
  let rec loop i =
    i = Iarr.length a.Rank.domain
    || (Sort.equal (Iarr.get a.Rank.domain i) (Iarr.get b.Rank.domain i) && loop (i + 1))
  in
  loop 0
;;

(* Install-door validator (mirrors [Array_defs.validate_ranks]): every registered
   constructor / selector / tester symbol must carry, in the environment, the canonical
   rank for its role in its datatype — a constructor returns the datatype sort, a tester
   is [(dt) -> Bool], a selector is [(dt) -> field]. A hand-built registry marking an
   arbitrary symbol (e.g. an uninterpreted-sort constant) as a constructor is thereby
   rejected at the install door, keeping every downstream DT consumer's sort assumptions
   (the theory's rules, the symmetry-breaking free-constant test) sound BY CONSTRUCTION.
   Raises [Invalid_argument] on a disagreeing or missing rank. *)
let validate_ranks t ~(rank_of : Symbol.t -> Rank.t option) =
  let check ~role sym ~want =
    match rank_of sym with
    | Some r when rank_matches r want -> ()
    | Some _ ->
      invalid_arg
        (Printf.sprintf
           "Datatype_defs.validate_ranks: %s registered as a %s does not have that \
            role's canonical rank for its datatype (full-signature disagreement)"
           (Symbol.name sym)
           role)
    | None ->
      invalid_arg
        (Printf.sprintf
           "Datatype_defs.validate_ranks: registered %s %s has no rank in the environment"
           role
           (Symbol.name sym))
  in
  Id_map.iter
    (fun _ (dt : datatype) ->
       let dt_sort = Sort.datatype_ dt.sort_sym in
       List.iter
         (fun (c : constructor) ->
            let field_sorts = List.map (fun (s : selector) -> s.field_sort) c.selectors in
            check ~role:"constructor" c.sym ~want:(Rank.create field_sorts dt_sort);
            check ~role:"tester" c.tester ~want:(Rank.create [ dt_sort ] Sort.bool);
            List.iter
              (fun (sel : selector) ->
                 check
                   ~role:"selector"
                   sel.sym
                   ~want:(Rank.create [ dt_sort ] sel.field_sort))
              c.selectors)
         dt.constructors)
    t.by_sort
;;

let is_empty t = Id_map.is_empty t.by_sort
let datatype_of_sort t sort_sym = Id_map.find_opt (id sort_sym) t.by_sort
let constructor_of_sym t sym = Id_map.find_opt (id sym) t.by_ctor
let selector_of_sym t sym = Id_map.find_opt (id sym) t.by_selector
let tester_of_sym t sym = Id_map.find_opt (id sym) t.by_tester
let is_datatype_sym t sym = Id_map.mem (id sym) t.by_sort
