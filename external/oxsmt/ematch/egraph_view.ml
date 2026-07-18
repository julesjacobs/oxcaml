(* Read-only e-graph view (ADR-0012 L2). See egraph_view.mli. *)

open Oxsmt_core

module Symbol_table = Hashtbl.Make (Symbol)
module Sort_table = Hashtbl.Make (Sort)

module Int_table = Hashtbl.Make (struct
    type t = int

    let equal = Int.equal
    let hash x = x land max_int
  end)

type t =
  { app_terms_by_symbol : Symbol.t -> Term.t list
  ; find_class_opt : Term.t -> int option
  ; equal_if_registered : Term.t -> Term.t -> bool
  ; class_members : Term.t -> Term.t list
  ; ground_terms_by_sort : Sort.t -> Term.t list
  }

let empty =
  { app_terms_by_symbol = (fun _ -> [])
  ; find_class_opt = (fun _ -> None)
  ; equal_if_registered = (fun a b -> Term.equal a b)
  ; class_members = (fun t -> [ t ])
  ; ground_terms_by_sort = (fun _ -> [])
  }
;;

let snapshot ?indexed view ~ground_terms =
  (* Preserve the caller's deterministic order while removing terms that occur through
     more than one source. Close under syntactic children before copying roots, so every
     snapshot closure is self-contained and never consults the live engine. *)
  let seen = Term.Table.create (List.length ground_terms) in
  let pending = Queue.create () in
  let add term =
    if not (Term.Table.mem seen term)
    then (
      Term.Table.replace seen term ();
      Queue.add term pending)
  in
  List.iter add ground_terms;
  let captured = ref [] in
  while not (Queue.is_empty pending) do
    let term = Queue.pop pending in
    captured := term :: !captured;
    (match (term : Term.t).node with
     | Bool_const _ | Int_const _ | Real_const _ -> ()
     | App (_, args) -> Iarr.iter add args
     | Arith lin -> Iarr.iter (fun (child, _) -> add child) lin.coeffs
     | Real_arith lin -> Iarr.iter (fun (child, _) -> add child) lin.coeffs
     | Le child | Not child -> add child
     | Eq (a, b) ->
       add a;
       add b
     | And xs | Or xs -> Iarr.iter add xs
     | Ite (c, a, b) ->
       add c;
       add a;
       add b)
  done;
  let ground_terms = List.rev !captured in
  let roots = Term.Table.create (List.length ground_terms) in
  List.iter
    (fun term ->
       match view.find_class_opt term with
       | Some root -> Term.Table.replace roots term root
       | None -> ())
    ground_terms;
  let root term = Term.Table.find_opt roots term in
  let indexed =
    match indexed with
    | Some value -> value
    | None ->
      (match Sys.getenv_opt "OXSMT_LEMMA_INDEX" with
       | Some ("1" | "true" | "yes") -> true
       | Some _ | None -> false)
  in
  let equal_if_registered a b =
    match root a, root b with
    | Some ra, Some rb -> Int.equal ra rb
    | _ -> Term.equal a b
  in
  if not indexed
  then
    (* Keep the OFF arm structurally identical to the original scan-based snapshot. *)
    { app_terms_by_symbol =
        (fun sym ->
           List.filter
             (fun (term : Term.t) ->
                match term.node with
                | App (head, _) -> Symbol.equal head sym
                | Bool_const _ | Int_const _ | Real_const _ | Arith _ | Real_arith _
                | Le _ | Eq _ | Not _ | And _ | Or _ | Ite _ -> false)
             ground_terms)
    ; find_class_opt = root
    ; equal_if_registered
    ; class_members =
        (fun term ->
           match root term with
           | None -> [ term ]
           | Some wanted ->
             List.filter
               (fun member ->
                  match root member with
                  | Some actual -> Int.equal actual wanted
                  | None -> false)
               ground_terms)
    ; ground_terms_by_sort =
        (fun sort ->
           List.filter
             (fun (term : Term.t) -> Sort.equal term.sort sort)
             ground_terms)
    }
  else (
    (* Iterate in reverse and prepend so each bucket retains [ground_terms]' exact order.
       The closures below are then O(1) table lookups instead of a full-universe scan for
       every trigger root, recursive argument, and seed-sort query. *)
    let apps = Symbol_table.create 64 in
    let classes = Int_table.create 64 in
    let sorts = Sort_table.create 16 in
    List.iter
      (fun (term : Term.t) ->
         Sort_table.replace
           sorts
           term.sort
           (term :: Option.value (Sort_table.find_opt sorts term.sort) ~default:[]);
         (match term.node with
          | App (head, _) ->
            Symbol_table.replace
              apps
              head
              (term :: Option.value (Symbol_table.find_opt apps head) ~default:[])
          | Bool_const _ | Int_const _ | Real_const _ | Arith _ | Real_arith _ | Le _
          | Eq _ | Not _ | And _ | Or _ | Ite _ -> ());
         match root term with
         | None -> ()
         | Some class_id ->
           Int_table.replace
             classes
             class_id
             (term :: Option.value (Int_table.find_opt classes class_id) ~default:[]))
      (List.rev ground_terms);
    { app_terms_by_symbol =
        (fun sym -> Option.value (Symbol_table.find_opt apps sym) ~default:[])
    ; find_class_opt = root
    ; equal_if_registered
    ; class_members =
        (fun term ->
           match root term with
           | None -> [ term ]
           | Some class_id -> Option.value (Int_table.find_opt classes class_id) ~default:[])
    ; ground_terms_by_sort =
        (fun sort -> Option.value (Sort_table.find_opt sorts sort) ~default:[])
    })
;;
