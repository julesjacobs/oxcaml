open Oxsmt_core

type binder =
  { id : int
  ; name : string
  ; sort : Sort.t
  }

type 'a t =
  | True
  | False
  | Atom of 'a
  | Not of 'a t
  | And of 'a t list
  | Or of 'a t list
  | Implies of 'a t * 'a t
  | Iff of 'a t * 'a t
  | Xor of 'a t * 'a t
  | Ite of 'a t * 'a t * 'a t
  | Forall of binder list * 'a t
  | Exists of binder list * 'a t

(* Process-global monotone binder-id source. Identity only needs to be unique, not
   reproducible across processes, so a plain counter suffices; rename-apart re-draws from
   the same source after NNF has duplicated binders. *)
let id_counter = ref 0

let fresh_id () =
  let id = !id_counter in
  incr id_counter;
  id
;;

let fresh_binder ~name ~sort = { id = fresh_id (); name; sort }

let rec map_atoms f = function
  | True -> True
  | False -> False
  | Atom a -> Atom (f a)
  | Not g -> Not (map_atoms f g)
  | And gs -> And (List.map (map_atoms f) gs)
  | Or gs -> Or (List.map (map_atoms f) gs)
  | Implies (a, b) -> Implies (map_atoms f a, map_atoms f b)
  | Iff (a, b) -> Iff (map_atoms f a, map_atoms f b)
  | Xor (a, b) -> Xor (map_atoms f a, map_atoms f b)
  | Ite (c, th, el) -> Ite (map_atoms f c, map_atoms f th, map_atoms f el)
  | Forall (bs, g) -> Forall (bs, map_atoms f g)
  | Exists (bs, g) -> Exists (bs, map_atoms f g)
;;

let rec iter_atoms f = function
  | True | False -> ()
  | Atom a -> f a
  | Not g -> iter_atoms f g
  | And gs | Or gs -> List.iter (iter_atoms f) gs
  | Implies (a, b) | Iff (a, b) | Xor (a, b) ->
    iter_atoms f a;
    iter_atoms f b
  | Ite (c, th, el) ->
    iter_atoms f c;
    iter_atoms f th;
    iter_atoms f el
  | Forall (_, g) | Exists (_, g) -> iter_atoms f g
;;

(* NNF by mutual recursion on requested polarity: [pos phi] is the NNF of [phi]; [neg phi]
   is the NNF of [(not phi)]. Boolean [Implies]/[Iff]/[Xor]/[Ite] are eliminated in terms
   of [And]/[Or]/[Not]-of-atom, and quantifiers dualize under [neg]. Each of
   [Iff]/[Xor]/[Ite] places its operands under BOTH polarities, duplicating any quantified
   operand — sound (the result is logically equivalent), but the duplicated copies must be
   renamed apart before Skolemization. *)
let rec nnf_pos = function
  | True -> True
  | False -> False
  | Atom a -> Atom a
  | Not g -> nnf_neg g
  | And gs -> And (List.map nnf_pos gs)
  | Or gs -> Or (List.map nnf_pos gs)
  | Implies (a, b) -> Or [ nnf_neg a; nnf_pos b ]
  (* a <-> b == (~a \/ b) /\ (~b \/ a) *)
  | Iff (a, b) -> And [ Or [ nnf_neg a; nnf_pos b ]; Or [ nnf_neg b; nnf_pos a ] ]
  (* a xor b == ~(a <-> b) == (a \/ b) /\ (~a \/ ~b) *)
  | Xor (a, b) -> And [ Or [ nnf_pos a; nnf_pos b ]; Or [ nnf_neg a; nnf_neg b ] ]
  (* Boolean ite(c, t, e) == (~c \/ t) /\ (c \/ e) *)
  | Ite (c, th, el) -> And [ Or [ nnf_neg c; nnf_pos th ]; Or [ nnf_pos c; nnf_pos el ] ]
  | Forall (bs, g) -> Forall (bs, nnf_pos g)
  | Exists (bs, g) -> Exists (bs, nnf_pos g)

and nnf_neg = function
  | True -> False
  | False -> True
  | Atom a -> Not (Atom a)
  | Not g -> nnf_pos g
  | And gs -> Or (List.map nnf_neg gs)
  | Or gs -> And (List.map nnf_neg gs)
  (* ~(a -> b) == a /\ ~b *)
  | Implies (a, b) -> And [ nnf_pos a; nnf_neg b ]
  (* ~(a <-> b) == a xor b == (a \/ b) /\ (~a \/ ~b) *)
  | Iff (a, b) -> And [ Or [ nnf_pos a; nnf_pos b ]; Or [ nnf_neg a; nnf_neg b ] ]
  (* ~(a xor b) == a <-> b == (~a \/ b) /\ (~b \/ a) *)
  | Xor (a, b) -> And [ Or [ nnf_neg a; nnf_pos b ]; Or [ nnf_neg b; nnf_pos a ] ]
  (* ~ite(c, t, e) == ite(c, ~t, ~e) == (~c \/ ~t) /\ (c \/ ~e) *)
  | Ite (c, th, el) -> And [ Or [ nnf_neg c; nnf_neg th ]; Or [ nnf_pos c; nnf_neg el ] ]
  | Forall (bs, g) -> Exists (bs, nnf_neg g)
  | Exists (bs, g) -> Forall (bs, nnf_neg g)
;;

let nnf = nnf_pos

let rec is_nnf = function
  | True | False | Atom _ -> true
  (* a [Not] is legal only immediately above an [Atom] *)
  | Not (Atom _) -> true
  | Not _ -> false
  | And gs | Or gs -> List.for_all is_nnf gs
  | Implies _ | Iff _ | Xor _ | Ite _ -> false
  | Forall (_, g) | Exists (_, g) -> is_nnf g
;;

(* rename-apart: walk top-down carrying a remap from old binder id -> fresh id. At each
   quantifier, freshen its binders and extend the remap for the body; a leaf atom's binder
   references are rewritten through the accumulated remap by [rename_atom]. Each
   quantifier OCCURRENCE gets independent fresh ids, so NNF-duplicated copies no longer
   share ids. *)
let rename_apart ~rename_atom phi =
  let rec go remap = function
    | True -> True
    | False -> False
    | Atom a -> Atom (rename_atom (fun i -> Option.value (remap i) ~default:i) a)
    | Not g -> Not (go remap g)
    | And gs -> And (List.map (go remap) gs)
    | Or gs -> Or (List.map (go remap) gs)
    | Implies (a, b) -> Implies (go remap a, go remap b)
    | Iff (a, b) -> Iff (go remap a, go remap b)
    | Xor (a, b) -> Xor (go remap a, go remap b)
    | Ite (c, th, el) -> Ite (go remap c, go remap th, go remap el)
    | Forall (bs, g) ->
      let bs', remap' = freshen remap bs in
      Forall (bs', go remap' g)
    | Exists (bs, g) ->
      let bs', remap' = freshen remap bs in
      Exists (bs', go remap' g)
  and freshen remap bs =
    let table = Hashtbl.create (List.length bs) in
    let bs' =
      List.map
        (fun b ->
          let id = fresh_id () in
          Hashtbl.replace table b.id id;
          { b with id })
        bs
    in
    let remap' i =
      match Hashtbl.find_opt table i with
      | Some id -> Some id
      | None -> remap i
    in
    bs', remap'
  in
  go (fun _ -> None) phi
;;

let rec binder_ids = function
  | True | False | Atom _ -> []
  | Not g -> binder_ids g
  | And gs | Or gs -> List.concat_map binder_ids gs
  | Implies (a, b) | Iff (a, b) | Xor (a, b) -> binder_ids a @ binder_ids b
  | Ite (c, th, el) -> binder_ids c @ binder_ids th @ binder_ids el
  | Forall (bs, g) | Exists (bs, g) -> List.map (fun b -> b.id) bs @ binder_ids g
;;

let to_string leaf phi =
  let buf = Buffer.create 256 in
  let binders bs = String.concat " " (List.map (fun b -> b.name) bs) in
  let rec go = function
    | True -> Buffer.add_string buf "true"
    | False -> Buffer.add_string buf "false"
    | Atom a -> Buffer.add_string buf (leaf a)
    | Not g ->
      Buffer.add_string buf "(not ";
      go g;
      Buffer.add_char buf ')'
    | And gs -> list "and" gs
    | Or gs -> list "or" gs
    | Implies (a, b) -> bin "=>" a b
    | Iff (a, b) -> bin "=" a b
    | Xor (a, b) -> bin "xor" a b
    | Ite (c, th, el) ->
      Buffer.add_string buf "(ite ";
      go c;
      Buffer.add_char buf ' ';
      go th;
      Buffer.add_char buf ' ';
      go el;
      Buffer.add_char buf ')'
    | Forall (bs, g) -> quant "forall" bs g
    | Exists (bs, g) -> quant "exists" bs g
  and list op gs =
    Buffer.add_char buf '(';
    Buffer.add_string buf op;
    List.iter
      (fun g ->
        Buffer.add_char buf ' ';
        go g)
      gs;
    Buffer.add_char buf ')'
  and bin op a b =
    Buffer.add_char buf '(';
    Buffer.add_string buf op;
    Buffer.add_char buf ' ';
    go a;
    Buffer.add_char buf ' ';
    go b;
    Buffer.add_char buf ')'
  and quant op bs g =
    Buffer.add_char buf '(';
    Buffer.add_string buf op;
    Buffer.add_string buf " (";
    Buffer.add_string buf (binders bs);
    Buffer.add_string buf ") ";
    go g;
    Buffer.add_char buf ')'
  in
  go phi;
  Buffer.contents buf
;;

type skolem_descr =
  { sk_binder : binder
  ; sk_deps : int list
  }

type 'a clause =
  { univ : binder list
  ; skolems : skolem_descr list
  ; matrix : 'a t
  }

(* Eliminate every [Exists], recording a Skolem descriptor per binder whose dependency
   list is the enclosing universal binder ids (in binding order). Requires NNF input, so
   [Not] wraps only atoms and every [Exists] is a genuine existential (polarity is
   explicit); [Implies]/[Iff]/[Xor]/[Ite] are already gone. Atoms keep referencing the
   eliminated binder id — a lowering resolves it to the Skolem term via the descriptor. *)
let skolemize phi =
  let descrs = ref [] in
  let rec go us = function
    | True -> True
    | False -> False
    | Atom a -> Atom a
    | Not g -> Not (go us g)
    | And gs -> And (List.map (go us) gs)
    | Or gs -> Or (List.map (go us) gs)
    | Forall (bs, g) -> Forall (bs, go (us @ List.map (fun b -> b.id) bs) g)
    | Exists (bs, g) ->
      List.iter (fun b -> descrs := { sk_binder = b; sk_deps = us } :: !descrs) bs;
      go us g
    | Implies _ | Iff _ | Xor _ | Ite _ ->
      invalid_arg "Fol.skolemize: input is not in NNF"
  in
  let phi' = go [] phi in
  List.rev !descrs, phi'
;;

(* Prenex all universals to the front. After [skolemize] no existentials remain, so a
   universal commutes out of both [And] and [Or] (binders are unique after rename-apart,
   so no capture): [(forall x. p) \/ q == forall x. (p \/ q)]. Returns
   [(all_univ, matrix)] with [matrix] quantifier-free. *)
let rec prenex = function
  | Forall (bs, g) ->
    let us, m = prenex g in
    bs @ us, m
  | And gs ->
    let uss, ms = List.split (List.map prenex gs) in
    List.concat uss, And ms
  | Or gs ->
    let uss, ms = List.split (List.map prenex gs) in
    List.concat uss, Or ms
  | (True | False | Atom _ | Not _) as m -> [], m
  | Exists _ | Implies _ | Iff _ | Xor _ | Ite _ ->
    invalid_arg "Fol.prenex: input still has existentials or non-NNF nodes"
;;

let rec conjuncts = function
  | And gs -> List.concat_map conjuncts gs
  | True -> []
  | m -> [ m ]
;;

let clausify ~rename_atom ~atom_refs phi =
  let n = rename_apart ~rename_atom (nnf phi) in
  let descrs, sk = skolemize n in
  let all_univ, matrix = prenex sk in
  let univ_ids = List.map (fun b -> b.id) all_univ in
  let descr_by_id = List.map (fun d -> d.sk_binder.id, d) descrs in
  List.filter_map
    (fun conj ->
      match conj with
      | True -> None
      | _ ->
        let refs = ref [] in
        iter_atoms (fun a -> refs := atom_refs a @ !refs) conj;
        let refs = List.sort_uniq compare !refs in
        let sks = List.filter_map (fun id -> List.assoc_opt id descr_by_id) refs in
        let needed =
          List.sort_uniq
            compare
            (List.filter (fun id -> List.mem id univ_ids) refs
             @ List.concat_map (fun d -> d.sk_deps) sks)
        in
        let univ = List.filter (fun b -> List.mem b.id needed) all_univ in
        Some { univ; skolems = sks; matrix = conj })
    (conjuncts matrix)
;;
