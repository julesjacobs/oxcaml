module H = Pref.Heap

type level = Generic | Finite of int
type ('a : immutable_data) shape = Var | Bool | Arrow of 'a * 'a | Link of 'a
type ('a : immutable_data) memo = Empty_memo | Memo of 'a * 'a
type node : immutable_data = {
  desc : node Pref.t shape;
  level : level;
  memo : node Pref.t memo;
}
type desc = node Pref.t shape

type ty = Variable of node Pref.t | Boolean | Function of ty * ty [@@inductive]
let[@def] (cell @ total) (desc : desc @ immutable) (depth : int) = {desc; level = Finite depth; memo = Empty_memo}
let[@def] (mark @ total) (v : node @ immutable) (epoch : node Pref.t @ immutable)
    (target : node Pref.t @ immutable) = {v with memo = Memo (epoch, target)}
let[@def] (equation @ total) (h : Pref.heap @ immutable)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (p : node Pref.t @ immutable) = ghost_ (match H.at h p with
  | None -> true | Some v -> match v.desc with
  | Var -> true | Bool -> rho p === Boolean
  | Arrow (a, b) -> rho p === Function (rho a, rho b) | Link q -> rho p === rho q)
let[@def] (payload_scoped @ total) (h : Pref.heap @ immutable) (v : node @ immutable) = ghost_ (
  (match v.memo with Empty_memo -> true | Memo (stamp, _) -> H.mem h stamp)
  && (match v.desc with Var | Bool -> true | Link q -> H.mem h q
    | Arrow (a, b) -> H.mem h a && H.mem h b))
let[@def] (source_ok @ total) (h : Pref.heap @ immutable) (p : node Pref.t @ immutable) = ghost_ (
  match H.at h p with None -> false | Some v -> H.mem h p
  && (match v.memo with Empty_memo -> true | Memo (stamp, _) -> H.mem h stamp)
  && (match v.desc with Var | Bool -> true | Link q -> H.mem h q
    | Arrow (a, b) -> H.mem h a && H.mem h b))

type history = Start | Fresh of history * node Pref.t * node Pref.t * node * desc
  | Alias of history * node Pref.t * node Pref.t * node [@@inductive]
let[@def] rec (mapping @ total) (d : history @ immutable) (p : node Pref.t @ immutable) = ghost_ (match d with
  | Start -> None | Fresh (rest, x, q, _, _) | Alias (rest, x, q, _) ->
    if p === x then Some q else mapping rest p)
let[@def] rec (heap @ total) (saved : Pref.heap @ immutable)
    (epoch : node Pref.t @ immutable) (depth : int) (d : history @ immutable) = ghost_ (match d with
  | Start -> H.put saved epoch (cell Bool depth)
  | Fresh (rest, p, q, old, desc) ->
    H.put (H.put (heap saved epoch depth rest) q (cell desc depth)) p (mark old epoch q)
  | Alias (rest, p, q, old) -> H.put (heap saved epoch depth rest) p (mark old epoch q))
let[@def] (target_for @ total) (saved : Pref.heap @ immutable) (d : history @ immutable)
    (p : node Pref.t @ immutable) (q : node Pref.t @ immutable) = ghost_ (
  H.mem saved p && match H.at saved p with
  | None -> false | Some v -> match v.level with
  | Finite _ -> q === p | Generic -> mapping d p === Some q)
let[@def] (ready @ total) (saved : Pref.heap @ immutable) (d : history @ immutable)
    (source : desc @ immutable) (dest : desc @ immutable) = ghost_ (match source, dest with
  | Var, Var | Bool, Bool -> true
  | Arrow (a, b), Arrow (x, y) -> target_for saved d a x && target_for saved d b y
  | _ -> false)
let[@def] rec (valid @ total) (saved : Pref.heap @ immutable)
    (epoch : node Pref.t @ immutable) (depth : int) (d : history @ immutable) = ghost_ (
  match d with
  | Start -> not (H.mem saved epoch) && depth >= 0
  | Fresh (rest, p, q, old, desc) -> valid saved epoch depth rest
    && H.mem saved p && H.at (heap saved epoch depth rest) p === Some old
    && old.level === Generic && mapping rest p === None
    && not (H.mem (heap saved epoch depth rest) q) && ready saved rest old.desc desc
  | Alias (rest, p, q, old) -> valid saved epoch depth rest
    && H.mem saved p && H.at (heap saved epoch depth rest) p === Some old
    && old.level === Generic && mapping rest p === None
    && (match old.desc with Link x -> target_for saved rest x q | _ -> false))

type copied = #{value : node Pref.t @@ aliased; state : Pref.token; history : history @@ ghost}
type instance = #{value : node Pref.t @@ aliased; state : Pref.token;
  epoch : node Pref.t @@ ghost; history : history @@ ghost}

let[@def] (instance_at @ total) (saved : Pref.heap @ immutable)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (want : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (p : node Pref.t @ immutable) = ghost_ (match H.at saved p with
  | None -> true | Some v -> match v.level with
  | Finite _ -> want p === rho p
  | Generic -> match v.desc with Var -> true | Bool -> want p === Boolean
    | Arrow (a, b) -> want p === Function (want a, want b) | Link q -> want p === want q)

type template = Boundary of node Pref.t | Parameter of node Pref.t | Constant of node Pref.t
  | Product of node Pref.t * template * template | Indirect of node Pref.t * template [@@inductive]
let[@def] (root @ total) (t : template @ immutable) = match t with
  | Boundary p | Parameter p | Constant p | Product (p, _, _) | Indirect (p, _) -> p
let[@def] (generic_desc @ total) (saved : Pref.heap @ immutable) (p : node Pref.t @ immutable)
    (desc : desc @ immutable) = ghost_ (match H.at saved p with None -> false
    | Some v -> v.level === Generic && v.desc === desc)
let[@def] (finite_node @ total) (saved : Pref.heap @ immutable) (p : node Pref.t @ immutable) = ghost_ (
  match H.at saved p with None -> false | Some v -> match v.level with Finite _ -> true | Generic -> false)
let[@def] rec (template @ total) (saved : Pref.heap @ immutable) (t : template @ immutable) = ghost_ (
  H.mem saved (root t) && match t with
  | Boundary p -> finite_node saved p
  | Parameter p -> generic_desc saved p Var
  | Constant p -> generic_desc saved p Bool
  | Product (p, a, b) -> generic_desc saved p (Arrow (root a, root b)) && template saved a && template saved b
  | Indirect (p, child) -> generic_desc saved p (Link (root child)) && template saved child)
let[@def] rec (interpret @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (choices : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (t : template @ immutable) = match t with
  | Boundary p -> rho p | Parameter p -> choices p | Constant _ -> Boolean
  | Product (_, a, b) -> Function (interpret rho choices a, interpret rho choices b)
  | Indirect (_, child) -> interpret rho choices child

type destination = Allocate of desc | Share of node Pref.t
let[@def] (prepared @ total) (saved : Pref.heap @ immutable) (d : history @ immutable)
    (source : desc @ immutable) (dest : destination @ immutable) = ghost_ (match dest with
  | Allocate desc -> ready saved d source desc
  | Share q -> match source with Link x -> target_for saved d x q | _ -> false)

let[@def] (available @ total) (saved : Pref.heap @ immutable) (d : history @ immutable)
    (p : node Pref.t @ immutable) = ghost_ (H.mem saved p && match H.at saved p with
  | None -> false | Some v -> match v.level with Finite _ -> true | Generic ->
    match mapping d p with None -> false | Some _ -> true)
let[@def] (image @ total) (saved : Pref.heap @ immutable) (d : history @ immutable)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (p : node Pref.t @ immutable) = ghost_ (match H.at saved p with
  | Some {level = Generic; _} -> (match mapping d p with Some q -> rho q | None -> rho p)
  | _ -> rho p)
let[@def] (children_available @ total) (saved : Pref.heap @ immutable) (d : history @ immutable)
    (desc : desc @ immutable) = ghost_ (match desc with Var | Bool -> true
  | Link q -> available saved d q | Arrow (a, b) -> available saved d a && available saved d b)

let[@def] (head_desc @ total) (t : template @ immutable) = match t with
  | Boundary _ | Parameter _ -> Var | Constant _ -> Bool
  | Product (_, a, b) -> Arrow (root a, root b) | Indirect (_, c) -> Link (root c)
let[@def] (head_generic @ total) (t : template @ immutable) = match t with Boundary _ -> false | _ -> true
