(* Lattice-valued ZDDs *)

module type LATTICE = sig
  type t

  val bot : t
  val top : t
  val join : t -> t -> t
  val meet : t -> t -> t
  val co_sub : t -> t -> t (* residual: join a (co_sub b a) = join a b *)
  val to_string : t -> string (* optional, for debug/printing *)
  val equal : t -> t -> bool
  val hash : t -> int
end

module type ORDERED = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
end

module Make (C : LATTICE) (V : ORDERED) = struct
  (* --------- variables --------- *)
  type node =
    | Leaf of { id : int; c : C.t }
    | Node of { id : int; v : var; lo : node; hi : node }

  and var = {
    id : int; (* ZDD order: smaller id = higher *)
    mutable state : var_state; (* type+state of the variable *)
  }

  and var_state = Unsolved | Solved of node | Rigid of V.t

  module Var = struct
    type t = var

    let var_id = ref (-1)
    let rigid_var_start = 100000000
    let rigid_var_id = ref rigid_var_start

    module VMap = Map.Make (struct
      type t = V.t

      let compare = V.compare
    end)

    let rigid_tbl : t VMap.t ref = ref VMap.empty

    let make state =
      match state with
      | Rigid _ ->
        incr rigid_var_id;
        { id = !rigid_var_id; state }
      | _ ->
        incr var_id;
        { id = !var_id; state }

    let make_var () = make Unsolved

    let make_rigid ~name () =
      match VMap.find_opt name !rigid_tbl with
      | Some v -> v
      | None ->
        let v = make (Rigid name) in
        rigid_tbl := VMap.add name v !rigid_tbl;
        v

    let _id v = v.id
  end

  (* --------- stable node ids --------- *)
  let next_node_id = ref (-1)

  let fresh_id () =
    incr next_node_id;
    !next_node_id

  let node_id = function Leaf n -> n.id | Node n -> n.id

  (* --------- leaf interning (ensures one pointer per equal coeff) --------- *)
  module LeafTbl = Hashtbl.Make (C)

  let leaf_tbl : node LeafTbl.t = LeafTbl.create 97

  let leaf (c : C.t) : node =
    match LeafTbl.find_opt leaf_tbl c with
    | Some n -> n
    | None ->
      let n = Leaf { id = fresh_id (); c } in
      LeafTbl.add leaf_tbl c n;
      n

  let bot = leaf C.bot
  let top = leaf C.top
  let is_bot_node n = n == bot
  let is_top_node n = n == top

  (* --------- unique table for internal nodes --------- *)
  module UKey = struct
    type t = int * int * int (* v.id, lo.id, hi.id *)

    let equal (a, b, c) (a', b', c') = a = a' && b = b' && c = c'
    let hash = Hashtbl.hash
  end

  module Unique = Hashtbl.Make (UKey)

  let initial_hashtbl_size = 1024
  let uniq_tbl : node Unique.t = Unique.create initial_hashtbl_size

  (* --------- persistent memos --------- *)
  module NodeTbl = struct
    module Tbl = Hashtbl.Make (struct
      type t = int

      let equal = ( = )
      let hash n = Hashtbl.hash n
    end)

    let create () = Tbl.create 1024
    let find_opt tbl n = Tbl.find_opt tbl (node_id n)
    let add tbl n r = Tbl.add tbl (node_id n) r
    let clear tbl = Tbl.clear tbl
  end

  (* Pack two 30-bit ids into one 60-bit int key. *)
  module PairKey = struct
    let bits = 30
    let mask = (1 lsl bits) - 1

    let[@inline] make_int a b =
      (* debug guard, elide in production *)
      assert (a land lnot mask = 0 && b land lnot mask = 0);
      ((a land mask) lsl bits) lor (b land mask)

    let[@inline] of_nodes h l = make_int (node_id h) (node_id l)
  end

  module NodePairTbl = struct
    module Tbl = Hashtbl.Make (struct
      type t = int

      let equal = ( = )
      let hash = Hashtbl.hash
    end)

    let create () = Tbl.create initial_hashtbl_size
    let find_opt tbl n m = Tbl.find_opt tbl (PairKey.of_nodes n m)
    let add tbl n m r = Tbl.add tbl (PairKey.of_nodes n m) r
    let clear = Tbl.clear
  end

  (* For asserting that var ids are strictly increasing down the tree. *)
  let var_index (n : node) : int =
    match n with Leaf _ -> 999999999 | Node n -> n.v.id

  (* Construct a node and hash-cons it. Must be in canonical form: hi = hi -
     low. *)
  let node_raw (v : var) (lo : node) (hi : node) : node =
    assert (v.id < var_index lo);
    assert (v.id < var_index hi);

    if is_bot_node hi then lo
    else
      let key = (v.id, node_id lo, node_id hi) in
      match Unique.find_opt uniq_tbl key with
      | Some n -> n
      | None ->
        Global_counters.inc "node_raw_alloc";
        let n = Node { id = fresh_id (); v; lo; hi } in
        Unique.add uniq_tbl key n;
        n

  (* Subtract subsets h - l *)
  let memo_subs = NodePairTbl.create ()
  let rec down0 = function Leaf { c; _ } -> c | Node { lo; _ } -> down0 lo

  let rec canonicalize (h : node) (l : node) : node =
    if node_id h = node_id l then bot
    else if is_bot_node l then h
    else if is_top_node l then bot
    (* No need to test if h is top or bot because that path is fast anyway *)
      else
      (* Value at empty set *)
      match NodePairTbl.find_opt memo_subs h l with
      | Some r -> r
      | None ->
        Global_counters.inc "canonicalize";
        let r =
          match h with
          | Leaf x -> leaf (C.co_sub x.c (down0 l))
          | Node nh -> (
            match l with
            | Leaf _ ->
              node_raw nh.v (canonicalize nh.lo l) (canonicalize nh.hi l)
            | Node nl ->
              if nh.v.id = nl.v.id then
                let lo' = canonicalize nh.lo nl.lo in
                (* let hi' = canonicalize (canonicalize nh.hi nl.lo) nl.hi in *)
                let hi' = canonicalize (canonicalize nh.hi nl.hi) nl.lo in
                (* let hi' = canonicalize nh.hi (join nl.lo nl.hi) in *)
                node_raw nh.v lo' hi'
              else if nh.v.id < nl.v.id then
                node_raw nh.v (canonicalize nh.lo l) (canonicalize nh.hi l)
              else (* h.id > l.id *)
                canonicalize h nl.lo)
        in
        NodePairTbl.add memo_subs h l r;
        if node_id h == node_id r then Global_counters.inc "canonicalize_hit";
        r

  let node (v : var) (lo : node) (hi : node) : node =
    (* Don't need to memo this because canonicalize and node_raw are memoized *)
    let hi' = canonicalize hi lo in
    node_raw v lo hi'

  let memo_join = NodePairTbl.create ()

  let rec join (a : node) (b : node) : node =
    if node_id a = node_id b then a
    else if is_bot_node a then b
    else if is_bot_node b then a
    else if is_top_node a || is_top_node b then top
    else
      match NodePairTbl.find_opt memo_join a b with
      | Some r -> r
      | None ->
        Global_counters.inc "join";
        let r =
          match a, b with
          | Leaf x, Leaf y -> leaf (C.join x.c y.c)
          | Node x, Leaf _ -> node x.v (join x.lo b) (join x.hi b)
          | Leaf _, Node y -> node y.v (join a y.lo) (join a y.hi)
          | Node x, Node y ->
            if x.v.id = y.v.id then node x.v (join x.lo y.lo) (join x.hi y.hi)
            else if x.v.id < y.v.id then node x.v (join x.lo b) (join x.hi b)
            else node y.v (join a y.lo) (join a y.hi)
        in
        NodePairTbl.add memo_join a b r;
        r

  let memo_meet = NodePairTbl.create ()

  let rec meet (a : node) (b : node) : node =
    if node_id a = node_id b then a
    else if is_top_node a then b
    else if is_top_node b then a
    else if is_bot_node a || is_bot_node b then bot
    else
      match NodePairTbl.find_opt memo_meet a b with
      | Some r -> r
      | None ->
        Global_counters.inc "meet";
        let r =
          match a, b with
          | Leaf x, Leaf y -> leaf (C.meet x.c y.c)
          | Node x, Leaf _ -> node x.v (meet x.lo b) (meet x.hi b)
          | Leaf _, Node y -> node y.v (meet a y.lo) (meet a y.hi)
          | Node x, Node y ->
            if x.v.id = y.v.id then node x.v (meet x.lo y.lo) (meet x.hi y.hi)
            else if x.v.id < y.v.id then node x.v (meet x.lo b) (meet x.hi b)
            else node y.v (meet a y.lo) (meet a y.hi)
        in
        NodePairTbl.add memo_meet a b r;
        r

  let sub_subsets (h : node) (l : node) : node = canonicalize h l

  (* --------- fixpoint solver --------- *)
  let rec down0 = function Leaf { c; _ } -> c | Node { lo; _ } -> down0 lo

  let rec force (n : node) : node =
    match n with
    | Leaf _ -> n
    | Node ({ v = { state = Solved s; _ }; lo; hi; _ } as nn) ->
      let lo' = force lo in
      let hi' = force hi in
      if nn.lo == lo' && nn.hi == hi' then n else node_raw nn.v lo' hi'
    | Node nn ->
      Global_counters.inc "force";
      let lo = force nn.lo in
      let hi = force nn.hi in
      begin
        match nn.v.state with
        | Rigid _ -> node_raw nn.v lo hi
        | Solved s ->
          let hi' = canonicalize (join hi s) lo in
          node_raw nn.v lo hi'
        | Unsolved -> node_raw nn.v lo hi
      end

  let make_var () = Var.make_var ()
  let new_var = make_var
  let var v = Node { id = fresh_id (); v; lo = bot; hi = top }
  let rigid x = Var.make_rigid ~name:x ()
  let const c = leaf c

  let solve_lfp (v : var) (rhs : node) : unit =
    v.state <- Solved (force rhs)

  module Queue = struct
    type t = var list ref

    let create () = ref []
    let push q v = q := v :: !q
    let pop q =
      match !q with [] -> None | x :: xs -> q := xs; Some x
    let is_empty q = !q = []
  end

  let gfp_queue = Queue.create ()

  let enqueue_gfp (v : var) (rhs : node) : unit =
    let rhs' = force rhs in
    begin
      match v.state with
      | Unsolved | Rigid _ -> ()
      | Solved s ->
        (* install obligation v >= rhs' *)
        let hi =
          match s with Leaf _ -> s | Node n -> node_raw n.v n.lo (join n.hi rhs')
        in
        v.state <- Solved hi
    end;
    Queue.push gfp_queue v

  let rec solve_pending () : unit =
    match Queue.pop gfp_queue with
    | None -> ()
    | Some v ->
      begin
        match v.state with
        | Unsolved | Rigid _ -> ()
        | Solved s ->
          (* push obligations down over s *)
          let rec push (n : node) : unit =
            match n with
            | Leaf _ -> ()
            | Node nn ->
              begin
                match nn.v.state with
                | Rigid _ -> ()
                | Unsolved ->
                  let rhs = node nn.v nn.lo nn.hi in
                  let rhs0 = down0 rhs in
                  let rhs = node nn.v (leaf rhs0) (canonicalize rhs (leaf rhs0)) in
                  solve_lfp nn.v rhs
                | Solved s' ->
                  (* install nn >= s' and continue down *)
                  let hi = canonicalize (join nn.hi s') nn.lo in
                  nn.hi <- hi
              end;
              push nn.lo;
              push nn.hi
          in
          push s
      end;
      solve_pending ()

  (* --------- linear helpers --------- *)
  let rec leq a b =
    match a, b with
    | Leaf x, Leaf y -> C.equal x.c y.c
    | Leaf x, Node n -> C.leq x.c (down0 b)
    | Node n, Leaf _ -> leq n.lo b && leq n.hi b
    | Node na, Node nb ->
      if na.v.id = nb.v.id then leq na.lo nb.lo && leq na.hi nb.hi
      else if na.v.id < nb.v.id then leq na.lo b && leq na.hi b
      else leq a nb.lo && leq a nb.hi

  let round_up n = down0 n

  (* Decompose a polynomial along a set of rigid variables. *)
  let decompose_linear ~(universe : var list) (n : node) : node * node list =
    (* Fold the polynomial over the Boolean algebra to collect coefficients. *)
    let rigid_ids =
      universe |> List.map (fun v -> v.id) |> List.sort_uniq compare
    in
    let rec coeffs_of ids node : node list =
      match ids with
      | [] -> [ node ]
      | id :: ids' -> (
        match node with
        | Leaf _ -> List.init (1 lsl List.length ids) (fun _ -> node)
        | Node n ->
          if n.v.id = id then
            let cl = coeffs_of ids' n.lo in
            let ch = coeffs_of ids' n.hi in
            List.map2 join cl ch
          else if n.v.id < id then
            let cl = coeffs_of ids node in
            let ch = coeffs_of ids node in
            List.map2 join cl ch
          else (* n.v.id > id *)
            let cl = coeffs_of ids' node in
            let ch = coeffs_of ids' node in
            List.map2 join cl ch)
    in
    let base = const (down0 n) in
    let coeffs = coeffs_of rigid_ids n in
    (base, coeffs)

  (* --------- debug printing --------- *)
  let rec to_string (n : node) : string =
    match n with
    | Leaf x -> Printf.sprintf "{%s}" (C.to_string x.c)
    | Node n ->
      Printf.sprintf "<%d>(%s,%s)" n.v.id (to_string n.lo) (to_string n.hi)

  let pp_debug (n : node) : string = to_string (force n)
  let pp (n : node) : string =
    let b = down0 n in
    Printf.sprintf "{base=%s;...}" (C.to_string b)
end

