(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
 { expect.opt; }
*)

module Register_allocation = struct
  type operand = Reg of int | Imm of int
  type operation = Add | Subtract | Equal | Less_than
  type instruction =
    | Move of int * operand * int
    | Binary of int * operation * operand * operand * int
    | Jump of int
    | Branch of operand * int * int
    | Return of operand

  type program = { code : instruction list; registers : int; inputs : int list }
  type state = Running of int * int list | Done of int | Stuck

  let[@def] rec (nth @ total) xs n =
    match xs with
    | [] -> None
    | x :: rest -> if n = 0 then Some x else nth rest (n - 1)

  let[@def] rec (write @ total) xs n value =
    match xs with
    | [] -> None
    | x :: rest ->
      if n = 0 then Some (value :: rest)
      else match write rest (n - 1) value with
        | None -> None
        | Some rest -> Some (x :: rest)

  let rec (write_preserves @ total) :
    (xs : int list) -> (n : int) -> (value : int) -> (m : int) ->
    {u : unit |
      n = m ||
      (match write xs n value with
       | None -> true
       | Some ys -> nth ys m === nth xs m)}
    @ immutable contended =
    fun xs n value m ->
    write_def xs n value;
    nth_def xs m;
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      if n = 0 then begin
        nth_def (value :: rest) m;
        refine_ u
      end else begin
        write_preserves rest (n - 1) value (m - 1);
        (match write rest (n - 1) value with
         | None -> ()
         | Some result -> nth_def (head :: result) m);
        refine_ u
      end

  let rec (write_reads @ total) :
    (xs : int list) -> (n : int) -> (value : int) ->
    {u : unit |
      match write xs n value with
      | None -> true
      | Some ys -> nth ys n === Some value}
    @ immutable contended =
    fun xs n value ->
    write_def xs n value;
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      if n = 0 then begin nth_def (value :: rest) n; refine_ u end
      else begin
        write_reads rest (n - 1) value;
        (match write rest (n - 1) value with
         | None -> ()
         | Some result -> nth_def (head :: result) n);
        refine_ u
      end

  let[@def] rec (zeros @ total) n = if n <= 0 then [] else 0 :: zeros (n - 1)
  [@@decreases n]

  let[@def] (value @ total) file operand =
    match operand with Reg r -> nth file r | Imm n -> Some n

  let[@def] (apply @ total) operation left right = match operation with
    | Add -> left + right
    | Subtract -> left - right
    | Equal -> if left = right then 1 else 0
    | Less_than -> if left < right then 1 else 0

  let[@def] (execute @ total) instruction file =
    match instruction with
      | Move (dst, operand, next) ->
        (match value file operand with
         | None -> Stuck
         | Some word ->
           match write file dst word with
           | None -> Stuck
           | Some file -> Running (next, file))
      | Binary (dst, operation, left, right, next) ->
        (match value file left, value file right with
         | Some left, Some right ->
           (match write file dst (apply operation left right) with
            | None -> Stuck
            | Some file -> Running (next, file))
         | _ -> Stuck)
      | Jump next -> Running (next, file)
      | Branch (condition, yes, no) ->
        (match value file condition with
         | None -> Stuck
         | Some word -> Running ((if word = 0 then no else yes), file))
      | Return operand ->
        (match value file operand with None -> Stuck | Some word -> Done word)

  let[@def] (step @ total) code state = match state with
    | Done _ | Stuck -> state
    | Running (pc, file) ->
      match nth code pc with
      | None -> Stuck
      | Some instruction -> execute instruction file

  type fuel = Z | S of fuel [@@inductive]

  let[@def] rec (advance @ total) code fuel state = match fuel with
    | Z -> state
    | S rest -> advance code rest (step code state)

  let[@def] rec (member @ total) (value : int) xs = match xs with
    | [] -> false
    | x :: rest -> x = value || member value rest

  let[@def] (add @ total) value xs = if member value xs then xs else value :: xs

  let[@def] rec (union @ total) xs ys = match xs with
    | [] -> ys
    | x :: rest -> add x (union rest ys)

  let[@def] rec (remove @ total) (value : int) xs = match xs with
    | [] -> []
    | x :: rest -> if x = value then remove value rest else x :: remove value rest

  let[@def] rec (subset @ total) xs ys = match xs with
    | [] -> true
    | x :: rest -> member x ys && subset rest ys

  let[@def] (operand_uses @ total) operand =
    match operand with Reg r -> [r] | Imm _ -> []

  let[@def] (uses @ total) instruction = match instruction with
    | Move (_, operand, _) | Branch (operand, _, _) | Return operand -> operand_uses operand
    | Binary (_, _, left, right, _) -> union (operand_uses left) (operand_uses right)
    | Jump _ -> []

  let[@def] (definition @ total) instruction = match instruction with
    | Move (dst, _, _) | Binary (dst, _, _, _, _) -> Some dst
    | Jump _ | Branch _ | Return _ -> None

  let[@def] (successors @ total) instruction = match instruction with
    | Move (_, _, next) | Binary (_, _, _, _, next) | Jump next -> [next]
    | Branch (_, yes, no) -> [yes; no]
    | Return _ -> []

  let[@def] rec (live_out @ total) live successors = match successors with
    | [] -> []
    | successor :: rest ->
      let at_successor = match nth live successor with None -> [] | Some xs -> xs in
      union at_successor (live_out live rest)

  let[@def] (survivors @ total) live instruction =
    let out = live_out live (successors instruction) in
    match definition instruction with
      | None -> out
      | Some dst -> remove dst out

  let rec (subset_member @ total) :
    (xs : int list) -> (ys : int list) -> (reg : int) ->
    {u : unit |
      not (subset xs ys && member reg xs) || member reg ys}
    @ immutable contended =
    fun xs ys reg ->
    subset_def xs ys;
    member_def reg xs;
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      if head = reg then refine_ u
      else begin subset_member rest ys reg; refine_ u end

  let rec (subset_cons @ total) :
    (xs : int list) -> (ys : int list) -> (head : int) ->
    {u : unit | not (subset xs ys) || subset xs (head :: ys)}
    @ immutable contended =
    fun xs ys head ->
    subset_def xs ys;
    subset_def xs (head :: ys);
    let u = () in
    match xs with
    | [] -> refine_ u
    | reg :: rest ->
      subset_cons rest ys head;
      member_def reg (head :: ys);
      refine_ u

  let rec (subset_reflexive @ total) :
    (xs : int list) -> {u : unit | subset xs xs}
    @ immutable contended =
    fun xs ->
    subset_def xs xs;
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      subset_reflexive rest;
      subset_cons rest rest head;
      member_def head xs;
      refine_ u

  let rec (remove_keeps @ total) :
    (removed : int) -> (xs : int list) -> (reg : int) ->
    {u : unit |
      removed = reg || not (member reg xs) || member reg (remove removed xs)}
    @ immutable contended =
    fun removed xs reg ->
    remove_def removed xs;
    member_def reg xs;
    member_def reg (remove removed xs);
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      remove_keeps removed rest reg;
      refine_ u

  let rec (union_keeps_right @ total) :
    (xs : int list) -> (ys : int list) -> (reg : int) ->
    {u : unit | not (member reg ys) || member reg (union xs ys)}
    @ immutable contended =
    fun xs ys reg ->
    union_def xs ys;
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      union_keeps_right rest ys reg;
      add_def head (union rest ys);
      member_def reg (head :: union rest ys);
      refine_ u

  let (add_keeps @ total) :
    (value : int) -> (xs : int list) -> (reg : int) ->
    {u : unit | not (member reg xs) || member reg (add value xs)}
    @ immutable contended =
    fun value xs reg ->
    add_def value xs;
    member_def reg (value :: xs);
    refine_ ()

  let rec (union_keeps_left @ total) :
    (xs : int list) -> (ys : int list) -> (reg : int) ->
    {u : unit | not (member reg xs) || member reg (union xs ys)}
    @ immutable contended =
    fun xs ys reg ->
    union_def xs ys;
    member_def reg xs;
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      union_keeps_left rest ys reg;
      add_keeps head (union rest ys) reg;
      add_def head (union rest ys);
      member_def reg (head :: union rest ys);
      refine_ u

  let rec (live_out_contains @ total) :
    (live : int list list) -> (successors : int list) ->
    (successor : int) -> (reg : int) -> (row : int list) ->
    {u : unit |
      not (member successor successors
           && nth live successor === Some row && member reg row)
      || member reg (live_out live successors)}
    @ immutable contended =
    fun live successors successor reg row ->
    live_out_def live successors;
    member_def successor successors;
    let u = () in
    match successors with
    | [] -> refine_ u
    | head :: rest ->
      if head = successor then begin
        union_keeps_left row (live_out live rest) reg;
        refine_ u
      end else begin
        live_out_contains live rest successor reg row;
        union_keeps_right
          (match nth live head with None -> [] | Some xs -> xs)
          (live_out live rest) reg;
        refine_ u
      end

  let (successor_live @ total) :
    (live : int list list) -> (instruction : instruction) ->
    (current : int list) -> (successor : int) ->
    (row : int list) -> (reg : int) ->
    {u : unit |
      not (subset (survivors live instruction) current
           && member successor (successors instruction)
           && nth live successor === Some row
           && member reg row)
      || definition instruction === Some reg
      || member reg current}
    @ immutable contended =
    fun live instruction current successor row reg ->
    survivors_def live instruction;
    live_out_contains live (successors instruction) successor reg row;
    let u = () in
    match definition instruction with
    | None ->
      subset_member (survivors live instruction) current reg;
      refine_ u
    | Some dst ->
      remove_keeps dst (live_out live (successors instruction)) reg;
      subset_member (survivors live instruction) current reg;
      refine_ u

  let[@def] (transfer @ total) live instruction =
    union (uses instruction) (survivors live instruction)

  let[@def] rec (sweep @ total) code live index = match code with
    | [] -> ([], false)
    | instruction :: rest ->
      let old = match nth live index with None -> [] | Some xs -> xs in
      let needed = transfer live instruction in
      let next = union old needed in
      let tail, changed = sweep rest live (index + 1) in
      next :: tail,
      changed || not (subset (uses instruction) old && subset (survivors live instruction) old)

  let[@def] rec (closed_from @ total) code live index = match code with
    | [] -> true
    | instruction :: rest ->
      let old = match nth live index with None -> [] | Some xs -> xs in
      subset (uses instruction) old
      && subset (survivors live instruction) old
      && closed_from rest live (index + 1)

  let rec (closed_lookup @ total) :
    (code : instruction list) -> (live : int list list) ->
    (index : int) -> (pc : int) -> (instruction : instruction) ->
    {u : unit |
      not (closed_from code live index)
      || not (nth code pc === Some instruction)
      ||
      (let at_pc = match nth live (index + pc) with
         | None -> [] | Some xs -> xs in
       subset (uses instruction) at_pc
       && subset (survivors live instruction) at_pc)}
    @ immutable contended =
    fun code live index pc instruction ->
    closed_from_def code live index;
    nth_def code pc;
    let u = () in
    match code with
    | [] -> refine_ u
    | head :: rest ->
      if pc = 0 then refine_ u
      else begin
        closed_lookup rest live (index + 1) (pc - 1) instruction;
        refine_ u
      end

  let rec (sweep_closed @ total) :
    (code : instruction list) -> (live : int list list) -> (index : int) ->
    {u : unit |
      match sweep code live index with
      | _, true -> true
      | _, false -> closed_from code live index} @ immutable contended =
    fun code live index ->
    sweep_def code live index;
    closed_from_def code live index;
    let u = () in
    match code with
    | [] -> refine_ u
    | _ :: rest ->
      sweep_closed rest live (index + 1);
      refine_ u

  let[@def] rec (empty_live @ total) (code : instruction list) : int list list =
    match code with [] -> [] | _ :: rest -> [] :: empty_live rest

  let[@def] rec (stabilize @ total) fuel code live =
    if fuel <= 0 then None
    else
      let next, changed = sweep code live 0 in
      if changed then stabilize (fuel - 1) code next else Some live
  [@@decreases fuel]

  let rec (stabilize_closed @ total) :
    (fuel : int) -> (code : instruction list) -> (live : int list list) ->
    {u : unit |
      match stabilize fuel code live with
      | None -> true
      | Some result -> closed_from code result 0} @ immutable contended =
    fun fuel code live ->
    stabilize_def fuel code live;
    let u = () in
    if fuel <= 0 then refine_ u
    else
      let next, changed = sweep code live 0 in
      if changed then begin
        stabilize_closed (fuel - 1) code next;
        refine_ u
      end else begin
        sweep_closed code live 0;
        refine_ u
      end
  [@@decreases fuel]

  type edge = int * int

  let[@def] rec (member_edge @ total) (pair : edge) edges = match edges with
    | [] -> false
    | (left, right) :: rest ->
      let wanted_left, wanted_right = pair in
      (left = wanted_left && right = wanted_right) || member_edge pair rest

  let[@def] (edge @ total) (a : int) (b : int) = if a < b then a, b else b, a

  let[@def] (add_edge @ total) (a : int) (b : int) graph =
    if a = b then graph
    else let pair = edge a b in if member_edge pair graph then graph else pair :: graph

  let[@def] rec (connect_one @ total) vertex vertices graph = match vertices with
    | [] -> graph
    | other :: rest -> connect_one vertex rest (add_edge vertex other graph)

  let[@def] rec (clique @ total) vertices graph = match vertices with
    | [] -> graph
    | vertex :: rest -> clique rest (connect_one vertex rest graph)

  let[@def] rec (interference @ total) code live index graph = match code with
    | [] -> graph
    | instruction :: rest ->
      let graph = match definition instruction with
        | None -> graph
        | Some dst -> connect_one dst (live_out live (successors instruction)) graph in
      interference rest live (index + 1) graph

  let[@def] (graph @ total) code live =
    let entry = match live with [] -> [] | xs :: _ -> xs in
    interference code live 0 (clique entry [])

  let[@def] rec (reverse_into @ total) xs acc = match xs with
    | [] -> acc
    | x :: rest -> reverse_into rest (x :: acc)

  let[@def] (reverse @ total) xs = reverse_into xs []

  let[@def] rec (range_down @ total) n =
    if n <= 0 then [] else (n - 1) :: range_down (n - 1)
  [@@decreases n]

  let[@def] (range @ total) n = reverse (range_down n)

  let[@def] (adjacent @ total) a b graph = member_edge (edge a b) graph

  let (add_edge_covers @ total) :
    (a : int) -> (b : int) -> (graph : edge list) ->
    {u : unit | a = b || adjacent a b (add_edge a b graph)}
    @ immutable contended =
    fun a b graph ->
    add_edge_def a b graph;
    adjacent_def a b (add_edge a b graph);
    let u = () in
    if a = b then refine_ u
    else if member_edge (edge a b) graph then refine_ u
    else begin
      member_edge_def (edge a b) ((edge a b) :: graph);
      refine_ u
    end

  let (add_edge_preserves @ total) :
    (pair : edge) -> (a : int) -> (b : int) -> (graph : edge list) ->
    {u : unit | not (member_edge pair graph) || member_edge pair (add_edge a b graph)}
    @ immutable contended =
    fun pair a b graph ->
    add_edge_def a b graph;
    let u = () in
    if a = b || member_edge (edge a b) graph then refine_ u
    else begin
      member_edge_def pair ((edge a b) :: graph);
      refine_ u
    end

  let rec (connect_one_preserves @ total) :
    (pair : edge) -> (vertex : int) -> (vertices : int list) ->
    (graph : edge list) ->
    {u : unit |
      not (member_edge pair graph)
      || member_edge pair (connect_one vertex vertices graph)}
    @ immutable contended =
    fun pair vertex vertices graph ->
    connect_one_def vertex vertices graph;
    let u = () in
    match vertices with
    | [] -> refine_ u
    | other :: rest ->
      add_edge_preserves pair vertex other graph;
      connect_one_preserves pair vertex rest (add_edge vertex other graph);
      refine_ u

  let rec (connect_one_covers @ total) :
    (vertex : int) -> (vertices : int list) -> (graph : edge list) ->
    (other : int) ->
    {u : unit |
      not (member other vertices) || vertex = other
      || adjacent vertex other (connect_one vertex vertices graph)}
    @ immutable contended =
    fun vertex vertices graph other ->
    connect_one_def vertex vertices graph;
    member_def other vertices;
    let u = () in
    match vertices with
    | [] -> refine_ u
    | head :: rest ->
      if head = other then begin
        add_edge_covers vertex head graph;
        connect_one_preserves (edge vertex other) vertex rest
          (add_edge vertex head graph);
        adjacent_def vertex other (connect_one vertex rest (add_edge vertex head graph));
        adjacent_def vertex other (add_edge vertex head graph);
        refine_ u
      end else begin
        connect_one_covers vertex rest (add_edge vertex head graph) other;
        refine_ u
      end

  let (edge_symmetric @ total) :
    (a : int) -> (b : int) -> {u : unit | edge a b === edge b a}
    @ immutable contended =
    fun a b ->
    edge_def a b;
    edge_def b a;
    refine_ ()

  let rec (clique_preserves @ total) :
    (pair : edge) -> (vertices : int list) -> (graph : edge list) ->
    {u : unit |
      not (member_edge pair graph) || member_edge pair (clique vertices graph)}
    @ immutable contended =
    fun pair vertices graph ->
    clique_def vertices graph;
    let u = () in
    match vertices with
    | [] -> refine_ u
    | vertex :: rest ->
      connect_one_preserves pair vertex rest graph;
      clique_preserves pair rest (connect_one vertex rest graph);
      refine_ u

  let rec (clique_covers @ total) :
    (vertices : int list) -> (graph : edge list) -> (a : int) -> (b : int) ->
    {u : unit |
      not (member a vertices && member b vertices) || a = b
      || adjacent a b (clique vertices graph)}
    @ immutable contended =
    fun vertices graph a b ->
    clique_def vertices graph;
    member_def a vertices;
    member_def b vertices;
    let u = () in
    match vertices with
    | [] -> refine_ u
    | vertex :: rest ->
      if vertex = a then begin
        connect_one_covers vertex rest graph b;
        clique_preserves (edge a b) rest (connect_one vertex rest graph);
        adjacent_def a b (connect_one vertex rest graph);
        adjacent_def a b (clique rest (connect_one vertex rest graph));
        refine_ u
      end else if vertex = b then begin
        connect_one_covers vertex rest graph a;
        edge_symmetric a b;
        clique_preserves (edge a b) rest (connect_one vertex rest graph);
        adjacent_def vertex a (connect_one vertex rest graph);
        adjacent_def a b (clique rest (connect_one vertex rest graph));
        refine_ u
      end else begin
        clique_covers rest (connect_one vertex rest graph) a b;
        refine_ u
      end

  let rec (interference_preserves @ total) :
    (pair : edge) -> (code : instruction list) ->
    (live : int list list) -> (index : int) -> (graph : edge list) ->
    {u : unit |
      not (member_edge pair graph)
      || member_edge pair (interference code live index graph)}
    @ immutable contended =
    fun pair code live index graph ->
    interference_def code live index graph;
    let u = () in
    match code with
    | [] -> refine_ u
    | instruction :: rest ->
      (match definition instruction with
       | None -> interference_preserves pair rest live (index + 1) graph
       | Some dst ->
         let next_graph =
           connect_one dst (live_out live (successors instruction)) graph in
         connect_one_preserves pair dst (live_out live (successors instruction)) graph;
         interference_preserves pair rest live (index + 1) next_graph);
      refine_ u

  let rec (interference_covers @ total) :
    (code : instruction list) -> (live : int list list) ->
    (index : int) -> (graph : edge list) -> (pc : int) ->
    (dst : int) -> (other : int) ->
    {u : unit |
      (match nth code pc with
       | None -> true
       | Some instruction ->
         not (definition instruction === Some dst)
         || not (member other (live_out live (successors instruction)))
         || dst = other
         || adjacent dst other (interference code live index graph))}
    @ immutable contended =
    fun code live index graph pc dst other ->
    nth_def code pc;
    interference_def code live index graph;
    let u = () in
    match code with
    | [] -> refine_ u
    | instruction :: rest ->
      let next_graph = match definition instruction with
        | None -> graph
        | Some defined ->
          connect_one defined (live_out live (successors instruction)) graph in
      if pc = 0 then begin
        (match definition instruction with
         | None -> ()
         | Some defined ->
           connect_one_covers defined
             (live_out live (successors instruction)) graph other;
           interference_preserves (edge dst other) rest live (index + 1) next_graph;
           adjacent_def dst other next_graph;
           adjacent_def dst other (interference rest live (index + 1) next_graph));
        refine_ u
      end else begin
        interference_covers rest live (index + 1) next_graph
          (pc - 1) dst other;
        refine_ u
      end

  let (graph_entry_covers @ total) :
    (code : instruction list) -> (live : int list list) ->
    (a : int) -> (b : int) ->
    {u : unit |
      (match live with
       | [] -> true
       | entry :: _ ->
         not (member a entry && member b entry) || a = b
         || adjacent a b (graph code live))}
    @ immutable contended =
    fun code live a b ->
    graph_def code live;
    let u = () in
    match live with
    | [] -> refine_ u
    | entry :: _ ->
      clique_covers entry [] a b;
      interference_preserves (edge a b) code live 0 (clique entry []);
      adjacent_def a b (clique entry []);
      adjacent_def a b (graph code live);
      refine_ u

  let (graph_write_covers @ total) :
    (code : instruction list) -> (live : int list list) ->
    (pc : int) -> (dst : int) -> (other : int) ->
    {u : unit |
      (match nth code pc with
       | None -> true
       | Some instruction ->
         not (definition instruction === Some dst)
         || not (member other (live_out live (successors instruction)))
         || dst = other || adjacent dst other (graph code live))}
    @ immutable contended =
    fun code live pc dst other ->
    graph_def code live;
    let entry = match live with [] -> [] | entry :: _ -> entry in
    interference_covers code live 0 (clique entry []) pc dst other;
    refine_ ()

  let[@def] rec (conflicts @ total) graph vertex (candidate : int) (colors : int list) index = match colors with
    | [] -> false
    | color :: rest ->
      (adjacent vertex index graph && candidate = color)
      || conflicts graph vertex candidate rest (index + 1)

  let rec (conflicts_pair @ total) :
    (graph : edge list) -> (vertex : int) -> (candidate : int) ->
    (colors : int list) -> (index : int) -> (offset : int) ->
    {u : unit |
      not (adjacent vertex (index + offset) graph)
      || not (nth colors offset === Some candidate)
      || conflicts graph vertex candidate colors index}
    @ immutable contended =
    fun graph vertex candidate colors index offset ->
    conflicts_def graph vertex candidate colors index;
    nth_def colors offset;
    let u = () in
    match colors with
    | [] -> refine_ u
    | color :: rest ->
      if offset = 0 then refine_ u
      else begin
        conflicts_pair graph vertex candidate rest (index + 1) (offset - 1);
        refine_ u
      end

  let[@def] rec (first_color @ total) graph vertex colors other_index choices =
    match choices with
    | [] -> None
    | candidate :: rest ->
      if conflicts graph vertex candidate colors other_index then
        first_color graph vertex colors other_index rest
      else Some candidate

  let rec (first_color_sound @ total) :
    (graph : edge list) -> (vertex : int) -> (colors : int list) ->
    (other_index : int) -> (choices : int list) ->
    {u : unit |
      match first_color graph vertex colors other_index choices with
      | None -> true
      | Some chosen -> not (conflicts graph vertex chosen colors other_index)}
      @ immutable contended =
    fun graph vertex colors other_index choices ->
    first_color_def graph vertex colors other_index choices;
    let u = () in
    match choices with
    | [] -> refine_ u
    | candidate :: rest ->
      if conflicts graph vertex candidate colors other_index then begin
        first_color_sound graph vertex colors other_index rest;
        refine_ u
      end else refine_ u

  let[@def] rec (proper_from @ total) graph index colors = match colors with
    | [] -> true
    | chosen :: rest ->
      not (conflicts graph index chosen rest (index + 1))
      && proper_from graph (index + 1) rest

  let rec (proper_separates @ total) :
    (graph : edge list) -> (index : int) -> (colors : int list) ->
    (left : int) -> (right : int) -> (color : int) ->
    {u : unit |
      not (proper_from graph index colors)
      || left < index || left >= right
      || not (adjacent left right graph)
      || not (nth colors (left - index) === Some color)
      || not (nth colors (right - index) === Some color)}
    @ immutable contended =
    fun graph index colors left right color ->
    proper_from_def graph index colors;
    nth_def colors (left - index);
    nth_def colors (right - index);
    let u = () in
    match colors with
    | [] -> refine_ u
    | chosen :: rest ->
      if left = index then begin
        conflicts_pair graph index chosen rest (index + 1)
          (right - index - 1);
        refine_ u
      end else begin
        proper_separates graph (index + 1) rest left right color;
        refine_ u
      end

  let (proper_distinct @ total) :
    (graph : edge list) -> (colors : int list) ->
    (a : int) -> (b : int) -> (color_a : int) -> (color_b : int) ->
    {u : unit |
      not (proper_from graph 0 colors
           && 0 <= a && 0 <= b && a <> b
           && adjacent a b graph
           && nth colors a === Some color_a
           && nth colors b === Some color_b)
      || color_a <> color_b}
    @ immutable contended =
    fun graph colors a b color_a color_b ->
    let u = () in
    if color_a <> color_b || a = b || a < 0 || b < 0 then refine_ u
    else if a < b then begin
      proper_separates graph 0 colors a b color_a;
      refine_ u
    end else begin
      edge_symmetric a b;
      adjacent_def a b graph;
      adjacent_def b a graph;
      proper_separates graph 0 colors b a color_a;
      refine_ u
    end

  let[@def] rec (color @ total) graph choices index vertices = match vertices with
    | [] -> Some []
    | _ :: rest ->
      (match color graph choices (index + 1) rest with
       | None -> None
       | Some colors ->
         match first_color graph index colors (index + 1) choices with
         | None -> None
         | Some chosen -> Some (chosen :: colors))

  let rec (color_sound @ total) :
    (graph : edge list) -> (choices : int list) -> (index : int) ->
    (vertices : int list) ->
    {u : unit |
      match color graph choices index vertices with
      | None -> true
      | Some colors -> proper_from graph index colors}
      @ immutable contended =
    fun graph choices index vertices ->
    color_def graph choices index vertices;
    let u = () in
    match vertices with
    | [] ->
      proper_from_def graph index [];
      refine_ u
    | _ :: rest ->
      color_sound graph choices (index + 1) rest;
      (match color graph choices (index + 1) rest with
       | None -> refine_ u
       | Some colors ->
         first_color_sound graph index colors (index + 1) choices;
         (match first_color graph index colors (index + 1) choices with
          | None -> refine_ u
          | Some chosen ->
            proper_from_def graph index (chosen :: colors);
            refine_ u))

  let (color_separates @ total) :
    (graph : edge list) -> (choices : int list) ->
    (vertices : int list) -> (a : int) -> (b : int) -> (chosen : int) ->
    {u : unit |
      match color graph choices 0 vertices with
      | None -> true
      | Some colors ->
        a < 0 || b < 0 || a = b || not (adjacent a b graph)
        || not (nth colors a === Some chosen)
        || not (nth colors b === Some chosen)}
    @ immutable contended =
    fun graph choices vertices a b chosen ->
    color_sound graph choices 0 vertices;
    let u = () in
    match color graph choices 0 vertices with
    | None -> refine_ u
    | Some colors ->
      if a < 0 || b < 0 then refine_ u
      else if a < b then begin
        proper_separates graph 0 colors a b chosen;
        refine_ u
      end else if b < a then begin
        edge_symmetric a b;
        adjacent_def a b graph;
        adjacent_def b a graph;
        proper_separates graph 0 colors b a chosen;
        refine_ u
      end else refine_ u

  let[@def] (rename_operand @ total) colors operand = match operand with
    | Imm word -> Some (Imm word)
    | Reg r -> (match nth colors r with None -> None | Some color -> Some (Reg color))

  let[@def] (rename_instruction @ total) colors instruction = match instruction with
    | Move (dst, operand, next) ->
      (match nth colors dst, rename_operand colors operand with
       | Some dst, Some operand -> Some (Move (dst, operand, next))
       | _ -> None)
    | Binary (dst, operation, left, right, next) ->
      (match nth colors dst, rename_operand colors left, rename_operand colors right with
       | Some dst, Some left, Some right -> Some (Binary (dst, operation, left, right, next))
       | _ -> None)
    | Jump next -> Some (Jump next)
    | Branch (condition, yes, no) ->
      (match rename_operand colors condition with
       | None -> None
       | Some condition -> Some (Branch (condition, yes, no)))
    | Return operand ->
      (match rename_operand colors operand with None -> None | Some operand -> Some (Return operand))

  let[@def] rec (rename @ total) colors code = match code with
    | [] -> Some []
    | instruction :: rest ->
      (match rename_instruction colors instruction, rename colors rest with
       | Some instruction, Some rest -> Some (instruction :: rest)
       | _ -> None)

  let rec (rename_nth @ total) :
    (colors : int list) -> (code : instruction list) ->
    (pc : int) -> (instruction : instruction) ->
    {u : unit |
      match rename colors code with
      | None -> true
      | Some target ->
        not (nth code pc === Some instruction)
        || nth target pc === rename_instruction colors instruction}
    @ immutable contended =
    fun colors code pc instruction ->
    rename_def colors code;
    nth_def code pc;
    let u = () in
    match code with
    | [] -> refine_ u
    | head :: rest ->
      if pc = 0 then begin
        (match rename_instruction colors head, rename colors rest with
         | Some target_head, Some target_rest ->
           nth_def (target_head :: target_rest) pc
         | _ -> ());
        refine_ u
      end else begin
        rename_nth colors rest (pc - 1) instruction;
        (match rename_instruction colors head, rename colors rest with
         | Some target_head, Some target_rest ->
           nth_def (target_head :: target_rest) pc
         | _ -> ());
        refine_ u
      end

  let[@def] rec (agree_on @ total) live colors (source : int list) (target : int list) =
    match live with
    | [] -> true
    | reg :: rest ->
      (match nth colors reg with
       | None -> false
       | Some physical ->
         match nth source reg, nth target physical with
         | Some left, Some right -> left = right
         | _ -> false)
      && agree_on rest colors source target

  let rec (agree_lookup @ total) :
    (live : int list) -> (colors : int list) ->
    (source : int list) -> (target : int list) -> (reg : int) ->
    {u : unit |
      not (agree_on live colors source target && member reg live)
      || (match nth colors reg with
          | None -> false
          | Some physical ->
            match nth source reg, nth target physical with
            | Some left, Some right -> left = right
            | _ -> false)}
    @ immutable contended =
    fun live colors source target reg ->
    agree_on_def live colors source target;
    member_def reg live;
    let u = () in
    match live with
    | [] -> refine_ u
    | head :: rest ->
      if head = reg then refine_ u
      else begin agree_lookup rest colors source target reg; refine_ u end

  let[@def] rec (separate_from @ total) regs (colors : int list)
      (head : int) (physical : int) =
    match regs with
    | [] -> true
    | reg :: rest ->
      (reg = head || match nth colors reg with
       | None -> false
       | Some chosen -> chosen <> physical)
      && separate_from rest colors head physical

  let rec (target_write_keeps_agreement @ total) :
    (regs : int list) -> (colors : int list) ->
    (source : int list) -> (target : int list) ->
    (head : int) -> (physical : int) -> (word : int) ->
    {u : unit |
      not (agree_on regs colors source target
           && separate_from regs colors head physical
           && nth source head === Some word
           && nth colors head === Some physical)
      || (match write target physical word with
          | None -> true
          | Some after -> agree_on regs colors source after)}
    @ immutable contended =
    fun regs colors source target head physical word ->
    agree_on_def regs colors source target;
    separate_from_def regs colors head physical;
    let u = () in
    match regs with
    | [] ->
      (match write target physical word with
       | None -> ()
       | Some after -> agree_on_def [] colors source after);
      refine_ u
    | reg :: rest ->
      target_write_keeps_agreement rest colors source target head physical word;
      (match nth colors reg with
       | None -> refine_ u
       | Some chosen ->
         if reg = head then write_reads target physical word
         else write_preserves target physical word chosen;
         (match write target physical word with
          | None -> refine_ u
          | Some after ->
            agree_on_def regs colors source after;
            refine_ u))

  let (value_agrees @ total) :
    (live : int list) -> (colors : int list) ->
    (source : int list) -> (target : int list) ->
    (operand : operand) ->
    {u : unit |
      not (agree_on live colors source target
           && subset (operand_uses operand) live)
      || (match rename_operand colors operand with
          | None -> true
          | Some renamed -> value source operand === value target renamed)}
    @ immutable contended =
    fun live colors source target operand ->
    rename_operand_def colors operand;
    value_def source operand;
    let u = () in
    match operand with
    | Imm word -> value_def target (Imm word); refine_ u
    | Reg reg ->
      operand_uses_def operand;
      member_def reg [reg];
      subset_member (operand_uses operand) live reg;
      agree_lookup live colors source target reg;
      (match nth colors reg with
       | None -> refine_ u
       | Some physical ->
         value_def target (Reg physical);
         refine_ u)

  let[@def] (operand_live @ total) live operand = match operand with
    | Imm _ -> true
    | Reg reg -> member reg live

  let (value_agrees_live @ total) :
    (live : int list) -> (colors : int list) ->
    (source : int list) -> (target : int list) ->
    (operand : operand) ->
    {u : unit |
      not (agree_on live colors source target && operand_live live operand)
      || (match rename_operand colors operand with
          | None -> true
          | Some renamed -> value source operand === value target renamed)}
    @ immutable contended =
    fun live colors source target operand ->
    operand_live_def live operand;
    rename_operand_def colors operand;
    value_def source operand;
    let u = () in
    match operand with
    | Imm word -> value_def target (Imm word); refine_ u
    | Reg reg ->
      agree_lookup live colors source target reg;
      (match nth colors reg with
       | None -> refine_ u
       | Some physical ->
         value_def target (Reg physical);
         refine_ u)

  let (binary_operands_live @ total) :
    (before : int list) -> (dst : int) -> (operation : operation) ->
    (left : operand) -> (right : operand) -> (next : int) ->
    {u : unit |
      not (subset (uses (Binary (dst, operation, left, right, next))) before)
      || (operand_live before left && operand_live before right)}
    @ immutable contended =
    fun before dst operation left right next ->
    let instruction = Binary (dst, operation, left, right, next) in
    uses_def instruction;
    let u = () in
    (match left with
     | Imm _ -> operand_live_def before left
     | Reg reg ->
       operand_uses_def left;
       member_def reg [reg];
       union_keeps_left (operand_uses left) (operand_uses right) reg;
       subset_member (uses instruction) before reg;
       operand_live_def before left);
    (match right with
     | Imm _ -> operand_live_def before right
     | Reg reg ->
       operand_uses_def right;
       member_def reg [reg];
       union_keeps_right (operand_uses left) (operand_uses right) reg;
       subset_member (uses instruction) before reg;
       operand_live_def before right);
    refine_ u

  let[@def] rec (safe_write @ total) next before (colors : int list)
      (dst : int) (physical : int) =
    match next with
    | [] -> true
    | reg :: rest ->
      (reg = dst
       || (member reg before
           && match nth colors reg with
              | None -> false
              | Some color -> color <> physical))
      && safe_write rest before colors dst physical

  let rec (write_agreement @ total) :
    (next : int list) -> (before : int list) ->
    (colors : int list) -> (source : int list) -> (target : int list) ->
    (dst : int) -> (physical : int) -> (word : int) ->
    {u : unit |
      not (agree_on before colors source target
           && safe_write next before colors dst physical
           && nth colors dst === Some physical)
      || (match write source dst word, write target physical word with
          | Some source_after, Some target_after ->
            agree_on next colors source_after target_after
          | _ -> true)}
    @ immutable contended =
    fun next before colors source target dst physical word ->
    safe_write_def next before colors dst physical;
    agree_on_def next colors
      (match write source dst word with None -> [] | Some xs -> xs)
      (match write target physical word with None -> [] | Some xs -> xs);
    let u = () in
    match next with
    | [] -> refine_ u
    | reg :: rest ->
      write_agreement rest before colors source target dst physical word;
      (match write source dst word, write target physical word with
       | Some source_after, Some target_after ->
         if reg = dst then begin
           write_reads source dst word;
           write_reads target physical word;
           refine_ u
         end else begin
           agree_lookup before colors source target reg;
           write_preserves source dst word reg;
           (match nth colors reg with
            | None -> refine_ u
            | Some color ->
              write_preserves target physical word color;
              refine_ u)
         end
       | _ -> refine_ u)

  let[@def] rec (length @ total) xs = match xs with [] -> 0 | _ :: rest -> 1 + length rest

  let rec (empty_live_length @ total) :
    (code : instruction list) ->
    {u : unit | length (empty_live code) = length code}
    @ immutable contended =
    fun code ->
    length_def code;
    empty_live_def code;
    let u = () in
    match code with
    | [] ->
      length_def [];
      length_def (empty_live code);
      refine_ u
    | _ :: rest ->
      empty_live_length rest;
      let tail = empty_live rest in
      length_def ([] :: tail);
      refine_ u

  let rec (sweep_length @ total) :
    (code : instruction list) -> (live : int list list) -> (index : int) ->
    {u : unit |
      match sweep code live index with
      | next, _ -> length next = length code}
    @ immutable contended =
    fun code live index ->
    sweep_def code live index;
    length_def code;
    let u = () in
    match code with
    | [] ->
      (match sweep code live index with next, _ -> length_def next);
      length_def [];
      refine_ u
    | instruction :: rest ->
      sweep_length rest live (index + 1);
      (match sweep rest live (index + 1) with
       | tail, _ ->
         let old = match nth live index with None -> [] | Some xs -> xs in
         let next = union old (transfer live instruction) in
         length_def (next :: tail));
      refine_ u

  let rec (stabilize_length @ total) :
    (fuel : int) -> (code : instruction list) -> (live : int list list) ->
    {u : unit |
      length live <> length code
      || (match stabilize fuel code live with
          | None -> true
          | Some result -> length result = length code)}
    @ immutable contended =
    fun fuel code live ->
    stabilize_def fuel code live;
    let u = () in
    if fuel <= 0 then refine_ u
    else begin
      sweep_length code live 0;
      (match sweep code live 0 with
       | next, changed ->
         if changed then stabilize_length (fuel - 1) code next);
      refine_ u
    end
  [@@decreases fuel]

  let rec (color_length @ total) :
    (graph : edge list) -> (choices : int list) ->
    (index : int) -> (vertices : int list) ->
    {u : unit |
      match color graph choices index vertices with
      | None -> true
      | Some colors -> length colors = length vertices}
    @ immutable contended =
    fun graph choices index vertices ->
    color_def graph choices index vertices;
    length_def vertices;
    let u = () in
    match vertices with
    | [] -> length_def []; refine_ u
    | _ :: rest ->
      color_length graph choices (index + 1) rest;
      (match color graph choices (index + 1) rest with
       | None -> refine_ u
       | Some colors ->
         (match first_color graph index colors (index + 1) choices with
          | None -> refine_ u
          | Some chosen -> length_def (chosen :: colors); refine_ u))

  let rec (rename_length @ total) :
    (colors : int list) -> (code : instruction list) ->
    {u : unit |
      match rename colors code with
      | None -> true
      | Some target -> length target = length code}
    @ immutable contended =
    fun colors code ->
    rename_def colors code;
    length_def code;
    let u = () in
    match code with
    | [] -> length_def []; refine_ u
    | instruction :: rest ->
      rename_length colors rest;
      (match rename_instruction colors instruction, rename colors rest with
       | Some renamed, Some renamed_rest ->
         length_def (renamed :: renamed_rest);
         refine_ u
       | _ -> refine_ u)

  let rec (zeros_length @ total) :
    (count : int) ->
    {u : unit | count < 0 || length (zeros count) = count}
    @ immutable contended =
    fun count ->
    zeros_def count;
    let u = () in
    if count < 0 then refine_ u
    else if count = 0 then begin
      length_def [];
      length_def (zeros count);
      refine_ u
    end
    else begin
      zeros_length (count - 1);
      length_def (0 :: zeros (count - 1));
      refine_ u
    end
  [@@decreases count]

  let rec (nth_present @ total) :
    (xs : int list) -> (index : int) ->
    {u : unit |
      index < 0 || index >= length xs
      || (match nth xs index with None -> false | Some _ -> true)}
    @ immutable contended =
    fun xs index ->
    nth_def xs index;
    length_def xs;
    let u = () in
    match xs with
    | [] -> refine_ u
    | _ :: rest ->
      if index = 0 then refine_ u
      else begin nth_present rest (index - 1); refine_ u end

  let rec (nth_live_present @ total) :
    (xs : int list list) -> (index : int) ->
    {u : unit |
      index < 0 || index >= length xs
      || (match nth xs index with None -> false | Some _ -> true)}
    @ immutable contended =
    fun xs index ->
    nth_def xs index;
    length_def xs;
    let u = () in
    match xs with
    | [] -> refine_ u
    | _ :: rest ->
      if index = 0 then refine_ u
      else begin nth_live_present rest (index - 1); refine_ u end

  let rec (nth_code_present @ total) :
    (xs : instruction list) -> (index : int) ->
    {u : unit |
      index < 0 || index >= length xs
      || (match nth xs index with None -> false | Some _ -> true)}
    @ immutable contended =
    fun xs index ->
    nth_def xs index;
    length_def xs;
    let u = () in
    match xs with
    | [] -> refine_ u
    | _ :: rest ->
      if index = 0 then refine_ u
      else begin nth_code_present rest (index - 1); refine_ u end

  let rec (write_present @ total) :
    (xs : int list) -> (index : int) -> (word : int) ->
    {u : unit |
      index < 0 || index >= length xs
      || (match write xs index word with None -> false | Some _ -> true)}
    @ immutable contended =
    fun xs index word ->
    write_def xs index word;
    length_def xs;
    let u = () in
    match xs with
    | [] -> refine_ u
    | _ :: rest ->
      if index = 0 then refine_ u
      else begin write_present rest (index - 1) word; refine_ u end

  let rec (write_length @ total) :
    (xs : int list) -> (index : int) -> (word : int) ->
    {u : unit |
      match write xs index word with
      | None -> true
      | Some ys -> length ys = length xs}
    @ immutable contended =
    fun xs index word ->
    write_def xs index word;
    length_def xs;
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      if index = 0 then begin length_def (word :: rest); refine_ u end
      else begin
        write_length rest (index - 1) word;
        (match write rest (index - 1) word with
         | None -> ()
         | Some result -> length_def (head :: result));
        refine_ u
      end

  let[@def] (valid_reg @ total) count reg = 0 <= reg && reg < count

  let[@def] (valid_operand @ total) count operand = match operand with
    | Imm _ -> true
    | Reg reg -> valid_reg count reg

  let (value_present @ total) :
    (count : int) -> (file : int list) -> (operand : operand) ->
    {u : unit |
      not (length file = count && valid_operand count operand)
      || (match value file operand with None -> false | Some _ -> true)}
    @ immutable contended =
    fun count file operand ->
    valid_operand_def count operand;
    value_def file operand;
    let u = () in
    match operand with
    | Imm _ -> refine_ u
    | Reg reg ->
      valid_reg_def count reg;
      nth_present file reg;
      refine_ u

  let[@def] rec (all_valid_reg @ total) count regs = match regs with
    | [] -> true
    | reg :: rest -> valid_reg count reg && all_valid_reg count rest

  let rec (all_valid_reg_weaken @ total) :
    (smaller : int) -> (larger : int) -> (regs : int list) ->
    {u : unit |
      smaller > larger || not (all_valid_reg smaller regs)
      || all_valid_reg larger regs}
    @ immutable contended =
    fun smaller larger regs ->
    all_valid_reg_def smaller regs;
    all_valid_reg_def larger regs;
    let u = () in
    match regs with
    | [] -> refine_ u
    | reg :: rest ->
      valid_reg_def smaller reg;
      valid_reg_def larger reg;
      all_valid_reg_weaken smaller larger rest;
      refine_ u

  let rec (range_down_valid @ total) :
    (count : int) ->
    {u : unit | all_valid_reg count (range_down count)}
    @ immutable contended =
    fun count ->
    let u = () in
    if count <= 0 then begin
      range_down_def count;
      all_valid_reg_def count [];
      refine_ u
    end else begin
      range_down_valid (count - 1);
      all_valid_reg_weaken (count - 1) count (range_down (count - 1));
      range_down_def count;
      all_valid_reg_def count ((count - 1) :: range_down (count - 1));
      valid_reg_def count (count - 1);
      refine_ u
    end
  [@@decreases count]

  let rec (reverse_into_valid @ total) :
    (count : int) -> (xs : int list) -> (acc : int list) ->
    {u : unit |
      not (all_valid_reg count xs && all_valid_reg count acc)
      || all_valid_reg count (reverse_into xs acc)}
    @ immutable contended =
    fun count xs acc ->
    reverse_into_def xs acc;
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      all_valid_reg_def count xs;
      all_valid_reg_def count (head :: acc);
      reverse_into_valid count rest (head :: acc);
      refine_ u

  let (range_valid @ total) :
    (count : int) -> {u : unit | all_valid_reg count (range count)}
    @ immutable contended =
    fun count ->
    range_down_valid count;
    reverse_into_valid count (range_down count) [];
    range_def count;
    reverse_def (range_down count);
    all_valid_reg_def count [];
    refine_ ()

  let rec (first_color_valid @ total) :
    (graph : edge list) -> (vertex : int) ->
    (colors : int list) -> (index : int) ->
    (choices : int list) -> (count : int) ->
    {u : unit |
      not (all_valid_reg count choices)
      || (match first_color graph vertex colors index choices with
          | None -> true
          | Some chosen -> valid_reg count chosen)}
    @ immutable contended =
    fun graph vertex colors index choices count ->
    first_color_def graph vertex colors index choices;
    all_valid_reg_def count choices;
    let u = () in
    match choices with
    | [] -> refine_ u
    | _ :: rest ->
      first_color_valid graph vertex colors index rest count;
      refine_ u

  let rec (color_valid @ total) :
    (graph : edge list) -> (choices : int list) ->
    (index : int) -> (vertices : int list) -> (count : int) ->
    {u : unit |
      not (all_valid_reg count choices)
      || (match color graph choices index vertices with
          | None -> true
          | Some colors -> all_valid_reg count colors)}
    @ immutable contended =
    fun graph choices index vertices count ->
    color_def graph choices index vertices;
    let u = () in
    match vertices with
    | [] -> all_valid_reg_def count []; refine_ u
    | _ :: rest ->
      color_valid graph choices (index + 1) rest count;
      (match color graph choices (index + 1) rest with
       | None -> refine_ u
       | Some colors ->
         first_color_valid graph index colors (index + 1) choices count;
         (match first_color graph index colors (index + 1) choices with
          | None -> refine_ u
          | Some chosen ->
            all_valid_reg_def count (chosen :: colors);
            refine_ u))

  let rec (all_valid_reg_lookup @ total) :
    (count : int) -> (regs : int list) -> (index : int) -> (reg : int) ->
    {u : unit |
      not (all_valid_reg count regs && nth regs index === Some reg)
      || valid_reg count reg}
    @ immutable contended =
    fun count regs index reg ->
    all_valid_reg_def count regs;
    nth_def regs index;
    let u = () in
    match regs with
    | [] -> refine_ u
    | _ :: rest ->
      if index = 0 then refine_ u
      else begin
        all_valid_reg_lookup count rest (index - 1) reg;
        refine_ u
      end

  let (color_total @ total) :
    (graph : edge list) -> (registers : int) -> (physical : int) ->
    {u : unit |
      registers < 0
      || (match color graph (range physical) 0 (zeros registers) with
          | None -> true
          | Some colors ->
            length colors = registers && all_valid_reg physical colors)}
    @ immutable contended =
    fun graph registers physical ->
    range_valid physical;
    zeros_length registers;
    color_length graph (range physical) 0 (zeros registers);
    color_valid graph (range physical) 0 (zeros registers) physical;
    refine_ ()

  let (color_lookup @ total) :
    (graph : edge list) -> (registers : int) -> (physical : int) ->
    (reg : int) ->
    {u : unit |
      registers < 0 || reg < 0 || reg >= registers
      || (match color graph (range physical) 0 (zeros registers) with
          | None -> true
          | Some colors ->
            match nth colors reg with
            | None -> false
            | Some chosen -> valid_reg physical chosen)}
    @ immutable contended =
    fun graph registers physical reg ->
    color_total graph registers physical;
    let u = () in
    match color graph (range physical) 0 (zeros registers) with
    | None -> refine_ u
    | Some colors ->
      nth_present colors reg;
      (match nth colors reg with
       | None -> refine_ u
       | Some chosen ->
         all_valid_reg_lookup physical colors reg chosen;
         refine_ u)

  let[@def] (valid_instruction @ total) registers nodes instruction =
    let valid_label label = valid_reg nodes label in
    match instruction with
    | Move (dst, operand, next) ->
      valid_reg registers dst && valid_operand registers operand && valid_label next
    | Binary (dst, _, left, right, next) ->
      valid_reg registers dst && valid_operand registers left
      && valid_operand registers right && valid_label next
    | Jump next -> valid_label next
    | Branch (condition, yes, no) ->
      valid_operand registers condition && valid_label yes && valid_label no
    | Return operand -> valid_operand registers operand

  let[@def] rec (all_valid_instructions @ total) registers nodes code = match code with
    | [] -> true
    | instruction :: rest ->
      valid_instruction registers nodes instruction
      && all_valid_instructions registers nodes rest

  let[@def] (valid @ total) program =
    let nodes = length program.code in
    0 < nodes && nodes <= 64 && 0 < program.registers && program.registers <= 32
    && all_valid_reg program.registers program.inputs
    && all_valid_instructions program.registers nodes program.code

  let rec (valid_instruction_lookup @ total) :
    (registers : int) -> (nodes : int) -> (code : instruction list) ->
    (pc : int) -> (instruction : instruction) ->
    {u : unit |
      not (all_valid_instructions registers nodes code
           && nth code pc === Some instruction)
      || valid_instruction registers nodes instruction}
    @ immutable contended =
    fun registers nodes code pc instruction ->
    all_valid_instructions_def registers nodes code;
    nth_def code pc;
    let u = () in
    match code with
    | [] -> refine_ u
    | _ :: rest ->
      if pc = 0 then refine_ u
      else begin
        valid_instruction_lookup registers nodes rest (pc - 1) instruction;
        refine_ u
      end

  let (valid_successor @ total) :
    (registers : int) -> (nodes : int) ->
    (instruction : instruction) -> (successor : int) ->
    {u : unit |
      not (valid_instruction registers nodes instruction
           && member successor (successors instruction))
      || valid_reg nodes successor}
    @ immutable contended =
    fun registers nodes instruction successor ->
    valid_instruction_def registers nodes instruction;
    successors_def instruction;
    let u = () in
    match instruction with
    | Move (_, _, next) ->
      member_def successor [next];
      member_def successor [];
      valid_reg_def nodes next;
      valid_reg_def nodes successor;
      refine_ u
    | Binary (_, _, _, _, next) ->
      member_def successor [next];
      member_def successor [];
      valid_reg_def nodes next;
      valid_reg_def nodes successor;
      refine_ u
    | Jump next ->
      member_def successor [next];
      member_def successor [];
      valid_reg_def nodes next;
      valid_reg_def nodes successor;
      refine_ u
    | Branch (_, yes, no) ->
      member_def successor [yes; no];
      member_def successor [no];
      member_def successor [];
      valid_reg_def nodes yes;
      valid_reg_def nodes no;
      valid_reg_def nodes successor;
      refine_ u
    | Return _ -> member_def successor []; refine_ u

  let (rename_operand_valid @ total) :
    (colors : int list) -> (registers : int) -> (physical : int) ->
    (operand : operand) ->
    {u : unit |
      not (length colors = registers
           && all_valid_reg physical colors
           && valid_operand registers operand)
      || (match rename_operand colors operand with
          | None -> true
          | Some renamed -> valid_operand physical renamed)}
    @ immutable contended =
    fun colors registers physical operand ->
    rename_operand_def colors operand;
    valid_operand_def registers operand;
    let u = () in
    match operand with
    | Imm _ -> valid_operand_def physical operand; refine_ u
    | Reg reg ->
      (match nth colors reg with
       | None -> refine_ u
       | Some chosen ->
         all_valid_reg_lookup physical colors reg chosen;
         valid_operand_def physical (Reg chosen);
         refine_ u)

  let (rename_instruction_valid @ total) :
    (colors : int list) -> (registers : int) -> (physical : int) ->
    (nodes : int) -> (instruction : instruction) ->
    {u : unit |
      not (length colors = registers
           && all_valid_reg physical colors
           && valid_instruction registers nodes instruction)
      || (match rename_instruction colors instruction with
          | None -> true
          | Some renamed -> valid_instruction physical nodes renamed)}
    @ immutable contended =
    fun colors registers physical nodes instruction ->
    rename_instruction_def colors instruction;
    valid_instruction_def registers nodes instruction;
    let u = () in
    match instruction with
    | Move (dst, operand, next) ->
      rename_operand_valid colors registers physical operand;
      (match nth colors dst, rename_operand colors operand with
       | Some chosen, Some renamed ->
         all_valid_reg_lookup physical colors dst chosen;
         valid_instruction_def physical nodes (Move (chosen, renamed, next));
         refine_ u
       | _ -> refine_ u)
    | Binary (dst, operation, left, right, next) ->
      rename_operand_valid colors registers physical left;
      rename_operand_valid colors registers physical right;
      (match nth colors dst, rename_operand colors left, rename_operand colors right with
       | Some chosen, Some left, Some right ->
         all_valid_reg_lookup physical colors dst chosen;
         valid_instruction_def physical nodes
           (Binary (chosen, operation, left, right, next));
         refine_ u
       | _ -> refine_ u)
    | Jump next ->
      valid_instruction_def physical nodes (Jump next);
      refine_ u
    | Branch (condition, yes, no) ->
      rename_operand_valid colors registers physical condition;
      (match rename_operand colors condition with
       | None -> refine_ u
       | Some renamed ->
         valid_instruction_def physical nodes (Branch (renamed, yes, no));
         refine_ u)
    | Return operand ->
      rename_operand_valid colors registers physical operand;
      (match rename_operand colors operand with
       | None -> refine_ u
       | Some renamed ->
         valid_instruction_def physical nodes (Return renamed);
         refine_ u)

  let[@def] rec (all_valid_live @ total) count live = match live with
    | [] -> true
    | row :: rest -> all_valid_reg count row && all_valid_live count rest

  let rec (all_valid_live_lookup @ total) :
    (count : int) -> (live : int list list) ->
    (index : int) -> (row : int list) ->
    {u : unit |
      not (all_valid_live count live && nth live index === Some row)
      || all_valid_reg count row}
    @ immutable contended =
    fun count live index row ->
    all_valid_live_def count live;
    nth_def live index;
    let u = () in
    match live with
    | [] -> refine_ u
    | _ :: rest ->
      if index = 0 then refine_ u
      else begin all_valid_live_lookup count rest (index - 1) row; refine_ u end

  let (add_valid @ total) :
    (count : int) -> (value : int) -> (xs : int list) ->
    {u : unit |
      not (valid_reg count value && all_valid_reg count xs)
      || all_valid_reg count (add value xs)}
    @ immutable contended =
    fun count value xs ->
    add_def value xs;
    all_valid_reg_def count (value :: xs);
    refine_ ()

  let rec (union_valid @ total) :
    (count : int) -> (xs : int list) -> (ys : int list) ->
    {u : unit |
      not (all_valid_reg count xs && all_valid_reg count ys)
      || all_valid_reg count (union xs ys)}
    @ immutable contended =
    fun count xs ys ->
    union_def xs ys;
    all_valid_reg_def count xs;
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      union_valid count rest ys;
      add_valid count head (union rest ys);
      refine_ u

  let rec (remove_valid @ total) :
    (count : int) -> (removed : int) -> (xs : int list) ->
    {u : unit |
      not (all_valid_reg count xs)
      || all_valid_reg count (remove removed xs)}
    @ immutable contended =
    fun count removed xs ->
    remove_def removed xs;
    all_valid_reg_def count xs;
    let u = () in
    match xs with
    | [] -> refine_ u
    | head :: rest ->
      remove_valid count removed rest;
      if head = removed then refine_ u
      else begin
        all_valid_reg_def count (head :: remove removed rest);
        refine_ u
      end

  let (operand_uses_valid @ total) :
    (count : int) -> (operand : operand) ->
    {u : unit |
      not (valid_operand count operand)
      || all_valid_reg count (operand_uses operand)}
    @ immutable contended =
    fun count operand ->
    operand_uses_def operand;
    valid_operand_def count operand;
    let u = () in
    match operand with
    | Imm _ -> all_valid_reg_def count []; refine_ u
    | Reg reg ->
      all_valid_reg_def count [reg];
      all_valid_reg_def count [];
      refine_ u

  let (uses_valid @ total) :
    (count : int) -> (nodes : int) -> (instruction : instruction) ->
    {u : unit |
      not (valid_instruction count nodes instruction)
      || all_valid_reg count (uses instruction)}
    @ immutable contended =
    fun count nodes instruction ->
    valid_instruction_def count nodes instruction;
    uses_def instruction;
    let u = () in
    match instruction with
    | Move (_, operand, _) | Branch (operand, _, _) | Return operand ->
      operand_uses_valid count operand;
      refine_ u
    | Binary (_, _, left, right, _) ->
      operand_uses_valid count left;
      operand_uses_valid count right;
      union_valid count (operand_uses left) (operand_uses right);
      refine_ u
    | Jump _ -> all_valid_reg_def count []; refine_ u

  let rec (live_out_valid @ total) :
    (count : int) -> (live : int list list) -> (successors : int list) ->
    {u : unit |
      not (all_valid_live count live)
      || all_valid_reg count (live_out live successors)}
    @ immutable contended =
    fun count live successors ->
    live_out_def live successors;
    let u = () in
    match successors with
    | [] -> all_valid_reg_def count []; refine_ u
    | successor :: rest ->
      live_out_valid count live rest;
      (match nth live successor with
       | None -> all_valid_reg_def count []
       | Some row -> all_valid_live_lookup count live successor row);
      union_valid count
        (match nth live successor with None -> [] | Some xs -> xs)
        (live_out live rest);
      refine_ u

  let (transfer_valid @ total) :
    (count : int) -> (nodes : int) ->
    (live : int list list) -> (instruction : instruction) ->
    {u : unit |
      not (all_valid_live count live
           && valid_instruction count nodes instruction)
      || all_valid_reg count (transfer live instruction)}
    @ immutable contended =
    fun count nodes live instruction ->
    transfer_def live instruction;
    uses_valid count nodes instruction;
    live_out_valid count live (successors instruction);
    survivors_def live instruction;
    let u = () in
    (match definition instruction with
     | None -> ()
     | Some dst -> remove_valid count dst (live_out live (successors instruction)));
    union_valid count (uses instruction) (survivors live instruction);
    refine_ u

  let rec (empty_live_valid @ total) :
    (count : int) -> (code : instruction list) ->
    {u : unit | all_valid_live count (empty_live code)}
    @ immutable contended =
    fun count code ->
    empty_live_def code;
    all_valid_live_def count (empty_live code);
    let u = () in
    match code with
    | [] -> refine_ u
    | _ :: rest ->
      empty_live_valid count rest;
      all_valid_reg_def count [];
      refine_ u

  let rec (sweep_valid @ total) :
    (count : int) -> (nodes : int) ->
    (code : instruction list) -> (live : int list list) -> (index : int) ->
    {u : unit |
      not (all_valid_instructions count nodes code
           && all_valid_live count live)
      || (match sweep code live index with
          | next, _ -> all_valid_live count next)}
    @ immutable contended =
    fun count nodes code live index ->
    sweep_def code live index;
    all_valid_instructions_def count nodes code;
    let u = () in
    match code with
    | [] -> all_valid_live_def count []; refine_ u
    | instruction :: rest ->
      sweep_valid count nodes rest live (index + 1);
      transfer_valid count nodes live instruction;
      (match nth live index with
       | None -> all_valid_reg_def count []
       | Some old -> all_valid_live_lookup count live index old);
      let old = match nth live index with None -> [] | Some xs -> xs in
      union_valid count old (transfer live instruction);
      (match sweep rest live (index + 1) with
       | tail, _ ->
         all_valid_live_def count (union old (transfer live instruction) :: tail));
      refine_ u

  let rec (stabilize_valid @ total) :
    (fuel : int) -> (count : int) -> (nodes : int) ->
    (code : instruction list) -> (live : int list list) ->
    {u : unit |
      not (all_valid_instructions count nodes code
           && all_valid_live count live)
      || (match stabilize fuel code live with
          | None -> true
          | Some result -> all_valid_live count result)}
    @ immutable contended =
    fun fuel count nodes code live ->
    stabilize_def fuel code live;
    let u = () in
    if fuel <= 0 then refine_ u
    else begin
      sweep_valid count nodes code live 0;
      (match sweep code live 0 with
       | next, changed ->
         if changed then stabilize_valid (fuel - 1) count nodes code next);
      refine_ u
    end
  [@@decreases fuel]

  let (protected_before @ total) :
    (code : instruction list) -> (live : int list list) ->
    (pc : int) -> (instruction : instruction) ->
    (before : int list) -> (successor : int) ->
    (row : int list) -> (reg : int) ->
    {u : unit |
      not (closed_from code live 0
           && nth code pc === Some instruction
           && nth live pc === Some before
           && member successor (successors instruction)
           && nth live successor === Some row
           && member reg row)
      || definition instruction === Some reg
      || member reg before}
    @ immutable contended =
    fun code live pc instruction before successor row reg ->
    closed_lookup code live 0 pc instruction;
    successor_live live instruction before successor row reg;
    refine_ ()

  let (protected_color @ total) :
    (code : instruction list) -> (live : int list list) ->
    (colors : int list) -> (pc : int) ->
    (instruction : instruction) -> (successor : int) ->
    (row : int list) -> (dst : int) -> (reg : int) ->
    (physical : int) -> (color : int) ->
    {u : unit |
      not (nth code pc === Some instruction
           && definition instruction === Some dst
           && member successor (successors instruction)
           && nth live successor === Some row
           && member reg row
           && dst <> reg && 0 <= dst && 0 <= reg
           && proper_from (graph code live) 0 colors
           && nth colors dst === Some physical
           && nth colors reg === Some color)
      || physical <> color}
    @ immutable contended =
    fun code live colors pc instruction successor row dst reg physical color ->
    live_out_contains live (successors instruction) successor reg row;
    graph_write_covers code live pc dst reg;
    proper_distinct (graph code live) colors dst reg physical color;
    refine_ ()

  let rec (safe_write_from @ total) :
    (code : instruction list) -> (live : int list list) ->
    (colors : int list) -> (registers : int) ->
    (pc : int) -> (instruction : instruction) ->
    (before : int list) -> (successor : int) ->
    (row : int list) -> (next : int list) ->
    (dst : int) -> (physical : int) ->
    {u : unit |
      not (closed_from code live 0
           && nth code pc === Some instruction
           && nth live pc === Some before
           && member successor (successors instruction)
           && nth live successor === Some row
           && subset next row
           && all_valid_reg registers next
           && definition instruction === Some dst
           && valid_reg registers dst
           && length colors = registers
           && proper_from (graph code live) 0 colors
           && nth colors dst === Some physical)
      || safe_write next before colors dst physical}
    @ immutable contended =
    fun code live colors registers pc instruction before successor row next dst physical ->
    safe_write_def next before colors dst physical;
    subset_def next row;
    all_valid_reg_def registers next;
    let u = () in
    match next with
    | [] -> refine_ u
    | reg :: rest ->
      safe_write_from code live colors registers pc instruction before
        successor row rest dst physical;
      if reg = dst then refine_ u
      else begin
        member_def reg next;
        subset_member next row reg;
        protected_before code live pc instruction before successor row reg;
        valid_reg_def registers reg;
        valid_reg_def registers dst;
        nth_present colors reg;
        (match nth colors reg with
         | None -> refine_ u
         | Some color ->
           protected_color code live colors pc instruction successor row
             dst reg physical color;
           refine_ u)
      end

  let (successor_row_present @ total) :
    (registers : int) -> (nodes : int) ->
    (instruction : instruction) -> (successor : int) ->
    (live : int list list) ->
    {u : unit |
      not (valid_instruction registers nodes instruction
           && member successor (successors instruction)
           && length live = nodes)
      || (match nth live successor with None -> false | Some _ -> true)}
    @ immutable contended =
    fun registers nodes instruction successor live ->
    valid_successor registers nodes instruction successor;
    valid_reg_def nodes successor;
    nth_live_present live successor;
    refine_ ()

  let rec (agree_no_write @ total) :
    (code : instruction list) -> (live : int list list) ->
    (colors : int list) -> (pc : int) ->
    (instruction : instruction) -> (before : int list) ->
    (successor : int) -> (row : int list) -> (next : int list) ->
    (source : int list) -> (target : int list) ->
    {u : unit |
      not (closed_from code live 0
           && nth code pc === Some instruction
           && nth live pc === Some before
           && definition instruction === None
           && member successor (successors instruction)
           && nth live successor === Some row
           && subset next row
           && agree_on before colors source target)
      || agree_on next colors source target}
    @ immutable contended =
    fun code live colors pc instruction before successor row next source target ->
    agree_on_def next colors source target;
    subset_def next row;
    let u = () in
    match next with
    | [] -> refine_ u
    | reg :: rest ->
      agree_no_write code live colors pc instruction before successor row rest
        source target;
      member_def reg next;
      subset_member next row reg;
      protected_before code live pc instruction before successor row reg;
      agree_lookup before colors source target reg;
      refine_ u

  let (entry_pair_distinct @ total) :
    (code : instruction list) -> (live : int list list) ->
    (whole : int list) -> (colors : int list) ->
    (a : int) -> (b : int) -> (color_a : int) -> (color_b : int) ->
    {u : unit |
      not (nth live 0 === Some whole
           && member a whole && member b whole
           && 0 <= a && 0 <= b && a <> b
           && proper_from (graph code live) 0 colors
           && nth colors a === Some color_a
           && nth colors b === Some color_b)
      || color_a <> color_b}
    @ immutable contended =
    fun code live whole colors a b color_a color_b ->
    nth_def live 0;
    let u = () in
    match live with
    | [] -> refine_ u
    | entry :: _ ->
      graph_entry_covers code live a b;
      proper_distinct (graph code live) colors a b color_a color_b;
      refine_ u

  let rec (entry_separate @ total) :
    (code : instruction list) -> (live : int list list) ->
    (whole : int list) -> (colors : int list) ->
    (registers : int) -> (head : int) ->
    (rest : int list) -> (physical : int) ->
    {u : unit |
      not (nth live 0 === Some whole
           && member head whole
           && subset rest whole
           && all_valid_reg registers rest
           && valid_reg registers head
           && length colors = registers
           && proper_from (graph code live) 0 colors
           && nth colors head === Some physical)
      || separate_from rest colors head physical}
    @ immutable contended =
    fun code live whole colors registers head rest physical ->
    separate_from_def rest colors head physical;
    subset_def rest whole;
    all_valid_reg_def registers rest;
    let u = () in
    match rest with
    | [] -> refine_ u
    | reg :: tail ->
      entry_separate code live whole colors registers head tail physical;
      if reg = head then refine_ u
      else begin
        member_def reg rest;
        subset_member rest whole reg;
        valid_reg_def registers reg;
        valid_reg_def registers head;
        nth_present colors reg;
        (match nth colors reg with
         | None -> refine_ u
         | Some chosen ->
           entry_pair_distinct code live whole colors head reg physical chosen;
           refine_ u)
      end

  type allocation = {
    code : instruction list;
    physical : int;
    source_registers : int;
    source_inputs : int list;
    input_slots : (int * int) list;
  }

  let[@def] rec (build_slots @ total) live coloring = match live with
    | [] -> Some []
    | reg :: rest ->
      (match nth coloring reg, build_slots rest coloring with
       | Some physical, Some slots -> Some ((reg, physical) :: slots)
       | _ -> None)

  let[@def] (allocate @ total) (program : program) physical =
    if not (valid program) || physical <= 0 || physical > 32 then None
    else
      let initial_live = empty_live program.code in
      ghost_ (stabilize_closed 2049 program.code initial_live);
      match stabilize 2049 program.code initial_live with
      | None -> None
      | Some live ->
        let entry = match live with [] -> [] | xs :: _ -> xs in
        if not (subset entry program.inputs) then None
        else
          let edges = graph program.code live in
          let choices = range physical in
          let vertices = zeros program.registers in
          ghost_ (color_sound edges choices 0 vertices);
          match color edges choices 0 vertices with
          | None -> None
          | Some coloring ->
            match rename coloring program.code with
            | None -> None
            | Some code ->
              (match build_slots entry coloring with
               | None -> None
               | Some input_slots ->
                 Some {code; physical; source_registers = program.registers;
                   source_inputs = program.inputs;
                   input_slots})

  let (allocate_sound @ total) :
    (program : program) -> (physical : int) ->
    {u : unit |
      match allocate program physical with
      | None -> true
      | Some {code = target_code; physical = out_physical;
              source_registers; source_inputs; input_slots} ->
        valid program
        && 0 < physical && physical <= 32
        && out_physical = physical
        && source_registers = program.registers
        && source_inputs === program.inputs
        && (match stabilize 2049 program.code (empty_live program.code) with
            | None -> false
            | Some live ->
              closed_from program.code live 0
              && length live = length program.code
              && all_valid_live program.registers live
              && (let entry = match live with [] -> [] | row :: _ -> row in
                  subset entry program.inputs
                  && (match color (graph program.code live) (range physical) 0
                              (zeros program.registers) with
                      | None -> false
                      | Some coloring ->
                        proper_from (graph program.code live) 0 coloring
                        && length coloring = program.registers
                        && all_valid_reg physical coloring
                        && rename coloring program.code === Some target_code
                        && build_slots entry coloring === Some input_slots)))}
    @ ghost =
    fun program physical -> ghost_ (
    allocate_def program physical;
    let u = () in
    if not (valid program) || physical <= 0 || physical > 32 then refine_ u
    else begin
      valid_def program;
      let initial_live = empty_live program.code in
      empty_live_length program.code;
      empty_live_valid program.registers program.code;
      stabilize_closed 2049 program.code initial_live;
      stabilize_length 2049 program.code initial_live;
      stabilize_valid 2049 program.registers (length program.code)
        program.code initial_live;
      (match stabilize 2049 program.code initial_live with
       | None -> refine_ u
       | Some live ->
         let entry = match live with [] -> [] | row :: _ -> row in
         if not (subset entry program.inputs) then refine_ u
         else begin
           let edges = graph program.code live in
           color_sound edges (range physical) 0 (zeros program.registers);
           color_total edges program.registers physical;
           (match color edges (range physical) 0 (zeros program.registers) with
            | None -> refine_ u
            | Some coloring ->
              (match rename coloring program.code with
               | None -> refine_ u
               | Some code ->
                 match build_slots entry coloring with
                 | None -> refine_ u
                 | Some slots -> refine_ u))
         end)
    end
    )

  let[@def] rec (same_shape @ total) xs ys = match xs, ys with
    | [], [] -> true
    | _ :: xs, _ :: ys -> same_shape xs ys
    | _ -> false

  let[@def] rec (load_inputs @ total) file registers values = match registers, values with
    | [], [] -> Some file
    | reg :: registers, value :: values ->
      (match write file reg value with
       | None -> None
       | Some file -> load_inputs file registers values)
    | _ -> None

  let rec (load_inputs_length @ total) :
    (file : int list) -> (registers : int list) -> (values : int list) ->
    {u : unit |
      match load_inputs file registers values with
      | None -> true
      | Some result -> length result = length file}
    @ immutable contended =
    fun file registers values ->
    load_inputs_def file registers values;
    let u = () in
    match registers, values with
    | reg :: rest, word :: words ->
      (match write file reg word with
       | None -> refine_ u
       | Some after ->
         write_length file reg word;
         load_inputs_length after rest words;
         refine_ u)
    | _ -> refine_ u

  let rec (load_inputs_present @ total) :
    (file : int list) -> (registers : int list) ->
    (values : int list) -> (count : int) ->
    {u : unit |
      not (length file = count
           && all_valid_reg count registers
           && same_shape registers values)
      || (match load_inputs file registers values with
          | None -> false
          | Some _ -> true)}
    @ immutable contended =
    fun file registers values count ->
    load_inputs_def file registers values;
    same_shape_def registers values;
    all_valid_reg_def count registers;
    let u = () in
    match registers, values with
    | [], [] -> refine_ u
    | reg :: rest, word :: words ->
      valid_reg_def count reg;
      write_present file reg word;
      (match write file reg word with
       | None -> refine_ u
       | Some after ->
         write_length file reg word;
         load_inputs_present after rest words count;
         refine_ u)
    | _ -> refine_ u

  let[@def] (source_initial @ total) (program : program) args =
    match load_inputs (zeros program.registers) program.inputs args with
    | None -> Stuck
    | Some file -> Running (0, file)

  let[@def] rec (load_slots @ total) file source slots = match slots with
    | [] -> Some file
    | (reg, physical) :: rest ->
      (match load_slots file source rest, nth source reg with
       | Some file, Some value -> write file physical value
       | _ -> None)

  let rec (load_slots_length @ total) :
    (file : int list) -> (source : int list) ->
    (slots : (int * int) list) ->
    {u : unit |
      match load_slots file source slots with
      | None -> true
      | Some result -> length result = length file}
    @ immutable contended =
    fun file source slots ->
    load_slots_def file source slots;
    let u = () in
    match slots with
    | [] -> refine_ u
    | (reg, physical) :: rest ->
      load_slots_length file source rest;
      (match load_slots file source rest, nth source reg with
       | Some middle, Some word -> write_length middle physical word
       | _ -> ());
      refine_ u

  let[@def] (target_initial @ total)
      physical source_registers source_inputs input_slots args =
    let loaded =
      if same_shape source_inputs args then
        match load_inputs (zeros source_registers) source_inputs args with
        | None -> None
        | Some source ->
          load_slots (zeros physical) source input_slots
      else None in
    match loaded with
    | None -> Stuck
    | Some file -> Running (0, file)

  let initial_of_allocation (allocation : allocation) args =
    target_initial allocation.physical allocation.source_registers
      allocation.source_inputs allocation.input_slots args

  let[@def] (related @ total) registers physical nodes live colors source target =
    match source, target with
    | Done left, Done right -> left = right
    | Running (pc, source_file), Running (target_pc, target_file) ->
      pc = target_pc
      && valid_reg nodes pc
      && length source_file = registers
      && length target_file = physical
      && (match nth live pc with
          | None -> false
          | Some row -> agree_on row colors source_file target_file)
    | _ -> false

  let[@def] (observable_equal @ total) source target =
    match source, target with
    | Done left, Done right -> left = right
    | Running (pc, _), Running (other_pc, _) -> pc = other_pc
    | _ -> false

  let (related_observable @ total) :
    (registers : int) -> (physical : int) -> (nodes : int) ->
    (live : int list list) -> (colors : int list) ->
    (source : state) -> (target : state) ->
    {u : unit |
      not (related registers physical nodes live colors source target)
      || observable_equal source target}
    @ immutable contended =
    fun registers physical nodes live colors source target ->
    related_def registers physical nodes live colors source target;
    observable_equal_def source target;
    let u = () in
    match source, target with
    | Done _, Done _ | Running _, Running _ -> refine_ u
    | _ -> refine_ u

  let (execute_related @ total) :
    (code : instruction list) -> (live : int list list) ->
    (colors : int list) -> (registers : int) ->
    (physical : int) -> (nodes : int) ->
    (pc : int) -> (instruction : instruction) ->
    (renamed : instruction) -> (source : int list) -> (target : int list) ->
    {u : unit |
      not (closed_from code live 0
           && length live = nodes
           && all_valid_live registers live
           && proper_from (graph code live) 0 colors
           && length colors = registers
           && all_valid_reg physical colors
           && valid_instruction registers nodes instruction
           && nth code pc === Some instruction
           && rename_instruction colors instruction === Some renamed
           && related registers physical nodes live colors
                (Running (pc, source)) (Running (pc, target)))
      || related registers physical nodes live colors
           (execute instruction source) (execute renamed target)}
    @ immutable contended =
    fun code live colors registers physical nodes pc instruction renamed source target ->
    related_def registers physical nodes live colors
      (Running (pc, source)) (Running (pc, target));
    rename_instruction_def colors instruction;
    execute_def instruction source;
    execute_def renamed target;
    let u = () in
    match nth live pc with
    | None -> refine_ u
    | Some before ->
      closed_lookup code live 0 pc instruction;
      (match instruction with
       | Jump next ->
         successors_def instruction;
         member_def next [next];
         definition_def instruction;
         successor_row_present registers nodes instruction next live;
         (match nth live next with
          | None -> refine_ u
          | Some row ->
            subset_reflexive row;
            agree_no_write code live colors pc instruction before next row row
              source target;
            valid_successor registers nodes instruction next;
            related_def registers physical nodes live colors
              (Running (next, source)) (Running (next, target));
            refine_ u)
       | Return operand ->
         uses_def instruction;
         valid_instruction_def registers nodes instruction;
         value_agrees before colors source target operand;
         value_present registers source operand;
         (match rename_operand colors operand with
          | None -> refine_ u
          | Some renamed_operand ->
            rename_instruction_def colors (Return operand);
            execute_def (Return operand) source;
            execute_def (Return renamed_operand) target;
            value_def source operand;
            value_def target renamed_operand;
            (match value source operand, value target renamed_operand with
             | Some left, Some right ->
               related_def registers physical nodes live colors
                 (Done left) (Done right);
               refine_ u
             | _ -> refine_ u))
       | Branch (condition, yes, no) ->
         uses_def instruction;
         definition_def instruction;
         valid_instruction_def registers nodes instruction;
         value_agrees before colors source target condition;
         value_present registers source condition;
         (match rename_operand colors condition with
          | None -> refine_ u
          | Some renamed_condition ->
            rename_instruction_def colors instruction;
            execute_def (Branch (condition, yes, no)) source;
            execute_def (Branch (renamed_condition, yes, no)) target;
            (match value source condition, value target renamed_condition with
             | Some word, Some target_word ->
               let next = if word = 0 then no else yes in
               successors_def instruction;
               member_def next [yes; no];
               member_def next [no];
               successor_row_present registers nodes instruction next live;
               (match nth live next with
                | None -> refine_ u
                | Some row ->
                  subset_reflexive row;
                  agree_no_write code live colors pc instruction before
                    next row row source target;
                  valid_successor registers nodes instruction next;
                  related_def registers physical nodes live colors
                    (Running (next, source)) (Running (next, target));
                  refine_ u)
             | _ -> refine_ u))
       | Move (dst, operand, next) ->
         uses_def instruction;
         definition_def instruction;
         valid_instruction_def registers nodes instruction;
         value_agrees before colors source target operand;
         value_present registers source operand;
         (match nth colors dst, rename_operand colors operand with
          | Some chosen, Some renamed_operand ->
            rename_instruction_def colors instruction;
            execute_def (Move (dst, operand, next)) source;
            execute_def (Move (chosen, renamed_operand, next)) target;
            all_valid_reg_lookup physical colors dst chosen;
            valid_reg_def registers dst;
            valid_reg_def physical chosen;
            successors_def instruction;
            member_def next [next];
            successor_row_present registers nodes instruction next live;
            (match value source operand, value target renamed_operand,
                   nth live next with
             | Some word, Some target_word, Some row ->
               all_valid_live_lookup registers live next row;
               subset_reflexive row;
               safe_write_from code live colors registers pc instruction before
                 next row row dst chosen;
               write_present source dst word;
               write_present target chosen word;
               (match write source dst word, write target chosen word with
                | Some source_after, Some target_after ->
                  write_agreement row before colors source target dst chosen word;
                  write_length source dst word;
                  write_length target chosen word;
                  valid_successor registers nodes instruction next;
                  related_def registers physical nodes live colors
                    (Running (next, source_after))
                    (Running (next, target_after));
                  refine_ u
                | _ -> refine_ u)
             | _ -> refine_ u)
          | _ -> refine_ u)
       | Binary (dst, operation, left, right, next) ->
         uses_def instruction;
         definition_def instruction;
         valid_instruction_def registers nodes instruction;
         binary_operands_live before dst operation left right next;
         value_agrees_live before colors source target left;
         value_agrees_live before colors source target right;
         value_present registers source left;
         value_present registers source right;
         (match nth colors dst, rename_operand colors left,
                rename_operand colors right with
          | Some chosen, Some renamed_left, Some renamed_right ->
            rename_instruction_def colors instruction;
            execute_def (Binary (dst, operation, left, right, next)) source;
            execute_def
              (Binary (chosen, operation, renamed_left, renamed_right, next))
              target;
            all_valid_reg_lookup physical colors dst chosen;
            valid_reg_def registers dst;
            valid_reg_def physical chosen;
            successors_def instruction;
            member_def next [next];
            successor_row_present registers nodes instruction next live;
            (match value source left, value source right,
                   value target renamed_left, value target renamed_right,
                   nth live next with
             | Some left_word, Some right_word,
               Some target_left, Some target_right, Some row ->
               all_valid_live_lookup registers live next row;
               subset_reflexive row;
               safe_write_from code live colors registers pc instruction before
                 next row row dst chosen;
               let word = apply operation left_word right_word in
               write_present source dst word;
               write_present target chosen word;
               (match write source dst word, write target chosen word with
                | Some source_after, Some target_after ->
                  write_agreement row before colors source target dst chosen word;
                  write_length source dst word;
                  write_length target chosen word;
                  valid_successor registers nodes instruction next;
                  related_def registers physical nodes live colors
                    (Running (next, source_after))
                    (Running (next, target_after));
                  refine_ u
                | _ -> refine_ u)
             | _ -> refine_ u)
          | _ -> refine_ u))

  let (step_related @ total) :
    (code : instruction list) -> (live : int list list) ->
    (colors : int list) -> (registers : int) ->
    (physical : int) -> (target_code : instruction list) ->
    (source : state) -> (target : state) ->
    {u : unit |
      not (closed_from code live 0
           && length live = length code
           && all_valid_live registers live
           && proper_from (graph code live) 0 colors
           && length colors = registers
           && all_valid_reg physical colors
           && all_valid_instructions registers (length code) code
           && rename colors code === Some target_code
           && related registers physical (length code) live colors source target)
      || related registers physical (length code) live colors
           (step code source) (step target_code target)}
    @ immutable contended =
    fun code live colors registers physical target_code source target ->
    related_def registers physical (length code) live colors source target;
    step_def code source;
    step_def target_code target;
    let u = () in
    match source, target with
    | Done left, Done right ->
      related_def registers physical (length code) live colors
        (Done left) (Done right);
      refine_ u
    | Running (pc, source_file), Running (target_pc, target_file) ->
      valid_reg_def (length code) pc;
      nth_code_present code pc;
      rename_length colors code;
      nth_code_present target_code pc;
      (match nth code pc with
       | None -> refine_ u
       | Some instruction ->
         valid_instruction_lookup registers (length code) code pc instruction;
         rename_nth colors code pc instruction;
         (match rename_instruction colors instruction with
          | None -> refine_ u
          | Some renamed ->
            execute_related code live colors registers physical (length code)
              pc instruction renamed source_file target_file;
            refine_ u))
    | _ -> refine_ u

  let rec (advance_related @ total) :
    (fuel : fuel) -> (code : instruction list) ->
    (live : int list list) -> (colors : int list) ->
    (registers : int) -> (physical : int) ->
    (target_code : instruction list) ->
    (source : state) -> (target : state) ->
    {u : unit |
      not (closed_from code live 0
           && length live = length code
           && all_valid_live registers live
           && proper_from (graph code live) 0 colors
           && length colors = registers
           && all_valid_reg physical colors
           && all_valid_instructions registers (length code) code
           && rename colors code === Some target_code
           && related registers physical (length code) live colors source target)
      || related registers physical (length code) live colors
           (advance code fuel source) (advance target_code fuel target)}
    @ immutable contended =
    fun fuel code live colors registers physical target_code source target ->
    advance_def code fuel source;
    advance_def target_code fuel target;
    let u = () in
    match fuel with
    | Z -> refine_ u
    | S rest ->
      step_related code live colors registers physical target_code source target;
      advance_related rest code live colors registers physical target_code
        (step code source) (step target_code target);
      refine_ u

  let rec (copy_agreement @ total) :
    (code : instruction list) -> (live : int list list) ->
    (whole : int list) -> (entry : int list) ->
    (colors : int list) -> (registers : int) -> (physical : int) ->
    (source : int list) -> (target : int list) ->
    {u : unit |
      not (nth live 0 === Some whole
           && subset entry whole
           && all_valid_reg registers entry
           && length colors = registers
           && all_valid_reg physical colors
           && proper_from (graph code live) 0 colors
           && length source = registers
           && length target = physical)
      || (match build_slots entry colors with
          | None -> true
          | Some slots ->
            match load_slots target source slots with
            | None -> true
            | Some after -> agree_on entry colors source after)}
    @ immutable contended =
    fun code live whole entry colors registers physical source target ->
    build_slots_def entry colors;
    subset_def entry whole;
    all_valid_reg_def registers entry;
    let u = () in
    match entry with
    | [] ->
      load_slots_def target source [];
      agree_on_def [] colors source target;
      refine_ u
    | head :: rest ->
      copy_agreement code live whole rest colors registers physical source target;
      member_def head entry;
      subset_member entry whole head;
      valid_reg_def registers head;
      nth_present source head;
      nth_present colors head;
      (match nth colors head, build_slots rest colors with
       | Some chosen, Some slots ->
         all_valid_reg_lookup physical colors head chosen;
         valid_reg_def physical chosen;
         entry_separate code live whole colors registers head rest chosen;
         load_slots_def target source ((head, chosen) :: slots);
         load_slots_length target source slots;
         (match load_slots target source slots, nth source head with
          | Some middle, Some word ->
            target_write_keeps_agreement rest colors source middle
              head chosen word;
            write_present middle chosen word;
            (match write middle chosen word with
             | None -> refine_ u
             | Some after ->
               write_reads middle chosen word;
               agree_on_def entry colors source after;
               refine_ u)
          | _ -> refine_ u)
       | _ -> refine_ u)

  let rec (copy_present @ total) :
    (entry : int list) -> (colors : int list) ->
    (registers : int) -> (physical : int) ->
    (source : int list) -> (target : int list) ->
    {u : unit |
      not (all_valid_reg registers entry
           && length colors = registers
           && all_valid_reg physical colors
           && length source = registers
           && length target = physical)
      || (match build_slots entry colors with
          | None -> false
          | Some slots ->
            match load_slots target source slots with
            | None -> false
            | Some _ -> true)}
    @ immutable contended =
    fun entry colors registers physical source target ->
    build_slots_def entry colors;
    all_valid_reg_def registers entry;
    let u = () in
    match entry with
    | [] -> load_slots_def target source []; refine_ u
    | head :: rest ->
      copy_present rest colors registers physical source target;
      valid_reg_def registers head;
      nth_present colors head;
      nth_present source head;
      (match nth colors head, build_slots rest colors with
       | Some chosen, Some slots ->
         all_valid_reg_lookup physical colors head chosen;
         valid_reg_def physical chosen;
         load_slots_length target source slots;
         (match load_slots target source slots, nth source head with
          | Some middle, Some word ->
            write_present middle chosen word;
            load_slots_def target source ((head, chosen) :: slots);
            refine_ u
          | _ -> refine_ u)
       | _ -> refine_ u)

  let (initial_related @ total) :
    (program : program) -> (physical : int) ->
    (live : int list list) -> (colors : int list) ->
    (entry : int list) -> (slots : (int * int) list) ->
    (args : int list) ->
    {u : unit |
      not (valid program
           && 0 < physical
           && nth live 0 === Some entry
           && all_valid_live program.registers live
           && length colors = program.registers
           && all_valid_reg physical colors
           && proper_from (graph program.code live) 0 colors
           && build_slots entry colors === Some slots
           && same_shape program.inputs args)
      || related program.registers physical (length program.code) live colors
           (source_initial program args)
           (target_initial physical program.registers program.inputs slots args)}
    @ immutable contended =
    fun program physical live colors entry slots args ->
    valid_def program;
    source_initial_def program args;
    target_initial_def physical program.registers program.inputs slots args;
    zeros_length program.registers;
    zeros_length physical;
    all_valid_live_lookup program.registers live 0 entry;
    subset_reflexive entry;
    load_inputs_present (zeros program.registers) program.inputs args
      program.registers;
    let u = () in
    match load_inputs (zeros program.registers) program.inputs args with
    | None -> refine_ u
    | Some source ->
      load_inputs_length (zeros program.registers) program.inputs args;
      copy_present entry colors program.registers physical source
        (zeros physical);
      copy_agreement program.code live entry entry colors program.registers
        physical source (zeros physical);
      (match load_slots (zeros physical) source slots with
       | None -> refine_ u
       | Some target ->
         load_slots_length (zeros physical) source slots;
         valid_reg_def (length program.code) 0;
         related_def program.registers physical (length program.code)
           live colors (Running (0, source)) (Running (0, target));
         refine_ u)

  let (preserves @ total) :
    (program : program) -> (physical : int) ->
    (args : int list) -> (fuel : fuel) ->
    {u : unit |
      match allocate program physical with
      | None -> true
      | Some {code = target_code; physical = out_physical;
              source_registers; source_inputs; input_slots} ->
        not (same_shape program.inputs args)
        || observable_equal
             (advance program.code fuel (source_initial program args))
             (advance target_code fuel
                (target_initial out_physical source_registers
                   source_inputs input_slots args))}
    @ ghost =
    fun program physical args fuel -> ghost_ (
    allocate_sound program physical;
    let u = () in
    match allocate program physical with
    | None -> refine_ u
    | Some {code = target_code; physical = out_physical;
            source_registers; source_inputs; input_slots} ->
      if not (same_shape program.inputs args) then refine_ u
      else begin
        valid_def program;
        (match stabilize 2049 program.code (empty_live program.code) with
         | None -> refine_ u
         | Some live ->
           let entry = match live with [] -> [] | row :: _ -> row in
           (match color (graph program.code live) (range physical) 0
                    (zeros program.registers) with
            | None -> refine_ u
            | Some colors ->
              length_def live;
              nth_def live 0;
              (match live with
               | [] -> refine_ u
               | _ :: _ ->
                 initial_related program physical live colors entry input_slots args;
                 advance_related fuel program.code live colors program.registers
                   physical target_code (source_initial program args)
                   (target_initial out_physical source_registers
                      source_inputs input_slots args);
                 related_observable program.registers physical
                   (length program.code) live colors
                   (advance program.code fuel (source_initial program args))
                   (advance target_code fuel
                      (target_initial out_physical source_registers
                         source_inputs input_slots args));
                 refine_ u)))
      end
    )
end
;;
[%%expect{|
module Register_allocation :
  sig
    type operand = Reg of int | Imm of int
    type operation = Add | Subtract | Equal | Less_than
    type instruction =
        Move of int * operand * int
      | Binary of int * operation * operand * operand * int
      | Jump of int
      | Branch of operand * int * int
      | Return of operand
    type program = {
      code : instruction list;
      registers : int;
      inputs : int list;
    }
    type state = Running of int * int list | Done of int | Stuck
    val nth : 'a list -> int -> 'a option
    val nth_def :
      (xs : 'a list) ->
      (n : int) ->
      {u : unit
        | (nth xs n) ===
            (match xs with
             | [] -> None
             | x::rest -> if n = 0 then Some x else nth rest (n - 1))}
    val write : 'a list -> int -> 'a -> 'a list option
    val write_def :
      (xs : 'a list) ->
      (n : int) ->
      (value : 'a) ->
      {u : unit
        | (write xs n value) ===
            (match xs with
             | [] -> None
             | x::rest' ->
                 if n = 0
                 then Some (value :: rest')
                 else
                   (match write rest' (n - 1) value with
                    | None -> None
                    | Some rest -> Some (x :: rest)))}
    val write_preserves :
      (xs : int list) ->
      ((n : int) ->
       (value : int) ->
       (m : int) ->
       {u : unit
         | (n = m) ||
             (match write xs n value with
              | None -> true
              | Some ys -> (nth ys m) === (nth xs m))} @ immutable) @ total
      stateful
    val write_reads :
      (xs : int list) ->
      ((n : int) ->
       (value : int) ->
       {u : unit
         | match write xs n value with
           | None -> true
           | Some ys -> (nth ys n) === (Some value)} @ immutable) @ total
      stateful
    val zeros : int -> int list
    val zeros_def :
      (n : int) ->
      {u : unit
        | (zeros n) === (if n <= 0 then [] else 0 :: (zeros (n - 1)))}
    val value : int list -> operand -> int option
    val value_def :
      (file : int list) ->
      (operand : operand) ->
      {u : unit
        | (value file operand) ===
            (match operand with | Reg r -> nth file r | Imm n -> Some n)}
    val apply : operation -> int -> int -> int
    val apply_def :
      (operation : operation) ->
      (left : int) ->
      (right : int) ->
      {u : unit
        | (apply operation left right) ===
            (match operation with
             | Add -> left + right
             | Subtract -> left - right
             | Equal -> if left = right then 1 else 0
             | Less_than -> if left < right then 1 else 0)}
    val execute : instruction -> int list -> state
    val execute_def :
      (instruction : instruction) ->
      (file' : int list) ->
      {u : unit
        | (execute instruction file') ===
            (match instruction with
             | Move (dst', operand', next') ->
                 (match value file' operand' with
                  | None -> Stuck
                  | Some word' ->
                      (match write file' dst' word' with
                       | None -> Stuck
                       | Some file'' -> Running (next', file'')))
             | Binary (dst, operation, left', right', next'') ->
                 (match ((value file' left'), (value file' right')) with
                  | (Some left, Some right) ->
                      (match write file' dst (apply operation left right)
                       with
                       | None -> Stuck
                       | Some file -> Running (next'', file))
                  | _ -> Stuck)
             | Jump next -> Running (next, file')
             | Branch (condition, yes, no) ->
                 (match value file' condition with
                  | None -> Stuck
                  | Some word'' ->
                      Running ((if word'' = 0 then no else yes), file'))
             | Return operand ->
                 (match value file' operand with
                  | None -> Stuck
                  | Some word -> Done word))}
    val step : instruction list -> state -> state
    val step_def :
      (code : instruction list) ->
      (state : state) ->
      {u : unit
        | (step code state) ===
            (match state with
             | Done _ | Stuck -> state
             | Running (pc, file) ->
                 (match nth code pc with
                  | None -> Stuck
                  | Some instruction -> execute instruction file))}
    type fuel = Z | S of fuel
    [@@inductive]
    val advance : instruction list -> fuel -> state -> state
    val advance_def :
      (code : instruction list) ->
      (fuel : fuel) ->
      (state : state) ->
      {u : unit
        | (advance code fuel state) ===
            (match fuel with
             | Z -> state
             | S rest -> advance code rest (step code state))}
    val member : int -> int list -> bool
    val member_def :
      (value : int) ->
      (xs : int list) ->
      {u : unit
        | (member value xs) ===
            (match xs with
             | [] -> false
             | x::rest -> (x = value) || (member value rest))}
    val add : int -> int list -> int list
    val add_def :
      (value : int) ->
      (xs : int list) ->
      {u : unit
        | (add value xs) === (if member value xs then xs else value :: xs)}
    val union : int list -> int list -> int list
    val union_def :
      (xs : int list) ->
      (ys : int list) ->
      {u : unit
        | (union xs ys) ===
            (match xs with | [] -> ys | x::rest -> add x (union rest ys))}
    val remove : int -> int list -> int list
    val remove_def :
      (value : int) ->
      (xs : int list) ->
      {u : unit
        | (remove value xs) ===
            (match xs with
             | [] -> []
             | x::rest ->
                 if x = value
                 then remove value rest
                 else x :: (remove value rest))}
    val subset : int list -> int list -> bool
    val subset_def :
      (xs : int list) ->
      (ys : int list) ->
      {u : unit
        | (subset xs ys) ===
            (match xs with
             | [] -> true
             | x::rest -> (member x ys) && (subset rest ys))}
    val operand_uses : operand -> int list
    val operand_uses_def :
      (operand : operand) ->
      {u : unit
        | (operand_uses operand) ===
            (match operand with | Reg r -> [r] | Imm _ -> [])}
    val uses : instruction -> int list
    val uses_def :
      (instruction : instruction) ->
      {u : unit
        | (uses instruction) ===
            (match instruction with
             | Move (_, operand, _) | Branch (operand, _, _) | Return operand
                 -> operand_uses operand
             | Binary (_, _, left, right, _) ->
                 union (operand_uses left) (operand_uses right)
             | Jump _ -> [])}
    val definition : instruction -> int option
    val definition_def :
      (instruction : instruction) ->
      {u : unit
        | (definition instruction) ===
            (match instruction with
             | Move (dst, _, _) | Binary (dst, _, _, _, _) -> Some dst
             | Jump _ | Branch (_, _, _) | Return _ -> None)}
    val successors : instruction -> int list
    val successors_def :
      (instruction : instruction) ->
      {u : unit
        | (successors instruction) ===
            (match instruction with
             | Move (_, _, next) | Binary (_, _, _, _, next) | Jump next ->
                 [next]
             | Branch (_, yes, no) -> [yes; no]
             | Return _ -> [])}
    val live_out : int list list -> int list -> int list
    val live_out_def :
      (live : int list list) ->
      (successors : int list) ->
      {u : unit
        | (live_out live successors) ===
            (match successors with
             | [] -> []
             | successor::rest ->
                 let at_successor =
                   match nth live successor with | None -> [] | Some xs -> xs in
                 union at_successor (live_out live rest))}
    val survivors : int list list -> instruction -> int list
    val survivors_def :
      (live : int list list) ->
      (instruction : instruction) ->
      {u : unit
        | (survivors live instruction) ===
            (let out = live_out live (successors instruction) in
             match definition instruction with
             | None -> out
             | Some dst -> remove dst out)}
    val subset_member :
      (xs : int list) ->
      ((ys : int list) ->
       (reg : int) ->
       {u : unit
         | (not ((subset xs ys) && (member reg xs))) || (member reg ys)} @ immutable) @ total
      stateful
    val subset_cons :
      (xs : int list) ->
      ((ys : int list) ->
       (head : int) ->
       {u : unit | (not (subset xs ys)) || (subset xs (head :: ys))} @ immutable) @ total
      stateful
    val subset_reflexive :
      (xs : int list) -> {u : unit | subset xs xs} @ immutable
    val remove_keeps :
      (removed : int) ->
      ((xs : int list) ->
       (reg : int) ->
       {u : unit
         | (removed = reg) ||
             ((not (member reg xs)) || (member reg (remove removed xs)))} @ immutable) @ total
      stateful
    val union_keeps_right :
      (xs : int list) ->
      ((ys : int list) ->
       (reg : int) ->
       {u : unit | (not (member reg ys)) || (member reg (union xs ys))} @ immutable) @ total
      stateful
    val add_keeps :
      (value : int) ->
      ((xs : int list) ->
       (reg : int) ->
       {u : unit | (not (member reg xs)) || (member reg (add value xs))} @ immutable) @ total
      stateful
    val union_keeps_left :
      (xs : int list) ->
      ((ys : int list) ->
       (reg : int) ->
       {u : unit | (not (member reg xs)) || (member reg (union xs ys))} @ immutable) @ total
      stateful
    val live_out_contains :
      (live : int list list) ->
      ((successors : int list) ->
       (successor : int) ->
       (reg : int) ->
       (row : int list) ->
       {u : unit
         | (not
              ((member successor successors) &&
                 (((nth live successor) === (Some row)) && (member reg row))))
             || (member reg (live_out live successors))} @ immutable) @ total
      stateful
    val successor_live :
      (live : int list list) ->
      ((instruction : instruction) ->
       (current : int list) ->
       (successor : int) ->
       (row : int list) ->
       (reg : int) ->
       {u : unit
         | (not
              ((subset (survivors live instruction) current) &&
                 ((member successor (successors instruction)) &&
                    (((nth live successor) === (Some row)) &&
                       (member reg row)))))
             ||
             (((definition instruction) === (Some reg)) ||
                (member reg current))} @ immutable) @ total
      stateful
    val transfer : int list list -> instruction -> int list
    val transfer_def :
      (live : int list list) ->
      (instruction : instruction) ->
      {u : unit
        | (transfer live instruction) ===
            (union (uses instruction) (survivors live instruction))}
    val sweep :
      instruction list -> int list list -> int -> int list list * bool
    val sweep_def :
      (code : instruction list) ->
      (live : int list list) ->
      (index : int) ->
      {u : unit
        | (sweep code live index) ===
            (match code with
             | [] -> ([], false)
             | instruction::rest ->
                 let old =
                   match nth live index with | None -> [] | Some xs -> xs in
                 let needed = transfer live instruction in
                 let next = union old needed in
                 (match sweep rest live (index + 1) with
                  | (tail, changed) ->
                      ((next :: tail),
                        (changed ||
                           (not
                              ((subset (uses instruction) old) &&
                                 (subset (survivors live instruction) old)))))))}
    val closed_from : instruction list -> int list list -> int -> bool
    val closed_from_def :
      (code : instruction list) ->
      (live : int list list) ->
      (index : int) ->
      {u : unit
        | (closed_from code live index) ===
            (match code with
             | [] -> true
             | instruction::rest ->
                 let old =
                   match nth live index with | None -> [] | Some xs -> xs in
                 (subset (uses instruction) old) &&
                   ((subset (survivors live instruction) old) &&
                      (closed_from rest live (index + 1))))}
    val closed_lookup :
      (code : instruction list) ->
      ((live : int list list) ->
       (index : int) ->
       (pc : int) ->
       (instruction : instruction) ->
       {u : unit
         | (not (closed_from code live index)) ||
             ((not ((nth code pc) === (Some instruction))) ||
                (let at_pc =
                   match nth live (index + pc) with
                   | None -> []
                   | Some xs -> xs in
                 (subset (uses instruction) at_pc) &&
                   (subset (survivors live instruction) at_pc)))} @ immutable) @ total
      stateful
    val sweep_closed :
      (code : instruction list) ->
      ((live : int list list) ->
       (index : int) ->
       {u : unit
         | match sweep code live index with
           | (_, true) -> true
           | (_, false) -> closed_from code live index} @ immutable) @ total
      stateful
    val empty_live : instruction list -> int list list
    val empty_live_def :
      (code : instruction list) ->
      {u : unit
        | (empty_live code) ===
            (match code with | [] -> [] | _::rest -> [] :: (empty_live rest) :
            int list list)}
    val stabilize :
      int -> instruction list -> int list list -> int list list option
    val stabilize_def :
      (fuel : int) ->
      (code : instruction list) ->
      (live : int list list) ->
      {u : unit
        | (stabilize fuel code live) ===
            (if fuel <= 0
             then None
             else
               (match sweep code live 0 with
                | (next, changed) ->
                    if changed
                    then stabilize (fuel - 1) code next
                    else Some live))}
    val stabilize_closed :
      (fuel : int) ->
      ((code : instruction list) ->
       (live : int list list) ->
       {u : unit
         | match stabilize fuel code live with
           | None -> true
           | Some result -> closed_from code result 0} @ immutable) @ total
      stateful
    type edge = int * int
    val member_edge : edge -> (int * int) list -> bool
    val member_edge_def :
      (pair : edge) ->
      (edges : (int * int) list) ->
      {u : unit
        | (member_edge pair edges) ===
            (match edges with
             | [] -> false
             | (left, right)::rest ->
                 (match pair with
                  | (wanted_left, wanted_right) ->
                      ((left = wanted_left) && (right = wanted_right)) ||
                        (member_edge pair rest)))}
    val edge : int -> int -> int * int
    val edge_def :
      (a : int) ->
      (b : int) ->
      {u : unit | (edge a b) === (if a < b then (a, b) else (b, a))}
    val add_edge : int -> int -> (int * int) list -> (int * int) list
    val add_edge_def :
      (a : int) ->
      (b : int) ->
      (graph : (int * int) list) ->
      {u : unit
        | (add_edge a b graph) ===
            (if a = b
             then graph
             else
               (let pair = edge a b in
                if member_edge pair graph then graph else pair :: graph))}
    val connect_one : int -> int list -> (int * int) list -> (int * int) list
    val connect_one_def :
      (vertex : int) ->
      (vertices : int list) ->
      (graph : (int * int) list) ->
      {u : unit
        | (connect_one vertex vertices graph) ===
            (match vertices with
             | [] -> graph
             | other::rest ->
                 connect_one vertex rest (add_edge vertex other graph))}
    val clique : int list -> (int * int) list -> (int * int) list
    val clique_def :
      (vertices : int list) ->
      (graph : (int * int) list) ->
      {u : unit
        | (clique vertices graph) ===
            (match vertices with
             | [] -> graph
             | vertex::rest -> clique rest (connect_one vertex rest graph))}
    val interference :
      instruction list ->
      int list list -> int -> (int * int) list -> (int * int) list
    val interference_def :
      (code : instruction list) ->
      (live : int list list) ->
      (index : int) ->
      (graph' : (int * int) list) ->
      {u : unit
        | (interference code live index graph') ===
            (match code with
             | [] -> graph'
             | instruction::rest ->
                 let graph =
                   match definition instruction with
                   | None -> graph'
                   | Some dst ->
                       connect_one dst
                         (live_out live (successors instruction)) graph' in
                 interference rest live (index + 1) graph)}
    val graph : instruction list -> int list list -> (int * int) list
    val graph_def :
      (code : instruction list) ->
      (live : int list list) ->
      {u : unit
        | (graph code live) ===
            (let entry = match live with | [] -> [] | xs::_ -> xs in
             interference code live 0 (clique entry []))}
    val reverse_into : 'a list -> 'a list -> 'a list
    val reverse_into_def :
      (xs : 'a list) ->
      (acc : 'a list) ->
      {u : unit
        | (reverse_into xs acc) ===
            (match xs with
             | [] -> acc
             | x::rest -> reverse_into rest (x :: acc))}
    val reverse : 'a list -> 'a list
    val reverse_def :
      (xs : 'a list) -> {u : unit | (reverse xs) === (reverse_into xs [])}
    val range_down : int -> int list
    val range_down_def :
      (n : int) ->
      {u : unit
        | (range_down n) ===
            (if n <= 0 then [] else (n - 1) :: (range_down (n - 1)))}
    val range : int -> int list
    val range_def :
      (n : int) -> {u : unit | (range n) === (reverse (range_down n))}
    val adjacent : int -> int -> (int * int) list -> bool
    val adjacent_def :
      (a : int) ->
      (b : int) ->
      (graph : (int * int) list) ->
      {u : unit | (adjacent a b graph) === (member_edge (edge a b) graph)}
    val add_edge_covers :
      (a : int) ->
      ((b : int) ->
       (graph : edge list) ->
       {u : unit | (a = b) || (adjacent a b (add_edge a b graph))} @ immutable) @ total
      stateful
    val add_edge_preserves :
      (pair : edge) ->
      ((a : int) ->
       (b : int) ->
       (graph : edge list) ->
       {u : unit
         | (not (member_edge pair graph)) ||
             (member_edge pair (add_edge a b graph))} @ immutable) @ total
      stateful
    val connect_one_preserves :
      (pair : edge) ->
      ((vertex : int) ->
       (vertices : int list) ->
       (graph : edge list) ->
       {u : unit
         | (not (member_edge pair graph)) ||
             (member_edge pair (connect_one vertex vertices graph))} @ immutable) @ total
      stateful
    val connect_one_covers :
      (vertex : int) ->
      ((vertices : int list) ->
       (graph : edge list) ->
       (other : int) ->
       {u : unit
         | (not (member other vertices)) ||
             ((vertex = other) ||
                (adjacent vertex other (connect_one vertex vertices graph)))} @ immutable) @ total
      stateful
    val edge_symmetric :
      (a : int) ->
      ((b : int) -> {u : unit | (edge a b) === (edge b a)} @ immutable) @ total
      stateful
    val clique_preserves :
      (pair : edge) ->
      ((vertices : int list) ->
       (graph : edge list) ->
       {u : unit
         | (not (member_edge pair graph)) ||
             (member_edge pair (clique vertices graph))} @ immutable) @ total
      stateful
    val clique_covers :
      (vertices : int list) ->
      ((graph : edge list) ->
       (a : int) ->
       (b : int) ->
       {u : unit
         | (not ((member a vertices) && (member b vertices))) ||
             ((a = b) || (adjacent a b (clique vertices graph)))} @ immutable) @ total
      stateful
    val interference_preserves :
      (pair : edge) ->
      ((code : instruction list) ->
       (live : int list list) ->
       (index : int) ->
       (graph : edge list) ->
       {u : unit
         | (not (member_edge pair graph)) ||
             (member_edge pair (interference code live index graph))} @ immutable) @ total
      stateful
    val interference_covers :
      (code : instruction list) ->
      ((live : int list list) ->
       (index : int) ->
       (graph : edge list) ->
       (pc : int) ->
       (dst : int) ->
       (other : int) ->
       {u : unit
         | match nth code pc with
           | None -> true
           | Some instruction ->
               (not ((definition instruction) === (Some dst))) ||
                 ((not
                     (member other (live_out live (successors instruction))))
                    ||
                    ((dst = other) ||
                       (adjacent dst other
                          (interference code live index graph))))} @ immutable) @ total
      stateful
    val graph_entry_covers :
      (code : instruction list) ->
      ((live : int list list) ->
       (a : int) ->
       (b : int) ->
       {u : unit
         | match live with
           | [] -> true
           | entry::_ ->
               (not ((member a entry) && (member b entry))) ||
                 ((a = b) || (adjacent a b (graph code live)))} @ immutable) @ total
      stateful
    val graph_write_covers :
      (code : instruction list) ->
      ((live : int list list) ->
       (pc : int) ->
       (dst : int) ->
       (other : int) ->
       {u : unit
         | match nth code pc with
           | None -> true
           | Some instruction ->
               (not ((definition instruction) === (Some dst))) ||
                 ((not
                     (member other (live_out live (successors instruction))))
                    ||
                    ((dst = other) || (adjacent dst other (graph code live))))} @ immutable) @ total
      stateful
    val conflicts : (int * int) list -> int -> int -> int list -> int -> bool
    val conflicts_def :
      (graph : (int * int) list) ->
      (vertex : int) ->
      (candidate : int) ->
      (colors : int list) ->
      (index : int) ->
      {u : unit
        | (conflicts graph vertex candidate colors index) ===
            (match colors with
             | [] -> false
             | color::rest ->
                 ((adjacent vertex index graph) && (candidate = color)) ||
                   (conflicts graph vertex candidate rest (index + 1)))}
    val conflicts_pair :
      (graph : edge list) ->
      ((vertex : int) ->
       (candidate : int) ->
       (colors : int list) ->
       (index : int) ->
       (offset : int) ->
       {u : unit
         | (not (adjacent vertex (index + offset) graph)) ||
             ((not ((nth colors offset) === (Some candidate))) ||
                (conflicts graph vertex candidate colors index))} @ immutable) @ total
      stateful
    val first_color :
      (int * int) list -> int -> int list -> int -> int list -> int option
    val first_color_def :
      (graph : (int * int) list) ->
      (vertex : int) ->
      (colors : int list) ->
      (other_index : int) ->
      (choices : int list) ->
      {u : unit
        | (first_color graph vertex colors other_index choices) ===
            (match choices with
             | [] -> None
             | candidate::rest ->
                 if conflicts graph vertex candidate colors other_index
                 then first_color graph vertex colors other_index rest
                 else Some candidate)}
    val first_color_sound :
      (graph : edge list) ->
      ((vertex : int) ->
       (colors : int list) ->
       (other_index : int) ->
       (choices : int list) ->
       {u : unit
         | match first_color graph vertex colors other_index choices with
           | None -> true
           | Some chosen ->
               not (conflicts graph vertex chosen colors other_index)} @ immutable) @ total
      stateful
    val proper_from : (int * int) list -> int -> int list -> bool
    val proper_from_def :
      (graph : (int * int) list) ->
      (index : int) ->
      (colors : int list) ->
      {u : unit
        | (proper_from graph index colors) ===
            (match colors with
             | [] -> true
             | chosen::rest ->
                 (not (conflicts graph index chosen rest (index + 1))) &&
                   (proper_from graph (index + 1) rest))}
    val proper_separates :
      (graph : edge list) ->
      ((index : int) ->
       (colors : int list) ->
       (left : int) ->
       (right : int) ->
       (color : int) ->
       {u : unit
         | (not (proper_from graph index colors)) ||
             ((left < index) ||
                ((left >= right) ||
                   ((not (adjacent left right graph)) ||
                      ((not ((nth colors (left - index)) === (Some color)))
                         ||
                         (not ((nth colors (right - index)) === (Some color)))))))} @ immutable) @ total
      stateful
    val proper_distinct :
      (graph : edge list) ->
      ((colors : int list) ->
       (a : int) ->
       (b : int) ->
       (color_a : int) ->
       (color_b : int) ->
       {u : unit
         | (not
              ((proper_from graph 0 colors) &&
                 ((0 <= a) &&
                    ((0 <= b) &&
                       ((a <> b) &&
                          ((adjacent a b graph) &&
                             (((nth colors a) === (Some color_a)) &&
                                ((nth colors b) === (Some color_b)))))))))
             || (color_a <> color_b)} @ immutable) @ total
      stateful
    val color :
      (int * int) list -> int list -> int -> 'a list -> int list option
    val color_def :
      (graph : (int * int) list) ->
      (choices : int list) ->
      (index : int) ->
      (vertices : 'a list) ->
      {u : unit
        | (color graph choices index vertices) ===
            (match vertices with
             | [] -> Some []
             | _::rest ->
                 (match color graph choices (index + 1) rest with
                  | None -> None
                  | Some colors ->
                      (match first_color graph index colors (index + 1)
                               choices
                       with
                       | None -> None
                       | Some chosen -> Some (chosen :: colors))))}
    val color_sound :
      (graph : edge list) ->
      ((choices : int list) ->
       (index : int) ->
       (vertices : int list) ->
       {u : unit
         | match color graph choices index vertices with
           | None -> true
           | Some colors -> proper_from graph index colors} @ immutable) @ total
      stateful
    val color_separates :
      (graph : edge list) ->
      ((choices : int list) ->
       (vertices : int list) ->
       (a : int) ->
       (b : int) ->
       (chosen : int) ->
       {u : unit
         | match color graph choices 0 vertices with
           | None -> true
           | Some colors ->
               (a < 0) ||
                 ((b < 0) ||
                    ((a = b) ||
                       ((not (adjacent a b graph)) ||
                          ((not ((nth colors a) === (Some chosen))) ||
                             (not ((nth colors b) === (Some chosen)))))))} @ immutable) @ total
      stateful
    val rename_operand : int list -> operand -> operand option
    val rename_operand_def :
      (colors : int list) ->
      (operand : operand) ->
      {u : unit
        | (rename_operand colors operand) ===
            (match operand with
             | Imm word -> Some (Imm word)
             | Reg r ->
                 (match nth colors r with
                  | None -> None
                  | Some color -> Some (Reg color)))}
    val rename_instruction : int list -> instruction -> instruction option
    val rename_instruction_def :
      (colors : int list) ->
      (instruction : instruction) ->
      {u : unit
        | (rename_instruction colors instruction) ===
            (match instruction with
             | Move (dst', operand', next') ->
                 (match ((nth colors dst'), (rename_operand colors operand'))
                  with
                  | (Some dst''', Some operand''') ->
                      Some (Move (dst''', operand''', next'))
                  | _ -> None)
             | Binary (dst'', operation, left', right', next'') ->
                 (match ((nth colors dst''), (rename_operand colors left'),
                          (rename_operand colors right'))
                  with
                  | (Some dst, Some left, Some right) ->
                      Some (Binary (dst, operation, left, right, next''))
                  | _ -> None)
             | Jump next -> Some (Jump next)
             | Branch (condition', yes, no) ->
                 (match rename_operand colors condition' with
                  | None -> None
                  | Some condition -> Some (Branch (condition, yes, no)))
             | Return operand'' ->
                 (match rename_operand colors operand'' with
                  | None -> None
                  | Some operand -> Some (Return operand)))}
    val rename : int list -> instruction list -> instruction list option
    val rename_def :
      (colors : int list) ->
      (code : instruction list) ->
      {u : unit
        | (rename colors code) ===
            (match code with
             | [] -> Some []
             | instruction'::rest' ->
                 (match ((rename_instruction colors instruction'),
                          (rename colors rest'))
                  with
                  | (Some instruction, Some rest) ->
                      Some (instruction :: rest)
                  | _ -> None))}
    val rename_nth :
      (colors : int list) ->
      ((code : instruction list) ->
       (pc : int) ->
       (instruction : instruction) ->
       {u : unit
         | match rename colors code with
           | None -> true
           | Some target ->
               (not ((nth code pc) === (Some instruction))) ||
                 ((nth target pc) === (rename_instruction colors instruction))} @ immutable) @ total
      stateful
    val agree_on : int list -> int list -> int list -> int list -> bool
    val agree_on_def :
      (live : int list) ->
      (colors : int list) ->
      (source : int list) ->
      (target : int list) ->
      {u : unit
        | (agree_on live colors source target) ===
            (match live with
             | [] -> true
             | reg::rest ->
                 (match nth colors reg with
                  | None -> false
                  | Some physical ->
                      (match ((nth source reg), (nth target physical)) with
                       | (Some left, Some right) -> left = right
                       | _ -> false))
                   && (agree_on rest colors source target))}
    val agree_lookup :
      (live : int list) ->
      ((colors : int list) ->
       (source : int list) ->
       (target : int list) ->
       (reg : int) ->
       {u : unit
         | (not ((agree_on live colors source target) && (member reg live)))
             ||
             (match nth colors reg with
              | None -> false
              | Some physical ->
                  (match ((nth source reg), (nth target physical)) with
                   | (Some left, Some right) -> left = right
                   | _ -> false))} @ immutable) @ total
      stateful
    val separate_from : int list -> int list -> int -> int -> bool
    val separate_from_def :
      (regs : int list) ->
      (colors : int list) ->
      (head : int) ->
      (physical : int) ->
      {u : unit
        | (separate_from regs colors head physical) ===
            (match regs with
             | [] -> true
             | reg::rest ->
                 ((reg = head) ||
                    (match nth colors reg with
                     | None -> false
                     | Some chosen -> chosen <> physical))
                   && (separate_from rest colors head physical))}
    val target_write_keeps_agreement :
      (regs : int list) ->
      ((colors : int list) ->
       (source : int list) ->
       (target : int list) ->
       (head : int) ->
       (physical : int) ->
       (word : int) ->
       {u : unit
         | (not
              ((agree_on regs colors source target) &&
                 ((separate_from regs colors head physical) &&
                    (((nth source head) === (Some word)) &&
                       ((nth colors head) === (Some physical))))))
             ||
             (match write target physical word with
              | None -> true
              | Some after -> agree_on regs colors source after)} @ immutable) @ total
      stateful
    val value_agrees :
      (live : int list) ->
      ((colors : int list) ->
       (source : int list) ->
       (target : int list) ->
       (operand : operand) ->
       {u : unit
         | (not
              ((agree_on live colors source target) &&
                 (subset (operand_uses operand) live)))
             ||
             (match rename_operand colors operand with
              | None -> true
              | Some renamed ->
                  (value source operand) === (value target renamed))} @ immutable) @ total
      stateful
    val operand_live : int list -> operand -> bool
    val operand_live_def :
      (live : int list) ->
      (operand : operand) ->
      {u : unit
        | (operand_live live operand) ===
            (match operand with | Imm _ -> true | Reg reg -> member reg live)}
    val value_agrees_live :
      (live : int list) ->
      ((colors : int list) ->
       (source : int list) ->
       (target : int list) ->
       (operand : operand) ->
       {u : unit
         | (not
              ((agree_on live colors source target) &&
                 (operand_live live operand)))
             ||
             (match rename_operand colors operand with
              | None -> true
              | Some renamed ->
                  (value source operand) === (value target renamed))} @ immutable) @ total
      stateful
    val binary_operands_live :
      (before : int list) ->
      ((dst : int) ->
       (operation : operation) ->
       (left : operand) ->
       (right : operand) ->
       (next : int) ->
       {u : unit
         | (not
              (subset (uses (Binary (dst, operation, left, right, next)))
                 before))
             || ((operand_live before left) && (operand_live before right))} @ immutable) @ total
      stateful
    val safe_write : int list -> int list -> int list -> int -> int -> bool
    val safe_write_def :
      (next : int list) ->
      (before : int list) ->
      (colors : int list) ->
      (dst : int) ->
      (physical : int) ->
      {u : unit
        | (safe_write next before colors dst physical) ===
            (match next with
             | [] -> true
             | reg::rest ->
                 ((reg = dst) ||
                    ((member reg before) &&
                       (match nth colors reg with
                        | None -> false
                        | Some color -> color <> physical)))
                   && (safe_write rest before colors dst physical))}
    val write_agreement :
      (next : int list) ->
      ((before : int list) ->
       (colors : int list) ->
       (source : int list) ->
       (target : int list) ->
       (dst : int) ->
       (physical : int) ->
       (word : int) ->
       {u : unit
         | (not
              ((agree_on before colors source target) &&
                 ((safe_write next before colors dst physical) &&
                    ((nth colors dst) === (Some physical)))))
             ||
             (match ((write source dst word), (write target physical word))
              with
              | (Some source_after, Some target_after) ->
                  agree_on next colors source_after target_after
              | _ -> true)} @ immutable) @ total
      stateful
    val length : 'a list -> int
    val length_def :
      (xs : 'a list) ->
      {u : unit
        | (length xs) ===
            (match xs with | [] -> 0 | _::rest -> 1 + (length rest))}
    val empty_live_length :
      (code : instruction list) ->
      {u : unit | (length (empty_live code)) = (length code)} @ immutable
    val sweep_length :
      (code : instruction list) ->
      ((live : int list list) ->
       (index : int) ->
       {u : unit
         | match sweep code live index with
           | (next, _) -> (length next) = (length code)} @ immutable) @ total
      stateful
    val stabilize_length :
      (fuel : int) ->
      ((code : instruction list) ->
       (live : int list list) ->
       {u : unit
         | ((length live) <> (length code)) ||
             (match stabilize fuel code live with
              | None -> true
              | Some result -> (length result) = (length code))} @ immutable) @ total
      stateful
    val color_length :
      (graph : edge list) ->
      ((choices : int list) ->
       (index : int) ->
       (vertices : int list) ->
       {u : unit
         | match color graph choices index vertices with
           | None -> true
           | Some colors -> (length colors) = (length vertices)} @ immutable) @ total
      stateful
    val rename_length :
      (colors : int list) ->
      ((code : instruction list) ->
       {u : unit
         | match rename colors code with
           | None -> true
           | Some target -> (length target) = (length code)} @ immutable) @ total
      stateful
    val zeros_length :
      (count : int) ->
      {u : unit | (count < 0) || ((length (zeros count)) = count)} @ immutable
    val nth_present :
      (xs : int list) ->
      ((index : int) ->
       {u : unit
         | (index < 0) ||
             ((index >= (length xs)) ||
                (match nth xs index with | None -> false | Some _ -> true))} @ immutable) @ total
      stateful
    val nth_live_present :
      (xs : int list list) ->
      ((index : int) ->
       {u : unit
         | (index < 0) ||
             ((index >= (length xs)) ||
                (match nth xs index with | None -> false | Some _ -> true))} @ immutable) @ total
      stateful
    val nth_code_present :
      (xs : instruction list) ->
      ((index : int) ->
       {u : unit
         | (index < 0) ||
             ((index >= (length xs)) ||
                (match nth xs index with | None -> false | Some _ -> true))} @ immutable) @ total
      stateful
    val write_present :
      (xs : int list) ->
      ((index : int) ->
       (word : int) ->
       {u : unit
         | (index < 0) ||
             ((index >= (length xs)) ||
                (match write xs index word with
                 | None -> false
                 | Some _ -> true))} @ immutable) @ total
      stateful
    val write_length :
      (xs : int list) ->
      ((index : int) ->
       (word : int) ->
       {u : unit
         | match write xs index word with
           | None -> true
           | Some ys -> (length ys) = (length xs)} @ immutable) @ total
      stateful
    val valid_reg : int -> int -> bool
    val valid_reg_def :
      (count : int) ->
      (reg : int) ->
      {u : unit | (valid_reg count reg) === ((0 <= reg) && (reg < count))}
    val valid_operand : int -> operand -> bool
    val valid_operand_def :
      (count : int) ->
      (operand : operand) ->
      {u : unit
        | (valid_operand count operand) ===
            (match operand with
             | Imm _ -> true
             | Reg reg -> valid_reg count reg)}
    val value_present :
      (count : int) ->
      ((file : int list) ->
       (operand : operand) ->
       {u : unit
         | (not (((length file) = count) && (valid_operand count operand)))
             ||
             (match value file operand with | None -> false | Some _ -> true)} @ immutable) @ total
      stateful
    val all_valid_reg : int -> int list -> bool
    val all_valid_reg_def :
      (count : int) ->
      (regs : int list) ->
      {u : unit
        | (all_valid_reg count regs) ===
            (match regs with
             | [] -> true
             | reg::rest ->
                 (valid_reg count reg) && (all_valid_reg count rest))}
    val all_valid_reg_weaken :
      (smaller : int) ->
      ((larger : int) ->
       (regs : int list) ->
       {u : unit
         | (smaller > larger) ||
             ((not (all_valid_reg smaller regs)) ||
                (all_valid_reg larger regs))} @ immutable) @ total
      stateful
    val range_down_valid :
      (count : int) ->
      {u : unit | all_valid_reg count (range_down count)} @ immutable
    val reverse_into_valid :
      (count : int) ->
      ((xs : int list) ->
       (acc : int list) ->
       {u : unit
         | (not ((all_valid_reg count xs) && (all_valid_reg count acc))) ||
             (all_valid_reg count (reverse_into xs acc))} @ immutable) @ total
      stateful
    val range_valid :
      (count : int) ->
      {u : unit | all_valid_reg count (range count)} @ immutable
    val first_color_valid :
      (graph : edge list) ->
      ((vertex : int) ->
       (colors : int list) ->
       (index : int) ->
       (choices : int list) ->
       (count : int) ->
       {u : unit
         | (not (all_valid_reg count choices)) ||
             (match first_color graph vertex colors index choices with
              | None -> true
              | Some chosen -> valid_reg count chosen)} @ immutable) @ total
      stateful
    val color_valid :
      (graph : edge list) ->
      ((choices : int list) ->
       (index : int) ->
       (vertices : int list) ->
       (count : int) ->
       {u : unit
         | (not (all_valid_reg count choices)) ||
             (match color graph choices index vertices with
              | None -> true
              | Some colors -> all_valid_reg count colors)} @ immutable) @ total
      stateful
    val all_valid_reg_lookup :
      (count : int) ->
      ((regs : int list) ->
       (index : int) ->
       (reg : int) ->
       {u : unit
         | (not
              ((all_valid_reg count regs) &&
                 ((nth regs index) === (Some reg))))
             || (valid_reg count reg)} @ immutable) @ total
      stateful
    val color_total :
      (graph : edge list) ->
      ((registers : int) ->
       (physical : int) ->
       {u : unit
         | (registers < 0) ||
             (match color graph (range physical) 0 (zeros registers) with
              | None -> true
              | Some colors ->
                  ((length colors) = registers) &&
                    (all_valid_reg physical colors))} @ immutable) @ total
      stateful
    val color_lookup :
      (graph : edge list) ->
      ((registers : int) ->
       (physical : int) ->
       (reg : int) ->
       {u : unit
         | (registers < 0) ||
             ((reg < 0) ||
                ((reg >= registers) ||
                   (match color graph (range physical) 0 (zeros registers)
                    with
                    | None -> true
                    | Some colors ->
                        (match nth colors reg with
                         | None -> false
                         | Some chosen -> valid_reg physical chosen))))} @ immutable) @ total
      stateful
    val valid_instruction : int -> int -> instruction -> bool
    val valid_instruction_def :
      (registers : int) ->
      (nodes : int) ->
      (instruction : instruction) ->
      {u : unit
        | (valid_instruction registers nodes instruction) ===
            (let valid_label label = valid_reg nodes label in
             match instruction with
             | Move (dst', operand', next') ->
                 (valid_reg registers dst') &&
                   ((valid_operand registers operand') && (valid_label next'))
             | Binary (dst, _, left, right, next'') ->
                 (valid_reg registers dst) &&
                   ((valid_operand registers left) &&
                      ((valid_operand registers right) &&
                         (valid_label next'')))
             | Jump next -> valid_label next
             | Branch (condition, yes, no) ->
                 (valid_operand registers condition) &&
                   ((valid_label yes) && (valid_label no))
             | Return operand -> valid_operand registers operand)}
    val all_valid_instructions : int -> int -> instruction list -> bool
    val all_valid_instructions_def :
      (registers : int) ->
      (nodes : int) ->
      (code : instruction list) ->
      {u : unit
        | (all_valid_instructions registers nodes code) ===
            (match code with
             | [] -> true
             | instruction::rest ->
                 (valid_instruction registers nodes instruction) &&
                   (all_valid_instructions registers nodes rest))}
    val valid : program -> bool
    val valid_def :
      (program : program) ->
      {u : unit
        | (valid program) ===
            (let nodes = length program.code in
             (0 < nodes) &&
               ((nodes <= 64) &&
                  ((0 < program.registers) &&
                     ((program.registers <= 32) &&
                        ((all_valid_reg program.registers program.inputs) &&
                           (all_valid_instructions program.registers nodes
                              program.code))))))}
    val valid_instruction_lookup :
      (registers : int) ->
      ((nodes : int) ->
       (code : instruction list) ->
       (pc : int) ->
       (instruction : instruction) ->
       {u : unit
         | (not
              ((all_valid_instructions registers nodes code) &&
                 ((nth code pc) === (Some instruction))))
             || (valid_instruction registers nodes instruction)} @ immutable) @ total
      stateful
    val valid_successor :
      (registers : int) ->
      ((nodes : int) ->
       (instruction : instruction) ->
       (successor : int) ->
       {u : unit
         | (not
              ((valid_instruction registers nodes instruction) &&
                 (member successor (successors instruction))))
             || (valid_reg nodes successor)} @ immutable) @ total
      stateful
    val rename_operand_valid :
      (colors : int list) ->
      ((registers : int) ->
       (physical : int) ->
       (operand : operand) ->
       {u : unit
         | (not
              (((length colors) = registers) &&
                 ((all_valid_reg physical colors) &&
                    (valid_operand registers operand))))
             ||
             (match rename_operand colors operand with
              | None -> true
              | Some renamed -> valid_operand physical renamed)} @ immutable) @ total
      stateful
    val rename_instruction_valid :
      (colors : int list) ->
      ((registers : int) ->
       (physical : int) ->
       (nodes : int) ->
       (instruction : instruction) ->
       {u : unit
         | (not
              (((length colors) = registers) &&
                 ((all_valid_reg physical colors) &&
                    (valid_instruction registers nodes instruction))))
             ||
             (match rename_instruction colors instruction with
              | None -> true
              | Some renamed -> valid_instruction physical nodes renamed)} @ immutable) @ total
      stateful
    val all_valid_live : int -> int list list -> bool
    val all_valid_live_def :
      (count : int) ->
      (live : int list list) ->
      {u : unit
        | (all_valid_live count live) ===
            (match live with
             | [] -> true
             | row::rest ->
                 (all_valid_reg count row) && (all_valid_live count rest))}
    val all_valid_live_lookup :
      (count : int) ->
      ((live : int list list) ->
       (index : int) ->
       (row : int list) ->
       {u : unit
         | (not
              ((all_valid_live count live) &&
                 ((nth live index) === (Some row))))
             || (all_valid_reg count row)} @ immutable) @ total
      stateful
    val add_valid :
      (count : int) ->
      ((value : int) ->
       (xs : int list) ->
       {u : unit
         | (not ((valid_reg count value) && (all_valid_reg count xs))) ||
             (all_valid_reg count (add value xs))} @ immutable) @ total
      stateful
    val union_valid :
      (count : int) ->
      ((xs : int list) ->
       (ys : int list) ->
       {u : unit
         | (not ((all_valid_reg count xs) && (all_valid_reg count ys))) ||
             (all_valid_reg count (union xs ys))} @ immutable) @ total
      stateful
    val remove_valid :
      (count : int) ->
      ((removed : int) ->
       (xs : int list) ->
       {u : unit
         | (not (all_valid_reg count xs)) ||
             (all_valid_reg count (remove removed xs))} @ immutable) @ total
      stateful
    val operand_uses_valid :
      (count : int) ->
      ((operand : operand) ->
       {u : unit
         | (not (valid_operand count operand)) ||
             (all_valid_reg count (operand_uses operand))} @ immutable) @ total
      stateful
    val uses_valid :
      (count : int) ->
      ((nodes : int) ->
       (instruction : instruction) ->
       {u : unit
         | (not (valid_instruction count nodes instruction)) ||
             (all_valid_reg count (uses instruction))} @ immutable) @ total
      stateful
    val live_out_valid :
      (count : int) ->
      ((live : int list list) ->
       (successors : int list) ->
       {u : unit
         | (not (all_valid_live count live)) ||
             (all_valid_reg count (live_out live successors))} @ immutable) @ total
      stateful
    val transfer_valid :
      (count : int) ->
      ((nodes : int) ->
       (live : int list list) ->
       (instruction : instruction) ->
       {u : unit
         | (not
              ((all_valid_live count live) &&
                 (valid_instruction count nodes instruction)))
             || (all_valid_reg count (transfer live instruction))} @ immutable) @ total
      stateful
    val empty_live_valid :
      (count : int) ->
      ((code : instruction list) ->
       {u : unit | all_valid_live count (empty_live code)} @ immutable) @ total
      stateful
    val sweep_valid :
      (count : int) ->
      ((nodes : int) ->
       (code : instruction list) ->
       (live : int list list) ->
       (index : int) ->
       {u : unit
         | (not
              ((all_valid_instructions count nodes code) &&
                 (all_valid_live count live)))
             ||
             (match sweep code live index with
              | (next, _) -> all_valid_live count next)} @ immutable) @ total
      stateful
    val stabilize_valid :
      (fuel : int) ->
      ((count : int) ->
       (nodes : int) ->
       (code : instruction list) ->
       (live : int list list) ->
       {u : unit
         | (not
              ((all_valid_instructions count nodes code) &&
                 (all_valid_live count live)))
             ||
             (match stabilize fuel code live with
              | None -> true
              | Some result -> all_valid_live count result)} @ immutable) @ total
      stateful
    val protected_before :
      (code : instruction list) ->
      ((live : int list list) ->
       (pc : int) ->
       (instruction : instruction) ->
       (before : int list) ->
       (successor : int) ->
       (row : int list) ->
       (reg : int) ->
       {u : unit
         | (not
              ((closed_from code live 0) &&
                 (((nth code pc) === (Some instruction)) &&
                    (((nth live pc) === (Some before)) &&
                       ((member successor (successors instruction)) &&
                          (((nth live successor) === (Some row)) &&
                             (member reg row)))))))
             ||
             (((definition instruction) === (Some reg)) ||
                (member reg before))} @ immutable) @ total
      stateful
    val protected_color :
      (code : instruction list) ->
      ((live : int list list) ->
       (colors : int list) ->
       (pc : int) ->
       (instruction : instruction) ->
       (successor : int) ->
       (row : int list) ->
       (dst : int) ->
       (reg : int) ->
       (physical : int) ->
       (color : int) ->
       {u : unit
         | (not
              (((nth code pc) === (Some instruction)) &&
                 (((definition instruction) === (Some dst)) &&
                    ((member successor (successors instruction)) &&
                       (((nth live successor) === (Some row)) &&
                          ((member reg row) &&
                             ((dst <> reg) &&
                                ((0 <= dst) &&
                                   ((0 <= reg) &&
                                      ((proper_from (graph code live) 0
                                          colors)
                                         &&
                                         (((nth colors dst) ===
                                             (Some physical))
                                            &&
                                            ((nth colors reg) ===
                                               (Some color)))))))))))))
             || (physical <> color)} @ immutable) @ total
      stateful
    val safe_write_from :
      (code : instruction list) ->
      ((live : int list list) ->
       (colors : int list) ->
       (registers : int) ->
       (pc : int) ->
       (instruction : instruction) ->
       (before : int list) ->
       (successor : int) ->
       (row : int list) ->
       (next : int list) ->
       (dst : int) ->
       (physical : int) ->
       {u : unit
         | (not
              ((closed_from code live 0) &&
                 (((nth code pc) === (Some instruction)) &&
                    (((nth live pc) === (Some before)) &&
                       ((member successor (successors instruction)) &&
                          (((nth live successor) === (Some row)) &&
                             ((subset next row) &&
                                ((all_valid_reg registers next) &&
                                   (((definition instruction) === (Some dst))
                                      &&
                                      ((valid_reg registers dst) &&
                                         (((length colors) = registers) &&
                                            ((proper_from (graph code live) 0
                                                colors)
                                               &&
                                               ((nth colors dst) ===
                                                  (Some physical))))))))))))))
             || (safe_write next before colors dst physical)} @ immutable) @ total
      stateful
    val successor_row_present :
      (registers : int) ->
      ((nodes : int) ->
       (instruction : instruction) ->
       (successor : int) ->
       (live : int list list) ->
       {u : unit
         | (not
              ((valid_instruction registers nodes instruction) &&
                 ((member successor (successors instruction)) &&
                    ((length live) = nodes))))
             ||
             (match nth live successor with | None -> false | Some _ -> true)} @ immutable) @ total
      stateful
    val agree_no_write :
      (code : instruction list) ->
      ((live : int list list) ->
       (colors : int list) ->
       (pc : int) ->
       (instruction : instruction) ->
       (before : int list) ->
       (successor : int) ->
       (row : int list) ->
       (next : int list) ->
       (source : int list) ->
       (target : int list) ->
       {u : unit
         | (not
              ((closed_from code live 0) &&
                 (((nth code pc) === (Some instruction)) &&
                    (((nth live pc) === (Some before)) &&
                       (((definition instruction) === None) &&
                          ((member successor (successors instruction)) &&
                             (((nth live successor) === (Some row)) &&
                                ((subset next row) &&
                                   (agree_on before colors source target)))))))))
             || (agree_on next colors source target)} @ immutable) @ total
      stateful
    val entry_pair_distinct :
      (code : instruction list) ->
      ((live : int list list) ->
       (whole : int list) ->
       (colors : int list) ->
       (a : int) ->
       (b : int) ->
       (color_a : int) ->
       (color_b : int) ->
       {u : unit
         | (not
              (((nth live 0) === (Some whole)) &&
                 ((member a whole) &&
                    ((member b whole) &&
                       ((0 <= a) &&
                          ((0 <= b) &&
                             ((a <> b) &&
                                ((proper_from (graph code live) 0 colors) &&
                                   (((nth colors a) === (Some color_a)) &&
                                      ((nth colors b) === (Some color_b)))))))))))
             || (color_a <> color_b)} @ immutable) @ total
      stateful
    val entry_separate :
      (code : instruction list) ->
      ((live : int list list) ->
       (whole : int list) ->
       (colors : int list) ->
       (registers : int) ->
       (head : int) ->
       (rest : int list) ->
       (physical : int) ->
       {u : unit
         | (not
              (((nth live 0) === (Some whole)) &&
                 ((member head whole) &&
                    ((subset rest whole) &&
                       ((all_valid_reg registers rest) &&
                          ((valid_reg registers head) &&
                             (((length colors) = registers) &&
                                ((proper_from (graph code live) 0 colors) &&
                                   ((nth colors head) === (Some physical))))))))))
             || (separate_from rest colors head physical)} @ immutable) @ total
      stateful
    type allocation = {
      code : instruction list;
      physical : int;
      source_registers : int;
      source_inputs : int list;
      input_slots : (int * int) list;
    }
    val build_slots : int list -> 'a list -> (int * 'a) list option
    val build_slots_def :
      (live : int list) ->
      (coloring : 'a list) ->
      {u : unit
        | (build_slots live coloring) ===
            (match live with
             | [] -> Some []
             | reg::rest ->
                 (match ((nth coloring reg), (build_slots rest coloring))
                  with
                  | (Some physical, Some slots) ->
                      Some ((reg, physical) :: slots)
                  | _ -> None))}
    val allocate : program -> int -> allocation option
    val allocate_def :
      (program : program) ->
      (physical : int) ->
      {u : unit
        | (allocate program physical) ===
            (if (not (valid program)) || ((physical <= 0) || (physical > 32))
             then None
             else
               (let initial_live = empty_live program.code in
                ghost_ (stabilize_closed 2049 program.code initial_live);
                (match stabilize 2049 program.code initial_live with
                 | None -> None
                 | Some live ->
                     let entry = match live with | [] -> [] | xs::_ -> xs in
                     if not (subset entry program.inputs)
                     then None
                     else
                       (let edges = graph program.code live in
                        let choices = range physical in
                        let vertices = zeros program.registers in
                        ghost_ (color_sound edges choices 0 vertices);
                        (match color edges choices 0 vertices with
                         | None -> None
                         | Some coloring ->
                             (match rename coloring program.code with
                              | None -> None
                              | Some code ->
                                  (match build_slots entry coloring with
                                   | None -> None
                                   | Some input_slots ->
                                       Some
                                         {
                                           code;
                                           physical;
                                           source_registers =
                                             (program.registers);
                                           source_inputs = (program.inputs);
                                           input_slots
                                         })))))))}
    val allocate_sound :
      (program : program) ->
      ((physical : int) ->
       {u : unit
         | match allocate program physical with
           | None -> true
           | Some
               { code = target_code; physical = out_physical;
                 source_registers; source_inputs; input_slots }
               ->
               (valid program) &&
                 ((0 < physical) &&
                    ((physical <= 32) &&
                       ((out_physical = physical) &&
                          ((source_registers = program.registers) &&
                             ((source_inputs === program.inputs) &&
                                ((match stabilize 2049 program.code
                                          (empty_live program.code)
                                  with
                                  | None -> false
                                  | Some live ->
                                      (closed_from program.code live 0) &&
                                        (((length live) =
                                            (length program.code))
                                           &&
                                           ((all_valid_live program.registers
                                               live)
                                              &&
                                              (let entry =
                                                 match live with
                                                 | [] -> []
                                                 | row::_ -> row in
                                               (subset entry program.inputs)
                                                 &&
                                                 ((match color
                                                           (graph
                                                              program.code
                                                              live)
                                                           (range physical) 0
                                                           (zeros
                                                              program.registers)
                                                   with
                                                   | None -> false
                                                   | Some coloring ->
                                                       (proper_from
                                                          (graph program.code
                                                             live) 0 coloring)
                                                         &&
                                                         (((length coloring)
                                                             =
                                                             program.registers)
                                                            &&
                                                            ((all_valid_reg
                                                                physical
                                                                coloring)
                                                               &&
                                                               (((rename
                                                                    coloring
                                                                    program.code)
                                                                   ===
                                                                   (Some
                                                                    target_code))
                                                                  &&
                                                                  ((build_slots
                                                                    entry
                                                                    coloring)
                                                                    ===
                                                                    (Some
                                                                    input_slots)))))))))))))))))} @ ghost) @ total
      stateful
    val same_shape : 'a list -> 'b list -> bool
    val same_shape_def :
      (xs' : 'a list) ->
      (ys' : 'b list) ->
      {u : unit
        | (same_shape xs' ys') ===
            (match (xs', ys') with
             | ([], []) -> true
             | (_::xs, _::ys) -> same_shape xs ys
             | _ -> false)}
    val load_inputs : 'a list -> int list -> 'a list -> 'a list option
    val load_inputs_def :
      (file' : 'a list) ->
      (registers' : int list) ->
      (values' : 'a list) ->
      {u : unit
        | (load_inputs file' registers' values') ===
            (match (registers', values') with
             | ([], []) -> Some file'
             | (reg::registers, value::values) ->
                 (match write file' reg value with
                  | None -> None
                  | Some file -> load_inputs file registers values)
             | _ -> None)}
    val load_inputs_length :
      (file : int list) ->
      ((registers : int list) ->
       (values : int list) ->
       {u : unit
         | match load_inputs file registers values with
           | None -> true
           | Some result -> (length result) = (length file)} @ immutable) @ total
      stateful
    val load_inputs_present :
      (file : int list) ->
      ((registers : int list) ->
       (values : int list) ->
       (count : int) ->
       {u : unit
         | (not
              (((length file) = count) &&
                 ((all_valid_reg count registers) &&
                    (same_shape registers values))))
             ||
             (match load_inputs file registers values with
              | None -> false
              | Some _ -> true)} @ immutable) @ total
      stateful
    val source_initial : program -> int list -> state
    val source_initial_def :
      (program : program) ->
      (args : int list) ->
      {u : unit
        | (source_initial program args) ===
            (match load_inputs (zeros program.registers) program.inputs args
             with
             | None -> Stuck
             | Some file -> Running (0, file))}
    val load_slots : 'a list -> 'a list -> (int * int) list -> 'a list option
    val load_slots_def :
      (file' : 'a list) ->
      (source : 'a list) ->
      (slots : (int * int) list) ->
      {u : unit
        | (load_slots file' source slots) ===
            (match slots with
             | [] -> Some file'
             | (reg, physical)::rest ->
                 (match ((load_slots file' source rest), (nth source reg))
                  with
                  | (Some file, Some value) -> write file physical value
                  | _ -> None))}
    val load_slots_length :
      (file : int list) ->
      ((source : int list) ->
       (slots : (int * int) list) ->
       {u : unit
         | match load_slots file source slots with
           | None -> true
           | Some result -> (length result) = (length file)} @ immutable) @ total
      stateful
    val target_initial :
      int -> int -> int list -> (int * int) list -> int list -> state
    val target_initial_def :
      (physical : int) ->
      (source_registers : int) ->
      (source_inputs : int list) ->
      (input_slots : (int * int) list) ->
      (args : int list) ->
      {u : unit
        | (target_initial physical source_registers source_inputs input_slots
             args)
            ===
            (let loaded =
               if same_shape source_inputs args
               then
                 match load_inputs (zeros source_registers) source_inputs
                         args
                 with
                 | None -> None
                 | Some source ->
                     load_slots (zeros physical) source input_slots
               else None in
             match loaded with
             | None -> Stuck
             | Some file -> Running (0, file))}
    val initial_of_allocation : allocation -> int list -> state
    val related :
      int ->
      int -> int -> int list list -> int list -> state -> state -> bool
    val related_def :
      (registers : int) ->
      (physical : int) ->
      (nodes : int) ->
      (live : int list list) ->
      (colors : int list) ->
      (source : state) ->
      (target : state) ->
      {u : unit
        | (related registers physical nodes live colors source target) ===
            (match (source, target) with
             | (Done left, Done right) -> left = right
             | (Running (pc, source_file), Running (target_pc, target_file))
                 ->
                 (pc = target_pc) &&
                   ((valid_reg nodes pc) &&
                      (((length source_file) = registers) &&
                         (((length target_file) = physical) &&
                            ((match nth live pc with
                              | None -> false
                              | Some row ->
                                  agree_on row colors source_file target_file)))))
             | _ -> false)}
    val observable_equal : state -> state -> bool
    val observable_equal_def :
      (source : state) ->
      (target : state) ->
      {u : unit
        | (observable_equal source target) ===
            (match (source, target) with
             | (Done left, Done right) -> left = right
             | (Running (pc, _), Running (other_pc, _)) -> pc = other_pc
             | _ -> false)}
    val related_observable :
      (registers : int) ->
      ((physical : int) ->
       (nodes : int) ->
       (live : int list list) ->
       (colors : int list) ->
       (source : state) ->
       (target : state) ->
       {u : unit
         | (not (related registers physical nodes live colors source target))
             || (observable_equal source target)} @ immutable) @ total
      stateful
    val execute_related :
      (code : instruction list) ->
      ((live : int list list) ->
       (colors : int list) ->
       (registers : int) ->
       (physical : int) ->
       (nodes : int) ->
       (pc : int) ->
       (instruction : instruction) ->
       (renamed : instruction) ->
       (source : int list) ->
       (target : int list) ->
       {u : unit
         | (not
              ((closed_from code live 0) &&
                 (((length live) = nodes) &&
                    ((all_valid_live registers live) &&
                       ((proper_from (graph code live) 0 colors) &&
                          (((length colors) = registers) &&
                             ((all_valid_reg physical colors) &&
                                ((valid_instruction registers nodes
                                    instruction)
                                   &&
                                   (((nth code pc) === (Some instruction)) &&
                                      (((rename_instruction colors
                                           instruction)
                                          === (Some renamed))
                                         &&
                                         (related registers physical nodes
                                            live colors
                                            (Running (pc, source))
                                            (Running (pc, target)))))))))))))
             ||
             (related registers physical nodes live colors
                (execute instruction source) (execute renamed target))} @ immutable) @ total
      stateful
    val step_related :
      (code : instruction list) ->
      ((live : int list list) ->
       (colors : int list) ->
       (registers : int) ->
       (physical : int) ->
       (target_code : instruction list) ->
       (source : state) ->
       (target : state) ->
       {u : unit
         | (not
              ((closed_from code live 0) &&
                 (((length live) = (length code)) &&
                    ((all_valid_live registers live) &&
                       ((proper_from (graph code live) 0 colors) &&
                          (((length colors) = registers) &&
                             ((all_valid_reg physical colors) &&
                                ((all_valid_instructions registers
                                    (length code) code)
                                   &&
                                   (((rename colors code) ===
                                       (Some target_code))
                                      &&
                                      (related registers physical
                                         (length code) live colors source
                                         target))))))))))
             ||
             (related registers physical (length code) live colors
                (step code source) (step target_code target))} @ immutable) @ total
      stateful
    val advance_related :
      (fuel : fuel) ->
      ((code : instruction list) ->
       (live : int list list) ->
       (colors : int list) ->
       (registers : int) ->
       (physical : int) ->
       (target_code : instruction list) ->
       (source : state) ->
       (target : state) ->
       {u : unit
         | (not
              ((closed_from code live 0) &&
                 (((length live) = (length code)) &&
                    ((all_valid_live registers live) &&
                       ((proper_from (graph code live) 0 colors) &&
                          (((length colors) = registers) &&
                             ((all_valid_reg physical colors) &&
                                ((all_valid_instructions registers
                                    (length code) code)
                                   &&
                                   (((rename colors code) ===
                                       (Some target_code))
                                      &&
                                      (related registers physical
                                         (length code) live colors source
                                         target))))))))))
             ||
             (related registers physical (length code) live colors
                (advance code fuel source) (advance target_code fuel target))} @ immutable) @ total
      stateful
    val copy_agreement :
      (code : instruction list) ->
      ((live : int list list) ->
       (whole : int list) ->
       (entry : int list) ->
       (colors : int list) ->
       (registers : int) ->
       (physical : int) ->
       (source : int list) ->
       (target : int list) ->
       {u : unit
         | (not
              (((nth live 0) === (Some whole)) &&
                 ((subset entry whole) &&
                    ((all_valid_reg registers entry) &&
                       (((length colors) = registers) &&
                          ((all_valid_reg physical colors) &&
                             ((proper_from (graph code live) 0 colors) &&
                                (((length source) = registers) &&
                                   ((length target) = physical)))))))))
             ||
             (match build_slots entry colors with
              | None -> true
              | Some slots ->
                  (match load_slots target source slots with
                   | None -> true
                   | Some after -> agree_on entry colors source after))} @ immutable) @ total
      stateful
    val copy_present :
      (entry : int list) ->
      ((colors : int list) ->
       (registers : int) ->
       (physical : int) ->
       (source : int list) ->
       (target : int list) ->
       {u : unit
         | (not
              ((all_valid_reg registers entry) &&
                 (((length colors) = registers) &&
                    ((all_valid_reg physical colors) &&
                       (((length source) = registers) &&
                          ((length target) = physical))))))
             ||
             (match build_slots entry colors with
              | None -> false
              | Some slots ->
                  (match load_slots target source slots with
                   | None -> false
                   | Some _ -> true))} @ immutable) @ total
      stateful
    val initial_related :
      (program : program) ->
      ((physical : int) ->
       (live : int list list) ->
       (colors : int list) ->
       (entry : int list) ->
       (slots : (int * int) list) ->
       (args : int list) ->
       {u : unit
         | (not
              ((valid program) &&
                 ((0 < physical) &&
                    (((nth live 0) === (Some entry)) &&
                       ((all_valid_live program.registers live) &&
                          (((length colors) = program.registers) &&
                             ((all_valid_reg physical colors) &&
                                ((proper_from (graph program.code live) 0
                                    colors)
                                   &&
                                   (((build_slots entry colors) ===
                                       (Some slots))
                                      && (same_shape program.inputs args))))))))))
             ||
             (related program.registers physical (length program.code) live
                colors (source_initial program args)
                (target_initial physical program.registers program.inputs
                   slots args))} @ immutable) @ total
      stateful
    val preserves :
      (program : program) ->
      ((physical : int) ->
       (args : int list) ->
       (fuel : fuel) ->
       {u : unit
         | match allocate program physical with
           | None -> true
           | Some
               { code = target_code; physical = out_physical;
                 source_registers; source_inputs; input_slots }
               ->
               (not (same_shape program.inputs args)) ||
                 (observable_equal
                    (advance program.code fuel (source_initial program args))
                    (advance target_code fuel
                       (target_initial out_physical source_registers
                          source_inputs input_slots args)))} @ ghost) @ total
      stateful
  end
|}]

let example =
  let open Register_allocation in
  { registers = 7; inputs = [0]; code = [
      Move (1, Reg 0, 1);
      Move (2, Imm 0, 2);
      Binary (3, Less_than, Imm 0, Reg 1, 3);
      Branch (Reg 3, 4, 10);
      Binary (4, Equal, Reg 1, Imm 2, 5);
      Branch (Reg 4, 6, 7);
      Move (5, Reg 2, 8);
      Binary (5, Add, Reg 2, Reg 1, 8);
      Binary (1, Subtract, Reg 1, Imm 1, 9);
      Move (2, Reg 5, 2);
      Move (6, Reg 2, 11);
      Return (Reg 6)
    ] }
;;
[%%expect{|
val example : Register_allocation.program =
  {Register_allocation.code =
    [Register_allocation.Move (1, Register_allocation.Reg 0, 1);
     Register_allocation.Move (2, Register_allocation.Imm 0, 2);
     Register_allocation.Binary (3, Register_allocation.Less_than,
      Register_allocation.Imm 0, Register_allocation.Reg 1, 3);
     Register_allocation.Branch (Register_allocation.Reg 3, 4, 10);
     Register_allocation.Binary (4, Register_allocation.Equal,
      Register_allocation.Reg 1, Register_allocation.Imm 2, 5);
     Register_allocation.Branch (Register_allocation.Reg 4, 6, 7);
     Register_allocation.Move (5, Register_allocation.Reg 2, 8);
     Register_allocation.Binary (5, Register_allocation.Add,
      Register_allocation.Reg 2, Register_allocation.Reg 1, 8);
     Register_allocation.Binary (1, Register_allocation.Subtract,
      Register_allocation.Reg 1, Register_allocation.Imm 1, 9);
     Register_allocation.Move (2, Register_allocation.Reg 5, 2);
     Register_allocation.Move (6, Register_allocation.Reg 2, 11);
     Register_allocation.Return (Register_allocation.Reg 6)];
   registers = 7; inputs = [0]}
|}]

let rec fuel n =
  if n <= 0 then Register_allocation.Z else Register_allocation.S (fuel (n - 1))
;;
[%%expect{|
val fuel : int -> Register_allocation.fuel = <fun>
|}]

let () =
  let open Register_allocation in
  match allocate example 3 with
  | None -> Printf.printf "allocation failed\n"
  | Some allocation ->
    Printf.printf "physical=%d slots=%d\n"
      allocation.physical (List.length allocation.input_slots);
    List.iter (fun input ->
      let source = advance example.code (fuel 80) (source_initial example [input]) in
      let target = advance allocation.code (fuel 80) (initial_of_allocation allocation [input]) in
      Printf.printf "input=%d equal=%b result=%s\n" input (source = target)
        (match target with Done word -> string_of_int word | Running _ -> "running" | Stuck -> "stuck"))
      [0; 1; 2; 3; 4]
;;
[%%expect{|
|}]

let () =
  let open Register_allocation in
  let dead_write = {
    registers = 2; inputs = [1];
    code = [Move (0, Imm 99, 1); Return (Reg 1)]
  } in
  let two_inputs = {
    registers = 2; inputs = [0; 1];
    code = [Binary (0, Add, Reg 0, Reg 1, 1); Return (Reg 0)]
  } in
  let duplicate_input = {
    registers = 1; inputs = [0; 0]; code = [Return (Reg 0)]
  } in
  let unused_input = {
    registers = 2; inputs = [0; 1]; code = [Return (Reg 0)]
  } in
  let forever = {registers = 1; inputs = []; code = [Jump 0]} in
  let dead_write_one = allocate dead_write 1 in
  let two_inputs_one = allocate two_inputs 1 in
  assert (dead_write_one = None && two_inputs_one = None);
  (match allocate dead_write 2 with
   | None -> assert false
   | Some allocation ->
     assert (advance allocation.code (fuel 2) (initial_of_allocation allocation [7]) = Done 7));
  (match allocate two_inputs 2 with
   | None -> assert false
   | Some allocation ->
     assert (advance allocation.code (fuel 2) (initial_of_allocation allocation [3; 4]) = Done 7));
  (match allocate duplicate_input 1 with
   | None -> assert false
   | Some allocation ->
     assert (advance allocation.code (fuel 1)
       (initial_of_allocation allocation [3; 5]) = Done 5));
  (match allocate unused_input 1 with
   | None -> assert false
   | Some allocation ->
     assert (advance allocation.code (fuel 1)
       (initial_of_allocation allocation [3; 99]) = Done 3));
  (match allocate forever 1 with
   | None -> assert false
   | Some allocation ->
     assert (observable_equal
       (advance forever.code (fuel 10) (source_initial forever []))
       (advance allocation.code (fuel 10) (initial_of_allocation allocation []))));
  Printf.printf "boundaries=passed\n"
;;
[%%expect{|
|}]
