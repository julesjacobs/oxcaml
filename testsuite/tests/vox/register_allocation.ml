open Register_allocation_spec

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
  | [] -> u
  | head :: rest ->
    if n = 0 then begin
      nth_def (value :: rest) m;
      u
    end else begin
      write_preserves rest (n - 1) value (m - 1);
      (match write rest (n - 1) value with
       | None -> ()
       | Some result -> nth_def (head :: result) m);
      u
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
  | [] -> u
  | head :: rest ->
    if n = 0 then begin nth_def (value :: rest) n; u end
    else begin
      write_reads rest (n - 1) value;
      (match write rest (n - 1) value with
       | None -> ()
       | Some result -> nth_def (head :: result) n);
      u
    end

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
  | [] -> u
  | head :: rest ->
    if head = reg then u
    else begin subset_member rest ys reg; u end

let rec (subset_cons @ total) :
  (xs : int list) -> (ys : int list) -> (head : int) ->
  {u : unit | not (subset xs ys) || subset xs (head :: ys)}
  @ immutable contended =
  fun xs ys head ->
  subset_def xs ys;
  subset_def xs (head :: ys);
  let u = () in
  match xs with
  | [] -> u
  | reg :: rest ->
    subset_cons rest ys head;
    member_def reg (head :: ys);
    u

let rec (subset_reflexive @ total) :
  (xs : int list) -> {u : unit | subset xs xs}
  @ immutable contended =
  fun xs ->
  subset_def xs xs;
  let u = () in
  match xs with
  | [] -> u
  | head :: rest ->
    subset_reflexive rest;
    subset_cons rest rest head;
    member_def head xs;
    u

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
  | [] -> u
  | head :: rest ->
    remove_keeps removed rest reg;
    u

let rec (union_keeps_right @ total) :
  (xs : int list) -> (ys : int list) -> (reg : int) ->
  {u : unit | not (member reg ys) || member reg (union xs ys)}
  @ immutable contended =
  fun xs ys reg ->
  union_def xs ys;
  let u = () in
  match xs with
  | [] -> u
  | head :: rest ->
    union_keeps_right rest ys reg;
    add_def head (union rest ys);
    member_def reg (head :: union rest ys);
    u

let (add_keeps @ total) :
  (value : int) -> (xs : int list) -> (reg : int) ->
  {u : unit | not (member reg xs) || member reg (add value xs)}
  @ immutable contended =
  fun value xs reg ->
  add_def value xs;
  member_def reg (value :: xs);
  ()

let rec (union_keeps_left @ total) :
  (xs : int list) -> (ys : int list) -> (reg : int) ->
  {u : unit | not (member reg xs) || member reg (union xs ys)}
  @ immutable contended =
  fun xs ys reg ->
  union_def xs ys;
  member_def reg xs;
  let u = () in
  match xs with
  | [] -> u
  | head :: rest ->
    union_keeps_left rest ys reg;
    add_keeps head (union rest ys) reg;
    add_def head (union rest ys);
    member_def reg (head :: union rest ys);
    u

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
  | [] -> u
  | head :: rest ->
    if head = successor then begin
      union_keeps_left row (live_out live rest) reg;
      u
    end else begin
      live_out_contains live rest successor reg row;
      union_keeps_right
        (match nth live head with None -> [] | Some xs -> xs)
        (live_out live rest) reg;
      u
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
    u
  | Some dst ->
    remove_keeps dst (live_out live (successors instruction)) reg;
    subset_member (survivors live instruction) current reg;
    u

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
  | [] -> u
  | head :: rest ->
    if pc = 0 then u
    else begin
      closed_lookup rest live (index + 1) (pc - 1) instruction;
      u
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
  | [] -> u
  | _ :: rest ->
    sweep_closed rest live (index + 1);
    u

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
  if fuel <= 0 then u
  else
    let next, changed = sweep code live 0 in
    if changed then begin
      stabilize_closed (fuel - 1) code next;
      u
    end else begin
      sweep_closed code live 0;
      u
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
  if a = b then u
  else if member_edge (edge a b) graph then u
  else begin
    member_edge_def (edge a b) ((edge a b) :: graph);
    u
  end

let (add_edge_preserves @ total) :
  (pair : edge) -> (a : int) -> (b : int) -> (graph : edge list) ->
  {u : unit | not (member_edge pair graph) || member_edge pair (add_edge a b graph)}
  @ immutable contended =
  fun pair a b graph ->
  add_edge_def a b graph;
  let u = () in
  if a = b || member_edge (edge a b) graph then u
  else begin
    member_edge_def pair ((edge a b) :: graph);
    u
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
  | [] -> u
  | other :: rest ->
    add_edge_preserves pair vertex other graph;
    connect_one_preserves pair vertex rest (add_edge vertex other graph);
    u

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
  | [] -> u
  | head :: rest ->
    if head = other then begin
      add_edge_covers vertex head graph;
      connect_one_preserves (edge vertex other) vertex rest
        (add_edge vertex head graph);
      adjacent_def vertex other (connect_one vertex rest (add_edge vertex head graph));
      adjacent_def vertex other (add_edge vertex head graph);
      u
    end else begin
      connect_one_covers vertex rest (add_edge vertex head graph) other;
      u
    end

let (edge_symmetric @ total) :
  (a : int) -> (b : int) -> {u : unit | edge a b === edge b a}
  @ immutable contended =
  fun a b ->
  edge_def a b;
  edge_def b a;
  ()

let rec (clique_preserves @ total) :
  (pair : edge) -> (vertices : int list) -> (graph : edge list) ->
  {u : unit |
    not (member_edge pair graph) || member_edge pair (clique vertices graph)}
  @ immutable contended =
  fun pair vertices graph ->
  clique_def vertices graph;
  let u = () in
  match vertices with
  | [] -> u
  | vertex :: rest ->
    connect_one_preserves pair vertex rest graph;
    clique_preserves pair rest (connect_one vertex rest graph);
    u

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
  | [] -> u
  | vertex :: rest ->
    if vertex = a then begin
      connect_one_covers vertex rest graph b;
      clique_preserves (edge a b) rest (connect_one vertex rest graph);
      adjacent_def a b (connect_one vertex rest graph);
      adjacent_def a b (clique rest (connect_one vertex rest graph));
      u
    end else if vertex = b then begin
      connect_one_covers vertex rest graph a;
      edge_symmetric a b;
      clique_preserves (edge a b) rest (connect_one vertex rest graph);
      adjacent_def vertex a (connect_one vertex rest graph);
      adjacent_def a b (clique rest (connect_one vertex rest graph));
      u
    end else begin
      clique_covers rest (connect_one vertex rest graph) a b;
      u
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
  | [] -> u
  | instruction :: rest ->
    (match definition instruction with
     | None -> interference_preserves pair rest live (index + 1) graph
     | Some dst ->
       let next_graph =
         connect_one dst (live_out live (successors instruction)) graph in
       connect_one_preserves pair dst (live_out live (successors instruction)) graph;
       interference_preserves pair rest live (index + 1) next_graph);
    u

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
  | [] -> u
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
      u
    end else begin
      interference_covers rest live (index + 1) next_graph
        (pc - 1) dst other;
      u
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
  | [] -> u
  | entry :: _ ->
    clique_covers entry [] a b;
    interference_preserves (edge a b) code live 0 (clique entry []);
    adjacent_def a b (clique entry []);
    adjacent_def a b (graph code live);
    u

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
  ()

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
  | [] -> u
  | color :: rest ->
    if offset = 0 then u
    else begin
      conflicts_pair graph vertex candidate rest (index + 1) (offset - 1);
      u
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
  | [] -> u
  | candidate :: rest ->
    if conflicts graph vertex candidate colors other_index then begin
      first_color_sound graph vertex colors other_index rest;
      u
    end else u

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
  | [] -> u
  | chosen :: rest ->
    if left = index then begin
      conflicts_pair graph index chosen rest (index + 1)
        (right - index - 1);
      u
    end else begin
      proper_separates graph (index + 1) rest left right color;
      u
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
  if color_a <> color_b || a = b || a < 0 || b < 0 then u
  else if a < b then begin
    proper_separates graph 0 colors a b color_a;
    u
  end else begin
    edge_symmetric a b;
    adjacent_def a b graph;
    adjacent_def b a graph;
    proper_separates graph 0 colors b a color_a;
    u
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
    u
  | _ :: rest ->
    color_sound graph choices (index + 1) rest;
    (match color graph choices (index + 1) rest with
     | None -> u
     | Some colors ->
       first_color_sound graph index colors (index + 1) choices;
       (match first_color graph index colors (index + 1) choices with
        | None -> u
        | Some chosen ->
          proper_from_def graph index (chosen :: colors);
          u))

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
  | None -> u
  | Some colors ->
    if a < 0 || b < 0 then u
    else if a < b then begin
      proper_separates graph 0 colors a b chosen;
      u
    end else if b < a then begin
      edge_symmetric a b;
      adjacent_def a b graph;
      adjacent_def b a graph;
      proper_separates graph 0 colors b a chosen;
      u
    end else u

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
  | [] -> u
  | head :: rest ->
    if pc = 0 then begin
      (match rename_instruction colors head, rename colors rest with
       | Some target_head, Some target_rest ->
         nth_def (target_head :: target_rest) pc
       | _ -> ());
      u
    end else begin
      rename_nth colors rest (pc - 1) instruction;
      (match rename_instruction colors head, rename colors rest with
       | Some target_head, Some target_rest ->
         nth_def (target_head :: target_rest) pc
       | _ -> ());
      u
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
  | [] -> u
  | head :: rest ->
    if head = reg then u
    else begin agree_lookup rest colors source target reg; u end

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
    u
  | reg :: rest ->
    target_write_keeps_agreement rest colors source target head physical word;
    (match nth colors reg with
     | None -> u
     | Some chosen ->
       if reg = head then write_reads target physical word
       else write_preserves target physical word chosen;
       (match write target physical word with
        | None -> u
        | Some after ->
          agree_on_def regs colors source after;
          u))

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
  | Imm word -> value_def target (Imm word); u
  | Reg reg ->
    operand_uses_def operand;
    member_def reg [reg];
    subset_member (operand_uses operand) live reg;
    agree_lookup live colors source target reg;
    (match nth colors reg with
     | None -> u
     | Some physical ->
       value_def target (Reg physical);
       u)

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
  | Imm word -> value_def target (Imm word); u
  | Reg reg ->
    agree_lookup live colors source target reg;
    (match nth colors reg with
     | None -> u
     | Some physical ->
       value_def target (Reg physical);
       u)

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
  u

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
  | [] -> u
  | reg :: rest ->
    write_agreement rest before colors source target dst physical word;
    (match write source dst word, write target physical word with
     | Some source_after, Some target_after ->
       if reg = dst then begin
         write_reads source dst word;
         write_reads target physical word;
         u
       end else begin
         agree_lookup before colors source target reg;
         write_preserves source dst word reg;
         (match nth colors reg with
          | None -> u
          | Some color ->
            write_preserves target physical word color;
            u)
       end
     | _ -> u)

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
    u
  | _ :: rest ->
    empty_live_length rest;
    let tail = empty_live rest in
    length_def ([] :: tail);
    u

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
    u
  | instruction :: rest ->
    sweep_length rest live (index + 1);
    (match sweep rest live (index + 1) with
     | tail, _ ->
       let old = match nth live index with None -> [] | Some xs -> xs in
       let next = union old (transfer live instruction) in
       length_def (next :: tail));
    u

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
  if fuel <= 0 then u
  else begin
    sweep_length code live 0;
    (match sweep code live 0 with
     | next, changed ->
       if changed then stabilize_length (fuel - 1) code next);
    u
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
  | [] -> length_def []; u
  | _ :: rest ->
    color_length graph choices (index + 1) rest;
    (match color graph choices (index + 1) rest with
     | None -> u
     | Some colors ->
       (match first_color graph index colors (index + 1) choices with
        | None -> u
        | Some chosen -> length_def (chosen :: colors); u))

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
  | [] -> length_def []; u
  | instruction :: rest ->
    rename_length colors rest;
    (match rename_instruction colors instruction, rename colors rest with
     | Some renamed, Some renamed_rest ->
       length_def (renamed :: renamed_rest);
       u
     | _ -> u)

let rec (zeros_length @ total) :
  (count : int) ->
  {u : unit | count < 0 || length (zeros count) = count}
  @ immutable contended =
  fun count ->
  zeros_def count;
  let u = () in
  if count < 0 then u
  else if count = 0 then begin
    length_def [];
    length_def (zeros count);
    u
  end
  else begin
    zeros_length (count - 1);
    length_def (0 :: zeros (count - 1));
    u
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
  | [] -> u
  | _ :: rest ->
    if index = 0 then u
    else begin nth_present rest (index - 1); u end

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
  | [] -> u
  | _ :: rest ->
    if index = 0 then u
    else begin nth_live_present rest (index - 1); u end

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
  | [] -> u
  | _ :: rest ->
    if index = 0 then u
    else begin nth_code_present rest (index - 1); u end

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
  | [] -> u
  | _ :: rest ->
    if index = 0 then u
    else begin write_present rest (index - 1) word; u end

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
  | [] -> u
  | head :: rest ->
    if index = 0 then begin length_def (word :: rest); u end
    else begin
      write_length rest (index - 1) word;
      (match write rest (index - 1) word with
       | None -> ()
       | Some result -> length_def (head :: result));
      u
    end

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
  | Imm _ -> u
  | Reg reg ->
    valid_reg_def count reg;
    nth_present file reg;
    u

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
  | [] -> u
  | reg :: rest ->
    valid_reg_def smaller reg;
    valid_reg_def larger reg;
    all_valid_reg_weaken smaller larger rest;
    u

let rec (range_down_valid @ total) :
  (count : int) ->
  {u : unit | all_valid_reg count (range_down count)}
  @ immutable contended =
  fun count ->
  let u = () in
  if count <= 0 then begin
    range_down_def count;
    all_valid_reg_def count [];
    u
  end else begin
    range_down_valid (count - 1);
    all_valid_reg_weaken (count - 1) count (range_down (count - 1));
    range_down_def count;
    all_valid_reg_def count ((count - 1) :: range_down (count - 1));
    valid_reg_def count (count - 1);
    u
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
  | [] -> u
  | head :: rest ->
    all_valid_reg_def count xs;
    all_valid_reg_def count (head :: acc);
    reverse_into_valid count rest (head :: acc);
    u

let (range_valid @ total) :
  (count : int) -> {u : unit | all_valid_reg count (range count)}
  @ immutable contended =
  fun count ->
  range_down_valid count;
  reverse_into_valid count (range_down count) [];
  range_def count;
  reverse_def (range_down count);
  all_valid_reg_def count [];
  ()

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
  | [] -> u
  | _ :: rest ->
    first_color_valid graph vertex colors index rest count;
    u

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
  | [] -> all_valid_reg_def count []; u
  | _ :: rest ->
    color_valid graph choices (index + 1) rest count;
    (match color graph choices (index + 1) rest with
     | None -> u
     | Some colors ->
       first_color_valid graph index colors (index + 1) choices count;
       (match first_color graph index colors (index + 1) choices with
        | None -> u
        | Some chosen ->
          all_valid_reg_def count (chosen :: colors);
          u))

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
  | [] -> u
  | _ :: rest ->
    if index = 0 then u
    else begin
      all_valid_reg_lookup count rest (index - 1) reg;
      u
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
  ()

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
  | None -> u
  | Some colors ->
    nth_present colors reg;
    (match nth colors reg with
     | None -> u
     | Some chosen ->
       all_valid_reg_lookup physical colors reg chosen;
       u)

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
  | [] -> u
  | _ :: rest ->
    if pc = 0 then u
    else begin
      valid_instruction_lookup registers nodes rest (pc - 1) instruction;
      u
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
    u
  | Binary (_, _, _, _, next) ->
    member_def successor [next];
    member_def successor [];
    valid_reg_def nodes next;
    valid_reg_def nodes successor;
    u
  | Jump next ->
    member_def successor [next];
    member_def successor [];
    valid_reg_def nodes next;
    valid_reg_def nodes successor;
    u
  | Branch (_, yes, no) ->
    member_def successor [yes; no];
    member_def successor [no];
    member_def successor [];
    valid_reg_def nodes yes;
    valid_reg_def nodes no;
    valid_reg_def nodes successor;
    u
  | Return _ -> member_def successor []; u

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
  | Imm _ -> valid_operand_def physical operand; u
  | Reg reg ->
    (match nth colors reg with
     | None -> u
     | Some chosen ->
       all_valid_reg_lookup physical colors reg chosen;
       valid_operand_def physical (Reg chosen);
       u)

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
       u
     | _ -> u)
  | Binary (dst, operation, left, right, next) ->
    rename_operand_valid colors registers physical left;
    rename_operand_valid colors registers physical right;
    (match nth colors dst, rename_operand colors left, rename_operand colors right with
     | Some chosen, Some left, Some right ->
       all_valid_reg_lookup physical colors dst chosen;
       valid_instruction_def physical nodes
         (Binary (chosen, operation, left, right, next));
       u
     | _ -> u)
  | Jump next ->
    valid_instruction_def physical nodes (Jump next);
    u
  | Branch (condition, yes, no) ->
    rename_operand_valid colors registers physical condition;
    (match rename_operand colors condition with
     | None -> u
     | Some renamed ->
       valid_instruction_def physical nodes (Branch (renamed, yes, no));
       u)
  | Return operand ->
    rename_operand_valid colors registers physical operand;
    (match rename_operand colors operand with
     | None -> u
     | Some renamed ->
       valid_instruction_def physical nodes (Return renamed);
       u)

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
  | [] -> u
  | _ :: rest ->
    if index = 0 then u
    else begin all_valid_live_lookup count rest (index - 1) row; u end

let (add_valid @ total) :
  (count : int) -> (value : int) -> (xs : int list) ->
  {u : unit |
    not (valid_reg count value && all_valid_reg count xs)
    || all_valid_reg count (add value xs)}
  @ immutable contended =
  fun count value xs ->
  add_def value xs;
  all_valid_reg_def count (value :: xs);
  ()

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
  | [] -> u
  | head :: rest ->
    union_valid count rest ys;
    add_valid count head (union rest ys);
    u

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
  | [] -> u
  | head :: rest ->
    remove_valid count removed rest;
    if head = removed then u
    else begin
      all_valid_reg_def count (head :: remove removed rest);
      u
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
  | Imm _ -> all_valid_reg_def count []; u
  | Reg reg ->
    all_valid_reg_def count [reg];
    all_valid_reg_def count [];
    u

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
    u
  | Binary (_, _, left, right, _) ->
    operand_uses_valid count left;
    operand_uses_valid count right;
    union_valid count (operand_uses left) (operand_uses right);
    u
  | Jump _ -> all_valid_reg_def count []; u

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
  | [] -> all_valid_reg_def count []; u
  | successor :: rest ->
    live_out_valid count live rest;
    (match nth live successor with
     | None -> all_valid_reg_def count []
     | Some row -> all_valid_live_lookup count live successor row);
    union_valid count
      (match nth live successor with None -> [] | Some xs -> xs)
      (live_out live rest);
    u

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
  u

let rec (empty_live_valid @ total) :
  (count : int) -> (code : instruction list) ->
  {u : unit | all_valid_live count (empty_live code)}
  @ immutable contended =
  fun count code ->
  empty_live_def code;
  all_valid_live_def count (empty_live code);
  let u = () in
  match code with
  | [] -> u
  | _ :: rest ->
    empty_live_valid count rest;
    all_valid_reg_def count [];
    u

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
  | [] -> all_valid_live_def count []; u
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
    u

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
  if fuel <= 0 then u
  else begin
    sweep_valid count nodes code live 0;
    (match sweep code live 0 with
     | next, changed ->
       if changed then stabilize_valid (fuel - 1) count nodes code next);
    u
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
  ()

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
  ()

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
  | [] -> u
  | reg :: rest ->
    safe_write_from code live colors registers pc instruction before
      successor row rest dst physical;
    if reg = dst then u
    else begin
      member_def reg next;
      subset_member next row reg;
      protected_before code live pc instruction before successor row reg;
      valid_reg_def registers reg;
      valid_reg_def registers dst;
      nth_present colors reg;
      (match nth colors reg with
       | None -> u
       | Some color ->
         protected_color code live colors pc instruction successor row
           dst reg physical color;
         u)
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
  ()

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
  | [] -> u
  | reg :: rest ->
    agree_no_write code live colors pc instruction before successor row rest
      source target;
    member_def reg next;
    subset_member next row reg;
    protected_before code live pc instruction before successor row reg;
    agree_lookup before colors source target reg;
    u

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
  | [] -> u
  | entry :: _ ->
    graph_entry_covers code live a b;
    proper_distinct (graph code live) colors a b color_a color_b;
    u

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
  | [] -> u
  | reg :: tail ->
    entry_separate code live whole colors registers head tail physical;
    if reg = head then u
    else begin
      member_def reg rest;
      subset_member rest whole reg;
      valid_reg_def registers reg;
      valid_reg_def registers head;
      nth_present colors reg;
      (match nth colors reg with
       | None -> u
       | Some chosen ->
         entry_pair_distinct code live whole colors head reg physical chosen;
         u)
    end

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
  if not (valid program) || physical <= 0 || physical > 32 then u
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
     | None -> u
     | Some live ->
       let entry = match live with [] -> [] | row :: _ -> row in
       if not (subset entry program.inputs) then u
       else begin
         let edges = graph program.code live in
         color_sound edges (range physical) 0 (zeros program.registers);
         color_total edges program.registers physical;
         (match color edges (range physical) 0 (zeros program.registers) with
          | None -> u
          | Some coloring ->
            (match rename coloring program.code with
             | None -> u
             | Some code ->
               match build_slots entry coloring with
               | None -> u
               | Some slots -> u))
       end)
  end
  )

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
     | None -> u
     | Some after ->
       write_length file reg word;
       load_inputs_length after rest words;
       u)
  | _ -> u

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
  | [], [] -> u
  | reg :: rest, word :: words ->
    valid_reg_def count reg;
    write_present file reg word;
    (match write file reg word with
     | None -> u
     | Some after ->
       write_length file reg word;
       load_inputs_present after rest words count;
       u)
  | _ -> u

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
  | [] -> u
  | (reg, physical) :: rest ->
    load_slots_length file source rest;
    (match load_slots file source rest, nth source reg with
     | Some middle, Some word -> write_length middle physical word
     | _ -> ());
    u

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
  | Done _, Done _ | Running _, Running _ -> u
  | _ -> u

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
  | None -> u
  | Some before ->
    closed_lookup code live 0 pc instruction;
    (match instruction with
     | Jump next ->
       successors_def instruction;
       member_def next [next];
       definition_def instruction;
       successor_row_present registers nodes instruction next live;
       (match nth live next with
        | None -> u
        | Some row ->
          subset_reflexive row;
          agree_no_write code live colors pc instruction before next row row
            source target;
          valid_successor registers nodes instruction next;
          related_def registers physical nodes live colors
            (Running (next, source)) (Running (next, target));
          u)
     | Return operand ->
       uses_def instruction;
       valid_instruction_def registers nodes instruction;
       value_agrees before colors source target operand;
       value_present registers source operand;
       (match rename_operand colors operand with
        | None -> u
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
             u
           | _ -> u))
     | Branch (condition, yes, no) ->
       uses_def instruction;
       definition_def instruction;
       valid_instruction_def registers nodes instruction;
       value_agrees before colors source target condition;
       value_present registers source condition;
       (match rename_operand colors condition with
        | None -> u
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
              | None -> u
              | Some row ->
                subset_reflexive row;
                agree_no_write code live colors pc instruction before
                  next row row source target;
                valid_successor registers nodes instruction next;
                related_def registers physical nodes live colors
                  (Running (next, source)) (Running (next, target));
                u)
           | _ -> u))
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
                u
              | _ -> u)
           | _ -> u)
        | _ -> u)
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
                u
              | _ -> u)
           | _ -> u)
        | _ -> u))

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
    u
  | Running (pc, source_file), Running (target_pc, target_file) ->
    valid_reg_def (length code) pc;
    nth_code_present code pc;
    rename_length colors code;
    nth_code_present target_code pc;
    (match nth code pc with
     | None -> u
     | Some instruction ->
       valid_instruction_lookup registers (length code) code pc instruction;
       rename_nth colors code pc instruction;
       (match rename_instruction colors instruction with
        | None -> u
        | Some renamed ->
          execute_related code live colors registers physical (length code)
            pc instruction renamed source_file target_file;
          u))
  | _ -> u

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
  | Z -> u
  | S rest ->
    step_related code live colors registers physical target_code source target;
    advance_related rest code live colors registers physical target_code
      (step code source) (step target_code target);
    u

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
    u
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
           | None -> u
           | Some after ->
             write_reads middle chosen word;
             agree_on_def entry colors source after;
             u)
        | _ -> u)
     | _ -> u)

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
  | [] -> load_slots_def target source []; u
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
          u
        | _ -> u)
     | _ -> u)

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
  | None -> u
  | Some source ->
    load_inputs_length (zeros program.registers) program.inputs args;
    copy_present entry colors program.registers physical source
      (zeros physical);
    copy_agreement program.code live entry entry colors program.registers
      physical source (zeros physical);
    (match load_slots (zeros physical) source slots with
     | None -> u
     | Some target ->
       load_slots_length (zeros physical) source slots;
       valid_reg_def (length program.code) 0;
       related_def program.registers physical (length program.code)
         live colors (Running (0, source)) (Running (0, target));
       u)

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
  | None -> u
  | Some {code = target_code; physical = out_physical;
          source_registers; source_inputs; input_slots} ->
    if not (same_shape program.inputs args) then u
    else begin
      valid_def program;
      (match stabilize 2049 program.code (empty_live program.code) with
       | None -> u
       | Some live ->
         let entry = match live with [] -> [] | row :: _ -> row in
         (match color (graph program.code live) (range physical) 0
                  (zeros program.registers) with
          | None -> u
          | Some colors ->
            length_def live;
            nth_def live 0;
            (match live with
             | [] -> u
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
               u)))
    end
  )

let (allocation_domain @ total) :
  (program : program) -> (physical : int) ->
  {u : unit |
    match allocate program physical with
    | None -> true
    | Some allocation ->
      valid program && 0 < physical && physical <= 32
      && allocation.physical = physical
      && allocation.source_registers = program.registers
      && allocation.source_inputs === program.inputs}
  @ ghost =
  fun program physical -> ghost_ (
    allocate_sound program physical;
    let u = () in
    u)
