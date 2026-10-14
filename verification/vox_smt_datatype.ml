module Plain_int_table = Hashtbl.Make (struct
  type t = int
  let equal = Int.equal
  let hash key = key land max_int
end)

module Mixed_int_table = Hashtbl.Make (struct
  type t = int
  let equal = Int.equal
  let hash x =
    let x = x lxor (x lsr (Sys.int_size / 2)) in
    let x = x lxor (x lsr 16) in
    let x = x * 0x45d9f3b in
    let x = x lxor (x lsr 16) in
    let x = x * 0x45d9f3b in
    (x lxor (x lsr 16)) land max_int
end)

module Int_table = struct
  type 'a representation =
    | Plain of 'a Plain_int_table.t
    | Mixed of 'a Mixed_int_table.t

  type 'a t =
    { mutable representation : 'a representation;
      mutable counts : int array
    }

  let create requested =
    let table = Plain_int_table.create requested in
    let capacity = (Plain_int_table.stats table).num_buckets in
    { representation = Plain table; counts = Array.make capacity 0 }

  let find_opt table key = match table.representation with
    | Plain data -> Plain_int_table.find_opt data key
    | Mixed data -> Mixed_int_table.find_opt data key

  let find table key = match table.representation with
    | Plain data -> Plain_int_table.find data key
    | Mixed data -> Mixed_int_table.find data key

  let mem table key = match table.representation with
    | Plain data -> Plain_int_table.mem data key
    | Mixed data -> Mixed_int_table.mem data key

  let fold f table acc = match table.representation with
    | Plain data -> Plain_int_table.fold f data acc
    | Mixed data -> Mixed_int_table.fold f data acc

  let iter f table = match table.representation with
    | Plain data -> Plain_int_table.iter f data
    | Mixed data -> Mixed_int_table.iter f data

  let add table key value = match table.representation with
    | Mixed data -> Mixed_int_table.add data key value
    | Plain data ->
      Plain_int_table.add data key value;
      let capacity = Array.length table.counts in
      let position = key land (capacity - 1) in
      table.counts.(position) <- table.counts.(position) + 1;
      if table.counts.(position) > 8 then begin
        let mixed = Mixed_int_table.create capacity in
        let bindings = Plain_int_table.fold
            (fun key value bindings -> (key, value) :: bindings) data [] in
        List.iter (fun (key, value) -> Mixed_int_table.add mixed key value)
          bindings;
        table.representation <- Mixed mixed;
        table.counts <- [||]
      end else if Plain_int_table.length data > 2 * capacity then begin
        let capacity = (Plain_int_table.stats data).num_buckets in
        let counts = Array.make capacity 0 in
        Plain_int_table.iter (fun key _ ->
          let position = key land (capacity - 1) in
          counts.(position) <- counts.(position) + 1) data;
        table.counts <- counts
      end

  let replace table key value = match table.representation with
    | Mixed data -> Mixed_int_table.replace data key value
    | Plain data ->
      if Plain_int_table.mem data key then
        Plain_int_table.replace data key value
      else add table key value
end

module Term = Vox_smt_term

module Literals = Hashtbl.Make (struct
  type t = int list
  let rec equal xs ys = match xs, ys with
    | [], [] -> true
    | x :: xs, y :: ys -> x = y && equal xs ys
    | _ -> false
  let hash = List.fold_left (fun hash literal -> hash * 65599 lxor literal) 0
end)

module Equalities = Hashtbl.Make (struct
  type t = int * int
  let equal (a, b) (c, d) = a = c && b = d
  let hash (a, b) = a * 65599 lxor b
end)

module Booleans = Int_table

type entry =
  { term : Term.term;
    datatype : int;
    constructor : (Term.symbol * Term.term array) option;
    mutable demanded : bool;
    mutable initialized : bool;
    mutable testers : (Term.symbol * int) list;
    expanded : (Term.symbol, unit) Hashtbl.t
  }

type t =
  { terms : Term.t;
    boolean : Term.term -> int;
    clause : int array -> unit;
    internalize : Term.term -> unit;
    poll : unit -> unit;
    entries : entry Int_table.t;
    declared_datatypes : unit Int_table.t;
    finite_cache : (bool * (int * Term.symbol list) list) Int_table.t;
    observed : unit Int_table.t;
    selectors : (Term.symbol * int * Term.term) Int_table.t;
    testers : (Term.symbol * Term.term) Int_table.t;
    emitted : unit Literals.t;
    equalities : int Equalities.t;
    booleans : int Booleans.t;
    mutable enum_equalities : (int * Term.term * Term.term * int) list;
    mutable enum_dirty : bool;
    mutable eager : bool;
    mutable generation : int;
    mutable revision : int
  }

type result = Done | Continue | Incomplete of string

let create ~terms ~boolean ~clause ~internalize ?(poll = fun () -> ()) () =
  { terms;
    boolean;
    clause;
    internalize;
    poll;
    entries = Int_table.create 64;
    declared_datatypes = Int_table.create 16;
    finite_cache = Int_table.create 16;
    observed = Int_table.create 128;
    selectors = Int_table.create 32;
    testers = Int_table.create 32;
    emitted = Literals.create 64;
    equalities = Equalities.create 64;
    booleans = Booleans.create 64;
    enum_equalities = [];
    enum_dirty = false;
    eager = false;
    generation = 0;
    revision = 0
  }

let entries t =
  Int_table.fold (fun _ entry acc -> entry :: acc) t.entries []

let generated t f =
  t.generation <- t.generation + 1;
  Fun.protect
    ~finally:(fun () -> t.generation <- t.generation - 1)
    f

let boolean t term =
  match Booleans.find_opt t.booleans term with
  | Some literal -> literal
  | None ->
    let literal = generated t (fun () -> t.boolean term) in
    Booleans.add t.booleans term literal;
    literal
let internalize t term = generated t (fun () -> t.internalize term)

let equal t left right =
  let key = if left <= right then left, right else right, left in
  match Equalities.find_opt t.equalities key with
  | Some literal -> literal
  | None ->
    let literal = boolean t (Term.equal_atom t.terms left right) in
    Equalities.add t.equalities key literal;
    literal

let emit t literals =
  t.poll ();
  let literals = List.sort_uniq Int.compare literals in
  if
    not (List.exists (fun literal -> List.mem (-literal) literals) literals)
    && not (Literals.mem t.emitted literals)
  then begin
    t.clause (Array.of_list literals);
    Literals.add t.emitted literals ();
    t.revision <- t.revision + 1
  end

let register_equality t ~literal left right =
  match Term.sort t.terms left with
  | Term.Named datatype ->
    let constructors = Term.constructors t.terms datatype in
    if constructors <> [] && List.for_all (fun constructor ->
        t.poll ();
        Array.length (Term.Unsafe.signature t.terms constructor).arguments = 0)
        constructors then begin
      t.enum_equalities
        <- (datatype, left, right, literal) :: t.enum_equalities;
      t.enum_dirty <- true
    end
  | Term.Bool | Term.Int | Term.Bitvec _ -> ()

let enum_cliques t =
  if t.enum_dirty then begin
    let groups = Hashtbl.create 8 in
    List.iter (fun (datatype, left, right, literal) ->
      t.poll ();
      let edges = Option.value (Hashtbl.find_opt groups datatype) ~default:[] in
      Hashtbl.replace groups datatype ((left, right, literal) :: edges))
      t.enum_equalities;
    Hashtbl.iter (fun datatype edges ->
      let cardinal = List.length (Term.constructors t.terms datatype) in
      let neighbours = Hashtbl.create 64 in
      let literals = Equalities.create (List.length edges) in
      let insert left right =
        let adjacent = Option.value (Hashtbl.find_opt neighbours left)
            ~default:[] in
        Hashtbl.replace neighbours left (right :: adjacent) in
      List.iter (fun (left, right, literal) ->
        t.poll ();
        if left <> right then begin
          let key = if left < right then left, right else right, left in
          if not (Equalities.mem literals key) then begin
            Equalities.add literals key literal;
            insert left right; insert right left
          end
        end) edges;
      let vertices = Hashtbl.fold (fun term adjacent vertices ->
        t.poll (); (term, List.length adjacent) :: vertices) neighbours [] in
      let vertices = List.sort (fun (a, degree_a) (b, degree_b) ->
        t.poll ();
        let order = Int.compare degree_b degree_a in
        if order <> 0 then order else Int.compare a b) vertices in
      let adjacent left right =
        t.poll ();
        Equalities.mem literals
          (if left < right then left, right else right, left) in
      let rec choose size clique = function
        | [] -> ()
        | (vertex, _) :: rest ->
          if List.for_all (adjacent vertex) clique then begin
            let clique = vertex :: clique in
            if size = cardinal then begin
              let rec pairs clauses = function
                | [] -> clauses
                | left :: rest ->
                  let clauses = List.fold_left (fun clauses right ->
                    t.poll ();
                    let key = if left < right then left, right
                      else right, left in
                    Equalities.find literals key :: clauses) clauses rest in
                  pairs clauses rest in
              emit t (pairs [] clique)
            end else choose (size + 1) clique rest
          end else choose size clique rest in
      choose 0 [] vertices) groups;
    t.enum_dirty <- false
  end

let finite_uncached t sort =
  let dependencies = ref [] in
  let known = Hashtbl.create 16 in
  let rec visit visiting = function
    | Term.Bool | Term.Bitvec _ -> true
    | Term.Int -> false
    | Term.Named datatype ->
      t.poll ();
      match Hashtbl.find_opt known datatype with
      | Some result -> result
      | None ->
        if List.mem datatype visiting then false else
          let constructors = Term.constructors t.terms datatype in
          dependencies := (datatype, constructors) :: !dependencies;
          let result = constructors <> []
            && List.for_all (fun constructor ->
              Array.for_all (visit (datatype :: visiting))
                (Term.Unsafe.signature t.terms constructor).arguments)
              constructors in
          Hashtbl.add known datatype result;
          result in
  let result = visit [] sort in
  result, !dependencies

let finite t sort =
  match sort with
  | Term.Bool | Term.Bitvec _ -> true
  | Term.Int -> false
  | Term.Named datatype ->
    let valid dependencies =
      List.for_all (fun (datatype, constructors) ->
        t.poll ();
        Term.constructors t.terms datatype == constructors) dependencies in
    match Int_table.find_opt t.finite_cache datatype with
    | Some (result, dependencies) when valid dependencies -> result
    | None | Some _ ->
      let result = finite_uncached t sort in
      Int_table.replace t.finite_cache datatype result;
      fst result

let constructor t term =
  match Term.Unsafe.node t.terms term with
  | Term.Apply (symbol, arguments) -> (
    match (Term.Unsafe.signature t.terms symbol).kind with
    | Term.Constructor _ -> Some (symbol, arguments)
    | Term.Uninterpreted | Term.Builtin _ | Term.Selector _ | Term.Tester _ ->
      None)
  | Term.Boolean _ | Term.Integer _ | Term.Bits _ -> None

let rec ensure t ~demanded term =
  t.poll ();
  match Term.sort t.terms term with
  | Term.Bool | Term.Int | Term.Bitvec _ -> ()
  | Term.Named datatype ->
    if Term.constructors t.terms datatype <> []
    then begin
      let demanded = demanded || finite t (Term.Named datatype) in
      let entry =
        match Int_table.find_opt t.entries term with
        | Some entry ->
          entry.demanded <- entry.demanded || demanded;
          entry
        | None ->
          let entry =
            { term;
              datatype;
              constructor = constructor t term;
              demanded;
              initialized = false;
              testers = [];
              expanded = Hashtbl.create 2
            }
          in
          Int_table.add t.entries term entry;
          entry
      in
      if (t.eager && entry.demanded) || Option.is_some entry.constructor
      then initialize t entry
    end

and initialize t (entry : entry) =
  if not entry.initialized
  then begin
    entry.initialized <- true;
    try
      let constructors = Term.constructors t.terms entry.datatype in
      let testers =
        List.map
          (fun constructor ->
            let tester = Term.tester t.terms constructor in
            constructor, boolean t (Term.apply t.terms tester [|entry.term|]))
          constructors
      in
      entry.testers <- testers;
      match entry.constructor with
      | Some (_, arguments) -> Array.iter (internalize t) arguments
      | None ->
        emit t (List.map snd testers);
        let rec exclude = function
          | [] -> ()
          | (_, first) :: rest ->
            List.iter (fun (_, second) -> emit t [-first; -second]) rest;
            exclude rest
        in
        exclude testers
    with exn ->
      entry.initialized <- false;
      entry.testers <- [];
      raise exn
  end

let enable_eager t =
  if not t.eager then begin
    t.eager <- true;
    let entries = entries t in
    List.iter (fun entry -> if entry.demanded then initialize t entry) entries
  end

let register_one t ~demanded term =
  ensure t ~demanded term;
  match Term.Unsafe.node t.terms term with
  | Term.Apply (symbol, arguments) -> (
    match (Term.Unsafe.signature t.terms symbol).kind with
    | Term.Selector (constructor, index) ->
      if Array.length arguments <> 1
      then invalid_arg "Vox_smt_datatype: malformed selector or tester";
      Int_table.replace t.selectors term
        (constructor, index, arguments.(0));
      ensure t ~demanded arguments.(0)
    | Term.Tester constructor ->
      if Array.length arguments <> 1
      then invalid_arg "Vox_smt_datatype: malformed selector or tester";
      Int_table.replace t.testers term (constructor, arguments.(0));
      ensure t ~demanded arguments.(0);
      Option.iter (initialize t) (Int_table.find_opt t.entries arguments.(0))
    | Term.Constructor _ ->
      Array.iter (ensure t ~demanded) arguments
    | Term.Uninterpreted | Term.Builtin _ -> ())
  | Term.Boolean _ | Term.Integer _ | Term.Bits _ -> ()

let register t term =
  if Term.has_datatypes t.terms then
  if t.generation <> 0 then register_one t ~demanded:false term
  else
    let rec observe term =
      if not (Int_table.mem t.observed term) then begin
        register_one t ~demanded:true term;
        (match Term.Unsafe.node t.terms term with
        | Term.Apply (_, arguments) -> Array.iter observe arguments
        | _ -> ());
        Int_table.add t.observed term ()
      end
    in
    observe term

let register_sort t = function
  | Term.Named datatype when Term.constructors t.terms datatype <> [] ->
    Int_table.replace t.declared_datatypes datatype ()
  | Term.Bool | Term.Int | Term.Bitvec _ | Term.Named _ -> ()

let productive t =
  let datatypes = Hashtbl.create 16 in
  let rec discover datatype =
    t.poll ();
    if not (Hashtbl.mem datatypes datatype)
    then begin
      Hashtbl.add datatypes datatype false;
      List.iter
        (fun constructor ->
          Array.iter
            (function
              | Term.Named child when Term.constructors t.terms child <> [] ->
                discover child
              | Term.Bool | Term.Int | Term.Bitvec _ | Term.Named _ -> ())
            (Term.Unsafe.signature t.terms constructor).arguments)
        (Term.constructors t.terms datatype)
    end
  in
  Int_table.iter (fun _ entry -> discover entry.datatype) t.entries;
  Int_table.iter (fun datatype () -> discover datatype) t.declared_datatypes;
  let inhabited = function
    | Term.Bool | Term.Int | Term.Bitvec _ -> true
    | Term.Named datatype ->
      Option.value (Hashtbl.find_opt datatypes datatype) ~default:true
  in
  let changed = ref true in
  while !changed do
    changed := false;
    Hashtbl.iter
      (fun datatype found ->
        t.poll ();
        if not found
           && List.exists
                (fun constructor ->
                  Array.for_all inhabited
                    (Term.Unsafe.signature t.terms constructor).arguments)
                (Term.constructors t.terms datatype)
        then begin
          Hashtbl.replace datatypes datatype true;
          changed := true
        end)
      datatypes
  done;
  Hashtbl.fold (fun _ inhabited all -> inhabited && all) datatypes true

let check_structure t ~representative entries =
  let constructed =
    List.fold_left
      (fun terms entry ->
        match entry.constructor with
        | None -> terms
        | Some (symbol, arguments) -> (entry.term, symbol, arguments) :: terms)
      [] entries
  in
  let classes = Hashtbl.create 32 in
  List.iter
    (fun (term, symbol, arguments) ->
      t.poll ();
      let class_ = representative term in
      match Hashtbl.find_opt classes class_ with
      | None -> Hashtbl.add classes class_ (term, symbol, arguments)
      | Some (other, other_symbol, other_arguments) ->
        let same = equal t term other in
        if symbol <> other_symbol
        then emit t [-same]
        else
          Array.iter2
            (fun argument other_argument ->
              let fields = equal t argument other_argument in
              emit t [-same; fields])
            arguments other_arguments)
    constructed;
  let selectors =
    Int_table.fold (fun term _ acc -> term :: acc) t.selectors [] in
  List.iter
    (fun selection ->
      t.poll ();
      let symbol, index, subject = Int_table.find t.selectors selection in
      match Hashtbl.find_opt classes (representative subject) with
      | Some (value, constructor, arguments) when symbol = constructor ->
        let same = equal t subject value in
        let field = equal t selection arguments.(index) in
        emit t [-same; field]
      | None | Some _ -> ())
    selectors;
  let testers = Int_table.fold (fun term _ acc -> term :: acc) t.testers [] in
  List.iter
    (fun test ->
      t.poll ();
      let symbol, subject = Int_table.find t.testers test in
      match Hashtbl.find_opt classes (representative subject) with
      | Some (value, constructor, _) ->
        let same = equal t subject value in
        let tested = boolean t test in
        emit t [-same; if symbol = constructor then tested else -tested]
      | None -> ())
    testers;
  let graph = Hashtbl.create 32 in
  List.iter
    (fun (term, _, arguments) ->
      t.poll ();
      let class_ = representative term in
      let edges = Option.value (Hashtbl.find_opt graph class_) ~default:[] in
      let edges = Array.fold_left
          (fun edges child ->
            match Term.sort t.terms child with
            | Term.Named datatype
              when Term.constructors t.terms datatype <> [] ->
              (term, child, representative child) :: edges
            | Term.Bool | Term.Int | Term.Bitvec _ | Term.Named _ -> edges)
          edges arguments in
      Hashtbl.replace graph class_ edges)
    constructed;
  let visited = Hashtbl.create 32 and active = Hashtbl.create 32 in
  let rec visit class_ path =
    t.poll ();
    if not (Hashtbl.mem visited class_)
    then begin
      Hashtbl.add active class_ ();
      let edges = Option.value (Hashtbl.find_opt graph class_) ~default:[] in
      List.iter
        (fun ((_, _, target) as edge) ->
          t.poll ();
          if Hashtbl.mem active target
          then begin
            let rec cycle edges = function
              | [] -> edges
              | ((parent, _, _) as previous) :: rest ->
                let edges = previous :: edges in
                if representative parent = target
                then edges
                else cycle edges rest
            in
            let edges =
              if target = class_ then [edge] else cycle [edge] path
            in
            let parents = List.map (fun (parent, _, _) -> parent) edges in
            let successors =
              match parents with
              | [] -> []
              | first :: rest -> rest @ [first]
            in
            let clause = List.map2
                (fun (_, child, _) parent ->
                  -(equal t child parent))
                edges successors in
            emit t clause
          end
          else visit target (edge :: path))
        edges;
      Hashtbl.remove active class_;
      Hashtbl.add visited class_ ()
    end
  in
  Hashtbl.iter (fun class_ _ -> visit class_ []) graph

let expand t entry symbol tester =
  if not (Hashtbl.mem entry.expanded symbol)
  then begin
    let signature = Term.Unsafe.signature t.terms symbol in
    let arguments =
      Array.mapi
        (fun index _ ->
          t.poll ();
          let selector = Term.selector t.terms symbol index in
          let selected =
            Term.apply t.terms selector [|entry.term|]
          in
          generated t (fun () ->
              ensure t ~demanded:false selected);
          internalize t selected;
          selected)
        signature.arguments
    in
    let constructed = Term.apply t.terms symbol arguments in
    generated t (fun () ->
        ensure t ~demanded:false constructed);
    internalize t constructed;
    let equation = Term.equal_atom t.terms entry.term constructed in
    emit t [-tester; boolean t equation];
    Hashtbl.add entry.expanded symbol ()
  end

let propagate t ~classes ~members ~constructor_members ~uses ~assigned
    ~explain ~imply ~conflict
    ~equate =
  enum_cliques t;
  match classes with
  | [] -> ()
  | _ ->
  let consequence left right literal =
    if assigned literal <> Some true then
      imply literal ~reason:(explain left right)
  in
  let field left right a b =
    match Term.sort t.terms a with
    | Term.Named _ | Term.Bool -> equate a b ~reason:(explain left right)
    | Term.Int | Term.Bitvec _ -> consequence left right (equal t a b)
  in
  List.iter (fun class_ ->
    match Term.sort t.terms class_ with
    | Term.Bool | Term.Int | Term.Bitvec _ -> ()
    | Term.Named datatype when Term.constructors t.terms datatype = [] -> ()
    | Term.Named _ ->
      let rec first = function
        | [] -> None
        | term :: rest ->
          match constructor t term with
          | None -> first rest
          | Some (symbol, fields) -> Some (term, symbol, fields, rest) in
      match first (constructor_members class_) with
      | None ->
        let entries = List.filter_map (Int_table.find_opt t.entries)
            (members class_) in
        if List.exists (fun entry -> entry.demanded) entries then begin
          let rec choose = function
            | [] -> ()
            | (entry : entry) :: rest ->
              if t.eager then initialize t entry;
              match List.find_opt (fun (_, lit) -> assigned lit = Some true)
                entry.testers with
              | Some (symbol, tester) -> expand t entry symbol tester
              | None -> choose rest
          in
          choose entries
        end
      | Some (value, symbol, fields, rest) ->
        List.iter (fun other ->
          match constructor t other with
          | None -> ()
          | Some (other_symbol, other_fields) ->
            t.poll ();
            if symbol <> other_symbol then
              conflict ~reason:(explain value other)
            else Array.iter2 (fun a b -> field value other a b)
              fields other_fields) rest;
        let applications = List.fold_left (fun applications member ->
          List.fold_left (fun applications term ->
            t.poll ();
            match Term.Unsafe.node t.terms term with
            | Term.Apply (operator, [|_|]) ->
              (match (Term.Unsafe.signature t.terms operator).kind with
              | Term.Selector (selected, _) when selected = symbol ->
                term :: applications
              | Term.Tester _ -> term :: applications
              | _ -> applications)
            | _ -> applications) applications (uses member))
          [] (members class_) |> List.sort_uniq Int.compare in
        List.iter (fun term ->
          t.poll ();
          match Term.Unsafe.node t.terms term with
          | Term.Apply (operator, [|subject|]) ->
            (match (Term.Unsafe.signature t.terms operator).kind with
            | Term.Selector (selected, index) when selected = symbol ->
              field subject value term fields.(index)
            | Term.Tester tested ->
              let literal = boolean t term in
              consequence subject value
                (if tested = symbol then literal else -literal)
            | _ -> ())
          | _ -> ()) applications) classes

let check t ~representative ~assigned =
  let started = t.revision in
  if not (productive t)
  then Incomplete "datatype has no finite inhabitant"
  else begin
    let entries = entries t in
    check_structure t ~representative entries;
    if t.revision <> started
    then Continue
    else begin
      let pending = ref false in
      let demanded_classes = Hashtbl.create 32 in
      let constructed_classes = Hashtbl.create 32 in
      List.iter
        (fun entry ->
          if entry.demanded
          then Hashtbl.replace demanded_classes (representative entry.term) ();
          if Option.is_some entry.constructor
          then Hashtbl.replace constructed_classes
                 (representative entry.term) ())
        entries;
      List.iter
        (fun (entry : entry) ->
          t.poll ();
          let class_ = representative entry.term in
          if (entry.demanded || Hashtbl.mem demanded_classes class_)
             && not (Hashtbl.mem constructed_classes class_)
          then begin
            initialize t entry;
            match entry.constructor with
            | Some _ -> ()
            | None -> (
              match List.find_opt
                      (fun (_, literal) -> assigned literal = Some true)
                      entry.testers with
              | None -> pending := true
              | Some (symbol, tester) ->
                expand t entry symbol tester;
                Hashtbl.replace constructed_classes class_ ())
          end)
        entries;
      if t.revision <> started || !pending then Continue else Done
    end
  end
