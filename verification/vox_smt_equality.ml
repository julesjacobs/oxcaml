module Term = Vox_smt_term
module IS = Set.Make (Int)

module Int_table = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal

  let hash x = x land max_int
end)

type reason =
  | Literal of int
  | Explained of (unit -> int array)
  | Congruence of Term.term array * Term.term array

type binding_kind =
  | Equality of Term.term * Term.term
  | Boolean of Term.term
  | Distinct of Term.term array

type binding =
  { literal : int;
    kind : binding_kind;
    mutable applied : bool;
    mutable queued : bool
  }

type disequality =
  { left : int;
    right : int;
    literal : int
  }

type distinct_group =
  { distinct_literal : int;
    distinct_roots : Term.term Int_table.t
  }

module Disequality_pairs = Hashtbl.Make (struct
  type t = int * int
  let equal (a, b) (c, d) = a = c && b = d
  let hash (a, b) = (a * 65599 lxor b) land max_int
end)

module Signatures = struct
  type entry = int
  let empty = -1
  type t =
    { mutable buckets : int array;
      mutable hashes : int array;
      mutable symbols : int array;
      mutable arguments : int array array;
      mutable terms : int array;
      mutable next : int array;
      mutable size : int }

  let create capacity =
    { buckets = Array.make capacity empty;
      hashes = Array.make capacity 0;
      symbols = Array.make capacity 0;
      arguments = Array.make capacity [||];
      terms = Array.make capacity 0;
      next = Array.make capacity empty;
      size = 0 }

  let rec hash_arguments parents arguments index hash =
    if index = Array.length arguments then hash land max_int else
      hash_arguments parents arguments (index + 1)
        (hash * 65599 lxor parents.(arguments.(index)))

  let hash parents symbol arguments =
    hash_arguments parents arguments 0 symbol

  let rec same parents arguments saved index =
    index = Array.length arguments
    || saved.(index) = parents.(arguments.(index))
       && same parents arguments saved (index + 1)

  let rec search table parents symbol arguments hash entry =
    if entry = empty then -1 else
    if table.hashes.(entry) = hash && table.symbols.(entry) = symbol
      && Array.length table.arguments.(entry) = Array.length arguments
      && same parents arguments table.arguments.(entry) 0
    then table.terms.(entry)
    else search table parents symbol arguments hash table.next.(entry)

  let find table parents symbol arguments hash =
    search table parents symbol arguments hash
      table.buckets.(hash land (Array.length table.buckets - 1))

  let resize table =
    let capacity = 2 * Array.length table.buckets in
    let buckets = Array.make capacity empty in
    for entry = 0 to table.size - 1 do
      let bucket = table.hashes.(entry) land (capacity - 1) in
      table.next.(entry) <- buckets.(bucket);
      buckets.(bucket) <- entry
    done;
    table.buckets <- buckets

  let grow table =
    let capacity = max 16 (2 * Array.length table.hashes) in
    let extend old initial =
      let values = Array.make capacity initial in
      Array.blit old 0 values 0 table.size;
      values in
    table.hashes <- extend table.hashes 0;
    table.symbols <- extend table.symbols 0;
    table.arguments <- extend table.arguments [||];
    table.terms <- extend table.terms 0;
    table.next <- extend table.next empty

  let add table parents symbol arguments hash term =
    if table.size >= 2 * Array.length table.buckets then resize table;
    if table.size = Array.length table.hashes then grow table;
    let bucket = hash land (Array.length table.buckets - 1) in
    let entry = table.size in
    let saved = table.arguments.(entry) in
    let saved =
      if Array.length saved = Array.length arguments then saved
      else Array.make (Array.length arguments) 0 in
    for index = 0 to Array.length saved - 1 do
      saved.(index) <- parents.(arguments.(index))
    done;
    table.hashes.(entry) <- hash;
    table.symbols.(entry) <- symbol;
    table.arguments.(entry) <- saved;
    table.terms.(entry) <- term;
    table.next.(entry) <- table.buckets.(bucket);
    table.buckets.(bucket) <- entry;
    table.size <- entry + 1;
    entry

  let remove table entry =
    if entry <> table.size - 1 then
      invalid_arg "Vox_smt_equality: signature rollback order";
    let bucket = table.hashes.(entry) land (Array.length table.buckets - 1) in
    if table.buckets.(bucket) <> entry then
      invalid_arg "Vox_smt_equality: signature bucket order";
    table.buckets.(bucket) <- table.next.(entry);
    table.size <- entry
end

module Bindings = Hashtbl.Make (struct
  type t = int * binding_kind

  let equal (l, x) (m, y) =
    l = m
    &&
    match x, y with
    | Equality (a, b), Equality (c, d) -> a = c && b = d
    | Boolean a, Boolean b -> a = b
    | Distinct a, Distinct b ->
      Array.length a = Array.length b && Array.for_all2 Int.equal a b
    | _ -> false

  let hash (literal, kind) =
    (match kind with
      | Equality (a, b) -> literal * 65599 lxor a * 65599 lxor b
      | Boolean a -> literal * 65599 lxor a
      | Distinct arguments -> literal * 65599 lxor Hashtbl.hash arguments)
    land max_int
end)

type proof =
  { position : int;
    parent : int;
    reason : reason option;
    previous : proof
  }

let rec no_proof =
  { position = 0; parent = -1; reason = None; previous = no_proof }

type node =
  { mutable size : int;
    mutable members : int list;
    mutable constructor_members : int list;
    mutable constant : int option;
    mutable disequalities : disequality list;
    mutable proof : proof;
    mutable uses : int list;
    mutable bindings : int list
  }

type undo =
  | Parents of int list * int
  | Class of int * int * int list * int list * int option * disequality list
  | Proof of int
  | Signature of Signatures.entry
  | Assignment
  | Applied of binding * bool
  | Disequalities of int * disequality list
  | Disequality_pair of (int * int)
  | Active_distinct of distinct_group list
  | Distinct_root of distinct_group * int

type merge =
  { left : int;
    right : int;
    reason : reason
  }

type marker =
  { undo_length : int;
    application_head : int;
    binding_head : int;
    offered_length : int;
    merges : merge list;
    conflict : int array option
  }

type 'a vector =
  { mutable data : 'a array;
    empty : 'a;
    mutable length : int
  }

let vector empty = { data = [||]; empty; length = 0 }

let append vector value =
  if vector.length = Array.length vector.data
  then begin
    let data = Array.make (max 16 (2 * vector.length)) vector.empty in
    Array.blit vector.data 0 data 0 vector.length;
    vector.data <- data
  end;
  vector.data.(vector.length) <- value;
  vector.length <- vector.length + 1

let get vector index =
  if index < 0 || index >= vector.length
  then invalid_arg "Vox_smt_equality: vector invariant";
  vector.data.(index)

let truncate vector length =
  Array.fill vector.data length (vector.length - length) vector.empty;
  vector.length <- length

type t =
  { terms : Term.t;
    congruence : Term.symbol -> bool;
    commutative : Term.symbol -> bool;
    poll : unit -> unit;
    mutable nodes : node option array;
    mutable parents : int array;
    mutable marks : int array;
    mutable expanded : int array;
    mutable stamp : int;
    mutable pure_uf : bool;
    signatures : Signatures.t;
    disequality_pairs : disequality Disequality_pairs.t;
    applications : int vector;
    bindings : binding vector;
    binding_ids : int Bindings.t;
    mutable literals : int list array;
    mutable assignments : int array;
    undo : undo vector;
    assignment_undo : int vector;
    offered : int vector;
    merges : merge Queue.t;
    pending : int Queue.t;
    mutable markers : marker list;
    mutable application_head : int;
    mutable binding_head : int;
    mutable revision : int;
    mutable changed : int list;
    mutable conflict : int array option;
    mutable active_distinct : distinct_group list
  }

type result =
  | Conflict of int array
  | Consistent of (int * int array) list

type lazy_result =
  | Lazy_conflict of int array
  | Lazy_consistent of (int * (unit -> int array)) list

let create ~terms ?(poll = fun () -> ())
    ?(congruence = fun _ -> true) ?(commutative = fun _ -> false) () =
  { terms; congruence; commutative;
    poll;
    nodes = [||];
    parents = [||];
    marks = [||];
    expanded = [||];
    stamp = 0;
    pure_uf = true;
    signatures = Signatures.create 128;
    disequality_pairs = Disequality_pairs.create 128;
    applications = vector (-1);
    bindings = vector
      { literal = 0; kind = Boolean (-1); applied = false; queued = false };
    binding_ids = Bindings.create 128;
    literals = Array.make 128 [];
    assignments = Array.make 128 0;
    undo = vector (Signature Signatures.empty);
    assignment_undo = vector 0;
    offered = vector (-1);
    merges = Queue.create ();
    pending = Queue.create ();
    markers = [];
    application_head = 0;
    binding_head = 0;
    revision = 0;
    changed = [];
    conflict = None;
    active_distinct = []
  }

let node t term =
  match t.nodes.(term) with
  | Some node -> node
  | None -> invalid_arg "Vox_smt_equality: unregistered term"

let find t term = t.parents.(term)

let next_stamp t =
  if t.stamp = max_int
  then begin
    Array.fill t.marks 0 (Array.length t.marks) 0;
    Array.fill t.expanded 0 (Array.length t.expanded) 0;
    t.stamp <- 0
  end;
  t.stamp <- t.stamp + 1;
  t.stamp

let revision t = t.revision

let is_registered t term =
  term >= 0 && term < Array.length t.nodes
  && Option.is_some t.nodes.(term)

let members t term = (node t (find t term)).members

let constructor_members t term = (node t (find t term)).constructor_members

let uses t term = (node t term).uses

let discard_changed t = t.changed <- []

let is_pure_uf t = t.pure_uf

let take_changed t =
  let changed = t.changed in
  t.changed <- [];
  let stamp = next_stamp t in
  let rec collect unique = function
    | [] -> unique
    | term :: rest ->
      let root = find t term in
      if t.marks.(root) = stamp then collect unique rest else begin
        t.marks.(root) <- stamp;
        collect (root :: unique) rest
      end in
  List.sort Int.compare (collect [] changed)

let snapshot t =
  let count = Term.count t.terms in
  if count <= Array.length t.parents then Array.sub t.parents 0 count else
  let result = Array.init count Fun.id in
  Array.blit t.parents 0 result 0 (Array.length t.parents);
  result

let rec register t term =
  if term < 0 || term >= Term.count t.terms
  then invalid_arg "Vox_smt_equality: invalid term";
  if term >= Array.length t.nodes
  then begin
    let nodes = Array.make (max (term + 1) (2 * Array.length t.nodes)) None in
    Array.blit t.nodes 0 nodes 0 (Array.length t.nodes);
    let parents = Array.init (Array.length nodes) Fun.id in
    Array.blit t.parents 0 parents 0 (Array.length t.parents);
    t.nodes <- nodes;
    t.parents <- parents;
    t.marks <- Array.make (Array.length nodes) 0;
    t.expanded <- Array.make (Array.length nodes) 0
  end;
  if t.nodes.(term) = None
  then begin
    t.poll ();
    t.revision <- t.revision + 1;
    if t.pure_uf then
      t.pure_uf <- (match Term.sort t.terms term with
        | Term.Bool -> true
        | Term.Named datatype -> Term.constructors t.terms datatype = []
        | Term.Int | Term.Bitvec _ -> false);
    let application, constant =
      match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, args) ->
        Array.iter (register t) args;
        t.congruence symbol, None
      | Term.Boolean _ | Term.Integer _ | Term.Bits _ -> false, Some term
    in
    t.nodes.(term)
      <- Some
           { size = 1;
             members = [term];
             constructor_members = (match Term.Unsafe.node t.terms term with
               | Term.Apply (symbol, _) ->
                 (match (Term.Unsafe.signature t.terms symbol).kind with
                 | Term.Constructor _ -> [term]
                 | _ -> [])
               | _ -> []);
             constant;
             disequalities = [];
             proof = no_proof;
             uses = [];
             bindings = []
           };
    t.changed <- term :: t.changed;
    if application
    then begin
      append t.applications term;
      match Term.Unsafe.node t.terms term with
      | Term.Apply (_, args) ->
        Array.iter
          (fun argument ->
            let n = node t argument in
            t.changed <- argument :: t.changed;
            n.uses <- term :: n.uses)
          args
      | _ -> ()
    end
  end

let representative t term =
  register t term;
  find t term

let mark t id =
  let binding = get t.bindings id in
  if not binding.queued
  then begin
    binding.queued <- true;
    Queue.add id t.pending
  end

let literal_bindings t literal =
  if literal < Array.length t.literals then t.literals.(literal) else []

let mark_literal t literal =
  List.iter (mark t) (literal_bindings t (abs literal))

let register_binding t literal kind terms =
  if literal = 0 || literal = min_int
  then invalid_arg "Vox_smt_equality: invalid literal";
  Array.iter (register t) terms;
  if not (Bindings.mem t.binding_ids (literal, kind))
  then begin
    let id = t.bindings.length in
    append t.bindings { literal; kind; applied = false; queued = false };
    Bindings.add t.binding_ids (literal, kind) id;
    let key = abs literal in
    if key >= Array.length t.literals then begin
      let capacity = max (key + 1) (2 * Array.length t.literals) in
      let bindings = Array.make capacity [] in
      Array.blit t.literals 0 bindings 0 (Array.length t.literals);
      t.literals <- bindings
    end;
    t.literals.(key) <- id :: t.literals.(key);
    Array.iter
      (fun term ->
        let n = node t term in
        n.bindings <- id :: n.bindings)
      terms;
    mark t id
  end

let register_equality t ~literal left right =
  if Term.sort t.terms left <> Term.sort t.terms right
  then invalid_arg "Vox_smt_equality: equality sorts";
  let left, right = if left < right then left, right else right, left in
  register_binding t literal (Equality (left, right)) [| left; right |]

let register_distinct t ~literal arguments =
  if literal = 0 || literal = min_int
  then invalid_arg "Vox_smt_equality: invalid literal";
  let arguments = Array.mapi (fun index term ->
    t.poll ();
    let sort = Term.sort t.terms term in
    (match sort with
    | Term.Named datatype when Term.constructors t.terms datatype = [] -> ()
    | _ -> invalid_arg "Vox_smt_equality: distinct uninterpreted sort");
    if index > 0 && sort <> Term.sort t.terms arguments.(0)
    then invalid_arg "Vox_smt_equality: distinct sorts";
    term) arguments in
  Array.iter (fun term -> t.poll (); register t term) arguments;
  register_binding t literal (Distinct arguments) [||]

let register_boolean t ~literal term =
  if Term.sort t.terms term <> Term.Bool
  then invalid_arg "Vox_smt_equality: Boolean sort required";
  register t (Term.boolean t.terms true);
  register t (Term.boolean t.terms false);
  register_binding t literal (Boolean term) [| term |]

let reason_pairs _t = function
  | Literal literal -> [literal], []
  | Explained why -> Array.to_list (why ()), []
  | Congruence (left, right) ->
    [], List.init (Array.length left) (fun i -> left.(i), right.(i))

let explain_pairs_at t position pairs literals =
  let proof term =
    let rec at proof =
      if proof.position <= position then proof
      else at proof.previous
    in
    at (node t term).proof
  in
  let literals =
    ref (List.fold_left (fun acc x -> IS.add x acc) IS.empty literals)
  in
  let pending = ref pairs in
  let expanded = next_stamp t in
  let add_edge term =
    if t.expanded.(term) <> expanded
    then begin
      t.expanded.(term) <- expanded;
      match (proof term).reason with
      | Some reason ->
        let direct, pairs = reason_pairs t reason in
        List.iter (fun x -> literals := IS.add x !literals) direct;
        pending := List.rev_append pairs !pending
      | None -> invalid_arg "Vox_smt_equality: missing proof edge"
    end
  in
  while !pending <> [] do
    t.poll ();
    match !pending with
    | [] -> ()
    | (left, right) :: rest ->
      pending := rest;
      if left <> right
      then begin
        let ancestors = next_stamp t in
        let current = ref left in
        while !current >= 0 do
          t.marks.(!current) <- ancestors;
          current := (proof !current).parent
        done;
        let ancestor = ref right in
        while !ancestor >= 0 && t.marks.(!ancestor) <> ancestors do
          ancestor := (proof !ancestor).parent
        done;
        if !ancestor < 0 then invalid_arg "Vox_smt_equality: unrelated proof";
        let path start =
          let current = ref start in
          while !current <> !ancestor do
            add_edge !current;
            current := (proof !current).parent
          done
        in
        path left;
        path right
      end
  done;
  Array.of_list (IS.elements !literals)

let explain_pairs t pairs literals =
  explain_pairs_at t t.undo.length pairs literals

let explain_later t left right =
  let position = t.undo.length in
  fun () -> explain_pairs_at t position [left, right] []

let explain t left right =
  register t left;
  register t right;
  if find t left <> find t right
  then None
  else Some (explain_pairs t [left, right] [])

let enqueue t left right reason =
  if find t left <> find t right
  then Queue.add { left; right; reason } t.merges

let equate t left right ~reason =
  if Term.sort t.terms left <> Term.sort t.terms right then
    invalid_arg "Vox_smt_equality.equate: sort mismatch";
  register t left;
  register t right;
  enqueue t left right (Explained reason)

let has_merges t = not (Queue.is_empty t.merges)

let propagates_disequalities t term =
  t.pure_uf && match Term.sort t.terms term with
  | Term.Named datatype -> Term.constructors t.terms datatype = []
  | Term.Bool | Term.Int | Term.Bitvec _ -> false

let index_disequality t (edge : disequality) =
  let left = find t edge.left and right = find t edge.right in
  let key = if left < right then left, right else right, left in
  if not (Disequality_pairs.mem t.disequality_pairs key) then begin
    append t.undo (Disequality_pair key);
    Disequality_pairs.add t.disequality_pairs key edge;
    true
  end else false

let mark_disequality t root other =
  let root, other = if (node t root).size <= (node t other).size
    then root, other else other, root in
  List.iter (fun term ->
    t.poll ();
    List.iter (fun id ->
      t.poll ();
      match (get t.bindings id).kind with
      | Equality (left, right) ->
        let a = find t left and b = find t right in
        if a = root && b = other || a = other && b = root then mark t id
      | Boolean _ | Distinct _ -> ()) (node t term).bindings)
    (node t root).members

let disequality_later t left right =
  if not (propagates_disequalities t left) then None else
  let a = find t left and b = find t right in
  let key = if a < b then a, b else b, a in
  match Disequality_pairs.find_opt t.disequality_pairs key with
  | None ->
    let rec find = function
      | [] -> None
      | group :: rest ->
        t.poll ();
        (match Int_table.find_opt group.distinct_roots a,
           Int_table.find_opt group.distinct_roots b with
        | Some x, Some y when a <> b ->
          let position = t.undo.length in
          Some (fun () -> explain_pairs_at t position
            [left, x; right, y] [group.distinct_literal])
        | _ -> find rest) in
    find t.active_distinct
  | Some edge ->
    let x, y = if find t edge.left = a then edge.left, edge.right
      else edge.right, edge.left in
    let position = t.undo.length in
    Some (fun () ->
      explain_pairs_at t position [left, x; right, y] [edge.literal])

let add_disequality t left right literal =
  if find t left = find t right
  then t.conflict <- Some (explain_pairs t [left, right] [literal])
  else begin
    let edge = { left; right; literal } in
    List.iter
      (fun root ->
        let n = node t root in
        append t.undo (Disequalities (root, n.disequalities));
        n.disequalities <- edge :: n.disequalities)
      [find t left; find t right];
    if propagates_disequalities t left && index_disequality t edge then begin
      mark_disequality t (find t left) (find t right)
    end
  end

let mark_distinct_class t root =
  if propagates_disequalities t root then
    List.iter (fun term ->
      t.poll ();
      List.iter (fun binding -> t.poll (); mark t binding)
        (node t term).bindings) (node t root).members

let activate_distinct t literal arguments =
  if Array.length arguments > 1 && t.conflict = None then begin
    let roots = Int_table.create (Array.length arguments) in
    let index = ref 0 in
    while !index < Array.length arguments && t.conflict = None do
      t.poll ();
      let term = arguments.(!index) in
      let root = find t term in
      (match Int_table.find_opt roots root with
      | Some previous ->
        t.conflict <- Some (explain_pairs t [term, previous] [literal])
      | None -> Int_table.add roots root term);
      incr index
    done;
    if t.conflict = None then begin
      let group = { distinct_literal = literal; distinct_roots = roots } in
      append t.undo (Active_distinct t.active_distinct);
      t.active_distinct <- group :: t.active_distinct;
      Array.iter (fun term ->
        t.poll (); mark_distinct_class t (find t term)) arguments
    end
  end

let rec transfer_distinct t small large = function
  | [] -> ()
  | _ when t.conflict <> None -> ()
  | group :: rest ->
    t.poll ();
    (match Int_table.find_opt group.distinct_roots small with
    | None -> ()
    | Some term ->
      (match Int_table.find_opt group.distinct_roots large with
      | Some previous ->
        t.conflict <- Some (explain_pairs t [term, previous]
          [group.distinct_literal])
      | None ->
        append t.undo (Distinct_root (group, large));
        Int_table.add group.distinct_roots large term;
        mark_distinct_class t large));
    transfer_distinct t small large rest

let apply_binding t (binding : binding) asserted =
  if not binding.applied
  then begin
    append t.undo (Applied (binding, false));
    binding.applied <- true;
    match binding.kind with
    | Equality (left, right) ->
      if asserted = binding.literal
      then enqueue t left right (Literal asserted)
      else add_disequality t left right asserted
    | Boolean term ->
      (match Term.Unsafe.node t.terms term with
      | Term.Apply (symbol, [| subject |]) -> (
        match (Term.Unsafe.signature t.terms symbol).kind with
        | Term.Tester _ -> t.changed <- subject :: t.changed
        | _ -> ())
      | _ -> ());
      let constant = Term.boolean t.terms (asserted = binding.literal) in
      enqueue t term constant (Literal asserted)
    | Distinct arguments ->
      if asserted = binding.literal then
        activate_distinct t asserted arguments
  end

let assignment t literal =
  if literal >= Array.length t.assignments then None
  else match t.assignments.(literal) with 0 -> None | value -> Some value

let assert_literal t literal =
  if literal = 0 || literal = min_int
  then invalid_arg "Vox_smt_equality: invalid asserted literal";
  let key = abs literal in
  match assignment t key with
  | Some old when old <> literal -> t.conflict <- Some [| old; literal |]
  | Some _ -> ()
  | None ->
    append t.assignment_undo key;
    append t.undo Assignment;
    if key >= Array.length t.assignments then begin
      let capacity = max (key + 1) (2 * Array.length t.assignments) in
      let values = Array.make capacity 0 in
      Array.blit t.assignments 0 values 0 (Array.length t.assignments);
      t.assignments <- values
    end;
    t.assignments.(key) <- literal;
    List.iter (fun id -> apply_binding t (get t.bindings id) literal)
      (literal_bindings t key)

let ordered_arguments t symbol arguments =
  let length = Array.length arguments in
  if length <= 1 || not (t.commutative symbol) then arguments
  else if length = 2 then begin
    t.poll ();
    let left = arguments.(0) and right = arguments.(1) in
    if t.parents.(left) <= t.parents.(right) then arguments
    else [|right; left|]
  end else
    let comparisons = ref 0 in
    let compare left right =
      if !comparisons land 31 = 0 then t.poll ();
      incr comparisons;
      Int.compare t.parents.(left) t.parents.(right) in
    let rec ordered index =
      index >= Array.length arguments
      || compare arguments.(index - 1) arguments.(index) <= 0
         && ordered (index + 1) in
    if ordered 1 then arguments else begin
      let arguments = Array.copy arguments in
      Array.sort compare arguments;
      arguments
    end

let add_signature t term =
  match Term.Unsafe.node t.terms term with
  | Term.Apply (symbol, arguments) ->
    let arguments = ordered_arguments t symbol arguments in
    let hash = Signatures.hash t.parents symbol arguments in
    let other = Signatures.find t.signatures t.parents symbol arguments hash in
    if other >= 0 then begin
      if term <> other && find t term <> find t other then begin
        (* Preserve the pairing across later representative changes. *)
        let prior = match Term.Unsafe.node t.terms other with
          | Term.Apply (_, arguments) -> ordered_arguments t symbol arguments
          | _ -> invalid_arg "Vox_smt_equality: congruence application" in
        enqueue t term other (Congruence (arguments, prior))
      end
    end else begin
      let entry = Signatures.add t.signatures t.parents symbol arguments
          hash term in
      append t.undo (Signature entry)
    end
  | _ -> invalid_arg "Vox_smt_equality: application required"

let set_proof t term parent reason =
  let n = node t term in
  append t.undo (Proof term);
  n.proof <-
    { position = t.undo.length; parent; reason; previous = n.proof }

let reroot t term =
  let current = ref (if (node t term).proof.parent < 0 then -1 else term) in
  let previous = ref (-1) and reason = ref None in
  while !current >= 0 do
    let n = node t !current in
    let next = n.proof.parent and next_reason = n.proof.reason in
    set_proof t !current !previous !reason;
    previous := !current;
    reason := next_reason;
    current := next
  done

let rec affected_uses t stamp affected = function
  | [] -> affected
  | parent :: rest ->
    let affected =
      if t.marks.(parent) = stamp then affected
      else begin
        t.marks.(parent) <- stamp;
        parent :: affected
      end
    in
    affected_uses t stamp affected rest

let rec affected_members t stamp affected = function
  | [] -> affected
  | term :: rest ->
    let affected = affected_uses t stamp affected (node t term).uses in
    affected_members t stamp affected rest

let rec set_parents t parent = function
  | [] -> ()
  | member :: rest ->
    t.parents.(member) <- parent;
    set_parents t parent rest

let rec mark_bindings t = function
  | [] -> ()
  | binding :: rest -> mark t binding; mark_bindings t rest

let rec mark_members t = function
  | [] -> ()
  | term :: rest ->
    mark_bindings t (node t term).bindings;
    mark_members t rest

let merge t work =
  let left_root = find t work.left and right_root = find t work.right in
  if left_root <> right_root
  then begin
    let left_root, right_root, left, right =
      if (node t left_root).size <= (node t right_root).size
      then left_root, right_root, work.left, work.right
      else right_root, left_root, work.right, work.left
    in
    let small = node t left_root and large = node t right_root in
    match small.constant, large.constant with
    | Some a, Some b when a <> b ->
      let direct, pairs = reason_pairs t work.reason in
      t.conflict
        <- Some (explain_pairs t ((left, a) :: (right, b) :: pairs) direct)
    | _ ->
      let stamp = next_stamp t in
      let affected = affected_members t stamp [] small.members in
      let affected = List.sort Int.compare affected in
      (* Old signatures mention the inactive root until rollback. *)
      append t.undo (Parents (small.members, left_root));
      append t.undo
        (Class
           ( right_root,
             large.size,
             large.members,
             large.constructor_members,
             large.constant,
             large.disequalities ));
      set_parents t right_root small.members;
      t.revision <- t.revision + 1;
      t.changed <- right_root :: t.changed;
      large.size <- large.size + small.size;
      large.members <- List.rev_append small.members large.members;
      large.constructor_members <-
        List.rev_append small.constructor_members large.constructor_members;
      large.disequalities
        <- List.rev_append small.disequalities large.disequalities;
      let gains_constant = large.constant = None && small.constant <> None in
      if large.constant = None then large.constant <- small.constant;
      reroot t left;
      set_proof t left right (Some work.reason);
      transfer_distinct t left_root right_root t.active_distinct;
      mark_members t
        (if gains_constant then large.members else small.members);
      List.iter
        (fun (edge : disequality) ->
          if t.conflict = None && find t edge.left = find t edge.right
          then
            t.conflict
              <- Some (explain_pairs t [edge.left, edge.right] [edge.literal]))
        small.disequalities;
      if propagates_disequalities t right_root then begin
        List.iter
          (fun (edge : disequality) ->
            t.poll ();
            let left = find t edge.left and right = find t edge.right in
            if left <> right && index_disequality t edge then
              mark_disequality t
                (if left = right_root then right else left) right_root)
          small.disequalities
      end;
      List.iter (add_signature t) affected
  end

let push t =
  t.markers
    <- { undo_length = t.undo.length;
         application_head = t.application_head;
         binding_head = t.binding_head;
         offered_length = t.offered.length;
         merges = List.of_seq (Queue.to_seq t.merges);
         conflict = t.conflict
       }
       :: t.markers

let undo_to t length =
  for i = t.undo.length - 1 downto length do
    match get t.undo i with
    | Parents (members, parent) ->
      t.revision <- t.revision + 1;
      (match members with
      | member :: _ -> t.changed <- t.parents.(member) :: parent :: t.changed
      | [] -> ());
      set_parents t parent members
    | Class (term, size, members, constructors, constant, disequalities) ->
      let n = node t term in
      n.size <- size;
      n.members <- members;
      n.constructor_members <- constructors;
      n.constant <- constant;
      n.disequalities <- disequalities
    | Proof term ->
      let n = node t term in
      n.proof <- n.proof.previous
    | Signature key -> Signatures.remove t.signatures key
    | Assignment ->
      let index = t.assignment_undo.length - 1 in
      let literal = get t.assignment_undo index in
      t.assignment_undo.length <- index;
      t.assignments.(literal) <- 0;
      mark_literal t literal
    | Applied (binding, applied) -> binding.applied <- applied
    | Disequalities (term, old) -> (node t term).disequalities <- old
    | Disequality_pair key -> Disequality_pairs.remove t.disequality_pairs key
    | Active_distinct groups -> t.active_distinct <- groups
    | Distinct_root (group, root) ->
      Int_table.remove group.distinct_roots root
  done;
  truncate t.undo length

let pop t =
  match t.markers with
  | [] -> invalid_arg "Vox_smt_equality.pop: no level"
  | marker :: rest ->
    undo_to t marker.undo_length;
    t.markers <- rest;
    t.application_head <- min t.application_head marker.application_head;
    t.binding_head <- min t.binding_head marker.binding_head;
    for i = marker.offered_length to t.offered.length - 1 do
      mark t (get t.offered i)
    done;
    truncate t.offered marker.offered_length;
    Queue.clear t.merges;
    List.iter (fun work -> Queue.add work t.merges) marker.merges;
    t.conflict <- marker.conflict

let reset t =
  t.revision <- t.revision + 1;
  undo_to t 0;
  t.markers <- [];
  t.application_head <- 0;
  t.binding_head <- 0;
  truncate t.offered 0;
  Queue.clear t.merges;
  while not (Queue.is_empty t.pending) do
    (get t.bindings (Queue.take t.pending)).queued <- false
  done;
  t.conflict <- None

let check_lazy t =
  while t.application_head < t.applications.length do
    t.poll ();
    add_signature t (get t.applications t.application_head);
    t.application_head <- t.application_head + 1
  done;
  while t.binding_head < t.bindings.length do
    t.poll ();
    let id = t.binding_head in
    let binding = get t.bindings id in
    Option.iter (apply_binding t binding)
      (assignment t (abs binding.literal));
    mark t id;
    t.binding_head <- t.binding_head + 1
  done;
  while t.conflict = None && not (Queue.is_empty t.merges) do
    t.poll ();
    merge t (Queue.take t.merges)
  done;
  if Queue.is_empty t.pending then
    match t.conflict with
    | Some reason -> Lazy_conflict reason
    | None -> Lazy_consistent []
  else begin
    let propagations = ref [] in
    let offered = lazy (Int_table.create 16) in
    let offer id literal reason =
      append t.offered id;
      let offered = Lazy.force offered in
      match Int_table.find_opt offered (-literal) with
      | Some other ->
        t.conflict <- Some (Array.append (reason ()) (other ()))
      | None ->
        if not (Int_table.mem offered literal) then begin
          Int_table.add offered literal reason;
          propagations := (literal, reason) :: !propagations
        end in
    while t.conflict = None && not (Queue.is_empty t.pending) do
      t.poll ();
      let id = Queue.take t.pending in
      let binding = get t.bindings id in
      binding.queued <- false;
      if not (Option.is_some (assignment t (abs binding.literal)))
      then match binding.kind with
      | Equality (left, right) when find t left = find t right ->
        offer id binding.literal (explain_later t left right)
      | Boolean term ->
        (match (node t (find t term)).constant with
        | None -> ()
        | Some constant ->
          (match Term.Unsafe.node t.terms constant with
          | Term.Boolean truth ->
            offer id (if truth then binding.literal else -binding.literal)
              (explain_later t term constant)
          | _ -> ()))
      | Equality (left, right) when t.pure_uf ->
        (match disequality_later t left right with
        | Some reason -> offer id (-binding.literal) reason
        | None -> ())
      | Equality _ | Distinct _ -> ()
    done;
    match t.conflict with
    | Some reason -> Lazy_conflict reason
    | None -> Lazy_consistent (List.rev !propagations)
  end

let check t =
  match check_lazy t with
  | Lazy_conflict reason -> Conflict reason
  | Lazy_consistent propagations ->
    Consistent
      (List.map (fun (literal, reason) -> literal, reason ()) propagations)
