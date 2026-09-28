open Hm_declarative
open Hm_type_proofs
open Hm_abstraction

let rec (add_assoc @ total) : (a : index) @ immutable -> (b : index) @ immutable ->
    (c : index) @ immutable -> {u : unit | add (add a b) c === add a (add b c)} @ ghost =
  fun a b c -> ghost_ (
    add_def a b; let ab = add a b in add_def ab c;
    let bc = add b c in add_def a bc;
    (match a with Z -> () | S a -> add_assoc a b c; ()); ())

let rec (position_bound @ total) : (ps : names) @ immutable ->
    (p : Copy_spec.node Pref.t) @ immutable -> (i : index) @ immutable ->
    {u : unit | position ps p === Some i} ->
    {u : unit | present (count ps) i} @ ghost = fun ps p i premise -> ghost_ (
    position_def ps p; count_def ps;
    let n = count ps in present_def n i; match ps with
    | No_names -> ()
    | Name (q, rest) -> if p === q then () else
      match position rest p with None -> ()
      | Some j -> position_bound rest p j (); ())

let rec (present_suffix @ total) : (k : index) @ immutable -> (n : index) @ immutable ->
    (i : index) @ immutable -> {u : unit | present k i} ->
    {u : unit | present (add k n) i} @ ghost = fun k n i premise -> ghost_ (
    present_def k i; add_def k n;
    let total = add k n in present_def total i; match k with
    | Z -> () | S k -> match i with Z -> ()
      | S i -> present_suffix k n i (); ())

let (abstract_free_wf @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (base : index) @ immutable ->
    (p : Copy_spec.node Pref.t) @ immutable ->
    {u : unit | mono_wf (add cut (add (count ps) base)) (abstract_free ps cut p)} @ ghost =
  fun ps cut base p -> ghost_ (
    abstract_free_def ps cut p; let n = count ps in let inside = add n base in
    let total = add cut inside in let t = abstract_free ps cut p in mono_wf_def total t;
    match position ps p with None -> ()
    | Some i -> position_bound ps p i (); present_suffix n base i ();
      present_added cut inside i (); ())

let rec (abstract_wf @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (base : index) @ immutable -> (t : mono) @ immutable ->
    {u : unit | mono_wf (add cut base) t} ->
    {u : unit | mono_wf (add cut (add (count ps) base)) (abstract_type ps cut t)} @ ghost =
  fun ps cut base t premise -> ghost_ (
    let old = add cut base in mono_wf_def old t;
    abstract_type_def ps cut t; let n = count ps in let total = add cut (add n base) in
    let changed = abstract_type ps cut t in mono_wf_def total changed;
    match t with
    | Parameter i -> shift_index_wf cut n base i (); ()
    | Free p -> abstract_free_wf ps cut base p; ()
    | Boolean | Word64 -> ()
    | List_type a -> abstract_wf ps cut base a (); ()
    | Function (a, b) -> abstract_wf ps cut base a ();
      abstract_wf ps cut base b (); ())

let (abstract_scheme_wf @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (base : index) @ immutable -> (s : scheme) @ immutable ->
    {u : unit | scheme_wf (add cut base) s} ->
    {u : unit | scheme_wf (add cut (add (count ps) base)) (abstract_scheme ps cut s)} @ ghost =
  fun ps cut base s premise -> ghost_ (
    let old = add cut base in scheme_wf_def old s;
    abstract_scheme_def ps cut s; let inside = add (count ps) base in let total = add cut inside in
    let changed = abstract_scheme ps cut s in scheme_wf_def total changed;
    match s with Forall (k, t) -> add_assoc k cut base; add_assoc k cut inside;
      let next = add k cut in abstract_wf ps next base t (); ())

let rec (abstract_context_wf @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (base : index) @ immutable -> (g : context) @ immutable ->
    {u : unit | context_wf (add cut base) g} ->
    {u : unit | context_wf (add cut (add (count ps) base)) (abstract_context ps cut g)} @ ghost =
  fun ps cut base g premise -> ghost_ (
    let old = add cut base in context_wf_def old g;
    abstract_context_def ps cut g; let total = add cut (add (count ps) base) in
    let changed = abstract_context ps cut g in context_wf_def total changed;
    match g with Empty_context -> ()
    | Binding (s, rest) -> abstract_scheme_wf ps cut base s ();
      abstract_context_wf ps cut base rest (); ())

let rec (abstract_arguments_length @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (args : arguments) @ immutable ->
    {u : unit | length (abstract_arguments ps cut args) === length args} @ ghost =
  fun ps cut args -> ghost_ (
    abstract_arguments_def ps cut args; length_def args;
    let changed = abstract_arguments ps cut args in length_def changed;
    (match args with No_arguments -> () | Argument (_, rest) ->
      abstract_arguments_length ps cut rest; ()); ())

let rec (open_index_skip @ total) : (args : arguments) @ immutable -> (i : index) @ immutable ->
    {u : unit | open_index args (add (length args) i) === Parameter i} @ ghost =
  fun args i -> ghost_ (
    length_def args; let k = length args in add_def k i;
    let j = add k i in open_index_def args j;
    (match args with No_arguments -> () | Argument (_, rest) -> open_index_skip rest i; ());
    ())

let rec (abstract_open_index @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (args : arguments) @ immutable -> (i : index) @ immutable ->
    {u : unit | abstract_type ps cut (open_index args i) ===
      open_index (abstract_arguments ps cut args)
        (shift_index (add (length args) cut) (count ps) i)} @ ghost =
  fun ps cut args i -> ghost_ (
    open_index_def args i; abstract_arguments_def ps cut args; length_def args;
    let n = length args in add_def n cut; let scope = add n cut in
    let k = count ps in shift_index_def scope k i;
    let changed = abstract_arguments ps cut args in
    let j = shift_index scope k i in open_index_def changed j;
    let opened = open_index args i in abstract_type_def ps cut opened;
    (match args with No_arguments -> () | Argument (_, rest) -> match i with
    | Z -> () | S i -> abstract_open_index ps cut rest i; ()); ())

let (abstract_open_free @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (args : arguments) @ immutable ->
    (p : Copy_spec.node Pref.t) @ immutable ->
    {u : unit | abstract_free ps cut p ===
      open_type (abstract_arguments ps cut args) (abstract_free ps (add (length args) cut) p)} @ ghost =
  fun ps cut args p -> ghost_ (
    abstract_free_def ps cut p; let k = length args in let scope = add k cut in
    abstract_free_def ps scope p; let changed = abstract_arguments ps cut args in
    let t = abstract_free ps scope p in open_type_def changed t;
    abstract_arguments_length ps cut args;
    (match position ps p with None -> () | Some i ->
      add_assoc k cut i; let j = add cut i in open_index_skip changed j; ());
    ())

let rec (abstract_open @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (args : arguments) @ immutable -> (t : mono) @ immutable ->
    {u : unit | abstract_type ps cut (open_type args t) ===
      open_type (abstract_arguments ps cut args) (abstract_type ps (add (length args) cut) t)} @ ghost =
  fun ps cut args t -> ghost_ (
    open_type_def args t; let scope = add (length args) cut in abstract_type_def ps scope t;
    let opened = open_type args t in abstract_type_def ps cut opened;
    let changed = abstract_arguments ps cut args in let body = abstract_type ps scope t in
    open_type_def changed body;
    (match t with
    | Parameter i -> abstract_open_index ps cut args i; ()
    | Free p -> abstract_open_free ps cut args p; ()
    | Boolean | Word64 -> ()
    | List_type a -> abstract_open ps cut args a; ()
    | Function (a, b) -> abstract_open ps cut args a; abstract_open ps cut args b; ());
    ())

let rec (shift_added @ total) : (a : index) @ immutable -> (cut : index) @ immutable ->
    (k : index) @ immutable -> (i : index) @ immutable ->
    {u : unit | shift_index (add a cut) k (add a i) === add a (shift_index cut k i)} @ ghost =
  fun a cut k i -> ghost_ (
    add_def a cut; add_def a i; let scope = add a cut in let j = add a i in
    shift_index_def scope k j; let shifted = shift_index cut k i in add_def a shifted;
    (match a with Z -> () | S a -> shift_added a cut k i; ()); ())

let rec (shift_commute @ total) : (local : index) @ immutable -> (cut : index) @ immutable ->
    (k : index) @ immutable -> (m : index) @ immutable -> (i : index) @ immutable ->
    {u : unit | shift_index (add local (add k cut)) m (shift_index local k i)
      === shift_index local k (shift_index (add local cut) m i)} @ ghost =
  fun local cut k m i -> ghost_ (
    let inside = add k cut in add_def local inside; add_def local cut;
    shift_index_def local k i;
    let big = add local inside in let shifted = shift_index local k i in
    shift_index_def big m shifted;
    let small = add local cut in shift_index_def small m i;
    let changed = shift_index small m i in shift_index_def local k changed;
    (match local with Z -> shift_added k cut m i; ()
    | S local -> match i with Z -> () | S i -> shift_commute local cut k m i; ());
    ())

let rec (shift_free_index @ total) : (local : index) @ immutable ->
    (cut : index) @ immutable -> (k : index) @ immutable -> (i : index) @ immutable ->
    {u : unit | add (add local (add k cut)) i === shift_index local k (add (add local cut) i)} @ ghost =
  fun local cut k i -> ghost_ (
    let inside = add k cut in add_def local inside; add_def local cut;
    let big = add local inside in add_def big i;
    let small = add local cut in add_def small i;
    let j = add small i in shift_index_def local k j;
    (match local with Z -> add_assoc k cut i; ()
    | S local -> shift_free_index local cut k i; ()); ())

let rec (abstract_shift @ total) : (ps : names) @ immutable ->
    (local : index) @ immutable -> (cut : index) @ immutable ->
    (k : index) @ immutable -> (t : mono) @ immutable ->
    {u : unit | abstract_type ps (add local (add k cut)) (shift local k t)
      === shift local k (abstract_type ps (add local cut) t)} @ ghost =
  fun ps local cut k t -> ghost_ (
    shift_def local k t; let big = add local (add k cut) in let small = add local cut in
    abstract_type_def ps small t; let shifted = shift local k t in abstract_type_def ps big shifted;
    let changed = abstract_type ps small t in shift_def local k changed;
    (match t with
    | Parameter i -> let m = count ps in shift_commute local cut k m i; ()
    | Free p -> abstract_free_def ps big p; abstract_free_def ps small p;
      let v = abstract_free ps small p in shift_def local k v;
      (match position ps p with None -> () | Some i -> shift_free_index local cut k i; ())
    | Boolean | Word64 -> ()
    | List_type a -> abstract_shift ps local cut k a; ()
    | Function (a, b) -> abstract_shift ps local cut k a; abstract_shift ps local cut k b; ());
    ())

let (abstract_weaken_scheme @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (k : index) @ immutable -> (s : scheme) @ immutable ->
    {u : unit | abstract_scheme ps (add k cut) (weaken_scheme k s)
      === weaken_scheme k (abstract_scheme ps cut s)} @ ghost = fun ps cut k s -> ghost_ (
    weaken_scheme_def k s; abstract_scheme_def ps cut s;
    let next = add k cut in let shifted = weaken_scheme k s in abstract_scheme_def ps next shifted;
    let changed = abstract_scheme ps cut s in weaken_scheme_def k changed;
    (match s with Forall (local, t) -> abstract_shift ps local cut k t; ()); ())

let rec (abstract_weaken_context @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (k : index) @ immutable -> (g : context) @ immutable ->
    {u : unit | abstract_context ps (add k cut) (weaken_context k g)
      === weaken_context k (abstract_context ps cut g)} @ ghost = fun ps cut k g -> ghost_ (
    weaken_context_def k g; abstract_context_def ps cut g;
    let next = add k cut in let shifted = weaken_context k g in abstract_context_def ps next shifted;
    let changed = abstract_context ps cut g in weaken_context_def k changed;
    (match g with Empty_context -> () | Binding (s, rest) -> abstract_weaken_scheme ps cut k s;
      abstract_weaken_context ps cut k rest; ()); ())

let rec (abstract_arguments_wf @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (base : index) @ immutable -> (args : arguments) @ immutable ->
    {u : unit | arguments_wf (add cut base) args} ->
    {u : unit | arguments_wf (add cut (add (count ps) base)) (abstract_arguments ps cut args)} @ ghost =
  fun ps cut base args premise -> ghost_ (
    let old = add cut base in arguments_wf_def old args;
    abstract_arguments_def ps cut args; let total = add cut (add (count ps) base) in
    let changed = abstract_arguments ps cut args in arguments_wf_def total changed;
    match args with No_arguments -> ()
    | Argument (a, rest) -> abstract_wf ps cut base a ();
      abstract_arguments_wf ps cut base rest (); ())

let rec (abstract_lookup @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (g : context) @ immutable -> (i : index) @ immutable ->
    {u : unit | match lookup g i with None -> lookup (abstract_context ps cut g) i === None
      | Some s -> lookup (abstract_context ps cut g) i === Some (abstract_scheme ps cut s)} @ ghost =
  fun ps cut g i -> ghost_ (
    lookup_def g i; abstract_context_def ps cut g;
    let changed = abstract_context ps cut g in lookup_def changed i;
    (match g with Empty_context -> () | Binding (_, rest) -> match i with
      Z -> () | S i -> abstract_lookup ps cut rest i; ()); ())

let rec (abstraction_typed @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (base : index) @ immutable ->
    (g : context) @ immutable -> (e : term) @ immutable ->
    (t : mono) @ immutable -> (d : typing) @ immutable ->
    {u : unit | typed (add cut base) g e t d} ->
    {u : unit | typed (add cut (add (count ps) base))
      (abstract_context ps cut g) e (abstract_type ps cut t) (abstract_typing ps cut d)} @ ghost =
  fun ps cut base g e t d premise -> ghost_ (
    let old = add cut base in typed_def old g e t d;
    abstract_type_def ps cut t; abstract_typing_def ps cut d;
    let total = add cut (add (count ps) base) in
    let changed_g = abstract_context ps cut g in let changed_t = abstract_type ps cut t in
    let changed_d = abstract_typing ps cut d in typed_def total changed_g e changed_t changed_d;
    abstract_wf ps cut base t ();
    abstract_context_wf ps cut base g ();
    match d with
    | Variable args -> (match e with Bound i -> (match lookup g i with
      | None -> ()
      | Some s ->
        abstract_lookup ps cut g i; abstract_arguments_wf ps cut base args ();
        abstract_arguments_length ps cut args;
        abstract_scheme_def ps cut s; let changed_s = abstract_scheme ps cut s in
        arity_def s; arity_def changed_s;
        open_scheme_def s args; let changed_args = abstract_arguments ps cut args in
        open_scheme_def changed_s changed_args;
        (match s with Forall (_, body) -> abstract_open ps cut args body; ()); ())
      | _ -> ())
    | Constant | Word_constant -> ()
    | Empty_list a -> abstract_wf ps cut base a (); ()
    | List_cons (a, head, tail) -> (match e with Cons (h, r) ->
      abstraction_typed ps cut base g h a head ();
      abstraction_typed ps cut base g r t tail (); () | _ -> ())
    | List_case (a, scrutinee, empty, nonempty) -> (match e with CaseList (s, l, r) ->
      abstract_wf ps cut base a ();
      let list = List_type a in abstract_type_def ps cut list;
      abstraction_typed ps cut base g s list scrutinee ();
      abstraction_typed ps cut base g l t empty ();
      let z = Z in add_def z cut;
      let hs = Forall (z, a) in let ts = Forall (z, list) in
      let tail = Binding (ts, g) in let both = Binding (hs, tail) in
      abstract_scheme_def ps cut hs; abstract_scheme_def ps cut ts;
      abstract_context_def ps cut tail; abstract_context_def ps cut both;
      abstraction_typed ps cut base both r t nonempty (); () | _ -> ())
    | Conditional (condition, yes, no) -> (match e with If (c, a, b) ->
      let bool = Boolean in abstract_type_def ps cut bool;
      abstraction_typed ps cut base g c bool condition ();
      abstraction_typed ps cut base g a t yes ();
      abstraction_typed ps cut base g b t no (); () | _ -> ())
    | Word_primitive (left, right) -> (match e with Primitive (op, a, b) ->
      let word = Word64 in abstract_type_def ps cut word;
      operation_type_def op; (match op with Add | Subtract -> () | Equal_word | Unsigned_less -> ());
      abstraction_typed ps cut base g a word left ();
      abstraction_typed ps cut base g b word right (); () | _ -> ())
    | Abstraction (a, body) -> (match e, t with
      | Lambda e, Function (_, b) ->
        let z = Z in add_def z cut; let arg_scheme = Forall (z, a) in
        let next = Binding (arg_scheme, g) in
        abstract_scheme_def ps cut arg_scheme; abstract_context_def ps cut next;
        abstraction_typed ps cut base next e b body (); ()
      | _ -> ())
    | Application (a, left, right) -> (match e with Apply (f, x) ->
        let ft = Function (a, t) in abstract_type_def ps cut ft;
        abstraction_typed ps cut base g f ft left ();
        abstraction_typed ps cut base g x a right (); ()
      | _ -> ())
    | Recursion (a, b, body) -> (match e with Recursive e ->
        let z = Z in add_def z cut; let arg_scheme = Forall (z, a) in let self_scheme = Forall (z, t) in
        let self = Binding (self_scheme, g) in let next = Binding (arg_scheme, self) in
        abstract_scheme_def ps cut arg_scheme; abstract_scheme_def ps cut self_scheme;
        abstract_context_def ps cut self; abstract_context_def ps cut next;
        abstraction_typed ps cut base next e b body (); ()
      | _ -> ())
    | Let_binding (s, rhs, body) -> (match e, s with Let (r, b), Forall (k, a) ->
        abstract_scheme_def ps cut s; abstract_scheme_wf ps cut base s ();
        abstract_weaken_context ps cut k g;
        let shifted = weaken_context k g in let next_cut = add k cut in
        let inside = add (count ps) base in add_assoc k cut base; add_assoc k cut inside;
        abstraction_typed ps next_cut base shifted r a rhs ();
        let next = Binding (s, g) in abstract_context_def ps cut next;
        abstraction_typed ps cut base next b t body (); ()
      | _ -> ()))

let rec (abstract_avoids @ total) : (ps : names) @ immutable ->
    (cut : index) @ immutable -> (t : mono) @ immutable -> {u : unit | avoids ps t} ->
    {u : unit | abstract_type ps cut t === shift cut (count ps) t} @ ghost =
  fun ps cut t premise -> ghost_ (
    avoids_def ps t; abstract_type_def ps cut t;
    let n = count ps in shift_def cut n t;
    match t with Parameter _ | Boolean | Word64 -> ()
    | Free p -> abstract_free_def ps cut p; ()
    | List_type a -> abstract_avoids ps cut a (); ()
    | Function (a, b) -> abstract_avoids ps cut a ();
      abstract_avoids ps cut b (); ())

let rec (add_zero @ total) : (k : index) @ immutable ->
    {u : unit | add k Z === k} @ ghost = fun k -> ghost_ (
    let z = Z in add_def k z;
    (match k with Z -> () | S k -> add_zero k; ()); ())

let rec (abstract_context_avoids @ total) : (ps : names) @ immutable ->
    (g : context) @ immutable -> {u : unit | context_avoids ps g} ->
    {u : unit | abstract_context ps Z g === weaken_context (count ps) g} @ ghost =
  fun ps g premise -> ghost_ (
    context_avoids_def ps g;
    let z = Z in let n = count ps in abstract_context_def ps z g; weaken_context_def n g;
    match g with Empty_context -> ()
    | Binding (s, rest) -> scheme_avoids_def ps s; abstract_scheme_def ps z s;
      weaken_scheme_def n s;
      (match s with Forall (k, t) -> add_zero k; abstract_avoids ps k t (); ());
      abstract_context_avoids ps rest (); ())

let (generalize_typing @ total) : (ps : names) @ immutable ->
    (n : index) @ immutable -> (g : context) @ immutable -> (e : term) @ immutable ->
    (t : mono) @ immutable -> (d : typing) @ immutable ->
    {u : unit | typed n g e t d && context_avoids ps g} ->
    {u : unit | typed (add (count ps) n) (weaken_context (count ps) g)
      e (abstract_type ps Z t) (abstract_typing ps Z d)} @ ghost =
  fun ps n g e t d premise -> ghost_ (
    let z = Z in let k = count ps in
    add_def z n; let total = add k n in add_def z total;
    abstraction_typed ps z n g e t d ();
    abstract_context_avoids ps g (); ())
