open Hm_declarative

type values = No_values | Value of Copy_spec.ty * values [@@inductive]
let[@def] rec (values_length @ total) (vs : values @ immutable) =
  match vs with No_values -> Z | Value (_, rest) -> S (values_length rest)
let[@def] rec (prefix @ total) (vs : values @ immutable)
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (i : index @ immutable) = match vs with
  | No_values -> xi i | Value (t, rest) ->
    match i with Z -> t | S i -> prefix rest xi i
let[@def] rec (eval @ total)
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (t : mono @ immutable) = match t with
  | Parameter i -> xi i | Free p -> Copy_spec.Variable p
  | Boolean -> Copy_spec.Boolean
  | Word64 -> Copy_spec.Word64
  | List_type a -> Copy_spec.List_type (eval xi a)
  | Function (a, b) -> Copy_spec.Function (eval xi a, eval xi b)
let[@def] rec (eval_arguments @ total)
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (args : arguments @ immutable) = match args with
  | No_arguments -> No_values
  | Argument (t, rest) -> Value (eval xi t, eval_arguments xi rest)
let[@def] rec (eval_prefixed @ total) (vs : values @ immutable)
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (t : mono @ immutable) = match t with
  | Parameter i -> prefix vs xi i | Free p -> Copy_spec.Variable p
  | Boolean -> Copy_spec.Boolean
  | Word64 -> Copy_spec.Word64
  | List_type a -> Copy_spec.List_type (eval_prefixed vs xi a)
  | Function (a, b) -> Copy_spec.Function (eval_prefixed vs xi a, eval_prefixed vs xi b)
let[@def] (meaning @ total)
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (s : scheme @ immutable) (vs : values @ immutable) = match s with
  | Forall (_, t) -> eval_prefixed vs xi t

let rec (eval_embed @ total) :
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (t : Copy_spec.ty) @ immutable ->
    {u : unit | eval xi (embed t) === t} @ ghost = fun xi t -> ghost_ (
  embed_def t; let e = embed t in eval_def xi e;
  (match t with Copy_spec.Variable _ | Copy_spec.Boolean | Copy_spec.Word64 -> ()
    | Copy_spec.List_type a -> eval_embed xi a; ()
    | Copy_spec.Function (a, b) -> eval_embed xi a; eval_embed xi b; ());
  ())

let rec (embed_wf @ total) : (n : index) @ immutable ->
    (t : Copy_spec.ty) @ immutable ->
    {u : unit | mono_wf n (embed t)} @ ghost = fun n t -> ghost_ (
  embed_def t; let e = embed t in mono_wf_def n e;
  (match t with Copy_spec.Variable _ | Copy_spec.Boolean | Copy_spec.Word64 -> ()
    | Copy_spec.List_type a -> embed_wf n a; ()
    | Copy_spec.Function (a, b) -> embed_wf n a; embed_wf n b; ());
  ())

let rec (eval_arguments_length @ total) :
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (args : arguments) @ immutable ->
    {u : unit | values_length (eval_arguments xi args) === length args} @ ghost =
  fun xi args -> ghost_ (
    eval_arguments_def xi args; length_def args;
    let vs = eval_arguments xi args in values_length_def vs;
    (match args with No_arguments -> ()
      | Argument (_, rest) -> eval_arguments_length xi rest; ());
    ())

let rec (eval_open_index @ total) :
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (args : arguments) @ immutable -> (i : index) @ immutable ->
    {u : unit | eval xi (open_index args i)
      === prefix (eval_arguments xi args) xi i} @ ghost = fun xi args i -> ghost_ (
    open_index_def args i; eval_arguments_def xi args;
    let vs = eval_arguments xi args in prefix_def vs xi i;
    let t = open_index args i in eval_def xi t;
    (match args with No_arguments -> () | Argument (_, rest) ->
      match i with Z -> () | S i -> eval_open_index xi rest i; ());
    ())

let rec (eval_open @ total) :
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (args : arguments) @ immutable -> (t : mono) @ immutable ->
    {u : unit | eval xi (open_type args t)
      === eval_prefixed (eval_arguments xi args) xi t} @ ghost =
  fun xi args t -> ghost_ (
    open_type_def args t; let vs = eval_arguments xi args in eval_prefixed_def vs xi t;
    let opened = open_type args t in eval_def xi opened;
    (match t with
    | Parameter i -> eval_open_index xi args i; ()
    | Free _ | Boolean | Word64 -> ()
    | List_type a -> eval_open xi args a; ()
    | Function (a, b) -> eval_open xi args a; eval_open xi args b; ());
    ())

let (eval_open_scheme @ total) :
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (s : scheme) @ immutable -> (args : arguments) @ immutable ->
    {u : unit | eval xi (open_scheme s args)
      === meaning xi s (eval_arguments xi args)} @ ghost =
  fun xi s args -> ghost_ (
    open_scheme_def s args;
    let vs = eval_arguments xi args in meaning_def xi s vs;
    (match s with Forall (_, t) -> eval_open xi args t; ());
    ())

let rec (prefix_skip @ total) : (vs : values) @ immutable ->
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (i : index) @ immutable ->
    {u : unit | prefix vs xi (add (values_length vs) i) === xi i} @ ghost =
  fun vs xi i -> ghost_ (
    values_length_def vs; let k = values_length vs in add_def k i;
    let j = add k i in prefix_def vs xi j;
    (match vs with No_values -> () | Value (_, rest) -> prefix_skip rest xi i; ());
    ())

let rec (prefix_shift @ total) : (ws : values) @ immutable ->
    (vs : values) @ immutable ->
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (zeta : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (equal : ((i : index) @ immutable ->
      {u : unit | zeta i === prefix vs xi i})) @ total ->
    (i : index) @ immutable ->
    {u : unit | prefix ws zeta (shift_index (values_length ws) (values_length vs) i)
      === prefix ws xi i} @ ghost = fun ws vs xi zeta equal i -> ghost_ (
    values_length_def ws; let cut = values_length ws in let k = values_length vs in
    shift_index_def cut k i; let j = shift_index cut k i in
    prefix_def ws zeta j; prefix_def ws xi i;
    (match ws with
    | No_values -> let j = add k i in equal j; prefix_skip vs xi i; ()
    | Value (_, rest) -> match i with Z -> ()
      | S i -> prefix_shift rest vs xi zeta equal i; ());
    ())

let rec (eval_shift @ total) : (ws : values) @ immutable ->
    (vs : values) @ immutable ->
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (zeta : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (equal : ((i : index) @ immutable ->
      {u : unit | zeta i === prefix vs xi i})) @ total ->
    (t : mono) @ immutable ->
    {u : unit | eval_prefixed ws zeta (shift (values_length ws) (values_length vs) t)
      === eval_prefixed ws xi t} @ ghost = fun ws vs xi zeta equal t -> ghost_ (
    let cut = values_length ws in let k = values_length vs in
    shift_def cut k t; let shifted = shift cut k t in
    eval_prefixed_def ws zeta shifted; eval_prefixed_def ws xi t;
    (match t with
    | Parameter i -> prefix_shift ws vs xi zeta equal i; ()
    | Free _ | Boolean | Word64 -> ()
    | List_type a -> eval_shift ws vs xi zeta equal a; ()
    | Function (a, b) -> eval_shift ws vs xi zeta equal a;
      eval_shift ws vs xi zeta equal b; ());
    ())

let (meaning_weaken @ total) : (ws : values) @ immutable ->
    (vs : values) @ immutable ->
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (zeta : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (equal : ((i : index) @ immutable ->
      {u : unit | zeta i === prefix vs xi i})) @ total ->
    (s : scheme) @ immutable -> {u : unit | values_length ws === arity s} ->
    {u : unit | meaning zeta (weaken_scheme (values_length vs) s) ws
      === meaning xi s ws} @ ghost = fun ws vs xi zeta equal s premise -> ghost_ (
    arity_def s; let k = values_length vs in
    weaken_scheme_def k s; let shifted = weaken_scheme k s in
    meaning_def zeta shifted ws; meaning_def xi s ws;
    (match s with Forall (_, t) -> eval_shift ws vs xi zeta equal t; ());
    ())

let rec (lookup_weaken @ total) : (k : index) @ immutable ->
    (g : context) @ immutable -> (i : index) @ immutable ->
    {u : unit | match lookup g i with
      None -> lookup (weaken_context k g) i === None
      | Some s -> lookup (weaken_context k g) i === Some (weaken_scheme k s)} @ ghost =
  fun k g i -> ghost_ (
    lookup_def g i; weaken_context_def k g;
    let shifted = weaken_context k g in lookup_def shifted i;
    (match g with Empty_context -> () | Binding (_, rest) ->
      match i with Z -> () | S i -> lookup_weaken k rest i; ());
    ())

let rec (open_index_wf @ total) : (n : index) @ immutable ->
    (args : arguments) @ immutable -> (i : index) @ immutable ->
    {u : unit | arguments_wf n args && present (add (length args) n) i} ->
    {u : unit | mono_wf n (open_index args i)} @ ghost =
  fun n args i premise -> ghost_ (
    arguments_wf_def n args; length_def args;
    let k = length args in add_def k n; let total = add k n in present_def total i;
    open_index_def args i; let t = open_index args i in mono_wf_def n t;
    match args with
    | No_arguments -> ()
    | Argument (_, rest) -> match i with Z -> ()
      | S i -> let () = open_index_wf n rest i () in ())

let rec (open_wf @ total) : (n : index) @ immutable ->
    (args : arguments) @ immutable -> (t : mono) @ immutable ->
    {u : unit | arguments_wf n args && mono_wf (add (length args) n) t} ->
    {u : unit | mono_wf n (open_type args t)} @ ghost =
  fun n args t premise -> ghost_ (
    let total = add (length args) n in
    mono_wf_def total t; open_type_def args t;
    let opened = open_type args t in mono_wf_def n opened;
    match t with
    | Parameter i -> let () = open_index_wf n args i () in ()
    | Free _ | Boolean | Word64 -> ()
    | List_type a -> open_wf n args a (); ()
    | Function (a, b) -> open_wf n args a ();
      open_wf n args b (); ())

let (open_scheme_wf @ total) : (n : index) @ immutable ->
    (s : scheme) @ immutable -> (args : arguments) @ immutable ->
    {u : unit | scheme_wf n s && arguments_wf n args && length args === arity s} ->
    {u : unit | mono_wf n (open_scheme s args)} @ ghost =
  fun n s args premise -> ghost_ (
    scheme_wf_def n s; arity_def s;
    open_scheme_def s args; match s with
    | Forall (_, t) -> let () = open_wf n args t () in ())

let rec (lookup_scoped @ total) : (g : context) @ immutable ->
    (i : index) @ immutable -> (s : scheme) @ immutable ->
    {u : unit | lookup g i === Some s} ->
    {u : unit | present (depth g) i} @ ghost = fun g i s premise -> ghost_ (
    lookup_def g i; depth_def g;
    let n = depth g in present_def n i; match g with Empty_context -> () | Binding (_, rest) ->
    match i with Z -> ()
    | S i -> let () = lookup_scoped rest i s () in ())

let rec (weaken_depth @ total) : (k : index) @ immutable -> (g : context) @ immutable ->
    {u : unit | depth (weaken_context k g) === depth g} @ ghost = fun k g -> ghost_ (
    weaken_context_def k g; depth_def g;
    let shifted = weaken_context k g in depth_def shifted;
    (match g with Empty_context -> () | Binding (_, rest) -> weaken_depth k rest; ());
    ())

let rec (typing_scoped @ total) : (n : index) @ immutable ->
    (g : context) @ immutable -> (e : term) @ immutable ->
    (t : mono) @ immutable -> (d : typing) @ immutable ->
    {u : unit | typed n g e t d} ->
    {u : unit | scoped_term (depth g) e} @ ghost = fun n g e t d premise -> ghost_ (
    typed_def n g e t d;
    let level = depth g in scoped_term_def level e;
    match d with
    | Variable _ -> (match e with Bound i -> (match lookup g i with
      | None -> () | Some s -> let () = lookup_scoped g i s () in ())
      | _ -> ())
    | Constant | Word_constant | Empty_list _ -> ()
    | List_cons (a, head, tail) -> (match e with
      | Cons (h, r) -> typing_scoped n g h a head (); typing_scoped n g r t tail (); ()
      | _ -> ())
    | List_case (a, scrutinee, empty, nonempty) -> (match e with
      | CaseList (s, l, r) -> typing_scoped n g s (List_type a) scrutinee ();
        typing_scoped n g l t empty ();
        let tail = Binding (Forall (Z, List_type a), g) in
        let both = Binding (Forall (Z, a), tail) in depth_def tail; depth_def both;
        typing_scoped n both r t nonempty (); ()
      | _ -> ())
    | Conditional (condition, yes, no) -> (match e with
      | If (c, a, b) -> typing_scoped n g c Boolean condition ();
        typing_scoped n g a t yes (); typing_scoped n g b t no (); ()
      | _ -> ())
    | Word_primitive (left, right) -> (match e with
      | Primitive (_, a, b) -> typing_scoped n g a Word64 left ();
        typing_scoped n g b Word64 right (); ()
      | _ -> ())
    | Abstraction (a, body) -> (match e, t with
      | Lambda e, Function (_, b) -> let next = Binding (Forall (Z, a), g) in
        depth_def next; typing_scoped n next e b body (); ()
      | _ -> ())
    | Application (a, left, right) -> (match e with Apply (f, x) ->
      let ft = Function (a, t) in typing_scoped n g f ft left ();
      typing_scoped n g x a right (); () | _ -> ())
    | Recursion (a, b, body) -> (match e with Recursive e ->
      let self = Binding (Forall (Z, t), g) in
      let next = Binding (Forall (Z, a), self) in depth_def self; depth_def next;
      typing_scoped n next e b body (); () | _ -> ())
    | Let_binding (s, rhs, body) -> (match e, s with
      | Let (r, b), Forall (k, a) -> let shifted = weaken_context k g in
        let total = add k n in weaken_depth k g;
        typing_scoped total shifted r a rhs ();
        let next = Binding (s, g) in depth_def next;
        typing_scoped n next b t body (); ()
      | _ -> ()))

let rec (present_added @ total) : (k : index) @ immutable ->
    (n : index) @ immutable -> (i : index) @ immutable ->
    {u : unit | present n i} ->
    {u : unit | present (add k n) (add k i)} @ ghost = fun k n i premise -> ghost_ (
    add_def k n; add_def k i;
    let total = add k n in let j = add k i in present_def total j;
    match k with Z -> ()
    | S k -> present_added k n i (); ())

let rec (shift_index_wf @ total) : (cut : index) @ immutable ->
    (k : index) @ immutable -> (n : index) @ immutable -> (i : index) @ immutable ->
    {u : unit | present (add cut n) i} ->
    {u : unit | present (add cut (add k n)) (shift_index cut k i)} @ ghost =
  fun cut k n i premise -> ghost_ (
    add_def cut n;
    let extended = add k n in add_def cut extended;
    shift_index_def cut k i; let old = add cut n in present_def old i;
    let total = add cut extended in let j = shift_index cut k i in present_def total j;
    match cut with
    | Z -> present_added k n i (); ()
    | S cut -> match i with Z -> ()
      | S i -> shift_index_wf cut k n i (); ())

let rec (shift_wf @ total) : (cut : index) @ immutable ->
    (k : index) @ immutable -> (n : index) @ immutable -> (t : mono) @ immutable ->
    {u : unit | mono_wf (add cut n) t} ->
    {u : unit | mono_wf (add cut (add k n)) (shift cut k t)} @ ghost =
  fun cut k n t premise -> ghost_ (
    let old = add cut n in
    let total = add cut (add k n) in mono_wf_def old t; shift_def cut k t;
    let shifted = shift cut k t in mono_wf_def total shifted;
    match t with
    | Parameter i -> shift_index_wf cut k n i (); ()
    | Free _ | Boolean | Word64 -> ()
    | List_type a -> shift_wf cut k n a (); ()
    | Function (a, b) -> shift_wf cut k n a ();
      shift_wf cut k n b (); ())

let (weaken_scheme_wf @ total) : (k : index) @ immutable ->
    (n : index) @ immutable -> (s : scheme) @ immutable ->
    {u : unit | scheme_wf n s} ->
    {u : unit | scheme_wf (add k n) (weaken_scheme k s)} @ ghost =
  fun k n s premise -> ghost_ (
    scheme_wf_def n s; weaken_scheme_def k s;
    let total = add k n in let shifted = weaken_scheme k s in scheme_wf_def total shifted;
    match s with Forall (cut, t) -> shift_wf cut k n t (); ())

let rec (weaken_context_wf @ total) : (k : index) @ immutable ->
    (n : index) @ immutable -> (g : context) @ immutable ->
    {u : unit | context_wf n g} ->
    {u : unit | context_wf (add k n) (weaken_context k g)} @ ghost =
  fun k n g premise -> ghost_ (
    context_wf_def n g; weaken_context_def k g;
    let total = add k n in let shifted = weaken_context k g in context_wf_def total shifted;
    match g with Empty_context -> ()
    | Binding (s, rest) -> weaken_scheme_wf k n s ();
      weaken_context_wf k n rest (); ())

let rec (open_empty @ total) : (t : mono) @ immutable ->
    {u : unit | open_type No_arguments t === t} @ ghost = fun t -> ghost_ (
    let args = No_arguments in open_type_def args t;
    (match t with
    | Parameter i -> open_index_def args i; ()
    | Free _ | Boolean | Word64 -> ()
    | List_type a -> open_empty a; ()
    | Function (a, b) -> open_empty a; open_empty b; ());
    ())

let rec (eval_valuation @ total) : (vs : values) @ immutable ->
    (xi : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (zeta : (index @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (equal : ((i : index) @ immutable ->
      {u : unit | zeta i === prefix vs xi i})) @ total ->
    (t : mono) @ immutable ->
    {u : unit | eval zeta t === eval_prefixed vs xi t} @ ghost =
  fun vs xi zeta equal t -> ghost_ (
    eval_def zeta t; eval_prefixed_def vs xi t;
    (match t with Parameter i -> equal i; () | Free _ | Boolean | Word64 -> ()
    | List_type a -> eval_valuation vs xi zeta equal a; ()
    | Function (a, b) -> eval_valuation vs xi zeta equal a;
      eval_valuation vs xi zeta equal b; ());
    ())
