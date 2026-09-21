open Hm_declarative
open Hm_type_proofs
open Hm_substitution

let rec (shift_embed @ total) : (cut : index) @ immutable -> (k : index) @ immutable ->
    (t : Copy_spec.ty) @ immutable ->
    {u : unit | shift cut k (embed t) === embed t} @ ghost = fun cut k t -> ghost_ (
    embed_def t; let e = embed t in shift_def cut k e;
    (match t with Copy_spec.Variable _ | Copy_spec.Boolean -> ()
    | Copy_spec.Function (a, b) -> shift_embed cut k a; shift_embed cut k b; ());
    let u = () in refine_ u)

let rec (open_embed @ total) : (args : arguments) @ immutable ->
    (t : Copy_spec.ty) @ immutable ->
    {u : unit | open_type args (embed t) === embed t} @ ghost = fun args t -> ghost_ (
    embed_def t; let e = embed t in open_type_def args e;
    (match t with Copy_spec.Variable _ | Copy_spec.Boolean -> ()
    | Copy_spec.Function (a, b) -> open_embed args a; open_embed args b; ());
    let u = () in refine_ u)

let rec (substitute_wf @ total) : (n : index) @ immutable ->
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (t : mono) @ immutable -> {u : unit | mono_wf n t} ->
    {u : unit | mono_wf n (substitute_type rho t)} @ ghost = fun n rho t premise -> ghost_ (
    let refine_ premise = premise in mono_wf_def n t; substitute_type_def rho t;
    let changed = substitute_type rho t in mono_wf_def n changed;
    let u = () in match t with
    | Parameter _ | Boolean -> refine_ u
    | Free p -> let v = rho p in embed_wf n v; refine_ u
    | Function (a, b) -> substitute_wf n rho a (refine_ u);
      substitute_wf n rho b (refine_ u); refine_ u)

let (substitute_scheme_wf @ total) : (n : index) @ immutable ->
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (s : scheme) @ immutable -> {u : unit | scheme_wf n s} ->
    {u : unit | scheme_wf n (substitute_scheme rho s)} @ ghost = fun n rho s premise -> ghost_ (
    let refine_ premise = premise in scheme_wf_def n s; substitute_scheme_def rho s;
    let changed = substitute_scheme rho s in scheme_wf_def n changed;
    let u = () in match s with Forall (k, t) -> let total = add k n in
      substitute_wf total rho t (refine_ u); refine_ u)

let rec (substitute_context_wf @ total) : (n : index) @ immutable ->
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (g : context) @ immutable -> {u : unit | context_wf n g} ->
    {u : unit | context_wf n (substitute_context rho g)} @ ghost = fun n rho g premise -> ghost_ (
    let refine_ premise = premise in context_wf_def n g; substitute_context_def rho g;
    let changed = substitute_context rho g in context_wf_def n changed;
    let u = () in match g with Empty_context -> refine_ u
    | Binding (s, rest) -> substitute_scheme_wf n rho s (refine_ u);
      substitute_context_wf n rho rest (refine_ u); refine_ u)

let rec (substitute_arguments_wf @ total) : (n : index) @ immutable ->
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (args : arguments) @ immutable -> {u : unit | arguments_wf n args} ->
    {u : unit | arguments_wf n (substitute_arguments rho args)
      && length (substitute_arguments rho args) === length args} @ ghost = fun n rho args premise -> ghost_ (
    let refine_ premise = premise in arguments_wf_def n args; length_def args;
    substitute_arguments_def rho args; let changed = substitute_arguments rho args in
    arguments_wf_def n changed; length_def changed;
    let u = () in match args with No_arguments -> refine_ u
    | Argument (a, rest) -> substitute_wf n rho a (refine_ u);
      substitute_arguments_wf n rho rest (refine_ u); refine_ u)

let rec (substitute_lookup @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (g : context) @ immutable -> (i : index) @ immutable ->
    {u : unit | match lookup g i with None -> lookup (substitute_context rho g) i === None
      | Some s -> lookup (substitute_context rho g) i === Some (substitute_scheme rho s)} @ ghost =
  fun rho g i -> ghost_ (
    lookup_def g i; substitute_context_def rho g;
    let changed = substitute_context rho g in lookup_def changed i;
    (match g with Empty_context -> () | Binding (_, rest) -> match i with
      Z -> () | S i -> substitute_lookup rho rest i; ()); let u = () in refine_ u)

let rec (substitute_shift @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (cut : index) @ immutable -> (k : index) @ immutable -> (t : mono) @ immutable ->
    {u : unit | substitute_type rho (shift cut k t) === shift cut k (substitute_type rho t)} @ ghost =
  fun rho cut k t -> ghost_ (
    shift_def cut k t; substitute_type_def rho t;
    let shifted = shift cut k t in substitute_type_def rho shifted;
    let changed = substitute_type rho t in shift_def cut k changed;
    (match t with
    | Parameter _ | Boolean -> ()
    | Free p -> let v = rho p in shift_embed cut k v; ()
    | Function (a, b) -> substitute_shift rho cut k a; substitute_shift rho cut k b; ());
    let u = () in refine_ u)

let (substitute_weaken_scheme @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (k : index) @ immutable -> (s : scheme) @ immutable ->
    {u : unit | substitute_scheme rho (weaken_scheme k s)
      === weaken_scheme k (substitute_scheme rho s)} @ ghost = fun rho k s -> ghost_ (
    weaken_scheme_def k s; substitute_scheme_def rho s;
    let shifted = weaken_scheme k s in substitute_scheme_def rho shifted;
    let changed = substitute_scheme rho s in weaken_scheme_def k changed;
    (match s with Forall (cut, t) -> substitute_shift rho cut k t; ()); let u = () in refine_ u)

let rec (substitute_weaken_context @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (k : index) @ immutable -> (g : context) @ immutable ->
    {u : unit | substitute_context rho (weaken_context k g)
      === weaken_context k (substitute_context rho g)} @ ghost = fun rho k g -> ghost_ (
    weaken_context_def k g; substitute_context_def rho g;
    let shifted = weaken_context k g in substitute_context_def rho shifted;
    let changed = substitute_context rho g in weaken_context_def k changed;
    (match g with Empty_context -> () | Binding (s, rest) ->
      substitute_weaken_scheme rho k s; substitute_weaken_context rho k rest; ());
    let u = () in refine_ u)

let rec (substitute_open_index @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (args : arguments) @ immutable -> (i : index) @ immutable ->
    {u : unit | substitute_type rho (open_index args i)
      === open_index (substitute_arguments rho args) i} @ ghost = fun rho args i -> ghost_ (
    open_index_def args i; substitute_arguments_def rho args;
    let changed = substitute_arguments rho args in open_index_def changed i;
    let opened = open_index args i in substitute_type_def rho opened;
    (match args with No_arguments -> () | Argument (_, rest) -> match i with
      Z -> () | S i -> substitute_open_index rho rest i; ()); let u = () in refine_ u)

let rec (substitute_open @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (args : arguments) @ immutable -> (t : mono) @ immutable ->
    {u : unit | substitute_type rho (open_type args t)
      === open_type (substitute_arguments rho args) (substitute_type rho t)} @ ghost =
  fun rho args t -> ghost_ (
    open_type_def args t; substitute_type_def rho t;
    let opened = open_type args t in substitute_type_def rho opened;
    let changed = substitute_type rho t in let parameters = substitute_arguments rho args in
    open_type_def parameters changed;
    (match t with Parameter i -> substitute_open_index rho args i; ()
    | Boolean -> () | Free p -> let v = rho p in open_embed parameters v; ()
    | Function (a, b) -> substitute_open rho args a; substitute_open rho args b; ());
    let u = () in refine_ u)

let rec (substitution_typed @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (n : index) @ immutable -> (g : context) @ immutable -> (e : term) @ immutable ->
    (t : mono) @ immutable -> (d : typing) @ immutable ->
    {u : unit | typed n g e t d} ->
    {u : unit | typed n (substitute_context rho g) e (substitute_type rho t)
      (substitute_typing rho d)} @ ghost = fun rho n g e t d premise -> ghost_ (
    let refine_ premise = premise in typed_def n g e t d;
    substitute_type_def rho t; substitute_typing_def rho d;
    let changed_g = substitute_context rho g in let changed_t = substitute_type rho t in
    let changed_d = substitute_typing rho d in typed_def n changed_g e changed_t changed_d;
    let u = () in substitute_wf n rho t (refine_ u);
    substitute_context_wf n rho g (refine_ u);
    match d with
    | Variable args -> (match e with Bound i -> (match lookup g i with
      | None -> refine_ u
      | Some s ->
        substitute_lookup rho g i; substitute_arguments_wf n rho args (refine_ u);
        substitute_scheme_def rho s; let changed_s = substitute_scheme rho s in
        arity_def s; arity_def changed_s;
        open_scheme_def s args; let changed_args = substitute_arguments rho args in
        open_scheme_def changed_s changed_args;
        (match s with Forall (_, body) -> substitute_open rho args body; ()); refine_ u)
      | _ -> refine_ u)
    | Constant -> refine_ u
    | Abstraction (a, body) -> (match e, t with
      | Lambda e, Function (_, b) ->
        let z = Z in let arg_scheme = Forall (z, a) in
        let next = Binding (arg_scheme, g) in
        substitute_scheme_def rho arg_scheme; substitute_context_def rho next;
        substitution_typed rho n next e b body (refine_ u); refine_ u
      | _ -> refine_ u)
    | Application (a, left, right) -> (match e with Apply (f, x) ->
        let ft = Function (a, t) in substitute_type_def rho ft;
        substitution_typed rho n g f ft left (refine_ u);
        substitution_typed rho n g x a right (refine_ u); refine_ u
      | _ -> refine_ u)
    | Recursion (a, b, body) -> (match e with Recursive e ->
        let z = Z in let arg_scheme = Forall (z, a) in let self_scheme = Forall (z, t) in
        let self = Binding (self_scheme, g) in let next = Binding (arg_scheme, self) in
        substitute_scheme_def rho arg_scheme; substitute_scheme_def rho self_scheme;
        substitute_context_def rho self; substitute_context_def rho next;
        substitution_typed rho n next e b body (refine_ u); refine_ u
      | _ -> refine_ u)
    | Let_binding (s, rhs, body) -> (match e, s with Let (r, b), Forall (k, a) ->
        substitute_scheme_def rho s; substitute_scheme_wf n rho s (refine_ u);
        substitute_weaken_context rho k g;
        let shifted = weaken_context k g in let total = add k n in
        substitution_typed rho total shifted r a rhs (refine_ u);
        let next = Binding (s, g) in substitute_context_def rho next;
        substitution_typed rho n next b t body (refine_ u); refine_ u
      | _ -> refine_ u))
