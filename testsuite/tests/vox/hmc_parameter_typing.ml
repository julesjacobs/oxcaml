open Hm_declarative
open Hm_interpreter_substitution
module P = Hm_interpreter_substitution_proofs
module W = Hm_type_proofs

let rec (lift_wf @ total) : (s : substitution) @ immutable ->
    (n : index) @ immutable -> (m : index) @ immutable ->
    (mapping : ((i : index) @ immutable ->
      {u : unit | not (present n i) || mono_wf m (at s.front s.tail i)})) @ total ->
    (k : index) @ immutable -> (i : index) @ immutable ->
    {u : unit | not (present (add k n) i)
      || mono_wf (add k m) (at (lift k s).front (lift k s).tail i)} @ ghost =
  fun s n m mapping k i -> ghost_ (
    add_def k n; add_def k m; lift_def k s;
    match k with
    | Z -> mapping i
    | S rest ->
      present_def (add k n) i;
      let base = lift rest s in let raised = bump base in
      (match i with
      | Z ->
        bump_def base; at_def raised.front raised.tail i;
        mono_wf_def (add k m) (Parameter Z); present_def (add k m) Z; ()
      | S j ->
        lift_wf s n m mapping rest j;
        P.at_bump base j;
        if present (add rest n) j then (
          let ty = at base.front base.tail j in
          add_def Z (add rest m); add_def Z (add (S Z) (add rest m));
          W.shift_wf Z (S Z) (add rest m) ty ();
          add_def (S Z) (add rest m); add_def Z (add rest m); ()) else ()))

let rec (type_wf @ total) : (s : substitution) @ immutable ->
    (n : index) @ immutable -> (m : index) @ immutable ->
    (mapping : ((i : index) @ immutable ->
      {u : unit | not (present n i) || mono_wf m (at s.front s.tail i)})) @ total ->
    (ty : mono) @ immutable -> {u : unit | mono_wf n ty} ->
    {u : unit | mono_wf m (act s ty)} @ ghost = fun s n m mapping ty premise -> ghost_ (
    mono_wf_def n ty; act_def s ty; mono_wf_def m (act s ty);
    match ty with
    | Parameter i -> mapping i
    | Free _ | Boolean | Word64 -> ()
    | List_type a -> type_wf s n m mapping a ()
    | Function (a, b) -> type_wf s n m mapping a (); type_wf s n m mapping b ())

let (scheme_well_formed @ total) : (s : substitution) @ immutable ->
    (n : index) @ immutable -> (m : index) @ immutable ->
    (mapping : ((i : index) @ immutable ->
      {u : unit | not (present n i) || mono_wf m (at s.front s.tail i)})) @ total ->
    (scheme : scheme) @ immutable -> {u : unit | scheme_wf n scheme} ->
    {u : unit | scheme_wf m (act_scheme s scheme)} @ ghost = fun s n m mapping scheme premise -> ghost_ (
    scheme_wf_def n scheme; act_scheme_def s scheme; scheme_wf_def m (act_scheme s scheme);
    match scheme with Forall (k, ty) ->
      let up = lift k s in let source = add k n in let target = add k m in
      let lifted : ((i : index) @ immutable ->
        {u : unit | not (present source i)
          || mono_wf target (at up.front up.tail i)}) @ total =
        fun i -> lift_wf s n m mapping k i in
      type_wf up source target lifted ty ())

let rec (context_well_formed @ total) : (s : substitution) @ immutable ->
    (n : index) @ immutable -> (m : index) @ immutable ->
    (mapping : ((i : index) @ immutable ->
      {u : unit | not (present n i) || mono_wf m (at s.front s.tail i)})) @ total ->
    (g : context) @ immutable -> {u : unit | context_wf n g} ->
    {u : unit | context_wf m (act_context s g)} @ ghost = fun s n m mapping g premise -> ghost_ (
    context_wf_def n g; act_context_def s g; context_wf_def m (act_context s g);
    match g with Empty_context -> () | Binding (scheme, rest) ->
      scheme_well_formed s n m mapping scheme (); context_well_formed s n m mapping rest ())

let rec (arguments_well_formed @ total) : (s : substitution) @ immutable ->
    (n : index) @ immutable -> (m : index) @ immutable ->
    (mapping : ((i : index) @ immutable ->
      {u : unit | not (present n i) || mono_wf m (at s.front s.tail i)})) @ total ->
    (args : arguments) @ immutable -> {u : unit | arguments_wf n args} ->
    {u : unit | arguments_wf m (act_arguments s args)} @ ghost = fun s n m mapping args premise -> ghost_ (
    arguments_wf_def n args; act_arguments_def s args; arguments_wf_def m (act_arguments s args);
    match args with No_arguments -> () | Argument (ty, rest) ->
      type_wf s n m mapping ty (); arguments_well_formed s n m mapping rest ())

let rec (typing_action @ total) : (s : substitution) @ immutable ->
    (n : index) @ immutable -> (m : index) @ immutable ->
    (mapping : ((i : index) @ immutable ->
      {u : unit | not (present n i) || mono_wf m (at s.front s.tail i)})) @ total ->
    (g : context) @ immutable -> (e : term) @ immutable ->
    (t : mono) @ immutable -> (d : typing) @ immutable ->
    {u : unit | typed n g e t d} ->
    {u : unit | typed m (act_context s g) e (act s t) (act_typing s d)} @ ghost =
  fun s n m mapping g e t d premise -> ghost_ (
    typed_def n g e t d; act_typing_def s d; act_def s t;
    let cg = act_context s g in let ct = act s t in let cd = act_typing s d in
    typed_def m cg e ct cd;
    type_wf s n m mapping t (); context_well_formed s n m mapping g ();
    (match d with
     | Variable args -> (match e with Bound i ->
       arguments_well_formed s n m mapping args ();
       P.lookup_action s g i; P.length_action s args;
       (match lookup g i with None -> () | Some scheme ->
         P.scheme_open s scheme args ();
         arity_def scheme; act_scheme_def s scheme;
         let changed = act_scheme s scheme in arity_def changed)
       | _ -> ())
     | Constant -> act_def s Boolean
     | Word_constant -> act_def s Word64
     | Empty_list a -> type_wf s n m mapping a (); act_def s (List_type a)
     | List_cons (a, head, tail) -> (match e with
       | Cons (h, r) -> typing_action s n m mapping g h a head ();
         typing_action s n m mapping g r t tail (); act_def s (List_type a)
       | _ -> ())
     | List_case (a, scrutinee, empty, nonempty) -> (match e with
       | CaseList (v, l, r) ->
         type_wf s n m mapping a ();
         typing_action s n m mapping g v (List_type a) scrutinee ();
         typing_action s n m mapping g l t empty ();
         let tail = Forall (Z, List_type a) in let head = Forall (Z, a) in
         let tg = Binding (tail, g) in let bg = Binding (head, tg) in
         typing_action s n m mapping bg r t nonempty ();
         act_context_def s bg; act_context_def s tg;
         act_scheme_def s head; act_scheme_def s tail; lift_def Z s;
         act_def s (List_type a)
       | _ -> ())
     | Conditional (condition, yes, no) -> (match e with
       | If (c, a, b) -> typing_action s n m mapping g c Boolean condition ();
         typing_action s n m mapping g a t yes (); typing_action s n m mapping g b t no ();
         act_def s Boolean
       | _ -> ())
     | Word_primitive (left, right) -> (match e with
       | Primitive (op, a, b) -> typing_action s n m mapping g a Word64 left ();
         typing_action s n m mapping g b Word64 right (); act_def s Word64;
         operation_type_def op;
         (match op with Add | Subtract -> () | Equal_word | Unsigned_less -> act_def s Boolean)
       | _ -> ())
     | Abstraction (a, body) -> (match e, t with
       | Lambda e, Function (_, b) ->
         let scheme = Forall (Z, a) in let bg = Binding (scheme, g) in
         typing_action s n m mapping bg e b body ();
         act_context_def s bg; act_scheme_def s scheme; lift_def Z s
       | _ -> ())
     | Application (a, left, right) -> (match e with
       | Apply (f, x) -> typing_action s n m mapping g f (Function (a, t)) left ();
         typing_action s n m mapping g x a right (); act_def s (Function (a, t))
       | _ -> ())
     | Recursion (a, b, body) -> (match e with
       | Recursive e ->
         let self = Forall (Z, t) in let arg = Forall (Z, a) in
         let sg = Binding (self, g) in let bg = Binding (arg, sg) in
         typing_action s n m mapping bg e b body ();
         act_context_def s bg; act_context_def s sg;
         act_scheme_def s self; act_scheme_def s arg; lift_def Z s
       | _ -> ())
     | Let_binding (scheme, rhs, body) -> (match e, scheme with
       | Let (r, b), Forall (k, a) ->
         scheme_well_formed s n m mapping scheme ();
         let up = lift k s in let rg = weaken_context k g in
         let source = add k n in let target = add k m in
         let lifted : ((i : index) @ immutable ->
           {u : unit | not (present source i) || mono_wf target (at up.front up.tail i)}) @ total =
           fun i -> lift_wf s n m mapping k i in
         typing_action up source target lifted rg r a rhs ();
         P.context_lift_weaken k s g;
         let bg = Binding (scheme, g) in typing_action s n m mapping bg b t body ();
         act_context_def s bg; act_scheme_def s scheme
       | _ -> ()));
    ())

