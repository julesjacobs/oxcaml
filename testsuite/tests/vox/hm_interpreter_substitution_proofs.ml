open Hm_declarative
open Hm_interpreter_substitution

let rec (at_shift @ total) : (k : index) @ immutable ->
    (args : arguments) @ immutable -> (tail : index) @ immutable ->
    (i : index) @ immutable ->
    {u : unit | at (shift_arguments k args) (add k tail) i
      === shift Z k (at args tail i)} @ ghost = fun k args tail i -> ghost_ (
  shift_arguments_def k args; at_def args tail i;
  let shifted = shift_arguments k args in let total = add k tail in
  at_def shifted total i;
  let t = at args tail i in shift_def Z k t;
  (match args with
  | No_arguments ->
    let j = add tail i in shift_index_def Z k j;
    Hm_abstraction_proofs.add_assoc k tail i
  | Argument (_, rest) -> match i with Z -> ()
    | S i -> at_shift k rest tail i);
  ())

let (at_bump @ total) : (s : substitution) @ immutable ->
    (i : index) @ immutable ->
    {u : unit | at (bump s).front (bump s).tail (S i)
      === shift Z (S Z) (at s.front s.tail i)} @ ghost = fun s i -> ghost_ (
  bump_def s; let one = S Z in add_def one s.tail; add_def Z s.tail;
  let shifted = shift_arguments one s.front in
  at_def (Argument (Parameter Z, shifted)) (S s.tail) (S i);
  at_shift one s.front s.tail i;
  ())

let rec (length_action @ total) : (s : substitution) @ immutable ->
    (args : arguments) @ immutable ->
    {u : unit | length (act_arguments s args) === length args} @ ghost =
  fun s args -> ghost_ (
    act_arguments_def s args; length_def args;
    let changed = act_arguments s args in length_def changed;
    (match args with No_arguments -> ()
     | Argument (_, rest) -> length_action s rest);
    ())

let rec (lookup_action @ total) : (s : substitution) @ immutable ->
    (g : context) @ immutable -> (i : index) @ immutable ->
    {u : unit | lookup (act_context s g) i ===
      (match lookup g i with None -> None | Some scheme -> Some (act_scheme s scheme))}
    @ ghost = fun s g i -> ghost_ (
  act_context_def s g; lookup_def g i;
  let changed = act_context s g in lookup_def changed i;
  (match g with Empty_context -> ()
   | Binding (_, rest) -> match i with Z -> ()
     | S i -> lookup_action s rest i);
  ())

let rec (at_open @ total) : (args : arguments) @ immutable ->
    (i : index) @ immutable ->
    {u : unit | at args Z i === open_index args i} @ ghost = fun args i -> ghost_ (
  at_def args Z i; open_index_def args i; add_def Z i;
  (match args with No_arguments -> ()
   | Argument (_, rest) -> match i with Z -> () | S i -> at_open rest i);
  ())

let rec (action_open @ total) : (args : arguments) @ immutable ->
    (t : mono) @ immutable ->
    {u : unit | act {front = args; tail = Z} t === open_type args t} @ ghost =
  fun args t -> ghost_ (
    act_def {front = args; tail = Z} t; open_type_def args t;
    (match t with
     | Parameter i -> at_open args i
     | Free _ | Boolean | Word64 -> ()
     | List_type a -> action_open args a
     | Function (a, b) -> action_open args a; action_open args b);
    ())

let rec (action_weaken @ total) : (k : index) @ immutable ->
    (t : mono) @ immutable ->
    {u : unit | act {front = No_arguments; tail = k} t === shift Z k t} @ ghost =
  fun k t -> ghost_ (
    act_def {front = No_arguments; tail = k} t; shift_def Z k t;
    (match t with
     | Parameter i -> at_def No_arguments k i; shift_index_def Z k i
     | Free _ | Boolean | Word64 -> ()
     | List_type a -> action_weaken k a
     | Function (a, b) -> action_weaken k a; action_weaken k b);
    ())

let rec (shift_successor @ total) : (k : index) @ immutable ->
    (t : mono) @ immutable ->
    {u : unit | shift Z (S Z) (shift Z k t) === shift Z (S k) t} @ ghost =
  fun k t -> ghost_ (
    shift_def Z k t; let changed = shift Z k t in
    shift_def Z (S Z) changed; shift_def Z (S k) t;
    (match t with
     | Parameter i -> shift_index_def Z k i; shift_index_def Z (S k) i;
       let j = add k i in shift_index_def Z (S Z) j;
       add_def (S Z) j; add_def Z j; add_def (S k) i
     | Free _ | Boolean | Word64 -> ()
     | List_type a -> shift_successor k a
     | Function (a, b) -> shift_successor k a; shift_successor k b);
    ())

let rec (at_lift_low @ total) : (k : index) @ immutable ->
    (s : substitution) @ immutable -> (i : index) @ immutable ->
    {u : unit | present k i} ->
    {u : unit | at (lift k s).front (lift k s).tail i === Parameter i} @ ghost =
  fun k s i premise -> ghost_ (
    present_def k i; lift_def k s;
    (match k with Z -> () | S k ->
      let lower = lift k s in bump_def lower;
      match i with
      | Z -> let shifted = shift_arguments (S Z) lower.front in
        at_def (Argument (Parameter Z, shifted)) (S lower.tail) Z
      | S i -> at_lift_low k s i (); at_bump lower i;
        shift_def Z (S Z) (Parameter i); shift_index_def Z (S Z) i;
        add_def (S Z) i; add_def Z i);
    ())

let rec (shift_zero @ total) : (t : mono) @ immutable ->
    {u : unit | shift Z Z t === t} @ ghost = fun t -> ghost_ (
  shift_def Z Z t;
  (match t with
   | Parameter i -> shift_index_def Z Z i; add_def Z i
   | Free _ | Boolean | Word64 -> ()
   | List_type a -> shift_zero a
     | Function (a, b) -> shift_zero a; shift_zero b);
  ())

let rec (at_lift_high @ total) : (k : index) @ immutable ->
    (s : substitution) @ immutable -> (i : index) @ immutable ->
    {u : unit | at (lift k s).front (lift k s).tail (add k i)
      === shift Z k (at s.front s.tail i)} @ ghost = fun k s i -> ghost_ (
  lift_def k s; add_def k i;
  (match k with
   | Z -> let t = at s.front s.tail i in
     shift_zero t
   | S k -> let lower = lift k s in let j = add k i in
     at_bump lower j; at_lift_high k s i;
     let t = at s.front s.tail i in shift_successor k t);
  ())

let rec (lift_add @ total) : (a : index) @ immutable ->
    (b : index) @ immutable -> (s : substitution) @ immutable ->
    {u : unit | lift a (lift b s) === lift (add a b) s} @ ghost =
  fun a b s -> ghost_ (
    let inner = lift b s in lift_def a inner; add_def a b;
    let k = add a b in lift_def k s;
    (match a with Z -> () | S a -> lift_add a b s);
    ())

let rec (open_shift_one @ total) : (head : mono) @ immutable ->
    (args : arguments) @ immutable -> (t : mono) @ immutable ->
    {u : unit | open_type (Argument (head, args)) (shift Z (S Z) t)
      === open_type args t} @ ghost = fun head args t -> ghost_ (
  shift_def Z (S Z) t; let changed = shift Z (S Z) t in
  let all = Argument (head, args) in open_type_def all changed;
  open_type_def args t;
  (match t with
   | Parameter i -> shift_index_def Z (S Z) i;
     add_def (S Z) i; add_def Z i; open_index_def all (S i)
   | Free _ | Boolean | Word64 -> ()
   | List_type a -> open_shift_one head args a
     | Function (a, b) -> open_shift_one head args a; open_shift_one head args b);
  ())

let rec (action_open_index @ total) : (s : substitution) @ immutable ->
    (args : arguments) @ immutable -> (i : index) @ immutable ->
    {u : unit | act s (open_index args i) ===
      open_type (act_arguments s args)
        (at (lift (length args) s).front (lift (length args) s).tail i)} @ ghost =
  fun s args i -> ghost_ (
    open_index_def args i; length_def args; act_arguments_def s args;
    let k = length args in lift_def k s;
    (match args with
     | No_arguments -> act_def s (Parameter i);
       let t = at s.front s.tail i in Hm_type_proofs.open_empty t
     | Argument (head, rest) ->
       let lower = lift (length rest) s in bump_def lower;
       match i with
       | Z -> let shifted = shift_arguments (S Z) lower.front in
         at_def (Argument (Parameter Z, shifted)) (S lower.tail) Z;
         let changed = act_arguments s rest in let h = act s head in
         let all = Argument (h, changed) in
         open_type_def all (Parameter Z); open_index_def all Z
       | S i -> at_bump lower i; action_open_index s rest i;
         let h = act s head in let changed = act_arguments s rest in
         let t = at lower.front lower.tail i in open_shift_one h changed t);
    ())

let rec (action_open_type @ total) : (s : substitution) @ immutable ->
    (args : arguments) @ immutable -> (t : mono) @ immutable ->
    {u : unit | act s (open_type args t) ===
      open_type (act_arguments s args) (act (lift (length args) s) t)} @ ghost =
  fun s args t -> ghost_ (
    open_type_def args t; let opened = open_type args t in act_def s opened;
    let up = lift (length args) s in act_def up t;
    let changed = act up t in let actuals = act_arguments s args in
    open_type_def actuals changed;
    (match t with
     | Parameter i -> action_open_index s args i
     | Free _ | Boolean | Word64 -> ()
     | List_type a -> action_open_type s args a
     | Function (a, b) -> action_open_type s args a; action_open_type s args b);
    ())

let rec (split_index @ total) : (k : index) @ immutable ->
    (i : index) @ immutable -> {r : index option | match r with
      | None -> present k i | Some j -> i === add k j} @ immutable = fun k i ->
  match k with
  | Z -> ghost_ (add_def Z i); Some i
  | S k -> match i with
    | Z -> ghost_ (present_def (S k) Z); None
    | S i -> let r = split_index k i in
      ghost_ (present_def (S k) (S i);
        match r with None -> () | Some j -> add_def (S k) j);
      r

let rec (present_prefix @ total) : (cut : index) @ immutable ->
    (k : index) @ immutable -> (i : index) @ immutable ->
    {u : unit | present cut i} ->
    {u : unit | present (add cut k) i} @ ghost = fun cut k i premise -> ghost_ (
  present_def cut i; add_def cut k;
  let total = add cut k in present_def total i;
  (match cut with Z -> () | S cut -> match i with Z -> ()
    | S i -> present_prefix cut k i ());
  ())

let rec (shift_low @ total) : (cut : index) @ immutable ->
    (k : index) @ immutable -> (i : index) @ immutable ->
    {u : unit | present cut i} ->
    {u : unit | shift_index cut k i === i} @ ghost = fun cut k i premise -> ghost_ (
  present_def cut i; shift_index_def cut k i;
  (match cut with Z -> () | S cut -> match i with Z -> ()
    | S i -> shift_low cut k i ());
  ())

let (shift_high @ total) : (cut : index) @ immutable ->
    (k : index) @ immutable -> (i : index) @ immutable ->
    {u : unit | shift_index cut k (add cut i) === add (add cut k) i} @ ghost =
  fun cut k i -> ghost_ (
    Hm_abstraction_proofs.shift_added cut Z k i;
    Hm_abstraction_proofs.add_zero cut;
    shift_index_def Z k i;
    Hm_abstraction_proofs.add_assoc cut k i;
    ())

let rec (shift_under @ total) : (cut : index) @ immutable ->
    (k : index) @ immutable -> (t : mono) @ immutable ->
    {u : unit | shift cut k (shift Z cut t) === shift Z (add cut k) t} @ ghost =
  fun cut k t -> ghost_ (
    shift_def Z cut t; let inner = shift Z cut t in shift_def cut k inner;
    let total = add cut k in shift_def Z total t;
    (match t with
     | Parameter i -> shift_index_def Z cut i; shift_index_def Z total i;
       shift_high cut k i
     | Free _ | Boolean | Word64 -> ()
     | List_type a -> shift_under cut k a
     | Function (a, b) -> shift_under cut k a; shift_under cut k b);
    ())

let rec (action_shift @ total) : (s : substitution) @ immutable ->
    (cut : index) @ immutable -> (k : index) @ immutable -> (t : mono) @ immutable ->
    {u : unit | act (lift (add cut k) s) (shift cut k t)
      === shift cut k (act (lift cut s) t)} @ ghost = fun s cut k t -> ghost_ (
  shift_def cut k t;
  let shifted = shift cut k t in let small = lift cut s in
  let total = add cut k in let large = lift total s in
  act_def large shifted; act_def small t;
  let changed = act small t in shift_def cut k changed;
  (match t with
   | Parameter i ->
     (match split_index cut i with
      | None -> shift_low cut k i (); present_prefix cut k i ();
        at_lift_low cut s i (); at_lift_low total s i ();
        shift_def cut k (Parameter i)
      | Some j -> shift_high cut k j; at_lift_high cut s j;
        at_lift_high total s j;
        let base = at s.front s.tail j in shift_under cut k base)
   | Free _ | Boolean | Word64 -> ()
   | List_type a -> action_shift s cut k a
     | Function (a, b) -> action_shift s cut k a; action_shift s cut k b);
  ())

let rec (context_lift_weaken @ total) : (k : index) @ immutable ->
    (s : substitution) @ immutable -> (g : context) @ immutable ->
    {u : unit | act_context (lift k s) (weaken_context k g)
      === weaken_context k (act_context s g)} @ ghost = fun k s g -> ghost_ (
  weaken_context_def k g; act_context_def s g;
  let weak = weaken_context k g in let up = lift k s in act_context_def up weak;
  let changed = act_context s g in weaken_context_def k changed;
  (match g with Empty_context -> () | Binding (scheme, rest) ->
    weaken_scheme_def k scheme; act_scheme_def s scheme;
    let ws = weaken_scheme k scheme in act_scheme_def up ws;
    let cs = act_scheme s scheme in weaken_scheme_def k cs;
    (match scheme with Forall (m, t) ->
      lift_add m k s; action_shift s m k t);
    context_lift_weaken k s rest);
  ())

module R = Hm_interpreter_typing

let (scheme_open @ total) : (s : substitution) @ immutable ->
    (scheme : scheme) @ immutable -> (args : arguments) @ immutable ->
    {u : unit | length args === arity scheme} ->
    {u : unit | act s (open_scheme scheme args) ===
      open_scheme (act_scheme s scheme) (act_arguments s args)} @ ghost =
  fun s scheme args premise -> ghost_ (
    arity_def scheme; open_scheme_def scheme args; act_scheme_def s scheme;
    let changed = act_scheme s scheme in let actuals = act_arguments s args in
    open_scheme_def changed actuals;
    (match scheme with Forall (_, t) -> action_open_type s args t);
    ())

let rec (typing_action @ total) : (s : substitution) @ immutable ->
    (g : context) @ immutable -> (e : term) @ immutable ->
    (t : mono) @ immutable -> (d : typing) @ immutable ->
    {u : unit | R.typed g e t d} ->
    {u : unit | R.typed (act_context s g) e (act s t) (act_typing s d)} @ ghost =
  fun s g e t d premise -> ghost_ (
    R.typed_def g e t d; act_typing_def s d; act_def s t;
    let cg = act_context s g in let ct = act s t in let cd = act_typing s d in
    R.typed_def cg e ct cd;
    (match d with
     | Variable args -> (match e with Bound i ->
       lookup_action s g i; length_action s args;
       (match lookup g i with None -> () | Some scheme ->
         scheme_open s scheme args ();
         arity_def scheme; act_scheme_def s scheme;
         let changed = act_scheme s scheme in arity_def changed)
       | _ -> ())
     | Constant -> act_def s Boolean
     | Word_constant -> act_def s Word64
     | Empty_list a -> act_def s (List_type a)
     | List_cons (a, head, tail) -> (match e with
       | Cons (h, r) -> typing_action s g h a head ();
         typing_action s g r t tail (); act_def s (List_type a)
       | _ -> ())
     | List_case (a, scrutinee, empty, nonempty) -> (match e with
       | CaseList (v, l, r) ->
         typing_action s g v (List_type a) scrutinee ();
         typing_action s g l t empty ();
         let tail = Forall (Z, List_type a) in let head = Forall (Z, a) in
         let tg = Binding (tail, g) in let bg = Binding (head, tg) in
         typing_action s bg r t nonempty ();
         act_context_def s bg; act_context_def s tg;
         act_scheme_def s head; act_scheme_def s tail; lift_def Z s;
         act_def s (List_type a)
       | _ -> ())
     | Conditional (condition, yes, no) -> (match e with
       | If (c, a, b) -> typing_action s g c Boolean condition ();
         typing_action s g a t yes (); typing_action s g b t no ();
         act_def s Boolean
       | _ -> ())
     | Word_primitive (left, right) -> (match e with
       | Primitive (op, a, b) -> typing_action s g a Word64 left ();
         typing_action s g b Word64 right (); act_def s Word64;
         operation_type_def op;
         (match op with Add | Subtract -> () | Equal_word | Unsigned_less -> act_def s Boolean)
       | _ -> ())
     | Abstraction (a, body) -> (match e, t with
       | Lambda e, Function (_, b) ->
         let scheme = Forall (Z, a) in let bg = Binding (scheme, g) in
         typing_action s bg e b body ();
         act_context_def s bg; act_scheme_def s scheme; lift_def Z s
       | _ -> ())
     | Application (a, left, right) -> (match e with
       | Apply (f, x) -> typing_action s g f (Function (a, t)) left ();
         typing_action s g x a right (); act_def s (Function (a, t))
       | _ -> ())
     | Recursion (a, b, body) -> (match e with
       | Recursive e ->
         let self = Forall (Z, t) in let arg = Forall (Z, a) in
         let sg = Binding (self, g) in let bg = Binding (arg, sg) in
         typing_action s bg e b body ();
         act_context_def s bg; act_context_def s sg;
         act_scheme_def s self; act_scheme_def s arg; lift_def Z s
       | _ -> ())
     | Let_binding (scheme, rhs, body) -> (match e, scheme with
       | Let (r, b), Forall (k, a) ->
         let up = lift k s in let rg = weaken_context k g in
         typing_action up rg r a rhs ();
         context_lift_weaken k s g;
         let bg = Binding (scheme, g) in typing_action s bg b t body ();
         act_context_def s bg; act_scheme_def s scheme
       | _ -> ()));
    ())

let[@def] (act_judgement @ total) (s : substitution @ immutable)
    (j : R.judgement @ immutable) = match j with
  | R.Value (v, t) -> R.Value (v, act s t)
  | R.Environment (env, g) -> R.Environment (env, act_context s g)

let rec (transport @ total) : (s : substitution) @ immutable ->
    (j : R.judgement) @ immutable -> (p : R.evidence) @ immutable ->
    {u : unit | R.valid p j} ->
    {q : R.evidence | R.valid q (act_judgement s j)} @ immutable ghost =
  fun s j p premise -> ghost_ (
    R.valid_def p j; act_judgement_def s j;
    match p, j with
    | R.Leaf, R.Value ((R.True | R.False), Boolean) ->
      act_def s Boolean; R.valid_def R.Leaf (act_judgement s j); R.Leaf
    | R.Leaf, R.Value (R.Word _, Word64) ->
      act_def s Word64; R.valid_def R.Leaf (act_judgement s j); R.Leaf
    | R.Leaf, R.Value (R.Nil, List_type a) ->
      act_def s (List_type a); R.valid_def R.Leaf (act_judgement s j); R.Leaf
    | R.Elements (head, tail), R.Value (R.Cons (h, r), List_type a) ->
      let hp = transport s (R.Value (h, a)) head () in
      let rp = transport s (R.Value (r, List_type a)) tail () in
      act_judgement_def s (R.Value (h, a));
      act_judgement_def s (R.Value (r, List_type a)); act_def s (List_type a);
      let q = R.Elements (hp, rp) in R.valid_def q (act_judgement s j); q
    | R.Leaf, R.Environment (R.Empty, Empty_context) ->
      act_context_def s Empty_context;
      R.valid_def R.Leaf (R.Environment (R.Empty, Empty_context)); R.Leaf
    | R.Capture (g, d, captured_proof),
        R.Value (R.Closure (body, env), Function (a, b)) ->
      let cp = transport s (R.Environment (env, g)) captured_proof () in
      act_judgement_def s (R.Environment (env, g));
      let arg = Forall (Z, a) in let bg = Binding (arg, g) in
      typing_action s bg body b d ();
      act_context_def s bg; act_scheme_def s arg; lift_def Z s;
      act_def s (Function (a, b));
      let q = R.Capture (act_context s g, act_typing s d, cp) in
      let target = act_judgement s j in R.valid_def q target; q
    | R.Capture (g, d, captured_proof),
        R.Value (R.Recursive_closure (body, env), Function (a, b)) ->
      let cp = transport s (R.Environment (env, g)) captured_proof () in
      act_judgement_def s (R.Environment (env, g));
      let t = Function (a, b) in let arg = Forall (Z, a) in
      let self = Forall (Z, t) in let sg = Binding (self, g) in
      let bg = Binding (arg, sg) in typing_action s bg body b d ();
      act_context_def s bg; act_context_def s sg;
      act_scheme_def s arg; act_scheme_def s self; lift_def Z s;
      act_def s t;
      let q = R.Capture (act_context s g, act_typing s d, cp) in
      let target = act_judgement s j in R.valid_def q target; q
    | R.Extend (head, rest),
        R.Environment (R.Bind (v, env), Binding (Forall (k, a), g)) ->
      let up = lift k s in let scheme = Forall (k, a) in
      let hp = transport up (R.Value (v, a)) head () in
      let rp = transport s (R.Environment (env, g)) rest () in
      act_judgement_def up (R.Value (v, a));
      act_judgement_def s (R.Environment (env, g));
      act_context_def s (Binding (scheme, g)); act_scheme_def s scheme;
      let q = R.Extend (hp, rp) in
      let target = act_judgement s j in R.valid_def q target; q
    | _ -> unreachable_ ())

let rec (weakening_under @ total) : (cut : index) @ immutable ->
    (k : index) @ immutable -> (t : mono) @ immutable ->
    {u : unit | act (lift cut {front = No_arguments; tail = k}) t
      === shift cut k t} @ ghost = fun cut k t -> ghost_ (
  let s = {front = No_arguments; tail = k} in let up = lift cut s in
  act_def up t; shift_def cut k t;
  (match t with
   | Parameter i -> (match split_index cut i with
     | None -> at_lift_low cut s i (); shift_low cut k i ()
     | Some j -> at_lift_high cut s j; shift_high cut k j;
       at_def No_arguments k j;
       let total = add k j in shift_def Z cut (Parameter total);
       shift_index_def Z cut total; Hm_abstraction_proofs.add_assoc cut k j)
   | Free _ | Boolean | Word64 -> ()
   | List_type a -> weakening_under cut k a
     | Function (a, b) -> weakening_under cut k a; weakening_under cut k b);
  ())

let rec (weakening_context @ total) : (k : index) @ immutable ->
    (g : context) @ immutable ->
    {u : unit | act_context {front = No_arguments; tail = k} g
      === weaken_context k g} @ ghost = fun k g -> ghost_ (
  let s = {front = No_arguments; tail = k} in
  act_context_def s g; weaken_context_def k g;
  (match g with Empty_context -> () | Binding (scheme, rest) ->
    act_scheme_def s scheme; weaken_scheme_def k scheme;
    (match scheme with Forall (m, t) -> weakening_under m k t);
    weakening_context k rest);
  ())

let (weaken_environment @ total) : (k : index) @ immutable ->
    (env : R.value) @ immutable -> (g : context) @ immutable ->
    (p : R.evidence) @ immutable -> {u : unit | R.valid p (R.Environment (env, g))} ->
    {q : R.evidence | R.valid q (R.Environment (env, weaken_context k g))}
      @ immutable ghost = fun k env g p premise -> ghost_ (
  let s = {front = No_arguments; tail = k} in let j = R.Environment (env, g) in
  let q = transport s j p () in
  act_judgement_def s j; weakening_context k g; q)

let (instantiate @ total) : (args : arguments) @ immutable ->
    (v : R.value) @ immutable -> (t : mono) @ immutable ->
    (p : R.evidence) @ immutable -> {u : unit | R.valid p (R.Value (v, t))} ->
    {q : R.evidence | R.valid q (R.Value (v, open_type args t))}
      @ immutable ghost = fun args v t p premise -> ghost_ (
  let s = {front = args; tail = Z} in let j = R.Value (v, t) in
  let q = transport s j p () in
  act_judgement_def s j; action_open args t; q)
