module D = Hm_declarative
module Check = Hm_elaboration_check

type assignments = Empty | Slot of D.mono option * assignments [@@inductive]

let[@def] rec (empty @ total) (count : D.index @ immutable) =
  match count with D.Z -> Empty | D.S n -> Slot (None, empty n)

let[@def] rec (drop @ total) (count : D.index @ immutable)
    (index : D.index @ immutable) =
  match count with
  | D.Z -> Some index
  | D.S n -> match index with D.Z -> None | D.S i -> drop n i

let[@def] rec (get @ total) (slots : assignments @ immutable) (index : D.index @ immutable) =
  match slots, index with Empty, _ -> None | Slot (value, _), D.Z -> value
  | Slot (_, rest), D.S index -> get rest index
let[@def] rec (width @ total) (slots : assignments @ immutable) = ghost_ (
  match slots with Empty -> D.Z | Slot (_, rest) -> D.S (width rest))
let[@def] rec (extends @ total) (before : assignments @ immutable) (after : assignments @ immutable) = ghost_ (
  match before, after with
  | Empty, Empty -> true
  | Slot (old, rest), Slot (value, tail) -> (match old with None -> true | Some ty -> value === Some ty) && extends rest tail
  | _ -> false)
let rec (reflexive @ total) : (slots : assignments) @ immutable -> {u : unit | extends slots slots} @ ghost =
  fun slots -> ghost_ (extends_def slots slots; match slots with Empty -> () | Slot (_, rest) -> reflexive rest)
let rec (transitive @ total) : (a : assignments) @ immutable -> (b : assignments) @ immutable -> (c : assignments) @ immutable ->
    {u : unit | extends a b && extends b c} -> {u : unit | extends a c} @ ghost =
  fun a b c premise -> ghost_ (
    extends_def a b; extends_def b c; extends_def a c;
    match a, b, c with Slot (_, ar), Slot (_, br), Slot (_, cr) -> transitive ar br cr () | _ -> ())
let rec (extended_get @ total) : (before : assignments) @ immutable -> (after : assignments) @ immutable ->
    (index : D.index) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | extends before after && get before index === Some ty} -> {u : unit | get after index === Some ty} @ ghost =
  fun before after index ty premise -> ghost_ (
    extends_def before after; get_def before index; get_def after index;
    match before, after, index with Slot (_, rest), Slot (_, tail), D.S index -> extended_get rest tail index ty () | _ -> ())
let rec (extended_width @ total) : (before : assignments) @ immutable -> (after : assignments) @ immutable ->
    {u : unit | extends before after} -> {u : unit | width before === width after} @ ghost =
  fun before after premise -> ghost_ (
    extends_def before after; width_def before; width_def after;
    match before, after with Slot (_, rest), Slot (_, tail) -> extended_width rest tail () | _ -> ())
let[@def] rec (bind @ total) (index : D.index @ immutable) (ty : D.mono @ immutable) (slots : assignments @ immutable) =
  match slots with
  | Empty -> None
  | Slot (old, rest) -> match index with
    | D.Z -> (match old with None -> Some (Slot (Some ty, rest))
      | Some previous -> if Check.mono_equal previous ty then Some slots else None)
    | D.S i -> (match bind i ty rest with None -> None | Some tail -> Some (Slot (old, tail)))

let rec (bind_sound @ total) : (index : D.index) @ immutable -> (ty : D.mono) @ immutable -> (slots : assignments) @ immutable ->
    {u : unit | match bind index ty slots with None -> true | Some after -> extends slots after && get after index === Some ty} @ ghost =
  fun index ty slots -> ghost_ (
    bind_def index ty slots;
    match slots with Empty -> () | Slot (old, rest) ->
      (match index with
      | D.Z -> (match old with None -> reflexive rest; extends_def slots (Slot (Some ty, rest)); get_def (Slot (Some ty, rest)) index
        | Some previous -> let same = Check.mono_equal previous ty in if same then (reflexive slots; get_def slots index) else ())
      | D.S i -> bind_sound i ty rest;
        (match bind i ty rest with None -> () | Some tail -> extends_def slots (Slot (old, tail)); get_def (Slot (old, tail)) index)))
let[@def] rec (matching @ total) (arity : D.index @ immutable) (pattern : D.mono @ immutable)
    (target : D.mono @ immutable) (slots : assignments @ immutable) = ghost_ (
  match pattern with
  | D.Parameter index -> (match drop arity index with None -> get slots index === Some target | Some i -> target === D.Parameter i)
  | D.Free p -> target === D.Free p
  | D.Boolean -> target === D.Boolean
  | D.Word64 -> target === D.Word64
  | D.List_type a -> (match target with D.List_type b -> matching arity a b slots | _ -> false)
  | D.Function (a, b) -> (match target with D.Function (x, y) -> matching arity a x slots && matching arity b y slots | _ -> false))
let rec (extend_matching @ total) : (arity : D.index) @ immutable -> (pattern : D.mono) @ immutable -> (target : D.mono) @ immutable ->
    (before : assignments) @ immutable -> (after : assignments) @ immutable ->
    {u : unit | matching arity pattern target before && extends before after} -> {u : unit | matching arity pattern target after} @ ghost =
  fun arity pattern target before after premise -> ghost_ (
    matching_def arity pattern target before; matching_def arity pattern target after;
    match pattern with
    | D.Parameter index -> (match drop arity index with None -> extended_get before after index target () | Some _ -> ())
    | D.List_type a -> (match target with D.List_type b -> extend_matching arity a b before after () | _ -> ())
    | D.Function (a, b) -> (match target with D.Function (x, y) -> extend_matching arity a x before after (); extend_matching arity b y before after () | _ -> ())
    | _ -> ())
let[@def] rec (match_type @ total) (arity : D.index @ immutable) (pattern : D.mono @ immutable)
    (target : D.mono @ immutable) (slots : assignments @ immutable) =
  match pattern with
  | D.Parameter i -> (match drop arity i with None -> bind i target slots
    | Some i -> if Check.mono_equal (D.Parameter i) target then Some slots else None)
  | D.Free p -> (match target with D.Free q -> if Pref.equal p q then Some slots else None | _ -> None)
  | D.Word64 -> (match target with D.Word64 -> Some slots | _ -> None)
  | D.Boolean -> (match target with D.Boolean -> Some slots | _ -> None)
  | D.List_type a -> (match target with D.List_type b -> match_type arity a b slots | _ -> None)
  | D.Function (a, b) -> (match target with
    | D.Function (x, y) -> (match match_type arity a x slots with
      | None -> None | Some middle -> match_type arity b y middle)
    | _ -> None)

let rec (match_sound @ total) : (arity : D.index) @ immutable -> (pattern : D.mono) @ immutable -> (target : D.mono) @ immutable ->
    (slots : assignments) @ immutable ->
    {u : unit | match match_type arity pattern target slots with None -> true | Some after -> extends slots after && matching arity pattern target after} @ ghost =
  fun arity pattern target slots -> ghost_ (
    match_type_def arity pattern target slots; reflexive slots; matching_def arity pattern target slots;
    match pattern with
    | D.Parameter i -> (match drop arity i with None -> bind_sound i target slots;
      (match bind i target slots with None -> () | Some after -> matching_def arity pattern target after) | Some i -> let same = Check.mono_equal (D.Parameter i) target in if same then () else ())
    | D.Free p -> (match target with D.Free q -> let same = Pref.equal p q in if same then () else () | _ -> ())
    | D.List_type a -> (match target with D.List_type b -> match_sound arity a b slots;
      (match match_type arity a b slots with None -> () | Some after -> matching_def arity pattern target after) | _ -> ())
    | D.Function (a, b) -> (match target with D.Function (x, y) -> match_sound arity a x slots;
      (match match_type arity a x slots with None -> () | Some middle -> match_sound arity b y middle;
        (match match_type arity b y middle with None -> () | Some after ->
          transitive slots middle after (); extend_matching arity a x middle after (); matching_def arity pattern target after))
      | _ -> ())
    | _ -> ())

let[@def] rec (arguments @ total) (slots : assignments @ immutable) =
  match slots with
  | Empty -> D.No_arguments
  | Slot (ty, rest) ->
    let ty = match ty with None -> D.Boolean | Some ty -> ty in
    D.Argument (ty, arguments rest)

let rec (empty_width @ total) : (arity : D.index) @ immutable -> {u : unit | width (empty arity) === arity} @ ghost =
  fun arity -> ghost_ (empty_def arity; width_def (empty arity); match arity with D.Z -> () | D.S rest -> empty_width rest)
let rec (argument_count @ total) : (slots : assignments) @ immutable -> {u : unit | D.length (arguments slots) === width slots} @ ghost =
  fun slots -> ghost_ (arguments_def slots; width_def slots; D.length_def (arguments slots);
    match slots with Empty -> () | Slot (_, rest) -> argument_count rest)
let rec (open_get @ total) : (slots : assignments) @ immutable -> (index : D.index) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | get slots index === Some ty} -> {u : unit | D.open_index (arguments slots) index === ty} @ ghost =
  fun slots index ty premise -> ghost_ (
    get_def slots index; arguments_def slots; D.open_index_def (arguments slots) index;
    match slots, index with Slot (_, rest), D.S index -> open_get rest index ty () | _ -> ())
let rec (open_dropped @ total) : (slots : assignments) @ immutable -> (index : D.index) @ immutable -> (rest : D.index) @ immutable ->
    {u : unit | drop (width slots) index === Some rest} -> {u : unit | D.open_index (arguments slots) index === D.Parameter rest} @ ghost =
  fun slots index rest premise -> ghost_ (
    width_def slots; drop_def (width slots) index; arguments_def slots; D.open_index_def (arguments slots) index;
    match slots, index with Slot (_, tail), D.S index -> open_dropped tail index rest () | _ -> ())
let rec (realize @ total) : (arity : D.index) @ immutable -> (pattern : D.mono) @ immutable -> (target : D.mono) @ immutable ->
    (slots : assignments) @ immutable -> {u : unit | width slots === arity && matching arity pattern target slots} ->
    {u : unit | D.open_type (arguments slots) pattern === target} @ ghost =
  fun arity pattern target slots premise -> ghost_ (
    matching_def arity pattern target slots; D.open_type_def (arguments slots) pattern;
    match pattern with
    | D.Parameter index -> (match drop arity index with None -> open_get slots index target () | Some rest -> open_dropped slots index rest ())
    | D.List_type a -> (match target with D.List_type b -> realize arity a b slots () | _ -> ())
    | D.Function (a, b) -> (match target with D.Function (x, y) -> realize arity a x slots (); realize arity b y slots () | _ -> ())
    | _ -> ())

let[@def] (infer_impl @ total) (scheme : D.scheme @ immutable) (target : D.mono @ immutable) =
  match scheme with D.Forall (arity, pattern) ->
    match match_type arity pattern target (empty arity) with None -> None | Some slots -> Some (arguments slots)

let (infer @ total) :
    (scheme : D.scheme) @ immutable -> (target : D.mono) @ immutable ->
    {r : D.arguments option | r === infer_impl scheme target &&
      match r with None -> true | Some args -> D.length args === D.arity scheme && D.open_scheme scheme args === target} @ immutable =
  fun scheme target ->
    let out = infer_impl scheme target in
    ghost_ (infer_impl_def scheme target;
      match scheme with D.Forall (arity, pattern) ->
        match_sound arity pattern target (empty arity);
        match match_type arity pattern target (empty arity) with None -> () | Some slots ->
          empty_width arity; extended_width (empty arity) slots (); argument_count slots;
          realize arity pattern target slots (); D.arity_def scheme; D.open_scheme_def scheme (arguments slots));
    out

let rec (bind_complete @ total) : (index : D.index) @ immutable -> (ty : D.mono) @ immutable ->
    (slots : assignments) @ immutable -> (witness : assignments) @ immutable ->
    {u : unit | extends slots witness && get witness index === Some ty} ->
    {u : unit | match bind index ty slots with None -> false | Some after -> extends after witness} @ ghost =
  fun index ty slots witness premise -> ghost_ (
    extends_def slots witness; get_def witness index; bind_def index ty slots;
    match slots, witness with
    | Slot (old, rest), Slot (_, tail) -> (match index with
      | D.Z -> (match old with
        | None -> extends_def (Slot (Some ty, rest)) witness
        | Some previous -> let same = Check.mono_equal previous ty in if same then () else ())
      | D.S i -> bind_complete i ty rest tail ();
        (match bind i ty rest with None -> () | Some after -> extends_def (Slot (old, after)) witness))
    | _ -> ())

let rec (match_complete @ total) : (arity : D.index) @ immutable -> (pattern : D.mono) @ immutable ->
    (target : D.mono) @ immutable -> (slots : assignments) @ immutable -> (witness : assignments) @ immutable ->
    {u : unit | extends slots witness && matching arity pattern target witness} ->
    {u : unit | match match_type arity pattern target slots with None -> false | Some after -> extends after witness} @ ghost =
  fun arity pattern target slots witness premise -> ghost_ (
    matching_def arity pattern target witness; match_type_def arity pattern target slots;
    match pattern with
    | D.Parameter i -> (match drop arity i with None -> bind_complete i target slots witness () | Some j -> let same = Check.mono_equal (D.Parameter j) target in if same then () else ())
    | D.Free p -> (match target with D.Free q -> let same = Pref.equal p q in if same then () else () | _ -> ())
    | D.List_type a -> (match target with D.List_type b -> match_complete arity a b slots witness () | _ -> ())
    | D.Function (a, b) -> (match target with
      | D.Function (x, y) -> match_complete arity a x slots witness ();
        (match match_type arity a x slots with None -> () | Some middle -> match_complete arity b y middle witness ())
      | _ -> ())
    | _ -> ())

let[@def] rec (witness @ total) (args : D.arguments @ immutable) = ghost_ (
  match args with D.No_arguments -> Empty | D.Argument (ty, rest) -> Slot (Some ty, witness rest))

let rec (empty_extends @ total) : (args : D.arguments) @ immutable ->
    {u : unit | extends (empty (D.length args)) (witness args)} @ ghost =
  fun args -> ghost_ (
    D.length_def args; witness_def args; empty_def (D.length args);
    extends_def (empty (D.length args)) (witness args);
    match args with D.No_arguments -> () | D.Argument (_, rest) -> empty_extends rest)

let rec (witness_index @ total) : (args : D.arguments) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | match drop (D.length args) index with
      | None -> get (witness args) index === Some (D.open_index args index)
      | Some rest -> D.open_index args index === D.Parameter rest} @ ghost =
  fun args index -> ghost_ (
    D.length_def args; drop_def (D.length args) index; witness_def args;
    D.open_index_def args index; get_def (witness args) index;
    match args, index with D.Argument (_, tail), D.S i -> witness_index tail i | _ -> ())

let rec (witness_matching @ total) : (args : D.arguments) @ immutable -> (pattern : D.mono) @ immutable ->
    {u : unit | matching (D.length args) pattern (D.open_type args pattern) (witness args)} @ ghost =
  fun args pattern -> ghost_ (
    D.open_type_def args pattern; matching_def (D.length args) pattern (D.open_type args pattern) (witness args);
    match pattern with
    | D.Parameter index -> witness_index args index
    | D.List_type a -> witness_matching args a
    | D.Function (a, b) -> witness_matching args a; witness_matching args b
    | _ -> ())

let (complete @ total) : (scheme : D.scheme) @ immutable -> (target : D.mono) @ immutable ->
    (args : D.arguments) @ immutable ->
    {u : unit | D.length args === D.arity scheme && D.open_scheme scheme args === target} ->
    {u : unit | match infer scheme target with None -> false | Some _ -> true} @ ghost =
  fun scheme target args premise -> ghost_ (
    D.arity_def scheme; D.open_scheme_def scheme args; infer_impl_def scheme target;
    match scheme with D.Forall (arity, pattern) ->
      empty_extends args; witness_matching args pattern;
      match_complete arity pattern target (empty arity) (witness args) ())

let[@def] rec (assignments_wf @ total) (n : D.index @ immutable) (slots : assignments @ immutable) = ghost_ (
  match slots with Empty -> true | Slot (ty, rest) ->
    (match ty with None -> true | Some ty -> D.mono_wf n ty) && assignments_wf n rest)

let rec (empty_wf @ total) : (n : D.index) @ immutable -> (arity : D.index) @ immutable ->
    {u : unit | assignments_wf n (empty arity)} @ ghost =
  fun n arity -> ghost_ (
    empty_def arity; assignments_wf_def n (empty arity);
    match arity with D.Z -> () | D.S rest -> empty_wf n rest)

let rec (bind_wf @ total) : (n : D.index) @ immutable -> (index : D.index) @ immutable ->
    (ty : D.mono) @ immutable -> (slots : assignments) @ immutable ->
    {u : unit | D.mono_wf n ty && assignments_wf n slots} ->
    {u : unit | match bind index ty slots with None -> true | Some after -> assignments_wf n after} @ ghost =
  fun n index ty slots premise -> ghost_ (
    bind_def index ty slots; assignments_wf_def n slots;
    match slots, index with
    | Slot (_, rest), D.Z -> assignments_wf_def n (Slot (Some ty, rest))
    | Slot (old, rest), D.S i -> bind_wf n i ty rest ();
      (match bind i ty rest with None -> () | Some tail -> assignments_wf_def n (Slot (old, tail)))
    | _ -> ())

let rec (match_wf @ total) : (n : D.index) @ immutable -> (arity : D.index) @ immutable ->
    (pattern : D.mono) @ immutable -> (target : D.mono) @ immutable -> (slots : assignments) @ immutable ->
    {u : unit | D.mono_wf n target && assignments_wf n slots} ->
    {u : unit | match match_type arity pattern target slots with None -> true | Some after -> assignments_wf n after} @ ghost =
  fun n arity pattern target slots premise -> ghost_ (
    match_type_def arity pattern target slots; D.mono_wf_def n target;
    match pattern with
    | D.Parameter i -> (match drop arity i with None -> bind_wf n i target slots () | Some _ -> ())
    | D.List_type a -> (match target with D.List_type b -> match_wf n arity a b slots () | _ -> ())
    | D.Function (a, b) -> (match target with D.Function (x, y) ->
      match_wf n arity a x slots ();
      (match match_type arity a x slots with None -> () | Some middle -> match_wf n arity b y middle ())
      | _ -> ())
    | _ -> ())

let rec (arguments_wf @ total) : (n : D.index) @ immutable -> (slots : assignments) @ immutable ->
    {u : unit | assignments_wf n slots} -> {u : unit | D.arguments_wf n (arguments slots)} @ ghost =
  fun n slots premise -> ghost_ (
    assignments_wf_def n slots; arguments_def slots; D.arguments_wf_def n (arguments slots);
    match slots with Empty -> () | Slot (ty, rest) ->
      arguments_wf n rest (); match ty with None -> D.mono_wf_def n D.Boolean | Some _ -> ())

let (well_formed @ total) : (n : D.index) @ immutable -> (scheme : D.scheme) @ immutable -> (target : D.mono) @ immutable ->
    {u : unit | D.mono_wf n target} ->
    {u : unit | match infer scheme target with None -> true | Some args -> D.arguments_wf n args} @ ghost =
  fun n scheme target premise -> ghost_ (
    infer_impl_def scheme target;
    match scheme with D.Forall (arity, pattern) ->
      empty_wf n arity; match_wf n arity pattern target (empty arity) ();
      match match_type arity pattern target (empty arity) with None -> () | Some slots -> arguments_wf n slots ())
