(* vox: verification-condition generation and discharge.

   Runs as a separate pass over the FINAL typedtree (the type checker emits no VCs; it
   backtracks internally). Walks the tree carrying a logical environment of facts; each
   [refine_] node yields the VC [facts |- p[v := name of e]]; [assume_] is reported as
   RUNTIME CHECKED (translcore compiles a check of the predicate) and [assume_unchecked_]
   as ASSUMED; neither goes to the solver. Facts come from the channels DESIGN.md
   enumerates -- binders (their refinements and contracts), selfification equations,
   unpacking, path facts from [if], dependent application, and match facts (positive and
   negative) -- deduplicated at emission.

   VCs are discharged by a Lean 4 subprocess. Solver error, unknown, and timeout all count
   as verification FAILURE. *)

open Types
open Typedtree

(* How an obligation is discharged: [Prove] goes to the solver; [Runtime_check]
   ([assume_]) is checked at runtime by compiled code; [Assume] ([assume_unchecked_]) is
   trusted outright. *)
type vc_kind =
  | Prove
  | Runtime_check
  | Assume

type vc =
  { vc_loc : Location.t
  ; vc_facts : Refinement.pred list (* Pbound-free *)
  ; vc_fact_provs : Location.t option list
      (* the source span each fact in [vc_facts] originated from, PARALLEL to it (same
         length, same order); [None] where the fact has no meaningful span. Rendered only
         under [-vox-dump-vc-provenance]. *)
  ; vc_goal : Refinement.pred (* Pbound-free *)
  ; vc_goal_prov : Location.t option
      (* the span of the refinement/annotation that induced the obligation; defaults to
         the obligation site ([vc_loc]). *)
  ; vc_kind : vc_kind
  }

(* Declaration sorts for logical names, per DESIGN.md: int as Int, bool as Bool, simple
   variants as solver datatypes, anything else at a single uninterpreted sort. *)
type dsort =
  | S_int
  | S_bool
  | S_data of Path.t * dsort list
    (* a "simple" variant/record, modelled with the datatype theory, INSTANTIATED at its
       type arguments' sorts. The declaration is registered once per path (generically,
       with [S_param]s standing for its type parameters); a USE mentions the concrete
       argument sorts here, e.g. [int mylist] is [S_data (mylist, [S_int])]. *)
  | S_param of int
    (* the [i]th type parameter of the datatype declaration currently being registered.
       INTERNAL to the registry: it appears only in a [dt_decl]'s field sorts, never in a
       USE-site sort (uses have no parameters in scope, so a type variable there degrades
       to [S_other]). *)
  | S_poly of Path.t * dsort list
    (* an instance of a [@@vox.poly] parameterized ABSTRACT type, modelled at a
       PARAMETERIZED opaque sort (one [opaque Vox_<t> : Type -> ... -> Type] per head)
       instantiated at the type arguments' sorts -- the abstract-carrier counterpart of a
       parameterized [S_data]. The point is elaboration: a sort-polymorphic ghost
       ([opaque cts {a : Type} : Vox_t a -> List a]) applied to such a value has its type
       argument INFERRED from the value's sort, so one polymorphic spec serves every
       element type -- including facts (lengths, ghost- to-ghost equations) that mention
       no element-sorted term. *)
  | S_tuple of dsort list
    (* an unlabeled tuple, modelled with one polymorphic product datatype per ARITY
       (VoxT2, VoxT3, ...) instantiated at the component sorts *)
  | S_iarray
    (* [int iarray], modelled by the built-in theory: an opaque sort VoxIA with
       Vox_ia_len/Vox_ia_get (Refinement.ia_len/ia_get) and the length-nonnegativity
       axiom, emitted when used *)
  | S_lean of string * dsort list
    (* a GHOST SORT: a block-defined Lean type, named VERBATIM by the string
       ([type iset [@@vox.sort lean "ISet"]] renders as [ISet]). Opaque to vox -- like
       [S_other] but carrying a caller-chosen Lean name instead of VoxU; the block is the
       grammar police for its every use. *)
  | S_arrow of dsort * dsort
    (* a FUNCTION type, modelled as the Lean arrow over its domain and codomain sorts
       ([int -> int -> bool] renders [Int -> Int -> Prop], since [bool] models at [Prop]).
       This is what lets a RELATION parameter [(r : (int -> int -> bool))] carry a genuine
       arrow sort, so a reflected lambda ([Refinement.Plam]) substituted for it, and the
       spec functions applied to it, are well typed. Curried arrows nest right. *)
  | S_other

(* Failure diagnostics show what the solver was given, so a failed obligation can be
   understood without re-running under -dump-vc. When one source name covers several
   stamps within a VC (shadowing), later ones display as name#2, name#3, ... in order of
   appearance, so a hypothesis about a shadowed variable cannot read as identical to the
   goal it fails to prove. *)
let display_fun_of_preds preds =
  let seen : (string, Ident.t list) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun p ->
      List.iter
        (fun id ->
          let name = Ident.name id in
          let ids =
            try Hashtbl.find seen name with
            | Not_found -> []
          in
          if not (List.exists (Ident.same id) ids)
          then Hashtbl.replace seen name (ids @ [ id ]))
        (Refinement.free_vars p))
    preds;
  fun id ->
    let name = Ident.name id in
    match Hashtbl.find_opt seen name with
    | Some (_ :: _ :: _ as ids) ->
      let rec index i = function
        | [] -> name
        | id' :: _ when Ident.same id id' ->
          if i = 1 then name else Printf.sprintf "%s#%d" name i
        | _ :: rest -> index (i + 1) rest
      in
      index 1 ids
    | _ -> name
;;

let vc_display_fun vc = display_fun_of_preds (vc.vc_goal :: vc.vc_facts)
let with_vc_display vc k = Refinement.with_var_display (vc_display_fun vc) k

let hyps_for_error vc =
  with_vc_display vc (fun () ->
    match vc.vc_facts with
    | [] -> "\nHypotheses: <none>"
    | fs ->
      "\nHypotheses:"
      ^ String.concat "" (List.map (fun f -> "\n  " ^ Refinement.to_string f) fs))
;;

let goal_for_error vc = with_vc_display vc (fun () -> Refinement.to_string vc.vc_goal)
let vcs : vc list ref = ref []

(* Toplevel sessions keep the legacy self-contained solver inputs (everything spliced);
   file compilation uses MODULE-MODE inputs ([module] header, [public import VoxCore] and
   the sig modules of imports). Set by the toplevel entry point. *)
let toplevel_active = ref false
let name_sorts : (Ident.t, dsort) Hashtbl.t = Hashtbl.create 64

(* gap #31: idents bound at their via SKELETON sort by a value binding, with the via
   type's maps. When such a binder feeds a via PARAMETER, its dependent name is the
   composite map applied to it (the image the parameter's contract speaks over), not the
   bare skeleton stamp. Image-bound via values (parameters, sealed abstracts) are absent
   from this table and keep their bare name. *)
let via_skel_binders : (Ident.t, Types.vox_map list) Hashtbl.t = Hashtbl.create 16

(* Source-level type of each recorded name, formatted -- populated only under the
   provenance dump flag, where the editor shows a VC's variables with their OxCaml type
   next to their solver sort. *)
let name_types : (Ident.t, string) Hashtbl.t = Hashtbl.create 64

(* Where each pattern-bound name was BOUND (the variable in the pattern), for the editor's
   hover-the-context-row highlight. Populated at [extend_pat] under the provenance flag;
   names minted elsewhere (synthetics, versions) simply have no entry. *)
let name_locs : (Ident.t, Location.t) Hashtbl.t = Hashtbl.create 64

(* Fresh unknowns minted by the pass itself; always "in scope". Numbered so distinct
   unknowns are distinguishable in diagnostics. *)
let synthetic_names : (Ident.t, unit) Hashtbl.t = Hashtbl.create 16
let unknown_counter = ref 0

(* Module-level values named by this unit's VCs ([Pglobal]s): sort per path, with the
   import's .cmi refinement as a global fact (see [register_global]). *)
let globals : (string, Path.t * dsort) Hashtbl.t = Hashtbl.create 16
let global_types : (string, string) Hashtbl.t = Hashtbl.create 16
let global_facts : Refinement.pred list ref = ref []

(* The solver-side declaration of a "simple" type: a variant becomes a free datatype; a
   record becomes a single-constructor datatype with named selectors (a Lean [structure]). *)
type dt_decl =
  | Dt_variant of int * (string * dsort list) list
    (* type-parameter count, then (constructor, field sorts). Field sorts may be
       [S_param i] for the declaration's [i]th parameter. *)
  | Dt_record of int * (string * dsort) list
    (* type-parameter count, then (label, sort). *)
  | Dt_opaque
(* an abstract type at its OWN uninterpreted sort ([@@vox.sort opaque]) rather than at the
   shared VoxU: declared [opaque Vox_<path> : Type], so interface blocks can state laws
   about values of exactly this type. A sealed implementation's concrete declaration
   registers the real datatype under the SAME solver name, which is how the seal's
   re-elaborated interface lands on the concrete type. *)

(* Simple-variant/record datatypes used by the current module's (or toplevel session's)
   VCs, in dependency order (the datatypes of a datatype's fields precede it;
   self-recursion is fine). Mutual recursion is not supported: detecting a back-edge
   POISONS the type being registered, which then sorts as [S_other] everywhere (sound:
   facts about its structure become ill-sorted and verification fails). *)
let datatypes : (Path.t * dt_decl) list ref = ref []
let registering : Path.t list ref = ref []
let poisoned : Path.t list ref = ref []
let find_datatype p = List.find_opt (fun (q, _) -> Path.same p q) !datatypes

(* Tuple ARITIES in use (types of names, tuple terms in predicates): each needs its
   product datatype (VoxT<n>) declared. Tuples are structural, so unlike [datatypes] there
   is nothing to render per type -- the arity determines the declaration. *)
let tuple_arities : int list ref = ref []

let register_tuple_arity n =
  if not (List.mem n !tuple_arities) then tuple_arities := !tuple_arities @ [ n ]
;;

let register_pred_tuple_arities p =
  List.iter register_tuple_arity (Refinement.tuple_arities p)
;;

(* [@@vox.poly] heads in use, each needing its parameterized opaque declared
   ([opaque Vox_<t> : Type -> ... -> Type]). Like [tuple_arities], there is nothing to
   render per INSTANCE -- the head and its arity determine the declaration; instances
   differ only in the argument sorts at the use site. *)
let poly_heads : (Path.t * int) list ref = ref []

let register_poly_head p n =
  if not (List.exists (fun (q, _) -> Path.same p q) !poly_heads)
  then poly_heads := !poly_heads @ [ p, n ]
;;

(* Reflected definitions ([total_] bindings) of the current module (or toplevel session),
   in definition order; emitted into the solver input between the [-vox-prelude] and the
   module's own embedded blocks, so those blocks may state lemmas about them (a prelude
   FILE precedes them: a definition may call an imported reflected function). *)
let spec_defs : Vox_reflect.spec_def list ref = ref []

(* [@@vox.lemma] exports: for each lemma, its solver-side name, the rendered Lean
   [theorem] + [grind_pattern] text, and its source location (for error attribution). *)
let lemma_defs : (string * string * Location.t) list ref = ref []

(* [-vox-explain-proofs]: the lemmas grind reported using to close each PROVED obligation,
   keyed by the VC's position among the [Prove]-kind VCs passed to [run_lean] (in order).
   [Some []] means grind closed it by arithmetic/logic alone; a missing key means the
   solver was not run or the suggestion could not be attributed. Populated by [run_lean]
   only when the flag is on and the whole file verified; read by [dump_vc]. *)
let used_lemmas : (int, string list) Hashtbl.t = Hashtbl.create 16

(* Per-VC verdicts for a FAILED solve, keyed by position among the [Prove] VCs (as
   [used_lemmas] is). Populated by [run_lean] only when [-vox-dump-vc-provenance] is on
   and the solve failed with all errors attributable to VC theorems: a VC whose theorem
   line carried no error is "proved"; the first failing one is classified ("disproved" if
   a counterexample validated, else "unproved") and the rest are "unproved". Read by
   [dump_vc], so the editor sees which obligations still hold when one fails. *)
let vc_verdicts : (int, string) Hashtbl.t = Hashtbl.create 16

(* Program-point states for the editor ([-vox-dump-states]): the scope-filtered fact
   context at the ENTRY of each expression the walker visits, keyed by the expression's
   span. The innermost span containing the cursor is the proof state of "here". *)
let point_states
  : (Location.t * (Refinement.pred * Location.t option) list * Ident.t list) list ref
  =
  ref []
;;

(* Module-level binders: excluded from a point's variable list (they are the module's
   interface, not local context -- listing every earlier top-level function at every point
   drowned the view). *)
let toplevel_names : (Ident.t, unit) Hashtbl.t = Hashtbl.create 16

(* Signatures of lemmas already registered THIS unit (ident + number of precondition
   hypotheses), so a later lemma's body can call an earlier one and the v2 translator can
   emit the right proof arguments. *)
let lemma_sigs : (Ident.t * int) list ref = ref []

(* Embedded solver blocks ([%%vox.lean ...]) of the module (or toplevel session) being
   verified, in source order: text (ending in a newline) and the block's location (solver
   errors inside a block are reported there). See the collection functions below. *)
let embedded_blocks : (string * Location.t) list ref = ref []

(* Solver blocks imported from other units' .cmis ([%%vox.lean] in their interfaces): unit
   name and blocks, in dependency order (a unit's blocks after the units it imports).
   Gathered from the persistent env at verification time; the definition travels with the
   defining module, so a client can never verify against a DIFFERENT version of a spec
   function used in an imported signature (the .cmi CRC forces re-verification when the
   spec changes). *)
let imported_specs : (string * Cmi_format.vox_spec_export) list ref = ref []

(* SSA versions for [let mutable] variables (flow-sensitive mutation). [mut_versions] maps
   each LIVE mutable binder to its current logical version -- a synthetic ident, so always
   in scope -- together with the binder's declared type; reads name the version and every
   write mints a fresh one. A version's facts are eternal truths about a VALUE, never
   about the cell, so they may flow anywhere downstream on the control path; the walker
   threads contexts (and saves/restores this table around branches) so they flow nowhere
   else. [mut_counts] numbers versions per binder for display and never rolls back. *)
let mut_versions : (Ident.t, Ident.t * Types.type_expr) Hashtbl.t = Hashtbl.create 16
let mut_counts : (Ident.t, int) Hashtbl.t = Hashtbl.create 16

(* Definitional equations [version = rhs-name], one per assignment. Unlike the
   declared-refinement instances (which are theorems proved under the assignment's path
   condition and stay path-scoped), these are Skolem-style definitions -- each version is
   defined once, as a function of strictly earlier names -- so adding them is a
   conservative extension in EVERY execution: an execution that never performs the
   assignment simply interprets the version by its equation. They are pulled into each VC
   by relevance (emit_vc). *)
let mut_defs : Refinement.pred list ref = ref []

(* Sanitized solver name -> the [path_uname] that claimed it; see the collision guard in
   [register_global]. *)
let global_snames : (string, string) Hashtbl.t = Hashtbl.create 16

let reset () =
  vcs := [];
  Hashtbl.reset name_sorts;
  Hashtbl.reset via_skel_binders;
  Hashtbl.reset name_types;
  Hashtbl.reset name_locs;
  Hashtbl.reset synthetic_names;
  Hashtbl.reset mut_versions;
  Hashtbl.reset mut_counts;
  mut_defs := [];
  datatypes := [];
  registering := [];
  poisoned := [];
  tuple_arities := [];
  poly_heads := [];
  spec_defs := [];
  lemma_defs := [];
  Hashtbl.reset used_lemmas;
  Hashtbl.reset vc_verdicts;
  point_states := [];
  Hashtbl.reset toplevel_names;
  lemma_sigs := [];
  unknown_counter := 0;
  embedded_blocks := [];
  imported_specs := [];
  Hashtbl.reset globals;
  Hashtbl.reset global_types;
  Hashtbl.reset global_snames;
  global_facts := []
;;

(* A STABLE string for a type path: no stamps, and a path rooted in the current unit is
   prefixed with the unit's name, so the same type gets the same solver-side name in its
   defining module and in every client (a [-vox-prelude] can then refer to it). Distinct
   paths that map to the same string (e.g. types in shadowed local modules) are detected
   at registration and rejected. *)
let rec path_uname (p : Path.t) =
  match p with
  | Path.Pident id ->
    if Ident.is_global_or_predef id
    then Ident.name id
    else (
      (* At the toplevel (and in expect tests) there is no unit: the name is used bare, so
         a session type [t] is [Vox_t], not the puzzling [Vox__t]. *)
      match Env.get_current_unit_name () with
      | "" -> Ident.name id
      | u -> u ^ "." ^ Ident.name id)
  | Path.Pdot (q, s) -> path_uname q ^ "." ^ s
  | Path.Papply (q, r) -> path_uname q ^ "(" ^ path_uname r ^ ")"
  | Path.Pextra_ty (q, _) -> path_uname q ^ ".#extra"
;;

(* An abstract type may declare that its LOGICAL REPRESENTATIVE is its value at a base
   sort ([@@vox.sort int]): values of the type are modelled as opaque Ints (or Bools)
   rather than at VoxU, so refinements can use them directly as the values they stand for
   -- ghost types whose denotation IS the value (prophecies; refs denoting their
   contents). TRUSTED: the declaring library asserts that every fact it issues about such
   values is true of that interpretation. The attribute must appear on the declaration in
   both the interface and the implementation (sorts are computed per-compilation from the
   visible declaration).

   [@@vox.sort opaque] instead gives the abstract type its OWN uninterpreted sort, named
   by path, so an interface block can declare model constants over exactly this type. NOT
   trusted: the sort carries no facts of its own, and the one asymmetry it permits -- an
   attributed interface over a concrete implementation -- is the sealed-abstraction
   pattern, sound because the concrete sort is one model of the opaque sort and every
   interface fact is either a checked contract or a sealed obligation.

   [@@vox.sort lean "ISet"] instead names a block-defined Lean type (see [S_lean]): the
   value is modelled at that named sort, opaque to vox but a real type in the module's
   blocks. *)
type sort_attr =
  | Sa_sort of dsort
  | Sa_opaque

(* A ghost sort's Lean name is rendered VERBATIM into binder declarations and predicate
   translation, so a malformed name would corrupt the solver input. Reject it eagerly
   (trust doctrine: a malformed ghost declaration is an error, not a silent VoxU
   degradation): the name must be a non-empty dotted Lean identifier -- each dot-separated
   segment starts with a letter or [_] and continues with letters, digits, [_] or [']
   (namespaced names like [Foo.Bar] are allowed; [Set Int], [""], [123] are not). *)
let validate_lean_sort_name ~loc name =
  let ident_seg seg =
    String.length seg > 0
    && (match seg.[0] with
        | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
        | _ -> false)
    && String.for_all
         (function
           | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
           | _ -> false)
         seg
  in
  if String.length name = 0
  then Location.raise_errorf ~loc "vox: vox.sort lean requires a non-empty Lean type name"
  else if not (List.for_all ident_seg (String.split_on_char '.' name))
  then
    Location.raise_errorf
      ~loc
      "vox: %S is not a valid Lean type name for vox.sort lean"
      name
    (* The emitter's sanitizer owns the [Vox_] (datatypes/tuples/opaques) and [v_]
       (reflected values) namespaces; a ghost name is rendered VERBATIM, so one in those
       namespaces could silently ALIAS a datatype's or value's emitted name (e.g.
       [lean "Vox_foo"] captured by the datatype [foo]'s [Vox_foo]). Reject it -- fail
       closed -- rather than let the collision pass unnoticed. *)
  else if String.length name >= 4 && String.equal (String.sub name 0 4) "Vox_"
  then
    Location.raise_errorf
      ~loc
      "vox: %S may not name a ghost sort -- the Vox_ prefix is reserved        for the \
       solver's emitted datatype names (it would collide)"
      name
  else if String.length name >= 2 && String.equal (String.sub name 0 2) "v_"
  then
    Location.raise_errorf
      ~loc
      "vox: %S may not name a ghost sort -- the v_ prefix is reserved        for the \
       solver's emitted value names (it would collide)"
      name;
  name
;;

let vox_sort_of_attribute (a : Parsetree.attribute) =
  if not (String.equal a.attr_name.txt "vox.sort")
  then None
  else (
    match a.attr_payload with
    | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] ->
      (match e.pexp_desc with
       | Pexp_ident { txt = Longident.Lident "int"; _ } -> Some (Sa_sort S_int)
       | Pexp_ident { txt = Longident.Lident "bool"; _ } -> Some (Sa_sort S_bool)
       | Pexp_ident { txt = Longident.Lident "opaque"; _ } -> Some Sa_opaque
       (* A ghost sort naming a block-defined Lean type. *)
       | Pexp_apply
           ( { pexp_desc = Pexp_ident { txt = Longident.Lident "lean"; _ }; _ }
           , [ ( Nolabel
               , { pexp_desc =
                     Pexp_constant { pconst_desc = Pconst_string (name, _, _); _ }
                 ; _
                 } )
             ] ) ->
         Some (Sa_sort (S_lean (validate_lean_sort_name ~loc:a.attr_loc name, [])))
       | Pexp_ident { txt = Longident.Lident s; _ } ->
         Location.raise_errorf
           ~loc:a.attr_loc
           "vox: unknown vox.sort %S (expected \"int\", \"bool\", \"opaque\", or lean \
            \"Name\")"
           s
       | _ ->
         Location.raise_errorf
           ~loc:a.attr_loc
           "vox: vox.sort takes a sort name (int, bool, opaque) or lean \"Name\"")
    | _ ->
      Location.raise_errorf
        ~loc:a.attr_loc
        "vox: vox.sort takes a single sort name, e.g. [@@@@vox.sort int]")
;;

(* Eager validation: a malformed [@@vox.sort] is an error even when no value of the type
   ever reaches a VC (a typo on a ghost type must not be silent), and so is the attribute
   on a pure ALIAS, where it would be silently ignored (sorting expands aliases to their
   definition first). Run wherever declarations pass by. *)
let validate_vox_sort_attributes ?(alias = false) (attrs : Parsetree.attributes) =
  List.iter
    (fun (a : Parsetree.attribute) ->
      match vox_sort_of_attribute a with
      | None -> ()
      | Some _ ->
        if alias
        then
          Location.raise_errorf
            ~loc:a.attr_loc
            "vox: vox.sort on a type alias has no effect (an alias expands to its \
             definition before sorting); put the attribute on the definition")
    attrs
;;

(* Is [arg_sorts] the declaration's own parameters, in order --
   [S_param 0; ...; S_param (arity-1)]? A recursive occurrence at exactly these is REGULAR
   (a uniform-parameter inductive); anything else (permuted, nested, or specialized
   parameters) is not a simple datatype. *)
let regular_self_args arg_sorts arity =
  List.length arg_sorts = arity
  && List.for_all2
       (fun s i ->
         match s with
         | S_param j -> Int.equal j i
         | _ -> false)
       arg_sorts
       (List.init arity Fun.id)
;;

(* Solver-side names are stamp-free: reject a distinct path that would alias an
   already-registered datatype's name. *)
(* Solver-side names go through [lean_sanitize], which maps every non-word char to '_':
   NOT injective (A.B.c and A_B.c collide). A collision would emit two same-named Lean
   binders -- the later SHADOWS the earlier while both hypotheses still attach, making the
   context inconsistent and every goal provable. So distinct paths whose SANITIZED names
   coincide are rejected outright, for values ([register_global]) and datatypes
   ([assert_uname_fresh]) alike. *)
let lean_sanitize s =
  let b = Bytes.of_string s in
  Bytes.iteri
    (fun i c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> ()
      | _ -> Bytes.set b i '_')
    b;
  Bytes.to_string b
;;

let assert_uname_fresh p =
  List.iter
    (fun (q, _) ->
      if String.equal (lean_sanitize (path_uname p)) (lean_sanitize (path_uname q))
         && not (String.equal (path_uname p) (path_uname q))
      then
        Location.raise_errorf
          ~loc:(Location.in_file !Location.input_name)
          "vox: types %s and %s would share the solver-side name %s; rename one of them"
          (path_uname q)
          (path_uname p)
          (lean_sanitize (path_uname p));
      if String.equal (path_uname p) (path_uname q)
      then
        Location.raise_errorf
          ~loc:(Location.in_file !Location.input_name)
          "vox: two distinct types would share the solver-side name %s; rename one of \
           them"
          (path_uname p))
    !datatypes
;;

(* TRUSTED opt-in: a parameterized abstract type declared [@@vox.poly] sorts its instances
   at a parameterized opaque, so sort-polymorphic ghosts over it elaborate at every
   instantiation (see [S_poly]). Like [@@vox.sort], the declaring library asserts that its
   ghost story is consistent per instantiation; the sorts themselves stay uninterpreted,
   so distinct instantiations can never exchange facts. *)
let vox_poly_attribute env p =
  match Env.find_type p env with
  | exception Not_found -> false
  | decl ->
    List.exists
      (fun (a : Parsetree.attribute) -> String.equal a.attr_name.txt "vox.poly")
      decl.type_attributes
;;

(* The sort of the type at path [p] applied to argument sorts [arg_sorts], registering it
   as a datatype (with its field datatypes, recursively) on first sight. [arg_sorts]
   instantiates the declaration's parameters at the USE; the declaration itself is
   registered generically (its field sorts mention [S_param]). *)
let rec datatype_sort env p arg_sorts =
  if Path.same p Predef.path_int
  then S_int
  else if Path.same p Predef.path_bool
  then S_bool
  else (
    match vox_sort_attribute env p arg_sorts with
    | Some s -> s
    | None -> datatype_sort_unattributed env p arg_sorts)

(* The declared refines component of [p]'s kind: [type t : value refines (...)], or the
   trusted [@@vox.sort int|bool] attribute, which Typedecl folds into the same field.
   [arg_sorts] instantiates [p]'s parameters at the use, so a [Vs_param] in the declared
   sort resolves to the concrete argument sort (or [S_other] if the use is under-applied).
   [@@vox.sort opaque] stays ATTRIBUTE-ONLY (never folded into the kind): the inclusion
   check then sees Vr_top on the interface side and accepts a concrete implementation --
   the sealed-abstraction asymmetry, sound because the concrete sort is one model of the
   opaque sort and every interface fact is either a checked contract or a sealed
   obligation. *)
and vox_sort_attribute env p arg_sorts =
  match Env.find_type p env with
  | exception Not_found -> None
  | decl ->
    (match Jkind.get_vox_refines decl.type_jkind with
     | Vr_sort vs -> Some (dsort_of_vox_sort env arg_sorts vs)
     | Vr_top ->
       (* Decl rebuilds along some typedecl paths drop the kind field; the attribute on
          the declaration is authoritative there. *)
       (match List.find_map vox_sort_of_attribute decl.type_attributes with
        | Some (Sa_sort (S_lean (name, []))) when arg_sorts <> [] ->
          (* The attribute carries no arity, so [vox_sort_of_attribute] yields the bare
             ghost name; a parameterized ghost declared this way applies to the use's
             argument sorts (which mirror its parameters positionally), exactly as the
             [Vr_sort] path does through [dsort_of_vox_sort]. *)
          Some (S_lean (name, arg_sorts))
        | Some (Sa_sort s) -> Some s
        | Some Sa_opaque ->
          (* arity-0 only: one uninterpreted sort cannot distinguish instantiations of a
             parameterized type *)
          if decl.type_params = [] then Some (datatype_sort_opaque p) else Some S_other
        | None -> None))

(* Turn a declared refinement sort into a solver sort, registering any datatype/tuple it
   mentions. [Vs_data] registers the MODELED datatype (so an abstract type modeled as a
   datatype lets clients use its constructors in predicates); [Vs_param i] reads the use's
   [arg_sorts]. *)
and dsort_of_vox_sort env arg_sorts (vs : Types.vox_sort) =
  match vs with
  | Vs_int -> S_int
  | Vs_bool -> S_bool
  | Vs_param i ->
    (match List.nth_opt arg_sorts i with
     | Some s -> s
     | None -> S_other)
  | Vs_tuple ss ->
    register_tuple_arity (List.length ss);
    S_tuple (List.map (dsort_of_vox_sort env arg_sorts) ss)
  | Vs_data (p, ss) -> datatype_sort env p (List.map (dsort_of_vox_sort env arg_sorts) ss)
  | Vs_opaque -> S_other
  | Vs_lean (name, args) -> S_lean (name, List.map (dsort_of_vox_sort env arg_sorts) args)
  (* The invariant is a FACT, not part of the modeling: values are modeled at the
     underlying sort (registered as usual). *)
  | Vs_fact (s, _) -> dsort_of_vox_sort env arg_sorts s

(* [@@vox.sort opaque]: register the path at its own uninterpreted sort. In an
   implementation whose concrete declaration was already registered (the attribute lives
   on the interface only), the concrete registration wins -- Path.same, same uname. *)
and datatype_sort_opaque p =
  if find_datatype p <> None
  then S_data (p, [])
  else (
    assert_uname_fresh p;
    datatypes := !datatypes @ [ p, Dt_opaque ];
    S_data (p, []))

and datatype_sort_unattributed env p arg_sorts =
  if List.exists (Path.same p) !poisoned
  then S_other
  else if find_datatype p <> None
  then S_data (p, arg_sorts)
  else if List.exists (Path.same p) !registering
  then (
    match !registering with
    | q :: _ when Path.same p q ->
      (* self-recursion: regular only if the occurrence is at the declaration's own
         parameters, in order. A non-regular (nested/permuted) recursion is not a simple
         datatype: poison it, so it -- and every use -- sorts as [S_other] (sound). *)
      let arity =
        match Env.find_type p env with
        | decl -> List.length decl.type_params
        | exception Not_found -> List.length arg_sorts
      in
      if regular_self_args arg_sorts arity
      then S_data (p, arg_sorts)
      else (
        poisoned := p :: !poisoned;
        S_other)
    | _ ->
      (* mutual recursion: poison the back-edge's target *)
      poisoned := p :: !poisoned;
      S_other)
  else (
    registering := p :: !registering;
    (* The pop must survive exceptions: at the toplevel the vox globals persist across
       phrases, and a stale [registering] entry would spuriously poison later phrases as
       mutual recursion. *)
    let decl =
      Fun.protect
        ~finally:(fun () -> registering := List.tl !registering)
        (fun () ->
          (* Classify constructor argument / field types in an environment where the
             declaration's own type parameters map positionally to [S_param i] (shared
             type-variable nodes, from the same declaration as the arguments). *)
          match Ctype.vox_simple_variant env p with
          | Some (params, cstrs) ->
            Some
              (Dt_variant
                 ( List.length params
                 , List.map
                     (fun (cd : Types.constructor_declaration) ->
                       ( Ident.name cd.cd_id
                       , List.map
                           (dsort_of_type ~params env)
                           (Types.tys_of_constr_args cd.cd_args) ))
                     cstrs ))
          | None ->
            (match Ctype.vox_simple_record env p with
             | Some (params, lbls) ->
               Some
                 (Dt_record
                    ( List.length params
                    , List.map
                        (fun (ld : Types.label_declaration) ->
                          Ident.name ld.ld_id, dsort_of_type ~params env ld.ld_type)
                        lbls ))
             | None -> None))
    in
    match decl with
    | None -> S_other
    | Some decl ->
      if List.exists (Path.same p) !poisoned
      then S_other
      else (
        assert_uname_fresh p;
        datatypes := !datatypes @ [ p, decl ];
        S_data (p, arg_sorts)))

(* [params] is the list of the declaration's type-variable nodes when classifying a
   datatype's fields (empty at every USE site): a type variable found among them sorts as
   [S_param i], its declaration parameter; a type variable at a use site is not among any
   [params] and degrades to [S_other]. *)
and dsort_of_type ?(visited = []) ?(params = []) env ty =
  let ty = Ctype.vox_expand_head env ty in
  (* A -rectypes cycle can run through a tuple with no nominal type on the path;
     revisiting a node degrades to the uninterpreted sort (sound). *)
  if List.mem (get_id ty) visited
  then S_other
  else (
    let visited = get_id ty :: visited in
    let param_index ty =
      let id = get_id ty in
      let rec find i = function
        | [] -> None
        | p :: rest -> if Int.equal (get_id p) id then Some i else find (i + 1) rest
      in
      find 0 params
    in
    match get_desc ty with
    | Tvar _ ->
      (match param_index ty with
       | Some i -> S_param i
       | None -> S_other)
    | Tconstr (p, [], _) -> datatype_sort env p []
    | Tconstr (p, [ elt ], _)
      when Path.same p Predef.path_iarray
           &&
           match get_desc (Ctype.vox_expand_head env elt) with
           | Tconstr (e, [], _) -> Path.same e Predef.path_int
           | _ -> false -> S_iarray
    | Tconstr (p, args, _) ->
      (* A parameterized head sorts as its datatype instantiated at the arguments' sorts
         (registered generically on first sight); a declared [refines] applies to every
         instance, and its own [Vs_param]s resolve against these same argument sorts. *)
      let arg_sorts = List.map (dsort_of_type ~visited ~params env) args in
      (match vox_sort_attribute env p arg_sorts with
       | Some s -> s
       | None ->
         if vox_poly_attribute env p
         then (
           (* [@@vox.poly] overrides the structural story even when the implementation
              side is a record: the trusted .ml's carriers must sort at the SAME opaque as
              the .mli's abstract types, or its ascriptions would be ill-sorted against
              its own ghosts. *)
           register_poly_head p (List.length arg_sorts);
           S_poly (p, arg_sorts))
         else datatype_sort env p arg_sorts)
    | Trefine (skel, maps, _) ->
      (* image-binder: a via type DENOTES at the composite image (the last map's target
         sort), so a binder of it is that image. The image sort is read from each map's
         TARGET TYPE ([vm_target]), not the stored [vm_sort]: [Subst] instantiates
         [vm_target] at a use ([int t]'s manifest carries [int iset]), so a PARAMETERIZED
         via renders its image at the right arguments ([(ISet Int)]), whereas [vm_sort] is
         generic ([Vs_param]) with no argument sorts in scope at this node. *)
      let skel_sort = dsort_of_type ~visited ~params env skel in
      List.iter
        (fun m -> ignore (dsort_of_type ~visited ~params env m.Types.vm_target : dsort))
        maps;
      (match List.rev maps with
       | [] -> skel_sort
       | last :: _ -> dsort_of_type ~visited ~params env last.Types.vm_target)
    | Ttuple comps
      when List.length comps >= 2
           && List.for_all (fun (lbl, _) -> Option.is_none lbl) comps ->
      register_tuple_arity (List.length comps);
      S_tuple (List.map (fun (_, t) -> dsort_of_type ~visited ~params env t) comps)
    | Tarrow (_, dom, cod, _) ->
      (* A function type models at the Lean arrow over its domain/codomain sorts, so a
         relation parameter [(r : (int -> int -> bool))] carries [Int -> Int -> Prop] and
         a reflected lambda / applied [r] is well typed. The domain is wrapped in a
         [Tpoly] mono; strip it. *)
      let strip t =
        match get_desc (Ctype.vox_expand_head env t) with
        | Tpoly (t', []) -> t'
        | _ -> t
      in
      S_arrow
        ( dsort_of_type ~visited ~params env (strip dom)
        , dsort_of_type ~visited ~params env cod )
    | _ -> S_other)
;;

(* One-line rendering of a type for the dump's scope section (the parse there is
   line-based, so Format's margin breaks must go). *)
let type_one_line ty =
  String.concat
    " "
    (List.filter
       (fun s -> s <> "")
       (String.split_on_char '\n' (Format.asprintf "%a" Printtyp.type_expr ty)
        |> List.map String.trim))
;;

let record_name ?(via_skel = false) env id ty =
  (* A transparent via binder at a value binding is registered at its SKELETON sort (gap
     #31): the binder is the plain payload in the logic, exactly as a [refine_] unpack
     binds it, so its construction and callee-contract facts (base sort) are well sorted.
     Only a KNOWN (spine-visible) [Trefine] is routed here; an abstract [refines] value
     keeps its image sort. *)
  let sort_ty =
    if via_skel
    then (
      match Types.get_desc (Ctype.vox_expand_head env ty) with
      | Trefine (skel, _ :: _, _) -> skel
      | _ -> ty)
    else ty
  in
  Hashtbl.replace name_sorts id (dsort_of_type env sort_ty);
  if !Clflags.vox_dump_vc_provenance
  then (
    (* The context display strips a TOP-level refinement: it has already become a logical
       hypothesis, so the row shows the skeleton (arrow contracts inside the type are kept
       -- they are not hypotheses). *)
    let display_ty =
      match Types.get_desc (Ctype.vox_expand_head env ty) with
      | Trefine (skel, _, _) -> skel
      | _ -> ty
    in
    Hashtbl.replace name_types id (type_one_line display_ty))
;;

(* Register the datatypes of any constructor application in [p]. Called wherever a
   predicate enters the fact/goal stream; a path that fails to register (not a simple
   variant here, or mutually recursive) is caught at discharge time. *)
let register_pred_paths env p =
  List.iter (fun q -> ignore (datatype_sort env q [])) (Refinement.constr_paths p);
  register_pred_tuple_arities p
;;

(* Register a [total_] binding: translate its body into an equation-style definition
   (Vox_reflect.translate_def) and queue it for emission. Solver-side names are the source
   names, so two reflected functions may not share one; the definition's datatypes are
   registered so its emission never degrades to VoxU. *)
let register_spec_def env (vb : Typedtree.value_binding) =
  let d = Vox_reflect.translate_def vb in
  List.iter
    (fun (d' : Vox_reflect.spec_def) ->
      if String.equal d'.sd_name d.sd_name
      then
        Location.raise_errorf
          ~loc:d.sd_loc
          "vox: two reflected functions would share the solver-side name %s; rename one \
           of them"
          d.sd_name)
    !spec_defs;
  List.iter (fun p -> ignore (datatype_sort env p [])) (Vox_reflect.def_datatype_paths d);
  List.iter
    register_pred_tuple_arities
    (Vox_reflect.body_preds
       (Option.to_list d.Vox_reflect.sd_decreases)
       d.Vox_reflect.sd_body);
  spec_defs := !spec_defs @ [ d ]
;;

(* Register the datatypes an exported refinement is ABOUT: refined skeletons plus
   constructor applications in the predicates. Used to compute the .cmi's spec export, so
   a client that never mentions these types itself still receives their declarations
   alongside the spec blocks that reference them. The walk is structural (like
   [uses_vox]): a refinement hidden behind a type alias is missed, so its datatype is not
   exported -- a client whose spec needs it then fails at the solver (closed), never
   falsely verifies. *)
let register_type_specs env ty =
  let rec go ty visited =
    if List.memq ty visited
    then ()
    else (
      let visited = ty :: visited in
      match get_desc ty with
      | Trefine (skel, maps, p) ->
        ignore (dsort_of_type env skel : dsort);
        (* register each via layer's target datatype, so its declaration and the map
           functions that mention it reach the solver; read from [vm_target] (the
           instantiated type), so a parameterized target registers at the right argument
           datatypes *)
        List.iter (fun m -> ignore (dsort_of_type env m.Types.vm_target : dsort)) maps;
        register_pred_paths env p;
        go skel visited
      | Tarrow (_, a, r, _) ->
        go a visited;
        go r visited
      | Tconstr (p, _ :: _, _) when vox_poly_attribute env p ->
        (* A [@@vox.poly] head registers even UNREFINED (a prophecy parameter): the unit's
           block declares ghosts at its parameterized sort, so clients need its opaque
           declared whether or not any refinement mentions the type. *)
        ignore (dsort_of_type env ty : dsort);
        List.iter (fun t -> go t visited) (Vox_dep.children ty)
      | _ -> List.iter (fun t -> go t visited) (Vox_dep.children ty))
  in
  go ty []
;;

let has_vox_attr name attrs =
  List.exists (fun (a : Parsetree.attribute) -> String.equal a.attr_name.txt name) attrs
;;

(* image-binder support: a via type's predicate is stored at the BASE sort, mentioning the
   image as [composite _]. Consuming a binder reads the IMAGE (the binder IS the composite
   image); the skeleton's own invariant conjuncts (still over the bare base [_]) drop,
   reached only through a [refine_] unpack. *)
let via_composite maps =
  List.fold_left
    (fun acc (m : Types.vox_map) -> Refinement.Pfun (m.Types.vm_fn, [ acc ]))
    Refinement.Pbound
    maps
;;

let rec pred_mentions_bound (p : Refinement.pred) =
  match p with
  | Refinement.Pbound -> true
  | Refinement.Pvar _ | Refinement.Pglobal _ | Refinement.Pint _ | Refinement.Pbool _ ->
    false
  | Refinement.Pconstr (_, _, args) | Refinement.Pfun (_, args) | Refinement.Ptuple args
    -> List.exists pred_mentions_bound args
  | Refinement.Pfield (_, _, a)
  | Refinement.Pis (_, _, a)
  | Refinement.Pproj (_, _, a)
  | Refinement.Pnot a
  | Refinement.Pquant (_, _, a)
  | Refinement.Plam (_, a) -> pred_mentions_bound a
  | Refinement.Pbinop (_, a, b)
  | Refinement.Pand (a, b)
  | Refinement.Por (a, b)
  | Refinement.Pimp (a, b) -> pred_mentions_bound a || pred_mentions_bound b
;;

let rec replace_subterm ~find ~by (p : Refinement.pred) =
  if Refinement.equal p find
  then by
  else (
    match p with
    | Refinement.Pbound
    | Refinement.Pvar _
    | Refinement.Pglobal _
    | Refinement.Pint _
    | Refinement.Pbool _ -> p
    | Refinement.Pconstr (path, c, args) ->
      Refinement.Pconstr (path, c, List.map (replace_subterm ~find ~by) args)
    | Refinement.Pfun (f, args) ->
      Refinement.Pfun (f, List.map (replace_subterm ~find ~by) args)
    | Refinement.Pfield (path, l, a) ->
      Refinement.Pfield (path, l, replace_subterm ~find ~by a)
    | Refinement.Ptuple args ->
      Refinement.Ptuple (List.map (replace_subterm ~find ~by) args)
    | Refinement.Pproj (n, i, a) -> Refinement.Pproj (n, i, replace_subterm ~find ~by a)
    | Refinement.Pis (path, c, a) -> Refinement.Pis (path, c, replace_subterm ~find ~by a)
    | Refinement.Pbinop (op, a, b) ->
      Refinement.Pbinop (op, replace_subterm ~find ~by a, replace_subterm ~find ~by b)
    | Refinement.Pand (a, b) ->
      Refinement.Pand (replace_subterm ~find ~by a, replace_subterm ~find ~by b)
    | Refinement.Por (a, b) ->
      Refinement.Por (replace_subterm ~find ~by a, replace_subterm ~find ~by b)
    | Refinement.Pnot a -> Refinement.Pnot (replace_subterm ~find ~by a)
    | Refinement.Pimp (a, b) ->
      Refinement.Pimp (replace_subterm ~find ~by a, replace_subterm ~find ~by b)
    | Refinement.Pquant (q, bd, a) ->
      Refinement.Pquant (q, bd, replace_subterm ~find ~by a)
    | Refinement.Plam (bs, a) -> Refinement.Plam (bs, replace_subterm ~find ~by a))
;;

let rec conjuncts (p : Refinement.pred) =
  match p with
  | Refinement.Pand (a, b) -> conjuncts a @ conjuncts b
  | _ -> [ p ]
;;

let via_image_facts maps pred id =
  let composite = via_composite maps in
  let p = replace_subterm ~find:composite ~by:(Refinement.Pvar id) pred in
  List.filter (fun c -> not (pred_mentions_bound c)) (conjuncts p)
;;

(* gap #31: in a KNOWN via binder's base predicate, a free variable other than the binder
   itself is there only because a dependent substitution put an argument where an
   IMAGE-sorted parameter stood (the image layer's preds are the only place such a var
   lands). If that argument is itself a skeleton-bound via value, its bare stamp is ill
   sorted at the image; rewrite it to the composite map applied to it ([once] ->
   [lrepr once]), the image the layer speaks over. The binder [id] is EXCLUDED: it
   legitimately appears at the skeleton in the invariant conjuncts ([bst id]). *)
let rec rewrite_skel_via_images ~except (p : Refinement.pred) =
  let go = rewrite_skel_via_images ~except in
  let under_own_map f v =
    (* [v] as the direct argument of its own innermost (first) map is already at the image
       -- leave it, do not re-apply the map. *)
    match Hashtbl.find_opt via_skel_binders v with
    | Some (m :: _) -> String.equal m.Types.vm_fn f
    | _ -> false
  in
  match p with
  | Refinement.Pvar v when not (List.exists (Ident.same v) except) ->
    (match Hashtbl.find_opt via_skel_binders v with
     | Some maps ->
       List.fold_left
         (fun acc (m : Types.vox_map) -> Refinement.Pfun (m.Types.vm_fn, [ acc ]))
         (Refinement.Pvar v)
         maps
     | None -> p)
  | Refinement.Pvar _
  | Refinement.Pbound
  | Refinement.Pglobal _
  | Refinement.Pint _
  | Refinement.Pbool _ -> p
  | Refinement.Pconstr (path, c, args) -> Refinement.Pconstr (path, c, List.map go args)
  | Refinement.Pfun (f, [ Refinement.Pvar v ]) when under_own_map f v -> p
  | Refinement.Pfun (f, args) -> Refinement.Pfun (f, List.map go args)
  | Refinement.Pfield (path, l, a) -> Refinement.Pfield (path, l, go a)
  | Refinement.Ptuple args -> Refinement.Ptuple (List.map go args)
  | Refinement.Pproj (n, i, a) -> Refinement.Pproj (n, i, go a)
  | Refinement.Pis (path, c, a) -> Refinement.Pis (path, c, go a)
  | Refinement.Pbinop (op, a, b) -> Refinement.Pbinop (op, go a, go b)
  | Refinement.Pand (a, b) -> Refinement.Pand (go a, go b)
  | Refinement.Por (a, b) -> Refinement.Por (go a, go b)
  | Refinement.Pnot a -> Refinement.Pnot (go a)
  | Refinement.Pimp (a, b) -> Refinement.Pimp (go a, go b)
  | Refinement.Pquant (q, bd, a) -> Refinement.Pquant (q, bd, go a)
  | Refinement.Plam (bs, a) -> Refinement.Plam (bs, go a)
;;

(* The refinement of a type, if any. *)
let refinement_of_type env ty =
  match get_desc (Ctype.vox_expand_head env ty) with
  | Trefine (_, _, p) -> Some p
  | _ -> None
;;

(* The declared INVARIANTS of a type's modeling:
   [type nat : value refines (int{ _ >= 0 })] gives every binder of type [nat] the closed
   fact [_ >= 0], even though [nat] is abstract and never expands to a [Trefine]. The head
   decl's [refines] is consulted like the [Tconstr] modeling arm does; the predicate is
   closed, so a parameterized head's instantiation touches only the (discarded) sort. A
   [Trefine] skeleton is followed so a written [nat{ ... }] collects the head invariant
   alongside its own refinement. *)
let invariant_preds env ty =
  let rec facts_of_vox_sort acc (vs : Types.vox_sort) =
    match vs with
    | Vs_fact (s, pred) -> facts_of_vox_sort (pred :: acc) s
    | Vs_int | Vs_bool | Vs_tuple _ | Vs_data _ | Vs_param _ | Vs_opaque | Vs_lean _ ->
      acc
  in
  let rec go ty =
    match get_desc (Ctype.vox_expand_head env ty) with
    | Trefine (skel, _, _) -> go skel
    | Tconstr (p, _, _) ->
      (match Env.find_type p env with
       | exception Not_found -> []
       | decl ->
         (match Jkind.get_vox_refines decl.type_jkind with
          | Vr_sort vs -> facts_of_vox_sort [] vs
          | Vr_top -> []))
    | _ -> []
  in
  go ty
;;

(* The refinement of an arrow PARAMETER type (a contract, DESIGN.md), looking under the
   [Tpoly] wrapper arrow domains carry. A genuinely polymorphic domain (non-empty univars)
   is NOT a contract -- typing leaves those rigid -- so the walker must not report one: it
   would emit obligations typing never stripped for. *)
let param_refinement env ty =
  match get_desc (Ctype.vox_expand_head env ty) with
  | Tpoly (t, []) -> refinement_of_type env t
  | Trefine (_, _, p) -> Some p
  | _ -> None
;;

let mut_read id =
  Option.map (fun (v, _) -> Refinement.Pvar v) (Hashtbl.find_opt mut_versions id)
;;

let mut_fresh env id ty =
  let k =
    match Hashtbl.find_opt mut_counts id with
    | Some k -> k + 1
    | None -> 0
  in
  Hashtbl.replace mut_counts id k;
  let name = if k = 0 then Ident.name id else Printf.sprintf "%s@%d" (Ident.name id) k in
  let v = Ident.create_local name in
  record_name env v ty;
  Hashtbl.replace synthetic_names v ();
  Hashtbl.replace mut_versions id (v, ty);
  v
;;

(* The declared refinement, instantiated at a fresh version: sound because rigid typing
   forced every write (and the initialization) through [refine_] at that type. *)
let mut_invariant env ty v =
  match refinement_of_type env ty with
  | Some p ->
    register_pred_paths env p;
    [ Refinement.subst_bound ~by:(Refinement.Pvar v) p ]
  | None -> []
;;

(* [m <- e] and initialization: the fresh version's definitional equation joins the global
   stream (sound everywhere); the declared refinement is returned for the PATH-SCOPED
   context. *)
let mut_assign env id ty ~rhs =
  let v = mut_fresh env id ty in
  mut_defs := Refinement.Pbinop (Refinement.Eq, Refinement.Pvar v, rhs) :: !mut_defs;
  mut_invariant env ty v
;;

(* Havoc: a fresh, unconstrained version (joins, loops, and constructs the walker does not
   model). Only the declared refinement survives: it holds at every program point. *)
let mut_havoc env id =
  match Hashtbl.find_opt mut_versions id with
  | None -> []
  | Some (_, ty) ->
    let v = mut_fresh env id ty in
    mut_invariant env ty v
;;

let save_versions () = Hashtbl.fold (fun k v acc -> (k, v) :: acc) mut_versions []

let restore_versions saved =
  Hashtbl.reset mut_versions;
  List.iter (fun (k, v) -> Hashtbl.replace mut_versions k v) saved
;;

let version_in snapshot id =
  List.find_map (fun (k, (v, _)) -> if Ident.same k id then Some v else None) snapshot
;;

(* The mutable variables (tracked at this point) that [e] assigns anywhere in its subtree.
   Complete because closures cannot capture mutable variables: every mutation is a
   syntactic [Texp_setmutvar]. *)
let written_mutables (e : expression) =
  let acc = ref [] in
  let it =
    { Tast_iterator.default_iterator with
      expr =
        (fun sub e' ->
          (match e'.exp_desc with
           | Texp_setmutvar ({ txt = id; _ }, _, _) ->
             if Hashtbl.mem mut_versions id && not (List.exists (Ident.same id) !acc)
             then acc := id :: !acc
           | _ -> ());
          Tast_iterator.default_iterator.expr sub e')
    }
  in
  it.expr it e;
  !acc
;;

let mut_havoc_written env e = List.concat_map (mut_havoc env) (written_mutables e)

(* The tracked mutable variables [e] READS ([Texp_mutvar]) anywhere in its subtree;
   complete for the same reason as [written_mutables]. *)
let read_mutables (e : expression) =
  let acc = ref [] in
  let it =
    { Tast_iterator.default_iterator with
      expr =
        (fun sub e' ->
          (match e'.exp_desc with
           | Texp_mutvar { txt = id; _ } ->
             if Hashtbl.mem mut_versions id && not (List.exists (Ident.same id) !acc)
             then acc := id :: !acc
           | _ -> ());
          Tast_iterator.default_iterator.expr sub e')
    }
  in
  it.expr it e;
  !acc
;;

(* Havoc facts for one unordered CHILD (application arguments, let-and right-hand sides,
   generic traversal): only the subtree-written variables the child itself reads get a
   fresh version. A child blind to a variable needs no name for it, and skipping the mint
   keeps version numbering readable. Call with the version table already restored to the
   construct's entry state. *)
let sibling_havoc env ~written child =
  List.concat_map
    (mut_havoc env)
    (List.filter (fun id -> List.exists (Ident.same id) written) (read_mutables child))
;;

(* Loop invariants ([@vox.invariant p]): a FORMULA over program variables, living in the
   logical environment -- not a refinement type: it never travels and is never compared.
   The elaborated template is instantiated at each boundary point by closing every mutable
   mention over the variable's current version (Thrust-style: the logic only ever sees
   stable names). Discipline (the classical quadruple): ASSERT over the entry versions;
   havoc; ASSUME over the head versions; ASSERT over the body-exit versions at the
   back-edge; after the loop, the head assumption stands alongside the negated guard. *)
let loop_invariant (e : expression) =
  let all =
    List.filter_map
      (fun (a : Parsetree.attribute) ->
        if String.equal a.attr_name.txt "vox.invariant"
        then (
          match a.attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (pred, _); _ } ] -> Some (pred, a.attr_loc)
          | _ ->
            Location.raise_errorf
              ~loc:a.attr_loc
              "vox: malformed [@vox.invariant] payload (expected a predicate)")
        else None)
      e.exp_attributes
  in
  match all with
  | [] -> None
  | (_, loc0) :: _ -> Some (List.map fst all, loc0)
;;

(* Close a formula template over the current versions of the mutable variables it
   mentions. *)
let close_over_versions p =
  Hashtbl.fold
    (fun id (v, _) p -> Refinement.subst_var id ~by:(Refinement.Pvar v) p)
    mut_versions
    p
;;

(* [ienv] is the environment the formula elaborates in: the loop expression's for a while
   loop, the BODY's for a for loop (where the index is bound). *)
let elab_loop_invariant ienv (e : expression) =
  match loop_invariant e with
  | None -> None
  | Some (preds, attr_loc) ->
    (* several [@vox.invariant] attributes conjoin *)
    let elab pred =
      let template, mentioned = Typetexp.elab_vox_invariant ienv pred in
      List.iter
        (fun id ->
          if not (Hashtbl.mem mut_versions id)
          then
            Location.raise_errorf
              ~loc:attr_loc
              "vox: the invariant mentions the mutable variable %s, which is not tracked \
               here (is it defined outside the enclosing function?)"
              (Ident.name id))
        mentioned;
      register_pred_paths ienv template;
      template
    in
    let template =
      match List.map elab preds with
      | [] -> assert false
      | t :: ts -> List.fold_left (fun acc t' -> Refinement.Pand (acc, t')) t ts
    in
    Some (template, attr_loc)
;;

(* Facts contributed by the binders of a pattern: every binder is recorded (for its
   declaration sort); binders of refined type contribute their refinement instantiated at
   the binder. *)
let binder_facts
  : type k. ?via_skel:bool -> Env.t -> k general_pattern -> Refinement.pred list
  =
  fun ?(via_skel = false) env pat ->
  List.concat_map
    (fun (id, _, ty, _, _) ->
      record_name ~via_skel env id ty;
      match get_desc (Ctype.vox_expand_head env ty) with
      | Trefine (_, (_ :: _ as maps), pred) ->
        let facts =
          if via_skel
          then (
            (* gap #31: bound at the skeleton, so inject the FULL base-sort predicate
               ([bst _ && <image contract> _] instantiated at the binder) -- the same
               facts a [refine_] unpack contributes: both the skeleton invariant and the
               map-link to the image the RHS established. Other via-skel vars in it
               (dependent- substituted arguments) are rewritten to their image. *)
            Hashtbl.replace via_skel_binders id maps;
            let fact =
              rewrite_skel_via_images
                ~except:[ id ]
                (Refinement.subst_bound ~by:(Refinement.Pvar id) pred)
            in
            register_pred_paths env fact;
            [ fact ])
          else (
            let facts = via_image_facts maps pred id in
            List.iter (register_pred_paths env) facts;
            facts)
        in
        let inv =
          List.map
            (fun p ->
              register_pred_paths env p;
              Refinement.subst_bound ~by:(Refinement.Pvar id) p)
            (invariant_preds env ty)
        in
        facts @ inv
      | _ ->
        let preds =
          (match refinement_of_type env ty with
           | Some p -> [ p ]
           | None -> [])
          @ invariant_preds env ty
        in
        List.map
          (fun p ->
            register_pred_paths env p;
            Refinement.subst_bound ~by:(Refinement.Pvar id) p)
          preds)
    (pat_bound_idents_full pat)
;;

(* The unpack fact: a pattern marked [refine_ x] binds [x] at the skeleton and contributes
   the SCRUTINEE's refinement at [x]. *)
let unpack_fact
  : type k.
    Env.t
    -> k general_pattern
    -> scrut:type_expr
    -> scrut_name:Refinement.pred option
    -> Refinement.pred list
  =
  fun env pat ~scrut ~scrut_name ->
  if not (has_vox_attr "vox.refine" pat.pat_attributes)
  then []
  else (
    match pat_bound_idents pat, get_desc (Ctype.vox_expand_head env scrut) with
    | [ id ], Trefine (_, (_ :: _ as maps), pred) ->
      (* image-binder unpack: [id] binds at the BASE skeleton with the scrutinee's base
         predicate AND the LINK [composite id = scrutinee-image] tying the opened base
         value to the image the scrutinee denotes. *)
      register_pred_paths env pred;
      let base = Refinement.subst_bound ~by:(Refinement.Pvar id) pred in
      let link =
        match scrut_name with
        | Some n ->
          let composite_id =
            List.fold_left
              (fun acc (m : Types.vox_map) -> Refinement.Pfun (m.Types.vm_fn, [ acc ]))
              (Refinement.Pvar id)
              maps
          in
          [ Refinement.Pbinop (Refinement.Eq, composite_id, n) ]
        | None -> []
      in
      base :: link
    | [ id ], Trefine (_, [], pred) ->
      register_pred_paths env pred;
      [ Refinement.subst_bound ~by:(Refinement.Pvar id) pred ]
    | _ -> [])
;;

(* The logical name of an expression: variables denote their stamp; expressions in the
   translatable int/bool fragment their logic translation (Vox_reflect); applications of
   simple-variant constructors their constructor term (over the names of their arguments
   -- "constructors get the usual refinements", and the arguments are themselves named, so
   translatable arithmetic reflects inside them); anything else is a fresh unknown. *)
let fresh_unknown env (e : expression) =
  incr unknown_counter;
  let id = Ident.create_local (Printf.sprintf "*unknown%d*" !unknown_counter) in
  record_name env id e.exp_type;
  Hashtbl.replace synthetic_names id ();
  Refinement.Pvar id
;;

(* RULE 2 -- a call that never returns normally. By parametricity a TOTAL function whose
   declared result type is a type variable that occurs in NONE of its argument types
   cannot produce a value of that type: any such call diverges or raises, so its result
   refinement is [false] and its continuation is vacuous. The test runs on the callee's
   SCHEME (the generalized type as declared, via [Env]) -- the use-site type has already
   been unified (a sequence LHS forces [unit], an [if] arm the arm type), so it is not the
   honest source.

   EXTERNALS break parametricity ([Obj.magic : 'a -> 'b = "%identity"] has a result
   variable in no argument yet RETURNS), so for a primitive the scheme test is IGNORED: it
   is bottom iff its name is a raising primitive (the primitive IS its semantics, as the
   reflection table keys on [%addint]). [raise] is [%raise]; everything built on top
   ([failwith], [invalid_arg], [exit], a user [let rec loop () : 'a]) is an ordinary value
   the scheme test classifies. *)
let raising_primitives =
  [ "%raise"; "%reraise"; "%raise_notrace"; "%raise_with_backtrace" ]
;;

let scheme_never_returns env scheme args =
  let rec peel ty doms = function
    | [] -> Some (ty, doms)
    | _ :: rest ->
      (match get_desc (Ctype.vox_expand_head env ty) with
       | Tarrow (_, dom, ret, _) -> peel ret (dom :: doms) rest
       | _ -> None)
  in
  match peel scheme [] args with
  | None -> false
  | Some (res, doms) ->
    let res = Ctype.vox_expand_head env res in
    (match get_desc res with
     | Tvar _ ->
       let rid = get_id res in
       not
         (List.exists
            (fun dom ->
              List.exists
                (fun v -> Int.equal (get_id v) rid)
                (Ctype.free_variables ~env dom))
            doms)
     | _ -> false)
;;

(* Does this application provably never return normally? *)
let diverging_apply env funct args =
  match funct.exp_desc with
  | Texp_ident { path; _ } ->
    (match Env.find_value path env with
     | vd ->
       let vd = Subst.Lazy.force_value_description vd in
       (match vd.val_kind with
        | Val_prim prim -> List.mem prim.Primitive.prim_name raising_primitives
        | _ -> scheme_never_returns env vd.val_type args)
     | exception Not_found -> false)
  | _ -> false
;;

(* An expression that never returns normally: a diverging application, or a compound all
   of whose exits diverge (a sequence/let whose tail does, an [if] both of whose branches
   do). *)
let rec diverges env (e : expression) : bool =
  match e.exp_desc with
  | Texp_apply (funct, args, _, _, _) -> diverging_apply env funct args
  | Texp_sequence (_, _, e2) -> diverges env e2
  | Texp_let (_, _, body) -> diverges env body
  | Texp_ifthenelse (_, e2, Some e3) -> diverges env e2 && diverges env e3
  | _ -> false
;;

(* The name a dependent argument was opened at by the type checker:
   [Vox_reflect.translate] is the typed twin of the surface translation
   [vox_open_dependent_arrow] substituted (the surface fragment is a subset of the typed
   one, and both key primitives and total_ functions on what the identifier resolves to),
   so the walker's instantiation of the remaining contracts agrees with the types. *)
let stable_arg_name (a : expression) : Refinement.pred option =
  (* [translate_nameable], not [translate]: the type checker's opening
     ([Vox_reflect.translate_surface]) names constructor applications and field reads, so
     the walker's twin must too, or the recovered result type would keep an unopened
     callee binder (out of scope, its fact dropped). [translate_nameable] is the typed
     superset of the surface fragment. *)
  match Vox_reflect.translate_nameable a with
  | Some _ as r -> r
  | None ->
    (* Tier 2: a non-reflectable call may still name its value by its own exact result
       contract, matching the type checker's opening. *)
    Vox_reflect.call_result_name a.exp_env a
;;

(* Register a module-level value on first sight: its sort (for the solver declaration)
   and, if its scheme carries a refinement, the .cmi fact at [Pglobal p], pulled into
   exactly the VCs that mention the path. The registry is the emit-time chokepoint: every
   channel that can put a [Pglobal] into a predicate (reflection of an ident, dependent
   substitution, an imported predicate) funnels through [emit_vc], which scans and
   registers. Two paths to one value register separately (both facts true, equality not
   assumed). *)
let rec register_global env (p : Path.t) =
  let key = path_uname p in
  if not (Hashtbl.mem globals key)
  then (
    match Env.find_value p env with
    | vd ->
      let vd = Subst.Lazy.force_value_description vd in
      let sname = lean_sanitize key in
      (match Hashtbl.find_opt global_snames sname with
       | Some other when not (String.equal other key) ->
         Location.raise_errorf
           ~loc:(Location.in_file !Location.input_name)
           "vox: values %s and %s would share the solver-side name g_%s; rename one of \
            them"
           other
           key
           sname
       | _ -> Hashtbl.replace global_snames sname key);
      Hashtbl.replace globals key (p, dsort_of_type env vd.val_type);
      if !Clflags.vox_dump_vc_provenance
      then Hashtbl.replace global_types key (type_one_line vd.val_type);
      (* Both the written refinement and the type's declared INVARIANTS attach at the
         path: [val zero : nat] carries the invariant exactly as
         [val zero : int{ _ >= 0 }] carries its refinement. *)
      let preds =
        (match refinement_of_type env vd.val_type with
         | Some pr -> [ pr ]
         | None -> [])
        @ invariant_preds env vd.val_type
      in
      List.iter
        (fun pr ->
          register_pred_paths env pr;
          let fact = Refinement.subst_bound ~by:(Refinement.Pglobal p) pr in
          List.iter (register_global env) (Refinement.free_globals fact);
          global_facts := fact :: !global_facts)
        preds
    | exception Not_found ->
      (* Unresolvable here (e.g. a stale path): declare at the uninterpreted sort; no
         fact. *)
      Hashtbl.replace globals key (p, S_other))
;;

let rec name_of_expr env (e : expression) : Refinement.pred =
  match Vox_reflect.translate ~mutvar:mut_read e with
  | Some p ->
    (* The translation may contain field projections; register their record types so the
       structure declarations reach the solver. *)
    register_pred_paths env p;
    p
  | None ->
    (match e.exp_desc with
     | Texp_construct (_, cstr, _, args, _) ->
       let path = Data_types.cstr_res_type_path cstr in
       (match datatype_sort env path [] with
        | S_data (_, _) ->
          Refinement.Pconstr
            (path, cstr.cstr_name, List.map (fun (_, a) -> name_of_expr env a) args)
        | S_int
        | S_bool
        | S_param _
        | S_tuple _
        | S_iarray
        | S_poly _
        | S_lean _
        | S_arrow _
        | S_other -> fresh_unknown env e)
     | Texp_record { fields; extended_expression; _ } when Array.length fields > 0 ->
       (* A record literal names the constructor term ["mk"] (a reserved lowercase name:
          real constructors are capitalized); in a functional update [{ b with l = e }],
          kept fields project out of the base's name. *)
       let path =
         Data_types.lbl_res_type_path
           (match fields.(0) with
            | lbl, _, _ -> lbl)
       in
       (match datatype_sort env path [] with
        | S_data (_, _) ->
          let base =
            Option.map (fun (be, _, _) -> name_of_expr env be) extended_expression
          in
          let arg_of (lbl, _, def) =
            match def, base with
            | Overridden (_, ex), _ -> name_of_expr env ex
            | Kept _, Some b -> Refinement.Pfield (path, lbl.Data_types.lbl_name, b)
            | Kept _, None ->
              (* unreachable: [Kept] implies a functional update *)
              fresh_unknown env e
          in
          Refinement.Pconstr (path, "mk", List.map arg_of (Array.to_list fields))
        | S_int
        | S_bool
        | S_param _
        | S_tuple _
        | S_iarray
        | S_poly _
        | S_lean _
        | S_arrow _
        | S_other -> fresh_unknown env e)
     | Texp_tuple (comps, _)
       when List.length comps >= 2
            && List.for_all (fun (lbl, _) -> Option.is_none lbl) comps ->
       (* An unlabeled tuple names its product term over the components' names
          ("constructors get the usual refinements"). *)
       register_tuple_arity (List.length comps);
       Refinement.Ptuple (List.map (fun (_, a) -> name_of_expr env a) comps)
     | Texp_field { record; label; _ } ->
       (* Mostly subsumed: [Vox_reflect.translate] projects immutable fields of simple
          records when the base itself translates. This fallback still fires when the base
          is only NAMEABLE (e.g. a field of a just-constructed record). *)
       let path = Data_types.lbl_res_type_path label in
       (match label.lbl_mut, datatype_sort env path [] with
        | Types.Immutable, S_data (_, _) ->
          Refinement.Pfield (path, label.lbl_name, name_of_expr env record)
        | _ -> fresh_unknown env e)
     | Texp_ifthenelse (_, e2, Some e3)
       when not (Bool.equal (diverges env e2) (diverges env e3)) ->
       (* An [if] with a diverging branch denotes, wherever its value is observed, the
          value of the OTHER branch (control took it, or the value would not exist):
          [let x = if b then raise E else 0] selfifies to [x = 0]. *)
       if diverges env e2 then name_of_expr env e3 else name_of_expr env e2
     | _ -> fresh_unknown env e)
;;

(* Selfification: a let binder names its RHS's value, so a binding whose pattern is a
   single variable contributes [x = name(rhs)] whenever the RHS has a stable logical name
   (its reflection, a constructor term, an immutable field read) -- fresh unknowns are
   skipped as pure noise. Sound because the binding IS the evaluation: if the RHS raised
   (division), [x] is never bound and the fact holds vacuously. This makes the aliasing
   idiom implicit: [let s = l + r] carries [s = l + r], with no [refine_] in sight, and an
   unpack [let refine_ x = e] additionally remembers WHICH value it opened. *)
let self_fact env id (rhs : expression) =
  match name_of_expr env rhs with
  | Refinement.Pvar u when Hashtbl.mem synthetic_names u -> []
  | n -> [ Refinement.Pbinop (Refinement.Eq, Refinement.Pvar id, n) ]
;;

let binding_self_facts env (vb : value_binding) =
  match vb.vb_pat.pat_desc with
  | Tpat_var { id; _ } -> self_fact env id vb.vb_expr
  | _ -> []
;;

(* The logical context of a program point: facts, plus the stamps in scope there. A fact
   mentioning an out-of-scope stamp must not be used: the same dead stamp can reach
   several unrelated points (e.g. through a refinement in a function's inferred result
   type that mentions the function's own parameters), and equating them would prove false
   facts. Out-of-scope facts are dropped (sound: fewer hypotheses); out-of-scope goals are
   errors. *)
type ctx =
  { cfacts : (Refinement.pred * Location.t option) list
  ; cscope : Ident.t list
  }

(* Tag a batch of freshly collected facts with the source span they originated from -- the
   binder pattern, the branch condition, the contract/invariant annotation -- or [None]
   where a fact is synthesized with no meaningful span (selfification equalities,
   mutable-version havoc, imported cross-unit facts). Threaded through [cfacts] so a
   hypothesis in a VC dump can point back at its origin under [-vox-dump-vc-provenance];
   invisible to the solver, which reads only the predicates. *)
let prov (loc : Location.t option) (ps : Refinement.pred list)
  : (Refinement.pred * Location.t option) list
  =
  List.map (fun p -> p, loc) ps
;;

let in_scope ctx id =
  List.exists (Ident.same id) ctx.cscope || Hashtbl.mem synthetic_names id
;;

let pred_in_scope ctx p = List.for_all (in_scope ctx) (Refinement.free_vars p)

(* Sort discipline for COMPILED runtime checks (assume_): the check must return the
   logic's verdict, so each construct is admitted only where the compiled operation
   provably agrees with the logical one:

   - arithmetic and order comparisons at [S_int] exactly;
   - equality at [S_int], [S_bool], and hereditarily-STRUCTURAL datatypes -- every
     component an int, a bool, or such a datatype -- where OCaml structural equality IS
     inductive equality. An atom-sorted component ([S_other]) would compare structurally
     at run time but by atom identity in the logic, so it is rejected;
   - constructor terms of monomorphic simple variants, built at run time from the
     registered representation;
   - applications of CURRENT-UNIT reflected [total_] functions, resolved by stamp through
     [Vox_reflect] (resolving the source name in the check site's environment could be
     captured by a program-level shadowing [let rev = ...], and a check calling the wrong
     function could admit a false fact). The reflected definition IS the runtime function,
     so calling it is faithful: termination is Lean-checked, and a division inside raises
     where the logic totalizes, which aborts the check rather than mis-answering it.

   Everything else stays rejected: tuples, projections, record fields (future work),
   quantifiers (not decidable by evaluation), top-level division (the logic's T-division
   is total, [tdiv x 0 = 0], where the program raises), prelude-only spec functions (no
   runtime denotation), and imported reflected functions (the predicate carries only the
   source name, not the module). *)

let rec dsort_equal a b =
  match a, b with
  | S_int, S_int | S_bool, S_bool | S_iarray, S_iarray -> true
  | S_lean (n1, xs), S_lean (n2, ys) ->
    String.equal n1 n2
    && List.compare_lengths xs ys = 0
    && List.for_all2 dsort_equal xs ys
  | S_param i, S_param j -> Int.equal i j
  | S_data (p, xs), S_data (q, ys) ->
    Path.same p q && List.compare_lengths xs ys = 0 && List.for_all2 dsort_equal xs ys
  | S_tuple xs, S_tuple ys ->
    List.compare_lengths xs ys = 0 && List.for_all2 dsort_equal xs ys
  | S_poly (p, xs), S_poly (q, ys) ->
    Path.same p q && List.compare_lengths xs ys = 0 && List.for_all2 dsort_equal xs ys
  | S_arrow (a1, b1), S_arrow (a2, b2) -> dsort_equal a1 a2 && dsort_equal b1 b2
  | ( ( S_int
      | S_bool
      | S_iarray
      | S_param _
      | S_data _
      | S_tuple _
      | S_lean _
      | S_arrow _
      | S_poly _
      | S_other )
    , _ ) -> false
;;

let rec structural_datatype ~seen p =
  List.exists (Path.same p) seen
  ||
  match find_datatype p with
  | Some (_, Dt_variant (0, cstrs)) ->
    List.for_all
      (fun (_, fields) -> List.for_all (structural_sort ~seen:(p :: seen)) fields)
      cstrs
  | Some (_, Dt_record (0, fields)) ->
    List.for_all (fun (_, s) -> structural_sort ~seen:(p :: seen) s) fields
  | Some (_, (Dt_variant _ | Dt_record _)) | None -> false
  | Some (_, Dt_opaque) ->
    (* A sealed datatype hides its representation from the logic; a runtime structural
       comparison would see through it. *)
    false

and structural_sort ~seen (s : dsort) =
  match s with
  | S_int | S_bool -> true
  | S_data (p, []) -> structural_datatype ~seen p
  | S_data (_, _ :: _)
  | S_param _ | S_tuple _ | S_iarray | S_poly _ | S_lean _ | S_arrow _ | S_other -> false
;;

let equality_sort s =
  match s with
  | S_int | S_bool -> true
  | S_data (p, []) -> structural_datatype ~seen:[] p
  | S_data (_, _ :: _)
  | S_param _ | S_tuple _ | S_iarray | S_poly _ | S_lean _ | S_arrow _ | S_other -> false
;;

(* The gate: raises unless [goal] compiles to a faithful check. Mirrored by the
   translation in Translcore.vox_assume_check, which compiles exactly the admitted forms. *)
let runtime_check_gate env ~loc goal =
  let err msg =
    Location.raise_errorf
      ~loc
      "vox: assume_ compiles a runtime check of this refinement, but %s; use \
       assume_unchecked_"
      msg
  in
  let sort_name s =
    match s with
    | S_int -> "Int"
    | S_bool -> "Bool"
    | S_data (p, _) -> "the datatype " ^ Path.name p
    | S_tuple _ -> "a tuple"
    | S_iarray -> "an iarray"
    | S_param _ | S_poly _ | S_lean _ | S_arrow _ | S_other -> "an opaque sort"
  in
  let rec term (p : Refinement.pred) : dsort =
    match p with
    | Refinement.Pint _ -> S_int
    | Refinement.Pbool _ -> S_bool
    | Refinement.Pbound ->
      (* Substituted away before emission; defensive. *)
      err "the checked value's sort is unknown"
    | Refinement.Pvar id ->
      (match Hashtbl.find_opt name_sorts id with
       | Some ((S_int | S_bool) as s) -> s
       | Some (S_data (q, []) as s) when structural_datatype ~seen:[] q -> s
       | Some _ | None ->
         err
           (Printf.sprintf
              "%s has a sort the check cannot evaluate faithfully (only ints, bools, and \
               datatypes built from them can be checked)"
              (if Hashtbl.mem synthetic_names id
               then "the checked value"
               else Ident.name id)))
    | Refinement.Pconstr (path, cname, args) ->
      (match datatype_sort env path [] with
       | S_data (_, []) -> ()
       | S_int
       | S_bool
       | S_data _
       | S_param _
       | S_tuple _
       | S_iarray
       | S_poly _
       | S_lean _
       | S_arrow _
       | S_other ->
         err
           (Printf.sprintf
              "the constructor %s's datatype cannot be built by the check"
              cname));
      (match find_datatype path with
       | Some (_, Dt_variant (0, cstrs)) ->
         (match List.assoc_opt cname cstrs with
          | Some fields ->
            if List.compare_lengths fields args <> 0
            then
              err
                (Printf.sprintf "the constructor %s is applied at the wrong arity" cname);
            List.iter2
              (fun field arg ->
                if not (dsort_equal field (term arg))
                then
                  err
                    (Printf.sprintf
                       "an argument of the constructor %s has the wrong sort"
                       cname))
              fields
              args
          | None ->
            err
              (Printf.sprintf
                 "the constructor %s is not part of its datatype's model"
                 cname))
       | Some _ | None ->
         err
           (Printf.sprintf
              "the constructor %s's datatype cannot be built by the check"
              cname));
      (* The runtime representation the translation builds. *)
      (match Env.find_type_descrs path env with
       | Type_variant (cstrs, _, _) ->
         (match
            List.find_opt
              (fun (c : Data_types.constructor_description) ->
                String.equal c.cstr_name cname)
              cstrs
          with
          | Some c ->
            (match c.cstr_tag, c.cstr_repr, c.cstr_inlined with
             | Ordinary _, Variant_boxed _, None -> ()
             | _ ->
               err
                 (Printf.sprintf
                    "the constructor %s's representation cannot be built by the check"
                    cname))
          | None ->
            err (Printf.sprintf "the constructor %s is not in scope at the check" cname))
       | _ -> err (Printf.sprintf "the constructor %s is not in scope at the check" cname)
       | exception Not_found ->
         err (Printf.sprintf "the constructor %s is not in scope at the check" cname));
      S_data (path, [])
    | Refinement.Pfun (f, args) ->
      (match Vox_reflect.reflected_for_check f with
       | None ->
         err
           (Printf.sprintf
              "%s has no runtime definition this check could call (only this unit's \
               total_ functions do)"
              f)
       | Some (id, ty) ->
         (* The check site's environment must resolve [f] to the registered reflected
            binding itself. A program-level shadowing would not fool the batch compiler
            (the stamped ident is a real lambda binding), but the TOPLEVEL resolves an
            out-of-phrase ident through a name-keyed value store at the defining phrase's
            execution, where the latest binding of that name wins -- a shadowed name there
            would hand the check the wrong function. *)
         (match Env.lookup_value ~use:false ~loc (Longident.Lident f) env with
          | Path.Pident id', _, _ when Ident.same id id' -> ()
          | _ ->
            err
              (Printf.sprintf
                 "%s is shadowed by another binding at this point, so the check could \
                  not call the reflected definition the predicate denotes"
                 f)
          | exception _ ->
            err
              (Printf.sprintf
                 "%s is not in scope at this point, so the check could not call the \
                  reflected definition the predicate denotes"
                 f));
         let rec arrow_sorts ty acc =
           match Types.get_desc (Ctype.vox_expand_head env ty) with
           | Tarrow (_, dom, cod, _) ->
             (* Arrow domains are [Tpoly]-wrapped. *)
             let dom =
               match Types.get_desc dom with
               | Tpoly (t, []) -> t
               | _ -> dom
             in
             arrow_sorts cod (dsort_of_type env dom :: acc)
           | _ -> List.rev acc, dsort_of_type env ty
         in
         let params, result = arrow_sorts ty [] in
         if List.compare_lengths params args <> 0
         then err (Printf.sprintf "%s is not applied at its full arity" f);
         List.iter2
           (fun param arg ->
             let s = term arg in
             if not (dsort_equal param s)
             then
               err
                 (Printf.sprintf
                    "an argument of %s has sort %s where %s is expected"
                    f
                    (sort_name s)
                    (sort_name param)))
           params
           args;
         (match result with
          | S_int | S_bool -> ()
          | S_data (q, []) when structural_datatype ~seen:[] q -> ()
          | S_data _
          | S_param _
          | S_tuple _
          | S_iarray
          | S_poly _
          | S_lean _
          | S_arrow _
          | S_other ->
            err
              (Printf.sprintf
                 "%s returns %s, which the check cannot handle"
                 f
                 (sort_name result)));
         result)
    | Refinement.Pbinop ((Refinement.Add | Refinement.Sub | Refinement.Mul), a, b) ->
      int_operand a;
      int_operand b;
      S_int
    | Refinement.Pbinop ((Refinement.Div | Refinement.Mod), _, _) ->
      err
        "it divides, and the logic's division is total (tdiv x 0 = 0) where the program \
         raises"
    | Refinement.Pbinop
        ((Refinement.Lt | Refinement.Le | Refinement.Gt | Refinement.Ge), a, b) ->
      int_operand a;
      int_operand b;
      S_bool
    | Refinement.Pbinop ((Refinement.Eq | Refinement.Neq), a, b) ->
      let sa = term a in
      let sb = term b in
      if not (dsort_equal sa sb)
      then
        err
          (Printf.sprintf
             "an equality compares %s against %s"
             (sort_name sa)
             (sort_name sb));
      if not (equality_sort sa)
      then
        err
          (Printf.sprintf
             "an equality at %s would compare structurally at run time but not in the \
              logic"
             (sort_name sa));
      S_bool
    | Refinement.Pand (a, b) | Refinement.Por (a, b) | Refinement.Pimp (a, b) ->
      bool_operand a;
      bool_operand b;
      S_bool
    | Refinement.Pnot a ->
      bool_operand a;
      S_bool
    | Refinement.Pglobal _ ->
      err "it mentions a module-level value, which the check cannot read"
    | Refinement.Pfield _
    | Refinement.Ptuple _
    | Refinement.Pproj _
    | Refinement.Pis _
    | Refinement.Pquant _
    | Refinement.Plam _ ->
      err
        (Printf.sprintf
           "it involves %s, which the compiled check cannot evaluate faithfully"
           Refinement.unreflectable_what)
  and int_operand p =
    match term p with
    | S_int -> ()
    | s ->
      err (Printf.sprintf "an arithmetic or order operand is %s, not Int" (sort_name s))
  and bool_operand p =
    match term p with
    | S_bool -> ()
    | s -> err (Printf.sprintf "a logical operand is %s, not Bool" (sort_name s))
  in
  bool_operand goal
;;

(* R1 (kinds study): reject order/arithmetic/tuple-projection applied to a value whose
   solver sort carries no such theory. Fixed-width and unboxed types
   (int64#/int32#/nativeint#, float#/float, char, unboxed products and records, or_null)
   all model at the uninterpreted sort, where only equality is available. Without this the
   obligation reaches Lean and fails instance synthesis, surfaced to the user as "NOT
   PROVED (may still hold)" -- which is misleading: the spec is unsatisfiable as stated,
   not merely unproved. Strictly LOOSER than [runtime_check_gate]: equality/disequality
   stay allowed at every sort (uninterpreted equality is sound). Permissive where a sort
   is UNKNOWN -- quantifier binders are intentionally unsorted, and spec-function / global
   results are opaque here -- so those are left to Lean exactly as before, avoiding false
   rejections of quantified-int arithmetic. *)
let check_operator_sorts ~loc (root : Refinement.pred) =
  let kind_name s =
    match s with
    | S_int -> "Int"
    | S_bool -> "Bool"
    | S_data (p, _) -> "the datatype " ^ Path.name p
    | S_tuple _ -> "a tuple"
    | S_iarray -> "an iarray"
    | S_param _ | S_poly _ | S_lean _ | S_arrow _ | S_other -> "an opaque sort"
  in
  (* best-effort, SIDE-EFFECT-FREE sort; [None] = unknown (be permissive). *)
  let rec sort_of (p : Refinement.pred) : dsort option =
    match p with
    | Refinement.Pint _ -> Some S_int
    | Refinement.Pbool _ -> Some S_bool
    | Refinement.Pvar id -> Hashtbl.find_opt name_sorts id
    | Refinement.Pbinop
        ( ( Refinement.Add
          | Refinement.Sub
          | Refinement.Mul
          | Refinement.Div
          | Refinement.Mod )
        , _
        , _ ) -> Some S_int
    | Refinement.Pbinop _ -> Some S_bool
    | Refinement.Pand _
    | Refinement.Por _
    | Refinement.Pimp _
    | Refinement.Pnot _
    | Refinement.Pis _
    | Refinement.Pquant _ -> Some S_bool
    | Refinement.Ptuple ps ->
      Some (S_tuple (List.map (fun p -> Option.value (sort_of p) ~default:S_other) ps))
    | Refinement.Pproj (_, i, a) ->
      (match sort_of a with
       | Some (S_tuple ss) -> List.nth_opt ss i
       | _ -> None)
    | Refinement.Pbound
    | Refinement.Pglobal _
    | Refinement.Pfun _
    | Refinement.Pconstr _
    | Refinement.Plam _
    | Refinement.Pfield _ -> None
  in
  let describe (p : Refinement.pred) =
    match p with
    | Refinement.Pvar id when not (Hashtbl.mem synthetic_names id) ->
      Printf.sprintf "\"%s\"" (Ident.name id)
    | _ -> "a subterm"
  in
  (* Reject ONLY the genuinely uninterpreted sort [S_other] (VoxU): that is where
     fixed-width and unboxed kinds land, and where Lean can synthesize no arithmetic /
     order / product instance, so the obligation can never be discharged. A ghost sort
     ([S_lean] -- e.g. a via image the author named [Int]), a datatype, or an abstract
     parameter is left to Lean exactly as before, so via-image arithmetic (cfold's [a + b]
     over [t refines Int]) keeps working. Unknown sorts (quantifier binders, spec-fn
     results) are also permissive. *)
  let voxu p =
    match sort_of p with
    | Some S_other -> true
    | _ -> false
  in
  let rec walk (p : Refinement.pred) =
    (match p with
     | Refinement.Pbinop
         ( (( Refinement.Add
            | Refinement.Sub
            | Refinement.Mul
            | Refinement.Div
            | Refinement.Mod
            | Refinement.Lt
            | Refinement.Le
            | Refinement.Gt
            | Refinement.Ge ) as op)
         , a
         , b ) ->
       let chk x =
         if voxu x
         then
           Location.raise_errorf
             ~loc
             "vox: the operator (%s) needs Int operands, but %s is modeled at %s; only \
              equality (= and <>) is available for this kind (fixed-width and unboxed \
              types are left uninterpreted)"
             (Refinement.binop_name op)
             (describe x)
             (kind_name (Option.value (sort_of x) ~default:S_other))
       in
       chk a;
       chk b
     | Refinement.Pproj (_, _, a) ->
       if voxu a
       then
         Location.raise_errorf
           ~loc
           "vox: fst/snd project a tuple, but %s is modeled at %s (unboxed products #( ) \
            are not modeled; only boxed tuples project)"
           (describe a)
           (kind_name (Option.value (sort_of a) ~default:S_other))
     | _ -> ());
    match p with
    | Refinement.Pbinop (_, a, b)
    | Refinement.Pand (a, b)
    | Refinement.Por (a, b)
    | Refinement.Pimp (a, b) ->
      walk a;
      walk b
    | Refinement.Pnot a
    | Refinement.Pfield (_, _, a)
    | Refinement.Pquant (_, _, a)
    | Refinement.Plam (_, a)
    | Refinement.Pproj (_, _, a)
    | Refinement.Pis (_, _, a) -> walk a
    | Refinement.Ptuple ps | Refinement.Pconstr (_, _, ps) | Refinement.Pfun (_, ps) ->
      List.iter walk ps
    | Refinement.Pint _
    | Refinement.Pbool _
    | Refinement.Pbound
    | Refinement.Pvar _
    | Refinement.Pglobal _ -> ()
  in
  walk root
;;

let emit_vc ~env ~loc ~ctx ~goal ~kind =
  (* The goal's provenance is the refinement/annotation text that induced the obligation.
     A refined type carries no syntactic loc for its annotation, so the best span
     available is the obligation site itself (which the header already reports): the two
     coincide, and emitting it uniformly keeps the format regular for the editor. *)
  let goal_prov = Some loc in
  (* Register every module-level value this VC mentions: its solver declaration (sort) and
     its .cmi refinement as a global fact -- the single chokepoint for all channels that
     can produce a [Pglobal] (reflection, dependent substitution, imported predicates). *)
  List.iter
    (register_global env)
    (List.concat_map Refinement.free_globals (goal :: List.map fst ctx.cfacts));
  (* Facts mentioning out-of-scope stamps (including any dependent binder a substitution
     failed to open) are dropped (sound: fewer hypotheses); such goals cannot be
     discharged and are errors. The same scope requirement applies to runtime-checked
     goals: the compiled check reads those variables at run time. *)
  (match kind with
   | Prove ->
     if not (pred_in_scope ctx goal)
     then
       Location.raise_errorf
         ~loc
         "vox: this obligation mentions a variable that has escaped its scope";
     check_operator_sorts ~loc goal
   | Runtime_check ->
     if not (pred_in_scope ctx goal)
     then
       Location.raise_errorf
         ~loc
         "vox: assume_ compiles a runtime check of this refinement, but it mentions a \
          variable that is not in scope here; use assume_unchecked_";
     runtime_check_gate env ~loc goal
   | Assume -> ());
  let facts = List.filter (fun (p, _) -> pred_in_scope ctx p) ctx.cfacts in
  (match kind with
   | Prove -> List.iter (fun (p, _) -> check_operator_sorts ~loc p) facts
   | Runtime_check | Assume -> ());
  (* Pull in the definitional equations reachable from the goal and facts (transitively
     through their right-hand sides); definitions mentioning out-of-scope program
     variables are dropped, which only weakens. *)
  let defs =
    let needed = Hashtbl.create 8 in
    let note p =
      List.iter
        (fun id -> Hashtbl.replace needed (Ident.unique_name id) ())
        (Refinement.free_vars p)
    in
    note goal;
    List.iter (fun (p, _) -> note p) facts;
    let rec grow acc remaining =
      let take, keep =
        List.partition
          (fun d ->
            match d with
            | Refinement.Pbinop (Refinement.Eq, Refinement.Pvar v, _) ->
              Hashtbl.mem needed (Ident.unique_name v)
            | _ -> false)
          remaining
      in
      if take = []
      then acc
      else (
        List.iter note take;
        grow (take @ acc) keep)
    in
    List.filter (pred_in_scope ctx) (grow [] !mut_defs)
  in
  let facts = facts @ prov None defs in
  (* Global facts (the .cmi refinements of module-level values named in this VC) arrive by
     NEED: an import's fact appears exactly in the VCs that mention its name. *)
  let facts =
    let mentioned = Hashtbl.create 8 in
    let note p =
      List.iter
        (fun id -> Hashtbl.replace mentioned (Ident.unique_name id) ())
        (Refinement.free_vars p);
      List.iter
        (fun gp -> Hashtbl.replace mentioned (path_uname gp) ())
        (Refinement.free_globals p)
    in
    note goal;
    List.iter (fun (p, _) -> note p) facts;
    let gfacts =
      List.filter
        (fun g ->
          List.exists
            (fun id -> Hashtbl.mem mentioned (Ident.unique_name id))
            (Refinement.free_vars g)
          || List.exists
               (fun gp -> Hashtbl.mem mentioned (path_uname gp))
               (Refinement.free_globals g))
        !global_facts
    in
    facts @ prov None gfacts
  in
  (* Several fact channels can deliver the same fact (a binder fact and its selfification
     equation, say); keep the first occurrence. Quadratic, but hypothesis lists are small. *)
  let facts =
    List.fold_left
      (fun acc (f, l) ->
        if List.exists (fun (g, _) -> Refinement.equal f g) acc
        then acc
        else (f, l) :: acc)
      []
      facts
    |> List.rev
  in
  vcs
  := { vc_loc = loc
     ; vc_facts = List.map fst facts
     ; vc_fact_provs = List.map snd facts
     ; vc_goal = goal
     ; vc_goal_prov = goal_prov
     ; vc_kind = kind
     }
     :: !vcs
;;

(* Escaped refinements (DESIGN: "escape is an error"). A binder's type may not carry
   refinements mentioning program variables that are not in scope at the binding: the same
   stamp can name a different value at another point (recursion re-binds it; unification
   propagates types across scopes), so such facts would be unsound. At the module level
   the rule is stricter: refinements in exported types may mention no program variables at
   all (predicates in .cmis are self-contained; stamps do not survive a compilation unit).
   Dependent-arrow binders are exempt where the type itself binds them
   ([iter_refinement_preds] reports them as [bound]). *)
type escape_mode =
  | Module_level
  | In_scope of ctx * Ident.t list (* extra idents treated as in scope *)

let check_type_escapes ~loc ~what mode ty =
  Vox_dep.iter_refinement_preds ty (fun ~bound p ->
    List.iter
      (fun v ->
        let bad =
          if List.exists (Ident.same v) bound
          then false
          else (
            match mode with
            | Module_level -> true
            | In_scope (ctx, extra) ->
              not (in_scope ctx v || List.exists (Ident.same v) extra))
        in
        if bad
        then
          Location.raise_errorf
            ~loc
            "vox: the type of %s carries a refinement mentioning %s, which %s; annotate \
             with a dependent arrow ((%s : ...) -> ...) or a self-contained refinement"
            what
            (Ident.name v)
            (match mode with
             | Module_level -> "may not appear in a module-level type"
             | In_scope _ -> "is not in scope here")
            (Ident.name v))
      (Refinement.free_vars p))
;;

let check_binder_escape ~toplevel ctx ~extra_scope (pat : _ general_pattern) id ty =
  let mode = if toplevel then Module_level else In_scope (ctx, extra_scope) in
  check_type_escapes ~loc:pat.pat_loc ~what:(Ident.name id) mode ty
;;

(* Backstop for binders the walker does not model (inside local or nested module
   structures, try handlers, letops, ...): they contribute no facts, but their types must
   still be escape-checked -- a stored closure typed with another activation's variable
   would otherwise smuggle false facts (the pattern's own binders count as in scope;
   siblings bound by the unmodeled construct do not, which is conservative). *)
(* Reflected definitions live at the top level of the current module: that is where
   walk_items registers and emits them. A marked binding anywhere else -- a local let, or
   a structure item of a nested or local module -- would be registered in the typing-time
   table (so its calls would translate) but never emitted, and a local one could capture
   enclosing variables; reject them all. *)
let reject_local_reflect (vb : Typedtree.value_binding) =
  if Vox_reflect.is_total_binding vb
  then
    Location.raise_errorf
      ~loc:vb.vb_loc
      "vox: total_ is only supported on top-level bindings of the current module"
;;

let backstop_pat : type k. ctx -> k general_pattern -> unit =
  fun ctx pat ->
  let bound = pat_bound_idents pat in
  List.iter
    (fun (id, _, ty, _, _) ->
      check_binder_escape ~toplevel:false ctx ~extra_scope:bound pat id ty)
    (pat_bound_idents_full pat)
;;

(* Every type reachable from any exported item of a signature -- values, type manifests,
   record fields, constructor arguments, extension constructors, submodules, module types,
   classes. Used both for the self-containment check and for computing the .cmi's spec
   export. *)
let rec iter_signature_types ~f (sg : Types.signature) =
  List.iter (iter_signature_item_types ~f) sg

and iter_signature_item_types ~f (item : Types.signature_item) =
  let check = f in
  let check_constructor_arguments ~what = function
    | Types.Cstr_tuple args ->
      List.iter
        (fun (ca : Types.constructor_argument) -> check ~loc:ca.ca_loc ~what ca.ca_type)
        args
    | Types.Cstr_record lbls ->
      List.iter
        (fun (ld : Types.label_declaration) -> check ~loc:ld.ld_loc ~what ld.ld_type)
        lbls
  in
  match item with
  | Sig_value (id, vd, _) -> check ~loc:vd.val_loc ~what:(Ident.name id) vd.val_type
  | Sig_type (id, decl, _, _) ->
    let what = "type " ^ Ident.name id in
    Option.iter (check ~loc:decl.type_loc ~what) decl.type_manifest;
    (match decl.type_kind with
     | Type_abstract _ | Type_open -> ()
     | Type_record (lbls, _, _) | Type_record_unboxed_product (lbls, _, _) ->
       List.iter
         (fun (ld : Types.label_declaration) -> check ~loc:ld.ld_loc ~what ld.ld_type)
         lbls
     | Type_variant (cds, _, _) ->
       List.iter
         (fun (cd : Types.constructor_declaration) ->
           check_constructor_arguments ~what cd.cd_args)
         cds)
  | Sig_typext (id, ext, _, _) ->
    check_constructor_arguments ~what:(Ident.name id) ext.ext_args
  | Sig_module (_, _, md, _, _) -> iter_module_type_types ~f md.md_type
  | Sig_modtype (_, mtd, _) -> Option.iter (iter_module_type_types ~f) mtd.mtd_type
  | Sig_class (id, cd, _, _) ->
    iter_class_type_types ~f ~loc:cd.cty_loc ~what:(Ident.name id) cd.cty_type
  | Sig_class_type (id, ctd, _, _) ->
    iter_class_type_types ~f ~loc:ctd.clty_loc ~what:(Ident.name id) ctd.clty_type
  | Sig_jkind _ -> ()

and iter_module_type_types ~f = function
  | Mty_ident _ | Mty_alias _ -> ()
  | Mty_signature sg -> iter_signature_types ~f sg
  | Mty_functor (param, res, _) ->
    (match param with
     | Unit -> ()
     | Named (_, mty, _) -> iter_module_type_types ~f mty);
    iter_module_type_types ~f res
  | Mty_strengthen (mty, _, _) -> iter_module_type_types ~f mty

and iter_class_type_types ~f ~loc ~what = function
  | Cty_constr (_, args, cty) ->
    List.iter (f ~loc ~what) args;
    iter_class_type_types ~f ~loc ~what cty
  | Cty_signature csig ->
    f ~loc ~what csig.csig_self;
    Vars.iter (fun _ (_, _, ty) -> f ~loc ~what ty) csig.csig_vars
  | Cty_arrow (_, ty, cty) ->
    f ~loc ~what ty;
    iter_class_type_types ~f ~loc ~what cty
;;

(* Module-level self-containment, applied to a whole signature (implementation, interface,
   or toplevel phrase): every refinement reachable from any exported item must be free of
   program variables. This is what makes .cmi predicates self-contained: stamps do not
   survive a compilation unit, so an imported [Pvar] can collide with an unrelated local
   stamp and prove false facts. *)
(* [@@vox.sort] hygiene over an exported signature: malformed payloads are errors even
   when no value of the type ever reaches a VC (a typo on a ghost type must not be
   silent). *)
let rec validate_signature_sorts (sg : Types.signature) =
  List.iter
    (fun (item : Types.signature_item) ->
      match item with
      | Sig_type (_, decl, _, _) ->
        let alias =
          (match decl.type_kind with
           | Type_abstract _ -> true
           | _ -> false)
          && Option.is_some decl.type_manifest
        in
        validate_vox_sort_attributes ~alias decl.type_attributes
      | Sig_value (_, vd, _) -> Vox_reflect.validate_reflect_attr vd.val_attributes
      | Sig_module (_, _, md, _, _) ->
        (match md.md_type with
         | Mty_signature sub -> validate_signature_sorts sub
         | _ -> ())
      | _ -> ())
    sg
;;

let check_signature (sg : Types.signature) =
  validate_signature_sorts sg;
  iter_signature_types sg ~f:(fun ~loc ~what ty ->
    check_type_escapes ~loc ~what Module_level ty)
;;

(* vox match facts ("the match refines the thing we matched on"): matching a variable
   scrutinee [sid] against a SIMPLE pattern contributes facts to the case's guard and
   body:
   - one constructor of a simple variant over variables/wildcards gives
     [sid = C x1 ... xn] (wildcards name fresh unknowns);
   - a simple-record pattern gives [xi = sid.li] per VARIABLE sub-pattern (per-field, so
     partial patterns are fine; non-variable fields contribute nothing). Anything deeper
     (nesting, aliases, or-patterns, constants) contributes nothing, which is sound. This
     is the constructor analogue of the [if] path fact; [let p = x in ...] gets the same
     facts. *)
let match_facts
  : type k. Env.t -> Refinement.pred -> k general_pattern -> Refinement.pred list
  =
  fun env subject pat ->
  (* Each constructor argument gets a NAME: the variable's own when the pattern is one (an
     alias counts), a fresh synthetic otherwise; the name denotes the matched component,
     so a deeper pattern destructures it in turn. *)
  (* A component's type refinement, instantiated at the component's own logical term: an
     UNNAMED sub-pattern still receives its fact ([let ((), ()) = e], [let (_, _) = e]) --
     the subject is already a term of the logic, so no binder is required. Variables and
     aliases contribute through [binder_facts] instead. *)
  let type_facts subject (ty : Types.type_expr) =
    let preds =
      (match refinement_of_type env ty with
       | Some p -> [ p ]
       | None -> [])
      @ invariant_preds env ty
    in
    List.map
      (fun p ->
        register_pred_paths env p;
        Refinement.subst_bound ~by:subject p)
      preds
  in
  let rec arg_term (p : value general_pattern) =
    (* The logic TERM denoting a matched component, built as DEEPLY as the pattern is
       structural: a variable is its stamp, a literal itself, a (possibly nested)
       simple-variant constructor its constructor term over its components' terms. A deep
       pattern [Node (Red, Node (Red, a, x, b), ..)] thus yields ONE nested term
       [Node (Red, Node (Red, a, x, b), ..)] instead of a chain of fresh unknowns each
       tied back by an equation -- which the solver could not reduce a reflected model
       call against (a combinatorial split per unknown). Anything else (a record, a tuple,
       a wildcard, an opaque value) still names a fresh unknown and destructures that in
       turn. Returns the term together with the facts the component contributes AT that
       term (its type refinement, an alias binding, a nested record/tuple's projections). *)
    match p.pat_desc with
    | Tpat_var { id; _ } -> Refinement.Pvar id, []
    | Tpat_constant (Const_int n) -> Refinement.Pint n, []
    | Tpat_alias { pattern = sub; id; _ } ->
      (* The alias names the whole component; destructure the aliased pattern AT that name
         (matching the pre-deep-term behavior -- avoids a spurious unknown for an aliased
         tuple/record). *)
      Refinement.Pvar id, value_facts (Refinement.Pvar id) sub
    | Tpat_construct (_, cstr, _, cargs, _)
      when match datatype_sort env (Data_types.cstr_res_type_path cstr) [] with
           | S_data (_, _) -> true
           | _ -> false ->
      let path = Data_types.cstr_res_type_path cstr in
      let parts = List.map (fun (_, a) -> arg_term a) cargs in
      let term =
        Refinement.Pconstr (path, cstr.Data_types.cstr_name, List.map fst parts)
      in
      term, type_facts term p.pat_type @ List.concat_map snd parts
    | _ ->
      let id = Ident.create_local "*vox-wild*" in
      record_name env id p.pat_type;
      Hashtbl.replace synthetic_names id ();
      ( Refinement.Pvar id
      , (match p.pat_desc with
         | Tpat_any -> []
         | _ -> value_facts (Refinement.Pvar id) p) )
  and constructor_facts subject cstr args =
    let path = Data_types.cstr_res_type_path cstr in
    match datatype_sort env path [] with
    | S_int
    | S_bool
    | S_param _
    | S_tuple _
    | S_iarray
    | S_poly _
    | S_lean _
    | S_arrow _
    | S_other -> []
    | S_data (_, _) ->
      let parts = List.map (fun (_, a) -> arg_term a) args in
      Refinement.Pbinop
        ( Refinement.Eq
        , subject
        , Refinement.Pconstr (path, cstr.Data_types.cstr_name, List.map fst parts) )
      :: List.concat_map snd parts
  and record_facts subject (fields : (_ * Data_types.label_description * _) list) =
    match fields with
    | [] -> []
    | (_, lbl0, _) :: _ ->
      let path = Data_types.lbl_res_type_path lbl0 in
      (match datatype_sort env path [] with
       | S_int
       | S_bool
       | S_param _
       | S_tuple _
       | S_iarray
       | S_poly _
       | S_lean _
       | S_arrow _
       | S_other -> []
       | S_data (_, _) ->
         List.concat_map
           (fun (_, (lbl : Data_types.label_description), sub) ->
             value_facts
               (Refinement.Pfield (path, lbl.lbl_name, subject))
               (sub : value general_pattern))
           fields)
  and tuple_facts subject (comps : (string option * value general_pattern) list) =
    (* Per component at [proj_i sid], RECURSIVELY: a variable ties to the projection, a
       deeper tuple/record/alias destructures the projection in turn, so
       [let ((x, _), _) = e] reaches [x]. Labeled tuples are not modelled. *)
    if List.exists (fun (lbl, _) -> Option.is_some lbl) comps
    then []
    else (
      let n = List.length comps in
      register_tuple_arity n;
      List.mapi (fun i (_, sub) -> i, sub) comps
      |> List.concat_map (fun (i, (sub : value general_pattern)) ->
        value_facts (Refinement.Pproj (n, i, subject)) sub))
  and value_facts subject (p : value general_pattern) =
    match p.pat_desc with
    | Tpat_construct (_, cstr, _, args, _) ->
      type_facts subject p.pat_type @ constructor_facts subject cstr args
    | Tpat_record (fields, _, _, _) ->
      type_facts subject p.pat_type @ record_facts subject fields
    | Tpat_tuple comps -> type_facts subject p.pat_type @ tuple_facts subject comps
    | Tpat_any -> type_facts subject p.pat_type
    | Tpat_constant (Const_int n) ->
      (* A literal payload propagates its equality: [Lit 0] contributes [subject = 0], not
         merely a fresh unknown. *)
      Refinement.Pbinop (Refinement.Eq, subject, Refinement.Pint n)
      :: type_facts subject p.pat_type
    | Tpat_constant _ -> type_facts subject p.pat_type
    | Tpat_alias { pattern = sub; id; _ } ->
      (* [p as x]: the alias names the subject, and [p] destructures it in turn. *)
      Refinement.Pbinop (Refinement.Eq, Refinement.Pvar id, subject)
      :: value_facts subject sub
    | Tpat_var { id; _ }
      when (not (Refinement.equal (Refinement.Pvar id) subject))
           && not (has_vox_attr "vox.refine" p.pat_attributes) ->
      (* A variable pattern aliases the scrutinee: [match s with y ->] (and a
         [function y ->] case, whose scrutinee is [fc_param]) learns [y = s];
         [let refine_ x = m] (which desugars to a match) ties the binder to a mutable
         scrutinee's version. The self-alias guard: [fc_param] IS the first variable
         case's ident (see [Typecore.name_cases]), and [x = x] is noise. A [refine_]
         unpack is EXCLUDED: its binder is at the BASE and the scrutinee at the IMAGE, so
         the raw [x = s] would be ill-sorted; [unpack_fact] contributes the correct link
         [composite x = s]. *)
      [ Refinement.Pbinop (Refinement.Eq, Refinement.Pvar id, subject) ]
    | _ -> []
  in
  match pat.pat_desc with
  | Tpat_value p -> value_facts subject (p :> value general_pattern)
  | Tpat_construct (_, cstr, _, args, _) ->
    type_facts subject pat.pat_type @ constructor_facts subject cstr args
  | Tpat_record (fields, _, _, _) ->
    type_facts subject pat.pat_type @ record_facts subject fields
  | Tpat_tuple comps -> type_facts subject pat.pat_type @ tuple_facts subject comps
  | Tpat_alias { pattern = sub; id; _ } ->
    Refinement.Pbinop (Refinement.Eq, Refinement.Pvar id, subject)
    :: value_facts subject sub
  | Tpat_var { id; _ }
    when (not (Refinement.equal (Refinement.Pvar id) subject))
         && not (has_vox_attr "vox.refine" pat.pat_attributes) ->
    (* Bare value patterns (let bindings and [function]-case arms reach here unwrapped).
       The self-alias guard: [fc_param] IS the first variable case's ident (see
       [Typecore.name_cases]), and [x = x] is noise. [refine_] unpacks are excluded (base
       vs image sort); [unpack_fact] supplies the [composite x = s] link instead. *)
    [ Refinement.Pbinop (Refinement.Eq, Refinement.Pvar id, subject) ]
  | _ -> []
;;

(* The instantiated RESULT type of an application: walk the arrow spine from the
   function's type, substituting each dependent binder by its argument's stable name --
   the same opening the application site performed at typing time. *)
let apply_result_type env funct (args : (_ * apply_arg) list) =
  let arrow_ty = ref funct.exp_type in
  List.iter
    (fun (_lbl, (arg : apply_arg)) ->
      match get_desc (Ctype.vox_expand_head env !arrow_ty) with
      | Tarrow ((_, _, _, binder), _dom, ret, _) ->
        (match arg, binder with
         | Arg (a, _), Some b ->
           (match stable_arg_name a with
            | Some by -> arrow_ty := Vox_dep.subst_binder b ~by ret
            | None -> arrow_ty := ret)
         | _ -> arrow_ty := ret)
      | _ -> ())
    args;
  !arrow_ty
;;

(* A refinement instantiated at a name can collapse to a triviality ([3 = 3],
   [p.px = p.px]) when the name IS the refinement's witness (exact-synthesis types at
   translatable scrutinees); such facts are dropped, not asserted. *)
let nontrivial_fact (f : Refinement.pred) =
  match f with
  | Refinement.Pbinop (Refinement.Eq, a, b) -> not (Refinement.equal a b)
  | _ -> true
;;

(* The refinement of an expression's RESULT, seen through implicit erasure. Implicit
   elimination erases an application's refined result to the skeleton precisely because an
   unnamed value's fact is unreachable -- "name it with a let to keep it"; a destructuring
   IS the naming, so the refinement is recovered the same way the re-refinement hook
   recovers it: from the callee's instantiated result type. The result position is
   followed through the result-transparent forms (a let's body, a sequence's tail); an
   [if] recovers only when both branches recover the SAME predicate (there is no single
   callee to ask, and the facts of one branch do not hold on the other). *)
let rec result_refinement env (e : expression) : Refinement.pred option =
  match refinement_of_type env e.exp_type with
  | Some p -> Some p
  | None ->
    (match e.exp_desc with
     | Texp_apply (funct, args, _, _, _) ->
       if diverging_apply env funct args
       then Some (Refinement.Pbool false)
       else refinement_of_type env (apply_result_type env funct args)
     | Texp_let (_, _, body) -> result_refinement env body
     | Texp_sequence (_, _, body) -> result_refinement env body
     | Texp_ifthenelse (_, e2, Some e3) ->
       (match diverges env e2, diverges env e3 with
        | true, false -> result_refinement env e3
        | false, true -> result_refinement env e2
        | _ ->
          (match result_refinement env e2, result_refinement env e3 with
           | Some p2, Some p3 when Refinement.equal p2 p3 -> Some p2
           | _ -> None))
     | _ -> None)
;;

(* A boolean CONDITION need not translate wholesale to still expose a refined atom:
   [a && mem k t] has no logic image ([mem] is opaque), yet its then-branch knows both [a]
   and [mem]'s spec. [decompose_bool] walks the &&/||/not structure, translating what it
   can and NAMING each untranslatable leaf; a refined leaf contributes its result
   refinement at the name, GUARDED by the short-circuit condition under which the leaf is
   evaluated -- a leaf the operator skips is never evaluated, so its contract is not
   established and must say nothing. It returns the condition's logic formula over those
   names together with the guarded spec facts; the [if]/[while] handlers use the formula
   as the path fact (negated on the else/exit) and attach the facts to both sides. The
   guard threads left-to-right: in [a && b] the right leaf is guarded by [a], in [a || b]
   by [not a]. *)
let rec decompose_bool env ~guard (e : expression)
  : Refinement.pred * Refinement.pred list
  =
  match Vox_reflect.translate ~mutvar:mut_read e with
  | Some p ->
    register_pred_paths env p;
    p, []
  | None ->
    let and_guard g f =
      match g with
      | Refinement.Pbool true -> f
      | _ -> Refinement.Pand (g, f)
    in
    let guarded eq =
      match guard with
      | Refinement.Pbool true -> eq
      | g -> Refinement.Pimp (g, eq)
    in
    (match e.exp_desc with
     | Texp_apply
         ( { exp_desc = Texp_ident { desc = { val_kind = Val_prim prim; _ }; _ }; _ }
         , [ (Nolabel, Arg (a, _)); (Nolabel, Arg (b, _)) ]
         , _
         , _
         , _ )
       when String.equal prim.prim_name "%sequand"
            || String.equal prim.prim_name "%sequor" ->
       let fa, sa = decompose_bool env ~guard a in
       let is_and = String.equal prim.prim_name "%sequand" in
       let guard_b =
         if is_and then and_guard guard fa else and_guard guard (Refinement.Pnot fa)
       in
       let fb, sb = decompose_bool env ~guard:guard_b b in
       let formula =
         if is_and then Refinement.Pand (fa, fb) else Refinement.Por (fa, fb)
       in
       formula, sa @ sb
     | Texp_apply
         ( { exp_desc = Texp_ident { desc = { val_kind = Val_prim prim; _ }; _ }; _ }
         , [ (Nolabel, Arg (a, _)) ]
         , _
         , _
         , _ )
       when String.equal prim.prim_name "%boolnot" ->
       let fa, sa = decompose_bool env ~guard a in
       Refinement.Pnot fa, sa
     | _ ->
       let n = name_of_expr env e in
       (match result_refinement env e with
        | Some p ->
          register_pred_paths env p;
          let eq = Refinement.subst_bound ~by:n p in
          if nontrivial_fact eq then n, [ guarded eq ] else n, []
        | None -> n, []))
;;

(* A destructuring binding whose scrutinee is NOT a variable still gets the match facts,
   through a NAME for the scrutinee: its logic translation when it has one (tuples
   included -- they are reflectable values), a fresh unknown otherwise; either way the
   name denotes the single evaluation being destructured. The scrutinee's own refinement
   holds at that name too (for a variable scrutinee the binder's facts already carry it).
   If evaluation raises instead of returning, the continuation never runs and the facts
   are vacuous. *)
let destructure_facts
  : type k. Env.t -> expression -> k general_pattern -> Refinement.pred list
  =
  fun env rhs pat ->
  let n = name_of_expr env rhs in
  let refn =
    match
      match refinement_of_type env pat.pat_type with
      | Some p -> Some p
      | None -> result_refinement env rhs
    with
    | Some p ->
      register_pred_paths env p;
      List.filter nontrivial_fact [ Refinement.subst_bound ~by:n p ]
    | None -> []
  in
  refn @ match_facts env n pat
;;

(* Negative match facts: if control reaches an arm, every EARLIER arm failed to match. A
   guard-free arm contributes a usable fact in two shapes. (1) A GROUND pattern --
   constants and constructors all the way down, no variables -- matches [subject] exactly
   when [subject] equals the corresponding logic term, so its failure is
   [not (subject = term)]; this reaches [Lit 0] and bare int literals.

   (2) Otherwise, when the failure is decided by the constructor HEAD alone (one
   constructor of a simple variant over variables or wildcards, the same shape that earns
   a positive fact), it is [not (subject is C)]. A deeper pattern with variables in
   sub-positions (e.g. [Node (R, Node (R, ..), ..)]) still contributes nothing: the head
   may have matched while a sub-pattern refuted, and the logic has no constructor-field
   projection to name the refuting position (the systematic fix is the bidirectional match
   walk). Guarded arms contribute nothing (the pattern may have matched with the guard
   false). *)
let pattern_negation
  : type k. Env.t -> Refinement.pred -> k general_pattern -> Refinement.pred option
  =
  fun env subject pat ->
  let rec ground_term (p : value general_pattern) : Refinement.pred option =
    match p.pat_desc with
    | Tpat_constant (Const_int n) -> Some (Refinement.Pint n)
    | Tpat_construct (_, cstr, _, args, _) -> ground_construct cstr args
    | _ -> None
  and ground_construct cstr args =
    let path = Data_types.cstr_res_type_path cstr in
    match datatype_sort env path [] with
    | S_data (_, _) ->
      let subs = List.map (fun (_, a) -> ground_term a) args in
      if List.for_all Option.is_some subs
      then
        Some
          (Refinement.Pconstr (path, cstr.Data_types.cstr_name, List.map Option.get subs))
      else None
    | S_bool ->
      (match cstr.Data_types.cstr_name with
       | "true" -> Some (Refinement.Pbool true)
       | "false" -> Some (Refinement.Pbool false)
       | _ -> None)
    | S_int | S_param _ | S_tuple _ | S_iarray | S_poly _ | S_lean _ | S_arrow _ | S_other
      -> None
  in
  (* Reconstruct a pattern as a logic TERM, minting a fresh existential binder for every
     variable/wildcard leaf and PINNING every constant and constructor position. [None]
     whenever a node cannot be represented faithfully (a record, a nested tuple, an
     or-pattern): over-generalising a pinned position would weaken the match predicate and
     unsoundly strengthen its negation, so the whole negative is abandoned instead. *)
  let rec reconstruct (p : value general_pattern)
    : (Refinement.pred * Ident.t list) option
    =
    match p.pat_desc with
    | Tpat_var _ | Tpat_any ->
      let e = Ident.create_local "*vox-ex*" in
      Some (Refinement.Pvar e, [ e ])
    | Tpat_constant (Const_int n) -> Some (Refinement.Pint n, [])
    | Tpat_alias { pattern = sub; _ } -> reconstruct sub
    | Tpat_construct (_, cstr, _, args, _) -> reconstruct_construct cstr args
    | _ -> None
  and reconstruct_construct cstr args =
    let path = Data_types.cstr_res_type_path cstr in
    match datatype_sort env path [] with
    | S_data (_, _) ->
      let rec go terms ids = function
        | [] -> Some (List.rev terms, ids)
        | (_, a) :: rest ->
          (match reconstruct a with
           | Some (t, is_) -> go (t :: terms) (ids @ is_) rest
           | None -> None)
      in
      (match go [] [] args with
       | Some (terms, ids) ->
         Some (Refinement.Pconstr (path, cstr.Data_types.cstr_name, terms), ids)
       | None -> None)
    | S_bool ->
      (match cstr.Data_types.cstr_name with
       | "true" -> Some (Refinement.Pbool true, [])
       | "false" -> Some (Refinement.Pbool false, [])
       | _ -> None)
    | S_int | S_param _ | S_tuple _ | S_iarray | S_poly _ | S_lean _ | S_arrow _ | S_other
      -> None
  in
  let wrap_exists ids body =
    List.fold_right
      (fun id acc -> Refinement.Pquant (Refinement.Qexists, id, acc))
      ids
      body
  in
  let trivial (p : value general_pattern) =
    match p.pat_desc with
    | Tpat_var _ | Tpat_any -> true
    | _ -> false
  in
  let head_negation cstr args =
    let path = Data_types.cstr_res_type_path cstr in
    match datatype_sort env path [] with
    | S_int
    | S_bool
    | S_param _
    | S_tuple _
    | S_iarray
    | S_poly _
    | S_lean _
    | S_arrow _
    | S_other -> None
    | S_data (_, _) ->
      let simple (_, (p : value general_pattern)) =
        match p.pat_desc with
        | Tpat_var _ | Tpat_any -> true
        | _ -> false
      in
      if List.for_all simple args
      then
        Some (Refinement.Pnot (Refinement.Pis (path, cstr.Data_types.cstr_name, subject)))
      else None
  in
  let ground_neg t = Refinement.Pnot (Refinement.Pbinop (Refinement.Eq, subject, t)) in
  (* The deepest sound negative for a constructor pattern that has variable/wildcard
     leaves: [not (exists f.., subject = C (..f..))], the leaves existentially bound and
     every other position pinned. Grind will not instantiate it under a plain goal, but
     once the spec function's match is [split] it refutes the overlapping model case (see
     [lean_theorem]'s split fallback, keyed on this existential). *)
  let exists_construct_neg cstr args =
    match reconstruct_construct cstr args with
    | Some (term, ids) ->
      Some
        (Refinement.Pnot
           (wrap_exists ids (Refinement.Pbinop (Refinement.Eq, subject, term))))
    | None -> None
  in
  let construct_neg cstr args =
    (* Prefer the head test (no churn on shallow arms); then the ground equality (a
       literal / nested-constructor payload); finally the existential negative for a deep
       pattern with variable leaves. *)
    match head_negation cstr args with
    | Some n -> Some n
    | None ->
      (match ground_construct cstr args with
       | Some t -> Some (ground_neg t)
       | None -> exists_construct_neg cstr args)
  in
  (* A tuple (multi-scrutinee) earlier arm failed to match iff some PINNED component does
     not match; the sound negative is the negation of the CONJUNCTION over the non-trivial
     components, each an existential component equality [exists f.., subject.i = <recon>].
     A bare-variable component always matches and is dropped; an unrepresentable pinned
     component abandons the whole negative (dropping it would weaken the conjunction). *)
  let tuple_neg comps =
    if List.exists (fun (lbl, _) -> Option.is_some lbl) comps
    then None
    else (
      let n = List.length comps in
      let rec build i acc = function
        | [] -> Some (List.rev acc)
        | (_, comp) :: rest ->
          if trivial comp
          then build (i + 1) acc rest
          else (
            match reconstruct comp with
            | Some (term, ids) ->
              let conj =
                wrap_exists
                  ids
                  (Refinement.Pbinop
                     (Refinement.Eq, Refinement.Pproj (n, i, subject), term))
              in
              build (i + 1) (conj :: acc) rest
            | None -> None)
      in
      match build 0 [] comps with
      | Some [] | None -> None
      | Some (h :: t) ->
        Some (Refinement.Pnot (List.fold_left (fun a c -> Refinement.Pand (a, c)) h t)))
  in
  let value_neg (p : value general_pattern) =
    match p.pat_desc with
    | Tpat_construct (_, cstr, _, args, _) -> construct_neg cstr args
    | Tpat_constant (Const_int n) -> Some (ground_neg (Refinement.Pint n))
    | Tpat_tuple comps -> tuple_neg comps
    | _ -> None
  in
  match pat.pat_desc with
  | Tpat_value p -> value_neg (p :> value general_pattern)
  | Tpat_construct (_, cstr, _, args, _) -> construct_neg cstr args
  | Tpat_tuple comps -> tuple_neg comps
  | _ -> None
;;

(* The constructor head an arm's own positive match fact asserts, when it asserts one: a
   (possibly aliased) constructor pattern over a simple variant -- exactly the shape whose
   [match_facts] emits [s = C ...] at the top level. *)
let pattern_positive_head : type k. Env.t -> k general_pattern -> string option =
  fun env pat ->
  let head cstr =
    let path = Data_types.cstr_res_type_path cstr in
    match datatype_sort env path [] with
    | S_int
    | S_bool
    | S_param _
    | S_tuple _
    | S_iarray
    | S_poly _
    | S_lean _
    | S_arrow _
    | S_other -> None
    | S_data (_, _) -> Some cstr.Data_types.cstr_name
  in
  let rec head_of : type k. k general_pattern -> string option =
    fun p ->
    match p.pat_desc with
    | Tpat_value v -> head_of (v :> value general_pattern)
    | Tpat_construct (_, cstr, _, _, _) -> head cstr
    | Tpat_alias { pattern = sub; _ } -> head_of sub
    | _ -> None
  in
  head_of pat
;;

(* The earlier-arm negations an arm actually needs, tagged with the earlier arm's pattern
   span as provenance. When the arm's own pattern asserts [s = C ...], a negation
   [not (s is C')] with a DIFFERENT constructor is subsumed by that equality (constructors
   are distinct in the model) and dropped; a SAME-name negation is kept -- it makes a
   duplicated arm's context inconsistent, which is what proves the dead arm. *)
let live_negations
  : type k.
    Env.t
    -> k general_pattern
    -> (Refinement.pred * Location.t) list
    -> (Refinement.pred * Location.t option) list
  =
  fun env pat negs ->
  let negs =
    match pattern_positive_head env pat with
    | None -> negs
    | Some cname ->
      List.filter
        (fun (n, _) ->
          match n with
          | Refinement.Pnot (Refinement.Pis (_, c, _)) -> String.equal c cname
          | _ -> true)
        negs
  in
  List.map (fun (n, l) -> n, Some l) negs
;;

(* Extend the context at a binding pattern: new stamps come into scope; refined binders
   contribute their facts (plus the scrutinee's refinement for unpack patterns). *)
let extend_pat
  : type k.
    ?toplevel:bool
    -> ?via_skel:bool
    -> ?scrut:type_expr
    -> ?scrut_name:Refinement.pred option
    -> Env.t
    -> ctx
    -> k general_pattern
    -> ctx
  =
  fun ?(toplevel = false) ?(via_skel = false) ?scrut ?(scrut_name = None) env ctx pat ->
  let bound = pat_bound_idents pat in
  List.iter
    (fun (id, (sloc : string Location.loc), ty, _, _) ->
      if !Clflags.vox_dump_vc_provenance then Hashtbl.replace name_locs id sloc.loc;
      if toplevel then Hashtbl.replace toplevel_names id ();
      check_binder_escape ~toplevel ctx ~extra_scope:bound pat id ty)
    (pat_bound_idents_full pat);
  let unpack =
    match scrut with
    | Some s -> unpack_fact env pat ~scrut:s ~scrut_name
    | None -> []
  in
  { cfacts =
      prov (Some pat.pat_loc) (unpack @ binder_facts ~via_skel env pat) @ ctx.cfacts
  ; cscope = bound @ ctx.cscope
  }
;;

(* Whether a computation pattern is free of exception patterns: only then does matching it
   guarantee the scrutinee ran to completion. *)
let rec exceptionless (p : computation general_pattern) =
  match p.pat_desc with
  | Tpat_value _ -> true
  | Tpat_exception _ -> false
  | Tpat_or (a, b, _) -> exceptionless a && exceptionless b
;;

(* The single value arm of a match, when it has exactly one arm (a computation case
   wrapping a value pattern, as unpack and destructuring lets desugar to). An arm
   containing an exception pattern does not qualify: it can be reached with the scrutinee
   interrupted between writes, so its state may not be threaded. *)
let single_arm : computation case list -> value case list -> computation case option =
  fun comp_cases val_cases ->
  match comp_cases, val_cases with
  | [ c ], [] when exceptionless c.c_lhs -> Some c
  | _ -> None
;;

(* Walk an expression under a logical context, collecting VCs. Returns the context for the
   expression's CONTINUATION: mutable-variable assignments extend it with the fresh
   version's definitional equation (and declared-refinement instance), and joins extend it
   with join facts. Everything is path-scoped -- facts proved under a branch's hypotheses
   never reach a sibling branch -- and the version table is saved and restored around
   branching so each branch names the state it actually sees. *)
(* Record the proof state of a program point ([-vox-dump-states]): the facts usable there
   (same scope filter a VC emitted there would apply) and every LOCAL tracked binder in
   scope (module-level names are the interface, not context). First record per span wins
   -- re-walks (guards, joins) do not overwrite the entry state. *)
let record_point loc ctx =
  let already = List.exists (fun (l, _, _) -> l = loc) !point_states in
  if not already
  then (
    let usable = List.filter (fun (f, _) -> pred_in_scope ctx f) ctx.cfacts in
    let seen = Hashtbl.create 8 in
    let ids =
      List.filter
        (fun id ->
          Hashtbl.mem name_sorts id
          && (not (Hashtbl.mem synthetic_names id))
          && (not (Hashtbl.mem toplevel_names id))
          &&
          let u = Ident.unique_name id in
          if Hashtbl.mem seen u
          then false
          else (
            Hashtbl.add seen u ();
            true))
        ctx.cscope
    in
    point_states := (loc, usable, ids) :: !point_states)
;;

let rec walk_expr _outer_env ctx (e : expression) : ctx =
  (* Use the node's OWN env, re-derived at every recursive call: an env threaded from the
     enclosing structure misses type declarations introduced by let-module (and friends)
     inside the expression, whose types would then silently sort at VoxU -- same bug class
     as the walk_items nested-module fix. *)
  let env = e.exp_env in
  if !Clflags.vox_dump_states && not e.exp_loc.Location.loc_ghost
  then record_point e.exp_loc ctx;
  (* Intro forms: the node itself carries the vox attribute and the refined type. *)
  let kind =
    if has_vox_attr "vox.refine" e.exp_attributes
    then Some Prove
    else if has_vox_attr "vox.assume" e.exp_attributes
    then Some Runtime_check
    else if has_vox_attr "vox.assume_unchecked" e.exp_attributes
    then Some Assume
    else None
  in
  (match kind with
   | Some kind ->
     (match refinement_of_type env e.exp_type with
      | Some p ->
        register_pred_paths env p;
        (* An intro-marked APPLICATION re-proves the expected refinement of a value whose
           own instantiated result refinement is a fact: selfify that refinement at the
           node's name -- the inline unpack that [let q = f x in q] used to spell. Sound:
           the value satisfies its type, and on any path where the goal matters the call
           has returned.

           A boolean CONNECTIVE ([&&]/[||]/[not]) is not a dependent call, so
           [apply_result_type] recovers nothing; model it with [decompose_bool] instead,
           whose formula is over the operand NAMES and whose (short-circuit-guarded) spec
           facts establish each refined-call operand's result -- otherwise the value is an
           opaque unknown and a TRUE connective goal spuriously DISPROVES (task #67; the
           operand facts are only dropped, never mis-stated, so this was a completeness
           gap, not unsound). *)
        let n, self_hyps =
          match e.exp_desc with
          | Texp_apply
              ( { exp_desc = Texp_ident { desc = { val_kind = Val_prim prim; _ }; _ }; _ }
              , _
              , _
              , _
              , _ )
            when String.equal prim.prim_name "%sequand"
                 || String.equal prim.prim_name "%sequor"
                 || String.equal prim.prim_name "%boolnot" ->
            decompose_bool env ~guard:(Refinement.Pbool true) e
          | Texp_apply (funct, args, _, _, _) ->
            let n = name_of_expr env e in
            ( n
            , (match refinement_of_type env (apply_result_type env funct args) with
               | Some ps when not (Refinement.equal ps p) ->
                 register_pred_paths env ps;
                 [ Refinement.subst_bound ~by:n ps ]
               | _ -> []) )
          | _ -> name_of_expr env e, []
        in
        emit_vc
          ~env
          ~loc:e.exp_loc
          ~ctx:{ ctx with cfacts = prov None self_hyps @ ctx.cfacts }
          ~goal:(Refinement.subst_bound ~by:n p)
          ~kind
      | None -> ())
   | None -> ());
  (* A [@vox.invariant] anywhere but on a loop would otherwise be SILENTLY unchecked --
     the worst failure mode for a verification annotation. *)
  (match e.exp_desc, loop_invariant e with
   | (Texp_while _ | Texp_for _), _ | _, None -> ()
   | _, Some (_, attr_loc) ->
     Location.raise_errorf
       ~loc:attr_loc
       "vox: [@vox.invariant] is only supported on while and for loops");
  match e.exp_desc with
  | Texp_apply (funct, args, _, _, _) ->
    (* The function and its arguments evaluate in unspecified order (right-to-left in
       practice): as in the generic traversal, each child starts from the entry versions
       with everything this application writes havocked, and the continuation havocs it
       again. For pure applications ([written] empty) this is identical to walking every
       child under the entry context. *)
    let saved = save_versions () in
    let written = written_mutables e in
    let child_ctx child =
      restore_versions saved;
      { ctx with cfacts = prov None (sibling_havoc env ~written child) @ ctx.cfacts }
    in
    ignore (walk_expr env (child_ctx funct) funct : ctx);
    (* Contract obligations (parameters as preconditions): each argument for a refined
       parameter must satisfy the predicate at its logical name; an intro-form argument
       ([refine_]/[assume_]/[assume_unchecked_]) carries its own obligation instead (the
       explicit-cast spelling). The dependent binder is substituted by the argument's
       translation (a variable, literal, or pure reflected expression -- enforced at
       typing time) as the spine is walked, mirroring the application-site opening. The
       obligation is emitted under the argument's child context, whose version state is
       what [name_of_expr] reads. *)
    let arrow_ty = ref funct.exp_type in
    List.iter
      (fun (_lbl, (arg : apply_arg)) ->
        let arg_expr =
          match arg with
          | Arg (a, _) -> Some a
          | Omitted _ -> None
        in
        let actx =
          match arg_expr with
          | Some a ->
            let actx = child_ctx a in
            ignore (walk_expr env actx a : ctx);
            actx
          | None -> ctx
        in
        match get_desc (Ctype.vox_expand_head env !arrow_ty) with
        | Tarrow ((_, _, _, binder), dom, ret, _) ->
          (match arg_expr with
           | Some a ->
             (match param_refinement env dom with
              | Some p
                when not
                       (has_vox_attr "vox.refine" a.exp_attributes
                        || has_vox_attr "vox.assume" a.exp_attributes
                        || has_vox_attr "vox.assume_unchecked" a.exp_attributes) ->
                register_pred_paths env p;
                (* Discharge the precondition at the SAME name the binder substitution
                   uses ([stable_arg_name]): for a call named by its exact result contract
                   (tier 2) this is the contract term (e.g. [bins x s]), so an invariant
                   precondition on the parameter type discharges from the callee's result
                   laws instead of stalling on a fresh unknown. A non-stable argument
                   keeps [name_of_expr]. *)
                let subject =
                  match stable_arg_name a with
                  | Some by -> by
                  | None -> name_of_expr env a
                in
                emit_vc
                  ~env
                  ~loc:a.exp_loc
                  ~ctx:actx
                  ~goal:(Refinement.subst_bound ~by:subject p)
                  ~kind:Prove
              | _ -> ());
             (match binder, stable_arg_name a with
              | Some b, Some by -> arrow_ty := Vox_dep.subst_binder b ~by ret
              | _ -> arrow_ty := ret)
           | None -> arrow_ty := ret)
        | _ -> ())
      args;
    restore_versions saved;
    { ctx with cfacts = prov None (mut_havoc_written env e) @ ctx.cfacts }
  | Texp_let (rec_flag, [ vb ], body) ->
    (* Reflected definitions are global; a local one could capture enclosing variables
       (translate_def's closedness check would also catch that, but the restriction is the
       honest one). *)
    reject_local_reflect vb;
    let ctx0 = walk_expr env ctx vb.vb_expr in
    let ctx' = extend_pat ~via_skel:true env ctx0 vb.vb_pat in
    (* A destructuring let of a variable gets the same facts a match case would:
       [let { x; y } = r in ...]. A let of a MUTABLE variable additionally pins its
       current value to the immutable binder ([let x = m]) -- the only way to name a
       mutable variable's value, since mutable stamps may not appear in refinements or
       dependent applications. A plain [let y = x] of an immutable variable is skipped:
       its alias fact is the SELF fact below (the variable arm of [match_facts] would
       duplicate it). *)
    let ctx' =
      match vb.vb_expr.exp_desc, vb.vb_pat.pat_desc with
      | Texp_ident _, Tpat_var _ -> ctx'
      | Texp_ident { path = Path.Pident id; _ }, _ ->
        { ctx' with
          cfacts =
            prov (Some vb.vb_pat.pat_loc) (match_facts env (Refinement.Pvar id) vb.vb_pat)
            @ ctx'.cfacts
        }
      | Texp_ident { path = (Path.Pdot _ | Path.Papply _) as p; _ }, _ ->
        { ctx' with
          cfacts =
            prov
              (Some vb.vb_pat.pat_loc)
              (match_facts env (Refinement.Pglobal p) vb.vb_pat)
            @ ctx'.cfacts
        }
      | Texp_mutvar { txt = mid; _ }, _ ->
        (match Hashtbl.find_opt mut_versions mid with
         | Some (v, _) ->
           (* [match_facts] ties a variable pattern to the version directly and
              destructures records/constructors through it. *)
           { ctx' with
             cfacts =
               prov
                 (Some vb.vb_pat.pat_loc)
                 (match_facts env (Refinement.Pvar v) vb.vb_pat)
               @ ctx'.cfacts
           }
         | None -> ctx')
      | _, Tpat_var _ -> ctx' (* selfification below carries the name *)
      | _, _ ->
        { ctx' with
          cfacts =
            prov (Some vb.vb_pat.pat_loc) (destructure_facts env vb.vb_expr vb.vb_pat)
            @ ctx'.cfacts
        }
    in
    (* Selfification (no self fact for a RECURSIVE binding: a cyclic constructor equation
       is unsatisfiable in the datatype theory). *)
    let ctx' =
      match rec_flag with
      | Recursive -> ctx'
      | Nonrecursive ->
        { ctx' with cfacts = prov None (binding_self_facts env vb) @ ctx'.cfacts }
    in
    (* The whitespace between [in] and the body is inside the LET's span but outside the
       body's -- without this extra state (from the binding's end to the let's end) a
       cursor there would fall back to the state WITHOUT the freshly-bound name. *)
    if !Clflags.vox_dump_states && not e.exp_loc.Location.loc_ghost
    then
      record_point { e.exp_loc with Location.loc_start = vb.vb_loc.Location.loc_end } ctx';
    walk_expr env ctx' body
  | Texp_let (rec_flag, vbs, body) ->
    List.iter reject_local_reflect vbs;
    (* [let .. and ..]: sibling evaluation order is unspecified, so each right-hand side
       walks under the ENTRY context and every mutable variable any of them writes is
       havocked. *)
    let saved = save_versions () in
    let written = List.concat_map (fun vb -> written_mutables vb.vb_expr) vbs in
    List.iter
      (fun vb ->
        restore_versions saved;
        let hfacts = sibling_havoc env ~written vb.vb_expr in
        ignore
          (walk_expr env { ctx with cfacts = prov None hfacts @ ctx.cfacts } vb.vb_expr
           : ctx))
      vbs;
    restore_versions saved;
    let havoc = List.concat_map (mut_havoc env) written in
    let ctx' =
      List.fold_left (fun ctx vb -> extend_pat ~via_skel:true env ctx vb.vb_pat) ctx vbs
    in
    let ctx' = { ctx' with cfacts = prov None havoc @ ctx'.cfacts } in
    let ctx' =
      List.fold_left
        (fun ctx vb ->
          match vb.vb_expr.exp_desc, vb.vb_pat.pat_desc with
          | Texp_ident _, Tpat_var _ -> ctx
          | Texp_ident { path = Path.Pident id; _ }, _ ->
            { ctx with
              cfacts =
                prov
                  (Some vb.vb_pat.pat_loc)
                  (match_facts env (Refinement.Pvar id) vb.vb_pat)
                @ ctx.cfacts
            }
          | Texp_ident { path = (Path.Pdot _ | Path.Papply _) as p; _ }, _ ->
            { ctx with
              cfacts =
                prov
                  (Some vb.vb_pat.pat_loc)
                  (match_facts env (Refinement.Pglobal p) vb.vb_pat)
                @ ctx.cfacts
            }
          | _, Tpat_var _ -> ctx
          | _, _ ->
            { ctx with
              cfacts =
                prov (Some vb.vb_pat.pat_loc) (destructure_facts env vb.vb_expr vb.vb_pat)
                @ ctx.cfacts
            })
        ctx'
        vbs
    in
    let ctx' =
      (* RECURSIVE bindings contribute no self fact: a cyclic constructor equation
         ([let rec ones = 1 :: ones]) is unsatisfiable in the solver's well-founded
         datatype theory, which would make the hypotheses inconsistent. A group that
         writes mutable variables contributes none either: sibling order makes its RHS
         names unstable. *)
      match rec_flag with
      | Nonrecursive when written = [] ->
        List.fold_left
          (fun ctx vb ->
            { ctx with cfacts = prov None (binding_self_facts env vb) @ ctx.cfacts })
          ctx'
          vbs
      | Recursive | Nonrecursive -> ctx'
    in
    if !Clflags.vox_dump_states && not e.exp_loc.Location.loc_ghost
    then (
      match List.rev vbs with
      | last :: _ ->
        record_point
          { e.exp_loc with Location.loc_start = last.vb_loc.Location.loc_end }
          ctx'
      | [] -> ());
    walk_expr env ctx' body
  | Texp_letmutable (vb, body) ->
    let ctx0 = walk_expr env ctx vb.vb_expr in
    backstop_pat ctx0 vb.vb_pat;
    (match vb.vb_pat.pat_desc with
     | Tpat_var { id; _ } ->
       let ty = vb.vb_pat.pat_type in
       let rhs = name_of_expr env vb.vb_expr in
       let facts = mut_assign env id ty ~rhs in
       let bctx = { ctx0 with cfacts = prov None facts @ ctx0.cfacts } in
       if !Clflags.vox_dump_states && not e.exp_loc.Location.loc_ghost
       then
         record_point
           { e.exp_loc with Location.loc_start = vb.vb_loc.Location.loc_end }
           bctx;
       let out = walk_expr env bctx body in
       (* the binder's scope ends; its versions (synthetic) live on *)
       Hashtbl.remove mut_versions id;
       out
     | _ ->
       (* the extension only allows single-variable patterns; stay conservative if that
          ever changes *)
       walk_expr env ctx0 body)
  | Texp_setmutvar ({ txt = id; _ }, _, rhs) ->
    let ctx0 = walk_expr env ctx rhs in
    (match Hashtbl.find_opt mut_versions id with
     | Some (_, ty) ->
       (* name the right-hand side BEFORE minting: its reads use the version being
          replaced *)
       let rhs_name = name_of_expr env rhs in
       { ctx0 with cfacts = prov None (mut_assign env id ty ~rhs:rhs_name) @ ctx0.cfacts }
     | None -> ctx0)
  | Texp_mutvar _ -> ctx
  | Texp_sequence (e1, _, e2) ->
    let ctx1 = walk_expr env ctx e1 in
    (* Thread e1's result refinement to e2, exactly as [let () = e1 in e2] would (RULE 1):
       name e1's value and instantiate its result refinement there. A diverging e1
       (Rule 2) contributes [false], so dead code after a mid-body raise is vacuous; a
       call with a dependent postcondition contributes that postcondition. *)
    let ctx1 =
      match result_refinement env e1 with
      | Some p ->
        register_pred_paths env p;
        let n = name_of_expr env e1 in
        { ctx1 with
          cfacts =
            prov
              (Some e1.exp_loc)
              (List.filter nontrivial_fact [ Refinement.subst_bound ~by:n p ])
            @ ctx1.cfacts
        }
      | None -> ctx1
    in
    walk_expr env ctx1 e2
  | Texp_match (scrut, _sort, comp_cases, val_cases, _partial) ->
    let saved_pre = save_versions () in
    let ctx0 = walk_expr env ctx scrut in
    let scrut_id =
      match scrut.exp_desc with
      | Texp_ident { path = Path.Pident id; _ } -> Some (Refinement.Pvar id)
      | Texp_ident { path = (Path.Pdot _ | Path.Papply _) as p; _ } ->
        (* A module-level scrutinee matches like a local one: its path name receives the
           match facts (loads are pure, so receiving facts stays vacuously sound for
           exception and effect arms). *)
        Some (Refinement.Pglobal p)
      | Texp_mutvar { txt = id; _ } ->
        (* the version pins the value read by the match *)
        Option.map (fun (v, _) -> Refinement.Pvar v) (Hashtbl.find_opt mut_versions id)
      | _ ->
        (* Any other scrutinee is destructured through its NAME (its logic translation, or
           a fresh unknown): value arms tie their patterns to it below; interrupted arms
           are already excluded. *)
        Some (name_of_expr env scrut)
    in
    (* The named scrutinee's refinement holds at the name (a variable scrutinee's binder
       facts already carry it). *)
    let scrut_facts : type k. k general_pattern -> Refinement.pred list =
      fun pat ->
      match scrut.exp_desc, scrut_id with
      | (Texp_ident _ | Texp_mutvar _), _ | _, None -> []
      | _, Some n ->
        (match
           match result_refinement env scrut with
           | Some p -> Some p
           | None -> refinement_of_type env pat.pat_type
         with
         | Some p ->
           register_pred_paths env p;
           List.filter nontrivial_fact [ Refinement.subst_bound ~by:n p ]
         | None -> [])
    in
    (match single_arm comp_cases val_cases with
     | Some c ->
       (* A single-arm match (unpacks [let refine_ x = e] and destructuring lets desugar
          to these) is straight-line code: the arm's out-context IS the continuation's
          state -- thread it, versions included, instead of joining. Sound also for a
          partial single-arm match: on pattern failure the continuation is unreachable. *)
       let ctx' =
         extend_pat ~scrut:scrut.exp_type ~scrut_name:scrut_id env ctx0 c.c_lhs
       in
       let ctx' =
         match scrut_id with
         | Some sid ->
           { ctx' with
             cfacts =
               prov
                 (Some c.c_lhs.pat_loc)
                 (scrut_facts c.c_lhs
                  (* A [refine_] unpack of a VIA value binds at the BASE while the
                     scrutinee is at the IMAGE, so the subject-alias fact [x = s] would be
                     ill-sorted; [unpack_fact] already supplied the correct link
                     [composite x = s]. (An ordinary refine_ keeps the alias: binder and
                     scrutinee share the base sort.) *)
                  @
                  if has_vox_attr "vox.refine" c.c_lhs.pat_attributes
                     &&
                     match get_desc (Ctype.vox_expand_head env scrut.exp_type) with
                     | Trefine (_, _ :: _, _) -> true
                     | _ -> false
                  then []
                  else match_facts env sid c.c_lhs)
               @ ctx'.cfacts
           }
         | None -> ctx'
       in
       let gctx =
         match c.c_guard with
         | None -> ctx'
         | Some g -> walk_expr env ctx' g
       in
       walk_expr env gctx c.c_rhs
     | None ->
       let saved = save_versions () in
       let do_case
         : type k.
           interrupted:bool -> (Refinement.pred * Location.t) list -> k case -> unit
         =
         fun ~interrupted negs c ->
         let base =
           if interrupted
           then (
             (* the arm can be reached with [scrut] interrupted between writes: neither
                its threaded versions nor its facts are valid here. Start from the
                pre-scrutinee state, with everything the scrutinee writes havocked. *)
             restore_versions saved_pre;
             { ctx with
               cfacts =
                 prov None (List.concat_map (mut_havoc env) (written_mutables scrut))
                 @ ctx.cfacts
             })
           else (
             restore_versions saved;
             ctx0)
         in
         let ctx' =
           if interrupted
           then extend_pat env base c.c_lhs
           else extend_pat ~scrut:scrut.exp_type ~scrut_name:scrut_id env base c.c_lhs
         in
         let ctx' =
           match scrut_id with
           | Some sid when not interrupted ->
             { ctx' with
               cfacts =
                 prov
                   (Some c.c_lhs.pat_loc)
                   (scrut_facts c.c_lhs @ match_facts env sid c.c_lhs)
                 @ live_negations env c.c_lhs negs
                 @ ctx'.cfacts
             }
           | _ -> ctx'
         in
         let gctx =
           match c.c_guard with
           | None -> ctx'
           | Some g -> walk_expr env ctx' g
         in
         ignore (walk_expr env gctx c.c_rhs : ctx)
       in
       (* Arms additionally see the negations of the guard-free simple arms ABOVE them.
          All ordinary arms -- value and exception, in source order -- arrive as
          computation cases (value patterns wrapped in [Tpat_value]); [val_cases] holds
          effect-handler arms. Exception and effect arms never contribute a negation
          (their patterns are not simple-variant constructors: exception and effect types
          are open), and they are INTERRUPTED arms: control reaches them with the
          scrutinee stopped between writes, so they receive the pre-scrutinee state
          (writes havocked) rather than the scrutinee's threaded facts and versions. *)
       let run_cases : type k. (k general_pattern -> bool) -> k case list -> unit =
         fun is_interrupted cases ->
         ignore
           (List.fold_left
              (fun negs c ->
                do_case ~interrupted:(is_interrupted c.c_lhs) negs c;
                match scrut_id, c.c_guard with
                | Some sid, None ->
                  (match pattern_negation env sid c.c_lhs with
                   | Some n -> negs @ [ n, c.c_lhs.pat_loc ]
                   | None -> negs)
                | _ -> negs)
              []
              cases
            : (Refinement.pred * Location.t) list)
       in
       run_cases (fun p -> not (exceptionless p)) comp_cases;
       run_cases (fun _ -> true) val_cases;
       restore_versions saved;
       (* havoc-join across arms in v1: written variables get a fresh version, keeping
          only the declared refinement. When some arm is interrupted, the continuation can
          be reached without the scrutinee having completed, so its facts may not be kept
          either. *)
       let base =
         if List.exists (fun c -> not (exceptionless c.c_lhs)) comp_cases
            || val_cases <> []
         then ctx
         else ctx0
       in
       { base with cfacts = prov None (mut_havoc_written env e) @ base.cfacts })
  | Texp_ifthenelse (cond, e_then, e_else) ->
    let ctx0 = walk_expr env ctx cond in
    (* The path fact is the condition's logic translation when it has one (a variable, or
       a translatable int/bool expression); translatable implies pure, so the versions its
       reads name are stable. When it does NOT translate, treat [if c] as
       [let n = c in if n]: DECOMPOSE the condition's &&/||/not structure, naming each
       untranslatable leaf and attaching a refined leaf's result refinement at its name
       (guarded by short-circuit), so a refined-bool decision procedure threads its spec
       into the branches even inside a conjunction ([if b && mem k t]). The then-branch
       gets the condition's formula, the else-branch its negation, and both (with the
       continuation) the guarded spec equations. A condition with no refined leaf
       contributes nothing, exactly as before. *)
    let cond_fact = Vox_reflect.translate ~mutvar:mut_read cond in
    Option.iter (register_pred_paths env) cond_fact;
    let ctx0, path_cond =
      match cond_fact with
      | Some c -> ctx0, Some c
      | None ->
        let formula, side_facts =
          decompose_bool env ~guard:(Refinement.Pbool true) cond
        in
        (match side_facts with
         | [] -> ctx0, None
         | _ ->
           register_pred_paths env formula;
           ( { ctx0 with cfacts = prov (Some cond.exp_loc) side_facts @ ctx0.cfacts }
           , Some formula ))
    in
    let with_fact f ctx =
      match path_cond with
      | None -> ctx
      | Some c -> { ctx with cfacts = (f c, Some cond.exp_loc) :: ctx.cfacts }
    in
    let saved = save_versions () in
    ignore (walk_expr env (with_fact (fun c -> c) ctx0) e_then : ctx);
    let vers_then = save_versions () in
    restore_versions saved;
    (match e_else with
     | Some e2 ->
       ignore (walk_expr env (with_fact (fun c -> Refinement.Pnot c) ctx0) e2 : ctx)
     | None -> ());
    let vers_else = save_versions () in
    restore_versions saved;
    (* Conditional join: a variable written by either branch gets a fresh version equated
       with the surviving branch's version under the reflected condition (havoc when the
       condition did not reflect). *)
    let join_facts =
      List.concat_map
        (fun (id, (v0, ty)) ->
          let vt = Option.value (version_in vers_then id) ~default:v0 in
          let ve = Option.value (version_in vers_else id) ~default:v0 in
          if Ident.same vt v0 && Ident.same ve v0
          then []
          else (
            let vj = mut_fresh env id ty in
            let inv = mut_invariant env ty vj in
            match cond_fact with
            | Some c ->
              Refinement.Por
                ( Refinement.Pand
                    ( c
                    , Refinement.Pbinop
                        (Refinement.Eq, Refinement.Pvar vj, Refinement.Pvar vt) )
                , Refinement.Pand
                    ( Refinement.Pnot c
                    , Refinement.Pbinop
                        (Refinement.Eq, Refinement.Pvar vj, Refinement.Pvar ve) ) )
              :: inv
            | None -> inv))
        saved
    in
    { ctx0 with cfacts = prov None join_facts @ ctx0.cfacts }
  | Texp_while { wh_cond; wh_body; _ } ->
    (* Head state: havoc everything the loop writes; head versions denote any iteration's
       entry, and the declared refinements re-attach (every write re-proved them). A
       [@vox.invariant] formula additionally follows the classical quadruple: ASSERTED
       over the entry versions, ASSUMED over the head versions, ASSERTED over the
       body-exit versions at the back-edge; after the loop the head assumption stands with
       the negated guard. The body walks under the reflected condition; normal exit
       happens at the test, so the continuation sees the head state plus its negation. *)
    let inv = elab_loop_invariant e.exp_env e in
    (match inv with
     | Some (template, attr_loc) ->
       (* entry: the first iteration's head state is the current one *)
       emit_vc ~env ~loc:attr_loc ~ctx ~goal:(close_over_versions template) ~kind:Prove
     | None -> ());
    let head = prov None (mut_havoc_written env e) in
    let head =
      match inv with
      | Some (template, attr_loc) -> (close_over_versions template, Some attr_loc) :: head
      | None -> head
    in
    let hctx = { ctx with cfacts = head @ ctx.cfacts } in
    let cctx = walk_expr env hctx wh_cond in
    let cond_fact = Vox_reflect.translate ~mutvar:mut_read wh_cond in
    (* Same treatment as [if]: decompose the condition's &&/||/not structure, so the body
       sees the condition's formula plus the guarded spec equations of its refined leaves
       and normal exit sees the negated formula. *)
    let cctx, path_cond =
      match cond_fact with
      | Some c -> cctx, Some c
      | None ->
        let formula, side_facts =
          decompose_bool env ~guard:(Refinement.Pbool true) wh_cond
        in
        (match side_facts with
         | [] -> cctx, None
         | _ ->
           register_pred_paths env formula;
           ( { cctx with cfacts = prov (Some wh_cond.exp_loc) side_facts @ cctx.cfacts }
           , Some formula ))
    in
    let saved = save_versions () in
    let bctx =
      match path_cond with
      | Some c -> { cctx with cfacts = (c, Some wh_cond.exp_loc) :: cctx.cfacts }
      | None -> cctx
    in
    let bctx_out = walk_expr env bctx wh_body in
    (match inv with
     | Some (template, attr_loc) ->
       (* back-edge: the next iteration's head state is the body's exit state *)
       emit_vc
         ~env
         ~loc:attr_loc
         ~ctx:bctx_out
         ~goal:(close_over_versions template)
         ~kind:Prove
     | None -> ());
    restore_versions saved;
    (match path_cond with
     | Some c ->
       { cctx with cfacts = (Refinement.Pnot c, Some wh_cond.exp_loc) :: cctx.cfacts }
     | None -> cctx)
  | Texp_for { for_id; for_from; for_to; for_dir; for_body; _ } ->
    let c0 = walk_expr env ctx for_from in
    let c1 = walk_expr env c0 for_to in
    (* Bounds are evaluated once, before any body write: NAME them (their reflection when
       translatable, a fresh unknown otherwise) before havocking. One name per bound
       serves the head bounds and the entry/post-loop index instances alike, so even an
       opaque bound yields a consistent quadruple. *)
    let from_n = name_of_expr env for_from in
    let to_n = name_of_expr env for_to in
    record_name env for_id for_from.exp_type;
    (* The invariant elaborates in the BODY's environment, where the index is bound. An
       index mention makes the quadruple index-aware: the entry assertion instantiates the
       index at the FIRST value, the back-edge assertion at the NEXT value (what it
       establishes is the next iteration's head state), and after the loop the head
       assumption stands at the one-past-the-end value when the loop ran -- at the first
       value otherwise (the entry assertion, over unchanged variables). *)
    let inv = elab_loop_invariant for_body.exp_env e in
    let mentions_index =
      match inv with
      | Some (template, _) ->
        List.exists (Ident.same for_id) (Refinement.free_vars template)
      | None -> false
    in
    let step p =
      let op =
        match for_dir with
        | Upto -> Refinement.Add
        | Downto -> Refinement.Sub
      in
      Refinement.Pbinop (op, p, Refinement.Pint 1)
    in
    let at_index by template =
      if mentions_index
      then (
        match by with
        | `First -> Refinement.subst_var for_id ~by:from_n template
        | `Past -> Refinement.subst_var for_id ~by:(step to_n) template
        | `Next ->
          Refinement.subst_var for_id ~by:(step (Refinement.Pvar for_id)) template)
      else template
    in
    (match inv with
     | Some (template, attr_loc) ->
       emit_vc
         ~env
         ~loc:attr_loc
         ~ctx:c1
         ~goal:(close_over_versions (at_index `First template))
         ~kind:Prove
     | None -> ());
    let head_havoc = mut_havoc_written env e in
    let head_inv =
      match inv with
      | Some (template, attr_loc) -> [ close_over_versions template, Some attr_loc ]
      | None -> []
    in
    (* The post-loop instance of the invariant: over the head (havoc) versions, which also
       denote the final state. With an index mention it splits on whether the loop ran;
       the empty case keeps the entry instance, sound because nothing was written. *)
    let post_inv =
      match inv with
      | None -> []
      | Some (template, attr_loc) ->
        if not mentions_index
        then head_inv
        else (
          let ran, empty =
            match for_dir with
            | Upto ->
              ( Refinement.Pbinop (Refinement.Le, from_n, to_n)
              , Refinement.Pbinop (Refinement.Gt, from_n, to_n) )
            | Downto ->
              ( Refinement.Pbinop (Refinement.Ge, from_n, to_n)
              , Refinement.Pbinop (Refinement.Lt, from_n, to_n) )
          in
          [ ( Refinement.Por
                ( Refinement.Pand (empty, close_over_versions (at_index `First template))
                , Refinement.Pand (ran, close_over_versions (at_index `Past template)) )
            , Some attr_loc )
          ])
    in
    let bounds =
      let lo, hi =
        match for_dir with
        | Upto -> from_n, to_n
        | Downto -> to_n, from_n
      in
      [ Refinement.Pbinop (Refinement.Le, lo, Refinement.Pvar for_id)
      ; Refinement.Pbinop (Refinement.Le, Refinement.Pvar for_id, hi)
      ]
    in
    let bctx =
      { cfacts =
          prov (Some e.exp_loc) bounds @ head_inv @ prov None head_havoc @ c1.cfacts
      ; cscope = for_id :: c1.cscope
      }
    in
    let saved = save_versions () in
    let bctx_out = walk_expr env bctx for_body in
    (match inv with
     | Some (template, attr_loc) ->
       emit_vc
         ~env
         ~loc:attr_loc
         ~ctx:bctx_out
         ~goal:(close_over_versions (at_index `Next template))
         ~kind:Prove
     | None -> ());
    restore_versions saved;
    { c1 with cfacts = post_inv @ prov None head_havoc @ c1.cfacts }
  | Texp_function { params; body; _ } ->
    (* A function body runs at call time: outer mutable variables are not live inside it
       (closures cannot capture them), so suspend the version table -- reads cannot occur,
       and invariants inside the body mentioning outer mutables are rejected by the
       liveness check rather than silently mis-instantiated. *)
    let suspended = save_versions () in
    Hashtbl.reset mut_versions;
    Fun.protect ~finally:(fun () -> restore_versions suspended)
    @@ fun () ->
    (* Contract facts (parameters as preconditions): a refined arrow DOMAIN contributes
       its predicate at the parameter's name -- the parameter itself is bound at the
       skeleton, and every caller discharged the predicate at its argument. The arrow's
       dependent binder is substituted by the parameter's stamp as the spine is walked,
       mirroring the definition-site opening done at typing time. (A parameter whose
       PATTERN still carries the refined type -- the pattern-annotation spelling --
       contributes through [binder_facts] instead; the guard avoids the duplicate.) *)
    let arrow_ty = ref e.exp_type in
    let ctx' =
      List.fold_left
        (fun ctx fp ->
          let pat, is_default =
            match fp.fp_kind with
            | Tparam_pat pat -> pat, false
            | Tparam_optional_default (pat, default, _) ->
              ignore (walk_expr env ctx default : ctx);
              pat, true
          in
          let ctx = extend_pat env ctx pat in
          match get_desc (Ctype.vox_expand_head env !arrow_ty) with
          | Tarrow ((_, _, _, binder), dom, ret, _) ->
            let id_opt =
              match pat.pat_desc with
              | Tpat_var { id; _ } -> Some id
              | _ -> None
            in
            let ctx =
              (* The dedup guard keys on the BINDERS' types, not the pattern's: a refined
                 pattern annotation keeps the refined [pat_type] (that is what flows to
                 the arrow) while binding its variable at the skeleton, and its fact must
                 come from here; only a binder that itself carries the refined type
                 (inference-refined parameters) contributes through [binder_facts]
                 instead. *)
              match param_refinement env dom with
              | Some p
                when (not is_default)
                     && List.for_all
                          (fun (_, _, ty, _, _) ->
                            Option.is_none (refinement_of_type env ty))
                          (pat_bound_idents_full pat) ->
                let name =
                  match id_opt with
                  | Some id -> Refinement.Pvar id
                  | None ->
                    incr unknown_counter;
                    let s =
                      Ident.create_local (Printf.sprintf "*param%d*" !unknown_counter)
                    in
                    record_name env s pat.pat_type;
                    Hashtbl.replace synthetic_names s ();
                    Refinement.Pvar s
                in
                register_pred_paths env p;
                { ctx with
                  cfacts =
                    (Refinement.subst_bound ~by:name p, Some pat.pat_loc) :: ctx.cfacts
                }
              | _ -> ctx
            in
            (match binder, id_opt with
             | Some b, Some id ->
               arrow_ty := Vox_dep.subst_binder b ~by:(Refinement.Pvar id) ret
             | _ -> arrow_ty := ret);
            ctx
          | _ -> ctx)
        ctx
        params
    in
    (match body with
     | Tfunction_body e -> ignore (walk_expr env ctx' e : ctx)
     | Tfunction_cases { fc_cases; fc_param; _ } ->
       (* The cases consume one more arrow, whose parameter is [fc_param]: a refined
          domain contributes its contract at [fc_param]'s stamp (the patterns were typed
          at the skeleton, like the other parameter spellings), and the cases are a match
          on [fc_param] -- they get match facts and the negations of earlier guard-free
          simple arms, exactly as [Texp_match] on a variable scrutinee. *)
       let ctx' =
         match get_desc (Ctype.vox_expand_head env !arrow_ty) with
         | Tarrow (_, dom, _, _) ->
           record_name env fc_param dom;
           (* [fc_param] is compiler-introduced: like the unnamed-param synthetics, it is
              always in scope for the cases. *)
           Hashtbl.replace synthetic_names fc_param ();
           (match param_refinement env dom with
            | Some p ->
              register_pred_paths env p;
              { ctx' with
                cfacts =
                  (Refinement.subst_bound ~by:(Refinement.Pvar fc_param) p, None)
                  :: ctx'.cfacts
              }
            | None -> ctx')
         | _ -> ctx'
       in
       ignore
         (List.fold_left
            (fun negs c ->
              let ctx'' = extend_pat env ctx' c.c_lhs in
              let ctx'' =
                { ctx'' with
                  cfacts =
                    prov
                      (Some c.c_lhs.pat_loc)
                      (match_facts env (Refinement.Pvar fc_param) c.c_lhs)
                    @ live_negations env c.c_lhs negs
                    @ ctx''.cfacts
                }
              in
              let gctx =
                match c.c_guard with
                | None -> ctx''
                | Some g -> walk_expr env ctx'' g
              in
              ignore (walk_expr env gctx c.c_rhs : ctx);
              match c.c_guard with
              | None ->
                (match pattern_negation env (Refinement.Pvar fc_param) c.c_lhs with
                 | Some n -> negs @ [ n, c.c_lhs.pat_loc ]
                 | None -> negs)
              | Some _ -> negs)
            []
            fc_cases
          : (Refinement.pred * Location.t) list));
    (* a function body runs at call time, not here: the continuation keeps the entry state
       (closures cannot capture mutable variables, so the body cannot write any variable
       we track) *)
    ctx
  | Texp_try (tried, cases, eff_cases) ->
    (* [tried] walks as straight-line code for its own VCs, but a handler arm runs with it
       interrupted between writes: arms receive the pre-try state with everything [tried]
       writes havocked (like the exception arms of a match), and the continuation --
       reachable through either path -- keeps the entry facts plus the havoc-join. *)
    let saved = save_versions () in
    ignore (walk_expr env ctx tried : ctx);
    restore_versions saved;
    let hctx =
      { ctx with
        cfacts =
          prov None (List.concat_map (mut_havoc env) (written_mutables tried))
          @ ctx.cfacts
      }
    in
    let hsaved = save_versions () in
    let do_handler (c : value case) =
      restore_versions hsaved;
      let ctx' = extend_pat env hctx c.c_lhs in
      let gctx =
        match c.c_guard with
        | None -> ctx'
        | Some g -> walk_expr env ctx' g
      in
      ignore (walk_expr env gctx c.c_rhs : ctx)
    in
    List.iter do_handler cases;
    List.iter do_handler eff_cases;
    restore_versions saved;
    { ctx with cfacts = prov None (mut_havoc_written env e) @ ctx.cfacts }
  | _ ->
    (* Generic traversal of children under the same context. Patterns reached this way
       belong to constructs the walker does not model (letops, local module structures,
       ...); they are escape-checked but contribute no facts. Children may evaluate in ANY
       order (arguments right-to-left in practice), so a child may neither see a sibling's
       threaded version nor keep an entry version a sibling may overwrite first: each
       child starts from the entry versions with everything this subtree writes havocked,
       and the continuation havocs it again. *)
    let saved = save_versions () in
    let written = written_mutables e in
    let it =
      { Tast_iterator.default_iterator with
        expr =
          (fun _ e' ->
            restore_versions saved;
            let hfacts = sibling_havoc env ~written e' in
            ignore
              (walk_expr env { ctx with cfacts = prov None hfacts @ ctx.cfacts } e' : ctx))
      ; pat =
          (fun sub (type k) (p : k general_pattern) ->
            backstop_pat ctx p;
            Tast_iterator.default_iterator.pat sub p)
      ; value_binding =
          (fun sub vb ->
            reject_local_reflect vb;
            Tast_iterator.default_iterator.value_binding sub vb)
      }
    in
    Tast_iterator.default_iterator.expr it e;
    restore_versions saved;
    let havoc = mut_havoc_written env e in
    if havoc = [] then ctx else { ctx with cfacts = prov None havoc @ ctx.cfacts }
;;

(* ------------------------------------------------------------------ *)
(* Serialization helpers *)

(* Arity of constructor [c] of the registered variant at [p]; testers of unregistered
   paths never reach serialization (usability filter). *)
let constr_arity p c =
  match find_datatype p with
  | Some (_, Dt_variant (_, constrs)) ->
    (match List.assoc_opt c constrs with
     | Some fields -> List.length fields
     | None -> 0)
  | Some (_, (Dt_record _ | Dt_opaque)) | None -> 0
;;

(* The stable name of the product datatype for tuple arity [n]: the same in every unit,
   like [path_uname]s, so clients deduplicate imported declarations by name. *)
let tuple_uname = Vox_module.tuple_uname

(* Does rendering this sort mention VoxU -- directly, inside a tuple instantiation, or as
   the degraded rendering of an unregistered datatype? *)
let rec sort_needs_voxu = function
  | S_other -> true
  | S_int | S_bool | S_iarray -> false
  (* a ghost sort renders its argument sorts, so a VoxU inside one ([(ISet VoxU)]) still
     needs VoxU emitted *)
  | S_lean (_, args) -> List.exists sort_needs_voxu args
  (* a parameter is a bound [Type] binder in the declaration it appears in, never VoxU *)
  | S_param _ -> false
  | S_tuple comps | S_poly (_, comps) -> List.exists sort_needs_voxu comps
  | S_arrow (a, b) -> sort_needs_voxu a || sort_needs_voxu b
  | S_data (p, args) -> find_datatype p = None || List.exists sort_needs_voxu args
;;

(* Same question for the built-in iarray theory (VoxIA and its operations), which is
   emitted only when something uses it. *)
let rec sort_needs_iarray = function
  | S_iarray -> true
  | S_int | S_bool | S_other | S_param _ -> false
  | S_lean (_, args) -> List.exists sort_needs_iarray args
  | S_tuple comps | S_poly (_, comps) -> List.exists sort_needs_iarray comps
  | S_arrow (a, b) -> sort_needs_iarray a || sort_needs_iarray b
  | S_data (_, args) -> List.exists sort_needs_iarray args
;;

(* The built-in iarray theory, emitted (right after VoxU) when anything in the input uses
   it: an S_iarray-sorted name, a datatype field at VoxIA, or a predicate applying the
   reserved operations. (An IMPORTED datatype decl referencing VoxIA in a module with no
   own iarray use is not detected -- the solver's unknown-identifier error fails closed
   there.) [get] is total in the logic, like division; length nonnegativity is the
   theory's one axiom, pattern-registered so grind instantiates it at every [len] term. *)
let lean_iarray_theory = Vox_module.lean_iarray_theory ()

(* A field sort that renders a named/via ([S_lean]) sort -- e.g. a via
   image type like [LList] -- directly or nested.  A datatype with such
   a field must be emitted AFTER the unit's blocks: the named sort may
   be DEFINED by a block (the via image's own [inductive]), and Lean
   resolves identifiers top-to-bottom, so a forward reference autobinds
   the name to a universe metavariable ([Sort ?u]) and the derived
   inductive fails to elaborate. *)
let rec sort_uses_lean = function
  | S_lean _ -> true
  | S_int | S_bool | S_other | S_iarray | S_param _ -> false
  | S_tuple comps | S_poly (_, comps) -> List.exists sort_uses_lean comps
  | S_data (_, args) -> List.exists sort_uses_lean args
  | S_arrow (a, b) -> sort_uses_lean a || sort_uses_lean b
;;

let dt_uses_lean_field = function
  | Dt_variant (_, cs) ->
    List.exists (fun (_, fs) -> List.exists sort_uses_lean fs) cs
  | Dt_record (_, fs) -> List.exists (fun (_, s) -> sort_uses_lean s) fs
  | Dt_opaque -> false
;;

let datatype_field_needs_iarray () =
  List.exists
    (fun (_, decl) ->
      match decl with
      | Dt_variant (_, constrs) ->
        List.exists (fun (_, fields) -> List.exists sort_needs_iarray fields) constrs
      | Dt_record (_, fields) -> List.exists (fun (_, fs) -> sort_needs_iarray fs) fields
      | Dt_opaque -> false)
    !datatypes
;;

let datatype_field_needs_voxu () =
  List.exists
    (fun (_, decl) ->
      match decl with
      | Dt_variant (_, constrs) ->
        List.exists (fun (_, fields) -> List.exists sort_needs_voxu fields) constrs
      | Dt_record (_, fields) -> List.exists (fun (_, fs) -> sort_needs_voxu fs) fields
      | Dt_opaque -> false)
    !datatypes
;;

let free_vars_of_vc vc = List.concat_map Refinement.free_vars (vc.vc_goal :: vc.vc_facts)

(* Embedded solver blocks: [%%vox.lean {lean|...|lean}] structure items carry solver-side
   text directly in the OCaml source. They are not "preludes": reflected definitions
   precede them, so a block may state lemmas about the module's own total_ functions.
   Blocks travel: an .mli's blocks -- and an mli-less unit's -- ride the .cmi's spec
   export to every client. *)

type vox_block_kind =
  | Not_a_block
  | Block
  | Bad_backend of string

let vox_block_of_extension txt =
  if String.equal txt "vox.lean"
  then Block
  else if String.length txt >= 4 && String.equal (String.sub txt 0 4) "vox."
  then
    (* Claim the whole vox.* item-extension namespace, so a misspelled block gets a vox
       error rather than "uninterpreted extension". *)
    Bad_backend txt
  else Not_a_block
;;

(* Whether Typemod should claim this extension item (including misspelled backends, so
   they get the vox error, not "uninterpreted extension"). *)
let is_vox_block_name txt =
  match vox_block_of_extension txt with
  | Block | Bad_backend _ -> true
  | Not_a_block -> false
;;

(* Validates and extracts the text of a [%%vox.lean] payload; used by Typemod (to accept
   the item) and by the collection below. *)
let vox_block_text (({ txt; loc }, payload) : Parsetree.extension) =
  match vox_block_of_extension txt with
  | Not_a_block -> None
  | Bad_backend b ->
    Location.raise_errorf ~loc "vox: unknown block extension %S (expected \"vox.lean\")" b
  | Block ->
    (match payload with
     | Parsetree.PStr
         [ { pstr_desc =
               Pstr_eval
                 ( { pexp_desc = Pexp_constant { pconst_desc = Pconst_string (s, _, _); _ }
                   ; _
                   }
                 , [] )
           ; _
           }
         ] -> Some s
     | _ ->
       Location.raise_errorf
         ~loc
         "vox: a solver block takes a single string literal, e.g. [%%%%vox.lean \
          {lean|...|lean}]")
;;

let normalize_block s =
  if String.length s > 0 && s.[String.length s - 1] = '\n' then s else s ^ "\n"
;;

(* [%%vox.lean] blocks are UNIT-LEVEL: the solver input is assembled once per compilation
   unit, and [collect_blocks] reads only the top-level items. A block nested in a module
   or functor body would be silently dropped -- its definitions then undefined at VC time,
   an error surfacing far from its cause -- so detect one and reject it with a clear
   message pointing at the fix. Traverses module/functor bodies (structures on the
   implementation side, module types on the interface side). *)
let nested_block_error attr_loc =
  Location.raise_errorf
    ~loc:attr_loc
    "vox: [%%%%vox.lean] blocks are unit-level; move to the file top level"
;;

let rec check_no_nested_blocks_module_expr (me : module_expr) =
  match me.mod_desc with
  | Tmod_structure str -> check_no_nested_blocks_structure str
  | Tmod_functor (_, body) -> check_no_nested_blocks_module_expr body
  | Tmod_constraint (me, _, _, _) -> check_no_nested_blocks_module_expr me
  | Tmod_apply (m1, m2, _) ->
    check_no_nested_blocks_module_expr m1;
    check_no_nested_blocks_module_expr m2
  | Tmod_apply_unit m1 -> check_no_nested_blocks_module_expr m1
  | Tmod_ident _ | Tmod_unpack _ -> ()

and check_no_nested_blocks_structure (str : structure) =
  List.iter
    (fun item ->
      match item.str_desc with
      | Tstr_attribute ({ attr_name = { txt; _ }; attr_loc; _ } : attribute)
        when is_vox_block_name txt -> nested_block_error attr_loc
      | Tstr_module mb -> check_no_nested_blocks_module_expr mb.mb_expr
      | Tstr_recmodule mbs ->
        List.iter (fun mb -> check_no_nested_blocks_module_expr mb.mb_expr) mbs
      | _ -> ())
    str.str_items

and check_no_nested_blocks_module_type (mty : module_type) =
  match mty.mty_desc with
  | Tmty_signature sg -> check_no_nested_blocks_signature sg
  | Tmty_functor (_, body, _) -> check_no_nested_blocks_module_type body
  | Tmty_with (mty, _) -> check_no_nested_blocks_module_type mty
  | Tmty_strengthen (mty, _, _) -> check_no_nested_blocks_module_type mty
  | Tmty_typeof me -> check_no_nested_blocks_module_expr me
  | Tmty_ident _ | Tmty_alias _ -> ()

and check_no_nested_blocks_signature (sg : signature) =
  List.iter
    (fun item ->
      match item.sig_desc with
      | Tsig_attribute ({ attr_name = { txt; _ }; attr_loc; _ } : attribute)
        when is_vox_block_name txt -> nested_block_error attr_loc
      | Tsig_module md -> check_no_nested_blocks_module_type md.md_type
      | Tsig_recmodule mds ->
        List.iter (fun md -> check_no_nested_blocks_module_type md.md_type) mds
      | _ -> ())
    sg.sig_items
;;

let collect_blocks (str : structure) =
  List.filter_map
    (fun item ->
      match item.str_desc with
      | Tstr_attribute ({ attr_name = { txt; _ }; attr_payload; attr_loc } : attribute)
        when is_vox_block_name txt ->
        (match vox_block_text ({ txt; loc = attr_loc }, attr_payload) with
         | Some s -> Some (normalize_block s, attr_loc)
         | None -> None)
      | Tstr_module mb ->
        check_no_nested_blocks_module_expr mb.mb_expr;
        None
      | Tstr_recmodule mbs ->
        List.iter (fun mb -> check_no_nested_blocks_module_expr mb.mb_expr) mbs;
        None
      | _ -> None)
    str.str_items
;;

(* Blocks of an INTERFACE ([%%vox.lean] in an .mli): collected by the .mli's compilation
   and saved into the .cmi (see Typemod), so they reach every client -- and the unit's own
   implementation, whose verification reads the interface's .cmi like any other import. *)
let collect_blocks_sig (sg : Typedtree.signature) =
  List.filter_map
    (fun item ->
      match item.sig_desc with
      | Tsig_attribute ({ attr_name = { txt; _ }; attr_payload; attr_loc } : attribute)
        when is_vox_block_name txt ->
        (match vox_block_text ({ txt; loc = attr_loc }, attr_payload) with
         | Some s -> Some (normalize_block s)
         | None -> None)
      | Tsig_module md ->
        check_no_nested_blocks_module_type md.md_type;
        None
      | Tsig_recmodule mds ->
        List.iter (fun md -> check_no_nested_blocks_module_type md.md_type) mds;
        None
      | _ -> None)
    sg.sig_items
;;

(* Imported spec exports in dependency order (a unit's spec after the units it imports;
   name order breaks ties, for determinism). *)
(* Search-path directories for Lean module artifacts: everywhere a .cmi can be found, plus
   the compiling unit's own directory (where its VoxCore/sig oleans are written). *)
let lean_path_dirs () =
  let own = Filename.dirname !Location.input_name in
  let dirs = Load_path.get_path_list () in
  let dirs = if List.exists (String.equal own) dirs then dirs else own :: dirs in
  (* the lean invocations change directory; entries must survive *)
  List.map
    (fun d -> if Filename.is_relative d then Filename.concat (Sys.getcwd ()) d else d)
    dirs
;;

let gather_imported_specs () =
  let all =
    Env.vox_imported_specs ()
    |> List.map (fun (name, export, deps) ->
      ( Compilation_unit.Name.to_string name
      , export
      , List.map Compilation_unit.Name.to_string deps ))
    |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  in
  let carriers = List.map (fun (n, _, _) -> n) all in
  let visited = ref [] in
  let out = ref [] in
  let rec visit n =
    if not (List.exists (String.equal n) !visited)
    then (
      visited := n :: !visited;
      match List.find_opt (fun (m, _, _) -> String.equal m n) all with
      | None -> ()
      | Some (_, export, deps) ->
        List.iter
          visit
          (List.filter (fun d -> List.exists (String.equal d) carriers) deps);
        out := (n, export) :: !out)
  in
  List.iter (fun (n, _, _) -> visit n) all;
  List.rev !out
;;

let imported_need_voxu () =
  List.exists (fun (_, vp) -> vp.Cmi_format.vp_needs_voxu) !imported_specs
;;

(* A datatype of THIS module whose stable name matches an imported declaration is not
   re-declared (see the emitters' [~skip]) -- which is only sound if it really is the same
   declaration. The renderers are deterministic, so comparing rendered text detects a
   local type shadowing an imported one at the same solver-side name. [render] is a
   parameter only because the Lean renderer it must be (the export stores the Lean
   rendering) is defined later in this file. *)
let check_imported_datatype_clashes ~render =
  let this_unit = Env.get_current_unit_name () in
  List.iter
    (fun ((p, _) as dt) ->
      let uname = path_uname p in
      List.iter
        (fun (unit, vp) ->
          List.iter
            (fun (n, leand) ->
              if String.equal n uname
                 && (not (String.equal (render dt : string) leand))
                 (* ... except OUR OWN interface's [@@vox.sort opaque] view of a type this
                    implementation knows concretely: the shared name is the point (the
                    seal's re-elaborated laws land on the concrete declaration), and the
                    emitters never splice both *)
                 && not
                      (String.equal unit this_unit
                       && String.equal (render (p, Dt_opaque) : string) leand)
              then
                Location.raise_errorf
                  ~loc:(Location.in_file !Location.input_name)
                  "vox: the type %s would share the solver-side name %s with a different \
                   datatype imported from unit %s; rename one of them"
                  uname
                  uname
                  unit)
            vp.Cmi_format.vp_datatypes)
        !imported_specs)
    !datatypes
;;

(* Where a line of generated solver input came from, for error attribution. *)
type block_src =
  | Local_block of Location.t
  | Imported_block of string (* unit name *)
  | Reflected_def of Vox_reflect.spec_def
  | Lemma of string * Location.t (* an [@@vox.lemma] theorem *)
  | Seal (* the trailing interface seal of a sig-bearing unit *)

let count_lines s = String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 s

(* The [-vox-prelude] file: user-written solver-side definitions (spec functions such as
   measures), inserted verbatim into every generated solver input just after the datatype
   declarations. Normalized to end in a newline; an unreadable file is a verification
   failure. *)
let prelude_cache : string option ref = ref None

let prelude () =
  match !prelude_cache with
  | Some c -> c
  | None ->
    let c =
      if String.equal !Clflags.vox_prelude ""
      then ""
      else (
        match
          let ic = open_in_bin !Clflags.vox_prelude in
          let n = in_channel_length ic in
          let c = really_input_string ic n in
          close_in ic;
          c
        with
        | c ->
          if String.length c > 0 && c.[String.length c - 1] = '\n' then c else c ^ "\n"
        | exception Sys_error msg ->
          Location.raise_errorf
            ~loc:(Location.in_file !Location.input_name)
            "vox: cannot read -vox-prelude file: %s"
            msg)
    in
    prelude_cache := Some c;
    c
;;

let vc_uses_spec_fun vc =
  List.exists Refinement.mentions_spec_fun (vc.vc_goal :: vc.vc_facts)
;;

(* ------------------------------------------------------------------ *)
(* Solver harness: [Sys.command] + temp files; no unix dependency. A wedged process is out
   of scope for v0. *)

let lean_command () =
  if not (String.equal !Clflags.vox_solver_path "")
  then !Clflags.vox_solver_path
  else (
    match Sys.getenv_opt "VOX_LEAN" with
    | Some s -> s
    | None -> "lean")
;;

(* ------------------------------------------------------------------ *)
(* Lean backend: VCs become Lean 4 theorems proved by [grind], batched one file per
   module, one theorem per line so failing line numbers map back to VCs. Int-sorted names
   are [Int], bool-sorted names are modelled as [Prop] (equality between boolean-valued
   predicates becomes [↔]), everything else lives in an opaque type [VoxU]. *)

let lean_name id = "v_" ^ lean_sanitize (Ident.unique_name id)
let lean_dt_name p = "Vox_" ^ lean_sanitize (path_uname p)
let lean_constr_name p c = lean_dt_name p ^ "." ^ lean_sanitize c

(* The Lean name of the [i]th type parameter of a datatype declaration: the binders
   [lean_datatype_decl] introduces (0-based, [a0 a1 ...]). *)
let lean_param_name i = "a" ^ Int.to_string i

let rec lean_sort = function
  | S_int -> "Int"
  | S_bool -> "Prop"
  | S_other -> "VoxU"
  | S_iarray -> "VoxIA"
  | S_lean (name, []) -> name
  | S_lean (name, args) ->
    "(" ^ name ^ " " ^ String.concat " " (List.map lean_sort args) ^ ")"
  | S_arrow (a, b) -> "(" ^ lean_sort a ^ " -> " ^ lean_sort b ^ ")"
  | S_param i -> lean_param_name i
  | S_tuple comps ->
    "("
    ^ tuple_uname (List.length comps)
    ^ " "
    ^ String.concat " " (List.map lean_sort comps)
    ^ ")"
  | S_poly (p, args) ->
    "(" ^ lean_dt_name p ^ " " ^ String.concat " " (List.map lean_sort args) ^ ")"
  | S_data (p, args) ->
    (match find_datatype p with
     | None -> "VoxU" (* unregistered: degrade, sound *)
     | Some _ ->
       (match args with
        | [] -> lean_dt_name p
        | _ ->
          "(" ^ lean_dt_name p ^ " " ^ String.concat " " (List.map lean_sort args) ^ ")"))
;;

(* The parameterized opaque for one [@@vox.poly] head, on a single line (the error-line
   mapping counts lines). *)
let lean_poly_decl (p, n) =
  let arrows = String.concat "" (List.init n (fun _ -> "Type -> ")) in
  Printf.sprintf "opaque %s : %sType\n" (lean_dt_name p) arrows
;;

(* The product structure for one tuple arity, universe-polymorphic over [Sort] so a Prop
   component (the Lean model of bool) instantiates as readily as a Type one -- the shape
   of core Lean's [PProd], with explicit universe binders so no auto-binding is relied on.
   One line, like the other declarations (the error-line mapping counts lines). *)
let lean_tuple_decl ?vis n = Vox_module.lean_tuple_decl ?vis n

(* One declaration, on a single line (the error-line mapping counts lines); self-recursion
   within a line is fine. Variants are inductives; records are structures, whose
   projections come built in. A PARAMETERIZED declaration binds its type parameters as
   explicit [Type] arguments ([a0 a1 ...], mirroring the tuple product's style), which the
   field sorts and (for a variant) the applied result type mention; a monomorphic
   declaration ([arity = 0]) renders exactly as before. *)
let lean_datatype_decl ?(vis = "") (p, decl) =
  let buf = Buffer.create 128 in
  Buffer.add_string buf vis;
  let arity =
    match decl with
    | Dt_variant (n, _) | Dt_record (n, _) -> n
    | Dt_opaque -> 0
  in
  let param_binders =
    String.concat
      ""
      (List.init arity (fun i -> Printf.sprintf " (%s : Type)" (lean_param_name i)))
  in
  (match decl with
   | Dt_variant (_, constrs) ->
     let applied =
       if arity = 0
       then lean_dt_name p
       else
         "("
         ^ lean_dt_name p
         ^ String.concat "" (List.init arity (fun i -> " " ^ lean_param_name i))
         ^ ")"
     in
     Buffer.add_string
       buf
       (Printf.sprintf "inductive %s%s : Type where" (lean_dt_name p) param_binders);
     List.iter
       (fun (cname, fields) ->
         Buffer.add_string buf (Printf.sprintf " | %s : " (lean_sanitize cname));
         List.iter (fun fs -> Buffer.add_string buf (lean_sort fs ^ " -> ")) fields;
         Buffer.add_string buf applied)
       constrs
   | Dt_record (_, fields) ->
     Buffer.add_string
       buf
       (Printf.sprintf "structure %s%s where" (lean_dt_name p) param_binders);
     List.iter
       (fun (l, fs) ->
         Buffer.add_string
           buf
           (Printf.sprintf " (%s : %s)" (lean_sanitize l) (lean_sort fs)))
       fields
   | Dt_opaque ->
     Buffer.add_string buf (Printf.sprintf "opaque %s : Type" (lean_dt_name p)));
  Buffer.add_char buf '\n';
  Buffer.contents buf
;;

(* All registered datatypes except [skip] (already declared by an
   imported export), in dependency order. *)
let lean_datatype_decls ?(filter = fun _ -> true) buf ~skip ~vis =
  List.iter
    (fun ((p, decl) as dt) ->
      if (not (List.exists (String.equal (path_uname p)) skip)) && filter decl
      then Buffer.add_string buf (lean_datatype_decl ~vis dt))
    !datatypes
;;

let boolish p =
  let open Refinement in
  match p with
  | Pbool _
  | Pbinop ((Eq | Neq | Lt | Le | Gt | Ge), _, _)
  | Pand _ | Por _ | Pnot _ | Pimp _ -> true
  | Pglobal p ->
    (match Hashtbl.find_opt globals (path_uname p) with
     | Some (_, S_bool) -> true
     | _ -> false)
  | Pvar id ->
    (match Hashtbl.find_opt name_sorts id with
     | Some S_bool -> true
     | _ -> false)
  | Pfield (p, l, _) ->
    (* a bool-sorted field is a [Prop] in the Lean model *)
    (match find_datatype p with
     | Some (_, Dt_record (_, fields)) ->
       (match List.assoc_opt l fields with
        | Some S_bool -> true
        | Some
            ( S_int
            | S_data _
            | S_param _
            | S_tuple _
            | S_iarray
            | S_poly _
            | S_lean _
            | S_arrow _
            | S_other )
        | None -> false)
     | Some (_, (Dt_variant _ | Dt_opaque)) | None -> false)
  | Pis _ | Pquant _ -> true
  (* A bool-sorted tuple COMPONENT is a Prop the model cannot see from the (untyped)
     projection alone: [=] between Props is emitted there, a sharp edge grind still
     handles via propext. *)
  | Pbound | Pint _ | Pconstr _ | Pfun _ | Ptuple _ | Pproj _ | Plam _
  | Pbinop ((Add | Sub | Mul | Div | Mod), _, _) -> false
;;

let rec lean_of_pred buf (p : Refinement.pred) =
  let open Refinement in
  let bin op a b =
    Buffer.add_char buf '(';
    lean_of_pred buf a;
    Buffer.add_string buf (" " ^ op ^ " ");
    lean_of_pred buf b;
    Buffer.add_char buf ')'
  in
  match p with
  | Pbound -> assert false
  | Pvar id -> Buffer.add_string buf (lean_name id)
  | Pglobal p -> Buffer.add_string buf ("g_" ^ lean_sanitize (path_uname p))
  | Pint n ->
    if n >= 0
    then Buffer.add_string buf (Printf.sprintf "(%d : Int)" n)
    else Buffer.add_string buf (Printf.sprintf "((%d : Int))" n)
  | Pbool b -> Buffer.add_string buf (if b then "True" else "False")
  | Pconstr (p, c, []) -> Buffer.add_string buf (lean_constr_name p c)
  | Pconstr (p, c, args) ->
    Buffer.add_string buf ("(" ^ lean_constr_name p c);
    List.iter
      (fun a ->
        Buffer.add_char buf ' ';
        lean_of_pred buf a)
      args;
    Buffer.add_char buf ')'
  | Pfun (f, []) -> Buffer.add_string buf f
  | Pfun (f, args) ->
    (* Spec function, emitted verbatim (no quoting is needed: every OCaml lowercase
       identifier, [']s included, is a valid Lean identifier); defined by a prelude (file,
       embedded block, or imported spec export) or a [total_] definition. *)
    Buffer.add_string buf ("(" ^ f);
    List.iter
      (fun a ->
        Buffer.add_char buf ' ';
        lean_of_pred buf a)
      args;
    Buffer.add_char buf ')'
  | Pfield (p, l, a) ->
    (* structure projection *)
    Buffer.add_string buf ("(" ^ lean_dt_name p ^ "." ^ lean_sanitize l ^ " ");
    lean_of_pred buf a;
    Buffer.add_char buf ')'
  | Ptuple args ->
    Buffer.add_string buf ("(" ^ tuple_uname (List.length args) ^ ".mk");
    List.iter
      (fun a ->
        Buffer.add_char buf ' ';
        lean_of_pred buf a)
      args;
    Buffer.add_char buf ')'
  | Pproj (n, i, a) ->
    Buffer.add_string buf (Printf.sprintf "(%s.p%d " (tuple_uname n) (i + 1));
    lean_of_pred buf a;
    Buffer.add_char buf ')'
  | Pquant (q, id, a) ->
    (* The binder is unannotated -- predicates are untyped, and Lean infers its sort from
       the body, exactly as for the existential encoding of [Pis] below; an uninferable
       binder is a solver error, i.e. a verification failure. *)
    Buffer.add_string
      buf
      ((match q with
        | Qforall -> "(∀ "
        | Qexists -> "(∃ ")
       ^ lean_name id
       ^ ", ");
    lean_of_pred buf a;
    Buffer.add_char buf ')'
  | Plam (ids, a) ->
    (* An anonymous Lean function [fun x y => body]; binders unannotated (Lean infers from
       the applied context, e.g. the relation parameter's ghost arrow sort). grind
       beta-reduces at an application, so a substituted lambda unfolds against the
       fixpoint that consumes it. *)
    Buffer.add_string buf "(fun";
    List.iter (fun id -> Buffer.add_string buf (" " ^ lean_name id)) ids;
    Buffer.add_string buf " => ";
    lean_of_pred buf a;
    Buffer.add_char buf ')'
  | Pis (p, c, a) ->
    (* existential tester; the exhaustiveness hypothesis emitted per tester subject
       (lean_theorem) lets grind case on it *)
    let n = constr_arity p c in
    Buffer.add_char buf '(';
    if n > 0
    then (
      Buffer.add_string buf "∃";
      for i = 0 to n - 1 do
        Buffer.add_string buf (Printf.sprintf " e%d" i)
      done;
      Buffer.add_string buf ", ");
    lean_of_pred buf a;
    Buffer.add_string buf (" = " ^ if n > 0 then "(" else "");
    Buffer.add_string buf (lean_constr_name p c);
    for i = 0 to n - 1 do
      Buffer.add_string buf (Printf.sprintf " e%d" i)
    done;
    if n > 0 then Buffer.add_char buf ')';
    Buffer.add_char buf ')'
  | Pbinop (Eq, a, b) -> bin (if boolish a || boolish b then "↔" else "=") a b
  | Pbinop (Neq, a, b) ->
    Buffer.add_string buf "(¬ ";
    bin (if boolish a || boolish b then "↔" else "=") a b;
    Buffer.add_char buf ')'
  | Pbinop (Add, a, b) -> bin "+" a b
  | Pbinop (Sub, a, b) -> bin "-" a b
  | Pbinop (Mul, a, b) -> bin "*" a b
  | Pbinop (((Div | Mod) as op), a, b) ->
    (* OCaml's [/] and [mod] truncate toward zero: exactly [Int.tdiv] and [Int.tmod]. *)
    Buffer.add_string buf (if op = Div then "(Int.tdiv " else "(Int.tmod ");
    lean_of_pred buf a;
    Buffer.add_char buf ' ';
    lean_of_pred buf b;
    Buffer.add_char buf ')'
  | Pbinop (Lt, a, b) -> bin "<" a b
  | Pbinop (Le, a, b) -> bin "≤" a b
  | Pbinop (Gt, a, b) -> bin ">" a b
  | Pbinop (Ge, a, b) -> bin "≥" a b
  | Pand (a, b) -> bin "∧" a b
  | Por (a, b) -> bin "∨" a b
  | Pnot a ->
    Buffer.add_string buf "(¬ ";
    lean_of_pred buf a;
    Buffer.add_char buf ')'
  | Pimp (a, b) -> bin "→" a b
;;

(* Reflected definitions, emitted between the datatypes and the prelude. [@[grind] def]
   registers the defining equations with grind. Termination is Lean's to check: structural
   recursion needs nothing, and a [@@vox.decreases e] metric becomes
   [termination_by (e).toNat] with an omega [decreasing_by], falling back to [grind] for
   the goals omega leaves opaque -- a recursion on [n / 2] decreases through [Int.tdiv],
   which omega treats as an atom (the branch guards are in context for those goals either
   way). The def name is the source name, so a [-vox-prelude] can state lemmas about it. *)
let lean_rsort (s : Vox_reflect.rsort) =
  match s with
  | Vox_reflect.Rint -> "Int"
  | Vox_reflect.Rbool -> "Prop"
  | Vox_reflect.Rdata p -> lean_sort (S_data (p, []))
;;

let rec lean_def_body buf (b : Vox_reflect.def_body) =
  match b with
  | Vox_reflect.Bpred p -> lean_of_pred buf p
  | Vox_reflect.Bite (c, a, b') ->
    Buffer.add_string buf "(if ";
    lean_of_pred buf c;
    Buffer.add_string buf " then ";
    lean_def_body buf a;
    Buffer.add_string buf " else ";
    lean_def_body buf b';
    Buffer.add_char buf ')'
  | Vox_reflect.Bcase (x, clauses) ->
    Buffer.add_string buf ("(match " ^ lean_name x ^ " with");
    List.iter
      (fun (cl : Vox_reflect.def_clause) ->
        Buffer.add_string buf (" | " ^ lean_constr_name cl.dc_path cl.dc_cstr);
        List.iter (fun f -> Buffer.add_string buf (" " ^ lean_name f)) cl.dc_fields;
        Buffer.add_string buf " => ";
        lean_def_body buf cl.dc_rhs)
      clauses;
    Buffer.add_char buf ')'
;;

let lean_spec_def buf (d : Vox_reflect.spec_def) =
  Buffer.add_string buf ("@[grind] def " ^ d.sd_name);
  List.iter
    (fun (id, s) ->
      Buffer.add_string buf (Printf.sprintf " (%s : %s)" (lean_name id) (lean_rsort s)))
    d.sd_params;
  Buffer.add_string buf (" : " ^ lean_rsort d.sd_ret ^ " := ");
  lean_def_body buf d.sd_body;
  Buffer.add_char buf '\n';
  match d.sd_decreases with
  | None -> ()
  | Some m ->
    Buffer.add_string buf "termination_by (";
    lean_of_pred buf m;
    Buffer.add_string buf ").toNat\ndecreasing_by all_goals (first | omega | grind)\n"
;;

(* [@@vox.lemma]: an ordinary recursive function whose refined result is a PROPOSITION
   over its parameters is a proof by induction (the recursive call is the induction
   hypothesis; see NOTES.md). We EXPORT that proposition as an ambient grind fact:
   [forall params, contracts -> Q] is emitted as a Lean [theorem], RE-PROVED by structural
   / functional induction + grind, with a [grind_pattern] so it fires at the spec-function
   applications it is about. Soundness is Lean's: a false or non-terminating "lemma"
   ([unit{ false }] self-calls, a partial match, an untrue proposition) has no proof under
   any alternative, so the [first] block fails and verification fails closed -- never
   registering a false universal. Structural [induction] is well-founded by construction
   and [fun_induction] borrows the reflected function's Lean-checked termination, so the
   lemma needs no trust and no metric of its own. *)
let pred_mentions_bound (p : Refinement.pred) =
  let open Refinement in
  let rec go = function
    | Pbound -> true
    | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> false
    | Pconstr (_, _, args) | Pfun (_, args) | Ptuple args -> List.exists go args
    | Pfield (_, _, a)
    | Pproj (_, _, a)
    | Pis (_, _, a)
    | Pnot a
    | Pquant (_, _, a)
    | Plam (_, a) -> go a
    | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) -> go a || go b
  in
  go p
;;

(* Outermost spec-function applications of [p] (do not descend into a [Pfun]'s own
   arguments): the trigger a lemma fact fires on. *)
let outermost_funs (p : Refinement.pred) =
  let open Refinement in
  let acc = ref [] in
  let rec go = function
    | Pfun (f, args) -> acc := (f, args) :: !acc
    | Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> ()
    | Pconstr (_, _, args) | Ptuple args -> List.iter go args
    | Pfield (_, _, a)
    | Pproj (_, _, a)
    | Pis (_, _, a)
    | Pnot a
    | Pquant (_, _, a)
    | Plam (_, a) -> go a
    | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
      go a;
      go b
  in
  go p;
  List.rev !acc
;;

(* Spec-function applications [f p] of a bare PARAMETER [p]: the [fun_induction]
   candidates (int-indexed lemmas borrow the reflected function's induction principle). *)
let funinduction_cands param_ids (p : Refinement.pred) =
  let open Refinement in
  let acc = ref [] in
  let rec go = function
    | Pfun (f, [ Pvar x ]) when List.exists (Ident.same x) param_ids ->
      acc := (f, x) :: !acc
    | Pfun (_, args) | Pconstr (_, _, args) | Ptuple args -> List.iter go args
    | Pfield (_, _, a)
    | Pproj (_, _, a)
    | Pis (_, _, a)
    | Pnot a
    | Pquant (_, _, a)
    | Plam (_, a) -> go a
    | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
      go a;
      go b
    | Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> ()
  in
  go p;
  List.fold_left
    (fun seen (f, x) ->
      if List.exists (fun (g, y) -> String.equal f g && Ident.same x y) seen
      then seen
      else (f, x) :: seen)
    []
    !acc
  |> List.rev
;;

(* v2 lemma export: translate the OCaml lemma body into a genuine Lean recursive proof
   term that mirrors it, so a lemma whose body already verified never fails to export
   (given Lean accepts termination). Match arms become Lean matches; each recursive /
   other-lemma call becomes a [have _ := lemma <actual args> <precond proofs>] at the
   EXACT instantiation the body used (accumulators and non-first-argument recursion
   included -- the difference from a blind [induction]); each arm's residual is [grind],
   with the same hypotheses the body VC had. Shapes the translator does not cover raise
   [Lemma_v2_unsupported], routing that lemma to the v1 tactic re-proof (still
   fail-closed). *)
exception Lemma_v2_unsupported

let rec subst_vars sub (p : Refinement.pred) =
  let open Refinement in
  let go = subst_vars sub in
  match p with
  | Pvar id ->
    (match List.assoc_opt id sub with
     | Some q -> q
     | None -> p)
  | Pbound | Pglobal _ | Pint _ | Pbool _ -> p
  | Pconstr (path, c, args) -> Pconstr (path, c, List.map go args)
  | Pfun (f, args) -> Pfun (f, List.map go args)
  | Pfield (path, l, a) -> Pfield (path, l, go a)
  | Ptuple args -> Ptuple (List.map go args)
  | Pproj (n, i, a) -> Pproj (n, i, go a)
  | Pis (path, c, a) -> Pis (path, c, go a)
  | Pbinop (op, a, b) -> Pbinop (op, go a, go b)
  | Pand (a, b) -> Pand (go a, go b)
  | Por (a, b) -> Por (go a, go b)
  | Pnot a -> Pnot (go a)
  | Pimp (a, b) -> Pimp (go a, go b)
  | Pquant (q, id, a) -> Pquant (q, id, go a)
  | Plam (bs, a) -> Plam (bs, go a)
;;

let pred_to_lean (p : Refinement.pred) =
  let b = Buffer.create 32 in
  lean_of_pred b p;
  Buffer.contents b
;;

(* [Some (fid, nhyps, arg_exprs)] if [e] is a saturated application of the lemma being
   defined ([self_id]) or of a previously-registered lemma. *)
let lemma_call ~self_id ~self_nhyps (e : expression) =
  match e.exp_desc with
  | Texp_apply ({ exp_desc = Texp_ident { path = Path.Pident fid; _ }; _ }, args, _, _, _)
    ->
    let nh =
      if Ident.same fid self_id then Some self_nhyps else List.assoc_opt fid !lemma_sigs
    in
    (match nh with
     | None -> None
     | Some nh ->
       let argexprs =
         List.filter_map
           (fun (lbl, a) ->
             match (lbl : Types.arg_label), a with
             | Nolabel, Arg (ae, _) -> Some ae
             | _ -> None)
           args
       in
       if List.length argexprs = List.length args then Some (fid, nh, argexprs) else None)
  | _ -> None
;;

(* Translate a lemma body into a Lean proof term. *)
let translate_lemma_body ~self_id ~self_nhyps ~has_decreases (body : expression) =
  let arg_lean sub ae =
    match Vox_reflect.translate_rhs ae with
    | Some pr -> pred_to_lean (subst_vars sub pr)
    | None -> raise Lemma_v2_unsupported
  in
  let call_have sub buf (fid, nh, argexprs) =
    Buffer.add_string buf ("have _ih := " ^ Ident.name fid);
    List.iter (fun ae -> Buffer.add_string buf (" (" ^ arg_lean sub ae ^ ")")) argexprs;
    for _ = 1 to nh do
      Buffer.add_string buf " (by grind)"
    done;
    Buffer.add_string buf "; "
  in
  (* Straight-line arm body: thread value-lets as a substitution, emit a [have] per
     recursive / lemma call, then close with [grind]. *)
  let rec leaf sub buf (e : expression) =
    match e.exp_desc with
    | Texp_construct (_, cstr, _, _, _) when String.equal cstr.cstr_name "()" -> ()
    | _ when lemma_call ~self_id ~self_nhyps e <> None ->
      (match lemma_call ~self_id ~self_nhyps e with
       | Some c -> call_have sub buf c
       | None -> ())
    | Texp_let (Nonrecursive, [ vb ], body2) ->
      (match lemma_call ~self_id ~self_nhyps vb.vb_expr with
       | Some c ->
         call_have sub buf c;
         leaf sub buf body2
       | None ->
         (match vb.vb_pat.pat_desc, Vox_reflect.translate_rhs vb.vb_expr with
          | Tpat_var { id; _ }, Some pr -> leaf ((id, subst_vars sub pr) :: sub) buf body2
          | _, Some _ -> leaf sub buf body2
          | _, None -> raise Lemma_v2_unsupported))
    | _ -> raise Lemma_v2_unsupported
  in
  let rec ctrl (e : expression) =
    match e.exp_desc with
    | Texp_match (scrut, _, cases, val_cases, _) when val_cases = [] ->
      let scrut_lean =
        match scrut.exp_desc with
        | Texp_ident { path = Path.Pident id; _ } -> lean_name id
        | _ -> raise Lemma_v2_unsupported
      in
      let b = Buffer.create 128 in
      Buffer.add_string b ("(match " ^ scrut_lean ^ " with");
      List.iter (fun c -> arm b c) cases;
      Buffer.add_char b ')';
      Buffer.contents b
    | Texp_ifthenelse (c, a, Some belse) ->
      (* [if]-controlled recursion is int-indexed: without a [@@vox.decreases] metric Lean
         cannot show termination structurally, so route to the v1 fallback
         (fun_induction). *)
      if not has_decreases then raise Lemma_v2_unsupported;
      let cl =
        match Vox_reflect.translate c with
        | Some pr -> pred_to_lean pr
        | None -> raise Lemma_v2_unsupported
      in
      "(if _h : " ^ cl ^ " then " ^ ctrl a ^ " else " ^ ctrl belse ^ ")"
    | _ ->
      let b = Buffer.create 64 in
      Buffer.add_string b "(by ";
      leaf [] b e;
      Buffer.add_string b "grind)";
      Buffer.contents b
  and arm b (c : computation case) =
    (match c.c_guard with
     | Some _ -> raise Lemma_v2_unsupported
     | None -> ());
    let rec cpat : type k. k general_pattern -> _ =
      fun pat ->
      match pat.pat_desc with
      | Tpat_value p -> cpat (p :> value general_pattern)
      | Tpat_construct (_, cstr, _, args, _) ->
        let path = Data_types.cstr_res_type_path cstr in
        let field (_, (p : value general_pattern)) =
          match p.pat_desc with
          | Tpat_var { id; _ } -> lean_name id
          | Tpat_any -> "_"
          | _ -> raise Lemma_v2_unsupported
        in
        path, cstr.cstr_name, List.map field args
      | _ -> raise Lemma_v2_unsupported
    in
    let path, cname, fields = cpat c.c_lhs in
    Buffer.add_string b (" | " ^ lean_constr_name path cname);
    List.iter (fun f -> Buffer.add_string b (" " ^ f)) fields;
    Buffer.add_string b " => ";
    Buffer.add_string b (ctrl c.c_rhs)
  in
  ctrl body
;;

let register_lemma env (vb : Typedtree.value_binding) =
  let loc = vb.vb_loc in
  let name =
    match vb.vb_pat.pat_desc with
    | Tpat_var { id; _ } -> Ident.name id
    | _ ->
      Location.raise_errorf
        ~loc
        "vox: [@@vox.lemma] requires a binding of a single variable"
  in
  List.iter
    (fun (d : Vox_reflect.spec_def) ->
      if String.equal d.sd_name name
      then
        Location.raise_errorf
          ~loc
          "vox: the [@@vox.lemma] %s shares its solver-side name with a reflected \
           function; rename one of them"
          name)
    !spec_defs;
  List.iter
    (fun (n, _, _) ->
      if String.equal n name
      then
        Location.raise_errorf
          ~loc
          "vox: two [@@vox.lemma]s would share the solver-side name %s; rename one of \
           them"
          name)
    !lemma_defs;
  let self_id =
    match vb.vb_pat.pat_desc with
    | Tpat_var { id; _ } -> id
    | _ -> Ident.create_local name
  in
  let rec skel ty =
    match get_desc (Ctype.vox_expand_head env ty) with
    | Tpoly (t, []) -> skel t
    | Trefine (sk, _, _) -> skel sk
    | _ -> ty
  in
  (* Prefer the FUNCTION's own parameter patterns as the canonical names: the v2 body
     translation refers to them, so the theorem binders must share their stamps. Fall back
     to the arrow's binders (and force the v1 tactic proof) for irregular shapes. *)
  let param_of_arrow id ty =
    match get_desc (Ctype.vox_expand_head env ty) with
    | Tarrow ((_, _, _, binder), dom, ret, _) ->
      let sort =
        Vox_reflect.rsort_of_type env ~loc ~what:"each [@@vox.lemma] parameter" (skel dom)
      in
      let contract =
        match param_refinement env dom with
        | Some pr -> Some (Refinement.subst_bound ~by:(Refinement.Pvar id) pr)
        | None -> None
      in
      let ret =
        match binder with
        | Some b -> Vox_dep.subst_binder b ~by:(Refinement.Pvar id) ret
        | None -> ret
      in
      Some ((id, sort, contract), ret)
    | _ -> None
  in
  let simple_param (fp : function_param) =
    match fp.fp_arg_label, fp.fp_kind with
    | Nolabel, Tparam_pat { pat_desc = Tpat_var { id; _ }; _ } -> Some id
    | _ -> None
  in
  let params, q, v2_body =
    match vb.vb_expr.exp_desc with
    | Texp_function { params = fps; body = Tfunction_body bexp; _ }
      when List.for_all (fun fp -> simple_param fp <> None) fps && fps <> [] ->
      let arrow = ref vb.vb_expr.exp_type in
      let ps =
        List.map
          (fun fp ->
            let id = Option.get (simple_param fp) in
            match param_of_arrow id !arrow with
            | Some (p, ret) ->
              arrow := ret;
              p
            | None -> raise Lemma_v2_unsupported)
          fps
      in
      let q =
        match refinement_of_type env !arrow with
        | Some pr -> pr
        | None ->
          Location.raise_errorf
            ~loc
            "vox: a [@@vox.lemma] must state a proposition as its refined result (e.g. \
             [unit{ ... }])"
      in
      ps, q, Some bexp
    | _ ->
      (* Type-only walk (arrow binders); body not translatable to v2. *)
      let rec walk acc ty =
        match get_desc (Ctype.vox_expand_head env ty) with
        | Tarrow (_, _, _, _) ->
          let id = Ident.create_local "_arg" in
          (match param_of_arrow id ty with
           | Some (p, ret) -> walk (p :: acc) ret
           | None -> List.rev acc, ty)
        | _ -> List.rev acc, ty
      in
      let params, rest = walk [] vb.vb_expr.exp_type in
      let q =
        match refinement_of_type env rest with
        | Some pr -> pr
        | None ->
          Location.raise_errorf
            ~loc
            "vox: a [@@vox.lemma] must state a proposition as its refined result (e.g. \
             [unit{ ... }])"
      in
      params, q, None
  in
  if params = []
  then
    Location.raise_errorf
      ~loc
      "vox: a [@@vox.lemma] must take at least one parameter to quantify over";
  if pred_mentions_bound q
  then
    Location.raise_errorf
      ~loc
      "vox: a [@@vox.lemma] result must be a proposition over the parameters ([unit{ ... \
       }]); it must not constrain the return value";
  List.iter
    (fun (_, s, c) ->
      (match s with
       | Vox_reflect.Rdata dp -> ignore (datatype_sort env dp [])
       | _ -> ());
      Option.iter (register_pred_paths env) c)
    params;
  register_pred_paths env q;
  let param_ids = List.map (fun (id, _, _) -> id) params in
  let self_nhyps = List.length (List.filter (fun (_, _, c) -> c <> None) params) in
  (* Common statement header: [theorem name (params) (h_i : C_i) : Q]. *)
  let header = Buffer.create 256 in
  Buffer.add_string header ("theorem " ^ name);
  List.iter
    (fun (id, s, _) ->
      Buffer.add_string header (Printf.sprintf " (%s : %s)" (lean_name id) (lean_rsort s)))
    params;
  List.iteri
    (fun i (_, _, c) ->
      match c with
      | None -> ()
      | Some pr ->
        Buffer.add_string header (Printf.sprintf " (h%d : " i);
        lean_of_pred header pr;
        Buffer.add_char header ')')
    params;
  Buffer.add_string header " : ";
  lean_of_pred header q;
  (* The [@@vox.decreases] metric, for int-indexed (non-structural) lemma recursion --
     emitted as [termination_by], exactly as for a reflected definition. *)
  let termination () =
    match Vox_reflect.find_attr "vox.decreases" vb.vb_attributes with
    | Some { attr_payload = PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]; _ } ->
      let menv =
        match v2_body with
        | Some b -> b.exp_env
        | None -> vb.vb_expr.exp_env
      in
      let m =
        Vox_reflect.translate_metric menv (List.map (fun (id, s, _) -> id, s) params) e
      in
      "\ntermination_by ("
      ^ pred_to_lean m
      ^ ").toNat\ndecreasing_by all_goals (first | omega | grind)\n"
    | _ -> "\n"
  in
  (* v1 tactic re-proof, the fallback. *)
  let v1_proof () =
    let b = Buffer.create 128 in
    Buffer.add_string b " := by\n  first\n  | grind\n";
    List.iter
      (fun (id, s, _) ->
        match s with
        | Vox_reflect.Rdata _ ->
          Buffer.add_string
            b
            (Printf.sprintf "  | (induction %s <;> grind)\n" (lean_name id))
        | _ -> ())
      params;
    List.iter
      (fun (f, x) ->
        Buffer.add_string
          b
          (Printf.sprintf "  | (fun_induction %s %s <;> grind)\n" f (lean_name x)))
      (funinduction_cands param_ids q);
    Buffer.contents b
  in
  let has_decreases = Vox_reflect.find_attr "vox.decreases" vb.vb_attributes <> None in
  (* v2: try the structural proof-term translation; on any unsupported shape, route to the
     v1 tactic re-proof. *)
  let proof, path =
    match v2_body with
    | Some bexp ->
      (try
         let term = translate_lemma_body ~self_id ~self_nhyps ~has_decreases bexp in
         " :=\n" ^ term ^ termination (), "structural"
       with
       | Lemma_v2_unsupported -> v1_proof (), "fallback")
    | None -> v1_proof (), "fallback"
  in
  if !Clflags.vox_dump_vc
  then Format.eprintf "vox: [@vox.lemma] %s exported via %s translation@." name path;
  let buf = Buffer.create 320 in
  Buffer.add_string buf (Buffer.contents header);
  Buffer.add_string buf proof;
  Buffer.add_char buf '\n';
  (match outermost_funs q with
   | (f, args) :: _ ->
     Buffer.add_string buf ("grind_pattern " ^ name ^ " => ");
     lean_of_pred buf (Refinement.Pfun (f, args));
     Buffer.add_char buf '\n'
   | [] -> ());
  lemma_sigs := !lemma_sigs @ [ self_id, self_nhyps ];
  lemma_defs := !lemma_defs @ [ name, Buffer.contents buf, loc ]
;;

(* The .cmi spec export of a unit: its reflected definitions (pre-rendered, as lean-only
   blocks ahead of the user's blocks, which may state lemmas about them), its blocks, plus
   pre-rendered declarations of the datatypes its exported refinements and definitions
   mention. Computed from a FRESH registration pass over the exported signature (batch
   compilation may leave another unit's datatype state in the globals), restored
   afterwards. No blocks and no definitions, no export: without spec functions clients
   register datatypes on demand as before. *)
(* A datatype named ONLY inside a [%%vox.lean] block's raw text (never in an OCaml
   refinement or reflected-function signature) is not registered on-sight, so
   [lean_datatype_decls] never declares it and the block's reference to its name fails to
   elaborate. At the toplevel the type declaration is even its own vox-free phrase, which
   is skipped entirely, so the block's phrase cannot see it as a structure item. So scan
   the blocks' text for [Vox_<name>] references and register each type the surrounding env
   resolves -- exactly the datatypes a block mentions. Inside a unit the emitted name is
   unit-qualified ([Vox_Htbl_bucket] for [Htbl.bucket]), so resolution also tries the
   token with the unit prefix stripped; a candidate only registers when its OWN emitted
   name reproduces the token exactly, so a bare [Vox_t] in a unit (which the emitter would
   never satisfy) registers nothing. Imported/qualified names do not resolve here and are
   declared by their own units' exports; registration is idempotent and a no-op for a
   non-simple type. *)
let register_datatypes_in_blocks env blocks =
  let is_ident_char c =
    (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || c = '_'
    || c = '\''
  in
  let unit_prefix =
    match Env.get_current_unit_name () with
    | "" -> None
    | u -> Some (lean_sanitize u ^ "_")
  in
  let candidates token =
    match unit_prefix with
    | Some pre
      when String.length token > String.length pre
           && String.equal (String.sub token 0 (String.length pre)) pre ->
      [ String.sub token (String.length pre) (String.length token - String.length pre)
      ; token
      ]
    | _ -> [ token ]
  in
  let register token =
    List.iter
      (fun name ->
        if String.length name > 0
        then (
          match Env.find_type_by_name (Longident.Lident name) env with
          | exception Not_found -> ()
          | p, _ ->
            if String.equal (lean_dt_name p) ("Vox_" ^ token)
            then ignore (datatype_sort env p [] : dsort)))
      (candidates token)
  in
  List.iter
    (fun (text, _loc) ->
      let n = String.length text in
      let i = ref 0 in
      while !i <= n - 4 do
        if String.equal (String.sub text !i 4) "Vox_"
           && (!i = 0 || not (is_ident_char text.[!i - 1]))
        then (
          let j = ref (!i + 4) in
          while !j < n && is_ident_char text.[!j] do
            incr j
          done;
          register (String.sub text (!i + 4) (!j - !i - 4));
          i := !j)
        else incr i
      done)
    blocks
;;

(* [S_lean]-field datatypes of the interface currently being sealed, as
   (uname, Lean name) pairs.  They -- and the blocks that reference them
   -- are ordered AROUND the ghost-sort-defining blocks in
   [build_sig_module].  Set by [cmi_export] while the freshly
   re-registered [!datatypes] is live (it is restored to the caller's
   value before [build_sig_module] runs, so [build_sig_module] cannot
   recompute it). *)
let sig_hold_back : (string * string) list ref = ref []

let str_contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  nl = 0
  || (let rec at i =
        i + nl <= hl
        && (String.equal (String.sub hay i nl) needle || at (i + 1))
      in
      at 0)
;;

(* Replace every occurrence of [needle] in [hay] with [repl]. *)
let str_replace_all hay needle repl =
  if String.equal needle "" then hay
  else begin
    let buf = Buffer.create (String.length hay) in
    let nl = String.length needle and hl = String.length hay in
    let i = ref 0 in
    while !i < hl do
      if !i + nl <= hl && String.equal (String.sub hay !i nl) needle
      then (Buffer.add_string buf repl; i := !i + nl)
      else (Buffer.add_char buf hay.[!i]; incr i)
    done;
    Buffer.contents buf
  end
;;

(* The named/via ([S_lean]) sort names a field of [decl] renders. *)
let dt_lean_sort_names decl =
  let rec go s acc =
    match s with
    | S_lean (n, args) -> List.fold_left (fun a x -> go x a) (n :: acc) args
    | S_tuple cs | S_poly (_, cs) -> List.fold_left (fun a x -> go x a) acc cs
    | S_data (_, args) -> List.fold_left (fun a x -> go x a) acc args
    | S_arrow (a, b) -> go b (go a acc)
    | S_int | S_bool | S_other | S_iarray | S_param _ -> acc
  in
  match decl with
  | Dt_variant (_, cs) ->
    List.concat_map (fun (_, fs) -> List.concat_map (fun s -> go s []) fs) cs
  | Dt_record (_, fs) -> List.concat_map (fun (_, s) -> go s []) fs
  | Dt_opaque -> []
;;

(* A local (impl-side, [public]-less) block that DEFINES a via/ghost sort
   referenced by a held-back PUBLIC datatype must expose that sort's
   declaration as [public]: the held-back datatype (emitted [public] so
   the seal can name it) forward-references the sort, and in a Lean module
   a public declaration may not mention a private one.  Publicise only the
   named sort's own declaration, nothing else in the block. *)
let publicize_ghost_decls names text =
  List.fold_left
    (fun text name ->
      List.fold_left
        (fun text kw ->
          let d = kw ^ " " ^ name in
          if str_contains text ("public " ^ d) then text
          else str_replace_all text d ("public " ^ d))
        text
        [ "inductive"; "opaque"; "structure" ])
    text
    names
;;

(* A block whose text names one of the held-back datatypes (a model def
   over a view ADT): it must follow that datatype's declaration. *)
let block_mentions_holdback names text =
  List.exists (fun (_, ln) -> str_contains text ln) names
;;

let cmi_export env (sg : Types.signature) ~defs ~blocks ~sig_module =
  let def_blocks =
    List.map
      (fun (d : Vox_reflect.spec_def) ->
        let b = Buffer.create 128 in
        lean_spec_def b d;
        Buffer.contents b)
      defs
  in
  let blocks = def_blocks @ blocks in
  if blocks = []
  then None
  else (
    let saved = !datatypes, !registering, !poisoned, !tuple_arities, !poly_heads in
    datatypes := [];
    registering := [];
    poisoned := [];
    tuple_arities := [];
    poly_heads := [];
    Misc.try_finally
      ~always:(fun () ->
        let d, r, po, ta, ph = saved in
        datatypes := d;
        registering := r;
        poisoned := po;
        tuple_arities := ta;
        poly_heads := ph)
      (fun () ->
        iter_signature_types sg ~f:(fun ~loc:_ ~what:_ ty -> register_type_specs env ty);
        (* datatypes the blocks name on-sight, or the export (and the sig module built
           from it) misses their declarations *)
        register_datatypes_in_blocks env (List.map (fun b -> b, Location.none) blocks);
        List.iter
          (fun d ->
            List.iter
              (fun p -> ignore (datatype_sort env p []))
              (Vox_reflect.def_datatype_paths d);
            List.iter
              register_pred_tuple_arities
              (Vox_reflect.body_preds
                 (Option.to_list d.Vox_reflect.sd_decreases)
                 d.Vox_reflect.sd_body))
          defs;
        let dts =
          List.map (fun ((p, _) as dt) -> path_uname p, lean_datatype_decl dt) !datatypes
        in
        (* Tuple product and parameterized-opaque declarations FIRST: the datatype
           declarations may reference either in field sorts, and this unit's blocks may
           name a [@@vox.poly] head directly (its ghosts' signatures) -- a client whose
           own types never mention the head still needs it declared. *)
        let dts =
          List.map (fun ((p, _) as hd) -> path_uname p, lean_poly_decl hd) !poly_heads
          @ List.map (fun n -> tuple_uname n, lean_tuple_decl n) !tuple_arities
          @ dts
        in
        sig_hold_back :=
          List.filter_map
            (fun (p, decl) ->
              if dt_uses_lean_field decl
              then Some (path_uname p, lean_dt_name p)
              else None)
            !datatypes;
        Some
          { Cmi_format.vp_datatypes = dts
          ; vp_needs_voxu = datatype_field_needs_voxu ()
          ; vp_blocks = blocks
          ; vp_sig_module = sig_module
          }))
;;

(* Save-site entry points (see Typemod / Compile_common). Reflected definitions are
   exported only from the cmi a unit writes itself: for a unit with an .mli the cmi comes
   from the interface, which has no bodies to reflect -- there, total_ functions stay
   private to the implementation (clients' calls degrade to unknowns; sound). *)
(* Building a unit's sig module (VoxSig_<Unit>.olean, next to its .cmi): the interface's
   datatype declarations (public) and its block text (verbatim; the author's own
   [public]/[@[expose]] markers are the interface), over VoxCore and the sig modules of
   its imports. Clients then [public import] the artifact: client verification depends on
   the INTERFACE alone, and an interface [axiom] becomes an OBLIGATION discharged by the
   implementation's seal, never trust. *)
let build_sig_module vp =
  let unit = Env.get_current_unit_name () in
  let dirs = lean_path_dirs () in
  let dir = Filename.dirname !Location.input_name in
  let err e =
    Location.raise_errorf
      ~loc:(Location.in_file !Location.input_name)
      "vox: could not build this interface's sig module:@ %s"
      e
  in
  (match
     Vox_module.ensure_core ~lean_command:(lean_command ()) ~lean_path_dirs:dirs ~dir
   with
   | Ok _ -> ()
   | Error e -> err e);
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "module\n";
  Buffer.add_string buf (Printf.sprintf "public import %s\n" Vox_module.core_module_name);
  let imported = gather_imported_specs () in
  List.iter
    (fun (u, (ivp : Cmi_format.vox_spec_export)) ->
      if ivp.Cmi_format.vp_sig_module && not (String.equal u unit)
      then
        Buffer.add_string
          buf
          (Printf.sprintf "public import %s\n" (Vox_module.sig_module_name u)))
    imported;
  (* Declarations already provided by an import are not re-spliced: the tuple products
     ride VoxCore, an import's datatypes ride its sig module. *)
  let covered n =
    Vox_module.core_tuple_uname n
    || List.exists
         (fun (u, (ivp : Cmi_format.vox_spec_export)) ->
           ivp.Cmi_format.vp_sig_module
           && (not (String.equal u unit))
           && List.exists (fun (n', _) -> String.equal n n') ivp.vp_datatypes)
         imported
  in
  (* Order around the ghost-sort-defining blocks (see [sig_hold_back]):
     (1) datatypes with no via/ghost field, (2) the blocks that do NOT
     reference a held-back datatype -- these DEFINE the ghost sorts,
     (3) the held-back datatypes (their fields now resolve), (4) the
     blocks that DO reference a held-back datatype (a model def over the
     view).  Any other order forward-references a name and Lean autobinds
     it to a universe metavariable ([Sort ?u]). *)
  let hb = !sig_hold_back in
  let held n = List.exists (fun (u, _) -> String.equal u n) hb in
  List.iter
    (fun (n, d) ->
      if (not (covered n)) && not (held n)
      then Buffer.add_string buf ("public " ^ d))
    vp.Cmi_format.vp_datatypes;
  let blocks_pre, blocks_post =
    List.partition
      (fun b -> not (block_mentions_holdback hb b))
      vp.Cmi_format.vp_blocks
  in
  List.iter (fun b -> Buffer.add_string buf b) blocks_pre;
  List.iter
    (fun (n, d) ->
      if (not (covered n)) && held n
      then Buffer.add_string buf ("public " ^ d))
    vp.Cmi_format.vp_datatypes;
  List.iter (fun b -> Buffer.add_string buf b) blocks_post;
  let olean_out =
    Filename.concat dir (Vox_module.sig_module_name unit ^ ".olean")
  in
  match
    Vox_module.build_olean
      ~lean_command:(lean_command ())
      ~lean_path_dirs:dirs
      ~olean_out
      ~module_name:(Vox_module.sig_module_name unit)
      (Buffer.contents buf)
  with
  | Ok () -> ()
  | Error e -> err e
;;

(* Does an interface [%%vox.lean] block already declare [name]? If so that block is
   authoritative and a [val total_ name] must not also emit an opaque stub (Lean would see
   the name twice). *)
let block_declares blocks name =
  let is_id c =
    (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || c = '_'
    || c = '\''
  in
  let hits text kw =
    let k = kw ^ " " in
    let nk = String.length k
    and nn = String.length name
    and nt = String.length text in
    let rec scan i =
      if i + nk + nn > nt
      then false
      else if String.equal (String.sub text i nk) k
              && String.equal (String.sub text (i + nk) nn) name
              && (i + nk + nn = nt || not (is_id text.[i + nk + nn]))
      then true
      else scan (i + 1)
    in
    scan 0
  in
  List.exists (fun text -> List.exists (hits text) [ "def"; "opaque"; "abbrev" ]) blocks
;;

(* Name-only export of [total_] spec functions declared in an .mli as
   [val total_ f : t1 -> ... -> tn -> ret]. The interface has no body, so the
   client-facing declaration is an uninterpreted [opaque f : sorts]: clients name [f] in
   refinements and receive facts about it (an exported contract that mentions [f], or a
   sealed obligation), but cannot UNFOLD it -- the implementation's [total_ f] equations
   stay private to the implementation, which discharges its own VCs with them and reads
   this stub only as any client would. Abstraction is the default: over an interface that
   hides a type, a client must not compute [f] on that type's constructors. To EXPOSE the
   equations instead, write them in an interface block
   ([@[grind, expose] public def f ... := ...]); then [f] unfolds everywhere (the block
   wins, and this stub is suppressed). *)
let total_spec_decls env (sg : Types.signature) ~blocks =
  List.filter_map
    (fun (item : Types.signature_item) ->
      match item with
      | Sig_value (id, vd, _)
        when Vox_reflect.has_total_attr vd.val_attributes
             && not (block_declares blocks (Ident.name id)) ->
        let loc = vd.val_loc in
        let rec decompose ty =
          match get_desc (Ctype.vox_expand_head env ty) with
          | Tarrow (_, a, r, _) ->
            let args, ret = decompose r in
            a :: args, ret
          | _ -> [], ty
        in
        let args, ret = decompose vd.val_type in
        (* The sort of each argument/result. A signature wraps a value type in [Tpoly]
           (and a refinement in [Trefine]); peel both to the head. int/bool are the ghost
           scalars; any other datatype sorts at its own [Vox_<path>]. The name is rendered
           from the PATH (path-keyed resolution is unavailable at .mli-export time), and
           the datatype itself is DECLARED in the sig module by
           [register_datatypes_in_blocks], which scans this very block's text for the same
           [Vox_<path>] token. *)
        let rec peel t =
          match get_desc (Ctype.vox_expand_head env t) with
          | Tpoly (t', _) -> peel t'
          | Trefine (skel, _, _) -> peel skel
          | d -> d
        in
        let sort what t =
          match peel t with
          | Tconstr (p, [], _) when Path.same p Predef.path_int -> "Int"
          | Tconstr (p, [], _) when Path.same p Predef.path_bool -> "Prop"
          | Tconstr (p, [], _) -> lean_dt_name p
          | _ ->
            Location.raise_errorf
              ~loc
              "vox: %s of the [val total_] spec function %s must be int, bool, or a \
               simple (non-parameterized) datatype"
              what
              (Ident.name id)
        in
        let arrows = List.map (sort "a parameter") args @ [ sort "the result" ret ] in
        Some
          (Printf.sprintf
             "public opaque %s : %s\n"
             (Ident.name id)
             (String.concat " -> " arrows))
      | _ -> None)
    sg
;;

let cmi_export_of_signature (tsg : Typedtree.signature) =
  let blocks = collect_blocks_sig tsg in
  let blocks = blocks @ total_spec_decls tsg.sig_final_env tsg.sig_type ~blocks in
  match
    cmi_export tsg.sig_final_env tsg.sig_type ~defs:[] ~blocks ~sig_module:(blocks <> [])
  with
  | None -> None
  | Some vp ->
    if vp.Cmi_format.vp_sig_module then build_sig_module vp;
    Some vp
;;

let cmi_export_of_structure (str : structure) (sg : Types.signature) =
  cmi_export
    str.str_final_env
    sg
    ~defs:!spec_defs
    ~blocks:(List.map fst (collect_blocks str))
    ~sig_module:false
;;

(* A verification condition needs the match-splitting proof fallback exactly when it
   carries a deep-pattern existential negation (a [Pnot] over a [Pquant Qexists ...]):
   grind will not instantiate such a negative under a plain goal, but once the spec
   function's match is [split] the negation refutes the overlapping model case. Ordinary
   negatives ([not (s is C)], ground disequalities) carry no existential and keep the
   plain [by grind]. *)
let rec pred_has_exists (p : Refinement.pred) =
  let open Refinement in
  match p with
  | Pquant (Qexists, _, _) -> true
  | Pquant (Qforall, _, a)
  | Pnot a
  | Pfield (_, _, a)
  | Pis (_, _, a)
  | Pproj (_, _, a)
  | Plam (_, a) -> pred_has_exists a
  | Pconstr (_, _, args) | Pfun (_, args) | Ptuple args ->
    List.exists pred_has_exists args
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
    pred_has_exists a || pred_has_exists b
  | Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> false
;;

let vc_needs_split vc =
  List.exists
    (fun f ->
      match f with
      | Refinement.Pnot b -> pred_has_exists b
      | _ -> false)
    vc.vc_facts
;;

(* The spec-function heads applied in a predicate, outermost-first and de-duplicated: the
   definitions the split fallback [unfold]s so the match they wrap becomes visible to
   [split]. *)
let pfun_heads p =
  let acc = ref [] in
  let add f = if not (List.mem f !acc) then acc := !acc @ [ f ] in
  let rec go (p : Refinement.pred) =
    let open Refinement in
    match p with
    | Pfun (f, args) ->
      add f;
      List.iter go args
    | Pconstr (_, _, args) | Ptuple args -> List.iter go args
    | Pfield (_, _, a)
    | Pis (_, _, a)
    | Pproj (_, _, a)
    | Pnot a
    | Pquant (_, _, a)
    | Plam (_, a) -> go a
    | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
      go a;
      go b
    | Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> ()
  in
  go p;
  !acc
;;

let lean_theorem ?(explain = false) buf i vc =
  Buffer.add_string buf (Printf.sprintf "theorem vc_%d " i);
  let seen = Hashtbl.create 16 in
  List.iter
    (fun id ->
      if not (Hashtbl.mem seen id)
      then (
        Hashtbl.add seen id ();
        let sort =
          match Hashtbl.find_opt name_sorts id with
          | Some ds -> lean_sort ds
          | None -> "VoxU"
        in
        Buffer.add_string buf (Printf.sprintf "(%s : %s) " (lean_name id) sort)))
    (free_vars_of_vc vc);
  let seen_g = Hashtbl.create 4 in
  List.iter
    (fun gp ->
      let key = path_uname gp in
      if not (Hashtbl.mem seen_g key)
      then (
        Hashtbl.add seen_g key ();
        let sort =
          match Hashtbl.find_opt globals key with
          | Some (_, ds) -> lean_sort ds
          | None -> "VoxU"
        in
        Buffer.add_string buf (Printf.sprintf "(g_%s : %s) " (lean_sanitize key) sort)))
    (List.concat_map Refinement.free_globals (vc.vc_goal :: vc.vc_facts));
  List.iteri
    (fun j f ->
      Buffer.add_string buf (Printf.sprintf "(h_%d : " j);
      lean_of_pred buf f;
      Buffer.add_string buf ") ")
    vc.vc_facts;
  (* Exhaustiveness hypotheses: for each tester subject among the facts, tell grind the
     subject IS one of its constructors, so it can case on the negations. The disjunction
     is [Por] over the positive testers, so it reuses the serializer; validated shape: (∃
     a, s = K a) ∨ ... ∨ s = M. *)
  let seen_subj = Hashtbl.create 4 in
  let exh = ref 0 in
  List.iter
    (fun f ->
      let rec collect (q : Refinement.pred) =
        (match q with
         | Refinement.Pis (path, _, ((Refinement.Pvar _ | Refinement.Pglobal _) as subj))
           ->
           let skey =
             match subj with
             | Refinement.Pvar id -> Ident.unique_name id
             | _ -> Refinement.to_string subj
           in
           let key = skey ^ "|" ^ path_uname path in
           if not (Hashtbl.mem seen_subj key)
           then (
             Hashtbl.add seen_subj key ();
             match find_datatype path with
             | Some (_, Dt_variant (_, constrs)) ->
               let disj =
                 match
                   List.map (fun (cname, _) -> Refinement.Pis (path, cname, subj)) constrs
                 with
                 | [] -> assert false (* simple variants are non-empty *)
                 | t :: ts -> List.fold_left (fun acc t' -> Refinement.Por (acc, t')) t ts
               in
               incr exh;
               Buffer.add_string buf (Printf.sprintf "(h_exh%d : " !exh);
               lean_of_pred buf disj;
               Buffer.add_string buf ") "
             | Some (_, (Dt_record _ | Dt_opaque)) | None -> ())
         | _ -> ());
        match q with
        | Refinement.Pis (_, _, a)
        | Refinement.Pfield (_, _, a)
        | Refinement.Pproj (_, _, a)
        | Refinement.Pquant (_, _, a)
        | Refinement.Plam (_, a)
        | Refinement.Pnot a -> collect a
        | Refinement.Pconstr (_, _, args)
        | Refinement.Pfun (_, args)
        | Refinement.Ptuple args -> List.iter collect args
        | Refinement.Pbinop (_, a, b)
        | Refinement.Pand (a, b)
        | Refinement.Por (a, b)
        | Refinement.Pimp (a, b) ->
          collect a;
          collect b
        | Refinement.Pbound
        | Refinement.Pvar _
        | Refinement.Pglobal _
        | Refinement.Pint _
        | Refinement.Pbool _ -> ()
      in
      collect f)
    vc.vc_facts;
  Buffer.add_string buf ": ";
  lean_of_pred buf vc.vc_goal;
  (* [grind?] proves the goal exactly as [grind] would AND reports the user facts it used
     -- but ONLY when it succeeds: on an unprovable goal it inserts [sorry] and succeeds
     with a warning, which would be unsound as a verifier. So [grind?] is used purely for
     the [-vox-explain-proofs] REPORT, in a second pass over a file [grind] has already
     fully verified (see [run_lean]); the verdict always comes from [grind]. *)
  if vc_needs_split vc
  then (
    let tail = if explain then "grind?" else "grind" in
    let prefix =
      match pfun_heads vc.vc_goal with
      | [] -> ""
      | fs -> "unfold " ^ String.concat " " fs ^ "; "
    in
    Buffer.add_string
      buf
      (Printf.sprintf " := by first | grind | (%ssplit <;> %s)\n" prefix tail))
  else Buffer.add_string buf (if explain then " := by grind?\n" else " := by grind\n")
;;

(* Returns the file contents and, per theorem, the 1-based line it occupies (for mapping
   lean's error locations back to VCs). *)
let lean_file ?(witness = "") ?(explain = false) vcs =
  let buf = Buffer.create 1024 in
  let needs_voxu =
    List.exists
      (fun vc ->
        List.exists
          (fun id ->
            match Hashtbl.find_opt name_sorts id with
            | Some s -> sort_needs_voxu s
            | None -> true)
          (free_vars_of_vc vc))
      vcs
    || datatype_field_needs_voxu ()
  in
  (* Header, as (text, provenance) SEGMENTS so line accounting cannot drift from what is
     emitted: VoxU (referenced by datatype fields, so first); each imported unit's
     datatype declarations in dependency order (deduplicated across units by stable name);
     this module's remaining datatypes; then -- only when some VC applies a spec function
     -- the prelude text: imported blocks in dependency order, the [-vox-prelude] file,
     this module's own blocks in source order; finally the elaboration bound. Theorems
     follow, one per line. A solver error inside a block is reported at the block's own
     location (or its defining unit). *)
  let want_spec_text = List.exists vc_uses_spec_fun vcs in
  (* Block text (imported blocks, -vox-prelude, own blocks) declares spec functions AT
     VoxU (e.g. [opaque f : VoxU -> Int]); if VoxU itself were not declared, Lean's
     autobound implicits would silently generalize those signatures
     ([{VoxU : Sort u} -> ...]), turning ill-sorted applications into polymorphic ones
     instead of errors. So the prelude implies VoxU. *)
  let needs_voxu = needs_voxu || imported_need_voxu () || want_spec_text in
  let needs_iarray =
    List.exists
      (fun vc ->
        List.exists
          (fun id ->
            match Hashtbl.find_opt name_sorts id with
            | Some s -> sort_needs_iarray s
            | None -> false)
          (free_vars_of_vc vc)
        || List.exists
             (fun p ->
               Refinement.mentions_fun Refinement.ia_len p
               || Refinement.mentions_fun Refinement.ia_get p)
             (vc.vc_goal :: vc.vc_facts))
      vcs
    || datatype_field_needs_iarray ()
  in
  let segments = ref [] in
  let seg ?src text = if text <> "" then segments := (text, src) :: !segments in
  (* MODULE-MODE (file compilation): the input is a Lean module over VoxCore (VoxU, the
     iarray theory, tuple products -- their inline emission below is toplevel-only) and
     the sig modules of sig-bearing imports, whose datatypes and blocks then ride the
     artifact instead of being spliced. The compiling unit's OWN interface, when
     sig-bearing, is neither imported nor spliced: its text is re-elaborated by the SEAL,
     appended after the theorems, which checks the implementation against it (an interface
     [axiom] is an obligation). Toplevel sessions keep the legacy self-contained shape. *)
  let file_mode = not !toplevel_active in
  let this_unit = Env.get_current_unit_name () in
  let self_vp =
    if file_mode
    then
      List.find_map
        (fun (u, vp) ->
          if String.equal u this_unit && vp.Cmi_format.vp_sig_module
          then Some vp
          else None)
        !imported_specs
    else None
  in
  let sealing = self_vp <> None in
  (* Two self-modes (the F* interface discipline). A TRANSPARENT unit -- no implementation
     blocks and no reflected definitions -- keeps its interface text spliced flat: the
     model lives wholly in the .mli, and the trailing seal degenerates to the axiom guard
     (an interface [axiom] finds only itself, an axiom, and is rejected as unimplemented
     -- transparent units cannot owe laws). A SEALED unit owns every interface constant in
     its implementation blocks; the interface text is elaborated only inside the seal's
     namespace, and laws are matched against same-named impl theorems. *)
  let splice_self = sealing && !embedded_blocks = [] && !spec_defs = [] in
  let sig_imported (vp : Cmi_format.vox_spec_export) u =
    file_mode && vp.Cmi_format.vp_sig_module && not (String.equal u this_unit)
  in
  (* Module files hide non-[public] declarations from public signatures: datatype
     declarations must be [public] for interface text (spliced or sealed) to mention them.
     The solver input is never imported, so the marker is otherwise inert. *)
  let dt_vis = if file_mode then "public " else "" in
  if file_mode
  then (
    seg "module\n";
    if sealing then seg "import Lean\n";
    seg (Printf.sprintf "public import %s\n" Vox_module.core_module_name);
    List.iter
      (fun (u, vp) ->
        if sig_imported vp u
        then seg (Printf.sprintf "public import %s\n" (Vox_module.sig_module_name u)))
      !imported_specs);
  if (not file_mode) && needs_voxu then seg "opaque VoxU : Type\n";
  if (not file_mode) && needs_iarray then seg lean_iarray_theory;
  let seen = ref [] in
  List.iter
    (fun (unit, vp) ->
      if sealing && (not splice_self) && String.equal unit this_unit
      then
        (* the seal re-elaborates the interface; the implementation's own datatype
           declarations below cover the types *)
        ()
      else
        List.iter
          (fun (n, leand) ->
            if not (List.exists (String.equal n) !seen)
            then (
              seen := n :: !seen;
              if (not (sig_imported vp unit))
                 && not (file_mode && Vox_module.core_tuple_uname n)
              then seg ~src:(Imported_block unit) (dt_vis ^ leand)))
          vp.Cmi_format.vp_datatypes)
    !imported_specs;
  (* Tuple product structures precede this module's datatypes (whose fields may be
     tuple-sorted); imported exports carry their own, deduplicated by the stable per-arity
     name. In file mode the common arities ride VoxCore; only wider ones are spliced. *)
  List.iter
    (fun n ->
      if ((not file_mode) || n > Vox_module.max_tuple_arity)
         && not (List.exists (String.equal (tuple_uname n)) !seen)
      then (
        seen := tuple_uname n :: !seen;
        seg (lean_tuple_decl ~vis:dt_vis n)))
    !tuple_arities;
  (* Parameterized opaques for this module's own [@@vox.poly] heads (imported units' heads
     arrive through their exported declaration lists above, deduplicated by stable name
     like datatypes); they precede the own datatypes, whose fields may be at such sorts. *)
  List.iter
    (fun ((p, _) as hd) ->
      let n = path_uname p in
      if not (List.exists (String.equal n) !seen)
      then (
        seen := n :: !seen;
        seg (lean_poly_decl hd)))
    !poly_heads;
  let own_decls = Buffer.create 256 in
  (* Datatypes with a named/via ([S_lean]) field are held back until
     AFTER this unit's own blocks (which may define that sort); see
     [dt_uses_lean_field]. *)
  lean_datatype_decls own_decls ~skip:!seen ~vis:dt_vis
    ~filter:(fun d -> not (dt_uses_lean_field d));
  seg (Buffer.contents own_decls);
  (* Imported blocks and the [-vox-prelude] file come BEFORE this module's reflected
     definitions: a definition may call an imported reflected function (whose definition
     rides the exporting unit's blocks). They are therefore also needed whenever this
     module has definitions, not only when a VC applies a spec function. *)
  if want_spec_text || !spec_defs <> []
  then (
    List.iter
      (fun (unit, vp) ->
        if (not (sig_imported vp unit))
           && not (sealing && (not splice_self) && String.equal unit this_unit)
        then
          List.iter
            (fun text -> seg ~src:(Imported_block unit) text)
            vp.Cmi_format.vp_blocks)
      !imported_specs;
    seg (prelude ()));
  (* Reflected definitions, unconditionally: they are checked (termination included) even
     when nothing else needs the prelude. This module's own blocks follow them, so a block
     may state lemmas about them. *)
  List.iter
    (fun (d : Vox_reflect.spec_def) ->
      let b = Buffer.create 128 in
      lean_spec_def b d;
      seg ~src:(Reflected_def d) (Buffer.contents b))
    !spec_defs;
  (* Own blocks straddle the held-back (via/ghost-field) datatypes: the
     ghost-sort-defining blocks come first, then those datatypes, then
     any block that references one (a model def over the view). *)
  let own_hb =
    List.filter_map
      (fun (p, decl) ->
        if dt_uses_lean_field decl
        then Some (path_uname p, lean_dt_name p)
        else None)
      !datatypes
  in
  let own_pre, own_post =
    if want_spec_text
    then
      List.partition
        (fun (s, _) -> not (block_mentions_holdback own_hb s))
        !embedded_blocks
    else [], []
  in
  (* In file mode the held-back view datatypes are emitted [public] (so
     the seal can name them); a local block that defines a via/ghost sort
     one of them references must expose THAT sort [public] too. *)
  let ghost_names =
    if file_mode
    then
      List.concat_map
        (fun (_, decl) ->
          if dt_uses_lean_field decl then dt_lean_sort_names decl else [])
        !datatypes
    else []
  in
  let emit_block (s, loc) =
    seg ~src:(Local_block loc) (publicize_ghost_decls ghost_names s)
  in
  List.iter emit_block own_pre;
  let own_lean_decls = Buffer.create 128 in
  lean_datatype_decls own_lean_decls ~skip:!seen ~vis:dt_vis
    ~filter:dt_uses_lean_field;
  if Buffer.length own_lean_decls > 0 then seg (Buffer.contents own_lean_decls);
  List.iter emit_block own_post;
  (* Bound elaboration per theorem: a diverging [grind] must count as
     a verification failure, not hang the build.  (A wedged process
     outside elaboration remains out of scope.)  Emitted
     after the prelude so a prelude may begin with [import], which
     Lean requires to be the first command in the file. *)
  (* [@@vox.lemma] facts: after the reflected defs and blocks they may
     mention, before the VC theorems that use them (the grind_pattern
     makes each fire by E-matching). *)
  List.iter
    (fun (nm, text, lemloc) -> seg ~src:(Lemma (nm, lemloc)) text)
    !lemma_defs;
  seg "set_option maxHeartbeats 400000\n";
  let segments = List.rev !segments in
  let block_ranges, first_line =
    List.fold_left
      (fun (ranges, start) (text, src) ->
        let n = count_lines text in
        let ranges =
          match src with
          | Some src -> (start, n, src) :: ranges
          | None -> ranges
        in
        ranges, start + n)
      ([], 1)
      segments
  in
  List.iter (fun (text, _) -> Buffer.add_string buf text) segments;
  (* Witness-check mode ([witness] non-empty): emit the given check theorems in place of
     the VC theorems and the seal -- the header and prelude are identical, so a validated
     witness is checked against exactly the theory the failed VC was discharged in.
     [explain] swaps grind for grind? on the normal path (the used-lemmas report pass;
     never combined with witness mode). *)
  if String.equal witness ""
  then (
    List.iteri (fun i vc -> lean_theorem ~explain buf i vc) vcs;
    match self_vp with
    | Some vp ->
      Buffer.add_string
        buf
        (Vox_module.seal_text ~sig_text:(String.concat "\n" vp.Cmi_format.vp_blocks))
    | None -> ())
  else Buffer.add_string buf witness;
  (* The seal follows the VC theorems (one line each); errors there are interface
     obligations, not VC failures. *)
  let seal_start = first_line + List.length vcs in
  let block_of_line line =
    if sealing && line >= seal_start
    then Some (Seal, line - seal_start + 1)
    else
      List.find_map
        (fun (start, n, src) ->
          if start <= line && line < start + n then Some (src, line - start + 1) else None)
        block_ranges
  in
  ( Buffer.contents buf
  , (fun line ->
      (* An error on a header line maps to no VC (and a negative index would make
         [List.nth_opt] raise). *)
      if line < first_line then None else List.nth_opt vcs (line - first_line))
  , block_of_line
  , first_line )
;;

(* Counterexample rendering: a failed [grind] prints, among its goal diagnostics, the
   arithmetic model its linear solver ended on ("[assign] v_n_308 := 7"). Rewritten to
   source names, that model is usually a concrete input on which the goal is false -- the
   single most useful thing a failure message can carry. Lines still mentioning internal
   [v_...] names after rewriting (values the VC cannot name) are dropped; ["a ^ 2"]-style
   bracketed ring monomials too. *)
let replace_all ~sub ~by s =
  let n = String.length sub in
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i <= String.length s - n do
    if String.equal (String.sub s !i n) sub
    then (
      Buffer.add_string buf by;
      i := !i + n)
    else (
      Buffer.add_char buf s.[!i];
      incr i)
  done;
  Buffer.add_substring buf s !i (String.length s - !i);
  Buffer.contents buf
;;

(* ------------------------------------------------------------------ *)
(* Witness-based classification of a FAILED VC.

   A [grind] failure alone does not tell truth from timeout: grind treats nonlinear terms
   as atoms, so for [x * x >= 0] it "refutes" the goal with the impossible atom value
   [x*x = -1]. Reporting that as a counterexample is a lie. Instead, once a VC fails we
   try to VALIDATE a real counterexample by evaluation: pick concrete values for the VC's
   binders and let Lean EVALUATE (via [decide]/[grind] on the fully ground instance)
   whether every hypothesis holds and the goal fails. A validated assignment DISPROVES the
   VC; if none is found the failure is UNKNOWN (automation gave up -- the property may
   still be true) and NO witness is shown. Nonlinear atoms are never assigned (only
   binders are), so a nonsense atom value can never masquerade as a counterexample. *)

(* Instantiate a datatype declaration's field sort at a use site's type arguments (turning
   each [S_param i] into [args.(i)]). *)
let rec inst_sort args = function
  | S_param i ->
    (match List.nth_opt args i with
     | Some s -> s
     | None -> S_other)
  | S_data (p, a) -> S_data (p, List.map (inst_sort args) a)
  | S_tuple cs -> S_tuple (List.map (inst_sort args) cs)
  | S_poly (p, a) -> S_poly (p, List.map (inst_sort args) a)
  | S_lean (n, a) -> S_lean (n, List.map (inst_sort args) a)
  | S_arrow (a, b) -> S_arrow (inst_sort args a, inst_sort args b)
  | (S_int | S_bool | S_iarray | S_other) as s -> s
;;

(* A sort whose values we can ENUMERATE and Lean can EVALUATE: Int, bool (a Prop, valued
   True/False), and simple variants/records whose fields are themselves evaluable. Opaque
   (VoxU), array, poly, ghost (S_lean) sorts and bare type parameters are not -- we cannot
   name ground inhabitants a decision procedure can compute over. *)
let rec sort_evaluable ?(seen = []) = function
  | S_int | S_bool -> true
  | S_tuple comps -> List.for_all (sort_evaluable ~seen) comps
  | S_data (p, args) ->
    (* A datatype we are already expanding recurs structurally (e.g. a list's tail): treat
       the recursive occurrence as evaluable -- [enum_sort] bounds the actual enumeration
       by [depth], so unlike here it terminates. Without this guard [sort_evaluable] loops
       forever on any recursive datatype. *)
    List.exists (Path.same p) seen
    ||
      (match find_datatype p with
      | Some (_, Dt_variant (_, constrs)) ->
        List.for_all
          (fun (_, fss) ->
            List.for_all
              (fun fs -> sort_evaluable ~seen:(p :: seen) (inst_sort args fs))
              fss)
          constrs
      | Some (_, Dt_record (_, fields)) ->
        List.for_all
          (fun (_, fs) -> sort_evaluable ~seen:(p :: seen) (inst_sort args fs))
          fields
      | Some (_, Dt_opaque) | None -> false)
  | S_param _ | S_poly _ | S_lean _ | S_iarray | S_arrow _ | S_other -> false
;;

let reflected_names () =
  List.map (fun (d : Vox_reflect.spec_def) -> d.Vox_reflect.sd_name) !spec_defs
;;

(* A predicate Lean can evaluate on a ground instance: no quantifiers, no constructor
   testers (existentials over uninterpretable sorts), and every spec-function application
   names a REFLECTED (computable) definition -- an opaque/block/axiom function has no
   runtime body. *)
let rec pred_evaluable names (p : Refinement.pred) =
  let open Refinement in
  match p with
  | Pquant _ | Pis _ | Plam _ -> false
  | Pfun (f, args) -> List.mem f names && List.for_all (pred_evaluable names) args
  | Pbound -> false
  | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> true
  | Pconstr (_, _, args) | Ptuple args -> List.for_all (pred_evaluable names) args
  | Pfield (_, _, a) | Pproj (_, _, a) | Pnot a -> pred_evaluable names a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
    pred_evaluable names a && pred_evaluable names b
;;

(* Can we even attempt witness validation? Every hypothesis and the goal must be
   evaluable, and every free binder (and mentioned global) must be at an evaluable sort.
   Otherwise the honest verdict is UNKNOWN, never DISPROVED. *)
let vc_evaluable vc =
  let names = reflected_names () in
  List.for_all (pred_evaluable names) (vc.vc_goal :: vc.vc_facts)
  && List.for_all
       (fun id ->
         match Hashtbl.find_opt name_sorts id with
         | Some s -> sort_evaluable s
         | None -> false)
       (free_vars_of_vc vc)
  && List.for_all
       (fun gp ->
         match Hashtbl.find_opt globals (path_uname gp) with
         | Some (_, s) -> sort_evaluable s
         | None -> false)
       (List.concat_map Refinement.free_globals (vc.vc_goal :: vc.vc_facts))
;;

let cap_list n xs = if List.length xs > n then List.filteri (fun i _ -> i < n) xs else xs

(* The integer candidate pool: values the grind model suggested (so a witness OUTSIDE the
   small spread is still reachable) followed by a small symmetric spread around zero. *)
let int_pool model_ints =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun n ->
      if Hashtbl.mem seen n
      then None
      else (
        Hashtbl.add seen n ();
        Some (Refinement.Pint n)))
    (model_ints @ [ 0; 1; -1; 2; -2; 3; -3 ])
  |> cap_list 12
;;

let product ~cap (doms : 'a list list) : 'a list list =
  List.fold_left
    (fun acc d ->
      cap_list cap (List.concat_map (fun row -> List.map (fun v -> row @ [ v ]) d) acc))
    [ [] ]
    doms
;;

(* Ground values of a sort, up to [depth] of datatype nesting. *)
let rec enum_sort ~model_ints ~depth s : Refinement.pred list =
  match s with
  | S_int -> int_pool model_ints
  | S_bool -> [ Refinement.Pbool false; Refinement.Pbool true ]
  | S_tuple comps ->
    let doms = List.map (enum_sort ~model_ints ~depth) comps in
    List.map (fun vs -> Refinement.Ptuple vs) (product ~cap:40 doms)
  | S_data (p, args) ->
    (match find_datatype p with
     | Some (_, Dt_variant (_, constrs)) ->
       cap_list
         40
         (List.concat_map
            (fun (cname, fss) ->
              if fss = []
              then [ Refinement.Pconstr (p, cname, []) ]
              else if depth <= 0
              then []
              else (
                let doms =
                  List.map
                    (fun fs ->
                      enum_sort ~model_ints ~depth:(depth - 1) (inst_sort args fs))
                    fss
                in
                List.map
                  (fun vs -> Refinement.Pconstr (p, cname, vs))
                  (product ~cap:40 doms)))
            constrs)
     | Some (_, Dt_record (_, fields)) ->
       if fields <> [] && depth <= 0
       then []
       else (
         let doms =
           List.map
             (fun (_, fs) -> enum_sort ~model_ints ~depth:(depth - 1) (inst_sort args fs))
             fields
         in
         (* a record is a single-constructor structure: its anonymous constructor is
            [<StructName>.mk], which [Pconstr .. "mk" ..] serializes correctly. *)
         List.map (fun vs -> Refinement.Pconstr (p, "mk", vs)) (product ~cap:40 doms))
     | Some (_, Dt_opaque) | None -> [])
  | S_param _ | S_poly _ | S_lean _ | S_iarray | S_arrow _ | S_other -> []
;;

(* Integers appearing on the right of a grind [[assign] name := N] line -- model hints to
   seed the integer pool. *)
let parse_model_ints assigns =
  List.filter_map
    (fun s ->
      match Misc.search_substring ":=" s 0 with
      | exception Not_found -> None
      | i ->
        let rhs = String.trim (String.sub s (i + 2) (String.length s - i - 2)) in
        let rhs = replace_all ~sub:"\xe3\x80\x8c" ~by:"" rhs in
        let rhs = replace_all ~sub:"\xe3\x80\x8d" ~by:"" rhs in
        int_of_string_opt (String.trim rhs))
    assigns
;;

(* A candidate assignment: a value for every free binder and global. *)
type wkey =
  | Wvar of Ident.t
  | Wglobal of Path.t

let candidate_domains vc model_ints =
  let seen = Hashtbl.create 8 in
  let vars =
    List.filter
      (fun id ->
        let u = Ident.unique_name id in
        if Hashtbl.mem seen u
        then false
        else (
          Hashtbl.add seen u ();
          true))
      (free_vars_of_vc vc)
  in
  let gseen = Hashtbl.create 4 in
  let gpaths =
    List.filter
      (fun gp ->
        let k = path_uname gp in
        if Hashtbl.mem gseen k
        then false
        else (
          Hashtbl.add gseen k ();
          true))
      (List.concat_map Refinement.free_globals (vc.vc_goal :: vc.vc_facts))
  in
  let var_dom id =
    match Hashtbl.find_opt name_sorts id with
    | Some s -> List.map (fun v -> Wvar id, v) (enum_sort ~model_ints ~depth:2 s)
    | None -> []
  in
  let glob_dom gp =
    match Hashtbl.find_opt globals (path_uname gp) with
    | Some (_, s) -> List.map (fun v -> Wglobal gp, v) (enum_sort ~model_ints ~depth:2 s)
    | None -> []
  in
  List.map var_dom vars @ List.map glob_dom gpaths
;;

(* One witness-check theorem: bind every candidate value with [let], then assert every
   hypothesis together with the NEGATED goal. If Lean can prove it, the assignment is a
   real counterexample. Both [decide] (ground Int/Prop) and [grind] (ground reflected
   functions) are tried; whichever closes it validates the witness. *)
let wc_theorem_text ~tac vc (row : (wkey * Refinement.pred) list) i =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Printf.sprintf "theorem wc_%d : (" i);
  List.iter
    (fun (k, v) ->
      (match k with
       | Wvar id ->
         let sort =
           match Hashtbl.find_opt name_sorts id with
           | Some ds -> lean_sort ds
           | None -> "VoxU"
         in
         Buffer.add_string buf (Printf.sprintf "let %s : %s := " (lean_name id) sort)
       | Wglobal gp ->
         let key = path_uname gp in
         let sort =
           match Hashtbl.find_opt globals key with
           | Some (_, ds) -> lean_sort ds
           | None -> "VoxU"
         in
         Buffer.add_string
           buf
           (Printf.sprintf "let g_%s : %s := " (lean_sanitize key) sort));
      lean_of_pred buf v;
      Buffer.add_string buf "; ")
    row;
  Buffer.add_char buf '(';
  List.iter
    (fun f ->
      Buffer.add_char buf '(';
      lean_of_pred buf f;
      Buffer.add_string buf ") \xe2\x88\xa7 ")
    vc.vc_facts;
  Buffer.add_string buf "(\xc2\xac (";
  lean_of_pred buf vc.vc_goal;
  Buffer.add_string buf ")))";
  Buffer.add_string buf (Printf.sprintf ") := %s\n" tac);
  Buffer.contents buf
;;

(* The VoxCore/sig import prefix a module-mode solver run needs (empty for toplevel
   sessions). Factored so witness validation runs in the same environment as the VC did. *)
let lean_env_prefix ~fallback_loc =
  if !toplevel_active
  then ""
  else (
    let dirs = lean_path_dirs () in
    (match
       Vox_module.ensure_core
         ~lean_command:(lean_command ())
         ~lean_path_dirs:dirs
         ~dir:(Filename.dirname !Location.input_name)
     with
     | Ok _ -> ()
     | Error e ->
       Location.raise_errorf
         ~loc:fallback_loc
         "vox: could not build the base theory module:@ %s"
         e);
    Vox_module.lean_path_env dirs ^ " ")
;;

(* Run the [n] witness-check theorems and return the indices that Lean PROVED (their line
   carries no error). A proved [wc_i] is a validated counterexample. *)
let run_witness_check ~fallback_loc contents ~n =
  let env_prefix = lean_env_prefix ~fallback_loc in
  let in_file = Filename.temp_file "voxwc" ".lean" in
  let out_file = Filename.temp_file "voxwc" ".out" in
  Misc.try_finally
    ~always:(fun () ->
      Misc.remove_file in_file;
      Misc.remove_file out_file)
    (fun () ->
      let oc = open_out in_file in
      output_string oc contents;
      close_out oc;
      (* [-D maxErrors]: with Lean's default (100) a batch of >100 failing candidates
         makes the frontend stop after 100 errors ("maximum number of errors reached,
         exiting"); the remaining theorems, then missing an error line, would be misread
         as validated witnesses. The option only takes effect from the command line. *)
      let cmd =
        Printf.sprintf
          "cd %s && %s%s -D maxErrors=1000000 %s > %s 2>&1"
          (Filename.quote (Filename.dirname in_file))
          env_prefix
          (Filename.quote (lean_command ()))
          (Filename.quote in_file)
          (Filename.quote out_file)
      in
      let _status = Sys.command cmd in
      (* the [n] theorems are the last [n] lines; the first sits at *)
      let first_wc_line = count_lines contents - n + 1 in
      let is_file_line l =
        String.length l > String.length in_file
        && String.equal (String.sub l 0 (String.length in_file)) in_file
      in
      let error_marker l =
        let needle = " error" in
        let m = String.length needle in
        let rec at j =
          if j + m > String.length l
          then false
          else if String.equal (String.sub l j m) needle
                  && j + m < String.length l
                  && (l.[j + m] = ':' || l.[j + m] = '(')
          then true
          else at (j + 1)
        in
        at 0
      in
      let failed = Hashtbl.create 16 in
      let ic = open_in out_file in
      (try
         while true do
           let l = input_line ic in
           if is_file_line l && error_marker l
           then (
             let rest =
               String.sub
                 l
                 (String.length in_file + 1)
                 (String.length l - String.length in_file - 1)
             in
             match String.index_opt rest ':' with
             | Some j ->
               (match int_of_string_opt (String.sub rest 0 j) with
                | Some ln -> Hashtbl.replace failed (ln - first_wc_line) ()
                | None -> ())
             | None -> ())
         done
       with
       | End_of_file -> ());
      close_in ic;
      List.filter (fun i -> not (Hashtbl.mem failed i)) (List.init n Fun.id))
;;

(* Validate a counterexample for [vc]; [Some row] is a proved assignment, [None] means
   none of the tried assignments checked. *)
let validate_witness ~fallback_loc vc assigns =
  if not (vc_evaluable vc)
  then None
  else (
    let model_ints = parse_model_ints assigns in
    let doms = candidate_domains vc model_ints in
    let candidates = cap_list 128 (product ~cap:128 doms) in
    match candidates with
    | [] -> None
    | _ ->
      (* [simp] with the reflected definitions EVALUATES a ground goal over spec functions
         and fails fast when it is false; [grind] is a last resort, and a low heartbeat
         bound keeps a doomed candidate from searching to the global limit. *)
      let defs = reflected_names () in
      let simp_part =
        if defs = [] then "simp" else "simp [" ^ String.concat ", " defs ^ "]"
      in
      let tac = Printf.sprintf "by first | decide | %s | grind" simp_part in
      let wtext =
        "set_option maxHeartbeats 8000\n"
        ^ String.concat
            ""
            (List.mapi (fun i row -> wc_theorem_text ~tac vc row i) candidates)
      in
      let contents, _, _, _ = lean_file ~witness:wtext [ vc ] in
      let n = List.length candidates in
      (match run_witness_check ~fallback_loc contents ~n with
       | [] -> None
       | ok :: _ -> Some (List.nth candidates ok)))
;;

let witness_display vc (row : (wkey * Refinement.pred) list) =
  with_vc_display vc (fun () ->
    let display = vc_display_fun vc in
    let entries =
      List.filter_map
        (fun (k, v) ->
          match k with
          | Wvar id -> Some (display id ^ " = " ^ Refinement.to_string v)
          | Wglobal gp -> Some (Path.name gp ^ " = " ^ Refinement.to_string v))
        row
    in
    match entries with
    | [] -> "\nThe goal is false unconditionally."
    | _ ->
      "\nCounterexample (validated -- every hypothesis holds and the goal fails here):"
      ^ String.concat "" (List.map (fun e -> "\n  " ^ e) entries))
;;

(* Classify a failed VC and raise the corresponding diagnostic: DISPROVED (a validated
   counterexample) or NOT PROVED (automation gave up -- no counterexample found; the
   property may still hold). *)
let classify_and_raise vc ~assigns ~msg =
  match validate_witness ~fallback_loc:vc.vc_loc vc assigns with
  | Some row ->
    Location.raise_errorf
      ~loc:vc.vc_loc
      "vox: verification failed -- goal DISPROVED (a counterexample was validated).@ \
       Goal: %s%s%s"
      (goal_for_error vc)
      (hyps_for_error vc)
      (witness_display vc row)
  | None ->
    Location.raise_errorf
      ~loc:vc.vc_loc
      "vox: verification failed -- NOT PROVED (automation gave up; no counterexample was \
       found, so the property may still hold).@ Goal: %s%s%s"
      (goal_for_error vc)
      (hyps_for_error vc)
      (if String.equal msg "" then "" else "\n(lean: " ^ msg ^ ")")
;;

(* [-vox-explain-proofs]: parse the [grind?] used-fact suggestions from a SUCCESSFUL
   solver run. Lean prints, per proved goal, a block headed "Try this:"/"Try these:"; the
   first "[apply] ..." line under it is the canonical suggestion. A first apply that
   mentions "for pattern" is grind's pattern-registration hint for an [@[grind]]
   declaration lacking a [grind_pattern] -- not a VC proof, so it is skipped. Every other
   block is one VC's proof, emitted in the same order as the theorems, so the k-th kept
   block is the k-th VC. The named lemmas grind used are the entries of its
   [grind only [...]] suggestion; a bare "grind only" with no bracket is an
   arithmetic/logic-only proof. Each entry may carry a use marker before the name --
   [usr foo] for a user lemma registered by [grind_pattern] (an [@@vox.lemma]), [= foo]
   for an equational lemma (an [@[grind]]/[@[grind =]] theorem, e.g. one from a
   [%%vox.lean] block that fires by ambient E-matching) -- and we take the name after any
   such marker. The name is already source-visible: an [@@vox.lemma]'s solver-side name is
   its OCaml identifier, and block/prelude theorem names pass through verbatim. *)
let parse_grind_used out_file =
  let lines =
    let ic = open_in out_file in
    let acc = ref [] in
    (try
       while true do
         acc := input_line ic :: !acc
       done
     with
     | End_of_file -> ());
    close_in ic;
    Array.of_list (List.rev !acc)
  in
  let n = Array.length lines in
  let contains ~sub s =
    let m = String.length sub in
    let rec at i =
      i + m <= String.length s && (String.equal (String.sub s i m) sub || at (i + 1))
    in
    at 0
  in
  let is_header l =
    let t = String.trim l in
    String.equal t "Try this:" || String.equal t "Try these:"
  in
  let is_indented l = String.length l > 0 && (l.[0] = ' ' || l.[0] = '\t') in
  (* the lemma names in [s]'s [grind only [...]] suggestion, in order of appearance. No
     bracket ("grind only") is an arithmetic/logic-only proof (the empty list). Each
     comma-separated entry is a use marker ([usr], [=], [=_], [<-], ...) followed by the
     lemma name; we take the trailing identifier of each entry. *)
  let used_names s =
    let m = String.length s in
    let is_id c =
      (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c = '_'
      || c = '\''
      || c = '.'
    in
    let is_id_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
    let opener = "only [" in
    let ol = String.length opener in
    let rec find i =
      if i + ol > m
      then None
      else if String.equal (String.sub s i ol) opener
      then Some (i + ol)
      else find (i + 1)
    in
    match find 0 with
    | None -> []
    | Some start ->
      let j = ref start in
      while !j < m && s.[!j] <> ']' do
        incr j
      done;
      let body = String.sub s start (!j - start) in
      List.filter_map
        (fun item ->
          (* the lemma name is the last identifier token of the entry, after its use
             marker; scan from the right. *)
          let n = String.length item in
          let e = ref n in
          while !e > 0 && not (is_id item.[!e - 1]) do
            decr e
          done;
          let b = ref !e in
          while !b > 0 && is_id item.[!b - 1] do
            decr b
          done;
          (* [#<hex>] is grind's name for an anonymous local fact, not a
             source-referenceable lemma -- drop it (so does a def-unfold like [fib], but
             that IS a named declaration, so it stays). *)
          if !e > !b && is_id_start item.[!b] && not (!b > 0 && item.[!b - 1] = '#')
          then Some (String.sub item !b (!e - !b))
          else None)
        (String.split_on_char ',' body)
  in
  let blocks = ref [] in
  let i = ref 0 in
  while !i < n do
    if is_header lines.(!i)
    then (
      (* the first "[apply]" line plus its wrapped continuation (up to the second
         "[apply]" alternative or the end of the indented body) *)
      let j = ref (!i + 1) in
      let apply_count = ref 0 in
      let first = Buffer.create 64 in
      while !j < n && is_indented lines.(!j) && not (is_header lines.(!j)) do
        let l = lines.(!j) in
        if contains ~sub:"[apply]" l then incr apply_count;
        if !apply_count = 1
        then (
          Buffer.add_char first ' ';
          Buffer.add_string first l);
        incr j
      done;
      let fa = Buffer.contents first in
      if !apply_count >= 1 && not (contains ~sub:"for pattern" fa)
      then blocks := used_names fa :: !blocks;
      i := !j)
    else incr i
  done;
  List.rev !blocks
;;

let run_lean vcs =
  (* Reflected definitions are checked (termination included) even when the module has no
     VCs of its own: a rejected definition must fail its defining module, not lie in wait. *)
  match vcs, !spec_defs, !lemma_defs with
  | [], [], [] -> ()
  | _ ->
    let fallback_loc =
      match vcs, !spec_defs, !lemma_defs with
      | vc :: _, _, _ -> vc.vc_loc
      | [], d :: _, _ -> d.Vox_reflect.sd_loc
      | [], [], (_, _, loc) :: _ -> loc
      | [], [], [] -> assert false
    in
    let contents, vc_of_line, block_of_line, first_line = lean_file vcs in
    let env_prefix =
      if !toplevel_active
      then ""
      else (
        (* Module-mode inputs import VoxCore (built on demand next to the unit's output)
           and sig oleans found on the load path. *)
        let dirs = lean_path_dirs () in
        (match
           Vox_module.ensure_core
             ~lean_command:(lean_command ())
             ~lean_path_dirs:dirs
             ~dir:(Filename.dirname !Location.input_name)
         with
         | Ok _ -> ()
         | Error e ->
           Location.raise_errorf
             ~loc:fallback_loc
             "vox: could not build the base theory module:@ %s"
             e);
        Vox_module.lean_path_env dirs ^ " ")
    in
    let in_file = Filename.temp_file "vox" ".lean" in
    let out_file = Filename.temp_file "vox" ".out" in
    Misc.try_finally
      ~always:(fun () ->
        Misc.remove_file in_file;
        Misc.remove_file out_file)
      (fun () ->
        let oc = open_out in_file in
        output_string oc contents;
        close_out oc;
        let cmd =
          (* module-mode inputs must live under lean's root directory: run from the temp
             dir the input was created in *)
          Printf.sprintf
            "cd %s && %s%s %s > %s 2>&1"
            (Filename.quote (Filename.dirname in_file))
            env_prefix
            (Filename.quote (lean_command ()))
            (Filename.quote in_file)
            (Filename.quote out_file)
        in
        let status = Sys.command cmd in
        if status <> 0
        then (
          (* Find the first error and map it back. Lean prints "<file>:L:C: error: ..."
             or, with a kind, "<file>:L:C: error(lean.some.kind): ...". Warnings use the
             same shapes with "warning" (e.g. for unused hypotheses); only errors count (a
             warning line before the real error must not steal the attribution). *)
          let error_marker l =
            let needle = " error" in
            let n = String.length needle in
            let rec at i =
              if i + n > String.length l
              then None
              else if String.equal (String.sub l i n) needle
                      && i + n < String.length l
                      && (l.[i + n] = ':' || l.[i + n] = '(')
              then Some (i + 1)
              else at (i + 1)
            in
            at 0
          in
          let ic = open_in out_file in
          let error_line = ref None in
          let msg = ref "" in
          let first_output = ref "" in
          (* The grind diagnostics that follow the first error include the arithmetic
             model ("[assign] x := 7") that refuted the goal; collect it until the next
             per-location message. *)
          let assigns = ref [] in
          let assigns_done = ref false in
          let is_file_line l =
            String.length l > String.length in_file
            && String.equal (String.sub l 0 (String.length in_file)) in_file
          in
          (try
             while true do
               let l = input_line ic in
               if String.equal !first_output "" then first_output := l;
               match !error_line with
               | None ->
                 (match String.index_opt l ':' with
                  | Some _ when is_file_line l && error_marker l <> None ->
                    let rest =
                      String.sub
                        l
                        (String.length in_file + 1)
                        (String.length l - String.length in_file - 1)
                    in
                    (match String.index_opt rest ':' with
                     | Some i ->
                       (match int_of_string_opt (String.sub rest 0 i) with
                        | Some n ->
                          error_line := Some n;
                          msg := l
                        | None -> ())
                     | None -> ())
                  | _ -> ())
               | Some _ ->
                 if is_file_line l
                 then assigns_done := true
                 else if not !assigns_done
                 then (
                   let t = String.trim l in
                   let tag = "[assign] " in
                   if String.length t > String.length tag
                      && String.equal (String.sub t 0 (String.length tag)) tag
                   then
                     assigns
                     := String.sub
                          t
                          (String.length tag)
                          (String.length t - String.length tag)
                        :: !assigns)
             done
           with
           | End_of_file -> ());
          close_in ic;
          let assigns = List.rev !assigns in
          (* Strip the (nondeterministic) temp-file prefix from the message; keep from
             "error"/"error(kind)" onward. *)
          let strip_msg m =
            match error_marker m with
            | Some i -> String.sub m i (String.length m - i)
            | None -> m
          in
          (* Under the provenance dump (editor mode), attribute a verdict to every Prove
             VC before raising: the theorem lines that carried a Lean error are the failed
             obligations, the rest are proved. We trust this only when EVERY error lands
             on a VC theorem -- a seal/block/reflect/import error can abort or cascade,
             which would make "everything else proved" untrue -- otherwise the table stays
             empty and the single-error behaviour below is unchanged. *)
          if !Clflags.vox_dump_vc_provenance
          then (
            let errored_lines =
              let ic = open_in out_file in
              let acc = ref [] in
              (try
                 while true do
                   let l = input_line ic in
                   if is_file_line l && error_marker l <> None
                   then (
                     let rest =
                       String.sub
                         l
                         (String.length in_file + 1)
                         (String.length l - String.length in_file - 1)
                     in
                     match String.index_opt rest ':' with
                     | Some i ->
                       (match int_of_string_opt (String.sub rest 0 i) with
                        | Some n -> acc := n :: !acc
                        | None -> ())
                     | None -> ())
                 done
               with
               | End_of_file -> ());
              close_in ic;
              List.rev !acc
            in
            let is_vc l =
              match vc_of_line l with
              | Some _ -> true
              | None -> false
            in
            if errored_lines <> [] && List.for_all is_vc errored_lines
            then (
              let errored_idx = List.map (fun l -> l - first_line) errored_lines in
              let first_failed = List.fold_left min max_int errored_idx in
              List.iteri
                (fun k vc ->
                  let verdict =
                    if not (List.mem k errored_idx)
                    then "proved"
                    else if k = first_failed
                    then (
                      match validate_witness ~fallback_loc:vc.vc_loc vc assigns with
                      | Some _ -> "disproved"
                      | None -> "unproved")
                    else "unproved"
                  in
                  Hashtbl.replace vc_verdicts k verdict)
                vcs));
          match !error_line with
          | None ->
            (* No per-theorem diagnostic: the solver itself failed (missing binary, crash,
               bad flags). Blaming a VC would hide the real cause. *)
            Location.raise_errorf
              ~loc:fallback_loc
              "vox: verification failed (lean solver error, exit %d): %s"
              status
              (if String.equal !first_output "" then "<no output>" else !first_output)
          | Some line ->
            (match block_of_line line with
             | Some (Local_block block_loc, rel_line) ->
               (* The error is inside an embedded [%%vox.lean] block: report it there, not
                  at a VC. *)
               Location.raise_errorf
                 ~loc:block_loc
                 "vox: error in this solver block (line %d of the block):@ %s"
                 rel_line
                 (strip_msg !msg)
             | Some (Imported_block unit, rel_line) ->
               (* The error is inside a spec prelude imported from another unit's
                  interface (e.g. two units exporting the same spec-function name). There
                  is no local source position; anchor at the current file. *)
               Location.raise_errorf
                 ~loc:(Location.in_file !Location.input_name)
                 "vox: error in the spec block imported from unit %s (line %d of its \
                  block):@ %s"
                 unit
                 rel_line
                 (strip_msg !msg)
             | Some (Seal, _) ->
               (* The implementation does not pay its interface's obligations; there is no
                  single expression to blame, so anchor at the unit. *)
               Location.raise_errorf
                 ~loc:(Location.in_file !Location.input_name)
                 "vox: the implementation does not seal its interface:@ %s"
                 (strip_msg !msg)
             | Some (Reflected_def d, _) ->
               (* The definition itself was rejected -- most often Lean could not
                  establish termination. *)
               let msg = strip_msg !msg in
               Location.raise_errorf
                 ~loc:d.Vox_reflect.sd_loc
                 "vox: the reflected definition of %s was rejected by the solver (is it \
                  terminating?  int-indexed recursion needs a [@@vox.decreases] \
                  metric)%s"
                 d.Vox_reflect.sd_name
                 (if String.equal msg "" then "" else "\n(lean: " ^ msg ^ ")")
             | Some (Lemma (nm, lem_loc), _) ->
               (* An [@@vox.lemma] theorem Lean could not re-prove: the proposition is
                  false, the recursion is not well-founded, or grind could not close the
                  induction. *)
               let msg = strip_msg !msg in
               Location.raise_errorf
                 ~loc:lem_loc
                 "vox: the [@@vox.lemma] %s was not proved (is the proposition true, and \
                  does the induction terminate?)%s"
                 nm
                 (if String.equal msg "" then "" else "\n(lean: " ^ msg ^ ")")
             | None ->
               let msg = strip_msg !msg in
               (* Lean reports a failing [import] at the module header (line 1), which
                  maps to no source; recover the unit from the message (e.g. two
                  interfaces exporting one spec-function name collide at import). *)
               let sig_import_failure =
                 let tag = "import " ^ Vox_module.sig_module_prefix in
                 match Misc.search_substring tag msg 0 with
                 | exception Not_found -> None
                 | j ->
                   let start = j + String.length tag in
                   let is_ident c =
                     (c >= 'A' && c <= 'Z')
                     || (c >= 'a' && c <= 'z')
                     || (c >= '0' && c <= '9')
                     || c = '_'
                     || c = '\''
                   in
                   let n = String.length msg in
                   let i = ref start in
                   while !i < n && is_ident msg.[!i] do
                     incr i
                   done;
                   if !i > start then Some (String.sub msg start (!i - start)) else None
               in
               (match sig_import_failure with
                | Some unit ->
                  Location.raise_errorf
                    ~loc:(Location.in_file !Location.input_name)
                    "vox: error in the spec imported from unit %s:@ %s"
                    unit
                    msg
                | None -> ());
               (match vc_of_line line, vcs with
                | Some vc, _ | None, vc :: _ -> classify_and_raise vc ~assigns ~msg
                | None, [] ->
                  Location.raise_errorf
                    ~loc:fallback_loc
                    "vox: verification failed (lean): %s"
                    (if String.equal msg "" then "<no output>" else msg))))
        else if !Clflags.vox_explain_proofs
        then (
          (* Verification SUCCEEDED and [-vox-explain-proofs] is on: run a SECOND pass
             with [grind?] purely to harvest the used-fact report. [grind?] is unsound as
             a verifier (it [sorry]s an unprovable goal and succeeds), so it must never
             decide the verdict; but this file has just fully verified under [grind], so
             every goal is provable and [grind?] closes each with a [grind only [...]]
             suggestion. Failure of this pass only degrades the report (verdict already
             stands), so it never raises. Attribute each suggestion to its VC by position
             (the theorems, hence their blocks, are in [vcs] order); attribute only when
             the counts line up, so a parse surprise degrades to "no used line" rather
             than a mislabelled one. *)
          let contents2, _, _, _ = lean_file ~explain:true vcs in
          let in2 = Filename.temp_file "vox" ".lean" in
          let out2 = Filename.temp_file "vox" ".out" in
          Misc.try_finally
            ~always:(fun () ->
              Misc.remove_file in2;
              Misc.remove_file out2)
            (fun () ->
              let oc = open_out in2 in
              output_string oc contents2;
              close_out oc;
              let cmd2 =
                Printf.sprintf
                  "cd %s && %s%s %s > %s 2>&1"
                  (Filename.quote (Filename.dirname in2))
                  env_prefix
                  (Filename.quote (lean_command ()))
                  (Filename.quote in2)
                  (Filename.quote out2)
              in
              ignore (Sys.command cmd2 : int);
              let blocks = parse_grind_used out2 in
              if List.length blocks = List.length vcs
              then List.iteri (fun k names -> Hashtbl.replace used_lemmas k names) blocks)))
;;

(* ------------------------------------------------------------------ *)

let print_pred ppf p = Format.pp_print_string ppf (Refinement.to_string p)

(* A source span as [line.col-line.col]: 1-based lines (matching the header's "Line N"
   convention) and 0-based columns (matching its "characters A-B"). Editor-parsable,
   appended after two spaces. *)
let span_string (loc : Location.t) =
  let s = loc.Location.loc_start
  and e = loc.Location.loc_end in
  Printf.sprintf
    "%d.%d-%d.%d"
    s.Lexing.pos_lnum
    (s.Lexing.pos_cnum - s.Lexing.pos_bol)
    e.Lexing.pos_lnum
    (e.Lexing.pos_cnum - e.Lexing.pos_bol)
;;

(* The provenance suffix for one dumped line: empty unless the provenance flag is on AND a
   span is known, so the plain [-dump-vc] output is byte-identical. *)
let prov_suffix = function
  | Some loc when !Clflags.vox_dump_vc_provenance -> "  @ " ^ span_string loc
  | _ -> ""
;;

(* The VC's variables and mentioned globals with their source type and solver sort, one
   per line as "name : TYPE ~> SORT" -- the editor's context display. Names come from the
   SAME display function the predicates print with, so the rows line up with the
   hypotheses. Emitted only under the provenance flag; plain [-dump-vc] output is
   unchanged. *)
let scope_entries_of_preds ?(extra_ids = []) preds =
  let display = display_fun_of_preds preds in
  let seen = Hashtbl.create 8 in
  let vars =
    List.filter
      (fun id ->
        (not (Hashtbl.mem toplevel_names id))
        &&
        let u = Ident.unique_name id in
        if Hashtbl.mem seen u
        then false
        else (
          Hashtbl.add seen u ();
          true))
      (List.concat_map Refinement.free_vars preds @ extra_ids)
  in
  let var_entry id =
    let oty =
      match Hashtbl.find_opt name_types id with
      | Some t -> t
      | None -> "_"
    in
    let sort =
      match Hashtbl.find_opt name_sorts id with
      | Some ds -> lean_sort ds
      | None -> "VoxU"
    in
    Printf.sprintf
      "%s : %s  ~>  %s%s"
      (display id)
      oty
      sort
      (prov_suffix (Hashtbl.find_opt name_locs id))
  in
  let gseen = Hashtbl.create 4 in
  let gpaths =
    List.filter
      (fun gp ->
        let key = path_uname gp in
        if Hashtbl.mem gseen key
        then false
        else (
          Hashtbl.add gseen key ();
          true))
      (List.concat_map Refinement.free_globals preds)
  in
  let g_entry gp =
    let key = path_uname gp in
    let oty =
      match Hashtbl.find_opt global_types key with
      | Some t -> t
      | None -> "_"
    in
    let sort =
      match Hashtbl.find_opt globals key with
      | Some (_, ds) -> lean_sort ds
      | None -> "VoxU"
    in
    Printf.sprintf "%s : %s  ~>  %s" (Path.name gp) oty sort
  in
  List.map var_entry vars @ List.map g_entry gpaths
;;

let scope_entries vc =
  if not !Clflags.vox_dump_vc_provenance
  then []
  else scope_entries_of_preds (vc.vc_goal :: vc.vc_facts)
;;

(* One [-vox-dump-states] block: same span header, hypothesis and scope formats as the VC
   dump, no goal. *)
(* A fact is MODULE-level when it mentions at least one variable and every variable it
   mentions is a top-level binder: true and usable, but noise at most cursor positions --
   the pane folds these away. *)
let module_level_fact f =
  match Refinement.free_vars f with
  | [] -> false
  | vars -> List.for_all (fun id -> Hashtbl.mem toplevel_names id) vars
;;

let dump_state
  ppf
  (loc, (facts : (Refinement.pred * Location.t option) list), (scope_ids : Ident.t list))
  =
  (* A binding contributes both a selfification and a binder fact with the same predicate;
     show one row, preferring the spanned copy. *)
  let facts =
    List.fold_left
      (fun acc (f, p) ->
        match List.find_opt (fun (f', _) -> Refinement.equal f f') acc with
        | None -> acc @ [ f, p ]
        | Some (_, None) when p <> None ->
          List.map (fun (f', p') -> if Refinement.equal f f' then f', p else f', p') acc
        | Some _ -> acc)
      []
      facts
  in
  let preds = List.map fst facts in
  Refinement.with_var_display (display_fun_of_preds preds)
  @@ fun () ->
  let scope = scope_entries_of_preds ~extra_ids:scope_ids preds in
  Format.fprintf
    ppf
    "@[<v 2>%a: vox state:@ hypotheses:%t%t@]@."
    Location.print_loc
    loc
    (fun ppf ->
      let locals = List.filter (fun (f, _) -> not (module_level_fact f)) facts in
      let mods = List.filter (fun (f, _) -> module_level_fact f) facts in
      if locals = []
      then Format.fprintf ppf " <none>"
      else
        List.iter
          (fun (f, p) -> Format.fprintf ppf "@ %a%s" print_pred f (prov_suffix p))
          locals;
      if mods <> []
      then (
        Format.fprintf ppf "@ module hypotheses:";
        List.iter
          (fun (f, p) -> Format.fprintf ppf "@ %a%s" print_pred f (prov_suffix p))
          mods))
    (fun ppf ->
      if scope <> []
      then (
        Format.fprintf ppf "@ scope:";
        List.iter (fun e -> Format.fprintf ppf "@ %s" e) scope))
;;

let dump_vc ppf ?used ?verdict vc =
  with_vc_display vc
  @@ fun () ->
  let scope = scope_entries vc in
  Format.fprintf
    ppf
    "@[<v 2>%a: vox VC%s:@ goal: %a%s@ hypotheses:%t%t%t%t@]@."
    Location.print_loc
    vc.vc_loc
    (match vc.vc_kind with
     | Prove -> ""
     | Runtime_check -> " (RUNTIME CHECKED)"
     | Assume -> " (ASSUMED)")
    print_pred
    vc.vc_goal
    (prov_suffix vc.vc_goal_prov)
    (fun ppf ->
      let pairs = List.combine vc.vc_facts vc.vc_fact_provs in
      (* The split is an editor affordance: plain [-dump-vc] output stays byte-identical
         (everything under hypotheses:). *)
      let locals, mods =
        if !Clflags.vox_dump_vc_provenance
        then List.partition (fun (f, _) -> not (module_level_fact f)) pairs
        else pairs, []
      in
      if locals = []
      then Format.fprintf ppf " <none>"
      else
        List.iter
          (fun (f, p) -> Format.fprintf ppf "@ %a%s" print_pred f (prov_suffix p))
          locals;
      if mods <> []
      then (
        Format.fprintf ppf "@ module hypotheses:";
        List.iter
          (fun (f, p) -> Format.fprintf ppf "@ %a%s" print_pred f (prov_suffix p))
          mods))
    (fun ppf ->
      if scope <> []
      then (
        Format.fprintf ppf "@ scope:";
        List.iter (fun e -> Format.fprintf ppf "@ %s" e) scope))
    (fun ppf ->
      (* [-vox-explain-proofs] under [-vox-dump-vc-provenance]: the user facts grind used
         to close this VC, or "<arithmetic>" if none. *)
      match used with
      | Some names when !Clflags.vox_dump_vc_provenance ->
        Format.fprintf
          ppf
          "@ used: %s"
          (match names with
           | [] -> "<arithmetic>"
           | _ -> String.concat ", " names)
      | _ -> ())
    (fun ppf ->
      (* On a FAILED solve under [-vox-dump-vc-provenance]: this VC's own verdict, so the
         editor can badge the obligations that still hold when a sibling fails. Absent on
         success (every Prove VC proved) and under plain [-dump-vc]. *)
      match verdict with
      | Some v when !Clflags.vox_dump_vc_provenance ->
        Format.fprintf ppf "@ verdict: %s" v
      | _ -> ())
;;

let discharge () =
  check_imported_datatype_clashes ~render:lean_datatype_decl;
  let all = List.rev !vcs in
  (* A constructor application whose datatype failed to register (the type is not a simple
     variant here, or is mutually recursive) cannot be declared to the solver: such a goal
     is an error, such a fact is dropped (sound). *)
  let pred_usable p =
    List.for_all (fun q -> find_datatype q <> None) (Refinement.constr_paths p)
  in
  let all =
    List.map
      (fun vc ->
        let needs_solver =
          match vc.vc_kind with
          | Prove -> true
          | Runtime_check | Assume -> false
        in
        if needs_solver && not (pred_usable vc.vc_goal)
        then
          Location.raise_errorf
            ~loc:vc.vc_loc
            "vox: this obligation mentions constructors of a type that is not usable \
             here (not a simple variant, or mutually recursive)";
        let kept =
          List.filter
            (fun (f, _) -> pred_usable f)
            (List.combine vc.vc_facts vc.vc_fact_provs)
        in
        { vc with vc_facts = List.map fst kept; vc_fact_provs = List.map snd kept })
      all
  in
  let needs_proof vc =
    match vc.vc_kind with
    | Prove -> true
    | Runtime_check | Assume -> false
  in
  let dump_all () =
    if !Clflags.vox_dump_vc || !Clflags.vox_dump_vc_provenance
    then (
      (* [used_lemmas] is keyed by position among the [Prove] VCs -- the order [run_lean]
         proved them -- so walk [all] with that counter to give each VC its own "used:"
         line. *)
      let prove_idx = ref 0 in
      List.iter
        (fun vc ->
          let used, verdict =
            match vc.vc_kind with
            | Prove ->
              let i = !prove_idx in
              incr prove_idx;
              Hashtbl.find_opt used_lemmas i, Hashtbl.find_opt vc_verdicts i
            | Runtime_check | Assume -> None, None
          in
          dump_vc Format.err_formatter ?used ?verdict vc)
        all);
    if !Clflags.vox_dump_states
    then List.iter (dump_state Format.err_formatter) (List.rev !point_states)
  in
  if !Clflags.vox_dry_run
  then dump_all ()
  else if !Clflags.vox_explain_proofs
  then (
    (* The used-lemma report exists only after the solver runs, so solve BEFORE dumping
       under this flag. A failing solve raises, as in the default order below -- but under
       the provenance dump (editor mode) [run_lean] first records each VC's verdict, so we
       still dump (with those verdicts) before re-raising, letting the editor badge the
       obligations that held. *)
    match run_lean (List.filter needs_proof all) with
    | () -> dump_all ()
    | exception exn ->
      if !Clflags.vox_dump_vc_provenance then dump_all ();
      raise exn)
  else (
    dump_all ();
    run_lean (List.filter needs_proof all))
;;

(* Entry point: called on the final typedtree of an implementation. *)
(* VCs arise only from [refine_]/[assume_] expressions and [refine_] patterns, all of
   which carry a "vox." attribute. Programs without any are skipped entirely: the pass
   must not even inspect (and via [Ctype.expand_head], mutate) the types of unannotated
   programs. *)
let uses_vox (str : structure) =
  (* Applications to contract parameters carry no vox syntax; the type checker flags them
     ([Vox_dep.contract_use_seen]) at the point it strips the parameter refinement, where
     the domain is already being expanded at the correct stage. Read-and-clear per
     unit/phrase. *)
  let contract_use = !Vox_dep.contract_use_seen in
  Vox_dep.contract_use_seen := false;
  let found = ref contract_use in
  let has_vox attrs =
    List.exists
      (fun (a : Parsetree.attribute) ->
        String.length a.attr_name.txt >= 4
        && String.equal (String.sub a.attr_name.txt 0 4) "vox.")
      attrs
  in
  (* A structural (no-expansion) check: a binder can have a refined type with no vox
     syntax of its own (e.g. it was bound to a refined value from another phrase or
     module) and must still contribute facts and be escape-checked. Aliases hiding a
     [Trefine] behind [Tconstr] are missed; expanding here would mutate the types of
     programs that never opted into vox, which this gate exists to prevent. *)
  let type_has_refine ty =
    let refined = ref false in
    Vox_dep.iter_refinement_preds ty (fun ~bound:_ _ -> refined := true);
    !refined
  in
  let it =
    { Tast_iterator.default_iterator with
      expr =
        (fun sub e ->
          if has_vox e.exp_attributes then found := true;
          Tast_iterator.default_iterator.expr sub e)
    ; pat =
        (fun sub (type k) (p : k general_pattern) ->
          if has_vox p.pat_attributes || type_has_refine p.pat_type then found := true;
          Tast_iterator.default_iterator.pat sub p)
    ; value_binding =
        (fun sub vb ->
          (* [total_] bindings need the pass even when no other vox syntax appears (their
             definitions must be registered, translated, and checked). *)
          if has_vox vb.vb_attributes then found := true;
          Tast_iterator.default_iterator.value_binding sub vb)
    }
  in
  it.structure it str;
  (* A phrase (or module) whose only vox content is a [%%vox.lean] block has no vox
     expressions, patterns, or bindings, but must still be walked: at the toplevel a
     prelude-only FIRST phrase would otherwise be skipped and its block silently dropped
     from every later phrase's solver input (the spec functions then elaborate as unbound
     identifiers, failing obligations for the wrong reason). *)
  !found
  || List.exists
       (fun item ->
         match item.str_desc with
         | Tstr_attribute (a : attribute) -> is_vox_block_name a.attr_name.txt
         | _ -> false)
       str.str_items
;;

let walk_items (str : structure) ctx =
  List.iter
    (fun item ->
      match item.str_desc with
      | Tstr_primitive vd -> Vox_reflect.validate_reflect_attr vd.val_attributes
      | Tstr_value (rec_flag, vbs) ->
        List.iter
          (fun vb ->
            Vox_reflect.validate_reflect_attr vb.vb_attributes;
            if Option.is_some (Vox_reflect.reflect_attr_name vb.vb_attributes)
               && Vox_reflect.is_total_binding vb
            then
              Location.raise_errorf
                ~loc:vb.vb_loc
                "vox: a value cannot be both total_ (a translated definition) and \
                 [@@vox.reflect] (an assumed Lean symbol); choose one")
          vbs;
        (match vbs with
         | _ :: _ :: _ when List.exists Vox_reflect.is_total_binding vbs ->
           (* Emission order is definition order, so a group could forward-reference;
              mutual recursion is not supported (matching the datatype restriction). *)
           Location.raise_errorf
             ~loc:(List.hd vbs).vb_loc
             "vox: total_ is not supported on multi-binding groups (mutually recursive \
              reflected functions are not supported)"
         | _ -> ());
        List.iter
          (fun vb ->
            if Vox_reflect.is_total_binding vb then register_spec_def str.str_final_env vb)
          vbs;
        List.iter
          (fun vb ->
            if List.exists
                 (fun (a : Parsetree.attribute) ->
                   String.equal a.attr_name.txt "vox.lemma")
                 vb.vb_attributes
            then register_lemma str.str_final_env vb)
          vbs;
        List.iter (fun vb -> ctx := walk_expr str.str_final_env !ctx vb.vb_expr) vbs;
        ctx
        := List.fold_left
             (fun ctx vb ->
               extend_pat ~toplevel:true ~via_skel:true str.str_final_env ctx vb.vb_pat)
             !ctx
             vbs;
        (match rec_flag with
         | Recursive ->
           (* No self facts for recursive bindings (cyclic constructor equations are
              unsatisfiable in the datatype theory). *)
           ()
         | Nonrecursive ->
           ctx
           := List.fold_left
                (fun ctx vb ->
                  { ctx with
                    cfacts =
                      prov None (binding_self_facts str.str_final_env vb) @ ctx.cfacts
                  })
                !ctx
                vbs)
      | _ ->
        let it =
          { Tast_iterator.default_iterator with
            (* the expression's OWN env, not the top-level structure's: inside a nested
               module, locally declared types (their attributes, constructors, labels) are
               only findable in the inner env -- with the outer env they silently sort at
               VoxU *)
            expr = (fun _ e -> ignore (walk_expr e.exp_env !ctx e : ctx))
          ; pat =
              (fun sub (type k) (p : k general_pattern) ->
                backstop_pat !ctx p;
                Tast_iterator.default_iterator.pat sub p)
          ; value_binding =
              (fun sub vb ->
                reject_local_reflect vb;
                Tast_iterator.default_iterator.value_binding sub vb)
          ; type_declaration =
              (fun sub td ->
                (* eager [@@vox.sort] validation for LOCAL declarations (exported ones are
                   covered by check_signature) *)
                let alias =
                  (match td.typ_kind with
                   | Ttype_abstract -> true
                   | _ -> false)
                  && Option.is_some td.typ_manifest
                in
                validate_vox_sort_attributes ~alias td.typ_attributes;
                Tast_iterator.default_iterator.type_declaration sub td)
          }
        in
        it.structure_item it item)
    str.str_items
;;

(* An interface/implementation pair must agree on [@@vox.sort] for every exported type:
   sorts are computed per-compilation from the VISIBLE declaration, so a mismatch would
   let clients reason at one sort against an implementation verified at another. *)
let check_sort_consistency (str : structure) (sg : Types.signature) =
  List.iter
    (fun (item : Types.signature_item) ->
      match item with
      | Sig_type (id, decl, _, _) ->
        let sig_sort = List.find_map vox_sort_of_attribute decl.type_attributes in
        (match
           Env.find_type_by_name (Longident.Lident (Ident.name id)) str.str_final_env
         with
         | exception Not_found -> ()
         | _, impl_decl ->
           let impl_sort =
             List.find_map vox_sort_of_attribute impl_decl.type_attributes
           in
           (match sig_sort, impl_sort with
            | Some Sa_opaque, None ->
              (* the sealed-abstraction pattern: clients reason at the interface's opaque
                 sort, the implementation at its concrete one. Sound: the concrete sort is
                 one model of the opaque sort, and every interface fact is either a
                 contract checked against the implementation or a block obligation paid at
                 the seal. *)
              ()
            | _ ->
              if not (sig_sort = impl_sort)
              then
                Location.raise_errorf
                  ~loc:impl_decl.type_loc
                  "vox: the vox.sort of type %s differs between the interface and the \
                   implementation; the attribute must appear identically on both \
                   declarations"
                  (Ident.name id)))
      | _ -> ())
    sg
;;

let check_implementation ?intf (str : structure) (sg : Types.signature) =
  (* The signature check is unconditional: a refined type can appear in an exported item
     (a type manifest, an exception, an external) with no vox syntax in any expression,
     and it must still be self-contained. It only reads types structurally, so it cannot
     perturb programs that never use vox. *)
  check_signature sg;
  (* [intf] is the .mli's signature when one exists (the inferred [sg] always agrees with
     the struct trivially). *)
  Option.iter (check_sort_consistency str) intf;
  if not (uses_vox str)
  then ()
  else (
    reset ();
    (* Blocks anywhere in the module are available to all of its VCs (they are emitted, in
       source order, into every solver input); blocks exported by imported units'
       interfaces -- including this unit's own .mli -- come from their .cmis. *)
    embedded_blocks := collect_blocks str;
    register_datatypes_in_blocks str.str_final_env !embedded_blocks;
    imported_specs := gather_imported_specs ();
    let ctx = ref { cfacts = []; cscope = [] } in
    walk_items str ctx;
    discharge ())
;;

(* Toplevel entry point (also the expect-test runner): phrases arrive one at a time, so
   the logical context persists across phrases, mirroring how facts accumulate down the
   items of an implementation. Skipping is per-session rather than per-phrase: once any
   phrase has used vox, later phrases are walked even without vox attributes of their own,
   so that their toplevel binders (which may carry refinements copied from earlier
   phrases) contribute facts. *)
let toplevel_ctx = ref { cfacts = []; cscope = [] }
let toplevel_blocks : (string * Location.t) list ref = ref []

let check_toplevel_phrase
  (str : structure)
  ~(sig_acc : Types.signature)
  (sg : Types.signature)
  =
  (* [sig_acc] (the session's accumulated signature) is re-checked on every phrase: typing
     this phrase can instantiate a weak type variable in an EARLIER phrase's signature
     with a refined type mentioning one of this phrase's variables -- e.g. a stored
     closure in a module-wrapped cell -- which phrase-local checks miss. *)
  check_signature sig_acc;
  check_signature sg;
  if !toplevel_active || uses_vox str
  then (
    toplevel_active := true;
    vcs := [];
    (* The session's committed blocks plus this phrase's; committed (like the facts below)
       only if the phrase discharges. *)
    embedded_blocks := !toplevel_blocks @ collect_blocks str;
    imported_specs := gather_imported_specs ();
    (* Reflected definitions and datatype registrations are committed the same way: a
       failed phrase is backtracked, so its rejected definition must not be re-emitted
       (and re-fail, blamed at the OLD location) by every later phrase, and its datatypes
       must not collide -- at their stamp-free solver-side names -- with the retried
       phrase's. *)
    let saved_spec_defs = !spec_defs in
    let saved_datatypes = !datatypes in
    let saved_poly_heads = !poly_heads in
    let ctx = ref !toplevel_ctx in
    Misc.try_finally
      ~exceptionally:(fun () ->
        spec_defs := saved_spec_defs;
        datatypes := saved_datatypes;
        poly_heads := saved_poly_heads)
      (fun () ->
        register_datatypes_in_blocks str.str_final_env !embedded_blocks;
        walk_items str ctx;
        (* Discharge before committing the phrase's facts: if verification fails, the
           toplevel backtracks the phrase, so its bindings never exist and their facts
           (e.g. a refuted contradictory refinement) must not be available to later
           phrases. *)
        discharge ());
    toplevel_ctx := !ctx;
    toplevel_blocks := !embedded_blocks)
;;
