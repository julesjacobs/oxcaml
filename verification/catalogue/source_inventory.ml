(* The syntax and name resolution that the catalogue's line counts need (see
   line_stats.py).

   source_inventory FILE.ml|FILE.mli ... parses each source and prints one JSON
   line per file: the modules it mentions, its declarations, and the byte ranges
   that erasure removes or that only the verifier reads (proof regions).

   source_inventory -typed FILE.cmt|FILE.cmti ... reads typed trees, compiled
   with -bin-annot, and prints one JSON line per unit: where each name is bound,
   the module aliases, functor applications and includes, and every reference to
   a value, type or exception with the byte range it occurs at. line_stats.py
   decides from the proof regions whether a reference is made at run time. *)

let str s = Printf.sprintf "%S" s

let array f xs = "[" ^ String.concat "," (List.map f xs) ^ "]"

let span (l : Location.t) =
  Printf.sprintf "[%d,%d]" l.loc_start.pos_cnum l.loc_end.pos_cnum

(* Parsed sources *)

open Parsetree

let rec lid = function
  | Longident.Lident n -> n
  | Ldot (l, n) -> lid l.txt ^ "." ^ n.txt
  | Lapply (a, b) -> lid a.txt ^ "(" ^ lid b.txt ^ ")"

let pattern_names p =
  let found = ref [] in
  let it =
    { Ast_iterator.default_iterator with
      pat =
        (fun self p ->
          (match p.ppat_desc with
          | Ppat_var n -> found := n.txt :: !found
          | _ -> ());
          Ast_iterator.default_iterator.pat self p)
    }
  in
  it.pat it p;
  String.concat "," (List.rev !found)

let modes xs =
  List.map
    (fun x ->
      let (Mode n) = x.Location.txt in
      n)
    xs

let ghost_modalities xs =
  List.exists
    (fun x ->
      let (Modality n) = x.Location.txt in
      n = "ghost")
    xs

let is_unit t =
  match t.ptyp_desc with
  | Ptyp_constr ({ txt = Longident.Lident "unit"; _ }, []) -> true
  | _ -> false

(* A type whose values carry no run-time information: a refined [unit], a ghost
   result, or [Ghost.t]. *)
let rec proof_type t =
  match t.ptyp_desc with
  | Ptyp_arrow (_, _, result, _, m, _) ->
    List.mem "ghost" (modes m) || proof_type result
  | Ptyp_poly (_, t) -> proof_type t
  | Ptyp_refine (_, base, _) -> is_unit base || proof_type base
  | Ptyp_constr (n, _) when lid n.txt = "Ghost.t" -> true
  | _ -> false

let proof_constraint = function
  | Some (Pvc_constraint { typ; _ }) -> proof_type typ
  | _ -> false

let rec proof_expr e =
  match e.pexp_desc with
  | Pexp_constraint (e, t, m) ->
    List.mem "ghost" (modes m)
    || Option.fold ~none:false ~some:proof_type t
    || proof_expr e
  | Pexp_function (_, c, body) -> (
    List.mem "ghost" (modes c.mode_annotations)
    || (match c.ret_type_constraint with
      | Some (Pconstraint t) -> proof_type t
      | _ -> false)
    || match body with Pfunction_body e -> proof_expr e | _ -> false)
  | Pexp_ghost _ -> true
  | _ -> false

(* The value computed is erased: after its parameters, the body is [ghost_
   e]. *)
let rec erased_body e =
  match e.pexp_desc with
  | Pexp_ghost _ -> true
  | Pexp_function (_, _, Pfunction_body e)
  | Pexp_constraint (e, _, _)
  | Pexp_newtype (_, _, e) ->
    erased_body e
  | _ -> false

let rec pattern_total p =
  match p.ppat_desc with
  | Ppat_constraint (p, _, m) -> List.mem "total" (modes m) || pattern_total p
  | _ -> false

let binding_total b =
  List.mem "total" (modes b.pvb_modes) || pattern_total b.pvb_pat

let rec pattern_ghost p =
  match p.ppat_desc with
  | Ppat_constraint (p, _, m) -> List.mem "ghost" (modes m) || pattern_ghost p
  | _ -> false

let binding_ghost b =
  List.mem "ghost" (modes b.pvb_modes) || pattern_ghost b.pvb_pat

let binding_proof b =
  proof_constraint b.pvb_constraint || proof_expr b.pvb_expr || binding_ghost b

(* A lemma: a total function whose result carries no run-time information.
   line_stats.py exempts the few that run (catalogue.json's runtime_units). *)
let binding_lemma b = binding_total b && binding_proof b

(* A type or pattern whose value is erased: [Ghost.t], a refined [unit], or
   anything at mode [ghost]. *)
let ghost_head t =
  match t.ptyp_desc with
  | Ptyp_constr (n, _) -> lid n.txt = "Ghost.t"
  | _ -> false

let erased_type t =
  match t.ptyp_desc with
  | Ptyp_refine (_, base, _) -> is_unit base
  | _ -> ghost_head t

let rec erased_pattern p =
  match p.ppat_desc with
  | Ppat_constraint (p, t, m) ->
    List.mem "ghost" (modes m)
    || Option.fold ~none:false ~some:erased_type t
    || erased_pattern p
  | _ -> false

(* A record type whose fields are all ghost has no run-time value. *)
let ghost_record t =
  match t.ptype_kind with
  | Ptype_record (_ :: _ as ls) ->
    List.for_all (fun l -> ghost_modalities l.pld_modalities) ls
  | _ -> ( match t.ptype_manifest with Some m -> ghost_head m | None -> false)

(* A local binding that computes nothing at run time. *)
let erased_binding b =
  binding_lemma b || binding_ghost b || erased_body b.pvb_expr
  || erased_pattern b.pvb_pat
  || (match b.pvb_constraint with
    | Some (Pvc_constraint { typ; _ }) -> ghost_head typ
    | _ -> false)
  (* [let x : {x : t | p} = y in]: a refinement of a value already computed. *)
  || (match b.pvb_constraint, b.pvb_expr.pexp_desc with
    | ( Some (Pvc_constraint { typ = { ptyp_desc = Ptyp_refine _; _ }; _ }),
        Pexp_ident _ ) ->
      true
    | _ -> false)
  ||
  match b.pvb_expr.pexp_desc with
  | Pexp_record ((_ :: _ as fields), None) ->
    List.for_all
      (fun (_, e) -> match e.pexp_desc with Pexp_ghost _ -> true | _ -> false)
      fields
  | _ -> false

let inspect file =
  let ic = open_in_bin file in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf file;
  let ast =
    if Filename.check_suffix file ".mli"
    then `Signature (Parse.interface lexbuf)
    else `Structure (Parse.implementation lexbuf)
  in
  close_in ic;
  Depend.free_structure_names := Depend.String.Set.empty;
  (match ast with
  | `Structure s -> Depend.add_implementation Depend.String.Map.empty s
  | `Signature s -> Depend.add_signature Depend.String.Map.empty s);
  let dependencies = Depend.String.Set.elements !Depend.free_structure_names in
  (* Proof regions. A refinement [{x : t | p}] is proof except for its payload
     type [t], which is the run-time type, unless [t] is [unit]. *)
  let proof = ref [] and measures = ref [] and weak = ref [] in
  let add l = proof := span l :: !proof in
  let add_range a b =
    if b > a then proof := Printf.sprintf "[%d,%d]" a b :: !proof
  in
  let regions =
    { Ast_iterator.default_iterator with
      expr =
        (fun self e ->
          (match e.pexp_desc with
          | Pexp_ghost _ -> add e.pexp_loc
          | Pexp_let (_, _, bs, body) ->
            List.iter (fun b -> if erased_binding b then add b.pvb_loc) bs;
            (* [let x = ghost_ e in]: the keywords too. *)
            if List.for_all erased_binding bs
            then
              add_range e.pexp_loc.loc_start.pos_cnum
                body.pexp_loc.loc_start.pos_cnum
          | _ -> ());
          Ast_iterator.default_iterator.expr self e);
      typ =
        (fun self t ->
          (match t.ptyp_desc with
          | Ptyp_refine (_, base, _) when is_unit base -> add t.ptyp_loc
          | Ptyp_constr _ when ghost_head t -> add t.ptyp_loc
          (* An erased parameter, with its arrow. *)
          | Ptyp_arrow (_, arg, result, m, _, _)
            when erased_type arg || List.mem "ghost" (modes m) ->
            add_range t.ptyp_loc.loc_start.pos_cnum
              result.ptyp_loc.loc_start.pos_cnum
          | Ptyp_refine (_, base, _) ->
            add_range t.ptyp_loc.loc_start.pos_cnum
              base.ptyp_loc.loc_start.pos_cnum;
            add_range base.ptyp_loc.loc_end.pos_cnum t.ptyp_loc.loc_end.pos_cnum
          | _ -> ());
          Ast_iterator.default_iterator.typ self t);
      (* Mode annotations are type information, like punctuation. *)
      modes =
        (fun _ xs ->
          List.iter (fun x -> weak := span x.Location.loc :: !weak) xs);
      modalities =
        (fun _ xs ->
          List.iter (fun x -> weak := span x.Location.loc :: !weak) xs);
      pat =
        (fun self p ->
          if erased_pattern p then add p.ppat_loc;
          Ast_iterator.default_iterator.pat self p);
      label_declaration =
        (fun self l ->
          if ghost_modalities l.pld_modalities then add l.pld_loc;
          Ast_iterator.default_iterator.label_declaration self l);
      constructor_declaration =
        (fun self c ->
          (match c.pcd_args with
          | Pcstr_tuple args ->
            List.iter
              (fun a -> if ghost_modalities a.pca_modalities then add a.pca_loc)
              args
          | Pcstr_record _ -> ());
          Ast_iterator.default_iterator.constructor_declaration self c);
      (* Termination measures, and the names they mention. *)
      attribute =
        (fun _ a ->
          if a.attr_name.txt = "decreases"
          then begin
            add a.attr_loc;
            let names =
              { Ast_iterator.default_iterator with
                expr =
                  (fun self e ->
                    (match e.pexp_desc with
                    | Pexp_ident x ->
                      measures
                        := Printf.sprintf "[%s,%d]"
                             (str (lid x.txt))
                             e.pexp_loc.loc_start.pos_cnum
                           :: !measures
                    | _ -> ());
                    Ast_iterator.default_iterator.expr self e)
              }
            in
            names.payload names a.attr_payload
          end)
    }
  in
  (match ast with
  | `Structure s -> regions.structure regions s
  | `Signature s -> regions.signature regions s);
  (* Declarations and the modules that contain them. *)
  let declarations = ref [] and modules = ref [] and context = ref [] in
  let declaration ?(lemma = false) ?(ghost = false) ?(erased = false) kind name
      l =
    declarations
      := Printf.sprintf
           "{\"kind\":%s,\"name\":%s,\"context\":%s,\"span\":%s,\"lemma\":%b,\"ghost\":%b,\"erased_body\":%b}"
           (str kind) (str name) (array str !context) (span l) lemma ghost
           erased
         :: !declarations
  in
  let rec structure items = List.iter item items
  and module_expr e =
    match e.pmod_desc with
    | Pmod_structure s -> structure s
    | Pmod_functor (_, e) | Pmod_constraint (e, _, _) -> module_expr e
    | Pmod_apply (a, b) ->
      module_expr a;
      module_expr b
    | _ -> ()
  and module_binding m =
    let before = !context in
    context := before @ [Option.value ~default:"_" m.pmb_name.txt];
    modules
      := Printf.sprintf "[%s,%s]" (array str !context) (span m.pmb_loc)
         :: !modules;
    module_expr m.pmb_expr;
    context := before
  and item s =
    match s.pstr_desc with
    | Pstr_value (_, bs) ->
      (* A binding owns the keywords before it ([let], [and], [rec]). *)
      let start = ref s.pstr_loc.loc_start in
      List.iter
        (fun b ->
          let l = { b.pvb_loc with loc_start = !start } in
          start := b.pvb_loc.loc_end;
          declaration ~lemma:(binding_lemma b) ~ghost:(binding_ghost b)
            ~erased:(erased_body b.pvb_expr) "value" (pattern_names b.pvb_pat) l)
        bs
    | Pstr_type (_, ts) ->
      let start = ref s.pstr_loc.loc_start in
      List.iter
        (fun t ->
          declaration ~ghost:(ghost_record t) "type" t.ptype_name.txt
            { t.ptype_loc with loc_start = !start };
          start := t.ptype_loc.loc_end)
        ts
    | Pstr_primitive v -> declaration "external" v.pval_name.txt s.pstr_loc
    | Pstr_exception e ->
      declaration "exception" e.ptyexn_constructor.pext_name.txt s.pstr_loc
    | Pstr_typext t ->
      List.iter
        (fun c -> declaration "exception" c.pext_name.txt c.pext_loc)
        t.ptyext_constructors
    | Pstr_eval (e, _) ->
      declaration ~erased:(erased_body e) "initialization" "" s.pstr_loc
    | Pstr_module m -> module_binding m
    | Pstr_recmodule ms -> List.iter module_binding ms
    | Pstr_include i -> module_expr i.pincl_mod
    | _ -> ()
  and signature items =
    List.iter
      (fun s ->
        match s.psig_desc with
        | Psig_value v ->
          declaration ~ghost:(proof_type v.pval_type) "signature"
            v.pval_name.txt s.psig_loc
        | Psig_type (_, ts) ->
          List.iter
            (fun t -> declaration "type" t.ptype_name.txt t.ptype_loc)
            ts
        | Psig_module m ->
          let before = !context in
          context := before @ [Option.value ~default:"_" m.pmd_name.txt];
          modules
            := Printf.sprintf "[%s,%s]" (array str !context) (span m.pmd_loc)
               :: !modules;
          module_type m.pmd_type;
          context := before
        | _ -> ())
      items.psg_items
  and module_type t =
    match t.pmty_desc with
    | Pmty_signature s -> signature s
    | Pmty_functor (_, t, _) | Pmty_with (t, _) -> module_type t
    | _ -> ()
  in
  (match ast with `Structure s -> structure s | `Signature s -> signature s);
  Printf.printf
    "{\"file\":%s,\"dependencies\":%s,\"declarations\":%s,\"modules\":%s,\"proof_spans\":%s,\"weak_spans\":%s,\"measure_refs\":%s}\n\
     %!"
    (str file) (array str dependencies)
    (array Fun.id (List.rev !declarations))
    (array Fun.id (List.rev !modules))
    (array Fun.id (List.rev !proof))
    (array Fun.id (List.rev !weak))
    (array Fun.id (List.rev !measures))

(* Typed trees *)

open Typedtree

let typed file =
  let cmt = Cmt_format.read_cmt file in
  let unit = Compilation_unit.name_as_string cmt.cmt_modname in
  let keys = Hashtbl.create 256 in
  let bindings = ref []
  and aliases = ref []
  and instances = ref []
  and includes = ref []
  and params = ref []
  and refs = ref [] in
  let bind kind id key (l : Location.t) =
    Hashtbl.replace keys (Ident.unique_name id) key;
    bindings
      := Printf.sprintf "[%s,%s,%s]" (str kind) (str key) (span l) :: !bindings
  in
  (* A path as a key: "Unit.M.x", with local modules named by where they are
     bound. [None] for names local to an expression. *)
  let rec key = function
    | Path.Pident id when Ident.is_global id -> Some (Ident.name id)
    | Pident id -> Hashtbl.find_opt keys (Ident.unique_name id)
    | Pdot (p, s) -> Option.map (fun k -> k ^ "." ^ s) (key p)
    | Papply (f, _) | Pextra_ty (f, _) -> key f
  in
  let reference kind path (l : Location.t) =
    match key path with
    | Some k ->
      refs
        := Printf.sprintf "[%s,%s,%d,%d]" (str kind) (str k)
             l.loc_start.pos_cnum l.loc_end.pos_cnum
           :: !refs
    | None -> ()
  in
  let type_of_constructor (c : Data_types.constructor_description) l =
    (match c.cstr_tag with Extension p -> reference "c" p l | _ -> ());
    match Data_types.cstr_res_type_path c with
    | p -> reference "t" p l
    | exception _ -> ()
  in
  let type_of_label (d : _ Data_types.gen_label_description) l =
    match Data_types.gen_lbl_res_type_path d with
    | p -> reference "t" p l
    | exception _ -> ()
  in
  let rec head m =
    match m.mod_desc with
    | Tmod_ident (p, _) -> key p
    | Tmod_constraint (m, _, _, _) | Tmod_apply_unit (m, _) -> head m
    | Tmod_apply (f, _, _, _, _) -> head f
    | _ -> None
  in
  let iterator =
    { Tast_iterator.default_iterator with
      expr =
        (fun self e ->
          (match e.exp_desc with
          | Texp_ident { path; _ } -> reference "v" path e.exp_loc
          | Texp_construct (_, c, _, _, _) -> type_of_constructor c e.exp_loc
          | Texp_record { fields; _ } ->
            Array.iter (fun (d, _, _) -> type_of_label d e.exp_loc) fields
          | Texp_field { label; _ } -> type_of_label label e.exp_loc
          | Texp_setfield { label; _ } -> type_of_label label e.exp_loc
          (* [let module M = N in]: M names N. *)
          | Texp_letmodule (Some id, _, _, m, _) ->
            Option.iter
              (fun k -> Hashtbl.replace keys (Ident.unique_name id) k)
              (head m)
          | _ -> ());
          Tast_iterator.default_iterator.expr self e);
      pat =
        (fun (type k) self (p : k general_pattern) ->
          (match p.pat_desc with
          | Tpat_construct (_, c, _, _, _) -> type_of_constructor c p.pat_loc
          | Tpat_record (fields, _, _) ->
            List.iter (fun (_, d, _) -> type_of_label d p.pat_loc) fields
          | _ -> ());
          Tast_iterator.default_iterator.pat self p);
      typ =
        (fun self t ->
          (match t.ctyp_desc with
          | Ttyp_constr (p, _, _) -> reference "t" p t.ctyp_loc
          | _ -> ());
          Tast_iterator.default_iterator.typ self t);
      (* Nested modules are walked by [structure] below, so that their names are
         bound first. *)
      module_expr = (fun _ _ -> ());
      module_type = (fun _ _ -> ())
    }
  in
  let rec arguments m =
    match m.mod_desc with
    | Tmod_apply (f, a, _, _, _) -> arguments f @ [head a]
    | Tmod_apply_unit (m, _) -> arguments m @ [None]
    | Tmod_constraint (m, _, _, _) -> arguments m
    | _ -> []
  in
  let rec is_apply m =
    match m.mod_desc with
    | Tmod_apply _ | Tmod_apply_unit _ -> true
    | Tmod_constraint (m, _, _, _) -> is_apply m
    | _ -> false
  in
  let included = ref 0 in
  let rec module_expr ?(position = 0) context m =
    match m.mod_desc with
    | Tmod_structure s -> structure context s
    | Tmod_ident (p, _) ->
      Option.iter
        (fun t ->
          aliases := Printf.sprintf "[%s,%s]" (str context) (str t) :: !aliases)
        (key p)
    | Tmod_functor (Named (Some id, _, _, _), body, _) ->
      let p = context ^ ".%" ^ string_of_int position ^ "_" ^ Ident.name id in
      Hashtbl.replace keys (Ident.unique_name id) p;
      params
        := Printf.sprintf "[%s,%s,%d]" (str p) (str context) position :: !params;
      module_expr ~position:(position + 1) context body
    | Tmod_functor (_, body, _) ->
      module_expr ~position:(position + 1) context body
    | Tmod_constraint (m, _, _, _) -> module_expr ~position context m
    | Tmod_apply _ | Tmod_apply_unit _ ->
      Option.iter
        (fun f ->
          instances
            := Printf.sprintf "[%s,%s,%s]" (str context) (str f)
                 (array
                    (function None -> "null" | Some k -> str k)
                    (arguments m))
               :: !instances)
        (head m)
    | Tmod_unpack _ -> ()
  and signature_names context (s : Types.signature) =
    List.iter
      (function
        | Types.Sig_value (id, _, _)
        | Sig_type (id, _, _, _)
        | Sig_typext (id, _, _, _)
        | Sig_module (id, _, _, _, _) ->
          Hashtbl.replace keys (Ident.unique_name id)
            (context ^ "." ^ Ident.name id)
        | _ -> ())
      s
  and structure context s = List.iter (item context) s.str_items
  and item context s =
    let walk f = f iterator in
    match s.str_desc with
    | Tstr_value (_, vbs) ->
      List.iter
        (fun vb ->
          List.iter
            (fun id ->
              bind "v" id (context ^ "." ^ Ident.name id) vb.vb_pat.pat_loc)
            (pat_bound_idents vb.vb_pat))
        vbs;
      walk (fun it -> List.iter (it.value_binding it) vbs)
    | Tstr_primitive v ->
      bind "v" v.val_id (context ^ "." ^ Ident.name v.val_id) v.val_loc;
      walk (fun it -> it.value_description it v)
    | Tstr_type (_, ts) ->
      List.iter
        (fun t ->
          bind "t" t.typ_id (context ^ "." ^ Ident.name t.typ_id) t.typ_loc)
        ts;
      walk (fun it -> List.iter (it.type_declaration it) ts)
    | Tstr_exception e ->
      let c = e.tyexn_constructor in
      bind "c" c.ext_id (context ^ "." ^ Ident.name c.ext_id) c.ext_loc;
      walk (fun it -> it.type_exception it e)
    | Tstr_typext t ->
      List.iter
        (fun c ->
          bind "c" c.ext_id (context ^ "." ^ Ident.name c.ext_id) c.ext_loc)
        t.tyext_constructors;
      walk (fun it -> it.type_extension it t)
    | Tstr_eval (e, _, _) -> walk (fun it -> it.expr it e)
    | Tstr_module mb -> module_binding context mb
    | Tstr_recmodule mbs -> List.iter (module_binding context) mbs
    | Tstr_include i ->
      (match i.incl_mod.mod_desc with
      | Tmod_structure s -> structure context s
      | _ ->
        let target =
          if is_apply i.incl_mod
          then begin
            incr included;
            let k = Printf.sprintf "%s.%%include%d" context !included in
            module_expr k i.incl_mod;
            Some k
          end
          else head i.incl_mod
        in
        Option.iter
          (fun t ->
            includes
              := Printf.sprintf "[%s,%s]" (str context) (str t) :: !includes)
          target);
      signature_names context i.incl_type
    | _ -> ()
  and module_binding context mb =
    let k = context ^ "." ^ Option.value ~default:"_" mb.mb_name.txt in
    Option.iter (fun id -> bind "m" id k mb.mb_loc) mb.mb_id;
    module_expr k mb.mb_expr
  in
  let rec signature context s =
    List.iter
      (fun (i : signature_item) ->
        match i.sig_desc with
        | Tsig_value v ->
          bind "v" v.val_id (context ^ "." ^ Ident.name v.val_id) v.val_loc;
          iterator.value_description iterator v
        | Tsig_type (_, ts) ->
          List.iter
            (fun t ->
              bind "t" t.typ_id (context ^ "." ^ Ident.name t.typ_id) t.typ_loc)
            ts;
          List.iter (iterator.type_declaration iterator) ts
        | Tsig_modsubst m ->
          Option.iter
            (fun t -> Hashtbl.replace keys (Ident.unique_name m.ms_id) t)
            (key m.ms_manifest)
        | Tsig_module m -> (
          let k = context ^ "." ^ Option.value ~default:"_" m.md_name.txt in
          let rec body position t =
            match t.mty_desc with
            | Tmty_signature s -> signature k s
            | Tmty_functor (Named (Some id, _, _, _), t, _) ->
              let p = k ^ ".%" ^ string_of_int position ^ "_" ^ Ident.name id in
              Hashtbl.replace keys (Ident.unique_name id) p;
              params
                := Printf.sprintf "[%s,%s,%d]" (str p) (str k) position
                   :: !params;
              body (position + 1) t
            | Tmty_functor (_, t, _) -> body (position + 1) t
            | Tmty_with (t, _) | Tmty_strengthen (t, _, _) -> body position t
            (* A named module type: everything the implementation defines is
               exported. *)
            | _ -> bind "export" (Ident.create_local "_") k m.md_loc
          in
          match m.md_type.mty_desc with
          | Tmty_alias (p, _) ->
            Option.iter
              (fun id ->
                Hashtbl.replace keys (Ident.unique_name id)
                  (Option.value ~default:k (key p)))
              m.md_id
          | _ ->
            Option.iter (fun id -> bind "m" id k m.md_loc) m.md_id;
            body 0 m.md_type)
        | Tsig_include _ ->
          bind "export" (Ident.create_local "_") context i.sig_loc
        | _ -> ())
      s.sig_items
  in
  (match cmt.cmt_annots with
  | Implementation s -> structure unit s
  | Interface s -> signature unit s
  | _ -> ());
  Printf.printf
    "{\"file\":%s,\"unit\":%s,\"bindings\":%s,\"aliases\":%s,\"instances\":%s,\"includes\":%s,\"params\":%s,\"refs\":%s}\n\
     %!"
    (str file) (str unit)
    (array Fun.id (List.rev !bindings))
    (array Fun.id (List.rev !aliases))
    (array Fun.id (List.rev !instances))
    (array Fun.id (List.rev !includes))
    (array Fun.id (List.rev !params))
    (array Fun.id (List.rev !refs))

let () =
  let run f file =
    try f file
    with exn ->
      Printf.eprintf "Failed: %s\n" file;
      Location.report_exception Format.err_formatter exn;
      exit 1
  in
  match List.tl (Array.to_list Sys.argv) with
  | "-typed" :: files -> List.iter (run typed) files
  | files -> List.iter (run inspect) files
