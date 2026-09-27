open Parsetree
let str s = Printf.sprintf "%S" s
let array f xs = "[" ^ String.concat "," (List.map f xs) ^ "]"
let pair a b = "[" ^ a ^ "," ^ b ^ "]"
let loc l = pair (string_of_int l.Location.loc_start.Lexing.pos_cnum)
  (string_of_int l.Location.loc_end.Lexing.pos_cnum)
let rec lid = function Longident.Lident n -> n | Ldot (l,n) -> lid l.txt ^ "." ^ n.txt
  | Lapply (a,b) -> lid a.txt ^ "(" ^ lid b.txt ^ ")"
let name p =
  let found = ref [] in
  let it = {Ast_iterator.default_iterator with pat = fun self p ->
    (match p.ppat_desc with Ppat_var n -> found := n.txt :: !found | _ -> ());
    Ast_iterator.default_iterator.pat self p} in
  it.pat it p; String.concat "," (List.rev !found)
let modes xs = List.map (fun x -> let Mode n = x.Location.txt in n) xs
let rec proof_type t = match t.ptyp_desc with
  | Ptyp_arrow (_,_,result,_,m,_) -> List.mem "ghost" (modes m) || proof_type result
  | Ptyp_poly (_,t) -> proof_type t
  | Ptyp_refine (_,base,_) -> (match base.ptyp_desc with
    | Ptyp_constr ({txt=Longident.Lident "unit";_},[]) -> true | _ -> proof_type base)
  | Ptyp_constr (n,_) when lid n.txt = "Ghost.t" -> true
  | _ -> false
let proof_constraint = function
  | Some (Pvc_constraint {typ;_}) -> proof_type typ | _ -> false
let rec proof_expr e = match e.pexp_desc with
  | Pexp_constraint (e,t,m) -> List.mem "ghost" (modes m)
    || Option.fold ~none:false ~some:proof_type t || proof_expr e
  | Pexp_function (_,c,body) -> List.mem "ghost" (modes c.mode_annotations)
    || (match c.ret_type_constraint with Some (Pconstraint t) -> proof_type t | _ -> false)
    || (match body with Pfunction_body e -> proof_expr e | _ -> false)
  | Pexp_ghost _ -> true | _ -> false
let rec pattern_total p = match p.ppat_desc with
  | Ppat_constraint (p,_,m) -> List.mem "total" (modes m) || pattern_total p
  | _ -> false
let binding_total b = List.mem "total" (modes b.pvb_modes) || pattern_total b.pvb_pat
let binding_proof b = proof_constraint b.pvb_constraint || proof_expr b.pvb_expr
  || List.mem "ghost" (modes b.pvb_modes)
let inspect file =
  let ic = open_in file in let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf file; let ast = if Filename.check_suffix file ".mli" then `Signature (Parse.interface lexbuf) else `Structure (Parse.implementation lexbuf) in close_in ic;
  Depend.free_structure_names := Depend.String.Set.empty;
  (match ast with `Structure s -> Depend.add_implementation Depend.String.Map.empty s | `Signature s -> Depend.add_signature Depend.String.Map.empty s);
  let dependencies = Depend.String.Set.elements !(Depend.free_structure_names) in
  let declarations = ref [] and aliases = ref [] and includes = ref [] and arguments = ref [] in
  let context = ref [] and opens = ref [] in
  let declaration ?(declared_total=false) kind n location walk attrs proof =
    let refs = ref [] and all_refs = ref [] and proof_spans = ref [] and found_modes = ref [] and type_refs = ref [] and repr_refs = ref [] and defines = ref [] and calls = ref [] in
    let collect skip =
      let locals = ref [] in
      let bound p = String.split_on_char ',' (name p) in
      let scoped names f = let before = !locals in locals := names @ before; f (); locals := before in
      let it = {Ast_iterator.default_iterator with
        modes=(fun _ xs -> found_modes := modes xs @ !found_modes);
        modalities=(fun _ xs -> List.iter (fun x -> let Modality n = x.Location.txt in found_modes := n :: !found_modes) xs);
        type_declaration=(fun self t ->
          (match t.ptype_kind with
           | Ptype_variant cs -> List.iter (fun c -> defines := c.pcd_name.txt :: !defines) cs
           | Ptype_record ls -> List.iter (fun l -> defines := l.pld_name.txt :: !defines) ls
           | _ -> ()); Ast_iterator.default_iterator.type_declaration self t);
        pat=(fun self p ->
          (match p.ppat_desc with
           | Ppat_construct (x,_) when skip -> repr_refs := lid x.Location.txt :: !repr_refs
           | Ppat_record (xs,_) when skip -> List.iter (fun (x,_) -> repr_refs:=lid x.Location.txt::!repr_refs) xs
           | _ -> ()); Ast_iterator.default_iterator.pat self p);
        case=(fun self c -> scoped (bound c.pc_lhs) (fun () -> Ast_iterator.default_iterator.case self c));
        expr=(fun self e ->
          (match e.pexp_desc with Pexp_apply ({pexp_desc=Pexp_ident n;_},_) when skip ->
            calls := pair (str (lid n.Location.txt)) (loc e.pexp_loc) :: !calls | _ -> ());
          (match e.pexp_desc with Pexp_ident x when not (List.mem (lid x.Location.txt) !locals) ->
            let r = pair (str (lid x.Location.txt)) (loc e.pexp_loc) in
            if skip then refs := r :: !refs else all_refs := r :: !all_refs
          | Pexp_construct (x,_) when skip -> repr_refs := lid x.Location.txt :: !repr_refs
          | Pexp_field (_,x) when skip -> repr_refs := lid x.Location.txt :: !repr_refs
          | Pexp_record (xs,_) when skip -> List.iter (fun (x,_) -> repr_refs:=lid x.Location.txt::!repr_refs) xs
          | Pexp_ghost _ -> if skip then proof_spans := loc e.pexp_loc :: !proof_spans
          | _ -> ());
          match e.pexp_desc with Pexp_ghost _ when skip -> ()
          | Pexp_let (_,recursive,bs,body) ->
            let names = List.concat_map (fun b -> bound b.pvb_pat) bs in
            (match recursive with Asttypes.Recursive -> scoped names (fun () -> List.iter (self.value_binding self) bs)
             | Nonrecursive -> List.iter (self.value_binding self) bs);
            scoped names (fun () -> self.expr self body)
          | Pexp_function (params,_,_) ->
            let names = List.concat_map (fun p -> match p.pparam_desc with
              | Pparam_val (_,_,p) -> bound p | _ -> []) params in
            scoped names (fun () -> Ast_iterator.default_iterator.expr self e)
          | _ -> Ast_iterator.default_iterator.expr self e);
        typ=(fun self t ->
          (match t.ptyp_desc with Ptyp_constr (x,_) when skip -> type_refs := lid x.Location.txt :: !type_refs | _ -> ());
          match t.ptyp_desc with
          | Ptyp_refine (_,base,_) when skip ->
            proof_spans := loc t.ptyp_loc :: !proof_spans; self.typ self base
          | _ -> Ast_iterator.default_iterator.typ self t);
        attribute=(fun _ _ -> ())} in walk it
    in collect true; collect false;
    declarations := Printf.sprintf
      "{\"declared_total\":%b,\"name\":%s,\"context\":%s,\"opens\":%s,\"kind\":%s,\"span\":%s,\"proof_signature\":%b,\"total\":%b,\"attributes\":%s,\"refs\":%s,\"all_refs\":%s,\"proof_spans\":%s,\"type_refs\":%s,\"repr_refs\":%s,\"defines\":%s,\"calls\":%s}"
      declared_total (str n) (array str !context) (array str !opens) (str kind) (loc location) proof (List.mem "total" !found_modes)
      (array str attrs) (array Fun.id (List.rev !refs)) (array Fun.id (List.rev !all_refs))
      (array Fun.id (List.rev !proof_spans)) (array str (List.sort_uniq String.compare !type_refs)) (array str (List.sort_uniq String.compare !repr_refs)) (array str (List.sort_uniq String.compare !defines)) (array Fun.id (List.rev !calls)) :: !declarations
  in
  let rec module_head e = match e.pmod_desc with
    | Pmod_ident x -> Some (lid x.Location.txt)
    | Pmod_constraint (e,_,_) | Pmod_apply_unit e -> module_head e
    | Pmod_apply (f,a) ->
      (match module_head a with Some n -> arguments:=n::!arguments | None -> ()); module_head f
    | _ -> None
  in
  let rec structure items = List.iter item items
  and module_expr e = match e.pmod_desc with
    | Pmod_structure s -> structure s
    | Pmod_functor (_,e) | Pmod_constraint (e,_,_) -> module_expr e
    | Pmod_apply (a,b) -> module_expr a; module_expr b | _ -> ()
  and module_binding m =
    let n = Option.value ~default:"_" m.pmb_name.txt in
    (match module_head m.pmb_expr with Some target ->
      aliases := pair (str (String.concat "." (!context@[n]))) (str target) :: !aliases
    | None -> ());
    let before = !context and before_opens = !opens in
    context := before@[n]; module_expr m.pmb_expr; context := before; opens := before_opens
  and item s = match s.pstr_desc with
    | Pstr_value (_,bs) -> List.iter (fun b ->
      declaration ~declared_total:(binding_total b) "value" (name b.pvb_pat) b.pvb_loc (fun it -> it.value_binding it b)
        (List.map (fun a -> a.attr_name.txt) b.pvb_attributes)
        (binding_proof b)) bs
    | Pstr_type (_,ts) -> List.iter (fun t ->
      declaration "type" t.ptype_name.txt t.ptype_loc (fun it -> it.type_declaration it t) [] false) ts
    | Pstr_primitive v -> declaration "external" v.pval_name.txt s.pstr_loc
        (fun it -> it.value_description it v) [] (proof_type v.pval_type)
    | Pstr_eval (e,_) -> declaration "initialization" "<init>" s.pstr_loc
        (fun it -> it.expr it e) [] (proof_expr e)
    | Pstr_module m -> module_binding m
    | Pstr_recmodule ms -> List.iter module_binding ms
    | Pstr_include i -> (match module_head i.pincl_mod with
      | Some target -> includes := pair (str (String.concat "." !context)) (str target) :: !includes
      | None -> module_expr i.pincl_mod)
    | Pstr_open o -> (match o.popen_expr.pmod_desc with Pmod_ident x -> opens:=lid x.Location.txt::!opens | _ -> ())
    | _ -> ()
  and signature items = List.iter (fun s -> match s.psig_desc with
    | Psig_value v -> declaration "signature" v.pval_name.txt s.psig_loc
      (fun it -> it.value_description it v) [] (proof_type v.pval_type)
    | Psig_type (_,ts) -> List.iter (fun t -> declaration "type" t.ptype_name.txt t.ptype_loc
      (fun it -> it.type_declaration it t) [] false) ts
    | Psig_module m ->
      let before = !context in context := before@[Option.value ~default:"_" m.pmd_name.txt];
      module_type m.pmd_type; context:=before
    | _ -> ()) items.psg_items
  and module_type t = match t.pmty_desc with
    | Pmty_signature s -> signature s
    | Pmty_functor (_,t,_) | Pmty_with (t,_) -> module_type t
    | _ -> ()
  in (match ast with `Structure s -> structure s | `Signature s -> signature s);
  let explicit_proof_spans = ref [] in
  let regions = {Ast_iterator.default_iterator with
    expr=(fun self e -> (match e.pexp_desc with Pexp_ghost _ ->
      explicit_proof_spans := loc e.pexp_loc :: !explicit_proof_spans | _ -> ());
      Ast_iterator.default_iterator.expr self e);
    typ=(fun self t -> (match t.ptyp_desc with Ptyp_refine _ ->
      explicit_proof_spans := loc t.ptyp_loc :: !explicit_proof_spans | _ -> ());
      Ast_iterator.default_iterator.typ self t);
    attribute=(fun _ _ -> ())} in
  (match ast with `Structure s -> regions.structure regions s
    | `Signature s -> regions.signature regions s);
  Printf.printf "{\"file\":%s,\"dependencies\":%s,\"aliases\":%s,\"includes\":%s,\"module_arguments\":%s,\"explicit_proof_spans\":%s,\"declarations\":%s}\n%!"
    (str file) (array str dependencies) (array Fun.id !aliases) (array Fun.id !includes) (array str !arguments) (array Fun.id !explicit_proof_spans) (array Fun.id (List.rev !declarations))
let () = List.iter (fun f -> try inspect f with exn ->
  Printf.eprintf "Failed: %s\n" f; Location.report_exception Format.err_formatter exn; exit 1)
  (List.tl (Array.to_list Sys.argv))
