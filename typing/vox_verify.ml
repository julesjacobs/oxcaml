(* vox: verification-condition generation and discharge.

   Runs as a separate pass over the FINAL typedtree (the type checker emits no VCs; it
   backtracks internally). Walks the tree carrying a logical environment of facts; each
   [refine_] node yields the VC [facts |- p[v := name of e]]; [assume_] is reported as
   ASSUMED. Facts come from exactly three places (DESIGN.md): unpacking / binders of
   refined type, path facts from [if], and (v1) dependent application.

   VCs are discharged by a Z3 subprocess over SMT-LIB2. Solver error, unknown, and timeout
   all count as verification FAILURE. *)

open Types
open Typedtree

type vc =
  { vc_loc : Location.t
  ; vc_facts : Refinement.pred list (* Pbound-free *)
  ; vc_goal : Refinement.pred (* Pbound-free *)
  ; vc_assumed : bool
  }

(* Declaration sorts for logical names, per DESIGN.md: int as Int, bool as Bool, anything
   else at a single uninterpreted sort. *)
type dsort =
  | S_int
  | S_bool
  | S_other

let vcs : vc list ref = ref []
let name_sorts : (Ident.t, dsort) Hashtbl.t = Hashtbl.create 64

let reset () =
  vcs := [];
  Hashtbl.reset name_sorts
;;

(* Expansion can fail on exotic types (e.g. stage errors inside quotations); fall back to
   no expansion, which is conservative. *)
let safe_expand_head env ty =
  match Ctype.expand_head env ty with
  | ty' -> ty'
  | exception _ -> ty
;;

let rec dsort_of_type env ty =
  match get_desc (safe_expand_head env ty) with
  | Tconstr (p, [], _) when Path.same p Predef.path_int -> S_int
  | Tconstr (p, [], _) when Path.same p Predef.path_bool -> S_bool
  | Trefine (skel, _) -> dsort_of_type env skel
  | _ -> S_other
;;

let record_name env id ty = Hashtbl.replace name_sorts id (dsort_of_type env ty)

let has_vox_attr name attrs =
  List.exists (fun (a : Parsetree.attribute) -> String.equal a.attr_name.txt name) attrs
;;

(* The refinement of a type, if any. *)
let refinement_of_type env ty =
  match get_desc (safe_expand_head env ty) with
  | Trefine (_, p) -> Some p
  | _ -> None
;;

(* Facts contributed by the binders of a pattern: every binder is recorded (for its
   declaration sort); binders of refined type contribute their refinement instantiated at
   the binder. *)
let binder_facts : type k. Env.t -> k general_pattern -> Refinement.pred list =
  fun env pat ->
  List.concat_map
    (fun (id, _, ty, _, _) ->
      record_name env id ty;
      match refinement_of_type env ty with
      | Some p -> [ Refinement.subst_bound ~by:(Refinement.Pvar id) p ]
      | None -> [])
    (pat_bound_idents_full pat)
;;

(* The unpack fact: a pattern marked [refine_ x] binds [x] at the skeleton and contributes
   the SCRUTINEE's refinement at [x]. *)
let unpack_fact
  : type k. Env.t -> k general_pattern -> scrut:type_expr -> Refinement.pred list
  =
  fun env pat ~scrut ->
  if not (has_vox_attr "vox.refine" pat.pat_attributes)
  then []
  else (
    match pat_bound_idents pat, refinement_of_type env scrut with
    | [ id ], Some p -> [ Refinement.subst_bound ~by:(Refinement.Pvar id) p ]
    | _ -> [])
;;

(* The logical name of an expression: variables denote their stamp, integer literals
   themselves; anything else is a fresh unknown. *)
let name_of_expr env (e : expression) : Refinement.pred =
  match e.exp_desc with
  | Texp_ident { path = Path.Pident id; _ } -> Refinement.Pvar id
  | Texp_constant (Const_int n) -> Refinement.Pint n
  | Texp_construct ({ txt = Longident.Lident "true"; _ }, _, _, [], _) ->
    Refinement.Pbool true
  | Texp_construct ({ txt = Longident.Lident "false"; _ }, _, _, [], _) ->
    Refinement.Pbool false
  | _ ->
    let id = Ident.create_local "*vox-unknown*" in
    record_name env id e.exp_type;
    Refinement.Pvar id
;;

let emit_vc ~loc ~facts ~goal ~assumed =
  vcs := { vc_loc = loc; vc_facts = facts; vc_goal = goal; vc_assumed = assumed } :: !vcs
;;

(* Walk an expression under a list of facts, collecting VCs. *)
let rec walk_expr env facts (e : expression) =
  (* Intro forms: the node itself carries the vox attribute and the refined type. *)
  let is_refine = has_vox_attr "vox.refine" e.exp_attributes in
  let is_assume = has_vox_attr "vox.assume" e.exp_attributes in
  if is_refine || is_assume
  then (
    match refinement_of_type env e.exp_type with
    | Some p ->
      let n = name_of_expr env e in
      emit_vc
        ~loc:e.exp_loc
        ~facts
        ~goal:(Refinement.subst_bound ~by:n p)
        ~assumed:is_assume
    | None -> ());
  match e.exp_desc with
  | Texp_let (_rec_flag, vbs, body) ->
    List.iter (fun vb -> walk_expr env facts vb.vb_expr) vbs;
    let facts' = List.concat_map (fun vb -> binder_facts env vb.vb_pat) vbs @ facts in
    walk_expr env facts' body
  | Texp_match (scrut, _sort, comp_cases, val_cases, _partial) ->
    walk_expr env facts scrut;
    let do_case : type k. k case -> unit =
      fun c ->
      let facts' =
        unpack_fact env c.c_lhs ~scrut:scrut.exp_type @ binder_facts env c.c_lhs @ facts
      in
      Option.iter (walk_expr env facts') c.c_guard;
      walk_expr env facts' c.c_rhs
    in
    List.iter do_case comp_cases;
    List.iter do_case val_cases
  | Texp_ifthenelse (cond, e_then, e_else) ->
    walk_expr env facts cond;
    let cond_fact =
      match cond.exp_desc with
      | Texp_ident { path = Path.Pident id; _ } -> Some (Refinement.Pvar id)
      | _ -> None
    in
    let with_fact f facts =
      match cond_fact with
      | None -> facts
      | Some c -> f c :: facts
    in
    walk_expr env (with_fact (fun c -> c) facts) e_then;
    Option.iter (walk_expr env (with_fact (fun c -> Refinement.Pnot c) facts)) e_else
  | Texp_function { params; body; _ } ->
    let facts' =
      List.concat_map
        (fun fp ->
          match fp.fp_kind with
          | Tparam_pat pat -> binder_facts env pat
          | Tparam_optional_default (pat, default, _) ->
            walk_expr env facts default;
            binder_facts env pat)
        params
      @ facts
    in
    (match body with
     | Tfunction_body e -> walk_expr env facts' e
     | Tfunction_cases { fc_cases; _ } ->
       List.iter
         (fun c ->
           let facts'' = binder_facts env c.c_lhs @ facts' in
           Option.iter (walk_expr env facts'') c.c_guard;
           walk_expr env facts'' c.c_rhs)
         fc_cases)
  | _ ->
    (* Generic traversal of children under the same facts. *)
    let it =
      { Tast_iterator.default_iterator with expr = (fun _ e' -> walk_expr env facts e') }
    in
    Tast_iterator.default_iterator.expr it e
;;

(* ------------------------------------------------------------------ *)
(* SMT-LIB2 serialization *)

let smt_name id = "|" ^ Ident.unique_name id ^ "|"

let rec smt_of_pred buf (p : Refinement.pred) =
  let open Refinement in
  match p with
  | Pbound -> assert false (* always substituted before discharge *)
  | Pvar id -> Buffer.add_string buf (smt_name id)
  | Pint n ->
    if n >= 0
    then Buffer.add_string buf (Int.to_string n)
    else Buffer.add_string buf (Printf.sprintf "(- %d)" (-n))
  | Pbool b -> Buffer.add_string buf (Bool.to_string b)
  | Pbinop (Neq, a, b) ->
    Buffer.add_string buf "(not (= ";
    smt_of_pred buf a;
    Buffer.add_char buf ' ';
    smt_of_pred buf b;
    Buffer.add_string buf "))"
  | Pbinop (op, a, b) ->
    let s =
      match op with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Eq -> "="
      | Lt -> "<"
      | Le -> "<="
      | Gt -> ">"
      | Ge -> ">="
      | Neq -> assert false
    in
    Buffer.add_string buf ("(" ^ s ^ " ");
    smt_of_pred buf a;
    Buffer.add_char buf ' ';
    smt_of_pred buf b;
    Buffer.add_char buf ')'
  | Pand (a, b) ->
    Buffer.add_string buf "(and ";
    smt_of_pred buf a;
    Buffer.add_char buf ' ';
    smt_of_pred buf b;
    Buffer.add_char buf ')'
  | Por (a, b) ->
    Buffer.add_string buf "(or ";
    smt_of_pred buf a;
    Buffer.add_char buf ' ';
    smt_of_pred buf b;
    Buffer.add_char buf ')'
  | Pnot a ->
    Buffer.add_string buf "(not ";
    smt_of_pred buf a;
    Buffer.add_char buf ')'
;;

let free_vars_of_vc vc = List.concat_map Refinement.free_vars (vc.vc_goal :: vc.vc_facts)

let smt_script vc =
  let buf = Buffer.create 512 in
  let seen = Hashtbl.create 16 in
  let needs_other =
    List.exists
      (fun id ->
        match Hashtbl.find_opt name_sorts id with
        | Some (S_int | S_bool) -> false
        | Some S_other | None -> true)
      (free_vars_of_vc vc)
  in
  if needs_other then Buffer.add_string buf "(declare-sort VoxU 0)\n";
  List.iter
    (fun id ->
      if not (Hashtbl.mem seen id)
      then (
        Hashtbl.add seen id ();
        let s =
          match Hashtbl.find_opt name_sorts id with
          | Some S_int -> "Int"
          | Some S_bool -> "Bool"
          | Some S_other | None -> "VoxU"
        in
        Buffer.add_string buf (Printf.sprintf "(declare-const %s %s)\n" (smt_name id) s)))
    (free_vars_of_vc vc);
  List.iter
    (fun f ->
      Buffer.add_string buf "(assert ";
      smt_of_pred buf f;
      Buffer.add_string buf ")\n")
    vc.vc_facts;
  Buffer.add_string buf "(assert (not ";
  smt_of_pred buf vc.vc_goal;
  Buffer.add_string buf "))\n(check-sat)\n";
  Buffer.contents buf
;;

(* ------------------------------------------------------------------ *)
(* Z3 harness: [Sys.command] + temp files; no unix dependency. The solver's own timeout
   flag bounds runtime. A wedged process is out of scope for v0. *)

type verdict =
  | Valid
  | Invalid
  | Unknown of string

let z3_command () =
  match Sys.getenv_opt "VOX_Z3" with
  | Some s -> s
  | None -> "z3"
;;

let run_z3 script =
  let in_file = Filename.temp_file "vox" ".smt2" in
  let out_file = Filename.temp_file "vox" ".out" in
  Misc.try_finally
    ~always:(fun () ->
      Misc.remove_file in_file;
      Misc.remove_file out_file)
    (fun () ->
      let oc = open_out in_file in
      output_string oc script;
      close_out oc;
      let cmd =
        Printf.sprintf
          "%s -T:10 %s > %s 2>&1"
          (Filename.quote (z3_command ()))
          (Filename.quote in_file)
          (Filename.quote out_file)
      in
      let status = Sys.command cmd in
      let first_line =
        let ic = open_in out_file in
        let l =
          try input_line ic with
          | End_of_file -> ""
        in
        close_in ic;
        l
      in
      match first_line with
      | "unsat" -> Valid
      | "sat" -> Invalid
      | "timeout" -> Unknown "solver timeout"
      | "unknown" -> Unknown "solver returned unknown"
      | other ->
        Unknown
          (Printf.sprintf
             "solver error (exit %d): %s"
             status
             (if String.equal other "" then "<no output>" else other)))
;;

(* ------------------------------------------------------------------ *)

let print_pred ppf p = Format.pp_print_string ppf (Refinement.to_string p)

let dump_vc ppf vc =
  Format.fprintf
    ppf
    "@[<v 2>%a: vox VC%s:@ goal: %a@ hypotheses:%t@]@."
    Location.print_loc
    vc.vc_loc
    (if vc.vc_assumed then " (ASSUMED)" else "")
    print_pred
    vc.vc_goal
    (fun ppf ->
      if vc.vc_facts = []
      then Format.fprintf ppf " <none>"
      else List.iter (fun f -> Format.fprintf ppf "@ %a" print_pred f) vc.vc_facts)
;;

let discharge () =
  let all = List.rev !vcs in
  if !Clflags.vox_dump_vc then List.iter (dump_vc Format.err_formatter) all;
  if !Clflags.vox_dry_run
  then ()
  else
    List.iter
      (fun vc ->
        if not vc.vc_assumed
        then (
          match run_z3 (smt_script vc) with
          | Valid -> ()
          | Invalid ->
            Location.raise_errorf
              ~loc:vc.vc_loc
              "vox: verification failed.@ Unprovable goal: %s"
              (Refinement.to_string vc.vc_goal)
          | Unknown reason ->
            Location.raise_errorf
              ~loc:vc.vc_loc
              "vox: verification failed (%s).@ Goal: %s"
              reason
              (Refinement.to_string vc.vc_goal)))
      all
;;

(* Entry point: called on the final typedtree of an implementation. *)
(* VCs arise only from [refine_]/[assume_] expressions and [refine_] patterns, all of
   which carry a "vox." attribute. Programs without any are skipped entirely: the pass
   must not even inspect (and via [Ctype.expand_head], mutate) the types of unannotated
   programs. *)
let uses_vox (str : structure) =
  let found = ref false in
  let has_vox attrs =
    List.exists
      (fun (a : Parsetree.attribute) ->
        String.length a.attr_name.txt >= 4
        && String.equal (String.sub a.attr_name.txt 0 4) "vox.")
      attrs
  in
  let it =
    { Tast_iterator.default_iterator with
      expr =
        (fun sub e ->
          if has_vox e.exp_attributes then found := true;
          Tast_iterator.default_iterator.expr sub e)
    ; pat =
        (fun sub (type k) (p : k general_pattern) ->
          if has_vox p.pat_attributes then found := true;
          Tast_iterator.default_iterator.pat sub p)
    }
  in
  it.structure it str;
  !found
;;

let check_implementation (str : structure) =
  if not (uses_vox str)
  then ()
  else (
    reset ();
    let facts = ref [] in
    List.iter
      (fun item ->
        match item.str_desc with
        | Tstr_value (_rec_flag, vbs) ->
          List.iter (fun vb -> walk_expr str.str_final_env !facts vb.vb_expr) vbs;
          facts
          := List.concat_map (fun vb -> binder_facts str.str_final_env vb.vb_pat) vbs
             @ !facts
        | _ ->
          let it =
            { Tast_iterator.default_iterator with
              expr = (fun _ e -> walk_expr str.str_final_env !facts e)
            }
          in
          it.structure_item it item)
      str.str_items;
    discharge ())
;;
