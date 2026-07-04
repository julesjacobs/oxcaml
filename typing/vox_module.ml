(* vox: Lean MODULE artifacts -- the plumbing layer for sealed
   signatures.  Predicate rendering stays in Vox_verify; this module
   owns the static Lean texts and the process/file mechanics:

   - VoxCore: the compiler-owned base theory (VoxU, the iarray theory,
     the tuple products), compiled ONCE per output directory and
     [public import]ed by every module-mode solver input, replacing
     the per-input splicing of those declarations.
   - Sig modules: a unit's .mli blocks compiled to VoxSig_<Unit>.olean
     next to its .cmi.  Clients import the artifact instead of
     splicing text, so client verification depends on the INTERFACE
     alone -- it exists before the implementation does, and no .ml
     edit can perturb it.  The block text is native module-system
     Lean: the author marks declarations [public] (and [@[expose]]
     where clients may unfold); Lean is the only grammar police.
   - The seal: appended (LAST -- see below) to a sig-bearing unit's
     own solver input.  It re-elaborates the sig text inside
     [namespace VoxSealSig] and runs a metaprogram checking that
     every sig [axiom] is implemented by a same-named PROVED theorem
     whose statement matches ([isDefEq] after stripping the
     namespace), that every sig [opaque] has an implementation at its
     type, and that no law's proof depends on the seal's own axioms.
     An interface [axiom] is therefore an OBLIGATION on the
     implementation, never trust.  Because the seal is the last
     section of the file, the implementation's theorems are
     elaborated before the sig axioms exist -- circularity is
     structurally impossible, and the [collectAxioms] guard is
     defense in depth. *)

let max_tuple_arity = 8

let tuple_uname n = "VoxT" ^ Int.to_string n

(* Is [name] a tuple product VoxCore already declares?  Spliced texts
   must not re-declare those (Lean rejects the clash); wider arities
   are not in VoxCore and still ride the splice. *)
let core_tuple_uname name =
  List.exists
    (fun n -> String.equal name (tuple_uname n))
    (List.init (max_tuple_arity - 1) (fun i -> i + 2))
;;

(* The product structure for one tuple arity, universe-polymorphic
   over [Sort] so a Prop component (the Lean model of bool)
   instantiates as readily as a Type one.  [vis] is prepended to the
   declaration ("public " inside module files). *)
let lean_tuple_decl ?(vis = "") n =
  let buf = Buffer.create 128 in
  Buffer.add_string buf vis;
  Buffer.add_string buf (Printf.sprintf "structure %s.{" (tuple_uname n));
  for i = 1 to n do
    if i > 1 then Buffer.add_string buf ", ";
    Buffer.add_string buf (Printf.sprintf "u%d" i)
  done;
  Buffer.add_string buf "}";
  for i = 1 to n do
    Buffer.add_string buf (Printf.sprintf " (t%d : Sort u%d)" i i)
  done;
  let univ =
    let rec go i =
      if i > n then "1" else Printf.sprintf "max u%d (%s)" i (go (i + 1))
    in
    go 1
  in
  Buffer.add_string buf (Printf.sprintf " : Sort (%s) where" univ);
  for i = 1 to n do
    Buffer.add_string buf (Printf.sprintf " (p%d : t%d)" i i)
  done;
  Buffer.add_char buf '\n';
  Buffer.contents buf
;;

let lean_iarray_theory ?(vis = "") () =
  Printf.sprintf
    "%sopaque VoxIA : Type\n\
     %sopaque Vox_ia_len : VoxIA -> Int\n\
     %sopaque Vox_ia_get : VoxIA -> Int -> Int\n\
     %saxiom Vox_ia_len_nonneg (a : VoxIA) : 0 <= Vox_ia_len a\n\
     grind_pattern Vox_ia_len_nonneg => Vox_ia_len a\n"
    vis vis vis vis
;;

(* The base theory every module-mode input imports.  All public:
   privacy is pointless for compiler-owned declarations, and clients
   reference every one of them. *)
let core_text =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "module\n\n";
  Buffer.add_string buf "public opaque VoxU : Type\n";
  Buffer.add_string buf (lean_iarray_theory ~vis:"public " ());
  for n = 2 to max_tuple_arity do
    Buffer.add_string buf (lean_tuple_decl ~vis:"public " n)
  done;
  Buffer.contents buf
;;

let core_module_name = "VoxCore"

(* A unit's sig-module name; the .olean sits next to the .cmi, so the
   ordinary load path doubles as LEAN_PATH. *)
let sig_module_prefix = "VoxSig_"
let sig_module_name unit = sig_module_prefix ^ unit

(* The seal, given the unit's sig block text (verbatim; its own
   [public] markers are harmless here).  Elaborated after the
   implementation and the unit's VC theorems. *)
let seal_text ~sig_text =
  "\nnamespace VoxSealSig\n"
  ^ sig_text
  ^ "\nend VoxSealSig\n\n"
  ^ "open Lean Meta in\n\
     run_meta do\n\
    \  let env \xe2\x86\x90 getEnv\n\
    \  let ns := `VoxSealSig\n\
    \  -- the implementation's constant is top-level in this input --\n\
    \  -- module-private, so its real name is mangled -- and Lean's own\n\
    \  -- exports can overload the short name (e.g. [insert]): among\n\
    \  -- the candidates, prefer the one USER-NAMED exactly [short]\n\
    \  let resolve (short : Name) : MetaM (Option Name) := do\n\
    \    let cs \xe2\x86\x90\n\
    \      try resolveGlobalConst (mkIdent short) catch _ => pure []\n\
    \    let exact := cs.filter fun c =>\n\
    \      c == short || privateToUserName? c == some short\n\
    \    match exact, cs with\n\
    \    | [c], _ => return some c\n\
    \    | [], [c] => return some c\n\
    \    | _, _ => return none\n\
    \  let strip (e : Expr) : MetaM Expr := do\n\
    \    Core.transform e (post := fun x => do\n\
    \      match x with\n\
    \      | .const n us =>\n\
    \        if ns.isPrefixOf n then\n\
    \          let short := n.replacePrefix ns Name.anonymous\n\
    \          match \xe2\x86\x90 resolve short with\n\
    \          | some r => return .done (.const r us)\n\
    \          | none => throwError \"vox seal: the interface \
     declares {short}, which the \
     implementation does not define\"\n\
    \        else return .continue\n\
    \      | _ => return .continue)\n\
    \  for (name, info) in env.constants.toList do\n\
    \    unless ns.isPrefixOf name do continue\n\
    \    let short := name.replacePrefix ns Name.anonymous\n\
    \    match info with\n\
    \    | .axiomInfo ai =>\n\
    \      let some implName \xe2\x86\x90 resolve short\n\
    \        | throwError \"vox seal: interface law {short} has \
     no implementation theorem\"\n\
    \      match env.find? implName with\n\
    \      | none => throwError \"vox seal: interface law {short} has no \
     implementation theorem\"\n\
    \      | some impl =>\n\
    \        let ity \xe2\x86\x90 strip ai.type\n\
    \        unless (\xe2\x86\x90 isDefEq ity impl.type) do\n\
    \          throwError \"vox seal: law {short}: the implementation's \
     statement does not match the interface's\"\n\
    \        (match impl with\n\
    \         | .thmInfo _ | .defnInfo _ => pure ()\n\
    \         | _ => throwError \"vox seal: law {short} must be proved by \
     the implementation, not assumed\")\n\
    \        let axs \xe2\x86\x90 collectAxioms implName\n\
    \        for ax in axs do\n\
    \          if ns.isPrefixOf ax then\n\
    \            throwError \"vox seal: law {short}'s proof depends on \
     interface law {ax.replacePrefix ns Name.anonymous} (circular)\"\n\
    \    | .opaqueInfo oi =>\n\
    \      let some implName \xe2\x86\x90 resolve short\n\
    \        | throwError \"vox seal: interface opaque {short} \
     has no implementation\"\n\
    \      match env.find? implName with\n\
    \      | none => throwError \"vox seal: interface opaque {short} has \
     no implementation\"\n\
    \      | some impl =>\n\
    \        let ity \xe2\x86\x90 strip oi.type\n\
    \        unless (\xe2\x86\x90 isDefEq ity impl.type) do\n\
    \          throwError \"vox seal: opaque {short}: the implementation's \
     type does not match the interface's\"\n\
    \    | _ => pure ()\n\
    \  pure ()\n"
;;

(* ------------------------------------------------------------------ *)
(* Process mechanics. *)

let lean_path_env dirs =
  "LEAN_PATH=" ^ String.concat ":" dirs
;;

(* Run [lean] on [text], producing [olean_out] atomically.  Lean's
   module system requires the source under the invocation's root
   directory and derives the MODULE NAME from the file name, so the
   source is written as [module_name].lean inside a private temp
   directory and lean runs from there; imports resolve through
   LEAN_PATH (absolute directories).  [-o X] also writes the module's
   private and server parts as [X.private]/[X.server] (renamed into
   place beside the .olean) and compiled IR as [X-with-last-extension
   -replaced].ir (removed; nothing here runs module code).  The temp
   name is unique so concurrent builders of a SHARED artifact
   (VoxCore) cannot interleave writes; the final renames are atomic
   and both orders leave identical artifacts.  Returns the solver's
   combined output on failure. *)
let build_olean ~lean_command ~lean_path_dirs ~olean_out ~module_name text =
  (* the command below changes directory; artifact paths must survive *)
  let olean_out =
    if Filename.is_relative olean_out
    then Filename.concat (Sys.getcwd ()) olean_out
    else olean_out
  in
  let tmp_dir =
    let f = Filename.temp_file "voxmod" ".d" in
    Sys.remove f;
    Sys.mkdir f 0o700;
    f
  in
  let src = Filename.concat tmp_dir (module_name ^ ".lean") in
  let tmp_out =
    Filename.temp_file
      ~temp_dir:(Filename.dirname olean_out)
      (Filename.basename olean_out ^ ".")
      ".tmp"
  in
  let tmp_ir = Filename.remove_extension tmp_out ^ ".ir" in
  let cleanup_tmp () =
    List.iter
      Misc.remove_file
      [ tmp_out; tmp_out ^ ".private"; tmp_out ^ ".server"; tmp_ir ]
  in
  let oc = open_out src in
  output_string oc text;
  close_out oc;
  let log = Filename.temp_file "voxmod" ".out" in
  let cmd =
    Printf.sprintf
      "cd %s && %s %s %s -o %s > %s 2>&1"
      (Filename.quote tmp_dir)
      (lean_path_env lean_path_dirs)
      (Filename.quote lean_command)
      (Filename.quote (module_name ^ ".lean"))
      (Filename.quote tmp_out)
      (Filename.quote log)
  in
  let status = Sys.command cmd in
  let output =
    let ic = open_in_bin log in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    s
  in
  Misc.remove_file src;
  Misc.remove_file log;
  (try Sys.rmdir tmp_dir with Sys_error _ -> ());
  if status = 0
  then (
    match
      List.iter
        (fun suffix ->
          let part = tmp_out ^ suffix in
          if Sys.file_exists part then Sys.rename part (olean_out ^ suffix))
        [ ""; ".private"; ".server" ]
    with
    | () ->
      Misc.remove_file tmp_ir;
      Ok ()
    | exception Sys_error e ->
      cleanup_tmp ();
      Error ("renaming the artifact: " ^ e))
  else (
    cleanup_tmp ();
    Error output)
;;

(* VoxCore.olean in [dir], built when missing or when the compiler's
   text changed (hash sidecar).  Concurrent builders race benignly:
   both produce identical artifacts and the rename is atomic. *)
let ensure_core ~lean_command ~lean_path_dirs ~dir =
  let olean = Filename.concat dir (core_module_name ^ ".olean") in
  let stamp = olean ^ ".src" in
  let digest = Digest.to_hex (Digest.string core_text) in
  let fresh =
    Sys.file_exists olean
    && Sys.file_exists stamp
    &&
    match open_in stamp with
    | ic ->
      let d = try input_line ic with _ -> "" in
      close_in ic;
      String.equal d digest
    | exception Sys_error _ -> false
  in
  if fresh
  then Ok olean
  else (
    match
      build_olean ~lean_command ~lean_path_dirs ~olean_out:olean
        ~module_name:core_module_name core_text
    with
    | Ok () ->
      let oc = open_out stamp in
      output_string oc digest;
      output_char oc '\n';
      close_out oc;
      Ok olean
    | Error e -> Error e)
;;
