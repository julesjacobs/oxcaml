(* Unused proof steps (warning 227).

   A proof step is a source construct whose effect on verification is to add
   facts: a lemma call (an application whose result is a refinement of [unit]),
   an [assume_], or a refinement written on a function parameter. VC generation
   ([Vox_vc]) marks each assumption a step makes. After a pass has proved all
   its queries, each proved query is proved again with every marked assumption
   [p] replaced by [u => p] for a fresh Boolean indicator [u], checked assuming
   the indicators; Z3 then reports an unsat core of indicators (see
   [Vox_smt.to_smtlib ~assumptions]). A step is used when one of its indicators
   is in some core, and reported when none is.

   Normal proofs are unchanged: indicators only appear in the second queries,
   which run after the pass's proofs and only when the warning is enabled for
   some step. A core need not be minimal, so a step in a core may still be
   unnecessary; but each query is proved from its core alone, so all the steps
   absent from every core can be removed together. A second query that is not
   proved, or a check abandoned for lack of budget, counts every step it
   mentions as used. The precise mode shrinks each core by proving the query
   again without each step in it (deletion-based minimization): it finds more
   unused steps, at the cost of a query per step in a core.

   A refined value can also be needed by the typer, which accepts it where its
   own refined type is expected without any proof. [Vox_vc] therefore only
   creates steps whose value is not used that way. *)

type kind =
  | Lemma_call of string
  | Assume
  | Argument of string

type step =
  { kind : kind;
    loc : Location.t;
    warnings : Warnings.state;
    parents : step list;
        (** The steps whose evaluation contains this one: removing one of them
            removes this one too. *)
    mutable used : bool
  }

type checker =
  { core :
      Location.t ->
      Vox_smt.query ->
      assumptions:Vox_smt.Symbol.t list ->
      deletion:bool ->
      Vox_smt.Symbol.t list option;
        (** [deletion] marks the precise mode's proofs without a step, which may
            use a smaller budget. *)
    precise : bool;
    precise_unreported : bool;
        (** The precise mode, only for steps whose warning is disabled (that
            [report] receives through [all_steps]). *)
    all_steps : bool;
        (** Check steps whose warning is disabled too; [report] then receives
            them, in their warning state. *)
    report : Location.t -> Warnings.t -> unit;
    abandoned : exn -> bool
        (** Exceptions from [core] that end the check without a result, such as
            an exhausted verification budget. *)
  }

let warning = function
  | Lemma_call name -> Warnings.Unused_proof_step (Unused_lemma_call name)
  | Assume -> Warnings.Unused_proof_step Unused_assume
  | Argument name -> Warnings.Unused_proof_step (Unused_argument name)

(* A step is identified by its kind and source location: the same code can be
   evaluated more than once, and the termination pass evaluates a function body
   again. *)
type key = kind * string * int * int

let key step =
  let loc = step.loc in
  ( step.kind,
    loc.loc_start.pos_fname,
    loc.loc_start.pos_cnum,
    loc.loc_end.pos_cnum )

let active : checker option ref = ref None

let steps : (key, step) Hashtbl.t = Hashtbl.create 16

(* Steps used by a proof in an earlier pass over the same unit (termination
   checks run while typing, before the unit's proofs). *)
let used_elsewhere : (key, unit) Hashtbl.t = Hashtbl.create 16

(* The steps that the facts made now belong to. *)
let current : step list ref = ref []

let enabled () =
  match !active with
  | None -> false
  | Some checker ->
    checker.all_steps
    || Warnings.is_active (Warnings.Unused_proof_step Unused_assume)

let create kind loc =
  if (not (enabled ())) || loc.Location.loc_ghost
  then None
  else
    let step =
      { kind;
        loc;
        warnings = Warnings.backup ();
        parents = !current;
        used = false
      }
    in
    let key = key step in
    match Hashtbl.find_opt steps key with
    | Some existing -> Some existing
    | None ->
      Hashtbl.add steps key step;
      Some step

let within steps f =
  let saved = !current in
  current := steps;
  Fun.protect ~finally:(fun () -> current := saved) f

(* Facts made by [f] belong to [step] (not to the enclosing steps, which are its
   parents). *)
let with_step step f =
  match step with None -> f () | Some step -> within [step] f

(* Facts made by [f] also belong to [step]. *)
let also_step step f =
  if List.memq step !current then f () else within (step :: !current) f

let rec use step =
  if not step.used
  then begin
    step.used <- true;
    List.iter use step.parents
  end

(* [query] has been proved; [indicators] maps its indicator symbols to the steps
   whose assumptions they guard. *)
let check checker loc (query : Vox_smt.query) indicators =
  let present =
    List.filter (fun (symbol, _) -> List.memq symbol query.symbols) indicators
  in
  if List.exists (fun (_, step) -> not step.used) present
  then begin
    let core ~deletion assumptions =
      checker.core loc query ~assumptions:(List.map fst assumptions) ~deletion
      |> Option.map (fun core ->
          List.filter (fun (symbol, _) -> List.memq symbol core) assumptions)
    in
    let precise step =
      checker.precise
      || checker.precise_unreported
         &&
         let saved = Warnings.backup () in
         Warnings.restore step.warnings;
         Fun.protect
           ~finally:(fun () -> Warnings.restore saved)
           (fun () -> not (Warnings.is_active (warning step.kind)))
    in
    match core ~deletion:false present with
    | None -> List.iter (fun (_, step) -> use step) present
    | Some found ->
      let found =
        (* Drop one step at a time, keeping the smaller core of each proof that
           succeeds without it. Steps already used elsewhere stay. *)
        let candidates =
          List.fold_left
            (fun candidates (_, step) ->
              if step.used || List.memq step candidates || not (precise step)
              then candidates
              else step :: candidates)
            [] found
        in
        List.fold_left
          (fun found step ->
            if not (List.exists (fun (_, s) -> s == step) found)
            then found
            else
              match
                core ~deletion:true
                  (List.filter (fun (_, s) -> s != step) found)
              with
              | Some smaller -> smaller
              | None -> found)
          found (List.rev candidates)
      in
      List.iter (fun (_, step) -> use step) found
  end

(* Core checks, which run after every proof of a pass. *)
let pending : (unit -> unit) Queue.t = Queue.create ()

let defer check = Queue.add check pending

(* Run [f] with [checker] active for a pass, then its core checks: they run
   after the pass's proofs, so they cannot change those proofs or use their
   budget. With [report], report the unused steps of this pass afterwards;
   otherwise remember the used ones for a later pass. A check that is abandoned
   reports nothing, and makes every step of a pass without [report] count as
   used. *)
let pass ?checker ~report f =
  let saved = !active in
  active := checker;
  let finish () =
    active := saved;
    current := [];
    let registered =
      Hashtbl.fold (fun key step acc -> (key, step) :: acc) steps []
    in
    Hashtbl.reset steps;
    Queue.clear pending;
    registered
  in
  let registered, complete =
    match
      f ();
      match checker with
      | None -> true
      | Some checker -> (
        try
          Queue.iter (fun check -> check ()) pending;
          true
        with exn when checker.abandoned exn -> false)
    with
    | complete -> finish (), complete
    | exception exn ->
      ignore (finish ());
      if report then Hashtbl.reset used_elsewhere;
      raise exn
  in
  match checker with
  | None -> ()
  | Some checker ->
    if report
    then begin
      let unused =
        List.filter
          (fun (key, step) ->
            complete && (not step.used) && not (Hashtbl.mem used_elsewhere key))
          registered
      in
      Hashtbl.reset used_elsewhere;
      List.iter
        (fun (_, step) ->
          let saved = Warnings.backup () in
          Warnings.restore step.warnings;
          Fun.protect
            ~finally:(fun () -> Warnings.restore saved)
            (fun () -> checker.report step.loc (warning step.kind)))
        (List.sort
           (fun (_, a) (_, b) ->
             compare
               (a.loc.loc_start.pos_fname, a.loc.loc_start.pos_cnum)
               (b.loc.loc_start.pos_fname, b.loc.loc_start.pos_cnum))
           unused)
    end
    else
      List.iter
        (fun (key, step) ->
          if step.used || not complete
          then Hashtbl.replace used_elsewhere key ())
        registered
