open Copy_spec
open Copy_heap_proofs
open Copy_model_proofs
open Effective_copy_spec
open Effective_copy_heap_proofs
open Effective_copy_metadata

let rec (with_copy_model @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation saved rho x})) @ total ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (wanted : ((x : node Pref.t) @ immutable -> {u : unit | effective_instance_at saved heads rho want x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
      (assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
        {u : unit | not (effective_target_for saved heads d p q) || tau q === want p})) @ total -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun saved heads scope rho model want wanted epoch depth d premise claim use -> ghost_ (
  effective_valid_def saved heads epoch depth d; heap_def saved epoch depth d;
  match d with
  | Clean ->
    let equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem saved x) || rho x === rho x}) @ total =
      fun x -> () in
    let assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | not (effective_target_for saved heads d p q) || rho q === want p}) @ total = fun p q ->
      effective_target_for_def saved heads d p q; mapping_def d p;
      scope p; source_ok_def saved p; wanted p; effective_instance_at_def saved heads rho want p; () in
    let next : ((x : node Pref.t) @ immutable ->
      {u : unit | equation (heap saved epoch depth d) rho x}) @ total = refine_ model in
    let () = use rho next equal assigned in ()
  | Start ->
    let desc = Bool in let v = cell desc depth in let value = Boolean in
    cell_def desc depth; payload_scoped_def saved v; describes_def rho desc value;
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (H.put saved epoch v) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
        {u : unit | tau epoch === value} -> {u : unit | claim}) @ total = fun tau next equal _fit ->
      let assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
          {u : unit | not (effective_target_for saved heads d p q) || tau q === want p}) @ total = fun p q ->
        effective_target_for_def saved heads d p q; mapping_def d p; scope p; source_ok_def saved p; wanted p; effective_instance_at_def saved heads rho want p; equal p;
        () in
      let next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x}) @ total = refine_ next in
      let () = use tau next equal assigned in () in
    let () = with_allocation_model saved scope rho model epoch v value () claim consume in ()
  | Fresh (rest, p, q, old, desc) ->
    let h = heap saved epoch depth rest in
    let prior_scope : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}
        @ total = fun x -> let () = history_scope saved heads scope epoch depth rest x () in () in
    let consume : ((tau0 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model0 : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth rest) tau0 x})) @ total ->
        (equal0 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau0 x === rho x})) @ total ->
        (assigned0 : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
          {u : unit | not (effective_target_for saved heads rest p q) || tau0 q === want p})) @ total ->
        {u : unit | claim}) @ total = fun tau0 model0 equal0 assigned0 ->
      let model0 : ((x : node Pref.t) @ immutable -> {u : unit | equation h tau0 x}) @ total = refine_ model0 in
      history_at saved heads epoch depth rest p ();
      scope p; source_ok_def saved p; wanted p; effective_instance_at_def saved heads rho want p; effective_ready_def saved heads rest old.desc desc;
      let value = want p in let v = cell desc depth in cell_def desc depth;
      ready_scoped saved heads epoch depth rest old.desc desc ();
      describes_def tau0 desc value;
      (match old.desc, desc with List a, List x -> assigned0 a x; ()
      | Arrow (a, b), Arrow (x, y) -> assigned0 a x; assigned0 b y; () | _ -> ());
      let consume1 : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation (H.put h q v) tau x})) @ total ->
          (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === tau0 x})) @ total ->
          {u : unit | tau q === value} -> {u : unit | claim}) @ total = fun tau model1 equal1 fit ->
        let h1 = H.put h q v in
        let model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation h1 tau x}) @ total = refine_ model1 in
        let next : (x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x}
            @ total = fun x -> mark_model rest h1 tau model1 p old epoch q x (); () in
        let equal : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x}
            @ total = fun x -> history_grows saved heads epoch depth rest x ();
          equal0 x; equal1 x; () in
        let assigned : ((a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
            {u : unit | not (effective_target_for saved heads d a b) || tau b === want a}) @ total = fun a b ->
          effective_target_for_def saved heads d a b; mapping_def d a; effective_target_for_def saved heads rest a b;
          if effective_target_for saved heads rest a b then (
            target_allocated saved heads epoch depth rest a b (); assigned0 a b; equal1 b; ())
          else () in
        let () = use tau next equal assigned in () in
      let () = with_allocation_model h prior_scope tau0 model0 q v value () claim consume1 in () in
    let () = with_copy_model saved heads scope rho model want wanted epoch depth rest () claim consume in ()
  | Alias (rest, p, q, old) ->
    let h = heap saved epoch depth rest in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model0 : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth rest) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
        (assigned0 : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
          {u : unit | not (effective_target_for saved heads rest p q) || tau q === want p})) @ total ->
        {u : unit | claim}) @ total = fun tau model0 equal assigned0 ->
      let model0 : ((x : node Pref.t) @ immutable -> {u : unit | equation h tau x}) @ total = refine_ model0 in
      history_at saved heads epoch depth rest p ();
      scope p; source_ok_def saved p; wanted p; effective_instance_at_def saved heads rho want p;
      (match old.desc with Link child -> assigned0 child q; () | _ -> ());
      let next : (x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x}
          @ total = fun x -> let () = mark_model rest h tau model0 p old epoch q x () in () in
      let assigned : ((a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
          {u : unit | not (effective_target_for saved heads d a b) || tau b === want a}) @ total = fun a b ->
        effective_target_for_def saved heads d a b; mapping_def d a; effective_target_for_def saved heads rest a b; assigned0 a b;
        () in
      let () = use tau next equal assigned in () in
    let () = with_copy_model saved heads scope rho model want wanted epoch depth rest () claim consume in ())
