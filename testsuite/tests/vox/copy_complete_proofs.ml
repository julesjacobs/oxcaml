open Copy_spec
open Copy_heap_proofs
open Copy_model_proofs

let rec (with_copy_model @ total) : (saved : Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation saved rho x})) @ total ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (wanted : ((x : node Pref.t) @ immutable -> {u : unit | instance_at saved rho want x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    {u : unit | valid saved epoch depth d} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
      (assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
        {u : unit | not (target_for saved d p q) || tau q === want p})) @ total -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun saved scope rho model want wanted epoch depth d premise claim use -> ghost_ (
  let refine_ premise = premise in valid_def saved epoch depth d; heap_def saved epoch depth d;
  let u = () in match d with
  | Clean ->
    let equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem saved x) || rho x === rho x}) @ total =
      fun x -> let u = () in refine_ u in
    let assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | not (target_for saved d p q) || rho q === want p}) @ total = fun p q ->
      target_for_def saved d p q; mapping_def d p;
      wanted p; instance_at_def saved rho want p; let u = () in refine_ u in
    let next : ((x : node Pref.t) @ immutable ->
      {u : unit | equation (heap saved epoch depth d) rho x}) @ total = refine_ model in
    let refine_ u = use rho next equal assigned in refine_ u
  | Start ->
    let desc = Bool in let v = cell desc depth in let value = Boolean in
    cell_def desc depth; payload_scoped_def saved v; describes_def rho desc value;
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (H.put saved epoch v) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
        {u : unit | tau epoch === value} -> {u : unit | claim}) @ total = fun tau next equal _fit ->
      let assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
          {u : unit | not (target_for saved d p q) || tau q === want p}) @ total = fun p q ->
        target_for_def saved d p q; mapping_def d p; wanted p; instance_at_def saved rho want p; equal p;
        let u = () in refine_ u in
      let next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x}) @ total = refine_ next in
      let refine_ u = use tau next equal assigned in refine_ u in
    let refine_ u = with_allocation_model saved scope rho model epoch v value (refine_ u) claim consume in refine_ u
  | Fresh (rest, p, q, old, desc) ->
    let h = heap saved epoch depth rest in
    let prior_scope : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}
        @ total = fun x -> let u = () in let refine_ u = history_scope saved scope epoch depth rest x (refine_ u) in refine_ u in
    let consume : ((tau0 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model0 : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth rest) tau0 x})) @ total ->
        (equal0 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau0 x === rho x})) @ total ->
        (assigned0 : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
          {u : unit | not (target_for saved rest p q) || tau0 q === want p})) @ total ->
        {u : unit | claim}) @ total = fun tau0 model0 equal0 assigned0 ->
      let model0 : ((x : node Pref.t) @ immutable -> {u : unit | equation h tau0 x}) @ total = refine_ model0 in
      let u = () in history_at saved epoch depth rest p (refine_ u);
      wanted p; instance_at_def saved rho want p; ready_def saved rest old.desc desc;
      let value = want p in let v = cell desc depth in cell_def desc depth;
      ready_scoped saved epoch depth rest old.desc desc (refine_ u);
      describes_def tau0 desc value;
      (match old.desc, desc with Arrow (a, b), Arrow (x, y) -> assigned0 a x; assigned0 b y; () | _ -> ());
      let consume1 : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation (H.put h q v) tau x})) @ total ->
          (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === tau0 x})) @ total ->
          {u : unit | tau q === value} -> {u : unit | claim}) @ total = fun tau model1 equal1 fit ->
        let refine_ fit = fit in let h1 = H.put h q v in
        let model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation h1 tau x}) @ total = refine_ model1 in
        let next : (x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x}
            @ total = fun x -> let u = () in
          mark_model rest h1 tau model1 p old epoch q x (refine_ u); let u = () in refine_ u in
        let equal : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x}
            @ total = fun x -> let u = () in history_grows saved epoch depth rest x (refine_ u);
          equal0 x; equal1 x; refine_ u in
        let assigned : ((a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
            {u : unit | not (target_for saved d a b) || tau b === want a}) @ total = fun a b ->
          target_for_def saved d a b; mapping_def d a; target_for_def saved rest a b;
          let u = () in
          if target_for saved rest a b then (
            target_allocated saved epoch depth rest a b (refine_ u); assigned0 a b; equal1 b; refine_ u)
          else refine_ u in
        let refine_ u = use tau next equal assigned in refine_ u in
      let refine_ u = with_allocation_model h prior_scope tau0 model0 q v value (refine_ u) claim consume1 in refine_ u in
    let refine_ u = with_copy_model saved scope rho model want wanted epoch depth rest (refine_ u) claim consume in refine_ u
  | Alias (rest, p, q, old) ->
    let h = heap saved epoch depth rest in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model0 : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth rest) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
        (assigned0 : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
          {u : unit | not (target_for saved rest p q) || tau q === want p})) @ total ->
        {u : unit | claim}) @ total = fun tau model0 equal assigned0 ->
      let model0 : ((x : node Pref.t) @ immutable -> {u : unit | equation h tau x}) @ total = refine_ model0 in
      let u = () in history_at saved epoch depth rest p (refine_ u);
      wanted p; instance_at_def saved rho want p;
      (match old.desc with Link child -> assigned0 child q; () | _ -> ());
      let next : (x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x}
          @ total = fun x -> let u = () in let refine_ u = mark_model rest h tau model0 p old epoch q x (refine_ u) in refine_ u in
      let assigned : ((a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
          {u : unit | not (target_for saved d a b) || tau b === want a}) @ total = fun a b ->
        target_for_def saved d a b; mapping_def d a; target_for_def saved rest a b; assigned0 a b;
        let u = () in refine_ u in
      let refine_ u = use tau next equal assigned in refine_ u in
    let refine_ u = with_copy_model saved scope rho model want wanted epoch depth rest (refine_ u) claim consume in refine_ u)
