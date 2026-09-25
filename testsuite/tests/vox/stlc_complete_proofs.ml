open Unifier_spec
open Unifier_finite_spec
open Unifier_finite_proofs
open Stlc_spec
open Stlc_graph_proofs
open Stlc_model_proofs

let rec (with_generation_model @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (env : env) @ immutable -> (g : graph) @ immutable -> (after : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (target : ty) @ immutable -> (d : typing) @ immutable ->
    {u : unit | built h env g after && env_allocated h env
      && typed (context_of rho env) (source g) target d} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | satisfies tau (constraints g) && tau (root g) === target} -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun h trees env g after rho model target d premise claim use -> ghost_ (
  let refine_ premise = premise in built_def h env g after; source_def g; root_def g; constraints_def g;
  let ctx = context_of rho env in let e = source g in typed_def ctx e target d;
  let u = () in
  match g with
  | GVar (i, p) ->
    (match d with
    | Variable ->
      lookup_context rho env i p (refine_ u);
      let equal : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho x === rho x}
          @ total = fun x -> let u = () in refine_ u in
      let cs = Nothing in satisfies_def rho cs;
      let next : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x}) @ total = refine_ model in
      let refine_ u = use rho next equal (refine_ u) in refine_ u
    | _ -> refine_ u)
  | GBool p -> (match d with
    | Constant ->
      let v = Bool in allocatable_def h v; describes_def rho v target;
      let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
          (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
          {u : unit | tau p === target} -> {u : unit | claim}) @ total = fun tau next equal fit ->
        let refine_ fit = fit in let cs = Nothing in satisfies_def tau cs;
        let u = () in let refine_ u = use tau next equal (refine_ u) in refine_ u in
      let refine_ u = with_allocation_model h trees rho model p v target after (refine_ u) claim consume in refine_ u
    | _ -> refine_ u)
  | GLam (arg, body, p, middle) -> (match d with
    | Abstraction (a, db) -> (match target with
      | TArrow (_, b) ->
        let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
        let v = Var in allocatable_def h v; describes_def rho v a;
        let trees1 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
            (if H.mem h1 x then finite h1 t else H.at h1 x === None)} @ immutable total = fun x ->
          let u = () in let refine_ t = allocation_finite_at h trees arg v x (refine_ u) in refine_ t in
        let trees2 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
            (if H.mem middle x then finite middle t else H.at middle x === None)} @ immutable total = fun x ->
          let u = () in let refine_ t = built_finite_at h1 trees1 env1 body middle x (refine_ u) in refine_ t in
        let consume1 : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation h1 rho1 x})) @ total ->
            (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
            {u : unit | rho1 arg === a} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
          let refine_ fit1 = fit1 in let u = () in
          context_equal h rho rho1 equal1 env (refine_ u); context_of_def rho1 env1;
          let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem h1 x}
              @ total = fun x -> let u = () in refine_ u in
          env_frame h h1 keep env (refine_ u); env_allocated_def h1 env1;
          let consume2 : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
              (model2 : ((x : node Pref.t) @ immutable -> {u : unit | equation middle rho2 x})) @ total ->
              (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
              {u : unit | satisfies rho2 (constraints body) && rho2 (root body) === b} ->
              {u : unit | claim}) @ total = fun rho2 model2 equal2 fit2 ->
            let refine_ fit2 = fit2 in let u = () in
            equal2 arg; built_frame h1 env1 body middle arg (refine_ u);
            let payload = Arrow (arg, root body) in
            allocatable_def middle payload; describes_def rho2 payload target;
            let consume3 : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
                (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
                (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || tau x === rho2 x})) @ total ->
                {u : unit | tau p === target} -> {u : unit | claim}) @ total = fun tau next equal3 fit3 ->
              let refine_ fit3 = fit3 in let u = () in
              let equal : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}
                  @ total = fun x -> let u = () in
                built_frame h1 env1 body middle x (refine_ u);
                equal1 x; equal2 x; equal3 x; refine_ u in
              built_equations h1 env1 body middle (refine_ u);
              let cs = constraints body in constraints_equal middle rho2 tau equal3 cs (refine_ u);
              let refine_ u = use tau next equal (refine_ u) in refine_ u in
            let refine_ u = with_allocation_model middle trees2 rho2 model2 p payload target after
              (refine_ u) claim consume3 in refine_ u in
          let refine_ u = with_generation_model h1 trees1 env1 body middle rho1 model1 b db
            (refine_ u) claim consume2 in refine_ u in
        let refine_ u = with_allocation_model h trees rho model arg v a h1
          (refine_ u) claim consume1 in refine_ u
      | _ -> refine_ u)
    | _ -> refine_ u)
  | GRec (arg, result, p, body) -> (match d with
    | Recursion (a, b, db) ->
      let h1 = H.put h arg Var in let h2 = H.put h1 result Var in
      let arrow = Arrow (arg, result) in let h3 = H.put h2 p arrow in
      let rest = Bind (p, env) in let env1 = Bind (arg, rest) in
      let v = Var in allocatable_def h v; allocatable_def h1 v; allocatable_def h2 arrow;
      describes_def rho v a;
      let trees1 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h1 x then finite h1 t else H.at h1 x === None)} @ immutable total = fun x ->
        let u = () in let refine_ t = allocation_finite_at h trees arg v x (refine_ u) in refine_ t in
      let trees2 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h2 x then finite h2 t else H.at h2 x === None)} @ immutable total = fun x ->
        let u = () in let refine_ t = allocation_finite_at h1 trees1 result v x (refine_ u) in refine_ t in
      let trees3 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h3 x then finite h3 t else H.at h3 x === None)} @ immutable total = fun x ->
        let u = () in let refine_ t = allocation_finite_at h2 trees2 p arrow x (refine_ u) in refine_ t in
      let consume1 : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation h1 rho1 x})) @ total ->
          (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
          {u : unit | rho1 arg === a} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
        let refine_ fit1 = fit1 in let u = () in describes_def rho1 v b;
        let consume2 : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (model2 : ((x : node Pref.t) @ immutable -> {u : unit | equation h2 rho2 x})) @ total ->
            (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
            {u : unit | rho2 result === b} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit2 ->
          let refine_ fit2 = fit2 in let u = () in equal2 arg;
          describes_def rho2 arrow target;
          let consume3 : ((rho3 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
              (model3 : ((x : node Pref.t) @ immutable -> {u : unit | equation h3 rho3 x})) @ total ->
              (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || rho3 x === rho2 x})) @ total ->
              {u : unit | rho3 p === target} -> {u : unit | claim}) @ total = fun rho3 model3 equal3 fit3 ->
            let refine_ fit3 = fit3 in let u = () in equal3 arg; equal3 result;
            let from0 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho3 x === rho x}
                @ total = fun x -> equal1 x; equal2 x; equal3 x; let u = () in refine_ u in
            context_equal h rho rho3 from0 env (refine_ u);
            context_of_def rho3 rest; context_of_def rho3 env1;
            let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem h3 x}
                @ total = fun x -> let u = () in refine_ u in
            env_frame h h3 keep env (refine_ u);
            env_allocated_def h3 rest; env_allocated_def h3 env1;
            let consume4 : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
                (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
                (equal4 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h3 x) || tau x === rho3 x})) @ total ->
                {u : unit | satisfies tau (constraints body) && tau (root body) === b} ->
                {u : unit | claim}) @ total = fun tau next equal4 fit4 ->
              let refine_ fit4 = fit4 in let u = () in equal4 result; equal4 p;
              let equal : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}
                  @ total = fun x -> keep x; from0 x; equal4 x; let u = () in refine_ u in
              let last = Equal (root body, result) in satisfies_def tau last;
              let cs = constraints g in satisfies_def tau cs;
              let refine_ u = use tau next equal (refine_ u) in refine_ u in
            let refine_ u = with_generation_model h3 trees3 env1 body after rho3 model3 b db
              (refine_ u) claim consume4 in refine_ u in
          let refine_ u = with_allocation_model h2 trees2 rho2 model2 p arrow target h3
            (refine_ u) claim consume3 in refine_ u in
        let refine_ u = with_allocation_model h1 trees1 rho1 model1 result v b h2
          (refine_ u) claim consume2 in refine_ u in
      let refine_ u = with_allocation_model h trees rho model arg v a h1
        (refine_ u) claim consume1 in refine_ u
    | _ -> refine_ u)
  | GApp (f, a, p, arrow, h1, h2) -> (match d with
    | Application (argty, df, da) ->
      let ft = TArrow (argty, target) in
      let trees1 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h1 x then finite h1 t else H.at h1 x === None)} @ immutable total = fun x ->
        let u = () in let refine_ t = built_finite_at h trees env f h1 x (refine_ u) in refine_ t in
      let trees2 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h2 x then finite h2 t else H.at h2 x === None)} @ immutable total = fun x ->
        let u = () in let refine_ t = built_finite_at h1 trees1 env a h2 x (refine_ u) in refine_ t in
      let h3 = H.put h2 p Var in let v = Var in allocatable_def h2 v;
      let trees3 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h3 x then finite h3 t else H.at h3 x === None)} @ immutable total = fun x ->
        let u = () in let refine_ t = allocation_finite_at h2 trees2 p v x (refine_ u) in refine_ t in
      let consume1 : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation h1 rho1 x})) @ total ->
          (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
          {u : unit | satisfies rho1 (constraints f) && rho1 (root f) === ft} ->
          {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
        let refine_ fit1 = fit1 in let u = () in
        context_equal h rho rho1 equal1 env (refine_ u);
        let keep1 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem h1 x}
            @ total = fun x -> let u = () in built_frame h env f h1 x (refine_ u); refine_ u in
        env_frame h h1 keep1 env (refine_ u);
        let consume2 : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (model2 : ((x : node Pref.t) @ immutable -> {u : unit | equation h2 rho2 x})) @ total ->
            (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
            {u : unit | satisfies rho2 (constraints a) && rho2 (root a) === argty} ->
            {u : unit | claim}) @ total = fun rho2 model2 equal2 fit2 ->
          let refine_ fit2 = fit2 in let u = () in describes_def rho2 v target;
          let consume3 : ((rho3 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
              (model3 : ((x : node Pref.t) @ immutable -> {u : unit | equation h3 rho3 x})) @ total ->
              (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || rho3 x === rho2 x})) @ total ->
              {u : unit | rho3 p === target} -> {u : unit | claim}) @ total = fun rho3 model3 equal3 fit3 ->
            let refine_ fit3 = fit3 in let u = () in
            let ra = root a in built_frame h1 env a h2 ra (refine_ u); equal3 ra;
            let payload = Arrow (ra, p) in allocatable_def h3 payload; describes_def rho3 payload ft;
            let consume4 : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
                (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
                (equal4 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h3 x) || tau x === rho3 x})) @ total ->
                {u : unit | tau arrow === ft} -> {u : unit | claim}) @ total = fun tau next equal4 fit4 ->
              let refine_ fit4 = fit4 in let u = () in
              let from1 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || tau x === rho1 x}
                  @ total = fun x -> let u = () in
                built_frame h1 env a h2 x (refine_ u);
                equal2 x; equal3 x; equal4 x; refine_ u in
              let from2 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || tau x === rho2 x}
                  @ total = fun x -> equal3 x; equal4 x; let u = () in refine_ u in
              let equal : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}
                  @ total = fun x -> keep1 x; equal1 x; from1 x; let u = () in refine_ u in
              built_equations h env f h1 (refine_ u); built_equations h1 env a h2 (refine_ u);
              let cf = constraints f in let ca = constraints a in
              constraints_equal h1 rho1 tau from1 cf (refine_ u);
              constraints_equal h2 rho2 tau from2 ca (refine_ u);
              let rf = root f in built_frame h env f h1 rf (refine_ u); from1 rf; equal4 p;
              let pair = And (cf, ca) in let last = Equal (rf, arrow) in
              satisfies_def tau pair; satisfies_def tau last;
              let cs = constraints g in satisfies_def tau cs;
              let refine_ u = use tau next equal (refine_ u) in refine_ u in
            let refine_ u = with_allocation_model h3 trees3 rho3 model3 arrow payload ft after
              (refine_ u) claim consume4 in refine_ u in
          let refine_ u = with_allocation_model h2 trees2 rho2 model2 p v target h3
            (refine_ u) claim consume3 in refine_ u in
        let refine_ u = with_generation_model h1 trees1 env a h2 rho1 model1 argty da
          (refine_ u) claim consume2 in refine_ u in
      let refine_ u = with_generation_model h trees env f h1 rho model ft df
        (refine_ u) claim consume1 in refine_ u
    | _ -> refine_ u))
