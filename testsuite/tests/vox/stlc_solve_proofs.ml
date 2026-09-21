open Unifier_spec
open Unifier_proofs
open Unifier_finite_spec
open Unifier_finite_proofs
open Stlc_spec

let rec (solved_frame @ total) : (h : Pref.heap) @ immutable -> (eqs : equations) @ immutable ->
    (ok : bool) -> (after : Pref.heap) @ immutable -> (d : solving) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | solved h eqs ok after d} ->
    {u : unit | H.mem after x = H.mem h x} @ ghost = fun h eqs ok after d x premise -> ghost_ (
  solved_def h eqs ok after d;
  let u = () in match d with
  | Done -> u
  | Unified ud -> (match eqs with
    | Equal (p, q) -> unified_frame h p q ok after ud x (u); u
    | _ -> u)
  | Sequence (middle, left_ok, left, right) -> (match eqs with
    | And (a, b) ->
      solved_frame h a left_ok middle left x (u);
      if left_ok then (solved_frame middle b ok after right x (u); u) else u
    | _ -> u))

let rec (solved_finite_at @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (eqs : equations) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : solving) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | solved h eqs ok after d} ->
    {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem after x then finite after t else H.at after x === None)} @ immutable ghost =
  fun h trees eqs ok after d x premise -> ghost_ (
    solved_def h eqs ok after d;
    let u = () in match d with
    | Done -> let t = trees x in t
    | Unified ud -> (match eqs with
      | Equal (p, q) -> unified_finite_at h trees p q ok after ud x (u)
      | _ -> let t = Free x in t)
    | Sequence (middle, left_ok, left, right) -> (match eqs with
      | And (a, b) ->
        let middle_trees : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
            (if H.mem middle x then finite middle t else H.at middle x === None)} @ immutable total = fun x ->
          let u = () in let t = solved_finite_at h trees a left_ok middle left x (u) in t in
        if left_ok then solved_finite_at middle middle_trees b ok after right x (u)
        else let t = middle_trees x in t
      | _ -> let t = Free x in t))

let rec (solved_backward_at @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (eqs : equations) @ immutable -> (after : Pref.heap) @ immutable -> (d : solving) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | solved h eqs true after d && satisfies rho eqs} ->
    {u : unit | equation after rho x} @ ghost = fun h rho model eqs after d x premise -> ghost_ (
  solved_def h eqs true after d; satisfies_def rho eqs;
  let u = () in match d with
  | Done -> model x; u
  | Unified ud -> (match eqs with
    | Equal (p, q) -> success_backward_at h rho model p q after ud x (u)
    | _ -> u)
  | Sequence (middle, left_ok, left, right) -> (match eqs with
    | And (a, b) ->
      let mid : (x : node Pref.t) @ immutable -> {u : unit | equation middle rho x}
          @ total = fun x -> let u = () in
        let u = solved_backward_at h rho model a middle left x (u) in u in
      solved_backward_at middle rho mid b after right x (u)
    | _ -> u))

let rec (solved_forward_at @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eqs : equations) @ immutable -> (after : Pref.heap) @ immutable -> (d : solving) @ immutable ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | solved h eqs true after d} ->
    {u : unit | equation h rho x && satisfies rho eqs} @ ghost = fun h rho eqs after d model x premise -> ghost_ (
  solved_def h eqs true after d; satisfies_def rho eqs;
  let u = () in match d with
  | Done -> model x; u
  | Unified ud -> (match eqs with
    | Equal (p, q) -> success_forward_at h rho p q after ud model x (u); u
    | _ -> u)
  | Sequence (middle, left_ok, left, right) -> (match eqs with
    | And (a, b) ->
      let mid : (x : node Pref.t) @ immutable -> {u : unit | equation middle rho x}
          @ total = fun x -> let u = () in
        let u = solved_forward_at middle rho b after right model x (u) in u in
      solved_forward_at middle rho b after right model x (u);
      solved_forward_at h rho a middle left mid x (u); u
    | _ -> u))

let rec (solved_refutes @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (eqs : equations) @ immutable -> (after : Pref.heap) @ immutable -> (d : solving) @ immutable ->
    {u : unit | solved h eqs false after d && satisfies rho eqs} -> {u : unit | false} @ ghost =
  fun h rho model eqs after d premise -> ghost_ (
    solved_def h eqs false after d; satisfies_def rho eqs;
    let u = () in match d with
    | Done -> u
    | Unified ud -> (match eqs with
      | Equal (p, q) -> failure_refutes h rho model p q after ud (u)
      | _ -> u)
    | Sequence (middle, left_ok, left, right) -> (match eqs with
      | And (a, b) -> if left_ok then (
        let mid : (x : node Pref.t) @ immutable -> {u : unit | equation middle rho x}
            @ total = fun x -> let u = () in
          let u = solved_backward_at h rho model a middle left x (u) in u in
        solved_refutes middle rho mid b after right (u))
        else solved_refutes h rho model a middle left (u)
      | _ -> u))
