module Q = Vox_egraph_match_spec
module R = Vox_egraph_rule_spec
module O = Vox_egraph_match_observation
module I = Vox_iarray
module M = Vox_egraph_union_spec

let (in_classes @ total) :
    (nodes : Q.node option iarray) @ immutable ->
    (parents : int iarray) @ immutable ->
    (count : {n : int | 0 <= n && n <= Iarray.length parents}) ->
    (id : int) -> (classes : int list) @ immutable ->
    {b : bool | b = Q.in_classes (O.observe nodes parents count) id classes} =
  fun nodes parents count id classes ->
    ghost_ (Q.in_classes_def (O.observe nodes parents count) id classes);
    if 0 <= id && id < count then (
      ghost_ (O.class_at nodes parents count id ());
      Q.member (M.root parents id) classes)
    else (
      ghost_ (
        O.observe_def nodes parents count;
        O.labels_length parents count;
        Q.class_id_def (O.observe nodes parents count) id);
      false)

let (layer @ total) :
    (nodes : Q.node option iarray) @ immutable ->
    (parents : int iarray) @ immutable ->
    (count : {n : int | 0 <= n && n <= Iarray.length parents}) ->
    (pat : R.pat) @ immutable ->
    (first : int list) @ immutable -> (second : int list) @ immutable ->
    (third : int list) @ immutable -> (value : Q.node) @ immutable ->
    {b : bool | b = Q.layer (O.observe nodes parents count)
      pat first second third value} =
  fun nodes parents count pat first second third value ->
    ghost_ (Q.layer_def (O.observe nodes parents count) pat first second third value);
    match pat, value with
    | R.Int_lit a, Q.Int_lit b -> (a = b)
    | R.Bool_lit a, Q.Bool_lit b -> (a = b)
    | R.Int_input, Q.Int_input | R.Bool_input, Q.Bool_input -> true
    | R.Add _, Q.Add (a, b) | R.Eq_int _, Q.Eq_int (a, b) ->
      let a = in_classes nodes parents count a first in
      let b = in_classes nodes parents count b second in
      (a && b)
    | R.Int_if _, Q.Int_if (c, a, b) | R.Bool_if _, Q.Bool_if (c, a, b) ->
      let c = in_classes nodes parents count c first in
      let a = in_classes nodes parents count a second in
      let b = in_classes nodes parents count b third in
      (c && a && b)
    | _ -> false

let rec (find_layer @ total) :
    (nodes : Q.node option iarray) @ immutable ->
    (parents : int iarray) @ immutable ->
    (allocated : {n : int | 0 <= n && n <= Iarray.length nodes &&
      n <= Iarray.length parents}) ->
    (pat : R.pat) @ immutable ->
    (first : int list) @ immutable -> (second : int list) @ immutable ->
    (third : int list) @ immutable -> (label : int) ->
    (count : {n : int | n <= allocated}) ->
    {r : int option |
      let graph = O.observe nodes parents allocated in
      (match r with
       | None -> not (Q.member label (Q.collect graph pat first second third count))
       | Some id -> 0 <= id && id < count &&
         Q.class_id graph id === Some label &&
         (match Q.node graph id with
          | None -> false
          | Some node -> Q.layer graph pat first second third node) &&
         Q.member label (Q.collect graph pat first second third count))} =
  fun nodes parents allocated pat first second third label count ->
    let graph = ghost_ (O.observe nodes parents allocated) in
    ghost_ (Q.collect_def graph pat first second third count);
    if count <= 0 then (
      ghost_ (Q.member_def label []);
      None)
    else
      let id = count - 1 in
      ghost_ (
        O.node_at nodes parents allocated id ();
        O.class_at nodes parents allocated id ());
      match I.at nodes id with
      | Some (Some node) ->
        let found = M.root parents id in
        if layer nodes parents allocated pat first second third node then (
          ghost_ (Q.member_def label
            (found :: Q.collect graph pat first second third id));
          if found = label then (Some id)
          else find_layer nodes parents allocated pat first second third label (id))
        else find_layer nodes parents allocated pat first second third label (id)
      | _ -> find_layer nodes parents allocated pat first second third label (id)
  [@@decreases if count > 0 then count else 0]

let rec (collect @ total) :
    (nodes : Q.node option iarray) @ immutable ->
    (parents : int iarray) @ immutable ->
    (allocated : {n : int | 0 <= n && n <= Iarray.length nodes &&
      n <= Iarray.length parents}) ->
    (pat : R.pat) @ immutable ->
    (first : int list) @ immutable -> (second : int list) @ immutable ->
    (third : int list) @ immutable ->
    (count : {n : int | n <= allocated}) ->
    {r : int list | r === Q.collect (O.observe nodes parents allocated)
      pat first second third count} @ immutable =
  fun nodes parents allocated pat first second third count ->
    let graph = ghost_ (O.observe nodes parents allocated) in
    ghost_ (Q.collect_def graph pat first second third count);
    if count <= 0 then []
    else
      let id = count - 1 in
      let rest = collect nodes parents allocated pat first second third (id) in
      ghost_ (
        O.node_at nodes parents allocated id ();
        O.class_at nodes parents allocated id ());
      match I.at nodes id with
      | Some (Some node) ->
        if layer nodes parents allocated pat first second third node then
          (M.root parents id :: rest)
        else rest
      | _ -> rest
  [@@decreases if count > 0 then count else 0]

let rec (classes @ total) :
    (nodes : Q.node option iarray) @ immutable ->
    (parents : int iarray) @ immutable ->
    (count : {n : int | 0 <= n && n <= Iarray.length nodes &&
      n <= Iarray.length parents}) ->
    (pat : R.pat) @ immutable -> (bindings : int list) @ immutable ->
    {r : int list | r === Q.classes (O.observe nodes parents count) pat bindings}
      @ immutable =
  fun nodes parents count pat bindings ->
    let graph = ghost_ (O.observe nodes parents count) in
    ghost_ (O.observe_def nodes parents count;
      Q.classes_def graph pat bindings);
    match pat with
    | R.Var index ->
      (match Q.binding bindings index with
       | None -> []
       | Some id ->
         if 0 <= id && id < count then (
           ghost_ (O.class_at nodes parents count id ());
           [M.root parents id])
         else (
           ghost_ (
             O.observe_def nodes parents count;
             O.labels_length parents count;
             Q.class_id_def graph id);
           []))
    | R.Add (left, right) | R.Eq_int (left, right) ->
      let first = classes nodes parents count left bindings in
      let second = classes nodes parents count right bindings in
      let result = collect nodes parents count pat first second [] count in
      ghost_ (O.observe_def nodes parents count;
      Q.classes_def graph pat bindings);
      result
    | R.Int_if (condition, yes, no) | R.Bool_if (condition, yes, no) ->
      let first = classes nodes parents count condition bindings in
      let second = classes nodes parents count yes bindings in
      let third = classes nodes parents count no bindings in
      let result = collect nodes parents count pat first second third count in
      ghost_ (O.observe_def nodes parents count;
      Q.classes_def graph pat bindings);
      result
    | R.Int_lit _ | R.Bool_lit _ | R.Int_input | R.Bool_input ->
      collect nodes parents count pat [] [] [] count

let (matches @ total) :
    (nodes : Q.node option iarray) @ immutable ->
    (parents : int iarray) @ immutable ->
    (count : {n : int | 0 <= n && n <= Iarray.length nodes &&
      n <= Iarray.length parents}) ->
    (pat : R.pat) @ immutable -> (bindings : int list) @ immutable ->
    (root : int) ->
    {b : bool | b = Q.matches (O.observe nodes parents count) pat bindings root} =
  fun nodes parents count pat bindings root ->
    ghost_ (Q.matches_def (O.observe nodes parents count) pat bindings root);
    let found = classes nodes parents count pat bindings in
    in_classes nodes parents count root found
