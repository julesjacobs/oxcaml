module Q = Vox_egraph_match_spec
module L = Vox_egraph_language_spec

let[@def] rec (origin @ total) (graph : Q.graph @ immutable) (id : int) =
  match Q.node graph id with
  | None -> None
  | Some (Q.Int_lit value) -> Some (L.Int_lit value)
  | Some (Q.Bool_lit value) -> Some (L.Bool_lit value)
  | Some Q.Int_input -> Some L.Int_input
  | Some Q.Bool_input -> Some L.Bool_input
  | Some (Q.Add (a, b)) ->
    if 0 <= a && a < id && 0 <= b && b < id then
      (match origin graph a, origin graph b with
       | Some a, Some b -> Some (L.Add (a, b)) | _ -> None)
    else None
  | Some (Q.Eq_int (a, b)) ->
    if 0 <= a && a < id && 0 <= b && b < id then
      (match origin graph a, origin graph b with
       | Some a, Some b -> Some (L.Eq_int (a, b)) | _ -> None)
    else None
  | Some (Q.Int_if (c, a, b)) ->
    if 0 <= c && c < id && 0 <= a && a < id && 0 <= b && b < id then
      (match origin graph c, origin graph a, origin graph b with
       | Some c, Some a, Some b -> Some (L.Int_if (c, a, b)) | _ -> None)
    else None
  | Some (Q.Bool_if (c, a, b)) ->
    if 0 <= c && c < id && 0 <= a && a < id && 0 <= b && b < id then
      (match origin graph c, origin graph a, origin graph b with
       | Some c, Some a, Some b -> Some (L.Bool_if (c, a, b)) | _ -> None)
    else None
  [@@decreases if id > 0 then id else 0]
