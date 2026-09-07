open Types

type t = Int | Bool

let classify env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tconstr (path, [], _) when Path.same path Predef.path_int -> Some Int
  | Tconstr (path, [], _) when Path.same path Predef.path_bool -> Some Bool
  | _ -> None

let rec classify_payload env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tpoly (ty, []) -> classify_payload env ty
  | Trefine refinement -> classify_payload env refinement.ref_payload
  | _ -> classify env ty
