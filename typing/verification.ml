let unavailable structure =
  let iterator =
    { Tast_iterator.default_iterator with
      expr = (fun self expression ->
        List.iter (function
          | Typedtree.Texp_refine, loc, _ ->
            Location.raise_errorf ~loc
              "Refinement verification is unavailable in this compiler"
          | _ -> ()) expression.Typedtree.exp_extra;
        Tast_iterator.default_iterator.expr self expression)
    }
  in
  iterator.structure iterator structure

let verifier = ref unavailable
let install verify = verifier := verify
let run structure = !verifier structure
