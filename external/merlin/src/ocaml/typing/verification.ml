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

let termination = ref (fun ~self:_ ~fn:_ ~measure ->
  Location.raise_errorf ~loc:measure.Typedtree.exp_loc
    "Numerical termination verification is unavailable in this compiler")

let install_termination check = termination := check
let check_termination ~self ~fn ~measure = !termination ~self ~fn ~measure
