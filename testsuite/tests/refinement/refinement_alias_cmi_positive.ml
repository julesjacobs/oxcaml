module Provider = Refinement_alias_cmi_provider

let use_nested () : unit{ Provider.Nested.p = true } =
  Provider.Nested.proof;
  ()

module A = Provider.Make (struct end)
module B = Provider.Make (struct end)

let use_a () : unit{ A.p = true } =
  A.proof;
  ()

let use_b () : unit{ B.p = true } =
  B.proof;
  ()

