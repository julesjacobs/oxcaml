module G = Vox_egraph_rule_handle
module F = Vox_egraph_fixedpoint_spec
let claim (state : G.t @ unique) =
  let #{G.status; fuel = _; state} = G.saturate state 0 1 0 in
  ghost_ (
    let view = borrow_ state in
    let _ : {u : unit | F.fixed (G.model view) (G.rules view)} = () in ());
  state
