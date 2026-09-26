module W = Hmc_word64
module Q = Wasm_word_sequence
module B = Wasm_u32
let[@def] (tag @ total) (unit : unit) : W.t @ immutable = {W.lo = 1; hi = 0}
let[@def] (layout @ total) (pc : W.t @ immutable) (ct : W.t @ immutable) (cp : W.t @ immutable) (at : W.t @ immutable) (ap : W.t @ immutable) = Q.Word (tag (), Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.Word (at, Q.Word (ap, Q.End))))))
let[@def] (prefix4 @ total) (pc : W.t @ immutable) (ct : W.t @ immutable) (cp : W.t @ immutable) = Q.Word (tag (), Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.End))))
let[@def] (prefix5 @ total) (pc : W.t @ immutable) (ct : W.t @ immutable) (cp : W.t @ immutable) (at : W.t @ immutable) = Q.Word (tag (), Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.Word (at, Q.End)))))
let[@def] (rest @ total) (ct : W.t @ immutable) (cp : W.t @ immutable) (at : W.t @ immutable) (ap : W.t @ immutable) = Q.Word (ct, Q.Word (cp, Q.Word (at, Q.Word (ap, Q.End))))
let[@def] (four @ total) (unit : unit) : B.u32 = 32
let[@def] (five @ total) (unit : unit) : B.u32 = 40
let[@def] (one @ total) (unit : unit) : B.u32 = 8
let (expose @ total) (pc : W.t @ immutable) (ct : W.t @ immutable) (cp : W.t @ immutable) (at : W.t @ immutable) (ap : W.t @ immutable) : {u : unit |
    Q.size (prefix4 pc ct cp) (four ()) && Q.size (prefix5 pc ct cp at) (five ())
    && Q.size (Q.Word (tag (), Q.End)) (one ())
    && Q.append (prefix4 pc ct cp) (Q.Word (at, Q.Word (ap, Q.End))) === layout pc ct cp at ap
    && Q.append (prefix5 pc ct cp at) (Q.Word (ap, Q.End)) === layout pc ct cp at ap
    && Q.append (Q.Word (tag (), Q.End)) (Q.Word (pc, rest ct cp at ap)) === layout pc ct cp at ap} @ ghost = ghost_ (
  four_def (); five_def (); one_def ();
  layout_def pc ct cp at ap; prefix4_def pc ct cp; prefix5_def pc ct cp at; rest_def ct cp at ap;
  Q.append_def (Q.Word (tag (), Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.End))))) (Q.Word (at, Q.Word (ap, Q.End)));
  Q.append_def (Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.End)))) (Q.Word (at, Q.Word (ap, Q.End)));
  Q.append_def (Q.Word (ct, Q.Word (cp, Q.End))) (Q.Word (at, Q.Word (ap, Q.End)));
  Q.append_def (Q.Word (cp, Q.End)) (Q.Word (at, Q.Word (ap, Q.End)));
  Q.append_def (Q.End) (Q.Word (at, Q.Word (ap, Q.End)));
  Q.append_def (Q.Word (tag (), Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.Word (at, Q.End)))))) (Q.Word (ap, Q.End));
  Q.append_def (Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.Word (at, Q.End))))) (Q.Word (ap, Q.End));
  Q.append_def (Q.Word (ct, Q.Word (cp, Q.Word (at, Q.End)))) (Q.Word (ap, Q.End));
  Q.append_def (Q.Word (cp, Q.Word (at, Q.End))) (Q.Word (ap, Q.End));
  Q.append_def (Q.Word (at, Q.End)) (Q.Word (ap, Q.End));
  Q.append_def (Q.End) (Q.Word (ap, Q.End));
  Q.append_def (Q.Word (tag (), Q.End)) (Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.Word (at, Q.Word (ap, Q.End))))));
  Q.append_def (Q.End) (Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.Word (at, Q.Word (ap, Q.End))))));
  Q.size_def (Q.Word (tag (), Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.End))))) 32;
  Q.size_def (Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.End)))) 24;
  Q.size_def (Q.Word (ct, Q.Word (cp, Q.End))) 16;
  Q.size_def (Q.Word (cp, Q.End)) 8;
  Q.size_def (Q.End) 0;
  Q.size_def (Q.Word (tag (), Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.Word (at, Q.End)))))) 40;
  Q.size_def (Q.Word (pc, Q.Word (ct, Q.Word (cp, Q.Word (at, Q.End))))) 32;
  Q.size_def (Q.Word (ct, Q.Word (cp, Q.Word (at, Q.End)))) 24;
  Q.size_def (Q.Word (cp, Q.Word (at, Q.End))) 16;
  Q.size_def (Q.Word (at, Q.End)) 8;
  Q.size_def (Q.End) 0;
  Q.size_def (Q.Word (tag (), Q.End)) 8;
  Q.size_def (Q.End) 0)
