module B = Wasm_u32
module W = Hmc_word64
module Q = Wasm_word_sequence
let[@def] (layout @ total) (a : W.t @ immutable) (b : W.t @ immutable) (c : W.t @ immutable) (d : W.t @ immutable) = Q.Word (a, Q.Word (b, Q.Word (c, Q.Word (d, Q.End))))
let[@def] (offset0 @ total) (unit : unit) : B.u32 = 0
let[@def] (offset1 @ total) (unit : unit) : B.u32 = 8
let[@def] (offset2 @ total) (unit : unit) : B.u32 = 16
let[@def] (offset3 @ total) (unit : unit) : B.u32 = 24
let (expose @ total) (a : W.t @ immutable) (b : W.t @ immutable) (c : W.t @ immutable) (d : W.t @ immutable) : {u : unit |
    Q.size (Q.End) (offset0 ())
    && Q.append (Q.End) (Q.Word (a, Q.Word (b, Q.Word (c, Q.Word (d, Q.End))))) === layout a b c d
    && Q.size (Q.Word (a, Q.End)) (offset1 ())
    && Q.append (Q.Word (a, Q.End)) (Q.Word (b, Q.Word (c, Q.Word (d, Q.End)))) === layout a b c d
    && Q.size (Q.Word (a, Q.Word (b, Q.End))) (offset2 ())
    && Q.append (Q.Word (a, Q.Word (b, Q.End))) (Q.Word (c, Q.Word (d, Q.End))) === layout a b c d
    && Q.size (Q.Word (a, Q.Word (b, Q.Word (c, Q.End)))) (offset3 ())
    && Q.append (Q.Word (a, Q.Word (b, Q.Word (c, Q.End)))) (Q.Word (d, Q.End)) === layout a b c d} @ ghost = ghost_ (
    layout_def a b c d;
    offset0_def ();
    Q.size_def (Q.End) 0;
    Q.append_def (Q.End) (Q.Word (a, Q.Word (b, Q.Word (c, Q.Word (d, Q.End)))));
    offset1_def ();
    Q.size_def (Q.Word (a, Q.End)) 8;
    Q.append_def (Q.Word (a, Q.End)) (Q.Word (b, Q.Word (c, Q.Word (d, Q.End))));
    Q.size_def (Q.End) 0;
    Q.append_def (Q.End) (Q.Word (b, Q.Word (c, Q.Word (d, Q.End))));
    offset2_def ();
    Q.size_def (Q.Word (a, Q.Word (b, Q.End))) 16;
    Q.append_def (Q.Word (a, Q.Word (b, Q.End))) (Q.Word (c, Q.Word (d, Q.End)));
    Q.size_def (Q.Word (b, Q.End)) 8;
    Q.append_def (Q.Word (b, Q.End)) (Q.Word (c, Q.Word (d, Q.End)));
    Q.size_def (Q.End) 0;
    Q.append_def (Q.End) (Q.Word (c, Q.Word (d, Q.End)));
    offset3_def ();
    Q.size_def (Q.Word (a, Q.Word (b, Q.Word (c, Q.End)))) 24;
    Q.append_def (Q.Word (a, Q.Word (b, Q.Word (c, Q.End)))) (Q.Word (d, Q.End));
    Q.size_def (Q.Word (b, Q.Word (c, Q.End))) 16;
    Q.append_def (Q.Word (b, Q.Word (c, Q.End))) (Q.Word (d, Q.End));
    Q.size_def (Q.Word (c, Q.End)) 8;
    Q.append_def (Q.Word (c, Q.End)) (Q.Word (d, Q.End));
    Q.size_def (Q.End) 0;
    Q.append_def (Q.End) (Q.Word (d, Q.End)))
let[@def] (width @ total) (unit : unit) : B.u32 = 32
let (size @ total) (a : W.t @ immutable) (b : W.t @ immutable) (c : W.t @ immutable) (d : W.t @ immutable) :
    {u : unit | Q.size (layout a b c d) (width ())} @ ghost = ghost_ (
  width_def (); layout_def a b c d;
  Q.size_def (layout a b c d) 32; Q.size_def (Q.Word (b, Q.Word (c, Q.Word (d, Q.End)))) 24;
  Q.size_def (Q.Word (c, Q.Word (d, Q.End))) 16; Q.size_def (Q.Word (d, Q.End)) 8; Q.size_def Q.End 0)
