; Regression guard for the model-completion node blowup (codex review item (e)). A binary
; recursive datatype (node has TWO Tree fields) with many pairwise-distinct free classes:
; the free-class completion assigns them distinct-index spine values. Recursing EVERY
; self-sorted field of `node` made distinct_base(idx) a 2^idx-node tree, so ~30 distinct
; classes built ~2^30 nodes before the depth guard — an effective hang/OOM (binary recursive
; datatypes are exactly the VC-like shape). The fix spines only ONE self field (O(idx) nodes)
; and adds a total-node budget that degrades to unknown; this now completes in <100ms.
; Trivially sat (infinite Tree domain). Checked sat.
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((Tree 0)) (((node (l Tree) (r Tree)) (leaf))))
(declare-fun t1 () Tree)(declare-fun t2 () Tree)(declare-fun t3 () Tree)
(declare-fun t4 () Tree)(declare-fun t5 () Tree)(declare-fun t6 () Tree)
(declare-fun t7 () Tree)(declare-fun t8 () Tree)(declare-fun t9 () Tree)
(declare-fun t10 () Tree)(declare-fun t11 () Tree)(declare-fun t12 () Tree)
(declare-fun t13 () Tree)(declare-fun t14 () Tree)(declare-fun t15 () Tree)
(declare-fun t16 () Tree)(declare-fun t17 () Tree)(declare-fun t18 () Tree)
(declare-fun t19 () Tree)(declare-fun t20 () Tree)(declare-fun t21 () Tree)
(declare-fun t22 () Tree)(declare-fun t23 () Tree)(declare-fun t24 () Tree)
(declare-fun t25 () Tree)(declare-fun t26 () Tree)(declare-fun t27 () Tree)
(declare-fun t28 () Tree)(declare-fun t29 () Tree)(declare-fun t30 () Tree)
(assert (distinct t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30))
(check-sat)
