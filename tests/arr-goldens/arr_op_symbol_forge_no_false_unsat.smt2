(set-logic QF_AX)
(declare-sort I 0)
(declare-sort E 0)
(declare-fun x () (Array I E))
(declare-fun y () I)
(declare-fun z () E)
(declare-fun c () (Array I E))
; a real store keyword over (I,E) registers the internal op symbol
; .oxsmt.arr.store|U:I|U:E (board #58: reserved prefix + '|' sort-key separators). That
; name is UNSPELLABLE in SMT-LIB (a '|' closes any quoted symbol; the simple charset has no
; '|'), and the public declare doors reject both the .oxsmt. prefix and the '|' byte, so it
; cannot be aliased by declaration.
(assert (= c (store x y z)))
; The closest a user can spell is a lookalike in the OLD public prefix, quoted to carry the
; ':' and '.'. It is a distinct, ordinary uninterpreted symbol (op classification is by the
; registered Symbol identity, never by name resemblance), so it is NOT the store operator.
(declare-fun |@arr.store.U:I.U:E| ((Array I E) I E) (Array I E))
(declare-fun a () (Array I E))
(declare-fun i () I)
(declare-fun v () E)
; g := the forged op; g(a,i,v) is UNINTERPRETED, so select(g(a,i,v),i) = v is NOT entailed => SAT
(assert (not (= (select (|@arr.store.U:I.U:E| a i v) i) v)))
(check-sat)
