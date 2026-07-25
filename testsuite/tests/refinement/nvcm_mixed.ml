(* The claim is false.  Compiling this implementation without verification
   against a verified interface must be refused: the interface's .cmi is
   already written and unmarked, so nothing would record that these
   refinements went unchecked. *)
let value : int{ _ = 0 } = 1
