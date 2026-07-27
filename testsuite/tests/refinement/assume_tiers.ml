(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-c";
 compiler_output = "assume_tiers.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/assume_tiers.reference";
 check-ocamlc.byte-output;
*)

(* Which admissions get a run-time check, and which do not.

   Nothing here is about acceptance: every one of these compiles.  The
   reference file is the whole assertion, and what it records is which
   predicates a run could refute. *)

type vec

external length : vec @ logical -> int @@ total = "vec_length"

(* Checked.  The widening: an uninterpreted function is modelled as being a
   function, so running it agrees with the model when it is one, and the
   comparison is at [int]. *)
let (length_nonneg_law @ total) (v : vec) : unit{ length v >= 0 } = assume ()

(* Checked.  [max_int] and [min_int] are the values the model says they are,
   under either name. *)
let (bounds_law @ total) (n : int) : unit{ min_int <= n && n <= max_int } =
  assume ()

let (int_bounds_law @ total) () : unit{ Int.max_int > Int.min_int } =
  assume ()

(* Checked.  A refined value that is a name is read again where the predicate
   mentions it, which observes nothing and needs no binding. *)
let named_subject (y : int) = (assume y : int{ _ > 0 })

(* Checked.  [int] crosses logicality, so a logical one is readable and the
   check the mode checker accepts is a check that can run. *)
let (logical_int_law @ total) (n : int @ logical) : unit{ n <= max_int } =
  assume ()

(* Checked, and reading a value whose consumer declares its parameter
   logical.  That is not the tier reaching past the mode checker: the check
   is typed as ordinary code, so it can read exactly what an ordinary call
   could, and an ordinary [length v] here would read the same thing. *)
let (logical_vec_law @ total) (v : vec @ logical) : unit{ length v >= 0 } =
  assume ()

(* Unchecked.  Equality at [string] is not the equality the backends reason
   about, so a run of it would not be a run of the statement. *)
let (string_law @ total) (s : string) : unit{ s = s } = assume ()

(* Unchecked.  The shifts are left undefined past the word width, so the
   machine and the model can disagree about them. *)
let (shift_law @ total) (n : int{ 0 <= _ && _ < 8 }) : unit{ 1 lsl n > 0 } =
  assume ()

(* Unchecked.  A refined value that is not a name would have to be bound to
   be read twice, and a binding the mode checker never saw is not worth a
   check. *)
let computed_subject (y : int) = (assume (y + 1) : int{ _ > 0 })
