(* TEST
 { bytecode; }
 { native; }
*)

type 'a handle

external copy : 'a iarray -> 'a handle = "caml_borrow_of_iarray"
external freeze : 'a handle -> 'a iarray = "caml_borrow_into_iarray"
external open_ : 'a handle -> 'a handle * 'a handle = "caml_borrow_open"
external restore : 'a handle -> 'a handle = "caml_borrow_restore"
external length : 'a handle -> int = "caml_borrow_length"
external get : 'a handle -> int -> 'a = "caml_borrow_get"
external set : 'a handle -> int -> 'a -> 'a handle = "caml_borrow_set"
external snapshot : 'a handle -> 'a iarray = "caml_borrow_snapshot"
external split : 'a handle -> int -> 'a handle * 'a handle * 'a handle
  = "caml_borrow_split"
external recombine : 'a handle -> 'a handle = "caml_borrow_recombine"
external finish : 'a handle -> unit = "caml_borrow_finish"
external owned_split : 'a handle -> int -> 'a handle * 'a handle
  = "caml_borrow_owned_split"
external owned_append : 'a handle -> 'a handle -> 'a handle
  = "caml_borrow_owned_append"

let () =
  let original = [: 1; 2; 3; 4 :] in
  let owner = copy original in
  let root, loan = open_ owner in
  let frame, left, right = split loan 2 in
  let left = set left 1 20 in
  let right = set right 0 30 in
  let snap = snapshot right in
  let right = set right 1 40 in
  finish left;
  finish right;
  finish (recombine frame);
  let result = freeze (restore root) in
  assert (original = [: 1; 2; 3; 4 :]);
  assert (snap = [: 30; 4 :]);
  assert (result = [: 1; 20; 30; 40 :]);
  let empty = copy [: :] in
  let zero = length empty in
  assert (zero = 0);
  let frame, left, right = split empty 0 in
  finish left;
  finish right;
  assert (freeze (recombine frame) = [: :]);
  let floats = copy [: 1.5; 2.5 :] in
  let floats = set floats 1 7.25 in
  let value = get floats 1 in
  assert (value = 7.25);
  assert (freeze floats = [: 1.5; 7.25 :]);
  (* Owners split without copying; adjacent pieces append in place, any
     other pair is copied; freezing a piece copies it out. *)
  let whole = copy [: 1; 2; 3; 4; 5 :] in
  let front, back = owned_split whole 2 in
  let front = set front 0 10 in
  let back = set back 2 50 in
  let joined = owned_append front back in
  assert (length joined = 5);
  let base = freeze joined in
  assert (base = [: 10; 2; 3; 4; 50 :]);
  let whole = copy base in
  let front, back = owned_split whole 3 in
  let swapped = owned_append back front in
  let swapped = set swapped 0 40 in
  assert (freeze swapped = [: 40; 50; 10; 2; 3 :]);
  let whole = copy base in
  let front, back = owned_split whole 1 in
  let middle, back = owned_split back 3 in
  let middle = set middle 0 20 in
  assert (freeze middle = [: 20; 3; 4 :]);
  assert (freeze (owned_append front back) = [: 10; 50 :]);
  assert (base = [: 10; 2; 3; 4; 50 :]);
  let floats = copy [: 1.5; 2.5; 3.5 :] in
  let left, right = owned_split floats 1 in
  assert (freeze (owned_append right left) = [: 2.5; 3.5; 1.5 :]);
  let empty, rest = owned_split (copy [: 7 :]) 0 in
  assert (length empty = 0);
  assert (freeze (owned_append empty rest) = [: 7 :]);
  print_endline "borrow storage: copies, splits, snapshots, empty and float arrays";
  print_endline "owned storage: splits, in-place and copying appends, frozen pieces"
