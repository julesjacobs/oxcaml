(* The positive gate links and runs.

   What a run adds over a compile is narrower than it first looks, and worth
   stating so the gate is not credited with more than it does.  Every
   implementation here is verified in this same test, so any behavioural
   mutation that breaks an exported law is refused while its module is being
   compiled and never reaches the run.  What verification cannot see is a
   functor body that never linked, an exception at instantiation, a
   divergence between a modelled primitive and the executed one -- and
   mistakes in this file.  That last one is the gate's main live job.

   Which is why the rows carry a shape column.  Every implementation answers
   every question in [SET] identically; that is what [SET] is for.  So a row
   of extensional answers is the same row for all four, the labels are
   prose, and nothing would tie the row labelled [avl] to [Gen_avl] -- an
   implementation could stop being exercised altogether with this gate still
   green.  [shape] is the one observation that differs between them, so each
   row below is that implementation's own fact rather than one fact printed
   eight times.

   Each row is: the extensional answers, then the keys in representation
   order.  A wrong row now looks like one of three things -- a changed
   answer digit, a shape belonging to a different implementation, or a
   shape in a different key order. *)

let flag value = if value then '1' else '0'

module Probe (S : sig
  include Key_intf.SET

  val shape : t -> key list
end) =
struct
  (* Insert the keys in the given order, then report: each inserted key is a
     member; an absent key is not; the absent key is not a member of
     [empty]; inserting in the reverse order gives an [equal] set; and
     re-inserting a key already present leaves it a member.  Then the
     representation, printed by [render]. *)
  let run name keys absent render =
    let insert_all order =
      List.fold_left (fun set key -> S.insert key set) S.empty order
    in
    let forwards = insert_all keys in
    let backwards = insert_all (List.rev keys) in
    let repeated = S.insert (List.hd keys) forwards in
    print_string name;
    print_char ' ';
    List.iter (fun key -> print_char (flag (S.member key forwards))) keys;
    print_char (flag (S.member absent forwards));
    print_char (flag (S.member absent S.empty));
    print_char ' ';
    print_char (flag (S.equal forwards backwards));
    print_char (flag (S.member (List.hd keys) repeated));
    print_char ' ';
    List.iter (fun key -> print_string (render key)) (S.shape forwards);
    print_newline ()
end

(* Integer keys, inserted in an order that is neither ascending nor
   descending, so that the unbalanced and the balanced tree are built into
   different shapes. *)
let int_keys = [ 3; 1; 4; 7; 2; 6; 5 ]
let render_int key = string_of_int key

let () =
  let module P = Probe (Gen_ulist.Make (Int_key)) in
  P.run "ulist  int " int_keys 9 render_int

let () =
  let module P = Probe (Gen_bst.Make (Int_key)) in
  P.run "bst    int " int_keys 9 render_int

let () =
  let module P = Probe (Gen_avl.Make (Int_key)) in
  P.run "avl    int " int_keys 9 render_int

let () =
  let module P = Probe (Gen_sorted.Make (Int_key)) in
  P.run "sorted int " int_keys 9 render_int

(* Pair keys, lexicographic.  Their lexicographic ranks are the integer keys
   above, so the two halves of the table build the same shapes and the
   tree rows separate for the same reason.

   The pairs are also chosen so that major-minor and minor-major
   lexicographic disagree completely: ascending on the first is
   (0,3) (0,5) (1,1) (1,2) (1,6) (2,0) (2,4), and on the second it is
   (2,0) (1,1) (1,2) (0,3) (2,4) (0,5) (1,6).  So the shape column reports
   which order the key actually supplied, and reversing [Pair_key.compare]
   changes the sorted row and both tree rows.

   The extensional answers cannot report that, and it would be wrong to
   claim they could: a set's behaviour under [empty], [insert], [member] and
   [equal] does not depend on WHICH total order the key supplies, only that
   it supplies one.  Those digits are identical under either order.  What
   they would catch is an alternative that is not a total order on these
   keys -- comparing on [major] alone ties (1,1) with (1,2). *)
let pair major minor : Pair_key.t = { major; minor }

(* Ranks 1 to 7 in lexicographic order. *)
let ranked =
  [| pair 0 3; pair 0 5; pair 1 1; pair 1 2; pair 1 6; pair 2 0; pair 2 4 |]
let pair_keys = List.map (fun rank -> ranked.(rank - 1)) int_keys

let render_pair (key : Pair_key.t) =
  string_of_int key.major ^ string_of_int key.minor

let () =
  let module P = Probe (Gen_ulist.Make (Pair_key)) in
  P.run "ulist  pair" pair_keys (pair 3 3) render_pair

let () =
  let module P = Probe (Gen_bst.Make (Pair_key)) in
  P.run "bst    pair" pair_keys (pair 3 3) render_pair

let () =
  let module P = Probe (Gen_avl.Make (Pair_key)) in
  P.run "avl    pair" pair_keys (pair 3 3) render_pair

let () =
  let module P = Probe (Gen_sorted.Make (Pair_key)) in
  P.run "sorted pair" pair_keys (pair 3 3) render_pair
