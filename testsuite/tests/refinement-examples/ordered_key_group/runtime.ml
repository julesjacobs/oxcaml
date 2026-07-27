(* The positive gate links and runs.

   Compiling the family shows that the proofs discharge.  It says nothing
   about the programs those proofs are about: a functor body that never
   linked, or a [member] that disagreed with the [member] the proofs are
   stated over, would be invisible to a compile-only gate.  So every
   implementation is instantiated at both keys, driven, and its answers
   printed.

   Printed rather than asserted, so a wrong answer arrives as a diff against
   [runtime.reference] naming the implementation and the position, rather
   than as a bare non-zero exit. *)

let flag value = if value then '1' else '0'

module Probe (S : Key_intf.SET) = struct
  (* Insert three keys in one order and in the reverse, then report, in this
     order: the three inserted keys are members; an absent key is not; the
     absent key is not a member of [empty]; the two insertion orders are
     [equal]; and re-inserting a key already present leaves it a member.
     Expected line, for every implementation at every key: [11100 11]. *)
  let run name first second third absent =
    let forwards =
      S.insert third (S.insert second (S.insert first S.empty))
    in
    let backwards =
      S.insert first (S.insert second (S.insert third S.empty))
    in
    let repeated = S.insert first forwards in
    print_string name;
    print_char ' ';
    print_char (flag (S.member first forwards));
    print_char (flag (S.member second forwards));
    print_char (flag (S.member third forwards));
    print_char (flag (S.member absent forwards));
    print_char (flag (S.member absent S.empty));
    print_char ' ';
    print_char (flag (S.equal forwards backwards));
    print_char (flag (S.member first repeated));
    print_newline ()
end

(* Integer keys.  The three inserted keys are given out of order so that an
   implementation cannot pass by keeping insertion order. *)
let () =
  let module P = Probe (Gen_ulist.Make (Int_key)) in
  P.run "ulist  int " 3 1 2 9

let () =
  let module P = Probe (Gen_bst.Make (Int_key)) in
  P.run "bst    int " 3 1 2 9

let () =
  let module P = Probe (Gen_avl.Make (Int_key)) in
  P.run "avl    int " 3 1 2 9

let () =
  let module P = Probe (Gen_sorted.Make (Int_key)) in
  P.run "sorted int " 3 1 2 9

(* Pair keys, lexicographic.  The three are chosen so that the lexicographic
   order disagrees with the order of either field taken alone:
   [(0, 5) < (1, 1) < (1, 2)] while [5 > 2 > 1] on the second field.  An
   implementation that reached past [K.compare] to some other order on the
   carrier would answer differently here. *)
let pair major minor : Pair_key.t = { major; minor }

let () =
  let module P = Probe (Gen_ulist.Make (Pair_key)) in
  P.run "ulist  pair" (pair 1 2) (pair 1 1) (pair 0 5) (pair 1 3)

let () =
  let module P = Probe (Gen_bst.Make (Pair_key)) in
  P.run "bst    pair" (pair 1 2) (pair 1 1) (pair 0 5) (pair 1 3)

let () =
  let module P = Probe (Gen_avl.Make (Pair_key)) in
  P.run "avl    pair" (pair 1 2) (pair 1 1) (pair 0 5) (pair 1 3)

let () =
  let module P = Probe (Gen_sorted.Make (Pair_key)) in
  P.run "sorted pair" (pair 1 2) (pair 1 1) (pair 0 5) (pair 1 3)
