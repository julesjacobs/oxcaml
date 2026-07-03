(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "seplist_lib.mli seplist_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* IN-PLACE LIST REVERSAL against the textbook separation-logic
   specification, really proved: [Lseg (l, vs)] owns a null-terminated
   segment holding the model list [vs]; the reflected [rev_append] is
   the specification; and [reverse] rewires next pointers, node by
   node, returning the SAME cells holding the reversed list.  The
   frame is explicit throughout; folds and unfolds go through the
   library ops ([uncons] returns the existential next pointer as a
   value) and the exported fold lemmas.  Every obligation is really
   proved through grind. *)

open Seplist_lib

let rec total_ rev_append (vs : ilist) (ws : ilist) : ilist =
  match vs with
  | INil -> ws
  | ICons (v, vs') -> rev_append vs' (ICons (v, ws))

(* The accumulator loop: [vs] mirrors the unreversed suffix (the model
   list is ordinary data, so the code recurses on it), [acc]/[ws] the
   reversed prefix.  Each step unfolds the head node, rewires it onto
   the accumulator, and folds it into the accumulator's segment. *)
let rec rev_app :
  (l : link) -> (vs : ilist) -> (acc : link) -> (ws : ilist) -> (p : hprop) ->
  htoken{ sat (Star (Lseg (l, vs), Star (Lseg (acc, ws), p))) (hp _) } @ unique ->
  stepped{ sat (Star (Lseg (_.lnk, rev_append vs ws), p)) (hp _.stok) }
    @ unique =
  fun l vs acc ws p t ->
  match vs with
  | INil -> { lnk = acc; stok = t }
  | ICons (v, vs') ->
    (match l with
     | Null ->
       (* dead: a null segment holds no cons; the facts here are
          contradictory *)
       { lnk = acc; stok = t }
     | Ptr r ->
       let q = Star (Lseg (acc, ws), p) in
       let st = uncons r v vs' q t in
       let { lnk = nxt; stok = t } = st in
       let q2 = Star (Lseg (nxt, vs'), q) in
       let t = set_next r v nxt acc q2 t in
       let acc' = Ptr r in
       let ws' = ICons (v, ws) in
       rev_app nxt vs' acc' ws' p t)

let reverse :
  (l : link) -> (vs : ilist) ->
  htoken{ sat (Star (Lseg (l, vs), Emp)) (hp _) } @ unique ->
  stepped{ sat (Star (Lseg (_.lnk, rev_append vs INil), Emp)) (hp _.stok) }
    @ unique =
  fun l vs t ->
  let e = Emp in
  let n = INil in
  let z = Null in
  rev_app l vs z n e t

(* Drive it: build [1; 2] in the heap, reverse in place, and read the
   values back off the reversed cells -- 2 then 1, proved. *)
let demo () : int{ _ = 21 } =
  let e = Emp in
  let nil = INil in
  let z = Null in
  let t = init () in
  let pr2 = alloc_node e 2 z t in
  let { nod = r2; ntok = t } = pr2 in
  let f2 = Star (Node (r2, 2, Null), e) in
  ignore f2;
  (* fold the singleton: mint an empty segment behind the node *)
  let l2 = ICons (2, nil) in
  let f2' = Star (Lseg (Ptr r2, l2), e) in
  ignore f2';
  let p2 = Ptr r2 in
  let pr1 = alloc_node f2' 1 p2 t in
  let { nod = r1; ntok = t } = pr1 in
  let l12 = ICons (1, l2) in
  let f1 = Star (Lseg (Ptr r1, l12), e) in
  ignore f1;
  let start = Ptr r1 in
  let pres = reverse start l12 t in
  let { lnk = res; stok = t } = pres in
  (* the reversed model list is [2; 1], computed by the reflected
     rev_append; read the two cells back *)
  (match res with
   | Null ->
     ignore t;
     0 (* dead: the reversed list is a cons *)
   | Ptr s2 ->
     let l1 = ICons (1, nil) in
     let ps = uncons s2 2 l1 e t in
     let { lnk = nx; stok = t } = ps in
     let g1 = Star (Lseg (nx, l1), e) in
     let pv = get_val s2 2 nx g1 t in
     let (x, t) = pv in
     (match nx with
      | Null ->
        ignore t;
        0 (* dead: one more cons to go *)
      | Ptr s1 ->
        let g2 = Star (Node (s2, 2, nx), e) in
        let ps' = uncons s1 1 nil g2 t in
        let { lnk = nx'; stok = t } = ps' in
        let g3 = Star (Lseg (nx', nil), g2) in
        let pv' = get_val s1 1 nx' g3 t in
        let (y, t) = pv' in
        ignore nx'; ignore t;
        x * 10 + y))
