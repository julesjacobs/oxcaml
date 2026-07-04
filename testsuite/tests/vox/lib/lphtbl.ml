(* Implementation of lphtbl.mli: a LINEAR-PROBING hash table over the
   POLYMORPHIC slices (lib/pslice) at element type int -- parallel
   keys/values arrays, keys non-negative with -1 the empty sentinel.
   Each operation opens nested borrow brackets (one loan per array)
   and runs a fuel-8 probe loop whose per-arm contract is EXACTLY the
   model's Int-fueled unfolding ([pfI]/[pikI]/[pivI]); the terminal
   arms resolve both loans, and the returned refinement carries the
   prophecies' values out.  Partial correctness: no termination
   obligation exists or is needed -- the fuel bounds the loop by
   construction, and probing 8 slots IS the model's semantics.
   Nothing here is assumed (the trust boundary is pslice's). *)

open Pslice

type opt =
  | Missing
  | Found of int

(* The fuel-8 probe-find loop: reads the key slot [i mod 8]; empty
   means Missing, a key match reads the value, otherwise advance.
   Terminal arms resolve BOTH loans so the result escapes the
   brackets globally; the facts [pfin = pnow] ride the refinement. *)
let rec probe :
  (f : int) -> (i : int{ 0 <= _ }) -> (k : int) ->
  (mk : int slice{ plen (pnow _) = 8 }) @ local unique ->
  (mv : int slice{ plen (pnow _) = 8 }) @ local unique ->
  opt{ _ = pfI f i k (pnow mk) (pnow mv)
       && pfin mk = pnow mk && pfin mv = pnow mv } @ unique =
  fun f i k mk mv ->
    if f <= 0
    then begin
      let _u1 = sdrop mk in
      let _u2 = sdrop mv in
      Missing
    end
    else begin
      let s : int{ _ = i mod 8 && 0 <= _ && _ < 8 } = refine_ (i mod 8) in
      let (x, mk1) = sget mk s in
      if x = -1
      then begin
        let _u1 = sdrop mk1 in
        let _u2 = sdrop mv in
        Missing
      end
      else if x = k
      then begin
        let (v, mv1) = sget mv s in
        let _u1 = sdrop mk1 in
        let _u2 = sdrop mv1 in
        Found v
      end
      else begin
        let r = probe (f - 1) (i + 1) k mk1 mv in
        r
      end
    end

(* The fuel-8 probe-insert loop: writes key and value at the first
   free or matching slot; the resolved loans' finals are exactly the
   model's [pikI]/[pivI]. *)
let rec ins :
  (f : int) -> (i : int{ 0 <= _ }) -> (k : int) -> (v : int) ->
  (mk : int slice{ plen (pnow _) = 8 }) @ local unique ->
  (mv : int slice{ plen (pnow _) = 8 }) @ local unique ->
  unit{ pfin mk = pikI f i k (pnow mk)
        && pfin mv = pivI f i k v (pnow mk) (pnow mv) } @ unique =
  fun f i k v mk mv ->
    if f <= 0
    then begin
      let _u1 = sdrop mk in
      let _u2 = sdrop mv in
      ()
    end
    else begin
      let s : int{ _ = i mod 8 && 0 <= _ && _ < 8 } = refine_ (i mod 8) in
      let (x, mk1) = sget mk s in
      if x = -1 || x = k
      then begin
        let mk2 = sset mk1 s k in
        let mv1 = sset mv s v in
        let _u1 = sdrop mk2 in
        let _u2 = sdrop mv1 in
        ()
      end
      else begin
        let r = ins (f - 1) (i + 1) k v mk1 mv in
        r
      end
    end

let create :
  unit ->
  (int varr{ wf (pcts _) && pconst (pcts _) (-1) }
   * int varr{ plen (pcts _) = 8 }) @ unique =
  fun () ->
    let e = -1 in
    let ks = pnew 8 e in
    let vs = pnew 8 0 in
    ( (ks : int varr{ wf (pcts _) && pconst (pcts _) (-1) }),
      (vs : int varr{ plen (pcts _) = 8 }) )

let find :
  (k : int{ 0 <= _ }) ->
  (ks : int varr{ wf (pcts _) }) @ unique ->
  (vs : int varr{ plen (pcts _) = 8 }) @ unique ->
  (opt{ _ = pfind k (pcts ks) (pcts vs) }
   * int varr{ pcts _ = pcts ks && wf (pcts _) }
   * int varr{ pcts _ = pcts vs && plen (pcts _) = 8 }) @ unique =
  fun k ks vs ->
    let pk = new_proph () in
    let pv = new_proph () in
    let i0 : int{ _ = home k && 0 <= _ } = refine_ (k mod 8) in
    let (ks', (vs', r)) =
      borrow pk ks (fun mk ->
        let r =
          borrow pv vs (fun mv ->
            (probe 8 i0 k mk mv
              : opt{ _ = pfind k (pcts ks) (pcts vs)
                     && ppv pk = pcts ks && ppv pv = pcts vs }))
        in
        r)
    in
    ( (r : opt{ _ = pfind k (pcts ks) (pcts vs) }),
      (ks' : int varr{ pcts _ = pcts ks && wf (pcts _) }),
      (vs' : int varr{ pcts _ = pcts vs && plen (pcts _) = 8 }) )

let add :
  (k : int{ 0 <= _ }) -> (v : int) ->
  (ks : int varr{ wf (pcts _) }) @ unique ->
  (vs : int varr{ plen (pcts _) = 8 }) @ unique ->
  (int varr{ pcts _ = pinsk k v (pcts ks) && wf (pcts _) }
   * int varr{ pcts _ = pinsv k v (pcts ks) (pcts vs) && plen (pcts _) = 8 })
    @ unique =
  fun k v ks vs ->
    let pk = new_proph () in
    let pv = new_proph () in
    let i0 : int{ _ = home k && 0 <= _ } = refine_ (k mod 8) in
    let (ks', (vs', u)) =
      borrow pk ks (fun mk ->
        let r =
          borrow pv vs (fun mv ->
            (ins 8 i0 k v mk mv
              : unit{ ppv pk = pinsk k v (pcts ks)
                      && ppv pv = pinsv k v (pcts ks) (pcts vs) }))
        in
        r)
    in
    ignore u;
    ( (ks' : int varr{ pcts _ = pinsk k v (pcts ks) && wf (pcts _) }),
      (vs'
        : int varr{ pcts _ = pinsv k v (pcts ks) (pcts vs)
                    && plen (pcts _) = 8 }) )
