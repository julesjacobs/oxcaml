(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* PARTIAL MAPS via [via]: a dictionary modelled as a TOTAL function
   [Int -> Option Int] (the Prop-set trick generalized to Option-valued).
   Representation is an assoc list; the image is the function model
   [Dict] reached by [toFun]; image-binder as in via_set.  This pins that
   grind handles Option-valued pointwise get/set reasoning -- both the
   "same key" law and the framing law under an implication -- as cleanly
   as the disjunction-based Prop-set laws, and refutes an overclaim.
   Surface limit: [Some] cannot appear in a refinement (Option is a
   predefined parameterized type), so [some v] is wrapped in [isVal]. *)

type alist = ANil | ACons of int * int * alist
type dict [@@vox.sort lean "Dict"]

[%%vox.lean {lean|
def Dict := Int -> Option Int
@[grind] def dget (m : Dict) (k : Int) : Option Int := m k
@[grind] def dset (m : Dict) (k v : Int) : Dict :=
  fun k' => if k' = k then some v else m k'
@[grind] def isVal (o : Option Int) (v : Int) : Prop := o = some v
@[grind] def toFun : Vox_alist -> Dict
  | .ANil => fun _ => none
  | .ACons k v rest => fun k' => if k' = k then some v else toFun rest k'
@[grind] def ok : Vox_alist -> Prop := fun _ => True
|lean}]

type d = alist{ ok _ } [@vox.via (toFun : dict)]
[%%expect{|
type alist = ANil | ACons of int * int * alist
type dict
type d = alist{ ok _ via (toFun : dict) }
|}]

(* law 1: reading the just-set key returns the value (Option-valued). *)
let get_set_same : (k : int) -> (v : int) -> (m : d)
  -> unit{ isVal (dget (dset m k v) k) v } =
  fun k v m -> ()
[%%expect{|
val get_set_same :
  (k : int) -> (v : int) -> (m : d) -> unit{ isVal (dget (dset m k v) k) v } =
  <fun>
|}]

(* law 2: reading a DIFFERENT key is unchanged (Option = Option). *)
let get_set_other : (k : int) -> (k' : int) -> (v : int) -> (m : d)
  -> unit{ (k' <> k) ==> (dget (dset m k v) k' = dget m k') } =
  fun k k' v m -> ()
[%%expect{|
val get_set_other :
  (k : int) ->
  (k' : int) ->
  (v : int) ->
  (m : d) -> unit{ (k' <> k) -> ((dget (dset m k v) k') = (dget m k')) } =
  <fun>
|}]

(* control: an OVERCLAIM (wrong value) must FAIL. *)
let bad : (k : int) -> (v : int) -> (m : d)
  -> unit{ isVal (dget (dset m k v) k) (v + 1) } =
  fun k v m -> ()
[%%expect{|
Line 3, characters 15-17:
3 |   fun k v m -> ()
                   ^^
Error: vox: verification failed (lean).
       Goal: isVal (dget (dset m k v) k) (v + 1)
Hypotheses: <none>
Possible counterexample:
  v = 0
  k = 2
(lean: error: `grind` failed)
|}]
