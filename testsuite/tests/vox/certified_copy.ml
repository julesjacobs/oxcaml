open Copy_spec
open Generalize_spec
open Copy_certificate_spec
module C = Representative_certificate
module E = Effective_level
module R = Effective_copy_runtime

type instance = #{value : node Pref.t @@ aliased; state : Pref.token;
  pool : pool @@ aliased; epoch : node Pref.t @@ ghost; history : history @@ ghost;
  certificate : C.certificate @@ ghost}

let instantiate : (c : R.context) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem c.R.saved x) || source_ok c.R.saved x})) Ghost.t) @ total ->
    (clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at c.R.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head c.R.saved heads.Ghost.ghost x})) Ghost.t) @ total ->
    (depth : {n : int | n === c.R.depth && n >= 0}) ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    (state : {t : Pref.token | Pref.own t === c.R.saved && pool === c.R.base
      && H.mem c.R.saved p && H.mem c.R.saved c.R.epoch}) @ unique ->
    {r : instance | certifies c.R.saved r.#certificate c.R.epoch c.R.depth
        r.#history p r.#value
      && clean_session r.#history && r.#epoch === c.R.epoch
      && Pref.own r.#state === Copy_cleanup_spec.swept
        (heap c.R.saved c.R.epoch c.R.depth r.#history) (Pooled_spec.touched r.#history)
      && r.#pool === Pooled_spec.registered c.R.base c.R.epoch r.#history} @ unique =
  fun c heads scope clean witness depth pool p state ->
    let refine_ out = R.instantiate c heads scope clean witness depth pool p state in
    let certificate = ghost_ (
      let u = () in let refine_ certificate = Copy_certificate_proofs.certify
        c.R.saved heads.Ghost.ghost witness.Ghost.ghost c.R.epoch c.R.depth
        out.#history p out.#value (refine_ u) in certificate) in
    let result = #{value = out.#value; state = out.#state; pool = out.#pool;
      epoch = out.#epoch; history = out.#history; certificate} in refine_ result
