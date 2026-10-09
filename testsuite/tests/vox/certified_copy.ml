open Copy_spec
open Generalize_spec
open Copy_certificate_spec
module C = Representative_certificate
module E = Effective_level
module R = Effective_copy_runtime

type instance = #{value : node Pref.t @@ aliased; state : node Pref.token;
  pool : pool @@ aliased; epoch : node Pref.t @@ ghost; history : history @@ ghost;
  certificate : C.certificate @@ ghost}

let instantiate : (c : Effective_copy_spec.context) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem c.Effective_copy_spec.saved x) || source_ok c.Effective_copy_spec.saved x})) Ghost.t) @ total ->
    (clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at c.Effective_copy_spec.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head c.Effective_copy_spec.saved heads.Ghost.ghost x})) Ghost.t) @ total ->
    (depth : {n : int | n === c.Effective_copy_spec.depth && n >= 0}) ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === c.Effective_copy_spec.saved && pool === c.Effective_copy_spec.base
      && H.mem c.Effective_copy_spec.saved p && H.mem c.Effective_copy_spec.saved c.Effective_copy_spec.epoch}) @ unique ->
    {r : instance | certifies c.Effective_copy_spec.saved r.#certificate c.Effective_copy_spec.epoch c.Effective_copy_spec.depth
        r.#history p r.#value
      && clean_session r.#history && r.#epoch === c.Effective_copy_spec.epoch
      && Pref.own r.#state === Copy_cleanup_spec.swept
        (heap c.Effective_copy_spec.saved c.Effective_copy_spec.epoch c.Effective_copy_spec.depth r.#history) (Pooled_spec.touched r.#history)
      && r.#pool === Pooled_spec.registered c.Effective_copy_spec.base c.Effective_copy_spec.epoch r.#history} @ unique =
  fun c heads scope clean witness depth pool p state ->
    let out = R.instantiate c heads scope clean witness depth pool p state in
    let certificate = ghost_ (
      let certificate = Copy_certificate_proofs.certify
        c.Effective_copy_spec.saved heads.Ghost.ghost witness.Ghost.ghost c.Effective_copy_spec.epoch c.Effective_copy_spec.depth
        out.#history p out.#value () in certificate) in
    let result = #{value = out.#value; state = out.#state; pool = out.#pool;
      epoch = out.#epoch; history = out.#history; certificate} in result
