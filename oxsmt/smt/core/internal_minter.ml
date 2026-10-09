(* Board #58 O-MINTER. See internal_minter.mli. The token is the cap-closure plus the
   [admit] gate behind an abstract type: no projection is exposed, so a holder can mint
   only [admit]-sanctioned marker names and cannot recover the cap or a re-delegatable
   general closure. *)

type t =
  { admit : string -> bool
  ; mint_fn : string -> Rank.t -> Symbol.t
  }

let create ~admit cap env = { admit; mint_fn = Env.declare_reserved cap env }

let mint t name rank =
  if not (t.admit name)
  then
    invalid_arg
      (Printf.sprintf "Internal_minter.mint: %s is not a sanctioned internal marker" name);
  t.mint_fn name rank
;;
