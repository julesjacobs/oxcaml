(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Generic fixpoint solver. *)

module type S = sig
  type key
  type value

  val fix : (key -> bool * value) -> ((key -> value) -> (key -> value)) -> (key -> value)
end

module Make (Key : Hashtbl.HashedType) (Value : sig
  type t
  val equal : t -> t -> bool
end) = struct
  type key = Key.t
  type value = Value.t

  type entry = {
    mutable current_value : value;
    initial_value : value;  (* Initial value for resetting HIGH priority entries *)
    (* The solver uses rounds to track freshness of computed values:
       - round >= 0: The value was computed or is currently computing in that round
       - round = -1: The value has reached fixpoint (solved) *)
    mutable round : int;
    is_high_priority : bool;
  }

  type priority = Low | High

  let fix priority_and_init f =
    (* Memoization table mapping keys to their entries *)
    let table = Hashtbl.create 0 in

    (* Get or create an entry for a key *)
    let get_entry key =
      match Hashtbl.find_opt table key with
      | Some entry -> entry
      | None ->
          let (is_high_priority, init_value) = priority_and_init key in
          let entry = {
            current_value = init_value;
            initial_value = init_value;  (* Store initial value for reset *)
            round = 0;  (* Start at round 0 *)
            is_high_priority;
          } in
          Hashtbl.add table key entry;
          entry
    in

    (* Ensure that a key (and all its dependencies) reach fixpoint *)
    let ensure_solved key =
      (* Local state for this solving session *)
      let changed = ref false in
      let round = ref 0 in
      let current_priority = ref High in

      (* Read a key's value during solving, potentially recomputing it *)
      let rec read_during_solve key =
        let entry = get_entry key in

        (* Skip if already evaluated this round or already solved *)
        if entry.round == !round || entry.round == -1 then
          entry.current_value
        else begin
          (* Mark as evaluated in this round (prevents cycles) *)
          entry.round <- !round;

          (* Compute new value using recursive function *)
          let new_value = f read_during_solve key in

          (* Only update if appropriate for current priority phase *)
          let should_update =
            !current_priority = Low ||
            (entry.is_high_priority && !current_priority = High)
          in

          if should_update then begin
            if not (Value.equal entry.current_value new_value) then begin
              entry.current_value <- new_value;
              changed := true  (* Track that something changed *)
            end
          end;

          entry.current_value
        end
      in

      (* Reset all HIGH priority entries to their initial values *)
      let reset_high_to_initial () =
        Hashtbl.iter (fun _ entry ->
          if entry.is_high_priority then
            entry.current_value <- entry.initial_value
        ) table
      in

      (* Inner loop: HIGH to fixpoint with reset *)
      let high_priority_fixpoint_with_reset () =
        reset_high_to_initial ();
        let rec iterate () =
          changed := false;
          current_priority := High;
          incr round;
          let _ = read_during_solve key in
          if !changed then iterate ()
        in
        iterate ()
      in

      (* Single LOW update pass *)
      let single_low_pass () =
        changed := false;
        current_priority := Low;
        incr round;
        let _ = read_during_solve key in
        !changed  (* return whether anything changed *)
      in

      (* Outer loop: alternate HIGH fixpoint and LOW pass *)
      let rec outer_loop () =
        high_priority_fixpoint_with_reset ();
        if single_low_pass () then
          outer_loop ()  (* Continue if LOW pass changed anything *)
      in
      
      outer_loop ();

      (* Mark all entries as solved since everything has stabilized *)
      Hashtbl.iter (fun _ entry -> entry.round <- -1) table
    in

    (* Main entry point: returns a memoized function *)
    fun key ->
      let entry = get_entry key in
      (* If not solved, run the solver to fixpoint *)
      if entry.round <> -1 then
        ensure_solved key;
      entry.current_value
end
