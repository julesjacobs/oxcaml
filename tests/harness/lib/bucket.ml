(* Log-scale bucketing of search counters for golden output (DESIGN.md §8).

   Golden files record buckets, not exact counts, so a golden diff appears only on an
   order-of-magnitude behavior change rather than on every ±1 wobble. Exact counts go to
   the uncommitted stats sidecar for nightly aggregation. *)

let label (v : int) : string =
  if v < 10 then "<10"
  else if v < 100 then "<100"
  else if v < 1000 then "<1k"
  else if v < 10000 then "<10k"
  else ">=10k"
