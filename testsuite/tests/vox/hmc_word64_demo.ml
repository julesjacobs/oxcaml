(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "hmc_word64.ml hmc_word64_demo.ml";
 { bytecode; }
 { native; }
*)
open Hmc_word64

let expect actual expected =
  if not (equal actual expected) then failwith "word64 mismatch"

let () =
  let zero = {lo = 0; hi = 0} in
  let one = {lo = 1; hi = 0} in
  let low_max = {lo = 4294967295; hi = 0} in
  let low_carry = {lo = 0; hi = 1} in
  let max = {lo = 4294967295; hi = 4294967295} in
  let sign = {lo = 0; hi = 2147483648} in
  expect (add low_max one) low_carry;
  expect (subtract low_carry one) low_max;
  expect (add max one) zero;
  expect (subtract zero one) max;
  expect (add sign sign) zero;
  expect (subtract zero sign) sign;
  if not (unsigned_less one sign && unsigned_less sign max)
    || unsigned_less max zero || unsigned_less one one
  then failwith "unsigned comparison mismatch"
