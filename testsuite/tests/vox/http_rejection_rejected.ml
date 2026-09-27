(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_http_spec.mli vox_http_spec.ml vox_http.mli vox_http.ml";
 readonly_files = "http_rejection_rejected.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

open Vox_http_spec
open Vox_http
module S = Vox_sequence

(* An invalid request line is rejected as a request line, not a header. *)
module Wrong_kind = struct
  let (claim @ total) (line : bytes) (suffix : bytes) :
      {u : unit | if safe_line line && not (valid_request_line line)
          && fits 16384 (S.append line [13; 10]) then
        status (feed (initial ()) (S.append line (13 :: 10 :: suffix))).state
          === Malformed Invalid_header
        else true} @ ghost =
    ghost_ (request_line_rejection line suffix)
end;;
[%%expect{|
module S = Vox_sequence
Line 13, characters 11-47:
13 |     ghost_ (request_line_rejection line suffix)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 8-12, characters 18-17:
 8 | ..................if safe_line line && not (valid_request_line line)
 9 |           && fits 16384 (S.append line [13; 10]) then
10 |         status (feed (initial ()) (S.append line (13 :: 10 :: suffix))).state
11 |           === Malformed Invalid_header
12 |         else true...........
  The refinement is stated here.
|}]

(* A valid request line is not rejected. *)
module Valid_line = struct
  let (claim @ total) (line : bytes) (suffix : bytes) :
      {u : unit | if safe_line line && fits 16384 (S.append line [13; 10]) then
        status (feed (initial ()) (S.append line (13 :: 10 :: suffix))).state
          === Malformed Invalid_request_line
        else true} @ ghost =
    ghost_ (request_line_rejection line suffix)
end;;
[%%expect{|
Line 7, characters 11-47:
7 |     ghost_ (request_line_rejection line suffix)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 3-6, characters 18-17:
3 | ..................if safe_line line && fits 16384 (S.append line [13; 10]) then
4 |         status (feed (initial ()) (S.append line (13 :: 10 :: suffix))).state
5 |           === Malformed Invalid_request_line
6 |         else true...........
  The refinement is stated here.
|}]

(* The error comes when the line ends, not before its line feed. *)
module Early = struct
  let (claim @ total) (line : bytes) (suffix : bytes) :
      {u : unit | if safe_line line && not (valid_request_line line)
          && fits 16384 (S.append line [13; 10]) then
        (feed (initial ()) (S.append line (13 :: 10 :: suffix))).rest
          === 10 :: suffix
        else true} @ ghost =
    ghost_ (request_line_rejection line suffix)
end;;
[%%expect{|
Line 8, characters 11-47:
8 |     ghost_ (request_line_rejection line suffix)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 3-7, characters 18-17:
3 | ..................if safe_line line && not (valid_request_line line)
4 |           && fits 16384 (S.append line [13; 10]) then
5 |         (feed (initial ()) (S.append line (13 :: 10 :: suffix))).rest
6 |           === 10 :: suffix
7 |         else true...........
  The refinement is stated here.
|}]

(* An invalid request line is not left incomplete. *)
module Incomplete_line = struct
  let (claim @ total) (line : bytes) :
      {u : unit | if safe_line line && not (valid_request_line line)
          && fits 16384 (S.append line [13; 10]) then
        status (feed (initial ()) (S.append line [13; 10])).state
          === Incomplete
        else true} @ ghost =
    ghost_ (request_line_rejection line [])
end;;
[%%expect{|
Line 8, characters 11-43:
8 |     ghost_ (request_line_rejection line [])
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 3-7, characters 18-17:
3 | ..................if safe_line line && not (valid_request_line line)
4 |           && fits 16384 (S.append line [13; 10]) then
5 |         status (feed (initial ()) (S.append line [13; 10])).state
6 |           === Incomplete
7 |         else true...........
  The refinement is stated here.
|}]

(* The header law needs the earlier header lines to be valid. *)
module Earlier_headers = struct
  let (claim @ total) (line : bytes) (headers : int list list) (bad : bytes)
      (suffix : bytes) :
      {u : unit | if valid_request_line line && safe_line line
          && nonempty bad && safe_line bad && not (valid_header bad)
          && fits 16384 (S.append line
            (13 :: 10 :: header_prefix headers (S.append bad [13; 10]))) then
        (feed (initial ()) (S.append line (13 :: 10 :: header_prefix headers
          (S.append bad (13 :: 10 :: suffix))))).rest === suffix
        else true} @ ghost =
    ghost_ (header_rejection line headers bad suffix)
end;;
[%%expect{|
Line 11, characters 11-53:
11 |     ghost_ (header_rejection line headers bad suffix)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 4-10, characters 18-17:
 4 | ..................if valid_request_line line && safe_line line
 5 |           && nonempty bad && safe_line bad && not (valid_header bad)
 6 |           && fits 16384 (S.append line
 7 |             (13 :: 10 :: header_prefix headers (S.append bad [13; 10]))) then
 8 |         (feed (initial ()) (S.append line (13 :: 10 :: header_prefix headers
 9 |           (S.append bad (13 :: 10 :: suffix))))).rest === suffix
10 |         else true...........
  The refinement is stated here.
|}]

(* The byte law says nothing about a byte in 0..255. *)
module In_range = struct
  let (claim @ total) (state : state) (rest : bytes) :
      {u : unit | if status state === Incomplete
          && total_consumed state < 16384 then
        status (feed state (65 :: rest)).state === Malformed Invalid_byte
        else true} @ ghost =
    ghost_ (invalid_byte_rejection state 65 rest)
end;;
[%%expect{|
Line 7, characters 11-49:
7 |     ghost_ (invalid_byte_rejection state 65 rest)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 3-6, characters 18-17:
3 | ..................if status state === Incomplete
4 |           && total_consumed state < 16384 then
5 |         status (feed state (65 :: rest)).state === Malformed Invalid_byte
6 |         else true...........
  The refinement is stated here.
|}]
