(*---------------------------------------------------------------------------
   Copyright (c) 2021 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Mu

let log fmt = Format.kfprintf (Fun.const ()) Format.std_formatter (fmt ^^ "@.")
let log_fail pp ~exp ~fnd =
  log "@[<v3>Expected: @[%a@]@,Found: @[%a@]@]" pp exp pp fnd

let assert_eq' eq pp fnd exp =
  if eq fnd exp then () else (log_fail pp ~exp ~fnd; assert false)

let assert_q = assert_eq' Q.equal Q.pp
let assert_int = assert_eq' Int.equal Format.pp_print_int
let assert_bool = assert_eq' Bool.equal Format.pp_print_bool
let assert_float = assert_eq' Float.equal Format.pp_print_float

let test_uniqueness () =
  log "Test representation uniqueness.";
  assert_q Q.(2 #/ 4) Q.(1 #/ 2);
  assert_q Q.(2 #/ (-4)) Q.(-1 #/ 2);
  assert_q Q.(-2 #/ 4) Q.(-1 #/ 2);
  ()

let test_compare () =
  log "Test Q.compare.";
  let q12 = Q.(1 #/ 2) and q32 = Q.(3 #/ 2) in
  let test_sym q0 q1 cmp =
    assert_int (Q.compare q0 q1) cmp;
    assert_int (Q.compare q1 q0) (-cmp);
  in
  test_sym q12 q32 (-1);
  test_sym q32 q32 0;
  test_sym q12 q12 0;
  test_sym Q.nan Q.nan 0;
  test_sym Q.nan Q.infinity (-1);
  test_sym Q.nan Q.neg_infinity (-1);
  test_sym Q.nan q12 (-1);
  test_sym Q.infinity Q.zero 1;
  test_sym Q.infinity Q.neg_infinity 1;
  ()

let test_predicates () =
  log "Test Q.{is_finite,is_infinite,is_nan,sign}";
  assert_bool Q.(is_finite zero) true;
  assert_bool Q.(is_finite one) true;
  assert_bool Q.(is_finite nan) false;
  assert_bool Q.(is_finite neg_infinity) false;
  assert_bool Q.(is_finite infinity) false;
  assert_bool Q.(is_infinite zero) false;
  assert_bool Q.(is_infinite one) false;
  assert_bool Q.(is_infinite nan) false;
  assert_bool Q.(is_infinite neg_infinity) true;
  assert_bool Q.(is_infinite infinity) true;
  assert_bool Q.(is_nan zero) false;
  assert_bool Q.(is_nan one) false;
  assert_bool Q.(is_nan nan) true;
  assert_bool Q.(is_nan neg_infinity) false;
  assert_bool Q.(is_nan infinity) false;
  assert_int Q.(sign zero) 0;
  assert_int Q.(sign minus_one) (-1);
  assert_int Q.(sign one) 1;
  assert_int Q.(sign nan) 0;
  assert_int Q.(sign neg_infinity) (-1);
  assert_int Q.(sign infinity) 1;
  ()

let test_to_int () =
  log "Test Q.{[checked_]to_int_{away_from,towards}_zero";
  assert_int Q.(to_int_towards_zero (1 #/ 4)) 0;
  assert_int Q.(to_int_away_from_zero (1 #/ 4)) 1;
  assert_int Q.(to_int_towards_zero (-1 #/ 4)) 0;
  assert_int Q.(to_int_away_from_zero (-1 #/ 4)) (-1);
  assert_int Q.(to_int_towards_zero (3 #/ 2)) 1;
  assert_int Q.(to_int_away_from_zero (3 #/ 2)) 2;
  assert_int Q.(to_int_towards_zero (-3 #/ 2)) (-1);
  assert_int Q.(to_int_away_from_zero (-3 #/ 2)) (-2);
  assert (Q.(checked_to_int_towards_zero (2 #/ 0)) = None);
  assert (Q.(checked_to_int_away_from_zero (2 #/ 0)) = None);
  ()

let test_to_float () =
  log "Test Q.to_float";
  assert_float Q.(to_float one) 1.0;
  assert_float Q.(to_float Q.(1 #/ 2)) 0.5;
  assert_float Q.(to_float zero) 0.0;
  assert_float Q.(to_float neg_infinity) Float.neg_infinity;
  assert_float Q.(to_float infinity) Float.infinity;
  assert (Float.is_nan (Q.(to_float nan)));
  ()

let test_add_sub () =
  log "Test Q.{add,sub}";
  assert_q Q.(2 #/ 3 + 1 #/ 3) Q.one;
  assert_q Q.(1 #/ 2 + 2 #/ 3) Q.(7 #/ 6);
  assert_q Q.(4 #/ 3 + nan) Q.nan;
  assert_q Q.(nan + 4 #/ 3) Q.nan;
  assert_q Q.(nan + nan) Q.nan;
  assert_q Q.(infinity + 2 #/ 3) Q.infinity;
  assert_q Q.(infinity + infinity) Q.infinity;
  assert_q Q.(infinity + neg_infinity) Q.nan;
  assert_q Q.(neg_infinity + 2 #/ 3) Q.neg_infinity;
  assert_q Q.(neg_infinity + neg_infinity) Q.neg_infinity;
  assert_q Q.(7 #/ 6 - 1 #/ 2) Q.(2 #/ 3);
  assert_q Q.(4 #/ 3 - nan) Q.nan;
  assert_q Q.(nan - 4 #/ 3) Q.nan;
  assert_q Q.(nan - nan) Q.nan;
  assert_q Q.(infinity - 2 #/ 3) Q.infinity;
  assert_q Q.(infinity - infinity) Q.nan;
  assert_q Q.(infinity - neg_infinity) Q.infinity;
  assert_q Q.(neg_infinity - 2 #/ 3) Q.neg_infinity;
  assert_q Q.(neg_infinity - neg_infinity) Q.nan;
  ()

let test_mul_div () =
  log "Test Q.{mul,div}.";
  assert_q Q.(2 #/ 3 * 2 #/ 3) Q.(4 #/ 9);
  assert_q Q.(1 #/ 2 * 2 #/ 3) Q.(1 #/ 3);
  assert_q Q.(56 #/ 22 * nan) Q.nan;
  assert_q Q.(nan * 56 #/ 22) Q.nan;
  assert_q Q.(nan * nan) Q.nan;
  assert_q Q.(infinity * 2 #/ 3) Q.infinity;
  assert_q Q.(infinity * (-2) #/ 3) Q.neg_infinity;
  assert_q Q.(infinity * infinity) Q.infinity;
  assert_q Q.(infinity * neg_infinity) Q.neg_infinity;
  assert_q Q.(neg_infinity * 2 #/ 3) Q.neg_infinity;
  assert_q Q.(neg_infinity * (-2) #/ 3) Q.infinity;
  assert_q Q.(neg_infinity * neg_infinity) Q.infinity;
  assert_q Q.(1 #/ 3 / 2 #/ 3) Q.(1 #/ 2);
  assert_q Q.(1 #/ 3 / nan) Q.nan;
  assert_q Q.(nan / 2 #/ 3) Q.nan;
  assert_q Q.(nan / nan) Q.nan;
  assert_q Q.(zero / zero) Q.nan;
  assert_q Q.(1 #/ 3 / zero) Q.infinity;
  assert_q Q.(-1 #/ 3 / zero) Q.neg_infinity;
  assert_q Q.(-1 #/ 3 / infinity) Q.zero;
  assert_q Q.(1 #/ 3 / infinity) Q.zero;
  assert_q Q.(-1 #/ 3 / neg_infinity) Q.zero;
  assert_q Q.(1 #/ 3 / neg_infinity) Q.zero;
  ()

let tests () =
  test_uniqueness ();
  test_predicates ();
  test_compare ();
  test_to_int ();
  test_to_float ();
  test_add_sub ();
  test_mul_div ();
  log "Success!"

let () = if !Sys.interactive then () else tests ()

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The mu programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
