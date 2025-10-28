open Alcotest

let test_even_fib () =
  let open Even_fib_sum in
  let limit = 4_000_000 in
  let exp = 4_613_732 in
  check int "rec" exp (even_fib_sum_rec limit);
  check int "tail" exp (even_fib_sum_tail limit);
  check int "module" exp (even_fib_sum_module limit);
  check int "map" exp (even_fib_sum_map_gen limit);
  check int "loops" exp (even_fib_sum_for limit);
  check int "seq" exp (even_fib_sum_seq limit)

let test_distinct_powers () =
  let open Distinct_powers in
  let amin, amax, bmin, bmax = (2, 100, 2, 100) in
  let exp = 9183 in
  check int "rec" exp (distinct_rec amin amax bmin bmax);
  check int "tail" exp (distinct_tail amin amax bmin bmax);
  check int "modular" exp (distinct_modular amin amax bmin bmax);
  check int "map" exp (distinct_map amin amax bmin bmax);
  check int "loops" exp (distinct_for amin amax bmin bmax);
  check int "seq" exp (distinct_seq amin amax bmin bmax)

let () =
  run "lab1"
    [
      ("even_fib", [ test_case "expected" `Quick test_even_fib ]);
      ("distinct", [ test_case "expected" `Quick test_distinct_powers ]);
    ]
