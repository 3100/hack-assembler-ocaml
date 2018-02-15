open OUnit2

let all_tests = "all_test" >::: [
  Parser_test.tests;
  Code_test.tests;
];;

let () =
  run_test_tt_main all_tests;;
