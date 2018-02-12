open OUnit2

let command_type_test =
  "detect A command" >:: (fun _ -> 
    assert_equal (Parser.command_type "@123") A_Command)

(* TODO remove tests below *)
let say_test =
  "echo string" >:: (fun _ -> assert_equal (Parser.say_hello "hello") "hello")

let simple_check_123_test =
  "contains 123" >:: (fun _ ->
    assert_equal (Parser.simple_check_123 "012345") true)

let a_command_test =
  "a command" >:: (fun _ ->
    assert_equal (Parser.a_command ()) A_Command)

let tests =
  "all_tests" >::: [ a_command_test; command_type_test; say_test; simple_check_123_test; ]
