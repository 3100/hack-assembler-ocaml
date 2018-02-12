open OUnit2

let assert_command str expected test_ctxt =
  assert_equal (Parser.command_type str) expected

let command_type_test =
  "command type" >::: [
    "' @1' is A command" >:: assert_command " @1" A_Command;
    "'@123' is A command" >:: assert_command "@123" A_Command;
    "' M = D + 1' is C command" >:: assert_command " M = D + 1" C_Command;
    "'D + 1; JGT' is C Command" >:: assert_command "D + 1; JGT" C_Command;
    "'0;JMP' is C Command" >:: assert_command "0;JMP" C_Command;
    "'0; JMP' is C Command" >:: assert_command "0; JMP" C_Command;
    "'(Loop)' is L Command" >:: assert_command "(Loop)" L_Command;
    "'(End)' is L Command" >:: assert_command "(End)" L_Command;
  ]

let tests =
  "all_tests" >::: [
    command_type_test;
  ]
