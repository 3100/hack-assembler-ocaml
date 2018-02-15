open OUnit2

let command_type_test =
  let assert_command str expected test_ctxt =
    assert_equal (Parser.command_type str) expected in
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

let symbol_test =
  let assert_symbol str expected test_ctxt =
    assert_equal (Parser.symbol str) expected in
  "symbol" >::: [
    "@123 -> 123" >:: assert_symbol "@123" "123";
    "@0 -> 0" >:: assert_symbol "@0" "0";
  ]

let dest_test = 
  let assert_dest str expected test_ctxt =
    assert_equal (Parser.dest str) expected in
  "dest" >::: [
    "0; JMP -> ''" >:: assert_dest "0; JMP" ""; (* In this case, dest is empty. *) 
    "D = M - 1 -> D" >:: assert_dest "D = M - 1" "D";
    "D - 1; JLE -> ''" >:: assert_dest "D - 1; JLE" "";
  ]

let comp_test =
  let assert_comp str expected test_ctxt =
    assert_equal (Parser.comp str) expected in
  "comp" >::: [
    "0; JMP -> 0" >:: assert_comp "0; JMP" "0"; (* In this case, comp is not empty. *) 
    "D = M - 1 -> D" >:: assert_comp "D = M - 1" "M-1";
    "D - 1; JLE -> D - 1" >:: assert_comp "D - 1; JLE" "D-1";
  ]

let tests =
  "all_tests" >::: [
    command_type_test; symbol_test; dest_test; comp_test;
  ]
