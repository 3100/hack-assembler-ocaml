open OUnit2

let command_type_test =
  let assert_command str expected test_ctxt =
    assert_equal (Parser.command_type str) expected in
  "command type" >::: [
    "' @1' is A command"        >:: assert_command " @1" A_Command;
    "'@123' is A command"       >:: assert_command "@123" A_Command;
    "   @R0 is A command"       >:: assert_command "   @R0" A_Command;
    "' M = D + 1' is C command" >:: assert_command " M = D + 1" C_Command;
    "'D + 1; JGT' is C Command" >:: assert_command "D + 1; JGT" C_Command;
    "'0;JMP' is C Command"      >:: assert_command "0;JMP" C_Command;
    "'D;JGT' is C Command"      >:: assert_command "D;JGT" C_Command;
    "'0; JMP' is C Command"     >:: assert_command "0; JMP" C_Command;
    "'(Loop)' is L Command"     >:: assert_command "(Loop)" L_Command;
    "'(End)' is L Command"      >:: assert_command "(End)" L_Command;
    "'(OUT_1) ' is L Command"   >:: assert_command "(OUT_1) " L_Command;
    "'(sys.init)' is L Command" >:: assert_command "(sys.init)" L_Command;
    "// this is comment line"   >:: assert_command "// this is comment line" Nothing;
    "\\n"                       >:: assert_command "\n" Nothing;
    "\\r"                       >:: assert_command "\r" Nothing;
    ""                          >:: assert_command "" Nothing;
  ]

let symbol_test =
  let assert_symbol str expected test_ctxt =
    assert_equal (Parser.symbol str) expected in
  "symbol" >::: [
    "@123 -> 123" >:: assert_symbol "@123" "123";
    "@0 -> 0" >:: assert_symbol "@0" "0";
    "(123) -> 123" >:: assert_symbol "(123)" "123";
    "@HOGE -> HOGE" >:: assert_symbol "@HOGE" "HOGE";
    "(END) -> END" >:: assert_symbol "(END)" "END";
  ]

let dest_test = 
  let assert_dest str expected test_ctxt =
    assert_equal (Parser.dest str) expected in
  "dest" >::: [
    "0; JMP     -> ''" >:: assert_dest "0; JMP"     ""; (* In this case, dest is empty. *) 
    "D = M - 1  -> D"  >:: assert_dest "D = M - 1"  "D";
    "D - 1; JLE -> ''" >:: assert_dest "D - 1; JLE" "";
    "D;JGT      -> ''" >:: assert_dest "D;JGT"      "";
  ]

let comp_test =
  let assert_comp str expected kest_ctxt =
    assert_equal (Parser.comp str) expected in
  "comp" >::: [
    "0; JMP -> 0"         >:: assert_comp "0; JMP"     "0"; (* In this case, comp is not empty. *) 
    "D = M - 1 -> D"      >:: assert_comp "D = M - 1"  "M-1";
    "D - 1; JLE -> D - 1" >:: assert_comp "D - 1; JLE" "D-1";
    "D;JGT"               >:: assert_comp "D;JGT"      "D";
  ]

let jump_test = 
  let assert_jump str expected kest_ctxt =
    assert_equal (Parser.jump str) expected in
  "jump" >::: [
    "0; JMP -> ''"      >:: assert_jump "0; JMP"     "JMP"; (* In this case, comp is not empty. *) 
    "D = M - 1 -> ''"   >:: assert_jump "D = M - 1"  "";
    "D - 1; JLE -> JLE" >:: assert_jump "D - 1; JLE" "JLE";
    "D;JGT -> JGT"      >:: assert_jump "D;JGT"      "JGT";
  ]

let has_more_commands_test =
  let assert_has_more_commands lines kest_ctxt =
    assert_equal (Parser.has_more_commands lines) true in
  let in_channel = open_in "sample.txt" in
  let lines = Parser.line_stream_of_channel in_channel in
  "has_more_commands" >::: [
    "case1" >:: assert_has_more_commands lines 
  ]

(* HACK warning messages when 'make' *)
let advance_test =
  "advance" >::: [
    "first line" >:: (fun _ -> 
      let in_channel = open_in "sample.txt" in
      let lines = Parser.line_stream_of_channel in_channel in
      assert_equal (Parser.advance lines) "testtest");
    "second line" >:: (fun _ ->
      let in_channel = open_in "sample.txt" in
      let lines = Parser.line_stream_of_channel in_channel in
      assert_equal (Parser.advance lines; Parser.advance lines) "hoge");
  ]

let tests =
  "parser_tests" >::: [
    command_type_test;
    symbol_test;
    dest_test;
    comp_test;
    jump_test;
    has_more_commands_test;
    advance_test;
  ]
