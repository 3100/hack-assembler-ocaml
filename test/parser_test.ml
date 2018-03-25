open OUnit2

let parse_test =
  let assert_command str expected test_ctxt =
    assert_equal (Parser.parse str) expected in
  "command type" >::: [
    "' @1' is A command"        >:: assert_command " @1"        (A_Command (Digit 1));
    "'@123' is A command"       >:: assert_command "@123"       (A_Command (Digit 123));
    "   @R0 is A command"       >:: assert_command "   @R0"     (A_Command (Symbol "R0"));
    "' M = D + 1' is C command" >:: assert_command " M = D + 1" (C_Command ("M", "D+1", ""));
    "'D + 1; JGT' is C Command" >:: assert_command "D + 1; JGT" (C_Command ("", "D+1", "JGT"));
    "'0;JMP' is C Command"      >:: assert_command "0;JMP"      (C_Command ("", "0", "JMP"));
    "'D;JGT' is C Command"      >:: assert_command "D;JGT"      (C_Command ("", "D", "JGT"));
    "'0; JMP' is C Command"     >:: assert_command "0; JMP"     (C_Command ("", "0", "JMP"));
    "'(Loop)' is L Command"     >:: assert_command "(Loop)"     (L_Command "Loop");
    "'(End)' is L Command"      >:: assert_command "(End)"      (L_Command "End");
    "'(OUT_1) ' is L Command"   >:: assert_command "(OUT_1) "   (L_Command "OUT_1");
    "'(sys.init)' is L Command" >:: assert_command "(sys.init)" (L_Command "sys.init");
    "// this is comment line"   >:: assert_command "// this is comment line" Nothing;
    "\\n"                       >:: assert_command "\n" Nothing;
    "\\r"                       >:: assert_command "\r" Nothing;
    ""                          >:: assert_command ""   Nothing;
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
      let _ = Parser.advance lines in
      assert_equal (Parser.advance lines) "hoge");
  ]

let tests =
  "parser_tests" >::: [
    parse_test;
    has_more_commands_test;
    advance_test;
  ]
