open OUnit2

let get_15bit_string_test =
  let assert_func strVal expected test_ctxt =
    assert_equal (Main.get_15bit_string strVal) expected in
  "get_15bit_string" >::: [
    "0  -> 000000000000000" >:: assert_func "0"  "000000000000000";
    "1  -> 000000000000001" >:: assert_func "1"  "000000000000001";
    "2  -> 000000000000010" >:: assert_func "2"  "000000000000010";
    "5  -> 000000000000101" >:: assert_func "5"  "000000000000101";
    "8  -> 000000000001000" >:: assert_func "8"  "000000000001000";
    "11 -> 000000000001011" >:: assert_func "11" "000000000001011";
  ]

let process_a_line_second_command_a_test =
  let symbol_table = Symboltable.init in 
  let symbol_table = Symboltable.add_entry "END" 3 symbol_table in
  let assert_func address expected test_ctxt =
    let (_, actual, _) = Main.process_a_line_second_command_a address 2 symbol_table in
    assert_equal actual expected
  in
  "process_a_line_second" >::: [
    "'@11'    -> '0000000000001011'" >:: assert_func (Parser.Digit 11)      "0000000000001011";
    "'@END'   -> '0000000000000011'" >:: assert_func (Parser.Symbol "END")  "0000000000000011";
    "'@new'   -> '0000000000000010'" >:: assert_func (Parser.Symbol "@new") "0000000000000010";
  ]

let process_a_line_second_command_a_integer_symbol_test =
  let symbol_table = Symboltable.init in
  let (table, actual, _) = Main.process_a_line_second_command_a (Parser.Digit 3) 15 symbol_table in
  "process_a_line_second integer_symbol" >:: (fun _ ->
    assert_equal (Symboltable.contains "3" table) false)

let process_a_line_second_command_c_test =
  let assert_func str expected test_ctxt =
    let (dest, comp, jump) = str in
    let actual = Main.process_a_line_second_command_c dest comp jump in
    assert_equal actual expected
  in
  "process_a_line_second" >::: [
    "'M=1'    -> '1110111111001000'" >:: assert_func ("M", "1", "")   "1110111111001000";
    "'D=M'    -> '1111110000010000'" >:: assert_func ("D", "M", "")   "1111110000010000";
    "'D;JGT'  -> '1110001100000001'" >:: assert_func ("", "D", "JGT") "1110001100000001";
  ]


let process_lines_first_test =
  (* TODO implement *)
  "process_lines_first" >:: (fun _ -> assert_equal true false)

(* HACK test with symbol *)
let process_lines_second_test =
  let symbol_table = Symboltable.init in 
  let assert_func lines expected test_ctxt =
    let actual = Main.process_lines_second lines symbol_table in 
    assert_equal actual expected
  in
  let in_channel = open_in "sample.asm" in
  let lines = Parser.line_stream_of_channel in_channel in
  let expected = [
    "0000000000001011";
    "1110111111001000";
    "1111110000010000";
    ] in
  "process_lines_second" >::: [
    "case1" >:: assert_func lines expected;
  ]

let tests =
  "main_tests" >::: [
    get_15bit_string_test;
    (* TODO process_a_line_first_test *)
    process_a_line_second_command_a_test;
    process_a_line_second_command_a_integer_symbol_test;
    process_a_line_second_command_c_test;
    (* TODO process_lines_first_test *)
    process_lines_second_test;
  ]
