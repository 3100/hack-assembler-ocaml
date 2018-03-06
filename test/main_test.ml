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

let process_a_line_test =
  let symbol_table = Symboltable.init and
    line_index = 0 in 
  let assert_func str expected test_ctxt =
    let (actual, _) = Main.process_a_line str line_index symbol_table in
    assert_equal actual expected
  in
  "process_a_line" >::: [
    "'@11'    -> '0000000000001011'" >:: assert_func "@11"     "0000000000001011";
    "'M=1'    -> '1110111111001000'" >:: assert_func "M=1"     "1110111111001000";
    "'D=M'    -> '1111110000010000'" >:: assert_func "D=M"     "1111110000010000";
    "'D;JGT'  -> '1110001100000001'" >:: assert_func "D;JGT"   "1110001100000001";
    "'//hoge' -> ''"                 >:: assert_func "//hoge"  "";
    "''       -> ''"                 >:: assert_func ""        "";
    "'\\n'    -> ''"                 >:: assert_func "\n"      "";
    "'(END)'  -> ''"                 >:: assert_func "(END)"   ""; (* In the case, symbol_table get one entry. *)
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
  "all_tests" >::: [
    get_15bit_string_test;
    process_a_line_test;
    process_lines_first_test;
    process_lines_second_test;
  ]
