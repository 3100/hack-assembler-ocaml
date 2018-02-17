open OUnit2

let dest_test = 
  let assert_dest mnemonic expected test_ctxt =
    assert_equal (Code.dest mnemonic) expected in
  "dest" >::: [
    "'' -> 000" >:: assert_dest "" "000"; (* In this case, dest is empty. *) 
    "M -> 001" >:: assert_dest "M" "001";
    "D -> 010" >:: assert_dest "D" "010";
    "MD -> 011" >:: assert_dest "MD" "011";
    "A -> 100" >:: assert_dest "A" "100";
    "AM -> 101" >:: assert_dest "AM" "101";
    "AD -> 110" >:: assert_dest "AD" "110";
    "AMD -> 111" >:: assert_dest "AMD" "111";
    "Invalid raises Not_found" >:: (fun _ -> assert_raises Not_found (fun () -> Code.dest "Invalid"));
  ]

let comp_test =
  let assert_comp mnemonic expected test_ctxt =
    assert_equal (Code.comp mnemonic) expected in
  "comp" >::: [
    "D|M -> 1010101" >:: assert_comp "D|M" "1010101";
    "D&M -> 1000000" >:: assert_comp "D&M" "1000000";
    "M-D -> 1000111" >:: assert_comp "M-D" "1000111";
    "D-M -> 1010011" >:: assert_comp "D-M" "1010011";
    "D+M -> 1000010" >:: assert_comp "D+M" "1000010";
    "M-1 -> 1110010" >:: assert_comp "M-1" "1110010";
    "M+1 -> 1110111" >:: assert_comp "M+1" "1110111";
    "-M  -> 1110011" >:: assert_comp "-M"  "1110011";
    "!M  -> 1110001" >:: assert_comp "!M"  "1110001";
    "M   -> 1110000" >:: assert_comp "M"   "1110000";
    "D|A -> 0010101" >:: assert_comp "D|A" "0010101";
    "D&A -> 0000000" >:: assert_comp "D&A" "0000000";
    "A-D -> 0000111" >:: assert_comp "A-D" "0000111";
    "D-A -> 0010011" >:: assert_comp "D-A" "0010011";
    "D+A -> 0000010" >:: assert_comp "D+A" "0000010";
    "A-1 -> 0110010" >:: assert_comp "A-1" "0110010";
    "D-1 -> 0001110" >:: assert_comp "D-1" "0001110";
    "A+1 -> 0110111" >:: assert_comp "A+1" "0110111";
    "D+1 -> 0011111" >:: assert_comp "D+1" "0011111";
    "-A  -> 0110011" >:: assert_comp "-A"  "0110011";
    "-D  -> 0001111" >:: assert_comp "-D"  "0001111";
    "!A  -> 0110001" >:: assert_comp "!A"  "0110001";
    "!D  -> 0001101" >:: assert_comp "!D"  "0001101";
    "A   -> 0110000" >:: assert_comp "A"   "0110000";
    "D   -> 0001100" >:: assert_comp "D"   "0001100";
    "-1  -> 0111010" >:: assert_comp "-1"  "0111010";
    "1   -> 0111111" >:: assert_comp "1"   "0111111";
    "0   -> 0101010" >:: assert_comp "0"   "0101010";
    "Invalid raises Not_found" >:: (fun _ -> assert_raises Not_found (fun () -> Code.comp "Invalid"));
  ]

let jump_test =
  let assert_jump mnemonic expected test_ctxt =
    assert_equal (Code.jump mnemonic) expected in
  "jump" >::: [
    "''  -> 000" >:: assert_jump ""    "000";
    "JGT -> 001" >:: assert_jump "JGT" "001";
    "JEQ -> 010" >:: assert_jump "JEQ" "010";
    "JGE -> 011" >:: assert_jump "JGE" "011";
    "JLT -> 100" >:: assert_jump "JLT" "100";
    "JNE -> 101" >:: assert_jump "JNE" "101";
    "JLE -> 110" >:: assert_jump "JLE" "110";
    "JMP -> 111" >:: assert_jump "JMP" "111";
    "Invalid raises Not_found" >:: (fun _ -> assert_raises Not_found (fun () -> Code.jump "Invalid"));
  ]

let tests =
  "all_tests" >::: [
    dest_test; comp_test; jump_test;
  ]
