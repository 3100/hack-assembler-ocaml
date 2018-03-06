open OUnit2

let through_test = 
  "case1" >:: (fun _ ->
    let m = Symboltable.init in
    let m = Symboltable.add_entry "key1" "000111001110" m in
    assert_equal (Symboltable.contains "key1" m) true;
    assert_equal (Symboltable.contains "key2" m) false;
    assert_equal (Symboltable.get_address "key1" m) "000111001110"
  )

let tests =
  "all_tests" >::: [
    through_test;
  ]
