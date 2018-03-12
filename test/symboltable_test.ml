open OUnit2

let through_test = 
  "through_test" >:: (fun _ ->
    let m = Symboltable.init in
    let m = Symboltable.add_entry "key1" 3 m in
    assert_equal (Symboltable.contains "key1" m) true;
    assert_equal (Symboltable.contains "key2" m) false;
    assert_equal (Symboltable.get_address "key1" m) 3 
  )

let duplicate_add_test =
  "duplicate_add_test" >:: (fun _ ->
    let m = Symboltable.init in
    let m = Symboltable.add_entry "key1" 3 m in
    let m = Symboltable.add_entry "key1" 10 m in (* This process must not update key1 address *)
    assert_equal (Symboltable.contains "key1" m) true;
    assert_equal (Symboltable.get_address "key1" m) 3 
  )

let tests =
  "symbltable_tests" >::: [
    through_test;
    duplicate_add_test;
  ]
