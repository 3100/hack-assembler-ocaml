open OUnit2

let say_test =
  "echo string" >:: (fun _ -> assert_equal (Main.say_hello "hello") "hello")

let tests =
  "all_tests" >::: [ say_test; ]
