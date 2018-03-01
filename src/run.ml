open Sys

(*
let rootdir = Sys.getcwd () in
let () = print_endline rootdir in
let in_path = String.concat "/" [root; "example/add/Add.asm";] and 
  out_path = String.concat "/" [root; "example/add/Add.hack";] in
  Main.run in_path out_path
*)

let run () =
  let root = Sys.getcwd() in
  let in_path = String.concat "/" [root; "example/add/Add.asm"] and 
    out_path = String.concat "/" [root; "example/add/Add.hack"] in
    Main.run in_path out_path

let _ = run ()