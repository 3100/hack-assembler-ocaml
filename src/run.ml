open Sys

let show_usage () =
  print_endline "usage: ./run.native in_asm_path out_hack_path"

let run () =
  match (Array.length Sys.argv) with
  | 3 ->
    let in_path = Sys.argv.(1) and
      out_path = Sys.argv.(2) in
      Main.run in_path out_path
  | _ -> show_usage ()

let _ = run () 
