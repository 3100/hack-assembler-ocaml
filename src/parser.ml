type command =
  | A_Command
  | C_Command
  | L_Command
  | Nothing;;

(* 初期化 *)
let line_stream_of_channel channel = 
  Stream.from
    (fun _ ->
      try Some (input_line channel) with End_of_file -> None);;
(*
let in_channel = open_in "example/add.asm";
let lines = Parser.line_stream_of_channel in_channel;
Stream.next lines;
*)

let has_more_commands lines =
  try Stream.empty(lines) != () with Stream.Failure -> true

let command_type line =
  let trimmed_line = String.trim line and 
    (* HACK: too dificult for my future self to understand. Also this does not work. *)
    (* is_c_command = Str.regexp "^\\([AMD]+=\\)?[\\+AMD01!|&-]+\\(;J[\\(GT|EQ|GE|LT|NE|LE|MP\\)]\\)?" and *)
    (* HACK: not complete pattern. *)
    is_c_command = Str.regexp "^\\([AMD]+=\\)?[^()]+\\(;J.+\\)?$" and 
    is_l_command = Str.regexp "^([a-zA-Z0-9]+)$" in 
    if String.get trimmed_line 0 == '@' then A_Command
    else if Str.string_match is_c_command trimmed_line 0 then C_Command
    else if Str.string_match is_l_command trimmed_line 0 then L_Command
    else Nothing

let advance lines =
  Stream.next lines

(* below are for sample tests *)

let say_hello msg = 
  msg

let simple_check_123 str =
  let regexp = Str.regexp ".*123.*" in
  Str.string_match regexp str 0 

let a_command () =
  A_Command