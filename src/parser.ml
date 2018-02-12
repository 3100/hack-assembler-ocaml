type command =
  | A_Command
  | C_Command
  | L_Command
  | Nothing;;

  (* HACK: too dificult for my future self to understand. Also this does not work. *)
(* let is_c_command = Str.regexp "^\\([AMD]+=\\)?[\\+AMD01!|&-]+\\(;J[\\(GT|EQ|GE|LT|NE|LE|MP\\)]\\)?" *)
(* HACK: not complete pattern. *)
let _is_c_command = Str.regexp "^\\([AMD]+=\\)?\\([^()]+\\)\\(;J.+\\)?$"
let _is_l_command = Str.regexp "^([a-zA-Z0-9]+)$" 

let _trim_line line = String.trim line

let line_stream_of_channel channel = 
  Stream.from
    (fun _ ->
      try Some (input_line channel) with End_of_file -> None);;
(*
let in_channel = open_in "example/add.asm";
let lines = Parser.line_stream_of_channel in_channel;
Stream.next lines;
*)

let advance lines =
  Stream.next lines

let command_type line =
  let trimmed = _trim_line line in 
  if String.get trimmed 0 == '@' then A_Command
  else if Str.string_match _is_c_command trimmed 0 then C_Command
  else if Str.string_match _is_l_command trimmed 0 then L_Command
  else Nothing

let has_more_commands lines =
  try Stream.empty(lines) != () with Stream.Failure -> true

(* Assume that this is only called when command_type is A_Command *)
let symbol line = 
  let trimmed = _trim_line line in 
  String.sub trimmed 1 (String.length(trimmed) - 1)

(* Assume that this is only called when command_type is C_Command *)
let dest line = 
  let trimmed = _trim_line line in 
  if String.contains trimmed '=' then
    let len = String.index trimmed  '=' in
    String.sub trimmed 0 (len - 1)
  else ""

(* Assume that this is only called when command_type is C_Command *)
let jump line = 
  let trimmed = _trim_line line in 
  if String.contains trimmed ';' then
    let si = (String.index trimmed ';') + 1 in 
    let len = (String.length trimmed) - 1 - si in
    String.sub trimmed si len 
  else ""

(* Assume that this is only called when command_type is C_Command *)
let comp line =
  let si = String.length (dest line) and
  trimmed = _trim_line line in
  let l = String.length trimmed and 
  jumplen = String.length (jump line) in
  let len = l - jumplen - 1 - si in
  String.sub trimmed si len 
