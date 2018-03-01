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

(* replace all " " with "" *)
let _trim_line line = 
  let comment_pattern = Str.regexp "//.*$" in
  let no_comment_line = (Str.replace_first comment_pattern "" line) in
  let pattern = Str.regexp "[ \r\n\t]" in
  Str.global_replace pattern "" no_comment_line 

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
  if String.length trimmed == 0 then Nothing
  else if String.get trimmed 0 == '@' then A_Command
  else if Str.string_match _is_c_command trimmed 0 then C_Command
  else if Str.string_match _is_l_command trimmed 0 then L_Command
  else Nothing

let has_more_commands lines =
  try Stream.empty(lines) != () with Stream.Failure -> true

(* Assume that this is only called when command_type is A_Command or L_Command *)
let symbol line = 
  let trimmed = _trim_line line in 
  let first_chr = String.get trimmed 0 in
  match first_chr with
  | '@' -> String.sub trimmed 1 (String.length(trimmed) - 1)
  (* HACK invalid input check *)
  | '(' -> String.sub trimmed 1 (String.length(trimmed) - 2)

(* Assume that this is only called when command_type is C_Command *)
let dest line = 
  let trimmed = _trim_line line in 
  if String.contains trimmed '=' then
    let len = String.index trimmed  '=' in
    String.sub trimmed 0 len
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
  (* HACK too complicated..*)
  let desti = String.length (dest line) in
  (* think of '=' *)
  let si = if desti == 0 then 0 else desti + 1 in 
  let trimmed = _trim_line line in
  let l = String.length trimmed and 
  jumplen = String.length (jump line) in
  (* think of ';' *)
  let jl = if jumplen == 0 then 0 else jumplen + 2 in 
  let len = l - jl - si in
  String.sub trimmed si len 
