(* WIP *)
type dest = string;;
type comp = string;;
type jmp = string;;
type symbol = string;;
type address =
  | Digit of int
  | Symbol of symbol;;
type command =
  A_Command of address 
  | C_Command of dest * comp * jmp 
  | L_Command of symbol
  | Nothing;;

exception Parse_Error

(* HACK: not complete pattern. *)
let _is_c_command = Str.regexp "^\\([AMD]+=\\)?\\([^();]+\\)\\(;J.+\\)?$"
let _is_l_command = Str.regexp "^(\\([a-zA-Z0-9_\\.\\$]+\\))$" 

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

let advance lines =
  Stream.next lines

let parse line =
  let trimmed = _trim_line line in 
  if String.length trimmed == 0 then Nothing
  else
    let first_chr = String.get trimmed 0 in
    match first_chr with
    | '@' ->
      let symbol = (String.sub trimmed 1 (String.length(trimmed) - 1)) in
      A_Command (Symbol symbol)
    | '(' when Str.string_match _is_l_command trimmed 0 -> L_Command (Str.matched_group 1 trimmed)
    | '(' -> raise Parse_Error
    | _ when Str.string_match _is_c_command trimmed 0 -> 
      let dest =
        try
          (* ex. "D=" *)
          let matched = Str.matched_group 1 trimmed in
          String.sub matched 0 (String.length(matched) - 1)
        with _ -> "" and
      comp = try Str.matched_group 2 trimmed with _ -> "" and
      jmp = 
        try
          (* ex. ";JMP" *)
          let matched = Str.matched_group 3 trimmed in
          String.sub matched 1 (String.length(matched) - 1)
        with _ -> "" 
      in 
      (*let () = Printf.printf "%s:%s:%s\n" dest comp jmp in *)
      C_Command (dest, comp, jmp) 
    | _ -> Nothing (* HACK: It's better to distinguish Parse_Error case from Nothing case. *)

let has_more_commands lines =
  try Stream.empty(lines) != () with Stream.Failure -> true

(* Getting a symbol as its label or a decimal string.
 * Assume that this is only called when command_type is A_Command or L_Command *)
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
    let len = (String.length trimmed) - si in
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
  let jl = if jumplen == 0 then 0 else jumplen + 1 in 
  let len = l - jl - si in
  String.sub trimmed si len 
