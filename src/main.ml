let get_15bit_string strVal =
  let rec hoge value list =
    match value with
    | 0 -> List.append ["0"] list 
    | 1 -> List.append ["1"] list
    | _ -> hoge (value lsr 1) (List.append [string_of_int (value mod 2)] list)
    in
  let int_val = int_of_string strVal in
  let result_list = hoge int_val [] in
  let result_str = String.concat "" result_list in
  let result_len = String.length result_str in
  if result_len == 15 then result_str
  else String.concat "" [ String.make (15 - result_len) '0'; result_str;]

let process_a_line line =
  let command = Parser.command_type line in
  match command with
  | A_Command -> String.concat "" ["0"; get_15bit_string (Parser.symbol line);] 
  | C_Command -> String.concat ""
      [ "111";
        Code.comp (Parser.comp line);
        Code.dest (Parser.dest line);
        Code.jump (Parser.jump line;)] 
  | _ -> "" (* TODO L_Command *)

(*
let rec _process_lines_inner lines ret_list =
  if Parser.has_more_commands lines then
    let line = Parser.advance lines in
    let binary_str = process_a_line line in
    _process_lines_inner lines List.append(ret_list [binary_str;])
  else
    ret_list

let process_lines lines =
  _process_lines_inner lines []

let run () =
  let in_channel = open_in "example/add.asm" in
  let lines = Parser.line_stream_of_channel in_channel in
  process_lines lines 
*)