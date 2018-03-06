open Printf

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

let process_a_line line lineIndex symbolTable =
  let command = Parser.command_type line in
  match command with
  | A_Command -> (String.concat "" ["0"; get_15bit_string (Parser.symbol line);], symbolTable)
  | C_Command -> 
      (String.concat ""
        [ "111";
          Code.comp (Parser.comp line);
          Code.dest (Parser.dest line);
          Code.jump (Parser.jump line)], symbolTable)
  | L_Command ->
    let symbol = Parser.symbol line and 
      address = get_15bit_string (string_of_int lineIndex) in
    let new_symbol_table = Symboltable.add_entry symbol address symbolTable in
    ("", new_symbol_table)
  | _ -> ("", symbolTable)

let process_a_line_first line lineIndex symbolTable =
  let command = Parser.command_type line in
  match command with
  | L_Command ->
    let symbol = Parser.symbol line and 
      address = get_15bit_string (string_of_int lineIndex) in
    Symboltable.add_entry symbol address symbolTable
  | _ -> symbolTable

(* WIP use symbol table *)
let process_a_line_second line symbolTable =
  let command = Parser.command_type line in
  match command with
  | A_Command -> String.concat "" ["0"; get_15bit_string (Parser.symbol line);]
  | C_Command -> String.concat ""
      [ "111";
        Code.comp (Parser.comp line);
        Code.dest (Parser.dest line);
        Code.jump (Parser.jump line);]
  | _ -> ""

(* update symbolTable with L_Command *)
let process_lines_first lines symbolTable =
  let rec _process_lines_inner lines lineIndex st =
    if Parser.has_more_commands lines then
      let line = Parser.advance lines in
      let new_symbol_table = process_a_line_first line lineIndex st in
      _process_lines_inner lines (lineIndex + 1) new_symbol_table
    else
      st
  in
  _process_lines_inner lines 0 symbolTable

let process_lines_second lines symbolTable =
  let rec _process_lines_inner lines ret_list st =
    if Parser.has_more_commands lines then
      let line = Parser.advance lines in
      let binary_str = process_a_line_second line st in 
      _process_lines_inner lines (List.append ret_list [binary_str]) st 
    else
      ret_list
  in
  _process_lines_inner lines [] symbolTable

let write_line outChannel line =
  match line with
  | "" -> ()
  | _  -> Printf.fprintf outChannel "%s\n" line

let run in_path out_path =
  let symbol_table = Symboltable.init in
  let in_channel = open_in in_path in
  let lines = Parser.line_stream_of_channel in_channel in
  let symbol_table = process_lines_first lines symbol_table in
  let lines = Parser.line_stream_of_channel in_channel in
  let binary_lst = process_lines_second lines symbol_table in
  let out_channel = open_out out_path in
  List.iter (write_line out_channel) binary_lst;
  close_out out_channel
