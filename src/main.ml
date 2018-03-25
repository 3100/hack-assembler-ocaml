open Printf

let defined_symbols = [
  ("SP",         0);
  ("LCL",        1);
  ("ARG",        2);
  ("THIS",       3);
  ("THAT",       4);
  ("R0",         0);
  ("R1",         1);
  ("R2",         2);
  ("R3",         3);
  ("R4",         4);
  ("R5",         5);
  ("R6",         6);
  ("R7",         7);
  ("R8",         8);
  ("R9",         9);
  ("R10",       10);
  ("R11",       11);
  ("R12",       12);
  ("R13",       13);
  ("R14",       14);
  ("R15",       15);
  ("SCREEN", 16384);
  ("KBD",    24576);]

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

(* HACK Is it better to be constant variable? *)
let prepare_symbol_table () =
  let rec _append_symbol list table =
    match list with
    | [] -> table
    | (symbol, address) :: tl ->
      let updated_table = Symboltable.add_entry symbol address table in
      _append_symbol tl updated_table
  in
  let table = Symboltable.init in
  _append_symbol defined_symbols table

(* Collecting a symbol from the line.
 * Assume the command of the line is L_Command or A_Command.
 * @param line string 
 * @param lineIndex used as the address of the line
 * @param symbolTable
 * @return an updated symbolTable 
 *)
let process_a_line_first line lineIndex symbolTable =
  let symbol = Parser.symbol line in 
  match int_of_string_opt(symbol) with
  | None -> Symboltable.add_entry symbol lineIndex symbolTable (* symbol is a label *)
  | _ -> symbolTable (* Do nothing when symbol is decimal string *)

(*
 * @return (table, binary, nextAddressInt)
 *)
let process_a_line_second_command_a line addressCandidate symbolTable =
  let _gen_binary addressStr =
    String.concat "" ["0"; get_15bit_string addressStr;]  in
  let symbol = Parser.symbol line in
  match Symboltable.contains symbol symbolTable with
  | false when int_of_string_opt(symbol) == None ->
    let table = Symboltable.add_entry symbol addressCandidate symbolTable in
    let binary = _gen_binary (string_of_int addressCandidate) in
    (table, binary, addressCandidate + 1)
  | _ -> 
    let address =
    (match int_of_string_opt symbol with
    | Some _ -> symbol 
    | None -> string_of_int (Symboltable.get_address symbol symbolTable)) in
    let binary = _gen_binary address in
    (symbolTable, binary, addressCandidate)

let process_a_line_second_command_c line =
  String.concat ""
    [ "111";
      Code.comp (Parser.comp line);
      Code.dest (Parser.dest line);
      Code.jump (Parser.jump line);]

(* update symbolTable with L_Command *)
let process_lines_first lines symbolTable =
  let rec _process_lines_inner lines lineIndex st =
    if Parser.has_more_commands lines then
      let line = Parser.advance lines in
      (* command is needed to confirm whether lineIndex + 1 is needed *)
      let command = Parser.parse line in
      match command with
      | L_Command _ ->
        let new_symbol_table = process_a_line_first line lineIndex st in
        _process_lines_inner lines lineIndex new_symbol_table
      | A_Command _ | C_Command _ ->
        _process_lines_inner lines (lineIndex + 1) st 
      | _ ->
        _process_lines_inner lines lineIndex st 
    else
      st
  in
  let first_line_index = 0 in
  _process_lines_inner lines first_line_index symbolTable

let process_lines_second lines symbolTable =
  let rec _process_lines_inner lines addressCandidate ret_list st =
    if Parser.has_more_commands lines then
      let line = Parser.advance lines in
      let command = Parser.parse line in
      match command with
      | A_Command _ ->
        let (updated_st, binary_str, next_address) = process_a_line_second_command_a line addressCandidate st in 
        _process_lines_inner lines next_address (List.append ret_list [binary_str]) updated_st 
      | C_Command _ ->
        let binary_str = process_a_line_second_command_c line in
        _process_lines_inner lines addressCandidate (List.append ret_list [binary_str]) st 
      | _ ->
        _process_lines_inner lines addressCandidate ret_list st 
    else
      ret_list
  in
  (* this is a spec *)
  let start_variable_address = 16 in
    _process_lines_inner lines start_variable_address [] symbolTable

let write_line outChannel line =
  match line with
  | "" -> ()
  | _  -> Printf.fprintf outChannel "%s\n" line

let run in_path out_path =
  let symbol_table = prepare_symbol_table () in
  let in_channel = open_in in_path in
  let lines = Parser.line_stream_of_channel in_channel in
  let symbol_table = process_lines_first lines symbol_table in
  let () = close_in in_channel in
  (* second reading *)
  let in_channel = open_in in_path in
  let lines = Parser.line_stream_of_channel in_channel in
  let binary_lst = process_lines_second lines symbol_table in
  let out_channel = open_out out_path in
  List.iter (write_line out_channel) binary_lst;
  close_out out_channel;
  close_in in_channel;
