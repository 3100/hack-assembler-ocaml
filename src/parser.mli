type dest = string
type comp = string
type jmp = string
type symbol = string
type address = Digit of int | Symbol of symbol
type command =
    A_Command of address
  | C_Command of dest * comp * jmp
  | L_Command of symbol
  | Nothing
exception Parse_Error
val line_stream_of_channel : in_channel -> string Stream.t
val advance : 'a Stream.t -> 'a
val parse : string -> command
val has_more_commands : 'a Stream.t -> bool
