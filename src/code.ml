let dest mnemonic =
  match mnemonic with
  | "AMD" -> "111"
  | "AD"  -> "110"
  | "AM"  -> "101"
  | "A"   -> "100"
  | "MD"  -> "011"
  | "D"   -> "010"
  | "M"   -> "001"
  | ""    -> "000"

let comp mnemonic =
  match mnemonic with
  | "D|M" -> "1010101"
  | "D&M" -> "1000000"  
  | "M-D" -> "1000111"
  | "D-M" -> "1010011"
  | "D+M" -> "1000010"
  | "M-1" -> "1110010"
  | "M+1" -> "1110111"
  | "-M"  -> "1110011"
  | "!M"  -> "1110001"
  | "M"   -> "1110000"
  | "D|A" -> "0010101"
  | "D&A" -> "0000000"
  | "A-D" -> "0000111"
  | "D-A" -> "0010011"
  | "D+A" -> "0000010"
  | "A-1" -> "0110010"
  | "D-1" -> "0001110"
  | "A+1" -> "0110111"
  | "D+1" -> "0011111"
  | "-A"  -> "0110011"
  | "-D"  -> "0001111"
  | "!A"  -> "0110001"
  | "!D"  -> "0001101"
  | "A"   -> "0110000"
  | "D"   -> "0001100"
  | "-1"  -> "0111010"
  | "1"   -> "0111111"
  | "0"   -> "0101010"