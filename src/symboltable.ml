(* cf. https://ocaml.org/learn/tutorials/map.ja.html *)
module SymbolMap = Map.Make(String)

let init =
  SymbolMap.empty

let add_entry symbol address table =
  SymbolMap.add symbol address table 

let contains symbol table =
  let maybe_address = SymbolMap.find_opt symbol table in
  match maybe_address with
  | Some _ -> true
  | None -> false

(* raise Not_found if symbol does not exist *)
let get_address symbol table =
  SymbolMap.find symbol table
