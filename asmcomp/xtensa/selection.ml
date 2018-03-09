open Arch

(* Recognition of addressing modes *)

class selector = object (_self)

inherit Selectgen.selector_generic as _super 

method is_immediate _ = false 

method select_addressing _chunk exp = (Iindexed 0, exp)

end 

let fundecl f = (new selector)#emit_fundecl f 