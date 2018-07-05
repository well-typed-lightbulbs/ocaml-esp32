(* CSE for the Xtensa Processor *)

open CSEgen

class cse = object

inherit cse_generic as _super

end

let fundecl f =
  (new cse)#fundecl f
