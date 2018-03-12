open Arch
open Cmm

(* Recognition of addressing modes *)

class selector = object (_self)

inherit Selectgen.selector_generic as _super 

method! regs_for tyv =
  Reg.createv (begin
                 (* Expand floats into pairs of integer registers *)
                 let rec expand = function
                   [] -> []
                 | Float :: tyl -> Int :: Int :: expand tyl
                 | ty :: tyl -> ty :: expand tyl in
                 Array.of_list (expand (Array.to_list tyv))
               end
               )

method is_immediate _ = false 

method select_addressing _chunk exp = (Iindexed 0, exp)

end 

let fundecl f = (new selector)#emit_fundecl f 