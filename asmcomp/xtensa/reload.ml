

class reload = object 

inherit Reloadgen.reload_generic as _super 

end

let fundecl f =
  (new reload)#fundecl f
