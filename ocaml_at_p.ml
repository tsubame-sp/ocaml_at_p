module Main = Typpx.Make.F(struct
  let tool_name = "ocaml@p"
  let args = []
  let firstUntypedTransformation = Typpx.Default.untyped_identity
  module Typemod = Typpx.Default.Typemod
  module TypedTransformation = Mod.Map
  let lastUntypedTransformation = Typpx.Default.untyped_identity
end)
                                
let () = Main.run ()

