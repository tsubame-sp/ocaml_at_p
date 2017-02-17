(* ======================================================================
 * Project Name    : OCaml@p
 * File Name       : ocaml_at_p.ml
 * Encoding        : utf-8
 
 * Copyright © 2016 Kenji Sakurai. All rights reserved.
 * ======================================================================
 *)

module Main = Typpx.Make.F(struct
  let tool_name = "ocaml_at_p"
  let args = []
  let firstUntypedTransformation = Typpx.Default.untyped_identity
  module Typemod = Typpx.Default.Typemod
  module TypedTransformation = Mod.Map
  let lastUntypedTransformation = Typpx.Default.untyped_identity
end)
                                
let () = Main.run ()

