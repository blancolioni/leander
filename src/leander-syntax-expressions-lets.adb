package body Leander.Syntax.Expressions.Lets is

   -------------
   -- To_Core --
   -------------

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference
   is
   begin
      return Leander.Core.Expressions.Let
        (This.Bindings.To_Core, This.Expr.To_Core);
   end To_Core;

end Leander.Syntax.Expressions.Lets;
