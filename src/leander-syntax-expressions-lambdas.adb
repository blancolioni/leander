package body Leander.Syntax.Expressions.Lambdas is

   -------------
   -- To_Core --
   -------------

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference
   is
   begin
      if This.Pat.Is_Variable then
         return Leander.Core.Expressions.Lambda
           (Leander.Core.To_Varid (This.Pat.Variable_Name),
            This.Expr.To_Core);
      else
         raise Constraint_Error with
         Leander.Source.Show (This.Pat.Location)
         & ": only variable patterns supported";
      end if;
   end To_Core;

end Leander.Syntax.Expressions.Lambdas;
