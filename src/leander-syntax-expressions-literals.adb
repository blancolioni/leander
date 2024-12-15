with Leander.Core.Literals;
package body Leander.Syntax.Expressions.Literals is

   -------------
   -- To_Core --
   -------------

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference
   is
   begin
      case This.Class is
         when Integer_Literal =>
            return Leander.Core.Expressions.Literal
              (Leander.Core.Literals.Integer_Literal
                 (Leander.Core.Show (This.Image)));
      end case;
   end To_Core;

end Leander.Syntax.Expressions.Literals;
