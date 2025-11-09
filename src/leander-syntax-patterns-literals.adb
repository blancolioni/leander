with Leander.Core.Literals;

package body Leander.Syntax.Patterns.Literals is

   -------------
   -- To_Core --
   -------------

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Patterns.Reference
   is
   begin
      case This.Class is
         when Integer_Literal =>
            return Leander.Core.Patterns.Literal
              (Leander.Core.Literals.Integer_Literal
                 (Ada.Strings.Unbounded.To_String (This.Image)));
         when Character_Literal =>
            return Leander.Core.Patterns.Literal
              (Leander.Core.Literals.Character_Literal (This.Code));
         when String_Literal =>
            return Leander.Core.Patterns.Literal
              (Leander.Core.Literals.String_Literal
                 (Ada.Strings.Unbounded.To_String (This.Image)));
         when Float_Literal =>
            return Leander.Core.Patterns.Literal
              (Leander.Core.Literals.Float_Literal
                 (Ada.Strings.Unbounded.To_String (This.Image)));
      end case;
   end To_Core;

end Leander.Syntax.Patterns.Literals;
