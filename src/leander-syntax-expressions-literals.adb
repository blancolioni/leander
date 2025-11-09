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
              (This.Location,
               Leander.Core.Literals.Integer_Literal
                 (Ada.Strings.Unbounded.To_String (This.Image)));
         when Character_Literal =>
            return Leander.Core.Expressions.Literal
              (This.Location,
               Leander.Core.Literals.Character_Literal
                 (This.Index));
         when String_Literal =>
            return Leander.Core.Expressions.Literal
              (This.Location,
               Leander.Core.Literals.String_Literal
                 (Ada.Strings.Unbounded.To_String (This.Image)));
         when Float_Literal =>
            return Leander.Core.Expressions.Literal
              (This.Location,
               Leander.Core.Literals.Float_Literal
                 (Ada.Strings.Unbounded.To_String (This.Image)));
      end case;
   end To_Core;

   ----------------
   -- To_Pattern --
   ----------------

   overriding function To_Pattern
     (This : Instance)
      return Leander.Syntax.Patterns.Reference
   is
   begin
      case This.Class is
         when Integer_Literal =>
            return Leander.Syntax.Patterns.Integer_Literal
              (This.Location,
               Ada.Strings.Unbounded.To_String (This.Image));
         when Character_Literal =>
            return Leander.Syntax.Patterns.Character_Literal
              (This.Location, This.Index);
         when String_Literal =>
            return Leander.Syntax.Patterns.String_Literal
              (This.Location,
               Ada.Strings.Unbounded.To_String (This.Image));
         when Float_Literal =>
            return Leander.Syntax.Patterns.Float_Literal
              (This.Location,
               Ada.Strings.Unbounded.To_String (This.Image));
      end case;
   end To_Pattern;

end Leander.Syntax.Expressions.Literals;
