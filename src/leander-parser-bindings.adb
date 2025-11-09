with Leander.Core;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Expressions;
with Leander.Parser.Types;

with Leander.Syntax.Patterns;
with Leander.Syntax.Types;

package body Leander.Parser.Bindings is

   function At_Binding return Boolean
   is (At_Variable);

   -------------------
   -- Parse_Binding --
   -------------------

   procedure Parse_Binding
     (To : Leander.Syntax.Bindings.Reference)
   is
      Loc  : constant Source.Source_Location := Current_Source_Location;
      Name : constant String := Scan_Identifier;
      Pats : constant Leander.Syntax.Patterns.Reference_Array :=
               Expressions.Parse_Patterns;

      procedure Parse_Type_Bindings
        (Acc : Leander.Core.Varid_Array);

      -------------------------
      -- Parse_Type_Bindings --
      -------------------------

      procedure Parse_Type_Bindings
        (Acc : Leander.Core.Varid_Array)
      is
      begin
         if Tok = Tok_Colon_Colon then
            Scan;
            declare
               Expr : constant Leander.Syntax.Types.Reference :=
                        Leander.Parser.Types.Parse_Type_Expression;
            begin
               for Id of Acc loop
                  To.Add_Type (Loc, Core.To_String (Id), Expr);
               end loop;
            end;
         elsif Tok = Tok_Comma then
            Scan;
            if not At_Identifier then
               Error ("expected an identifier");
               while Tok_Indent > 1 loop
                  Scan;
               end loop;
               return;
            end if;

            declare
               use type Core.Varid_Array;
               Name : constant Core.Varid :=
                        Core.To_Varid (Scan_Identifier);
            begin
               Parse_Type_Bindings (Acc & Name);
            end;
         end if;
      end Parse_Type_Bindings;

   begin
      if Tok = Tok_Comma or else Tok = Tok_Colon_Colon then
         if Pats'Length /= 0 then
            Error ("expect a single name in type Binding");
         end if;
         Parse_Type_Bindings ([Core.To_Varid (Name)]);
      elsif Tok = Tok_Equal then
         Scan;
         declare
            Expr : constant Leander.Syntax.Expressions.Reference :=
                     Leander.Parser.Expressions.Parse_Expression;
         begin
            To.Add_Binding (Loc, Name, Pats, Expr);
         end;
      else
         Error ("expected '::' or '=' at " & Tok'Image);
         raise Parse_Error;
      end if;
   end Parse_Binding;

end Leander.Parser.Bindings;
