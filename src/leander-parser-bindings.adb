with Leander.Core;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Expressions;
with Leander.Parser.Types;

with Leander.Syntax.Patterns;
with Leander.Syntax.Qualified_Types;

package body Leander.Parser.Bindings is

   function At_Binding return Boolean
   is (At_Variable
       or else At_Constructor
       or else Tok <= [Tok_Left_Bracket, Tok_Left_Paren]);

   function Parse_LHS
     (Context : Parse_Context'Class)
      return Leander.Syntax.Bindings.Binding_LHS;

   -------------------
   -- Parse_Binding --
   -------------------

   procedure Parse_Binding
     (Context : Parse_Context'Class;
      To      : Leander.Syntax.Bindings.Reference)
   is
      Loc  : constant Source.Source_Location := Current_Source_Location;
      LHS : constant Syntax.Bindings.Binding_LHS := Parse_LHS (Context);

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
               Expr : constant Leander.Syntax.Qualified_Types.Reference :=
                        Leander.Parser.Types.Parse_Qualified_Type_Expression
                          (Context);
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
         if Syntax.Bindings.Pats (LHS)'Length /= 0 then
            Error ("expect a single name in type Binding");
         end if;
         Parse_Type_Bindings ([Core.To_Varid (Syntax.Bindings.Name (LHS))]);
      elsif Tok = Tok_Equal then
         Scan;
         declare
            Expr : Leander.Syntax.Expressions.Reference :=
                     Leander.Parser.Expressions.Parse_Expression
                       (Context);
         begin
            if Tok = Tok_Where then
               declare
                  Loc : constant Source.Source_Location := Current_Source_Location;
                  Bs  : constant Leander.Syntax.Bindings.Reference :=
                    Leander.Syntax.Bindings.Empty;
               begin
                  Scan;
                  if Tok = Tok_Left_Brace then
                     Scan;
                     loop
                        Bindings.Parse_Binding (Context, Bs);
                        if Tok = Tok_Semi then
                           Scan;
                        else
                           exit;
                        end if;
                     end loop;
                     if Tok = Tok_Right_Brace then
                        Scan;
                     else
                        Error ("missing '}'");
                     end if;
                  else
                     declare
                        Indent : constant Positive := Tok_Indent;
                     begin
                        while Tok_Indent >= Indent loop
                           if Bindings.At_Binding then
                              Bindings.Parse_Binding (Context, Bs);
                           else
                              Error ("expected a binding");
                              exit;
                           end if;
                        end loop;
                     end;
                  end if;

                  Expr := Syntax.Expressions.Let (Loc, Bs, Expr);
               end;
            end if;
            To.Add_Binding (Loc, LHS, Expr);
         end;
      else
         Error ("expected '::' or '=' at " & Tok'Image);
         raise Parse_Error;
      end if;
   end Parse_Binding;

   ---------------
   -- Parse_LHS --
   ---------------

   function Parse_LHS
     (Context : Parse_Context'Class)
      return Leander.Syntax.Bindings.Binding_LHS
   is
      Pat : constant Syntax.Patterns.Reference :=
        Parser.Expressions.Parse_Atomic_Pattern (Context);
   begin
      if At_Operator then
         declare
            use type Leander.Syntax.Patterns.Reference_Array;
            Name : constant String := Scan_Identifier;
            Pats : constant Leander.Syntax.Patterns.Reference_Array :=
              Expressions.Parse_Patterns (Context);
         begin
            return Syntax.Bindings.Create_Binding_LHS (Name, Pat & Pats);
         end;
      elsif not Pat.Is_Variable then
         Error ("syntax error");
         raise Parse_Error;
      else
         declare
            Name : constant String := Pat.Variable_Name;
            Pats : constant Leander.Syntax.Patterns.Reference_Array :=
              Expressions.Parse_Patterns (Context);
         begin
            return Syntax.Bindings.Create_Binding_LHS (Name, Pats);
         end;
      end if;
   end Parse_LHS;

end Leander.Parser.Bindings;
