with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Expressions;
with Leander.Parser.Types;

with Leander.Types.Trees;

package body Leander.Parser.Declarations is

   function At_Declaration return Boolean;

   procedure Parse_Data_Declaration
     (Env : in out Leander.Environments.Environment);

   procedure Parse_Value_Binding
     (Env : in out Leander.Environments.Environment);

   procedure Skip_Declaration;

   --------------------
   -- At_Declaration --
   --------------------

   function At_Declaration return Boolean is
      use Leander.Parser.Lexical.Set_Of_Tokens;
   begin
      return Tok <= +(Tok_Identifier, Tok_Data);
   end At_Declaration;

   ----------------------------
   -- Parse_Data_Declaration --
   ----------------------------

   procedure Parse_Data_Declaration
     (Env : in out Leander.Environments.Environment)
   is
      Tycon : Leander.Types.Trees.Tree_Type;
   begin
      pragma Assert (Tok = Tok_Data);
      Scan;

      if not At_Constructor then
         Error ("expected a type constructor");
         return;
      end if;

      declare
         Name : constant String := Tok_Text;
      begin
         Tycon := Leander.Parser.Types.Parse_Type;

         if Tok /= Tok_Equal then
            Error ("expected '='");
            Skip_Declaration;
            return;
         end if;

         Scan;

         Env.Declare_Data_Type (Name, Tycon);

         loop
            if not At_Constructor then
               Error ("expected constructor");
               while Tok /= Tok_Vertical_Bar
                 and then Tok_Indent > 1
               loop
                  Scan;
               end loop;
            else
               declare
                  Con_Name : constant String := Tok_Text;
                  Con_Type : constant Leander.Types.Trees.Tree_Type :=
                               Leander.Parser.Types.Parse_Type_Constructor
                                 (Tycon);
               begin
                  Env.Insert_Constructor
                    (Type_Name => Name,
                     Name      => Con_Name,
                     Con_Type  => Con_Type);
               end;
            end if;
            if Tok = Tok_Vertical_Bar then
               Scan;
            else
               exit;
            end if;
         end loop;
      end;

   end Parse_Data_Declaration;

   -----------------------
   -- Parse_Declaration --
   -----------------------

   procedure Parse_Declaration
     (Env : in out Leander.Environments.Environment)
   is
   begin
      if not At_Declaration then
         Error ("declaration expected");
         while not At_Declaration and then Tok /= Tok_End_Of_File loop
            Scan;
         end loop;
      end if;

      if At_Declaration then
         if Tok = Tok_Data then
            Parse_Data_Declaration (Env);
         elsif Tok = Tok_Identifier then
            Parse_Value_Binding (Env);
         else
            raise Program_Error with
              "expected to be at a declaration";
         end if;
      end if;
   end Parse_Declaration;

   -------------------------
   -- Parse_Value_Binding --
   -------------------------

   procedure Parse_Value_Binding
     (Env : in out Leander.Environments.Environment)
   is
      Name : constant String := Tok_Text;
   begin
      Scan;
      if Tok /= Tok_Equal then
         Error ("expected '='");
         Skip_Declaration;
         return;
      end if;
      Scan;

      if not Leander.Parser.Expressions.At_Expression then
         Error ("expected an expression");
         Skip_Declaration;
         return;
      end if;

      declare
         Value : constant Leander.Syntax.Syntax_Tree :=
                   Leander.Parser.Expressions.Parse_Expression;
      begin
         Env.Insert_Value (Name, Value.To_Core);
      end;
   end Parse_Value_Binding;

   ----------------------
   -- Skip_Declaration --
   ----------------------

   procedure Skip_Declaration is
   begin
      while Tok_Indent > 1 loop
         Scan;
      end loop;
   end Skip_Declaration;

end Leander.Parser.Declarations;
