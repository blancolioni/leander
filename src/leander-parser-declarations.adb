with Leander.Core.Kinds;
with Leander.Core.Predicates;
with Leander.Core.Schemes;
with Leander.Core.Types;
with Leander.Core.Tyvars;
with Leander.Data_Types;
with Leander.Data_Types.Builder;
with Leander.Names;
with Leander.Parser.Bindings;
with Leander.Parser.Expressions;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;
with Leander.Parser.Types;
with Leander.Syntax.Bindings;
with Leander.Syntax.Classes;
with Leander.Syntax.Types;

package body Leander.Parser.Declarations is

   procedure Parse_Class_Declaration
     (Context : in out Parse_Context'Class);

   procedure Parse_Data_Declaration
     (Context : Parse_Context'Class);

   procedure Parse_Foreign_Import
     (Context : Parse_Context'Class);

   procedure Parse_Constraints
     (Context : Parse_Context'Class;
      Add_Constraint : not null access
        procedure (Constraint_Name : String;
                   Variable_Name   : String));

   procedure Skip_Declaration;

   --------------------
   -- At_Declaration --
   --------------------

   function At_Declaration return Boolean is
   begin
      return At_Variable or else At_Constructor or else
        Tok <= [Tok_Class, Tok_Data, Tok_Foreign,
                Tok_Infix, Tok_Infixl, Tok_Infixr,
                Tok_Instance];
   end At_Declaration;

   -----------------------------
   -- Parse_Class_Declaration --
   -----------------------------

   procedure Parse_Class_Declaration
     (Context : in out Parse_Context'Class)
   is

      Builder : Leander.Syntax.Classes.Builder_Instance;

      procedure Add_Constraint
        (Constraint_Name : String;
         Variable_Name   : String);

      --------------------
      -- Add_Constraint --
      --------------------

      procedure Add_Constraint
        (Constraint_Name : String;
         Variable_Name   : String)
      is
         Name : constant Leander.Names.Leander_Name :=
                  Leander.Names.To_Leander_Name (Constraint_Name);
      begin
         if Context.Environment.Exists
           (Name, Leander.Environment.Class_Binding)
         then
            Builder.Add_Constraint (Constraint_Name, Variable_Name);
         else
            Error ("no such class: " & Constraint_Name);
         end if;
      end Add_Constraint;

      Indent : constant Positive := Tok_Indent;

   begin
      pragma Assert (Tok = Tok_Class);
      Scan;

      Parse_Constraints (Context, Add_Constraint'Access);

      if not At_Constructor_Name then
         Error ("missing class constructor");
         Skip_Declaration;
         return;
      end if;

      declare
         Name : constant String := Scan_Identifier;
         Varid : constant String :=
                   (if At_Variable_Name
                    then Scan_Identifier
                    else "*");
         Tyvar : constant Leander.Core.Types.Reference :=
                   Leander.Core.Types.TVar
                     (Leander.Core.Tyvars.Tyvar
                        (Core.To_Varid (Varid), Leander.Core.Kinds.Star));

      begin
         if Varid = "*" then
            Error ("missing type variable");
         end if;

         Builder.Start_Class (Name, Varid);

         if Tok = Tok_Where then
            Scan;
         else
            Error ("missing 'where'");
            while Tok_Indent > 1 loop
               Scan;
            end loop;
            return;
         end if;

         declare
            Constraints : constant Leander.Core.Predicates.Predicate_Array :=
                            [Leander.Core.Predicates.Predicate
                               (Name, Tyvar)];
            Bindings    : constant Leander.Syntax.Bindings.Reference :=
                         Leander.Syntax.Bindings.Empty
                           (Leander.Core.Class_Context, Constraints);
         begin
            while Tok_Indent > Indent loop
               Leander.Parser.Bindings.Parse_Binding (Context, Bindings);
            end loop;

            Builder.Add_Bindings (Bindings.To_Core);
            Context.Add_Class (Name);
            Context.Environment.Type_Class (Builder.Get_Class);
         end;
      end;
   end Parse_Class_Declaration;

   -----------------------
   -- Parse_Constraints --
   -----------------------

   procedure Parse_Constraints
     (Context : Parse_Context'Class;
      Add_Constraint : not null access
        procedure (Constraint_Name : String;
                   Variable_Name   : String))
   is
      function Is_Type_Class (Name : String) return Boolean
      is (Context.Environment.Exists
          (Leander.Names.To_Leander_Name (Name),
             Leander.Environment.Class_Binding));

   begin
      if At_Constructor_Name
        and then Next_Tok = Tok_Identifier
        and then Next_Tok (2) = Tok_Double_Right_Arrow
      then
         if Is_Type_Class (Tok_Text) then
            Add_Constraint (Tok_Text, Tok_Text (1));
         else
            Error ("no such class: " & Tok_Text);
         end if;

         Scan;
         if not At_Variable_Name then
            Error ("expected a type variable");
         end if;
         Scan;
         Scan;
      elsif Tok = Tok_Left_Paren then
         Scan;
         while At_Constructor_Name loop
            if not Is_Type_Class (Tok_Text) then
               Error ("no such class: " & Tok_Text);
            end if;
            declare
               Class_Name : constant String := Tok_Text;
            begin
               Scan;
               if At_Variable_Name then
                  declare
                     Var_Name : constant String := Tok_Text;
                  begin
                     Scan;
                     Add_Constraint (Class_Name, Var_Name);
                  end;
               else
                  Error ("expected a type variable");
               end if;
            end;

            if Tok = Tok_Comma then
               Scan;
               if not At_Constructor_Name then
                  Error ("missing class context");
               end if;
            elsif At_Constructor_Name then
               Error ("missing ','");
            end if;
         end loop;

         if Tok = Tok_Right_Paren then
            Scan;
         else
            Error ("missing ')' at " & Tok_Text);
         end if;

         if Tok = Tok_Double_Right_Arrow then
            Scan;
         else
            Error ("missing '=>'");
         end if;
      end if;
   end Parse_Constraints;

   ----------------------------
   -- Parse_Data_Declaration --
   ----------------------------

   procedure Parse_Data_Declaration
     (Context : Parse_Context'Class)
   is
      Builder : Leander.Data_Types.Builder.Data_Type_Builder;
   begin
      pragma Assert (Tok = Tok_Data);
      Scan;

      if not At_Constructor then
         Error ("expected a type constructor");
         return;
      end if;

      declare
         TExpr : constant Leander.Syntax.Types.Reference :=
                   Leander.Parser.Types.Parse_Type_Expression (Context);
         Data : constant Leander.Core.Types.Reference :=
                   TExpr.To_Core;
      begin

         if Tok /= Tok_Equal then
            Error ("expected '='");
            Skip_Declaration;
            return;
         end if;

         Scan;

         Builder.Start (Data);

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
                  Con_Args : array (1 .. 10) of Leander.Syntax.Types.Reference;
                  Count    : Natural := 0;
                  Indent   : constant Positive := Tok_Indent;
               begin
                  Scan;
                  while Tok_Indent > Indent
                    and then Parser.Types.At_Atomic_Type loop
                     Count := Count + 1;
                     Con_Args (Count) :=
                       Parser.Types.Parse_Atomic_Type (Context);
                  end loop;

                  declare
                     Con_Type : Leander.Core.Types.Reference := Data;
                  begin
                     for I in reverse 1 .. Count loop
                        Con_Type :=
                          Leander.Core.Types.Fn
                            (Con_Args (I).To_Core, Con_Type);
                     end loop;

                     Builder.Add_Con
                       (Leander.Core.To_Conid (Con_Name),
                        Leander.Core.Schemes.Quantify
                          (Con_Type.Get_Tyvars, [], Con_Type));
                  end;
               end;
            end if;
            if Tok = Tok_Vertical_Bar then
               Scan;
            else
               exit;
            end if;
         end loop;

         Builder.Build;

         Context.Environment.Data_Type (Builder.Data_Type);
      end;

   end Parse_Data_Declaration;

   ------------------------
   -- Parse_Declarations --
   ------------------------

   procedure Parse_Declarations
     (Context : in out Parse_Context'Class)
   is
      Bindings : constant Leander.Syntax.Bindings.Reference :=
                   Leander.Syntax.Bindings.Empty;
   begin
      while At_Declaration loop
         if Leander.Parser.Bindings.At_Binding then
            Leander.Parser.Bindings.Parse_Binding (Context, Bindings);
         elsif Tok = Tok_Foreign then
            Scan;
            if Tok = Tok_Import then
               Scan;
               Parse_Foreign_Import (Context);
            else
               Error ("only foreign imports Supported");
               while Tok_Indent > 1 loop
                  Scan;
               end loop;
            end if;
         elsif Tok in Tok_Infix | Tok_Infixl | Tok_Infixr then
            declare
               Assoc : constant Expressions.Associativity_Type :=
                         (if Tok = Tok_Infixl
                          then Expressions.Left
                          elsif Tok = Tok_Infixr
                          then Expressions.Right
                          else Expressions.None);
               Priority : Expressions.Priority_Range := 0;
            begin
               Scan;
               if Tok /= Tok_Integer_Literal then
                  Error ("expected an integer Precedence");
               else
                  declare
                     P : constant Natural :=
                           Natural'Value (Tok_Text);
                  begin
                     if P in 0 .. 9 then
                        Priority := Expressions.Priority_Range (P);
                     else
                        Error ("priority must be in the range 0 .. 9");
                     end if;
                  end;
                  Scan;
               end if;

               Expressions.Add_Fixity (Scan_Identifier, Assoc, Priority);

               while Tok = Tok_Comma loop
                  Scan;
                  if not At_Operator then
                     Error ("operator expected");
                     while Tok_Indent > 1 and then not At_Operator loop
                        Scan;
                     end loop;
                  end if;
                  if At_Operator then
                     Expressions.Add_Fixity (Scan_Identifier, Assoc, Priority);
                  end if;
               end loop;
            end;
         elsif Tok = Tok_Data then
            Parse_Data_Declaration (Context);
         elsif Tok = Tok_Class then
            Parse_Class_Declaration (Context);
         else
            Error ("only bindings are supported");
            Scan;
            Skip_Declaration;
         end if;
      end loop;
      Context.Environment.Bindings (Bindings.To_Core);
   end Parse_Declarations;

   --------------------------
   -- Parse_Foreign_Import --
   --------------------------

   procedure Parse_Foreign_Import
     (Context : Parse_Context'Class)
   is
      Import_Type  : constant String := Tok_Text;
   begin

      if Import_Type /= "skit" then
         Error ("only skit imports are supported");
      end if;

      Scan;

      if Tok = Tok_String_Literal then
         declare
            Foreign_Name : constant String := Tok_Text;
         begin
            Scan;

            if Tok = Tok_Identifier then
               declare
                  Local_Name : constant String := Tok_Text;
               begin
                  Scan;
                  if Tok = Tok_Colon_Colon then
                     Scan;
                     declare
                        Local_Type : constant Syntax.Types.Reference :=
                                       Parser.Types.Parse_Type_Expression
                                         (Context);
                     begin
                        Context.Environment.Foreign_Import
                          (Local_Name, Foreign_Name,
                           Local_Type.To_Core);
                     end;
                  else
                     Error ("missing import type");
                  end if;
               end;
            else
               Error ("missing name");
            end if;
         end;
      else
         Error ("missing local name");
      end if;
   end Parse_Foreign_Import;

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
