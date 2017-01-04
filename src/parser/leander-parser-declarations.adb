with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Leander.Prelude;

with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Expressions;
with Leander.Parser.Types;

with Leander.Types.Class_Constraints;
with Leander.Types.Trees;

with Leander.Core.Trees;

with Leander.Syntax.Expressions;

with Leander.Primitives;

with Leander.Errors;

package body Leander.Parser.Declarations is

   function At_Declaration return Boolean;

   procedure Parse_Class_Declaration
     (Env : in out Leander.Environments.Environment);

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
      return Tok <= +(Tok_Identifier, Tok_Class, Tok_Data);
   end At_Declaration;

   -----------------------------
   -- Parse_Class_Declaration --
   -----------------------------

   procedure Parse_Class_Declaration
     (Env : in out Leander.Environments.Environment)
   is
      use Ada.Strings.Unbounded;
      Class    : Leander.Types.Class_Constraints.Class_Constraint;
      Var_Name : Unbounded_String;
   begin
      pragma Assert (Tok = Tok_Class);
      Scan;

      Class.Create;

      if At_Constructor_Name
        and then Next_Tok = Tok_Identifier
        and then Next_Tok (2) = Tok_Double_Right_Arrow
      then
         if Env.Has_Class_Binding (Tok_Text) then
            Class.Add_Context (Env.Class_Binding (Tok_Text));
         else
            Error ("no such class: " & Tok_Text);
         end if;

         Scan;
         if At_Variable_Name then
            Var_Name := To_Unbounded_String (Tok_Text);
         else
            Error ("expected a type variable");
         end if;
         Scan;
         Scan;
      elsif Tok = Tok_Left_Paren then
         Scan;
         while At_Constructor_Name loop
            if Env.Has_Class_Binding (Tok_Text) then
               Class.Add_Context (Env.Class_Binding (Tok_Text));
            else
               Error ("no such class: " & Tok_Text);
            end if;
            Scan;
            if At_Variable_Name then
               if Var_Name /= Null_Unbounded_String then
                  if Var_Name /= Tok_Text then
                     Error ("type variable name does not match '"
                            & To_String (Var_Name) & "'");
                  end if;
                  Scan;
               else
                  Var_Name := To_Unbounded_String (Tok_Text);
               end if;
               Scan;
            else
               Error ("expected a type variable");
            end if;
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
            Error ("missing ')'");
         end if;

         if Tok = Tok_Double_Right_Arrow then
            Scan;
         else
            Error ("missing '=>'");
         end if;
      end if;

      if not At_Constructor_Name then
         Error ("missing class constructor");
         while Tok_Indent > 1 loop
            Scan;
         end loop;
         return;
      end if;

      declare
         Name : constant String := Scan_Identifier;
         Tyvar : constant String :=
                   (if At_Variable_Name
                    then Scan_Identifier
                    else "*");
      begin
         if Tyvar = "*" then
            Error ("missing type variable");
         end if;
         Class.Set_Constraint (Name, Tyvar);
         Env.Insert_Class_Binding (Name, Class);

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
            Class_Env : Leander.Environments.Environment;

            procedure Copy_Binding
              (Name      : String;
               Tree      : Leander.Core.Trees.Tree_Type;
               Signature : Leander.Types.Trees.Tree_Type);

            ------------------
            -- Copy_Binding --
            ------------------

            procedure Copy_Binding
              (Name      : String;
               Tree      : Leander.Core.Trees.Tree_Type;
               Signature : Leander.Types.Trees.Tree_Type)
            is
            begin
               if Signature.Is_Empty then
                  Leander.Errors.Error
                    (Tree.Get_Node.Source,
                     "default implementation has no signature");
                  return;
               end if;

               Class.Add_Method
                 (Name, Signature, Tree);

            end Copy_Binding;

         begin
            Class_Env.Create (Name);
            Class_Env.Insert_Type_Variable (Tyvar, Class.Type_Variable);

            Parse_Value_Binding (Class_Env);
            Class_Env.Scan_Local_Bindings
              (Copy_Binding'Access);
         end;

      end;

   end Parse_Class_Declaration;

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
         Tycon := Leander.Parser.Types.Parse_Type (Env);

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
                                 (Env, Tycon);
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
         elsif Tok = Tok_Class then
            Parse_Class_Declaration (Env);
         elsif Leander.Parser.Expressions.At_Pattern then
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
      use Leander.Syntax;

      package Tree_Vectors is
        new Ada.Containers.Vectors (Positive, Syntax_Tree_Record);
      Source  : constant Leander.Source.Source_Reference :=
                  Current_Source_Reference;
      Pat_Source : Leander.Source.Source_Reference :=
                     Current_Source_Reference;
      Indent     : constant Positive := Tok_Indent;
      Pattern : Syntax_Tree :=
                  Leander.Parser.Expressions.Parse_Pattern;
      Current : Syntax_Tree := Pattern.Left_Most;
      Pats : Tree_Vectors.Vector;
      Exps : Tree_Vectors.Vector;
      Last : Boolean := False;

      function Pattern_Name
        (Pat : Syntax_Tree)
         return String
         renames Leander.Syntax.Expressions.Application_Name;

      function Pattern_Arguments
        (Pat : Syntax_Tree)
         return Array_Of_Syntax_Trees
        renames Leander.Syntax.Expressions.Application_Arguments;

      function Parse_Type_Signature
        return Leander.Types.Trees.Tree_Type;

      --------------------------
      -- Parse_Type_Signature --
      --------------------------

      function Parse_Type_Signature
        return Leander.Types.Trees.Tree_Type
      is
      begin
         if Tok = Tok_Colon_Colon then
            Scan;
            return Leander.Parser.Types.Parse_Type (Env);
         elsif Tok = Tok_Comma then
            Scan;
            if not At_Name then
               Error ("expected a name");
            else
               declare
                  Name : constant String := Scan_Identifier;
                  Signature : constant Leander.Types.Trees.Tree_Type :=
                                Parse_Type_Signature;
               begin
                  Env.Insert_Signature (Name, Signature);
                  return Signature;
               end;
            end if;
         else
            Error ("expected ',' or '::'");
         end if;

         declare
            Indent : constant Positive := Tok_Indent;
         begin
            while Tok_Indent >= Indent loop
               Scan;
            end loop;
         end;
         return Leander.Types.Trees.Empty;
      end Parse_Type_Signature;

   begin
      loop
         if not Last
           and then Pattern_Name (Pattern) = Pattern_Name (Current)
           and then Tok /= Tok_Comma and then Tok /= Tok_Colon_Colon
         then
            Pats.Append (Syntax_Tree_Record (Pattern));
            if Tok = Tok_Equal then
               Scan;
            else
               Error ("missing '='");
            end if;
            Exps.Append
              (Syntax_Tree_Record
                 (Leander.Parser.Expressions.Parse_Expression));
            if Leander.Parser.Expressions.At_Pattern
              and then Tok_Indent >= Indent
            then
               Pat_Source := Current_Source_Reference;
               Pattern := Leander.Parser.Expressions.Parse_Expression;
            else
               Last := True;
            end if;
         else
            if not Pats.Is_Empty then
               declare
                  Name       : constant String :=
                                 Pattern_Name (Pats.First_Element);
                  First_Args : constant Array_Of_Syntax_Trees :=
                                 Pattern_Arguments
                                   (Pats.First_Element);
                  Pat_Count  : constant Natural := First_Args'Length;
                  Fun_Args   : Array_Of_Syntax_Trees (1 .. Pat_Count);
                  Value      : Syntax_Tree_Record;
                  Simple     : Boolean := False;
               begin

                  if Pats.Last_Index = 1
                    and then (for all X of First_Args =>
                                Leander.Syntax.Expressions.Is_Variable (X))
                  then
                     Value := Exps.First_Element;
                     for I in Fun_Args'Range loop
                        Fun_Args (I) := First_Args (I);
                     end loop;
                  else
                     for I in Fun_Args'Range loop
                        Fun_Args (I) :=
                          Syntax_Tree_Record
                            (Leander.Syntax.Expressions.Variable
                               (First_Args (I).Source, New_Variable));
                     end loop;

                     if Pat_Count = 0 then
                        null;
                     elsif Pat_Count = 1 then
                        Value :=
                          Syntax_Tree_Record
                            (Leander.Syntax.Expressions.Case_Expression
                               (Source,
                                Leander.Syntax.Expressions.Variable
                                  (Source, Fun_Args (1).Show)));
                     else
                        Value :=
                          Syntax_Tree_Record
                            (Leander.Syntax.Expressions.Constructor
                               (Pats.First_Element.Source,
                                Leander.Primitives.Tuple_Name (Pat_Count)));
                        Leander.Prelude.Use_Tuple (Pat_Count);
                        for I in Fun_Args'Range loop
                           Value :=
                             Syntax_Tree_Record
                               (Leander.Syntax.Expressions.Apply
                                  (First_Args (I).Source,
                                   Value, Fun_Args (I)));
                        end loop;
                        Value :=
                          Syntax_Tree_Record
                            (Leander.Syntax.Expressions.Case_Expression
                               (Source, Value));
                     end if;

                     for I in 1 .. Pats.Last_Index loop
                        declare
                           Pat  : Syntax_Tree := Pats (I);
                           Exp  : constant Syntax_Tree := Exps (I);
                           Args : constant Array_Of_Syntax_Trees :=
                                    Pattern_Arguments (Pat);
                        begin
                           if Args'Length /= Pat_Count then
                              Leander.Errors.Error
                                (Pat.Source,
                                 "inconsistent patterns in declaration");
                           end if;
                           if Args'Length = 0 then
                              pragma Assert (I = 1);
                              Value := Exps.Element (I);
                              Simple := True;
                              exit;
                           end if;

                           if Args'Length = 1 then
                              Pat := Syntax_Tree (Args (Args'First));
                           elsif Args'Length > 1 then
                              Pat :=
                                Leander.Syntax.Expressions.Constructor
                                  (Pat_Source,
                                   Leander.Primitives.Tuple_Name
                                     (Args'Length));
                              for Arg of Args loop
                                 Pat :=
                                   Leander.Syntax.Expressions.Apply
                                     (Arg.Source, Pat, Arg);
                              end loop;
                           end if;

                           Leander.Syntax.Expressions.Add_Case_Alternate
                             (Value, Pat, Exp);
                        end;
                     end loop;
                  end if;

                  if not Simple then
                     for I in reverse Fun_Args'Range loop
                        Value :=
                          Syntax_Tree_Record
                            (Leander.Syntax.Expressions.Lambda
                               (Source, Fun_Args (I).Show, Value));
                     end loop;
                  end if;
                  Env.Insert_Value (Name, Value.To_Core);
               end;

            end if;

            if Tok = Tok_Comma or else Tok = Tok_Colon_Colon then
               declare
                  Signature : constant Leander.Types.Trees.Tree_Type :=
                                Parse_Type_Signature;
               begin
                  Env.Insert_Signature (Pattern_Name (Pattern), Signature);
               end;

               if Leander.Parser.Expressions.At_Pattern
                 and then Tok_Indent >= Indent
               then
                  Pattern :=
                    Leander.Parser.Expressions.Parse_Pattern;
               else
                  Last := True;
               end if;
            end if;

            if Last then
               exit;
            else
               Current := Pattern;
               Pats.Clear;
               Exps.Clear;
            end if;
         end if;

      end loop;
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
