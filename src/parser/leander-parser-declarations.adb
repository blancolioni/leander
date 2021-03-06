with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with Leander.Prelude;

with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Expressions;
with Leander.Parser.Types;

with Leander.Types.Bindings;
with Leander.Types.Class_Constraints;
with Leander.Types.Compiler;
with Leander.Types.Instances.Derived;
with Leander.Types.Trees;

with Leander.Core.Cases;
with Leander.Core.Lets;
with Leander.Core.Trees;

with Leander.Primitives;

with Leander.Errors;
with Leander.Logging;

package body Leander.Parser.Declarations is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   function At_Declaration return Boolean;

   procedure Parse_Class_Declaration
     (Env : in out Leander.Environments.Environment);

   procedure Parse_Data_Declaration
     (Env : in out Leander.Environments.Environment);

   procedure Parse_Foreign_Import
     (Env : in out Leander.Environments.Environment);

   procedure Parse_Instance_Declaration
     (Env : in out Leander.Environments.Environment);

   procedure Parse_Constraints
     (Env : in out Leander.Environments.Environment;
      Add_Constraint : not null access
        procedure (Constraint_Name : String;
                   Variable_Name   : String));

   procedure Skip_Declaration;

   --------------------
   -- At_Declaration --
   --------------------

   function At_Declaration return Boolean is
      use Leander.Parser.Lexical.Set_Of_Tokens;
   begin
      return At_Variable or else At_Constructor or else
        Tok <= +(Tok_Class, Tok_Data, Tok_Foreign,
                 Tok_Infix, Tok_Infixl, Tok_Infixr,
                 Tok_Instance);
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
      Indent   : constant Positive := Tok_Indent + 1;

      procedure Add_Constraint
        (Constraint_Name : String;
         Variable_Name   : String);

      procedure Add_Constraint
        (Constraint_Name : String;
         Variable_Name   : String)
      is
      begin
         if Env.Has_Class_Binding (Constraint_Name) then
            Class.Add_Context (Env.Class_Binding (Constraint_Name));
         else
            Error ("no such class: " & Constraint_Name);
         end if;
         if Var_Name /= Null_Unbounded_String then
            if Var_Name /= Variable_Name then
               Error ("type variable name does not match '"
                      & To_String (Var_Name) & "'");
            end if;
         else
            Var_Name := To_Unbounded_String (Variable_Name);
         end if;
      end Add_Constraint;

   begin
      pragma Assert (Tok = Tok_Class);
      Scan;

      Class.Create;

      Parse_Constraints (Env, Add_Constraint'Access);

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
               Env.Insert_Signature (Name, Signature);

            end Copy_Binding;

         begin
            Class_Env.Create_Temporary_Environment (Env, Name);
            Class_Env.Insert_Type_Variable (Tyvar, Class.Type_Variable);

            Parse_Value_Bindings (Class_Env, Indent);
            Class_Env.Scan_Local_Bindings
              (Copy_Binding'Access);
         end;

      end;

   end Parse_Class_Declaration;

   -----------------------
   -- Parse_Constraints --
   -----------------------

   procedure Parse_Constraints
     (Env : in out Leander.Environments.Environment;
      Add_Constraint : not null access
        procedure (Constraint_Name : String;
                   Variable_Name   : String))
   is
   begin
      if At_Constructor_Name
        and then Next_Tok = Tok_Identifier
        and then Next_Tok (2) = Tok_Double_Right_Arrow
      then
         if Env.Has_Class_Binding (Tok_Text) then
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
            if not Env.Has_Class_Binding (Tok_Text) then
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
                  Con_Type : Leander.Types.Trees.Tree_Type;
                  Con_Args : array (1 .. 10) of Leander.Types.Trees.Tree_Type;
                  Count    : Natural := 0;
                  Indent   : constant Positive := Tok_Indent;
               begin
                  Scan;
                  if Tok = Tok_Left_Brace then
                     declare
                        Con : Leander.Types.Bindings.Constructor_Binding;
                     begin
                        Scan;
                        while At_Variable_Name loop
                           declare
                              Vars : String_Lists.List;
                              Field_Type : Leander.Types.Trees.Tree_Type;
                           begin
                              while At_Variable_Name loop
                                 Vars.Append (Scan_Identifier);
                              end loop;
                              if Tok = Tok_Colon_Colon then
                                 Scan;
                              else
                                 Error ("missing '::'");
                              end if;

                              Field_Type :=
                                Leander.Parser.Types.Parse_Type (Env);

                              for Var_Name of Vars loop
                                 Con.Add_Field (Var_Name, Field_Type);
                              end loop;

                              if Tok = Tok_Comma then
                                 Scan;
                                 if not At_Variable_Name then
                                    if Tok = Tok_Right_Brace then
                                       Error ("extra ',' ignored");
                                    else
                                       Error ("missing field");
                                    end if;
                                 end if;
                              elsif At_Variable_Name then
                                 if Tok_Indent < Indent then
                                    exit;
                                 else
                                    Error ("missing ','");
                                 end if;
                              end if;
                           end;
                        end loop;

                        if Tok = Tok_Right_Brace then
                           Scan;
                        else
                           Error ("missing '}'");
                        end if;

                        Env.Insert_Constructor
                          (Type_Name => Name,
                           Con_Name  => Con_Name,
                           Binding   => Con);

                     end;
                  else
                     while Tok_Indent > Indent
                       and then Parser.Types.At_Atomic_Type loop
                        Count := Count + 1;
                        Con_Args (Count) :=
                          Parser.Types.Parse_Atomic_Type (Env);
                     end loop;

                     Con_Type := Tycon;

                     for I in reverse 1 .. Count loop
                        Con_Type :=
                          Leander.Core.Map_Operator.Apply
                            (Con_Args (I)).Apply (Con_Type);
                     end loop;

                     Env.Insert_Constructor
                       (Type_Name => Name,
                        Name      => Con_Name,
                        Con_Type  => Con_Type,
                        Con_Arity => Count);
                  end if;
               end;
            end if;
            if Tok = Tok_Vertical_Bar then
               Scan;
            else
               exit;
            end if;
         end loop;

         Leander.Types.Compiler.Compile_Algebraic_Type
           (Env, Name);

         if Tok = Tok_Deriving then
            Scan;
            if At_Constructor_Name then
               Leander.Types.Instances.Derived.Derive_Instance
                 (Env, Env.Type_Constructor_Binding (Name).Type_Pattern,
                  Get_Identifier);
            elsif Tok = Tok_Left_Paren then
               Scan;
               while At_Constructor_Name loop
                  Leander.Types.Instances.Derived.Derive_Instance
                    (Env, Env.Type_Constructor_Binding (Name).Type_Pattern,
                     Scan_Identifier);
                  if Tok = Tok_Comma then
                     Scan;
                  else
                     exit;
                  end if;
               end loop;
               if Tok = Tok_Right_Paren then
                  Scan;
               else
                  Error ("missing ')'");
               end if;
            end if;
         end if;
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
         elsif Tok = Tok_Instance then
            Parse_Instance_Declaration (Env);
         elsif Tok = Tok_Foreign
           and then Next_Tok = Tok_Import
         then
            Scan;
            Scan;
            Parse_Foreign_Import (Env);
         elsif Leander.Parser.Expressions.At_Pattern then
            Parse_Value_Bindings (Env, 1);
         elsif Tok = Tok_Infixl
           or else Tok = Tok_Infixr
           or else Tok = Tok_Infix
         then
            declare
               use Leander.Parser.Expressions;
               Assoc : constant Associativity_Type :=
                         (if Tok = Tok_Infixl
                          then Left
                          elsif Tok = Tok_Infixr
                          then Right
                          else None);
               Priority : Priority_Range :=
                            Priority_Range'Last;
            begin
               Scan;
               if Tok = Tok_Integer_Literal then
                  declare
                     Value : constant Natural :=
                               Natural'Value (Tok_Text);
                  begin
                     if Value > Natural (Priority_Range'Last) then
                        Error ("priority must be in range 0 .. 9");
                     else
                        Priority := Priority_Range (Value);
                     end if;
                  end;
                  Scan;
               else
                  Error ("missing priority");
               end if;

               if not At_Operator then
                  Error ("missing operator list");
               else
                  while At_Operator loop
                     declare
                        Name : constant String := Scan_Identifier;
                     begin
                        Add_Fixity (Name, Assoc, Priority);
                        if Tok = Tok_Comma then
                           Scan;
                           if not At_Operator then
                              Error ("missing operator name");
                           end if;
                        else
                           exit;
                        end if;
                     end;
                  end loop;
               end if;
            end;

         else
            Internal_Error
              ("expected to be at a declaration at " & Tok_Text);
         end if;
      end if;
   end Parse_Declaration;

   --------------------------
   -- Parse_Foreign_Import --
   --------------------------

   procedure Parse_Foreign_Import
     (Env : in out Leander.Environments.Environment)
   is
      Foreign_Name : constant String :=
                       Tok_Text;
   begin
      if Tok = Tok_String_Literal then
         Scan;
      end if;
      if Tok = Tok_Identifier then
         declare
            Local_Name : constant String := Tok_Text;
            Local_Type : Leander.Types.Trees.Tree_Type;
         begin
            Scan;
            if Tok = Tok_Colon_Colon then
               Scan;
               Local_Type := Leander.Parser.Types.Parse_Type (Env);
               Env.Insert_Foreign_Import
                 (Local_Name, Foreign_Name, Local_Type);
            else
               Error ("missing import type");
            end if;
         end;
      else
         Error ("missing name");
      end if;
   end Parse_Foreign_Import;

   --------------------------------
   -- Parse_Instance_Declaration --
   --------------------------------

   procedure Parse_Instance_Declaration
     (Env : in out Leander.Environments.Environment)
   is
      Instance : Leander.Types.Instances.Type_Instance;
      Indent   : constant Positive := Tok_Indent;
   begin
      pragma Assert (Tok = Tok_Instance);
      Scan;
      Instance.Create;

      declare
         procedure Add (Constraint_Name, Variable_Name : String);

         ---------
         -- Add --
         ---------

         procedure Add (Constraint_Name, Variable_Name : String) is
            Variable : Leander.Types.Type_Node :=
                         Leander.Types.Variable (Variable_Name);
         begin
            Variable.Add_Constraint
              (Env.Class_Binding (Constraint_Name));
            Instance.Add_Constraint (Leander.Types.Trees.Leaf (Variable));
         end Add;

      begin
         Parse_Constraints (Env, Add'Access);
      end;

      if At_Constructor_Name then
         declare
            Class_Name : constant String := Tok_Text;
            Instance_Type : Leander.Types.Trees.Tree_Type;
            Instance_Env  : Leander.Environments.Environment;
         begin
            Scan;
            Instance_Type :=
              Leander.Parser.Types.Parse_Atomic_Type (Env);

            Instance.Set_Class_Assertion
              (Instance_Type, Class_Name);

            if Tok = Tok_Where then
               Scan;
            else
               Error ("missing 'where'");
            end if;

            Instance_Env.Create_Temporary_Environment
              (Env, Class_Name);

            Parse_Value_Bindings (Instance_Env, Tok_Indent);

            declare
               procedure Process
                 (Name  : String;
                  Value : Leander.Core.Trees.Tree_Type);

               -------------
               -- Process --
               -------------

               procedure Process
                 (Name  : String;
                  Value : Leander.Core.Trees.Tree_Type)
               is
               begin
                  Leander.Logging.Log
                    ("implementing: " & Class_Name & " " & Name
                     & " = " & Value.Show);
                  Instance.Implement (Name, Value);
               end Process;

            begin
               Instance_Env.Scan_Local_Bindings (Process'Access);
            end;

            Leander.Logging.Log
              ("instance " & Class_Name & " " & Instance_Type.Show);

            Env.Add_Type_Assertion
              (Instance_Type.Head.Show, Instance);

         end;
      else
         Error ("missing class name");
         while Tok_Indent > Indent loop
            Scan;
         end loop;
      end if;
   end Parse_Instance_Declaration;

   --------------------------
   -- Parse_Value_Bindings --
   --------------------------

   procedure Parse_Value_Bindings
     (Env    : in out Leander.Environments.Environment;
      Indent : Positive)
   is

      use Leander.Core.Trees;

      package Tree_Vectors is
        new Ada.Containers.Vectors (Positive, Tree_Type);
      Source  : constant Leander.Source.Source_Reference :=
                  Current_Source_Reference;
      Pat_Source : Leander.Source.Source_Reference :=
                     Current_Source_Reference;
      Pattern : Tree_Type :=
                  Leander.Parser.Expressions.Parse_Pattern;
      Current : Tree_Type := Pattern.First_Leaf;
      Pats : Tree_Vectors.Vector;
      Exps : Tree_Vectors.Vector;
      Last : Boolean := False;

      function Pattern_Name
        (Pat : Tree_Type)
         return String
      is (Pat.Head.Show);

      function Pattern_Arguments
        (Pat : Tree_Type)
         return Array_Of_Trees
        renames Arguments;

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
            while Tok_Indent > Indent loop
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
            Pats.Append (Pattern);
            declare
               Exp : Leander.Core.Trees.Tree_Type :=
                       Leander.Parser.Expressions.Parse_Guarded_Expression;
            begin
               if Tok = Tok_Where then
                  declare
                     Indent    : constant Positive := Tok_Indent;
                     Where_Env : Leander.Environments.Environment;
                  begin
                     Where_Env.Create_Temporary_Environment
                       (Env, "where");
                     Scan;
                     Parse_Value_Bindings (Where_Env, Indent);
                     Exp := Leander.Core.Lets.Let_Expression (Where_Env, Exp);
                  end;
               end if;

               Exps.Append (Exp);
            end;

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
                  First_Args : constant Array_Of_Trees :=
                                 Pattern_Arguments
                                   (Pats.First_Element);
                  Pat_Count  : constant Natural := First_Args'Length;
                  Fun_Args   : Array_Of_Trees (1 .. Pat_Count);
                  Value      : Tree_Type;
                  Simple     : Boolean := False;
                  Builder    : Leander.Core.Cases.Case_Builder;
               begin

                  if Pats.Last_Index = 1
                    and then (for all X of First_Args =>
                                X.Is_Leaf and then X.Get_Node.Is_Variable)
                  then
                     Value := Exps.First_Element;
                     for I in Fun_Args'Range loop
                        Fun_Args (I) := First_Args (I);
                     end loop;
                  else
                     for I in Fun_Args'Range loop
                        Fun_Args (I) :=
                          Leaf
                            (Leander.Core.Variable
                               (First_Args (I).Head.Source, New_Variable));
                     end loop;

                     if Pat_Count = 0 then
                        null;
                     elsif Pat_Count = 1 then
                        Value :=
                          Leaf
                            (Leander.Core.Variable
                               (Source,
                                Fun_Args (1).Show));
                     else
                        Value :=
                          Leaf
                            (Leander.Core.Constructor
                               (Pats.First_Element.Head.Source,
                                Leander.Primitives.Tuple_Name (Pat_Count)));
                        Leander.Prelude.Use_Tuple (Pat_Count);
                        for I in Fun_Args'Range loop
                           Value :=
                             Apply (Value, Fun_Args (I));
                        end loop;
                     end if;

                     Builder.Set_Case_Expression (Value);

                     for I in 1 .. Pats.Last_Index loop
                        declare
                           Pat   : Tree_Type := Pats (I);
                           Exp   : constant Tree_Type := Exps (I);
                           Args  : constant Array_Of_Trees :=
                                     Pattern_Arguments (Pat);
                        begin
                           if Args'Length /= Pat_Count then
                              Leander.Errors.Error
                                (Pat.Get_Node.Source,
                                 "inconsistent patterns in declaration");
                           end if;
                           if Args'Length = 0 then
                              pragma Assert (I = 1);
                              Value := Exps.Element (I);
                              Simple := True;
                              exit;
                           end if;

                           if Args'Length = 1 then
                              Pat := Args (Args'First);
                           elsif Args'Length > 1 then
                              Pat :=
                                Leaf (Leander.Core.Constructor
                                      (Pat_Source,
                                         Leander.Primitives.Tuple_Name
                                           (Args'Length)));
                              for Arg of Args loop
                                 Pat := Pat.Apply (Arg);
                              end loop;
                           end if;

                           Builder.Add_Alt (Pat, Exp);
                        end;
                     end loop;

                     Value := Builder.Transform;

                  end if;

                  if not Simple then
                     for I in reverse Fun_Args'Range loop
                        Value :=
                          Apply
                            (Leander.Core.Lambda (Source, Fun_Args (I).Show),
                             Value);
                     end loop;
                  end if;
                  if Env.Has_Local_Binding (Name) then
                     Leander.Errors.Error
                       (Pats.First_Element.Head.Source,
                        "redefinition of '" & Name & "'");
                     Leander.Errors.Error
                       (Env.Local_Binding (Name).Head.Source,
                        "original definition of " & Name);
                  else
                     Env.Insert_Value (Name, Value);
                  end if;
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
   end Parse_Value_Bindings;

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
