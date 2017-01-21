with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Strings.Fixed.Hash;

with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Declarations;

with Leander.Core.Cases;

with Leander.Prelude;
with Leander.Primitives;

package body Leander.Parser.Expressions is

   type Fixity_Record is
      record
         Associativity : Associativity_Type := Left;
         Priority      : Priority_Range := 9;
      end record;

   package Fixity_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Fixity_Record,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   Fixities : Fixity_Maps.Map;

   function At_Atomic_Expression return Boolean;
   function At_Atomic_Pattern return Boolean renames At_Atomic_Expression;

   function Parse_Atomic_Expression return Leander.Core.Trees.Tree_Type;
   function Parse_Case_Expression return Leander.Core.Trees.Tree_Type;
   function Parse_Left_Expression return Leander.Core.Trees.Tree_Type;

   function Parse_Atomic_Pattern return Leander.Core.Trees.Tree_Type
     renames Parse_Atomic_Expression;

   function Parse_Pattern return Leander.Core.Trees.Tree_Type
     renames Parse_Expression;

   ----------------
   -- Add_Fixity --
   ----------------

   procedure Add_Fixity
     (Operator      : String;
      Associativity : Associativity_Type;
      Priority      : Priority_Range)
   is
   begin
      Fixities.Insert (Operator, (Associativity, Priority));
   end Add_Fixity;

   --------------------------
   -- At_Atomic_Expression --
   --------------------------

   function At_Atomic_Expression return Boolean is
      use Leander.Parser.Lexical.Set_Of_Tokens;
   begin
      return At_Name
        or else  Tok <= +(Tok_Integer_Literal,
                          Tok_Character_Literal, Tok_Float_Literal,
                          Tok_String_Literal,
                          Tok_Left_Paren, Tok_Left_Bracket);
   end At_Atomic_Expression;

   -------------------
   -- At_Expression --
   -------------------

   function At_Expression return Boolean is
      use Leander.Parser.Lexical.Set_Of_Tokens;
   begin
      return At_Atomic_Expression
        or else Tok <= +Tok_Lambda;
   end At_Expression;

   -----------------------------
   -- Parse_Atomic_Expression --
   -----------------------------

   function Parse_Atomic_Expression return Leander.Core.Trees.Tree_Type is
      Current : constant Leander.Source.Source_Reference :=
                  Current_Source_Reference;

      function Parse_Rest_Of_List
        return Leander.Core.Trees.Tree_Type;

      function Parse_Rest_Of_Tuple
        return Leander.Core.Trees.Array_Of_Trees;

      ------------------------
      -- Parse_Rest_Of_List --
      ------------------------

      function Parse_Rest_Of_List
        return Leander.Core.Trees.Tree_Type
      is
      begin
         if At_Expression then
            declare
               Head : constant Leander.Core.Trees.Tree_Type :=
                        Parse_Expression;
            begin
               if Tok = Tok_Comma then
                  Scan;

                  declare
                     Cons : constant Leander.Core.Core_Node :=
                              Leander.Core.Constructor
                                (Head.Get_Node.Source, ":");
                  begin
                     return Leander.Core.Trees.Apply
                       (Leander.Core.Trees.Apply (Cons, Head),
                        Parse_Rest_Of_List);
                  end;
               else
                  declare
                     Cons : constant Leander.Core.Core_Node :=
                              Leander.Core.Constructor
                                (Head.Get_Node.Source, ":");
                     Empty : constant Leander.Core.Core_Node :=
                               Leander.Core.Constructor
                                 (Head.Get_Node.Source, "[]");
                  begin
                     return Leander.Core.Trees.Apply
                       (Leander.Core.Trees.Apply (Cons, Head), Empty);
                  end;
               end if;
            end;
         else
            Error ("expected an expression");
            return Leander.Core.Trees.Leaf
              (Leander.Core.Constructor (Current, "[]"));
         end if;
      end Parse_Rest_Of_List;

      -------------------------
      -- Parse_Rest_Of_Tuple --
      -------------------------

      function Parse_Rest_Of_Tuple
        return Leander.Core.Trees.Array_Of_Trees
      is
         use type Leander.Core.Trees.Array_Of_Trees;
      begin
         if At_Expression then
            declare
               E : constant Leander.Core.Trees.Tree_Type := Parse_Expression;
            begin
               if Tok = Tok_Comma then
                  Scan;
                  if not At_Expression then
                     Error ("missing expression");
                     return (1 => E);
                  else
                     return E & Parse_Rest_Of_Tuple;
                  end if;
               else
                  return (1 => E);
               end if;
            end;
         else
            declare
               Empty : Leander.Core.Trees.Array_Of_Trees (1 .. 0);
            begin
               return Empty;
            end;
         end if;
      end Parse_Rest_Of_Tuple;

   begin
      if At_Identifier then
         declare
            Name : constant String := Scan_Identifier;
         begin
            if Name (Name'First) in 'A' .. 'Z'
              or else Name (Name'First) = ':'
            then
               return Leander.Core.Trees.Leaf
                 (Leander.Core.Constructor (Current, Name));
            else
               return Leander.Core.Trees.Leaf
                 (Leander.Core.Variable (Current, Name));
            end if;
         end;
      elsif Tok = Tok_Integer_Literal then
         declare
            Literal : constant String := Tok_Text;
            Expr    : constant Leander.Core.Trees.Tree_Type :=
                        Leander.Core.Trees.Leaf
                          (Leander.Core.Literal
                             (Current, Literal));
         begin
            Scan;
            Expr.Set_Annotation (Leander.Primitives.Int_Type);
            return Expr;
         end;
      elsif Tok = Tok_Character_Literal then
         declare
            Ch : constant Character := Tok_Character_Value;
            Expr    : constant Leander.Core.Trees.Tree_Type :=
                        Leander.Core.Trees.Leaf
                          (Leander.Core.Literal
                             (Current, Natural'Image (Character'Pos (Ch))));
         begin
            Scan;
            Expr.Set_Annotation (Leander.Primitives.Char_Type);
            return Expr;
         end;
      elsif Tok = Tok_String_Literal then
         declare
            S : constant String := Tok_Text;
            E : Leander.Core.Trees.Tree_Type :=
                  Leander.Core.Trees.Leaf
                    (Leander.Core.Constructor
                       (Current, "[]"));
         begin
            for Ch of reverse S loop
               declare

                  App : constant Leander.Core.Trees.Tree_Type :=
                          Leander.Core.Trees.Apply
                            (Leander.Core.Constructor (Current, ":"),
                             Leander.Core.Literal
                               (Current,
                                (Natural'Image (Character'Pos (Ch)))));
               begin
                  App.Right.Set_Annotation (Leander.Primitives.Char_Type);
                  E := App.Apply (E);
               end;
            end loop;
            Scan;
            return E;
         end;
      elsif Tok = Tok_Left_Paren and then Next_Tok = Tok_Right_Paren then
         Scan;
         Scan;
         return Leander.Core.Trees.Leaf
           (Leander.Core.Constructor (Current, "()"));
      elsif Tok = Tok_Left_Paren then
         Scan;
         if At_Operator then
            declare
               Name : constant String := Scan_Identifier;
            begin
               if At_Expression then
                  declare
                     Expr : Leander.Core.Trees.Tree_Type :=
                              Parse_Left_Expression;
                     X    : constant String := New_Variable;
                  begin
                     Expr :=
                       Leander.Core.Trees.Apply
                         (Leander.Core.Trees.Apply
                            (Leander.Core.Variable (Current, Name),
                             Leander.Core.Variable (Current, X)),
                          Expr);
                     Expr :=
                       Leander.Core.Trees.Apply
                         (Leander.Core.Lambda (Current, X),
                          Expr);
                     if Tok = Tok_Right_Paren then
                        Scan;
                     else
                        Error ("missing ')'");
                     end if;
                     return Expr;
                  end;
               else
                  Error ("missing expression");
                  return Leander.Core.Trees.Empty;
               end if;
            end;
         else
            declare
               use type Leander.Core.Trees.Array_Of_Trees;
               Expr : Leander.Core.Trees.Tree_Type := Parse_Expression;
            begin
               if Tok = Tok_Comma then
                  Scan;
                  declare
                     Tuple : constant Leander.Core.Trees.Array_Of_Trees :=
                               Expr & Parse_Rest_Of_Tuple;
                  begin
                     Leander.Prelude.Use_Tuple (Tuple'Length);
                     Expr :=
                       Leander.Core.Trees.Leaf
                         (Leander.Core.Constructor
                            (Current,
                             Leander.Primitives.Tuple_Name (Tuple'Length)));
                     for I in Tuple'Range loop
                        Expr := Expr.Apply (Tuple (I));
                     end loop;
                  end;
               end if;
               if Tok = Tok_Right_Paren then
                  Scan;
               else
                  Error ("missing ')'");
               end if;
               return Expr;
            end;
         end if;
      elsif Tok = Tok_Left_Bracket then
         declare
            use Leander.Core, Leander.Core.Trees;
            Expr : Tree_Type;
         begin
            Scan;
            if Tok = Tok_Right_Bracket then
               Expr :=
                 Leander.Core.Trees.Leaf
                   (Leander.Core.Constructor
                      (Current, "[]"));
            else
               Expr := Parse_Expression;

               if Tok = Tok_Right_Bracket then
                  Expr :=
                    Apply
                      (Apply
                         (Constructor (Current, ":"),
                          Expr),
                       Constructor (Current, "[]"));

               elsif Tok = Tok_Dot_Dot then
                  Scan;

                  if At_Expression then
                     declare
                        Finish : constant Tree_Type := Parse_Expression;
                     begin
                        Expr :=
                          Apply
                            (Apply (Variable (Current, "enumFromTo"), Expr),
                             Finish);
                     end;
                  else
                     Expr :=
                       Apply (Variable (Current, "enumFrom"), Expr);
                  end if;

               elsif Tok = Tok_Comma then
                  Scan;

                  declare
                     Next : Tree_Type := Parse_Expression;
                  begin
                     if Tok = Tok_Right_Bracket then
                        Next :=
                          Apply
                            (Apply
                               (Constructor (Current, ":"),
                                Next),
                             Constructor (Current, "[]"));

                        Expr :=
                          Apply
                            (Apply
                               (Constructor (Current, ":"),
                                Expr),
                             Next);
                     elsif Tok = Tok_Dot_Dot then
                        Scan;
                        if Tok = Tok_Right_Bracket then
                           Expr :=
                             Apply (Variable (Current, "enumFromThen"),
                                    Expr).
                             Apply (Next);
                        elsif At_Expression then
                           declare
                              Finish : constant Tree_Type := Parse_Expression;
                           begin
                              Expr :=
                                Apply (Variable (Current, "enumFromThenTo"),
                                       Expr).
                                Apply (Next).
                                Apply (Finish);
                           end;
                        else
                           Error ("expected an expression");
                        end if;
                     elsif At_Expression then
                        declare
                           Rest : Tree_Type :=
                                    Parse_Rest_Of_List;
                        begin
                           Rest :=
                             Apply (Variable (Current, ":"),
                                    Next)
                             .Apply (Rest);

                           Expr :=
                             Apply (Variable (Current, ":"),
                                    Expr)
                             .Apply (Rest);
                        end;
                     else
                        Error ("expected an expression");
                     end if;
                  end;
               else
                  Error ("expected a list");
               end if;
            end if;
            if Tok = Tok_Right_Bracket then
               Scan;
            else
               Error ("missing ']'");
            end if;
            return Expr;
         end;
      else
         Error ("expected atomic expression");
         Scan;
         return Leander.Core.Trees.Leaf
           (Leander.Core.Variable (Current, "_"));
      end if;
   end Parse_Atomic_Expression;

   ---------------------------
   -- Parse_Case_Expression --
   ---------------------------

   function Parse_Case_Expression return Leander.Core.Trees.Tree_Type is
   begin
      pragma Assert (Tok = Tok_Case);
      Scan;

      declare
         E : constant Leander.Core.Trees.Tree_Type := Parse_Expression;
         Builder : Leander.Core.Cases.Case_Builder;
      begin
         Builder.Set_Case_Expression (E);
         if Tok = Tok_Of then
            Scan;
         else
            Error ("missing 'of'");
         end if;

         declare
            Indent : constant Positive := Tok_Indent;
         begin
            while Tok_Indent >= Indent
              and then At_Atomic_Pattern
            loop
               declare
                  Pat : constant Leander.Core.Trees.Tree_Type :=
                          Leander.Parser.Expressions.Parse_Pattern;
               begin
                  if Tok = Tok_Right_Arrow then
                     Scan;
                  else
                     Error ("missing '->'");
                  end if;

                  declare
                     Exp : constant Leander.Core.Trees.Tree_Type :=
                             Parse_Expression;
                  begin
                     Builder.Add_Alt (Pat, Exp);
                  end;
               end;
            end loop;
         end;

         return Builder.Transform;
      end;
   end Parse_Case_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression return Leander.Core.Trees.Tree_Type is
      use Leander.Core, Leander.Core.Trees;

      package Tree_Stacks is
        new Ada.Containers.Doubly_Linked_Lists (Tree_Type);

      Operator_Stack : Tree_Stacks.List;
      Value_Stack : Tree_Stacks.List;

      procedure Pop_Operator;

      procedure Push_Operator
        (Operator : Tree_Type);

      ------------------
      -- Pop_Operator --
      ------------------

      procedure Pop_Operator is
         Operator       : constant Tree_Type :=
                            Operator_Stack.Last_Element;
         Right, Left    : Tree_Type;
      begin
         Operator_Stack.Delete_Last;

         Right := Value_Stack.Last_Element;
         Value_Stack.Delete_Last;
         Left := Value_Stack.Last_Element;
         Value_Stack.Delete_Last;
         Value_Stack.Append (Operator.Apply (Left).Apply (Right));
      end Pop_Operator;

      -------------------
      -- Push_Operator --
      -------------------

      procedure Push_Operator
        (Operator : Tree_Type)
      is
         Op_Fixity : Fixity_Record;
         Op_Name : constant String := Operator.Get_Node.Show;
      begin
         if Fixities.Contains (Op_Name) then
            Op_Fixity := Fixities.Element (Op_Name);
         else
            Fixities.Insert (Op_Name, Op_Fixity);
         end if;

         while not Operator_Stack.Is_Empty loop
            declare
               Top : constant Tree_Type :=
                       Operator_Stack.Last_Element;
               Top_Fixity : constant Fixity_Record :=
                              Fixities.Element (Top.Get_Node.Show);
               Pop : Boolean;
            begin
               Pop :=
                 (Op_Fixity.Associativity = Left
                  and then Op_Fixity.Priority >= Top_Fixity.Priority)
                 or else (Op_Fixity.Associativity /= Left
                          and then Op_Fixity.Priority > Top_Fixity.Priority);

               if Pop then
                  Pop_Operator;
               else
                  exit;
               end if;
            end;
         end loop;

         Operator_Stack.Append (Operator);
      end Push_Operator;

   begin

      Value_Stack.Append (Parse_Left_Expression);

      while At_Operator loop
         declare
            Current  : constant Leander.Source.Source_Reference :=
                         Current_Source_Reference;
            Is_Con   : constant Boolean := At_Constructor_Op;
            Name     : constant String := Scan_Identifier;
            Operator : constant Tree_Type :=
                         Leaf
                           (if Is_Con
                            then Constructor (Current, Name)
                            else Variable (Current, Name));
         begin
            Push_Operator (Operator);
            Value_Stack.Append (Parse_Left_Expression);
         end;
      end loop;

      while not Operator_Stack.Is_Empty loop
         Pop_Operator;
      end loop;

      return Value_Stack.First_Element;
   end Parse_Expression;

   ---------------------------
   -- Parse_Left_Expression --
   ---------------------------

   function Parse_Left_Expression return Leander.Core.Trees.Tree_Type is
      Current : constant Leander.Source.Source_Reference :=
                  Current_Source_Reference;
   begin
      if At_Atomic_Expression then
         declare
            Indent  : constant Positive := Tok_Indent;
            Expr    : Leander.Core.Trees.Tree_Type :=
                        Parse_Atomic_Expression;
         begin
            while At_Atomic_Expression
              and then Tok_Indent > Indent
            loop
               Expr := Expr.Apply (Parse_Atomic_Expression);
            end loop;
            return Expr;
         end;
      elsif Tok = Tok_Lambda then
         Scan;
         declare
            Name : constant String :=
                     (if At_Variable then Tok_Text else "_");
         begin
            if not At_Variable then
               Error ("missing lambda variable");
            else
               Scan;
            end if;

            if Tok = Tok_Right_Arrow then
               Scan;
            else
               Error ("missing ->");
            end if;

            declare
               Expr : constant Leander.Core.Trees.Tree_Type :=
                        Parse_Expression;
            begin
               return Leander.Core.Trees.Leaf
                 (Leander.Core.Lambda (Current, Name)).Apply (Expr);
            end;
         end;
      elsif Tok = Tok_Case then
         return Parse_Case_Expression;
      elsif Tok = Tok_Let then
         declare
            Env    : Leander.Environments.Environment;
            Indent : constant Positive := Tok_Indent;
         begin
            Env.Create ("let-env");
            Scan;
            Leander.Parser.Declarations.Parse_Value_Bindings (Env, Indent);

            if Tok = Tok_In then
               Scan;
            else
               Error ("missing 'in'");
            end if;

            declare
--                 Bindings : Leander.Core.Trees.Tree_Type :=
--                              Leander.Core.Trees.Empty;
               Expr     : constant Leander.Core.Trees.Tree_Type :=
                            Parse_Expression;

--                 procedure Add_Binding
--                   (Name : String;
--                    Tree : Leander.Core.Trees.Tree_Type;
--                    Signature : Leander.Types.Trees.Tree_Type);

            begin
--                 Env.Scan_Local_Bindings (Add_Binding'Access);
--                 Expr :=
--                   Leander.Core.Trees.Apply
--                     (Leander.Core.Let (Current),
--                      Leander.Core.Trees.Apply
--                        (Expr, Bindings));
               return Expr;
            end;
         end;
      elsif Tok = Tok_If then
         declare
            Cond, True_Expr, False_Expr : Leander.Core.Trees.Tree_Type;
            True_Pos, False_Pos         : Leander.Source.Source_Reference;
         begin
            Scan;
            Cond := Parse_Expression;
            if Tok = Tok_Semi then
               Scan;
            end if;
            if Tok = Tok_Then then
               Scan;
            else
               Error ("missing 'then'");
            end if;
            True_Pos := Current_Source_Reference;
            True_Expr := Parse_Expression;
            if Tok = Tok_Semi then
               Scan;
            end if;
            if Tok = Tok_Else then
               Scan;
            else
               Error ("missing 'else'");
            end if;
            False_Pos := Current_Source_Reference;
            False_Expr := Parse_Expression;

            declare
               Builder : Leander.Core.Cases.Case_Builder;
            begin
               Builder.Set_Case_Expression (Cond);
               Builder.Add_Alt
                 (Leander.Core.Trees.Leaf
                    (Leander.Core.Constructor
                         (False_Pos, "False")),
                  False_Expr);
               Builder.Add_Alt
                 (Leander.Core.Trees.Leaf
                    (Leander.Core.Constructor
                         (True_Pos, "True")),
                  True_Expr);
               return Builder.Transform;
            end;
         end;
      else
         Internal_Error ("expected to be at an expression: " & Tok_Text);
         return Leander.Core.Trees.Empty;
      end if;

   end Parse_Left_Expression;

begin
   Add_Fixity (":", Right, 5);
end Leander.Parser.Expressions;
