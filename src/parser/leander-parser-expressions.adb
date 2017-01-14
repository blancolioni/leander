with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Strings.Fixed.Hash;

with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Prelude;
with Leander.Primitives;

with Leander.Syntax.Expressions;

package body Leander.Parser.Expressions is

   type Associativity_Type is (Left, Right, None);
   pragma Unreferenced (Right, None);
   type Priority_Range is range 0 .. 9;

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

   function Parse_Atomic_Expression return Leander.Syntax.Syntax_Tree;
   function Parse_Case_Expression return Leander.Syntax.Syntax_Tree;
   function Parse_Left_Expression return Leander.Syntax.Syntax_Tree;

   function Parse_Atomic_Pattern return Leander.Syntax.Syntax_Tree
     renames Parse_Atomic_Expression;

   function Parse_Pattern return Leander.Syntax.Syntax_Tree
     renames Parse_Expression;

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

   function Parse_Atomic_Expression return Leander.Syntax.Syntax_Tree is
      Current : constant Leander.Source.Source_Reference :=
                  Current_Source_Reference;

      function Parse_Rest_Of_Tuple
        return Leander.Syntax.Array_Of_Syntax_Trees;

      -------------------------
      -- Parse_Rest_Of_Tuple --
      -------------------------

      function Parse_Rest_Of_Tuple
        return Leander.Syntax.Array_Of_Syntax_Trees
      is
         use type Leander.Syntax.Array_Of_Syntax_Trees;
      begin
         if At_Expression then
            declare
               E : constant Leander.Syntax.Syntax_Tree_Record :=
                     Leander.Syntax.Syntax_Tree_Record
                       (Parse_Expression);
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
               Empty : Leander.Syntax.Array_Of_Syntax_Trees (1 .. 0);
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
               return Leander.Syntax.Expressions.Constructor (Current, Name);
            else
               return Leander.Syntax.Expressions.Variable (Current, Name);
            end if;
         end;
      elsif Tok = Tok_Integer_Literal then
         declare
            Literal : constant String := Tok_Text;
         begin
            Scan;
            return Leander.Syntax.Expressions.Literal
              (Current, Literal, Leander.Primitives.Int_Type);
         end;
      elsif Tok = Tok_Character_Literal then
         declare
            Ch : constant Character := Tok_Character_Value;
         begin
            Scan;
            return Leander.Syntax.Expressions.Literal
              (Current, Natural'Image (Character'Pos (Ch)),
               Leander.Primitives.Char_Type);
         end;
      elsif Tok = Tok_String_Literal then
         declare
            S : constant String := Tok_Text;
            E : Leander.Syntax.Syntax_Tree :=
                  Leander.Syntax.Expressions.Constructor
                    (Current, "[]");
         begin
            for Ch of reverse S loop
               declare
                  App : constant Leander.Syntax.Syntax_Tree :=
                          Leander.Syntax.Expressions.Apply
                            (Current,
                             Leander.Syntax.Expressions.Constructor
                               (Current, ":"),
                             Leander.Syntax.Expressions.Literal
                               (Current, (Natural'Image (Character'Pos (Ch))),
                                Leander.Primitives.Char_Type));
               begin
                  E := Leander.Syntax.Expressions.Apply (Current, App, E);
               end;
            end loop;
            Scan;
            return E;
         end;
      elsif Tok = Tok_Left_Paren then
         Scan;
         declare
            use type Leander.Syntax.Array_Of_Syntax_Trees;
            Expr : Leander.Syntax.Syntax_Tree := Parse_Expression;
         begin
            if Tok = Tok_Comma then
               Scan;
               declare
                  Tuple : constant Leander.Syntax.Array_Of_Syntax_Trees :=
                            Leander.Syntax.Syntax_Tree_Record (Expr)
                          & Parse_Rest_Of_Tuple;
               begin
                  Leander.Prelude.Use_Tuple (Tuple'Length);
                  Expr :=
                    Leander.Syntax.Expressions.Constructor
                      (Current,
                       Leander.Primitives.Tuple_Name (Tuple'Length));
                  for I in Tuple'Range loop
                     Expr :=
                       Leander.Syntax.Expressions.Apply
                         (Tuple (I).Source, Expr, Tuple (I));
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
      elsif Tok = Tok_Left_Bracket then
         if Next_Tok = Tok_Right_Bracket then
            declare
               Expr : constant Leander.Syntax.Syntax_Tree :=
                        Leander.Syntax.Expressions.Constructor
                          (Current, "[]");
            begin
               Scan;
               Scan;
               return Expr;
            end;
         else
            Error ("lists not implemented yet");
            while Tok /= Tok_Right_Bracket
              and then Tok /= Tok_End_Of_File
            loop
               Scan;
            end loop;
            if Tok = Tok_Right_Bracket then
               Scan;
            end if;
            return Leander.Syntax.Expressions.Variable (Current, "_");
         end if;
      else
         Error ("expected atomic expression");
         Scan;
         return Leander.Syntax.Expressions.Variable (Current, "_");
      end if;
   end Parse_Atomic_Expression;

   ---------------------------
   -- Parse_Case_Expression --
   ---------------------------

   function Parse_Case_Expression return Leander.Syntax.Syntax_Tree is
      Current : constant Leander.Source.Source_Reference :=
                  Current_Source_Reference;
   begin
      pragma Assert (Tok = Tok_Case);
      Scan;

      declare
         E : constant Leander.Syntax.Syntax_Tree := Parse_Expression;
         Result : constant Leander.Syntax.Syntax_Tree :=
                    Leander.Syntax.Expressions.Case_Expression
                      (Current, E);
      begin
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
                  Pat : constant Leander.Syntax.Syntax_Tree :=
                          Leander.Parser.Expressions.Parse_Pattern;
               begin
                  if Tok = Tok_Right_Arrow then
                     Scan;
                  else
                     Error ("missing '->'");
                  end if;

                  declare
                     Exp : constant Leander.Syntax.Syntax_Tree :=
                             Parse_Expression;
                  begin
                     Leander.Syntax.Expressions.Add_Case_Alternate
                       (Case_Expr  => Result,
                        Pattern    => Pat,
                        Expression => Exp);
                  end;
               end;
            end loop;
         end;

         return Result;
      end;
   end Parse_Case_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression return Leander.Syntax.Syntax_Tree is
      use Leander.Syntax;

      package Tree_Stacks is
        new Ada.Containers.Doubly_Linked_Lists (Syntax_Tree_Record);

      Operator_Stack : Tree_Stacks.List;
      Value_Stack : Tree_Stacks.List;

      procedure Pop_Operator;

      procedure Push_Operator
        (Operator : Syntax_Tree_Record);

      ------------------
      -- Pop_Operator --
      ------------------

      procedure Pop_Operator is
         Operator       : constant Syntax_Tree_Record :=
                            Operator_Stack.Last_Element;
         Right, Left    : Syntax_Tree_Record;
      begin
         Operator_Stack.Delete_Last;

         Right := Value_Stack.Last_Element;
         Value_Stack.Delete_Last;
         Left := Value_Stack.Last_Element;
         Value_Stack.Delete_Last;
         Value_Stack.Append
           (Syntax_Tree_Record
              (Leander.Syntax.Expressions.Apply
                   (Left.Source,
                    Leander.Syntax.Expressions.Apply
                      (Left.Source, Operator, Left),
                    Right)));
      end Pop_Operator;

      -------------------
      -- Push_Operator --
      -------------------

      procedure Push_Operator
        (Operator : Syntax_Tree_Record)
      is
         Op_Fixity : Fixity_Record;
      begin
         if Fixities.Contains (Operator.Show) then
            Op_Fixity := Fixities.Element (Operator.Show);
         else
            Fixities.Insert (Operator.Show, Op_Fixity);
         end if;

         while not Operator_Stack.Is_Empty loop
            declare
               Top : constant Syntax_Tree_Record :=
                       Operator_Stack.Last_Element;
               Top_Fixity : constant Fixity_Record :=
                              Fixities.Element (Top.Show);
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

      Value_Stack.Append (Syntax_Tree_Record (Parse_Left_Expression));

      while At_Operator loop
         declare
            Current  : constant Leander.Source.Source_Reference :=
                         Current_Source_Reference;
            Is_Con   : constant Boolean := At_Constructor_Op;
            Name     : constant String := Scan_Identifier;
            Operator : constant Syntax_Tree_Record :=
                         (if Is_Con
                          then Leander.Syntax.Expressions.Constructor
                            (Current, Name)
                          else Leander.Syntax.Expressions.Variable
                            (Current, Name));
         begin
            Push_Operator (Operator);
            Value_Stack.Append (Syntax_Tree_Record (Parse_Left_Expression));
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

   function Parse_Left_Expression return Leander.Syntax.Syntax_Tree is
      Current : constant Leander.Source.Source_Reference :=
                  Current_Source_Reference;
   begin
      if At_Atomic_Expression then
         declare
            Indent  : constant Positive := Tok_Indent;
            Expr    : Leander.Syntax.Syntax_Tree :=
                        Parse_Atomic_Expression;
         begin
            while At_Atomic_Expression
              and then Tok_Indent > Indent
            loop
               Expr :=
                 Leander.Syntax.Expressions.Apply
                   (Current, Expr, Parse_Atomic_Expression);
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
               Expr : constant Leander.Syntax.Syntax_Tree :=
                        Parse_Expression;
            begin
               return Leander.Syntax.Expressions.Lambda (Current, Name, Expr);
            end;
         end;
      elsif Tok = Tok_Case then
         return Parse_Case_Expression;
      elsif Tok = Tok_If then
         declare
            Cond, True_Expr, False_Expr : Leander.Syntax.Syntax_Tree_Record;
            True_Pos, False_Pos         : Leander.Source.Source_Reference;
         begin
            Scan;
            Cond := Leander.Syntax.Syntax_Tree_Record (Parse_Expression);
            if Tok = Tok_Semi then
               Scan;
            end if;
            if Tok = Tok_Then then
               Scan;
            else
               Error ("missing 'then'");
            end if;
            True_Pos := Current_Source_Reference;
            True_Expr := Leander.Syntax.Syntax_Tree_Record (Parse_Expression);
            if Tok = Tok_Semi then
               Scan;
            end if;
            if Tok = Tok_Else then
               Scan;
            else
               Error ("missing 'else'");
            end if;
            False_Pos := Current_Source_Reference;
            False_Expr := Leander.Syntax.Syntax_Tree_Record (Parse_Expression);

            declare
               Result : constant Leander.Syntax.Syntax_Tree :=
                          Leander.Syntax.Expressions.Case_Expression
                            (Current, Cond);
            begin
               Leander.Syntax.Expressions.Add_Case_Alternate
                 (Result,
                  Leander.Syntax.Expressions.Constructor
                    (True_Pos, "True"),
                  True_Expr);
               Leander.Syntax.Expressions.Add_Case_Alternate
                 (Result,
                  Leander.Syntax.Expressions.Constructor
                    (False_Pos, "False"),
                  False_Expr);
               return Result;
            end;
         end;
      else
         raise Constraint_Error with
           "expected to be at an expression";
      end if;

   end Parse_Left_Expression;

end Leander.Parser.Expressions;
