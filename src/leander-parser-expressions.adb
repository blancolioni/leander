with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;
with Leander.Core.Literals;
with Leander.Core.Types;

with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

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
   function Parse_Atomic_Expression
     return Leander.Core.Expressions.Reference;
   function Parse_Left_Expression
     return Leander.Core.Expressions.Reference;

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
   begin
      return At_Name
        or else  Tok <= [Tok_Integer_Literal,
                         Tok_Character_Literal, Tok_Float_Literal,
                         Tok_String_Literal,
                         Tok_Left_Paren, Tok_Left_Bracket];
   end At_Atomic_Expression;

   -------------------
   -- At_Expression --
   -------------------

   function At_Expression return Boolean is
   begin
      return At_Atomic_Expression
        or else (At_Operator and then Tok_Text = "-")
        or else Tok <= +Tok_Lambda;
   end At_Expression;

   function Parse_Atomic_Expression
     return Leander.Core.Expressions.Reference
   is
      use Leander.Core.Expressions;
   begin
      if At_Variable then
         return Var : constant Reference :=
           Variable (Leander.Core.Id (Tok_Text))
         do
            Scan;
         end return;
      elsif Tok = Tok_Integer_Literal then
         return Lit : constant Reference :=
           Literal (Leander.Core.Literals.Integer_Literal (Tok_Text))
         do
            Scan;
         end return;
      elsif Tok = Tok_Left_Paren then
         Scan;
         return E : constant Reference := Parse_Expression do
            Expect (Tok_Right_Paren, []);
         end return;
      else
         Error ("expected an atomic expression");
         raise Parse_Error;
      end if;
   end Parse_Atomic_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression return Leander.Core.Expressions.Reference is
      use Leander.Core.Expressions;

      package Tree_Stacks is
        new Ada.Containers.Doubly_Linked_Lists (Core.Expressions.Reference);

      Operator_Stack : Tree_Stacks.List;
      Value_Stack    : Tree_Stacks.List;

      procedure Pop_Operator;

      procedure Push_Operator
        (Operator : Core.Expressions.Reference);

      procedure Push_Value (Value : Core.Expressions.Reference);
      function Pop_Value return Core.Expressions.Reference;

      ------------------
      -- Pop_Operator --
      ------------------

      procedure Pop_Operator is
         Operator : constant Reference := Operator_Stack.Last_Element;
         Right    : constant Reference := Pop_Value;
         Left     : constant Reference := Pop_Value;
      begin
         Operator_Stack.Delete_Last;
         Push_Value (Apply (Apply (Operator, Left), Right));
      end Pop_Operator;

      ---------------
      -- Pop_Value --
      ---------------

      function Pop_Value return Core.Expressions.Reference is
      begin
         if Value_Stack.Is_Empty then
            raise Parse_Error;
         end if;

         return V : constant Reference := Value_Stack.Last_Element do
            Value_Stack.Delete_Last;
         end return;
      end Pop_Value;

      -------------------
      -- Push_Operator --
      -------------------

      procedure Push_Operator
        (Operator : Core.Expressions.Reference)
      is
         Op_Fixity : Fixity_Record;
         Op_Name   : constant String := Operator.Show;
      begin
         if Fixities.Contains (Op_Name) then
            Op_Fixity := Fixities.Element (Op_Name);
         else
            Fixities.Insert (Op_Name, Op_Fixity);
         end if;

         while not Operator_Stack.Is_Empty loop
            declare
               Top        : constant Reference :=
                              Operator_Stack.Last_Element;
               Top_Fixity : constant Fixity_Record :=
                              Fixities.Element (Top.Show);
               Pop        : Boolean;
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

      ----------------
      -- Push_Value --
      ----------------

      procedure Push_Value (Value : Core.Expressions.Reference) is
      begin
         Value_Stack.Append (Value);
      end Push_Value;

   begin

      Push_Value (Parse_Left_Expression);

      while At_Operator loop
         declare
            Is_Con   : constant Boolean := At_Constructor_Op;
            Name     : constant String := Scan_Identifier;
            Operator : constant Reference :=
                         (if Is_Con
                          then Constructor (Core.Id (Name), Core.Types.T_Error)
                          else Variable (Core.Id (Name)));
         begin
            Push_Operator (Operator);
            Push_Value (Parse_Left_Expression);
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

   function Parse_Left_Expression
     return Leander.Core.Expressions.Reference
   is
   begin
      if At_Atomic_Expression then
         declare
            Indent  : constant Positive := Tok_Indent;
            Expr    : Leander.Core.Expressions.Reference :=
                        Parse_Atomic_Expression;
         begin
            while At_Atomic_Expression
              and then Tok_Indent > Indent
            loop
               Expr := Core.Expressions.Apply
                 (Expr, Parse_Atomic_Expression);
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
               Expr : constant Leander.Core.Expressions.Reference :=
                        Parse_Expression;
            begin
               return Leander.Core.Expressions.Lambda
                 (Leander.Core.Id (Name),
                  Expr);
            end;
         end;
      else
         Error ("expected to be at an expression");
         raise Parse_Error;
      end if;
   end Parse_Left_Expression;

end Leander.Parser.Expressions;
