with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;

with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Bindings;
with Leander.Syntax.Bindings;
with Leander.Syntax.Patterns;

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
     return Leander.Syntax.Expressions.Reference;
   function Parse_Left_Expression
     return Leander.Syntax.Expressions.Reference;

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
        or else Tok <= [Tok_Lambda, Tok_Let];
   end At_Expression;

   -----------------------------
   -- Parse_Atomic_Expression --
   -----------------------------

   function Parse_Atomic_Expression
     return Leander.Syntax.Expressions.Reference
   is
      use Leander.Syntax.Expressions;
      Loc : constant Source.Source_Location := Current_Source_Location;

      function Parse_Rest_Of_List
        return Leander.Syntax.Expressions.Reference
        with Pre => At_Expression;

      ------------------------
      -- Parse_Rest_Of_List --
      ------------------------

      function Parse_Rest_Of_List
        return Leander.Syntax.Expressions.Reference
      is
         Loc : constant Source.Source_Location := Current_Source_Location;
         X   : constant Reference := Parse_Expression;
         XS  : Reference;
      begin
         if Tok = Tok_Comma then
            Scan;
            if At_Expression then
               XS := Parse_Rest_Of_List;
            else
               Error ("missing list element");
               while Tok /= Tok_Right_Bracket
                 and then Tok /= Tok_End_Of_File
               loop
                  Scan;
               end loop;
               XS := Constructor
                 (Current_Source_Location, "[]");
               if Tok = Tok_Right_Bracket then
                  Scan;
               end if;
            end if;
         elsif Tok = Tok_Right_Bracket then
            XS := Constructor
              (Current_Source_Location, "[]");
            Scan;
         else
            Error ("missing ']'");
            XS := Constructor
              (Current_Source_Location, "[]");
         end if;

         return Syntax.Expressions.Application
           (Loc,
            Syntax.Expressions.Application
              (Loc,
               Constructor (Loc, ":"),
               X),
            Xs);
      end Parse_Rest_Of_List;

   begin
      if At_Variable_Name then
         return Var : constant Reference :=
           Variable (Loc, Scan_Identifier);
      elsif At_Constructor_Name then
         return Con : constant Reference :=
           Constructor (Loc, Scan_Identifier);
      elsif Tok = Tok_Integer_Literal then
         return Lit : constant Reference :=
           Integer_Literal (Loc, Tok_Text)
         do
            Scan;
         end return;
      elsif Tok = Tok_Left_Paren then
         Scan;
         return E : constant Reference := Parse_Expression do
            Expect (Tok_Right_Paren, []);
         end return;
      elsif Tok = Tok_Left_Bracket then
         Scan;
         if Tok = Tok_Right_Bracket then
            Scan;
            return Constructor (Loc, "[]");
         elsif At_Expression then
            return Parse_Rest_Of_List;
         else
            Error ("missing list");
            return Constructor (Loc, "[]");
         end if;
      else
         Error ("expected an atomic expression");
         raise Parse_Error;
      end if;
   end Parse_Atomic_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression return Leander.Syntax.Expressions.Reference is
      use Leander.Syntax.Expressions;

      package Expression_Stacks is
        new Ada.Containers.Doubly_Linked_Lists (Syntax.Expressions.Reference);

      package Operator_Stacks is
        new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

      Operator_Stack : Operator_Stacks.List;
      Value_Stack    : Expression_Stacks.List;

      procedure Pop_Operator;

      procedure Push_Operator
        (Op_Name : String);

      procedure Push_Value (Value : Syntax.Expressions.Reference);
      function Pop_Value return Syntax.Expressions.Reference;

      ------------------
      -- Pop_Operator --
      ------------------

      procedure Pop_Operator is
         Op_Name  : constant String := Operator_Stack.Last_Element;
         Right    : constant Reference := Pop_Value;
         Left     : constant Reference := Pop_Value;
         Operator : constant Reference :=
                      (if Is_Constructor (Op_Name)
                       then Constructor (Left.Location, Op_Name)
                       else Variable (Left.Location, Op_Name));
      begin
         Operator_Stack.Delete_Last;
         Push_Value (Application
                     (Left.Location,
                        Application
                          (Left.Location, Operator, Left), Right));
      end Pop_Operator;

      ---------------
      -- Pop_Value --
      ---------------

      function Pop_Value return Syntax.Expressions.Reference is
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
        (Op_Name : String)
      is
         Op_Fixity : Fixity_Record;
      begin
         if Fixities.Contains (Op_Name) then
            Op_Fixity := Fixities.Element (Op_Name);
         else
            Fixities.Insert (Op_Name, Op_Fixity);
         end if;

         while not Operator_Stack.Is_Empty loop
            declare
               Top        : constant String :=
                              Operator_Stack.Last_Element;
               Top_Fixity : constant Fixity_Record :=
                              Fixities.Element (Top);
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

         Operator_Stack.Append (Op_Name);
      end Push_Operator;

      ----------------
      -- Push_Value --
      ----------------

      procedure Push_Value (Value : Syntax.Expressions.Reference) is
      begin
         Value_Stack.Append (Value);
      end Push_Value;

   begin

      Push_Value (Parse_Left_Expression);

      while At_Operator loop
         declare
            Name     : constant String := Scan_Identifier;
         begin
            Push_Operator (Name);
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
     return Leander.Syntax.Expressions.Reference
   is
   begin
      if At_Atomic_Expression then
         declare
            Indent  : constant Positive := Tok_Indent;
            Expr    : Leander.Syntax.Expressions.Reference :=
                        Parse_Atomic_Expression;
         begin
            while At_Atomic_Expression
              and then Tok_Indent > Indent
            loop
               declare
                  Loc : constant Source.Source_Location :=
                          Current_Source_Location;
               begin
                  Expr :=
                    Syntax.Expressions.Application
                      (Loc,
                       Expr,
                       Parse_Atomic_Expression);
               end;
            end loop;
            return Expr;
         end;
      elsif Tok = Tok_Lambda then
         Scan;
         declare
            Loc  : constant Source.Source_Location :=
                     Current_Source_Location;
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
               Expr : constant Leander.Syntax.Expressions.Reference :=
                        Parse_Expression;
            begin
               return Leander.Syntax.Expressions.Lambda
                 (Loc,
                  Syntax.Patterns.Variable (Loc, Name), Expr);
            end;
         end;
      elsif Tok = Tok_Let then
         declare
            Loc : constant Source.Source_Location := Current_Source_Location;
            Bs  : constant Leander.Syntax.Bindings.Reference :=
                    Leander.Syntax.Bindings.Empty;
         begin
            Scan;
            if Tok = Tok_Left_Brace then
               Scan;
               loop
                  Bindings.Parse_Binding (Bs);
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
               while Bindings.At_Binding loop
                  Bindings.Parse_Binding (Bs);
               end loop;
            end if;
            if Tok /= Tok_In then
               Error ("expected 'in' or binding at " & Tok'Image);
            else
               Scan;
            end if;

            declare
               Expr : constant Syntax.Expressions.Reference :=
                        Parse_Expression;
            begin
               return Syntax.Expressions.Let
                 (Loc, Bs, Expr);
            end;
         end;
      else
         Error ("expected to be at an expression");
         raise Parse_Error;
      end if;
   end Parse_Left_Expression;

end Leander.Parser.Expressions;
