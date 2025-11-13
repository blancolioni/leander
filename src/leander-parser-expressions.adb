with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;
with Ada.Text_IO;

with Leander.Names;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Bindings;
with Leander.Parser.Sequences;
with Leander.Source;
with Leander.Syntax.Bindings;
with Leander.Syntax.Expressions;

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
     (Context : Parse_Context'Class)
      return Leander.Syntax.Expressions.Reference;

   function Parse_Left_Expression
     (Context : Parse_Context'Class)
      return Leander.Syntax.Expressions.Reference;

   type Statement_Type is
     (Expression_Statement,
      Binding_Statement,
      Let_Statement);

   type Statement_Record (Class : Statement_Type) is
      record
         Location : Leander.Source.Source_Location;
         case Class is
            when Expression_Statement =>
               Expression : Syntax.Expressions.Reference;
            when Binding_Statement =>
               Pattern    : Syntax.Patterns.Reference;
               Value      : Syntax.Expressions.Reference;
            when Let_Statement =>
               Bindings   : Syntax.Bindings.Reference;
         end case;
      end record;

   function Parse_Statement
     (Context : Parse_Context'Class)
     return Statement_Record;

   package Statement_Sequence_Parser is
     new Leander.Parser.Sequences
       (Element_Name => "statement",
        Element_Type => Statement_Record,
        At_Element   => At_Expression,
        Parse      => Parse_Statement);

   type Case_Alt_Record is
      record
         Pat : Leander.Syntax.Patterns.Reference;
         Exp : Leander.Syntax.Expressions.Reference;
      end record;

   function Parse_Case_Alt (Context : Parse_Context'Class)
                            return Case_Alt_Record;

   package Case_Alt_Parser is
     new Leander.Parser.Sequences
       (Element_Name => "alt",
        Element_Type => Case_Alt_Record,
        At_Element   => At_Atomic_Expression,
        Parse        => Parse_Case_Alt);

   ----------------
   -- Add_Fixity --
   ----------------

   procedure Add_Fixity
     (Operator      : String;
      Associativity : Associativity_Type;
      Priority      : Priority_Range)
   is
   begin
      if Fixities.Contains (Operator) then
         Warning ("redefinition of operator " & Operator);
         Fixities.Delete (Operator);
      end if;
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
        or else Tok <= [Tok_Lambda, Tok_Let, Tok_Do, Tok_Case, Tok_If];
   end At_Expression;

   ----------------
   -- At_Pattern --
   ----------------

   function At_Pattern return Boolean is
   begin
      return At_Atomic_Expression;
   end At_Pattern;

   -----------------------------
   -- Parse_Atomic_Expression --
   -----------------------------

   function Parse_Atomic_Expression
     (Context : Parse_Context'Class)
      return Leander.Syntax.Expressions.Reference
   is
      use Leander.Syntax.Expressions;
      Loc : constant Source.Source_Location := Current_Source_Location;

      function Parse_Rest_Of_List
        return Leander.Syntax.Expressions.Reference
        with Pre => At_Expression;

      function Parse_Rest_Of_Tuple
        (First : Leander.Syntax.Expressions.Reference)
         return Leander.Syntax.Expressions.Reference;

      ------------------------
      -- Parse_Rest_Of_List --
      ------------------------

      function Parse_Rest_Of_List
        return Leander.Syntax.Expressions.Reference
      is
         Loc : constant Source.Source_Location := Current_Source_Location;
         X   : constant Reference := Parse_Expression (Context);
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

      -------------------------
      -- Parse_Rest_Of_Tuple --
      -------------------------

      function Parse_Rest_Of_Tuple
        (First : Leander.Syntax.Expressions.Reference)
         return Leander.Syntax.Expressions.Reference
      is

         function Go return Leander.Syntax.Expressions.Reference_Array;

         --------
         -- Go --
         --------

         function Go return Leander.Syntax.Expressions.Reference_Array is
         begin
            if Tok = Tok_Comma then
               Scan;
               declare
                  T : constant Reference := Parse_Expression (Context);
               begin
                  return T & Go;
               end;
            else
               if Tok = Tok_Right_Paren then
                  Scan;
               else
                  Error ("missing ')'");
               end if;
               return [];
            end if;
         end Go;

         Ts : constant Reference_Array := Go;
         Count : constant Positive := Ts'Length;
         Commas : constant String (1 .. Count) := [others => ','];
         Con    : constant String := '(' & Commas & ')';
         T      : Reference :=
                    Application
                      (First.Location, Constructor (First.Location, Con), First);
      begin
         for Item of Ts loop
            T := Application (Item.Location, T, Item);
         end loop;
         return T;
      end Parse_Rest_Of_Tuple;

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
      elsif Tok = Tok_Character_Literal then
         return Lit : constant Reference :=
           Character_Literal (Loc, Character'Pos (Tok_Character_Value))
         do
            Scan;
         end return;
      elsif Tok = Tok_String_Literal then
         return Lit : constant Reference :=
           String_Literal (Loc, Tok_Text)
         do
            Scan;
         end return;
      elsif Tok = Tok_Left_Paren then
         Scan;
         if Tok = Tok_Right_Paren then
            Scan;
            return Constructor (Loc, "()");
         end if;

         declare
            First : constant Reference := Parse_Expression (Context);
         begin
            if Tok = Tok_Right_Paren then
               Scan;
               return First;
            elsif Tok = Tok_Comma then
               return Parse_Rest_Of_Tuple (First);
            else
               Error ("expected ')'");
               return First;
            end if;
         end;
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

   --------------------
   -- Parse_Case_Alt --
   --------------------

   function Parse_Case_Alt
     (Context : Parse_Context'Class)
      return Case_Alt_Record
   is
      Pat : constant Leander.Syntax.Patterns.Reference :=
              Parse_Expression (Context).To_Pattern;
   begin
      Expect (Tok_Right_Arrow, [Tok_Identifier]);

      declare
         Expr : constant Leander.Syntax.Expressions.Reference :=
                  Parse_Expression (Context);
      begin
         return (Pat, Expr);
      end;
   end Parse_Case_Alt;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Context : Parse_Context'Class)
     return Leander.Syntax.Expressions.Reference
   is
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
                  and then Op_Fixity.Priority < Top_Fixity.Priority)
                 or else (Op_Fixity.Associativity /= Left
                          and then Op_Fixity.Priority >= Top_Fixity.Priority);

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

      Push_Value (Parse_Left_Expression (Context));

      while At_Operator loop
         declare
            Name     : constant String := Scan_Identifier;
         begin
            Push_Operator (Name);
            Push_Value (Parse_Left_Expression (Context));
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
     (Context : Parse_Context'Class)
     return Leander.Syntax.Expressions.Reference
   is
   begin
      if At_Atomic_Expression then
         declare
            Indent  : constant Positive := Tok_Indent;
            Expr    : Leander.Syntax.Expressions.Reference :=
                        Parse_Atomic_Expression (Context);
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
                       Parse_Atomic_Expression (Context));
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
                        Parse_Expression (Context);
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
               while Bindings.At_Binding loop
                  Bindings.Parse_Binding (Context, Bs);
               end loop;
            end if;
            if Tok /= Tok_In then
               Error ("expected 'in' or binding at " & Tok'Image);
            else
               Scan;
            end if;

            declare
               Expr : constant Syntax.Expressions.Reference :=
                        Parse_Expression (Context);
            begin
               return Syntax.Expressions.Let
                 (Loc, Bs, Expr);
            end;
         end;
      elsif Tok = Tok_Case then
         Scan;
         declare
            Loc  : constant Source.Source_Location := Current_Source_Location;
            F_Id : constant Leander.Names.Leander_Name :=
                     Leander.Names.New_Name;
            E    : constant Syntax.Expressions.Reference :=
                     Parse_Expression (Context);
            Bs   : constant Leander.Syntax.Bindings.Reference :=
                     Leander.Syntax.Bindings.Empty;

            procedure On_Alt
              (Alt : Case_Alt_Record);

            ------------
            -- On_Alt --
            ------------

            procedure On_Alt
              (Alt : Case_Alt_Record)
            is
            begin
               Bs.Add_Binding
                 (Alt.Pat.Location,
                  Leander.Names.To_String (F_Id),
                  [Alt.Pat],
                  Alt.Exp);
            end On_Alt;
         begin
            if Tok = Tok_Of then
               Scan;
            else
               Error ("missing 'of'");
            end if;

            Case_Alt_Parser.Parse_Sequence (Context, On_Alt'Access);

            return Syntax.Expressions.Let
              (Loc, Bs,
               Leander.Syntax.Expressions.Application
                 (E.Location,
                  Leander.Syntax.Expressions.Variable
                    (E.Location,
                     Leander.Names.To_String (F_Id)),
                  E));
         end;
      elsif Tok = Tok_If then
         declare
            Loc : constant Source.Source_Location := Current_Source_Location;
            Cond, T, F : Syntax.Expressions.Reference;
            Fn         : constant String :=
                           Leander.Names.To_String (Leander.Names.New_Name);
            Bs         : constant Leander.Syntax.Bindings.Reference :=
                           Leander.Syntax.Bindings.Empty;
         begin
            Scan;
            Cond := Parse_Expression (Context);
            if Tok = Tok_Then then
               Scan;
               T := Parse_Expression (Context);
               if Tok = Tok_Else then
                  Scan;
                  F := Parse_Expression (Context);
               else
                  Error ("missing 'else'");
                  F := Syntax.Expressions.Constructor (Loc, "False");
               end if;
            else
               Error ("missing 'then'");
               T := Syntax.Expressions.Constructor (Loc, "True");
               F := Syntax.Expressions.Constructor (Loc, "False");
            end if;

            Bs.Add_Binding
              (Loc, Fn,
               [Syntax.Patterns.Constructor (Loc, "True", [])],
               T);
            Bs.Add_Binding
              (Loc, Fn,
               [Syntax.Patterns.Constructor (Loc, "False", [])],
               F);
            return Syntax.Expressions.Let
              (Loc, Bs,
               Leander.Syntax.Expressions.Application
                 (Loc,
                  Leander.Syntax.Expressions.Variable
                    (Loc, Fn),
                  Cond));
         end;

      elsif Tok = Tok_Do then
         Scan;
         declare

            package Statement_Lists is
              new Ada.Containers.Indefinite_Doubly_Linked_Lists
                (Statement_Record);

            Stmts : Statement_Lists.List;

            procedure On_Statement (Item : Statement_Record);

            function To_Expression
              (Position : Statement_Lists.Cursor)
               return Syntax.Expressions.Reference;

            ------------------
            -- On_Statement --
            ------------------

            procedure On_Statement (Item : Statement_Record) is
            begin
               Stmts.Append (Item);
            end On_Statement;

            -------------------
            -- To_Expression --
            -------------------

            function To_Expression
              (Position : Statement_Lists.Cursor)
               return Syntax.Expressions.Reference
            is
               use Leander.Syntax.Expressions;
               use Statement_Lists;
               Stmt     : constant Statement_Record :=
                            Element (Position);
               Next_Pos : constant Statement_Lists.Cursor :=
                            Next (Position);
            begin
               case Stmt.Class is
                  when Expression_Statement =>
                     if Has_Element (Next_Pos) then
                        return Application
                          (Stmt.Location,
                           Application
                             (Stmt.Location,
                              Variable
                                (Stmt.Location,
                                 ">>"),
                              Stmt.Expression),
                           To_Expression (Next_Pos));
                     else
                        return Stmt.Expression;
                     end if;
                  when Binding_Statement =>
                     if not Has_Element (Next_Pos) then
                        Ada.Text_IO.Put_Line
                          (Leander.Source.Show (Stmt.Location)
                           & ": binding cannot be the last expression");
                        return Stmt.Value;
                     else
                        declare
                           Rest : constant Syntax.Expressions.Reference :=
                                    To_Expression (Next_Pos);
                           Ok   : constant String := "$ok";
                           Bs   : constant Syntax.Bindings.Reference :=
                                    Syntax.Bindings.Empty;
                        begin
                           Bs.Add_Binding
                             (Stmt.Location, Ok, [Stmt.Pattern], Rest);

                           declare
                              E_Let : constant Syntax.Expressions.Reference :=
                                        Syntax.Expressions.Let
                                          (Stmt.Location,
                                           Bs,
                                           Application
                                             (Stmt.Location,
                                              Application
                                                (Stmt.Location,
                                                 Variable
                                                   (Stmt.Location,
                                                    ">>="),
                                                 Stmt.Value),
                                              Variable (Stmt.Location, Ok)));
                           begin
                              return E_Let;
                           end;
                        end;
                     end if;
                  when Let_Statement =>
                     return Syntax.Expressions.Let
                       (Stmt.Location,
                        Stmt.Bindings,
                        To_Expression (Next_Pos));
               end case;
            end To_Expression;

         begin
            Statement_Sequence_Parser.Parse_Sequence
              (Context, On_Statement'Access);
            return To_Expression (Stmts.First);
         end;
      else
         Error ("expected to be at an expression near " & Tok'Image);
         raise Parse_Error;
      end if;
   end Parse_Left_Expression;

   --------------------
   -- Parse_Patterns --
   --------------------

   function Parse_Patterns
     (Context : Parse_Context'Class)
      return Leander.Syntax.Patterns.Reference_Array
   is
      use type Leander.Syntax.Expressions.Reference;
      use type Leander.Syntax.Patterns.Reference_Array;
   begin
      if not At_Atomic_Expression then
         return [];
      end if;

      declare
         Expr : constant Syntax.Expressions.Reference :=
                  Parse_Atomic_Expression (Context);
         Pat  : constant Syntax.Patterns.Reference :=
                  (if Expr = null then null else Expr.To_Pattern);
      begin
         if Expr = null then
            return [];
         end if;
         if At_Atomic_Expression then
            return Pat & Parse_Patterns (Context);
         else
            return [Pat];
         end if;
      end;
   end Parse_Patterns;

   ---------------------
   -- Parse_Statement --
   ---------------------

   function Parse_Statement
     (Context : Parse_Context'Class)
      return Statement_Record
   is
      Loc : constant Source.Source_Location := Current_Source_Location;
   begin
      if Tok = Tok_Let then
         declare
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
               while Bindings.At_Binding loop
                  Bindings.Parse_Binding (Context, Bs);
               end loop;
            end if;

            return Statement_Record'
              (Class    => Let_Statement,
               Location => Loc,
               Bindings => Bs);
         end;
      else
         declare
            E : Syntax.Expressions.Reference :=
                  Parse_Expression (Context);
         begin
            if Tok = Tok_Left_Arrow then
               declare
                  Pat : constant Syntax.Patterns.Reference :=
                        E.To_Pattern;
               begin
                  Scan;
                  E := Parse_Expression (Context);
                  return Statement_Record'
                    (Class    => Binding_Statement,
                     Location => Loc,
                     Pattern  => Pat,
                     Value    => E);
               end;
            else
               return Statement_Record'
                 (Class      => Expression_Statement,
                  Location   => Loc,
                  Expression => E);
            end if;
         end;
      end if;
   end Parse_Statement;

end Leander.Parser.Expressions;
