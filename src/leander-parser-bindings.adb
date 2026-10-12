with Ada.Strings.Unbounded;
with Leander.Core;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Expressions;
with Leander.Parser.Types;
with Leander.Source;

with Leander.Syntax.Expressions;
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
      elsif Tok = Tok_Equal or else Tok = Tok_Vertical_Bar then
         declare
            RHS  : constant Leander.Parser.Expressions.Guarded_RHS :=
                     Leander.Parser.Expressions.Parse_Guarded_RHS
                       (Context, Tok_Equal);
            Expr : Leander.Syntax.Expressions.Reference := RHS.Expr;
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
            --  The 'where' group wraps the whole guard chain, not each
            --  arm: its bindings are in scope in the guards as well as in
            --  the bodies.
            To.Add_Binding
              (Loc, LHS, Expr,
               Ada.Strings.Unbounded.To_String (RHS.Fallthrough));
         end;
      else
         Error ("expected '::', '=' or '|' at " & Tok'Image);
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
      function Parse_Operand
        (First : Syntax.Patterns.Reference;
         Loc   : Leander.Source.Source_Location)
         return Syntax.Patterns.Reference;
      --  An operand of an infix definition is a whole pattern, not an
      --  atomic one: a bare constructor takes the atomic patterns that
      --  follow it as its arguments, as in "Just x <|> _" (Haskell 2010,
      --  section 4.4.3; #132). First is the atomic pattern already read.

      -------------------
      -- Parse_Operand --
      -------------------

      function Parse_Operand
        (First : Syntax.Patterns.Reference;
         Loc   : Leander.Source.Source_Location)
         return Syntax.Patterns.Reference
      is
      begin
         if First.Is_Constructor and then First.Constructor_Args'Length = 0
         then
            declare
               Args : constant Leander.Syntax.Patterns.Reference_Array :=
                        Expressions.Parse_Patterns (Context);
            begin
               if Args'Length > 0 then
                  return Syntax.Patterns.Constructor
                    (Loc, First.Constructor_Name, Args);
               end if;
            end;
         end if;
         return First;
      end Parse_Operand;

      function Infix_LHS
        (Pat : Syntax.Patterns.Reference;
         Loc : Leander.Source.Source_Location)
         return Leander.Syntax.Bindings.Binding_LHS;
      --  "pat varop pat", with Pat the start of the left operand.

      ---------------
      -- Infix_LHS --
      ---------------

      function Infix_LHS
        (Pat : Syntax.Patterns.Reference;
         Loc : Leander.Source.Source_Location)
         return Leander.Syntax.Bindings.Binding_LHS
      is
         Left : constant Syntax.Patterns.Reference := Parse_Operand (Pat, Loc);
      begin
         if not At_Operator then
            Error ("syntax error");
            raise Parse_Error;
         end if;

         declare
            Name      : constant String := Scan_Identifier;
            Right_Loc : constant Leander.Source.Source_Location :=
                          Current_Source_Location;
            Right     : constant Syntax.Patterns.Reference :=
                          Parse_Operand
                            (Parser.Expressions.Parse_Atomic_Pattern
                               (Context),
                             Right_Loc);
         begin
            return Syntax.Bindings.Create_Binding_LHS (Name, [Left, Right]);
         end;
      end Infix_LHS;

      Loc   : constant Leander.Source.Source_Location :=
                Current_Source_Location;
      Paren : constant Boolean := Tok = Tok_Left_Paren;
      First : constant Syntax.Expressions.Reference :=
                Parser.Expressions.Parse_Atomic_Expression (Context);
      Head  : constant String :=
                (if Paren then Syntax.Expressions.Head_Variable (First)
                 else "");
      Args  : constant Syntax.Expressions.Reference_Array :=
                Syntax.Expressions.Arguments (First);
   begin
      if Head /= "" and then Args'Length > 0 then
         --  "( funlhs ) apat {apat}": a definition written applied
         --  inside parentheses, as in "(f <+> g) x" or "(f x) y". Its
         --  head is a variable, where a parenthesised pattern's is a
         --  constructor (#132).
         declare
            use type Leander.Syntax.Patterns.Reference_Array;
            Inner : constant Leander.Syntax.Patterns.Reference_Array :=
                      [for A of Args => A.To_Pattern];
         begin
            return Syntax.Bindings.Create_Binding_LHS
              (Head, Inner & Expressions.Parse_Patterns (Context));
         end;
      end if;

      declare
         Pat : constant Syntax.Patterns.Reference := First.To_Pattern;
      begin
         if At_Operator or else Pat.Is_Constructor then
            return Infix_LHS (Pat, Loc);
         elsif not Pat.Is_Variable then
            Error ("syntax error");
            raise Parse_Error;
         else
            return Syntax.Bindings.Create_Binding_LHS
              (Pat.Variable_Name, Expressions.Parse_Patterns (Context));
         end if;
      end;
   end Parse_LHS;

end Leander.Parser.Bindings;
