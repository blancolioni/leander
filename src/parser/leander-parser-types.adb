with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Kinds.Trees;

with Leander.Core;

with Leander.Primitives;

package body Leander.Parser.Types is

   function At_Atomic_Type return Boolean
   is (At_Name or else Tok = Tok_Left_Bracket or else Tok = Tok_Left_Paren);

   -----------------------
   -- Parse_Atomic_Type --
   -----------------------

   function Parse_Atomic_Type
     (Env : Leander.Environments.Environment)
      return Leander.Types.Trees.Tree_Type
   is
      Expr   : Leander.Types.Trees.Tree_Type;
   begin
      if At_Variable then
         declare
            Name : constant String := Tok_Text;
         begin
            Scan;
            if Env.Has_Type_Variable_Binding (Name) then
               Expr := Env.Type_Variable_Binding (Name);
            else
               Expr :=
                 Leander.Types.Trees.Leaf
                   (Leander.Types.Variable (Name));
            end if;
         end;
      elsif At_Constructor then
         Expr :=
           Leander.Types.Trees.Leaf
             (Leander.Types.Constructor (Tok_Text));
         Scan;
      elsif Tok = Tok_Left_Paren and then Next_Tok = Tok_Right_Paren then
         Scan;
         Scan;
         Expr := Leander.Types.Trees.Leaf (Leander.Types.Constructor ("()"));
      elsif Tok = Tok_Left_Paren then
         Scan;
         Expr := Parse_Type (Env);
         if Tok = Tok_Comma then
            declare
               Args : array (1 .. 10) of Leander.Types.Trees.Tree_Type;
               Count : Natural := 0;
            begin
               loop
                  Count := Count + 1;
                  Args (Count) := Expr;
                  if Tok = Tok_Comma then
                     Scan;
                     Expr := Parse_Type (Env);
                  else
                     exit;
                  end if;
               end loop;
               Expr := Leander.Primitives.Tuple_Type (Count);
               for I in 1 .. Count loop
                  Expr := Expr.Apply (Args (I));
               end loop;
            end;
         end if;
         if Tok = Tok_Right_Paren then
            Scan;
         else
            Error ("missing ')'");
         end if;
      elsif Tok = Tok_Left_Bracket then
         Scan;
         if Tok = Tok_Right_Bracket then
            Scan;
            Expr :=
              Leander.Types.Trees.Leaf
                (Leander.Types.Constructor ("[]"));
         else
            Expr := Parse_Type (Env);
            if Tok = Tok_Right_Bracket then
               Scan;
            else
               Error ("missing ']'");
            end if;
            Expr :=
              Leander.Types.Trees.Apply
                (Leander.Types.Constructor ("[]"),
                 Expr);
         end if;
      else
         raise Program_Error with
           "expected an atomic type";
      end if;
      return Expr;
   end Parse_Atomic_Type;

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type
     (Env : Leander.Environments.Environment)
      return Leander.Types.Trees.Tree_Type
   is
      Result : Leander.Types.Trees.Tree_Type :=
                 Leander.Types.Trees.Empty;
      Expr   : Leander.Types.Trees.Tree_Type;
      Indent : constant Positive := Tok_Indent;
   begin

      while At_Atomic_Type loop

         Expr := Parse_Atomic_Type (Env);

         if Result.Is_Empty then
            Result := Expr;
         else
            Result := Result.Apply (Expr);
         end if;

         exit when Tok_Indent <= Indent;
      end loop;

      if Tok = Tok_Right_Arrow then
         Scan;
         declare
            Target : constant Leander.Types.Trees.Tree_Type :=
                       Parse_Type (Env);
         begin
            return Leander.Core.Map_Operator.Apply (Result)
              .Apply (Target);
         end;
      else
         return Result;
      end if;

   end Parse_Type;

   ----------------------------
   -- Parse_Type_Constructor --
   ----------------------------

   function Parse_Type_Constructor
     (Env    : Leander.Environments.Environment;
      Target : Leander.Types.Trees.Tree_Type)
      return Leander.Types.Trees.Tree_Type
   is
      Vars  : array (1 .. 10) of Leander.Types.Trees.Tree_Type;
      Count : Natural := 0;
      Indent : constant Positive := Tok_Indent;
   begin
      pragma Assert (At_Constructor);
      Scan;
      while Tok_Indent > Indent and then At_Atomic_Type loop
         Count := Count + 1;
         Vars (Count) := Parse_Atomic_Type (Env);
      end loop;
      declare
         Result : Leander.Types.Trees.Tree_Type := Target;
      begin
         for I in reverse 1 .. Count loop
            Result :=
              Leander.Core.Map_Operator.Apply
                (Vars (I)).Apply (Result);
         end loop;
         return Result;
      end;
   end Parse_Type_Constructor;

end Leander.Parser.Types;
