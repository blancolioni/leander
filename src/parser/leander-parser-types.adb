with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Kinds.Trees;

with Leander.Core;

package body Leander.Parser.Types is

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type return Leander.Types.Trees.Tree_Type is
      Result : Leander.Types.Trees.Tree_Type;
   begin
      if At_Variable then
         declare
            Name : constant String := Tok_Text;
         begin
            Scan;

            Result :=
              Leander.Types.Trees.Leaf
                (Leander.Types.Variable
                   (Name,
                    Leander.Kinds.Trees.Leaf
                      (Leander.Kinds.Primitive)));
         end;
      elsif At_Constructor then
         declare
            use Leander.Types.Trees;
            Indent  : constant Positive := Tok_Indent;
            Kind    : Leander.Kinds.Trees.Tree_Type :=
                        Leander.Kinds.Trees.Leaf
                          (Leander.Kinds.Primitive);
            Con     : constant String := Tok_Text;
            Vars    : array (1 .. 10) of Tree_Type;
            Count   : Natural := 0;
         begin
            Scan;
            while At_Variable and then Tok_Indent > Indent loop
               Count := Count + 1;
               Vars (Count) := Parse_Type;
               Kind :=
                 Leander.Kinds.Trees.Apply
                   (Leander.Kinds.Trees.Apply
                      (Leander.Kinds.Map,
                       Leander.Kinds.Primitive),
                    Kind);
            end loop;

            Result := Leaf (Leander.Types.Constructor (Con, Kind));
            for I in 1 .. Count loop
               Result := Apply (Result, Vars (I));
            end loop;
            Result.Set_Annotation
              (Leander.Kinds.Trees.Leaf
                 (Leander.Kinds.Primitive));
         end;
      elsif Tok = Tok_Left_Paren then
         Scan;
         Result := Parse_Type;
         if Tok = Tok_Right_Paren then
            Scan;
         else
            Error ("missing ')'");
         end if;
      else
         raise Program_Error with
           "expected a variable or constructor";
      end if;

      if Tok = Tok_Right_Arrow then
         Scan;
         declare
            Target : constant Leander.Types.Trees.Tree_Type :=
                       Parse_Type;
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
     (Target : Leander.Types.Trees.Tree_Type)
      return Leander.Types.Trees.Tree_Type
   is
      Vars  : array (1 .. 10) of Leander.Types.Trees.Tree_Type;
      Count : Natural := 0;
      Indent : constant Positive := Tok_Indent;
   begin
      pragma Assert (At_Constructor);
      Scan;
      while Tok_Indent > Indent and then At_Variable loop
         Count := Count + 1;
         Vars (Count) := Parse_Type;
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
