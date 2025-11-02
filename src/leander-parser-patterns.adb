with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

package body Leander.Parser.Patterns is

   function At_Pattern return Boolean
   is (Tok <= [Tok_Identifier, Tok_Left_Paren, Tok_Integer_Literal,
       Tok_Character_Literal, Tok_String_Literal, Tok_Float_Literal,
       Tok_Left_Bracket]);

   -------------------
   -- Parse_Pattern --
   -------------------

   function Parse_Pattern return Leander.Syntax.Patterns.Reference is
      use Leander.Syntax.Patterns;
      Loc : constant Leander.Source.Source_Location := Current_Source_Location;
   begin
      if Tok = Tok_Identifier then
         if Tok_Text = "_" then
            return Pat : constant Reference := Wildcard (Loc) do
               Scan;
            end return;
         else
            return Pat : constant Reference := Variable (Loc, Tok_Text) do
               Scan;
            end return;
         end if;
      elsif Tok = Tok_Left_Bracket and then Next_Tok = Tok_Right_Bracket then
         return Pat : constant Reference :=
           Leander.Syntax.Patterns.Constructor (Loc, "[]", [])
         do
            Scan;
            Scan;
         end return;
      elsif Tok = Tok_Integer_Literal then
         return Pat : constant Reference :=
           Leander.Syntax.Patterns.Integer_Literal (Loc, Tok_Text)
         do
            Scan;
         end return;
      elsif Tok = Tok_Left_Paren then
         if Next_Tok = Tok_Right_Paren then
            return Pat : constant Reference :=
              Leander.Syntax.Patterns.Constructor (Loc, "()", [])
            do
               Scan;
               Scan;
            end return;
         else
            Scan;
            if not At_Constructor then
               Error ("expected a constructor, found " & Tok_Text);
               Skip_To ([], [Tok_Equal, Tok_End_Of_File]);
               return Leander.Syntax.Patterns.Wildcard (Loc);
            end if;

            declare
               Con : constant String := Scan_Identifier;
               Pats : Leander.Syntax.Patterns.Reference_Array (1 .. 20);
               Last : Natural := 0;
            begin
               while Patterns.At_Pattern loop
                  Last := Last + 1;
                  Pats (Last) := Patterns.Parse_Pattern;
               end loop;
               if Tok /= Tok_Right_Paren then
                  Error ("missing ')'");
               else
                  Scan;
               end if;
               return Leander.Syntax.Patterns.Constructor
                 (Loc, Con, Pats (1 .. Last));
            end;
         end if;

      else
         Error ("expected a pattern at " & Tok'Image);
         Skip_To ([], [Tok_Equal, Tok_End_Of_File]);
         return Wildcard (Loc);
      end if;
   end Parse_Pattern;

end Leander.Parser.Patterns;
