with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

package body Leander.Parser.Sequences is

   --------------------
   -- Parse_Sequence --
   --------------------

   procedure Parse_Sequence
     (On_Parsed : not null access
        procedure (Element : Element_Type))
   is
   begin
      if Tok = Tok_Left_Brace then
         Scan;
         while At_Element loop
            declare
               E : constant Element_Type := Parse;
            begin
               On_Parsed (E);
               if Tok = Tok_Semi then
                  Scan;
                  if not At_Element then
                     Error ("expected " & Element_Name);
                  end if;
               elsif At_Element then
                  Error ("missing ';'");
               end if;
            end;
         end loop;
         if Tok = Tok_Right_Brace then
            Scan;
         else
            Error ("missing '}'");
         end if;
      elsif At_Element then
         declare
            Indent : constant Positive := Tok_Indent;
         begin
            while Tok_Indent >= Indent
              and then At_Element
            loop
               declare
                  E : constant Element_Type := Parse;
               begin
                  On_Parsed (E);
               end;
            end loop;
         end;
      else
         Error ("expected " & Element_Name);
      end if;
   end Parse_Sequence;

end Leander.Parser.Sequences;
