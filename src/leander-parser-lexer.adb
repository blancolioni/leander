with Ada.Characters.Latin_1;

with Leander.Parser.Escapes;

package body Leander.Parser.Lexer is

   use Ada.Strings.Unbounded;
   use Leander.Parser.Tokens;

   package Latin_1 renames Ada.Characters.Latin_1;

   Tab_Size : constant := 8;

   subtype Keyword_Token is Token range Tok_Else .. Tok_Colon_Colon;

   function Keyword_Text (Kind : Keyword_Token) return String
   is (case Kind is
          when Tok_Else               => "else",
          when Tok_If                 => "if",
          when Tok_Then               => "then",
          when Tok_Case               => "case",
          when Tok_Of                 => "of",
          when Tok_Do                 => "do",
          when Tok_Type               => "type",
          when Tok_Class              => "class",
          when Tok_Where              => "where",
          when Tok_Instance           => "instance",
          when Tok_Data               => "data",
          when Tok_Deriving           => "deriving",
          when Tok_Import             => "import",
          when Tok_In                 => "in",
          when Tok_Infix              => "infix",
          when Tok_Infixl             => "infixl",
          when Tok_Infixr             => "infixr",
          when Tok_Let                => "let",
          when Tok_Module             => "module",
          when Tok_Newtype            => "newtype",
          when Tok_Foreign            => "foreign",
          when Tok_Equal              => "=",
          when Tok_Lambda             => "\",
          when Tok_Dot_Dot            => "..",
          when Tok_Vertical_Bar       => "|",
          when Tok_Right_Arrow        => "->",
          when Tok_Left_Arrow         => "<-",
          when Tok_Double_Right_Arrow => "=>",
          when Tok_Colon_Colon        => "::");

   function Is_Digit (Ch : Character) return Boolean
   is (Ch in '0' .. '9');

   function Is_Letter (Ch : Character) return Boolean
   is (Ch in 'a' .. 'z' | 'A' .. 'Z');

   function Is_Symbol (Ch : Character) return Boolean
   is (Ch in ':' | '!' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<'
            | '=' | '>' | '?' | '@' | '\' | '^' | '|' | '-' | '~');
   --  The characters an operator is made of. '#' is Haskell's too, but
   --  here it belongs with the letters, because the Prelude names its
   --  primitives #primIntAdd and so on.

   function Is_Name_Start (Ch : Character) return Boolean
   is (Is_Letter (Ch) or else Ch in '_' | '#');

   function Is_Name_Body (Ch : Character) return Boolean
   is (Is_Name_Start (Ch) or else Is_Digit (Ch) or else Ch = ''');

   function Is_White (Ch : Character) return Boolean
   is (Ch in ' ' | Latin_1.HT | Latin_1.LF | Latin_1.VT | Latin_1.FF
            | Latin_1.CR);

   function Keyword_Kind (Text : String) return Token;
   --  The reserved word or operator Text is, or else Tok_Identifier.

   function Digit_Value (Ch : Character) return Natural
   is (case Ch is
          when '0' .. '9' => Character'Pos (Ch) - Character'Pos ('0'),
          when 'a' .. 'f' => Character'Pos (Ch) - Character'Pos ('a') + 10,
          when 'A' .. 'F' => Character'Pos (Ch) - Character'Pos ('A') + 10,
          when others     => Natural'Last);

   ------------------
   -- Keyword_Kind --
   ------------------

   function Keyword_Kind (Text : String) return Token is
   begin
      for Kind in Keyword_Token loop
         if Keyword_Text (Kind) = Text then
            return Kind;
         end if;
      end loop;
      return Tok_Identifier;
   end Keyword_Kind;

   --------------
   -- Tokenize --
   --------------

   procedure Tokenize
     (Source      : String;
      Tokens      : out Token_Vectors.Vector;
      Diagnostics : out Diagnostic_Vectors.Vector)
   is
      Pos        : Positive := Source'First;
      Line       : Positive := 1;
      Line_Start : Positive := Source'First;

      --  Where the token being scanned began.
      Start        : Positive := Source'First;
      Start_Line   : Positive := 1;
      Start_Column : Positive := 1;
      Start_Indent : Positive := 1;

      function At_End return Boolean
      is (Pos > Source'Last);

      function Peek (Offset : Natural := 0) return Character
      is (if Pos + Offset <= Source'Last
          then Source (Pos + Offset)
          else Latin_1.NUL);
      --  NUL stands for "past the end"; it is not anything a token can
      --  start or continue with, so every test against it fails.

      function Column return Positive
      is (Pos - Line_Start + 1);

      procedure Add (Kind : Token; Text : String);
      --  A token running from Start to just before Pos.

      procedure Advance;
      procedure Block_Comment;
      procedure Character_Literal;
      procedure Identifier;
      function Is_Line_Comment return Boolean;
      procedure Mark_Start;
      procedure Number;
      procedure Report (Message : String);
      --  At the start of the current token.

      procedure Skip_Escape (In_String : Boolean);
      --  Pos is at a backslash in a literal. Step over the whole escape,
      --  as far as is needed to know where the literal ends; decoding is
      --  Escapes' business.

      procedure Skip_White_And_Comments;
      procedure String_Literal;
      function Visual_Column return Positive;

      ---------
      -- Add --
      ---------

      procedure Add (Kind : Token; Text : String) is
      begin
         Tokens.Append
           (Token_Info'
              (Kind   => Kind,
               Text   => To_Unbounded_String (Text),
               Line   => Start_Line,
               Column => Start_Column,
               Indent => Start_Indent));
      end Add;

      -------------
      -- Advance --
      -------------

      procedure Advance is
      begin
         if Source (Pos) = Latin_1.LF then
            Line := Line + 1;
            Line_Start := Pos + 1;
         end if;
         Pos := Pos + 1;
      end Advance;

      -------------------
      -- Block_Comment --
      -------------------

      procedure Block_Comment is
         Depth : Natural := 0;
      begin
         --  Report 2.3: block comments nest, and a pragma ({-# ... #-}) is
         --  one too, as far as anything here is concerned.
         Mark_Start;
         loop
            if At_End then
               Report ("unterminated block comment");
               exit;
            elsif Peek = '{' and then Peek (1) = '-' then
               Depth := Depth + 1;
               Advance;
               Advance;
            elsif Peek = '-' and then Peek (1) = '}' then
               Depth := Depth - 1;
               Advance;
               Advance;
               exit when Depth = 0;
            else
               Advance;
            end if;
         end loop;
      end Block_Comment;

      -----------------------
      -- Character_Literal --
      -----------------------

      procedure Character_Literal is
         Raw_Start : Positive;
         Closed    : Boolean := False;
      begin
         Advance;
         Raw_Start := Pos;

         while not At_End and then Peek /= Latin_1.LF loop
            if Peek = ''' then
               Closed := True;
               exit;
            elsif Peek = '\' then
               Skip_Escape (In_String => False);
            else
               Advance;
            end if;
         end loop;

         declare
            Raw     : constant String := Source (Raw_Start .. Pos - 1);
            Value   : Character := ' ';
            Message : Unbounded_String;
         begin
            if Closed then
               Advance;
               Leander.Parser.Escapes.Decode_Character (Raw, Value, Message);
               if Message /= Null_Unbounded_String then
                  Report (To_String (Message));
               end if;
            else
               Report ("unterminated character literal");
            end if;

            Add (Tok_Character_Literal, [Value]);
         end;
      end Character_Literal;

      ----------------
      -- Identifier --
      ----------------

      procedure Identifier is
      begin
         if Is_Symbol (Peek) then
            while Is_Symbol (Peek) loop
               Advance;
            end loop;
         else
            Advance;
            while Is_Name_Body (Peek) loop
               Advance;
            end loop;
         end if;

         declare
            Text : constant String := Source (Start .. Pos - 1);
         begin
            Add (Keyword_Kind (Text), Text);
         end;
      end Identifier;

      ---------------------
      -- Is_Line_Comment --
      ---------------------

      function Is_Line_Comment return Boolean is
         Next : Positive := Pos;
      begin
         --  Report 2.3: two or more dashes start a comment unless they
         --  are part of a longer operator, so "-->" is not one.
         if Peek /= '-' or else Peek (1) /= '-' then
            return False;
         end if;

         while Next <= Source'Last and then Source (Next) = '-' loop
            Next := Next + 1;
         end loop;

         return Next > Source'Last or else not Is_Symbol (Source (Next));
      end Is_Line_Comment;

      ----------------
      -- Mark_Start --
      ----------------

      procedure Mark_Start is
      begin
         Start := Pos;
         Start_Line := Line;
         Start_Column := Column;
         Start_Indent := Visual_Column;
      end Mark_Start;

      ------------
      -- Number --
      ------------

      procedure Number is

         Text     : Unbounded_String;
         Is_Float : Boolean := False;
         Mark     : Positive;

         procedure Based (Base : Positive);
         --  Pos is at the first digit of a 0x or 0o literal.

         procedure Scan_Digits;

         -----------
         -- Based --
         -----------

         procedure Based (Base : Positive) is
            Value    : Long_Long_Integer := 0;
            Too_Big  : Boolean := False;
         begin
            while Digit_Value (Peek) < Base loop
               if Value > (Long_Long_Integer'Last - 15) / 16 then
                  Too_Big := True;
               else
                  Value :=
                    Value * Long_Long_Integer (Base)
                    + Long_Long_Integer (Digit_Value (Peek));
               end if;
               Advance;
            end loop;

            if Too_Big then
               Report ("integer literal is too large");
               Value := 0;
            end if;

            declare
               Image : constant String := Value'Image;
            begin
               Add (Tok_Integer_Literal, Image (Image'First + 1 .. Image'Last));
            end;
         end Based;

         -----------------
         -- Scan_Digits --
         -----------------

         procedure Scan_Digits is
         begin
            while Is_Digit (Peek) loop
               Advance;
            end loop;
         end Scan_Digits;

      begin
         if Peek = '0'
           and then Peek (1) in 'x' | 'X'
           and then Digit_Value (Peek (2)) < 16
         then
            Advance;
            Advance;
            Based (16);
            return;
         elsif Peek = '0'
           and then Peek (1) in 'o' | 'O'
           and then Digit_Value (Peek (2)) < 8
         then
            Advance;
            Advance;
            Based (8);
            return;
         end if;

         Scan_Digits;
         Text := To_Unbounded_String (Source (Start .. Pos - 1));

         --  A dot is a decimal point only with a digit after it, so that
         --  [1..5] is 1, .., 5.
         if Peek = '.' and then Is_Digit (Peek (1)) then
            Is_Float := True;
            Advance;
            Mark := Pos;
            Scan_Digits;
            Append (Text, "." & Source (Mark .. Pos - 1));
         else
            Append (Text, ".0");
         end if;

         if Peek in 'e' | 'E'
           and then (Is_Digit (Peek (1))
                     or else (Peek (1) in '+' | '-'
                              and then Is_Digit (Peek (2))))
         then
            Is_Float := True;
            Advance;
            Mark := Pos;
            if Peek in '+' | '-' then
               Advance;
            end if;
            Scan_Digits;
            Append (Text, "E" & Source (Mark .. Pos - 1));
         end if;

         if Is_Float then
            Add (Tok_Float_Literal, To_String (Text));
         else
            Add (Tok_Integer_Literal, Source (Start .. Pos - 1));
         end if;
      end Number;

      ------------
      -- Report --
      ------------

      procedure Report (Message : String) is
      begin
         Diagnostics.Append
           (Diagnostic'
              (Line    => Start_Line,
               Column  => Start_Column,
               Message => To_Unbounded_String (Message)));
      end Report;

      -----------------
      -- Skip_Escape --
      -----------------

      procedure Skip_Escape (In_String : Boolean) is
      begin
         Advance;

         if At_End or else (Peek = Latin_1.LF and then not In_String) then
            return;
         elsif Peek = '^' then
            --  \^ takes exactly one more character, whatever it is:
            --  "\^\" is control-backslash and then the end of the string.
            Advance;
            if not At_End and then Peek /= Latin_1.LF then
               Advance;
            end if;
         elsif In_String and then Is_White (Peek) then
            --  A gap, which may run over several lines.
            while Is_White (Peek) loop
               Advance;
            end loop;
            if Peek = '\' then
               Advance;
            end if;
         else
            Advance;
         end if;
      end Skip_Escape;

      -----------------------------
      -- Skip_White_And_Comments --
      -----------------------------

      procedure Skip_White_And_Comments is
      begin
         loop
            if At_End then
               exit;
            elsif Is_White (Peek) then
               Advance;
            elsif Is_Line_Comment then
               while not At_End and then Peek /= Latin_1.LF loop
                  Advance;
               end loop;
            elsif Peek = '{' and then Peek (1) = '-' then
               Block_Comment;
            else
               exit;
            end if;
         end loop;
      end Skip_White_And_Comments;

      --------------------
      -- String_Literal --
      --------------------

      procedure String_Literal is
         Raw_Start : Positive;
         Closed    : Boolean := False;
      begin
         Advance;
         Raw_Start := Pos;

         while not At_End and then Peek /= Latin_1.LF loop
            if Peek = '"' then
               Closed := True;
               exit;
            elsif Peek = '\' then
               Skip_Escape (In_String => True);
            else
               Advance;
            end if;
         end loop;

         declare
            Raw     : constant String := Source (Raw_Start .. Pos - 1);
            Value   : Unbounded_String;
            Message : Unbounded_String;
         begin
            if Closed then
               Advance;
            else
               Report ("unterminated string literal");
            end if;

            Leander.Parser.Escapes.Decode_String (Raw, Value, Message);
            if Closed and then Message /= Null_Unbounded_String then
               Report (To_String (Message));
            end if;

            Add (Tok_String_Literal, To_String (Value));
         end;
      end String_Literal;

      -------------------
      -- Visual_Column --
      -------------------

      function Visual_Column return Positive is
         Result : Positive := 1;
      begin
         for I in Line_Start .. Pos - 1 loop
            if Source (I) = Latin_1.HT then
               Result := Result + Tab_Size - (Result - 1) mod Tab_Size;
            else
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Visual_Column;

   begin
      Tokens.Clear;
      Diagnostics.Clear;

      loop
         Skip_White_And_Comments;
         exit when At_End;

         Mark_Start;

         case Peek is
            when '0' .. '9' =>
               Number;
            when ''' =>
               Character_Literal;
            when '"' =>
               String_Literal;
            when '(' | ')' | ',' | ';' | '[' | ']' | '`' | '{' | '}' =>
               Advance;
               Add ((case Source (Start) is
                       when '('    => Tok_Left_Paren,
                       when ')'    => Tok_Right_Paren,
                       when ','    => Tok_Comma,
                       when ';'    => Tok_Semi,
                       when '['    => Tok_Left_Bracket,
                       when ']'    => Tok_Right_Bracket,
                       when '`'    => Tok_Back_Tick,
                       when '{'    => Tok_Left_Brace,
                       when others => Tok_Right_Brace),
                    Source (Start .. Start));
            when others =>
               if Is_Symbol (Peek) or else Is_Name_Start (Peek) then
                  Identifier;
               else
                  Advance;
                  Add (Tok_Bad_Character, Source (Start .. Start));
               end if;
         end case;
      end loop;

      Tokens.Append
        (Token_Info'
           (Kind   => Tok_End_Of_File,
            Text   => Null_Unbounded_String,
            Line   => Line + (if Pos > Line_Start then 1 else 0),
            Column => 0,
            Indent => 0));
   end Tokenize;

end Leander.Parser.Lexer;
