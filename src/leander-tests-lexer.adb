with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Leander.Parser.Lexer;
with Leander.Parser.Tokens;

package body Leander.Tests.Lexer is

   use Ada.Strings.Unbounded;
   use Leander.Parser.Lexer;

   LF : constant Character := Ada.Characters.Latin_1.LF;
   HT : constant Character := Ada.Characters.Latin_1.HT;

   function Image (N : Natural) return String;

   function Show_Diagnostics (Source : String) return String;
   --  Each diagnostic as "line:column: message", separated by "; ".

   function Show_Positions (Source : String) return String;
   --  Each token as "line:column:indent", separated by spaces.

   function Show_Tokens (Source : String) return String;
   --  Each token as KIND[text], without the TOK_ prefix and leaving out
   --  the final END_OF_FILE. A character outside printable ASCII is
   --  shown as \ and its three-digit code; a backslash is printable, so
   --  it is shown as itself.

   -----------
   -- Image --
   -----------

   function Image (N : Natural) return String is
   begin
      return Ada.Strings.Fixed.Trim (N'Image, Ada.Strings.Left);
   end Image;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Test ("lexer: names and reserved operators",
            "IDENTIFIER[f'] IDENTIFIER[x#y] EQUAL[=] LAMBDA[\] "
            & "IDENTIFIER[z] RIGHT_ARROW[->] IDENTIFIER[z]",
            Show_Tokens ("f' x#y = \z -> z"));
      Test ("lexer: reserved operators only as a whole run",
            "DOUBLE_RIGHT_ARROW[=>] COLON_COLON[::] LEFT_ARROW[<-] "
            & "VERTICAL_BAR[|] IDENTIFIER[==] IDENTIFIER[|>]",
            Show_Tokens ("=> :: <- | == |>"));
      Test ("lexer: keywords",
            "MODULE[module] IDENTIFIER[M] WHERE[where] "
            & "IDENTIFIER[modules]",
            Show_Tokens ("module M where modules"));
      Test ("lexer: special characters",
            "LEFT_PAREN[(] RIGHT_PAREN[)] COMMA[,] SEMI[;] "
            & "LEFT_BRACKET[[] RIGHT_BRACKET[]] BACK_TICK[`] "
            & "LEFT_BRACE[{] RIGHT_BRACE[}]",
            Show_Tokens ("(),;[]`{}"));
      Test ("lexer: a qualified name arrives in pieces",
            "IDENTIFIER[M] IDENTIFIER[.] IDENTIFIER[x] "
            & "IDENTIFIER[M] IDENTIFIER[.+]",
            Show_Tokens ("M.x M.+"));

      Test ("lexer: decimal, hex and octal",
            "INTEGER_LITERAL[42] INTEGER_LITERAL[31] INTEGER_LITERAL[16] "
            & "INTEGER_LITERAL[15] INTEGER_LITERAL[8]",
            Show_Tokens ("42 0x1F 0X10 0o17 0O10"));
      Test ("lexer: 0x with no digits is 0 then a name",
            "INTEGER_LITERAL[0] IDENTIFIER[x]",
            Show_Tokens ("0x"));
      Test ("lexer: floats",
            "FLOAT_LITERAL[1.5] FLOAT_LITERAL[2.0E3] "
            & "FLOAT_LITERAL[2.5E-1] FLOAT_LITERAL[1.0E+2]",
            Show_Tokens ("1.5 2e3 2.5E-1 1e+2"));
      Test ("lexer: a range is not a float (#95)",
            "LEFT_BRACKET[[] INTEGER_LITERAL[1] DOT_DOT[..] "
            & "INTEGER_LITERAL[5] RIGHT_BRACKET[]]",
            Show_Tokens ("[1..5]"));
      Test ("lexer: an exponent needs digits",
            "INTEGER_LITERAL[2] IDENTIFIER[e]",
            Show_Tokens ("2e"));

      Test ("lexer: line comment",
            "IDENTIFIER[a] IDENTIFIER[b]",
            Show_Tokens ("a -- comment" & LF & "b"));
      Test ("lexer: a longer run of dashes is a comment",
            "IDENTIFIER[a]",
            Show_Tokens ("a ------ x"));
      Test ("lexer: dashes then a symbol are an operator",
            "IDENTIFIER[a] IDENTIFIER[-->] IDENTIFIER[b]",
            Show_Tokens ("a --> b"));
      Test ("lexer: nested block comments",
            "IDENTIFIER[a] IDENTIFIER[b]",
            Show_Tokens ("a {- x {- y -} z -} b"));
      Test ("lexer: a pragma is skipped",
            "IDENTIFIER[f]",
            Show_Tokens ("{-# INLINE f #-} f"));

      Test ("lexer: character literals",
            "CHARACTER_LITERAL[a] CHARACTER_LITERAL[\010] "
            & "CHARACTER_LITERAL['] CHARACTER_LITERAL[\]",
            Show_Tokens ("'a' '\n' '\'' '\\'"));
      Test ("lexer: string literal escapes",
            "STRING_LITERAL[ab\001A]",
            Show_Tokens ("""a\&b\SOH\65"""));
      Test ("lexer: control-backslash before the closing quote",
            "STRING_LITERAL[\028] IDENTIFIER[x]",
            Show_Tokens ("""\^\"" x"));
      Test ("lexer: a gap across lines",
            "STRING_LITERAL[abcd] IDENTIFIER[x]",
            Show_Tokens ("""ab\" & LF & "   \cd"" x"));
      Test ("lexer: two character literals and an operator",
            "CHARACTER_LITERAL[a] IDENTIFIER[==] CHARACTER_LITERAL[b]",
            Show_Tokens ("'a' == 'b'"));

      Test ("lexer: positions",
            "1:1:1 2:2:9 2:4:11 3:0:0",
            Show_Positions ("a" & LF & HT & "b c"));
      Test ("lexer: end of file after a final newline",
            "1:1:1 2:0:0",
            Show_Positions ("a" & LF));
      Test ("lexer: an empty source is just the end of file",
            "1:0:0",
            Show_Positions (""));
      Test ("lexer: CR LF line ends",
            "1:1:1 2:1:1 3:0:0",
            Show_Positions ("a" & Ada.Characters.Latin_1.CR & LF & "b"));

      Test ("lexer: no diagnostics for good source",
            "",
            Show_Diagnostics ("x = 'a' : ""b"""));
      Test ("lexer: unterminated character literal",
            "1:5: unterminated character literal",
            Show_Diagnostics ("x = 'a"));
      Test ("lexer: unterminated string literal",
            "2:3: unterminated string literal",
            Show_Diagnostics ("x" & LF & "y ""abc" & LF & "z"));
      Test ("lexer: unterminated block comment",
            "1:3: unterminated block comment",
            Show_Diagnostics ("a {- {- -}"));
      Test ("lexer: a bad escape is reported at the literal",
            "1:3: unknown escape \q",
            Show_Diagnostics ("a ""\q"""));
      Test ("lexer: a bad character is a token, not a diagnostic",
            "IDENTIFIER[a] BAD_CHARACTER[\001]",
            Show_Tokens ("a" & Character'Val (1)));
   end Run_Tests;

   ----------------------
   -- Show_Diagnostics --
   ----------------------

   function Show_Diagnostics (Source : String) return String is
      Tokens      : Token_Vectors.Vector;
      Diagnostics : Diagnostic_Vectors.Vector;
      Result      : Unbounded_String;
   begin
      Tokenize (Source, Tokens, Diagnostics);
      for D of Diagnostics loop
         if Result /= Null_Unbounded_String then
            Append (Result, "; ");
         end if;
         Append (Result,
                 Image (D.Line) & ":" & Image (D.Column) & ": "
                 & To_String (D.Message));
      end loop;
      return To_String (Result);
   end Show_Diagnostics;

   --------------------
   -- Show_Positions --
   --------------------

   function Show_Positions (Source : String) return String is
      Tokens      : Token_Vectors.Vector;
      Diagnostics : Diagnostic_Vectors.Vector;
      Result      : Unbounded_String;
   begin
      Tokenize (Source, Tokens, Diagnostics);
      for T of Tokens loop
         if Result /= Null_Unbounded_String then
            Append (Result, " ");
         end if;
         Append (Result,
                 Image (T.Line) & ":" & Image (T.Column) & ":"
                 & Image (T.Indent));
      end loop;
      return To_String (Result);
   end Show_Positions;

   -----------------
   -- Show_Tokens --
   -----------------

   function Show_Tokens (Source : String) return String is
      use type Leander.Parser.Tokens.Token;
      Tokens      : Token_Vectors.Vector;
      Diagnostics : Diagnostic_Vectors.Vector;
      Result      : Unbounded_String;
   begin
      Tokenize (Source, Tokens, Diagnostics);
      for T of Tokens loop
         exit when T.Kind = Leander.Parser.Tokens.Tok_End_Of_File;

         if Result /= Null_Unbounded_String then
            Append (Result, " ");
         end if;

         declare
            Kind : constant String := T.Kind'Image;
         begin
            Append (Result, Kind (Kind'First + 4 .. Kind'Last) & "[");
         end;

         for Ch of To_String (T.Text) loop
            if Ch in ' ' .. '~' then
               Append (Result, Ch);
            else
               declare
                  Code : constant String := Image (Character'Pos (Ch));
               begin
                  Append (Result,
                          "\" & (1 .. 3 - Code'Length => '0') & Code);
               end;
            end if;
         end loop;

         Append (Result, "]");
      end loop;
      return To_String (Result);
   end Show_Tokens;

end Leander.Tests.Lexer;
