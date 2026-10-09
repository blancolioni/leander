with Leander.Parser.Tokens;            use Leander.Parser.Tokens;

private package Leander.Parser.Lexical is

   --  The parser's view of the token stream: the current token, a little
   --  lookahead, and diagnostics against the current token's position.
   --
   --  Sources nest -- a module's imports are loaded while its own file is
   --  still open -- so Open and Close keep a stack, and every query is
   --  about the innermost source. Each source is lexed in full by
   --  Leander.Parser.Lexer when it is opened, and any lexical errors are
   --  reported then.
   --
   --  Nothing here requires a source to be open: with none, Tok is
   --  Tok_End_Of_File and Error and Warning report without a position.

   type Token_Set is array (Positive range <>) of Token;

   function "+" (Left : Token_Set; Right : Token) return Token_Set
   is (Left & Right);

   function "<=" (Left : Token; Right : Token_Set) return Boolean
   is (for some T of Right => T = Left);

   procedure Open (Name : String);
   --  Open the source file Name.

   procedure Open_String (Text : String);
   --  Open Text as a source, under the name "user input".

   procedure Close;

   function Tok return Token;
   function Tok_Text return String;
   function Tok_Character_Value return Character;
   function Tok_Line return Natural;
   function Tok_Column return Natural;
   function Tok_Indent return Natural;
   function Tok_File_Name return String;

   function Next_Tok (Ahead : Positive := 1) return Token;
   function Tok_Text (Ahead : Natural) return String;

   procedure Scan;

   procedure Expect
     (T          : Token;
      Skip_Up_To : Token_Set);
   --  If T is the current token, scan past it. Otherwise report it as
   --  missing and skip to T or anything in Skip_Up_To, scanning past T
   --  if that is where it stopped.

   procedure Expect
     (T          : Token;
      Skip_Up_To : Token);

   procedure Skip_To
     (Skip_To_And_Parse : Token_Set;
      Skip_To_And_Stop  : Token_Set);
   --  Skip to anything in either set, then scan past it if it was in
   --  Skip_To_And_Stop.

   procedure Error (Message : String);
   procedure Warning (Message : String);

end Leander.Parser.Lexical;
