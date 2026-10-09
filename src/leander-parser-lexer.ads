with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Leander.Parser.Tokens;

package Leander.Parser.Lexer is

   --  Haskell 2010 lexical syntax (Report chapter 2), done in one pass
   --  over a whole source text, with nothing kept between calls.
   --
   --  Identifiers and operators keep the shapes the parser was written
   --  against: a qualified name such as M.x arrives as M, ., x, and the
   --  parser joins adjacent pieces. A run of symbol characters is one
   --  token, and the reserved operators (=, \, .., |, ->, <-, =>, ::)
   --  are recognised only as a whole run.

   type Token_Info is
      record
         Kind   : Leander.Parser.Tokens.Token;
         Text   : Ada.Strings.Unbounded.Unbounded_String;
         Line   : Natural;
         Column : Natural;
         Indent : Natural;
      end record;
   --  Text is the token as written, except for literals: a string or
   --  character literal holds its decoded value, an integer its value in
   --  decimal (whatever base it was written in), and a float a form that
   --  Float'Value accepts.
   --
   --  Column counts characters from 1. Indent is the visual column, with
   --  a tab stop every 8 columns, which is what layout is measured in.
   --
   --  The last token is always Tok_End_Of_File, on the line after the
   --  last one, with Column and Indent 0: the parser's layout loops all
   --  have the shape "while Tok_Indent > N", and rely on it to stop.

   package Token_Vectors is
     new Ada.Containers.Vectors (Positive, Token_Info);

   type Diagnostic is
      record
         Line    : Positive;
         Column  : Positive;
         Message : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Diagnostic_Vectors is
     new Ada.Containers.Vectors (Positive, Diagnostic);

   procedure Tokenize
     (Source      : String;
      Tokens      : out Token_Vectors.Vector;
      Diagnostics : out Diagnostic_Vectors.Vector);
   --  Lines in Source are separated by LF; a CR before an LF is ignored.
   --  A lexical error is recorded in Diagnostics and lexing carries on,
   --  so Tokens is always complete.

end Leander.Parser.Lexer;
