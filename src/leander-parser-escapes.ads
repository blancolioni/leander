with Ada.Strings.Unbounded;

package Leander.Parser.Escapes is

   --  Haskell 2010 escapes (Report 2.6): the single-character escapes
   --  \a \b \f \n \r \t \v \\ \" \', the ASCII control names (\NUL,
   --  \SOH, ... \DEL) and carets (\^A), decimal, octal (\o) and hex (\x)
   --  codes, and, in strings only, the empty escape \& and gaps.  The
   --  lexer hands the text between the quotes over undecoded.
   --
   --  Characters are Latin-1, so a code above 255 is rejected.

   procedure Decode_String
     (Raw     : String;
      Result  : out Ada.Strings.Unbounded.Unbounded_String;
      Message : out Ada.Strings.Unbounded.Unbounded_String);
   --  Message is empty on success; otherwise it describes the first bad
   --  escape, and Result holds what was decoded before it.

   procedure Decode_Character
     (Raw     : String;
      Result  : out Character;
      Message : out Ada.Strings.Unbounded.Unbounded_String);
   --  As Decode_String, but Raw must decode to exactly one character.

end Leander.Parser.Escapes;
