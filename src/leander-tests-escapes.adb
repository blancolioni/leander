with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Leander.Parser.Escapes;

package body Leander.Tests.Escapes is

   use Ada.Strings.Unbounded;

   function Codes (S : String) return String;
   --  The character codes of S, space-separated, so that control
   --  characters show up in a test report.

   procedure Test_String (Name, Raw, Expected : String);
   --  Expected is the codes of the decoded string, or the error message.

   procedure Test_Character (Name, Raw, Expected : String);

   -----------
   -- Codes --
   -----------

   function Codes (S : String) return String is
      Result : Unbounded_String;
   begin
      for Ch of S loop
         if Result /= Null_Unbounded_String then
            Append (Result, ' ');
         end if;
         Append (Result,
                 Ada.Strings.Fixed.Trim
                   (Natural'Image (Character'Pos (Ch)),
                    Ada.Strings.Left));
      end loop;
      return To_String (Result);
   end Codes;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      LF : constant Character := Ada.Characters.Latin_1.LF;
   begin
      Test_String ("escape: plain text", "abc", "97 98 99");
      Test_String ("escape: single characters", "\a\b\f\n\r\t\v",
                   "7 8 12 10 13 9 11");
      Test_String ("escape: quotes and backslash", "\\\""\'", "92 34 39");
      Test_String ("escape: decimal", "\65\0\255", "65 0 255");
      Test_String ("escape: hex", "\x41\x4a\x4A", "65 74 74");
      Test_String ("escape: octal", "\o101\o7", "65 7");
      Test_String ("escape: control names", "\NUL\BEL\ESC\SP\DEL",
                   "0 7 27 32 127");
      Test_String ("escape: longest control name", "\SOH", "1");
      Test_String ("escape: shorter control name", "\SOx", "14 120");
      Test_String ("escape: caret", "\^@\^A\^Z\^[\^\\^]\^^\^_",
                   "0 1 26 27 28 29 30 31");
      Test_String ("escape: empty", "a\&b", "97 98");
      Test_String ("escape: empty ends a name", "\SO\&H", "14 72");
      Test_String ("escape: empty ends a number", "\1\&2", "1 50");
      Test_String ("escape: gap", "a\   \b", "97 98");
      Test_String ("escape: gap across lines", "a\" & LF & "   \b", "97 98");

      Test_String ("escape: unknown", "a\qb", "unknown escape \q");
      Test_String ("escape: unknown name", "\XYZ", "unknown escape \X");
      Test_String ("escape: out of range", "\256",
                   "character code 256 is out of range (Latin-1 only)");
      Test_String ("escape: huge number", "\99999999999999999999",
                   "character code 999 is out of range (Latin-1 only)");
      Test_String ("escape: hex without digits", "\xg",
                   "missing digits in numeric escape");
      Test_String ("escape: bad caret", "\^a", "bad control escape \^");
      Test_String ("escape: unterminated gap", "\   x",
                   "unterminated string gap");
      Test_String ("escape: trailing backslash", "a\", "unterminated escape");

      Test_Character ("char escape: plain", "A", "65");
      Test_Character ("char escape: decimal", "\65", "65");
      Test_Character ("char escape: quote", "\'", "39");
      Test_Character ("char escape: double quote", """", "34");
      Test_Character ("char escape: control name", "\DEL", "127");
      Test_Character ("char escape: empty literal", "",
                      "empty character literal");
      Test_Character ("char escape: two characters", "ab",
                      "character literal holds more than one character");
      Test_Character ("char escape: empty escape", "\&",
                      "\& is only allowed in a string");
      Test_Character ("char escape: gap", "\ \",
                      "a gap is only allowed in a string");
   end Run_Tests;

   --------------------
   -- Test_Character --
   --------------------

   procedure Test_Character (Name, Raw, Expected : String) is
      Value   : Character;
      Message : Unbounded_String;
   begin
      Leander.Parser.Escapes.Decode_Character (Raw, Value, Message);
      Test (Name, Expected,
            (if Message = Null_Unbounded_String
             then Codes ([Value])
             else To_String (Message)));
   end Test_Character;

   -----------------
   -- Test_String --
   -----------------

   procedure Test_String (Name, Raw, Expected : String) is
      Value   : Unbounded_String;
      Message : Unbounded_String;
   begin
      Leander.Parser.Escapes.Decode_String (Raw, Value, Message);
      Test (Name, Expected,
            (if Message = Null_Unbounded_String
             then Codes (To_String (Value))
             else To_String (Message)));
   end Test_String;

end Leander.Tests.Escapes;
