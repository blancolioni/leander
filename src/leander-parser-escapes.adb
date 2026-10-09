with Ada.Characters.Latin_1;

package body Leander.Parser.Escapes is

   use Ada.Strings.Unbounded;

   type Control_Name is
      record
         Name   : String (1 .. 3);
         Length : Positive;
         Code   : Natural;
      end record;

   type Control_Name_Array is array (Positive range <>) of Control_Name;

   Control_Names : constant Control_Name_Array :=
                     [("NUL", 3, 0), ("SOH", 3, 1), ("STX", 3, 2),
                      ("ETX", 3, 3), ("EOT", 3, 4), ("ENQ", 3, 5),
                      ("ACK", 3, 6), ("BEL", 3, 7), ("BS ", 2, 8),
                      ("HT ", 2, 9), ("LF ", 2, 10), ("VT ", 2, 11),
                      ("FF ", 2, 12), ("CR ", 2, 13), ("SO ", 2, 14),
                      ("SI ", 2, 15), ("DLE", 3, 16), ("DC1", 3, 17),
                      ("DC2", 3, 18), ("DC3", 3, 19), ("DC4", 3, 20),
                      ("NAK", 3, 21), ("SYN", 3, 22), ("ETB", 3, 23),
                      ("CAN", 3, 24), ("EM ", 2, 25), ("SUB", 3, 26),
                      ("ESC", 3, 27), ("FS ", 2, 28), ("GS ", 2, 29),
                      ("RS ", 2, 30), ("US ", 2, 31), ("SP ", 2, 32),
                      ("DEL", 3, 127)];

   procedure Decode
     (Raw       : String;
      In_String : Boolean;
      Result    : out Unbounded_String;
      Message   : out Unbounded_String);

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Raw       : String;
      In_String : Boolean;
      Result    : out Unbounded_String;
      Message   : out Unbounded_String)
   is
      use Ada.Characters.Latin_1;

      Index : Positive := Raw'First;

      procedure Fail (Text : String);
      procedure Add_Code (Code : Natural);
      procedure Numeric (Base : Positive);
      procedure Control;
      procedure Gap;
      procedure Escape;

      function Is_White (Ch : Character) return Boolean
      is (Ch in ' ' | HT | LF | VT | FF | CR);

      function Digit_Value (Ch : Character) return Natural
      is (case Ch is
             when '0' .. '9' => Character'Pos (Ch) - Character'Pos ('0'),
             when 'a' .. 'f' => Character'Pos (Ch) - Character'Pos ('a') + 10,
             when 'A' .. 'F' => Character'Pos (Ch) - Character'Pos ('A') + 10,
             when others     => Natural'Last);

      --------------
      -- Add_Code --
      --------------

      procedure Add_Code (Code : Natural) is
      begin
         if Code > 255 then
            Fail ("character code" & Code'Image
                  & " is out of range (Latin-1 only)");
         else
            Append (Result, Character'Val (Code));
         end if;
      end Add_Code;

      -------------
      -- Control --
      -------------

      procedure Control is
         Best : Natural := 0;
      begin
         for I in Control_Names'Range loop
            declare
               Item : Control_Name renames Control_Names (I);
               Last : constant Natural := Index + Item.Length - 1;
            begin
               if Last <= Raw'Last
                 and then Raw (Index .. Last) = Item.Name (1 .. Item.Length)
                 and then (Best = 0
                           or else Item.Length > Control_Names (Best).Length)
               then
                  Best := I;
               end if;
            end;
         end loop;

         if Best = 0 then
            Fail ("unknown escape \" & Raw (Index));
         else
            Add_Code (Control_Names (Best).Code);
            Index := Index + Control_Names (Best).Length;
         end if;
      end Control;

      ------------
      -- Escape --
      ------------

      procedure Escape is
         Ch : constant Character := Raw (Index);
      begin
         case Ch is
            when 'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\' | '"' | ''' =>
               Append
                 (Result,
                  (case Ch is
                      when 'a'    => BEL,
                      when 'b'    => BS,
                      when 'f'    => FF,
                      when 'n'    => LF,
                      when 'r'    => CR,
                      when 't'    => HT,
                      when 'v'    => VT,
                      when others => Ch));
               Index := Index + 1;
            when '&' =>
               if not In_String then
                  Fail ("\& is only allowed in a string");
               end if;
               Index := Index + 1;
            when '0' .. '9' =>
               Numeric (10);
            when 'o' =>
               Index := Index + 1;
               Numeric (8);
            when 'x' =>
               Index := Index + 1;
               Numeric (16);
            when '^' =>
               Index := Index + 1;
               if Index <= Raw'Last and then Raw (Index) in '@' .. '_' then
                  Add_Code (Character'Pos (Raw (Index)) - Character'Pos ('@'));
                  Index := Index + 1;
               else
                  Fail ("bad control escape \^");
               end if;
            when 'A' .. 'Z' =>
               Control;
            when others =>
               if Is_White (Ch) then
                  Gap;
               else
                  Fail ("unknown escape \" & Ch);
               end if;
         end case;
      end Escape;

      ----------
      -- Fail --
      ----------

      procedure Fail (Text : String) is
      begin
         if Message = Null_Unbounded_String then
            Message := To_Unbounded_String (Text);
         end if;
      end Fail;

      ---------
      -- Gap --
      ---------

      procedure Gap is
      begin
         if not In_String then
            Fail ("a gap is only allowed in a string");
         end if;

         while Index <= Raw'Last and then Is_White (Raw (Index)) loop
            Index := Index + 1;
         end loop;

         if Index <= Raw'Last and then Raw (Index) = '\' then
            Index := Index + 1;
         else
            Fail ("unterminated string gap");
         end if;
      end Gap;

      -------------
      -- Numeric --
      -------------

      procedure Numeric (Base : Positive) is
         Start : constant Positive := Index;
         Code  : Natural := 0;
      begin
         while Index <= Raw'Last
           and then Digit_Value (Raw (Index)) < Base
         loop
            --  Once past 255 the value is rejected anyway, so stop
            --  growing it rather than risk overflow.
            if Code <= 255 then
               Code := Code * Base + Digit_Value (Raw (Index));
            end if;
            Index := Index + 1;
         end loop;

         if Index = Start then
            Fail ("missing digits in numeric escape");
         else
            Add_Code (Code);
         end if;
      end Numeric;

   begin
      Result := Null_Unbounded_String;
      Message := Null_Unbounded_String;

      while Index <= Raw'Last and then Message = Null_Unbounded_String loop
         if Raw (Index) = '\' then
            Index := Index + 1;
            if Index > Raw'Last then
               Fail ("unterminated escape");
            else
               Escape;
            end if;
         else
            Append (Result, Raw (Index));
            Index := Index + 1;
         end if;
      end loop;
   end Decode;

   ----------------------
   -- Decode_Character --
   ----------------------

   procedure Decode_Character
     (Raw     : String;
      Result  : out Character;
      Message : out Ada.Strings.Unbounded.Unbounded_String)
   is
      Decoded : Unbounded_String;
   begin
      Decode (Raw, False, Decoded, Message);
      Result := ' ';

      if Message = Null_Unbounded_String then
         if Length (Decoded) = 0 then
            Message := To_Unbounded_String ("empty character literal");
         elsif Length (Decoded) > 1 then
            Message :=
              To_Unbounded_String
                ("character literal holds more than one character");
         else
            Result := Element (Decoded, 1);
         end if;
      end if;
   end Decode_Character;

   -------------------
   -- Decode_String --
   -------------------

   procedure Decode_String
     (Raw     : String;
      Result  : out Ada.Strings.Unbounded.Unbounded_String;
      Message : out Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Decode (Raw, True, Result, Message);
   end Decode_String;

end Leander.Parser.Escapes;
