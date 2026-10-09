with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Streams.Stream_IO;

with Leander.Errors;
with Leander.Parser.Lexer;

package body Leander.Parser.Lexical is

   use Ada.Strings.Unbounded;
   use Leander.Parser.Lexer;

   type Session is
      record
         File_Name : Unbounded_String;
         Tokens    : Token_Vectors.Vector;
         Current   : Positive := 1;
      end record;

   package Session_Vectors is
     new Ada.Containers.Vectors (Positive, Session);

   Sessions : Session_Vectors.Vector;

   function Is_Open return Boolean
   is (not Sessions.Is_Empty);

   function Token_At (Ahead : Natural) return Token_Info;
   --  The token Ahead places past the current one, or the final
   --  Tok_End_Of_File if that is past the end. Only valid when Is_Open.

   procedure Push (Name : String; Source : String);
   --  Lex Source, report its lexical errors, and make it the current
   --  source.

   function Read_File (Name : String) return String;
   --  The contents of the file Name, with CR LF turned into LF.

   procedure Skip_To (Stop : Token_Set);

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      if Is_Open then
         Sessions.Delete_Last;
      end if;
   end Close;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      if Is_Open then
         Leander.Errors.Report
           (Tok_File_Name, Tok_Line, Tok_Column, Message);
      else
         Leander.Errors.Report ("", 0, 0, Message);
      end if;
   end Error;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (T          : Token;
      Skip_Up_To : Token_Set)
   is
   begin
      if Tok = T then
         Scan;
      else
         Error ("missing " & Token'Image (T));
         Skip_To (Skip_Up_To + T);
         if Tok = T then
            Scan;
         end if;
      end if;
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (T          : Token;
      Skip_Up_To : Token)
   is
   begin
      Expect (T, Token_Set'[Skip_Up_To]);
   end Expect;

   --------------
   -- Next_Tok --
   --------------

   function Next_Tok (Ahead : Positive := 1) return Token is
   begin
      return (if Is_Open then Token_At (Ahead).Kind else Tok_End_Of_File);
   end Next_Tok;

   ----------
   -- Open --
   ----------

   procedure Open (Name : String) is
   begin
      Push (Name, Read_File (Name));
   end Open;

   -----------------
   -- Open_String --
   -----------------

   procedure Open_String (Text : String) is
   begin
      Push ("user input", Text);
   end Open_String;

   ----------
   -- Push --
   ----------

   procedure Push (Name : String; Source : String) is
      New_Session : Session;
      Problems    : Diagnostic_Vectors.Vector;
   begin
      New_Session.File_Name := To_Unbounded_String (Name);
      Tokenize (Source, New_Session.Tokens, Problems);

      for Problem of Problems loop
         Leander.Errors.Report
           (Name, Problem.Line, Problem.Column, To_String (Problem.Message));
      end loop;

      Sessions.Append (New_Session);
   end Push;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (Name : String) return String is
      use Ada.Streams.Stream_IO;
      File : File_Type;
   begin
      Open (File, In_File, Name);

      declare
         Size   : constant Natural := Natural (Ada.Directories.Size (Name));
         Raw    : String (1 .. Size);
         Result : String (1 .. Size);
         Last   : Natural := 0;
      begin
         String'Read (Stream (File), Raw);
         Close (File);

         for I in Raw'Range loop
            if Raw (I) = Ada.Characters.Latin_1.CR
              and then I < Raw'Last
              and then Raw (I + 1) = Ada.Characters.Latin_1.LF
            then
               null;
            else
               Last := Last + 1;
               Result (Last) := Raw (I);
            end if;
         end loop;

         return Result (1 .. Last);
      end;
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Read_File;

   ----------
   -- Scan --
   ----------

   procedure Scan is
   begin
      if Is_Open then
         declare
            Top : Session renames Sessions (Sessions.Last_Index);
         begin
            if Top.Current < Top.Tokens.Last_Index then
               Top.Current := Top.Current + 1;
            end if;
         end;
      end if;
   end Scan;

   -------------
   -- Skip_To --
   -------------

   procedure Skip_To (Stop : Token_Set) is
   begin
      while Tok /= Tok_End_Of_File and then not (Tok <= Stop) loop
         Scan;
      end loop;
   end Skip_To;

   -------------
   -- Skip_To --
   -------------

   procedure Skip_To
     (Skip_To_And_Parse : Token_Set;
      Skip_To_And_Stop  : Token_Set)
   is
   begin
      Skip_To (Skip_To_And_Parse & Skip_To_And_Stop);
      if Tok <= Skip_To_And_Stop then
         Scan;
      end if;
   end Skip_To;

   ---------
   -- Tok --
   ---------

   function Tok return Token is
   begin
      return (if Is_Open then Token_At (0).Kind else Tok_End_Of_File);
   end Tok;

   -------------------------
   -- Tok_Character_Value --
   -------------------------

   function Tok_Character_Value return Character is
      Text : constant String := Tok_Text;
   begin
      return (if Tok = Tok_Character_Literal and then Text'Length = 1
              then Text (Text'First)
              else Ada.Characters.Latin_1.NUL);
   end Tok_Character_Value;

   ----------------
   -- Tok_Column --
   ----------------

   function Tok_Column return Natural is
   begin
      return (if Is_Open then Token_At (0).Column else 0);
   end Tok_Column;

   -------------------
   -- Tok_File_Name --
   -------------------

   function Tok_File_Name return String is
   begin
      return (if Is_Open
              then To_String (Sessions (Sessions.Last_Index).File_Name)
              else "");
   end Tok_File_Name;

   ----------------
   -- Tok_Indent --
   ----------------

   function Tok_Indent return Natural is
   begin
      return (if Is_Open then Token_At (0).Indent else 0);
   end Tok_Indent;

   --------------
   -- Tok_Line --
   --------------

   function Tok_Line return Natural is
   begin
      return (if Is_Open then Token_At (0).Line else 0);
   end Tok_Line;

   --------------
   -- Tok_Text --
   --------------

   function Tok_Text return String is
   begin
      return Tok_Text (0);
   end Tok_Text;

   --------------
   -- Tok_Text --
   --------------

   function Tok_Text (Ahead : Natural) return String is
   begin
      return (if Is_Open then To_String (Token_At (Ahead).Text) else "");
   end Tok_Text;

   --------------
   -- Token_At --
   --------------

   function Token_At (Ahead : Natural) return Token_Info is
      Top   : Session renames Sessions (Sessions.Last_Index);
      Index : constant Positive :=
                Positive'Min (Top.Current + Ahead, Top.Tokens.Last_Index);
   begin
      return Top.Tokens (Index);
   end Token_At;

   -------------
   -- Warning --
   -------------

   procedure Warning (Message : String) is
   begin
      if Is_Open then
         Leander.Errors.Report
           (Tok_File_Name, Tok_Line, Tok_Column, Message,
            Is_Warning => True);
      else
         Leander.Errors.Report ("", 0, 0, Message, Is_Warning => True);
      end if;
   end Warning;

end Leander.Parser.Lexical;
