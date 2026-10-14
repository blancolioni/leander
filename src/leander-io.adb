with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

package body Leander.IO is

   use Ada.Wide_Wide_Text_IO;

   type Null_IO_Instance is new Abstraction with null record;

   overriding procedure Put
     (This       : in out Null_IO_Instance;
      File_Index : Natural;
      Value      : Wide_Wide_Character)
   is null;

   overriding function Get
     (This       : in out Null_IO_Instance;
      File_Index : Natural)
      return Integer
   is (End_Of_Input);

   overriding procedure Add_Path_Character
     (This  : in out Null_IO_Instance;
      Code  : Natural;
      Fresh : Boolean)
   is null;

   overriding function Open
     (This : in out Null_IO_Instance;
      Mode : Open_Mode)
      return Integer
   is (No_Such_File);

   overriding procedure Close
     (This       : in out Null_IO_Instance;
      File_Index : Natural)
   is null;

   Singleton_Null_IO  : aliased Null_IO_Instance := (null record);
   Singleton_Local_IO : aliased Local_IO_Type;

   procedure Free is
     new Ada.Unchecked_Deallocation (File_Type, File_Access);

   function File
     (This       : Local_IO_Type'Class;
      File_Index : Natural)
      return File_Access
   is (if File_Index >= 3
         and then File_Index - 2 <= This.Files.Last_Index
       then This.Files (File_Index - 2)
       else null);
   --  The open file numbered File_Index, or null.

   function Null_IO return Reference
   is (Singleton_Null_IO'Access);

   function Local_IO return Reference
   is (Singleton_Local_IO'Access);

   ------------------------
   -- Add_Path_Character --
   ------------------------

   overriding procedure Add_Path_Character
     (This  : in out Local_IO_Type;
      Code  : Natural;
      Fresh : Boolean)
   is
   begin
      if Fresh then
         This.Path := Ada.Strings.Unbounded.Null_Unbounded_String;
      end if;
      Ada.Strings.Unbounded.Append
        (This.Path, (if Code <= 255 then Character'Val (Code) else '?'));
   end Add_Path_Character;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (This       : in out Local_IO_Type;
      File_Index : Natural)
   is
      F : File_Access := File (This, File_Index);
   begin
      if F /= null then
         if Is_Open (F.all) then
            Close (F.all);
         end if;
         Free (F);
         This.Files (File_Index - 2) := null;
      end if;
   end Close;

   ---------
   -- Get --
   ---------

   overriding function Get
     (This       : in out Local_IO_Type;
      File_Index : Natural)
      return Integer
   is
      F  : constant File_Access := File (This, File_Index);
      Ch : Wide_Wide_Character;
   begin
      if File_Index = Standard_Input then
         --  Whatever the program has written so far, a prompt say, has
         --  to be seen before it waits for an answer.
         Flush (Ada.Wide_Wide_Text_IO.Standard_Output);
      elsif F = null then
         return End_Of_Input;
      end if;

      loop
         if File_Index = Standard_Input then
            Get_Immediate (Ada.Wide_Wide_Text_IO.Standard_Input, Ch);
         else
            Get_Immediate (F.all, Ch);
         end if;
         exit when Ch /= Wide_Wide_Character'Val (13);
      end loop;
      return Wide_Wide_Character'Pos (Ch);
   exception
      when Ada.IO_Exceptions.End_Error =>
         return End_Of_Input;
   end Get;

   ----------
   -- Open --
   ----------

   overriding function Open
     (This : in out Local_IO_Type;
      Mode : Open_Mode)
      return Integer
   is
      Path : constant String := Ada.Strings.Unbounded.To_String (This.Path);
      F    : File_Access := new File_Type;
   begin
      This.Path := Ada.Strings.Unbounded.Null_Unbounded_String;

      case Mode is
         when Read_Mode =>
            Open (F.all, In_File, Path);
         when Write_Mode =>
            Create (F.all, Out_File, Path);
         when Append_Mode =>
            begin
               Open (F.all, Append_File, Path);
            exception
               when Ada.IO_Exceptions.Name_Error =>
                  Create (F.all, Out_File, Path);
            end;
      end case;

      --  The first free number, or a new one.
      for I in 1 .. This.Files.Last_Index loop
         if This.Files (I) = null then
            This.Files (I) := F;
            return I + 2;
         end if;
      end loop;
      This.Files.Append (F);
      return This.Files.Last_Index + 2;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Free (F);
         return No_Such_File;
      when Ada.IO_Exceptions.Use_Error =>
         Free (F);
         return Permission_Denied;
   end Open;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (This       : in out Local_IO_Type;
      File_Index : Natural;
      Value      : Wide_Wide_Character)
   is
      F : constant File_Access := File (This, File_Index);

      procedure Write (To : File_Type);
      --  A line feed ends the line through New_Line: written with Put,
      --  Text_IO would not count the line as ended, and would end it again
      --  when the file closes, or the program exits.

      -----------
      -- Write --
      -----------

      procedure Write (To : File_Type) is
      begin
         if Value = Wide_Wide_Character'Val (10) then
            New_Line (To);
         else
            Put (To, Value);
         end if;
      end Write;

   begin
      if File_Index = Standard_Error then
         Write (Ada.Wide_Wide_Text_IO.Standard_Error);
      elsif F /= null then
         Write (F.all);
      else
         Write (Ada.Wide_Wide_Text_IO.Standard_Output);
      end if;
   end Put;

end Leander.IO;
