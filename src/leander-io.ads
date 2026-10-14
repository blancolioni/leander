private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with Ada.Wide_Wide_Text_IO;

package Leander.IO is

   --  What a program's IO primitives reach: streams by number, as a
   --  program's handles are numbered.  0, 1 and 2 are standard input,
   --  output and error; an opened file has a number from 3 up.

   Standard_Input  : constant := 0;
   Standard_Output : constant := 1;
   Standard_Error  : constant := 2;

   End_Of_Input : constant := -1;
   --  What Get returns once there is nothing left to read.

   No_Such_File      : constant := -1;
   Permission_Denied : constant := -2;
   --  What Open returns when it cannot open the file.

   type Open_Mode is (Read_Mode, Write_Mode, Append_Mode);

   type Abstraction is interface;
   type Reference is access all Abstraction'Class;

   procedure Put
     (This       : in out Abstraction;
      File_Index : Natural;
      Value      : Wide_Wide_Character)
   is abstract;

   function Get
     (This       : in out Abstraction;
      File_Index : Natural)
      return Integer
   is abstract;
   --  The next character's code, or End_Of_Input.  A carriage return is
   --  dropped, so that a line ends with a line feed whatever wrote it.

   procedure Add_Path_Character
     (This : in out Abstraction;
      Code : Natural;
      Fresh : Boolean)
   is abstract;
   --  Add a character to the path the next Open uses, starting a new
   --  path when Fresh.  A path arrives one character at a time, as a
   --  program's error message does.

   function Open
     (This : in out Abstraction;
      Mode : Open_Mode)
      return Integer
   is abstract;
   --  Open the file at the path collected so far: its number, or
   --  No_Such_File or Permission_Denied.  Writing creates the file, or
   --  empties it; appending creates it if need be.

   procedure Close
     (This       : in out Abstraction;
      File_Index : Natural)
   is abstract;

   function Null_IO return Reference;
   function Local_IO return Reference;

   type Local_IO_Type is new Abstraction with private;
   --  The terminal, and the file system.  A test can extend it to stand
   --  in for the terminal and still reach real files.

   overriding procedure Put
     (This       : in out Local_IO_Type;
      File_Index : Natural;
      Value      : Wide_Wide_Character);

   overriding function Get
     (This       : in out Local_IO_Type;
      File_Index : Natural)
      return Integer;

   overriding procedure Add_Path_Character
     (This  : in out Local_IO_Type;
      Code  : Natural;
      Fresh : Boolean);

   overriding function Open
     (This : in out Local_IO_Type;
      Mode : Open_Mode)
      return Integer;

   overriding procedure Close
     (This       : in out Local_IO_Type;
      File_Index : Natural);

private

   type File_Access is access Ada.Wide_Wide_Text_IO.File_Type;

   package File_Vectors is
     new Ada.Containers.Vectors (Positive, File_Access);

   type Local_IO_Type is new Abstraction with
      record
         Files : File_Vectors.Vector;
         --  File number N is Files (N - 2); a closed one is null.
         Path  : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Leander.IO;
