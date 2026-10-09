with Ada.Directories;
with Ada.Text_IO;

package body Leander.Errors is

   Seen : Boolean := False;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Seen := False;
   end Clear;

   ----------------
   -- Had_Errors --
   ----------------

   function Had_Errors return Boolean is (Seen);

   ----------------
   -- Note_Error --
   ----------------

   procedure Note_Error is
   begin
      Seen := True;
   end Note_Error;

   ------------
   -- Report --
   ------------

   procedure Report
     (File_Name  : String;
      Line       : Natural;
      Column     : Natural;
      Message    : String;
      Is_Warning : Boolean := False)
   is
      function Image (N : Natural) return String;
      function Simple_Name return String;

      -----------
      -- Image --
      -----------

      function Image (N : Natural) return String is
         S : constant String := N'Image;
      begin
         return S (S'First + 1 .. S'Last);
      end Image;

      -----------------
      -- Simple_Name --
      -----------------

      function Simple_Name return String is
      begin
         return Ada.Directories.Simple_Name (File_Name);
      exception
         when others =>
            --  "user input" and the like are not paths at all.
            return File_Name;
      end Simple_Name;

      Where : constant String :=
                (if File_Name = "" then "" else Simple_Name & ":")
                & (if Line = 0 then "" else Image (Line) & ":")
                & (if Line = 0 or else Column = 0
                   then ""
                   else Image (Column) & ":");
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Where & (if Where = "" then "" else " ")
         & (if Is_Warning then "warning: " else "")
         & Message);

      if not Is_Warning then
         Seen := True;
      end if;
   end Report;

end Leander.Errors;
