with Ada.Text_IO;

package body Leander.Logging is

   Enabled : Boolean := False;
   File : Ada.Text_IO.File_Type;

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      if Enabled then
         Ada.Text_IO.Put_Line (File, Message);
      end if;
   end Log;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging is
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, "leander.log");
      Enabled := True;
   end Start_Logging;

   ------------------
   -- Stop_Logging --
   ------------------

   procedure Stop_Logging is
   begin
      Enabled := False;
      Ada.Text_IO.Close (File);
   end Stop_Logging;

end Leander.Logging;
