with Ada.Text_IO;
with Leander.Logging;

package body Leander.Errors is

   Error_Count : Natural := 0;

   ------------------
   -- Clear_Errors --
   ------------------

   procedure Clear_Errors is
   begin
      Error_Count := 0;
   end Clear_Errors;

   -----------
   -- Error --
   -----------

   procedure Error
     (Source  : Leander.Source.Source_Reference;
      Message : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Leander.Source.Show (Source) & ": " & Message);
      Leander.Logging.Log
        (Leander.Source.Show (Source) & ": " & Message);
      Error_Count := Error_Count + 1;
   end Error;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors return Boolean is
   begin
      return Error_Count > 0;
   end Has_Errors;

end Leander.Errors;
