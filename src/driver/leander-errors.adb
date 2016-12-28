with Ada.Text_IO;

package body Leander.Errors is

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
   end Error;

end Leander.Errors;
